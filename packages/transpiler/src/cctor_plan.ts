import * as abaplint from "@abaplint/core";
import {chainOf, constantSource} from "./constant_value";

/** When the class constructors of a class pool run at load, relative to what they use.
 *
 * A class constructor runs when its module loads (structures/class_implementation.ts), not on
 * first use as in ABAP. A global class or interface it names must exist by then, so its module
 * is required (requires.ts). A requirement that would close an import cycle is not taken (two
 * constructors naming each other's classes, a constructor creating its own subclass, a ring):
 * every module of the cycle would await another and none would settle. That pool's class
 * constructors WAIT instead, for the module of the class they named to be registered.
 *
 * A waiting pool delays what depends on its class constructor having run, in ABAP's order:
 * - a subclass's class constructor runs after its superclass's (ABAP runs them top-down);
 * - a class constructor that names a waiting class runs after that class's constructor;
 * - an interface constant read at load from a waiting class's constant is set after that
 *   class's constructor ran.
 * Those wait too, for the class constructor they depend on to have run. The waits are released
 * at the end of the modules involved (`settle`), in dependency order, each exactly once. What
 * still waits when init.mjs has imported everything is an error naming it, never a silent
 * initial value (initialization.ts).
 *
 * The decision is made over the whole registry, in the order of the object names, against the
 * load-time edges the modules really have: superclass, CX_ROOT, the global superclasses of
 * local classes, interface constants read from classes, and the constructor requirements
 * already taken. The import graph stays acyclic. */
export class CctorPlan {
  private static readonly plans = new WeakMap<abaplint.IRegistry, CctorPlan>();

  /** the plan of this registry, built once per transpiler run (Transpiler.run forgets it) */
  public static of(reg: abaplint.IRegistry): CctorPlan {
    let plan = this.plans.get(reg);
    if (plan === undefined) {
      plan = new CctorPlan(reg);
      this.plans.set(reg, plan);
    }
    return plan;
  }

  public static forget(reg: abaplint.IRegistry): void {
    this.plans.delete(reg);
  }

  private readonly required = new Map<string, string[]>();
  /** name -> what it waits for: "X" = X's module registered, "X!" = X's class constructor ran */
  private readonly needs = new Map<string, string[]>();
  private readonly releasing = new Set<string>();

  private constructor(reg: abaplint.IRegistry) {
    const objects = new Map<string, abaplint.ABAPObject>();
    for (const obj of reg.getObjects()) {
      if (obj instanceof abaplint.Objects.Class || obj instanceof abaplint.Objects.Interface) {
        objects.set(obj.getName().toUpperCase(), obj);
      }
    }
    const isClass = (name: string) => objects.get(name) instanceof abaplint.Objects.Class;
    const edges = new Map<string, Set<string>>();
    const edge = (from: string, to: string) => {
      if (from !== to && objects.has(to)) {
        let set = edges.get(from);
        if (set === undefined) {
          set = new Set();
          edges.set(from, set);
        }
        set.add(to);
      }
    };
    const supers = new Map<string, string[]>();
    const constantReads = new Map<string, string[]>();
    for (const [name, obj] of objects) {
      if (obj instanceof abaplint.Objects.Interface) {
        // an interface reads a class constant at load (constant_value.ts, requires.ts)
        const reads = new Set<string>();
        for (const file of obj.getABAPFiles()) {
          for (const statement of file.getStatements()) {
            const chain = statement.get() instanceof abaplint.Statements.Constant ? chainOf(statement) : undefined;
            if (chain === undefined) {
              continue;
            }
            const declared = statement.findDirectExpression(abaplint.Expressions.DefinitionName)?.concatTokens() ?? "";
            try {
              const source = constantSource(reg, chain, [(name + "=>" + declared).toUpperCase()]);
              if (source.kind === "member" && source.object.getName().toUpperCase() !== name) {
                reads.add(source.object.getName().toUpperCase());
              }
            } catch {
              // a chain that never reaches a value: the interface's own transpile refuses it
            }
          }
        }
        for (const r of reads) {
          edge(name, r);
        }
        constantReads.set(name, [...reads].sort());
        continue;
      }
      const clas = obj as abaplint.Objects.Class;
      const chain: string[] = [];
      let sup = clas.getDefinition()?.getSuperClass()?.toUpperCase();
      while (sup !== undefined && chain.includes(sup) === false) {
        chain.push(sup);
        const next = objects.get(sup);
        sup = next instanceof abaplint.Objects.Class ? next.getDefinition()?.getSuperClass()?.toUpperCase() : undefined;
      }
      supers.set(name, chain);
      if (chain.length > 0) {
        edge(name, chain[0]);
      }
      if (name !== "CX_ROOT") {
        edge(name, "CX_ROOT");
      }
      for (const file of obj.getSequencedFiles()) {
        if (file.getFilename().endsWith(".testclasses.abap")) {
          continue;
        }
        for (const c of file.getInfo().listClassDefinitions()) {
          if (c.superClassName !== undefined) {
            edge(name, c.superClassName.toUpperCase());
          }
        }
      }
    }
    const reaches = (from: string, to: string): boolean => {
      const seen = new Set<string>([from]);
      const queue = [from];
      while (queue.length > 0) {
        const at = queue.shift()!;
        if (at === to) {
          return true;
        }
        for (const next of edges.get(at) ?? []) {
          if (seen.has(next) === false) {
            seen.add(next);
            queue.push(next);
          }
        }
      }
      return false;
    };

    const add = (map: Map<string, string[]>, key: string, value: string) => {
      const list = map.get(key) ?? [];
      if (list.includes(value) === false) {
        map.set(key, [...list, value]);
      }
    };
    const names = [...objects.keys()].sort();
    const constructors = new Map<string, string[]>();
    for (const name of names) {
      const targets = isClass(name) ? constructorTargets(objects.get(name)!) : undefined;
      if (targets !== undefined) {
        constructors.set(name, targets.filter(t => objects.has(t) && t !== name).sort());
      }
    }
    // the requirements, and the cycle skips: those pools wait for the module to register
    for (const [name, targets] of constructors) {
      for (const target of targets) {
        if (reaches(target, name)) {
          add(this.needs, name, target);
          this.releasing.add(target);
        } else {
          edge(name, target);
          add(this.required, name, target);
        }
      }
    }
    // what depends on a waiting class constructor having run waits too, to a fixed point
    for (let changed = true; changed;) {
      changed = false;
      const now = new Set(this.needs.keys());
      for (const name of names) {
        let depends: string[];
        if (constructors.has(name)) {
          depends = [...(supers.get(name) ?? []), ...(this.required.get(name) ?? [])];
        } else if (isClass(name)) {
          continue;
        } else {
          depends = constantReads.get(name) ?? [];
        }
        for (const d of depends) {
          if (now.has(d) && (this.needs.get(name) ?? []).includes(d + "!") === false) {
            add(this.needs, name, d + "!");
            changed = true;
          }
        }
      }
    }
  }

  /** the classes and interfaces whose modules a pool's module requires for its class constructors */
  public requiredBy(name: string): readonly string[] {
    return this.required.get(name.toUpperCase()) ?? [];
  }

  /** what a pool's class constructors (or an interface's constants read at load) wait for:
   * "X" = the module of X registered, "X!" = the class constructor of X ran */
  public waitingFor(name: string): readonly string[] {
    return this.needs.get(name.toUpperCase()) ?? [];
  }

  /** whether waits are released at the end of this module */
  public releases(name: string): boolean {
    return this.releasing.has(name.toUpperCase()) || this.needs.has(name.toUpperCase());
  }

  public anyWaiting(): boolean {
    return this.needs.size > 0;
  }
}

/** the global classes and interfaces the CLASS_CONSTRUCTOR implementations of a class pool name;
 * undefined for a pool without class constructors */
function constructorTargets(obj: abaplint.ABAPObject): string[] | undefined {
  const ret = new Set<string>();
  let any = false;
  for (const file of obj.getABAPFiles()) {
    if (file.getFilename().endsWith(".testclasses.abap")) {
      continue;
    }
    let inside = false;
    for (const statement of file.getStatements()) {
      const s = statement.get();
      if (s instanceof abaplint.Statements.MethodImplementation) {
        inside = statement.findDirectExpression(abaplint.Expressions.MethodName)?.concatTokens().toUpperCase() === "CLASS_CONSTRUCTOR";
        any = any || inside;
        continue;
      } else if (s instanceof abaplint.Statements.EndMethod) {
        inside = false;
        continue;
      } else if (inside === false) {
        continue;
      }
      for (const e of [...statement.findAllExpressions(abaplint.Expressions.ClassName),
        ...statement.findAllExpressions(abaplint.Expressions.TypeNameOrInfer)]) {
        const name = e.concatTokens().toUpperCase();
        if (name !== obj.getName().toUpperCase()) {
          ret.add(name);
        }
      }
    }
  }
  return any ? [...ret] : undefined;
}

/** the queue of waiting class constructors: plain generated code, defined by the first module
 * that needs it, so a module needs nothing beyond the runtime it already has. `settle` runs every
 * job whose needs are met, repeatedly, each once; a job's own name counts as "ran" after it. */
export const CCTOR_QUEUE = "globalThis.__abapWaitingConstructors";

function queue(): string {
  return `${CCTOR_QUEUE} ??= {jobs: [], waiting: new Set(), met(n) {\n`
    + `const name = n.endsWith("!") ? n.slice(0, -1) : n;\n`
    + `return abap.Classes[name] !== undefined && (n.endsWith("!") === false || this.waiting.has(name) === false);\n`
    + `}, async settle() {\n`
    + `for (let ran = true; ran;) {\n`
    + `ran = false;\n`
    + `for (const job of [...this.jobs]) {\n`
    + `if (this.jobs.includes(job) && job.needs.every(n => this.met(n))) {\n`
    + `this.jobs.splice(this.jobs.indexOf(job), 1);\n`
    + `await job.run();\n`
    + `this.waiting.delete(job.name);\n`
    + `ran = true;\n`
    + `}\n`
    + `}\n`
    + `}\n`
    + `}};\n`;
}

export function waitingConstructors(name: string, needs: readonly string[], calls: string): string {
  return queue()
    + `${CCTOR_QUEUE}.waiting.add(${JSON.stringify(name)});\n`
    + `${CCTOR_QUEUE}.jobs.push({name: ${JSON.stringify(name)}, `
    + `needs: [${needs.map(n => JSON.stringify(n)).join(", ")}], run: async () => {\n${calls}}});\n`;
}

export function releaseConstructors(): string {
  return queue() + `await ${CCTOR_QUEUE}.settle();\n`;
}

/** the end of init.mjs: what still waits is an error, never a silent initial value */
export function nothingWaits(): string {
  return `if ((${CCTOR_QUEUE}?.jobs.length ?? 0) > 0) {\n`
    + `  const waiting = ${CCTOR_QUEUE}.jobs.map(j => j.name + " for " + j.needs.join(", ")).join("; ");\n`
    + `  throw new Error("class constructors never ran at load, still waiting: " + waiting);\n`
    + `}\n`;
}

import * as abaplint from "@abaplint/core";

/** When the class constructors of a class pool run at load, relative to the classes they name.
 *
 * A class constructor runs when its module loads (structures/class_implementation.ts). A global
 * class it names -- CREATE OBJECT ... TYPE x, NEW x( ), x=>m( ) -- must exist by then, so its
 * module is required (requires.ts). A requirement that would close an import cycle is not taken:
 * two constructors naming each other's classes, a constructor creating its own subclass (which
 * requires it as its superclass), a ring. Every module of such a cycle would await another and
 * none would settle. The pool's class constructors then wait instead: they are queued at the
 * end of the pool's module and run at the end of the module of the class they named, once
 * every class they name is registered.
 *
 * The decision is made over the whole registry, in the order of the class names, against the
 * load-time edges the modules really have: superclass, CX_ROOT, the global superclasses of
 * local classes, and the constructor requirements already taken. The graph stays acyclic. */
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
  private readonly waiting = new Map<string, string[]>();
  private readonly releasing = new Set<string>();

  private constructor(reg: abaplint.IRegistry) {
    const classes = new Map<string, abaplint.ABAPObject>();
    for (const obj of reg.getObjects()) {
      if (obj instanceof abaplint.Objects.Class) {
        classes.set(obj.getName().toUpperCase(), obj);
      }
    }
    const edges = new Map<string, Set<string>>();
    const edge = (from: string, to: string) => {
      if (from !== to && classes.has(to)) {
        let set = edges.get(from);
        if (set === undefined) {
          set = new Set();
          edges.set(from, set);
        }
        set.add(to);
      }
    };
    for (const [name, obj] of classes) {
      const clas = obj as abaplint.Objects.Class;
      const sup = clas.getDefinition()?.getSuperClass();
      if (sup) {
        edge(name, sup.toUpperCase());
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
    for (const name of [...classes.keys()].sort()) {
      for (const target of constructorTargets(classes.get(name)!).filter(t => classes.has(t)).sort()) {
        if (target === name) {
          continue;
        }
        if (reaches(target, name)) {
          this.waiting.set(name, [...(this.waiting.get(name) ?? []), target]);
          this.releasing.add(target);
        } else {
          edge(name, target);
          this.required.set(name, [...(this.required.get(name) ?? []), target]);
        }
      }
    }
  }

  /** the classes whose modules a pool's module requires for its class constructors */
  public requiredBy(name: string): readonly string[] {
    return this.required.get(name.toUpperCase()) ?? [];
  }

  /** the classes a pool's class constructors wait for, instead of requiring them */
  public waitingFor(name: string): readonly string[] {
    return this.waiting.get(name.toUpperCase()) ?? [];
  }

  /** whether waiting class constructors are released at the end of this class's module */
  public releases(name: string): boolean {
    return this.releasing.has(name.toUpperCase());
  }
}

/** the global classes the CLASS_CONSTRUCTOR implementations of a class pool name by class name */
function constructorTargets(obj: abaplint.ABAPObject): string[] {
  const ret = new Set<string>();
  for (const file of obj.getABAPFiles()) {
    if (file.getFilename().endsWith(".testclasses.abap")) {
      continue;
    }
    let inside = false;
    for (const statement of file.getStatements()) {
      const s = statement.get();
      if (s instanceof abaplint.Statements.MethodImplementation) {
        inside = statement.findDirectExpression(abaplint.Expressions.MethodName)?.concatTokens().toUpperCase() === "CLASS_CONSTRUCTOR";
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
  return [...ret];
}

/** the queue the waiting class constructors go into, and its release: plain generated code,
 * so a module needs nothing beyond the runtime it already has */
export const CCTOR_QUEUE = "globalThis.__abapWaitingClassConstructors";

export function waitingConstructors(needs: readonly string[], calls: string): string {
  return `(${CCTOR_QUEUE} ??= []).push({needs: [${needs.map(n => JSON.stringify(n)).join(", ")}], run: async () => {\n${calls}}});\n`;
}

export function releaseConstructors(): string {
  return `for (const waiting of [...(${CCTOR_QUEUE} ?? [])]) {\n`
    + `if (waiting.needs.every(n => abap.Classes[n] !== undefined)) {\n`
    + `${CCTOR_QUEUE}.splice(${CCTOR_QUEUE}.indexOf(waiting), 1);\n`
    + `await waiting.run();\n`
    + `}\n`
    + `}\n`;
}

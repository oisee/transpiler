import {ABAP} from "..";
import {FieldSymbol} from "../types";

declare const abap: ABAP;

// EXPORT ... TO SHARED MEMORY / SHARED BUFFER and the IMPORT that reads it
// back. On a system both media live in the shared memory of the application
// server: a data cluster is stored under (dbtab, area, id) and, for a client
// dependent dbtab, the client; every session of the server sees it. Here the
// server is the process, so the clusters live in a store of this module, one
// per medium (a cluster put into SHARED MEMORY is not found in SHARED BUFFER).
//
// What is stored is a copy of each parameter's value at the time of the
// EXPORT. A later EXPORT with the same key replaces the whole cluster. An
// IMPORT of a key that holds no cluster sets sy-subrc 4 and leaves its targets
// alone; otherwise sy-subrc is 0 and every parameter the cluster holds is
// copied to its target, a parameter it does not hold leaves its target alone.
//
// Not modelled: the conversion of the ID to the length of the dbtab's key
// field (an ID longer than that field is not cut), the displacement of
// SHARED BUFFER clusters when the buffer is full, and the refusal of
// references inside an exported value.

export type DataClusterMedium = "SHARED MEMORY" | "SHARED BUFFER";

export interface IExportClusterOptions {
  medium: DataClusterMedium;
  table: string;
  area: string;
  id: any;
  client?: any;
  parameters: {[name: string]: any};
}

export interface IImportClusterOptions {
  medium: DataClusterMedium;
  table: string;
  area: string;
  id: any;
  client?: any;
  parameters: {[name: string]: any};
}

const stores: {[medium: string]: Map<string, Map<string, any>>} = {};

function storeOf(medium: string): Map<string, Map<string, any>> {
  if (stores[medium] === undefined) {
    stores[medium] = new Map();
  }
  return stores[medium];
}

function plain(value: any): string {
  if (value === undefined) {
    return "";
  }
  if (value instanceof FieldSymbol) {
    return plain(value.getPointer());
  }
  if (typeof value === "string" || typeof value === "number") {
    return String(value);
  }
  return String(value.get());
}

function keyOf(options: IExportClusterOptions | IImportClusterOptions): string {
  const client = options.client === undefined ? abap.builtin.sy.get().mandt.get() : plain(options.client);
  return [
    options.table.toUpperCase(),
    client,
    options.area.toUpperCase(),
    plain(options.id).replace(/ +$/, ""),
  ].join("\n");
}

function copyOf(value: any): any {
  if (value instanceof FieldSymbol) {
    return copyOf(value.getPointer());
  }
  if (typeof value === "string" || typeof value === "number") {
    return value;
  }
  return value.clone();
}

export function exportCluster(options: IExportClusterOptions): void {
  const cluster = new Map<string, any>();
  for (const name of Object.keys(options.parameters)) {
    cluster.set(name.toUpperCase(), copyOf(options.parameters[name]));
  }
  storeOf(options.medium).set(keyOf(options), cluster);
}

export function importCluster(options: IImportClusterOptions): void {
  const cluster = storeOf(options.medium).get(keyOf(options));
  if (cluster === undefined) {
    abap.builtin.sy.get().subrc.set(4);
    return;
  }
  for (const name of Object.keys(options.parameters)) {
    const stored = cluster.get(name.toUpperCase());
    if (stored === undefined) {
      continue;
    }
    // the stored copy stays in the store: the target gets a copy of it, so
    // a change to the target does not reach the next IMPORT
    options.parameters[name].set(copyOf(stored));
  }
  abap.builtin.sy.get().subrc.set(0);
}

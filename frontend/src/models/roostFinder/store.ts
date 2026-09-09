import { useSyncExternalStore } from 'react';

export interface RoostFinderInputs {
  detectors: string;
  master: string;
  sunset: string | null;
}

type Listener = () => void;

let current: RoostFinderInputs | null = null;
const listeners = new Set<Listener>();

export function setRoostFinderInputs(inputs: RoostFinderInputs | null): void {
  current = inputs;
  for (const l of listeners) l();
}

export function getRoostFinderInputs(): RoostFinderInputs | null {
  return current;
}

function subscribe(listener: Listener): () => void {
  listeners.add(listener);
  return () => listeners.delete(listener);
}

export function useRoostFinderInputs(): RoostFinderInputs | null {
  return useSyncExternalStore(subscribe, getRoostFinderInputs, getRoostFinderInputs);
}

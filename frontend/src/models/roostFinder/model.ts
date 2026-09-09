import type { ModelDef, ModelParams } from '@gsbio/engine';
import type { RoostFinderWasmParams } from '../../wasm/roostCompute';

export const ROOST_FINDER_MODEL_ID = 'roost-finder';

export const roostFinderModel: ModelDef = {
  id: ROOST_FINDER_MODEL_ID,
  name: 'Roost Finder',
  description: 'Estimates a bat roost location from per-detector call data.',
  params: [
    { key: 'diffusivity', label: 'Diffusivity (m²/s)', type: 'number', min: 0.1, step: 0.1, default: 81.7 },
    { key: 'capture_radius', label: 'Capture radius (m)', type: 'number', min: 1, step: 1, default: 15 },
    { key: 'grid_size', label: 'Grid size', type: 'number', min: 2, step: 1, default: 500 },
    { key: 't0', label: 't0 (s)', type: 'number', min: 0.0001, step: 0.01, default: 0.01 },
    { key: 't1', label: 't1 (s)', type: 'number', min: 1, step: 1, default: 5400 },
    { key: 'minutes_after_sunset', label: 'Minutes after sunset', type: 'number', min: 1, step: 1, default: 90 },
    { key: 'loss', label: 'Loss metric', type: 'range', min: 0, max: 1, step: 1, default: 0 },
    { key: 'per_night', label: 'Per-night counts', type: 'range', min: 0, max: 1, step: 1, default: 1 },
  ],
};

export function roostParamsToArgs(p: ModelParams): RoostFinderWasmParams {
  return {
    minutesAfterSunset: p.minutes_after_sunset ?? 90,
    perNight: (p.per_night ?? 1) === 1,
    gridSize: Math.round(p.grid_size ?? 500),
    captureRadius: p.capture_radius ?? 15,
    diffusivity: p.diffusivity ?? 81.7,
    t0: p.t0 ?? 0.01,
    t1: p.t1 ?? 5400,
    loss: (p.loss ?? 0) === 0 ? 'l2' : 'l1',
  };
}

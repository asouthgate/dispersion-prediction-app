import { roostFinderComputeAsync } from '../../wasm-connectivity/lib/wasm.js';

export interface RoostFinderWasmParams {
  gridSize: number;
  diffusivity: number;
  t0: number;
  t1: number;
}

export interface RoostDetectorPoint {
  x: number;
  y: number;
  count: number;
}

export interface RoostFinderWasmResult {
  x: number;
  y: number;
  loss: number;
  surface_png_base64: string;
  weighted_mean_x: number;
  weighted_mean_y: number;
  detectors: RoostDetectorPoint[];
  warnings: string[];
}

export async function computeRoostFinder(
  detectorsCsv: string,
  masterCsv: string,
  sunsetCsv: string,
  params: RoostFinderWasmParams,
): Promise<RoostFinderWasmResult> {
  const json = await roostFinderComputeAsync(
    detectorsCsv,
    masterCsv,
    sunsetCsv,
    params.gridSize,
    params.diffusivity,
    params.t0,
    params.t1,
  );
  const parsed = JSON.parse(json) as RoostFinderWasmResult & { error?: string };
  if (parsed.error) throw new Error(`Roost finder error: ${parsed.error}`);
  return parsed;
}

export function base64ToBlobUrl(b64: string, type: string): string {
  const binary = atob(b64);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
  return URL.createObjectURL(new Blob([bytes], { type }));
}

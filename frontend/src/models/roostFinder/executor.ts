import type { Executor, ResultLayerEntry, SimulationEngine } from '@gsbio/engine';
import { computeRoostFinder, base64ToBlobUrl, type RoostFinderWasmResult } from '../../wasm/roostCompute';
import {
  roostFinderModel,
  roostParamsToArgs,
  ROOST_SURFACE_LAYER_ID,
  ROOST_MARKERS_LAYER_ID,
  ROOST_INPUTS_SOURCE_ID,
  type RoostFinderInputs,
} from './model';
import { bngToWgs84LngLat } from '../../utils/projections';

export interface RoostMarkers {
  predicted: { lng: number; lat: number };
  weightedMean: { lng: number; lat: number };
  detectors: { lng: number; lat: number; count: number }[];
}

export interface RoostFinderSummary {
  predicted: { x: number; y: number };
  loss: number;
  detectorCount: number;
}

function bngBoundsToWgs84(raw: RoostFinderWasmResult): [number, number, number, number] {
  let xmin = Infinity;
  let xmax = -Infinity;
  let ymin = Infinity;
  let ymax = -Infinity;
  for (const d of raw.detectors) {
    if (d.x < xmin) xmin = d.x;
    if (d.x > xmax) xmax = d.x;
    if (d.y < ymin) ymin = d.y;
    if (d.y > ymax) ymax = d.y;
  }
  const [west, south] = bngToWgs84LngLat(xmin, ymin);
  const [east, north] = bngToWgs84LngLat(xmax, ymax);
  return [west, south, east, north];
}

function toMarkers(raw: RoostFinderWasmResult): RoostMarkers {
  const [plng, plat] = bngToWgs84LngLat(raw.x, raw.y);
  const [wlng, wlat] = bngToWgs84LngLat(raw.weighted_mean_x, raw.weighted_mean_y);
  const detectors = raw.detectors.map((d) => {
    const [lng, lat] = bngToWgs84LngLat(d.x, d.y);
    return { lng, lat, count: d.count };
  });
  return {
    predicted: { lng: plng, lat: plat },
    weightedMean: { lng: wlng, lat: wlat },
    detectors,
  };
}

function markersFeatureCollection(markers: RoostMarkers): GeoJSON.FeatureCollection {
  const features: GeoJSON.Feature[] = markers.detectors.map((d) => ({
    type: 'Feature',
    geometry: { type: 'Point', coordinates: [d.lng, d.lat] },
    properties: { kind: 'detector', count: d.count },
  }));
  features.push({
    type: 'Feature',
    geometry: { type: 'Point', coordinates: [markers.predicted.lng, markers.predicted.lat] },
    properties: { kind: 'predicted' },
  });
  features.push({
    type: 'Feature',
    geometry: { type: 'Point', coordinates: [markers.weightedMean.lng, markers.weightedMean.lat] },
    properties: { kind: 'mean' },
  });
  return { type: 'FeatureCollection', features };
}

export function createRoostFinderExecutor(): Executor {
  return {
    async preprocess(ctx) {
      const inputs = ctx.sources.find((s) => s.id === ROOST_INPUTS_SOURCE_ID)?.data as RoostFinderInputs | undefined;
      if (!inputs || !inputs.detectors || !inputs.master) {
        ctx.onLog?.('error', 'No detector/call CSVs loaded.');
        throw new Error('Import detector and call data CSVs first.');
      }
      return { payload: inputs };
    },

    async submit(ctx) {
      const inputs = ctx.payload as RoostFinderInputs;
      const args = roostParamsToArgs(ctx.params);

      ctx.onLog?.('info', 'Computing roost error surface in browser via WebAssembly…');
      const raw = await computeRoostFinder(inputs.detectors, inputs.master, inputs.sunset ?? '', args);

      for (const w of raw.warnings) ctx.onLog?.('warning', w);
      ctx.onLog?.('info', `Predicted roost (BNG): ${raw.x.toFixed(1)}, ${raw.y.toFixed(1)}  loss=${raw.loss.toExponential(4)}`);

      const bounds = bngBoundsToWgs84(raw);
      const surfaceUrl = base64ToBlobUrl(raw.surface_png_base64, 'image/png');

      const layers: ResultLayerEntry[] = [
        { id: ROOST_SURFACE_LAYER_ID, name: 'Roost surface', envelope: { kind: 'image', url: surfaceUrl, bounds } },
        {
          id: ROOST_MARKERS_LAYER_ID,
          name: 'Roost markers',
          envelope: {
            kind: 'geojson',
            data: markersFeatureCollection(toMarkers(raw)),
            styleProperty: 'kind',
            circleStyles: [
              { value: 'detector', color: '#111111', radius: 4, strokeColor: '#ffffff', strokeWidth: 1 },
              { value: 'predicted', color: '#e11d48', radius: 8 },
              { value: 'mean', color: '#2563eb', radius: 7 },
            ],
          },
        },
      ];

      const summary: RoostFinderSummary = {
        predicted: { x: raw.x, y: raw.y },
        loss: raw.loss,
        detectorCount: raw.detectors.length,
      };

      return { layers, summary };
    },
  };
}

export function installRoostFinder(engine: SimulationEngine): void {
  engine.registerModel(roostFinderModel);
  engine.registerExecutor(roostFinderModel.id, createRoostFinderExecutor());
}

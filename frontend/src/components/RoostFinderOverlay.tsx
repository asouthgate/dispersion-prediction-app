import { useEffect, useRef } from 'react';
import { useEngine, useResults, type TerraDraw2DRenderer } from '@gsbio/engine';
import {
  ROOST_FINDER_MODEL_ID,
  ROOST_SURFACE_LAYER_ID,
  type RoostMarkers,
} from '../models/roostFinder';

const MARKERS_SOURCE = 'roost-finder-markers';
const DETECTOR_LAYER = 'roost-finder-detectors';
const PREDICTED_LAYER = 'roost-finder-predicted';
const MEAN_LAYER = 'roost-finder-mean';

function beforeId(map: maplibregl.Map): string | undefined {
  return map.getStyle().layers.find((l) => l.id.startsWith('td-'))?.id;
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

function removeMarkers(map: maplibregl.Map) {
  for (const id of [DETECTOR_LAYER, PREDICTED_LAYER, MEAN_LAYER]) {
    try { map.removeLayer(id); } catch { /* not present */ }
  }
  try { map.removeSource(MARKERS_SOURCE); } catch { /* not present */ }
}

export function RoostFinderOverlay({ renderer }: { renderer: TerraDraw2DRenderer }) {
  const engine = useEngine();
  const { summaries } = useResults();
  const lastKey = useRef<string | null>(null);

  const runSummary = summaries.find(
    (s) =>
      s.modelId === ROOST_FINDER_MODEL_ID &&
      s.status === 'succeeded' &&
      s.visibleLayerIds.includes(ROOST_SURFACE_LAYER_ID),
  );

  const summary = runSummary
    ? (engine.findRun(runSummary.runId)?.result?.summary as { markers?: RoostMarkers } | undefined)
    : undefined;
  const markers = summary?.markers;

  useEffect(() => {
    const map = renderer.getMap();
    if (!map) return;

    if (!markers) {
      if (lastKey.current !== null) {
        lastKey.current = null;
        removeMarkers(map);
      }
      return;
    }

    const key = `${markers.predicted.lng.toFixed(8)},${markers.predicted.lat.toFixed(8)}|${markers.detectors.length}`;
    if (key === lastKey.current) return;
    const existed = lastKey.current !== null;
    lastKey.current = key;

    if (existed) removeMarkers(map);

    const insertId = beforeId(map);
    map.addSource(MARKERS_SOURCE, { type: 'geojson', data: markersFeatureCollection(markers) });
    map.addLayer({ id: DETECTOR_LAYER, type: 'circle', source: MARKERS_SOURCE, filter: ['==', ['get', 'kind'], 'detector'], paint: { 'circle-radius': 4, 'circle-color': '#111111', 'circle-stroke-color': '#ffffff', 'circle-stroke-width': 1 } }, insertId);
    map.addLayer({ id: PREDICTED_LAYER, type: 'circle', source: MARKERS_SOURCE, filter: ['==', ['get', 'kind'], 'predicted'], paint: { 'circle-radius': 8, 'circle-color': '#e11d48' } }, insertId);
    map.addLayer({ id: MEAN_LAYER, type: 'circle', source: MARKERS_SOURCE, filter: ['==', ['get', 'kind'], 'mean'], paint: { 'circle-radius': 7, 'circle-color': '#2563eb' } }, insertId);
  }, [renderer, markers]);

  return null;
}

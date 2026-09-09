import { useState } from 'react';
import { useModel, useResults, RunPanel, ResultsPanel } from '@gsbio/engine';
import {
  setRoostFinderInputs,
  useRoostFinderInputs,
} from '../models/roostFinder';
import { RunLogModal } from './RunLogModal';

function FileField({
  label,
  loaded,
  onFile,
  hint,
}: {
  label: string;
  loaded: boolean;
  onFile: (text: string) => void;
  hint?: string;
}) {
  const handleFile = (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (!file) return;
    const reader = new FileReader();
    reader.onload = () => onFile(reader.result as string);
    reader.readAsText(file);
  };

  return (
    <div className="roost-file-field">
      <span className="field-label">{label}</span>
      <input type="file" accept=".csv" onChange={handleFile} />
      {hint && <span className="hint">{hint}</span>}
      {loaded && <span className="roost-file-loaded">Loaded</span>}
    </div>
  );
}

export function RoostFinderImport() {
  const inputs = useRoostFinderInputs();
  const [detectors, setDetectors] = useState<string | null>(inputs?.detectors ?? null);
  const [master, setMaster] = useState<string | null>(inputs?.master ?? null);
  const [sunset, setSunset] = useState<string | null>(inputs?.sunset ?? null);

  const sync = (d: string | null, m: string | null, s: string | null) => {
    setDetectors(d);
    setMaster(m);
    setSunset(s);
    setRoostFinderInputs(d != null && m != null ? { detectors: d, master: m, sunset: s } : null);
  };

  return (
    <div className="panel-section">
      <p className="hint">
        Import detector and call data CSVs. Coordinates are British National Grid.
        Required columns must match the roost-finder contract (unrecognised columns
        are ignored).
      </p>
      <FileField
        label="Detectors CSV"
        loaded={detectors != null}
        onFile={(t) => sync(t, master, sunset)}
        hint="detector, x, y, n_active_days"
      />
      <FileField
        label="Master call data CSV"
        loaded={master != null}
        onFile={(t) => sync(detectors, t, sunset)}
        hint="detector, date, time"
      />
      <FileField
        label="Sunset CSV (optional)"
        loaded={sunset != null}
        onFile={(t) => sync(detectors, master, t)}
        hint="date, sunset_time (HH:MM:SS) — enables temporal filtering"
      />
      <p className="hint">
        {detectors != null && master != null
          ? 'Detector and call data ready to run.'
          : 'Load detectors + master CSVs to enable the run.'}
      </p>
    </div>
  );
}

export function RoostFinderParams() {
  const { state, setModelParam } = useModel();
  const p = state.params;

  const num = (key: string, fallback: number) => (typeof p[key] === 'number' ? p[key] : fallback);

  return (
    <div className="panel-section">
      <label className="field">
        <span className="field-label">Diffusivity (m²/s)</span>
        <input type="number" step={0.1} min={0.1} value={num('diffusivity', 81.7)} onChange={(e) => setModelParam('diffusivity', Number(e.target.value))} />
      </label>
      <label className="field">
        <span className="field-label">Capture radius (m)</span>
        <input type="number" step={1} min={1} value={num('capture_radius', 15)} onChange={(e) => setModelParam('capture_radius', Number(e.target.value))} />
      </label>
      <label className="field">
        <span className="field-label">Grid size</span>
        <input type="number" step={1} min={2} value={num('grid_size', 500)} onChange={(e) => setModelParam('grid_size', Number(e.target.value))} />
      </label>
      <label className="field">
        <span className="field-label">t0 (seconds)</span>
        <input type="number" step={0.01} min={0.0001} value={num('t0', 0.01)} onChange={(e) => setModelParam('t0', Number(e.target.value))} />
      </label>
      <label className="field">
        <span className="field-label">t1 (seconds)</span>
        <input type="number" step={1} min={1} value={num('t1', 5400)} onChange={(e) => setModelParam('t1', Number(e.target.value))} />
      </label>
      <label className="field">
        <span className="field-label">Minutes after sunset</span>
        <input type="number" step={1} min={1} value={num('minutes_after_sunset', 90)} onChange={(e) => setModelParam('minutes_after_sunset', Number(e.target.value))} />
      </label>
      <label className="field">
        <span className="field-label">Loss metric</span>
        <select value={num('loss', 0)} onChange={(e) => setModelParam('loss', Number(e.target.value))}>
          <option value={0}>l2 (squared error)</option>
          <option value={1}>l1 (absolute error)</option>
        </select>
      </label>
      <label className="field roost-field-inline">
        <input type="checkbox" checked={num('per_night', 1) === 1} onChange={(e) => setModelParam('per_night', e.target.checked ? 1 : 0)} />
        <span className="field-label">Per-night counts (divide by active nights)</span>
      </label>
    </div>
  );
}

export function RoostFinderRun() {
  const { summaries } = useResults();
  const inputs = useRoostFinderInputs();
  const [logRunId, setLogRunId] = useState<string | null>(null);
  const logRun = logRunId ? summaries.find((s) => s.runId === logRunId) ?? null : null;

  const handleViewLog = (runId: string) => setLogRunId(runId);

  return (
    <div className="generate-actions">
      {!inputs && <p className="hint">Import detector and call data CSVs first.</p>}
      <RunPanel />
      <hr className="generate-divider" />
      <ResultsPanel onViewLog={handleViewLog} />
      <RunLogModal run={logRun} onClose={() => setLogRunId(null)} />
    </div>
  );
}

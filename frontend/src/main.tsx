import { StrictMode, useState, useRef, useEffect } from 'react';
import { createRoot } from 'react-dom/client';
import { createEngine, EngineProvider } from '@gsbio/engine';
import { App } from './App';
import { installHorseshoeBat, horseshoeBatModel } from './models/horseshoeBat';
import type { PipelineStage } from './models/horseshoeBat';
import { installRoostFinder, ROOST_FINDER_MODEL_ID, ROOST_SURFACE_LAYER_ID } from './models/roostFinder';
import type { PanelTab } from './components/SidePanel';
import { acquireToken } from './auth';
import { trackPageview } from './analytics';
import './styles/index.css';

export function AppRoot() {
  const [stage, setStage] = useState<PipelineStage>('coverage');
  const [activeTab, setActiveTab] = useState<PanelTab>('connectivity');
  const stageRef = useRef(stage);
  stageRef.current = stage;
  // const [token, setToken] = useState<string | null>(null);

  useEffect(() => {
    acquireToken().then(t => {
      // setToken(t);
      trackPageview(t);
    }).catch(console.error);
  }, []);

  const [engine] = useState(() => {
    const e = createEngine();
    e.autoShowResults = true;
    installHorseshoeBat(e, () => stageRef.current);
    installRoostFinder(e);
    return e;
  });

  useEffect(() => {
    const stageMap: Record<PipelineStage, string | null> = {
      current: 'log_current',
      resistance: 'log_total_res',
      coverage: null,
    };
    if (activeTab === 'roost') {
      engine.setModel(ROOST_FINDER_MODEL_ID);
      engine.defaultLayerId = ROOST_SURFACE_LAYER_ID;
    } else {
      engine.setModel(horseshoeBatModel.id);
      engine.defaultLayerId = stageMap[stage] ?? null;
    }
  }, [activeTab, stage, engine]);

  // if (!token) return null;

  return (
    <EngineProvider engine={engine}>
      <App
        stage={stage}
        onStageChange={setStage}
        activeTab={activeTab}
        onTabChange={setActiveTab}
      />
    </EngineProvider>
  );
}

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <AppRoot />
  </StrictMode>,
);

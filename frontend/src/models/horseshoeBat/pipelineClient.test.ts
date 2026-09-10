import { describe, it, expect, vi, beforeEach } from 'vitest';

vi.mock('../../auth', () => ({
  fetchWithAuth: vi.fn(),
}));

import { createPipelineAdapter } from './pipelineClient';
import { fetchWithAuth } from '../../auth';

const mockFetch = fetchWithAuth as unknown as ReturnType<typeof vi.fn>;

function jsonResponse(body: unknown, ok = true, status = 200) {
  return Promise.resolve({
    ok,
    status,
    json: () => Promise.resolve(body),
  } as Response);
}

const COMPLETED_JOB = {
  job_id: 'job-1',
  status: 'completed',
  progress: 1,
  progress_label: 'Done',
  error: null,
  warnings: [],
  layers: [{ id: 'road_res', name: 'Road Resistance', url: '/api/rasters/job-1/road_res.png', bounds: [0, 0, 1, 1] }],
};

describe('createPipelineAdapter', () => {
  beforeEach(() => {
    mockFetch.mockReset();
  });

  it('starts a job at the correct stage endpoint and returns its id', async () => {
    mockFetch.mockImplementation(() => jsonResponse({ job_id: 'job-1' }));

    const adapter = createPipelineAdapter('coverage');
    const id = await adapter.start({ roost: {}, params: {} }, new AbortController().signal);

    expect(id).toBe('job-1');
    const [url, init] = mockFetch.mock.calls[0] as [string, RequestInit];
    expect(url.endsWith('/pipeline/coverage')).toBe(true);
    expect(init.method).toBe('POST');
    expect(JSON.parse(String(init.body))).toEqual({ roost: {}, params: {} });
  });

  it('throws with the server detail when start fails', async () => {
    mockFetch.mockImplementation(() => jsonResponse({ detail: 'Server busy' }, false, 429));
    const adapter = createPipelineAdapter('resistance');
    await expect(adapter.start({}, new AbortController().signal)).rejects.toThrow('Failed to start pipeline: Server busy');
  });

  it('fetches status and maps log lines to leveled entries', async () => {
    mockFetch.mockImplementation((url: string) => {
      if (url.includes('/logs')) {
        return jsonResponse({ lines: ['step one', 'stderr:careful'], offset: 2, has_more: false });
      }
      return jsonResponse(COMPLETED_JOB);
    });

    const adapter = createPipelineAdapter('current');
    const poll = await adapter.fetch('job-1', new AbortController().signal, 0);

    expect(poll.status).toBe('completed');
    expect(poll.progressLabel).toBe('Done');
    expect(poll.result).toEqual(COMPLETED_JOB);
    expect(poll.logs).toEqual({
      entries: [
        { level: 'info', message: 'step one' },
        { level: 'warning', message: 'stderr:careful' },
      ],
      offset: 2,
    });
  });

  it('requests logs from the given offset', async () => {
    mockFetch.mockImplementation((url: string) => {
      if (url.includes('/logs')) {
        expect(url).toContain('offset=5');
        return jsonResponse({ lines: [], offset: 5, has_more: false });
      }
      return jsonResponse(COMPLETED_JOB);
    });

    const adapter = createPipelineAdapter('resistance');
    await adapter.fetch('job-1', new AbortController().signal, 5);
  });

  it('cancels by deleting the job', async () => {
    mockFetch.mockImplementation(() => jsonResponse({}));
    const adapter = createPipelineAdapter('coverage');
    await adapter.cancel('job-1');
    const [url, init] = mockFetch.mock.calls[0] as [string, RequestInit];
    expect(url.endsWith('/pipeline/job-1')).toBe(true);
    expect(init.method).toBe('DELETE');
  });
});

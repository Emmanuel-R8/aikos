// Browser app entry point for Taiko emulator
import { TaikoEmulator } from '../src/main';

let emulator: TaikoEmulator | null = null;

// DOM elements
const fileDropZone = document.getElementById('fileDropZone')!;
const fileInput = document.getElementById('fileInput') as HTMLInputElement;
const fileInfo = document.getElementById('fileInfo')!;
const displayCanvas = document.getElementById('displayCanvas') as HTMLCanvasElement;
const startBtn = document.getElementById('startBtn') as HTMLButtonElement;
const stopBtn = document.getElementById('stopBtn') as HTMLButtonElement;
const resetBtn = document.getElementById('resetBtn') as HTMLButtonElement;
const stepBtn = document.getElementById('stepBtn') as HTMLButtonElement;
const statusDisplay = document.getElementById('statusDisplay')!;
const pcValue = document.getElementById('pcValue')!;
const spValue = document.getElementById('spValue')!;
const fpValue = document.getElementById('fpValue')!;
const fpsValue = document.getElementById('fpsValue')!;
const traceEnabled = document.getElementById('traceEnabled') as HTMLInputElement;
const exportTraceBtn = document.getElementById('exportTraceBtn') as HTMLButtonElement;
const errorDisplay = document.getElementById('errorDisplay')!;

// File loading handlers
fileDropZone.addEventListener('dragover', (e) => {
  e.preventDefault();
  fileDropZone.classList.add('dragover');
});

fileDropZone.addEventListener('dragleave', () => {
  fileDropZone.classList.remove('dragover');
});

fileDropZone.addEventListener('drop', async (e) => {
  e.preventDefault();
  fileDropZone.classList.remove('dragover');

  const files = e.dataTransfer?.files;
  if (files && files.length > 0) {
    await loadFile(files[ 0 ]);
  }
});

fileInput.addEventListener('change', async (e) => {
  const files = (e.target as HTMLInputElement).files;
  if (files && files.length > 0) {
    await loadFile(files[ 0 ]);
  }
});

fileDropZone.addEventListener('click', () => {
  fileInput.click();
});

// Control button handlers
startBtn.addEventListener('click', () => {
  if (emulator) {
    emulator.start();
    updateButtonStates(true);
  }
});

stopBtn.addEventListener('click', () => {
  if (emulator) {
    emulator.stop();
    updateButtonStates(false);
  }
});

resetBtn.addEventListener('click', () => {
  if (emulator) {
    emulator.reset();
    updateButtonStates(false);
  }
});

stepBtn.addEventListener('click', () => {
  if (emulator) {
    emulator.step();
  }
});

exportTraceBtn.addEventListener('click', () => {
  if (emulator) {
    const trace = emulator.exportTrace();
    if (trace) {
      const blob = new Blob([ trace ], { type: 'text/plain' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = 'taiko_trace.txt';
      a.click();
      URL.revokeObjectURL(url);
    }
  }
});

// Load sysout file
async function loadFile(file: File) {
  try {
    showError(null);
    statusDisplay.innerHTML = '<p>Loading file...</p>';

    const arrayBuffer = await file.arrayBuffer();
    fileInfo.textContent = `File: ${file.name} (${(file.size / 1024).toFixed(2)} KB)`;

    statusDisplay.innerHTML = '<p>Initializing emulator...</p>';

    emulator = new TaikoEmulator(displayCanvas, {
      enableTracing: traceEnabled.checked
    });

    await emulator.loadSysout(arrayBuffer);

    statusDisplay.innerHTML = '<p>Emulator ready</p>';
    updateButtonStates(false);
    startUpdateLoop();

  } catch (error) {
    showError(error);
    statusDisplay.innerHTML = '<p>Error loading file</p>';
  }
}

// Update button states
function updateButtonStates(running: boolean) {
  startBtn.disabled = !emulator || running;
  stopBtn.disabled = !emulator || !running;
  resetBtn.disabled = !emulator;
  stepBtn.disabled = !emulator || running;
  exportTraceBtn.disabled = !emulator || !emulator.hasTrace();
}

// Update loop for metrics
let lastFrameTime = performance.now();
let frameCount = 0;
let fps = 0;

function startUpdateLoop() {
  function update() {
    if (emulator) {
      const state = emulator.getState();
      pcValue.textContent = `0x${state.pc.toString(16)}`;
      spValue.textContent = `0x${state.sp.toString(16)}`;
      fpValue.textContent = `0x${state.fp.toString(16)}`;

      // Calculate FPS
      frameCount++;
      const now = performance.now();
      if (now - lastFrameTime >= 1000) {
        fps = frameCount;
        frameCount = 0;
        lastFrameTime = now;
      }
      fpsValue.textContent = fps.toString();

      updateButtonStates(state.running);
    }
    requestAnimationFrame(update);
  }
  requestAnimationFrame(update);
}

// Error display
function showError(error: Error | null) {
  if (error) {
    errorDisplay.style.display = 'block';
    errorDisplay.innerHTML = `
            <h3>Error</h3>
            <pre>${error.message}\n${error.stack}</pre>
        `;
  } else {
    errorDisplay.style.display = 'none';
  }
}

// Initial state
updateButtonStates(false);

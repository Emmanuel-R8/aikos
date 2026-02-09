// Main entry point for Taiko emulator
import { VM } from './vm/vm';
import { runVM, executeStep } from './vm/execution';
import { loadSysout } from './io/sysout';
import { WebGLRenderer } from './display/webgl';
import { ExecutionTrace } from './vm/trace';

/**
 * Taiko emulator class
 * Main interface for browser integration
 */
export class TaikoEmulator {
  private vm: VM;
  private canvas: HTMLCanvasElement;
  private options: EmulatorOptions;
  private traceEnabled: boolean = false;
  private traceBuffer: string[] = [];
  private renderer: WebGLRenderer | null = null;
  private tracer: ExecutionTrace | null = null;
  private animationFrameId: number | null = null;

  constructor(canvas: HTMLCanvasElement, options: EmulatorOptions = {}) {
    this.canvas = canvas;
    this.options = options;
    this.vm = new VM();
    this.traceEnabled = options.enableTracing ?? false;

    // Initialize WebGL renderer
    try {
      this.renderer = new WebGLRenderer(canvas);
    } catch (error) {
      console.error('Failed to initialize WebGL:', error);
    }

    // Initialize execution tracer
    if (this.traceEnabled) {
      this.tracer = new ExecutionTrace();
    }

    // Set max steps if provided
    if (options.maxSteps !== undefined) {
      this.vm.maxSteps = options.maxSteps;
      this.vm.stepCapActive = true;
    }
  }

  /**
   * Load sysout file
   */
  async loadSysout(arrayBuffer: ArrayBuffer): Promise<void> {
    const result = await loadSysout(arrayBuffer);
    this.vm.initializeMemory(
      result.virtualMemory,
      result.fptovpTable,
      result.atomSpaceOffset,
      result.defSpaceOffset,
      result.valSpaceOffset
    );
    this.vm.pc = result.initialPC;
    this.vm.stackPtr = result.initialSP;
    this.vm.cstkptrl = result.initialSP;

    // Initialize display region from IFPAGE
    // TODO: Get actual display address from IFPAGE or sysout
    // For now, use placeholder
    const displayWidth = result.ifpage.screenwidth || 1024;
    const displayHeight = 768; // Placeholder
    const displayAddr = 0x200000; // Placeholder - actual address from sysout
    this.vm.initializeDisplay(displayAddr, displayWidth, displayHeight);
  }

  /**
   * Start execution
   */
  start(): void {
    if (this.vm.virtualMemory === null) {
      throw new Error('Sysout not loaded');
    }

    // Start execution loop
    this.runExecutionLoop();
  }

  /**
   * Execution loop with display updates
   */
  private runExecutionLoop(): void {
    const executeFrame = () => {
      if (this.vm.stopRequested || !this.vm.running) {
        this.animationFrameId = null;
        return;
      }

      // Execute a batch of instructions
      const batchSize = 1000; // Execute 1000 instructions per frame
      for (let i = 0; i < batchSize && !this.vm.stopRequested; i++) {
        if (!executeStep(this.vm)) {
          break;
        }
      }

      // Update display
      this.updateDisplay();

      // Continue loop
      this.animationFrameId = requestAnimationFrame(executeFrame);
    };

    this.vm.running = true;
    this.vm.stopRequested = false;
    this.animationFrameId = requestAnimationFrame(executeFrame);
  }

  /**
   * Update display from VM display region
   */
  private updateDisplay(): void {
    if (!this.renderer || !this.vm.displayRegion) return;

    const displayRegion = this.vm.displayRegion;
    const width = this.vm.displayWidth;
    const height = this.vm.displayHeight;

    if (width > 0 && height > 0) {
      this.renderer.updateDisplay(displayRegion, width, height);
      this.renderer.render();
    }
  }

  /**
   * Stop execution
   */
  stop(): void {
    this.vm.stopRequested = true;
    this.vm.running = false;
    if (this.animationFrameId !== null) {
      cancelAnimationFrame(this.animationFrameId);
      this.animationFrameId = null;
    }
  }

  /**
   * Reset VM state
   */
  reset(): void {
    this.vm.reset();
  }

  /**
   * Execute single step
   */
  step(): void {
    if (this.vm.virtualMemory === null) {
      throw new Error('Sysout not loaded');
    }
    executeStep(this.vm);
  }

  /**
   * Get VM state
   */
  getState(): VMState {
    return {
      pc: this.vm.pc,
      sp: this.vm.getStackPtrOffset(),
      fp: this.vm.getFramePtrOffset(),
      running: this.vm.running,
    };
  }

  /**
   * Check if trace is available
   */
  hasTrace(): boolean {
    return this.tracer !== null && this.tracer.export().length > 0;
  }

  /**
   * Export trace
   */
  exportTrace(): string {
    if (this.tracer) {
      return this.tracer.export();
    }
    return '';
  }
}

export interface EmulatorOptions {
  enableTracing?: boolean;
  maxSteps?: number;
}

export interface VMState {
  pc: number;
  sp: number;
  fp: number;
  running: boolean;
}

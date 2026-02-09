// CLI runner for Taiko emulator (headless execution for parity testing)
import { VM } from './vm/vm';
import { runVM, executeStep } from './vm/execution';
import { loadSysout } from './io/sysout';
import { ExecutionTrace } from './vm/trace';
import { decodeInstructionFromMemory } from './vm/dispatch/decoder';
import { initializeVM } from './vm/initialization';
import * as fs from 'fs';

/**
 * Run Taiko emulator from command line
 * Usage: bun run src/cli.ts <sysout_file> [max_steps]
 */
async function main() {
    const args = process.argv.slice(2);

    if (args.length === 0) {
        console.error('Usage: bun run src/cli.ts <sysout_file> [max_steps]');
        process.exit(1);
    }

    const sysoutPath = args[0];
    const maxSteps = args[1] ? parseInt(args[1], 10) : null;

    console.error(`Loading sysout: ${sysoutPath}`);

    // Read sysout file
    let sysoutBuffer: ArrayBuffer;
    try {
        const fileBuffer = fs.readFileSync(sysoutPath);
        sysoutBuffer = fileBuffer.buffer.slice(fileBuffer.byteOffset, fileBuffer.byteOffset + fileBuffer.byteLength);
    } catch (error) {
        console.error(`Failed to read sysout file: ${error}`);
        process.exit(1);
    }

    // Load sysout
    let loadResult;
    try {
        loadResult = await loadSysout(sysoutBuffer);
    } catch (error) {
        console.error(`Failed to load sysout: ${error}`);
        process.exit(1);
    }

    console.error(`Loaded sysout: ${loadResult.virtualMemory.length} bytes virtual memory`);
    console.error(`Initial PC: 0x${loadResult.initialPC.toString(16)}`);
    console.error(`Initial SP: 0x${loadResult.initialSP.toString(16)}`);

    // Initialize VM
    const vm = new VM();
    vm.initializeMemory(
        loadResult.virtualMemory,
        loadResult.fptovpTable,
        loadResult.atomSpaceOffset,
        loadResult.defSpaceOffset,
        loadResult.valSpaceOffset,
        loadResult.plistSpaceOffset,
        loadResult.dtdOffset
    );

    // Initialize VM state (similar to C start_lisp())
    // This sets up stack, frame, PC, IVar, FuncObj from the sysout state
    console.error('Calling initializeVM...');
    const initSuccess = initializeVM(vm, loadResult.ifpage);
    console.error(`initializeVM result: ${initSuccess}`);
    if (!initSuccess) {
        // Fallback: use initial values from sysout loading
        // This happens when the stack is sparse (not loaded from sysout)
        vm.pc = loadResult.initialPC;
        vm.stackPtr = loadResult.initialSP;
        vm.cstkptrl = loadResult.initialSP;
        console.error('VM initialization failed (sparse stack?), using fallback values');
    } else {
        console.error(`VM initialized successfully: PC=0x${vm.pc.toString(16)}, SP=0x${vm.stackPtr.toString(16)}, IVar=${vm.ivar !== null ? `0x${vm.ivar.toString(16)}` : 'null'}, FuncObj=${vm.funcObj !== null ? `0x${vm.funcObj.toString(16)}` : 'null'}`);
    }

    // Set max steps if provided
    if (maxSteps !== null) {
        vm.maxSteps = maxSteps;
        vm.stepCapActive = true;
        console.error(`Max steps: ${maxSteps}`);
    }

    // Initialize tracing
    const tracer = new ExecutionTrace();

    console.error('Starting execution...');

    // Execute with tracing
    let stepCount = 0;
    while (!vm.stopRequested) {
        if (vm.virtualMemory === null) break;

        // Decode instruction
        const instruction = decodeInstructionFromMemory(vm, vm.pc);
        if (instruction === null) {
            console.error(`Failed to decode instruction at PC=0x${vm.pc.toString(16)}`);
            break;
        }

        // Execute step (tracer is passed to executeStep)
        const continueExecution = executeStep(vm, tracer);
        if (!continueExecution) {
            break;
        }

        stepCount++;

        // Progress indicator
        if (stepCount % 1000 === 0) {
            console.error(`Executed ${stepCount} steps, PC=0x${vm.pc.toString(16)}`);
        }
    }

    console.error(`Execution complete: ${stepCount} steps`);
    console.error(`Final PC: 0x${vm.pc.toString(16)}`);

    // Export trace
    const traceOutput = tracer.export();
    console.log(traceOutput);
}

main().catch((error) => {
    console.error('Fatal error:', error);
    process.exit(1);
});

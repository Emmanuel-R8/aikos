// Execution tracing (unified format)
// Matches C/Zig trace format for parity testing

import type { VM } from './vm';
import type { Instruction } from './dispatch/opcode';
import { Opcode } from './dispatch/opcode';

/**
 * Unified trace format logger
 * Format: LINE#|PC|INSTRUCTION|OPCODE|OPERANDS|REGISTERS|FLAGS|SP_FP|STACK_SUMMARY|MEMORY_CONTEXT|FP_VP_FO_VA|BS_MEM|NOTES
 */
export class ExecutionTrace {
    private lines: string[] = [];
    private lineNumber: number = 0;

    /**
     * Log instruction execution
     */
    logInstruction(vm: VM, instruction: Instruction, isLastStep: boolean = false): void {
        const line = this.formatTraceLine(vm, instruction, isLastStep);
        this.lines.push(line);
        this.lineNumber++;
    }

    /**
     * Format trace line
     */
    private formatTraceLine(vm: VM, instruction: Instruction, isLastStep: boolean): string {
        const parts: string[] = [];

        // LINE#
        parts.push(this.lineNumber.toString());

        // PC
        parts.push(`0x${vm.pc.toString(16)}`);

        // INSTRUCTION (opcode name)
        parts.push(Opcode[instruction.opcode] || `0x${instruction.opcode.toString(16)}`);

        // OPCODE (hex)
        parts.push(`0x${instruction.opcode.toString(16)}`);

        // OPERANDS (hex bytes)
        const operandsHex = Array.from(instruction.operands)
            .map(b => `0x${b.toString(16).padStart(2, '0')}`)
            .join(',');
        parts.push(operandsHex || '-');

        // REGISTERS (PC, SP, FP, TOS)
        const registers = `PC=0x${vm.pc.toString(16)} SP=0x${vm.getStackPtrOffset().toString(16)} FP=0x${vm.getFramePtrOffset().toString(16)} TOS=0x${vm.topOfStack.toString(16)}`;
        parts.push(registers);

        // FLAGS
        parts.push('-'); // TODO: Implement flags

        // SP_FP
        parts.push(`SP=0x${vm.getStackPtrOffset().toString(16)} FP=0x${vm.getFramePtrOffset().toString(16)}`);

        // STACK_SUMMARY
        parts.push(`TOS=0x${vm.topOfStack.toString(16)}`);

        // MEMORY_CONTEXT
        parts.push('-'); // TODO: Implement memory context

        // FP_VP_FO_VA
        parts.push('-'); // TODO: Implement FPtoVP info

        // BS_MEM
        parts.push('-'); // TODO: Implement byte-swap memory info

        // NOTES
        parts.push('-');

        return parts.join('|');
    }

    /**
     * Export trace
     */
    export(): string {
        return this.lines.join('\n');
    }

    /**
     * Clear trace
     */
    clear(): void {
        this.lines = [];
        this.lineNumber = 0;
    }
}

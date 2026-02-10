= Parity Divergences Catalog

*Date*: 2026-02-09
*Status*: Active - Continuously Updated
*Purpose*: Systematically document all identified divergences between emulator implementations

== Overview

This document catalogs all identified divergences between the C reference implementation and alternative implementations (Zig, TypeScript, Lisp). Each divergence is assigned a unique ID and tracked through resolution.

== Divergence Tracking Structure

Each divergence entry contains:

- *Divergence ID*: Unique identifier (e.g., DIV-001)
- *Affected Implementations*: Which implementations diverge from C
- *First Occurrence*: Step number and PC address where divergence first appears
- *Field(s) That Diverge*: Specific trace fields that differ
- *Root Cause Analysis*: Investigation findings
- *Fix Status*: OPEN, IN_PROGRESS, RESOLVED, DEFERRED
- *Related Files*: Source files involved
- *Date Identified*: When divergence was first detected
- *Date Resolved*: When divergence was fixed (if applicable)

== Divergence Categories

=== Critical Divergences

Divergences that prevent correct execution or cause immediate failures.

=== Memory Divergences

Divergences related to memory access, addressing, or paging.

=== Stack Divergences

Divergences related to stack pointer, frame pointer, or stack operations.

=== Opcode Divergences

Divergences in opcode implementation or execution semantics.

=== Trace Format Divergences

Divergences in trace output format (should be rare once unified format is established).

== Known Divergences

=== DIV-001: [Placeholder - First Divergence]

*Status*: OPEN
*Date Identified*: TBD
*Affected Implementations*: TBD
*First Occurrence*: Step TBD, PC TBD
*Fields*: TBD
*Root Cause*: TBD
*Related Files*: TBD
*Notes*: This is a template entry. Replace with actual divergence data.

== Divergence Statistics

*Total Divergences Identified*: 0
*Open*: 0
*In Progress*: 0
*Resolved*: 0
*Deferred*: 0

*Last Updated*: 2026-02-09

== Related Documentation

- Multi-Implementation Comparison: See comparison scripts in `scripts/`
- Trace Format Specification: `documentation/specifications/vm-core/trace-and-logging-formats.typ`
- Critical Debugging Techniques: `documentation/core/critical-debugging-technique.typ`
- Implementation Status: `documentation/implementations/zig-implementation.typ`, `documentation/implementations/lisp-implementation.typ`

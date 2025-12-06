<!--
Sync Impact Report:
Version change: [none] → 1.0.0
Modified principles: N/A (initial creation)
Added sections: Core Principles (5), Platform Requirements, Development Standards, Governance
Removed sections: N/A
Templates requiring updates:
  ✅ .specify/templates/plan-template.md - Constitution Check section aligns with principles
  ✅ .specify/templates/spec-template.md - No changes needed (generic template)
  ✅ .specify/templates/tasks-template.md - No changes needed (generic template)
Follow-up TODOs: None
-->

# Maiko Constitution

## Core Principles

### I. Platform Portability
Maiko MUST support multiple operating systems and processor architectures. The codebase MUST abstract platform-specific functionality behind consistent interfaces. Supported platforms include macOS, FreeBSD, Linux, Solaris, and Windows. Supported architectures include i386, x86_64, arm64, arm7l, and SPARC. Platform detection MUST be automatic during build. Rationale: Maiko serves a diverse user base across different systems; portability ensures broad accessibility.

### II. Build System Flexibility
Maiko MUST support both CMake and make build systems. Both build systems MUST produce equivalent binaries. Build configuration options MUST be consistent across both systems (e.g., display subsystem selection, network type). Rationale: Different developers and deployment environments prefer different build tools; supporting both maximizes developer productivity and deployment flexibility.

### III. Display Abstraction
Maiko MUST support multiple display subsystems (X11 and SDL). Display subsystem selection MUST be configurable at build time. The display interface MUST be abstracted to allow adding new subsystems without modifying core VM code. Rationale: Different platforms and use cases require different display technologies; abstraction enables flexibility while maintaining a single codebase.

### IV. Code Quality & Memory Safety
All C code MUST follow consistent coding standards (see `maiko/.clang-format`). Memory management MUST be explicit and safe; use of unsafe memory operations MUST be justified. Assembly code MUST be clearly documented with platform-specific notes. Rationale: Maiko is a virtual machine requiring high reliability; memory safety and code quality are critical for stability and maintainability.

### V. Testing & Validation
Critical VM operations MUST have test coverage. Platform-specific code paths MUST be validated on their target platforms. Build system changes MUST be tested with both CMake and make. Rationale: VM correctness is essential; comprehensive testing prevents regressions across diverse platforms and build configurations.

## Platform Requirements

Maiko MUST maintain compatibility with the Medley Interlisp byte-coded instruction set. Network subsystem support MUST be configurable (NONE, SUN_DLPI, SUN_NIT, NETHUB). Display subsystem selection MUST not affect VM core functionality. Version compatibility MUST be maintained per `maiko/inc/version.h` release numbering.

## Development Standards

Code contributions MUST follow the existing code style and structure. Platform-specific implementations MUST be isolated in clearly marked sections or files. Build scripts MUST handle platform detection automatically. Documentation MUST be updated when adding new platforms or architectures. All PRs MUST verify build success on at least one target platform.

## Governance

This constitution supersedes all other development practices. Amendments require:
- Documentation of the rationale for change
- Impact assessment on existing platforms/build systems
- Update to this constitution file with version increment
- Review and approval by maintainers

All PRs and code reviews MUST verify compliance with these principles. Complexity additions (e.g., new platforms, build systems) MUST be justified with clear use cases. Use `.specify/templates/plan-template.md` for feature planning and constitution compliance checks.

**Version**: 1.0.0 | **Ratified**: 2025-12-04 | **Last Amended**: 2025-12-04

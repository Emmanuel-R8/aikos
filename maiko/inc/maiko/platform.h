#ifndef MAIKO_PLATFORM_H
#define MAIKO_PLATFORM_H 1

/*
 * Set up various preprocessor definitions based upon
 * the platform.
 */

#if defined(__linux__) && !defined(__wsl1__)
#define MAIKO_OS_LINUX 1
#define MAIKO_OS_NAME "Linux"
#define MAIKO_OS_UNIX_LIKE 1
#define MAIKO_OS_DETECTED 1
#endif

#ifdef __EMSCRIPTEN__
#define MAIKO_OS_LINUX 1
#define MAIKO_OS_EMSCRIPTEN 1
#define MAIKO_OS_NAME "Emscripten"
#define MAIKO_EMULATE_TIMER_INTERRUPTS 1
#define MAIKO_EMULATE_ASYNC_INTERRUPTS 1
#define MAIKO_OS_UNIX_LIKE 1
#define MAIKO_OS_DETECTED
#define MAIKO_ARCH_NAME "WebAssembly"
#define MAIKO_ARCH_WORD_BITS 32
#define MAIKO_ARCH_DETECTED 1
#endif

/* __x86_64__: GNU C, __x86_64: Sun Studio, _M_AMD64: Visual Studio */
#if defined(__x86_64__) || defined(__x86_64) || defined(_M_AMD64)
#define MAIKO_ARCH_X86_64 1
#define MAIKO_ARCH_NAME "x86_64"
#define MAIKO_ARCH_WORD_BITS 64
#define MAIKO_ARCH_DETECTED 1
#endif

/* __aarch64__: GNU C */
#ifdef __aarch64__
#define MAIKO_ARCH_ARM64 1
#define MAIKO_ARCH_NAME "arm64"
#define MAIKO_ARCH_WORD_BITS 64
#define MAIKO_ARCH_DETECTED 1
#endif

/* Modern GNU C, Clang, Sun Studio  provide __BYTE_ORDER__
 * Older GNU C (ca. 4.0.1) provides __BIG_ENDIAN__/__LITTLE_ENDIAN__
 */
#if defined(__BYTE_ORDER__)
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define BYTESWAP 1
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#undef BYTESWAP
#else
#error "Unknown byte order"
#endif
#elif __BIG_ENDIAN__ == 1
#undef BYTESWAP
#elif __LITTLE_ENDIAN__ == 1
#define BYTESWAP 1
#else
#error "Could not detect byte order"
#endif

#ifndef MAIKO_OS_DETECTED
#error "Could not detect OS."
#endif

#ifndef MAIKO_ARCH_DETECTED
#error "Could not detect system architecture."
#endif

#endif /* MAIKO_PLATFORM_H */

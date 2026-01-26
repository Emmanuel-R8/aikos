// Error types for Maiko VM implementation
// Error unions provide type-safe error handling

/// VM core errors
pub const VMError = error{
    InvalidAddress,
    StackOverflow,
    StackUnderflow,
    InvalidOpcode,
    StorageFull,
    SysoutLoadFailed,
    InterruptError,
    MemoryAccessFailed,
    InvalidStackPointer,
    InvalidFramePointer,
    DivisionByZero,
    InvalidNumberType,
};

/// Memory management errors
pub const MemoryError = error{
    AllocationFailed,
    InvalidAddress,
    PageMappingFailed,
    SysoutLoadFailed,
    SysoutSaveFailed,
    StorageFull,
    GCOverflow,
};

/// Display subsystem errors
pub const DisplayError = error{
    SDLInitFailed,
    SDLWindowCreationFailed,
    SDLRendererCreationFailed,
    SDLTextureCreationFailed,
    SDLPixelFormatFailed,
    InvalidRegion,
};

/// I/O subsystem errors
pub const IOError = error{
    InvalidPathname,
    FileNotFound,
    PermissionDenied,
    ReadFailed,
    WriteFailed,
    KeycodeTranslationFailed,
};
// Browser event handling
// Translates browser keyboard/mouse events to Lisp events

/**
 * Keycode mapping from browser keycodes to Lisp keycodes
 * Based on maiko/src/sdl.c keymap array (74 entries)
 * Maps browser keycodes -> SDL keycodes -> Lisp keycodes
 *
 * The keymap is structured as pairs: [lisp_keycode, sdl_keycode]
 * We need to reverse this to: browser_keycode -> lisp_keycode
 */
const BROWSER_TO_SDL_KEYMAP: Map<number, number> = new Map([
    // Basic keys
    [8, 42],      // Backspace -> SDLK_BACKSPACE
    [9, 34],      // Tab -> SDLK_TAB
    [13, 44],     // Enter -> SDLK_RETURN
    [27, 33],     // Escape -> SDLK_ESCAPE
    [32, 57],     // Space -> SDLK_SPACE
    [33, 63],     // Page Up -> SDLK_PAGEUP
    [34, 70],     // Page Down -> SDLK_KP_3 (PGDN)
    [35, 90],     // End -> SDLK_END
    [36, 62],     // Home -> SDLK_HOME
    [37, 0],      // Left Arrow -> (mapped separately)
    [38, 0],      // Up Arrow -> (mapped separately)
    [39, 0],      // Right Arrow -> (mapped separately)
    [40, 0],      // Down Arrow -> (mapped separately)
    [45, 89],     // Insert -> SDLK_INSERT
    [46, 0],      // Delete -> (mapped separately)

    // Function keys
    [112, 0],     // F1
    [113, 0],     // F2
    [114, 0],     // F3
    [115, 67],    // F4 -> SDLK_F4
    [116, 68],    // F5 -> SDLK_F5
    [117, 66],    // F6 -> SDLK_F7 (UTIL2)
    [118, 0],     // F7
    [119, 0],     // F8
    [120, 80],    // F9 -> SDLK_F9
    [121, 61],    // F10 -> SDLK_F11
    [122, 61],    // F11 -> SDLK_F11
    [123, 91],    // F12 -> SDLK_F12

    // Number row
    [48, 8],      // 0 -> SDLK_0
    [49, 32],     // 1 -> SDLK_1
    [50, 17],     // 2 -> SDLK_2
    [51, 16],     // 3 -> SDLK_3
    [52, 1],      // 4 -> SDLK_4
    [53, 0],      // 5 -> SDLK_5
    [54, 2],      // 6 -> SDLK_6
    [55, 4],      // 7 -> SDLK_7
    [56, 53],     // 8 -> SDLK_8
    [57, 22],     // 9 -> SDLK_9

    // Letters (A-Z) - mapped to lowercase SDL codes
    [65, 21],     // A -> SDLK_a
    [66, 39],     // B -> SDLK_b
    [67, 37],     // C -> SDLK_c
    [68, 5],      // D -> SDLK_d
    [69, 3],      // E -> SDLK_e
    [70, 35],     // F -> SDLK_f
    [71, 50],     // G -> SDLK_g
    [72, 52],     // H -> SDLK_h
    [73, 23],     // I -> SDLK_i
    [74, 38],     // J -> SDLK_j
    [75, 9],      // K -> SDLK_k
    [76, 26],     // L -> SDLK_l
    [77, 55],     // M -> SDLK_m
    [78, 54],     // N -> SDLK_n
    [79, 25],     // O -> SDLK_o
    [80, 11],     // P -> SDLK_p
    [81, 19],     // Q -> SDLK_q
    [82, 48],     // R -> SDLK_r
    [83, 20],     // S -> SDLK_s
    [84, 49],     // T -> SDLK_t
    [85, 6],      // U -> SDLK_u
    [86, 7],      // V -> SDLK_v
    [87, 18],     // W -> SDLK_w
    [88, 24],     // X -> SDLK_x
    [89, 51],     // Y -> SDLK_y
    [90, 40],     // Z -> SDLK_z

    // Punctuation and symbols
    [186, 43],    // Semicolon -> SDLK_SEMICOLON
    [187, 59],    // Equals -> SDLK_EQUALS
    [188, 27],    // Comma -> SDLK_COMMA
    [189, 10],    // Minus -> SDLK_MINUS
    [190, 42],    // Period -> SDLK_PERIOD
    [191, 12],    // Slash -> SDLK_SLASH
    [192, 45],    // Backtick -> SDLK_BACKQUOTE
    [219, 58],    // Left Bracket -> SDLK_LEFTBRACKET
    [220, 0],     // Backslash -> (mapped separately)
    [221, 29],    // Right Bracket -> SDLK_RIGHTBRACKET
    [222, 28],    // Quote -> SDLK_QUOTE

    // Keypad
    [96, 98],     // KP 0 -> SDLK_KP_0
    [97, 81],     // KP 1 -> SDLK_KP_7
    [98, 82],     // KP 2 -> SDLK_KP_8
    [99, 83],     // KP 3 -> SDLK_KP_9
    [100, 84],    // KP 4 -> SDLK_KP_4
    [101, 85],    // KP 5 -> SDLK_KP_5
    [102, 87],    // KP 6 -> SDLK_KP_6
    [103, 81],    // KP 7 -> SDLK_KP_7
    [104, 82],    // KP 8 -> SDLK_KP_8
    [105, 83],    // KP 9 -> SDLK_KP_9
    [106, 95],    // KP Multiply -> SDLK_KP_MULTIPLY
    [107, 96],    // KP Plus -> SDLK_KP_PLUS (approximate)
    [109, 96],    // KP Minus -> SDLK_KP_MINUS
    [110, 13],    // KP Period -> SDLK_KP_PERIOD
    [111, 65],    // KP Divide -> SDLK_KP_DIVIDE
]);

/**
 * SDL to Lisp keycode mapping
 * Based on maiko/src/sdl.c keymap array structure
 * Maps SDL keycode -> Lisp keycode
 */
const SDL_TO_LISP_KEYMAP: Map<number, number> = new Map([
    // Create reverse mapping from SDL keymap pairs
    // [lisp_keycode, sdl_keycode] -> sdl_keycode -> lisp_keycode
    [42, 42],     // SDLK_PERIOD -> 42
    [1, 1],       // SDLK_4 -> 1
    [2, 2],       // SDLK_6 -> 2
    [3, 3],       // SDLK_e -> 3
    [4, 4],       // SDLK_7 -> 4
    [5, 5],       // SDLK_d -> 5
    [6, 6],       // SDLK_u -> 6
    [7, 7],       // SDLK_v -> 7
    [8, 8],       // SDLK_0 -> 8
    [9, 9],       // SDLK_k -> 9
    [10, 10],     // SDLK_MINUS -> 10
    [11, 11],     // SDLK_p -> 11
    [12, 12],     // SDLK_SLASH -> 12
    [13, 13],     // SDLK_KP_PERIOD -> 13
    [14, 14],     // SDLK_SCROLLLOCK -> 14
    [15, 15],     // SDLK_BACKSPACE -> 15
    [16, 16],     // SDLK_3 -> 16
    [17, 17],     // SDLK_2 -> 17
    [18, 18],     // SDLK_w -> 18
    [19, 19],     // SDLK_q -> 19
    [20, 20],     // SDLK_s -> 20
    [21, 21],     // SDLK_a -> 21
    [22, 22],     // SDLK_9 -> 22
    [23, 23],     // SDLK_i -> 23
    [24, 24],     // SDLK_x -> 24
    [25, 25],     // SDLK_o -> 25
    [26, 26],     // SDLK_l -> 26
    [27, 27],     // SDLK_COMMA -> 27
    [28, 28],     // SDLK_QUOTE -> 28
    [29, 29],     // SDLK_RIGHTBRACKET -> 29
    [31, 31],     // SDLK_LALT -> 31
    [32, 32],     // SDLK_1 -> 32
    [33, 33],     // SDLK_ESCAPE -> 33
    [34, 34],     // SDLK_TAB -> 34
    [35, 35],     // SDLK_f -> 35
    [36, 36],     // SDLK_LCTRL -> 36
    [37, 37],     // SDLK_c -> 37
    [38, 38],     // SDLK_j -> 38
    [39, 39],     // SDLK_b -> 39
    [40, 40],     // SDLK_z -> 40
    [41, 41],     // SDLK_LSHIFT -> 41
    [43, 43],     // SDLK_SEMICOLON -> 43
    [44, 44],     // SDLK_RETURN -> 44
    [45, 45],     // SDLK_BACKQUOTE -> 45
    [47, 47],     // SDLK_RCTRL -> 47
    [48, 48],     // SDLK_r -> 48
    [49, 49],     // SDLK_t -> 49
    [50, 50],     // SDLK_g -> 50
    [51, 51],     // SDLK_y -> 51
    [52, 52],     // SDLK_h -> 52
    [53, 53],     // SDLK_8 -> 53
    [54, 54],     // SDLK_n -> 54
    [55, 55],     // SDLK_m -> 55
    [56, 56],     // SDLK_CAPSLOCK -> 56
    [57, 57],     // SDLK_SPACE -> 57
    [58, 58],     // SDLK_LEFTBRACKET -> 58
    [59, 59],     // SDLK_EQUALS -> 59
    [60, 60],     // SDLK_RSHIFT -> 60
    [61, 61],     // SDLK_F11 -> 61
    [62, 62],     // SDLK_HOME -> 62
    [63, 63],     // SDLK_PAGEUP -> 63
    [64, 64],     // SDLK_KP_EQUALS -> 64
    [65, 65],     // SDLK_KP_DIVIDE -> 65
    [66, 66],     // SDLK_F7 -> 66
    [67, 67],     // SDLK_F4 -> 67
    [68, 68],     // SDLK_F5 -> 68
    [69, 69],     // SDLK_KP_2 -> 69
    [70, 70],     // SDLK_KP_3 -> 70
    [76, 76],     // SDLK_KP_ENTER -> 76
    [80, 80],     // SDLK_F9 -> 80
    [81, 81],     // SDLK_KP_7 -> 81
    [82, 82],     // SDLK_KP_8 -> 82
    [83, 83],     // SDLK_KP_9 -> 83
    [84, 84],     // SDLK_KP_4 -> 84
    [85, 85],     // SDLK_KP_5 -> 85
    [86, 86],     // SDLK_LALT -> 86
    [87, 87],     // SDLK_KP_6 -> 87
    [89, 89],     // SDLK_INSERT -> 89
    [90, 90],     // SDLK_END -> 90
    [91, 91],     // SDLK_F12 -> 91
    [92, 92],     // SDLK_PRINTSCREEN -> 92
    [93, 93],     // SDLK_MODE -> 93
    [94, 94],     // SDLK_KP_1 -> 94
    [95, 95],     // SDLK_KP_MULTIPLY -> 95
    [96, 96],     // SDLK_KP_MINUS -> 96
    [97, 97],     // SDLK_HELP -> 97
    [98, 98],     // SDLK_KP_0 -> 98
]);

/**
 * Translate browser keycode to Lisp keycode
 *
 * @param browserKeycode Browser keycode (from KeyboardEvent.keyCode)
 * @returns Lisp keycode, or browser keycode if no mapping found
 */
export function translateKeycode(browserKeycode: number): number {
    // First map browser keycode to SDL keycode
    const sdlKeycode = BROWSER_TO_SDL_KEYMAP.get(browserKeycode);
    if (sdlKeycode === undefined || sdlKeycode === 0) {
        // No mapping found, return browser keycode as fallback
        return browserKeycode;
    }

    // Then map SDL keycode to Lisp keycode
    const lispKeycode = SDL_TO_LISP_KEYMAP.get(sdlKeycode);
    if (lispKeycode === undefined) {
        // No mapping found, return SDL keycode as fallback
        return sdlKeycode;
    }

    return lispKeycode;
}

/**
 * Handle keyboard event
 *
 * @param event Browser keyboard event
 * @returns Lisp keycode, or null if event should be ignored
 */
export function handleKeyboardEvent(event: KeyboardEvent): number | null {
    const lispKeycode = translateKeycode(event.keyCode);
    return lispKeycode;
}

/**
 * Handle mouse event
 * Translates window coordinates to display coordinates
 *
 * @param event Browser mouse event
 * @param canvas Canvas element
 * @param pixelScale Pixel scale factor (default: 1)
 * @returns Mouse coordinates and button, or null if invalid
 */
export function handleMouseEvent(
    event: MouseEvent,
    canvas: HTMLCanvasElement,
    pixelScale: number = 1
): { x: number; y: number; button: number } | null {
    const rect = canvas.getBoundingClientRect();
    const x = Math.floor((event.clientX - rect.left) / pixelScale);
    const y = Math.floor((event.clientY - rect.top) / pixelScale);

    return {
        x,
        y,
        button: event.button,
    };
}

// BitBLT operations
// Converts bit array to pixel data

export class BitBLT {
  /**
   * Convert display region (DLword array) to pixel data
   * Each bit in the DLword array represents a pixel
   *
   * @param displayRegion DLword array representing display
   * @param width Display width in pixels
   * @param height Display height in pixels
   * @param foreground Foreground color (RGBA)
   * @param background Background color (RGBA)
   * @returns RGBA pixel data
   */
  static convertToPixels(
    displayRegion: Uint16Array,
    width: number,
    height: number,
    foreground: [ number, number, number, number ] = [ 255, 255, 255, 255 ],
    background: [ number, number, number, number ] = [ 0, 0, 0, 255 ]
  ): Uint8Array {
    const pixels = new Uint8Array(width * height * 4);

    for (let y = 0; y < height; y++) {
      for (let x = 0; x < width; x++) {
        const wordIndex = Math.floor((y * width + x) / 16);
        const bitIndex = (y * width + x) % 16;

        if (wordIndex < displayRegion.length) {
          const word = displayRegion[ wordIndex ];
          const bit = (word >> (15 - bitIndex)) & 1;
          const pixelIndex = (y * width + x) * 4;

          if (bit) {
            pixels[ pixelIndex ] = foreground[ 0 ];
            pixels[ pixelIndex + 1 ] = foreground[ 1 ];
            pixels[ pixelIndex + 2 ] = foreground[ 2 ];
            pixels[ pixelIndex + 3 ] = foreground[ 3 ];
          } else {
            pixels[ pixelIndex ] = background[ 0 ];
            pixels[ pixelIndex + 1 ] = background[ 1 ];
            pixels[ pixelIndex + 2 ] = background[ 2 ];
            pixels[ pixelIndex + 3 ] = background[ 3 ];
          }
        }
      }
    }

    return pixels;
  }
}

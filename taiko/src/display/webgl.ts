// WebGL display renderer
// Renders display region to canvas using WebGL 2.0

export class WebGLRenderer {
  private gl: WebGL2RenderingContext | null = null;
  private canvas: HTMLCanvasElement;
  private program: WebGLProgram | null = null;
  private texture: WebGLTexture | null = null;
  private displayWidth: number = 0;
  private displayHeight: number = 0;

  constructor(canvas: HTMLCanvasElement) {
    this.canvas = canvas;
    this.initWebGL();
  }

  /**
   * Initialize WebGL context
   */
  private initWebGL(): void {
    const gl = this.canvas.getContext('webgl2');
    if (!gl) {
      throw new Error('WebGL 2.0 not supported');
    }
    this.gl = gl;

    // Create shader program
    this.program = this.createShaderProgram();

    // Create texture for display region
    this.texture = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, this.texture);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
  }

  /**
   * Create shader program for bit-to-pixel conversion
   */
  private createShaderProgram(): WebGLProgram {
    if (!this.gl) throw new Error('WebGL not initialized');

    const vertexShaderSource = `#version 300 es
            in vec2 a_position;
            in vec2 a_texCoord;
            out vec2 v_texCoord;
            void main() {
                gl_Position = vec4(a_position, 0.0, 1.0);
                v_texCoord = a_texCoord;
            }
        `;

    const fragmentShaderSource = `#version 300 es
            precision highp float;
            uniform sampler2D u_texture;
            uniform vec2 u_resolution;
            in vec2 v_texCoord;
            out vec4 fragColor;
            void main() {
                vec2 coord = v_texCoord;
                vec4 color = texture(u_texture, coord);
                fragColor = color;
            }
        `;

    const vertexShader = this.compileShader(this.gl.VERTEX_SHADER, vertexShaderSource);
    const fragmentShader = this.compileShader(this.gl.FRAGMENT_SHADER, fragmentShaderSource);

    const program = this.gl.createProgram();
    if (!program) throw new Error('Failed to create program');

    this.gl.attachShader(program, vertexShader);
    this.gl.attachShader(program, fragmentShader);
    this.gl.linkProgram(program);

    if (!this.gl.getProgramParameter(program, this.gl.LINK_STATUS)) {
      const info = this.gl.getProgramInfoLog(program);
      throw new Error(`Program link failed: ${info}`);
    }

    return program;
  }

  /**
   * Compile shader
   */
  private compileShader(type: number, source: string): WebGLShader {
    if (!this.gl) throw new Error('WebGL not initialized');

    const shader = this.gl.createShader(type);
    if (!shader) throw new Error('Failed to create shader');

    this.gl.shaderSource(shader, source);
    this.gl.compileShader(shader);

    if (!this.gl.getShaderParameter(shader, this.gl.COMPILE_STATUS)) {
      const info = this.gl.getShaderInfoLog(shader);
      this.gl.deleteShader(shader);
      throw new Error(`Shader compilation failed: ${info}`);
    }

    return shader;
  }

  /**
   * Update display region
   */
  updateDisplay(displayRegion: Uint16Array, width: number, height: number): void {
    if (!this.gl || !this.texture || !this.program) return;

    this.displayWidth = width;
    this.displayHeight = height;

    // Convert display region to RGBA texture data
    const rgbaData = new Uint8Array(width * height * 4);
    for (let i = 0; i < displayRegion.length; i++) {
      const word = displayRegion[ i ];
      const y = Math.floor(i / width);
      const x = i % width;
      const idx = (y * width + x) * 4;

      // Convert bits to pixels (simplified - full implementation would handle foreground/background colors)
      const bit = word & 1;
      rgbaData[ idx ] = bit ? 255 : 0;     // R
      rgbaData[ idx + 1 ] = bit ? 255 : 0; // G
      rgbaData[ idx + 2 ] = bit ? 255 : 0; // B
      rgbaData[ idx + 3 ] = 255;            // A
    }

    // Update texture
    this.gl.bindTexture(this.gl.TEXTURE_2D, this.texture);
    this.gl.texImage2D(this.gl.TEXTURE_2D, 0, this.gl.RGBA, width, height, 0, this.gl.RGBA, this.gl.UNSIGNED_BYTE, rgbaData);
  }

  /**
   * Render display to canvas
   */
  render(): void {
    if (!this.gl || !this.program || !this.texture) return;

    this.gl.useProgram(this.program);
    this.gl.bindTexture(this.gl.TEXTURE_2D, this.texture);

    // Set up viewport
    this.gl.viewport(0, 0, this.canvas.width, this.canvas.height);

    // Clear canvas
    this.gl.clearColor(0, 0, 0, 1);
    this.gl.clear(this.gl.COLOR_BUFFER_BIT);

    // Create quad vertices (full screen)
    const positions = new Float32Array([
      -1, -1,  0, 1,  // bottom-left
       1, -1,  1, 1,  // bottom-right
      -1,  1,  0, 0,  // top-left
       1,  1,  1, 0,  // top-right
    ]);

    // Create buffer
    const positionBuffer = this.gl.createBuffer();
    this.gl.bindBuffer(this.gl.ARRAY_BUFFER, positionBuffer);
    this.gl.bufferData(this.gl.ARRAY_BUFFER, positions, this.gl.STATIC_DRAW);

    // Set up attributes
    const positionLoc = this.gl.getAttribLocation(this.program, 'a_position');
    const texCoordLoc = this.gl.getAttribLocation(this.program, 'a_texCoord');

    if (positionLoc >= 0) {
      this.gl.enableVertexAttribArray(positionLoc);
      this.gl.vertexAttribPointer(positionLoc, 2, this.gl.FLOAT, false, 16, 0);
    }

    if (texCoordLoc >= 0) {
      this.gl.enableVertexAttribArray(texCoordLoc);
      this.gl.vertexAttribPointer(texCoordLoc, 2, this.gl.FLOAT, false, 16, 8);
    }

    // Set uniforms
    const resolutionLoc = this.gl.getUniformLocation(this.program, 'u_resolution');
    if (resolutionLoc) {
      this.gl.uniform2f(resolutionLoc, this.canvas.width, this.canvas.height);
    }

    // Draw quad
    this.gl.drawArrays(this.gl.TRIANGLE_STRIP, 0, 4);
  }
}

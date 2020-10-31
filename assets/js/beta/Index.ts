let cubeRotation: number = 0.0;

const BetaHook = {
  mounted() {
    const betaNode: HTMLElement | null = document.getElementById("beta");
    if (betaNode) { console.log(`🧪 Beta mode enabled!`); }

    const canvasNode = <HTMLCanvasElement>document.getElementById("webgl-canvas");
    const webglContext: WebGLRenderingContext | null = canvasNode.getContext("webgl");

    if (webglContext == null) {
      alert("Unable to initialize WebGL. Your browser or machine may not support it.");
      return;
    }

    webglContext.clearColor(0.0, 0.0, 0.0, 1.0);
    webglContext.clear(webglContext.COLOR_BUFFER_BIT);

    const shaderProgram: WebGLProgram | null = initializeShaderProgram(webglContext, vertexShaderSource, fragmentShaderSource);

    if (shaderProgram) {
      const programInfo = {
        program: shaderProgram,
        attribLocations: {
          vertexPosition: webglContext.getAttribLocation(shaderProgram, 'aVertexPosition'),
          vertexColor: webglContext.getAttribLocation(shaderProgram, 'aVertexColor'),
        },
        uniformLocations: {
          projectionMatrix: webglContext.getUniformLocation(shaderProgram, 'uProjectionMatrix'),
          modelViewMatrix: webglContext.getUniformLocation(shaderProgram, 'uModelViewMatrix'),
        },
      };

      const buffers = initializeBuffers(webglContext);

      let then: number = 0;

      const render = (now: number): void => {
        now *= 0.001;
        const deltaTime = now - then;
        then = now;

        drawScene(webglContext, programInfo, buffers, deltaTime);

        requestAnimationFrame(render);
      }
      requestAnimationFrame(render);
    }
  }
}

const vertexShaderSource: string = `
attribute vec4 aVertexPosition;
attribute vec4 aVertexColor;

uniform mat4 uModelViewMatrix;
uniform mat4 uProjectionMatrix;

varying lowp vec4 vColor;

void main() {
  gl_Position = uProjectionMatrix * uModelViewMatrix * aVertexPosition;
  vColor = aVertexColor;
}
`;

const fragmentShaderSource: string = `
varying lowp vec4 vColor;

void main(void) {
  gl_FragColor = vColor;
}
`;

const loadShader = (webglContext: WebGLRenderingContext, type: number, source: string) => {
  const shader: WebGLShader | null = webglContext.createShader(type);

  if (shader) {
    webglContext.shaderSource(shader, source);
    webglContext.compileShader(shader);

    if (!webglContext.getShaderParameter(shader, webglContext.COMPILE_STATUS)) {
      alert("An error occurred compiling the shaders: " + webglContext.getShaderInfoLog(shader));
      webglContext.deleteShader(shader);
      return null;
    }
  }

  return shader;
};


const initializeShaderProgram = (webglContext: WebGLRenderingContext, vertexShaderSource: string, fragmentShaderSource: string) => {
  const vertexShader: WebGLShader | null = loadShader(webglContext, webglContext.VERTEX_SHADER, vertexShaderSource);
  const fragmentShader: WebGLShader | null = loadShader(webglContext, webglContext.FRAGMENT_SHADER, fragmentShaderSource);

  const shaderProgram: WebGLProgram | null = webglContext.createProgram();

  if (shaderProgram) {
    if (vertexShader) webglContext.attachShader(shaderProgram, vertexShader);
    if (fragmentShader) webglContext.attachShader(shaderProgram, fragmentShader);
    webglContext.linkProgram(shaderProgram);

    if (!webglContext.getProgramParameter(shaderProgram, webglContext.LINK_STATUS)) {
      alert('Unable to initialize the shader program: ' + webglContext.getProgramInfoLog(shaderProgram));
      return null;
    }
  }

  return shaderProgram;
}

const initializeBuffers = (webglContext: WebGLRenderingContext) => {
  const positionBuffer: WebGLBuffer | null = webglContext.createBuffer();

  webglContext.bindBuffer(webglContext.ARRAY_BUFFER, positionBuffer);

  const positions: number[] = [
    // Front face
    -1.0, -1.0, 1.0,
    1.0, -1.0, 1.0,
    1.0, 1.0, 1.0,
    -1.0, 1.0, 1.0,

    // Back face
    -1.0, -1.0, -1.0,
    -1.0, 1.0, -1.0,
    1.0, 1.0, -1.0,
    1.0, -1.0, -1.0,

    // Top face
    -1.0, 1.0, -1.0,
    -1.0, 1.0, 1.0,
    1.0, 1.0, 1.0,
    1.0, 1.0, -1.0,

    // Bottom face
    -1.0, -1.0, -1.0,
    1.0, -1.0, -1.0,
    1.0, -1.0, 1.0,
    -1.0, -1.0, 1.0,

    // Right face
    1.0, -1.0, -1.0,
    1.0, 1.0, -1.0,
    1.0, 1.0, 1.0,
    1.0, -1.0, 1.0,

    // Left face
    -1.0, -1.0, -1.0,
    -1.0, -1.0, 1.0,
    -1.0, 1.0, 1.0,
    -1.0, 1.0, -1.0,
  ];

  webglContext.bufferData(webglContext.ARRAY_BUFFER,
    new Float32Array(positions),
    webglContext.STATIC_DRAW);

  const faceColors = [
    [1.0, 1.0, 1.0, 1.0],    // Front face: white
    [1.0, 0.0, 0.0, 1.0],    // Back face: red
    [0.0, 1.0, 0.0, 1.0],    // Top face: green
    [0.0, 0.0, 1.0, 1.0],    // Bottom face: blue
    [1.0, 1.0, 0.0, 1.0],    // Right face: yellow
    [1.0, 0.0, 1.0, 1.0],    // Left face: purple
  ];

  let colors: number[] = [];

  for (var j = 0; j < faceColors.length; ++j) {
    const c = faceColors[j];
    colors = colors.concat(c, c, c, c);
  }

  const colorBuffer: WebGLBuffer | null = webglContext.createBuffer();
  webglContext.bindBuffer(webglContext.ARRAY_BUFFER, colorBuffer);
  webglContext.bufferData(webglContext.ARRAY_BUFFER, new Float32Array(colors), webglContext.STATIC_DRAW);

  const indexBuffer: WebGLBuffer | null = webglContext.createBuffer();
  webglContext.bindBuffer(webglContext.ELEMENT_ARRAY_BUFFER, indexBuffer);

  const indices: number[] = [
    0, 1, 2, 0, 2, 3,    // front
    4, 5, 6, 4, 6, 7,    // back
    8, 9, 10, 8, 10, 11,   // top
    12, 13, 14, 12, 14, 15,   // bottom
    16, 17, 18, 16, 18, 19,   // right
    20, 21, 22, 20, 22, 23,   // left
  ];

  webglContext.bufferData(webglContext.ELEMENT_ARRAY_BUFFER, new Uint16Array(indices), webglContext.STATIC_DRAW);

  return {
    position: positionBuffer,
    color: colorBuffer,
    indices: indexBuffer
  };
}

const drawScene = (webglContext: WebGLRenderingContext, programInfo: any, buffers: any, deltaTime: number) => {
  webglContext.clearColor(0.0, 0.0, 0.0, 1.0);
  webglContext.clearDepth(1.0);
  webglContext.enable(webglContext.DEPTH_TEST);
  webglContext.depthFunc(webglContext.LEQUAL);

  webglContext.clear(webglContext.COLOR_BUFFER_BIT | webglContext.DEPTH_BUFFER_BIT);

  const fieldOfView = 45 * Math.PI / 180;   // in radians
  const zNear = 0.1;
  const zFar = 100.0;
  const projectionMatrix = mat4.create();

  const aspect = webglContext.canvas.clientWidth / webglContext.canvas.clientHeight;

  mat4.perspective(projectionMatrix, fieldOfView, aspect, zNear, zFar);

  const modelViewMatrix = mat4.create();

  mat4.translate(modelViewMatrix, modelViewMatrix, [-0.0, 0.0, -6.0]);

  mat4.rotate(modelViewMatrix, modelViewMatrix, cubeRotation, [0, 0, 1]);
  mat4.rotate(modelViewMatrix, modelViewMatrix, cubeRotation * .7, [0, 1, 0]);

  {
    const numComponents = 3;
    const type = webglContext.FLOAT;
    const normalize = false;
    const stride = 0;
    const offset = 0;
    webglContext.bindBuffer(webglContext.ARRAY_BUFFER, buffers.position);
    webglContext.vertexAttribPointer(
      programInfo.attribLocations.vertexPosition,
      numComponents,
      type,
      normalize,
      stride,
      offset);
    webglContext.enableVertexAttribArray(
      programInfo.attribLocations.vertexPosition);
  }

  {
    const numComponents = 4;
    const type = webglContext.FLOAT;
    const normalize = false;
    const stride = 0;
    const offset = 0;
    webglContext.bindBuffer(webglContext.ARRAY_BUFFER, buffers.color);
    webglContext.vertexAttribPointer(
      programInfo.attribLocations.vertexColor,
      numComponents,
      type,
      normalize,
      stride,
      offset);
    webglContext.enableVertexAttribArray(
      programInfo.attribLocations.vertexColor);
  }

  webglContext.useProgram(programInfo.program);

  webglContext.uniformMatrix4fv(
    programInfo.uniformLocations.projectionMatrix,
    false,
    projectionMatrix);
  webglContext.uniformMatrix4fv(
    programInfo.uniformLocations.modelViewMatrix,
    false,
    modelViewMatrix);

  {
    const offset = 0;
    const vertexCount = 4;
    webglContext.drawArrays(webglContext.TRIANGLE_STRIP, offset, vertexCount);
  }

  webglContext.bindBuffer(webglContext.ELEMENT_ARRAY_BUFFER, buffers.indices);

  {
    const vertexCount = 36;
    const type = webglContext.UNSIGNED_SHORT;
    const offset = 0;
    webglContext.drawElements(webglContext.TRIANGLES, vertexCount, type, offset);
  }

  cubeRotation += deltaTime;
}

export default BetaHook;

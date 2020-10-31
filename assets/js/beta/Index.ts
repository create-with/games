let squareRotation: number = 0.0;

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
    -1.0, 1.0,
    1.0, 1.0,
    -1.0, -1.0,
    1.0, -1.0,
  ];

  webglContext.bufferData(webglContext.ARRAY_BUFFER,
    new Float32Array(positions),
    webglContext.STATIC_DRAW);

  const colors: number[] = [
    1.0, 1.0, 1.0, 1.0,    // white
    1.0, 0.0, 0.0, 1.0,    // red
    0.0, 1.0, 0.0, 1.0,    // green
    0.0, 0.0, 1.0, 1.0,    // blue
  ];

  const colorBuffer: WebGLBuffer | null = webglContext.createBuffer();
  webglContext.bindBuffer(webglContext.ARRAY_BUFFER, colorBuffer);
  webglContext.bufferData(webglContext.ARRAY_BUFFER, new Float32Array(colors), webglContext.STATIC_DRAW);

  return {
    position: positionBuffer, color: colorBuffer
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

  mat4.perspective(projectionMatrix,
    fieldOfView,
    aspect,
    zNear,
    zFar);

  const modelViewMatrix = mat4.create();

  mat4.translate(modelViewMatrix, modelViewMatrix, [-0.0, 0.0, -6.0]);

  mat4.rotate(modelViewMatrix, modelViewMatrix, squareRotation, [0, 0, 1]);

  {
    const numComponents = 2;
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

  squareRotation += deltaTime;
}

export default BetaHook;

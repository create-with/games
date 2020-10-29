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

    // Black Background
    webglContext.clearColor(0.0, 0.0, 0.0, 1.0);
    webglContext.clear(webglContext.COLOR_BUFFER_BIT);
  }
}

export default BetaHook;

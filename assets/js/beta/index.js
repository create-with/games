const BetaHook = {
  mounted() {
    const betaNode = document.getElementById("beta");
    if (betaNode) { console.log(`🧪 Beta mode enabled!`); }
  }
}

export default BetaHook;

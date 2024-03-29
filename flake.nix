{
  inputs.nixpkgs.url = github:poscat0x04/nixpkgs/dev;
  inputs.flake-utils.url = github:poscat0x04/flake-utils;

  outputs = { self, nixpkgs, flake-utils, ... }: with flake-utils;
    eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        in
          with pkgs;
          {
            devShell = telegraph-dev.envFunc { withHoogle = true; };
            defaultPackage = telegraph;
          }
    ) // {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages.override {
            overrides = hself: hsuper: {
              optics-core = hsuper.optics-core_0_4;
              optics-th = hsuper.optics-th_0_4;
            };
          };
          telegraph = self.haskell.lib.dontCheck (hpkgs.callCabal2nix "telegraph" ./. {});
        in
          with super; with haskell.lib;
          {
            inherit telegraph;
            telegraph-dev = addBuildTools telegraph [
              haskell-language-server
              cabal-install
            ];
          };
    };
}

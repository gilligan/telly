{ compiler ? "ghc843", pkgs ? import ./nix/nixpkgs.nix }:

let

  haskellPackages = pkgs.haskell.packages.${compiler};
  drv = haskellPackages.callCabal2nix "telly" ./. {};

  telly-ghcid = pkgs.writeScriptBin "telly-ghcid"
  ''
    #!${pkgs.stdenv.shell}
    ${haskellPackages.ghcid}/bin/ghcid -c "${haskellPackages.cabal-install}/bin/cabal new-repl"
  '';

in

  { 
    telly = drv;

    telly-shell = haskellPackages.shellFor {
      withHoogle = true;
      packages = p: [drv];
      buildInputs = with pkgs; [ cabal-install telly-ghcid ];
    };
  }

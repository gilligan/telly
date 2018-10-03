{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, lens, split, stdenv
      , taggy, taggy-lens, tasty, tasty-discover, tasty-hspec, text
      }:
      mkDerivation {
        pname = "libtelly";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base bytestring lens split taggy taggy-lens text
        ];
        testHaskellDepends = [
          aeson base bytestring tasty tasty-discover tasty-hspec text
        ];
        description = "Turns a html snippet into json :)";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv

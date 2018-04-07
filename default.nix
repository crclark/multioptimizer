{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-casing, async, base, bytestring
      , clock, containers, directory, errors, exceptions, list-t, mtl
      , operational, optparse-generic, pretty-simple, process, random-fu
      , random-source, safe, scientific, stdenv, stm, tasty, tasty-hunit
      , text, transformers, unordered-containers, vector
      , vector-algorithms
      }:
      mkDerivation {
        pname = "multioptimizer";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          async base clock containers errors exceptions list-t mtl
          operational random-fu random-source safe stm transformers vector
          vector-algorithms
        ];
        executableHaskellDepends = [
          aeson aeson-casing async base bytestring clock containers directory
          errors exceptions list-t mtl operational optparse-generic
          pretty-simple process random-fu random-source safe scientific stm
          text transformers unordered-containers vector vector-algorithms
        ];
        testHaskellDepends = [
          async base clock containers errors exceptions list-t mtl
          operational pretty-simple random-fu random-source safe stm tasty
          tasty-hunit transformers vector vector-algorithms
        ];
        homepage = "https://github.com/githubuser/multioptimizer#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv

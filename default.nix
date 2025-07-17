# Optionally put the specified nix version of the package in the environment
{ environment ? "shell" }:
let
  pkgs = import ./nix/nixpkgs-pinned.nix { };

  defaultEnv = pkgs.haskellPackages.shellFor {
    packages = p: [ p.hoff ];

    buildInputs = [
      pkgs.dia
      pkgs.dpkg
      pkgs.git
      pkgs.niv
      pkgs.shellcheck

      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.haskell-language-server
      pkgs.haskellPackages.implicit-hie
      pkgs.haskellPackages.stylish-haskell
      pkgs.haskellPackages.weeder
    ];

    withHoogle = true;
  };

  environments = { shell = defaultEnv; };
in environments."${environment}"

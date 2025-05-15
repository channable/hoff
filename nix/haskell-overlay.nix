{ sources ? import ./sources.nix, pkgs }:
self: super: {
  hoff = self.callPackage ../hoff.nix { };

  # This version isn't in our nixpkgs yet, but we need it because it adds
  # support for reactions endpoints.
  github =
    pkgs.haskell.lib.overrideCabal super.github {
      version = "0.30";
      sha256 = "sha256-Lq4kOOcZIIBFXW8XNhak4HfryyBjB54Ewks4/fNrPBM=";
      # Since the version in nixpkgs uses a revision, we need to explicitly
      # unset it (and the corresponding `.cabal` file hash).
      revision = null;
      editedCabalFile = null;
    };
}

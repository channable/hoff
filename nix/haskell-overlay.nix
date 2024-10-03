{ sources ? import ./sources.nix, pkgs }:
self: super: {
  hoff = self.callPackage ../hoff.nix { };

  github =
    pkgs.haskell.lib.compose.appendPatches
      [
        # https://github.com/haskell-github/github/pull/509
        (pkgs.fetchpatch {
          name = "github.patch";
          url = "https://github.com/haskell-github/github/commit/623105d3987c4bb4e67d48e5ae36a3af97480be9.patch";
          sha256 = "sha256-3zRYnrxg9G+druD8o5iejCnTclxd2eg1V7BAO6USjzo=";
        })
      ]
      super.github;
}

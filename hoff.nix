{
# `pkgs` is needed for git. Specifying `git` here would use the `git` library
# from Hackage instead
pkgs, mkDerivation

# Core packages
, coreutils, glibcLocales, lib, makeWrapper, nix-gitignore, openssh

# Haskell packages
, QuickCheck, aeson, aeson-pretty, blaze-html, blaze-markup, bytestring
, containers, cryptonite, directory, effectful, extra, file-embed, filepath
, generic-arbitrary, github, hspec, hspec-core, http-client, http-conduit
, http-types, megaparsec, memory, monad-logger, optparse-applicative, process
, process-extras, prometheus, prometheus-metrics-ghc, quickcheck-instances
, scotty, stm, text, text-format, time, uuid, vector, wai
, wai-middleware-prometheus, warp, warp-tls }:
mkDerivation {
  pname = "hoff";
  version = "0.40.0"; # please keep consistent with hoff.cabal

  src = let
    # We do not want to include all files, because that leads to a lot of things
    # that nix has to copy to the temporary build directory that we don't want
    # to have in there (e.g. the `.dist-newstyle` directory, the `.git`
    # directory, etc.)
    prefixWhitelist = builtins.map builtins.toString [
      ./app
      ./package
      ./src
      ./static
      ./tests
      ./hoff.cabal
      ./license
    ];
    # Compute source based on whitelist
    whitelistFilter = path: _type:
      lib.any (prefix: lib.hasPrefix prefix path) prefixWhitelist;
    gitignore = builtins.readFile ./.gitignore;
    gitignoreFilter =
      nix-gitignore.gitignoreFilterPure whitelistFilter gitignore ./.;
    whitelistedSrc = lib.cleanSourceWith {
      src = lib.cleanSource ./.;
      filter = gitignoreFilter;
    };
  in whitelistedSrc;

  buildTools = [ makeWrapper ];

  postInstall = ''
    # Set LOCALE_ARCHIVE so that glibc can find the locales it needs when running on Ubuntu
    # machines.
    wrapProgram $out/bin/hoff --set LOCALE_ARCHIVE ${glibcLocales}/lib/locale/locale-archive
  '';

  isLibrary = false;
  isExecutable = true;

  executableToolDepends = [ pkgs.git coreutils openssh ];

  testDepends = [ pkgs.git coreutils openssh ];

  libraryHaskellDepends = [
    QuickCheck
    aeson
    aeson-pretty
    blaze-html
    blaze-markup
    bytestring
    containers
    cryptonite
    directory
    effectful
    extra
    file-embed
    filepath
    generic-arbitrary
    github
    hspec
    hspec-core
    http-client
    http-conduit
    http-types
    megaparsec
    memory
    monad-logger
    optparse-applicative
    process
    process-extras
    prometheus
    prometheus-metrics-ghc
    quickcheck-instances
    scotty
    stm
    text
    text-format
    time
    uuid
    vector
    wai
    wai-middleware-prometheus
    warp
    warp-tls
  ];

  homepage = "https://github.com/channable/hoff";

  license = lib.licenses.asl20;
}

with (import <nixpkgs> {});

stdenv.mkDerivation {

  name = "myEnv";

  buildInputs = [
    git hwloc
    hpx
    stack
    openssh
    pkgconfig
    bash
    which
    haskell.packages.lts-5_9.ghc
  ];

  STACK_IN_NIX_EXTRA_ARGS
      = " --extra-lib-dirs=${glpk}/lib"
      + " --extra-include-dirs=${glpk}/include"
      + " --extra-lib-dirs=${pcre}/lib"
      + " --extra-include-dirs=${pcre}/include"
  ;
}

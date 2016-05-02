with (import <nixpkgs> {});
with (import ./hpx-2.2.0.nix);

stdenv.mkDerivation {

  name = "myEnv";

#  hpx = import ./hpx-2.2.0.nix;
  buildInputs = [
    git openssh pkgconfig
    hpxEnv
    haskell.packages.lts-5_9.ghc
  ];

  STACK_IN_NIX_EXTRA_ARGS
      = " --extra-lib-dirs=${glpk}/lib"
      + " --extra-include-dirs=${glpk}/include"
      + " --extra-lib-dirs=${pcre}/lib"
      + " --extra-include-dirs=${pcre}/include"
  ;
}

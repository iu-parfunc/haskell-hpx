
# Build hpx

with import <nixpkgs> {}; {
     hpxEnv = stdenv.mkDerivation {
       name = "hpx";
       version = "2.2.0";
       builder = ./build_hpx_in_nix.sh;
       src = fetchurl {
         url = http://hpx.crest.iu.edu/release/hpx-2.2.0.tar.gz;
         sha256 = "01sc85ifwdg87jvcgkkqmjg09j2h1d9i1y5vb3lifhrq6za399bs";
       };

       enableParallelBuilding = true;

       buildInputs = [ stdenv perl ];
     };
}

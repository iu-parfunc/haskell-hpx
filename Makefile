
# A cheat sheet for different ways to build:

# This is the one that works right now
default: build-manual

build:
	stack build --no-nix

build-nix:
	stack build
# This option ^ produces the following nix-shell call:

# nix-shell --pure
#    -I nixpkgs=https://github.com/iu-parfunc/nixpkgs/archive/tag-hpx-2.2.0-nixpkgs-16.03.tar.gz
#   -E "with (import <nixpkgs> {});
#       runCommand \"myEnv\" {
#          buildInputs=lib.optional stdenv.isLinux glibcLocales ++
#               [ hpx git pkgconfig hwloc libcxx haskell.packages.lts-5_9.ghc ];
#          STACK_PLATFORM_VARIANT=''nix'';
#          STACK_IN_NIXSHELL=1;
#          STACK_IN_NIX_EXTRA_ARGS='' --extra-lib-dirs=${hpx}/lib
#                   --extra-include-dirs=${hpx}/include  --extra-lib-dirs=${git}/lib --extra-include-dirs=${git}/include  --extra-lib-dirs=${pkgconfig}/lib --extra-include-dirs=${pkgconfig}/include  --extra-lib-dirs=${hwloc}/lib --extra-include-dirs=${hwloc}/include  --extra-lib-dirs=${libcxx}/lib --extra-include-dirs=${libcxx}/include  --extra-lib-dirs=${haskell.packages.lts-5_9.ghc}/lib --extra-include-dirs=${haskell.packages.lts-5_9.ghc}/include  '' ;
#       } \"\""
#   --command "'/nix/store/kff433s61k9808vwb9m0yl8pmipirvxs-stack-1.0.4.3/bin/stack' $STACK_IN_NIX_EXTRA_ARGS '--internal-re-exec-version=1.0.4.3' 'build' '--verbose'" @(stack_HKArHFFZq7p7BGX6AeJtxh:Stack.Exec src/Stack/Exec.hs:51:5)


# Build by producing our OWN call into nix-shell:
build-manual:
	nix-shell --pure -I "nixpkgs=https://github.com/iu-parfunc/nixpkgs/archive/tag-hpx-2.2.0-nixpkgs-16.03.tar.gz" shell.nix --run "stack build --no-nix"


# Enter a shell interactively, where we SHOULD in principle be able to build with "make build":
shell:
	nix-shell --pure -I "nixpkgs=https://github.com/iu-parfunc/nixpkgs/archive/tag-hpx-2.2.0-nixpkgs-16.03.tar.gz" shell.nix



clean:
	stack clean
	rm -rf .stack-work


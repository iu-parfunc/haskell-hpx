
# A cheat sheet for different ways to build:

default: build-manual

build:
	stack build --no-nix

build-nix:
	stack build

# Build by producing our OWN call into nix-shell:
build-manual:
	nix-shell --pure -I "nixpkgs=https://github.com/iu-parfunc/nixpkgs/archive/tag-hpx-2.2.0-nixpkgs-16.03.tar.gz" shell.nix --run "stack build --no-nix"


# Enter a shell interactively, where we SHOULD in principle be able to build with "make build":
shell:
	nix-shell --pure -I "nixpkgs=https://github.com/iu-parfunc/nixpkgs/archive/tag-hpx-2.2.0-nixpkgs-16.03.tar.gz" shell.nix


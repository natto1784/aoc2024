{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          basePackages = pkgs.haskellPackages;
          devShell = {
            enable = true;
            hlsCheck.enable = true;

            tools = hp: {
              inherit (pkgs)
                nixpkgs-fmt;
              inherit (hp)
                cabal-fmt
                fourmolu;
            };
          };
        };

        packages.default = self'.packages.aoc2024;
        apps =
          let
            name = n: "day${(if n < 10 then "0" else "" ) + toString n}";
          in
          with pkgs.lib; genAttrs (map name (range 1 21))
            (n: {
              type = "app";
              program = "${self'.packages.aoc2024}/bin/${n}";
            });
      };
    };
}

{
  description = "Postgres bindings for Idris2";

  inputs = {
    idris2-packageset.url = "github:mattpolzin/nix-idris2-packages";
  };

  outputs = { self, nixpkgs, idris2-packageset, ... }:
    let
      inherit (nixpkgs) lib;
      forEachSystem =
        f: lib.genAttrs lib.systems.flakeExposed (system: f system nixpkgs.legacyPackages.${system});
    in
    {
      packages = forEachSystem (
        system: pkgs:
        let
          buildIdris' = idris2-packageset.buildIdris'.${system};
        in
        {
          default = buildIdris' {
            ipkgName = "pg-idris";
            src = builtins.path {
              path = ./.;
              name = "pg-idris-src";
            };
            buildInputs = [
              pkgs.postgresql.lib
            ];
            nativeBuildInputs = [
              pkgs.postgresql.pg_config
            ];
          };
        }
      );
      devShells = forEachSystem (
        system: pkgs:
        let
          inherit (idris2-packageset.packages.${system}) idris2 idris2Lsp;
          inherit (nixpkgs.legacyPackages.${system}) mkShell;
        in
        {
          default = mkShell {
            packages = [
              idris2
              idris2Lsp
            ];

            inputsFrom = [
              self.packages.${system}.default
            ];
          };
        }
      );
      formatter = forEachSystem (system: pkgs: pkgs.nixfmt-rfc-style);

      checks = forEachSystem (
        system: pkgs: {
          readme =
            let pg-idris = self.packages.${system}.default;
                buildIdris = idris2-packageset.buildIdris.${system};
            in (buildIdris {
              src = ./.;
              ipkgName = "readme-check";
              # ^ there is not actually an ipkg but the field is required
              idrisLibraries = [ pg-idris ];
              dontInstall = true;
            }).library'.overrideAttrs (finalAttrs: {
              buildPhase = ''
                runHook preBuild

	              idris2 \
                  -p indexed \
                  -p elab-util \
                  -p parser \
                  -p parser-json \
                  -p pg-idris \
                  --check ./README.md \
                  | tee $out

                runHook postBuild
              '';
            });
        }
      );
    };
}

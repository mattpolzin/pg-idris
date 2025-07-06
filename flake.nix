{
  description = "Postgres bindings for Idris2";

  inputs = {
    idris2-packageset.url = "github:mattpolzin/nix-idris2-packages";
  };

  outputs =
    {
      self,
      nixpkgs,
      idris2-packageset,
      ...
    }:
    let
      inherit (nixpkgs) lib;
      forEachSystem =
        f:
        lib.genAttrs lib.systems.flakeExposed (
          system:
          f system nixpkgs.legacyPackages.${system} idris2-packageset.packages.${system}.idris2Packages
        );
    in
    {
      packages = forEachSystem (
        system: pkgs: idris2Packages:
        let
          buildIdris = idris2-packageset.buildIdris.${system};
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

          testLib =
            (buildIdris {
              ipkgName = "pg-idris-tests";
              src = builtins.path {
                path = ./tests;
                name = "pg-idris-tests-src";
              };
              idrisLibraries = [
                self.packages.${system}.default
              ];
            }).library';

          test =
            (buildIdris {
              ipkgName = "pg-idris-tests";
              src = builtins.path {
                path = ./tests;
                name = "pg-idris-tests-src";
              };
              meta.mainProgram = "test";
              buildInputs = [
                pkgs.postgresql.lib
              ];
              idrisLibraries = [
                self.packages.${system}.default
              ];
              postInstall = ''
                wrapProgram $out/bin/test \
                  --suffix LD_LIBRARY_PATH   : ${lib.makeLibraryPath [ pkgs.postgresql.lib ]} \
                  --suffix DYLD_LIBRARY_PATH : ${lib.makeLibraryPath [ pkgs.postgresql.lib ]} \
                  --suffix IDRIS2_PACKAGE_PATH : ${self.packages.${system}.testLib.IDRIS2_PACKAGE_PATH} \
                  --suffix IDRIS2_PACKAGE_PATH : ${
                    lib.makeLibraryPath [ self.packages.${system}.testLib ]
                  }/idris2-0.7.0
                  # ^ The tests need to know about the pg-idris package path at
                  # runtime because each test builds and/or runs some Idris code.
              '';
            }).executable;

          example =
            (buildIdris {
              ipkgName = "example";
              src = builtins.path {
                path = ./Example;
                name = "pg-idris-example-src";
              };
              meta.mainProgram = "example";
              buildInputs = [
                pkgs.postgresql.lib
              ];
              idrisLibraries = [
                self.packages.${system}.default
              ];
              postInstall = ''
                wrapProgram $out/bin/example \
                  --suffix LD_LIBRARY_PATH   : ${lib.makeLibraryPath [ pkgs.postgresql.lib ]} \
                  --suffix DYLD_LIBRARY_PATH : ${lib.makeLibraryPath [ pkgs.postgresql.lib ]}
              '';
            }).executable;
        }
      );
      devShells = forEachSystem (
        system: pkgs: idris2Packages:
        let
          inherit (idris2Packages) idris2 idris2Lsp;
          inherit (pkgs) mkShell;
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
          tests = mkShell {
            packages = [
              idris2
              idris2Lsp
            ];

            inputsFrom = [
              self.packages.${system}.test
            ];
          };
        }
      );
      formatter = forEachSystem (
        system: pkgs: idris2Packages:
        pkgs.nixfmt-rfc-style
      );

      checks = forEachSystem (
        system: pkgs: idris2Packages: {
          example = self.packages.${system}.example;

          readme =
            let
              pg-idris = self.packages.${system}.default;
              buildIdris = idris2-packageset.buildIdris.${system};
            in
            (buildIdris {
              src = ./.;
              ipkgName = "readme-check";
              # ^ there is not actually an ipkg but the field is required
              idrisLibraries = [ pg-idris ];
              dontInstall = true;
            }).library'.overrideAttrs
              (finalAttrs: {
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
          compTimeTests =
            let
              test = lib.getExe self.packages.${system}.test;
            in
            pkgs.stdenvNoCC.mkDerivation {
              pname = "compTimeTest-check";
              version = self.packages.${system}.default;

              src = ./tests;

              nativeBuildInputs = [
                pkgs.coreutils
                idris2Packages.idris2
              ];

              preBuild = ''
                patchShebangs --build \
                  check.sh run.sh
              '';

              buildPhase = ''
                runHook preBuild

                ${test} idris2 --only compile_time | tee $out

                runHook postBuild
              '';

              dontInstall = true;
            };
          unitTests =
            let
              test = lib.getExe self.packages.${system}.test;
            in
            pkgs.stdenvNoCC.mkDerivation {
              pname = "unitTest-check";
              version = self.packages.${system}.default;

              src = ./tests;

              nativeBuildInputs = [
                pkgs.coreutils
                idris2Packages.idris2
              ];

              preBuild = ''
                patchShebangs --build \
                  check.sh run.sh
              '';

              buildPhase = ''
                runHook preBuild

                ${test} idris2 --only unit_tests | tee $out

                runHook postBuild
              '';

              dontInstall = true;
            };
        }
      );

      templates.executable = {
        description = "A flake for an executable Idris2 project that depends on pg-idris";
        path = ./nix/templates/executable;
      };
    };
}

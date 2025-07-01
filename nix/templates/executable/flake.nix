{
  description = "Postgres based Idris2 Program";

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
        f: lib.genAttrs lib.systems.flakeExposed (system: f system nixpkgs.legacyPackages.${system});
    in
    {
      packages = forEachSystem (
        system: pkgs:
        let
          buildIdris = idris2-packageset.buildIdris.${system};
        in
        {
          default = buildIdris {
            ipkgName = "my-project";
            # TODO: change this ^
            src = ./.;
            idrisLibraries = [ idris2-packageset.idris2Packages.packdb.pg-idris ];
            postInstall = ''
              # --- change binary name:
              wrapProgram $out/bin/my-prog \
                --suffix LD_LIBRARY_PATH   : ${lib.makeLibraryPath [ pkgs.postgresql.lib ]} \
                --suffix DYLD_LIBRARY_PATH : ${lib.makeLibraryPath [ pkgs.postgresql.lib ]}
            '';
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
    };
}

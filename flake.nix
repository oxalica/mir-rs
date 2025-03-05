{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs =
    { self, nixpkgs }:
    let
      inherit (nixpkgs) lib;
      eachSystem = lib.genAttrs lib.systems.flakeExposed;
      allPkgs = eachSystem (system: nixpkgs.legacyPackages.${system});
    in
    {
      packages = eachSystem (
        system:
        let
          pkgs = allPkgs.${system};
        in
        {
          mir = pkgs.stdenv.mkDerivation rec {
            pname = "mir";
            # NB: Sync with submodule.
            version = "1.0.0";

            src = pkgs.buildPackages.fetchFromGitHub {
              owner = "vnmakarov";
              repo = "mir";
              tag = "v${version}";
              hash = "sha256-arqSUIyXp1SvS/ohP+uJdqXfSdnrAfKtnZyBzBel6BM=";
            };

            enableParallelBuilding = true;

            makeFlags = [
              "GITCOMMIT=v${version}"
              "PREFIX=${placeholder "out"}"
            ];
          };
        }
      );

      devShells = eachSystem (
        system:
        let
          pkgs = allPkgs.${system};
        in
        {
          bindgen = pkgs.mkShell {
            nativeBuildInputs = [ pkgs.rustPlatform.bindgenHook ];
          };
        }
      );
    };
}

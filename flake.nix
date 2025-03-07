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

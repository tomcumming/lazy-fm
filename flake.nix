{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
  };
  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" ];
    in
    builtins.foldl'
      (ss: s:
        let
          pkgs = nixpkgs.legacyPackages."${s}";
        in
        ss //
        {
          devShells."${s}".default = pkgs.mkShell {
            packages = [
              pkgs.nodejs_22
              pkgs.esbuild
              pkgs.purescript
            ];
          };
          formatter."${s}" = pkgs.nixpkgs-fmt;
        })
      { }
      systems;
}

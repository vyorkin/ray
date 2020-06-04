let
  nixpkgs = import ./nix/release.nix;

in nixpkgs.haskellPackages.shellFor {
  packages = p: [nixpkgs.haskellPackages.ray];

  buildInputs = with nixpkgs.haskellPackages; [
    cabal-install

    ghcid

    hlint
    weeder

    hoogle

    hasktags
    haskdogs

    pretty-simple
    pretty-show
  ];

  shellHook = ''
    export CABAL_DIR=${builtins.toString ./.cabal}
  '';
}

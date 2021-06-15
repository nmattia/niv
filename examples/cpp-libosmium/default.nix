let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {
    overlays = [
      (_: _: { inherit sources; })
      (import ./overlay.nix)
    ];
  };
in
  with pkgs;

  stdenv.mkDerivation {
    name = "nix-cpp-demo";
    nativeBuildInputs = [ cmake pkgconfig ];
    src = pkgs.lib.cleanSource ./.;

    # tell Cmake location of all headers
    cmakeFlags = [
      "-DEXTERNAL_INCLUDE_DIRECTORIES=${lib.strings.makeSearchPathOutput "dev" "include" libosmium.buildInputs}"
    ];

    buildInputs = lib.lists.concatLists [
      # We want to check if dependencies exist using find_package
      [
        libosmium.buildInputs
      ]
      # dependencies
      [
        libosmium
      ]
    ];

    installPhase = ''
      mkdir -p $out/bin
      cp bin/hello-world $out/bin
    '';
  }

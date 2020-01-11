{ 
  # import the sources
  sources ? import ./nix/sources.nix
}:     
let 
  pkgs = import sources.nixpkgs { 
     overlays = [ (import ./overlay.nix) ];
  };
in
with pkgs;

stdenv.mkDerivation {
    name = "nix-cpp-demo";
    nativeBuildInputs = [ cmake pkgconfig];
    src = ./.;

    # tell Cmake location of all headers
    cmakeFlags = [
        "-DEXTERNAL_INCLUDE_DIRECTORIES=${stdenv.lib.strings.makeSearchPathOutput "dev" "include" libosmium.buildInputs}"
    ];

    buildInputs = stdenv.lib.lists.concatLists[
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
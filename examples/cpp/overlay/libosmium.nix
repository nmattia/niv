{ stdenv, fetchFromGitHub, cmake, protozero, expat, zlib, bzip2, boost, sources ? import ../nix/sources.nix  }:

stdenv.mkDerivation rec {
  name = "libosmium";
  src = sources.libosmium;

  nativeBuildInputs = [ cmake ];
  cmakeFlags = [
    "-DBUILD_BENCHMARKS=OFF"
    "-DBUILD_DATA_TESTS=OFF"
    "-DBUILD_EXAMPLES=OFF"
    "-DBUILD_HEADERS=OFF"
    "-DBUILD_TESTING=OFF"
  ];
  buildInputs = [
    protozero 
    expat
    zlib 
    bzip2  
    boost
  ];
}
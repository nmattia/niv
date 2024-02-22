{ stdenv, cmake, protozero, expat, zlib, lz4, bzip2, boost, sources }:

stdenv.mkDerivation {
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
    lz4
    bzip2
    boost
  ];
}

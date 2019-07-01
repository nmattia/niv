{ sources, lib, python3Packages }:

python3Packages.buildPythonApplication rec {
  pname = "termtosvg";
  version = "0.0.0";

  src = sources.termtosvg;

  doCheck = false;

  propagatedBuildInputs = with python3Packages; [ lxml pyte ];

  meta = with lib; {
    inherit (sources.termtosvg) homepage description;
    license = licenses.bsd3;
    maintainers = with maintainers; [ ma27 ];
  };
}

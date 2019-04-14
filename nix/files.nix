{ lib
, writeText
, runCommand
}:
rec
{

  # Lists all the (relative paths to the) files in the directory.
  listFilesInDir = dir:
    let
      go = dir: dirName:
        lib.lists.concatLists
        (
          lib.attrsets.mapAttrsToList
            (path: ty:
              if ty == "directory"
              then
                go "${dir}/${path}" "${dirName}${path}/"
              else
                [ "${dirName}${path}" ]
            )
            (builtins.readDir dir)
        );
    in go dir "";

  # Like nixpkgs' sourceByRegex but doesn't depend on the directory name when
  # computing the hash. Also doesn't require matching on the dir name to
  # actually enter the dir.
  sourceByRegex = name: src: regexes:
    let
      files = builtins.filter (x: x.keep) (map mk (listFilesInDir src));
      mk = path:
        let relPath = lib.removePrefix (toString src + "/") (toString path);
        in
          { inherit relPath;
            path = src + ("/" + path);
            keep = lib.any (re: builtins.match re relPath != null) regexes;
          };

      files' = map (x: x.path) files;
      paths' = map (x: x.relPath) files;

      filesAbs = writeText "foo" (lib.concatStringsSep "\n" files');
      filesRel = writeText "bar" (lib.concatStringsSep "\n" paths');
    in
    runCommand "source-${name}" {}
      ''
        mkdir -p $out

        paste ${filesAbs} ${filesRel} | while IFS="$(printf '\t')" read -r f1 f2
        do
          f="$out/$f2"
          mkdir -p $(dirname $f)
          echo $f1
          echo $f2
          echo $f
          cp $f1 $f
        done
      '';
}

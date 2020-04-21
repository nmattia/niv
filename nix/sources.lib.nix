{ nixpkgs ? import <nixpkgs> { }
}:

with nixpkgs;

rec {

  build-replacement-mapping =
    #
    # Replace occurences of <`prefix`:KEY> with VAL in `tpl` where
    # KEY and VAL are from `mapping`
    #
    # tpl     = the template string
    # prefix  = string keying the replacement
    # mapping = mapping from some key to the replacement value
    #
    # Ex:
    #   prefix  = "env"
    #   mapping = { HELLO = "WORLD"; }
    #
    #   ==> { from = [ "<env:HELLO>" ]; to = [ "WORLD" ]; }

    { prefix            # :: String
    , mapping           # :: { KEY = VAL }
    , skip-keys ? [ ]   # :: [String] == mapping keys to exclude
    , delim_left ? "<"  #
    , delim_right ? ">" #
    , sep ? ":"         # separator between `prefix` and KEY
    }: # ==> { from :: [String];  to :: [String]; }

      with builtins;
      with lib;
      let
        cleaned-mapping = filterAttrs (k: v: !elem k skip-keys) mapping;
      in
      zipAttrs
        (mapAttrsToList
          (k: v:
          {
            from = "${delim_left}${prefix}${sep}${k}${delim_right}";
            to = v;
          }
          )
          cleaned-mapping);


  getEnv = name:
    #
    # :: String -> null or String
    #
    # a version of builtins.getEnv that returns null on failure
    #
    let val = builtins.getEnv name;
    in if val == "" then null else val;


  generate-replacements = spec:
    #
    # :: Spec -> ReplacementMapping
    #
    let
      inherit (lib) elem filterAttrs flatten genAttrs isString mapAttrs zipAttrs;
      ignore-keys = [ "url_template" "url" "sha256" "type" ];
      var-mapping = build-replacement-mapping { prefix = ""; sep = ""; mapping = filterAttrs (k: v: !elem k ignore-keys && isString v) spec; };
      runtime-env = build-replacement-mapping { prefix = "env"; mapping = genAttrs (spec.runtime.env or [ ]) getEnv; };
    in
    mapAttrs (_: n: flatten n) (zipAttrs [ var-mapping runtime-env ]);


  apply-templates = attrName: spec:
    #
    # :: String -> Spec -> String
    #
    # Expand the templates of the given `attrName` in `spec`
    #
    let
      inherit (builtins) getAttr;
      transform = generate-replacements spec;
      value = spec."${attrName}" or (builtins.trace "ERROR: unknown attribute '${attrName}' in spec ${asString spec}" (getAttr attrName spec));
    in
    lib.replaceStrings transform.from transform.to value;


  asString = val:
    #
    # :: Any -> String
    #
    # Like `builtins.toString` but tries to not yell.
    #
    # Will `trace` if no known prettyfier for value's type.
    #
    with builtins;
    if isAttrs val then toJSON val
    else if isBool val then if val then "true" else "false"
    else if isFloat val then toString val
    else if isInt val then toString val
    else if isList val then "[ ${toString (map asString val)} ]"
    else if isNull val then "<null>"
    else if isString val then val
    else trace "asString: Unknown type: ${typeOf val}" "<<${typeOf val}>>";


  elaborate-url = spec:
    #
    # :: Spec -> Spec
    #
    # If no url is provided, expand the url template, otherwise NoOp
    #
    spec // { url = spec.url or (apply-templates "url_template" spec); };


  tests =
    #
    # Test the functions here.  This is an AttrSet of test name to
    # result, with logging done using trace
    #
    let
      check = expected: actual:
        if expected == actual
        then { passed = true; msg = null; }
        else { passed = false; msg = "Expected: ${asString expected} got ${asString actual}"; };
      report = name: result:
        #
        # Report if a test passed or failed.  Failures trigger an
        # abort for non-zero exit.
        #
        let val = if result.passed then result else abort result.msg;
        in builtins.trace "Test ${name} ${ if result.passed then "passed" else "failed"}" val;
    in
    lib.mapAttrs report
      {
        build-replacement-mapping =
          check
            { from = [ "<env:HELLO>" ]; to = [ "WORLD" ]; }
            (build-replacement-mapping { prefix = "env"; mapping = { HELLO = "WORLD"; }; });

        generate-replacements =
          check
            {
              from = [ "<domain>" "<env:HELLO>" ];
              to = [ "foo.bar" "world" ];
            }
            (generate-replacements
              {
                url_template = "http://<domain>.tld/<env:HELLO>.whatever.ext";
                domain = "foo.bar";
                runtime.env = [ "HELLO" ]; # export HELLO=world required by parent process
              }
            );

        apply-templates =
          check
            "http://foo.bar.tld/world.whatever.ext"
            (apply-templates "url_template"
              {
                url_template = "http://<domain>.tld/<env:HELLO>.whatever.ext";
                domain = "foo.bar";
                runtime.env = [ "HELLO" ]; # export HELLO=world required by parent process
              }
            );

        elaborate-url-is-noop =
          let spec = { url = "hello-world"; hello = "hello"; world = "world"; url_template = "<hello>-<world>"; }; in
          check
            spec
            (elaborate-url spec);

        elaborate-url-updates =
          let spec = { hello = "hello"; world = "world"; url_template = "<hello>-<world>"; }; in
          check
            (spec // { url = "hello-world"; })
            (elaborate-url spec);

        asString-bool-true = check "true" (asString true);
        asString-bool-false = check "false" (asString false);
        asString-float = check "42.000000" (asString 42.0);
        asString-int = check "42" (asString 42);
        asString-list-of-int = check "[ 42 ]" (asString [ 42 ]);
        asString-null = check "<null>" (asString null);
        asString-string = check "wat" (asString "wat");


      };
}

let esyStoreVersion = "3";

module Path = {
  include Path;
  let join = (x, y) => Filename.concat(x, y) |> Path.v;
};

let cwd = Sys.getcwd();
let releasePackagePath = cwd;
let releaseExportPath = Path.(v(releasePackagePath) / "_export");
let releaseBinPath = Path.(v(releasePackagePath) / "bin");
let unpaddedStorePath = Path.(v(releasePackagePath) / esyStoreVersion);

let getStorePathForPrefix = (prefix, ocamlPkgName, ocamlVersion) => {
  let ocamlrunStorePath =
    ocamlPkgName ++ "-" ++ ocamlVersion ++ "-########/bin/ocamlrun";

  let esyStorePaddingLength =
    127
    - String.length("!#")
    - String.length("/" ++ "i" ++ "/" ++ ocamlrunStorePath);
  let prefixLength = String.length(prefix ++ "/" ++ esyStoreVersion);
  let paddingLength = esyStorePaddingLength - prefixLength;
  if (paddingLength < 0) {
    failwith(
      "Esy prefix path is too deep in the filesystem, Esy won't be able to relocate artefacts",
    );
  };
  let p = Path.join(prefix, esyStoreVersion);
  print_endline("[P]: " ++ Path.show(p));
  Path.v(
    Path.show(p)
    ++ String.make(esyStorePaddingLength - String.length(Path.show(p)), '_'),
  );
};

type checkError =
  | NoBuildFound
  | ReleaseAlreadyInstalled
  | EsyLibError(EsyLib.Run.error);

type checkResult = result(unit, checkError);
let main = (ocamlPkgName, ocamlVersion, rewritePrefix) => {
  print_endline("[ocamlPkgName]: " ++ ocamlPkgName);
  print_endline("[ocamlVersion]: " ++ ocamlVersion);
  print_endline("[rewritePrefix]: " ++ string_of_bool(rewritePrefix));
  // Store prefix path calculation

  let check = () => {
    let%lwt buildFound = Fs.exists(releaseExportPath);
    switch (buildFound) {
    | Ok(true) =>
      if (!rewritePrefix) {
        Lwt.return(Ok());
      } else {
        let storePath =
          getStorePathForPrefix(
            releasePackagePath,
            ocamlPkgName,
            ocamlVersion,
          );
        let%lwt storeFound = Fs.exists(storePath);
        Lwt.return(
          switch (storeFound) {
          | Ok(true) => Error(ReleaseAlreadyInstalled)
          | Ok(false) => Ok()
          | Error(err) => Error(EsyLibError(err))
          },
        );
      }
    | Ok(false) => Lwt.return(Error(NoBuildFound))
    | Error(err) => Lwt.return(Error(EsyLibError(err)))
    };
  };

  let%lwt checkResult = check();

  switch (checkResult) {
  | Ok(_) => print_endline("tout ok")
  | Error(NoBuildFound) => print_endline("No build found!")
  | Error(ReleaseAlreadyInstalled) =>
    print_endline("Release already installed!")
  | Error(EsyLibError(err)) => print_endline(EsyLib.Run.formatError(err))
  };

  Lwt.return_nil;
  // let initStore = {
  //   let storePrefixPath =
  //     if (rewritePrefix) {
  //       getStorePathForPrefix(releasePackagePath, ocamlPkgName, ocamlVersion);
  //     } else {
  //       unpaddedStorePath;
  //     };
  //   ();
  // };
  // print_endline(storePrefixPath |> Path.show);
};

open Cmdliner;

let ocamlPkgName = {
  let doc = "OCaml package name";
  Arg.(
    value
    & opt(string, "ocaml")
    & info(["ocaml-pkg-name"], ~docv="RELEASE CONFIG", ~doc)
  );
};

let ocamlVersion = {
  let doc = "OCaml package version";
  Arg.(
    value
    & opt(string, "n.00.0000")
    & info(["ocaml-version"], ~docv="RELEASE CONFIG", ~doc)
  );
};

let rewritePrefix = {
  let doc = "Whether to rewrite prefixes in the binary or not";
  Arg.(
    value
    & opt(bool, true)
    & info(["rewrite-prefix"], ~docv="RELEASE CONFIG", ~doc)
  );
};

let lwt_main = (ocamlPkgName, ocamlVersion, rewritePrefix) =>
  Lwt_main.run(main(ocamlPkgName, ocamlVersion, rewritePrefix));

let main_t =
  Term.(const(lwt_main) $ ocamlPkgName $ ocamlVersion $ rewritePrefix);

let info = {
  let doc = "Export native builds and rewrite prefixes in them";
  let man = [`S(Manpage.s_bugs), `P("")];

  Term.info(
    "NatEsyInstallRelease",
    ~version="0.0.0",
    ~doc,
    ~exits=Term.default_exits,
    ~man,
  );
};

let () = Term.exit @@ Term.eval((main_t, info));

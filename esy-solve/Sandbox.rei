open EsyPackageConfig;

/** Sandbox */;

type t = {
  /*** Configuration. */
  cfg: Config.t,
  spec: EsyFetch.SandboxSpec.t,
  /*** Root package. */
  root: InstallManifest.t,
  /*** A set of resolutions. */
  resolutions: Resolutions.t,
  /*** Resolver associated with a sandbox. */
  resolver: Resolver.t,
};

let make:
  (
    ~gitUsername: option(string),
    ~gitPassword: option(string),
    ~cfg: Config.t,
    ~os: System.Platform.t,
    ~arch: System.Arch.t,
    EsyFetch.SandboxSpec.t
  ) =>
  RunAsync.t(t);
let digest:
  (~os: System.Platform.t, ~arch: System.Arch.t, SolveSpec.t, t) =>
  RunAsync.t(Digestv.t);

open Switch_builder

let dump_name = Fmt.of_to_string OpamPackage.Name.to_string
let dump_version = Fmt.of_to_string OpamPackage.Version.to_string

let dump_package ppf OpamPackage.{ name; version } =
  Fmt.pf ppf "%a.%a" dump_name name dump_version version

let dump_package_set =
  Fmt.iter ~sep:(Fmt.any "; ") OpamPackage.Set.iter dump_package

let trace_package_set ~label package_set =
  Fmt.epr "%s: %a\n" label dump_package_set package_set;
  package_set

let dump_error_map ppf =
  OpamPackage.Map.iter
    (fun package result ->
      match result with
      | Ok () -> ()
      | Error err ->
          Fmt.pf ppf "%a: %a\n" dump_package package Check.pp_error err)

let trace_errors_map map =
  Fmt.epr "%a\n" dump_error_map map;
  map

let main () =
  let main_repo = Repository.path "/home/marek/tarides/opam-repository" in
  let stats = Repository.Stats.create () in
  let cache = Repository.Cache.create () in
  let repo = Repository.cache cache (Repository.stats stats main_repo) in
  Repository.packages repo
  |> Version_policy.(
       apply
         (always_latest
         |> override (OpamPackage.of_string "ocaml-config.2")
         |> override (OpamPackage.of_string "ocaml.4.14.0")
         |> override (OpamPackage.of_string "ocaml-base-compiler.4.14.0")))
  |> Select.(
       apply repo
         [
           has_no_depexts;
           is_compatible_with (OpamPackage.of_string "ocaml.4.14.0");
           exclude_package_names
             [
               OpamPackage.Name.of_string "base-domains";
               OpamPackage.Name.of_string "ocaml-variants";
               OpamPackage.Name.of_string "ocaml-system";
               OpamPackage.Name.of_string "jbuilder";
             ];
           exclude_packages
             [
               OpamPackage.of_string "glical.0.0.7";
               OpamPackage.of_string "pa_qualified.0.6";
               OpamPackage.of_string "pa_where.0.4";
               OpamPackage.of_string "hdfs.0.4";
               OpamPackage.of_string "ocamlyices.0.7.1";
               OpamPackage.of_string "wiringpi.0.0.1";
               OpamPackage.of_string "ocamldiff.1.1";
               OpamPackage.of_string "pa_comprehension.0.4";
               OpamPackage.of_string "pa_solution.0.7";
               OpamPackage.of_string "unix-dirent.0.3.5";
               OpamPackage.of_string "conjury.2.1";
               OpamPackage.of_string "ocamlog.0.2";
               OpamPackage.of_string "ringo.0.9";
             ];
           exclude_package_prefix "ocaml-option-";
           exclude_package_prefix "ocaml-options-";
           exclude_package_prefix "dkml-";
         ])
  |> trace_package_set ~label:"all"
  |> Closure.apply repo
  |> trace_package_set ~label:"after first closure apply"
  |> Check.apply repo |> trace_errors_map |> Check.remove_packages_with_errors
  |> trace_package_set ~label:"after check"
  |> Closure.apply repo
  |> trace_package_set ~label:"after second closure apply"
  |> Closure.apply repo
  |> trace_package_set ~label:"after third closure apply"
  |> Monorepo.analyze repo
  |> Monorepo.opam_file |> OpamFile.OPAM.write_to_string |> print_endline

let () = main ()

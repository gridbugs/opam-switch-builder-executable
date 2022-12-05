let main_repo_path = "/home/s/src/switch-builder-executable/repos/default"

let dune_universe_repo_path =
  "/home/s/src/switch-builder-executable/repos/dune-universe"

let pkg_to_filtered_formula pkg : OpamTypes.filtered_formula =
  let name = OpamPackage.name pkg in
  let version = OpamPackage.version pkg in
  Atom
    ( name,
      Atom (Constraint (`Eq, FString (OpamPackage.Version.to_string version)))
    )

let pkg_set_to_filtered_formula pkg_set : OpamTypes.filtered_formula =
  OpamPackage.Set.elements pkg_set
  |> List.map pkg_to_filtered_formula
  |> OpamFormula.ands

let pkg_set_to_opam_file pkg_set =
  OpamFile.OPAM.empty
  |> OpamFile.OPAM.with_depends (pkg_set_to_filtered_formula pkg_set)

let is_dune_command (args, _) =
  List.exists
    (fun (h, _) ->
      match (h : OpamTypes.simple_arg) with
      | CString s ->
          (*print_endline (Printf.sprintf "> %s %b" s (String.equal s "dune")); *)
          String.equal s "dune"
      | _ -> false)
    args

let depends_on_dune opam =
  let deps = OpamFile.OPAM.depends opam in
  let dep_name_list =
    OpamFormula.fold_left (fun xs (x, _) -> x :: xs) [] deps
  in
  List.exists
    (fun p ->
      let s = OpamPackage.Name.to_string p in
      String.equal s "dune")
    dep_name_list

let build_command_uses_dune opam =
  let build_cmd = OpamFile.OPAM.build opam in
  match build_cmd with
  | [] -> false
  | build_cmd -> List.exists is_dune_command build_cmd

let builds_with_dune opam = depends_on_dune opam || build_command_uses_dune opam

let dep_closure repo pkg_set =
  let open Switch_builder in
  let force_keep_names =
    OpamPackage.Name.Set.of_list
      (List.map OpamPackage.Name.of_string [ "ocaml"; "ounit" ])
  in
  let assumed_deps =
    OpamPackage.Name.Set.of_list
      (List.map OpamPackage.Name.of_string
         [
           "base-bigarray";
           "base-threads";
           "base-unix";
           "dune";
           "ocaml";
           "ocaml-base-compiler";
           "ocaml-config";
           "ocaml-options-vanilla";
         ])
  in
  let builds_with_dune p =
    let ret = builds_with_dune (Repository.read_opam repo p) in
    if not ret then
      print_endline
        (Printf.sprintf "    ^^^^ %s doesn't build with dune"
           (OpamPackage.to_string p));
    ret
  in
  print_endline
    (Printf.sprintf "pkg_set_size %d" (OpamPackage.Set.cardinal pkg_set));
  let pkg_list = OpamPackage.Set.elements pkg_set in
  let by_name =
    pkg_list
    |> List.map (fun pkg -> (OpamPackage.name pkg, pkg))
    |> OpamPackage.Name.Map.of_list
  in
  let package_has_deps_in_set p =
    let opam = Repository.read_opam repo p in
    let env =
      Env.common
      |> Env.extend "version"
           (OpamVariable.S
              (OpamPackage.version p |> OpamPackage.Version.to_string))
    in
    let deps = OpamFile.OPAM.depends opam |> OpamFilter.filter_formula env in
    let res =
      Eval.m
        (fun (name, version_formula) ->
          if OpamPackage.Name.Set.mem name assumed_deps then Ok ()
          else
            match OpamPackage.Name.Map.find_opt name by_name with
            | None ->
                let () =
                  print_endline
                    (Printf.sprintf "    ---- %s missing dep outright: %s"
                       (OpamPackage.to_string p)
                       (OpamPackage.Name.to_string name))
                in
                Error ()
            | Some dep ->
                let dep_version = OpamPackage.version dep in
                let has_compatible_dep =
                  OpamFormula.check_version_formula version_formula dep_version
                in
                if has_compatible_dep then Ok ()
                else
                  let () =
                    print_endline
                      (Printf.sprintf
                         "    xxxx %s missing dep by version number: %s"
                         (OpamPackage.to_string p)
                         (OpamPackage.to_string dep))
                  in
                  Error ())
        deps
    in
    Result.is_ok res
  in
  let keep_package p =
    OpamPackage.Name.Set.mem (OpamPackage.name p) force_keep_names
    || (builds_with_dune p && package_has_deps_in_set p)
  in
  List.filter keep_package pkg_list |> OpamPackage.Set.of_list

let dep_closure_fixed repo pkg_set =
  Switch_builder.Import.fixpoint ~equal:OpamPackage.Set.equal
    ~f:(fun pkg_set -> dep_closure repo pkg_set)
    pkg_set

let remove_one_conflict repo pkg_set =
  let pkg_list = OpamPackage.Set.elements pkg_set in
  let by_name =
    pkg_list
    |> List.map (fun pkg -> (OpamPackage.name pkg, pkg))
    |> OpamPackage.Name.Map.of_list
  in
  let open Switch_builder in
  let package_has_conflicts_in_set p =
    let opam = Repository.read_opam repo p in
    let env =
      Env.common
      |> Env.extend "version"
           (OpamVariable.S
              (OpamPackage.version p |> OpamPackage.Version.to_string))
    in
    let conflicts_formula =
      OpamFile.OPAM.conflicts opam |> OpamFilter.filter_formula env
    in
    let res =
      Eval.m
        (fun (name, version_formula) ->
          match OpamPackage.Name.Map.find_opt name by_name with
          | None -> Ok ()
          | Some pkg_in_set ->
              let version = OpamPackage.version pkg_in_set in
              let version_matches_formula =
                OpamFormula.check_version_formula version_formula version
              in
              if version_matches_formula then Error () else Ok ())
        conflicts_formula
    in
    Result.is_error res
  in
  match List.find_opt package_has_conflicts_in_set pkg_list with
  | None -> pkg_set
  | Some p ->
      print_endline
        (Printf.sprintf "removing due to conflict: %s" (OpamPackage.to_string p));
      OpamPackage.Set.remove p pkg_set

let shrink_step repo pkg_set =
  remove_one_conflict repo (dep_closure_fixed repo pkg_set)

let shrink repo =
  Switch_builder.Import.fixpoint ~equal:OpamPackage.Set.equal
    ~f:(shrink_step repo)

let _override_version_policy_from_package_set package_set version_policy =
  OpamPackage.Set.fold Switch_builder.Version_policy.override package_set
    version_policy

let () =
  print_endline "";
  let open Switch_builder in
  let dune_universe_repo = Repository.path dune_universe_repo_path in
  let repo =
    Repository.combine (Repository.path main_repo_path) dune_universe_repo
  in
  (*
  let latest_dune_universe_packages =
    Version_policy.(
      apply always_latest (Repository.packages dune_universe_repo)) 
  in *)
  let cache = Repository.Cache.create () in
  let repo = Repository.cache cache repo in
  (*
  let depends = Package_db.depends repo in
  let compatible_dune_universe_depends =
    Package_db.compatible_versions_for_package_set depends
      ~package_set:latest_dune_universe_packages
    |> Version_policy.(apply always_latest_dune)
in *)
  let packages = Repository.packages repo in
  let latest =
    Version_policy.(
      apply
        (always_latest
        (* |> override_version_policy_from_package_set
              compatible_dune_universe_depends *)
        |> override (OpamPackage.of_string "ocamlfind.1.8.1+dune")
        |> override (OpamPackage.of_string "ocaml.4.14.0")
        |> override (OpamPackage.of_string "ocaml-base-compiler.4.14.0")))
      packages
  in
  let latest_filtered =
    Select.(
      apply repo
        [
          is_compatible_with (OpamPackage.of_string "ocaml.4.14.0");
          is_compatible_with (OpamPackage.of_string "dune.3.6.0");
          is_compatible_with (OpamPackage.of_string "ppxlib.0.28.0");
          exclude_package_names
            [
              OpamPackage.Name.of_string "ocaml-variants";
              OpamPackage.Name.of_string "winsvc";
              OpamPackage.Name.of_string "ocamlog";
              OpamPackage.Name.of_string "nlfork";
            ];
        ]
        latest)
  in
  let latest_closed = shrink repo latest_filtered in
  print_endline (Printf.sprintf "%d" (OpamPackage.Set.cardinal latest_closed));
  let opam_file = pkg_set_to_opam_file latest_closed in
  let opam_file_string = OpamFile.OPAM.write_to_string opam_file in
  let f =
    Unix.openfile "./experiment/all1.opam" [ Unix.O_WRONLY; Unix.O_TRUNC ] 777
  in
  let _ =
    Unix.write_substring f opam_file_string 0 (String.length opam_file_string)
  in
  Unix.close f;
  ()

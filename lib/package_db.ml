module Repo = struct
  type t = {
    version_sets_by_name : OpamPackage.Version.Set.t OpamPackage.Name.Map.t;
    repository : Switch_builder.Repository.t;
  }

  let of_repository repository : t =
    let version_sets_by_name = Switch_builder.Repository.packages repository in
    { version_sets_by_name; repository }

  let find_opt { version_sets_by_name; _ } ~name :
      OpamPackage.Version.Set.t option =
    OpamPackage.Name.Map.find_opt name version_sets_by_name

  let opam_file { repository; _ } ~package =
    Switch_builder.Repository.read_opam repository package

  let all_packages { version_sets_by_name; _ } =
    OpamPackage.Name.Map.to_seq version_sets_by_name
    |> Seq.map (fun (name, version_set) ->
           OpamPackage.Version.Set.to_seq version_set
           |> Seq.map (OpamPackage.create name))
    |> Seq.concat

  let all_opam_files t =
    all_packages t |> Seq.map (fun package -> opam_file t ~package)
end

module Package_set = struct
  type t = OpamPackage.Version.Set.t

  let lookup_matching_formula repo ~name
      ~(version_formula : OpamFormula.version_formula) =
    match Repo.find_opt repo ~name with
    | None -> OpamPackage.Version.Set.empty
    | Some candidates ->
        OpamPackage.Version.Set.filter
          (OpamFormula.check_version_formula version_formula)
          candidates
end

module Version_table = struct
  type t = Package_set.t OpamPackage.Name.Map.t

  let empty = OpamPackage.Name.Map.empty

  let of_filtered_formula repo ~env
      ~(filtered_formula : OpamTypes.filtered_formula) : t =
    let formula = OpamFilter.filter_formula env filtered_formula in
    OpamFormula.fold_left
      (fun acc (name, version_formula) ->
        let package_set =
          Package_set.lookup_matching_formula repo ~name ~version_formula
        in
        (name, package_set) :: acc)
      [] formula
    |> OpamPackage.Name.Map.of_list

  let compatible_merge =
    OpamPackage.Name.Map.merge (fun _ a b ->
        match (a, b) with
        | None, None -> None
        | Some x, None | None, Some x -> Some x
        | Some a, Some b -> Some (OpamPackage.Version.Set.inter a b))

  let check_for_empty t =
    OpamPackage.Name.Map.to_seq t
    |> Seq.find (fun (_, versions) -> OpamPackage.Version.Set.is_empty versions)
    |> function
    | None -> ()
    | Some (name, _) ->
        failwith
          (Printf.sprintf "no candidate versions for package '%s'"
             (OpamPackage.Name.to_string name))
end

module Table = struct
  type t = Version_table.t OpamPackage.Map.t

  let of_repo (repo : Repo.t) ~f : t =
    Repo.all_opam_files repo
    |> Seq.map (fun opam ->
           let filtered_formula = f opam in
           let env =
             Switch_builder.Env.common
             |> Switch_builder.Env.extend "version"
                  (OpamVariable.S
                     (OpamFile.OPAM.version opam
                    |> OpamPackage.Version.to_string))
           in
           let package = OpamFile.OPAM.package opam in
           let version_table =
             Version_table.of_filtered_formula repo ~env ~filtered_formula
           in
           (package, version_table))
    |> OpamPackage.Map.of_seq

  let of_repository repository = of_repo (Repo.of_repository repository)
  let depends = of_repository ~f:OpamFile.OPAM.depends
  let conflicts = of_repository ~f:OpamFile.OPAM.conflicts
  let find_exn t ~package = OpamPackage.Map.find package t

  let compatible_versions_for_package_set t ~package_set =
    let version_table =
      OpamPackage.Set.to_seq package_set
      |> Seq.map (fun package -> find_exn t ~package)
      |> Seq.fold_left Version_table.compatible_merge Version_table.empty
    in
    Version_table.check_for_empty version_table;
    version_table
end

include Table

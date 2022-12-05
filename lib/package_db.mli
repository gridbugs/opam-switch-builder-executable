type t

val depends : Switch_builder.Repository.t -> t
val conflicts : Switch_builder.Repository.t -> t

val find_exn :
  t -> package:OpamPackage.t -> OpamPackage.Version.Set.t OpamPackage.Name.Map.t

val compatible_versions_for_package_set :
  t ->
  package_set:OpamPackage.Set.t ->
  OpamPackage.Version.Set.t OpamPackage.Name.Map.t

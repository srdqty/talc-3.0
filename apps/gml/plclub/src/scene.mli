
type u =
    Sphere of Program.v
  | Cube of Program.v
  | Cylinder of Program.v
  | Cone of Program.v
  | Plane of Program.v
  | Transform of Matrix.t
  | Union of u * u
  | Inter of u * u
  | Diff of u * u


let epsilon = 1e-5

let dtr = acos (-1.) /. 180.
let rtd = 180. /. acos (-1.)

let dcos t = cos (t *. dtr)
let dsin t = sin (t *. dtr)
let dtan t = tan (t *. dtr)
let dacos x = rtd *. acos x

let infinity = 1. /. 0.
let minus_infinity = -. 1. /. 0.

let max_float x y : float = if x >= y then x else y

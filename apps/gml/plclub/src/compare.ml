let _ =
  let img1 = Ppm.load Sys.argv.(1) in
  let img2 = Ppm.load Sys.argv.(2) in
  let w = Ppm.width img1 in
  let h = Ppm.height img1 in
  let s = ref 0 in
  let d = ref 0 in
  for j = 0 to h - 1 do
    for i = 0 to w - 1 do
      for k = 0 to 2 do
        let v = abs (Ppm.get img1 i j k - Ppm.get img2 i j k) in
        s := !s + v;
        d := max !d v
      done
    done
  done;
  Format.printf "Distance: %d@." !s;
  Format.printf "Largest difference: %d@." !d

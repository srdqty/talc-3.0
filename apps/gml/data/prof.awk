function profile_all(scenes,flag) {
   for(j in scene) {
    system("../build/raytrace_p " flag " " scene[j] ".gml");
    system("gprof -b ../build/raytrace_p > " scene[j] ".gprof"); 
    system("../build/gml_ip.exe " flag " " scene[j] ".gml");
    system("mv fmon.data " scene[j] ".fprof");
  }
}

BEGIN {
  scene[1] = "chess";
  scene[2] = "dice";
  scene[3] = "fractal";
  scene[4] = "go";
  scene[5] = "kal";
  scene[6] = "mandel";
  scene[7] = "snowgoon";
  scene[8] = "texture";
  scene[9] = "tree";

  start = systime();
  msg = strftime("Started at %m/%d/%Y %H:%M:%S",start);
  print msg;
  
  profile_all(scene,"");
  system("awk -f gprof_sum.awk *.gprof > stat/gprof.sum");
  system("awk -f fprof_sum.awk *.fprof > stat/fprof.sum");

  profile_all(scene,"-b");
  system("awk -f gprof_sum.awk *.gprof > stat/gprof_b.sum");
  system("awk -f fprof_sum.awk *.fprof > stat/fprof_b.sum");

  end = systime();
  msg = strftime("Ended at %m/%d/%Y %H:%M:%S",end);
  print msg;

}



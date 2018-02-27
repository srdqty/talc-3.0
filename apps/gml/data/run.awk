function one_run(exec,name,scene) {
      system("echo Logging " scene " " name " >> check.log ");
      system("../tm ./tm.log \"" scene " " name "\" ../build/" exec " " scene ".gml >> /dev/null");
      system("../build/compare " scene "_ref.ppm " scene ".ppm >> check.log 2>> check_err.log");

}

function gml_ir(flags) {
  return "gml_ir_" flags ".exe";
}

function one_stat_run(flags,scene) {
  fl = flags "s";
  one_run(gml_ir(fl), "gml_ir_" fl, scene);
  system("mv gml.log stat/gml_ir_" fl ".m");
}

BEGIN {

  flags[1] = "f";
#  flags[2] = "t";
#  flags[3] = "i";
#  flags[4] = "h";
#  flags[5] = "v";
#  flags[6] = "V";

#  scene[1] = "chess";
  scene[2] = "dice";
#  scene[3] = "fractal";
#  scene[4] = "go";
#  scene[5] = "kal";
#  scene[6] = "mandel";
#  scene[7] = "snowgoon";
#  scene[8] = "texture";
#  scene[9] = "tree";

  start = systime();
  msg = strftime("Started at %m/%d/%Y %H:%M:%S",start);
  print msg;

  for(j in scene) {
    sc = scene[j];
    one_run("raytrace","plclub",sc);      
    one_run("raytrace -b","plclub_b",sc); 
    one_run("gml_i.exe","gml_i",sc);      
    one_run("gml_i.exe -b", "gml_i_b", sc);

    for(i in flags) {
      one_run(gml_ir(flags[i]),"gml_ir_" flags[i],sc);
      one_stat_run(flags[i],sc);
    }

  }

  system("echo "" >> tm.log");

  end = systime();
  msg = strftime("Ended at %m/%d/%Y %H:%M:%S",end);
  print msg;

  
}




setAutoThreshold("Default");
//run("Threshold...");
//setThreshold(0, 1);
setOption("BlackBackground", true);
run("Convert to Mask");
run("Set Measurements...", "area limit redirect=None decimal=3");
run("Measure");

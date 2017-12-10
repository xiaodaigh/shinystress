# gen_nsw
source("src/0a_setup.r")
source("src/0c_stand_alone_gen_data.r")
set.seed(1)
system.time(indata <- gen_datatable_synthetic())
system.time(NSW_only <- indata[state == "NSW",])
fst::write.fst(NSW_only,"indata/NSW_only.fst")
# gen_nsw
source("src/0a_setup.r")
source("src/0c_stand_alone_gen_data.r")

i = 1
for(yr in 2017:2013) {
  for(mths in 12:1) {
    gen_datatable_synthetic2(N = 2e6/((1+0.04/12)^i), mk = yr*100+mths)
  }
}
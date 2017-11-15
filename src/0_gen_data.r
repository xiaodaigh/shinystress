library(data.table)
library(fst)

data_path <- "."


state_prob <- c(7,5,3,2,1.2,0.5,0.3,0.4)
risk_grade_prob <- 0.5*(1/2)^(0:19)

gen_datatable_synthetic <- function(N=1e7) {
  data.table(
    month = rep(201711,N),
    id3 = sprintf("id%010d",1:N), # id
    curr_bal =  rgamma(N,2,0.5)*500000, # numeric e.g. 23.5749
    int_rate = rep(0.05, N),
    risk_grade = sample(1:20, prob = risk_grade_prob),
    state = sample(c("NSW","VIC","QLD","WA","SA","TAS","NT","ACT"), replace = T, prob = state_prob/sum(state_prob))
  )[,risk_grade_pd := c(0.0001*(1.55)^(1:20))[risk_grade]]
}

system.time(res <- gen_datatable_synthetic())
pryr::object_size(res)

write.fst(res,"simulation.fst", 100)
system.time(read.fst("simulation.fst"))

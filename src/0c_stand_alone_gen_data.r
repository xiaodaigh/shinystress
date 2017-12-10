state_prob <- c(7,5,3,2,1.2,0.5,0.3,0.4)
risk_grade_prob <- 0.5*(1/2)^(0:19)
N = 2e6

gen_lvr <- function(N) {
  x = -rgamma(N,9,0.5)
  x = x - min(x)
  x = x/max(x)
  pmax(0.01, x)
}

gen_datatable_synthetic2 <- function(N=2e6, mk = 201711) {
  dt = data.table(
    month = rep(mk,N),
    account_id = sprintf("id%010d",1:N), # id
    curr_bal =  rgamma(N,2,0.5)*125000, # numeric e.g. 23.5749
    curr_lvr = gen_lvr(N),
    int_rate = rep(0.05, N),
    risk_grade = sample(1:20, N, replace = T, prob = risk_grade_prob),
    state = sample(c("NSW","VIC","QLD","WA","SA","TAS","NT","ACT"), N, replace = T, prob = state_prob/sum(state_prob)),
    default = sample(0:1, N, replace = T, prob = c(0.97, 0.03))
  )[,risk_grade_pd := c(0.0001*(1.575)^(1:20))[risk_grade]][, value := curr_bal/curr_lvr]
  fst::write_fst(dt, sprintf("synthetic_data/%d.fst", mk), compress=100)
}

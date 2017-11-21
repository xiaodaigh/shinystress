state_prob <- c(7,5,3,2,1.2,0.5,0.3,0.4)
risk_grade_prob <- 0.5*(1/2)^(0:19)
N = 2e6

gen_lvr <- function(N) {
  x = -rgamma(N,9,0.5)
  x = x - min(x)
  x = x/max(x)
  pmax(0.01, x)
}

gen_datatable_synthetic <- function(N=2e6) {
  data.table(
    month = rep(201711,N),
    account_id = sprintf("id%010d",1:N), # id
    curr_bal =  rgamma(N,2,0.5)*125000, # numeric e.g. 23.5749
    curr_lvr = gen_lvr(N),
    int_rate = rep(0.05, N),
    risk_grade = sample(1:20, N, replace = T, prob = risk_grade_prob),
    state = sample(c("NSW","VIC","QLD","WA","SA","TAS","NT","ACT"), N, replace = T, prob = state_prob/sum(state_prob)),
    default = sample(0:1, N, replace = T, prob = c(0.97, 0.03))
  )[,risk_grade_pd := c(0.0001*(1.575)^(1:20))[risk_grade]][, value := curr_bal/curr_lvr]
}

set.seed(1)
system.time(indata <- gen_datatable_synthetic())
system.time(WA_only <- indata[state == "WA",])
fst::write.fst(WA_only,"indata/WA_only.fst")
k = log(90/100/100)/log(indata[,mean(risk_grade_pd)])

indata[,risk_grade_pd := exp(k*log(risk_grade_pd))]

orig_normal <- indata[sample(1:.N,.N/5),]
write.fst(orig_normal,"orig/orig_normal.fst", 100)
orig_80lvr <- indata[curr_lvr <= 0.8,][sample(1:.N,.N/5),]
write.fst(orig_80lvr,"orig/orig_80lvr.fst", 100)


transition_table <- data.table(expand.grid(1:20,1:20))
setnames(transition_table, names(transition_table), c("risk_grade","risk_grade_to"))
transition_table[,tran_prob := {
  toto = risk_grade - risk_grade_to
  ifelse(risk_grade == risk_grade_to, 1, 
         ifelse(toto > 0, (0.04*(21-risk_grade))^toto, (0.02*risk_grade)^(-toto)))
}]

transition_table[,tran_prob := tran_prob/sum(tran_prob), risk_grade]
transition_table[order(risk_grade)]
transition_table[,sum(tran_prob), risk_grade] # checks passed

# save the data
system.time(fst::write.fst(indata, "indata/normal.fst", 100))
system.time(fst::write.fst(transition_table, "tran_tbl/normal.fst", 100))


# generate stressed transition table
transition_table <- data.table(expand.grid(1:20,1:20))
setnames(transition_table, names(transition_table), c("risk_grade","risk_grade_to"))
transition_table[,tran_prob := {
  toto = risk_grade - risk_grade_to
  ifelse(risk_grade == risk_grade_to, 1, 
         ifelse(toto > 0, (0.02*(21-risk_grade))^toto, (0.04*risk_grade)^(-toto)))
}]

transition_table[,tran_prob := tran_prob/sum(tran_prob), risk_grade]
transition_table[order(risk_grade)]
transition_table[,sum(tran_prob), risk_grade] # checks passed

system.time(fst::write.fst(transition_table, "tran_tbl/stress.fst", 100))
source("src/0a_setup.r")
source("src/1_functions.r")


system.time(indata[, risk_grade_to := apply_transition(risk_grade, transition_table)])
indata[order(risk_grade),.N,risk_grade]
indata[order(risk_grade),.N,risk_grade_to]

set.seed(1)
indata[order(risk_grade),.N, risk_grade]
indata[,mean(value)]

indata[,sum(curr_bal)]
system.time(indata2 <- indata %>% 
              simulate_one_period(1, stress_macro_tbl) %>% {.$indata} %>% 
              simulate_one_period(2, stress_macro_tbl) %>% {.$indata} %>% 
              simulate_one_period(3, stress_macro_tbl) %>% {.$indata} 
)
indata2[,sum(curr_bal)]
indata2[order(risk_grade),.N, risk_grade]
indata2[,.N]
indata2[,.N,default]
indata2[,mean(value)]

indata[,.N,default]
indata[,.N]
indata2 <- default_transition(indata)
indata2[,.N,default]
indata2[,.N]


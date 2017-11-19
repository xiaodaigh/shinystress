source("src/0_gen_data.r")
source("src/1_functions.r")

system.time(indata[, risk_grade_to := apply_transition(risk_grade, transition_table)])
indata[order(risk_grade),.N,risk_grade]
indata[order(risk_grade),.N,risk_grade_to]

set.seed(1)
simulate_one_period <- function(indata, period_id, macros_table) {
  indata2 <- indata %>% 
    default_transition %>% 
    churn_transition(0.1) %>% 
    {.[,risk_grade := apply_transition(risk_grade, transition_table)]} %>% 
    update_valuation(period_id, macros_table) %>% 
    {rbindlist(list(., originate_new_loans(orig_normal, 2e11 - 8e10)), use.names = T, fill = T)}
  
  new_losses <- indata2[default >= 2, ]
  print(new_losses[,sum(pmax(0, curr_bal - value))])
  
  list(indata = indata2[default < 2,], new_losses = new_losses)
}

stress_macro_tbl <- data.table(hpi = c(100, 90, 85, 80))

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


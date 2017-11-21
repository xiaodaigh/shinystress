apply_transition <- function(rg, transition_table) {
  tt1 <- transition_table[order(risk_grade_to),.(risk_grade_to, cumprob = cumsum(tran_prob)),risk_grade]
  mcf <- sapply(unique(tt1$risk_grade), function(rg) {
    cp <- tt1[risk_grade == rg, unique(sort(cumprob))]
    function(x) as.integer(cut(x, c(-Inf, cp)))
  })
  
  urg <- unique(rg)
  res <- copy(rg)
  sapply(urg, function(u) {
    ii <- which(u == rg)
    res[ii] <<- mcf[[u]](runif(length(ii)))
  })
  res
}

churn_transition <- function(indata, pct) {
  indata[runif(.N) >= pct, ]
}

default_transition <- function(indata) {
  indata2 <- copy(indata)[,default := ifelse(default>=1, default+1,  risk_grade_pd >= runif(.N))]
  indata2
}

update_valuation <- function(indata, macro_period_id, macros_table) {
  indata2 <- copy(indata)[,value := value * macros_table[macro_period_id+1, hpi]/macros_table[macro_period_id, hpi]]
  return(indata2)
}

originate_new_loans <- function(indata, target_dollar, sim_period_id) {
  # browser()
  mean_bal <- indata[,mean(curr_bal)]
  res <- indata[sample(1:.N, max(1,target_dollar/mean_bal), replace = T),]
  res[,orig_period_id := rep(sim_period_id, .N)]
  
  if(res[,sum(curr_bal) - target_dollar >= mean_bal]) {
    return(res)
  } else {
    return(rbindlist(list(res, originate_new_loans(indata, target_dollar - res[,sum(curr_bal)], sim_period_id))))
  }
}

simulate_one_period <- function(indata, sim_period_id, origdata, target_dollar, macros_table, transition_table) {
  #browser()
  indata2 <- indata %>% 
    default_transition %>% 
    churn_transition(0.1) %>% 
    {.[,risk_grade := apply_transition(risk_grade, transition_table)]} %>% 
    update_valuation(sim_period_id, macros_table) %>% 
    {rbindlist(list(., originate_new_loans(origdata, target_dollar, sim_period_id)), use.names = T, fill = T)}
  
  # this is used to indicate the latest
  indata2[,period_id := rep(sim_period_id,.N)]
  
  new_losses <- indata2[default >= 2, ]
  # print(new_losses[,sum(pmax(0, curr_bal - value))])
  
  list(indata = indata2[default < 2,], new_losses = new_losses)
}

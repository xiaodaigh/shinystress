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

update_valuation <- function(indata, period_id, macros_table) {
  indata2 <- copy(indata)[,value := value * macros_table[period_id+1, hpi]/macros_table[period_id, hpi]]
  return(indata2)
}

originate_new_loans <- function(indata, target_dollar) {
  # browser()
  mean_bal <- indata[,mean(curr_bal)]
  res <- indata[sample(1:.N, max(1,target_dollar/mean_bal), replace = T),]
  
  if(res[,sum(curr_bal) - target_dollar >= mean_bal]) {
    return(res)
  } else {
    return(rbindlist(list(res, originate_new_loans(indata, target_dollar - res[,sum(curr_bal)]))))
  }
}

# 
# orig_normal[,mean(curr_bal)]
# 
# a = originate_new_loans(orig_normal, 2e9)
# a[,sum(curr_bal)]
# 

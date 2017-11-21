indata <- fst::read.fst("indata//normal.fst",as.data.table = T)

# Your Data naturally follows some distribution
indata[order(risk_grade),.N,risk_grade][,.(risk_grade, dn = N/sum(N))]
indata[,lvr_bin := cut(curr_lvr, c(0,0.8, 1))]
indata[order(lvr_bin),.N,lvr_bin][,.(lvr_bin, dn = N/sum(N))]
indata[order(state),.N,state][,.(state, dn = N/sum(N))]


# For risk appettite setting, you may be asked how a portfolio with a particular distribution
# might perform

# e.g. VIC population grows so both NSW and VIC has 36% of portflio
# and risk_grade 10+ collectively has 10% of distribution
# and 50% of loans have LVR greater than 80%

# use dog breeder algorithms
# generate separate datasets that satisfies one of the criteria above
# append them together, now the new data has characteristics of all 3
# sample from this new dataset + a random sample from the original by repeating steps above

indata[, vic_nsw_other:= ifelse(state %in% c("NSW","VIC"), state, "Other")]
indata[, risk_grade10 := ifelse(risk_grade >= 10, "rg >= 10", "rg < 10")]

vicnsw_target <- data.table(vic_nsw_other = c("NSW","VIC","Other"), target = c(0.36, 0.36, 0.28))
risk_grade10_target <- data.table(risk_grade10 = c("rg >= 10", "rg < 10"), target = c(0.05,0.95))
lvr_bin_target <- data.table(lvr_bin = c("(0,0.8]", "(0.8,1]"), target = c(0.7,0.3))

sample_to_target <- function(indata, target) {
  lapply(1:nrow(target), function(i) {
    # browser()
    indata1 <- merge(indata, target[i,], all.y = T)
    ii = sample(nrow(indata1), target[i,target]*nrow(indata)/3, replace = T)
    indata1[ii,]
  }) %>% rbindlist(use.names = T)
}

benchmark_to_target <- function(target, indata) {
  x = sapply(1:nrow(target), function(i) {
    # browser()
    indata1 <- merge(indata, target[i,], all.y = T)
    nrow(indata1)/nrow(indata) - target[i,target]
  })
  sum(x^2)
}

sample_from_data <- indata
res <- 1
iter <- 0
last_res <- 2
while(last_res - res >= 0.00001 & iter < 100) {
  last_res <- res
  vs <- sample_to_target(sample_from_data, vicnsw_target)[,target := NULL]
  # vs[,.N, vic_nsw_other]
  rg <- sample_to_target(sample_from_data, risk_grade10_target)[,target := NULL]
  # rg[,.N, risk_grade10]s
  lvrbin <- sample_to_target(sample_from_data, lvr_bin_target)[,target := NULL]
  # lvrbin[,.N, lvr_bin]
  
  sample_from_data <- rbindlist(list(vs, rg, lvrbin),use.names = T,fill = T)
  res = sum(sapply(
    list(vicnsw_target, risk_grade10_target, lvr_bin_target), 
    benchmark_to_target, sample_from_data))
  print(res)
  # if (last_res - res >= 0.00001 & iter < 100) {
  #   rindata <- indata[sample(nrow(indata), nrow(indata)/3),]
  #   sample_from_data <- list(sample_from_data, rindata) %>% rbindlist(use.names = T)
  #   sample_from_data <- sample_from_data[sample(nrow(sample_from_data),nrow(indata))]
  #   # print(nrow(sample_from_data))
  # }
  
  iter = iter + 1
}
# sample_from_data
indata[,.N, vic_nsw_other][,.(vic_nsw_other, dn = N/sum(N))]
sample_from_data[,.N, vic_nsw_other][,.(vic_nsw_other, dn = N/sum(N))]

indata[,.N, risk_grade10][,.(risk_grade10, dn = N/sum(N))]
sample_from_data[,.N, risk_grade10][,.(risk_grade10, dn = N/sum(N))]

indata[,.N, risk_grade10][,.(risk_grade10, dn = N/sum(N))]
sample_from_data[,.N, lvr_bin][,.(lvr_bin, dn = N/sum(N))]

sample_from_data[,dplyr::n_distinct(account_id)]

fst::write.fst(sample_from_data, "indata/synthetic.fst", 100)

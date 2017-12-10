#6 


# library(disk.frame)
library(data.table)
library(future)
plan(multiprocess)
df = disk.frame("synthetic_data")


# i wnated to create a histroical transition matrix
nrow(df)# i only need one column

# random access to rows and columns
# and parallelised on all CPU cores
system.time(quick_freq <- df[default == 0 & month == 201712
               , by = risk_grade
               , .N
               , keep = c("default", "month", "risk_grade")]) # 1 second
quick_freq[order(risk_grade)]


pt = proc.time()
system.time(tmp <-  df[default == 0,.(month, account_id, risk_grade)
                       ,keep = c("month", 
                                "account_id", 
                                "risk_grade", "default")]) # 30 seconds

system.time(setkey(tmp, account_id, month)) # 2 seconds
system.time(tmp[order(month)
                , by = account_id
                , lag_risk_grade := lag(risk_grade)])

tran_tbl = tmp[order(lag_risk_grade, risk_grade)
               , by = .(month, lag_risk_grade, risk_grade)
               , .N] # negligible

tran_tbl
timetaken(pt)
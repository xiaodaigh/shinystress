

indata[,summary(curr_bal)]
indata[,summary(curr_lvr)]
indata[,summary(value)]
indata[order(risk_grade),mean(risk_grade_pd),risk_grade]

# pryr::object_size(indata)
# system.time(write.fst(indata,"simulation.fst", 100))
# system.time(read.fst("simulation.fst"))
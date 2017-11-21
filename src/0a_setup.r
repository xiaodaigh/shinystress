library(data.table)
library(fst)
library(magrittr)
library(dtplyr)
library(ggplot2)
library(digest)

data_path <- "."

stress_macro_tbl <- data.table(hpi = c(100, 90, 85, 80), `Unemp Rate` = c(0.05, 0.07, 0.065, 0.06), `Cash Rate` = c(1.5, 2.5, 2.5, 2.5)) 
fst::write.fst(stress_macro_tbl,"macro_tbl/HPI-drop-20.fst")

stress_macro_tbl <- data.table(hpi = c(100, 95, 90, 90), `Unemp Rate` = c(0.05, 0.06, 0.06, 0.06), `Cash Rate` = c(1.5, 2.5, 2.5, 2.5))
fst::write.fst(stress_macro_tbl,"macro_tbl/HPI-drop-10.fst")

stress_macro_tbl <- data.table(hpi = c(100, 80, 70, 65), `Unemp Rate` = c(0.05, 0.08, 0.10, 0.09), `Cash Rate` = c(1.5, 2.75, 2.5, 2.5))
fst::write.fst(stress_macro_tbl,"macro_tbl/HPI-drop-35.fst")

stress_macro_tbl <- data.table(hpi = c(100, 105, 110, 115), `Unemp Rate` = c(0.05, 0.05, 0.05, 0.05), `Cash Rate` = c(1.5, 1.5,1.5,1.5))
fst::write.fst(stress_macro_tbl,"macro_tbl/HPI-grow-5.fst")
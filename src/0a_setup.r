library(data.table)
library(fst)
library(magrittr)
library(dtplyr)
library(ggplot2)
library(digest)

data_path <- "."

stress_macro_tbl <- data.table(hpi = c(100, 90, 85, 80))
fst::write.fst(stress_macro_tbl,"macro_tbl/HPI-drop-20.fst")

stress_macro_tbl <- data.table(hpi = c(100, 95, 90, 90))
fst::write.fst(stress_macro_tbl,"macro_tbl/HPI-drop-10.fst")

stress_macro_tbl <- data.table(hpi = c(100, 80, 70, 65))
fst::write.fst(stress_macro_tbl,"macro_tbl/HPI-drop-35.fst")

stress_macro_tbl <- data.table(hpi = c(100, 105, 110, 115))
fst::write.fst(stress_macro_tbl,"macro_tbl/HPI-grow-5.fst")
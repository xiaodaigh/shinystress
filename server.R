#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

source("src/0a_setup.r")
source("src/1_functions.r")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  input_data <- reactive({
    res <- fst::read.fst(paste0("indata/", input$si_input_popn,".fst"), as.data.table = T)
    res[,period_id := 0]
    res[,orig_period_id := 0]
    return(res)
  })
  
  orig_data <- reactive({
    fst::read.fst(paste0("orig/", input$si_orig_popn,".fst"), as.data.table = T)
  })
  
  macro_tbl <- reactive({
    fst::read.fst(paste0("macro_tbl/", input$si_macro_tbl,".fst"), as.data.table = T)
  })
  
  tran_tbl <- reactive({
    fst::read.fst(paste0("tran_tbl/", input$si_tran_tbl,".fst"), as.data.table = T)
  })
  
  simulation_results <- reactive({
    # a digest used to cache the results
    di <- digest(list(input$si_input_popn, 
                 input$si_orig_popn, 
                 input$si_macro_tbl, 
                 input$si_tran_tbl,
                 input$slider_n_sim))
    
    if(input$cb_cache) {
      if(di %in% dir("cache")) {
        return(list(
          outdata = fst::read.fst(file.path("cache",di, "outdata.fst"), as.data.table = T),
          lossesdata = fst::read.fst(file.path("cache",di, "lossesdata.fst"), as.data.table = T)
        ))
      }
    }
    
    # either cache set to false or there is nothing cached
    indata <- input_data()
    lossesdata <- list()
    outdata <-list()
    orig_data1 <- orig_data()
    tran_tbl1 <- tran_tbl()
    macro_tbl1 <- macro_tbl()
    # browser()
    indata %>% 
      simulate_one_period(1, orig_data1, macro_tbl1, tran_tbl1) %>% {lossesdata[[1]] <<- .$new_losses; outdata[[1]] <<- .$indata; .$indata} %>% 
      simulate_one_period(2, orig_data1, macro_tbl1, tran_tbl1) %>% {lossesdata[[2]] <<- .$new_losses; outdata[[2]] <<- .$indata; .$indata} %>% 
      simulate_one_period(3, orig_data1, macro_tbl1, tran_tbl1) %>% {lossesdata[[3]] <<- .$new_losses; outdata[[3]] <<- .$indata; .$indata} 
      
    # browser()
    
    fnl_res <- list(outdata = rbindlist(outdata), lossesdata = rbindlist(lossesdata))
    
    ## save them as cache
    if(!dir.exists(file.path("cache",di))) {
      dir.create(file.path("cache",di))
    }
    fst::write.fst(fnl_res$outdata, file.path("cache",di, "outdata.fst"), 100)
    fst::write.fst(fnl_res$lossesdata, file.path("cache",di, "lossesdata.fst"), 100)
    
    # return
    return(fnl_res)
  })
  
  granular_simulation_losses <- reactive({
    simulation_results()$lossesdata
  })
  
  granular_simulation_out <- reactive({
    simulation_results()$outdata
  })
  
  loss_summary_tbl <- reactive({
    gsl = granular_simulation_losses()
    #browser()
    gsl[, .(losses = sum(pmax(0, curr_bal - value))), period_id]
  })
   
  output$lossesPlot <- renderPlot({
    ggplot(loss_summary_tbl()) + 
      geom_bar(aes(x = period_id, weight = losses)) + 
      xlab("Time (year)") + 
      ylab("Losses $")
  })
  
  output$lossestbl <- renderDataTable({
    lst <- copy(loss_summary_tbl())
    lst[,`Cumul Losses $m` := round(cumsum(losses/1000000))]
    lst[,`Losses $m` := round(losses/1000000)][, losses:=NULL]
  })
  
  observe({
    nn = names(granular_simulation_losses())
    updateSelectizeInput(session,"si_pivot_by", choices = nn, selected = "period_id")
  })
  
  losses_pivot_tbl <- reactive({
    gsl <- copy(granular_simulation_losses())
    gsl[, losses := pmax(0, curr_bal - value)]
    code = sprintf("gsl[,.(`Losses $m` = sum(losses)), .(%s)]", input$si_pivot_by %>% paste(collapse =","))
    eval(parse(text = code))
  })
  
  output$pivot_tbl <- renderDataTable({
    if(input$btn_pivot_by) {
      isolate({
        losses_pivot_tbl()
      })
    }
  })
  
  output$btn_dl_pivot_by <- downloadHandler(
    filename = function() {
      paste('losses-', input$si_pivot_by %>% paste(collapse =" "), '.csv', sep="")
    },
    content = function(con) {
      write.csv(losses_pivot_tbl(), con)
    }
  )
})

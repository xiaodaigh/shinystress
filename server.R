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
library(tidyr)

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
  
  observe({
    nn = names(input_data())
    updateSelectizeInput(session,"si_variables", choices = nn)
  })
  
  output$dt_freq <- renderDataTable({
    a <- prof_var_dt()
    if(is.null(a)) return(NULL)
    code = sprintf("a[,.N,%s]", input$si_variables)
    eval(parse(text = code))
  })
  
  prof_var_dt <- reactive({
    id = input_data()
    # browser()
    
    if(!input$si_variables %in% names(id)) {
      return(c())
    }
    setDT(id)
    a <- id[,input$si_variables, with = F]
  })
  
  prof_var <- reactive({
    a <- prof_var_dt()[[1]]
    a
  })
  
  output$plot_dn <- renderPlot({
    a <- prof_var()
    if(mode(a) == "numeric") {
      plot(density(a), main = sprintf("Distribution of %s", input$si_variables))
    } else {
      
    }
  })
  
  output$dt_summ_var <- renderDataTable({
    a <- prof_var()
    if(mode(a) == "numeric") {
      data.table(attr = names(summary(a)), values = fivenum(a))
    } else {
      table(a)
    }
  })
  
  simulation_results <- reactive({
    
    withProgress(message = 'Simulation in progress', value = 0, {
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
    
      
      indata %T>% 
        {incProgress(0/3, detail = paste("Simulating period ", 1))} %>% simulate_one_period(1, orig_data1, macro_tbl1, tran_tbl1) %>% {lossesdata[[1]] <<- .$new_losses; outdata[[1]] <<- .$indata; .$indata} %T>% 
        {incProgress(1/3, detail = paste("Simulating period ", 2))} %>% simulate_one_period(2, orig_data1, macro_tbl1, tran_tbl1) %>% {lossesdata[[2]] <<- .$new_losses; outdata[[2]] <<- .$indata; .$indata} %T>% 
        {incProgress(2/3, detail = paste("Simulating period ", 3))} %>% simulate_one_period(3, orig_data1, macro_tbl1, tran_tbl1) %>% {lossesdata[[3]] <<- .$new_losses; outdata[[3]] <<- .$indata; .$indata} 

    
      incProgress(5/6, detail = paste("Saving results"))
      # browser()
      
      fnl_res <- list(outdata = rbindlist(outdata), lossesdata = rbindlist(lossesdata))
      
      ## save them as cache
      if(!dir.exists(file.path("cache",di))) {
        dir.create(file.path("cache",di))
      }
      fst::write.fst(fnl_res$outdata, file.path("cache",di, "outdata.fst"), 100)
      fst::write.fst(fnl_res$lossesdata, file.path("cache",di, "lossesdata.fst"), 100)
      
      return(fnl_res)
    })
    # return
    
  })
  
  granular_simulation_losses <- reactive({
    simulation_results()$lossesdata
  })
  
  granular_simulation_out <- reactive({
    simulation_results()$outdata
  })
  
  loss_summary_tbl <- reactive({
    gsl = granular_simulation_losses()
    gsl[, .(curr_bal = sum(curr_bal), avg_lvr = mean(curr_lvr), avg_value = mean(value), losses = sum(pmax(0, curr_bal - value))), period_id]
  })
   
  output$lossesPlot <- renderPlot({
    withProgress(message = 'Plotting in progress', value = 0, {
      incProgress(1/2, detail = paste("Readying Plot"))
      gg = ggplot(loss_summary_tbl()) + 
        geom_bar(aes(x = period_id, weight = losses)) + 
        xlab("Time (year)") + 
        ylab("Losses $")
      incProgress(5/6, detail = paste("ready "))
      return(gg)
    })
  })
  
  output$lossestbl <- renderDataTable({
    lst <- copy(loss_summary_tbl())
    lst[,`Cumul Losses $m` := round(cumsum(losses/1000000))]
    lst[,`Losses $m` := round(losses/1000000)][, losses:=NULL][,avg_value:=NULL]
    lst[,`Exposure at Default`:= round(curr_bal/1000000)][,curr_bal:=NULL][,avg_lvr:=NULL]
  })
  
  observe({
    nn = names(granular_simulation_losses())
    updateSelectizeInput(session,"si_pivot_by", choices = nn, selected = "period_id")
  })
  
  losses_pivot_tbl <- reactive({
    gsl <- copy(granular_simulation_losses())
    gsl[, losses := pmax(0, curr_bal - value)]
    code = sprintf("gsl[,.(`Losses $m` = round(sum(losses)/1000000)), .(%s)]", input$si_pivot_by %>% paste(collapse =","))
    eval(parse(text = code))
  })
  
  
  output$tbl_macros <- renderDataTable({
    macro_tbl()
  })
  
  output$tbl_macros2 <- renderDataTable({
    fst::read.fst(paste0("macro_tbl/", input$si_macro_tbl2,".fst"), as.data.table = T)
  })
  
  output$plot_pd_grade_tran <- renderPlot({
    tt = fst::read.fst(paste0("tran_tbl/", input$si_tran_tbl2,".fst"), as.data.table = T)
    
    tt <- tt[risk_grade != risk_grade_to]

    p <- ggplot(data =  tt, aes(x = risk_grade_to, y = risk_grade)) +
      geom_tile(aes(fill = tran_prob), colour = "white") +
      geom_text(aes(label = sprintf("%.2f",tran_prob)), vjust = 1) +
      scale_fill_gradient(low = "white", high = "steelblue")
    
    p
  })
  
  output$tbl_pds <- renderDataTable({
    idata = input_data()
    idata[, .(risk_grade_pd = paste0(round(mean(risk_grade_pd),3)*100,"%")), risk_grade]
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

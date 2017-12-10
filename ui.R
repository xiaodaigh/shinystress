library(shiny)
library(shinydashboard)

dashboardPage(
  skin = "red",
  dashboardHeader(title = "ShinyStress Dashboard",
                  dropdownMenu(type = "notifications")
                  ),
  dashboardSidebar(
    sidebarMenu(
      # menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Simulation", tabName = "simulation", icon = icon("microchip")),
      menuItem("Probability of Default Model", tabName = "pd_tab", icon = icon("low-vision")),
      menuItem("Losses Pivot", tabName = "pivot", icon = icon("cube")),
      menuItem("Data Profile", tabName = "data_prof", icon = icon("area-chart")),
      menuItem("Settings", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
        fluidRow(
          box(
            title = "Portfolio at a glance",
            status = "primary",
            solidHeader = T,
            valueBoxOutput("infobox_accts"),
            plotOutput("At a glance", height = 250)),
          box(
            title = "PD Segment distribution",
            status = "primary",
            solidHeader = T)
        )
      ),
      tabItem(tabName="pd_tab"
        ,fluidRow(
          box(
            title = "Controls",
            status = "primary",
            solidHeader = T,
            shiny::selectizeInput("si_macro_tbl2", "Macro Forecasts", choices = sapply(dir("macro_tbl"), function(indatafile) {
              l = nchar(indatafile)
              substr(indatafile, 1, l-4)
            }, USE.NAMES = F))
            ,shiny::selectizeInput("si_tran_tbl2", "PD Grade Transition Table", choices = sapply(dir("tran_tbl"), function(indatafile) {
              l = nchar(indatafile)
              substr(indatafile, 1, l-4)
            }, USE.NAMES = F))
          ),box(
            title = "Macros",
            status = "primary",
            solidHeader = T,
            shiny::dataTableOutput("tbl_macros2")
          )
        )
        ,fluidRow(
          box(
            title = "PD Grade Transition",
            status = "primary",
            solidHeader = T,
            shiny::plotOutput("plot_pd_grade_tran")
          ),
          box(
            title = "Probability of default",
            status = "primary",
            solidHeader = T,
            shiny::dataTableOutput("tbl_pds")
          )
        )
      ),
      tabItem(tabName = "simulation",
        fluidRow(
          box(
            title = "Simulation Controls",
            status = "primary",
            solidHeader = T,
            shiny::selectizeInput("si_input_popn", "Input Population", choices = sapply(dir("indata"), function(indatafile) {
              l = nchar(indatafile)
              substr(indatafile, 1, l-4)
            }, USE.NAMES = F)),
            shiny::selectizeInput("si_orig_popn", "Origination Population", choices = sapply(dir("orig"), function(indatafile) {
              l = nchar(indatafile)
              substr(indatafile, 1, l-4)
            }, USE.NAMES = F)),
            shiny::textInput("ti_orig_dollar", "Orig. Size ($m) per period", value = (2e11 - 8e10)/1000000),
            shiny::selectizeInput("si_macro_tbl", "Macro Forecasts", choices = sapply(dir("macro_tbl"), function(indatafile) {
              l = nchar(indatafile)
              substr(indatafile, 1, l-4)
            }, USE.NAMES = F)),
            shiny::selectizeInput("si_tran_tbl", "PD Grade Transition Table", choices = sapply(dir("tran_tbl"), function(indatafile) {
              l = nchar(indatafile)
              substr(indatafile, 1, l-4)
            }, USE.NAMES = F)),
            shiny::sliderInput("si_workout_length", "Workout period length", 2, min = 1, max = 5)
          ),
          # box(
          #   title = "Macro-economics",
          #   status = "info",
          #   solidHeader = T,
          #   shiny::dataTableOutput("tbl_macros")
          # )
          box(
            title = "Losses Plot",
            status = "warning",
            solidHeader = T,
            plotOutput("lossesPlot", height = 250)
          )
        ),
        fluidRow(
          box(
            title = "Losses Table",
            status = "warning",
            solidHeader = T,
            shiny::dataTableOutput("lossestbl")
          )
        )
      ),
      tabItem(tabName = "pivot",
        fluidRow(
          box(
            selectizeInput("si_pivot_by","Pivot By", choices = "", multiple=T),
            actionButton("btn_pivot_by","Pivot By"),
            downloadButton("btn_dl_pivot_by", label = "Download Table", class = NULL)
          ),
          box(
            title = "Losses Pivot",
            status = "info",
            solidHeader = T,
            shiny::dataTableOutput("pivot_tbl")
          )
        )
      ),
      tabItem(tabName = "data_prof",
              h2("Data Profiles"),
              fluidRow(
                box(
                  title = "Variable",
                  status = "primary",
                  solidHeader = T,
                  shiny::selectizeInput("si_variables", "Input Variables", choices = "")
                ),
                box(
                  title = "Summary",
                  status = "info",
                  solidHeader = T,
                  shiny::dataTableOutput("dt_summ_var")
                )
              ),
              fluidRow(
                box(
                  title = "Frequency",
                  status = "info",
                  solidHeader = T,
                  shiny::dataTableOutput("dt_freq")
                ),
                box(
                  title = "Distribution",
                  status = "info",
                  solidHeader = T,
                  shiny::plotOutput("plot_dn")
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "widgets",
        h2("ShinyStress Settings"),
        fluidRow(
          box(
            title = "Cache Results",
            status = "primary",
            solidHeader = T,
            shiny::checkboxInput("cb_cache", "Use Cache", T)
          ),
          box(
            title = "Number Simulations",
            status = "primary",
            solidHeader = T,
            sliderInput("slider_n_sim", "Number of Simulation:", 0, 100, 1)
          )
        )
      )
    )
  )
)


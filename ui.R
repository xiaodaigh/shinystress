library(shiny)
library(shinydashboard)

dashboardPage(
  skin = "red",
  dashboardHeader(title = "ShinyStress dashboard",
                  dropdownMenu(type = "notifications")
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Simulation", tabName = "simulation", icon = icon("microchip")),
      menuItem("Pivot", tabName = "pivot", icon = icon("cube")),
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
            shiny::selectizeInput("si_orig_popn", "Origin Population", choices = sapply(dir("orig"), function(indatafile) {
              l = nchar(indatafile)
              substr(indatafile, 1, l-4)
            }, USE.NAMES = F)),
            shiny::selectizeInput("si_macro_tbl", "Macro Forecasts", choices = sapply(dir("macro_tbl"), function(indatafile) {
              l = nchar(indatafile)
              substr(indatafile, 1, l-4)
            }, USE.NAMES = F)),
            shiny::selectizeInput("si_tran_tbl", "PD Grade Transition Table", choices = sapply(dir("tran_tbl"), function(indatafile) {
              l = nchar(indatafile)
              substr(indatafile, 1, l-4)
            }, USE.NAMES = F))
          ),
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
            shiny::dataTableOutput("lossestbl"),
            width = 12
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


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
                  solidHeader = T,
                  plotOutput("At a glance", height = 250)),
                
                box(
                  title = "LGD Segment distribution",
                  status = "primary",
                  solidHeader = T,
                  plotOutput("At a glance", height = 250)
                )
              )
      ),
      
      tabItem(tabName = "simulation",
              fluidRow(
                box(
                  title = "Losses",
                  status = "primary",
                  solidHeader = T,
                  plotOutput("At a glance", height = 250)
                  )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content"),
              fluidRow(
                box(
                  title = "Losses",
                  status = "primary",
                  solidHeader = T,
                  plotOutput("At a glance", height = 250)
                ),
                
                box(
                  title = "Number Simulations",
                  sliderInput("slider_n_sim", "Number of Simulation:", 0, 100, 1)
                )
              )
      )
    )
  )
)


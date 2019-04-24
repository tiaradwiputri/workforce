#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(scales)
library(DT)

women <- read.csv("jobs_gender.csv")
t <- readRDS("theme2.RDS")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Women in  Workforce"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Industry Analysis", tabName = "industry")
    )
  ),
  dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
          box(width = 12,
              tags$style(".nav-tabs-custom .nav-tabs li.active {
                      border-color: #d63e2d;
                    }"),
              infoBox(title = "Year Range", 
                      value = paste(min(women$year), max(women$year), sep = "-"),
                      color = "navy",
                      icon = icon("chart-line")),
              infoBox(title = "Total Workforce",
                      value = comma(sum(women$total_workers)),
                      color = "navy",
                      icon = icon("users")),
              infoBox(title = "Total Industry",
                      value = length(levels(women$major_category)),
                      color = "navy",
                      icon = icon("industry")),
              tabBox(width = 12,
                    tabPanel("Industry Overview",
                              dataTableOutput("industrydt")
                             ),
                    tabPanel("Occupation Overview",
                             dataTableOutput("occupationdt"))
                  )
          )
        ),
        tabItem(tabName = "industry",
                box(width = 12,
                    fluidRow(
                      column(width = 6,
                             selectInput("industrySel1", label = "Pick an industry:", choices = levels(women$major_category))
                             ),
                      column(width = 6,
                             selectInput("industrySel2", label = "Pick an industry:", choices = levels(women$major_category), selected = levels(women$major_category)[2])    
                             )
                    ),
                    fluidRow(
                      column(width = 6,
                             plotlyOutput("industryComp1")
                             ),
                      column(width = 6,
                             plotlyOutput("industryComp2")
                             )
                    )
                )
        )
      )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$industrydt <- renderDataTable({
    dt <- women %>% 
      group_by(major_category) %>% 
      summarise(
        TotalWorkers = sum(total_workers, na.rm = T),
        TotalMen = sum(workers_male, na.rm = T),
        TotalWomen = sum(workers_female, na.rm = T),
        FemalePercentage = TotalWomen/TotalWorkers
      )
    
    dt %>% 
      datatable(options = list(paging = F, searching = F)) %>% 
      formatPercentage('FemalePercentage', 2) %>% 
      formatCurrency(c('TotalWorkers','TotalMen','TotalWomen'), currency = "", interval = 3, digits = 0)
      
  })
  
  output$occupationdt <- renderDataTable({
    dt <- women %>% 
      group_by(occupation) %>% 
      summarise(
        TotalWorkers = sum(total_workers, na.rm = T),
        TotalMen = sum(workers_male, na.rm = T),
        TotalWomen = sum(workers_female, na.rm = T),
        percent_female = TotalWomen/TotalWorkers
      )
  })
  
  output$industryComp1 <- renderPlotly({
    data <- women %>% 
      filter(major_category == input$industrySel1) %>% 
      group_by(year) %>% 
      summarise(
        TotalWomen = sum(workers_female, na.rm = T),
        TotalMen = sum(workers_male, na.rm = T)
      ) %>% 
      melt(id.vars = "year") 
      g <- ggplot(data, aes(x = year, y = value, text = sprintf("Value: %s", comma(value)))) +
        geom_line(aes(group = variable, color = variable), size = 1.3) +
        geom_point(aes(color = variable), size = 6) +
        labs(title = paste("Number of Workforce in",input$industrySel1 ,"Industry"),
             subtitle = "Men and Women comparison",
             color = "",
             y = "Number of Workforce",
             x = "Year") +
        t +
        scale_y_continuous(labels = scales::comma)
      ggplotly(g, tooltip = "text")
        
  })
  
  output$industryComp2 <- renderPlotly({
    data <- women %>% 
      filter(major_category == input$industrySel2) %>% 
      group_by(year) %>% 
      summarise(
        TotalWomen = sum(workers_female, na.rm = T),
        TotalMen = sum(workers_male, na.rm = T)
      ) %>% 
      melt(id.vars = "year") 
    g <- ggplot(data, aes(x = year, y = value, text = sprintf("Value: %s", comma(value)))) +
      geom_line(aes(group = variable, color = variable), size = 1.3) +
      geom_point(aes(color = variable), size = 6) +
      labs(title = paste("Number of Workforce in",input$industrySel2 ,"Industry"),
           subtitle = "Men and Women comparison",
           color = "",
           y = "Number of Workforce",
           x = "Year") +
      t +
      scale_y_continuous(labels = scales::comma)
    ggplotly(g, tooltip = "text")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


# load libraries
library(shiny)
library(rsconnect)
library(ggplot2)
library(dplyr)
library(plotly)

# authorization of the account
rsconnect::setAccountInfo(name='akyneal', token='27031347FE166FAC8380BE849FA46ADE', secret='lnyro+EJUmCTyGMsIJ/+Q0mRcjMEkOE/Ky0Sy7sb')

df <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv")

nation <- df %>%
  select(ICD.Chapter, Year, State, Deaths, Population, Crude.Rate) %>%
  mutate(Crude.Rate = round((sum(Deaths) / sum(Population)) * 10^5, 1)) %>%
  mutate(State = "National") %>%
  select(ICD.Chapter, Year, Crude.Rate, State) %>%
  group_by(ICD.Chapter, Year)

nation$State <- as.character(nation$State)

state <- df %>%
  select(ICD.Chapter, Year, Crude.Rate, State)

df2 <- union_all(state, nation)

# define layout with ui
ui <- fluidPage(
  headerPanel('State Mortality Rates'),
  sidebarPanel(
    selectInput('State', 'State', unique(df2$State), selected='NY'),
    selectInput('ICD.Chapter', 'Cause', unique(df2$ICD.Chapter), selected='Certain infectious and parasitic diseases')
  ),
  mainPanel(
    plotlyOutput('plot1')
  )
)

#define server function(plotting) with server
server <- function(input, output, session) {
  
  nationalData <- reactive({
    nation %>%
      filter(ICD.Chapter == input$ICD.Chapter)
  })
  
  statedata <- reactive({
    df3 <- df2 %>%
      filter(State == input$State, ICD.Chapter == input$ICD.Chapter)
  })
  
  combined <- reactive({
    merge(x = nationalData(), y = statedata(), all = TRUE)
  })
  
  output$plot1 <- renderPlotly({
    
    df3 <- df2 %>%
      filter(State == input$State, ICD.Chapter == input$ICD.Chapter)
    
    plot_ly(combined(), x = ~Year, y = ~Crude.Rate, color = ~State, type='scatter',
            mode = 'lines')
  })
  
}

shinyApp(ui = ui, server = server)
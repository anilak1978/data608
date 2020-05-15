# authorization of the account
rsconnect::setAccountInfo(name='akyneal', token='27031347FE166FAC8380BE849FA46ADE', secret='lnyro+EJUmCTyGMsIJ/+Q0mRcjMEkOE/Ky0Sy7sb')

#load libraries
library(shiny)              
library(tidyverse)          
library(scales)

#load the dataset locally
shiny_data <- read.csv("shiny_data.csv")

#define custom plotting


plotRevenue <- function(dataframe, factorVariable, topN=10) {
  var_col <- enquo(factorVariable)
  dataframe %>% group_by(!!var_col) %>% summarize(rev=sum(transactionRevenue)) %>% filter(rev>0) %>% top_n(topN, wt=rev) %>% ungroup() %>%
    ggplot(aes_(x=var_col, y=~rev, fill=var_col)) +
    geom_bar(stat='identity')+
    scale_y_continuous(labels=comma)+
    labs(x="", y="Revenues (USD)")+
    theme(legend.position="none")
}

# define UI layout for the shiny app

ui <- fluidPage(
  headerPanel('Acquisition by Channel'),
  sidebarPanel(
    selectInput('continent', 'continent', unique(shiny_data$continent),
                selected='Europe') #will display default continent
    
  ),
  mainPanel(
    htmlOutput(outputId = 'selection'),
    plotOutput('plot1', height="auto"),
    h6("Acquisition by Channel per Continent")
  )
)

# define server logic for data and input

server <- shinyServer(function(input, output, session) {
  selectedData <- reactive({
    shiny_data %>% filter(continent == input$continent)
  })
  
  output$selection <- renderText({
    paste('<b> Revenue for: </b>', input$continent)
  })
  
  output$plot1 <- renderPlot({
    
    plotRevenue(selectedData(), channelGrouping)
    
  }, height = function() {
    session$clientData$output_plot1_width}
  )
})

shinyApp(ui = ui, server = server)


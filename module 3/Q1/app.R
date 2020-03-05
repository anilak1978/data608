# load libraries
library(shiny)
library(rsconnect)
library(ggplot2)
library(dplyr)
library(plotly)
library(sqldf)
# authorization of the account
rsconnect::setAccountInfo(name='akyneal', token='27031347FE166FAC8380BE849FA46ADE', secret='lnyro+EJUmCTyGMsIJ/+Q0mRcjMEkOE/Ky0Sy7sb')

# load the dataset in R and review
df <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv")
# define UI layout for the shiny app

ui <- fluidPage(
  headerPanel('State Moratlity Rate by Cause'),
  sidebarPanel(
    selectInput('Cause', 'Cause', unique(df$ICD.Chapter),
                selected='Certain infectious and parasitic diseases') #will display default cause
    
  ),
  mainPanel(
    htmlOutput(outputId = 'selection'),
    plotOutput('plot1', height="auto"),
    h6("The Crude Mortality Rate across all States")
  )
)

# define server logic for data and input

server <- shinyServer(function(input, output, session) {
  selectedData <- reactive({
    df %>% filter(ICD.Chapter == input$Cause & Year == 2010 )
  })
  
  output$selection <- renderText({
    paste('<b> Crude rate for: </b>', input$Cause)
  })
  
  output$plot1 <- renderPlot({
    
    ggplot(selectedData(), aes(x=reorder(State, -Crude.Rate), y=Crude.Rate)) +
      geom_col(fill = "#ffa600") +
      coord_flip() +
      geom_text(aes(label=Crude.Rate),
                size=3,
                hjust=-0.2,
                color="#003f5c") +
      xlab("State") +
      ylab("Crude Rate") +
      theme(panel.background = element_blank())
  }, height = function() {
    session$clientData$output_plot1_width}
  )
})

shinyApp(ui = ui, server = server)
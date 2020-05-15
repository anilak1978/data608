# authorization of the account
rsconnect::setAccountInfo(name='akyneal', token='27031347FE166FAC8380BE849FA46ADE', secret='lnyro+EJUmCTyGMsIJ/+Q0mRcjMEkOE/Ky0Sy7sb')


library(shiny)              
library(tidyverse)          
library(scales)


#load the dataset locally
shiny_data <- read.csv("shiny_data.csv")

# set up date range
daterange_options <- list("Last 7 Days" = 7,
                          "Last 30 Days" = 30,
                          "Last 60 Days" = 60,
                          "Last 90 Days" = 90)

# set up metric options
metric_options <- list("Revenue" = "transactionRevenue",
                       "Bounces" = "bounces",
                       "visitNumber" = "visitNumber",
                       "Page Views" = "pageviews")

# set up dimension options

dimension_options <- list("Channel" = "channelGrouping",
                          "Device" = "deviceCategory")


# theme
default_theme <-   theme_bw() +
  theme(axis.text = element_text(face = "bold", size = 14, colour = "grey10"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = "none",
        legend.justification = "center",
        strip.text.x = element_text(face = "bold", size = 14, colour = "grey10"),
        strip.text.y = element_text(face = "bold", size = 14, colour = "grey10", angle = 180),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0,"in"),
        panel.background = element_rect(fill = NA, color = "gray90"))


# calculate the totals of whatever is selected in X
calc_dim_x_includes <- function(data, dim_count){
  group_by(data, dim_x) %>% summarise(total = sum(metric)) %>%
    arrange(-total) %>% top_n(dim_count, total) %>% select(dim_x)
}

# calculate the values in whatever is selected in Y
calc_dim_y_includes <- function(data, dim_count){
  group_by(data, dim_y) %>% summarise(total = sum(metric)) %>%
    arrange(total) %>% top_n(dim_count, total) %>% select(dim_y) 
}


#set up ui

ui <- fluidPage(
  
  
  
  # Application title
  titlePanel("New vs Return User Channel Acquisition"),
  
  tags$hr(),
  
  # Sidebar with the user-controllable inputs 
  sidebarLayout(
    sidebarPanel(
      
      # Horizontal line.
      tags$hr(style="border-color: #777777;"),
      
      # The date range dropdown, including a default value
      selectInput("daterange", label = "Select a date range:", 
                  choices = daterange_options, 
                  selected = 30),
      
      # Horizontal line.
      tags$hr(style="border-color: #777777;"),
      
      # The metric dropdown
      selectInput("metric", label = "Select a metric:", 
                  choices = metric_options, 
                  selected = "transactionRevenue"),
      
      # Horizontal line.
      tags$hr(style="border-color: #777777;"),
      
      # The dimension selector (dropdown) for the X-axis
      selectInput("x_dim", label = "Select the X dimension and how many values to show:", 
                  choices = dimension_options, 
                  selected = "channelGrouping"),
      
      # Select the max number of values to show in the X axis
      sliderInput("dim_x_count",
                  label = NULL,
                  min = 1,
                  max = 10,
                  value = 6),
      
      # Horizontal line.
      tags$hr(style="border-color: #777777;"),
      
      # The dimension selector (dropdown) for the X-axis
      selectInput("y_dim", label = "Select the Y dimension and how many values to show:", 
                  choices = dimension_options, 
                  selected = "deviceCategory"),
      
      # Select the max number of values to show in the Y dimension
      sliderInput("dim_y_count",
                  label = NULL,
                  min = 1,
                  max = 10,
                  value = 5)
    ),
    
    # Show the heatmap and sparklines
    mainPanel(
      tags$h4("Metric Totals"),
      plotOutput("heatmap"),
      tags$hr(),
      tags$h4("Daily Trend"),
      plotOutput("sparklines")
    )
  )
)



server <- function(input, output) {
  
  base_data <- reactive({
    
    # Calculate the start and end dates.
    start_date <- shiny_data$Date-as.numeric(input$daterange)-1
    end_date <- shiny_data$Date-1
    
    dimensions <- c("date",input$x_dim, input$y_dim)
    
    # set the data.
    ga_data <- shiny_data %>%
      filter(shiny_data$Date > start_date & shiny_data$Date < end_date)
    
    
    colnames(ga_data) <- c("date","dim_x","dim_y","metric")

    ga_data
    
  })
  
  # We want to get the "top X" values for each dimension.
  dim_x_includes <- reactive({calc_dim_x_includes(base_data(), input$dim_x_count)})
  dim_y_includes <- reactive({calc_dim_y_includes(base_data(), input$dim_y_count)})
  
  # Build the heatmap
  output$heatmap <- renderPlot({
    
    x_includes <- dim_x_includes()
    y_includes <- dim_y_includes()
    
    plot_data <- base_data()
    
    # Filter the data down to just include the "top X" dimensions.
    plot_totals <- plot_data %>% as.data.frame() %>% 
      filter(dim_x %in% x_includes$dim_x, dim_y %in% y_includes$dim_y) %>%
      group_by(dim_x, dim_y) %>% 
      summarise(total = sum(metric))
    
    # Make the totals heatmap
    ggplot(plot_totals, aes(dim_x, dim_y)) + 
      geom_tile(aes(fill = total), color="white", size = 1) +
      scale_fill_gradient(low = "white", high = "green") +
      scale_x_discrete(limits = x_includes$dim_x) +
      scale_y_discrete(limits = y_includes$dim_y) +
      geom_text(aes(label = comma(total)), size = 7) +
      default_theme +
      theme(panel.border = element_rect(fill=NA, colour = "white"))
  })
  
  output$sparklines <- renderPlot({
    
    
    x_includes <- dim_x_includes()
    y_includes <- dim_y_includes()
    
    plot_data <- base_data()
    
    plot_trends <- filter(plot_data, dim_x %in% x_includes$dim_x, dim_y %in% y_includes$dim_y)
    
    # We need to change dim1 and dim2 to be factors.
    plot_trends$dim_x <- factor(plot_trends$dim_x,
                                levels = as.character(x_includes$dim_x))
    
    plot_trends$dim_y <- factor(plot_trends$dim_y,
                                levels = as.character(rev(y_includes$dim_y)))
    
    ggplot(plot_trends, aes(date, metric)) +
      geom_line() +
      facet_grid(dim_y~dim_x,
                 switch = "both") +
      default_theme +
      theme(axis.text = element_blank())
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

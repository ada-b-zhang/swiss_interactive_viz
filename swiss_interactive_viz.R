library(dplyr)
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Study the Relationship between Variables in R's Swiss Dataset"),
  
  # Add headings
  div(
    h3("An Application for Interactive Data Visualization", style = "color: black;"),
    style = "text-align: left;"
  ), 
  div(
    h4("by Ada Zhang", style = "color: darkorchid;"),
    style = "text-align: left;"
  ),
  div(
    h5("For more information on R's swiss dataset: ", 
       tags$a("Click here", href = "https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/swiss", target = "_blank"),
       style = "color: black;"),
    style = "text-align: left;"
  ),
  
  # Add emojis
  div(
    HTML('<span style="font-size: 100px;">&#129328;</span>'),  
    HTML('<span style="font-size: 100px;">&#128118;</span>'),  
    HTML('<span style="font-size: 100px;">&#127868;</span>'),  
    style = "text-align: left;"
  ), 
  sidebarLayout(
    sidebarPanel(
      selectInput("variable1", "Select 1st Variable", choices = c("Fertility", "Agriculture", "Examination", "Education", "Catholic", "Infant.Mortality")),
      selectInput("variable2", "Select 2nd Variable", choices = c("Fertility", "Agriculture", "Examination", "Education", "Catholic", "Infant.Mortality")),
      selectInput("point_type", "Select Point Type", choices = c("circle", "square", "triangle", "diamond", "cross")),
      selectInput("bin_size", "Select Bin Size", choices = c("1", "5", "10", "20", "30", "40", "50")),
      selectInput("line_type", "Select Line Type", choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")),
      selectInput("plot_color", "Select Plot Color", choices = c("purple", "magenta", "violet", "mediumpurple", "mediumorchid4", "magenta3", "darkorchid1")),
      actionButton("filterButton", "Filter")
    ),
    mainPanel(
      plotOutput("plot1"),
      plotOutput("plot2"),
      plotOutput("plot3")
    )
  )
)

server <- function(input, output) {
  data <- shiny::reactive({swiss})
  
  filtered_data <- shiny::reactive({
    variable1 <- input$variable1
    variable2 <- input$variable2
    
    # Check if either variable selection is null, and return NULL if so
    if (is.null(variable1) || is.null(variable2)) {
      return(NULL)
    }
    filtered <- data()  # Store the filtered data
    filtered
  })
  
  observeEvent(input$filterButton, {
    filtered <- filtered_data()
    
    selected_color <- input$plot_color
    selected_point_type <- input$point_type
    selected_bin_size <- as.numeric(input$bin_size)
    selected_line_type <- input$line_type
    
    output$plot1 <- renderPlot({
      ggplot(filtered, aes(x = !!sym(input$variable1), y = !!sym(input$variable2))) +
        theme_light() + 
        geom_point(shape = selected_point_type, color = selected_color, size = 3) +
        geom_smooth(method=lm, color="black") + 
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = "Scatterplot of Variable 1 & Variable 2", x = input$variable1, y = input$variable2)
    })
    
    output$plot2 <- renderPlot({
      ggplot(filtered, aes(x = !!sym(input$variable1))) +
        theme_light() + 
        geom_histogram(fill = selected_color, bins = selected_bin_size, color="black") +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = "Histogram of Variable 1", x = input$variable1)
    })
    
    output$plot3 <- renderPlot({
      ggplot(filtered, aes(x = !!sym(input$variable1), y = !!sym(input$variable2))) +
        theme_light() + 
        geom_line(linetype = selected_line_type, color = selected_color) +
        geom_point(color="black") + 
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = "Line Plot of Variable 1 and Variable 2", x = input$variable1, y = input$variable2)
    })
  })
}

shinyApp(ui, server)


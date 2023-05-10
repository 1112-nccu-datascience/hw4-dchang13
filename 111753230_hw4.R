# Load necessary packages
library(shiny)
library(shinythemes)
library(factoextra)
library(FactoMineR)
library(tidyverse)
library(ggbiplot)
library(ca)
# Load data
data(iris)

# Define UI for the Shiny app
ui <- fluidPage(
  
  # Set theme
  theme = shinytheme("cerulean"),
  
  # Set page title
  titlePanel("Iris Dataset Analysis"),
  
  # Define sidebar layout with tabs
  sidebarLayout(
    sidebarPanel(
      
      wellPanel(
        h4("Basic Information"),
        p("Name: 張義猷"),
        p("Department: 資科計一"),
        p("Student Number: 111753230")
      ),
      # Add options for PCA and CA analyses
      tabsetPanel(
        tabPanel("PCA Analysis",
                 selectInput(inputId = "pca_x_input",
                             label = "Select X Principal Component:",
                             choices = c("PC1", "PC2", "PC3", "PC4"),
                             selected = "PC1"),
                 selectInput(inputId = "pca_y_input",
                             label = "Select Y Principal Component:",
                             choices = c("PC1", "PC2", "PC3", "PC4"),
                             selected = "PC2")
        ),
        
        tabPanel("CA Analysis",
                 selectInput(inputId = "ca_x_input",
                             label = "Select X Variable:",
                             choices = colnames(iris)),
                 selectInput(inputId = "ca_y_input",
                             label = "Select Y Variable:",
                             choices = colnames(iris))
        ),
        tabPanel("Extra Visualizations",
                 selectInput(inputId = "extra_input",
                             label = "Select Plot Type:",
                             choices = c("Boxplot", "Density Plot", "Scatter Plot"),
                             selected = "Boxplot")
        )
      )
    ),
    
    # Define main panel layout
    mainPanel(
      
      # Add outputs for PCA and CA analyses
      tabsetPanel(
        tabPanel("PCA Analysis",
                 plotOutput(outputId = "pca_plot"),
                 tableOutput(outputId = "pca_table")
        ),
        
        tabPanel("CA Analysis",
                 plotOutput(outputId = "ca_plot"),
                 tableOutput(outputId = "ca_table")
        ),
        tabPanel("Extra Visualizations",
                 plotOutput(outputId = "extra_plot")
        ),
        tabPanel("Data",
                 tableOutput(outputId = "data_table")
        )
      )
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output) {
  
  # 顯示數據
  data_all <- reactive({
    iris
  })
  output$data_table <- renderTable({
    data_all()
  })
  
  # PCA analysis
  pca_data <- reactive({
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]
    ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
    x_var <- switch(input$pca_x_input,
                    "PC1" = ir.pca$x[, 1],
                    "PC2" = ir.pca$x[, 2],
                    "PC3" = ir.pca$x[, 3],
                    "PC4" = ir.pca$x[, 4])
    
    y_choices <- c("PC1", "PC2", "PC3", "PC4")
    y_choices <- y_choices[y_choices != input$pca_x_input]
    y_var <- switch(input$pca_y_input,
                    "PC1" = ir.pca$x[, 1],
                    "PC2" = ir.pca$x[, 2],
                    "PC3" = ir.pca$x[, 3],
                    "PC4" = ir.pca$x[, 4])
    data.frame(x_var, y_var, ir.species)
  })
  
  output$pca_plot <- renderPlot({
    pca_data() %>% ggplot(aes(x = x_var, y = y_var, color = ir.species)) +
      geom_point() + labs(x = input$pca_x_input, y = input$pca_y_input) +
      ggtitle("PCA Biplot")
  })
  
  output$pca_table <- renderPrint({
    summary(prcomp(log(iris[, 1:4]), center = TRUE, scale. = TRUE))
  })
  
  # CA analysis
  output$ca_plot <- renderPlot({
    mydata <- iris[, c(input$ca_x_input, input$ca_y_input)]
    mytable <- with(mydata, table(mydata[[1]], mydata[[2]]))
    
    prop_table_row <- prop.table(mytable, 1)
    prop_table_col <- prop.table(mytable, 2)
    
    fit <- ca(mytable)
    
    plot(fit, mass = TRUE, contrib = "absolute", map = "rowgreen", arrows = c(FALSE, TRUE))
  })
  
  # Extra visualizations
  output$extra_plot <- renderPlot({
    if(input$extra_input == "Boxplot"){
      boxplot(iris[,1:4], main = "Boxplot of Iris Dataset",
              col = c("#00AFBB", "#E7B800", "#FC4E07")[iris$Species])
    }
    else if(input$extra_input == "Density Plot"){
      par(mfrow = c(2,2))
      for(i in 1:4){
        dens <- density(iris[,i])
        plot(dens, main = colnames(iris)[i])
      }
    }
    else{
      pairs(iris, col = c("#00AFBB", "#E7B800", "#FC4E07")[iris$Species],
            pch = 21, bg = c("#00AFBB", "#E7B800", "#FC4E07")[iris$Species])
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server) 

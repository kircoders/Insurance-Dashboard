library(shiny)
library(shinythemes)
library(ggplot2)

# Load your dataset
insurance <- read.csv("insurance.csv")
colnames(insurance) <- tolower(colnames(insurance))

# Define UI
ui <- fluidPage(
  theme = shinytheme("superhero"),
  
  titlePanel("Insurance Charges Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var",
                  "Choose a Predictor Variable:",
                  list("Age" = "age",
                       "BMI" = "bmi",
                       "Children" = "children")),
      hr(),
      tableOutput("regout"),
      hr(),
      textInput(
        "inputval",
        "Enter a value to predict charges:",
        placeholder = "e.g., 35"
      ),
      textOutput("prediction")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter Plot", plotOutput("scatterPlot")),
        tabPanel("Residual Plot", plotOutput("resPlot"))
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    x <- insurance[[input$var]]
    ggplot(insurance, aes(x = x, y = charges)) +
      geom_point(color = "cyan", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "white") +
      labs(
        x = input$var,
        y = "Charges",
        title = paste("Charges vs", input$var)
      )
  })
  
  output$resPlot <- renderPlot({
    x <- insurance[[input$var]]
    fit <- lm(charges ~ x, data = insurance)  # FIXED
    res <- residuals(fit)
    ggplot(data.frame(x = x, res = res), aes(x = x, y = res)) +
      geom_point(color = "orange", size = 3) +
      labs(
        x = input$var,
        y = "Residuals",
        title = paste("Residuals vs", input$var)
      )
  })
  
  output$regout <- renderTable({
    x <- insurance[[input$var]]
    fit <- lm(charges ~ x, data = insurance)  # FIXED
    out <- summary(fit)
    s <- fit$coefficients[2]
    r <- out$r.squared
    t <- out$coefficients[2, 3]
    p <- out$coefficients[2, 4]
    df <- data.frame(
      Statistic = c("Slope", "R-Squared", "t-Statistic", "p-Value"),
      Value = c(s, r, t, p)
    )
    df
  })
  
  output$prediction <- renderText({
    val <- as.numeric(input$inputval)
    if (is.na(val)) return("Enter a numeric value above.")
    x <- insurance[[input$var]]
    fit <- lm(charges ~ x, data = insurance)  # FIXED
    pred <- predict(fit, newdata = data.frame(x = val))
    paste("Predicted Charges: $", round(pred, 2))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

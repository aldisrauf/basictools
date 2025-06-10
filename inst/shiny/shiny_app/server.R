server <- function(input, output) {
  output$distPlot <- shiny::renderPlot({
    data <- rnorm(input$n)
    hist(data, col = "skyblue", border = "white", main = "Histogram Data Acak")
  })
}

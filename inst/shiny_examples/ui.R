ui <- shiny::fluidPage(
  shiny::titlePanel("Contoh Sederhana Shiny"),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::sliderInput("n",
                         "Jumlah Observasi:",
                         min = 10,
                         max = 500,
                         value = 100)
    ),

    shiny::mainPanel(
      shiny::plotOutput("distPlot")
    )
  )
)

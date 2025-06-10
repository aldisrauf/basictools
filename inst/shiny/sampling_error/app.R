
install_load <- function (package1, ...)  {

  # convert arguments to vector
  packages <- c(package1, ...)

  # start loop to determine if each package is installed
  for(package in packages){

    # if package is installed locally, load
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))

    # if package is not installed locally, download, then load
    else {
      install.packages(package)
      do.call("library", list(package))
    }
  }
}


install_load("shiny") #shiny
install_load("shinythemes") #tema shiny
install_load("readxl") #read_xlsx
install_load("haven") #read_sav
install_load("dplyr") #buat select
install_load("ggplot2") #buat bikin plot
install_load("rhandsontable") #sudah tidak dipakai, dulunya untuk edit tabel di aplikasi
install_load("DT") #tampilan tabel biar bagus
install_load("sae") #sae
install_load("olsrr") #untuk stepwise
install_load("corrplot") #gajadi dipakai
install_load("shinycssloaders") #untuk loading
install_load("rmarkdown") #untuk output pdf dkk
install_load("gridExtra") #untuk pdf
install_load("grid") #untuk pdf
install_load("gtable") #untuk pdf yang bagus
install_load("tinytex") #untuk pdf
install_load("ggpubr")  #untuk tampilan plot
install_load("ggthemes") #untuk tampilan plot
install_load("shinyWidgets") #tampilan select dkk
install_load("ggcorrplot") #untuk bikin corrplot
install_load("shinyLP") #jumbotron
install_load("shinyBS") #nampilin modal tutorial
install_load("broom") #rapiin hasil sw
install_load("shinyjs") #toggle rlb



# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <-
  tagList(
    tags$style(type = 'text/css',
               '.navbar {
               font-size: 15px;}',

               '.dropdown-menu {
               font-size: 15px;}'
    ),

    useShinyjs(),

    navbarPage(
      title = div(
        img(src="logo.png", height = "40px",
            style = "position: relative; margin-top: -14px"),
        "SAEBENCH"
      ),
      theme = shinytheme("flatly"),

      tabPanel(
        title = "Home",
        icon = icon("home",
                    lib = "glyphicon"),
        jumbotron("Estimasi Sampling Error",
                  "Aplikasi Shiny Untuk Estimasi Sampling Error",
                  buttonLabel = "Tutorial"),
        bsModal("modalExample",
                "Video Tutorial", "tabBut", size = "large" ,
                p("Tonton video berikut untuk melihat panduan penggunaan aplikasi"),
                iframe(width = "560", height = "315",  url_link = "https://www.youtube.com/embed/0fEN9pKYrlU")
        ),
        wellPanel(
          div(
            h3("Overview:"),
            br(),
            withMathJax(),
            p("Aplikasi ini berisi penghitungan SAE dengan menggunakan beberapa metode benchmark. Berikut ulasan singkat mengenai benchmark:"),
            p("Misalkan  \\(\\theta_{i} \\) adalah nilai rata-rata untuk wilayah \\( i \\) dan \\(\\theta_{+} = \\sum_{i=1}^{m} W_{i} \\theta_{i}\\) adalah agregasi rata-rata, di mana
              \\(W_{i} = \\frac{N_{i}}{N} \\) adalah penimbang untuk tiap area. Setiap area tersampel, maka \\(\\sum_{i=1}^{m} W_{i} = 1 \\)
              . Misalkan suatu estimasi langsung \\(\\hat\\theta_{+} = \\sum_{i=1}^{m} W_{i} \\hat\\theta_{i}\\) dari \\(\\hat\\theta_{+} \\)
              bersifat reliable, maka estimasi area kecil \\(\\theta^{H}_{i} \\) diharapkan agar saat diagregasikan, akan sesuai dengan estimasi langsung yang sudah reliable
              \\(\\hat\\theta_{+} \\) (Rao dan Molina, 2015). $$ \\hat\\theta_{+} - \\sum_{i=1}^{m} W_{i} \\hat\\theta^{H}_{i} = \\sum_{i=1}^{m} W_{i} (1 - \\hat\\gamma_{i}) (\\hat\\theta_{i} - x_{i} \\hat\\beta) \\neq 0 $$
              Maka dibutuhkan modifikasi pada \\(\\hat\\theta^{H}_{i} \\) agar benchmarking dapat dilakukan.
              "),

            p("Berikut metode benhcmark yang tersedia dalam aplikasi ini: "),
            tags$ul(
              HTML(
                "<li> <p> Ratio Benchmark </p> </li>
                <li> <p> Different Benchmark </p> </li>
                <li> <p> Optimal Benchmark </p> </li>
                <li> <p> You-Rao Benchmark </p> </li>
                <li> <p> Wang-Fuller-Qu Benchmark </p> </li>


                "
              )
            ),
            p("Dalam aplikasi ini pun tersedia beberapa menu tambahan seperti visualisasi data dan penghitungan Regresi Linear Berganda.")
          )
        )
      ),

      tabPanel(
        title = "Data",
        icon = icon("list-alt",
                    lib = "glyphicon"),
        sidebarLayout(
          sidebarPanel(
            wellPanel(
              prettyRadioButtons(
                inputId = "jenisFile",
                label = "Pilih jenis file yang ingin diinput",
                choices = c("CSV (comma delimited)"="csv",
                            "XLSX"="excel",
                            "SPSS"="spss"),
                selected = "csv",
                shape = "curve",
                icon = icon("check"),
                animation = "tada"
              ),

              fileInput(
                inputId = "file",
                label = "Upload file yang ingin diinput",
                accept = c(
                  'text/csv',
                  'text/comma-separated-values,text/plain',
                  '.csv',
                  '.xlsx',
                  '.xls',
                  '.sav'
                )
              )
            )

          ),
          mainPanel(
            dataTableOutput("dataInput") %>% withSpinner(color="#0dc5c1", type = 8)
          )
        )
      ),

      navbarMenu(
        title = "Visualisasi",
        icon = icon("signal",
                    lib = "glyphicon"),
        tabPanel(
          title = "Univariate",
          wellPanel(
            h3("Plot Histogram"),
            "Klik untuk memilih variabel",
            dropdownButton(
              selectInput(
                inputId = "sel_plotHisto",
                label = "Pilih variabel",
                choices = NA
              ),
              circle = TRUE, status = "primary",
              icon = icon("gear"), width = "300px"
            ),
            plotOutput("plotHisto") %>% withSpinner(color="#0dc5c1", type = 8)
          ),

          wellPanel(
            h3("Plot Density"),
            "Klik untuk memilih variabel",
            dropdownButton(
              selectInput(
                inputId = "sel_plotDensity",
                label = "Pilih variabel",
                choices = NA
              ),
              circle = TRUE, status = "primary",
              icon = icon("gear"), width = "300px"
            ),
            plotOutput("plotDensity") %>% withSpinner(color="#0dc5c1", type = 8)
          )
        ),

        tabPanel(
          title = "Multivariate",
          wellPanel(
            h3("Boxplot"),
            "Klik untuk memilih variabel",
            dropdownButton(
              uiOutput("ui_sel_plotBox"),
              circle = TRUE, status = "primary",
              icon = icon("gear"), width = "300px"
            ),
            plotOutput("plotBox") %>% withSpinner(color="#0dc5c1", type = 8)
          ),

          wellPanel(
            h3("Scatter Plot"),
            "Klik untuk memilih variabel",
            dropdownButton(
              selectInput(
                inputId = "sel_plotScatter_1",
                label = "Pilih variabel Y",
                choices = NA
              ),
              selectInput(
                inputId = "sel_plotScatter_2",
                label = "Pilih variabel X",
                choices = NA
              ),
              circle = TRUE, status = "primary",
              icon = icon("gear"), width = "300px"
            ),
            plotOutput("plotScatter") %>% withSpinner(color="#0dc5c1", type = 8)
          ),

          wellPanel(
            h3("Plot Korelasi"),
            "Klik untuk memilih variabel",
            dropdownButton(
              uiOutput("ui_sel_plotCorr"),
              circle = TRUE, status = "primary",
              icon = icon("gear"), width = "300px"
            ),
            plotOutput("plotCorr") %>% withSpinner(color="#0dc5c1", type = 8)
          )
        )
      )
    )
  )


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {


  # IMPORT DATA -------------------------------------------------------------

  #Membaca data browse
  dataBrowse <- reactive({
    inFile <- input$file
    req(inFile)
    if(input$jenisFile=="csv"){
      if(substr(inFile$datapath,nchar(inFile$datapath)-2,nchar(inFile$datapath)) =="csv"){
        d<<-read.csv(inFile$datapath, header = TRUE)
      }else{
        d<<-data.frame()
      }
    }else{
      if(input$jenisFile=="excel"){
        if(substr(inFile$datapath,nchar(inFile$datapath)-3,nchar(inFile$datapath))=="xlsx"){
          d<<-read_xlsx(inFile$datapath)
        }else{
          d<<-data.frame()
        }

      }else{
        if(input$jenisFile=="spss"){
          if(substr(inFile$datapath,nchar(inFile$datapath)-2,nchar(inFile$datapath))=="sav"){
            d<<-read_sav(inFile$datapath)
          }else{
            d<<-data.frame()
          }

        }
      }
    }
    d<<-data.frame(d)

    d
  })

  #Output table dari data browse
  output$dataInput<-renderDataTable({
    df<-dataBrowse()
    rownames(df)<-1:nrow(df)
    DT::datatable(df,
                  options = list(
                    "scrollY"="500px",
                    "scrollX"=TRUE,
                    "searching"=FALSE,
                    "paging"=FALSE
                  ))
  })

  myData<-reactive({
    dataOn<<-dataBrowse()
    dataOn
  })



  # PILIHAN PADA SELECT INPUT -----------------------------------------------

  #Memanggil variabel numerik saja
  varName<-reactive({
    df<-myData()
    if(nrow(df)==0){
      NA
    }else{
      colnames(df)[sapply(df, is.numeric)]
    }
  })

  #Menghilangkan variabel yang sudah terpilih pada suatu select input
  varSisa<-function(x, values){
    terpilih <- as.logical(match(x, values, nomatch = 0))
    return(x[!terpilih])
  }


  # VISUALISASI UNIVARIATE --------------------------------------------------

  #Pilih variabel
  observe({
    y<-input$sel_plotHisto
    updateSelectInput(session, "sel_plotHisto",
                      choices = varName(),
                      selected = y)

    x<-input$sel_plotDensity
    updateSelectInput(session, "sel_plotDensity",
                      choices = varName(),
                      selected = x)
  })

  #Susun data untuk histogram
  data_plot_histo<-reactive({
    dataku<-myData()
    toPlot<-dplyr::select(dataku, input$sel_plotHisto)
    toPlot<-as.data.frame(toPlot)
    colnames(toPlot)<-"variable"
    toPlot
  })

  #Buat plot histogram
  output$plotHisto<-renderPlot({
    req(input$sel_plotHisto)
    toPlot<-data_plot_histo()
    gg<-ggplot(data=toPlot, aes(variable)) +
      geom_histogram(fill="#18bc9c", col="#18bc9c", alpha=.5) +
      labs(x=input$sel_plotHisto, y="Count") +
      theme_hc()+
      theme(plot.background = element_rect(fill = "#ecf0f1"))

    gg+ggtitle(paste("Variabel: ",input$sel_plotHisto))+theme(plot.title = element_text(face="bold", size=20,hjust = 0.5))
  })

  #Susun data untuk density
  data_plot_density<-reactive({
    dataku<-myData()
    toPlot<-dplyr::select(dataku, input$sel_plotDensity)
    toPlot<-as.data.frame(toPlot)
    colnames(toPlot)<-"variable"
    toPlot
  })

  #Buat plot density
  output$plotDensity<-renderPlot({
    req(input$sel_plotDensity)
    toPlot<-data_plot_density()
    gg<-ggplot(data=toPlot, aes(variable)) +
      geom_density(fill="#18bc9c", col="#18bc9c", alpha=.5) +
      labs(x=input$sel_plotDensity, y="Count") +
      theme_hc()+
      theme(plot.background = element_rect(fill = "#ecf0f1"))

    gg+ggtitle(paste("Variabel: ",input$sel_plotDensity)) + theme(plot.title = element_text(face="bold", size=20,hjust = 0.5))
  })


  # VISUALISASI MULTIVARIATE ------------------------------------------------

  #Pilih variabel untuk plot korelasi (multiple input)
  output$ui_sel_plotCorr<-renderUI(
    multiInput(
      inputId = "sel_plotCorr",
      label = "Variabel",
      choices = varName()
    )
  )

  #Buat corrplot
  output$plotCorr<-renderPlot({
    req(input$sel_plotCorr)
    if(length(input$sel_plotCorr)>1){
      dataku<-myData()
      corrData<-dplyr::select(dataku, input$sel_plotCorr)
      df<-data.frame(corrData)
      corr <- round(cor(df), 2)
      ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE,
                 colors = c("#6D9EC1", "white", "#E46726"),
                 ggtheme = ggplot2::theme(plot.background = element_rect(fill = "#ecf0f1")))
    }else{
      #Tidak muncul apapun
    }

  })

  #Pilih variabel untuk scatter plot
  observe({

    y<-input$sel_plotScatter_1
    x<-input$sel_plotScatter_2

    updateSelectInput(session, "sel_plotScatter_1",
                      choices = varSisa(varName(), x),
                      selected = y)

    updateSelectInput(session, "sel_plotScatter_2",
                      choices = varSisa(varName(), y) ,
                      selected = x)
  })

  #Tampilkan scatter plot
  output$plotScatter<-renderPlot({
    req(input$sel_plotScatter_1)
    req(input$sel_plotScatter_2)

    dataku<-myData()

    var1<-input$sel_plotScatter_1
    var2<-input$sel_plotScatter_2

    y<-dplyr::select(dataku, var1)
    x<-dplyr::select(dataku, var2)

    df<-data.frame(y,x)
    colnames(df)<-c("y","x")

    gg<-ggplot(data=df, aes(x = x, y = y)) +
      geom_point(fill="#18bc9c", col="#18bc9c", alpha=.5) +
      labs(x=var2, y=var1) +
      theme_hc()+
      theme(plot.background = element_rect(fill = "#ecf0f1"))

    gg+ggtitle(paste("Variabel:",input$sel_plotScatter_1,"dan",input$sel_plotScatter_2,sep=" "))+theme(plot.title = element_text(face="bold", size=20,hjust = 0.5))
  })

  #Pilih variabel untuk boxplot
  output$ui_sel_plotBox<-renderUI(
    multiInput(
      inputId = "sel_plotBox",
      label = "Variabel",
      choices = varName()
    )
  )

  #Siapkan data untuk boxplot
  data_plot_box<-reactive({
    dataku<-myData()
    var<-input$sel_plotBox
    y<-dataku[,var]
    m<-nrow(dataku)
    n<-nrow(dataku)*length(var)

    namaVar<-c()
    for (i in 1:length(var)) {
      namaVar<-c(namaVar,rep(var[i],m))
    }

    nilaiVar<-c()
    for (i in 1:length(var)) {
      if(length(var)==1){
        nilaiVar<-y
      }else{
        nilaiVar<-c(nilaiVar,y[,i])
      }
    }

    toPlot<-data.frame(id=rep(c(1:m),length(var)), var=namaVar, value=nilaiVar)
    colnames(toPlot)<-c("id","var","value")
    toPlot
  })

  #Output boxplot
  output$plotBox<-renderPlot({
    req(input$sel_plotBox)
    toPlot<-data_plot_box()
    gg<- ggplot(data=toPlot, aes(x = var, y = value, fill=var)) +
      geom_boxplot(alpha=.5) +
      theme_hc()+
      theme(plot.background = element_rect(fill = "#ecf0f1"))

    gg
  })




}

# Run the application
shinyApp(ui = ui, server = server)

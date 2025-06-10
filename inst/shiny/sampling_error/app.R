
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
        jumbotron("Small Area Estimation with Benchmarking Methods",
                  "Metode SAE yang menghasilkan estimasi dengan konsistensi agregasi dengan estimasi langsung.",
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
      ),

      tabPanel(
        title = "RLB",
        icon = icon("transfer",
                    lib = "glyphicon"),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "rlb_sel_y",
              label = "Variabel Dependen",
              choices = NA
            ),
            pickerInput(
              inputId = "rlb_sel_x",
              label = "Variabel Independen",
              choices = NA,
              multiple = TRUE,
              options = list(
                "live-search"=TRUE,
                "actions-box"=TRUE
              )
            ),
            shinyWidgets::actionBttn(
              inputId = "rlb_calc",
              label = "Calculate",
              color = "default", no_outline = FALSE, style = "unite"
            ),
            shinyWidgets::actionBttn(
              inputId = "resetRLB",
              label = "Reset Input",
              color = "default", no_outline = FALSE, style = "unite"
            ),
            br(),
            br(),
            conditionalPanel(
              condition = "input.rlb_calc>0",
              actionButton(
                inputId = "copyVar",
                label = "Gunakan Variabel pada Analisis"
              )
            )
          ),
          mainPanel(
            dataTableOutput("rlbOut_model") %>% withSpinner(color="#0dc5c1", type = 8),
            dataTableOutput("rlbOut_fit") %>% withSpinner(color="#0dc5c1", type = 8),
            conditionalPanel(
              condition = "input.rlb_calc>0",
              actionButton(
                inputId = "showRLB",
                label = "Show Details"
              )
            ),
            shinyjs::hidden(
              div(
                id="placehasilRLB",
                verbatimTextOutput(
                  outputId = "hasilRLB"
                )  %>% withSpinner(color="#0dc5c1", type = 8)
              )
            )
          )
        )
      ),

      tabPanel(
        title = "EBLUP Benchmarking Methods",
        icon = icon("play",
                    lib = "glyphicon"),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "sel_y",
              label = "Variabel Estimasi",
              choices = NA
            ),
            pickerInput(
              inputId = "sel_x",
              label = "Variabel Penyerta",
              choices = NA,
              multiple = TRUE,
              options = list(
                "live-search"=TRUE,
                "actions-box"=TRUE
              )
            ),
            selectInput(
              inputId = "sel_vardir",
              label = "Varians Estimasi",
              choices = NA
            ),
            selectInput(
              inputId = "sel_weight",
              label = "Penimbang",
              choices = NA
            ),
            prettyRadioButtons(
              inputId = "sel_method",
              label = "Metode Benchmark",
              choices = c(
                "Ratio Benchmark*"="rb",
                "Different Benchmark"="db",
                "Optimal Benchmark*"="ob",
                "You Rao"="yr",
                "Wang Fuller Qu"="wfq"
              ),
              shape = "curve",
              icon = icon("check"),
              animation = "tada"
            ),
            "* tidak dapat menghitung MSE dan RSE",
            br(),
            br(),
            shinyWidgets::actionBttn(
              inputId = "calc",
              label = "Calculate",
              color = "default", no_outline = FALSE, style = "unite"
            ),
            shinyWidgets::actionBttn(
              inputId = "reset",
              label = "Reset Input",
              color = "default", no_outline = FALSE, style = "unite"
            ),
            br(),br(),
            conditionalPanel(
              condition = "input.calc>0",
              wellPanel(
                h3("Export Laporan Lengkap"),
                radioGroupButtons(
                  inputId = "tipeFileReport",
                  label = "Pilih Tipe File",
                  choices = c(
                    "PDF"="pdf",
                    "HTML"="html",
                    "WORD"="docx"
                  ),
                  selected = "pdf",
                  status = "primary",
                  checkIcon = list(
                    yes = icon("ok",
                               lib = "glyphicon"),
                    no = icon("remove",
                              lib = "glyphicon")),
                  justified = TRUE
                ),
                textInput(inputId = "namaFileReport", label = "Nama file"),
                downloadBttn(outputId = "downloadDataReport", label = "Export", color = "default", no_outline = FALSE, style = "unite")

              )
            )
          ),
          mainPanel(

            tabsetPanel(
              tabPanel(
                title = "Data",
                dataTableOutput("datashow") %>% withSpinner(color="#0dc5c1", type = 8)
              ),
              tabPanel(
                title = "Model",
                dataTableOutput("modelReg") %>% withSpinner(color="#0dc5c1", type = 8),
                dataTableOutput("refgamma") %>% withSpinner(color="#0dc5c1", type = 8),
                dataTableOutput("gamma") %>% withSpinner(color="#0dc5c1", type = 8)
              ),
              tabPanel(
                title = "Estimasi",
                dataTableOutput(
                  outputId = "hasilEst"
                ) %>% withSpinner(color="#0dc5c1", type = 8),
                conditionalPanel(
                  condition = "input.calc>0",
                  wellPanel(
                    radioGroupButtons(
                      inputId = "tipeFileEst",
                      label = "Pilih Tipe File",
                      choices = c(
                        "PDF"="pdf",
                        "HTML"="html",
                        "EXCEL"="csv",
                        "SPSS"="sav"
                      ),
                      selected = "csv",
                      status = "primary",
                      checkIcon = list(
                        yes = icon("ok",
                                   lib = "glyphicon"),
                        no = icon("remove",
                                  lib = "glyphicon")),
                      justified = TRUE
                    ),
                    textInput(inputId = "namaFileEst", label = "Nama file"),
                    downloadBttn(outputId = "downloadDataEst", label = "Export", color = "default", no_outline = FALSE, style = "unite")
                  )
                )
              ),

              tabPanel(
                title = "Agregasi",
                conditionalPanel(
                  condition = "input.calc>0",
                  h3("Agregasi")
                ),
                dataTableOutput("agregasi") %>% withSpinner(color="#0dc5c1", type = 8),
                br(),
                br(),
                conditionalPanel(
                  condition = "input.calc>0",
                  h3("Bias Empiris")
                ),
                dataTableOutput("biasEmpiris") %>% withSpinner(color="#0dc5c1", type = 8)
              ),

              tabPanel(
                title = "MSE",
                plotOutput("MSEplot") %>% withSpinner(color="#0dc5c1", type = 8),
                br(),
                verbatimTextOutput("summaryMSE") %>% withSpinner(color="#0dc5c1", type = 8),
                br(),
                dataTableOutput("MSEtable") %>% withSpinner(color="#0dc5c1", type = 8),
                conditionalPanel(
                  condition = "input.calc>0",
                  wellPanel(
                    radioGroupButtons(
                      inputId = "tipeFileMSE",
                      label = "Pilih Tipe File",
                      choices = c(
                        "PDF"="pdf",
                        "HTML"="html",
                        "EXCEL"="csv",
                        "SPSS"="sav"
                      ),
                      selected = "csv",
                      status = "primary",
                      checkIcon = list(
                        yes = icon("ok",
                                   lib = "glyphicon"),
                        no = icon("remove",
                                  lib = "glyphicon")),
                      justified = TRUE
                    ),
                    textInput(inputId = "namaFileMSE", label = "Nama file"),
                    downloadBttn(outputId = "downloadDataMSE", label = "Export", color = "default", no_outline = FALSE, style = "unite")
                  )
                )
              ),

              tabPanel(
                title = "RSE",
                plotOutput("RSEplot") %>% withSpinner(color="#0dc5c1", type = 8),
                br(),
                verbatimTextOutput("summaryRSE") %>% withSpinner(color="#0dc5c1", type = 8),
                br(),
                dataTableOutput("RSEtable") %>% withSpinner(color="#0dc5c1", type = 8),
                conditionalPanel(
                  condition = "input.calc>0",
                  wellPanel(
                    radioGroupButtons(
                      inputId = "tipeFileRSE",
                      label = "Pilih Tipe File",
                      choices = c(
                        "PDF"="pdf",
                        "HTML"="html",
                        "EXCEL"="csv",
                        "SPSS"="sav"
                      ),
                      selected = "csv",
                      status = "primary",
                      checkIcon = list(
                        yes = icon("ok",
                                   lib = "glyphicon"),
                        no = icon("remove",
                                  lib = "glyphicon")),
                      justified = TRUE
                    ),
                    textInput(inputId = "namaFileRSE", label = "Nama file"),
                    downloadBttn(outputId = "downloadDataRSE", label = "Export", color = "default", no_outline = FALSE, style = "unite")
                  )
                )
              )
            )
          )
        )
      )

    )
  )

# Define server logic required to draw a histogram
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



  # RLB STEPWISE ------------------------------------------------------------

  #Pilih variabel
  observe({
    y<-input$rlb_sel_y
    x<-input$rlb_sel_x


    updateSelectInput(session, "rlb_sel_y",
                      choices = varSisa(varName(), c(x)),
                      selected = y)

    updatePickerInput(session, "rlb_sel_x",
                      choices = varSisa(varName(), c(y)),
                      selected = x)

  })

  #Menghitung summary dari RLB stepwise
  rlb_reactive_summ<-eventReactive(input$rlb_calc,{
    dataku<-myData()
    y<-dplyr::select(dataku, input$rlb_sel_y)
    x<-dplyr::select(dataku, input$rlb_sel_x)
    df_rlb<<-data.frame(y, x)
    x_list<-input$rlb_sel_x
    x_pakai<-""
    for (i in 1:length(x)) {
      if(i==1){
        x_pakai<-paste(x_pakai, x_list[i], sep = "")
      }else{
        x_pakai<-paste(x_pakai, x_list[i], sep = " + ")
      }
    }
    formul<-paste(input$rlb_sel_y, x_pakai,sep = " ~ ")
    formulafix<-as.formula(formul)
    lm1<-lm(formula = formulafix, data = df_rlb)

    if(length(input$rlb_sel_x)==1){
      summary(lm1)
    }else{
      olsrr::ols_step_both_p(lm1, 0.05, details=TRUE)
    }
  })

  #MEnampilkan hasil hitungan summary
  output$hasilRLB<-renderPrint({
    rlb_reactive_summ()
  })

  #Mengolah model RLB
  rlb_reactive_model<-eventReactive(input$rlb_calc,{
    dataku<-myData()
    y<-dplyr::select(dataku, input$rlb_sel_y)
    x<-dplyr::select(dataku, input$rlb_sel_x)
    df_rlb<<-data.frame(y, x)
    x_list<-input$rlb_sel_x
    x_pakai<-""
    for (i in 1:length(x)) {
      if(i==1){
        x_pakai<-paste(x_pakai, x_list[i], sep = "")
      }else{
        x_pakai<-paste(x_pakai, x_list[i], sep = " + ")
      }
    }
    formul<-paste(input$rlb_sel_y, x_pakai,sep = " ~ ")
    formulafix<-as.formula(formul)
    lm1<-lm(formula = formulafix, data = df_rlb)

    if(length(input$rlb_sel_x)==1){
      lm1
    }else{
      sw1<-olsrr::ols_step_both_p(lm1, 0.05)
      xku<-""
      for (i in 1:length(sw1$predictors)) {
        if(i==1){
          xku<-paste(xku,sw1$predictors[i],sep = "")
        }else{
          xku<-paste(xku,sw1$predictors[i],sep = " + ")
        }
      }
      y<-input$rlb_sel_y
      form<-as.formula(paste(y,xku,sep = " ~ "))

      lm2<-lm(form,df_rlb)
      lm2
    }
  })

  #Menampilkan model stepwise RLB
  output$rlbOut_model<-renderDataTable({
    DT::datatable(broom::tidy(rlb_reactive_model()), options = list(paging=FALSE, searching=FALSE, ordering=FALSE))

  })

  #Menampilkan kebaikan model
  output$rlbOut_fit<-renderDataTable({
    DT::datatable(broom::glance(rlb_reactive_model())[,1:6], options = list(paging=FALSE, searching=FALSE, ordering=FALSE))
  })

  #Menampilkan atau menyembunyikan details dari RLB
  observeEvent(input$showRLB,{
    shinyjs::toggle("placehasilRLB")
  })


  # ANALISIS SAE ------------------------------------------------------------

  #Pilih variabel SAE
  observe({
    y<-input$sel_y
    x<-input$sel_x
    vardir<-input$sel_vardir
    weight<-input$sel_weight

    updateSelectInput(session, "sel_y",
                      choices = varSisa(varName(), c(x,vardir,weight)),
                      selected = y)

    updatePickerInput(session, "sel_x",
                      choices = varSisa(varName(), c(y,vardir,weight)),
                      selected = x)

    updateSelectInput(session, "sel_vardir",
                      choices = varSisa(varName(), c(y,x,weight)),
                      selected = vardir)

    updateSelectInput(session, "sel_weight",
                      choices = varSisa(varName(), c(y,x,vardir)),
                      selected = weight)

  })

  #Seleksi data yang dipilih berdasarkan select input untuk analisis
  selectData<-eventReactive(input$calc,{
    dataku<-myData()
    y<-dplyr::select(dataku, input$sel_y)
    x<-dplyr::select(dataku, input$sel_x)
    vardir<-dplyr::select(dataku, input$sel_vardir)
    weight<-dplyr::select(dataku, input$sel_weight)
    df<-data.frame(y=y,vardir=vardir,weight=weight,x=x)
    x_pakai<-c()
    for (i in 1:ncol(x)) {
      x_pakai<-c(x_pakai,paste("x",i,sep = ""))
    }
    colnam<-c("y","vardir","weight",x_pakai)
    colnames(df)<-colnam
    df<-na.omit(df)
    df
  })

  #Tampilkan data yang diselect
  output$datashow<-renderDataTable({
    df<-selectData()
    DT::datatable(df, options = list(
      "scrollY"="500px",
      "scrollX"=TRUE,
      "searching"=FALSE,
      "paging"=FALSE
    ))
  })

  #Estimasi EBLUP benchmark
  eblupKu<-eventReactive(input$calc,{
    dataku<-selectData()
    x_pakai<-c()
    for (i in 1:length(input$sel_x)) {
      if(i!=1){
        x_pakai<-paste(x_pakai,colnames(dataku)[3+i],sep = " + ")
      }else{
        x_pakai<-colnames(dataku)[3+i]
      }
    }
    formulaku<-as.formula(paste("y",x_pakai, sep = " ~ "))

    if(input$sel_method=="rb"){
      hasil<-eblupRB(formula = formulaku, vardir = vardir, weight = weight, data=dataku)
    }else{
      if(input$sel_method=="db"){
        hasil<-eblupDB(formula = formulaku, vardir = vardir, weight = weight, data = dataku)
      }else{
        if(input$sel_method=="ob"){
          hasil<-eblupWFQ(formula = formulaku, vardir = vardir, weight = weight, data = dataku)
        }else{
          if(input$sel_method=="yr"){
            hasil<-eblupYR(formula = formulaku, vardir = vardir, weight = weight, data = dataku)
          }else{
            if(input$sel_method=="wfq"){
              hasil<-eblupAUG(formula = formulaku, vardir = vardir, weight = weight, data = dataku)
            }
          }
        }
      }
    }

    hasil
  })

  #Menampilkan model regresi dari model eblup
  output$modelReg<-renderDataTable({

    hasil<-eblupKu()$fit$estcoef
    hasil<-as.data.frame(hasil)
    if(input$sel_method=="wfq"){
      row.names(hasil)<-c("(Intercept)",input$sel_x,"AUG")
    }else{
      row.names(hasil)<-c("(Intercept)",input$sel_x)
    }
    DT::datatable(hasil, options = list(paging=FALSE, searching=FALSE, ordering=FALSE))
  })

  output$refgamma<-renderDataTable({

    ref<-eblupKu()$fit$refvar
    dataku<-selectData()
    gamma<-ref/(dataku$vardir+ref)
    hasil<-data.frame(refvar=ref, mean_gamma=mean(gamma))
    DT::datatable(hasil, options = list(paging=FALSE, searching=FALSE, ordering=FALSE))
  })

  output$gamma<-renderDataTable({

    ref<-eblupKu()$fit$refvar
    dataku<-selectData()
    gamma<-ref/(dataku$vardir+ref)
    hasil<-data.frame(gamma=gamma)
    DT::datatable(hasil, options = list(paging=FALSE, searching=FALSE, ordering=FALSE))
  })


  #Menampilkan hasil estimasi
  output$hasilEst<-renderDataTable({
    df<-estimasi()
    DT::datatable(df,
                  options = list(
                    "scrollY"="500px",
                    "scrollX"=TRUE,
                    "searching"=FALSE,
                    "paging"=FALSE
                  ))
  })

  #Menghasilkan tabel hasil estimasi Y
  estimasi<-eventReactive(input$calc,{
    dataku<-selectData()
    x_pakai<-c()
    for (i in 1:length(input$sel_x)) {
      if(i!=1){
        x_pakai<-paste(x_pakai,colnames(dataku)[3+i],sep = " + ")
      }else{
        x_pakai<-colnames(dataku)[3+i]
      }
    }
    formulaku<-as.formula(paste("y",x_pakai, sep = " ~ "))
    if(input$sel_method=="rb"){
      hasil<-eblupRB(formula = formulaku, vardir = vardir, weight = weight, data=dataku)
    }else{
      if(input$sel_method=="db"){
        hasil<-eblupDB(formula = formulaku, vardir = vardir, weight = weight, data = dataku)
      }else{
        if(input$sel_method=="ob"){
          hasil<-eblupWFQ(formula = formulaku, vardir = vardir, weight = weight, data = dataku)
        }else{
          if(input$sel_method=="yr"){
            hasil<-eblupYR(formula = formulaku, vardir = vardir, weight = weight, data = dataku)
          }else{
            if(input$sel_method=="wfq"){
              hasil<-eblupAUG(formula = formulaku, vardir = vardir, weight = weight, data = dataku)
            }
          }
        }
      }
    }
    generate<-data.frame(dataku$y,hasil$eblup)
    colnames(generate)<-c("Direct","EBLUP","BENCHMARKED_EBLUP")
    generate
  })

  #Download hasil estimasi
  output$downloadDataEst <- downloadHandler(
    filename = function(){
      if((input$namaFileEst)==""){
        paste("data-",Sys.Date(),".",input$tipeFileEst,sep = "")
      }else{
        paste(input$namaFileEst,".",input$tipeFileEst, sep="")
      }

    },
    content = function(file) {
      df<-estimasi()
      if(input$tipeFileEst=="pdf"){
        #pdf(file, onefile = TRUE)

        marginbiasa<-2
        margin <- unit(marginbiasa,"cm")
        panjangKolom<-(21-2*(marginbiasa+1))/ncol(df)
        tg<-tableGrob(df, widths= rep(unit(panjangKolom,"cm"),ncol(df)))
        tg <- gtable_add_grob(tg,
                              grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                              t = 2, b = nrow(tg), l = 1, r = ncol(tg))

        tg <- gtable_add_grob(tg,
                              grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                              t = 1, l = 1, r = ncol(tg))

        tg$vp <- viewport(width=unit(21,"cm") - margin,
                          height=unit(29.7,"cm")- margin)

        fullheight <- convertHeight(sum(tg$heights), "cm", valueOnly = TRUE)

        margin_cm <- convertHeight(margin, "cm", valueOnly = TRUE)
        a4height <- 29.7 - margin_cm
        nrows <- nrow(tg)
        npages <- ceiling(fullheight / a4height)

        heights <- convertHeight(tg$heights, "cm", valueOnly = TRUE)
        rows <- cut(cumsum(heights), include.lowest = FALSE,
                    breaks = c(0, cumsum(rep(a4height, npages))))

        groups <- split(seq_len(nrows), rows)

        gl <- lapply(groups, function(id) tg[id,])

        pdf(file, paper = "a4", width = 0, height = 0)
        for(page in seq_len(npages)){
          grid.newpage()
          grid.rect(width=unit(21,"cm") - margin,
                    height=unit(29.7,"cm")- margin)
          grid.draw(gl[[page]])
        }
        dev.off()
      }else{
        if(input$tipeFileEst=="html"){
          tempReport <- file.path("./export/outEst.Rmd")
          file.copy("outEst.Rmd", tempReport, overwrite = TRUE)

          params <- list(n = df)

          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }else{
          if(input$tipeFileEst=="csv"){
            write.csv(df, file, row.names = FALSE)
          }else{
            if(input$tipeFileEst=="sav"){
              write_sav(df,file)
            }
          }
        }
      }
    }
  )

  #Tampilkan hasil agregasi
  output$agregasi<-renderDataTable({
    dataku<-selectData()
    est<-estimasi()

    agrDir<-sum(dataku$weight*est$Direct)
    agrEBLUP<-sum(dataku$weight*est$EBLUP)
    agrBENCH<-sum(dataku$weight*est$BENCHMARKED_EBLUP)

    tag<-c("Direct","EBLUP","BENCHMARKED_EBLUP")
    agr<-c(agrDir,agrEBLUP,agrBENCH)

    df<-data.frame(tag,agr)
    colnames(df)<-c("Estimasi","Agregasi")
    DT::datatable(df, options = list(paging=FALSE, searching=FALSE, ordering=FALSE))
  })

  #Tampilkan bias empiris
  output$biasEmpiris<-renderDataTable({
    dataku<-selectData()
    est<-estimasi()

    biasEBLUP<-sum(dataku$weight*(dataku$y - est$EBLUP))
    biasBENCH<-sum(dataku$weight*(dataku$y - est$BENCHMARKED_EBLUP))

    tag<-c("EBLUP","BENCHMARKED_EBLUP")
    bias<-c(biasEBLUP,biasBENCH)

    df<-data.frame(tag,bias)
    colnames(df)<-c("Estimasi","Bias Empiris")
    DT::datatable(df, options = list(paging=FALSE, searching=FALSE, ordering=FALSE))
  })

  #Hitung MSE
  mse<-eventReactive(input$calc,{
    dataku<-selectData()
    x_pakai<-c()
    for (i in 1:length(input$sel_x)) {
      if(i!=1){
        x_pakai<-paste(x_pakai,colnames(dataku)[3+i],sep = " + ")
      }else{
        x_pakai<-colnames(dataku)[3+i]
      }
    }
    formulaku<-as.formula(paste("y",x_pakai, sep = " ~ "))
    msedir<-dataku$vardir

    eblupbiasa<-mseFH(formula = formulaku, vardir = vardir, data=dataku)
    mseeblup<-eblupbiasa$mse

    if(input$sel_method=="rb"){
      hasil<-data.frame(rep(NA, nrow(dataku)))
      hasil$mse<-rep(NA, nrow(dataku))
    }else{
      if(input$sel_method=="db"){
        hasil<-mseDB(formula = formulaku, vardir = vardir, weight = weight, data = dataku)
      }else{
        if(input$sel_method=="ob"){
          hasil<-data.frame(rep(NA, nrow(dataku)))
          hasil$mse<-rep(NA, nrow(dataku))
        }else{
          if(input$sel_method=="yr"){
            hasil<-mseYR(formula = formulaku, vardir = vardir, weight = weight, data = dataku)
          }else{
            if(input$sel_method=="wfq"){
              hasil<-mseAUG(formula = formulaku, vardir = vardir, weight = weight, data = dataku)
            }
          }
        }
      }
    }
    generate<-data.frame(Direct=msedir, EBLUP=mseeblup, EBLUP_BENCHMARK=hasil$mse)
    generate
  })

  #Summary dari MSE
  summMSE<-eventReactive(input$calc,{
    summary(mse())
  })

  #Tampilkan summary MSE
  output$summaryMSE<-renderPrint(
    summMSE()
  )

  #Tampilkan tabel MSE
  output$MSEtable<-renderDataTable({
    df<-mse()
    DT::datatable(df, options = list(
      "scrollY"="500px",
      "scrollX"=TRUE,
      "searching"=FALSE,
      "paging"=FALSE
    ))
  })

  #Buat plot MSE
  plotMSE<-eventReactive(input$calc,{
    dataku<-mse()
    m<-nrow(dataku)
    if(input$sel_method %in% c("rb","ob")){
      n<-nrow(dataku)*2
      toPlot<-data.frame(id=rep(c(1:m),2), type=c(rep("DIRECT",m),rep("EBLUP",m)), MSE=c(dataku$Direct, dataku$EBLUP))

      ggplot(data = toPlot, mapping = aes(x=id, y=MSE)) +
        geom_line(aes(color = type), size = 1) +
        theme_minimal()
    }else{
      n<-nrow(dataku)*3
      toPlot<-data.frame(id=rep(c(1:m),3), type=c(rep("DIRECT",m),rep("EBLUP",m),rep("EBLUP BENCHMARK",m)), MSE=c(dataku$Direct, dataku$EBLUP, dataku$EBLUP_BENCHMARK))

      ggplot(data = toPlot, mapping = aes(x=id, y=MSE)) +
        geom_line(aes(color = type), size = 1) +
        theme_minimal()
    }
  })

  #Tampilkan plot MSE
  output$MSEplot<-renderPlot({
    plotMSE()
  })

  #Download MSE
  output$downloadDataMSE <- downloadHandler(
    filename = function(){
      if((input$namaFileMSE)==""){
        paste("data-",Sys.Date(),".",input$tipeFileMSE,sep = "")
      }else{
        paste(input$namaFileMSE,".",input$tipeFileMSE, sep="")
      }

    },
    content = function(file) {
      df<-mse()
      if(input$tipeFileMSE=="pdf"){
        marginbiasa<-2
        margin <- unit(marginbiasa,"cm")
        panjangKolom<-(21-2*(marginbiasa+1))/ncol(df)
        tg<-tableGrob(df, widths= rep(unit(panjangKolom,"cm"),ncol(df)))

        tg <- gtable_add_grob(tg,
                              grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                              t = 2, b = nrow(tg), l = 1, r = ncol(tg))
        tg <- gtable_add_grob(tg,
                              grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                              t = 1, l = 1, r = ncol(tg))

        ?tableGrob
        tg$vp <- viewport(width=unit(21,"cm") - margin,
                          height=unit(29.7,"cm")- margin)

        fullheight <- convertHeight(sum(tg$heights), "cm", valueOnly = TRUE)

        margin_cm <- convertHeight(margin, "cm", valueOnly = TRUE)
        a4height <- 29.7 - margin_cm
        nrows <- nrow(tg)
        npages <- ceiling(fullheight / a4height)

        heights <- convertHeight(tg$heights, "cm", valueOnly = TRUE)
        rows <- cut(cumsum(heights), include.lowest = FALSE,
                    breaks = c(0, cumsum(rep(a4height, npages))))

        groups <- split(seq_len(nrows), rows)



        gl <- lapply(groups, function(id) tg[id,])

        pdf(file, paper = "a4", width = 0, height = 0)
        for(page in seq_len(npages)){
          grid.newpage()
          grid.rect(width=unit(21,"cm") - margin,
                    height=unit(29.7,"cm")- margin)
          grid.draw(gl[[page]])
        }
        dev.off()
      }else{
        if(input$tipeFileMSE=="html"){
          tempReport <- file.path("./export/outMSE.Rmd")
          file.copy("outMSE.Rmd", tempReport, overwrite = TRUE)

          params <- list(n = df)

          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }else{
          if(input$tipeFileMSE=="csv"){
            write.csv(df, file, row.names = FALSE)
          }else{
            if(input$tipeFileMSE=="sav"){
              write_sav(df,file)
            }
          }
        }
      }
    }
  )

  #Hitung RSE
  rse<-eventReactive(input$calc,{
    est<-estimasi()
    mse<-mse()


    if(input$sel_method %in% c("rb","ob")){
      generate<-sqrt(mse[,1:2])/est[,1:2]*100
      colnames(generate)<-c("Direct","EBLUP")
    }else{
      generate<-sqrt(mse)/est*100
      colnames(generate)<-c("Direct","EBLUP","EBLUP_BENCHMARK")
    }
    generate
  })

  #Buat plot RSE
  plotRSE<-eventReactive(input$calc,{
    dataku<-rse()
    m<-nrow(dataku)

    if(input$sel_method %in% c("rb","ob")){
      n<-nrow(dataku)*2
      toPlot<-data.frame(id=rep(c(1:m),2), type=c(rep("DIRECT",m),rep("EBLUP",m)), RSE=c(dataku$Direct, dataku$EBLUP))

      ggplot(data = toPlot, mapping = aes(x=id, y=RSE)) +
        geom_line(aes(color = type), size = 1) +
        theme_minimal()
    }else{
      n<-nrow(dataku)*3
      toPlot<-data.frame(id=rep(c(1:m),3), type=c(rep("DIRECT",m),rep("EBLUP",m),rep("EBLUP BENCHMARK",m)), RSE=c(dataku$Direct, dataku$EBLUP, dataku$EBLUP_BENCHMARK))
      ggplot(data = toPlot, mapping = aes(x=id, y=RSE)) +
        geom_line(aes(color = type), size = 1) +
        theme_minimal()
    }
  })

  #Tampilkan plot RSE
  output$RSEplot<-renderPlot({
    plotRSE()
  })

  #Tampilkan tabel RSE
  output$RSEtable<-renderDataTable({
    df<-rse()
    DT::datatable(df, options = list(
      "scrollY"="500px",
      "scrollX"=TRUE,
      "searching"=FALSE,
      "paging"=FALSE
    ))
  })

  #Hitung summary RSE
  summRSE<-eventReactive(input$calc,{
    summary(rse())
  })

  #Tampilkan summary RSE
  output$summaryRSE<-renderPrint(
    summRSE()
  )

  #Download RSE
  output$downloadDataRSE <- downloadHandler(
    filename = function(){
      if((input$namaFileRSE)==""){
        paste("data-",Sys.Date(),".",input$tipeFileRSE,sep = "")
      }else{
        paste(input$namaFileRSE,".",input$tipeFileRSE, sep="")
      }

    },
    content = function(file) {
      df<-rse()
      if(input$tipeFileRSE=="pdf"){
        marginbiasa<-2
        margin <- unit(marginbiasa,"cm")
        panjangKolom<-(21-2*(marginbiasa+1))/ncol(df)
        tg<-tableGrob(df, widths= rep(unit(panjangKolom,"cm"),ncol(df)))
        tg <- gtable_add_grob(tg,
                              grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                              t = 2, b = nrow(tg), l = 1, r = ncol(tg))
        tg <- gtable_add_grob(tg,
                              grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                              t = 1, l = 1, r = ncol(tg))

        ?tableGrob
        tg$vp <- viewport(width=unit(21,"cm") - margin,
                          height=unit(29.7,"cm")- margin)

        fullheight <- convertHeight(sum(tg$heights), "cm", valueOnly = TRUE)

        margin_cm <- convertHeight(margin, "cm", valueOnly = TRUE)
        a4height <- 29.7 - margin_cm
        nrows <- nrow(tg)
        npages <- ceiling(fullheight / a4height)

        heights <- convertHeight(tg$heights, "cm", valueOnly = TRUE)
        rows <- cut(cumsum(heights), include.lowest = FALSE,
                    breaks = c(0, cumsum(rep(a4height, npages))))

        groups <- split(seq_len(nrows), rows)


        gl <- lapply(groups, function(id) tg[id,])

        pdf(file, paper = "a4", width = 0, height = 0)
        for(page in seq_len(npages)){
          grid.newpage()
          grid.rect(width=unit(21,"cm") - margin,
                    height=unit(29.7,"cm")- margin)
          grid.draw(gl[[page]])
        }
        dev.off()
      }else{
        if(input$tipeFileRSE=="html"){
          tempReport <- file.path("./export/outRSE.Rmd")
          file.copy("outRSE.Rmd", tempReport, overwrite = TRUE)

          params <- list(n = df)

          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }else{
          if(input$tipeFileRSE=="csv"){
            write.csv(df, file, row.names = FALSE)
          }else{
            if(input$tipeFileRSE=="sav"){
              write_sav(df,file)
            }
          }
        }
      }
    }
  )

  #Untuk syarat conditional panel dari export report
  show.report<-eventReactive(input$calc,{
    return(0)
  })

  #Download report
  output$downloadDataReport <- downloadHandler(
    filename = function(){
      if((input$namaFileReport)==""){
        paste("data-",Sys.Date(),".",input$tipeFileReport,sep = "")
      }else{
        paste(input$namaFileReport,".",input$tipeFileReport, sep="")
      }

    },
    content = function(file) {
      hasil<-eblupKu()$fit$estcoef

      hasil<-as.data.frame(hasil)
      if(input$sel_method=="wfq"){
        row.names(hasil)<-c("(Intercept)",input$sel_x,"AUG")
      }else{
        row.names(hasil)<-c("(Intercept)",input$sel_x)
      }

      dataku<-selectData()
      est<-estimasi()

      biasEBLUP<-sum(dataku$weight*(dataku$y - est$EBLUP))
      biasBENCH<-sum(dataku$weight*(dataku$y - est$BENCHMARKED_EBLUP))

      tag<-c("EBLUP","BENCHMARKED_EBLUP")
      bias<-c(biasEBLUP,biasBENCH)

      df_bias<-data.frame(tag,bias)
      colnames(df_bias)<-c("Estimasi","Bias Empiris")

      agrDir<-sum(dataku$weight*est$Direct)
      agrEBLUP<-sum(dataku$weight*est$EBLUP)
      agrBENCH<-sum(dataku$weight*est$BENCHMARKED_EBLUP)

      tag<-c("Direct","EBLUP","BENCHMARKED_EBLUP")
      agr<-c(agrDir,agrEBLUP,agrBENCH)

      df_agregasi<-data.frame(tag,agr)
      colnames(df_agregasi)<-c("Estimasi","Agregasi")

      if(input$tipeFileReport=="html"){
        # Set up parameters to pass to Rmd document
        params <- list(
          metode= input$sel_method,
          y=input$sel_y,
          x=input$sel_x,
          modelreg= hasil,
          est= estimasi(),
          bias= df_bias,
          agregasi= df_agregasi,
          mse= mse(),
          rse= rse()
        )

        tempReport <- file.path("./export/outReportHtml.Rmd")
        file.copy("outReportHtml.Rmd", tempReport, overwrite = TRUE)


        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )

      }else{
        if(input$tipeFileReport=="pdf"){
          params <- list(
            metode= input$sel_method,
            y=input$sel_y,
            x=input$sel_x,
            modelreg= hasil,
            est= estimasi(),
            bias= df_bias,
            agregasi= df_agregasi,
            mse= mse(),
            rse= rse()
          )

          getwd()
          src <- normalizePath('./export/outReportPdf.Rmd')
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, 'outReportPdf.Rmd', overwrite = TRUE)

          out<-rmarkdown::render('outReportPdf.Rmd',
                                 params = params,
                                 output_format = "pdf_document",
                                 envir = new.env(parent = globalenv())
          )
          file.rename(out,file)
        }else{
          if(input$tipeFileReport=="docx"){
            params <- list(
              metode= input$sel_method,
              y=input$sel_y,
              x=input$sel_x,
              modelreg= hasil,
              est= estimasi(),
              bias= df_bias,
              agregasi= df_agregasi,
              mse= mse(),
              rse= rse()
            )

            tempReport <- file.path("./export/outReportWord.Rmd")
            file.copy("outReportWord.Rmd", tempReport, overwrite = TRUE)


            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
          }
        }
      }
    }

  )

  #Reset input di tab analisis
  observeEvent(input$reset,{
    reset("sel_y")
    reset("sel_x")
    reset("sel_vardir")
    reset("sel_weight")
  })

  #Reset input di tab RLB
  observeEvent(input$resetRLB,{
    reset("rlb_sel_y")
    reset("rlb_sel_x")
  })

  #Gunakan variabel RLB pada analisis
  observeEvent(input$copyVar,{
    y<-input$rlb_sel_y
    model<-broom::tidy(rlb_reactive_model())
    x<-model$term[-1]



    updateSelectInput(session, "sel_y",
                      selected = y)

    updatePickerInput(session, "sel_x",
                      selected = x)

  })


}

# Run the application
shinyApp(ui = ui, server = server)


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

install_load("rio") #shiny
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
#install_load("rmarkdown") #untuk output pdf dkk
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
install_load("survey") #toggle rlb
install_load("srvyr") #toggle rlb

survey_est = function(est_type = "mean", x, denominator = NULL, na.rm = FALSE, vartype =  c("se", "ci", "var", "cv"), level = 0.95, proportion = FALSE, prop_method = c("logit", "likelihood", "asin", "beta", "mean", "xlogit"), deff = FALSE, df = NULL){
  if(est_type == "mean"){
    res = srvyr::survey_mean(x = x, na.rm = na.rm, vartype = vartype, level = level, proportion = proportion, prop_method = prop_method, deff = deff, df = df)
  }else{
    if(est_type == "prop"){
      res = srvyr::survey_mean(x = x, na.rm = na.rm, vartype = vartype, level = level, proportion = TRUE, prop_method = prop_method, deff = deff, df = df)
    }else{
      if(est_type == "total"){
        res = srvyr::survey_total(x = x, na.rm = na.rm, vartype = vartype, level = level, proportion = proportion, prop_method = prop_method, deff = deff, df = df)
      }else{
        if(est_type == "ratio"){
          res = srvyr::survey_ratio(numerator = x, denominator = denominator, na.rm = na.rm, vartype = vartype, level = level, proportion = proportion, prop_method = prop_method, deff = deff, df = df)
        }else{
          res = NULL
        }
      }
    }
  }

  return(res)
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  is.numeric(x) && all(abs(x - round(x)) < tol, na.rm = TRUE)
}

is.zero.one <- function(x) {
  is.numeric(x) && all(x %in% c(0, 1), na.rm = TRUE)
}

options(survey.lonely.psu = "adjust")

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
        #img(src="icon.png", height = "40px", style = "position: relative; margin-top: -14px"),
        icon("eye-open",lib = "glyphicon"),
        "RSEE"
      ),
      theme = shinytheme("flatly"),

      tabPanel(
        title = "Home",
        icon = icon("home",
                    lib = "glyphicon"),
        jumbotron("RSEE",
                  "R Sampling Error Estimator",
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
            p("Aplikasi ini dapat digunakan untuk estimasi sampling error."),

            p("Beberapa fitur dari aplikasi ini: "),
            tags$ul(
              HTML(
                "<li> <p> Visualisasi Data </p> </li>
                <li> <p> Estimasi Sampling Error </p> </li>


                "
              )
            )
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
                selected = "excel",
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
          ),

          wellPanel(
            h3("Boxplot"),
            "Klik untuk memilih variabel",
            dropdownButton(
              uiOutput("ui_sel_plotBox"),
              circle = TRUE, status = "primary",
              icon = icon("gear"), width = "300px"
            ),
            plotOutput("plotBox") %>% withSpinner(color="#0dc5c1", type = 8)
          )
        ),

        tabPanel(
          title = "Multivariate",

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


# UI - SET DESIGN ---------------------------------------------------------

      tabPanel(
        title = "Set Survey Design",
        icon = icon("cog",
                    lib = "glyphicon"),
        sidebarLayout(
          sidebarPanel(
            shinyjs::useShinyjs(),
            id = "side-panel-setdesign",
            selectInput(
              inputId = "sel_ids",
              label = "Ids",
              choices = NULL
            ),
            selectInput(
              inputId = "sel_strata",
              label = "Strata *",
              choices = NULL
            ),
            selectInput(
              inputId = "sel_fpc",
              label = "FPC",
              choices = NULL
            ),
            selectInput(
              inputId = "sel_weight",
              label = "Weights *",
              choices = NULL
            ),
            "* wajib terisi",
            br(),
            br(),
            shinyWidgets::actionBttn(
              inputId = "save_design",
              label = "Save",
              color = "default", no_outline = FALSE, style = "unite"
            ),
            shinyWidgets::actionBttn(
              inputId = "reset_design",
              label = "Reset Input",
              color = "default", no_outline = FALSE, style = "unite"
            ),
            tags$script("
              Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                Shiny.onInputChange(variableName, null);
              });
            ")
          ),
          mainPanel(
            shinyjs::useShinyjs(),
            id = "main-panel-setdesign",

            tabsetPanel(
              tabPanel(
                shinyjs::useShinyjs(),
                id = "side-panel-setdesign-des",
                title = "Survey Design",
                verbatimTextOutput("summaryDesain") %>% withSpinner(color="#0dc5c1", type = 8)
              )

            )
          )
        )
      ),


# UI - ESTIMASI ----------------------------------------------------------------

      tabPanel(
        title = "Estimasi",
        icon = icon("signal", lib = "glyphicon"),
        sidebarLayout(
          sidebarPanel(
            shinyjs::useShinyjs(),

            selectInput(
              inputId = "sel_agg",
              label = "Aggregation *",
              choices = NA
            ),

            prettyRadioButtons(
              inputId = "sel_method",
              label = "Estimation *",
              choices = c(
                "Mean"="mean",
                "Proportion"="prop",
                "Total"="total",
                "Ratio"="ratio"
              ),
              shape = "curve",
              icon = icon("check"),
              animation = "tada"
            ),

            pickerInput(
              inputId = "sel_varest",
              label = "Variable *",
              multiple = TRUE,
              choices = NA
            ),

            conditionalPanel(
              condition = "input.sel_method=='ratio'",
              selectInput(
                inputId = "sel_denom",
                label = "Denominator *",
                choices = NA
              )
            ),

            prettyRadioButtons(
              inputId = "sel_narm",
              label = "NA Remove",
              choices = c(
                "TRUE"="true",
                "FALSE"="false"
              ),
              selected = "true",
              shape = "curve",
              icon = icon("check"),
              animation = "tada"
            ),

            "* wajib terisi",
            br(),
            br(),
            shinyWidgets::actionBttn(
              inputId = "go_est",
              label = "Run",
              color = "default", no_outline = FALSE, style = "unite"
            ),
            shinyWidgets::actionBttn(
              inputId = "reset_est",
              label = "Reset Input",
              color = "default", no_outline = FALSE, style = "unite"
            ),

            tags$script("
              Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                Shiny.onInputChange(variableName, null);
              });
            ")
          ),
          mainPanel(
            shinyjs::useShinyjs(),
            id = "main-panel-setdesign",

            tabsetPanel(
              tabPanel(
                shinyjs::useShinyjs(),
                title = "Hasil Estimasi",
                dataTableOutput("data_est") %>% withSpinner(color="#0dc5c1", type = 8)
              ),

              tabPanel(
                shinyjs::useShinyjs(),
                title = "Point Estimate Summary",
                plotOutput("plotPointEstimate") %>% withSpinner(color="#0dc5c1", type = 8)
              ),

              tabPanel(
                shinyjs::useShinyjs(),
                title = "RSE Summary",
                wellPanel(
                  h3("RSE Summary"),
                  dataTableOutput("rse_summary_table") %>% withSpinner(color="#0dc5c1", type = 8)
                ),
                wellPanel(
                  h3("RSE Summary by Category"),
                  dataTableOutput("rse_kat_table") %>% withSpinner(color="#0dc5c1", type = 8)
                ),
                plotOutput("plotRSE") %>% withSpinner(color="#0dc5c1", type = 8)
              ),

              tabPanel(

                shinyjs::useShinyjs(),
                title = "Export",

                conditionalPanel(
                  condition = "input.go_est>0",
                  br(),
                  br(),
                  radioGroupButtons(
                    inputId = "tipeFileEst",
                    label = "Pilih Tipe File",
                    choices = c(
                      "XLSX"="xlsx"
                    ),
                    selected = "xlsx",
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
            )
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
        d<<-import(inFile$datapath, header = TRUE)
      }else{
        d<<-data.frame()
      }
    }else{
      if(input$jenisFile=="excel"){
        if(substr(inFile$datapath,nchar(inFile$datapath)-3,nchar(inFile$datapath))=="xlsx"){
          d<<-import(inFile$datapath)
        }else{
          d<<-data.frame()
        }

      }else{
        if(input$jenisFile=="spss"){
          if(substr(inFile$datapath,nchar(inFile$datapath)-2,nchar(inFile$datapath))=="sav"){
            d<<-import(inFile$datapath)
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

  #Memanggil seluruh variabel
  varNameAll<-reactive({
    df<-myData()
    if(nrow(df)==0){
      NA
    }else{
      colnames(df)
    }
  })

  #Memanggil variabel numerik saja
  varName<-reactive({
    df<-myData()
    if(nrow(df)==0){
      NA
    }else{
      colnames(df)[sapply(df, is.numeric)]
    }
  })

  #Memanggil variabel kategorik saja
  varNameKategorik<-reactive({
    df<-myData()
    if(nrow(df)==0){
      NA
    }else{
      colnames(df)[sapply(df, is.character)]
    }
  })


  #Memanggil variabel integer saja
  varNameInteger<-reactive({
    df<-myData()
    if(nrow(df)==0){
      NA
    }else{
      colnames(df)[sapply(df, is.wholenumber)]
    }
  })

  #Memanggil variabel prop saja
  varNameZeroOne<-reactive({
    df<-myData()
    if(nrow(df)==0){
      NA
    }else{
      colnames(df)[sapply(df, is.zero.one)]
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

  #Tampilkan data yang diselect
  output$datashow<-renderDataTable({
    df<-myData()
    DT::datatable(df, options = list(
      "scrollY"="500px",
      "scrollX"=TRUE,
      "searching"=FALSE,
      "paging"=FALSE
    ))
  })


# SET DESAIN SAMPEL -------------------------------------------------------

  #Pilih variabel untuk set desain sampel
  observeEvent(dataBrowse(), {

    ids <-  (input$sel_ids)
    strata <- (input$sel_strata)
    fpc <- (input$sel_fpc)
    weight <- (input$sel_weight)

    choices = c(varNameAll())

    isolate({
      updateSelectInput(session, "sel_ids",
                        choices = setdiff(choices, c(strata, fpc, weight)),
                        selected = ids)

      updateSelectInput(session, "sel_strata",
                        choices = setdiff(choices, c(ids, fpc, weight)),
                        selected = strata)

      updateSelectInput(session, "sel_fpc",
                        choices = setdiff(choices, c(strata, ids, weight)),
                        selected = fpc)

      updateSelectInput(session, "sel_weight",
                        choices = setdiff(choices, c(strata, fpc, ids)),
                        selected = weight)

    })

    # isolate({
    #   updateSelectInput(session, "sel_ids",
    #                     choices = setdiff(choices, c(strata, fpc, weight)),
    #                     selected = character(0))
    #
    #   updateSelectInput(session, "sel_strata",
    #                     choices = setdiff(choices, c(ids, fpc, weight)),
    #                     selected = character(0))
    #
    #   updateSelectInput(session, "sel_fpc",
    #                     choices = setdiff(choices, c(strata, ids, weight)),
    #                     selected = character(0))
    #
    #   updateSelectInput(session, "sel_weight",
    #                     choices = setdiff(choices, c(strata, fpc, ids)),
    #                     selected = character(0))
    #
    # })

    selected_ids(NULL)
    selected_strata(NULL)
    selected_fpc(NULL)
    selected_weight(NULL)

  })


  selected_ids <- reactiveVal(NULL)
  selected_strata <- reactiveVal(NULL)
  selected_fpc <- reactiveVal(NULL)
  selected_weight <- reactiveVal(NULL)

  desain_survei = reactiveVal(NULL)

  valid_inputs <- reactive({

    strata_ok = FALSE
    if(!is.null(input$sel_strata)){
      if(!is.na(input$sel_strata)){
        if(!input$sel_strata %in% c("", "NA")){
          strata_ok = TRUE
        }
      }
    }

    weight_ok = FALSE
    if(!is.null(input$sel_weight)){
      if(!is.na(input$sel_weight)){
        if(!input$sel_weight %in% c("", "NA")){
          weight_ok = TRUE
        }
      }
    }

    weight_ok && strata_ok

  })

  observeEvent(input$reset_design, {
    shinyjs::reset("side-panel-setdesign")
    shinyjs::reset("main-panel-setdesign")
    shinyjs::reset("side-panel-setdesign-data")
    shinyjs::reset("side-panel-setdesign-des")
    shinyjs::reset("side-panel-setdesign-tes")

    choices = c(varNameAll())

    updateSelectInput(session, "sel_ids", choices = choices, selected = character(0))
    updateSelectInput(session, "sel_strata", choices = choices, selected = character(0))
    updateSelectInput(session, "sel_fpc", choices = choices, selected = character(0))
    updateSelectInput(session, "sel_weight", choices = choices, selected = character(0))


    session$sendCustomMessage(type = "resetValue", message = "sel_ids")
    session$sendCustomMessage(type = "resetValue", message = "sel_strata")
    session$sendCustomMessage(type = "resetValue", message = "sel_fpc")
    session$sendCustomMessage(type = "resetValue", message = "sel_weight")


    desain_survei(NULL)

  })

  observeEvent(input$save_design,{


    if (!valid_inputs()) {
      showNotification("Strata dan Weight harus terisi", type = "error")
      return(NULL)
    }

    selected_ids(input$sel_ids)
    selected_strata(input$sel_strata)
    selected_fpc(input$sel_fpc)
    selected_weight(input$sel_weight)


#
#     if(is.null(input$sel_ids)){
#       print("NYAMPE A")
#       ids_sel <- "--NOTHING--"
#     }else{
#       if(is.na(input$ids_sel)){
#         print("NYAMPE B")
#         ids_sel <- "--NOTHING--"
#       }else{
#         print("NYAMPE C")
#         ids_sel <- ifelse(input$sel_ids %in% c("", "NA"), "--NOTHING--", input$sel_ids)
#       }
#     }
#
#     if(is.null(input$sel_fpc)){
#       print("NYAMPE D")
#       fpc_sel <- "--NOTHING--"
#     }else{
#       if(is.na(input$fpc_sel)){
#         print("NYAMPE E")
#         fpc_sel <- "--NOTHING--"
#       }else{
#         print("NYAMPE F")
#         fpc_sel <- ifelse(input$sel_fpc %in% c("", "NA"), "--NOTHING--", input$sel_fpc)
#       }
#     }

    ids_sel <- ifelse(is.null(input$sel_ids) || is.na(input$sel_ids) || input$sel_ids == "", "--NOTHING--", input$sel_ids)
    fpc_sel <- ifelse(is.null(input$sel_fpc) || is.na(input$sel_fpc) || input$sel_fpc == "", "--NOTHING--", input$sel_fpc)

    strata_sel = input$sel_strata
    weight_sel = input$sel_weight

    print(ids_sel)
    print(fpc_sel)

    dataku<-myData()

    if(ids_sel != "--NOTHING--"){
      if(fpc_sel != "--NOTHING--"){
        des <- dataku %>%
          mutate(across(strata_sel, \(x) factor(x))) %>%
          srvyr::as_survey_design(
            id = ids_sel,
            strata = strata_sel,
            fpc = fpc_sel,
            w = weight_sel
          )
      }else{
        des <- dataku %>%
          mutate(across(strata_sel, \(x) factor(x))) %>%
          srvyr::as_survey_design(
            id = ids_sel,
            strata = strata_sel,
            w = weight_sel
          )
      }
    }else{
      if(fpc_sel != "--NOTHING--"){
        des <- dataku %>%
          mutate(across(strata_sel, \(x) factor(x))) %>%
          srvyr::as_survey_design(
            id = 1,
            strata = strata_sel,
            fpc = fpc_sel,
            w = weight_sel
          )
      }else{
        des <- dataku %>%
          mutate(across(strata_sel, \(x) factor(x))) %>%
          srvyr::as_survey_design(
            id = 1,
            strata = strata_sel,
            w = weight_sel
          )
      }
    }

    desain_survei(des)

  })

  output$summaryDesain<-renderPrint(
    print(desain_survei())
  )

  # ESTIMASI -------------------------------------------------------

  #Pilih variabel untuk set desain sampel
  observeEvent(dataBrowse(), {

    varest <- (input$sel_varest)
    agg <- (input$sel_agg)
    denom <- (input$sel_denom)

    choices <- c(varNameAll())

    choices_num <- c(varName())
    choices_kat <- c(varNameKategorik())
    choices_int <- c(varNameInteger())


    choices_final <- c(choices_kat, choices_int)

    updateSelectInput(session, "sel_agg",
                      choices = choices_final,
                      selected = agg)


    updatePickerInput(session, "sel_varest",
                      choices = choices_num,
                      selected = varest)


    updateSelectInput(session, "sel_denom",
                      choices = choices_num,
                      selected = denom)


  })

  #Pilih variabel untuk set desain sampel
  observeEvent(input$sel_method, {

    varest <- (input$sel_varest)

    choices_num = c(varName())
    choices_kat = c(varNameKategorik())
    choices_int = c(varNameInteger())


    choices_zeroone <- c(varNameZeroOne())

    if(input$sel_method == "prop"){
      choices_final = choices_zeroone
    }else{
      choices_final = choices_num
    }

    updatePickerInput(session, "sel_varest", choices = choices_final, selected = character(0))


  })

  valid_inputs_est <- reactive({

    varest_ok = FALSE
    if(!any(is.null(input$sel_varest))){
      if(!any(is.na(input$sel_varest))){
        if(!any(input$sel_varest %in% c("", "NA"))){
          varest_ok = TRUE
        }
      }
    }

    agg_ok = FALSE
    if(!is.null(input$sel_agg)){
      if(!is.na(input$sel_agg)){
        if(!input$sel_agg %in% c("", "NA")){
          agg_ok = TRUE
        }
      }
    }

    varest_ok && agg_ok

  })

  valid_inputs_est_denom <- reactive({
    denom_ok = FALSE
    if(!is.null(input$sel_denom)){
      if(!is.na(input$sel_denom)){
        if(!input$sel_denom %in% c("", "NA")){
          denom_ok = TRUE
        }
      }
    }

    denom_ok
  })

  observeEvent(input$reset_est, {

    choices = c(varNameAll())
    choices_num = c(varName())
    choices_kat = c(varNameKategorik())
    choices_int = c(varNameInteger())


    if(input$sel_method == "prop"){
      choices_final = c(choices_kat, choices_int)
    }else{
      choices_final = choices_num
    }

    choices_kat_int = c(choices_kat, choices_int)

    updatePickerInput(session, "sel_varest", choices = choices_final, selected = character(0))
    updateSelectInput(session, "sel_agg", choices = choices_kat_int, selected = character(0))
    updateSelectInput(session, "sel_denom", choices = choices_num, selected = character(0))


    session$sendCustomMessage(type = "resetValue", message = "sel_varest")
    session$sendCustomMessage(type = "resetValue", message = "sel_agg")
    session$sendCustomMessage(type = "resetValue", message = "sel_denom")


  })

  estimate = reactiveVal(NULL)

  observeEvent(input$go_est,{

    req(desain_survei())
    if (!valid_inputs_est()) {
      showNotification("Semua harus terisi", type = "error")
      return(NULL)
    }
#
#     srvyr::survey_mean()
#
#
    des_survei = desain_survei()

    my_narm = ifelse(input$sel_narm == "true", T, F)

    if(input$sel_method != "ratio"){
      est = des_survei %>%
        srvyr::group_by_at(input$sel_agg) %>%
        srvyr::summarise(
          across(
            input$sel_varest,
            list(
              est = ~survey_est(est_type = input$sel_method, .x, deff = T, vartype = c("se", "var", "cv", "ci"), na.rm = T),
              n = ~sum(!is.na(.x))
            )
          )
        )
    }else{
      est = des_survei %>%
        srvyr::group_by_at(input$sel_agg) %>%
        srvyr::summarise(
          across(
            input$sel_varest,
            list(
              est = ~survey_est(est_type = input$sel_method, .x, denominator = !!sym(input$sel_denom), deff = T, vartype = c("se", "var", "cv", "ci"), na.rm = T),
              n = ~sum(!is.na(.x))
            )
          )
        )

    }

    estimate(est)

  })

  output$data_est<-renderDataTable({
    req(estimate())
    df<-estimate()
    rownames(df)<-1:nrow(df)
    DT::datatable(df,
                  options = list(
                    "scrollY"="500px",
                    "scrollX"=TRUE,
                    "searching"=FALSE,
                    "paging"=FALSE
                  ))
  })


  #Download hasil estimasi
  output$downloadDataEst <- downloadHandler(
    filename = function(){
      if((input$namaFileEst)==""){
        paste("data-",gsub(":", "_", substr(Sys.time(), 1,19)),".",input$tipeFileEst,sep = "")
      }else{
        paste(input$namaFileEst,".",input$tipeFileEst, sep="")
      }

    },
    content = function(file) {
      df<-estimate()
      #write.csv(df, file, row.names = FALSE)
      rio::export(df, file)
    }
  )


# VISUALISASI -------------------------------------------------------------

  #Susun data untuk histogram
  data_CI<-reactive({
    req(estimate())
    df<-estimate()
    df_long_est = df %>%
      dplyr::select(!!sym(input$sel_agg), ends_with("est"), ends_with("low"), ends_with("upp")) %>%
      tidyr::pivot_longer(!input$sel_agg, names_to = "Variable", values_to = "Estimate") %>%
      dplyr::mutate(
        tes = substr(Variable, (nchar(Variable) - 3), nchar(Variable)) ,
        type = case_when(
          tes == "_est" ~ "EST",
          tes == "_low" ~ "LOW",
          tes == "_upp" ~ "UPP",
        ),
        Variable = gsub("_est", "", Variable)
      ) %>%
      rowwise() %>%
      mutate(
        Variable = gsub(tes, "", Variable)
      ) %>%
      select(-c(tes)) %>%
      tidyr::pivot_wider(names_from = type, values_from = Estimate)

    df_long_est
  })

  #Buat plot histogram
  output$plotPointEstimate<-renderPlot({
    toPlot<-data_CI()

    gg <- toPlot %>%
      ggplot(aes(x = !!sym(input$sel_agg), y = EST)) +
      geom_point(size = 2) +
      geom_errorbar(aes(ymin = LOW, ymax = UPP), width = 0.2) +
      facet_wrap(~Variable, scales = "free", ncol = 1)+
      theme_hc()+
      theme(plot.background = element_rect(fill = "#ecf0f1"))

    gg+ggtitle(paste("Point Estimate and Confidence Interval"))+theme(plot.title = element_text(face="bold", size=20,hjust = 0.5))
  }, height = function() {
    vars <- length(input$sel_varest)
    # 200px per variable + 100px extra for titles/margins
    200 * vars + 100
  })

  #Susun data untuk histogram
  data_RSE <- reactive({
    req(estimate())
    df<-estimate()
    df_long_rse <- df %>%
      dplyr::select(!!sym(input$sel_agg), ends_with("cv")) %>%
      tidyr::pivot_longer(!input$sel_agg, names_to = "Variable", values_to = "RSE") %>%
      dplyr::mutate(
        Variable = gsub("_est_cv", "", Variable),
        RSE = RSE*100,
        RSE = round(RSE, 2)
      )


    df_long_rse
  })

  output$rse_summary_table<-renderDataTable({
    req(data_RSE())
    df<-data_RSE()

    df = df %>%
      mutate(RSE = ifelse(RSE == 0, NA, RSE)) %>%
      dplyr::group_by(Variable) %>%
      summarise(
        MIN = min(RSE, na.rm = T),
        Q1 = quantile(RSE, 1/4, na.rm = T),
        MEAN = min(RSE, na.rm = T),
        MEDIAN = min(RSE, na.rm = T),
        Q3 = quantile(RSE, 3/4, na.rm = T),
        MAX = max(RSE, na.rm = T),
        `Not Available` = sum(is.na(RSE)),
      )

    rownames(df)<-1:nrow(df)
    DT::datatable(df,
                  options = list(
                    "scrollY"="200px",
                    "scrollX"=TRUE,
                    "searching"=FALSE,
                    "paging"=FALSE
                  ))
  })

  output$rse_kat_table<-renderDataTable({
    req(data_RSE())
    df<-data_RSE()

    df = df %>%
      distinct(Variable) %>%
      tidyr::crossing(
        data.frame(cv_kat = c("(0,25]", "(25,50]", "(50,Inf]", "NA"))
      ) %>%
      left_join(
        df %>%
          dplyr::group_by(Variable) %>%
          mutate(
            cv_kat = cut(x = RSE, breaks = c(0, 25, 50, Inf)),
            cv_kat = as.character(cv_kat),
            cv_kat = ifelse(is.na(cv_kat), "NA", cv_kat)
          ) %>%
          dplyr::group_by(Variable, cv_kat) %>%
          dplyr::summarise(
            Jumlah = n()
          ), by = c("Variable", "cv_kat")
      ) %>%
      replace(is.na(.), 0) %>%
      tidyr::pivot_wider(names_from = cv_kat, values_from = Jumlah)

    rownames(df)<-1:nrow(df)
    DT::datatable(df,
                  options = list(
                    "scrollY"="200px",
                    "scrollX"=TRUE,
                    "searching"=FALSE,
                    "paging"=FALSE
                  ))
  })

  #Buat plot histogram
  output$plotRSE<-renderPlot({
    toPlot<-data_RSE()

    gg <- toPlot %>%
      distinct(Variable) %>%
      tidyr::crossing(
        data.frame(cv_kat = c("(0,25]", "(25,50]", "(50,Inf]", "NA"))
      ) %>%
      left_join(
        toPlot %>%
          dplyr::group_by(Variable) %>%
          mutate(
            cv_kat = cut(x = RSE, breaks = c(0, 25, 50, Inf)),
            cv_kat = as.character(cv_kat),
            cv_kat = ifelse(is.na(cv_kat), "NA", cv_kat)
          ) %>%
          dplyr::group_by(Variable, cv_kat) %>%
          dplyr::summarise(
            Jumlah = n()
          ), by = c("Variable", "cv_kat")
      ) %>%
      replace(is.na(.), 0) %>%
      ggplot(aes(x = cv_kat, y = Jumlah, fill = cv_kat, label = Jumlah)) +
      geom_bar(stat = "identity", , position = 'dodge') +
      geom_text(position = position_dodge(width = 1), vjust = -0.5) +
      facet_wrap(~Variable, ncol = 1)+
      theme_hc()+
      theme(plot.background = element_rect(fill = "#ecf0f1"))

    gg+ggtitle(paste("RSE"))+theme(plot.title = element_text(face="bold", size=20,hjust = 0.5))
  }, height = function() {
    vars <- length(input$sel_varest)
    # 200px per variable + 100px extra for titles/margins
    200 * vars + 100
  })


}

# Run the application
shinyApp(ui = ui, server = server)

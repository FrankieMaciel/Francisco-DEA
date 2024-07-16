library(lintr)
library(shiny)
library(shinycssloaders)
library(pracma)
library(fractaldim)
library(openxlsx)
library(shinyjs)
library(Benchmarking)
# Trocar por DEAR / RDEA
library(shinydashboard)
library(shinyalert)
library(DT)
library(TSA)
library(longmemo)
library(ptsuite)
# Não está mais disponivel no CRAN
library(plyr)
library(shinyFiles)
library(tools)
library(htmltools)
library(markdown)

# Constants
datasetFolder <- "./datasets/"
tableColumnNames <- c("DMU", "FractalDim", "TCP_AVG", "Hurst", "Var", "Whittle's Estimator", "Alfa Tail Shape Parameter")

## JavaScript that dis/enables the ABILITY to click the tab (without changing aesthetics)

jsname <- c(
  "table.on('key', function(e, datatable, key, cell, originalEvent){",
  "  var targetName = originalEvent.target.localName;",
  "  if(key == 13 && targetName == 'body'){",
  "    $(cell.node()).trigger('dblclick.dt');",
  "  }",
  "});",
  "table.on('keydown', function(e){",
  "  var keys = [9,13,37,38,39,40];",
  "  if(e.target.localName == 'input' && keys.indexOf(e.keyCode) > -1){",
  "    $(e.target).trigger('blur');",
  "  }",
  "});",
  "table.on('key-focus', function(e, datatable, cell, originalEvent){",
  "  var targetName = originalEvent.target.localName;",
  "  var type = originalEvent.type;",
  "  if(type == 'keydown' && targetName == 'input'){",
  "    if([9,37,38,39,40].indexOf(originalEvent.keyCode) > -1){",
  "      $(cell.node()).trigger('dblclick.dt');",
  "    }",
  "  }",
  "});"
)

ui <- navbarPage(
  title = "FRANCISCO", id = "tabs",
  tabPanel(
    "Inicio",
    useShinyjs(), # Set up shinyjs

    sidebarLayout(
      sidebarPanel(
        fileInput("idArquivo", "Selecione o seu arquivo", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values", "text/tab-separated-values", ".txt", ".xlsx", ".csv", ".tsv")),
        shinyDirButton("folder", "Selecione a pasta do seu dataset", "Por favor selecione uma pasta", FALSE),
        sliderInput("windowSize", "Tamanho da janela temporal:", min = 1, max = 10, value = 1, step = 1),
        sliderInput("windowIndex", "Janela temporal à ser exibida:", min = 1, max = 10, value = 1, step = 1),
        tags$hr(),
        selectInput("typDMU", "Tipo de Arquivo:",
          c("DMU do APACHE BENCH" = "apache", "DMU do IPERF" = "iperf", "DMU Númerica" = "numerica", "Tabela" = "tabela"),
          selected = "apache"
        ),
        selectInput("variable", "Variaveis:",
          c(
            "FractalDim" = "FractalDim",
            "TCP_AVG" = "TCP_AVG",
            "Hurst" = "Hurst",
            "Var" = "Var",
            "Whittle's Estimator" = "Whittle's Estimator",
            "Alfa Tail Shape Parameter" = "Alfa Tail Shape Parameter"
          ),
          multiple = TRUE
        ),
        selectInput(
          "sep", "Metódos de Dimensão Fractal:",
          c(
            "Madogram" = "madogram", "Variogram" = "variogram", "Rodogram" = "rodogram",
            "Variation" = "variation", "Incr1" = "incr1", "Boxcount" = "boxcount",
            "Hallwood" = "hallwood", "Periodogram" = "periodogram", "Wavelet" = "wavelet",
            "DctII" = "dctII", "Genton" = "genton"
          )
        ),
        actionButton("idBotao", "Gerar Tabela"),
        div(
          id = "oculDEA",
          selectInput("idInputs", "Selecione Inputs", choices = NULL, selected = NULL, multiple = TRUE),
          selectInput("idOutputs", "Selecione Outputs", choices = NULL, selected = NULL, multiple = TRUE),
          selectInput("mod", "Modelo:",
            c("CCR" = "CRS", "BCC" = "VRS", "SCCR" = "SCCR", "SBM" = "ADD"),
            selected = "CRS"
          ),
          selectInput("ori", "Orientacoes:",
            c("IN" = "in", "OUT" = "out"),
            selected = "in"
          ),
          actionButton("idDEA", "Analyse DEA"),
        ),
        width = 3
      ),
      mainPanel(
        shinycssloaders::withSpinner(
          DTOutput("tbl")
        ),
        div(
          id = "oculButton",
          actionButton("idDeleteRows", " Delete Linha"),
          downloadButton("downloadTable", "Baixar"),
        ),
        shinyjs::hidden(
          div(
            id = "conteudoOpcional",
            actionButton("idAtualizar", "Atualizar"),
          )
        ),
      )
    )
  ),
  tabPanel(
    "Ajuda",
    fluidPage(
      includeMarkdown("./ajuda.md")
    )
  ),
  tabPanel(
    "DMU Analysis",
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        tags$head(tags$style("
        section.content {
          overflow-y: hidden;
        }
        ")),
        tags$h1("DMU Analysis"),
        shinyjs::hidden(
          div(
            id = "semSerieTemporal",
            tags$h1("Sem serie Temporal"),
          )
        ),
        shinyjs::hidden(
          div(
            id = "SerieTemporal",
            box(
              title = "DMU", status = "danger", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              textOutput("nameDMU"),
            ),
            box(
              title = "Fractal Dimension", status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = 4,
              textOutput("valueFractalDim")
            ),
            box(
              title = "TCP Average", status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = 4,
              textOutput("valueTCP_AVG")
            ),
            box(
              title = "Hurst Parameter", status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = 4,
              textOutput("valueHurst")
            ),
            box(
              title = "Variance", status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = 4,
              textOutput("valueVar")
            ),
            box(
              title = "Whittle's Estimator", status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = 4,
              textOutput("valueWhittlesEstimator")
            ),
            box(
              title = "Alfa Tail Shape Parameter", status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = 4,
              textOutput("valueTailParameter")
            ),
            box(
              title = "Periodogram 1", status = "primary", solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("graficoDMUPeriodograma"),
            ),
            box(
              title = "ACF Plot", status = "primary", solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("graficoDMUPeriodogramaacf"),
            ),
            box(
              title = "Histograma", status = "primary", solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("graficoDMUHistograma"),
            ),
            box(
              title = "Periodogram 2", status = "primary", solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("graficoDMUts"),
            ),
            box(
              title = "LLCD", status = "primary", solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("graficoLLCD"),
            ),
          )
        ),
      )
    )
  ),
  tabPanel(
    "DEA Table",
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        tags$h3("Table"),
        box(
          title = NULL, status = "info", solidHeader = TRUE,
          collapsible = FALSE, width = 4,
          tableOutput("OutDEA"),
        ),
      )
    )
  ),
  tabPanel(
    "GRAPHIC DEA",
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        tags$h3("Graphic"),
        tags$h1("Efficient Frontier"),
        plotOutput("graficoDea"),
        tags$h1("Ray Unbounded"),
        plotOutput("graficoDeafrontier")
      )
    )
  ),
)

fooTable <<- data.frame(matrix(ncol = 7, nrow = 0))
fooTableDEA <<- data.frame()
fooDash <<- data.frame()
timeSeries <<- list()
colnames(fooTable) <- tableColumnNames
maxVectorSize <- 0

global <<- reactiveValues(response = FALSE)
options(browser = "firefox")
options(shiny.port = 8888)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.

options(shiny.maxRequestSize = 9 * 1024^2)

processTableData <- function(filesDirsTable, fileIndex) {
  arquivo <- read.xlsx(filesDirsTable[[fileIndex, "datapath"]], startRow = 1)
  if (is.null(arquivo)) stop()
  if (nrow(fooTable) > 0) {
    fooTable <<- rbind(fooTable[, colnames(arquivo)], arquivo)
    fooTable[, 2:ncol(fooTable)] <<- sapply(fooTable[2:ncol(fooTable)], as.numeric)
    fooTable[, 2:ncol(fooTable)] <<- format(round(fooTable[, 2:ncol(fooTable)], 3), nsmall = 3)
    options(scipen = 999)
    str(fooTable)
    fooDash <<- fooTable
  } else {
    fooTable <<- arquivo
    fooTable[, 2:ncol(fooTable)] <<- sapply(fooTable[2:ncol(fooTable)], as.numeric)
    fooTable[, 2:ncol(fooTable)] <<- format(round(fooTable[, 2:ncol(fooTable)], 3), nsmall = 3)
    fooDash <<- fooTable
  }
  listTable <- vector(mode = "list", length = nrow(arquivo))
  timeSeries <<- c(timeSeries, listTable)
}

processIperfData <- function(filesDirsTable, fileIndex) {
  arquivo <- read.csv(filesDirsTable[[fileIndex, "datapath"]], header = FALSE, sep = "", skip = 6)
  df <- data.frame(arquivo[, 8], arquivo[, 7])

  if (is.null(df)) return()
  colnames(df) <- c("Um", "Dois")
  new_df <- df[!grepl("K", df$Um), ]
  new_df2 <- df[!grepl("K", df$Dois), ]
  colnames(new_df) <- c("DMU", "EX")
  colnames(new_df2) <- c("EX", "DMU")
  df_vetor <- rbind(new_df, new_df2)
  return(as.numeric(as.character(df_vetor$DMU)))
}

processApacheData <- function(filesDirsTable, fileIndex) {
  arquivo <- read.csv(filesDirsTable[[fileIndex, "datapath"]], header = FALSE, sep = ",", skip = 1)
  return(c(as.numeric(unlist(arquivo[2]))))
}

processOtherData <- function(filesDirsTable, fileIndex) {
  arquivo <- read.table(filesDirsTable[[fileIndex, "datapath"]])
  if (is.null(arquivo)) return()
  else return(c(as.numeric(unlist(arquivo))))
}

calculateDMU <- function(filesDirsTable, inputType, fractalDivisionMethod, userSelectedVectorSize, userSelectedWindow) {
  kindOfProcess <- NULL
  if (inputType == "iperf") kindOfProcess <- processIperfData
  else if (inputType == "apache") kindOfProcess <- processApacheData
  else kindOfProcess <- processOtherData

  biggestVector <- 0
  maxAmountOfWindows <- 0

  # find biggest vetor
  vectors <- lapply(seq_along(filesDirsTable[, 1]), function(nr) {
    vetor <- kindOfProcess(filesDirsTable, nr)

    if (length(vetor) > biggestVector) biggestVector <<- length(vetor)
    return(vetor)
  })

  maxAmountOfWindows <- floor(biggestVector / 25)
  amountPerWindow <- floor(biggestVector / userSelectedVectorSize)

  # process data
  for (nr in seq_along(filesDirsTable[, 1])) {
    name <- tools::file_path_sans_ext(filesDirsTable[[nr, "name"]])
    vetor <- vectors[[nr]]
    num_full_groups <- length(vetor) %/% amountPerWindow
    remainder <- length(vetor) %% amountPerWindow

    vetor_particionado <- split(vetor, c(rep(1:num_full_groups, each = amountPerWindow), rep(num_full_groups + 1, remainder)))
    if (remainder > 0) {
      vetor_particionado <- vetor_particionado[-length(vetor_particionado)]
    }
    subvetor <- vetor_particionado[[userSelectedWindow]]

    dim <- as.numeric(unlist(fd.estimate(subvetor, method = fractalDivisionMethod)[2]))
    dim <- format(round(dim, 3), nsmall = 3)

    media <- mean(subvetor, na.rm = FALSE)
    media <- format(round(media, 3), nsmall = 3)

    hurst <- as.numeric(unlist(hurstexp(subvetor, display = FALSE, d = amountPerWindow)[1]))
    hurst <- format(round(hurst, 3), nsmall = 3)

    varianca <- as.numeric(unlist(var(subvetor, na.rm = TRUE)[1]))
    varianca <- format(round(varianca, 3), nsmall = 3)

    whittleEstimator <- WhittleEst(subvetor)
    whittleEstimator <- whittleEstimator$coefficients[[1, 1]]
    whittleEstimator <- format(round(whittleEstimator, 3), nsmall = 3)

    # o vetor não pode ter zeros
    d <- subvetor
    d <- d[d != 0]
    tailParameter <- alpha_mle(d)
    tailParameter <- format(round(tailParameter$shape, 3), nsmall = 3)

    if (is.element("NULL", timeSeries)) {
      fooTableDMU <- data.frame(matrix(ncol = 7, nrow = 0))
      fooDashDMU <- data.frame(matrix(ncol = 7, nrow = 0))

      colnames(fooTableDMU) <- tableColumnNames
      colnames(fooDashDMU) <- tableColumnNames
      fooTableDMU[nrow(fooTableDMU) + 1, ] <- c(name, dim, media, hurst, varianca, whittleEstimator, tailParameter)
      fooTable <<- rbind(fooTable, fooTableDMU[, colnames(fooTable)])

      fooDashDMU[nrow(fooTable), ] <- c(name, dim, media, hurst, varianca, whittleEstimator, tailParameter)
      fooDash <<- fooDashDMU
    } else {
      if (ncol(fooTable) < 7) {
        fooTable <<- data.frame(matrix(ncol = 7, nrow = 0))
        colnames(fooTable) <<- tableColumnNames
      }

      fooTable[nrow(fooTable) + 1, ] <<- c(name, dim, media, hurst, varianca, whittleEstimator, tailParameter)
      fooDash <<- fooTable
    }
    timeSeries[length(timeSeries) + 1] <<- list(subvetor)
  }
  return(maxAmountOfWindows)
}

checkAndProcessData <- function(filesDirsTable, inputType, choosenVariables, fractalDivisionMethod, userSelectedVectorSize, userSelectedWindow) {
  if (is.null(filesDirsTable) || length(filesDirsTable[, 1]) < 1) stop("Sem arquivo")
  if (inputType == "tabela") {
    for (nr in seq_along(filesDirsTable[, 1])) {
      processTableData(filesDirsTable, nr)
    }
  } else {
    maxAmountOfWindows <- calculateDMU(filesDirsTable, inputType, fractalDivisionMethod, userSelectedVectorSize, userSelectedWindow)
  }

  if (is.null(choosenVariables)) {
    if (inputType == "tabela" || is.element("NULL", timeSeries)) {
      fooTableDEA <<- fooTable
    } else {
      fooTableDEA <<- fooTable[, tableColumnNames]
    }
  } else {
    if (is.element("NULL", timeSeries)) {
      vetor <- c(colnames(fooTable[, 2:ncol(fooTable)]) == choosenVariables)
      numeroTRUE <- sum(vetor, na.rm = TRUE)
      numeroVar <- length(choosenVariables)
      condTRUE <- numeroTRUE == numeroVar

      if (is.element("FALSE", vetor) && !isTRUE(condTRUE)) {
        fooTableDEA <<- fooTable
        stop("O limite máximo de colunas desta tábela é o número de colunas da tábela adicionada, caso queira mais colunas, tera que adicionar DMU por DMU")
      }
    }
    fooTableDEA <<- fooTable[, c("DMU", choosenVariables)]
  }
  return(maxAmountOfWindows)
}

server <- function(input, output, session) {
  hide("oculDEA")
  hide("oculButton")
  hideTab(inputId = "tabs", target = "DMU Analysis")
  hideTab(inputId = "tabs", target = "DEA Table")
  hideTab(inputId = "tabs", target = "GRAPHIC DEA")
  disable("windowSize")
  disable("windowIndex")
  disable("tbl")
  output$tbl <- renderDT(data.frame(matrix(ncol = 0, nrow = 0)))

  # selecionar dataset
  shinyDirChoose(input, "folder", roots = c(wd = datasetFolder), filetypes = c("", "txt", "csv"))

  filesDirsTable <- reactiveVal(data.frame(datapath = character(), name = character(), stringsAsFactors = FALSE))
  oldFileIndex <- reactiveVal(0)

  session$onSessionEnded(function() {
    fooTable <<- data.frame(matrix(ncol = 7, nrow = 0))
    colnames(fooTable) <<- tableColumnNames
    fooTableDEA <<- data.frame()
    fooDash <<- data.frame()
    timeSeries <<- list()
    hideTab(inputId = "tabs", target = "DMU Analysis")
    click("idAtualizar")
  })

  # Loads all files in the selected folder when the user clicks to select the folder
  observe({
    chosen_folder <- reactive(input$folder)
    req(is.list(input$folder))
    datasetDir <- parseDirPath(c(wd = datasetFolder), chosen_folder())
    files_in_folder <- list.files(path = datasetDir, full.names = TRUE, recursive = TRUE, pattern = "\\.(csv)$")
    arquivos_df <- data.frame(
      datapath = files_in_folder,
      name = file_path_sans_ext(basename(files_in_folder)),
      stringsAsFactors = FALSE
    )
    filesDirsTable(arquivos_df)
    # TODO: Hide window selection slider
    disable("windowSize")
    disable("windowIndex")
    updateSliderInput(session, "windowIndex", value = 1)
    updateSliderInput(session, "windowSize", value = 1)
  })

  # loads the file when the user selects it
  observeEvent(input$idArquivo, {
    req(input$idArquivo)

    arquivo_caminho <- input$idArquivo$datapath
    arquivo_nome <- tools::file_path_sans_ext(basename(input$idArquivo$name))

    filesDirsTable(data.frame(datapath = arquivo_caminho, name = arquivo_nome, stringsAsFactors = FALSE))
  })

  observeEvent(input$windowSize, {
    updateSliderInput(session, "windowIndex", max = input$windowSize)
  })

  observeEvent(input$idBotao, {
    arquivos_df <- filesDirsTable()

    processEvent <- function(filesDirsTable) {
      output$tbl <- renderDT(
        data.frame(),
        callback = JS(jsname),
        extensions = "KeyTable",
        options = list(keys = TRUE),
        selection = "single"
      )
      tryCatch(
        withCallingHandlers(
          {
            maxAmountOfWindows <- checkAndProcessData(filesDirsTable, input$typDMU, input$variable, input$sep, input$windowSize, input$windowIndex)
            updateSliderInput(session, "windowSize", max = maxAmountOfWindows)
            enable("windowSize")
            enable("windowIndex")
          },
          warning = function(msg) {
            shinyalert(
              title = "Aviso",
              text = toString(msg),
              type = "warning",
              closeOnClickOutside = TRUE,
              showCancelButton = FALSE,
              showConfirmButton = TRUE,
              confirmButtonText = "OK",
              confirmButtonCol = "darkred",
            )
          }
        ),
        error = function(msg) {
          shinyalert(
            title = "Erro",
            text = toString(msg),
            type = "error",
            closeOnClickOutside = TRUE,
            showCancelButton = FALSE,
            showConfirmButton = TRUE,
            confirmButtonText = "OK",
            confirmButtonCol = "darkred",
          )
          stop(msg)
        }
      )

      oldFileIndex(length(filesDirsTable))
      if (nrow(fooTableDEA) == 1) {
        hide("oculDEA")
        hideTab(inputId = "tabs", target = "DEA Table")
        hideTab(inputId = "tabs", target = "GRAPHIC DEA")
      }
      rownames(fooTableDEA) <<- seq_len(nrow(fooTableDEA))
      rownames(fooTable) <<- seq_len(nrow(fooTable))
      rownames(fooDash) <<- seq_len(nrow(fooDash))
      shinyalert(title = "Pronto!", type = "success")

      if (input$typDMU != "tabela") {
        if (is.null(input$variable)) {
          fooTable <<- fooDash[, tableColumnNames]
        } else {
          fooTable <<- fooDash[, c("DMU", input$variable)]
        }
        fooTableDEA <<- fooTable
      }

      if (nrow(fooTableDEA) > 1) {
        show("oculDEA")
        updateSelectInput(session, "idInputs",
          choices = colnames(fooTableDEA[, 2:ncol(fooTableDEA)]),
        )

        updateSelectInput(session, "idOutputs",
          choices = colnames(fooTableDEA[, 2:ncol(fooTableDEA)]),
        )
      }

      if (nrow(fooTable) > 0) show("oculButton")

      enable("tbl")
      output$tbl <- renderDT(
        fooTableDEA,
        editable = list(target = "cell", disable = list(columns = c(0, 2:ncol(fooTableDEA)))),
        callback = JS(jsname),
        extensions = "KeyTable",
        options = list(keys = TRUE),
        selection = "single"
      )

      output$downloadTable <- downloadHandler(
        filename = "table.xlsx",
        content = function(file) {
          write.xlsx(fooTableDEA, file, rowNames = FALSE)
        }
      )
    }

    # check if there has already data
    if (nrow(fooTable) >= 1) {
      shinyalert(
        title = "Deseja criar uma nova Tabela?",
        text = "Caso não, aumentaremos o numero de linhas da tabela atual",
        type = "warning",
        closeOnClickOutside = TRUE,
        showCancelButton = TRUE,
        cancelButtonText = "Não",
        showConfirmButton = TRUE,
        confirmButtonText = "Sim",
        confirmButtonCol = "darkred",
        callbackR = function(input) {
          if (input == "TRUE") {
            # Removes the previous data
            fooTable <<- data.frame(matrix(ncol = 7, nrow = 0))
            colnames(fooTable) <<- tableColumnNames
            fooTableDEA <<- data.frame()
            fooDash <<- data.frame()
            timeSeries <<- list()
            hideTab(inputId = "tabs", target = "DMU Analysis")
            click("idAtualizar")
            # remove everything from filesDirTable before oldFileIndex
            filesTable <- filesDirsTable()
            oldIndex <- oldFileIndex()
            if (nrow(filesTable) > oldIndex) {
              filesDirsTable(filesTable[(oldIndex + 1):nrow(filesTable), ])
            } else {
              filesDirsTable(data.frame(datapath = character(), name = character(), stringsAsFactors = FALSE))
            }
          }
          processEvent(filesDirsTable())
        }
      )
    } else {
      processEvent(filesDirsTable())
    }
  })

  observeEvent(input$idAtualizar, {
    if (nrow(fooTable) < 2) {
      if (nrow(fooTable) < 1) hide("oculButton")
      hide("oculDEA")
      hideTab(inputId = "tabs", target = "DEA Table")
      hideTab(inputId = "tabs", target = "GRAPHIC DEA")
    }

    output$tbl <- renderDT(
      fooTableDEA,
      editable = list(target = "cell", disable = list(columns = c(0, 2:ncol(fooTableDEA)))),
      callback = JS(jsname),
      extensions = "KeyTable",
      options = list(
        keys = TRUE
      ), selection = "single"
    )
  })

  observeEvent(input$idDeleteRows, {
    if (!is.null(isolate(input$tbl_rows_selected))) {
      row <- isolate(input$tbl_rows_selected)
      fooTable <<- isolate(fooTable[-row, ])
      fooTableDEA <<- isolate(fooTableDEA[-row, ])
      fooDash <<- isolate(fooDash[-row, ])

      rownames(fooTableDEA) <<- seq_len(nrow(fooTableDEA))
      rownames(fooTable) <<- seq_len(nrow(fooTable))
      rownames(fooDash) <<- seq_len(nrow(fooDash))

      timeSeries <<- timeSeries[-row]
      if (nrow(fooTable) > 1) click("idDEA")
      click("idAtualizar")
    } else {
      shinyalert(
        title = "Selecione a linha que deseja Excluir",
        type = "error",
        closeOnClickOutside = TRUE,
        showCancelButton = FALSE,
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "darkred",
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(input$tbl_cell_edit, {
    row <- input$tbl_cell_edit$row
    clmn <- input$tbl_cell_edit$col
    fooTable[row, clmn] <<- input$tbl_cell_edit$value
    fooTableDEA[row, clmn] <<- input$tbl_cell_edit$value
    fooDash[row, clmn] <<- input$tbl_cell_edit$value
    click("idAtualizar")
  })

  observeEvent(input$tbl_rows_selected, {
    row <- input$tbl_rows_selected
    if (length(timeSeries[[row]]) > 0) {
      showTab(inputId = "tabs", target = "DMU Analysis")
      hide(id = "semSerieTemporal")
      show(id = "SerieTemporal")
      show(id = "oi")
      output$nameDMU <- renderText(fooDash[row, 1])
      output$valueFractalDim <- renderText(fooDash[row, 2])
      output$valueTCP_AVG <- renderText(fooDash[row, 3])
      output$valueHurst <- renderText(fooDash[row, 4])
      output$valueVar <- renderText(fooDash[row, 5])
      output$valueWhittlesEstimator <- renderText(fooDash[row, 6])
      output$valueTailParameter <- renderText(fooDash[row, 7])
      output$graficoDMUHistograma <- renderPlot(hist(timeSeries[[row]], col = "darkblue", border = "black"))
      output$graficoDMUPeriodograma <- renderPlot(periodogram(timeSeries[[row]]))
      output$graficoDMUPeriodogramaacf <- renderPlot(acf(timeSeries[[row]]))
      output$graficoDMUts <- renderPlot(spec.pgram(timeSeries[[row]]))
      c <- timeSeries[[row]]
      c <- c[c != 0]
      log_log_timeseries <- log(c)
      output$graficoLLCD <- renderPlot(plot(ecdf(log_log_timeseries)))
    } else {
      showTab(inputId = "tabs", target = "DMU Analysis")
      show(id = "oi")
      hide(id = "SerieTemporal")
      show(id = "semSerieTemporal")
    }
  })

  observeEvent(input$idDEA, {
    showTab(inputId = "tabs", target = "DEA Table")
    showTab(inputId = "tabs", target = "GRAPHIC DEA")


    dataMatrix <- as.matrix(sapply(fooTableDEA[, 2:ncol(fooTableDEA)], as.numeric))

    rownames(dataMatrix) <- fooTableDEA[, "DMU"] # drop the no data rows in dataMatrix
    delete.na <- function(DF, n = 0) {
      DF[rowSums(is.na(DF)) <= n, ]
    }

    data_dea <- delete.na(dataMatrix) # creates the data_dea table

    if (is.null(input$idInputs)) {
      inputs <- data_dea[, "FractalDim"] # select only input variables values
    } else {
      inputs <- data_dea[, input$idInputs] # select only input variables values
    }

    if (is.null(input$idOutputs)) {
      if (colnames(data_dea)[1] != "FractalDim") {
        col_idx <- grep("FractalDim", names(data_dea))
        data_dea <- data_dea[, c(col_idx, (seq_len(data_dea))[-col_idx])]
        outputs <- data_dea[, c(2:ncol(data_dea))] # select only output variables values, SLACK=TRUE
      } else {
        outputs <- data_dea[, c(2:ncol(data_dea))] # select only output variables values, SLACK=TRUE
      }
    } else {
      outputs <- data_dea[, input$idOutputs] # select only input variables values
    }

    if (input$mod == "SCCR") {
      dea <- sdea(inputs, outputs, RTS = "CRS", ORIENTATION = input$ori) # runs super-efficiency input-oriented CCR DEA model

      output$graficoDea <- renderPlot(dea.plot(inputs, outputs, RTS = "CRS", ORIENTATION = input$ori))
      output$graficoDeafrontier <- renderPlot(dea.plot.frontier(inputs, outputs, RTS = "CRS"))
    } else if (input$mod == "ADD") {
      dea <- sdea(inputs, outputs, RTS = input$mod, ORIENTATION = input$ori)

      output$graficoDea <- renderPlot(dea.plot(inputs, outputs, RTS = input$mod, ORIENTATION = "in-out"))
      output$graficoDeafrontier <- renderPlot(dea.plot.frontier(inputs, outputs, RTS = input$mod))
    } else {
      dea <- dea(inputs, outputs, RTS = input$mod, ORIENTATION = input$ori, SLACK = TRUE)

      output$graficoDea <- renderPlot(dea.plot(inputs, outputs, RTS = input$mod, ORIENTATION = input$ori))
      output$graficoDeafrontier <- renderPlot(dea.plot.frontier(inputs, outputs, RTS = input$mod))
    }

    listDEA <- data.matrix(dea$eff)
    tableDea <- data.frame(listDEA)
    tableDea <- cbind(DMU = rownames(tableDea), tableDea)

    if (input$mod == "SCCR" || input$mod == "ADD") {
      colnames(tableDea) <- c("DMU", "Efficiency Ranking Index")
    } else {
      if (input$ori == "out") {
        tableDea[seq_len(tableDea), 2] <- 1 / tableDea[seq_len(tableDea), 2]
      }
      colnames(tableDea) <- c("DMU", "Efficiency Index")
    }

    rownames(tableDea) <- seq_len(tableDea)
    tableDea <- arrange(tableDea, desc(tableDea[, 2]))

    output$OutDEA <<- renderTable(tableDea, rownames = TRUE)
  })
}

shinyApp(ui, server)

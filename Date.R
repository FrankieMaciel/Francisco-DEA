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
library(dplyr)
library(shinyFiles)
library(tools)
library(htmltools)
library(markdown)
library(stringi)

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
        div(
          id = "folderDiv",
          shinyDirButton("folder", "Selecione a pasta do seu dataset", "Por favor selecione uma pasta", FALSE),
        ),
        sliderInput("windowSize", "Tamanho da janela temporal:", min = 1, max = 10, value = 1, step = 1),
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
        sliderInput("windowIndex", "Janela temporal à ser exibida:", min = 1, max = 10, value = 1, step = 1),
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
    value = "ajudaPage",
    fluidPage(
      div(
        style = "justify-content: center; display: flex;",
        textInput("search", "", placeholder = "buscar...", width = "70%"),
      ),
      div(
        style = "justify-content: center; display: flex; padding-inline: 5rem;",
        uiOutput("filteredMarkdown")
      )
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
        tags$h3("Indices DEA por janela de tempo"),
        DTOutput("OutDEA")
      )
    ),
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
  tags$style(HTML("
    .navbar-nav {
      float: none;
    }

    .navbar-nav li a[data-value='ajudaPage'] {
        right: 0;
        position: relative;
    }

    @media (min-width: 768px) {
      .navbar-nav li a[data-value='ajudaPage'] {
        right: 1rem;
        position: absolute;
      }
    }

    .nav li {
      position: static;
    }

    button#folder {
      overflow-wrap: break-word;
      display: inline-block;
      white-space: normal;
      width: 100%;
    }

    #folderDiv {
      justify-content: center;
      display: flex;
      margin: 0;
      padding: 0 0 2rem 0;
    }

    #idArquivo_progress {
      margin-bottom: 0;
    }
  "))
)

fooTable <<- data.frame(matrix(ncol = 7, nrow = 0))
fooTableDEA <<- data.frame()
fooDash <<- data.frame()
timeSeries <<- list()
colnames(fooTable) <- tableColumnNames
maxVectorSize <<- 0
biggestVector <<- 0
maxAmountOfWindows <<- 0
maxAmountOfIndexes <<- 0
vectors <<- list()

timeSeriesResultTable <<- list()
showDataFrame <<- data.frame()

global <<- reactiveValues(response = FALSE)
options(browser = "firefox")
options(shiny.port = 8888)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.

options(shiny.maxRequestSize = 9 * 1024^2)

editTable <- function(input, output) {
  row <- input$tbl_rows_selected
  timeseriesIndex <- input$windowIndex
  windowTimeSeries <- timeSeries[[timeseriesIndex]]
  selectedTimeSeries <- windowTimeSeries[[row]]

  if (length(selectedTimeSeries) < 1 && is.null(selectedTimeSeries)) {
    showTab(inputId = "tabs", target = "DMU Analysis")
    show(id = "oi")
    hide(id = "SerieTemporal")
    show(id = "semSerieTemporal")
    return()
  }

  showTab(inputId = "tabs", target = "DMU Analysis")
  hide(id = "semSerieTemporal")
  show(id = "SerieTemporal")
  show(id = "oi")
  output$nameDMU <- renderText(showDataFrame[row, 1])
  output$valueFractalDim <- renderText(showDataFrame[row, 2])
  output$valueTCP_AVG <- renderText(showDataFrame[row, 3])
  output$valueHurst <- renderText(showDataFrame[row, 4])
  output$valueVar <- renderText(showDataFrame[row, 5])
  output$valueWhittlesEstimator <- renderText(showDataFrame[row, 6])
  output$valueTailParameter <- renderText(showDataFrame[row, 7])
  output$graficoDMUHistograma <- renderPlot(hist(selectedTimeSeries, col = "darkblue", border = "black"))
  output$graficoDMUPeriodograma <- renderPlot(periodogram(selectedTimeSeries))
  output$graficoDMUPeriodogramaacf <- renderPlot(acf(selectedTimeSeries))
  output$graficoDMUts <- renderPlot(spec.pgram(selectedTimeSeries))
  c <- selectedTimeSeries
  c <- c[c != 0]
  log_log_timeseries <- log(c)
  output$graficoLLCD <- renderPlot(plot(ecdf(log_log_timeseries)))
}

renderShowTable <- function(output, session, WindowIndex) {
  if (nrow(showDataFrame) > 0) {
    show("oculDEA")
    show("oculButton")
    updateSelectInput(session, "idInputs",
      choices = colnames(showDataFrame[, 2:ncol(showDataFrame)]),
    )

    updateSelectInput(session, "idOutputs",
      choices = colnames(showDataFrame[, 2:ncol(showDataFrame)]),
    )
  }

  enable("tbl")
  output$tbl <- renderDT(
    showDataFrame,
    editable = list(target = "cell", disable = list(columns = c(0, 2:ncol(showDataFrame)))),
    callback = JS(jsname),
    extensions = "KeyTable",
    options = list(keys = TRUE),
    selection = "single"
  )
  output$downloadTable <- downloadHandler(
    filename = sprintf("table_timeseries_%s.xlsx", WindowIndex),
    content = function(file) {
      write.xlsx(showDataFrame, file, rowNames = FALSE)
    }
  )
}

createShowTable <- function(WindowIndex, choosenVariables, inputType, output, session) {
  if (is.null(timeSeriesResultTable) || length(timeSeriesResultTable) < 1) {
    return()
  }

  showTableWindow <- timeSeriesResultTable[[WindowIndex]]
  showDataFrame <<- data.frame(matrix(unlist(showTableWindow), nrow = length(showTableWindow), byrow = TRUE))
  colnames(showDataFrame) <<- tableColumnNames

  if (inputType != "tabela" && !is.null(choosenVariables)) {
    FiltereSshowDataFrame <- showDataFrame[, c("DMU", choosenVariables)]
    showDataFrame <<- FiltereSshowDataFrame
  }

  renderShowTable(output, session, WindowIndex)
}

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
  if (ncol(arquivo) < 8) {
    return(NULL)
  }
  df <- data.frame(arquivo[, 8], arquivo[, 7])
  print(df)
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
  if (ncol(arquivo) >= 2) {
    return(c(as.numeric(unlist(arquivo[2]))))
  } else {
    return(NULL)
  }
}

processOtherData <- function(filesDirsTable, fileIndex) {
  arquivo <- read.table(filesDirsTable[[fileIndex, "datapath"]])
  if (is.null(arquivo)) {
    return(NULL)
  } else {
    return(c(as.numeric(unlist(arquivo))))
  }
}

preprocessData <- function(filesDirsTable, inputType) {
  if (length(filesDirsTable) < 1) {
    stop("Selecione um arquivo ou dataset primeiro!")
  }
  kindOfProcess <- NULL
  if (inputType == "iperf") {
    kindOfProcess <- processIperfData
  } else if (inputType == "apache") {
    kindOfProcess <- processApacheData
  } else if (inputType == "tabela") {
    kindOfProcess <- processTableData
  } else {
    kindOfProcess <- processOtherData
  }

  # find biggest vetor
  vectors <<- lapply(seq_along(filesDirsTable[, 1]), function(nr) {
    vetor <- kindOfProcess(filesDirsTable, nr)
    if (is.null(vetor)) {
      shinyalert(
        title = "Aviso",
        text = "Selecione um tipo de arquivo válido!",
        type = "warning",
        closeOnClickOutside = TRUE,
        showCancelButton = FALSE,
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "darkred",
      )
      return(NULL)
    }
    if (length(vetor) > biggestVector) biggestVector <<- length(vetor)
    return(vetor)
  })
  if (is.null(vectors)) {
    return()
  }
  maxAmountOfWindows <<- floor(biggestVector / 25)
}

calculateDMU <- function(filesDirsTable, fractalDivisionMethod, userSelectedVectorSize) {
  amountPerWindow <- floor(biggestVector / userSelectedVectorSize)
  timeSeriesResultTable <<- list()
  # process data
  for (windowIndex in 1:userSelectedVectorSize) {
    DMUresults <- vector()
    timeSeriesResult <- list()
    for (nr in seq_along(filesDirsTable[, 1])) {
      name <- tools::file_path_sans_ext(filesDirsTable[[nr, "name"]])
      vetor <- vectors[[nr]]
      num_full_groups <- length(vetor) %/% amountPerWindow
      remainder <- length(vetor) %% amountPerWindow
      vetor_particionado <- split(vetor, c(rep(1:num_full_groups, each = amountPerWindow), rep(num_full_groups + 1, remainder)))
      if (remainder > 0) {
        vetor_particionado <- vetor_particionado[-length(vetor_particionado)]
      }

      subvetor <- vetor_particionado[[windowIndex]]
      timeSeriesResult <- c(timeSeriesResult, list(subvetor))

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

      results <- c(name, dim, media, hurst, varianca, whittleEstimator, tailParameter)
      DMUresults <- c(DMUresults, list(results))
    }

    if (is.element("NULL", timeSeries)) {
      fooTableDMU <- data.frame(matrix(ncol = 7, nrow = 0))
      fooDashDMU <- data.frame(matrix(ncol = 7, nrow = 0))

      colnames(fooTableDMU) <- tableColumnNames
      colnames(fooDashDMU) <- tableColumnNames
      fooTableDMU[nrow(fooTableDMU) + 1, ] <- list(DMUresults)
      print(fooTableDMU)
      fooTable <<- rbind(fooTable, fooTableDMU[, colnames(fooTable)])

      fooDashDMU[nrow(fooTable), ] <- list(DMUresults)
      fooDash <<- fooDashDMU
    } else {
      if (ncol(fooTable) < 7) {
        fooTable <<- data.frame(matrix(ncol = 7, nrow = 0))
        colnames(fooTable) <<- tableColumnNames
      }

      timeSeriesResultTable <<- c(timeSeriesResultTable, list(DMUresults))
      timeSeries <<- c(timeSeries, list(timeSeriesResult))
    }
  }
  maxAmountOfIndexes <<- length(timeSeriesResultTable)
}

checkAndProcessData <- function(filesDirsTable, inputType, choosenVariables, fractalDivisionMethod, userSelectedVectorSize) {
  if (is.null(filesDirsTable) || length(filesDirsTable[, 1]) < 1) {
    shinyalert(
      title = "Aviso",
      text = "Selecione um arquivo ou dataset primeiro!",
      type = "warning",
      closeOnClickOutside = TRUE,
      showCancelButton = FALSE,
      showConfirmButton = TRUE,
      confirmButtonText = "OK",
      confirmButtonCol = "darkred",
    )
    return(FALSE)
  }
  if (inputType == "tabela") {
    for (nr in seq_along(filesDirsTable[, 1])) {
      processTableData(filesDirsTable, nr)
    }
  } else {
    calculateDMU(filesDirsTable, fractalDivisionMethod, userSelectedVectorSize)
  }

  if (is.null(choosenVariables)) {
    if (inputType == "tabela" || is.element("NULL", timeSeries)) {
    } else {
    }
  } else {
    if (is.element("NULL", timeSeries)) {
      if (is.element("FALSE", vetor) && !isTRUE(condTRUE)) {
        stop("O limite máximo de colunas desta tábela é o número de colunas da tábela adicionada, caso queira mais colunas, tera que adicionar DMU por DMU")
      }
    }
  }
  return(TRUE)
}

processDEA <- function(input, output) {
  showTab(inputId = "tabs", target = "DEA Table")
  showTab(inputId = "tabs", target = "GRAPHIC DEA")
  combinedTableDea <- data.frame()

  for (windowIndex in 1:length(timeSeriesResultTable)) {
    resultTimeWindow <- timeSeriesResultTable[[windowIndex]]
    resultTimeWindowDataFrame <- data.frame(matrix(unlist(resultTimeWindow), nrow = length(resultTimeWindow), byrow = TRUE))
    colnames(resultTimeWindowDataFrame) <- tableColumnNames

    if (input$typDMU != "tabela" && !is.null(input$variable)) {
      FiltereSshowDataFrame <- resultTimeWindowDataFrame[, c("DMU", input$variable)]
      resultTimeWindowDataFrame <- FiltereSshowDataFrame
    }
    dataMatrix <- as.matrix(sapply(resultTimeWindowDataFrame[, 2:ncol(resultTimeWindowDataFrame)], as.numeric))

    rownames(dataMatrix) <- resultTimeWindowDataFrame[, "DMU"] # drop the no data rows in dataMatrix
    delete.na <- function(DF, n = 0) {
      DF[rowSums(is.na(DF)) <= n, ]
    }

    data_dea <- delete.na(dataMatrix) # creates the data_dea table
    defaultInput <- "FractalDim"

    if (is.null(input$idInputs)) {
      if (length(input$variable) < 1) {
        inputs <- data_dea[, "FractalDim"] # select only input variables values
      } else if ("FractalDim" %in% input$variable) {
        inputs <- data_dea[, "FractalDim"] # select only input variables values
      } else {
        inputs <- data_dea[, input$variable[1]]
        defaultInput <- input$variable[1]
      }
    } else {
      inputs <- data_dea[, input$idInputs] # select only input variables values
    }

    if (is.null(input$idOutputs)) {
      if (colnames(data_dea)[1] != defaultInput) {
        col_idx <- grep(defaultInput, names(data_dea))
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
      colnames(tableDea) <- c("DMU", sprintf("Efficiency Ranking Index timeseries %s", windowIndex))
    } else {
      if (input$ori == "out") {
        tableDea[seq_len(nrow(tableDea)), 2] <- 1 / tableDea[seq_len(nrow(tableDea)), 2]
      }
      colnames(tableDea) <- c("DMU", sprintf("Efficiency Index timeseries %s", windowIndex))
    }

    rownames(tableDea) <- seq_len(nrow(tableDea))
    tableDea <- arrange(tableDea, tableDea[, 1])

    if (nrow(combinedTableDea) < 1) {
      combinedTableDea <- tableDea
    } else {
      combinedTableDea <- cbind(combinedTableDea, tableDea[[2]])
    }
  }

  output$OutDEA <- renderDT({
    datatable(
      combinedTableDea,
      options = list(pageLength = 10, autoWidth = TRUE, ordering = TRUE)
    )
  })
}

server <- function(input, output, session) {
  hide("oculDEA")
  hide("oculButton")
  hideTab(inputId = "tabs", target = "DMU Analysis")
  hideTab(inputId = "tabs", target = "DEA Table")
  hideTab(inputId = "tabs", target = "GRAPHIC DEA")
  updateSliderInput(session, "windowIndex", value = 1)
  updateSliderInput(session, "windowSize", value = 1)
  disable("windowSize")
  disable("windowIndex")
  disable("tbl")
  output$tbl <- renderDT(data.frame(matrix(ncol = 0, nrow = 0)))

  # selecionar dataset
  shinyDirChoose(input, "folder", roots = c(wd = datasetFolder), filetypes = c("", "txt", "xlsx", "csv", ".tsv"))

  filesDirsTable <- reactiveVal(data.frame(datapath = character(), name = character(), stringsAsFactors = FALSE))
  oldFileIndex <- reactiveVal(0)

  observeEvent(input$windowIndex, {
    req(input$windowIndex)
    arquivos_df <- filesDirsTable()
    createShowTable(input$windowIndex, input$variable, input$typDMU, output, session)
  })

  session$onSessionEnded(function() {
    fooTable <<- data.frame(matrix(ncol = 7, nrow = 0))
    colnames(fooTable) <<- tableColumnNames
    fooTableDEA <<- data.frame()
    fooDash <<- data.frame()
    timeSeries <<- list()
    hideTab(inputId = "tabs", target = "DMU Analysis")
    click("idAtualizar")
  })

  # Carrega a página de ajuda
  fullContent <- reactive({
    fileContent <- readLines("./ajuda.md")
    markdown::markdownToHTML(text = paste(fileContent, collapse = "\n"), fragment.only = TRUE)
  })

  output$filteredMarkdown <- renderUI({
    contentText <- fullContent()
    if (input$search == "") {
      HTML(markdownToHTML(text = contentText, fragment.only = TRUE))
    } else {
      lines <- readLines("./ajuda.md")
      normalizedLines <- stri_trans_general(tolower(lines), "Latin-ASCII")
      normalizedSearch <- stri_trans_general(tolower(stri_trim(input$search)), "Latin-ASCII")

      filteredLines <- c()
      inRelevantSection <- FALSE
      currentSectionLevel <- 0

      for (i in seq_along(normalizedLines)) {
        line <- lines[i]
        normalizedLine <- normalizedLines[i]

        # Identifica o nível da seção com base nos cabeçalhos (#, ##, ###)
        if (grepl("^#\\s", normalizedLine)) {
          sectionLevel <- 1
        } else if (grepl("^##\\s", normalizedLine)) {
          sectionLevel <- 2
        } else if (grepl("^###\\s", normalizedLine)) {
          sectionLevel <- 3
        } else {
          sectionLevel <- 0
        }
        lineContainsSearch <- grepl(normalizedSearch, normalizedLine)

        if (sectionLevel > 0 && sectionLevel <= currentSectionLevel) {
          inRelevantSection <- FALSE
        }
        if (lineContainsSearch) {
          inRelevantSection <- TRUE
        }
        if (inRelevantSection) {
          filteredLines <- c(filteredLines, line)
        }
        if (sectionLevel > 0) {
          currentSectionLevel <- sectionLevel
        }
      }
      if (length(filteredLines) > 0) {
        filteredText <- paste(filteredLines, collapse = "\n")
        filteredHTML <- markdownToHTML(text = filteredText, fragment.only = TRUE)
        HTML(filteredHTML)
      } else {
        HTML("Nenhum resultado encontrado.")
      }
    }
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
    preprocessData(arquivos_df, input$typDMU)
    updateSliderInput(session, "windowSize", value = 1, max = maxAmountOfWindows)
    enable("windowSize")
  })

  # loads the file when the user selects it
  observeEvent(input$idArquivo, {
    req(input$idArquivo)

    arquivo_caminho <- input$idArquivo$datapath
    arquivo_nome <- tools::file_path_sans_ext(basename(input$idArquivo$name))
    arquivos_df <- data.frame(datapath = arquivo_caminho, name = arquivo_nome, stringsAsFactors = FALSE)
    filesDirsTable(arquivos_df)
    preprocessData(arquivos_df, input$typDMU)
    updateSliderInput(session, "windowSize", value = 1, max = maxAmountOfWindows)
    enable("windowSize")
  })

  observeEvent(input$idBotao, {
    hide("oculDEA")
    hide("oculButton")
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
            checkStatus <- checkAndProcessData(filesDirsTable, input$typDMU, input$variable, input$sep, input$windowSize)
            if (!checkStatus) {
              return()
            }
            updateSliderInput(session, "windowIndex", max = maxAmountOfIndexes)
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

      updateSliderInput(session, "windowIndex", value = 1)
      createShowTable(1, input$variable, input$typDMU, output, session)
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
    if (nrow(showDataFrame) < 2) {
      if (nrow(showDataFrame) < 1) hide("oculButton")
      hide("oculDEA")
      hideTab(inputId = "tabs", target = "DEA Table")
      hideTab(inputId = "tabs", target = "GRAPHIC DEA")
    }
    createShowTable(input$windowIndex, input$variable, input$typDMU, output, session)
  })

  observeEvent(input$idDeleteRows,
    {
      if (!is.null(isolate(input$tbl_rows_selected))) {
        row <- isolate(input$tbl_rows_selected)
        timeSeriresIndex <- input$windowIndex
        timeSeriesResultTable[[timeSeriresIndex]] <<- timeSeriesResultTable[[timeSeriresIndex]][-row]

        timeSeries[[timeSeriresIndex]] <<- timeSeries[[timeSeriresIndex]][-row]

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
    },
    ignoreInit = TRUE
  )

  observeEvent(input$tbl_cell_edit, {
    row <- input$tbl_cell_edit$row
    clmn <- input$tbl_cell_edit$col
    timeSeriresIndex <- input$windowIndex
    timeSeriesResultTable[[timeSeriresIndex]][[row]][[clmn]] <<- input$tbl_cell_edit$value
    click("idAtualizar")
  })

  observeEvent(input$tbl_rows_selected, {
    editTable(input, output)
  })

  observeEvent(input$idDEA, {
    processDEA(input, output)
  })
}

shinyApp(ui, server)

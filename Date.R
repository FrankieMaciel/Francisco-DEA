library(shiny);
library(pracma);
library(fractaldim);
library(openxlsx);
library(shinyjs);
library(Benchmarking);
library(shinydashboard);
library(shinyalert);
library(DT);
library(TSA);
library(longmemo);
library(ptsuite);
library(plyr)

#library(TSstudio);

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



ui <- navbarPage(title = "FRANCISCO", id = "tabs",
  tabPanel("Inicio", 
  useShinyjs(),  # Set up shinyjs

  sidebarLayout(
    sidebarPanel(
      fileInput('idArquivo', 'Selecione o seu arquivo', multiple = TRUE, accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','.txt', '.xlsx', '.csv','.tsv')),
      tags$hr(),
      selectInput("typDMU", "Tipo de Arquivo:",
                  c("DMU do APACHE BENCH" = "apache", "DMU do IPERF" = "iperf", "DMU Númerica" = "numerica", "Tabela" = "tabela"), selected = "apache"),
      selectInput("variable", "Variaveis:",
                  c("FractalDim" = "FractalDim",
                    "TCP_AVG" = "TCP_AVG",
                    "Hurst" = "Hurst",
                    "Var" = "Var",
                    "Whittle's Estimator" = "Whittle's Estimator",
                    "Alfa Tail Shape Parameter" = "Alfa Tail Shape Parameter"),   multiple = TRUE),

      selectInput("sep", "Metódos de Dimensão Fractal:",
                  c("Madogram" = "madogram", "Variogram" = "variogram", "Rodogram" = "rodogram",
                    "Variation" = "variation", "Incr1" = "incr1", "Boxcount" = "boxcount",
                    "Hallwood" = "hallwood", "Periodogram" = "periodogram", "Wavelet" = "wavelet",
                    "DctII" = "dctII", "Genton" = "genton")),
      actionButton("idBotao","Gerar Tabela"),
      div(id = "oculDEA",
          selectInput('idInputs','Selecione Inputs',choices=NULL, selected=NULL, multiple = TRUE),
          selectInput('idOutputs','Selecione Outputs',choices=NULL, selected=NULL, multiple = TRUE),
          
          selectInput("mod", "Modelo:",
                  c("CCR" = "CRS", "BCC" = "VRS", "SCCR" = "SCCR", "SBM" = "ADD" ), selected = "CRS"),
      selectInput("ori", "Orientacoes:",
                  c("IN" = "in", "OUT" = "out"),  selected = "in"),
      actionButton("idDEA","Analyse DEA"),
      )
      
      
       , width = 3),
    
    mainPanel(
      DTOutput("tbl"),
      
      div(id = "oculButton",
          actionButton("idDeleteRows"," Delete Linha"),
          downloadButton("downloadTable", "Baixar"),
      ),
      shinyjs::hidden(
        div(id = "conteudoOpcional",
        actionButton("idAtualizar","Atualizar"),
        )
      ),


      
      )
    )
  ),

        
  tabPanel("DMU Analysis", 
           
           dashboardPage(
             dashboardHeader(disable = TRUE),
             dashboardSidebar(disable = TRUE),
             
             dashboardBody(
               tags$head(tags$style("section.content { overflow-y: hidden; }")),
               tags$h1("DMU Analysis"),
               
               shinyjs::hidden(
                 div(id = "semSerieTemporal",
                     tags$h1("Sem serie Temporal"),
                     
                 )
                 ),
               shinyjs::hidden(
                 div(id = "SerieTemporal",
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
  tabPanel("DEA Table",
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
    tabPanel("GRAPHIC DEA",
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
colnames(fooTable) <- c("DMU", "FractalDim", "TCP_AVG", "Hurst", "Var", "Whittle's Estimator", 
                   "Alfa Tail Shape Parameter")
timeSeries <<- list()

global <<- reactiveValues(response = FALSE)
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)
server <- function(input, output, session) {
  hide("oculDEA")
  hide("oculButton")
  hideTab(inputId = "tabs", target = "DMU Analysis")
  hideTab(inputId = "tabs", target = "DEA Table")
  hideTab(inputId = "tabs", target = "GRAPHIC DEA")
  


  
  observeEvent(input$idBotao, {

if(is.null(input$idArquivo)){
  print("Sem arquivo")
  
}else{

    
    if(nrow(fooTable) >= 1){
     # print("1")
      shinyalert(title = "Deseja criar uma nova Tabela?",
                 text = "Caso não, aumentaremos o numero de linhas da tabela atual",
                 type = "warning",
                 closeOnClickOutside = TRUE,
                 showCancelButton = TRUE,
                 cancelButtonText = 'No',
                 showConfirmButton = TRUE,
                 confirmButtonText = 'Yes',
                 confirmButtonCol = "darkred",
                 timer = 15000, # 15 seconds
                 callbackR = function(x) { 
                   global$response <- x
                   shinyalert(title = "Salvou", 
                              #text = "Restart the application",
                              type = "success")
                 }
      )
    }

      
  observe({
      req(input$shinyalert)
    
      if (!global$response == "FALSE" && input$shinyalert) {
        print("Aqui")
        
        if(input$typDMU=="tabela"){
          nLinha <- 0
          for(nr in 1:length(input$idArquivo[, 1])){
            arquivo <- NULL
            tryCatch(
              withCallingHandlers(
                arquivo <- read.xlsx(input$idArquivo[[nr, 'datapath']],  startRow = 1),
                message = function(m) {
                  print("message")
                  
                  print(w$message, type = "message")
                },
                warning = function(w) {
                  print("waring")
                  
                  print(w$message, type = "warning")
                }
              ),
              error = function(e){ 
                arquivo <- NULL
                print(e$message, type = "error")
                
              }
            )
            print("table")
            print(arquivo)
            if(is.null(arquivo)){
              print("erro")
            }else{
            nLinha <- nLinha + nrow(arquivo)
            print("Total Linha")
            
            print(nLinha)
            }
          }
            
          print("tabela new table")
          
          print("Tamanho da tabela")
          print(nrow(fooTable))

          numeHouse <- nrow(fooTable) - nLinha + 1
          print("linha que vão ficar")
          
          print(numeHouse)
          

        }else{
          numeHouse <- nrow(fooTable) - length(input$idArquivo[, 1]) + 1
          print(numeHouse)
          
        }
        

        print("erro aqui?")
        #fooTableDMU <<- fooTableDEA[numeHouse:nrow(fooTable),]
        
        fooTableDEA <<- fooTableDEA[numeHouse:nrow(fooTableDEA),]
        fooTable <<- fooTable[numeHouse:nrow(fooTable),]
        fooDash <<- fooDash[numeHouse:nrow(fooDash),]
        
        
        if(nrow(fooTableDEA) == 1){
          hide("oculDEA")
          hideTab(inputId = "tabs", target = "DEA Table")
          hideTab(inputId = "tabs", target = "GRAPHIC DEA")
        }
        rownames(fooTableDEA) <<- seq_len(nrow(fooTableDEA))
        rownames(fooTable) <<- seq_len(nrow(fooTable))
        rownames(fooDash) <<- seq_len(nrow(fooDash))
        if(!input$typDMU=="tabela"){
        if(is.null(input$variable)){
          fooTable <<- fooDash[,c("DMU", "FractalDim", "TCP_AVG", "Hurst", "Var", "Whittle's Estimator", 
                                  "Alfa Tail Shape Parameter")]
          fooTableDEA <<- fooTable
            
        }else{
          fooTable <<- fooDash[, c("DMU", input$variable)]
          fooTableDEA <<- fooTable
          
        }
        }  
        
        
        print(fooTableDEA)
        print(fooTable)
        print("fooDash depois de pedir para excluir")
        print(fooDash)
        print(fooTableDEA)
        
        timeSeries <<- timeSeries[numeHouse:length(timeSeries)]

        #rownames(fooTableDMU) <- seq_len(nrow(fooTableDMU))
        #fooTable <<- fooTable[-(1:nrow(fooTable)),]
        #fooTableDEA <<- fooTableDEA[-(1:nrow(fooTableDEA)),]
        
        #fooTable <<- rbind(fooTable, fooTableDMU)
        #fooTableDEA <<- rbind(fooTableDEA, fooTableDMU)
        
        #timeSeries <<- timeSeries[-(1:length(timeSeries))]
        #timeSeries[length(timeSeries) + 1] <<- list(timeSeriesDMU[[1]])
        hideTab(inputId = "tabs", target = "DMU Analysis")
        
        #timeSeries[1:length(timeSeries)] <- NULL
        #timeSeries <<- timeSeriesDMU[[1]]
        #timeSeries <<- c(timeSeries, timeSeriesDMU[[1]])
        
        click("idAtualizar")
        
        # Reset value
        global$response <- "FALSE"
      } # End of confirmation button if
    
  }) 
  
    

   
    for(nr in 1:length(input$idArquivo[, 1])){
      if(input$typDMU=="tabela"){
        arquivo <- NULL
        tryCatch(
          withCallingHandlers(
            arquivo <- read.xlsx(input$idArquivo[[nr, 'datapath']],  startRow = 1),
            message = function(m) {
              print("message")
              
              print(w$message, type = "message")
            },
            warning = function(w) {
              print("waring")
              
              print(w$message, type = "warning")
            }
          ),
          error = function(e){ 
            arquivo <- NULL
            print(e$message, type = "error")
            
          }
        )
        print("table")
        print(arquivo)
        if(is.null(arquivo)){
          print("erro")
        }else{
        #print(nrow(arquivo))
        #print(colnames(arquivo))
        #print(names(arquivo))
        #print(input$variable)
        
        #print(arquivo[,c("DMU", input$variable)])
        
        #name <- arquivo[,"DMU"]
        #print(arquivo[,2])
        
        #dim <- as.numeric(arquivo[,2])
        
        #dim <- format(round(dim, 3), nsmall = 3)
        
        #media <- as.numeric(arquivo[,3])
        #media <- format(round(media, 3), nsmall = 3)
        #print(media)
        
        #print(typeof(media))
        
        #hurst <- as.numeric(arquivo[,4])
        #hurst <- format(round(hurst, 3), nsmall = 3)
        
        #varianca <- as.numeric(arquivo[,5])
        #varianca <- format(round(varianca, 3), nsmall = 3)
        
        if(nrow(fooTable)>0){
          #list_1 <- list(DMU=name, FractalDim=dim, TCP_AVG =media, Hurst=hurst, Var=varianca)
          #fooTable2 <- as.data.frame(list_1)
          #print(isTRUE(all.equal(fooTable,arquivo)))
          
          #print(colnames(fooTable) ==  colnames(arquivo))
          #if(isTRUE(colnames(fooTable) == colnames(arquivo))){
           print(" Erro aqui, se o usário for add depois de uma nova de uma dmu")
            
           print(arquivo)
           print(fooTable)
          print(fooDash)

          fooTable <<- rbind(fooTable[,colnames(arquivo)], arquivo)
          print("Dobrando?")
          
          print(fooTable)
          #mutate_if(is.numeric, round)
          fooTable[,2:ncol(fooTable)] <<- sapply(fooTable[2:ncol(fooTable)],as.numeric)
          
          print(fooTable)
          fooTable[,2:ncol(fooTable)] <<- format(round(fooTable[,2:ncol(fooTable)], 3), nsmall = 3)
          
          #fooTable <- apply(fooTable, 2, round, 3)
          options(scipen = 999)
          print(fooTable)
          
          str(fooTable)
          
          fooDash <<- fooTable
          
          #}
          #else{
            #print("Erro aqui")
            
          #}
          
          #fooTable$DMU <<- as.character(fooTable$DMU)
          
        }else{
          #list_1 <- list(DMU=name, FractalDim=dim, TCP_AVG =media, Hurst=hurst, Var=varianca)
          #fooTable <<- as.data.frame(arquivo[,c("DMU", input$variable)])

          fooTable <<- arquivo
          
          #fooTable <<- rbind(fooTable, arquivo[,c("DMU", input$variable)])


          #mutate_if(is.numeric, round)
          fooTable[,2:ncol(fooTable)] <<- sapply(fooTable[2:ncol(fooTable)],as.numeric)


          fooTable[,2:ncol(fooTable)] <<- format(round(fooTable[,2:ncol(fooTable)], 3), nsmall = 3)

          #fooTable <- apply(fooTable, 2, round, 3)
          
          
          print(fooTable)
          
          fooDash <<- fooTable
          
                    #print(typeof(fooTable$Hurst))
          
          #fooTable$DMU <<- as.character(fooTable$DMU)
        }
        listTable <- vector(mode = "list", length = nrow(arquivo))
        timeSeries <<- c(timeSeries, listTable)
        print(timeSeries)
        }
      }else{
      if(input$typDMU=="iperf"){
        name <- tools::file_path_sans_ext(input$idArquivo[[nr, 'name']])

        arquivo <- read.csv(input$idArquivo[[nr, 'datapath']], header = F, sep ="", skip = 6)
        df <- NULL
        tryCatch(
          withCallingHandlers(
            df<-data.frame(arquivo[,8],arquivo[,7]),
            message = function(m) {
              print("message")
              
              print(w$message, type = "message")
            },
            warning = function(w) {
              print("waring")
              
              print(w$message, type = "warning")
            }
          ),
          error = function(e){ 
            df <- NULL
            print(e$message, type = "error")
            
          }
        )
        
        if(is.null(df)){
          vetor <- df
        }else{
          
        print(df)
        
        colnames(df) <- c("Um", "Dois")
        new_df=df[!grepl("K",df$Um),]
        new_df2=df[!grepl("K",df$Dois),]
        colnames(new_df) <- c("DMU", "EX")
        colnames(new_df2) <- c("EX", "DMU")
        df_vetor <- rbind(new_df, new_df2)
        vetor <- as.numeric(as.character(df_vetor$DMU))
        }
        
      } else if(input$typDMU=="apache"){
        
        name <- tools::file_path_sans_ext(input$idArquivo[[nr, 'name']])
        print("Apache")
        arquivo <- read.csv(input$idArquivo[[nr, 'datapath']], header = F, sep =",", skip = 1)
        print("Apache")
        #shinyCatch(stop("error with blocking"), blocking_level = "error")
        vetor <- NULL
        tryCatch(
          withCallingHandlers(
            vetor <- c(as.numeric(unlist(arquivo[2]))),
            message = function(m) {
              print("message")
              
              print(w$message, type = "message")
            },
            warning = function(w) {
              print("waring")
              
              print(w$message, type = "warning")
              }
          ),
          error = function(e){ 
            vetor <- NULL
            print(e$message, type = "error")
            
          }
        )
        

      


      } else{

        name <- tools::file_path_sans_ext(input$idArquivo[[nr, 'name']])
        
        arquivo <- NULL
        tryCatch(
          withCallingHandlers(
            arquivo <- read.table(input$idArquivo[[nr, 'datapath']]),
            message = function(m) {
              print("message")
              
              print(w$message, type = "message")
            },
            warning = function(w) {
              print("waring")
              
              print(w$message, type = "warning")
            }
          ),
          error = function(e){ 
            arquivo <- NULL
            print(e$message, type = "error")
            
          }
        )
        
        print(arquivo)
        if(is.null(arquivo)){
          vetor <- NULL
          }else{
            vetor <- c(as.numeric(unlist(arquivo)))
          }
        
      }
        if(is.null(vetor)){
          print("error")
        }else{
        

        dim <- as.numeric(unlist(fd.estimate(vetor, method=input$sep)[2]))
        dim <- format(round(dim, 3), nsmall = 3) 

        media = mean(vetor, na.rm=FALSE)
        media <- format(round(media, 3), nsmall = 3) 

        hurst <- as.numeric(unlist(hurstexp(vetor)[1]))
        hurst <- format(round(hurst, 3), nsmall = 3) 

        varianca <- as.numeric(unlist(var(vetor, na.rm=TRUE)[1]))
        varianca <- format(round(varianca, 3), nsmall = 3) 
        
        whittleEstimator <-  WhittleEst(vetor)
        #print(whittleEstimator)
        whittleEstimator <-whittleEstimator$coefficients[[1,1]]
        #print(whittleEstimator)
        whittleEstimator <- format(round(whittleEstimator, 3), nsmall = 3) 
        #print(whittleEstimator)
        # o vetor não pode ter zeros
        d <- vetor
        d <- d[ d != 0]
        tailParameter <- alpha_mle(d) 
        tailParameter <- format(round(tailParameter$shape, 3), nsmall = 3) 
        #print(tailParameter)
        
        #Warning in alpha_hills(d, length(d)) :
        #Setting k as the number of observations makes it equivalent to the MLE (alpha_mle function).
        #hillsEstimator <- alpha_hills(d, length(d))
        #hillsEstimator <- format(round(hillsEstimator$shape, 3), nsmall = 3)

        if(is.element("NULL", timeSeries)){
          print(" Erro aqui, se a coluna for menor, se o usuário for add, em cima de xsl")
          
          print(fooTable)
          
          fooTableDMU <- data.frame(matrix(ncol = 7, nrow = 0))
          fooDashDMU <- data.frame(matrix(ncol = 7, nrow = 0))
          
          colnames(fooTableDMU) <- c("DMU", "FractalDim", "TCP_AVG", "Hurst", "Var", "Whittle's Estimator", 
                             "Alfa Tail Shape Parameter")
          colnames(fooDashDMU) <- c("DMU", "FractalDim", "TCP_AVG", "Hurst", "Var", "Whittle's Estimator", 
                                     "Alfa Tail Shape Parameter")
          fooTableDMU[nrow(fooTableDMU) + 1,] <- c(name, dim, media, hurst, varianca, whittleEstimator, tailParameter)

          fooTable <<- rbind(fooTable, fooTableDMU[,colnames(fooTable)])
          
          fooDashDMU[nrow(fooTable),] <- c(name, dim, media, hurst, varianca, whittleEstimator, tailParameter)
          fooDash <<- fooDashDMU
           print("fooDash")
          
          print(fooDash)

          print(fooTable)
        }else{
          print(fooDash)
          print("AQUi new erro")
          if(ncol(fooTable) < 7){
            
            fooTable <<- data.frame(matrix(ncol = 7, nrow = 0))
            
            colnames(fooTable) <<- c("DMU", "FractalDim", "TCP_AVG", "Hurst", "Var", "Whittle's Estimator", 
                                     "Alfa Tail Shape Parameter")
          }


          
          fooTable[nrow(fooTable) + 1,] <<- c(name, dim, media, hurst, varianca, whittleEstimator, tailParameter)
          fooDash <<- fooTable
          print(fooTable)
          }
        timeSeries[length(timeSeries) + 1] <<- list(vetor)
        
      }
    }

      
      

    }
  
  if(is.null(input$variable)){
    if(input$typDMU=="tabela" || is.element("NULL", timeSeries)){
      fooTableDEA <<- fooTable      
    }else{
      fooTableDEA <<- fooTable[,c("DMU", "FractalDim", "TCP_AVG", "Hurst", "Var", "Whittle's Estimator", 
                        "Alfa Tail Shape Parameter")]
    }
    

  }else{
    if(is.element("NULL", timeSeries)){
      
      print("erro AQUi")
      print(input$variable)    
      print(colnames(fooTable[,2:ncol(fooTable)]))    
      vetor <- c(colnames(fooTable[,2:ncol(fooTable)]) == input$variable)
      print(vetor)
      numeroTRUE <- sum(vetor, na.rm = TRUE)
      print(numeroTRUE)
      numeroVar <- length(input$variable)
      print(numeroVar)
      condTRUE <- numeroTRUE == numeroVar
      print(condTRUE)
      print(is.element("FALSE", vetor))
      
      print(!is.element("FALSE", vetor))
      

      #print(ncol(fooTable[,2:ncol(fooTable)]) > length(input$variable))
      #print(is.element(input$variable, fooTable[,2:ncol(fooTable)]))
      if(!is.element("FALSE", vetor) || isTRUE(condTRUE)){
        print("Aqui entra só XSL")
        fooTableDEA <<- fooTable[,c("DMU", input$variable)]
        print(fooTableDEA)
        
      }else{
        print("oi")
        shinyalert(title = "O limite maximo de colunas desta tabela é o numero de colunas da tabela adicionada ",
                   text = "Caso queira mais colunas, tera que add DMU por DMU",
                   type = "error",
                   closeOnClickOutside = TRUE,
                   showCancelButton = FALSE,
                   cancelButtonText = 'No',
                   showConfirmButton = TRUE,
                   confirmButtonText = 'OK',
                   confirmButtonCol = "darkred",
                   timer = 15000, # 15 seconds
        ) 
        fooTableDEA <<- fooTable      
        
        
        #timeSeries <<- timeSeries[1:length(timeSeries)-1]
        #fooTableDEA <<- fooTable[1:nrow(fooTable)-1,]
        
        print(fooTable)
        
        print("eliminar o último fooTableDEA")
      }
      
    }else{
      print("aqui nesse else?")
      fooTableDEA <<- fooTable[,c("DMU", input$variable)]
      print(fooTableDEA)
      
    }

  }
  
    
    #tableDea <- read.xlsx()
    if(nrow(fooTableDEA) > 1){

      show("oculDEA")
      
      #Names=as.character(fooTable[,1]) # COL names
      #FractalDim=as.numeric(unlist(fooTable[,2])) #Fractal Dimension 

      #TCP_AVG=as.numeric(unlist(fooTable[,3])) # Mean of TCP bandwidth between VMs on appraisal
      #Hurst=as.numeric(unlist(fooTable[,4])) # Fractal Memory per timeseries
      #Varianca=as.numeric(unlist(fooTable[,5])) # Variança

      #dataMatrix=cbind(FractalDim,TCP_AVG,Hurst) # creates the data matrix
      #dataMatrix <- fooTable[,2:ncol(fooTable)]
      #fooTable2 <<- rbind(fooTable2, fooTable)
      #NewDataframe <- merge.data.frame(fooTable2, fooTable, all.x=TRUE)

      updateSelectInput(session, "idInputs",
                        choices = colnames(fooTableDEA[,2:ncol(fooTableDEA)]), 
      )
      
      updateSelectInput(session, "idOutputs",
                        choices = colnames(fooTableDEA[,2:ncol(fooTableDEA)]), 
      )
      
      
# 
      observeEvent(input$idDEA, {
        print("DEA")
        
        print(fooTable)
        
        showTab(inputId = "tabs", target = "DEA Table")
        showTab(inputId = "tabs", target = "GRAPHIC DEA")
        
        
        dataMatrix <- as.matrix(sapply(fooTableDEA[,2:ncol(fooTableDEA)], as.numeric))
        
        rownames(dataMatrix)=fooTableDEA[,"DMU"] # drop the no data rows in dataMatrix
        delete.na <- function(DF, n=0) {
          DF[rowSums(is.na(DF)) <= n,]
        }

        
        
        
        data_dea = delete.na(dataMatrix) #creates the data_dea table
        
        if(is.null(input$idInputs)){
          inputs = data_dea[,"FractalDim"] # select only input variables values
        }
        else{
          inputs = data_dea[,input$idInputs] # select only input variables values
        }
        
        print("inputs")
        print(inputs)
        
        if(is.null(input$idOutputs)){
          print(colnames(data_dea)[1])
          
          print(colnames(data_dea)[1] != "FractalDim")
          if(colnames(data_dea)[1] != "FractalDim"){
            col_idx <- grep("FractalDim", names(data_dea))
            data_dea <- data_dea[, c(col_idx, (1:ncol(data_dea))[-col_idx])]
            print(data_dea)
            outputs = data_dea[,c(2:ncol(data_dea))] # select only output variables values, SLACK=TRUE
          }else{
            outputs = data_dea[,c(2:ncol(data_dea))] # select only output variables values, SLACK=TRUE
          print("is null, FractalDIM")
            }
          
        }
        else{
          print("sem null, FractalDIM")
          
          outputs = data_dea[,input$idOutputs] # select only input variables values
          
        }
        
        print("outputs")
        print(outputs)
        
        if(input$mod == "SCCR"){
          dea <- sdea(inputs,outputs,RTS="CRS",ORIENTATION=input$ori) # runs super-efficiency input-oriented CCR DEA model
          
          output$graficoDea <- renderPlot(dea.plot(inputs,outputs,RTS="CRS",ORIENTATION=input$ori))
          output$graficoDeafrontier <- renderPlot(dea.plot.frontier(inputs,outputs,RTS="CRS"))

        }else if(input$mod == "ADD"){
          dea <-sdea(inputs, outputs,RTS=input$mod,ORIENTATION=input$ori)
          
          output$graficoDea <- renderPlot(dea.plot(inputs,outputs,RTS=input$mod,ORIENTATION="in-out"))
          output$graficoDeafrontier <- renderPlot(dea.plot.frontier(inputs,outputs,RTS=input$mod))
          
        }else{
          dea <-dea(inputs, outputs,RTS=input$mod,ORIENTATION=input$ori, SLACK=TRUE)
          
          output$graficoDea <- renderPlot(dea.plot(inputs,outputs,RTS=input$mod,ORIENTATION=input$ori))
          output$graficoDeafrontier <- renderPlot(dea.plot.frontier(inputs,outputs,RTS=input$mod))
        }
        print(dea$eff)
       

        
        #print(typeof(dea$eff))
        listDEA <- data.matrix(dea$eff)
        print(typeof(listDEA))
        print(listDEA)
        print(dea$eff)
        tableDea <- data.frame(listDEA)
        print(tableDea)
        tableDea <- cbind(DMU = rownames(tableDea), tableDea)
        print(tableDea)
        
        #rownames(tableDea) <- 1:nrow(tableDea)
        #print(tableDea)
        if(input$mod == "SCCR" || input$mod == "ADD"){
          colnames(tableDea) <- c("DMU","Efficiency Ranking Index")
        }else{
          print(tableDea)
          
          if(input$ori == "out"){
            tableDea[1:nrow(tableDea),2] = 1/tableDea[1:nrow(tableDea),2]
            print("tableDea")
            
            print(tableDea)
          }
          colnames(tableDea) <- c("DMU","Efficiency Index")
        }
        print(tableDea)
        
        rownames(tableDea) <- 1:nrow(tableDea)
        print(tableDea)
        tableDea <- arrange(tableDea,desc(tableDea[,2]))
        
        #tableDea[,2] <- tableDea[,2]
        #print(tableDea)
        
        #tableSDEA <- data.frame(sort(SCCR_I$eff, decreasing=TRUE))
        #tableSDEA <- cbind(DMU = rownames(tableSDEA), tableSDEA)
        #rownames(tableSDEA) <- 1:nrow(tableSDEA)
        #colnames(tableSDEA) <- c("DMU", "Efficiency Ranking Index")
        
        output$OutDEA <<- renderTable(tableDea, rownames=TRUE)
        #output$OutSupDEA <- renderTable({tableSDEA})
        
        
        
      })
        
        
      #CCR_I=dea(inputs, outputs,RTS="CRS",ORIENTATION="IN", SLACK=TRUE)
      
      #CCR_O=dea(inputs,outputs,RTS="CRS",ORIENTATION="OUT", SLACK=TRUE) # runs output-oriented CCR DEA model
      
# 
      #SCCR_I=sdea(inputs,outputs,RTS="CRS",ORIENTATION="IN") # runs super-efficiency input-oriented CCR DEA model
#     
#     
      #BCC_I=dea(inputs,outputs,RTS="VRS",ORIENTATION="IN", SLACK=TRUE) # runs input-oriente dBCC DEA model
#     
      #BCC_O=dea(inputs,outputs,RTS="VRS",ORIENTATION="OUT", SLACK=TRUE) # runs output-oriented BCC DEA model
#     
     #results
      #data.frame(CCR_I$eff,CCR_I$slack,CCR_I$sx,CCR_I$sy); #exbhits respectively the efficiency score, if there are slacks, input slack, output shortage
      #data.frame(CCR_O$eff,CCR_O$slack,CCR_O$sx,CCR_O$sy); #exbhits respectively the efficiency score, if there are slacks, input slack, output shortage
      #data.frame(BCC_I$eff,BCC_I$slack,BCC_I$sx,BCC_I$sy); #exbhits respectively the efficiency score, if there are slacks, input slack, output shortage
      #data.frame(BCC_O$eff,BCC_O$slack,BCC_O$sx,BCC_O$sy); #exbhits respectively the efficiency  score, if there are slacks, input slack, output shortage
#     #print(data.frame(CCR_I$eff)); #exbhits only the efficiency score because this function does not shows the slacks.
#     #print(data.frame(SCCR_I$eff)) #exbhits only the efficiency score because this function does not shows the slacks.
     #print(data.frame(sort(SCCR_I$eff, decreasing=TRUE)));
     
      
      #output$graficoDea <- renderPlot(dea.plot(inputs,outputs,RTS="vrs",ORIENTATION="out"))
      #output$graficoDeafrontier <- renderPlot(dea.plot.frontier(inputs,outputs,RTS="CRS"))
      
     

 
      #tableDea <- data.frame(sort(CCR_I$eff))
      #tableDea <- cbind(DMU = rownames(tableDea), tableDea)
      #rownames(tableDea) <- 1:nrow(tableDea)
      #colnames(tableDea) <- c("DMU", "Efficiency Index")
      
      #tableSDEA <- data.frame(sort(SCCR_I$eff, decreasing=TRUE))
      #tableSDEA <- cbind(DMU = rownames(tableSDEA), tableSDEA)
      #rownames(tableSDEA) <- 1:nrow(tableSDEA)
      #colnames(tableSDEA) <- c("DMU", "Efficiency Ranking Index")
      
      #output$OutDEA <- renderTable({tableDea})
      #output$OutSupDEA <- renderTable({tableSDEA})
     
    }
    observeEvent(input$tbl_cell_edit, {
      row  <- input$tbl_cell_edit$row
      clmn <- input$tbl_cell_edit$col
      print(fooTableDEA[row, clmn])
      print(input$tbl_cell_edit$value)
      fooTable[row, clmn] <<- input$tbl_cell_edit$value
      fooTableDEA[row, clmn] <<- input$tbl_cell_edit$value
      fooDash[row, clmn] <<- input$tbl_cell_edit$value
        click("idAtualizar")
    })

    
    
  
    observeEvent(input$tbl_rows_selected, {
      print("aqui seleção")
      
      row  <- input$tbl_rows_selected
      print(row)
      print(timeSeries)
      
      print(timeSeries[[row]])
      
      print(length(timeSeries[[row]]))


      if(length(timeSeries[[row]]) > 0) {
        showTab(inputId = "tabs", target = "DMU Analysis")
        hide(id="semSerieTemporal")
        show(id="SerieTemporal")
        show(id="oi")
        output$nameDMU <- renderText(fooDash[row, 1])
        output$valueFractalDim <- renderText(fooDash[row, 2])
        output$valueTCP_AVG <- renderText(fooDash[row, 3])
        output$valueHurst <- renderText(fooDash[row, 4])
        output$valueVar <- renderText(fooDash[row, 5])
        output$valueWhittlesEstimator <- renderText(fooDash[row, 6])
        output$valueTailParameter <- renderText(fooDash[row, 7])
        output$graficoDMUHistograma <- renderPlot(hist(timeSeries[[row]], col="darkblue", border="black"))
        output$graficoDMUPeriodograma <- renderPlot(periodogram(timeSeries[[row]]))
        output$graficoDMUPeriodogramaacf <- renderPlot(acf(timeSeries[[row]]))
        output$graficoDMUts <- renderPlot(spec.pgram(timeSeries[[row]]))
        c <- timeSeries[[row]]
        c <- c[ c != 0]
        

        log_log_timeseries = log(c)
        output$graficoLLCD <- renderPlot(plot(ecdf(log_log_timeseries)))
      } else{
        showTab(inputId = "tabs", target = "DMU Analysis")
        show(id="oi")
        hide(id="SerieTemporal")
        show(id="semSerieTemporal")

      }
    })
    if(nrow(fooTable) > 0){
      show("oculButton")
      }
    
    
  
    output$tbl = renderDT(
      fooTableDEA, editable = list(target = "cell", disable = list(columns =c(0,2:ncol(fooTableDEA)))),
      callback = JS(jsname),
      extensions = "KeyTable",
      options = list(
        keys = TRUE
      ), selection = 'single')
    

    output$downloadTable <- downloadHandler(
       filename = function() {
         "table.xlsx"
       },
       content = function(file) {
         write.xlsx(fooTableDEA, file, rowNames = FALSE)
       }
     )
  }
  })
  
  observeEvent(input$idAtualizar, {
    if(nrow(fooTable) < 1){
      hide("oculButton")
      #print(fooTable)
      #colnames(fooTable) <<- NULL
      #print(fooTable)
      
    }
    if(nrow(fooTable) < 2){
      hide("oculDEA")
      hideTab(inputId = "tabs", target = "DEA Table")
      hideTab(inputId = "tabs", target = "GRAPHIC DEA")
      
    }
    
    output$tbl = renderDT(
      fooTableDEA, editable = list(target = "cell", disable = list(columns =c(0,2:ncol(fooTableDEA)))),
      callback = JS(jsname),
      extensions = "KeyTable",
      options = list(
        keys = TRUE
      ), selection = 'single')
    
  })

    observeEvent(input$idDeleteRows, {



      if(!is.null(isolate(input$tbl_rows_selected))){
        
        row <- isolate(input$tbl_rows_selected)
        print("Excluir entrou excluir")
        fooTable <<- isolate(fooTable[-row,])
        fooTableDEA <<- isolate(fooTableDEA[-row,])
        fooDash<<- isolate(fooDash[-row,])
        
        rownames(fooTableDEA) <<- seq_len(nrow(fooTableDEA))
        rownames(fooTable) <<- seq_len(nrow(fooTable))
        rownames(fooDash) <<- seq_len(nrow(fooDash))
        
        timeSeries <<- timeSeries[-row]
        print(timeSeries)
        if(nrow(fooTable) > 1){
          click("idDEA")
          
        }
        click("idAtualizar")
        
      }else{
        shinyalert(title = "Selecione a linha que deseja Excluir",
                   #text = "Caso queira mais colunas, tera que add DMU por DMU",
                   type = "error",
                   closeOnClickOutside = TRUE,
                   showCancelButton = FALSE,
                   cancelButtonText = 'No',
                   showConfirmButton = TRUE,
                   confirmButtonText = 'OK',
                   confirmButtonCol = "darkred",
                   timer = 15000, # 15 seconds
        ) 
        
        
      }
    }, ignoreInit = TRUE)
    

 
  

}

shinyApp(ui,server)
library(shiny);
library(pracma);
library(fractaldim);
library(xlsx);
library(openxlsx);
library(shinyjs);
library(Benchmarking);
library(shinydashboard);
library(DT);
library(TSA);
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
      fileInput('idArquivo', 'Selecione o seu arquivo', multiple = TRUE, accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','.csv','.tsv')),
      tags$hr(),
      radioButtons('typDMU', 'Tipo de Arquivo:', list("DMU do APACHE BENCH" = "apache", "DMU do IPERF" = "iperf", "DMU Númerica" = "numerica", "Tabela" = "tabela"), selected = "apache" ),
      
      checkboxInput('header', 'Enviar tabela', TRUE),
      selectInput("sep", "Metódos de Dimensão Fractal:",
                  c("Madogram" = "madogram", "Variogram" = "variogram", "Rodogram" = "rodogram",
                    "Variation" = "variation", "Incr1" = "incr1", "Boxcount" = "boxcount",
                    "Hallwood" = "hallwood", "Periodogram" = "periodogram", "Wavelet" = "wavelet",
                    "DctII" = "dctII", "Genton" = "genton")),
      actionButton("idBotao","Ler o arquivo"),
      
       ),
    
    mainPanel(
      
      DTOutput("tbl"),
      downloadButton("downloadTable", "Download"),
      
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
                   collapsible = TRUE, width = 3,
                   textOutput("valueFractalDim")
                 ),
                 box(
                   title = "TCP Average", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE, width = 3,
                   textOutput("valueTCP_AVG")
                 ),

                 box(
                   title = "Hurst Parameter", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE, width = 3,
                   textOutput("valueHurst")
                 ),
                 box(
                   title = "Variance", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE, width = 3,
                   textOutput("valueVar")
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
               tableOutput("OutDEA"),
               tableOutput("OutSupDEA"),
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

foo <<- data.frame(matrix(ncol = 5, nrow = 0))
colnames(foo) <- c("DMU", "FractalDim", "TCP_AVG", "Hurst", "Var")
timeSeries <<- list()

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)
server <- function(input, output) {
  hideTab(inputId = "tabs", target = "DMU Analysis")
  hideTab(inputId = "tabs", target = "DEA Table")
  hideTab(inputId = "tabs", target = "GRAPHIC DEA")
  
  observeEvent(input$idBotao, {
    for(nr in 1:length(input$idArquivo[, 1])){
      if(input$typDMU=="tabela"){
        arquivo <- read.xlsx(input$idArquivo[[nr, 'datapath']],  startRow = 1)
        
        name <- arquivo[,1]

        dim <- as.numeric(arquivo[,2])
        dim <- format(round(dim, 2), nsmall = 2)
        
        media <- as.numeric(arquivo[,3])
        media <- format(round(media, 2), nsmall = 2)
        
        hurst <- as.numeric(arquivo[,4])
        hurst <- format(round(hurst, 2), nsmall = 2)
        
        varianca <- as.numeric(arquivo[,5])
        varianca <- format(round(varianca, 2), nsmall = 2)
        
        if(nrow(foo)>0){
          list_1 <- list(DMU=name, FractalDim=dim, TCP_AVG =media, Hurst=hurst, Var=varianca)
          foo2 <- as.data.frame(list_1)
          foo <<- rbind(foo, foo2)
          foo$DMU <<- as.character(foo$DMU)
          
        }else{
          list_1 <- list(DMU=name, FractalDim=dim, TCP_AVG =media, Hurst=hurst, Var=varianca)
          foo <<- as.data.frame(list_1)
          foo$DMU <<- as.character(foo$DMU)
        }
        listTable <- vector(mode = "list", length = length(name))
        timeSeries <<- c(timeSeries, listTable)
        

      }else{
      if(input$typDMU=="iperf"){
        name <- tools::file_path_sans_ext(input$idArquivo[[nr, 'name']])

        arquivo <- read.csv(input$idArquivo[[nr, 'datapath']], header = F, sep ="", skip = 6)
        df<-data.frame(arquivo[,8],arquivo[,7])
        colnames(df) <- c("Um", "Dois")
        new_df=df[!grepl("K",df$Um),]
        new_df2=df[!grepl("K",df$Dois),]
        colnames(new_df) <- c("DMU", "EX")
        colnames(new_df2) <- c("EX", "DMU")
        df_vetor <- rbind(new_df, new_df2)
        vetor <- as.numeric(as.character(df_vetor$DMU))
        
      } else if(input$typDMU=="apache"){
        
        name <- tools::file_path_sans_ext(input$idArquivo[[nr, 'name']])
        arquivo <- read.csv(input$idArquivo[[nr, 'datapath']], header = F, sep =",", skip = 1)
        vetor <- c(as.numeric(unlist(arquivo[2])))
        
      } else{

        name <- tools::file_path_sans_ext(input$idArquivo[[nr, 'name']])
        arquivo <- read.table(input$idArquivo[[nr, 'datapath']])
        vetor <- c(as.numeric(unlist(arquivo)))
        
      }
        
        dim <- as.numeric(unlist(fd.estimate(vetor, method=input$sep)[2]))
        dim <- format(round(dim, 2), nsmall = 2) 

        media = mean(vetor, na.rm=FALSE)
        media <- format(round(media, 2), nsmall = 2) 
        
        hurst <- as.numeric(unlist(hurstexp(vetor)[1]))
        hurst <- format(round(hurst, 2), nsmall = 2) 
        
        varianca <- as.numeric(unlist(var(vetor, na.rm=TRUE)[1]))
        varianca <- format(round(varianca, 2), nsmall = 2) 
        
        foo[nrow(foo) + 1,] <<- c(name, dim, media, hurst, varianca)
        timeSeries[length(timeSeries) + 1] <<- list(vetor)

      }
      

      
      

    }
    #tableDea <- read.xlsx()
    if(nrow(foo) > 1){

      
      Names=as.character(foo[,1]) # COL names
      FractalDim=as.numeric(unlist(foo[,2])) #Fractal Dimension 

      TCP_AVG=as.numeric(unlist(foo[,3])) # Mean of TCP bandwidth between VMs on appraisal
      Hurst=as.numeric(unlist(foo[,4])) # Fractal Memory per timeseries
      Varianca=as.numeric(unlist(foo[,5])) # Variança

      #dataMatrix=cbind(FractalDim,TCP_AVG,Hurst) # creates the data matrix
      #dataMatrix <- foo[,2:ncol(foo)]
      dataMatrix <- as.matrix(sapply(foo[,2:ncol(foo)], as.numeric))

      rownames(dataMatrix)=Names # drop the no data rows in dataMatrix
      delete.na <- function(DF, n=0) {
         DF[rowSums(is.na(DF)) <= n,]
      }
      

      data_dea = delete.na(dataMatrix) #creates the data_dea table
    
   
      inputs = data_dea[,1] # select only input variables values
      outputs = data_dea[,c(2:ncol(data_dea))] # select only output variables values, SLACK=TRUE

# 
      CCR_I=dea(inputs, outputs,RTS="CRS",ORIENTATION="IN", SLACK=TRUE)
      
      CCR_O=dea(inputs,outputs,RTS="CRS",ORIENTATION="OUT", SLACK=TRUE) # runs output-oriented CCR DEA model
      
# 
      SCCR_I=sdea(inputs,outputs,RTS="CRS",ORIENTATION="IN") # runs super-efficiency input-oriented CCR DEA model
#     
#     
      BCC_I=dea(inputs,outputs,RTS="VRS",ORIENTATION="IN", SLACK=TRUE) # runs input-oriente dBCC DEA model
#     
      BCC_O=dea(inputs,outputs,RTS="VRS",ORIENTATION="OUT", SLACK=TRUE) # runs output-oriented BCC DEA model
#     
     #results
      data.frame(CCR_I$eff,CCR_I$slack,CCR_I$sx,CCR_I$sy); #exbhits respectively the efficiency score, if there are slacks, input slack, output shortage
      data.frame(CCR_O$eff,CCR_O$slack,CCR_O$sx,CCR_O$sy); #exbhits respectively the efficiency score, if there are slacks, input slack, output shortage
      data.frame(BCC_I$eff,BCC_I$slack,BCC_I$sx,BCC_I$sy); #exbhits respectively the efficiency score, if there are slacks, input slack, output shortage
      data.frame(BCC_O$eff,BCC_O$slack,BCC_O$sx,BCC_O$sy); #exbhits respectively the efficiency  score, if there are slacks, input slack, output shortage
#     #print(data.frame(CCR_I$eff)); #exbhits only the efficiency score because this function does not shows the slacks.
#     #print(data.frame(SCCR_I$eff)) #exbhits only the efficiency score because this function does not shows the slacks.
     #print(data.frame(sort(SCCR_I$eff, decreasing=TRUE)));
     
     
      output$graficoDea <- renderPlot(dea.plot(inputs,outputs,RTS="vrs",ORIENTATION="out"))
      output$graficoDeafrontier <- renderPlot(dea.plot.frontier(inputs,outputs,RTS="CRS"))
      
     

 
      tableDea <- data.frame(sort(CCR_I$eff))
      tableDea <- cbind(DMU = rownames(tableDea), tableDea)
      rownames(tableDea) <- 1:nrow(tableDea)
      colnames(tableDea) <- c("DMU", "Efficiency Index")
      
      tableSDEA <- data.frame(sort(SCCR_I$eff, decreasing=TRUE))
      tableSDEA <- cbind(DMU = rownames(tableSDEA), tableSDEA)
      rownames(tableSDEA) <- 1:nrow(tableSDEA)
      colnames(tableSDEA) <- c("DMU", "Efficiency Ranking Index")
      
      output$OutDEA <- renderTable({tableDea})
      output$OutSupDEA <- renderTable({tableSDEA})
     
    }
    

    observeEvent(input$tbl_cell_edit, {
      row  <- input$tbl_cell_edit$row
      clmn <- input$tbl_cell_edit$col
      print(foo[row, clmn])
      print(input$tbl_cell_edit$value)
      print("aqui1")
      foo[row, clmn] <<- input$tbl_cell_edit$value
        click("idAtualizar")
    })

    
    
  
    observeEvent(input$tbl_rows_selected, {
      row  <- input$tbl_rows_selected
      print(row)
      print(length(timeSeries[[row]]))


      if(length(timeSeries[[row]]) > 0) {
        showTab(inputId = "tabs", target = "DMU Analysis")
        hide(id="semSerieTemporal")
        show(id="SerieTemporal")
        show(id="oi")
        output$nameDMU <- renderText(foo[row, 1])
        output$valueFractalDim <- renderText(foo[row, 2])
        output$valueTCP_AVG <- renderText(foo[row, 3])
        output$valueHurst <- renderText(foo[row, 4])
        output$valueVar <- renderText(foo[row, 5])
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
    
  
    output$tbl = renderDT(
      foo, editable = list(target = "cell", disable = list(columns =c(0,2:ncol(foo)))),
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
         write.xlsx(foo, file, rowNames = FALSE)
       }
     )
  })
  
  observeEvent(input$idAtualizar, {
    output$tbl = renderDT(
      foo, editable = list(target = "cell", disable = list(columns =c(0,2:ncol(foo)))),
      callback = JS(jsname),
      extensions = "KeyTable",
      options = list(
        keys = TRUE
      ), selection = 'single')
    

  })
  
  

  

}

shinyApp(ui,server)
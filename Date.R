library(shiny);
library(pracma);
library(fractaldim);
library(xlsx);
library(openxlsx);
library(shinyjs);
library(Benchmarking);
library(DT)


js <- c(
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



ui <- fluidPage(
  useShinyjs(),  # Set up shinyjs
  
  titlePanel("Francisco"),
  sidebarLayout(
    sidebarPanel(
      fileInput('idArquivo', 'Selecione o seu arquivo', multiple = TRUE, accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','.csv','.tsv')),
      tags$hr(),
      radioButtons('typDMU', 'Tipo de Arquivo:', list("DMU do IPERF" = "iperf", "DMU Númerica" = "numerica", "Tabela" = "tabela"), selected = "iperf" ),
      
      checkboxInput('header', 'Enviar tabela', TRUE),
      radioButtons('sep', 'Metódos de Dimensão Fractal', list("Madogram" = "madogram", "Rodogram" = "rodogram", "Variogram" = "variogram","Variation" = "variation", "Periodogram" = "periodogram","Hallwood" = "hallwood" ), selected = "madogram" ),
       ),
    mainPanel(
      actionButton("idBotao","Ler o arquivo"),
      
      downloadButton("downloadTable", "Download"),
      DTOutput("tbl"),
      actionButton("idAtualizar","Atualizar"),
      tableOutput("outTableId"),
      tableOutput("OutDEA"),
      tableOutput("OutSupDEA"),
      plotOutput("graficoDea"),
      plotOutput("graficoDeafrontier")
      

      
    )
  )
)
foo <<- data.frame(matrix(ncol = 5, nrow = 0))
colnames(foo) <- c("DMU", "FractalDim", "TCP_AVG", "Hurst", "Var")

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)
server <- function(input, output) {
  
  observeEvent(input$idBotao, {
    for(nr in 1:length(input$idArquivo[, 1])){
      
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
        
      } else{
        
        name <- tools::file_path_sans_ext(input$idArquivo[[nr, 'name']])
        arquivo <- read.table(input$idArquivo[[nr, 'datapath']])
        vetor <- c(as.numeric(unlist(arquivo)))
        
      }
      
      if(input$typDMU=="tabela"){
        arquivo <- read.xlsx(input$idArquivo[[nr, 'datapath']],  startRow = 1)
    
        name <- arquivo[,1]
        print(name)
        
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
        

      }else{
        dim <- as.numeric(unlist(fd.estimate(vetor, method=input$sep)[2]))
        print(dim)
        dim <- format(round(dim, 2), nsmall = 2) 
        print(dim)
        
        media = mean(vetor, na.rm=FALSE)
        media <- format(round(media, 2), nsmall = 2) 
        
        hurst <- as.numeric(unlist(hurstexp(vetor)[1]))
        hurst <- format(round(hurst, 2), nsmall = 2) 
        
        varianca <- as.numeric(unlist(var(vetor, na.rm=TRUE)[1]))
        varianca <- format(round(varianca, 2), nsmall = 2) 
        
        foo[nrow(foo) + 1,] <<- c(name, dim, media, hurst, varianca)
      }
      

    }
    #tableDea <- read.xlsx()
    if(nrow(foo) > 1){
      print(nrow(foo))
    
      Names=as.character(foo[,1]) # COL names
      print(Names)
      FractalDim=as.numeric(unlist(foo[,2])) #Fractal Dimension 
      print(FractalDim)
      
      TCP_AVG=as.numeric(unlist(foo[,3])) # Mean of TCP bandwidth between VMs on appraisal
      Hurst=as.numeric(unlist(foo[,4])) # Fractal Memory per timeseries
      Varianca=as.numeric(unlist(foo[,5])) # Variança

      dataMatrix=cbind(FractalDim,TCP_AVG,Hurst) # creates the data matrix
      rownames(dataMatrix)=Names # drop the no data rows in dataMatrix
      delete.na <- function(DF, n=0) {
         DF[rowSums(is.na(DF)) <= n,]
      }
      data_dea = delete.na(dataMatrix) #creates the data_dea table
    
   
      inputs = data_dea[,1] # select only input variables values
      outputs = data_dea[,c(2,3)] # select only output variables values, SLACK=TRUE
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
     
      tableSDEA <- data.frame(sort(SCCR_I$eff, decreasing=TRUE))
      tableSDEA <- cbind(DMU = rownames(tableSDEA), tableSDEA)
      rownames(tableSDEA) <- 1:nrow(tableSDEA)
 
      output$OutDEA <- renderTable({tableDea})
      output$OutSupDEA <- renderTable({tableSDEA})
     
    }
    
    output$outTableId <- renderTable({foo})
    
    
    observeEvent(input$tbl_cell_edit, {
      row  <- input$tbl_cell_edit$row
      clmn <- input$tbl_cell_edit$col
      print(foo[row, clmn])
      print(input$tbl_cell_edit$value)
      foo[row, clmn] <<- input$tbl_cell_edit$value
        click("idAtualizar")
    })
    
  
    output$tbl = renderDT(
      foo, editable = list(target = "cell", disable = list(columns =c(0,2:ncol(foo)))),
      callback = JS(js),
      extensions = "KeyTable",
      options = list(
        keys = TRUE
      ))
    

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
      callback = JS(js),
      extensions = "KeyTable",
      options = list(
        keys = TRUE
      ))
    output$outTableId <- renderTable({foo})
    
    
  })
  
  

  

}

shinyApp(ui,server)
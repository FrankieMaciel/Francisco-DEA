library(shiny);
library(pracma);
library(fractaldim);
library(xlsx);
library(openxlsx);
library(shinyjs);
library(Benchmarking)


ui <- fluidPage(
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
    #print(input$header)
    #arquivo <- read.csv(input$idArquivo$datapath, header = input$header,sep = input$sep)
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
    
        name <- as.character(arquivo[,2])
        dim <- as.numeric(arquivo[,3])
        media = as.numeric(arquivo[,4])
        hurst <- as.numeric(arquivo[,5])
        varianca <- as.numeric(arquivo[,6])
        
        list_1 <- list(DMU=name, FractalDim=dim, TCP_AVG =media, Hurst=hurst, Var=varianca)
        foo <<- as.data.frame(list_1)

      }else{
        dim <- as.numeric(unlist(fd.estimate(vetor, method=input$sep)[2]))
        media = mean(vetor, na.rm=FALSE)
        hurst <- as.numeric(unlist(hurstexp(vetor)[1]))
        varianca <- as.numeric(unlist(var(vetor, na.rm=TRUE)[1]))
        
        foo[nrow(foo) + 1,] <<- c(name, dim, media, hurst, varianca)
      }

    }
    #tableDea <- read.xlsx()
    

     Names=strsplit(as.character(foo[,1]),"_m") # COL names
     FractalDim=as.numeric(unlist(foo[,2])) #Fractal Dimension 
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
# 
     SCCR_I=sdea(inputs,outputs,RTS="CRS",ORIENTATION="IN") # runs super-efficiency input-oriented CCR DEA model
#     
     CCR_O=dea(inputs,outputs,RTS="CRS",ORIENTATION="OUT", SLACK=TRUE) # runs output-oriented CCR DEA model
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
     
     output$downloadTable <- downloadHandler(
       filename = function() {
         "table.xlsx"
       },
       content = function(file) {
         write.xlsx(foo, file, row.names = TRUE)
       }
     )
    

    
 
     tableDea <- data.frame(sort(CCR_I$eff))
     tableDea <- cbind(DMU = rownames(tableDea), tableDea)
     rownames(tableDea) <- 1:nrow(tableDea)
     
     tableSDEA <- data.frame(sort(SCCR_I$eff, decreasing=TRUE))
     tableSDEA <- cbind(DMU = rownames(tableSDEA), tableSDEA)
     rownames(tableSDEA) <- 1:nrow(tableSDEA)
 
     output$outTableId <- renderTable({foo})
     output$OutDEA <- renderTable({tableDea})
     output$OutSupDEA <- renderTable({tableSDEA})
    
    
  })
  
}
shinyApp(ui,server)
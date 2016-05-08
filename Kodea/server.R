library(shiny)
# instalatzeko https://www.bioconductor.org/packages/release/bioc/html/simpleaffy.html
library(simpleaffy)
library(affy)
library(ggplot2)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 100MB.
options(shiny.maxRequestSize = 100*1024^2)

shinyServer(function(input, output) {
  
  
  # Linux edo windows
  if(.Platform$OS.type == "unix") {
      separator <- "/"
  } else {
      separator <- "\\"
  }

  # REACTIVE VALUES ----------------------------------------------------

  rvalues <- reactiveValues()
  rvalues$file_names <- NULL
  rvalues$directory <- NULL
  rvalues$raw.data <- NULL
  rvalues$processed.data <- NULL
  
  # DATA UPDATING ------------------------------------------------------
  
  getRawData <- reactive({
	data <- NULL
    # Check for changes in the loaded files
	if (!is.null(input$data_loader) & (is.null(rvalues$file_names) ||
		length(input$data_loader$names)!=length(rvalues$file_names) || 
		all(sort(input$data_loader$names)==sort(rvalues$file_names)))) {
	  
	  
		# Create a temporary directory
		rvalues$directory <- paste0(tempdir(),separator,gsub(" ","_",gsub(":","_",date())))
		dir.create(rvalues$directory)
				
		# Rename the files to reset their original name
		sapply(1:nrow(input$data_loader), 
			   FUN=function(i) {
				file.copy(input$data_loader$datapath[i], paste0(rvalues$directory, separator,
				input$data_loader$name[i]))
			   })
		
		# Update the rvalues object
		rvalues$file_names <- input$data_loader$name
		
		# Fitxeroa zein den bilatu
		covdesc <- rvalues$file_names[-grep("*.CEL", rvalues$file_names)]
		
		rvalues$raw.data <- read.affy(path=rvalues$directory, covdesc=covdesc)
		rvalues$fitxategiak <- input$data_loader$name
		#data <- rvalues$raw.data
		data <- rvalues
	}
	return(data)
  })
  
  # OUTPUTS ------------------------------------------------------------
  
  
  
  ###########
  # Panel 1 #
  ###########
  
  output$panel1_output <- renderUI({
    raw.data <- getRawData()
    fitxategiak <-  raw.data$fitxategiak[grep("*.CEL", raw.data$fitxategiak)]
    if (length(fitxategiak) > 0){
        selectInput("panel1_dataset", "Data set", fitxategiak)
    }
  })
  
  output$panel1_irudia <- renderPlot({
    raw.data <- getRawData()
    if (!is.null(input$panel1_dataset) && !is.null(raw.data$raw.data)){
        pos <- grep(input$panel1_dataset, colnames(raw.data$raw.data))
        if (length(pos) > 0 && pos > 0){
          image(raw.data$raw.data[,pos])
        }
    }
  })
  
  observeEvent(input$ezabatu, {
    raw.data <- getRawData()
    if (!is.null(raw.data$raw.data) && !is.null(raw.data$fitxategiak) && !is.null(input$panel1_dataset)){
      raw.data$fitxategiak <- raw.data$fitxategiak[raw.data$fitxategiak != input$panel1_dataset]
      pos <- grep(input$panel1_dataset, colnames(raw.data$raw.data))
      if (length(pos) > 0 && pos > 0){
        raw.data$raw.data <- raw.data$raw.data[-pos]
      }
    }
    
  })
  
  ###########
  # Panel 2 #
  ###########
  
  output$panel2_boxplot <- renderPlot({
    raw.data <- getRawData()
    res <- NULL
    if(!is.null(rvalues$directory)){
      boxplot(raw.data$raw.data, col=raw.data$raw.data@phenoData@data$Type)#     boxplot(raw.data$raw.data)
    }
  })
  
  output$panel2_output <- renderUI({
    raw.data <- getRawData()
    fitxategiak <-  raw.data$fitxategiak[grep("*.CEL", raw.data$fitxategiak)]
    selectInput("panel2_dataset", "Data set", fitxategiak)
  })
  
  observeEvent(input$ezabatu2, {
    raw.data <- getRawData()
    raw.data$fitxategiak <- raw.data$fitxategiak[raw.data$fitxategiak != input$panel2_dataset]
    raw.data$raw.data <- raw.data$raw.data[-grep(input$panel2_dataset, colnames(raw.data$raw.data))]
  })
  
  
  
  
  output$panel2_irudia1 <- renderPlot({
    raw.data <- getRawData()
    res <- NULL
    if(!is.null(rvalues$directory)){
      res <- hist(raw.data$raw.data)
    }
    print(res)
  })
  
  output$panel2_irudia2 <- renderPlot({
    raw.data <- getRawData()
    res <- NULL
    if(!is.null(rvalues$directory)){
      data.deg <- AffyRNAdeg(raw.data$raw.data)
      res <- plotAffyRNAdeg(data.deg)
    }
    print(res)
  })
  
  ###########
  # Panel 3 #
  ###########
  
  output$panel3_output <- renderUI({
    raw.data <- getRawData()
    fitxategiak <-  raw.data$fitxategiak[grep("*.CEL", raw.data$fitxategiak)]
    selectInput("panel3_dataset", "Data set", fitxategiak)
  })
  
  
  
  output$panel3_irudia <- renderPlot({
    
    
    
    
    raw.data <- getRawData()
    
    num.arrays <- length(raw.data$raw.data)
    
    medians <- sapply(1:num.arrays, 
                      FUN=function(i) {
                        return(median(exprs(raw.data$raw.data[,i])))
                      })
    
    id.ref <- order(medians)[num.arrays/2]
    
    plotMA <- function (data, index) {
      m <- exprs(data[,index]) - exprs(data[,id.ref])
      a <- (exprs(data[,index]) + exprs(data[,id.ref]))/2
      ma.plot(a,m,cex=0.75,lwd=3)
    }
    
    # Get the data set with the appropriate name
    #dat <- get(input$dataset)
    plotMA(raw.data$raw.data, grep(input$panel3_dataset, colnames(raw.data$raw.data)))
  })
  
  observeEvent(input$ezabatu3, {
    raw.data <- getRawData()
    raw.data$fitxategiak <- raw.data$fitxategiak[raw.data$fitxategiak != input$panel3_dataset]
    raw.data$raw.data <- raw.data$raw.data[-grep(input$panel3_dataset, colnames(raw.data$raw.data))]
  })
  
  ###########
  # Panel 4 #
  ###########
  
  output$panel4_irudia <- renderPlot({
    raw.data <- getRawData()
    mas5.data <- call.exprs(raw.data$raw.data,"mas5")
    qcs <- qc(raw.data$raw.data,mas5.data)
    plot(qcs)
  })
  
  observeEvent(input$ezabatu4, {
    raw.data <- getRawData()
    raw.data$fitxategiak <- raw.data$fitxategiak[raw.data$fitxategiak != input$panel4_dataset]
    raw.data$raw.data <- raw.data$raw.data[-grep(input$panel4_dataset, colnames(raw.data$raw.data))]
  })
  
  output$panel4_output <- renderUI({
    raw.data <- getRawData()
    fitxategiak <-  raw.data$fitxategiak[grep("*.CEL", raw.data$fitxategiak)]
    selectInput("panel4_dataset", "Data set", fitxategiak)
  })
  
  ###########
  # Panel 5 #
  ###########
  
  output$panel5_output <- renderUI({
    raw.data <- getRawData()
    fitxategiak <-  raw.data$fitxategiak[grep("*.CEL", raw.data$fitxategiak)]
    selectInput("panel5_dataset", "Data set", fitxategiak)
  })
  
  observeEvent(input$ezabatu5, {
    raw.data <- getRawData()
    raw.data$fitxategiak <- raw.data$fitxategiak[raw.data$fitxategiak != input$panel5_dataset]
    raw.data$raw.data <- raw.data$raw.data[-grep(input$panel5_dataset, colnames(raw.data$raw.data))]
  })
  
  output$panel5_irudia1 <- renderPlot({
    raw.data <- getRawData()
    # Normalization
    rma.data <- call.exprs(raw.data$raw.data,"rma")
    
    
    # Boxplot and distribution after normalization
    boxplot(exprs(rma.data), col=raw.data$raw.data@phenoData@data$Type)
  })
  
  output$panel5_irudia2 <- renderPlot({
    raw.data <- getRawData()
    
    rma.data <- call.exprs(raw.data$raw.data,"rma")
    plot(density(exprs(rma.data[,1]),col=raw.data$raw.data@phenoData@data$Type[1]))
    sapply(2:length(raw.data$raw.data), 
           FUN=function(i) {
             lines(density(exprs(rma.data[,i])), col=raw.data$raw.data@phenoData@data$Type[i])
           })
  })
  
  ###########
  # Panel 6 #
  ###########
  
  
  output$panel6_output <- renderUI({
    raw.data <- getRawData()
    fitxategiak <-  raw.data$fitxategiak[grep("*.CEL", raw.data$fitxategiak)]
    selectInput("panel6_dataset", "Data set", fitxategiak)
  })
  
  output$panel6_irudia <- renderPlot({
    
    raw.data <- getRawData()
    rma.data <- call.exprs(raw.data$raw.data,"rma")
    
    medians <- sapply(1:length(raw.data$raw.data), 
                      FUN=function(i) {
                        return(median(exprs(raw.data$raw.data[,i])))
                      })
    
    id.ref <- order(medians)[length(raw.data$raw.data)/2]
    
    plotMA <- function (data, index) { 
      m <- exprs(data[,index]) - exprs(data[,id.ref])
      a <- (exprs(data[,index]) + exprs(data[,id.ref]))/2
      ma.plot(a,m,cex=0.75,lwd=3)
    }
    
    
    plotMA(rma.data, grep(input$panel6_dataset, colnames(raw.data$raw.data)))
  })
  
  observeEvent(input$ezabatu6, {
    raw.data <- getRawData()
    raw.data$fitxategiak <- raw.data$fitxategiak[raw.data$fitxategiak != input$panel6_dataset]
    raw.data$raw.data <- raw.data$raw.data[-grep(input$panel6_dataset, colnames(raw.data$raw.data))]
  })
  
  

})


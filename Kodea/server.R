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
		rvalues$raw.data <- read.affy(path=rvalues$directory)
		#data <- rvalues$raw.data
		data <- rvalues
	}
	return(data)
  })
  
  # OUTPUTS ------------------------------------------------------------
  
  output$myPlot <- renderPlot({
     raw.data <- getRawData()
     #image(raw.data$raw.data[,grep(dat, colnames(raw.data$raw.data))])
  })
  
  
  
  output$choose_dataset <- renderUI({
    raw.data <- getRawData()
    fitxategiak <-  raw.data$file_names[grep("*.CEL", raw.data$file_names)]
    selectInput("dataset", "Data set", fitxategiak)
  })
  
  
  output$plot2 <- renderPlot({
    raw.data <- getRawData()
    
    # Get the data set with the appropriate name
    #dat <- get(input$dataset)
    image(raw.data$raw.data[,grep(input$dataset, colnames(raw.data$raw.data))])
  })
  
  input$do <- observe({
    if(input$ezabatu > 0){
      #TODO: Elementua ezabatzen da
      #Beharbada mezu bat adieraziko da ezabatu egin dela
      raw.data$raw.data <- isolate(raw.data$raw.data[,grep(input$dataset, colnames(raw.data$raw.data))])
    }
    
  })
  
  
  ## COMBOBOX BAT JARRIKO DA HEMEN -------------------------------------
  ##
  ##TODO: 

})


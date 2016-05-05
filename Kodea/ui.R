

header <- h1("Bioinformatika")

fileLoader <- fileInput (inputId="data_loader", 
						 label="Upload Files", 
						 multiple=TRUE)

shinyUI(fluidPage(
	header, 
	fileLoader,
	  tabsetPanel("true",
	    tabPanel("Plot", plotOutput(outputId="myPlot", width="500px")),
	    tabPanel("kaka", uiOutput("choose_dataset"), plotOutput(outputId="plot2", width="500px"), actionButton("ezabatu", "Ezabatu")),
	    tabPanel("kaka 2")
	  )
))


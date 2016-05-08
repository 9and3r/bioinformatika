

header <- h1("Bioinformatika")

fileLoader <- fileInput (inputId="data_loader", 
						 label="Upload Files", 
						 multiple=TRUE)

shinyUI(fluidPage(
	header, 
	fileLoader,
	  tabsetPanel("true",
	    tabPanel("kaka", uiOutput("panel1_output"), plotOutput(outputId="panel1_irudia", width="500px"), actionButton("ezabatu", "Ezabatu")),
	    tabPanel("kaka 2", plotOutput(outputId="panel2_boxplot", width="500px"), plotOutput(outputId="panel2_irudia1", width="500px"), plotOutput(outputId="panel2_irudia2", width="500px")),
	    tabPanel("kaka 3", uiOutput("panel3_output"), plotOutput(outputId="panel3_irudia", width="500px"), actionButton("ezabatu2", "Ezabatu"))
	   )
))


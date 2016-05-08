

header <- h1("Bioinformatika")

fileLoader <- fileInput (inputId="data_loader", 
						 label="Upload Files", 
						 multiple=TRUE)

shinyUI(fluidPage(
	header, 
	fileLoader,
	  tabsetPanel("true",
	    tabPanel("kaka", uiOutput("panel1_output"), plotOutput(outputId="panel1_irudia", width="500px"), actionButton("ezabatu", "Ezabatu")),
	    tabPanel("kaka 2", plotOutput(outputId="panel2_boxplot", width="500px"), plotOutput(outputId="panel2_irudia1", width="500px"), plotOutput(outputId="panel2_irudia2", width="500px"), uiOutput("panel2_output"), actionButton("ezabatu2", "Ezabatu")),
	    tabPanel("kaka 3", uiOutput("panel3_output"), plotOutput(outputId="panel3_irudia", width="500px"), actionButton("ezabatu3", "Ezabatu")),
	    tabPanel("kaka 4", plotOutput(outputId="panel4_irudia", width="500px"), uiOutput("panel4_output"), actionButton("ezabatu4", "Ezabatu")),
	    tabPanel("kaka 5", plotOutput(outputId="panel5_irudia1", width="500px"), plotOutput(outputId="panel5_irudia2", width="500px"), uiOutput("panel5_output"), actionButton("ezabatu5", "Ezabatu")),
	    tabPanel("kaka 6", uiOutput("panel6_output"), plotOutput(outputId="panel6_irudia", width="500px"), actionButton("ezabatu6", "Ezabatu"))
	   )
))


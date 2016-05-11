

header <- h1("Bioinformatika")

fileLoader <- fileInput (inputId="data_loader", 
						 label="Upload Files", 
						 multiple=TRUE)

shinyUI(fluidPage(
	header, 
	fileLoader,
	  tabsetPanel("true",
	    tabPanel("kaka", uiOutput("panel1_output"), plotOutput(outputId="panel1_irudia", width="500px"), actionButton("ezabatu", "Ezabatu")),
	    tabPanel("kaka 2", uiOutput("panel2_output"), actionButton("ezabatu2", "Ezabatu"), plotOutput(outputId="panel2_boxplot", width="500px"), plotOutput(outputId="panel2_irudia1", width="500px"), plotOutput(outputId="panel2_irudia2", width="500px")),
	    tabPanel("kaka 3", uiOutput("panel3_output"), plotOutput(outputId="panel3_irudia", width="500px"), actionButton("ezabatu3", "Ezabatu")),
	    tabPanel("kaka 4", uiOutput("panel4_output"), actionButton("ezabatu4", "Ezabatu"), plotOutput(outputId="panel4_irudia", width="500px")),
	    tabPanel("kaka 5", uiOutput("panel5_output"), actionButton("ezabatu5", "Ezabatu"), plotOutput(outputId="panel5_irudia1", width="500px"), plotOutput(outputId="panel5_irudia2", width="500px")),
	    tabPanel("kaka 6", uiOutput("panel6_output"), plotOutput(outputId="panel6_irudia", width="500px"), actionButton("ezabatu6", "Ezabatu"))
	   )
))


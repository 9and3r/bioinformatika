

header <- h1("Bioinformatika")

fileLoader <- fileInput (inputId="data_loader", 
						 label="Upload Files", 
						 multiple=TRUE)



shinyUI(fluidPage(
  title = 'Bioinformatika',
	header, 
  wellPanel(
    fileLoader,
    uiOutput("output_ui_panel")),
	  tabsetPanel("true",
	    tabPanel("Irudiaen egiaztapena", plotOutput(outputId="panel1_irudia", width="500px")),
	    tabPanel("Grafiko esanguratsuak", plotOutput(outputId="panel2_boxplot", width="500px"), plotOutput(outputId="panel2_irudia1", width="500px"), plotOutput(outputId="panel2_irudia2", width="500px")),
	    tabPanel("Erregresioen grafikoa", plotOutput(outputId="panel3_irudia", width="500px")),
	    tabPanel("QC estatistikak", plotOutput(outputId="panel4_irudia", width="500px")),
	    tabPanel("Normalizatutako ondorengo grafikoak", plotOutput(outputId="panel5_irudia1", width="500px"), plotOutput(outputId="panel5_irudia2", width="500px")),
	    tabPanel("Erregresioak normalizatu ondoren", plotOutput(outputId="panel6_irudia", width="500px"))
	   )
))


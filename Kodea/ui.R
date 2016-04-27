

header <- h1("Bioinformatika")

fileLoader <- fileInput (inputId="data_loader", 
						 label="Upload Files", 
						 multiple=TRUE)

shinyUI(fluidPage(
	header, 
	fileLoader,
	plotOutput(outputId="file_list", width="500px"),
	plotOutput(outputId="myPlot", width="500px")
))


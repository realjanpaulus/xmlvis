library(curl)
library(docstring, warn.conflicts = FALSE)
library(jsonlite, warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(shinythemes)
source("utils.R")


# global variables #

urlcorpus <- "https://dracor.org/api/corpora/ger"
urlcorpora <- 'https://dracor.org/api/corpora'
urlcorporashort <- "https://dracor.org/api/corpora/"


# == #
# UI #
# == #

ui <- fluidPage(theme = shinytheme("united"),
	headerPanel("XMLvis: A visualization of XML corpora"),
	sidebarLayout(
		sidebarPanel(
			verticalLayout(
				uiOutput("corpora"),
				uiOutput("authors"),
				uiOutput("base"), 
				tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))
			),
			wellPanel()
		),
		mainPanel(
			tags$style(type="text/css",
				".shiny-output-error { visibility: hidden; }",
				".shiny-output-error:before { visibility: hidden; }"
			)
		)
	)


)

# ====== #
# SERVER #
# ====== #

server <- function(input, output){
	corp <- reactive({
		lang <- input$corpus
		if(is.null(lang)) return(NULL)
		extractdramas(paste0(urlcorporashort, lang))
	})

	#TODO: für alle corpora: selectcorpora(urlcorpora))
	# render corpus selection
	output$corpora <- renderUI({
		selectInput("corpus",
		            label = "Choose a corpus",
		            choices = selectcorpora(urlcorpora))          
	})

	# render author selection
	output$authors <- renderUI({
	    if (is.null(corp() )) return(NULL)
	    selectInput("selectedauthor",
	                label = "Choose a writer from a list:",
	                choices = selectauthors(corp() ))
  	})

	# render play selection
	output$base <- renderUI({
		if (is.null( corp() )) return(NULL)
		#todo: ehemals "file2download"
		selectInput("selectedplay", 
					label = "Choose his/her play to visualize:",
					choices = selectplays(corp(), input$selectedauthor))
	})


}

# Run the app
options(shiny.port = 3000)
options(shiny.launch.browser = TRUE)
shinyApp(ui = ui, server = server)


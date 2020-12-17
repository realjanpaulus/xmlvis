library(curl)
library(docstring, warn.conflicts = FALSE)
library(jsonlite, warn.conflicts = FALSE)
library(httr, warn.conflicts = FALSE)
library(methods)
library(shiny, warn.conflicts = FALSE)
library(shinythemes)
library(XML)
source("utils.R")


# global variables #

urlcorpus <- "https://dracor.org/api/corpora/ger"
urlcorpora <- "https://dracor.org/api/corpora"
urlcorporashort <- "https://dracor.org/api/corpora/"


# == #
# UI #
# == #

ui <- fluidPage(theme = shinytheme("united"),
	headerPanel("XMLvis: A visualization of XML corpora"),
	sidebarLayout(
		sidebarPanel(
			verticalLayout(
				uiOutput("select_corpora"),
				uiOutput("checkbox_wholecorpus"),
				# only renders author/play selection if checkbox is not activated
				conditionalPanel(condition = "input.checkbox_wholecorpus != 1",
					uiOutput("select_authors"),
					uiOutput("select_plays")
				),
				tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))
			),
			wellPanel(
				#TODO: überarbeiten
				conditionalPanel(condition = "input.tabselected == 1",
					uiOutput("function1")
				),
				conditionalPanel(condition = "input.tabselected == 2",
					uiOutput("function2")
				),
				conditionalPanel(condition = "input.tabselected == 'info'",
					uiOutput("functioninfo")
				)
				
			)
		),
		mainPanel(
			tags$style(type="text/css",
				".shiny-output-error { visibility: hidden; }",
				".shiny-output-error:before { visibility: hidden; }"
			),
			tabsetPanel(id="tabselected",
				tabPanel("Function 1", value=1),
				tabPanel("Function 2", value=2),
				tabPanel("Function 3", 
					uiOutput("xmltest")
				),
				tabPanel("Function 4", value=4),
				tabPanel("Info", value="info")
			)
		)
	)

)

# ====== #
# SERVER #
# ====== #

server <- function(input, output) {



	# TODO: erklären
	corp <- reactive({
		lang <- input$corpus
		if(is.null(lang)) return(NULL)
		extractdramas(paste0(urlcorporashort, lang))
	})

	# render corpus selection
	output$select_corpora <- renderUI({
		selectInput("corpus",
		            label = "Choose a corpus",
		            choices = selectcorpora(urlcorpora))          
	})

	# render whole corpus checkbox
	output$checkbox_wholecorpus <- renderUI({
		checkboxInput("checkbox_wholecorpus", 
					  label = "Use whole corpus")
	})

	# render author selection
	output$select_authors <- renderUI({
	    if (is.null(corp() )) return(NULL)
	    selectInput("selectedauthor",
	                label = "Choose a writer from a list:",
	                choices = selectauthors(corp() ))
  	})

	# render play selection
	# input$selectedplay = https://dracor.org/api/corpora/als/play/arnold-der-pfingstmontag/networkdata/csv
	output$select_plays <- renderUI({
		if (is.null(corp() )) return(NULL)
		#todo: ehemals "file2download"
		selectInput("selectedplay", 
					label = "Choose his/her play to visualize:",
					choices = selectplays(corp(), input$selectedauthor))
	})


	# todo: anderer funktionsname
	# todo: dummy function ändern
	output$function1 <- renderUI({
		selectInput("selectedfunction1",
					label = "Select something:",
					choices = c("A" = 1, "B" = 2, "C" = 3))
	})

	# todo: anderer funktionsname
	# todo: dummy function ändern
	output$function2 <- renderUI({
		selectInput("selectedfunction2",
					label = "Select something else:",
					choices = c("X" = 1, "Y" = 2, "Z" = 3))
	})

	# todo: anderer funktionsname
	# todo: dummy function ändern
	output$functioninfo <- renderUI({
		selectInput("selectedfunctioninfo",
					label = "more info",
					choices = c("google" = 1, "bing" = 2))
	})

	#todo: weg, da nur test.
	output$xmltest <- renderUI({
		# TODO: andere Lösung?
		spokentexturl <- paste0(urlcorpora, "/", input$corpus, "/play/", 
								input$selectedplay, "/spoken-text")
		if (is.null(corp() )) return(NULL)
		# TODO: substr weg
		helpText(substr(getplaytext(spokentexturl), 1, 10),
				 label="xmllink")
	})

}

# Run the app
options(shiny.port = 3000)
options(shiny.launch.browser = TRUE)
shinyApp(ui = ui, server = server)


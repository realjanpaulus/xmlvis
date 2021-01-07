library(curl)
library(docstring, warn.conflicts = FALSE)
library(DT)
library(jsonlite, warn.conflicts = FALSE)
library(httr, warn.conflicts = FALSE)
library(methods)
# todo
# library(NLP, warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(shinythemes)
# todo
# library(tm, warn.conflicts = FALSE)
library(XML)
source("utils.R")


# global variables #

about <- "about.html"
more <- "more.html"


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
				# only renders author/play selection if the "Use whole checkbox" 
				# checkbox is not activated
				conditionalPanel(condition = "input.checkbox_wholecorpus == false",
					uiOutput("select_authors"),
					# only renders play selection if the "Use all plays" 
					# checkbox is not activated
					conditionalPanel(condition = "input.checkbox_allplays == false",
						uiOutput("select_plays")
					),
					uiOutput("checkbox_allplays")
				),
				tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))
			),
			wellPanel(
				conditionalPanel(condition = "input.tabselected == 1",
					uiOutput("statscontrols")
				),
				conditionalPanel(condition = "input.tabselected == 2",
					uiOutput("wordfreqtitle"),
					# todo weg?
					# uiOutput("wordfreqcontrols")
					uiOutput("checkbox_stopwords")
				),
				#TODO: überarbeiten
				conditionalPanel(condition = "input.tabselected == 11",
					uiOutput("function1")
				),
				conditionalPanel(condition = "input.tabselected == 22",
					uiOutput("function2")
				)
				
			)
		),
		mainPanel(
			tags$style(type="text/css",
				".shiny-output-error { visibility: hidden; }",
				".shiny-output-error:before { visibility: hidden; }"
			),
			tabsetPanel(id="tabselected",
				tabPanel(title = uiOutput("statspanel"), 
						 uiOutput("statspage"), 
						 value=1),
				tabPanel(title = uiOutput("wordfreqpanel"), 
						 DT::dataTableOutput(outputId = "wordfreqtable"),
						 value=2),
				#todo weg!
				tabPanel(title = "Function 1", value=11),
				tabPanel(title = "Function 2", value=22),
				tabPanel(title = "Function 3", uiOutput("xmltest"), value=3),
				tabPanel(title = "Function 4", value=4),
				tabPanel(title = "About", includeHTML(about), value="about"),
				tabPanel(title = "More", includeHTML(more), value="more")
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
		            choices = selectcorpora(urlcorpus))
					# EXPAND: change to `urlcorpora` for all corpora
	})

	# render whole corpus checkbox
	output$checkbox_wholecorpus <- renderUI({
		checkboxInput("checkbox_wholecorpus", 
					  label = "Use whole corpus",
					  value = FALSE)
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

	# render all plays checkbox
	output$checkbox_allplays <- renderUI({
		checkboxInput("checkbox_allplays", 
					  label = "Use all plays",
					  value = FALSE)
	})

	# render stopwords checkbox (often reused)
	output$checkbox_stopwords <- renderUI({
		checkboxInput("checkbox_stopwords",
					  label = "Consider stopwords",
					  value = FALSE)
	})


	#############################################
	### Render tab 1: Plays/corpus statistics ###
	#############################################

	# TODO!!!

	# render tab name dynamically
	output$statspanel <- renderText({
		if (!isTRUE(input$checkbox_wholecorpus) & !isTRUE(input$checkbox_allplays)) {
			"Play statistics"
		} else if (!isTRUE(input$checkbox_wholecorpus) & isTRUE(input$checkbox_allplays)) {
			"Plays statistics"
		} else {
			"Corpus statistics"
		}
	})

	# todo: label/choices ändern
	output$statscontrols <- renderUI({
		selectInput("selected_stats_controls",
					label = "Select something:",
					choices = c("A" = 1, "B" = 2, "C" = 3))
	})

	# render statistics page
	# todo
	output$statspage<- renderUI({
		# todo: html interaktiv? oder lieber table?

		if (!isTRUE(input$checkbox_wholecorpus) & !isTRUE(input$checkbox_allplays)) {

			includeHTML(more)
		} else if (!isTRUE(input$checkbox_wholecorpus) & isTRUE(input$checkbox_allplays)) {
			includeHTML(about)
		} else {
			renderText({
				"Corpus"
			})
			#renderDataTable()
		}

	})


	############################################
	### Render tab 2: Word Frequencies Table ###
	############################################


	# render tab name dynamically
	# todo: doing this globally?
	output$wordfreqpanel <- renderText({
		if (!isTRUE(input$checkbox_wholecorpus) & !isTRUE(input$checkbox_allplays)) {
			"Play word frequencies"
		} else if (!isTRUE(input$checkbox_wholecorpus) & isTRUE(input$checkbox_allplays)) {
			"Plays word frequencies"
		} else {
			"Corpus word frequencies"
		}
	})

	# render dynamic panel heading
	output$wordfreqtitle <- renderText({
		"<h4><u>Adjust the word frequency table</u></h4>"
	})

	# todo: label/choices ändern
	output$wordfreqcontrols <- renderUI({
		selectInput("selected_wordfreq_controls",
					label = "Select something:",
					choices = c("A" = 1, "B" = 2, "C" = 3))
	})

	# render word frequency table
	output$wordfreqtable<- DT::renderDataTable({
		# todo weg: placeholder
		#mtcars
		

		# todo: https://shiny.rstudio.com/articles/datatables.html

		if (!isTRUE(input$checkbox_wholecorpus) & !isTRUE(input$checkbox_allplays)) {
			# only one play
			spokentexturl <- paste0(urlcorpora, "/", input$corpus, "/play/", 
									input$selectedplay, "/spoken-text")
			
			# todo if
			if (!isTRUE(input$checkbox_wholecorpus)) {
				df = wordfreqdf(spokentexturl, lang, remove_stopwords = TRUE)
			} else {
				df = wordfreqdf(spokentexturl, lang)
			}
			DT::datatable(df,
						  filter = 'top', 
						  options = list(lengthMenu = c(5, 10, 20, 30, 50, 100),
						  				 orderClasses = TRUE,
						  				 pageLength = 10))

			

		} else if (!isTRUE(input$checkbox_wholecorpus) & isTRUE(input$checkbox_allplays)) {
			# all plays
			# todo
			DT::datatable(mtcars, 
						  options = list(lengthMenu = c(5, 10, 20, 30, 50, 100),
						  				 orderClasses = TRUE,
						  				 pageLength = 10))
		} else {
			# whole corpus
			# todo
			DT::datatable(mtcars, 
						  options = list(lengthMenu = c(5, 10, 20, 30, 50, 100),
						  				 orderClasses = TRUE,
						  				 pageLength = 10))
		}
		

	})


	#todo: weg, da nur test.
	output$xmltest <- renderUI({
		# TODO: andere Lösung?
		spokentexturl <- paste0(urlcorpora, "/", input$corpus, "/play/", 
								input$selectedplay, "/spoken-text")
		if (is.null(corp() )) return(NULL)
		# TODO: substr weg
		# helpText(substr(getplaytext(spokentexturl), 1, 10),
		# 		 label="xmllink")
		# TODO: interaktiv mit Worteingabe
		#		siehe	https://shiny.rstudio.com/articles/dynamic-ui.html
		helpText(paste0("Anzahl des Wortes '", "ich", "': ", countwords(spokentexturl, "ich")),
				 label="outputtext")

	})




	### Render tab TODO ###

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




}

# Run the app
options(shiny.port = 3000)
options(shiny.launch.browser = TRUE)
shinyApp(ui = ui, server = server)


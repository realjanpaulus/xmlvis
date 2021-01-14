library(curl)
library(docstring, warn.conflicts = FALSE)
library(DT)
library(jsonlite, warn.conflicts = FALSE)
library(httr, warn.conflicts = FALSE)
library(methods)
library(plyr)
# todo
# library(NLP, warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(shinycssloaders)
library(shinythemes)
# todo
# library(tm, warn.conflicts = FALSE)
library(XML)
source("utils.R")


# global variables #

about <- "www/about.html"
more <- "www/more.html"
wordfreqhtml <- "www/wordfreqtext.html"


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
				uiOutput("select_authors"),
				# only renders play selection if the "Use all plays" 
				# checkbox is not activated
				conditionalPanel(condition = "input.checkbox_allplays == false",
					uiOutput("select_plays")
				),
				uiOutput("checkbox_allplays"),
				tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))
			),
			wellPanel(
				conditionalPanel(condition = "input.tabselected == 1",
					uiOutput("statscontrols")
				),
				conditionalPanel(condition = "input.tabselected == 2",
					uiOutput("wordfreqpaneltitle"),
					# todo weg?
					# uiOutput("wordfreqcontrols")
					uiOutput("checkbox_stopwords"),
					uiOutput("checkbox_punctation"),
					uiOutput("checkbox_lowercase")
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
				tabPanel(title = "Word frequencies", 
						 titlePanel(uiOutput("wordfreqtitle")),
						 fluidRow(
						 	column(width=6, 
						 		   style="padding-top: 22px;",
						 		   DT::dataTableOutput(outputId = "wordfreqtable")),
						 	column(width=6, 
						 		   style="padding-top: 0px; padding-right: 22px;",
						 		   uiOutput("wordfreqtext")) 
						 ),
						 value=2),
				tabPanel(title = "Statistics", 
						 uiOutput("statspage"), 
						 value=1), #todo: reihenfolge ändern? das eins hoch
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

	
	# reactive corpus list with all information of a corpus,
	# taken from json file, which was identified by its input `lang`
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

	# render punctation checkbox (often reused)
	output$checkbox_punctation <- renderUI({
		checkboxInput("checkbox_punctation",
					  label = "Remove punctation",
					  value = TRUE)
	})

	# render lowercase checkbox (often reused)
	output$checkbox_lowercase <- renderUI({
		checkboxInput("checkbox_lowercase",
					  label = "Convert all words to lowercase",
					  value = TRUE)
	})


	#############################################
	### Render tab 1: Plays/corpus statistics ###
	#############################################

	# TODO!!!

	# render tab name dynamically
	# todo: different name, see render tab 2
	output$statspanel <- renderText({
		if (!isTRUE(input$checkbox_allplays)) {
			"Play statistics <br/>(Single play)"
		} else {
			"Plays statistics <br/>(All plays)"
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

		if (!isTRUE(input$checkbox_allplays)) {
			includeHTML(more)
		} else {
			includeHTML(about)
		}

	})



####
	#
	#
	# todo: hier aktuell
####
	#
	#



	#todo: position noch richtig?
	############################################
	### Render tab 2: Word Frequencies Table ###
	############################################


	# render dynamic tab title
	output$wordfreqtitle <- renderText({
		if (!isTRUE(input$checkbox_allplays)) {
			"Word frequencies of a single play"
		} else {
			"Word frequencies of all plays of an author"
		} 
	})

	# render dynamic panel heading
	output$wordfreqpaneltitle <- renderText({
		"<h4><u>Adjust the word frequency table</u></h4>"
	})

	# render HTML page with instructions on how to use table
	output$wordfreqtext <- renderUI({
		includeHTML(wordfreqhtml)
	})

	# render word frequency table
	output$wordfreqtable<- DT::renderDataTable({
		
		lang <- input$corpus

		## only one play ##
		if (!isTRUE(input$checkbox_wholecorpus) & !isTRUE(input$checkbox_allplays)) {
			spokentexturl <- paste0(urlcorpora, "/", input$corpus, "/play/", 
									input$selectedplay, "/spoken-text")
			playtext <- getplaytext(spokentexturl)



			withProgress(message = 'Counting word frequencies', {
				df = wordfreqdf(playtext, 
								lang, 
								remove_punct = input$checkbox_punctation,
								tolower = input$checkbox_lowercase, 
								remove_stopwords = input$checkbox_stopwords)
			})

			DT::datatable(df,
						  filter = 'top', 
						  options = list(lengthMenu = c(5, 10, 20, 30, 50, 100),
						  				 orderClasses = TRUE,
						  				 pageLength = 10))
		
		## all plays of an author ##
		} else if (!isTRUE(input$checkbox_wholecorpus) & isTRUE(input$checkbox_allplays)) {
			
			playnames <- selectplays(corp(), input$selectedauthor)

			n <- length(playnames)
			dflist <- vector("list", n)

			# progressbar for loading plays
			withProgress(message = 'Loading plays', value = 0, {
				
				i <- 0
				for(play in playnames) {
					spokentexturl <- paste0(urlcorpora, "/", input$corpus, "/play/", 
											play, "/spoken-text")
					
					playtext <- getplaytext(spokentexturl)
					withProgress(message = 'Counting word frequencies', {
						df <- wordfreqdf(playtext, 
										lang, 
										remove_punct = input$checkbox_punctation,
										tolower = input$checkbox_lowercase, 
										remove_stopwords = input$checkbox_stopwords)
						
					})

					# Increment the progress bar, and update the detail text.
					i <- i + 1
		    		incProgress(1/n, detail = paste("Play", i, "of", n))

		    		dflist[[i]] <- df

				}

			})

			# combining all document-term-dataframes into one
			withProgress(message = "Counting document frequencies", {
				df <- Reduce(add_dfs, dflist)
			})
			

			DT::datatable(df,
						  filter = 'top', 
						  options = list(lengthMenu = c(5, 10, 20, 30, 50, 100),
						  				 orderClasses = TRUE,
						  				 pageLength = 10))
		} 
	})

	
	# todo: label/choices ändern
	# todo: überhaupt nötig?
	output$wordfreqcontrols <- renderUI({
		selectInput("selected_wordfreq_controls",
					label = "Select something:",
					choices = c("A" = 1, "B" = 2, "C" = 3))
	})

	

#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################


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

	###
	#
	# WORDCLOUD
	# https://tutorials.quanteda.io/statistical-analysis/frequency/
	# https://quanteda.io/articles/pkgdown/examples/plotting.html
	#
	###


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
options(shiny.autoreload = TRUE)
options(shiny.launch.browser = TRUE)
shinyApp(ui = ui, server = server)


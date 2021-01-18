library(curl)
library(docstring, warn.conflicts = FALSE)
library(DT)
library(jsonlite, warn.conflicts = FALSE)
library(httr, warn.conflicts = FALSE)
library(methods)
library(plyr)
library(RColorBrewer)
library(shiny, warn.conflicts = FALSE)
library(shinycssloaders)
library(shinythemes)
library(XML)
source("utils.R")


# global variables #

about <- "www/about.html"
more <- "www/more.html"
wordcloudhtml <- "www/wordcloudtext.html"
wordfreqhtml <- "www/wordfreqtext.html"


urlcorpus <- "https://dracor.org/api/corpora/ger"
urlcorpora <- "https://dracor.org/api/corpora"
urlcorporashort <- "https://dracor.org/api/corpora/"

# colorblind friendly color palettes
colors_sequential <- brewer.pal(n = 8, name = 'PRGn')
colors_sequential2 <- rev(brewer.pal(n = 8, name = 'PRGn'))
colors_qualitative <- brewer.pal(n = 6, name = 'Set2')



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
				# only renders play selection if the "Use all plays" checkbox 
				# is not activated
				conditionalPanel(condition = "input.checkbox_allplays == false",
					uiOutput("select_plays")
				),
				uiOutput("checkbox_allplays"),
				uiOutput("textpreprocessingtitle"),
				uiOutput("checkbox_stopwords"),
				uiOutput("checkbox_punctation"),
				uiOutput("checkbox_lowercase"),
				tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))
			),
			wellPanel(
				#todo: überarbeiten!! vllt weg?
				conditionalPanel(condition = "input.tabselected == 1",
					uiOutput("statscontrols")
				),
				# wordcloud panel
				conditionalPanel(condition = "input.tabselected == 3",
					conditionalPanel(condition = "input.checkbox_allplays == true",
							uiOutput("wordcloudallplayspaneltitle"),
							uiOutput("checkbox_wordcloud_comparison"),
							conditionalPanel(condition = "input.checkbox_wordcloud_comparison == true",
									sliderInput("wordcloud_label_size",
												"Size of the labels:",
												min = 1,
												max = 4,
												step = 0.1,
												value = 2),
									sliderInput("wordcloud_label_padding",
												"Padding of the labels:",
												min = -0.2,
												max = 0.2,
												step = 0.01,
												value = -0.1)
							)
						),
					uiOutput("wordcloudpaneltitle"),					
					sliderInput("wordcloud_min_freq", 
								"Minimum frequency:",
								min = 1,  
								max = 200,
								step = 1, 
								value = 20),
					sliderInput("wordcloud_max_n_words",
								"Maximum number of words:",
								min = 1,  
								max = 300,  
								step = 1,
								value = 50),
					# EXPAND: min padding could be decreased (e.g. -1)
					#		  for other languages with a non-latin script
					sliderInput("wordcloud_word_padding",
								"Word padding:",
								min = 0,  
								max = 1,
								step = 0.1,  
								value = 0.1),
					sliderInput("wordcloud_window_size",
								"Window size:",
								min = 1,
								max = 2.2,
								step = 0.1,
								value = 1.8),		
				)
				
			)
		),
		mainPanel(
			tags$style(type="text/css",
				".shiny-output-error { visibility: hidden; }",
				".shiny-output-error:before { visibility: hidden; }"
			),
			tabsetPanel(id="tabselected",
				tabPanel(title = "Statistics", 
						 uiOutput("statspage"), 
						 value=1), 
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
				tabPanel(title = "Wordcloud", 
						 titlePanel(uiOutput("wordcloudtitle")),
						 fluidRow(
							column(width=6,
								   align="center",
								   style="padding-top: 0px;",
								   plotOutput(outputId = "wordcloudplotter",
								   			  inline = TRUE)
							),
							column(width=6,
								   style="padding-top: 0px; padding-right: 22px;",
								   uiOutput("wordcloudtext")) 
						 ),
						 value=3),
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

	#####################
	### sidebar panel ###
	#####################
	
	# reactive corpus list with all informations of a corpus,
	# taken from json file, which was identified by its input `lang`
	corp <- reactive({
		lang <- input$corpus
		if(is.null(lang)) return(NULL)
		extractdramas(paste0(urlcorporashort, lang))
	})

	# render corpus selection
	output$select_corpora <- renderUI({
		selectInput("corpus",
					label = "Choose a corpus:",
					choices = selectcorpora(urlcorpus))
					# EXPAND: change to `urlcorpora` for all corpora
	})

	# render author selection
	output$select_authors <- renderUI({
		if (is.null(corp() )) return(NULL)
		selectInput("selectedauthor",
					label = "Choose an author:",
					choices = selectauthors(corp() ))
	})

	# render play selection
	output$select_plays <- renderUI({
		if (is.null(corp() )) return(NULL)
		selectInput("selectedplay", 
					label = "Choose his/her play:",
					choices = selectplays(corp(), input$selectedauthor))
	})

	# render all plays checkbox
	output$checkbox_allplays <- renderUI({
		checkboxInput("checkbox_allplays", 
					  label = "Use all plays",
					  value = FALSE)
	})

	##################################
	### Text preprocessing options ###
	##################################

	output$textpreprocessingtitle <- renderText({
		"<h4><u>Text preprocessing options</u></h4>"
	})


	# render stopwords checkbox
	output$checkbox_stopwords <- renderUI({
		checkboxInput("checkbox_stopwords",
					  label = "Remove stopwords",
					  value = TRUE)
	})


	# render punctation checkbox
	output$checkbox_punctation <- renderUI({
		checkboxInput("checkbox_punctation",
					  label = "Remove punctation",
					  value = TRUE)
	})

	# render lowercase checkbox
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
	# output$statspanel <- renderText({
	# 	if (!isTRUE(input$checkbox_allplays)) {
	# 		"Play statistics <br/>(Single play)"
	# 	} else {
	# 		"Plays statistics <br/>(All plays)"
	# 	}
	# })

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



	# todo: unused
	# 	idee: anteil der stoppwörter an text anzeigen
	output$percentage_stopwords <- renderText({

		lang <- input$corpus

		## only one play ##
		if (!isTRUE(input$checkbox_allplays)) {
			spokentexturl <- paste0(urlcorpora, "/", input$corpus, "/play/", 
									input$selectedplay, "/spoken-text")
			playtext <- getplaytext(spokentexturl)

			playtext

			

		
		## all plays of an author ##
		} else {
			
			playnames <- selectplays(corp(), input$selectedauthor)

			n <- length(playnames)
			allplays <- ""

			for(play in playnames) {
				spokentexturl <- paste0(urlcorpora, "/", input$corpus, "/play/", 
										play, "/spoken-text")
				
				playtext <- getplaytext(spokentexturl)
				allplays <- paste(allplays, playtext, sep="")
			}

			# progressbar for loading plays
			withProgress(message = 'Loading plays', value = 0, {
				
				i <- 0
				for(play in playnames) {
					spokentexturl <- paste0(urlcorpora, "/", input$corpus, "/play/", 
											play, "/spoken-text")
					
					playtext <- getplaytext(spokentexturl)
					allplays <- paste(allplays, playtext, sep="")

					# Increment the progress bar, and update the detail text.
					i <- i + 1
					incProgress(1/n, detail = paste("Play", i, "of", n))

				}

			})

			withProgress(message = 'Generating word cloud', {
				wordcloudplot(allplays, 
							  lang, 
							  remove_punct = input$checkbox_punctation,
							  tolower = input$checkbox_lowercase, 
							  remove_stopwords = input$checkbox_stopwords)
			})
			
		} 
	})

	
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

	# render HTML page with instructions on how to use table
	output$wordfreqtext <- renderUI({
		includeHTML(wordfreqhtml)
	})

	# render word frequency table
	output$wordfreqtable<- DT::renderDataTable({
		
		lang <- input$corpus

		## only one play ##
		if (!isTRUE(input$checkbox_allplays)) {
			spokentexturl <- paste0(urlcorpora, "/", input$corpus, "/play/", 
									input$selectedplay, "/spoken-text")
			playtext <- getplaytext(spokentexturl)

			withProgress(message = 'Counting word frequencies', {
				df <- wordfreqdf(playtext, 
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
		} else {
			
			playnames <- selectplays(corp(), input$selectedauthor)

			n <- length(playnames)
			dflist <- vector("list", n)

			# progressbar for loading plays
			withProgress(message = 'Loading plays', value = 0, {
				
				i <- 1
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
					incProgress(1/n, detail = paste("Play", i, "of", n))
					dflist[[i]] <- df
					i <- i + 1
				}
			})

			# combining all document-term-dataframes into one
			withProgress(message = "Counting document frequencies", {
				df <- Reduce(combine_dfs, dflist)
			})
			
			DT::datatable(df,
						  filter = 'top', 
						  options = list(lengthMenu = c(5, 10, 20, 30, 50, 100),
										 orderClasses = TRUE,
										 pageLength = 10))
		} 
	})



	###############################
	### Render tab 3: Wordcloud ###
	###############################


	# render dynamic tab title
	output$wordcloudtitle <- renderText({
		if (!isTRUE(input$checkbox_allplays)) {
			"Wordcloud of a single play"
		} else {
			if (isTRUE(input$checkbox_wordcloud_comparison)) {
				"Comparison wordcloud of all plays of an author"
			} else {
				"Wordcloud of all plays of an author"
			}
		} 
	})

	# render panel title
	output$wordcloudpaneltitle <- renderText({
		"<h4><u>Wordcloud options</u></h4>"
	})

	# render optional  sub-panel title when "Use all plays" is selected
	output$wordcloudallplayspaneltitle <- renderText({
		"<h4><u>Wordcloud comparison options</u></h4>"
	})
	
	# render HTML page with instructions on how to use the wordclouds
	output$wordcloudtext <- renderUI({
		includeHTML(wordcloudhtml)
	})
	
	# render comparison checkbox
	output$checkbox_wordcloud_comparison <- renderUI({
		checkboxInput("checkbox_wordcloud_comparison",
					  label = span("Compare play wordclouds", 
					  				tags$br(), 
					  				tags$u("Notes:"), 
					  				tags$ul(
						  				tags$li("Only the first 8 plays 
						  						can be compared."),
						  				tags$li("Authors with only one play 
						  						won't plot a comparison wordcloud.")
					  				) 
			  		  ), 
			  		  value = FALSE)
	})

	
	# render one of three wordclouds: single play, all plays, all plays comparison
	output$wordcloudplotter <- renderPlot({

		lang <- input$corpus

		## only one play ##
		if (!isTRUE(input$checkbox_allplays)) {
			spokentexturl <- paste0(urlcorpora, "/", input$corpus, "/play/", 
									input$selectedplay, "/spoken-text")
			playtext <- getplaytext(spokentexturl)

			withProgress(message = 'Generating word cloud', {
				wordcloudplot(playtext, 
							  lang, 
							  remove_punct = input$checkbox_punctation,
							  tolower = input$checkbox_lowercase, 
							  remove_stopwords = input$checkbox_stopwords,
							  color = colors_sequential,
							  max_words = input$wordcloud_max_n_words,
							  min_freq = input$wordcloud_min_freq,
							  word_padding = input$wordcloud_word_padding)
			})

		## all plays of an author ##
		} else {
			playnames <- selectplays(corp(), input$selectedauthor)
			n <- length(playnames)
			allplays <- vector("list", n)

			withProgress(message = 'Loading plays', value = 0, {
				i <- 1
				for(play in playnames) {
					spokentexturl <- paste0(urlcorpora, "/", input$corpus, "/play/", 
											play, "/spoken-text")
					
					playtext <- getplaytext(spokentexturl)
					allplays[[i]] <- playtext

					# Increment the progress bar, and update the detail text.
					incProgress(1/n, detail = paste("Play", i, "of", n))
					i <- i + 1

				}
			})

			if (isTRUE(input$checkbox_wordcloud_comparison) & length(allplays) > 1) {
				names(allplays) <- names(playnames)
				if (length(allplays) > 8) {
					allplays <- allplays[1:8]
				}
				textobj <- corpus(unlist(allplays, use.names=TRUE))
				colors <- colors_qualitative
			} else {
				textobj <- paste(allplays, collapse = " ")
				colors <- colors_sequential2
			}

			if (length(allplays) > 1) {
				withProgress(message = 'Generating word cloud', {
					wordcloudplot(textobj, 
								  lang, 
								  remove_punct = input$checkbox_punctation,
								  tolower = input$checkbox_lowercase, 
								  remove_stopwords = input$checkbox_stopwords,
								  color = colors,
								  comparison = input$checkbox_wordcloud_comparison,
								  labeloffset = input$wordcloud_label_padding,
								  labelsize = input$wordcloud_label_size,
								  max_words = input$wordcloud_max_n_words,
								  min_freq= input$wordcloud_min_freq,
								  word_padding = input$wordcloud_word_padding)
				})
			}
		} 
	}, 
	height=function(){300*input$wordcloud_window_size},
	width=function(){300*input$wordcloud_window_size})


	

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
		#        label="xmllink")
		# TODO: interaktiv mit Worteingabe
		#       siehe   https://shiny.rstudio.com/articles/dynamic-ui.html
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


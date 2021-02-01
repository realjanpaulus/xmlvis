library(curl)
library(docstring, warn.conflicts = FALSE)
library(DT)
library(ggplot2)
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


### global variables ###

about <- "www/about.html"
more <- "www/more.html"
wordcloudhtml <- "www/wordcloudtext.html"
wordfreqhtml <- "www/wordfreqtext.html"


urlcorpus <- "https://dracor.org/api/corpora/ger"
urlcorpora <- "https://dracor.org/api/corpora"
urlcorporashort <- "https://dracor.org/api/corpora/"

# colorblind friendly color palettes
color_point <- "#7080a2"
colors_sequential <- brewer.pal(n = 8, name = 'PRGn')
colors_sequential2 <- rev(brewer.pal(n = 8, name = 'PRGn'))
colors_qualitative <- brewer.pal(n = 6, name = 'Set2')



# ============================== #
# ============= UI ============= #
# ============================== #

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
				conditionalPanel(condition = "input.tabselected != 'reader'",
					uiOutput("textpreprocessingtitle"),
					# todo: das weg?
					conditionalPanel(condition = "input.tabselected != 'kwic' && input.tabselected != 'reader'",
						uiOutput("checkbox_stopwords")
					),	
					uiOutput("checkbox_punctation"),
					uiOutput("checkbox_lowercase")
				),
				tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))
			),
			wellPanel(
				#todo: überarbeiten!! weg?
				# conditionalPanel(condition = "input.tabselected == 'stats'",
				# 	uiOutput("stats_panel_title"),
				# 	uiOutput("stats_checkbox_unique_words")
				# 	# todo
				# ),
				conditionalPanel(condition = "input.tabselected == 'freq' && input.wordfreqtabselected == 'Distribution'",
					uiOutput("wordfreq_panel_title"),
					uiOutput("wordfreq_top_feature_slider")
				),
				# wordcloud panel
				conditionalPanel(condition = "input.tabselected == 'cloud'",
					conditionalPanel(condition = "input.checkbox_allplays == true",
							conditionalPanel(condition = "input.wordcloudtabselected == 'Comparison wordcloud'",
								uiOutput("wordcloud_comparison_panel_title"),
								uiOutput("wordcloud_label_size"),
								uiOutput("wordcloud_label_padding")
							)
						),
					uiOutput("wordcloud_panel_title"),	
					uiOutput("wordcloud_min_freq"),	
					uiOutput("wordcloud_max_n_words"),		
					# EXPAND: min padding could be decreased (e.g. -1)
					#		  for other languages with a non-latin script
					uiOutput("wordcloud_word_padding"),
					uiOutput("wordcloud_window_size")	
				),
				conditionalPanel(condition = "input.tabselected == 'kwic'",
					uiOutput("kwic_panel_title"),
					uiOutput("kwic_checkbox_alphabetical"),
					uiOutput("kwic_select_token")
				)
				
			)
		),
		mainPanel(
			tags$style(type="text/css",
				".shiny-output-error { visibility: hidden; }",
				".shiny-output-error:before { visibility: hidden; }",
				"#reader_text {
					border-style: groove;
					background-color: #f5f5f5;
					padding-top: 5px;
	                width: 90%;
	                height: 700px;
	                overflow: scroll;
                }"
			),
			tabsetPanel(
				id="tabselected",
				tabPanel(title = "Statistics", 
							titlePanel(uiOutput("stats_title")),
							fluidRow(
								column(width=6, 
									   style="padding-top: 10px;",
									   DT::dataTableOutput(outputId = "stats_table")
									   )
							    ),
							    #TODO!!!! html machen
								column(width=6, 
									   style="padding-top: 0px; padding-right: 22px;",
									   uiOutput("")
							),							
						 	value = "stats"),
				tabPanel(title = "Reader",
						 titlePanel(uiOutput("reader_title")),
						 uiOutput("reader_text"),
						 value = "reader"),
				tabPanel(title = "Word frequencies", 
						 titlePanel(uiOutput("wordfreqtitle")),
						 fluidRow(
							column(width=6, 
								   style="padding-top: 10px;",
								   tabsetPanel(
								   		id="wordfreqtabselected",
								   		tabPanel(title = "Table",
								   				 br(),
								   				 DT::dataTableOutput(outputId = "wordfreqtable")
								   				),
								   		tabPanel(title = "Distribution",
								   				 br(),
								   				 plotOutput(outputId = "wordfreqdistribution")
								   				 )
								   )
						    ),
							column(width=6, 
								   style="padding-top: 0px; padding-right: 22px;",
								   uiOutput("wordfreqtext")) 
						 ),
						 value="freq"),
				tabPanel(title = "Wordcloud", 
						 titlePanel(uiOutput("wordcloud_title")),
						 fluidRow(
							column(width=6,
								   align="center",
								   style="padding-top: 10px;",
								   conditionalPanel(condition = "input.checkbox_allplays == true",
										tabsetPanel(
											id="wordcloudtabselected",
											tabPanel(title = "Simple wordcloud",
													 br()),
											tabPanel(title = "Comparison wordcloud",
													 br())
										)
								   ),
								   plotOutput(outputId = "wordcloud_plotter",
								   			  inline = TRUE)
							),
							column(width=6,
								   style="padding-top: 0px; padding-right: 22px;",
								   uiOutput("wordcloud_text")) 
						 ),
						 value="cloud"),
				tabPanel(title = "KWIC",
						titlePanel(uiOutput("kwic_title")),
						fluidRow(
							column(width=8,
								   align="center",
								   style="padding-top: 10px;",
								   DT::dataTableOutput(outputId = "kwic_table")
							),
							column(width=4,
								   style="padding-top: 0px; padding-right: 22px;",
								   uiOutput("")) #todo: andere html!!!
						),
						value="kwic"),
				tabPanel(title = "About", includeHTML(about), value="about"),
				tabPanel(title = "More", includeHTML(more), value="more")
			)
		)
	)

)

# ============================== #
# =========== SERVER =========== #
# ============================== #

server <- function(input, output) {

	#####################
	### Sidebar panel ###
	#####################
	
	# reactive corpus list with all informations of a corpus,
	# taken from json file, which was identified by its input `lang`
	corp <- reactive({
		lang <- input$selectedcorpus
		if(is.null(lang)) return(NULL)
		extractdramas(paste0(urlcorporashort, lang))
	})

	# render corpus selection
	output$select_corpora <- renderUI({
		selectInput("selectedcorpus",
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


	###########################################
	### Render tab: Plays/corpus statistics ###
	###########################################

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



	# render dynamic tab title
	output$stats_title <- renderText({
		if (!isTRUE(input$checkbox_allplays)) {
			"Statistics of a play"
		} else {
			"Statistics of all plays of the selected author"
		} 
	})


	# render statistics panel title
	#todo weg?
	# output$stats_panel_title <- renderText({
	# 	"<h4><u>Statistics options</u></h4>"
	# })


	# todo: weg
	# output$stats_checkbox_unique_words <- renderUI({
	# 	checkboxInput("stats_checkbox_unique_words",
	# 				  label = "Use only unique words",
	# 				  value = FALSE)
	# })


	# render statistics page
	# todo
	output$stats_table<- DT::renderDataTable({
		lang <- input$selectedcorpus

		## only one play ##
		if(!isTRUE(input$checkbox_allplays)) {
			textobj <- get_text(corpusobj = input$selectedcorpus,
									url = urlcorpora,
									play_name = input$selectedplay)

		## all plays of an author ##
		} else {
			playnames <- selectplays(corp(), input$selectedauthor)

			n <- length(playnames)
			allplays <- vector("list", n)

			withProgress(message = 'Loading plays', value = 0, {
				i <- 1
				for(play in playnames) {
					playtext <- get_text(corpusobj = input$selectedcorpus,
											url = urlcorpora,
											play_name = play)
					allplays[[i]] <- playtext

					# Increment the progress bar, and update the detail text.
					incProgress(1/n, detail = paste("Play", i, "of", n))
					i <- i + 1

				}
			})

			textobj <- paste(allplays, sep = "", collapse = "")

		}

		df <- statistics_df(textobj,
							lang,
							all_plays = isTRUE(input$checkbox_allplays),
							remove_punct = input$checkbox_punctation,
							tolower = input$checkbox_lowercase, 
							remove_stopwords = input$checkbox_stopwords,
							unique_words = input$stats_checkbox_unique_words)
		
		withProgress(message = 'Loading plays', value = 0, {
			DT::datatable(df, 
							options = list(dom = 't', ordering=FALSE), 
							escape = FALSE, 
							selection = "none")
		})
	})





	###############################
	### Render tab: Play Reader ###
	###############################

	# render dynamic tab title
	output$reader_title <- renderText({
		if (!isTRUE(input$checkbox_allplays)) {
			"Simple play reader"
		} else {
			"Simple plays reader"
		} 
		
	})

	output$reader_text <- renderUI({
		lang <- input$selectedcorpus

		## only one play ##
		if (!isTRUE(input$checkbox_allplays)) {
			textobj <- get_text(corpusobj = input$selectedcorpus,
									url = urlcorpora,
									play_name = input$selectedplay)

		## all plays of an author ##
		} else {

			playnames <- selectplays(corp(), input$selectedauthor)
			n <- length(playnames)
			allplays <- vector("list", n)

			withProgress(message = 'Loading plays', value = 0, {
				i <- 1
				for(play in playnames) {
					playtext <- get_text(corpusobj = input$selectedcorpus,
											url = urlcorpora,
											play_name = play)

					playtitle <- names(playnames)[[i]]
					allplays[[i]] <- paste0("<br><h2>", playtitle, "</h2><br>", playtext)

					# Increment the progress bar, and update the detail text.
					incProgress(1/n, detail = paste("Play", i, "of", n))
					i <- i + 1

				}
			})

			textobj <- paste(allplays, sep = "", collapse = "")

		} 

		#todo
		#HTML(textobj)
		HTML(gsub("\n", "<br>", textobj))
		#textobj


	})

	
	##########################################
	### Render tab: Word Frequencies Table ###
	##########################################

	# render dynamic tab title
	output$wordfreqtitle <- renderText({
		if (!isTRUE(input$checkbox_allplays)) {
			"Word frequencies of a single play"
		} else {
			"Word frequencies of all plays of an author"
		} 
	})

	# render wordfreq panel title
	output$wordfreq_panel_title <- renderText({
		"<h4><u>Word frequencies options</u></h4>"
	})

	# render wordfreq top n features slider
	output$wordfreq_top_feature_slider <- renderUI({
		sliderInput("wordfreq_top_feature_slider",
					"Top n frequent features:",
					min = 1,
					max = 100, #EXPAND: for more n features
					step = 1,
					value = 20)
	})


	# render HTML page with instructions on how to use table
	output$wordfreqtext <- renderUI({
		includeHTML(wordfreqhtml)
	})

	# render word frequency table
	output$wordfreqtable <- DT::renderDataTable({
		
		lang <- input$selectedcorpus

		## only one play ##
		if (!isTRUE(input$checkbox_allplays)) {

			playtext <- get_text(corpusobj = input$selectedcorpus,
									url = urlcorpora,
									play_name = input$selectedplay)



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
										 pageLength = 10),
						  selection = "none")
		
		## all plays of an author ##
		} else {
			
			playnames <- selectplays(corp(), input$selectedauthor)

			n <- length(playnames)
			dflist <- vector("list", n)

			withProgress(message = 'Loading plays', value = 0, {
				
				i <- 1
				for(play in playnames) {

					playtext <- get_text(corpusobj = input$selectedcorpus,
											url = urlcorpora,
											play_name = play)
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

	# render word frequency distribution 
	output$wordfreqdistribution <- renderPlot({
		lang <- input$selectedcorpus

		## only one play ##
		if (!isTRUE(input$checkbox_allplays)) {
			playtext <- get_text(corpusobj = input$selectedcorpus,
									url = urlcorpora,
									play_name = input$selectedplay)

			withProgress(message = 'Counting word frequencies', {
				frequency_distribution(playtext, 
									   lang, 
									   remove_punct = input$checkbox_punctation,
									   tolower = input$checkbox_lowercase, 
									   remove_stopwords = input$checkbox_stopwords,
									   top_n_features = input$wordfreq_top_feature_slider,
									   color_point = color_point)
			})

		## all plays of an author ##
		} else {
			
			playnames <- selectplays(corp(), input$selectedauthor)

			n <- length(playnames)
			allplays <- vector("list", n)

			withProgress(message = 'Loading plays', value = 0, {
				i <- 1
				for(play in playnames) {
					playtext <- get_text(corpusobj = input$selectedcorpus,
											url = urlcorpora,
											play_name = play)
					allplays[[i]] <- playtext

					# Increment the progress bar, and update the detail text.
					incProgress(1/n, detail = paste("Play", i, "of", n))
					i <- i + 1

				}
			})

			textobj <- paste(allplays, sep = "", collapse = "")

			withProgress(message = 'Counting word frequencies', {
				frequency_distribution(textobj, 
									   lang, 
									   remove_punct = input$checkbox_punctation,
									   tolower = input$checkbox_lowercase, 
									   remove_stopwords = input$checkbox_stopwords,
									   top_n_features = input$wordfreq_top_feature_slider,
									   color_point = color_point) 
			})
		} 
	})



	#############################
	### Render tab: Wordcloud ###
	#############################


	# render dynamic tab title
	output$wordcloud_title <- renderText({
		if (!isTRUE(input$checkbox_allplays)) {
			"Wordcloud of a single play"
		} else {
			if (isTRUE(input$wordcloudtabselected == "Comparison wordcloud")) {
				"Comparison wordcloud of all plays of an author"
			} else {
				"Wordcloud of all plays of an author"
			}
		} 
	})

	# render wordcloud label size panel entry
	output$wordcloud_label_size <- renderUI({
		sliderInput("wordcloud_label_size",
					"Size of the play names:",
					min = 1,
					max = 4,
					step = 0.1,
					value = 2)
	})

	# render word cloud label padding panel entry
	output$wordcloud_label_padding <- renderUI({
		sliderInput("wordcloud_label_padding",
					"Padding of the play names:",
					min = -0.2,
					max = 0.2,
					step = 0.01,
					value = -0.1)
	})

	# render wordcloud min frequency panel entry
	output$wordcloud_min_freq <- renderUI({
		sliderInput("wordcloud_min_freq", 
					"Minimum frequency:",
					min = 1,  
					max = 200,
					step = 1, 
					value = 20)
	})

	# render wordcloud max n words panel entry
	output$wordcloud_max_n_words <- renderUI({
		sliderInput("wordcloud_max_n_words",
					"Maximum number of words:",
					min = 1,
					max = 300,
					step = 1,
					value = 50)
	})

	# render wordcloud word padding panel entry
	output$wordcloud_word_padding <- renderUI({
		sliderInput("wordcloud_word_padding",
					"Word padding:",
					min = 0,
					max = 1,
					step = 0.1,
					value = 0.1)
	})

	# render wordcloud window size panel entry
	output$wordcloud_window_size <- renderUI({
		sliderInput("wordcloud_window_size",
					"Size:",
					min = 1,
					max = 2.2,
					step = 0.1,
					value = 1.8)
	})



	# render panel title
	output$wordcloud_panel_title <- renderText({
		"<h4><u>Wordcloud options</u></h4>"
	})

	# render optional sub-panel title when "Use all plays" is selected
	output$wordcloud_comparison_panel_title <- renderText({
		"<h4><u>Wordcloud comparison options</u></h4>"
	})
	
	# render HTML page with instructions on how to use the wordclouds
	output$wordcloud_text <- renderUI({
		includeHTML(wordcloudhtml)
	})
	
	
	# render one of three wordclouds: single play, all plays, all plays comparison
	output$wordcloud_plotter <- renderPlot({

		lang <- input$selectedcorpus

		## only one play ##
		if (!isTRUE(input$checkbox_allplays)) {
			playtext <- get_text(corpusobj = input$selectedcorpus,
									url = urlcorpora,
									play_name = input$selectedplay)
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
					playtext <- get_text(corpusobj = input$selectedcorpus,
											url = urlcorpora,
											play_name = play)
					allplays[[i]] <- playtext

					# Increment the progress bar, and update the detail text.
					incProgress(1/n, detail = paste("Play", i, "of", n))
					i <- i + 1

				}
			})

			if (isTRUE(input$wordcloudtabselected == "Comparison wordcloud") & length(allplays) > 1) {
				names(allplays) <- names(playnames)
				if (length(allplays) > 8) {
					allplays <- allplays[1:8]
				}
				textobj <- corpus(unlist(allplays, use.names=TRUE))
				colors <- colors_qualitative
				comparison_check = TRUE
			} else {
				textobj <- paste(allplays, sep = "", collapse = "")
				colors <- colors_sequential2
				comparison_check = FALSE
			}

			
			withProgress(message = 'Generating word cloud', {
				wordcloudplot(textobj, 
							  lang, 
							  remove_punct = input$checkbox_punctation,
							  tolower = input$checkbox_lowercase, 
							  remove_stopwords = input$checkbox_stopwords,
							  color = colors,
							  comparison = comparison_check,
							  labeloffset = input$wordcloud_label_padding,
							  labelsize = input$wordcloud_label_size,
							  max_words = input$wordcloud_max_n_words,
							  min_freq= input$wordcloud_min_freq,
							  word_padding = input$wordcloud_word_padding)
			})
			
		} 
	}, 
	height=function(){300*input$wordcloud_window_size},
	width=function(){300*input$wordcloud_window_size})


	########################
	### Render tab: KWIC ###
	########################

	# render kwic title
	output$kwic_title <- renderText({
		"KWIC (= Keywords in context)"
	})


	# TODO: panel
	# - optionales filtering
	# - auswahl von allen tokens
	# 	- alphabetisch
	# 	- häufigste wörter
	# - stopword removal und so weiter
	# - docname anders

	# render panel title
	output$kwic_panel_title <- renderText({
		"<h4><u>KWIC options</u></h4>"
	})

	output$kwic_checkbox_alphabetical <- renderUI({
		checkboxInput("kwic_checkbox_alphabetical",
					  label = "Use alphabetical list",
					  value = FALSE)
	})


	# TODO!
	# -  todo: standard wort selecten, in utils.R
	output$kwic_select_token <- renderUI({

		lang <- input$selectedcorpus

		if (!isTRUE(input$checkbox_allplays)) {
			textobj <- get_text(corpusobj = input$selectedcorpus,
								url = urlcorpora,
								play_name = input$selectedplay)
		} else {
			playnames <- selectplays(corp(), input$selectedauthor)
			n <- length(playnames)
			allplays <- vector("list", n)

			
			for(play in playnames) {
				playtext <- get_text(corpusobj = input$selectedcorpus,
										url = urlcorpora,
										play_name = play)
				allplays[[i]] <- playtext
			}

			textobj <- paste(allplays, sep = "", collapse = "")
		}



		# TODO: stopwords klappen jetzt auch!!!
		if(isTRUE(input$kwic_checkbox_alphabetical)) {
			token_list <- get_tokens(textobj,
									lang,
									remove_punct = input$checkbox_punctation,
									tolower = input$checkbox_lowercase,
									sort_by = "alphabetical")
		} else {
			token_list <- get_tokens(textobj,
									lang,
									remove_punct = input$checkbox_punctation,
									tolower = input$checkbox_lowercase,
									sort_by = "frequency")
		}

		selectInput("kwic_select_token",
					label = "Select a word:",
					choices = token_list)
	})

	

	# render kwic table
	output$kwic_table <- DT::renderDataTable({

		lang <- input$selectedcorpus

		## only one play ##
		if (!isTRUE(input$checkbox_allplays)) {
			playtext <- get_text(corpusobj = input$selectedcorpus,
									url = urlcorpora,
									play_name = input$selectedplay)


			# todo: anderes pattern
			withProgress(message = 'Counting word frequencies', {
				df <- kwic_df(playtext, 
								lang,
								pattern = input$kwic_select_token,
								remove_punct = input$checkbox_punctation,
								tolower = input$checkbox_lowercase)
			})

			#todo: mehr optionen
			DT::datatable(df, filter = "top",
						options = list(lengthMenu = c(5, 10, 20, 30, 50, 100),
									   pageLength = 5))

		## all plays of an author ##
		} else {
			
			playnames <- selectplays(corp(), input$selectedauthor)
			n <- length(playnames)
			allplays <- vector("list", n)

			withProgress(message = 'Loading plays', value = 0, {
				i <- 1
				for(play in playnames) {
					playtext <- get_text(corpusobj = input$selectedcorpus,
											url = urlcorpora,
											play_name = play)
					allplays[[i]] <- playtext

					# Increment the progress bar, and update the detail text.
					incProgress(1/n, detail = paste("Play", i, "of", n))
					i <- i + 1

				}
			})

			if (length(allplays) > 1) {
				names(allplays) <- names(playnames)

				textobj <- corpus(unlist(allplays, use.names=TRUE))
			} else {
				textobj <- paste(allplays, sep = "", collapse = "")
			}

			# todo: anderes pattern
			withProgress(message = 'Counting word frequencies', {
				df <- kwic_df(textobj, 
								lang,
								pattern = input$kwic_select_token,
								remove_punct = input$checkbox_punctation,
								tolower = input$checkbox_lowercase)
			})
			DT::datatable(df, filter = "top",
							options = list(lengthMenu = c(5, 10, 20, 30, 50, 100),
							pageLength = 5))
		} 
	})


}

# Run the app
options(shiny.port = 3000)
options(shiny.autoreload = TRUE)
options(shiny.launch.browser = TRUE)
options(warn=1)
shinyApp(ui = ui, server = server)


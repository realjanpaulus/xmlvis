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
library(shinyWidgets)
library(XML)
source("utils.R")


### global variables ###

about <- "www/about.html"
kwichtml <- "www/kwictext.html"
more <- "www/more.html"
statisticshtml <- "www/statistics.html"
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
				
				conditionalPanel(condition = "input.statstabselected != 'stats_tab_playlength' && 
												input.statstabselected != 'stats_tab_ld' && 
												input.statstabselected != 'stats_tab_tr'",
					# only renders play selection if the "Use all plays" checkbox 
					# is not activated
					conditionalPanel(condition = "input.checkbox_allplays == false",
						uiOutput("select_plays")
					),
					uiOutput("checkbox_allplays")
				),
				conditionalPanel(condition = "input.statstabselected == 'stats_tab_playlength' || 
												input.statstabselected == 'stats_tab_ld' ||
												input.statstabselected == 'stats_tab_tr'",
			
					uiOutput("substitute_text_allplays")
				),
				conditionalPanel(condition = "input.tabselected != 'reader'",
					uiOutput("textpreprocessing_title"),	
					uiOutput("checkbox_stopwords"),
					uiOutput("checkbox_punctation"),
					uiOutput("checkbox_lowercase")
				),
				tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))
			),
			wellPanel(
				# statistics panel
				conditionalPanel(condition = "input.tabselected == 'stats'",
					uiOutput("stats_panel_title"),
					uiOutput("stats_select_lexical_diversity"),
					uiOutput("stats_select_readability")
				),
				# wordfrequency distribution panel
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
				# kwic panel
				conditionalPanel(condition = "input.tabselected == 'kwic'",
					uiOutput("kwic_panel_title"),
					uiOutput("kwic_window_slider"),
					uiOutput("kwic_radio_sorting"),
					uiOutput("kwic_select_token")
				)
			)
		),
		mainPanel(
			# some styling
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
							fluidRow(
								column(width=6, 
										style="padding-top: 0px;",
										titlePanel(uiOutput("stats_title")),
										br(),
										tabsetPanel(id="statstabselected",
											tabPanel(title = "Overview table", 
													value = "stats_tab_table",
													br(),
													DT::dataTableOutput(outputId = "stats_table")),
								   			tabPanel(title = "Play lengths comparison",
								   					value = "stats_tab_playlength",
									   				br(),
									   				plotOutput(outputId = "stats_plot_playlength")),									   		
									   		tabPanel(title = "Lexical diversity plot",
									   				value = "stats_tab_ld",
									   			br(),
									   			plotOutput(outputId = "stats_plot_ld")),
									   		tabPanel(title = "Text readability plot",
									   				value = "stats_tab_tr",
									   				br(),
									   				plotOutput(outputId = "stats_plot_tr"))   	
										)
							    	),
								column(width=6, 
									   style="padding-top: 5px; padding-right: 22px;",
									   uiOutput("stats_text"))
							),							
						 	value = "stats"),
				tabPanel(title = "Reader",
						 titlePanel(uiOutput("reader_title")),
						 br(),
						 uiOutput("reader_text"),
						 value = "reader"),
				tabPanel(title = "Word frequencies", 
						fluidRow(
							column(width=6, 
								   style="padding-top: 0px;",
								   titlePanel(uiOutput("wordfreqtitle")),
								   br(),
								   tabsetPanel(id="wordfreqtabselected",
								   		tabPanel(title = "Table",
								   				br(),
								   				DT::dataTableOutput(outputId = "wordfreq_table")
								   				),
								   		tabPanel(title = "Distribution",
								   				 br(),
								   				 plotOutput(outputId = "wordfreq_distribution")
								   				 )
								   )
						    ),
							column(width=6, 
								   style="padding-top: 5px; padding-right: 22px;",
								   uiOutput("wordfreq_text")) 
						 ),
						 value="freq"),
				tabPanel(title = "Wordcloud", 
						 fluidRow(
							column(width=6,
								   align="left",
								   style="padding-top: 0px;",
								   titlePanel(uiOutput("wordcloud_title")),
								   br(),
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
								   style="padding-top: 5px; padding-right: 22px;",
								   uiOutput("wordcloud_text")) 
						 ),
						 value="cloud"),
				tabPanel(title = "KWIC",
						fluidRow(
							column(width=6,
								   align="left",
								   style="padding-top: 10px;",
								   titlePanel(uiOutput("kwic_title")),
								   br(),
								   DT::dataTableOutput(outputId = "kwic_table")
							),
							column(width=6,
								   style="padding-top: 5px; padding-right: 22px;",
								   uiOutput("kwic_text"))
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

	# render all plays substitute text
	output$substitute_text_allplays <- renderText({
		"<h5><i><u>Note</u>: Using all plays by default</i></h5>"
	})

	##################################
	### Text preprocessing options ###
	##################################

	output$textpreprocessing_title <- renderText({
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


	######################################
	### Render tab: Play(s) statistics ###
	#######################################

	
	# render dynamic tab title
	output$stats_title <- renderText({
		if (!isTRUE(input$checkbox_allplays)) {
			"Statistics of a play"
		} else {
			"Statistics of all plays of the selected author"
		} 
	})

	# render HTML page with instructions on how to use stats table
	output$stats_text <- renderUI({
		includeHTML(statisticshtml)
	})

	# render statistics panel title
	output$stats_panel_title <- renderText({
		"<h4><u>Statistics options</u></h4>"
	})

	# render lexical diversity measure selection
	output$stats_select_lexical_diversity <- renderUI({
		if (is.null(corp() )) return(NULL)
		selectInput("stats_select_lexical_diversity",
					label = "Measure for the lexical diversity:",
					choices = c("TTR: Token-Type Ratio", 
								"C: Herdan's C", 
								"R: Root TTR", 
								"CTTR: Corrected TTR", 
								"U: Uber Index", 
								"S: Summers Index", 
								"K: Yule's K", 
								"I: Yule's I", 
								"D: Simpson's D", 
								"Vm: Herdan's Vm", 
								"Maas: Maas indices", 
								"MATTR: Moving-Average Type-Token Ratio", 
								"MSTTR: Mean Segmental Type-Token Ratio"))
	})

	# render readability measure selection
	output$stats_select_readability <- renderUI({
		if (is.null(corp() )) return(NULL)
		selectInput("stats_select_readability",
					label = "Measure for the text readability:",
					choices = c("ARI", "ARI.simple", "Bormuth.MC", "Bormuth.GP", "Coleman", "Coleman.C2",
							"Coleman.Liau.ECP", "Coleman.Liau.grade", "Coleman.Liau.short", 
							"Dale.Chall", "Dale.Chall.Old", "Dale.Chall.PSK", "Danielson.Bryan", 
							"Danielson.Bryan.2", "Dickes.Steiwer", "DRP", "ELF", "Farr.Jenkins.Paterson",
							"Flesch", "Flesch.PSK", "Flesch.Kincaid", "FOG", "FOG.PSK", "FOG.NRI",
							"FORCAST", "FORCAST.RGL", "Fucks", "Linsear.Write", "LIW", "nWS", "nWS.2",
							"nWS.3", "nWS.4", "RIX", "Scrabble", "SMOG", "SMOG.C", "SMOG.simple",
							"SMOG.de", "Spache", "Spache.old", "Strain", "Traenkle.Bailer", "Traenkle.Bailer.2",
							"Wheeler.Smith", "meanSentenceLength", "meanWordSyllables"))
	})
	
	# render statistics table
	output$stats_table <- DT::renderDataTable({
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
							lexical_diversity_measure = input$stats_select_lexical_diversity,
							readability_measure = input$stats_select_readability)
		
		withProgress(message = 'Loading plays', value = 0, {
			DT::datatable(df, 
							options = list(dom = 't', ordering=FALSE), 
							escape = FALSE, 
							selection = "none")
		})
	})

	# render play length plot
	output$stats_plot_playlength <- renderPlot({

	})

	# render lexical diversity plot
	output$stats_plot_ld <- renderPlot({

		lang <- input$selectedcorpus
		ld_measure <- get_lexical_diversity_name(input$stats_select_lexical_diversity)

		playnames <- selectplays(corp(), input$selectedauthor)
		n <- length(playnames)
		allplays <- list()

		withProgress(message = 'Loading plays', value = 0, {
			i <- 1
			for(play in playnames) {
				playtext <- get_text(corpusobj = input$selectedcorpus,
										url = urlcorpora,
										play_name = play)
				playtokens <- get_tokens(playtext, lang,
										remove_punct = input$checkbox_punctation,
										tolower = input$checkbox_lowercase, 
										remove_stopwords = input$checkbox_stopwords)
				playname <- names(playnames)[[i]]
				allplays[[playname]] <- textstat_lexdiv(playtokens, measure = ld_measure)[[2]]
				
				# Increment the progress bar, and update the detail text.
					incProgress(1/n, detail = paste("Play", i, "of", n))
					i <- i + 1

			}
		})
	
		lexical_diversity_distribution(allplays, ld_measure)
	})

	# render text readability plot
	output$stats_plot_tr <- renderPlot({

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

	# render reader text
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

		HTML(gsub("\n", "<br>", textobj))
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
	output$wordfreq_text <- renderUI({
		includeHTML(wordfreqhtml)
	})

	# render word frequency table
	output$wordfreq_table <- DT::renderDataTable({
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
	output$wordfreq_distribution <- renderPlot({
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

	# render HTML page with instructions on how to use KWIC
	output$kwic_text <- renderUI({
		includeHTML(kwichtml)
	})


	# render panel title
	output$kwic_panel_title <- renderText({
		"<h4><u>KWIC options</u></h4>"
	})


	# render kwic sorting radio buttons
	output$kwic_radio_sorting <- renderUI({
		radioButtons("kwic_radio_sorting",
					  label = "Select sorting method:",
					  choices = c("alphabetical", "most frequent words"),
					  selected =  "most frequent words")
	})

	# render kwic word window slider
	output$kwic_window_slider <- renderUI({
		sliderInput("kwic_window_slider",
					"Number of words before/after keyword",
					min = 1,
					max = 20,
					step = 1,
					value = 5)
	})

	# render wordcloud window size panel entry
	# EXPAND: higher max slider?
	output$wordcloud_window_size <- renderUI({
		sliderInput("wordcloud_window_size",
					"Size:",
					min = 1,
					max = 2.2,
					step = 0.1,
					value = 1.8)
	})

	# render token selection
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

			i <- 1
			for(play in playnames) {
				playtext <- get_text(corpusobj = input$selectedcorpus,
										url = urlcorpora,
										play_name = play)
				allplays[[i]] <- playtext
			}

			textobj <- paste(allplays, sep = "", collapse = "")
		}


		# select sorting method ("alphabetical" or "frequency")
		if(input$kwic_radio_sorting == "alphabetical") {
			token_list <- get_tokens(textobj,
									lang,
									tolower = input$checkbox_lowercase,
									remove_punct = input$checkbox_punctation,
									remove_stopwords = input$checkbox_stopwords,
									sort_by = "alphabetical")
		} else if (input$kwic_radio_sorting == "most frequent words") {
			token_list <- get_tokens(textobj,
									lang,
									tolower = input$checkbox_lowercase,
									remove_punct = input$checkbox_punctation,
									remove_stopwords = input$checkbox_stopwords,
									sort_by = "mfw+")


		}

		selectizeInput("kwic_select_token",
					label = "Select a keyword:",
					choices = token_list,
					# EXPAND: for plays/combined plays with a higher token count
					options = list(maxOptions = 500000), 
					selected = "")
	})

	

	# render kwic table
	output$kwic_table <- DT::renderDataTable({

		lang <- input$selectedcorpus
		textobj <- ""
		docname <- ""

		## only one play ##
		if (!isTRUE(input$checkbox_allplays)) {
			textobj <- get_text(corpusobj = input$selectedcorpus,
									url = urlcorpora,
									play_name = input$selectedplay)
			playnames <- selectplays(corp(), input$selectedauthor)
			docname <- names(playnames)[playnames == input$selectedplay]
			

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
				docname <- names(playnames)[[1]]
			}
		} 

		withProgress(message = 'Counting word frequencies', {
			df <- kwic_df(textobj, 
							lang,
							pattern = sub(" *\\(.*", "", input$kwic_select_token),
							window = input$kwic_window_slider,
							docname = docname,
							tolower = input$checkbox_lowercase,
							remove_punct = input$checkbox_punctation)
		})

		DT::datatable(df, filter = "top",
					options = list(lengthMenu = c(5, 10, 20, 30, 50, 100),
								   pageLength = 5))
	})


}

# Run the app
options(shiny.port = 3000)
options(shiny.autoreload = TRUE)
options(shiny.launch.browser = TRUE)
options(warn=1)
shinyApp(ui = ui, server = server)


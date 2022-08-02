# library("curl")
library("docstring", warn.conflicts = FALSE)
library("DT")
library("ggplot2")
library("jsonlite", warn.conflicts = FALSE)
library("httr", warn.conflicts = FALSE)
library("methods")
library("plyr")
library("RColorBrewer")
library("shiny", warn.conflicts = FALSE)
library("shinycssloaders")
library("shinythemes")
library("shinyWidgets")
source("utils.R")


### global variables ###

about <- "www/about.html"
kwichtml <- "www/kwictext.html"
statisticshtml <- "www/statistics_overviewtable.html"
trendshtml <- "www/trendstext.html"
wordcloudhtml <- "www/wordcloudtext_simple.html"
wordfreqhtml <- "www/wordfreqtext_table.html"


urlreadercorpus <- "https://dracor.org/ger"
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
				conditionalPanel(condition = "(input.tabselected == 'reader' &&
												input.reader_radio_text_representation == 'Raw text') ||
												input.tabselected == 'freq' ||
												input.tabselected == 'trends' ||
												input.tabselected == 'kwic' ||
												input.tabselected == 'about' ||
												(input.tabselected == 'stats' &&
													input.statstabselected == 'stats_tab_table') ||
												(input.tabselected == 'cloud' &&
													input.wordcloudtabselected == 'wordcloud_tab_simple')",

					# multiple play selection or single play
					conditionalPanel(condition = "input.checkbox_multiple_plays == true",
						uiOutput("select_multiple_plays")
					),
					conditionalPanel(condition = "input.checkbox_multiple_plays == false",
						uiOutput("select_plays")
					),
					uiOutput("checkbox_multiple_plays")
				),
				conditionalPanel(condition = "(input.tabselected == 'stats' &&
													input.statstabselected == 'stats_tab_playlength') ||
												(input.tabselected == 'stats' &&
													input.statstabselected == 'stats_tab_ld') ||
												(input.tabselected == 'stats' &&
													input.statstabselected == 'stats_tab_tr') ||
												(input.tabselected == 'cloud' &&
			 										input.wordcloudtabselected == 'wordcloud_tab_comparison')",
			 		conditionalPanel(condition = "(input.tabselected == 'stats' &&
													input.statstabselected == 'stats_tab_playlength') ||
												(input.tabselected == 'stats' &&
													input.statstabselected == 'stats_tab_ld') ||
												(input.tabselected == 'stats' &&
													input.statstabselected == 'stats_tab_tr')",
			 			uiOutput("select_multiple_plays_special")
			 		),
			 		conditionalPanel(condition = "input.tabselected == 'cloud' &&
			 										input.wordcloudtabselected == 'wordcloud_tab_comparison'",
						uiOutput("select_wordcloud_comparison_plays")
					),
					uiOutput("substitute_text_multiple_plays")
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
					uiOutput("stats_select_text_readability"),
					conditionalPanel(condition = "input.statstabselected == 'stats_tab_playlength' ||
													input.statstabselected == 'stats_tab_ld' ||
													input.statstabselected == 'stats_tab_tr'",
								uiOutput("stats_plot_options_title"),
								uiOutput("stats_plot_vis_type_radio"),
								conditionalPanel(condition = "input.statstabselected == 'stats_tab_playlength'",
									uiOutput("stats_plot_tokentype_radio")
								),
								uiOutput("stats_checkbox_plot_title_linebreak"),
								uiOutput("stats_plot_title_size_slider")
					)
				),
				# reader panel
				conditionalPanel(condition = "input.tabselected == 'reader'",
					uiOutput("reader_radio_text_representation")
				),
				# wordfrequency panel
				conditionalPanel(condition = "input.tabselected == 'freq'",
					uiOutput("wordfreq_panel_title"),
					uiOutput("wordfreq_frequency_radio"),
					# wordfrequency distribution panel
					conditionalPanel(condition = "input.tabselected == 'freq'
												&& input.wordfreqtabselected == 'Distribution'",
						uiOutput("wordfreq_checkbox_plot_revert"),
						uiOutput("wordfreq_top_feature_slider"),
						uiOutput("wordfreq_checkbox_plot_title_linebreak"),
						uiOutput("wordfreq_plot_title_size_slider"),
					)

				),
				# wordcloud panel
				conditionalPanel(condition = "input.tabselected == 'cloud'",
					conditionalPanel(condition = "input.wordcloudtabselected == 'wordcloud_tab_comparison'",
						uiOutput("wordcloud_comparison_panel_title"),
						uiOutput("wordcloud_label_size"),
						uiOutput("wordcloud_label_padding"),
						br()

					),
					uiOutput("wordcloud_panel_title"),
					uiOutput("wordcloud_min_freq"),
					uiOutput("wordcloud_max_n_words"),
					# EXPAND: min padding could be decreased (e.g. -1)
					#		  for other languages with a non-latin script
					uiOutput("wordcloud_word_padding"),
					uiOutput("wordcloud_window_size")
				),
				# trends panel
				conditionalPanel(condition = "input.tabselected == 'trends'",
					uiOutput("trends_panel_title"),
					uiOutput("trends_n_segments"),
					uiOutput("trends_frequency_radio"),
					uiOutput("trends_radio_sorting"),
					uiOutput("trends_select_token"),
					uiOutput("trends_checkbox_plot_title_linebreak"),
					uiOutput("trends_plot_title_size_slider")
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
									tabsetPanel(
										id="wordcloudtabselected",
										tabPanel(title = "Simple wordcloud",
												value = "wordcloud_tab_simple",
												br(),
												plotOutput(outputId = "wordcloud_plotter",
															inline = TRUE)
										),
										tabPanel(title = "Comparison wordcloud",
												value = "wordcloud_tab_comparison",
												br(),
												plotOutput(outputId = "wordcloud_comparison_plotter",
															inline = TRUE)
										)
									),
							),
							column(width=6,
								   style="padding-top: 5px; padding-right: 22px;",
								   uiOutput("wordcloud_text"))
						 ),
						 value="cloud"),
				tabPanel(title = "Trends",
						fluidRow(
							column(width = 6,
									align="left",
									style="padding-top: 10px;",
									titlePanel(uiOutput("trends_title")),
									br(),
									plotOutput(outputId = "trends_plotter")),
							column(width=6,
								   style="padding-top: 5px; padding-right: 22px;",
								   uiOutput("trends_text"))
						),
						value="trends"),
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
				tabPanel(title = "About", includeHTML(about), value="about")
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
		lang <- input$select_corpora
		if(is.null(lang)) return(NULL)
		extractdramas(paste0(urlcorporashort, lang))
	})

	# render corpus selection
	output$select_corpora <- renderUI({
		selectInput("select_corpora",
					label = "Choose a corpus:",
					choices = selectcorpora(urlcorpus))
					# EXPAND: change to `urlcorpora` for all corpora
	})

	# render author selection
	output$select_authors <- renderUI({
		if (is.null(corp() )) return(NULL)
		selectInput("select_authors",
					label = "Choose an author:",
					choices = selectauthors(corp() ))
	})

	# render play selection
	output$select_plays <- renderUI({
		if (is.null(corp() )) return(NULL)
		selectInput("select_plays",
					label = "Choose his/her play:",
					choices = selectplays(corp(), input$select_authors))
	})

	# render various play selection
	output$select_multiple_plays <- renderUI({
		if (is.null(corp() )) return(NULL)
		if(input$tabselected == "trends") {
			selected_l <- c(selectplays(corp(), input$select_authors))[1:2]
		} else {
			selected_l <- c(selectplays(corp(), input$select_authors))[1]
		}
		pickerInput("select_multiple_plays",
					label = "Choose his/her plays:",
					choices = c(selectplays(corp(), input$select_authors)),
					selected = selected_l,
					multiple = TRUE,
					options = pickerOptions(
					        actionsBox = TRUE,
					        maxOptions = 100,
					        selectedTextFormat = "count > 3",
					        size = 10
					    )
					)
	})


	# render various play selection (special)
	output$select_multiple_plays_special <- renderUI({
		if (is.null(corp() )) return(NULL)
		pickerInput("select_multiple_plays_special",
					label = "Choose his/her plays:",
					choices = c(selectplays(corp(), input$select_authors)),
					selected = c(selectplays(corp(), input$select_authors))[1:2],
					multiple = TRUE,
					options = pickerOptions(
					        actionsBox = TRUE,
					        maxOptions = 100,
					        selectedTextFormat = "count > 3",
					        size = 10
					    )
					)
	})


	# render all plays checkbox
	output$checkbox_multiple_plays <- renderUI({
		checkboxInput("checkbox_multiple_plays",
					  label = "Select multiple plays",
					  value = FALSE)
	})


	# render all plays substitute text
	output$substitute_text_multiple_plays <- renderText({
		"<h5><i><u>Note</u>: Using multiple play selection by default</i></h5>"
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


	##############################
	### Render tab: Statistics ###
	##############################


	# render dynamic tab title
	output$stats_title <- renderText({
		if (!isTRUE(input$checkbox_multiple_plays)) {
			if (isTRUE(input$statstabselected == 'stats_tab_playlength') |
				isTRUE(input$statstabselected == 'stats_tab_ld') |
				isTRUE(input$statstabselected == 'stats_tab_tr')) {
				"Statistics of selected plays"
			} else {
				"Statistics of a play"
			}
		} else {
			"Statistics of selected plays"
		}
	})

	# render stats HTML page for selected tab
	output$stats_text <- renderUI({
		if (isTRUE(input$statstabselected == "stats_tab_playlength")) {
			statisticshtml <- "www/statistics_pl.html"
		} else if (isTRUE(input$statstabselected == "stats_tab_ld")) {
			statisticshtml <- "www/statistics_ld.html"
		} else if (isTRUE(input$statstabselected == "stats_tab_tr")) {
			statisticshtml <- "www/statistics_tr.html"
		} else {
			statisticshtml <- "www/statistics_overviewtable.html"
		}
		includeHTML(statisticshtml)
	})

	# render statistics panel title
	output$stats_panel_title <- renderText({
		"<h4><u>General statistics options</u></h4>"
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
	output$stats_select_text_readability <- renderUI({
		if (is.null(corp() )) return(NULL)
		selectInput("stats_select_text_readability",
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

	# render statistics plot options
	output$stats_plot_options_title <- renderText({
		"<h4><u>Plot options</u></h4>"
	})

	# render radio buttons for visualization method selection
	output$stats_plot_vis_type_radio <- renderUI({
		radioButtons("stats_plot_vis_type_radio",
					  label = "Select a visualization method:",
					  choices = c("Line Plot", "Bar chart"),
					  selected =  "Line Plot")
	})

	# render radio buttons for word count method selection
	output$stats_plot_tokentype_radio <- renderUI({
		radioButtons("stats_plot_tokentype_radio",
					  label = "Select word count method:",
					  choices = c("Tokens", "Types"),
					  selected =  "Tokens")
	})

	# render checkbox for plot title linebreak option
	output$stats_checkbox_plot_title_linebreak <- renderUI({
		checkboxInput("stats_checkbox_plot_title_linebreak",
					  label = "Use a linebreak in the title of the plot",
					  value = TRUE)
	})

	# render slider for plot title size
	output$stats_plot_title_size_slider <- renderUI({
		sliderInput("stats_plot_title_size_slider",
					"Plot title size:",
					min = 5,
					max = 40,
					step = 1,
					value = 22)
	})

	# render statistics table
	output$stats_table <- DT::renderDataTable({
		lang <- input$select_corpora

		## only one play ##
		if(!isTRUE(input$checkbox_multiple_plays)) {
			textobj <- get_text(corpusobj = input$select_corpora,
									url = urlcorpora,
									play_name = input$select_plays)

		## selected plays of an author ##
		} else {
			textobj <- parse_texts(corpusobj = input$select_corpora,
									url = urlcorpora,
									available_plays = selectplays(corp(), input$select_authors),
									selected_plays = input$select_multiple_plays)
		}


		# EXPAND: different approach?
		# Circumvent following warning:
		# Warning: Error in textstat_frequency.dfm: dfm must have at least one non-zero value
		if(isTRUE(nchar(textobj) > 0)) {
			df <- statistics_df(textobj,
							lang,
							multiple_plays = isTRUE(input$checkbox_multiple_plays),
							remove_punct = input$checkbox_punctation,
							tolower = input$checkbox_lowercase,
							remove_stopwords = input$checkbox_stopwords,
							lexical_diversity_measure = input$stats_select_lexical_diversity,
							readability_measure = input$stats_select_text_readability)
		} else {
			df <- data.frame("name" = "...", "result" = "...")
		}


		withProgress(message = 'Loading plays', value = 0, {
			DT::datatable(df,
				options = list(dom = 't', ordering=FALSE),
				escape = FALSE,
				selection = "none")
		})
	})

	# render play length plot
	output$stats_plot_playlength <- renderPlot({

		playnames <- get_playnames(selectplays(corp(), input$select_authors),
										input$select_multiple_plays_special)
		n <- length(playnames)

		# authors with only one play or only one selected play won't plot anything
		if(isTRUE(length(selectplays(corp(), input$select_authors)) > 1) &
			isTRUE(n > 1)) {

			lang <- input$select_corpora

			textobj_l <- list()

			withProgress(message = 'Loading plays', value = 0, {
				i <- 1
				for(play in playnames) {
					playtext <- get_text(corpusobj = input$select_corpora,
											url = urlcorpora,
											play_name = play)
					playname <- names(playnames)[[i]]


					if(isTRUE(input$stats_plot_tokentype_radio == "Tokens")) {
						textobj_l[[playname]] <- statistics_df(playtext, lang,
															multiple_plays = TRUE,
															remove_punct = input$checkbox_punctation,
															tolower = input$checkbox_lowercase,
															remove_stopwords = input$checkbox_lowercase)[[1]][1]
					} else if(isTRUE(input$stats_plot_tokentype_radio == "Types")) {
						textobj_l[[playname]] <- statistics_df(playtext, lang,
															multiple_plays = TRUE,
															remove_punct = input$checkbox_punctation,
															tolower = input$checkbox_lowercase,
															remove_stopwords = input$checkbox_lowercase)[[1]][2]
					}

					# Increment the progress bar, and update the detail text.
					incProgress(1/n, detail = paste("Play", i, "of", n))
					i <- i + 1

				}
			})

			if(isTRUE(input$stats_plot_tokentype_radio == "Tokens")) {
				text_stat_name <- "Play length (by tokens)"
			} else if(isTRUE(input$stats_plot_tokentype_radio == "Types")) {
				text_stat_name <- "Play length (by types)"
			}

			statistics_distribution(textobj_l,
									input$select_authors,
									measure = text_stat_name,
									text_stat_name = text_stat_name,
									plot_method = input$stats_plot_vis_type_radio,
									plot_title_size = input$stats_plot_title_size_slider,
									plot_title_linebreak = input$stats_checkbox_plot_title_linebreak)


		}
	})

	# render lexical diversity plot
	output$stats_plot_ld <- renderPlot({

		playnames <- get_playnames(selectplays(corp(), input$select_authors),
										input$select_multiple_plays_special)
		n <- length(playnames)

		# authors with only one play or only one selected play won't plot anything
		if(isTRUE(length(selectplays(corp(), input$select_authors)) > 1) &
			isTRUE(n > 1)) {

			lang <- input$select_corpora
			ld_measure <- get_lexical_diversity_name(input$stats_select_lexical_diversity)


			textobj_l <- list()

			withProgress(message = 'Loading plays', value = 0, {
				i <- 1
				for(play in playnames) {
					playtext <- get_text(corpusobj = input$select_corpora,
											url = urlcorpora,
											play_name = play)
					playtokens <- get_tokens(playtext, lang,
											remove_punct = input$checkbox_punctation,
											tolower = input$checkbox_lowercase,
											remove_stopwords = input$checkbox_stopwords)
					playname <- names(playnames)[[i]]
					textobj_l[[playname]] <- textstat_lexdiv(playtokens, measure = ld_measure)[[2]]

					# Increment the progress bar, and update the detail text.
					incProgress(1/n, detail = paste("Play", i, "of", n))
					i <- i + 1
				}
			})

			statistics_distribution(textobj_l,
									input$select_authors,
									ld_measure,
									text_stat_name = "Lexical diversity",
									plot_method = input$stats_plot_vis_type_radio,
									plot_title_size = input$stats_plot_title_size_slider,
									plot_title_linebreak = input$stats_checkbox_plot_title_linebreak)
		}
	})

	# render text readability plot
	output$stats_plot_tr <- renderPlot({

		playnames <- get_playnames(selectplays(corp(), input$select_authors),
										input$select_multiple_plays_special)
		n <- length(playnames)

		# authors with only one play or only one selected play won't plot anything
		if(isTRUE(length(selectplays(corp(), input$select_authors)) > 1) &
			isTRUE(n > 1)) {

			lang <- input$select_corpora
			tr_measure <- input$stats_select_text_readability

			textobj_l <- list()

			withProgress(message = 'Loading plays', value = 0, {
				i <- 1
				for(play in playnames) {
					playtext <- get_text(corpusobj = input$select_corpora,
											url = urlcorpora,
											play_name = play)
					playname <- names(playnames)[[i]]
					textobj_l[[playname]] <- textstat_readability(playtext, measure = tr_measure)[[2]]

					# Increment the progress bar, and update the detail text.
						incProgress(1/n, detail = paste("Play", i, "of", n))
						i <- i + 1

				}
			})


			statistics_distribution(textobj_l,
									input$select_authors,
									tr_measure,
									text_stat_name = "Text readability",
									plot_method = input$stats_plot_vis_type_radio,
									plot_title_size = input$stats_plot_title_size_slider,
									plot_title_linebreak = input$stats_checkbox_plot_title_linebreak)
		}
	})



	###############################################
	### Render tab: Play Reader 				###
	###############################################
	### EXPAND: the reader is a prototype. 		###
	###		  	It could be expanded with e.g.:	###
	###		  	- CETEI							###
	###		  	- TEI Boilerplate + XSLT		###
	###		  	- own javascript files			###
	###############################################



	# render dynamic tab title
	output$reader_title <- renderText({
		if (!isTRUE(input$checkbox_multiple_plays)) {
			"Simple play reader"
		} else {
			"Simple plays reader"
		}
	})

	# render reader text representation radio buttons
	output$reader_radio_text_representation <- renderUI({
		radioButtons("reader_radio_text_representation",
					  label = "Select text representation:",
					  choices = c("Raw text", "Embedded HTML"),
					  selected =  "Raw text")
	})

	# render reader text
	output$reader_text <- renderUI({

		if(isTRUE(input$reader_radio_text_representation == "Embedded HTML")) {
			link <- paste0(urlreadercorpus, "/", input$select_plays, "#text")
			tags$iframe(src=link, height="100%", width="100%")

		} else if(isTRUE(input$reader_radio_text_representation == "Raw text")) {

			lang <- input$select_corpora

			## only one play ##
			if (!isTRUE(input$checkbox_multiple_plays)) {
				textobj <- get_text(corpusobj = input$select_corpora,
										url = urlcorpora,
										play_name = input$select_plays)
			## selected plays of an author ##
			} else {
				textobj <- parse_texts(corpusobj = input$select_corpora,
										url = urlcorpora,
										available_plays = selectplays(corp(), input$select_authors),
										selected_plays = input$select_multiple_plays,
										add_title = TRUE)
			}

			HTML(gsub("\n", "<br>", textobj))
		}
	})


	##########################################
	### Render tab: Word Frequencies Table ###
	##########################################

	# render dynamic tab title
	output$wordfreqtitle <- renderText({
		if (!isTRUE(input$checkbox_multiple_plays)) {
			"Word frequencies of a single play"
		} else {
			"Word frequencies of selected plays of an author"
		}
	})

	# render wordfreq panel title
	output$wordfreq_panel_title <- renderText({
		"<h4><u>Word frequencies options</u></h4>"
	})


	# render order of frequencies in wordfreq plot
	output$wordfreq_checkbox_plot_revert <- renderUI({
		checkboxInput("wordfreq_checkbox_plot_revert",
					  label = "Revert the order of the frequencies",
					  value = FALSE)
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

	# render wordfreq plot frequency method
	output$wordfreq_frequency_radio <- renderUI({
		radioButtons("wordfreq_frequency_radio",
					  label = "Select frequency method:",
					  choices = c("Relative Frequencies", "Absolute Frequencies"),
					  selected =  "Absolute Frequencies")
	})

	# render checkbox for wordfreq distribution title linebreak option
	output$wordfreq_checkbox_plot_title_linebreak <- renderUI({
		checkboxInput("wordfreq_checkbox_plot_title_linebreak",
					  label = "Use a linebreak in the title of the plot",
					  value = TRUE)
	})

	# render slider for wordfreq distribution title size
	output$wordfreq_plot_title_size_slider <- renderUI({
		sliderInput("wordfreq_plot_title_size_slider",
					"Plot title size:",
					min = 5,
					max = 40,
					step = 1,
					value = 22)
	})


	# render HTML page with instructions on how to use table or distribution
	output$wordfreq_text <- renderUI({
		if (isTRUE(input$wordfreqtabselected == "Table")) {
			wordfreqhtml <- "www/wordfreqtext_table.html"
		} else if(isTRUE(input$wordfreqtabselected == "Distribution")) {
			wordfreqhtml <- "www/wordfreqtext_plot.html"
		}
		includeHTML(wordfreqhtml)
	})

	# render word frequency table
	output$wordfreq_table <- DT::renderDataTable({
		lang <- input$select_corpora

		## only one play ##
		if (!isTRUE(input$checkbox_multiple_plays)) {

			textobj <- get_text(corpusobj = input$select_corpora,
									url = urlcorpora,
									play_name = input$select_plays)

			withProgress(message = 'Counting word frequencies', {
				df <- freq_df(textobj,
								lang,
								remove_punct = input$checkbox_punctation,
								tolower = input$checkbox_lowercase,
								remove_stopwords = input$checkbox_stopwords,
								frequency_method = input$wordfreq_frequency_radio)
			})


		## selected plays of an author ##
		} else {

			playnames <- get_playnames(selectplays(corp(), input$select_authors),
										input$select_multiple_plays)
			n <- length(playnames)
			dflist <- vector("list", n)

			withProgress(message = 'Loading plays', value = 0, {
				i <- 1
				for(play in playnames) {
					playtext <- get_text(corpusobj = input$select_corpora,
											url = urlcorpora,
											play_name = play)
					withProgress(message = 'Counting word frequencies', {
						df <- freq_df(playtext,
										lang,
										remove_punct = input$checkbox_punctation,
										tolower = input$checkbox_lowercase,
										remove_stopwords = input$checkbox_stopwords,
										frequency_method = input$wordfreq_frequency_radio)

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

		}

		DT::datatable(df,
					  filter = 'top',
					  options = list(lengthMenu = c(5, 10, 20, 30, 50, 100),
									 orderClasses = TRUE,
									 pageLength = 10),
					  selection = "none")
	})

	# render word frequency distribution
	output$wordfreq_distribution <- renderPlot({
		lang <- input$select_corpora

		## only one play ##
		if (!isTRUE(input$checkbox_multiple_plays)) {
			textobj <- get_text(corpusobj = input$select_corpora,
									url = urlcorpora,
									play_name = input$select_plays)

		## selected plays of an author ##
		} else {
			textobj <- parse_texts(corpusobj = input$select_corpora,
									url = urlcorpora,
									available_plays = selectplays(corp(), input$select_authors),
									selected_plays = input$select_multiple_plays)
		}

		withProgress(message = 'Counting word frequencies', {
			frequency_distribution(textobj,
									lang,
									remove_punct = input$checkbox_punctation,
									tolower = input$checkbox_lowercase,
									remove_stopwords = input$checkbox_stopwords,
									all_playnames = selectplays(corp(), input$select_authors),
									author_name = input$select_authors,
									color_point = color_point,
									frequency_method = input$wordfreq_frequency_radio,
						  			multiple_plays = isTRUE(input$checkbox_multiple_plays),
						  			play_name = input$select_plays,
						  			plot_title_linebreak = input$wordfreq_checkbox_plot_title_linebreak,
									plot_title_size = input$wordfreq_plot_title_size_slider,
									revert_order = input$wordfreq_checkbox_plot_revert,
						  			top_n_features = input$wordfreq_top_feature_slider)
		})
	})



	#############################
	### Render tab: Wordcloud ###
	#############################


	# render dynamic tab title
	output$wordcloud_title <- renderText({
		if (!isTRUE(input$checkbox_multiple_plays)) {
			"Wordcloud of a single play"
		} else {
			if (isTRUE(input$wordcloudtabselected == "wordcloud_tab_comparison")) {
				"Comparison wordcloud of selected plays"
			} else {
				"Wordcloud of selected plays"
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
	# EXPAND: higher max slider?
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


	# render play selection for wordcloud comparison
	output$select_wordcloud_comparison_plays <- renderUI({
		if (is.null(corp())) return(NULL)
		pickerInput(inputId = "select_wordcloud_comparison_plays",
			label = "Select the plays to be compared:",
			choices = c(selectplays(corp(), input$select_authors)),
			selected = c(selectplays(corp(), input$select_authors))[1:2],
			multiple = TRUE,
			options = pickerOptions(
		        actionsBox = TRUE,
		        maxOptions = 8,
		        selectAllText = "Select All (max. 8)",
		        selectedTextFormat = "count > 3",
		        size = 10
		      )
		)
	})

	# render HTML page with instructions on how to use the wordclouds
	output$wordcloud_text <- renderUI({
		if (isTRUE(input$wordcloudtabselected == "wordcloud_tab_comparison")) {
			wordcloudhtml <- "www/wordcloudtext_comparison.html"
		} else {
			wordcloudhtml <- "www/wordcloudtext_simple.html"
		}
		includeHTML(wordcloudhtml)
	})


	# render wordcloud of a single play or selected plays
	output$wordcloud_plotter <- renderPlot({

		lang <- input$select_corpora

		## only one play ##
		if (!isTRUE(input$checkbox_multiple_plays)) {
			textobj <- get_text(corpusobj = input$select_corpora,
									url = urlcorpora,
									play_name = input$select_plays)

			colors <- colors_sequential

		## selected plays of an author ##
		} else {
			textobj <- parse_texts(corpusobj = input$select_corpora,
									url = urlcorpora,
									available_plays = selectplays(corp(), input$select_authors),
									selected_plays = input$select_multiple_plays)
			colors <- colors_sequential2
		}


		withProgress(message = 'Generating word cloud', {
				wordcloudplot(textobj,
							  lang,
							  remove_punct = input$checkbox_punctation,
							  tolower = input$checkbox_lowercase,
							  remove_stopwords = input$checkbox_stopwords,
							  color = colors,
							  max_words = input$wordcloud_max_n_words,
							  min_freq= input$wordcloud_min_freq,
							  word_padding = input$wordcloud_word_padding)
			})
	},
	height=function(){300*input$wordcloud_window_size},
	width=function(){300*input$wordcloud_window_size})


	# render comparison wordcloud
	output$wordcloud_comparison_plotter <- renderPlot({
		playnames <- get_playnames(selectplays(corp(), input$select_authors),
										input$select_wordcloud_comparison_plays)
		n <- length(playnames)

		# authors with only one play or only one selected play won't plot anything
		if(isTRUE(length(selectplays(corp(), input$select_authors)) > 1) &
			isTRUE(n > 1)) {

			lang <- input$select_corpora

			textobj <- parse_texts(corpusobj = input$select_corpora,
									url = urlcorpora,
									available_plays = selectplays(corp(), input$select_authors),
									selected_plays = input$select_wordcloud_comparison_plays,
									skip_union = TRUE)


			names(textobj) <- names(playnames)
			if (length(textobj) > 8) {
				textobj <- textobj[1:8]
			}

			textobj <- corpus(unlist(textobj, use.names=TRUE))
			colors <- colors_qualitative


			withProgress(message = 'Generating word cloud', {
				wordcloudplot(textobj,
							  lang,
							  remove_punct = input$checkbox_punctation,
							  tolower = input$checkbox_lowercase,
							  remove_stopwords = input$checkbox_stopwords,
							  color = colors,
							  comparison = TRUE,
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


	##########################
	### Render tab: Trends ###
	##########################

	# render trends title
	output$trends_title <- renderText({
		if (!isTRUE(input$checkbox_multiple_plays)) {
			"Trends of a single play"
		} else {
			"Trends of selected plays"
		}
	})

	# render HTML page with instructions on how to use the trends
	output$trends_text <- renderUI({
		includeHTML(trendshtml)
	})


	# render trends panel title
	output$trends_panel_title <- renderText({
		"<h4><u>Trends options</u></h4>"
	})

	# render trends sorting radio buttons
	output$trends_radio_sorting <- renderUI({
		radioButtons("trends_radio_sorting",
					  label = "Select target word sorting method:",
					  choices = c("alphabetical", "most frequent words"),
					  selected =  "most frequent words")
	})

	# render trends token selection
	output$trends_select_token <- renderUI({

		lang <- input$select_corpora
		if (!isTRUE(input$checkbox_multiple_plays)) {
			textobj <- get_text(corpusobj = input$select_corpora,
								url = urlcorpora,
								play_name = input$select_plays)
		} else {
			textobj <- parse_texts(corpusobj = input$select_corpora,
									url = urlcorpora,
									available_plays = selectplays(corp(), input$select_authors),
									selected_plays = input$select_multiple_plays)
		}


		# select sorting method ("alphabetical", "most frequent words" or no sorting)
		if(isTRUE(input$trends_radio_sorting == "alphabetical")) {
			token_list <- get_tokens(textobj,
									lang,
									tolower = input$checkbox_lowercase,
									remove_punct = input$checkbox_punctation,
									remove_stopwords = input$checkbox_stopwords,
									sort_by = "alphabetical")
		} else if(isTRUE(input$trends_radio_sorting == "most frequent words")) {
			token_list <- get_tokens(textobj,
									lang,
									tolower = input$checkbox_lowercase,
									remove_punct = input$checkbox_punctation,
									remove_stopwords = input$checkbox_stopwords,
									sort_by = "mfw+")
		} else {
			token_list <- get_tokens(textobj,
									lang,
									tolower = input$checkbox_lowercase,
									remove_punct = input$checkbox_punctation,
									remove_stopwords = input$checkbox_stopwords,
									sort_by = "")
		}

		selectizeInput("trends_select_token",
					label = "Select the target word:",
					choices = token_list,
					# EXPAND: for plays/combined plays with a higher token count
					options = list(maxOptions = 500000),
					selected = "")
	})

	# EXPAND: max value
	# render slider for trends plot title size
	output$trends_n_segments <- renderUI({
		sliderInput("trends_n_segments",
					"Number of segments:",
					min = 2,
					max = 50,
					step = 1,
					value = 5)
	})

	# render frequency radio buttons for trend
	output$trends_frequency_radio <- renderUI({
		radioButtons("trends_frequency_radio",
					  label = "Select frequency method:",
					  choices = c("Relative Frequencies", "Absolute Frequencies"),
					  selected =  "Relative Frequencies")
	})

	# render checkbox for trends plot title linebreak option
	output$trends_checkbox_plot_title_linebreak <- renderUI({
		checkboxInput("trends_checkbox_plot_title_linebreak",
					  label = "Use a linebreak in the title of the plot",
					  value = TRUE)
	})

	# render slider for trends plot title size
	output$trends_plot_title_size_slider <- renderUI({
		sliderInput("trends_plot_title_size_slider",
					"Plot title size:",
					min = 5,
					max = 40,
					step = 1,
					value = 22)
	})


	# render trends plot
	output$trends_plotter <- renderPlot({

		lang <- input$select_corpora

		## only one play ##
		if (!isTRUE(input$checkbox_multiple_plays)) {

			textobj <- get_text(corpusobj = input$select_corpora,
								url = urlcorpora,
								play_name = input$select_plays)

			withProgress(message = 'Generating trend plot', {
				if(isTRUE(nchar(textobj) > 0)) {
					trends_segments_plot(textobj,
									lang = lang,
									url = urlcorpora,
									remove_punct = input$checkbox_punctation,
									tolower = input$checkbox_lowercase,
									remove_stopwords = input$checkbox_stopwords,
									n_segments = input$trends_n_segments,
									author_name = input$select_authors,
									all_playnames = selectplays(corp(), input$select_authors),
									target_word = sub(" *\\(.*", "", input$trends_select_token),
									play_name = input$select_plays,
									frequency_method = input$trends_frequency_radio,
									plot_title_size = input$trends_plot_title_size_slider,
									plot_title_linebreak = input$trends_checkbox_plot_title_linebreak)
				} else {
					df <- data.frame()
					ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
				}

			})


		## selected plays of an author ##
		} else {

			playnames <- get_playnames(selectplays(corp(), input$select_authors),
										input$select_multiple_plays)

			# authors with only one play or only one selected play won't plot anything
			if(isTRUE(length(selectplays(corp(), input$select_authors)) > 1) &
				isTRUE(length(playnames) > 1)) {
				withProgress(message = 'Generating trend plot', {
					trends_plays_plot(corpusobj = input$select_corpora,
						playnames = playnames,
						url = urlcorpora,
						lang = lang,
						remove_punct = input$checkbox_punctation,
						tolower = input$checkbox_lowercase,
						remove_stopwords = input$checkbox_stopwords,
						author_name = input$select_authors,
						frequency_method = input$trends_frequency_radio,
						target_word = sub(" *\\(.*", "", input$trends_select_token),
						plot_title_size = input$trends_plot_title_size_slider,
						plot_title_linebreak = input$trends_checkbox_plot_title_linebreak)
				})
			}
		}
	})


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
					"Number of words before/after keyword:",
					min = 1,
					max = 20,
					step = 1,
					value = 5)
	})


	# render token selection
	output$kwic_select_token <- renderUI({

		lang <- input$select_corpora


		if (!isTRUE(input$checkbox_multiple_plays)) {
			textobj <- get_text(corpusobj = input$select_corpora,
								url = urlcorpora,
								play_name = input$select_plays)
		} else {
			textobj <- parse_texts(corpusobj = input$select_corpora,
									url = urlcorpora,
									available_plays = selectplays(corp(), input$select_authors),
									selected_plays = input$select_multiple_plays)
		}


		# select sorting method ("alphabetical", "most frequent words" or no sorting)
		if(isTRUE(input$kwic_radio_sorting == "alphabetical")) {
			token_list <- get_tokens(textobj,
									lang,
									tolower = input$checkbox_lowercase,
									remove_punct = input$checkbox_punctation,
									remove_stopwords = input$checkbox_stopwords,
									sort_by = "alphabetical")
		} else if(isTRUE(input$kwic_radio_sorting == "most frequent words")) {
			token_list <- get_tokens(textobj,
									lang,
									tolower = input$checkbox_lowercase,
									remove_punct = input$checkbox_punctation,
									remove_stopwords = input$checkbox_stopwords,
									sort_by = "mfw+")
		} else {
			token_list <- get_tokens(textobj,
									lang,
									tolower = input$checkbox_lowercase,
									remove_punct = input$checkbox_punctation,
									remove_stopwords = input$checkbox_stopwords,
									sort_by = "")
		}

		selectizeInput("kwic_select_token",
					label = "Select a keyword:",
					choices = token_list,
					# EXPAND: for plays/combined plays with a higher token count
					options = list(maxOptions = 500000),
					selected = NULL)
	})


	# render kwic table
	output$kwic_table <- DT::renderDataTable({

		lang <- input$select_corpora
		textobj <- ""
		docname <- ""

		## only one play ##
		if (!isTRUE(input$checkbox_multiple_plays)) {
			textobj <- get_text(corpusobj = input$select_corpora,
									url = urlcorpora,
									play_name = input$select_plays)
			playnames <- selectplays(corp(), input$select_authors)
			docname <- names(playnames)[playnames == input$select_plays]

		## selected plays of an author ##
		} else {
			playnames <- get_playnames(selectplays(corp(), input$select_authors), input$select_multiple_plays)
			tmp_textobj <- parse_texts(corpusobj = input$select_corpora,
										url = urlcorpora,
										available_plays = selectplays(corp(), input$select_authors),
										selected_plays = input$select_multiple_plays,
										skip_union = TRUE)

			if (length(tmp_textobj) > 1) {
				names(tmp_textobj) <- names(playnames)
				textobj <- corpus(unlist(tmp_textobj, use.names=TRUE))
			} else {
				textobj <- paste(tmp_textobj, sep = "", collapse = "")
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

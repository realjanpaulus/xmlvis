library(curl)
library(docstring, warn.conflicts = FALSE)
library(DT)
library(jsonlite, warn.conflicts = FALSE)
library(methods)
library(quanteda, warn.conflicts = FALSE)
library("quanteda.textplots", warn.conflicts = FALSE)
library("quanteda.textstats", warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(shinythemes)
library(stringr)


#################
# api functions #
#################


extractdramas <- function(urlcorpus) {
	#' Extracts dramas from a corpus.
	#'
	#' Takes an url to a corpus json file and extracs the "dramas" entry.
	#'
	#' @param urlcorpus an url to a corpus.
	return(fromJSON(urlcorpus, flatten = TRUE)$dramas)
}


selectauthors <- function(corpus) {
	#' Selects a list of authors from corpus.
	#'
	#' Takes a corpus and returns a list of all the included authors.
	#'
	#' @param corpus a list of informations/contents of the selected corpus.
	authors <- unique(corpus$author.name)
	names(authors) <- unique(corpus$author.name)
	return(authors)
}


selectcorpora <- function(urlcorpora) {
	#' Selects a list of corpora from url.
	#'
	#' Takes an url to a json file with corpora infos 
	#' and collects all names in a list.
	#'
	#' @param urlcorpora an url to all corpora.
	corpora <- fromJSON(urlcorpora)
	corporanames <- corpora$name
	names(corporanames) <- corpora$title
	return(as.list(corporanames))
}


selectplays <- function(corpus, input = input) {
	#' Selects a list of all plays from an author within a corpus.
	#' 
	#' Takes a corpus and an input (id name, consisting of author + title) and
	#' returns a list of the authors plays (play name + publication year)
	#' which is sorted by the publication year in ascending order.
	#'
	#' @param corpus a list of informations/contents of the selected corpus.
	#' @param input the name of the selected author.
	links <- corpus[corpus$author.name == input, "name"]
	years <- corpus[corpus$author.name == input, "yearNormalized"]
	titles <- corpus[corpus$author.name == input, "title"]

	# combining play title and play publication year, e.g. "Brot! (1888)"
	titles_years <- list()
	i <- 1
	for(element in titles) {
		titles_years[[i]] <- paste0(titles[[i]], " (", years[[i]], ")")
		i <- i+1
	}
	names(links) <- titles_years
	return(links[order(as.numeric(gsub("[^0-9]+", "", names(links))))])
}


##############################################
# text processing and other helper functions #
##############################################


combine_dfs <- function(df1, df2) {
	#' Combines two dataframes to one dataframe.
	#'
	#' Combines two dataframes to one dataframe by the column "feature"
	#' and sorts it by the column "frequency" (descending).
	#'
	#' @param df1 a dataframe, created by quanteda::textstat_frequency()
	#' @param df2 a dataframe, created by quanteda::textstat_frequency()
	df <- ddply(rbind(df1, df2), "feature", numcolwise(sum))
	return(arrange(df, desc(frequency)))
}


documenttermdf <- function(textobj, 
	lang, 
	remove_punct = TRUE, 
	tolower = TRUE, 
	remove_stopwords = FALSE,
	use_relative_frequency = FALSE) {
	#' Creates a document-term matrix from a plain text or a quanteda corpus object.
	#'
	#' Takes plain text of a play or a quanteda corpus object 
	#' and creates a document-term matrix from the text. 
	#' The matrix is stored inside a dataframe.
	#'
	#' @param textobj text as string.
	#' @param lang language identificator as string.
	#' @param remove_punct boolean value if punctation should be removed.
	#' @param tolower boolean value if words should be converted to lowercase.
	#' @param remove_stopwords boolean value if stopwords should be removed.
	#' @param use_relative_frequency boolean value if relative frequency should be used.

	if (isTRUE(remove_stopwords)) {
		df <- dfm(textobj, 
				  remove_punct = remove_punct, 
				  tolower = tolower,
				  remove = get_stopwords(lang))
	} else {
		df <- dfm(textobj, 
				  remove_punct = remove_punct, 
				  tolower = tolower)
	}

	if(isTRUE(use_relative_frequency)) {
		df <- dfm_weight(df, scheme = "prop")

	}

	return(df)
}


get_lexical_diversity_name <- function(lexical_diversity_measure) {
	#' Returns the correct lexical diversity measure string.
	#'
	#' Takes a string and checks if it's a valid string for the measure parameter of
	#' the quanteda lexical diversity function.
	#'  
	#' @param lexical_diversity_measure string which indicates a lexical diversity measure.
	#'		  All available options can be found here: 
	#'		  https://www.rdocumentation.org/packages/quanteda.textstats/versions/0.91/topics/textstat_lexdiv
	ld_measure_options <- c("TTR", "C", "R", "CTTR", "U", "S", "K", "I", "D", "Vm", "Maas", "MATTR", "MSTTR")
	names(ld_measure_options) <- c("TTR: Token-Type Ratio", "C: Herdan's C", "R: Root TTR", "CTTR: Corrected TTR", 
						"U: Uber Index", "S: Summers Index", "K: Yule's K", "I: Yule's I", "D: Simpson's D", 
						"Vm: Herdan's Vm", "Maas: Maas indices", "MATTR: Moving-Average Type-Token Ratio", 
						"MSTTR: Mean Segmental Type-Token Ratio")
	if (isTRUE(lexical_diversity_measure %in% ld_measure_options)) {
		return(lexical_diversity_measure)
	} else if (isTRUE(lexical_diversity_measure %in% names(ld_measure_options))) {
		return(getElement(ld_measure_options, lexical_diversity_measure))
	} else {
		return("TTR: Token-Type Ratio")
	}
}


get_playnames <- function(available_plays, selected_plays) {
	#' Selects plays from a list of plays and returns them in a list with names.
	#' 
	#' Takes a list of all available plays of an author and a list of plays which
	#' should be selected and returns a list of the selected plays with names,
	#' taken from the available plays list.
	#'
	#' @param available_plays list of all available plays of an author.
	#' @param selected_plays list of plays which should be selected.

	available_plays_names <- names(available_plays)

	playnames <- list()
	i <- 1
	for (element in available_plays) {
		if(element %in% selected_plays) {
			playnames[[available_plays_names[i]]] <- element
			i <- i+1
		}
	}
	return(playnames)
}


get_stopwords <- function(lang) {
	#' Get a list of stopwords by its language.
	#'
	#' Takes a language string and returns the correct stopword list.
	#'
	#' @param lang a string for a language.
	output <- lang
	src <- "stopwords-iso"

	# EXPAND: more "else if" for more corpora
	if (lang == "ger") {
		output <- "german"
		src <- "stopwords-iso"
	}
	return(stopwords(output, source=src))
}


get_text <- function(corpusobj, url, play_name) {
	#' Extracts the play text by an url.
	#' 
	#' Takes an url to the plain text of an url and extracts the text.
	#'
	#' @param corpusobj shiny corpus object.
	#' @param url a string with an url to the plain text of an url.
	#' @param playname name of the play as string.

	texturl <- paste0(url, "/", corpusobj, "/play/", play_name, "/spoken-text")
	text <- content(GET(texturl), "text", encoding="UTF-8")
	return(text)
}


get_tokens <- function(textobj, 
	lang, 
	tolower = TRUE, 
	remove_punct = TRUE, 
	remove_stopwords = TRUE, 
	sort_by = "") {
	#' Get quanteda token object from a text object.
	#'
	#' Takes a text object and creates a (sorted) quanteda tokens object. 
	#'
	#' @param textobj text as string.
	#' @param lang language identificator as string.
	#' @param remove_punct boolean value if punctation should be removed.
	#' @param tolower boolean value if words should be converted to lowercase.
	#' @param remove_stopwords boolean value if stopwords should be removed.
	#' @param sort_by string to indicate sorting method. Available options are
	#'		  'alphabetical', 'mfw' (= most frequent words in descending order), 
	#'		  'mfw+' (= mfw + frequency in parantheses) or an empty or another string 
	#'		  which means no sorting will be used. 

	toks <- tokens(textobj, remove_punct = remove_punct)

	if (isTRUE(tolower)) {
		toks <- tokens_tolower(toks)
	}

	if(isTRUE(remove_stopwords)) {
		toks <- tokens_remove(toks, pattern = get_stopwords(lang))
	}

	# alphabetical order
	if(isTRUE(sort_by == "alphabetical")) {
		toks <- str_sort(unlist(toks), numeric = TRUE)

	# most frequent words in descending order
	} else if (isTRUE(sort_by == "mfw")) {
		df <- freq_df(textobj, lang, remove_punct = remove_punct, tolower = tolower, remove_stopwords = remove_stopwords)
		toks <- c(df[["feature"]])
	# most frequent words + their frequency in parantheses in descending order
	} else if (isTRUE(sort_by == "mfw+")) {
		df <- freq_df(textobj, lang, remove_punct = remove_punct, tolower = tolower, remove_stopwords = remove_stopwords)
		
		toks <- list()
		toks_mfw <- c(df[["feature"]])
		toks_freq <- c(df[["frequency"]])
		i <- 1
		for(element in toks_mfw) {
			toks[[i]] <- paste0(toks_mfw[[i]], " (", toks_freq[[i]], ")")
			i <- i+1
		}
	}

	return(toks)
}


parse_texts <- function(corpusobj, 
	url, 
	available_plays = list(), 
	selected_plays = list(), 
	add_title = FALSE, 
	skip_union = FALSE) {
	#' Takes a corpus object and returns a string (or list) with all
	#' selected plays in it.
	#'
	#' Takes a corpus object, a list of all plays of the author and a list
	#' with plays to be selected. The names for the selected plays will be
	#' gathered with the help of the list of all plays. The function then 
	#' returns a string (or list) of all selected plays, the text is gathered
	#' by the plays names.
	#'
	#' @param corpusobj shiny corpus object.
	#' @param url a string with an url to the plain text of an url.
	#' @param available_plays list of all available plays of an author.
	#' @param selected_plays list of plays which should be selected.
	#' @param add_title boolean if title should be added to play text.
	#' @param skip_union boolean if union of selected plays to a string
	#'		  should be skipped. If TRUE, a list of plays is returned.

	
	playnames <- get_playnames(available_plays, selected_plays) 

	n <- length(playnames)
	tmp_textobj <- vector("list", n)

	withProgress(message = 'Loading plays', value = 0, {
		i <- 1
		for(play in playnames) {
			playtext <- get_text(corpusobj = corpusobj,
									url = url,
									play_name = play)
			tmp_textobj[[i]] <- playtext

			if(isTRUE(add_title)) {
				playtitle <- names(playnames)[[i]]
				tmp_textobj[[i]] <- paste0("<br><h2>", playtitle, "</h2><br>", playtext)
			}

			# Increment the progress bar, and update the detail text.
			incProgress(1/n, detail = paste("Play", i, "of", n))
			i <- i + 1
		}
	})

	if(!isTRUE(skip_union)) {
		textobj <- paste(tmp_textobj, sep = "", collapse = "")
	} else {
		textobj <- tmp_textobj
	}

	return(textobj)
}


##################
# tool functions #
##################


freq_df <- function(textobj, 
	lang, 
	remove_punct = TRUE, 
	tolower = TRUE, 
	remove_stopwords = FALSE, 
	frequency_method = "Absolute Frequencies",
	top_n_features = NULL) {
	#' Extracts term and document frequencies from a plain text 
	#' or a quanteda corpus object.
	#'
	#' Takes plain text of a play or a quanteda corpus object 
	#' and creates a dataframe with term and document frequencies of the text. 
	#' Columns "rank" and "group" are removed.
	#'
	#' @param playtext text of the play as string.
	#' @param lang language identificator as string.
	#' @param remove_punct boolean value if punctation should be removed.
	#' @param tolower boolean value if words should be converted to lowercase.
	#' @param remove_stopwords boolean value if stopwords should be removed.
	#' @param frequency_method string name of the frequency method ("Relative Frequencies"
	#'		  or "Absolute Frequencies").
	#' @param top_n_features numerical value which indicates the top n features.
	

	if(frequency_method == "Relative Frequencies") {
		use_relative_frequency <- TRUE

	} else {
		use_relative_frequency <- FALSE
	}

	df <- documenttermdf(textobj, lang, 
						 remove_punct = remove_punct,
					   	 tolower = tolower, 
					   	 remove_stopwords = remove_stopwords,
					   	 use_relative_frequency = use_relative_frequency)
	return(subset(textstat_frequency(df, n=top_n_features), select=-c(rank, group)))	
}


frequency_distribution <- function(textobj, 
	lang, 
	remove_punct = TRUE, 
	tolower = TRUE, 
	remove_stopwords = FALSE, 
	all_playnames = list(), 
	author_name = "", 
	color_point = "#8DA0CB", 
	frequency_method = "Relative Frequencies",
	multiple_plays = FALSE, 
	play_name = "", 
	plot_title_linebreak = FALSE, 
	plot_title_size = 22, 
	revert_order = FALSE, 
	top_n_features = 100) {
	#' Creates a frequency distribution from a text.
	#'
	#' Takes a plain text or quanteda corpus object, computes a word frequency
	#' dataframe and and creates a frequency distribution from it.
	#'
	#' @param textobj text as string.
	#' @param lang language identificator as string.
	#' @param remove_punct boolean value if punctation should be removed.
	#' @param tolower boolean value if words should be converted to lowercase.
	#' @param remove_stopwords boolean value if stopwords should be removed.
	#' @param all_playnames list with names of all plays of the author.
	#' @param author_name string name of the author.
	#' @param color_point hex code as string which indicates the color of the points.
	#' @param frequency_method string name of the frequency method ("Relative Frequencies"
	#'		  or "Absolute Frequencies").
	#' @param multiple_plays boolean if mutliple plays are used.
	#' @param play_name string name of the play.
	#' @param plot_title_linebreak boolean if plot title should contain linebreak.
	#' @param plot_title_size numeric which indicates size of plot title.
	#' @param revert_order boolean if frequencies should be sorted in
	#'		  ascending or descending order.
	#' @param top_n_features numerical value which indicates the top n features.


	if (isTRUE(plot_title_linebreak)) {
		lb <- "\n"
	} else {
		lb <- ""
	}

	if(isTRUE(multiple_plays)) {
		plot_title <- paste0("Word frequency distribution ", lb, 
						"of the selected plays by '", author_name, "'")
	} else {
		playnames <- get_playnames(all_playnames, play_name) 
		playname <- names(unlist(playnames))[1]

		plot_title <- paste0("Word frequency distribution ", lb, 
						"of the play '", playname, "' by '", author_name, "'")
	}


	if(frequency_method == "Relative Frequencies") {
		use_relative_frequency <- TRUE
		y_col_name <- "Relative frequency"
	} else {
		use_relative_frequency <- FALSE
		y_col_name <- "Absolute frequency"
	}

	df <- freq_df(textobj, lang, 
					 remove_punct = remove_punct,
					 tolower = tolower, 
					 remove_stopwords = remove_stopwords,
					 frequency_method = frequency_method,
					 top_n_features = top_n_features)


	if(isTRUE(revert_order)) {
		df$feature <- with(df, reorder(feature, frequency))
	} else {
		df$feature <- with(df, reorder(feature, -frequency))
	}

	# EXPAND: different size, base_size values for different corpora
	if(top_n_features <= 25) {
		size = 5
		base_size = 24
	} else if((top_n_features > 25) & (top_n_features <= 50)) {
		size = 4
		base_size = 18
	} else if((top_n_features > 50) & (top_n_features <= 75)) {
		size = 3
		base_size = 12
	} else {
		size = 2
		base_size = 8
	}


	g <- ggplot(data = df, aes(x = feature, y = frequency)) + 
				geom_point(colour=color_point, size=size) + 
				labs(title = plot_title, y = y_col_name, x = "Features") +
				scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
				theme_bw(base_size=base_size) + 
				theme(axis.text.x = element_text(angle = 90, hjust = 1),
						plot.title = element_text(size=plot_title_size,
													margin=margin(0,0,10,0)))

	return(g)
}


# EXPAND: allow phrases
kwic_df <- function(textobj, 
	lang, 
	pattern, 
	window = 5, 
	docname = "", 
	tolower = TRUE, 
	remove_punct = TRUE) {
	#' Creates a quanteda KWIC dataframe.
	#' 
	#' Takes a text object and a pattern/keyword to create a KWIC DataFrame from them.
	#'
	#' @param textobj text as string.
	#' @param lang language identificator as string.
	#' @param pattern string which indicates the keyword to be analysed in KWIC df.
	#' @param window numeric which indicates the number of words before and after the pattern.
	#' @param docname string which, if not empty, sets the name of the document.
	#' @param tolower boolean value if words should be converted to lowercase.
	#' @param remove_punct boolean value if punctation should be removed.
	toks <- get_tokens(textobj, lang, 
						tolower = tolower,
						remove_punct = remove_punct, 
						remove_stopwords = FALSE) 


	if(isTRUE(docname != "")) {
		docnames(toks) <- docname
	}
	
	df <- kwic(toks, pattern = pattern, window = window)
	df <- subset(df, select = -c(pattern, to))
	names(df)[names(df) == 'from'] <- 'from/to'
	return(df)
}


statistics_df <- function(textobj, 
	lang, 
	multiple_plays = FALSE, 
	remove_punct = TRUE, 
	tolower = TRUE, 
	remove_stopwords = FALSE, 
	lexical_diversity_measure = "TTR", 
	readability_measure = "Flesch") {
	#' Creates a dataframe with several statistics from a text object.
	#'
	#' Takes a text object and returns a dataframe with the following statistical
	#' properties from the text object: token counts, type count, stopword proportion,
	#' sentence count, numbers count, lexical diversity and text readability.
	#'
	#' @param textobj text as string.
	#' @param lang language identificator as string.
	#' @param multiple_plays boolean if multiple plays will be used.
	#' @param remove_punct boolean value if punctation should be removed.
	#' @param tolower boolean value if words should be converted to lowercase.
	#' @param remove_stopwords boolean value if stopwords should be removed.
	#' @param lexical_diversity_measure string which indicates a lexical diversity measure.
	#'		  All available options can be found here: 
	#'		  https://www.rdocumentation.org/packages/quanteda.textstats/versions/0.91/topics/textstat_lexdiv
	#' @param readability_measure string which indicates the text readability measure.
	#'		  All available options can be found here: 
	#'		  https://www.rdocumentation.org/packages/quanteda/versions/2.1.2/topics/textstat_readability

	
	if (isTRUE(multiple_plays)) {
		string_play_name <- "selected plays"
	} else {
		string_play_name <- "the play"
	}

	# token obj
	toks <- get_tokens(textobj, lang, tolower = tolower, remove_punct = remove_punct, remove_stopwords = remove_stopwords)
	toks_no_sw <- get_tokens(textobj, lang, tolower = tolower, remove_punct = remove_punct, remove_stopwords = TRUE)
	toks_sw <- get_tokens(textobj, lang, tolower = tolower, remove_punct = remove_punct, remove_stopwords = FALSE)


	## word counts ##
	tokens_count <- ntoken(toks)
	types_count <- ntype(toks)

	## hapax legomenon ##
	df <- freq_df(textobj, lang, remove_punct = remove_punct, tolower = tolower, remove_stopwords = remove_stopwords)
	hapax_legomenon <- nrow(subset(df, frequency==1))


	## proportion stop words ##
	toks_no_sw_count <- ntype(toks_no_sw)
	toks_sw_count <- ntype(toks_sw)

	count_stopwords <- toks_sw_count - toks_no_sw_count
	proportion_stopwords <- paste0("~",(round(count_stopwords / toks_sw_count, digits=3)*100),"%")


	## count sentences ##
	count_sentences <- nsentence(textobj)


	## count numbers ##
	count_numbers <- str_count(textobj, '\\d+')

	## lexical diversity ##
	lexical_diversity <- round(textstat_lexdiv(toks, 
								measure = get_lexical_diversity_name(lexical_diversity_measure))[[2]], 
								digits=3)

	## readability ##
	readability <- round(textstat_readability(textobj, measure = readability_measure)[[2]], digits=3)


	if (isTRUE(remove_punct) & isTRUE(remove_stopwords)) {
		string_length_play <- "<br><i>without punctuation and stop words</i>"
	} else if(isTRUE(remove_punct)) {
		string_length_play <- "<br><i>without punctuation</i>"
	} else if (isTRUE(remove_stopwords)) {
		string_length_play <- "<br><i>without stop words</i>"
	} else {
		string_length_play <- ""
	}


	tokens_count_name <- paste0("<b>", "Overall word count of ", string_play_name, " (= <u>token</u> count)", 
								"</b>", string_length_play)
	types_count_name <- paste0("<b>", "Unique word count of ", string_play_name, " (= <u>type</u> count)", 
										"</b>", string_length_play)
	hapax_legomenon_name <- paste0("<b>", "Count of Hapax legomenon in ", string_play_name, 
										"</b>", string_length_play)
	proportion_stopwords_name <- paste0("<b>", "Proportion of stop words in ", string_play_name, "</b>")
	count_sentences_name <- paste0("<b>", "Count of sentences in ", string_play_name, "</b>")
	count_numbers_name <- paste0("<b>", "Count of numbers in ", string_play_name, "</b>")
	lexical_diversity_name <- paste0("<b>", "Lexical diversity in ", string_play_name, "</b>")
	readability_name <- paste0("<b>", "Text readability of ", string_play_name, "</b>")

	index_col <- list(tokens_count_name, types_count_name, hapax_legomenon_name, proportion_stopwords_name, count_sentences_name, count_numbers_name, lexical_diversity_name, readability_name)
	result_col <- list(tokens_count, types_count, hapax_legomenon, proportion_stopwords, count_sentences, 
					 count_numbers, lexical_diversity, readability)

	df <- data.frame("name" = unlist(index_col), "result" = unlist(result_col))
	rownames(df) <- df$name
	df$name <- NULL

	return(df)
}


statistics_distribution <- function(l, 
	author_name, 
	measure, 
	text_stat_name = "Lexical diversity", 
	plot_method = "Line Plot", 
	plot_title_linebreak = FALSE, 
	plot_title_size = 22) {
	#' Creates a line plot with a specified measure (lexical diversity, text readability) 
	#' from a list of playnames.
	#'
	#' Takes a list of playnames and a specified measure (lexical diversity, text readability)
	#' and returns a line plot with the play names on the x-axis and the measures on the y-axis. 
	#' 
	#' @param l list with names of all plays of an author.
	#' @param author_name string with the name of the author.
	#' @param measure string which indicates a the measures name.
	#'		  All available options can be found here: 
	#'		  - https://www.rdocumentation.org/packages/quanteda.textstats/versions/0.91/topics/textstat_lexdiv
	#'		  - https://www.rdocumentation.org/packages/quanteda/versions/2.1.2/topics/textstat_readability
	#' @param text_stat_name string which indicates the text statistics name which should be plotted. 
	#' @param plot_method string which indicates plot method ("Line Plot" or "Bar chart").
	#' @param plot_title_linebreak boolean if plot title should contain linebreak.
	#' @param plot_title_size numeric which indicates size of plot title.

	n <- length(l)
	name_col <- str_trunc(names(unlist(l)), 20)
	value_col <- as.numeric(paste(unlist(l)))
	df <- data.frame("name" = name_col, "value" = value_col)


	# EXPAND: different base_size values for different corpora
	if(n < 5) {
		base_size <- 20
		point_size <- 4
		line_size <- 0.8
	} else if((n >= 5) & (n < 15)) {
		base_size <- 18
		point_size <- 3
		line_size <- 0.6
	} else if(n >= 15) {
		base_size <- 16
		point_size <- 2
		line_size <- 0.4
	} 
	if (isTRUE(plot_title_linebreak)) {
		lb <- "\n"
	} else {
		lb <- ""
	}

	# EXPAND: include option (e.g. checkbox) by which a fixed coordinate system can be selected
	
		
	if (isTRUE(plot_method == "Bar chart")) {
		g <- ggplot(data = df, 
				aes(x = name, y = value, group = 1)) + 
				geom_bar(stat='identity', color="#8DA0CB", fill="#8DA0CB") + 
				# EXPAND: reduce/expand spacing bewtween x-axis and line plot/bar chart?
				scale_y_continuous(expand = expansion(mult = c(0.01, .1))) + 
				labs(title = paste0(text_stat_name, " of the plays by ", lb, "'", author_name, "'"), 
						y = measure, x = "Play names") + 
				theme_bw(base_size=base_size) + 
				theme(axis.text.x = element_text(angle = 90, hjust = 1),
						plot.title = element_text(size=plot_title_size,
													margin=margin(0,0,10,0)))
	} else {
		g <- ggplot(data = df, 
				aes(x = name, y = value, group = 1)) + 
				geom_line(aes(group=1), size = line_size) + 
				geom_point(colour="#8DA0CB", size = point_size) +
				scale_y_continuous(limits = c(0, max(df$value) * 1.1)) +
				labs(title = paste0(text_stat_name, " of the plays by ", lb, "'", author_name, "'"), 
						y = measure, x = "Play names") + 
				theme_bw(base_size=base_size) + 
				theme(axis.text.x = element_text(angle = 90, hjust = 1),
						plot.title = element_text(size=plot_title_size,
													margin=margin(0,0,10,0)))
	}
	
	
	return(g)
}


trends_plays_plot <- function(corpusobj, 
	playnames,
	lang, 
	url, 
	remove_punct = TRUE,
	tolower = TRUE, 
	remove_stopwords = FALSE,
	author_name = "",
	frequency_method = "Relative Frequencies",
	target_word = "",
	plot_title_linebreak = FALSE,
	plot_title_size = 22) {
	#' Plots a trend plot of a target word in a selection of plays.
	#'
	#' Takes a corpus object, a list of plays, a target word and a frequency method
	#' and plots a trend plot of the selected target word within the combined plays.
	#'
	#' @param corpusobj shiny corpus object.
	#' @param playnames list of plays of an author.
	#' @param lang language identificator as string.
	#' @param url a string with an url to the plain text of an url.
	#' @param remove_punct boolean value if punctation should be removed.
	#' @param tolower boolean value if words should be converted to lowercase.
	#' @param remove_stopwords boolean value if stopwords should be removed.
	#' @param author_name string with the name of the author.
	#' @param frequency_method string name of the frequency method ("Relative Frequencies"
	#'		  or "Absolute Frequencies").
	#' @param target_word string which indicates the target word.
	#' @param plot_title_linebreak boolean if plot title should contain linebreak.
	#' @param plot_title_size numeric which indicates size of plot title.
	

	freq_list <- list()

	i <- 1
	for (play in playnames) {
		playtext <- get_text(corpusobj = corpusobj,
							url = url,
							play_name = play)
	
		frequency_df <- freq_df(playtext, lang, 
								tolower = tolower,
								remove_punct = remove_punct,
								remove_stopwords = remove_stopwords) 

		if(isTRUE(target_word %in% frequency_df$feature)) {
			target_freq <- frequency_df$frequency[frequency_df$feature == target_word]

			if(frequency_method == "Relative Frequencies") {
				toks <- get_tokens(playtext, 
									lang = lang,
									tolower = tolower,
									remove_punct = remove_punct,
									remove_stopwords = remove_stopwords)
				token_count <- ntoken(toks)
				target_freq <- target_freq / token_count
			}

			
		} else {
			target_freq <- 0
		}

		#to avoid errors with ".text" after playname 
		target_freq <- as.numeric(target_freq)
		
		playname <- names(playnames)[[i]]
		freq_list[[playname]] <- target_freq

		i <- i + 1
	}

	if(isTRUE(frequency_method == "Relative Frequencies")) {
		y_col_name <- "Relative Frequencies"
	} else {
		y_col_name <- "Absolute Frequencies"
	}


	n <- length(freq_list)
	name_col <- str_trunc(names(unlist(freq_list)), 20)
	freq_col <- as.numeric(paste(unlist(freq_list)))
	df <- data.frame("name" =  name_col, "frequency" = freq_col)


	if(n < 5) {
		base_size <- 20
		point_size <- 4
		line_size <- 0.8
	} else if((n >= 5) & (n < 10)) {
		base_size <- 18
		point_size <- 3
		line_size <- 0.6
	} else if((n >= 10) & (n < 15)) {
		base_size <- 16
		point_size <- 2
		line_size <- 0.4
	} else if(n >= 15) {
		base_size <- 14
		point_size <- 1
		line_size <- 0.3
	} 

	if (isTRUE(plot_title_linebreak)) {
		lb <- "\n"
	} else {
		lb <- ""
	}

	plot_title <- paste0("The word '", target_word, "' in the selected plays ", lb, 
						"by '", author_name, "'")

	g <- ggplot(data = df, aes(x = name, y = frequency)) + 
			geom_line(aes(group=1), size = line_size) + 
			geom_point(colour="#8DA0CB", size = point_size) +
			labs(title = plot_title, y = y_col_name, x = "Selected plays") + 
			scale_y_continuous(limits = c(0, max(df$frequency) * 1.5)) + 
			theme_bw(base_size=base_size) + 
			theme(axis.text.x = element_text(angle = 90, hjust = 1),
					plot.title = element_text(size=plot_title_size,
												margin=margin(0,0,10,0)))

	return(g)
}


trends_segments_plot <- function(textobj, 
	lang, 
	url, 
	remove_punct = TRUE,
	tolower = TRUE, 
	remove_stopwords = FALSE,
	n_segments = 10,
	author_name = "",
	all_playnames = "",
	target_word = "",
	play_name = "",
	frequency_method = "Absolute Frequencies",
	plot_title_linebreak = FALSE,
	plot_title_size = 22) {
	
	#' Plots a trend plot of a target word in segments of a given play.
	#'
	#' Takes a text object, a number of segments, a target word and a frequency method
	#' and plots a trend plot of the selected target word within the different segments.
	#'
	#' @param textobj text as string.
	#' @param lang language identificator as string.
	#' @param url a string with an url to the plain text of an url.
	#' @param remove_punct boolean value if punctation should be removed.
	#' @param tolower boolean value if words should be converted to lowercase.
	#' @param remove_stopwords boolean value if stopwords should be removed.
	#' @param n_segments numerical value which indicates number of segments.
	#' @param author_name string with the name of the author.
	#' @param all_playnames list with names of all plays of the author.
	#' @param target_word string which indicates the target word.
	#' @param play_name string name of the play.
	#' @param frequency_method string name of the frequency method ("Relative Frequencies"
	#'		  or "Absolute Frequencies").
	#' @param plot_title_linebreak boolean if plot title should contain linebreak.
	#' @param plot_title_size numeric which indicates size of plot title.

	playnames <- get_playnames(all_playnames, play_name) 
	playname <- names(unlist(playnames))[1]

	toks <- get_tokens(textobj, 
						lang = lang,
						tolower = tolower,
						remove_punct = remove_punct,
						remove_stopwords = remove_stopwords)

	token_count <- ntoken(toks)


	segment_size <- ceiling(token_count / n_segments)
	tok_segments <- tokens_chunk(toks, size = segment_size)
	target_word_counts <- list()
	segments_list <- seq(1:length(tok_segments))

	i <- 1
	for(element in segments_list) {

		frequency_df <- freq_df(tok_segments[paste0("text1.", i)], 
							lang, 
							tolower = tolower,
							remove_punct = remove_punct,
							remove_stopwords = remove_stopwords) 

		target_freq <- frequency_df$frequency[frequency_df$feature == target_word]

		# check for numeric(0)
		if(length(target_freq) == 0) {
			target_freq <- 0
		}

		target_word_counts[[i]] <- target_freq
		i <- i + 1
	}

	df <- data.frame("segment" = segments_list, "frequency" = unlist(target_word_counts))


	# absolute or relative frequencies
	if(isTRUE(frequency_method == "Relative Frequencies")) {
		y_col_name <- "Relative Frequencies"
		df["frequency"] <- lapply(df["frequency"], function(x) x/token_count)
	} else {
		y_col_name <- "Absolute Frequencies"
	}

	
	### plotting ###

	if(n_segments <= 5) {
		base_size <- 20
		point_size <- 4
		line_size <- 0.8
	} else if((n_segments > 5) & (n_segments <= 20)) {
		base_size <- 18
		point_size <- 3
		line_size <- 0.6
	} else if((n_segments > 20) & (n_segments <= 30)) {
		base_size <- 16
		point_size <- 2
		line_size <- 0.4
	} else if(n_segments > 30) {
		base_size <- 14
		point_size <- 1
		line_size <- 0.3
	}


	if (isTRUE(plot_title_linebreak)) {
		lb <- "\n"
	} else {
		lb <- ""
	}


	plot_title <- paste0("The word '", target_word, "' in the segments ", lb, 
					"of the play '", playname, "' by '", author_name, "'")
	
	
	g <- ggplot(data = df, aes(x = segment, y = frequency)) + 
			geom_line(aes(group = 1), size = line_size) +
			geom_point(colour = "#8DA0CB", size = point_size) +
			labs(title = plot_title, y = y_col_name, x = "Segments") + 
			scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) + 
			scale_y_continuous(limits = c(0, max(df$frequency) * 1.5)) +
			theme_bw(base_size = base_size) + 
			theme(plot.title = element_text(size=plot_title_size,
											margin=margin(0,0,10,0)))
	

	return(g)
}


wordcloudplot <- function(textobj, lang, 
	remove_punct = TRUE,
	tolower = TRUE, 
	remove_stopwords = FALSE,
	colors = c('red', 'pink', 'green', 'purple', 'orange', 'blue'),
	comparison = FALSE,
	labeloffset = 0,
	labelsize = 3.5,
	max_words = 20,
	min_freq = 5,
	word_padding = 0) {
	#' Creates a wordcloud from a plain text or a quanteda corpus object.
	#'
	#' Takes a plain text or quanteda corpus object, computes a document-term matrix
	#' and creates a wordcloud from it.
	#'
	#' @param textobj text as string.
	#' @param lang language identificator as string.
	#' @param remove_punct boolean value if punctation should be removed.
	#' @param tolower boolean value if words should be converted to lowercase.
	#' @param remove_stopwords boolean value if stopwords should be removed.
	#' @param colors vector which indicates the color palette for the word frequencies.
	#' @param comparison boolean value if comparison word cloud should be plotted.
	#'					 Does only work with a quanteda corpus object.
	#' @param labeloffset numeric value which indicates the wordcloud label padding
	#'					  or offset. Does only work together with @param comparison.
	#' @param labelsize numeric value which indicates the wordcloud label size.
	#'					Does only work together with @param comparison.
	#' @param max_words numeric value which indicates the possible maximum of words
	#'					which will be plotted.
	#' @param min_freq numeric value which indicates how high the frequency of a word
	#' 				   must be in order to be plotted
	#' @param word_padding numeric value which indicates the padding space 
	#'					   between the words. Is multiplied by -1 so that a 
	#'					   bigger value indicates a higher padding space.
	df <- documenttermdf(textobj, lang, 
						 remove_punct = remove_punct,
					   	 tolower = tolower, 
					   	 remove_stopwords = remove_stopwords)

	return(textplot_wordcloud(df,
							  adjust = word_padding * -1,
							  color = colors,
							  comparison = comparison,
							  labeloffset = labeloffset,
							  labelsize = labelsize,
							  max_words = max_words,
							  min_count = min_freq))
}



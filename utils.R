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
library(wordcloud2)
library(XML)


#################
# api functions #
#################

extractdramas <- function(urlcorpus) {
	#' Extracts dramas from corpus.
	#'
	#' Takes an url to a corpus json file and extracs the "dramas" entry.
	#'
	#' @param urlcorpus an url to a corpus.
	return(fromJSON(urlcorpus, flatten = TRUE)$dramas)
}



selectauthors <- function(corpus) {
	#' Selects list of authors from corpus.
	#'
	#' Takes a corpus and returns a list of all the included authors.
	#'
	#' @param corpus a list of informations/contents of the selected corpus.
	authors <- unique(corpus$author.name)
	names(authors) <- unique(corpus$author.name)
	return(authors)
}

selectcorpora <- function(urlcorpora) {
	#' Selects list of corpora from url.
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
	#' Selects list of all plays from an author within a corpus.
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


#############################
# text processing functions #
#############################

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

documenttermdf <- function(textobj, lang, 
						   remove_punct = TRUE,
						   tolower = TRUE, 
						   remove_stopwords = FALSE) {
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

	return(df)
}


get_lexical_diversity_name <- function(lexical_diversity_measure) {
	#' Returns correct lexical diversity measure string.
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

get_stopwords <- function(lang) {
	#' Get a list of stopwords by its language.
	#'
	#' Takes a language string and returns the correct stopword list.
	#'
	#' @param lang a string for a language.
	output <- lang
	src <- "stopwords-iso"

	# EXPAND: for more corpora
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
		df <- wordfreqdf(textobj, lang, 
							remove_punct = remove_punct,
							tolower = tolower, 
							remove_stopwords = remove_stopwords)
		toks <- c(df[["feature"]])
	# most frequent words + their frequency in parantheses in descending order
	} else if (isTRUE(sort_by == "mfw+")) {
		df <- wordfreqdf(textobj, lang, 
							remove_punct = remove_punct,
							tolower = tolower, 
							remove_stopwords = remove_stopwords)
		
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

##################
# tool functions #
##################


frequency_distribution <- function(textobj, lang, 
								   remove_punct = TRUE,
						  		   tolower = TRUE, 
						  		   remove_stopwords = FALSE,
						  		   top_n_features = 100,
						  		   color_point = "#8DA0CB") {
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
	#' @param top_n_features numerical value which indicates the top n features.
	#' @param color_point hex code as string which indicates the color of the points.
	df <- wordfreqdf(textobj, lang, 
					 remove_punct = remove_punct,
					 tolower = tolower, 
					 remove_stopwords = remove_stopwords,
					 top_n_features = top_n_features)

	df$feature <- with(df, reorder(feature, -frequency))


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

	g <- ggplot(data = df, 
				aes(x = feature, y = frequency)) + 
				geom_point(colour=color_point, size=size) + 
				scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
				theme_bw(base_size=base_size) + 
				theme(axis.text.x = element_text(angle = 90, hjust = 1))

	return(g)
}


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




statistics_df <- function(textobj, lang, 
							all_plays = FALSE,
							remove_punct = TRUE,
							tolower = TRUE, 
							remove_stopwords = FALSE,
							lexical_diversity_measure = "TTR",
							readability_measure = "Flesch") {
	#' Creates a dataframe with several statistics from a text object
	#'
	#' Takes a text object and returns a dataframe with the following statistical
	#' properties from the text object: token counts, type count, stopword proportion,
	#' sentence count, numbers count, lexical diversity and text readability.
	#'
	#' @param textobj text as string.
	#' @param lang language identificator as string.
	#' @param remove_punct boolean value if punctation should be removed.
	#' @param tolower boolean value if words should be converted to lowercase.
	#' @param remove_stopwords boolean value if stopwords should be removed.
	#' @param lexical_diversity_measure string which indicates a lexical diversity measure.
	#'		  All available options can be found here: 
	#'		  https://www.rdocumentation.org/packages/quanteda.textstats/versions/0.91/topics/textstat_lexdiv
	#' @param readability_measure string which indicates the text readability measure.
	#'		  All available options can be found here: 
	#'		  https://www.rdocumentation.org/packages/quanteda/versions/2.1.2/topics/textstat_readability

	

	if (isTRUE(all_plays)) {
		string_play_name <- "all plays"
	} else {
		string_play_name <- "the play"
	}
	


	# stats summary
	toks_no_sw <- get_tokens(textobj, lang,
						tolower = tolower,
						remove_punct = remove_punct,
						remove_stopwords = TRUE)
	stats_summary_no_sw <- textstat_summary(toks_no_sw)

	toks_sw <- get_tokens(textobj, lang,
						tolower = tolower,
						remove_punct = remove_punct,
						remove_stopwords = FALSE)
	stats_summary_sw <- textstat_summary(toks_sw)

	if(isTRUE(remove_stopwords)) {
		toks <- toks_no_sw
		stats_summary <- stats_summary_no_sw
	} else {
		toks <- toks_sw
		stats_summary <- stats_summary_sw
	}

	

	## word counts ##
	tokens_count <- stats_summary[["tokens"]]
	types_count <- stats_summary[["types"]]


	## proportion stop words ##
	count_stopwords <- stats_summary_sw[["types"]] - stats_summary_no_sw[["types"]]
	proportion_stopwords <- paste0("~",(round(count_stopwords / stats_summary_sw[["types"]], digits=3)*100),"%")

	## count sentences ##
	count_sentences <- nsentence(textobj)

	## count numbers ##
	count_numbers <- stats_summary[["numbers"]]



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
	proportion_stopwords_name <- paste0("<b>", "Proportion of stop words in ", string_play_name, "</b>")
	count_sentences_name <- paste0("<b>", "Count of sentences in ", string_play_name, "</b>")
	count_numbers_name <- paste0("<b>", "Count of numbers in ", string_play_name, "</b>")
	lexical_diversity_name <- paste0("<b>", "Lexical diversity in ", string_play_name, "</b>")
	readability_name <- paste0("<b>", "Text readability of ", string_play_name, "</b>")

	
	index_col <- c(tokens_count_name, types_count_name, proportion_stopwords_name, 
		count_sentences_name, count_numbers_name, lexical_diversity_name, readability_name)
	result_col <- c(tokens_count, types_count, proportion_stopwords, count_sentences, 
					 count_numbers, lexical_diversity, readability)

	df <- data.frame("name" = index_col, "result" = result_col)
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
	#' @param l list with names of all plays of an author
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
	name_col <- stringr::str_trunc(names(unlist(l)), 20)
	value_col <- as.numeric(paste(unlist(l)))
	df <- data.frame("name" =  name_col, "value" = value_col)


	# EXPAND: different base_size values for different corpora
	if(n < 5) {
		base_size <- 20
	} else if((n >= 5) & (n < 10)) {
		base_size <- 17
	} else if(n >= 10) {
		base_size <- 14
	}
	if (isTRUE(plot_title_linebreak)) {
		lb <- "\n"
	} else {
		lb <- ""
	}

	# EXPAND: include option (e.g. checkbox) with which a fixed coordinate system can be selected
	
		
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
				geom_line(size=1) + 
				labs(title = paste0(text_stat_name, " of the plays by ", lb, "'", author_name, "'"), 
						y = measure, x = "Play names") + 
				theme_bw(base_size=base_size) + 
				theme(axis.text.x = element_text(angle = 90, hjust = 1),
						plot.title = element_text(size=plot_title_size,
													margin=margin(0,0,10,0)))
	}
	
	
	return(g)
}


wordcloudplot <- function(textobj, lang, 
						  remove_punct = TRUE,
						  tolower = TRUE, 
						  remove_stopwords = FALSE,
						  colors = c('red', 'pink', 'green', 
							  		'purple', 'orange', 'blue'),
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

wordfreqdf <- function(textobj, lang, 
					   remove_punct = TRUE,
					   tolower = TRUE, 
					   remove_stopwords = FALSE,
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
	#' @param top_n_features numerical value which indicates the top n features.
	

	df <- documenttermdf(textobj, lang, 
						 remove_punct = remove_punct,
					   	 tolower = tolower, 
					   	 remove_stopwords = remove_stopwords)
	

	return(subset(textstat_frequency(df, n=top_n_features), select=-c(rank, group)))
}



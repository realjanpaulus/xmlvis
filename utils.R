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


# todo
get_tokens <- function(textobj,
						lang,
						tolower = TRUE,
						remove_punct = TRUE,
						remove_stopwords = TRUE,
						sort_by = "") {
	#'
	#' TODO
	#'
	toks <- tokens(textobj, remove_punct = remove_punct)
	if (isTRUE(tolower)) {
		toks <- tokens_tolower(toks)
	}

	if(isTRUE(remove_stopwords)) {
		toks <- tokens_remove(toks, pattern = get_stopwords(lang))
	}

	if(isTRUE(sort_by == "alphabetical")) {
		# todo: hier gibts fehlermeldung
		toks <- str_sort(c(toks), numeric = TRUE)
	} else if (isTRUE(sort_by == "frequency")) {
		#todo
		toks <- toks
	}
	return(toks)
}

##################
# tool functions #
##################

count_words <- function(textobj, lang,
						remove_punct = TRUE,
						tolower = TRUE, 
						remove_stopwords = FALSE,
						unique_words = FALSE) {
	#'
	#' TODO
	#' 

	tokenized_text <- get_tokens(textobj, lang, 
								tolower = tolower,
								remove_punct = remove_punct, 
								remove_stopwords = remove_stopwords)
	if(isTRUE(unique_words)) {
		return(ntype(tokenized_text))
	} else {
		return(ntoken(tokenized_text))
	}
	
}

frequency_distribution <- function(textobj, lang, 
								   remove_punct = TRUE,
						  		   tolower = TRUE, 
						  		   remove_stopwords = FALSE,
						  		   top_n_features = 100,
						  		   color_point ="#8DA0CB") {
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

#todo
kwic_df <- function(textobj,
					lang,
					pattern,
					remove_punct = TRUE,
					tolower = TRUE) {
	#'
	#' TODO: remove stopwords und so wieder einfügen?
	#'
	toks <- get_tokens(textobj, lang, remove_punct = remove_punct, tolower = tolower)
	
	df <- kwic(toks, pattern = pattern)
	df <- subset(df, select = -c(pattern))
	return(df)
}


#todo
statistics_df <- function(textobj, lang, 
							all_plays = FALSE,
							remove_punct = TRUE,
							tolower = TRUE, 
							remove_stopwords = FALSE,
							unique_words = FALSE) {
	#'
	#' TODO
	#'


	# TODO: INCLUDE
	# - Vocabularity Density
	# - Average Words Per Sentence

	toks <- get_tokens(textobj, lang,
						tolower = tolower,
						remove_punct = remove_punct,
						remove_stopwords = remove_stopwords)

	if (isTRUE(all_plays)) {
		string_play_name <- "all plays"
	} else {
		string_play_name <- "the play"
	}

	## word counts ##
	word_count <- count_words(textobj,
								lang,
								remove_punct = remove_punct,
								tolower = tolower, 
								remove_stopwords = remove_stopwords,
								unique_words = FALSE)
	unique_word_count <- count_words(textobj,
								lang,
								remove_punct = remove_punct,
								tolower = tolower, 
								remove_stopwords = remove_stopwords,
								unique_words = TRUE)


	## proportion stop words ##
	word_count_with_stopwords <- count_words(textobj, lang, 
												tolower=TRUE,
												remove_stopwords=FALSE,
												unique_words=TRUE)
	word_count_no_stopwords	<- count_words(textobj, lang, 
											tolower=TRUE,
											remove_stopwords=TRUE,
											unique_words=TRUE)
	count_stopwords <- word_count_with_stopwords - word_count_no_stopwords
	proportion_stopwords <- paste0("~",round(count_stopwords / word_count_with_stopwords, digits=2),"%")


	## lexical diversity ##

		

	# TODO: hier weiter, das in tabelle implementieren
	# TODO: auch das angucken: https://towardsdatascience.com/linguistic-complexity-measures-for-text-nlp-e4bf664bd660

	lexical_diversity <- textstat_lexdiv(toks, measure = "TTR")
	print(lexical_diversity)


	if (isTRUE(remove_punct) & isTRUE(remove_stopwords)) {
		string_length_play <- "<br><i>without punctuation and stop words</i>"
	} else if(isTRUE(remove_punct)) {
		string_length_play <- "<br><i>without punctuation</i>"
	} else if (isTRUE(remove_stopwords)) {
		string_length_play <- "<br><i>without stop words</i>"
	} else {
		string_length_play <- ""
	}


	word_count_name <- paste0("<b>", "Overall word count of ", string_play_name, " (= <u>token</u> count)", 
								"</b>", string_length_play)
	unique_word_count_name <- paste0("<b>", "Unique word count of ", string_play_name, " (= <u>type</u> count)", 
										"</b>", string_length_play)
	proportion_stopwords_name <- paste0("<b>", "Proportion of stop words in ", string_play_name, "</b>")

	#todo erweiteren!!
	index_col <- c(word_count_name, unique_word_count_name, proportion_stopwords_name)
	result_col <- c(word_count, unique_word_count, proportion_stopwords)

	df <- data.frame("name" = index_col, "result" = result_col)
	rownames(df) <- df$name
	df$name <- NULL

	return(df)
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



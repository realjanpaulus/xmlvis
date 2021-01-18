library(curl)
library(docstring, warn.conflicts = FALSE)
library(DT)
library(jsonlite, warn.conflicts = FALSE)
library(methods)
library(quanteda, warn.conflicts = FALSE)
library("quanteda.textstats", warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(shinythemes)
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

getplaytext <- function(texturl) {
	#' Extracts the play text by an url.
	#'
	#' Takes an url to the plain text of an url and extracts the text.
	#'
	#' @param texturl a string with an url to the plain text of an url.

	return(content(GET(texturl), "text", encoding="UTF-8"))
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
	#' @param playtext text of the play as string.
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
	#' @param playtext text of the play as string.
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
							  comparison = comparison,
							  adjust = word_padding * -1,
							  labeloffset = labeloffset,
							  labelsize = labelsize,
							  max_words = max_words,
							  min_count = min_freq,
							  color = colors))
}

wordfreqdf <- function(textobj, lang, remove_punct = TRUE,
					   tolower = TRUE, remove_stopwords = FALSE) {
	#' Extracts term and document frequencies from a plain text or a quanteda corpus object.
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
	

	df <- documenttermdf(textobj, lang, 
						 remove_punct = remove_punct,
					   	 tolower = tolower, 
					   	 remove_stopwords = remove_stopwords)
	

	return(subset(textstat_frequency(df), select=-c(rank, group)))
}



#####################
# todo weg fucntions#
#####################



# TODO: weg?
countwords <- function(texturl, word) {
	#' TODO
	s <- getplaytext(texturl)
	wordlist <- unlist(strsplit(s, " "))
	return(length(grep(word, wordlist, ignore.case = TRUE)))
}




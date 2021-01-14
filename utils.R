#TODOs:
# - sortieren der funktionen alphabetisch und thematisch

library(curl)
library(docstring, warn.conflicts = FALSE)
library(DT)
library(jsonlite, warn.conflicts = FALSE)
library(methods)
library(quanteda, warn.conflicts = FALSE)
library("quanteda.textstats", warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(shinythemes)
library(XML)


extractdramas <- function(urlcorpus) {
	#' Extracts dramas from corpus.
	#'
	#' Takes an url to a corpus json file and extracs the "dramas" entry.
	#'
	#' @param urlcorpus an url to a corpus.
	return(fromJSON(urlcorpus, flatten = TRUE)$dramas)
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


selectplays <- function(corpus, input = input) {
	#' Selects list of all plays from an author within a corpus.
	#' 
	#' Takes a corpus and an input (= author name) and returns a list of the authors plays.
	#'
	#' @param corpus a list of informations/contents of the selected corpus.
	#' @param input the name of the selected author.
	links <- corpus[corpus$author.name == input, "name"]
	names(links) <- corpus[corpus$author.name == input, "title"]
	return(links[order(names(links))])
}



getplaytext <- function(texturl) {
	#' Extracts the play text by an url.
	#'
	#' Takes an url to the plain text of an url and extracts the text.
	#'
	#' @param texturl a string with an url to the plain text of an url.

	return(content(GET(texturl), "text", encoding="UTF-8"))
} 


# TODO: weg?
countwords <- function(texturl, word) {
	#' TODO
	s <- getplaytext(texturl)
	wordlist <- unlist(strsplit(s, " "))
	return(length(grep(word, wordlist, ignore.case = TRUE)))
}


# todo: weg (aktuell keine funktion)
langchecker <- function(lang) {
	

	# EXPAND: for more corpora
	output <- lang
	if (lang == "ger") {
		output <- "german"
	}

	return(output)
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



add_dfs <- function(df1, df2) {
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


wordfreqdf <- function(playtext, lang, remove_punct = TRUE,
					   tolower = TRUE, remove_stopwords = FALSE) {
	#' Extracts term and document frequencies from a plain text.
	#'
	#' Takes plain text of a play and creates a dataframe
	#' with term and document frequencies of the text. 
	#' Columns "rank" and "group" are removed.
	#'
	#' @param playtext text of the play as string.
	#' @param lang language identificator as string.
	#' @param remove_punct boolean value if punctation should be removed.
	#' @param tolower boolean value if words should be converted to lowercase.
	#' @param remove_stopwords boolean value if stopwords should be removed.
	

	if (isTRUE(remove_stopwords)) {
		df <- dfm(playtext, 
				  remove_punct = remove_punct, 
				  tolower = tolower,
				  remove = get_stopwords(lang))
	} else {
		df <- dfm(playtext, 
				  remove_punct = remove_punct, 
				  tolower = tolower)
	}
	

	return(subset(textstat_frequency(df), select=-c(rank, group)))
}








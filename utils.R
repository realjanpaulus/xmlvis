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


langchecker <- function(lang) {
	#' Get the correct language identifier.
	#'
	#' Takes a language string and returns correct language string.
	#'
	#' @param lang a string for a language.

	# EXPAND: for more corpora
	languages <- list("ger"="german", "german"="german",
					  "shake"="english", "english"="english")

	return(languages$lang)
}


wordfreqdf <- function(texturl, lang, remove_stopwords = FALSE) {
	#' Extracts term and document frequencies from a plain text by its url.
	#'
	#' Takes an url to the plain text of an url and creates a dataframe
	#' with term and document frequencies of the text.
	#'
	#' @param texturl a string with an url to the plain text of an url.
	# todo
	playtext <- getplaytext(texturl)
	if (remove_stopwords) {
		df <- dfm(playtext, remove_punct = TRUE, remove = stopwords(langchecker(lang)))
	} else {
		df <- dfm(playtext, remove_punct = TRUE)
	}
	
	return(textstat_frequency(df))
}








library(curl)
library(docstring, warn.conflicts = FALSE)
library(jsonlite, warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(shinythemes)


extractdramas <- function(urlcorpus) {
	#' Extracts dramas from corpus.
	#'
	#' Takes an url to a corpus json file and extracs the "dramas" entry.
	#'
	#' @param urlcorpus an url to a corpus.
	return(fromJSON(urlcorpus, flatten = TRUE)$dramas)
}


selectcorpora <- function(urlcorpora){
	#' Selects list of corpora from url.
	#'
	#' Takes an url to a json file with corpora infos 
	#' and collects all names in a list.
	#'
	#' @param urlcorpora an url to all corpora
	corpora <- fromJSON(urlcorpora)
	corporanames <- corpora$name
	names(corporanames) <- corpora$title
	return(as.list(corporanames))
}

selectauthors <- function(corpus){
	#' TODO
	#'
	#'
	#'
	#' @param corpus 
	authors <- unique(corpus$author.name)
	names(authors) <- unique(corpus$author.name)
	return(authors)
}

selectplays <- function(corpus, input = input){
	#' TODO
	links <- corpus[corpus$author.name == input, "networkdataCsvUrl"]
	names(links) <- corpus[corpus$author.name == input,"title"]
	return(links[order(names(links))])
}
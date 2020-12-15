library(curl)
library(docstring, warn.conflicts = FALSE)
library(jsonlite, warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(shinythemes)


# dracor corpora urls TODO: andere namen?
urlcorpus <- "https://dracor.org/api/corpora/ger"
urlcorpora <- 'https://dracor.org/api/corpora'
urlcorporashort <- "https://dracor.org/api/corpora/"

extractdramas <- function(urlcorpus) {
	#' Extract dramas from corpus
	#'
	#' Takes an url to a corpus json file and extracs the "dramas" entry.
	#'
	#' @param urlcorpus an url to a corpus
	return(fromJSON(urlcorpus, flatten = TRUE)$dramas)
}

dramas <- extractdramas(urlcorpus)
print(dramas$id)

# selectcorpus <- function(urlcorpora){
# 	#'
# 	#'
# 	#'
# 	#'
# 	#' @param urlcorpora an url to all corpora
# 	corpora <- fromJSON(urlcorpora)
# 	corporanames <- corpora$name
# 	names(corporanames) <- corpora$title
# 	as.list(corporanames)
# }




# # Define UI ----
# ui <- fluidPage(theme = shinytheme("united"),
#   headerPanel("XMLvis: A visualization of XML corpora"),
   
#    sidebarLayout(
#       sidebarPanel(
#         fileInput(inputId = "corpus",
#                   label = "Choose a corpus",
#                   multiple = TRUE,
#                   # TODO: mehr akzeptieren
#                   accept = c('.txt', 'text/plain'))
#         ),
#    mainPanel("Main Panel")
#    )
# )



# # Define server logic ----
# server <- function(input, output) {
  
# }

# # Run the app
# options(shiny.port = 3000)
# options(shiny.launch.browser = TRUE)
# shinyApp(ui = ui, server = server)
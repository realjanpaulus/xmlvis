# XMLvis: A visualization of XML corpora

**XMLvis** is a visualization and corpus insight tool for the corpora of the <a href="https://dracor.org/">DraCor</a> project.

The following table shows the supported corpora. Currently only the **German Drama Corpus** is supported.

| DraCor corpora | supported | 
| -------------- | ----------| 
| Alsatian | :x:  |
| Caldéron | :x: | 
| German | :heavy_check_mark: |
| Greek | :x: | 
| Italian | :x: | 
| Roman | :x: | 
| Russian | :x: | 
| Shakespeare | :x: | 
| Spanish | :x: | 
| Swedish | :x: | 
| Tatar | :x: | 



## Requirements

The following packages are required. To install the requirements, copy + paste the following lines into an R environment:

```

install.packages(curl)
install.packages(docstring)
install.packages(DT)
install.packages(ggplot2)
install.packages(jsonlite)
install.packages(httr)
install.packages(methods)
install.packages(plyr)
install.packages(quanteda)
install.packages("quanteda.textplots")
install.packages("quanteda.textstats")
install.packages(RColorBrewer)
install.packages(shiny)
install.packages(shinycssloaders)
install.packages(shinythemes)
install.packages(shinyWidgets)
install.packages(stringr)

```

## Getting started

### Online usage

You can access XMLvis without installation right from your browser following [this link]( https://realjanpaulus.shinyapps.io/xmlvis/). The first time you open the page, it may take a few seconds for all the content to load.

### Offline usage

To start the application in a Terminal, change your directory to `xmlvis` and run the file `app.R`:

```
$ Rscript app.R
```
# psyTeachR styles and functions
# do not edit!!!!!

library(tidyverse)


# default knitr options
knitr::opts_chunk$set(
  echo       = TRUE,
  results    = "hold",
  out.width = '100%',
  fig.width  = 8, 
  fig.height = 5, 
  fig.align = 'center',
  fig.cap='**CAPTION THIS FIGURE!!**'
)

# make docs directory and include .nojekyll file for github
if (!dir.exists('docs')) dir.create('docs')
file.create('docs/.nojekyll')

## set global theme options for figures
theme_set(theme_bw())

## set class for a chunk using class="className"
knitr::knit_hooks$set(class = function(before, options, envir) {
  if (before) {
    sprintf("<div class = '%s'>", options$class)
  } else {
    "</div>"
  }
})

## verbatim code chunks
knitr::knit_hooks$set(verbatim = function(before, options, envir) {
  if (before) {
    sprintf("<div class='verbatim'><code>&#96;&#96;&#96;{%s}</code>", options$verbatim)
  } else {
    "<code>&#96;&#96;&#96;</code></div>"
  }
})

## verbatim inline R in backticks
backtick <- function(code) {
  code <- gsub("\\$", "\\\\$", code) 
  paste0("<code>&#096;", code, "&#096;</code>")
}


myglossary <- list()

## link to glossary with shortdef on hover
glossary <- function(term, display = NULL, shortdef = "", link = TRUE) {
  lcterm <- gsub(" ", "-", tolower(term), fixed = TRUE)
  if (is.null(display)) display <- term
  first_letter <- substr(lcterm, 1, 1)
  url <- paste0("https://psyteachr.github.io/glossary/", first_letter)
  if (shortdef == "") {
    hash <- paste0("#", lcterm, " dfn")
    shortdef <- xml2::read_html(url) %>% 
      rvest::html_node(hash) %>%
      rvest::html_text() %>%
      gsub("\'", "&#39;", .)
  }
  
  ## add to global glossary for this book
  myglossary[lcterm] <<- shortdef
  
  if (link) {
    paste0("<a class='glossary' target='_blank' title='", shortdef, 
           "' href='", url, "#", lcterm, "'>", display, "</a>")
  } else {
    paste0("<a class='glossary' title='", shortdef, "'>", display, "</a>")
  }
}

glossary_table <- function() {
  as.data.frame(myglossary) %>% 
    t() %>% 
    as.data.frame() %>%
    rownames_to_column(var="term") %>%
    rename("definition" = V1) %>%
    mutate(term = paste0("<a class='glossary' target='_blank' ",
                         "href='https://psyteachr.github.io/glossary/",
                         substr(term, 1, 1), "#", term, "'>",
                         gsub(".", " ", term, fixed = 1), "</a>")) %>%
    arrange(term) %>%
    knitr::kable(escape = FALSE)
}

## palette with psyTeachR logo colour
psyteachr_colours <- function(vals = 1:6) {
  ptrc <- c(
    "pink" = "#983E82",
    "orange" = "#E2A458",
    "yellow" = "#F5DC70",
    "green" = "#59935B",
    "blue" = "#467AAC",
    "purple" = "#61589C"
  )
  
  unname(ptrc[vals])
}
psyteachr_colors <- psyteachr_colours
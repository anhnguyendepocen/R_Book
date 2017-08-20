#' ---
#' title:     "Notes on R for Statistics"
#' subtitle:  "Chapter 00 Installing R and RStudio"
#' date:      "August 2016"
#' author:    "Georges Monette"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: false
#'     theme: readable
#'     fig.width: 12
#'     fig.height: 10
#' bibliography: mm.bib
#' link-citations: yes
#' ---
#' Updated: `r format(Sys.time(), '%B %d %Y %H:%M')`
#' 
#+ include=FALSE
knitr::opts_chunk$set(comment="  ", error = TRUE)
if(.Platform$OS.type == 'windows') windowsFonts(Arial=windowsFont("TT Arial")) 
interactive <- FALSE  # do not run interactive code
#'
library(shiny)
library(imdb)
library(tidyverse)
library(cluster)

source("R/auxfn.R")
source("R/ui.R")
source("R/server.R")

shinyApp(ui, server)

library(esquisse)

## set global options
options(shiny.maxRequestSize=100*1024^3) #100G
options(encoding = "UTF-8")
options(stringsAsFactors = FALSE)
rm(list=ls())

## source function
source('./R/plot.R')
source('./R/dat_format.R')

shinyApp(ui, server)
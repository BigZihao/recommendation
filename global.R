
library(magrittr)
library(shiny)
library(dplyr)
library(ggplot2)
library(gam)
library(car)
library(mgcv)
library(splines)
library(plotly)
library(rbokeh)
library(effects)
library(reshape2)
library(scales)
library(corrplot)
library(xts)
library(dygraphs)
library(plotly)
library(tidyr)
library(VIM)
library(mice)
library(chorddiag)
library(shinythemes)


final6=readRDS("data/Datatoshiny1201.rds")

b2=b1=final6 %>% filter(Product_Code==  "CXB12")


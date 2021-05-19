#01_load
#load packages
library(tidyverse)
library(readxl)
library(ggrepel)
library(cowplot)
library(readxl)
library(patchwork)
library(scales)

#load data

D <- read_excel("MASTER copy survey2020 covid19.xlsx", 
                sheet = "Quantitative coded data")
#Convert character to numeric
D<-mutate_if(D, is.character,as.numeric)

#Add some small functions
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))} #useful for the ordering
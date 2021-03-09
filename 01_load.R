#01_load_data
library(tidyverse)
library(readxl)
library(ggrepel)
library(cowplot)
library(readxl)
library(patchwork)
library(scales)

D <- read.csv("C:/Users/MFontela/Nextcloud/OYSTER/Survey2020/Quant_folder_20210219/Datasheets/Quant_mf.csv", sep=";") #quantitative data
rm(list = ls())

source('utils.R')
load_libs("terra")

path <- "G:/Meine Ablage/GEE_Export"

all_files <- list()

for (i in list.files(path)){
  paste0(path, "/", i)
}

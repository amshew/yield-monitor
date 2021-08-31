rm(list=ls()) # clear env

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  curl
)

id <- "" # Google file ID
sURL <- sprintf("https://docs.google.com/uc?id=%s&export=download", id) # google drive export/download link
con <- curl(sURL) # open the drive connection to R
dat <- read.csv(con) # read in the csv via the connection

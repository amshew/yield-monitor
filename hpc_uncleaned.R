rm(list=ls()) # clear env vars

library(dplyr)
library(purrr)

files_to_read <- list.files(
  path = "yieldmodelv4/",        # directory to search within
  pattern = ".*.*csv", # regex pattern, some explanation below
  recursive = TRUE,   # search subdirectories
  full.names = TRUE
)

data_ls <- lapply(files_to_read, function(x) {
  tryCatch(read.csv(x), error=function(e) NULL)
}) # read all the matching files

# remove the nulls
data_ls <- data_ls %>% discard(is.null)

data_ls <- lapply(data_ls, transform, Seeding_Variety=as.character(Seeding_Variety))

dat_df <- bind_rows(data_ls)

write.csv(dat_df, "uncleaned_yield.csv")
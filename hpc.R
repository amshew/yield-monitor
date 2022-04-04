rm(list=ls()) # clear env vars

library(dplyr)
library(purrr)

files_to_read <- list.files(
  path = "yieldmodelv4/", # directory to search within
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

# Uncleaned dataframe
dat_df <- bind_rows(data_ls)
#write.csv(dat_df, "uncleaned_yield.csv")

###################################################################

# Clean the DF
clean_fields <- function(x){
  sd.y <- sd(x$Yield)*2 # 2 standard deviations; 95% confidence interval
  m.y <- mean(x$Yield)
  out.up <- m.y+sd.y
  out.down <- m.y-sd.y
  dat_clean <- x[x$Yield > out.down & 
                           x$Yield < out.up,]
}

dat_ls_clean <- lapply(data_ls, clean_fields)

# Column class threw an error
dat_ls_clean <- lapply(dat_ls_clean, transform, Application_3_ID=as.character(Application_3_ID))
dat_ls_clean <- lapply(dat_ls_clean, transform, Application_4_ID=as.character(Application_4_ID))
dat_ls_clean <- lapply(dat_ls_clean, transform, Application_5_ID=as.character(Application_5_ID))
dat_ls_clean <- lapply(dat_ls_clean, transform, Application_6_ID=as.character(Application_6_ID))
dat_ls_clean <- lapply(dat_ls_clean, transform, Application_7_ID=as.character(Application_7_ID))
dat_ls_clean <- lapply(dat_ls_clean, transform, Application_8_ID=as.character(Application_8_ID))
dat_ls_clean <- lapply(dat_ls_clean, transform, Application_9_ID=as.character(Application_9_ID))
dat_ls_clean <- lapply(dat_ls_clean, transform, Application_10_ID=as.character(Application_10_ID))

# Create clean DF
dat_clean <- bind_rows(dat_ls_clean)
write.csv(dat_clean, "clean_yield.csv")





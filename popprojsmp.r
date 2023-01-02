# Initialize the environment  ---------------------------------------------------------------------

# delete all objects 
rm(list=ls())
# Clear console
cat("\014")  # ctrl+L

library(tidyverse)
library(readxl)

# Global variables  ---------------------------------------------------------------------
LocID <- 840    # United States


# read projection parameters
prj <- list()
fname <- paste0("data/", LocID, "_ccc.xlsx")
print(fname)
df <- read_excel(fname, sheet = "BasePopulation")
# sum(df$value)
prj$basepop <- df
prj$lfts <- read_excel(fname, sheet = "Mortality")
prj$tfr <- read_excel(fname, sheet = "TFR")
prj$srb <- read_excel(fname, sheet = "SRB")
prj$pasfr <- read_excel(fname, sheet = "PASFRs")
prj$pasfr <- read_excel(fname, sheet = "PASFRs")
prj$migr <-  read_excel(fname, sheet = "Migration")

# run cohort component method

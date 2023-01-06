# Main script for computing decomposition
# Select a country below and run the script 
# 2023/1/7 # Kirill Andreev (kirillandreev.com and https://www.linkedin.com/in/kirill-andreev-8bb12362)

# Initialize the environment  ---------------------------------------------------------------------
# delete all objects 
rm(list=ls())
# Clear console
cat("\014")  # ctrl+L

library(tidyverse)
library(readxl)
source("popprojsmp.r")
source("poasrf.r")

# Global variables  ---------------------------------------------------------------------

# Select country here:
CountryName <- "United States"
# CountryName <- "Japan"
# CountryName <- "Australia"
# CountryName <- "Germany"
# CountryName <- "United Kingdom"
# CountryName <- "Canada"
# CountryName <- "New Zealand"
# CountryName <- "France"
# CountryName <- "Italy"
# CountryName <- "Spain"

# get numeric country ID
if(CountryName == "United States"){
    LocID <- 840
}else 
if(CountryName == "Japan"){
    LocID <- 392
}else 
if(CountryName == "Australia"){
    LocID <- 36
}else 
if(CountryName == "Germany"){
    LocID <- 276
}else 
if(CountryName == "United Kingdom"){
    LocID <- 826
}else 
if(CountryName == "Canada"){
    LocID <- 124 
}else 
if(CountryName == "New Zealand"){
    LocID <- 554
}else 
if(CountryName == "France"){
    LocID <- 250
}else 
if(CountryName == "Italy"){
    LocID <- 380
}else 
if(CountryName == "Spain"){
    LocID <- 724
}else{
    stop("error: unknown country")
}

# Prepare projections -----------------------------------------------------
prjs <- list()

# Read parameters of the FMG projection, all components are changing 
prj <- list()
fname <- paste0("data/", LocID, "_FMG.xlsx")
# print(fname)

prj$basepop <- read_excel(fname, sheet = "BasePopulation")  # Base population
prj$lfts    <- read_excel(fname, sheet = "Mortality")       # Life tables
prj$tfr     <- read_excel(fname, sheet = "TFR")             # Total fertility rate
prj$srb     <- read_excel(fname, sheet = "SRB")             # Sex ratio at birth
prj$pasfrs  <- read_excel(fname, sheet = "PASFRs")          # Proportional age-specific fertility rates
prj$migr    <- read_excel(fname, sheet = "Migration")       # net migration

if (length(prj$basepop) == 0){
    stop("No data found")
}

prjs$FMG <- prj

# ccc projection, fertility and mortality are constant, migration is zero
prj <- prjs$FMG

years <- unique(prj$lfts$year)
lfts <- prj$lfts[prj$lfts$year == years[1],]
lfts0 <- lfts
for(i in 2:length(years)){
    lf <- lfts0
    lf$year <- rep(0, length(lfts0$year)) + years[i]
    lfts <- rbind(lfts, lf)    
}
prj$lfts <- lfts

prj$tfr$value <- rep(prj$tfr$value[1], length(prj$tfr$value))
prj$srb$value <- rep(prj$srb$value[1], length(prj$srb$value))

years <- unique(prj$pasfrs$year)
pasfrs <- prj$pasfrs[prj$pasfrs$year == years[1],]
pasfrs0 <- pasfrs
for(i in 2:length(years)){
    pasfr <- pasfrs0
    pasfr$year <- rep(0, length(pasfrs0$year)) + years[i]
    pasfrs <- rbind(pasfrs, pasfr)    
}
prj$pasfrs <- pasfrs

prj$migr$value <- rep(0, length(prj$migr$value))

prjs$ccc <- prj

# Fcc, only fertility is changing
prj <- prjs$ccc
prj$tfr     <- prjs$FMG$tfr
prj$srb     <- prjs$FMG$srb
prj$pasfrs  <- prjs$FMG$pasfrs
prjs$Fcc <- prj

# cMc, only mortality is changing
prj <- prjs$ccc
prj$lfts <- prjs$FMG$lfts
prjs$cMc <- prj

# ccG, only migration is changing
prj <- prjs$ccc
prj$migr <- prjs$FMG$migr
prjs$ccG <- prj

# FcG, fertility and migration are changing 
prj <- prjs$ccc
prj$tfr     <- prjs$FMG$tfr
prj$srb     <- prjs$FMG$srb
prj$pasfrs  <- prjs$FMG$pasfrs
prj$migr    <- prjs$FMG$migr
prjs$FcG    <- prj

# cMG, mortality and migration are changing
prj <- prjs$ccc
prj$lfts <- prjs$FMG$lfts
prj$migr <- prjs$FMG$migr
prjs$cMG <- prj

# FMc, fertility and mortality are changing
prj <- prjs$ccc
prj$tfr     <- prjs$FMG$tfr
prj$srb     <- prjs$FMG$srb
prj$pasfrs  <- prjs$FMG$pasfrs
prj$lfts    <- prjs$FMG$lfts
prjs$FMc <- prj

# Run projections ---------------------------------------------------------
prjout <- list()
prjnames = c("FMG", "ccc", "Fcc", "cMc", "ccG", "FcG", "cMG", "FMc")
for (i in 1:length(prjnames)){
    prj <- prjs[[prjnames[i]]]
    prjout[[prjnames[i]]] <- popprojsmp(prj) 
}

# Compute POASRs from projected age structures ----------------------------------------------------------
poasrs <- list()
for (i in 1:length(prjnames)){
    prjouti <- prjout[[prjnames[i]]]
    poasrs[[prjnames[i]]] <- poasrf(prjouti$pas)
}

# Compute contributions ---------------------------------------------------
transitions <- matrix(c("ccc","Fcc","FMc","FMG",
                        "ccc","Fcc","FcG","FMG",
                        "ccc","cMc","FMc","FMG",
                        "ccc","cMc","cMG","FMG",
                        "ccc","ccG","FcG","FMG",
                        "ccc","ccG","cMG","FMG"),nrow = 4,ncol = 6)

components <- matrix(c( "A","F","M","G",
                        "A","F","G","M",
                        "A","M","F","G",
                        "A","M","G","F",
                        "A","G","F","M",
                        "A","G","M","F"),nrow = 4,ncol = 6)

ctrbs <- matrix(nrow = 4,ncol = 6)

year1 <- 2010
year2 <- 2050
for (j in 1:ncol(transitions)) {
    
    v <- poasrs[[transitions[1,j]]]
    v <- v$value[v$sex == 0 & v$year == year1]
    t <- rep(NA, nrow(transitions) + 1)
    t[1] <- v
    for (i in 1:nrow(transitions)) {
        # print(paste0(transitions[i,j],i,j))
        
        v <- poasrs[[transitions[i,j]]]
        v <- v$value[v$sex == 0 & v$year == year2]
        t[i+1] <- v
    }
    ctrb <- diff(t) # contributions
    ctrbs[,j] <- ctrb
}

# compute average contributions
ctrbs <- as.vector(ctrbs)
components <- as.vector(components)

ctrbA <- mean(ctrbs[components == "A"])
ctrbF <- mean(ctrbs[components == "F"])
ctrbM <- mean(ctrbs[components == "M"])
ctrbG <- mean(ctrbs[components == "G"])

# check total change in a POASR
poasr1 <- poasrs$FMG$value[poasrs$FMG$sex == 0 & poasrs$FMG$year == year1]
poasr2 <- poasrs$FMG$value[poasrs$FMG$sex == 0 & poasrs$FMG$year == year2]
poasrtc <- poasr2 - poasr1
dv <- poasrtc - (ctrbA + ctrbF + ctrbM + ctrbG)

#if(!(dv == 0)){
if(abs(dv) > 1e-6){
    stop("Check of the total change of a POASR failed")
}

# percentages 
ctrbAprc <- ctrbA / poasrtc * 100
ctrbFprc <- ctrbF / poasrtc * 100
ctrbMprc <- ctrbM / poasrtc * 100
ctrbGprc <- ctrbG / poasrtc * 100

# printout ----------------------------------------------------------------
print(paste0("Country: ", CountryName))
print(paste0("POASR in ", year1, " = ", poasr1))
print(paste0("POASR in ", year2, " = ", poasr2))
print(paste0("Total change in POASR = ", poasrtc))
print(paste0("Change due to "))
print(paste0("    Momentum of population aging = ", ctrbA, " Percentage = ", ctrbAprc))
print(paste0("    Fertility = ", ctrbF, " Percentage = ", ctrbFprc))
print(paste0("    Mortality = ", ctrbM, " Percentage = ", ctrbMprc))
print(paste0("    Migration = ", ctrbG, " Percentage = ", ctrbGprc))

# save 
ofname <- paste0("output/", LocID, ".RData")
save(year1, year2, poasr1, poasr2, poasrtc, ctrbA, ctrbF, ctrbM, ctrbG, ctrbAprc, ctrbFprc, ctrbMprc, ctrbGprc, file = ofname)

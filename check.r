# Compile results for different countries

# Initialize the environment  ---------------------------------------------------------------------
# delete all objects 
rm(list=ls())
# Clear console, ctrl+L
cat("\014")

library(tidyverse)
library(readxl)
source("popprojsmp.r")
source("poasrf.r")

# Global variables  ---------------------------------------------------------------------
LocIDs <- c(276, 392, 826, 380, 250, 840, 36, 124, 554, 724)
LocIDN <- length(LocIDs)

poasr1s     <- rep(NaN, LocIDN)
poasr2s     <- rep(NaN, LocIDN)
poasrtcs    <- rep(NaN, LocIDN)
ctrbAs      <- rep(NaN, LocIDN)
ctrbAprcs   <- rep(NaN, LocIDN)
ctrbFs      <- rep(NaN, LocIDN)
ctrbFprcs   <- rep(NaN, LocIDN)
ctrbMs      <- rep(NaN, LocIDN)
ctrbMprcs   <- rep(NaN, LocIDN)
ctrbGs      <- rep(NaN, LocIDN)
ctrbGprcs   <- rep(NaN, LocIDN)

for(locidi in 1:LocIDN){
    LocID <- LocIDs[locidi]
    fname <- paste0("output/", LocID, ".Rdata")
    load(fname)
    print(LocID)
    
    poasr1s[locidi] <- poasr1
    poasr2s[locidi] <- poasr2
    poasrtcs[locidi] <- poasrtc

    ctrbAs[locidi] <- ctrbA 
    ctrbAprcs[locidi] <- ctrbAprc
    
    ctrbFs[locidi] <- ctrbF
    ctrbFprcs[locidi] <- ctrbFprc
    
    ctrbMs[locidi] <- ctrbM
    ctrbMprcs[locidi] <- ctrbMprc
    
    ctrbGs[locidi] <- ctrbG
    ctrbGprcs[locidi] <- ctrbGprc
    
}

print(poasr1s)
print(poasr2s)
print(poasrtcs)
print(ctrbAs)
print(ctrbAprcs)
print(ctrbFs)
print(ctrbFprcs)
print(ctrbMs)
print(ctrbMprcs)
print(ctrbGs)
print(ctrbGprcs)



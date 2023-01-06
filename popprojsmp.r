# Simple cohort component population projection
popprojsmp <- function(prj) {

# years to run
years <- seq(2010, 2045, by = 5)
ny <- length(years)
nages <- 5
nyears <- 5

# age structures
popm <- prj$basepop$value[prj$basepop$sex == 1]
popf <- prj$basepop$value[prj$basepop$sex == 2]
nagegroups <- length(popm)
z <- matrix(0, nagegroups, length(years))
popm <- cbind(popm, z)
popf <- cbind(popf, z)
agegroups <- seq(1, nagegroups)

# births
birthstf <- rep(0, ny)
birthstm <- rep(0, ny)
# births of migrants
# birthstfmigr <- rep(0, ny)
# birthstmmigr <- rep(0, ny)

# fertility indices for the standard age groups
fi1 <- 15 / 5 + 1
fi2 <- 45 / 5 + 1

# sex: females, males
sexes <- c(2, 1)

for(yi in seq_along(years)){

    year <- years[yi] # current year

    for(si in seq_along(sexes)){

        sex <- sexes[si]  # current sex

        # Demographic parameters for the current year
        pasfrsy <- prj$pasfrs$value[prj$pasfrs$year == year]
        tfry <- prj$tfr$value[prj$tfr$year == year]
        srby <- prj$srb$value[prj$srb$year == year]
        survy <- prj$lfts$Sx[prj$lfts$year == year & prj$lfts$sex == sex]
        migry <- prj$migr$value[prj$migr$year == year & prj$migr$sex == sex]

        # population at the beginning of the current period
        if (sex == 1) {
            pop1 <- popm[,yi]
        } else {
            pop1 <- popf[,yi]
        }

        # population at the end of the current period
        pop2 <- rep(0, length(pop1))

        # apply survival ratios, age group 5-9 or higher
        for(ai in 1:(length(pop2)-2)){
            pop2[ai+1] = pop1[ai] * survy[ai+1]
        }
        # for the open age group apply the last survivor ratio to age groups 95-99 & 100+
        pop2[length(pop2)] <- (pop1[length(pop1)] + pop1[length(pop1)-1]) * survy[nagegroups]

        # compute births
        if (sex == 2) {
            exps <- (pop1[fi1:fi2] + pop2[fi1:fi2]) / 2     # population exposure: linear extrapolation for the middle of the 5-year period
            asfrsy <- pasfrsy * tfry / nages;               # age-specific fertility rates
            birthsy <- exps * asfrsy                        # births per year
            birthsyt <- sum(birthsy) * nyears               # total births over 5-year period
            birthstf[yi] <- birthsyt / (1 + srby)           # female births
            birthstm[yi] <- birthsyt - birthstf[yi]         # male births

            # births of migrants
            # Not clear if we need births of migrants if the migrants are assumed to arrive at the end of the projection period
            # exps <- migry[fi1:fi2] / 2              # assume only half of migrants exposed to the population birth rates
            # birthsmigr <- exps * asfrsy             # births per year
            # birthsmigr <- sum(birthsmigr) * nyears  # total births over 5-year period
            # birthstfmigr[yi] = birthsmigr  / (1 + srby)         # female migrant births
            # birthstmmigr[yi] = birthsmigr - birthstfmigr[yi]    # male migrant births

        }

        # compute population in the 0-4 age group
        
        # births for the current sex
        if (sex == 1) {
            birthsyc <- birthstm[yi]
#            birthsycmigr <- birthstmmigr[yi]
        } else {
            birthsyc <- birthstf[yi]
#            birthsycmigr<- birthstfmigr[yi]
        }
        # survive births of the native population and migrants to age group 0-4
#        pop2[1] = (birthsyc + birthsycmigr) * survy[1]
        pop2[1] = birthsyc * survy[1]
        
        # add migration at the end of the period 
        pop2 <- pop2 + migry

        # copy results to males or females
        if (sex == 1) {
            popm[, yi+1] <- pop2
        } else {
            popf[, yi+1] <- pop2
        }

    } # end of sex

} # end of year

# output ------------------------------------------------------------------

# population by age and sex (pas)
# males
v <- as.vector(popm)
n <- length(n)
pas <- data.frame(value = v)            # population
n <- dim(pas)[1]
pas <- cbind(pas, sex = rep(1, n))      # sex

v <- matrix(0, ny + 1, nagegroups)
y <- c(years, years[length(years)] + nyears)
v <- v + y
v <- t(v)
v <- as.vector(v)
pas <- cbind(pas, year = v)

a <- prj$basepop$age[prj$basepop$sex == 1]
v <- matrix(0, nagegroups, ny + 1)
v <- v + a
v <- as.vector(v)
pas <- cbind(pas, age = v) 

al <- prj$basepop$agelength[prj$basepop$sex == 1]
v <- matrix(0, nagegroups, ny + 1)
v <- v + al
v <- as.vector(v)
pas <- cbind(pas, agelength = v) 

# females
pasf <- pas
pasf$value <- as.vector(popf)
pasf$sex <- pasf$sex + 1

# both sexes
past <- pas
past$value <- pas$value + pasf$value
past$sex <- pas$sex - 1

pas <- rbind(past, pas, pasf)
past <- NULL
pasf <- NULL

pas <- pas[, c("sex", "year", "age", "agelength", "value")] # order fields

rs <- list()
rs$pas <- pas
return(rs)

# # head(pas)
# # write.table(pas, file="c:/~/r.csv", quote=FALSE, row.names = FALSE, col.names = FALSE, sep = ",")
# # # stop("Stoped here!")

}

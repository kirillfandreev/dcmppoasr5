# Compute potential old-age support ratio 
poasrf <- function(pas) {

age1 <- 20
age2 <- 65

# Compute POASR for each combination of sex and year    
dfo <- unique(pas[,c("sex", "year")])
n <- nrow(dfo)
poasr <- rep(0, n)
for (i in 1:n) {
    pasi <- pas[pas$sex == dfo$sex[i] & pas$year == dfo$year[i],]
    #print(pasi)
    p2 <- sum(pasi$value[pasi$age >= age2])
    p1 <- sum(pasi$value[pasi$age < age2 & pasi$age >= age1])
    poasr[i] <- p1 / p2
}
dfo <- cbind(dfo, value = poasr)

return(dfo)
}

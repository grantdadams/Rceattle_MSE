library(gmRi)

spr_fun <- function(F){
  ages = 1:10
  wt = 1:10
  sel = 1/(1+exp(-1*ages + 2))
  mat = 1/(1+exp(-1*ages + 6))
  M1 <- -0.2 * ages
  spr0 = sum( exp(M1) * wt * mat * 0.5)
  sprF = sum( exp(M1 - sel*F) * wt * mat * 0.5)
  return(sprF/spr0)
}
curve(sapply(x, spr_fun), from = 0, to = 10)
F30 = uniroot(function(x) spr_fun(x) - 0.30, c(0,10))$root
F35 = uniroot(function(x) spr_fun(x) - 0.35, c(0,10))$root
F40 = uniroot(function(x) spr_fun(x) - 0.4, c(0,10))$root
F48 = uniroot(function(x) spr_fun(x) - 0.48, c(0,10))$root
F20 = uniroot(function(x) spr_fun(x) - 0.2, c(0,10))$root
F45 = uniroot(function(x) spr_fun(x) - 0.45, c(0,10))$root

tier3hcr_fun <- function(Pcur = 0.5, Ptarget = 0.4, Plimit = 0, Alpha = 0.05, Ftarget = F40, Flimit = F35){
  if(Pcur > Ptarget){
   Ftarget = Ftarget
   Flimit = Flimit
  } 
  if(Alpha< Pcur/Ptarget & Pcur/Ptarget <= 1){
    Flimit = Flimit * (Pcur/Ptarget - Alpha)/(1-Alpha)
    Ftarget = Ftarget * (Pcur/Ptarget - Alpha)/(1-Alpha)
  }
  if(Alpha>=Pcur/Ptarget ){
    Flimit = 0
    Ftarget = 0
  }
  if(Pcur < Plimit ){
    Flimit = 0
    Ftarget = 0
  }
  return(list(Flimit = Flimit, Ftarget = Ftarget))
}


sessfcat1hcr_fun <- function(Pcur = 0.5, Ptarget = 0.35, Plimit = 0.2, Ftarget = F48, Flimit = F20){
  if(Pcur > Ptarget){
    Ftarget = Ftarget
  } 
  if(Pcur < Ptarget & Pcur> Plimit){
    Ftarget = Ftarget * (Pcur/Plimit - 1)
  }
  if(Pcur < Plimit ){
    Ftarget = 0
  }
  return(list(Flimit = Flimit, Ftarget = Ftarget))
}

pfmccat1hcr_fun <- function(Pcur = 0.5, Ptarget = 0.40, Plimit = 0.1, Flimit = F45, sigma = 0.5, Pstar = 0.45){
  if(Pcur > Ptarget){
    Ftarget = qnorm(Pstar, Flimit, sigma)
  } 
  if(Pcur < Ptarget & Pcur> Plimit){
    Ftarget = qnorm(Pstar, Flimit, sigma) * Ptarget * (Pcur - Plimit)/((Ptarget - Plimit)*Pcur)
  }
  if(Pcur < Plimit ){
    Ftarget = 0
  }
  return(list(Flimit = Flimit, Ftarget = Ftarget))
}


Pvals <- seq(0,1,length.out = 1000)
Tier3Ftargets <- sapply(Pvals, function(x) tier3hcr_fun(Pcur = x, Plimit = 0.2, Alpha = 0.05)$Ftarget)
Tier3Flimits <- sapply(Pvals, function(x) tier3hcr_fun(Pcur = x, Plimit = 0.2, Alpha = 0.05)$Flimit)

Tier3FtargetsATF <- sapply(Pvals, function(x) tier3hcr_fun(Pcur = x, Plimit = 0, Alpha = 0.05)$Ftarget)
Tier3FlimitsATF <- sapply(Pvals, function(x) tier3hcr_fun(Pcur = x, Plimit = 0, Alpha = 0.05)$Flimit)

SESSFCat1Ftargets <- sapply(Pvals, function(x) sessfcat1hcr_fun(Pcur = x)$Ftarget)
SESSFCat1Flimits <- sapply(Pvals, function(x) sessfcat1hcr_fun(Pcur = x)$Flimit)

PFMCCat1Ftargets <- sapply(Pvals, function(x) pfmccat1hcr_fun(Pcur = x)$Ftarget)
PFMCCat1Flimits <- sapply(Pvals, function(x) pfmccat1hcr_fun(Pcur = x)$Flimit)

PFMCCat1FtargetsFlatfish <- sapply(Pvals, function(x) pfmccat1hcr_fun(Pcur = x, Ptarget = 0.25, Plimit = 0.05, Flimit = F30, sigma = 0.5, Pstar = 0.45)$Ftarget)


#--------------------------
# Plot it
par(mar = c(3, 3, 0.5, 0.5))

# Colors (by EM/HCR)
MPcols <- gmri_pal("mixed")(10)[1:8]

plot(NA,NA, ylim = c(0, 1.4), xlim = c(0,1), ylab = NA, yaxt = "na", xlab = "Depletion")
mtext(side = 2, "Fishing mortality (F)", line = 1.5)
mtext(side = 1, "Relative depletion", line = 2)

# abline(h = Tier3Ftargets[length(Tier3Ftargets)], lwd = 3, col = SamplingCol[4])

# - Tier 3
lines(x = Pvals, y = Tier3Ftargets, col = MPcols[1], lwd = 3, lty = 2)
lines(x = Pvals, y = Tier3FtargetsATF, col = MPcols[1], lwd = 3, lty = 1)
text(x=Pvals[1000], y = Tier3Ftargets[1000]+0.07, expression('1) F'["40%"]), offset = -2,pos = 4, col = MPcols[1])
#lines(x = Pvals, y = Tier3Flimits, col = MPcols[1], lwd = 3, lty = 2)


# - Cat 1
# -- 40-10
lines(x = Pvals, y = PFMCCat1Ftargets, col = MPcols[3], lwd = 3, lty = 1)
text(x=Pvals[900], y = PFMCCat1Ftargets[1000]+0.07, expression(paste("2a) ", Phi,'(0.45, F'["45%"],", 0.5)")), offset = -6,pos = 4, col = MPcols[3])
#lines(x = Pvals, y = PFMCCat1Flimits, col = 1, lwd = 3, lty = 2)

# -- 25-5
lines(x = Pvals, y = PFMCCat1FtargetsFlatfish, col = MPcols[4], lwd = 3, lty = 1)
text(x=Pvals[900], y = PFMCCat1FtargetsFlatfish[1000]+0.07, expression(paste("2b) ", Phi,'(0.45, F'["30%"],", 0.5)")), offset = -6,pos = 4, col = MPcols[4])
#lines(x = Pvals, y = PFMCCat1Flimits, col = MPcols[3], lwd = 3, lty = 2)


# - Tier 1
lines(x = Pvals, y = SESSFCat1Ftargets, col = MPcols[5], lwd = 3, lty = 1)
text(x=0.4, y = SESSFCat1Ftargets[1000]+0.07, expression('3) F'["48%"]), offset = 0,pos = 4, col = MPcols[5])


# - F40
abline(h = 0.75 * F40, col = MPcols[7], lwd = 3)
text(x=Pvals[1000], y = 0.75 * F40-0.07, expression('4) 0.75 * F'["40%"]), offset = -6 ,pos = 4, col = MPcols[7])


legend("bottomright", c("HCR 1 (NPFMC)", "HCR 2a (PFMC 40-10)", "HCR 2b (PFMC 25-5)", "HCR 3 (SESSF)", "HCR 4 (NEFMC)"), col = c(MPcols[1], MPcols[3], MPcols[4], MPcols[5], MPcols[7]), lty = 1, lwd = 4, bty = "n")
#legend("top", c("Ftarget", "Flimit"), col = c(1,1), lty = c(1,2), lwd = rep(2,2), bty = "n")


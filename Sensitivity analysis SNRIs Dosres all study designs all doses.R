

##################################################################
#     SENSITIVITY ANALYSIS using only SNRIs
#     ANALYSIS INCLUDING all dose and all drugs
#################################################################

pdf("Sensitivity analysis SNRIs Meta-analytic dose plots.pdf")
sink("Sensitivity analysis SNRIs Meta-analytic dose-response analysis.txt")

cat("\n \n \n DOSE-RESPONSE ANALYSIS OF SNRIs GIVEN AT ANY DOSE AND PLACEBO \n \n \n")
cat("From all analyses I excluded studies with zero events and studies with less than 2 different doses evaluated.\n 
    Active drugs as well as Placebo are included.\n")


#delete single arm studies

#Create 3 index variables to exlcude studies per outcome (response, dropout, dropoutAE) that do not contribute to the dose-response (e.g. 0 events, different doses etc)
DOSESNRIs=cleandosresdata.fun(DOSESNRIs,Study_No,logRR,Responders,No_randomised,hayasaka_ddd,"exc")
DOSESNRIs=cleandosresdata.fun(DOSESNRIs,Study_No,logRRdrop,Dropouts_total,No_randomised,hayasaka_ddd,"excdrop")
DOSESNRIs=cleandosresdata.fun(DOSESNRIs,Study_No,logRRdropAE,Dropouts_sideeffects,No_randomised,hayasaka_ddd,"excdropAE")

##REPORTING
cat("\n", paste("There are", length(unique(DOSESNRIs$Study_No)), "studies comparing all doses .", "\n"))
cat("which include the drugs:", unique(DOSESNRIs$Drug), "\n")
cat("\nThe knots I used in the splines are at doses 10,20,50 mg")
knots=c(10,20,50)
################
#1. response
###############

cat("\n-----------------------------------------------\n")
cat("\n-------- RESPONSE -----------------------------\n")

mymoredata=DOSESNRIs[DOSESNRIs$exc==F,]

cat("\n-------- Linear response -----------------------------\n")
cat(paste("******For the linear model there are", length(unique(mymoredata$Study_No)), "studies", "\n"))
text=paste(length(unique(mymoredata$Study_No)),"studies comparing SNRIs for response")

#linear
doseresRR=dosresmeta(formula=logRR~hayasaka_ddd, id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata,method = "reml")
print(summary(doseresRR))
cat(paste("RESULT: The RR for response from dose 10 to dose 30 is",round(predict(doseresRR,delta=30,exp=T)[2]/predict(doseresRR,delta=10,exp=T)[2],3), "\n"))
with(predict(doseresRR, expo = TRUE, order = TRUE), {
  plot(hayasaka_ddd, pred, log = "y", type = "l",
       xlim = c(0, 80), ylim = c(.75, 2.5),xlab="Dose",ylab="RR",main=c("Linear",text))
  lines(hayasaka_ddd,  ci.lb, lty = 2)
  lines(hayasaka_ddd, ci.ub, lty = 2)
  rug(hayasaka_ddd, quiet = TRUE) })
#with(mymoredata,points(hayasaka_ddd[logRR!=0],exp(logRR[logRR!=0])))
with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))

cat("\n-------- Splines response -----------------------------\n")
#cubic splines

cat("\n******For the spline model we have in total",length(unique(mymoredata$Study_No)),"studies")

  doseresRR=dosresmeta(formula=logRR~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(hayasaka_ddd=seq(0,80,1))
  xref=min(mymoredata$hayasaka_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, log = "y", type = "l",
         xlim = c(0, 80), ylim = c(.75, 2.5),xlab="Dose",ylab="RR",main=c("Splines",text))
    matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
  #with(mymoredata,points(hayasaka_ddd[logRR!=0],exp(logRR[logRR!=0])))
  with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))

################
#2. dropout
###############
cat("\n-----------------------------------------------\n")
cat("\n-------- DROPOUT  -----------------------------\n")

mymoredata=DOSESNRIs[DOSESNRIs$excdrop==F,] 


cat("\n-------- Linear dropout -----------------------------\n")
cat(paste("\n******For the linear model there are", length(unique(mymoredata$Study_No)), "studies", "\n"))
text=paste(length(unique(mymoredata$Study_No)),"studies comparing SNRIs for dropout")

#linear
doseresRR=dosresmeta(formula=logRRdrop~hayasaka_ddd, id=Study_No, type=type,cases=Dropouts_total,n=No_randomised,se=selogRRdrop,data=mymoredata )
print(summary(doseresRR))
cat(paste("RESULT: The RR for dropout from dose 10 to dose 30 is",round(predict(doseresRR,delta=30,exp=T)[2]/predict(doseresRR,delta=10,exp=T)[2],3), "\n"))

with(predict(doseresRR, expo = TRUE, order = TRUE), {
  plot(hayasaka_ddd, pred, log = "y", type = "l",
       xlim = c(0, 80), ylim = c(.75, 2.5),xlab="Dose",ylab="RR",main=c("Linear",text))
  lines(hayasaka_ddd,  ci.lb, lty = 2)
  lines(hayasaka_ddd, ci.ub, lty = 2)
  rug(hayasaka_ddd, quiet = TRUE) })
#with(mymoredata,points(hayasaka_ddd[logRR!=0],exp(logRR[logRR!=0])))
with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))

  
cat("\n-------- Splines dropout -----------------------------\n")

cat("******For the splines model we have in total",length(unique(mymoredata$Study_No)),"studies")

  doseresRR=dosresmeta(formula=logRRdrop~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_total,n=No_randomised,se=selogRRdrop,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(hayasaka_ddd=seq(0,80,1))
  xref=min(mymoredata$hayasaka_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, log = "y", type = "l",
         xlim = c(0, 80), ylim = c(.75, 2.5),xlab="Dose",ylab="RR",main=c("Splines",text))
    matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
  #with(mymoredata,points(hayasaka_ddd[logRRdrop!=0],exp(logRRdrop[logRRdrop!=0])))
  with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))


################################
#3. dropout due to AE
###############################
cat("\n-----------------------------------------------\n")
cat("\n-------- DROPOUT DUE TO AE --------------------\n")
mymoredata=DOSESNRIs[DOSESNRIs$excdropAE==F,] 

cat("\n-------- Linear dropout AE -----------------------------\n")
cat(paste("**** For the linear model", length(unique(mymoredata$Study_No)), "studies", "\n"))
text=paste(length(unique(mymoredata$Study_No)),"studies comparing SNRIs for dropout due to AE")

#linear
doseresRR=dosresmeta(formula=logRRdropAE~hayasaka_ddd, id=Study_No, type=type,cases=Dropouts_sideeffects,n=No_randomised,se=selogRRdropAE,data=mymoredata)
print(summary(doseresRR))
cat(paste("RESULT: The RR for AEdropout from dose 10 to dose 30 is",round(predict(doseresRR,delta=30,exp=T)[2]/predict(doseresRR,delta=10,exp=T)[2],3), "\n"))
cat(paste("RESULT: The RR for AEdropout from dose 30 to dose 80  is",round(predict(doseresRR,delta=80,exp=T)[2]/predict(doseresRR,delta=30,exp=T)[2],3), "\n"))

with(predict(doseresRR, expo = TRUE, order = TRUE), {
  plot(hayasaka_ddd, pred, log = "y", type = "l",
       xlim = c(0, 80), ylim = c(.5, 5),xlab="Dose",ylab="RR",main=c("Linear",text))
  lines(hayasaka_ddd,  ci.lb, lty = 2)
  lines(hayasaka_ddd, ci.ub, lty = 2)
  rug(hayasaka_ddd, quiet = TRUE) })
#with(mymoredata,points(hayasaka_ddd[logRRdropAE!=0],exp(logRRdropAE[logRRdropAE!=0])))
with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))

cat("\n-------- Splines dropout AE -----------------------------\n")
#cubic splines

cat("\n******For the splines model we have in total",length(unique(mymoredata$Study_No)),"studies\n")

  doseresRR=dosresmeta(formula=logRRdropAE~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_sideeffects,n=No_randomised,se=selogRRdropAE,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(hayasaka_ddd=seq(0,80,1))
  xref=min(mymoredata$hayasaka_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, log = "y", type = "l",
         xlim = c(0, 80), ylim = c(.5, 5),xlab="Dose",ylab="RR",main=c("Splines",text))
    matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
  #with(mymoredata,points(hayasaka_ddd[logRRdropAE!=0],exp(logRRdropAE[logRRdropAE!=0])))
  with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))

dev.off()
sink()



##################################################################
#     SENSITIVITY ANALYSIS using Jakubovski_ddd
#     ANALYSIS INCLUDING all dose and all drugs
#################################################################

pdf("Sensitivity analysis Jakobovski Meta-analytic dose plots for all drugs and doses.pdf")
sink("Sensitivity analysis Jakobovski Meta-analytic dose-response analysis for all drugs and doses.txt")

cat("\n \n \n DOSE RESPONSE ANALYSIS OF ALL ACTIVE DRUGS GIVEN AT ANY DOSE using Jakubovski AND PLACEBO \n \n \n")
cat("From all analyses I excluded studies with zero events and studies with less than 2 different doses evaluated.\n 
    Active drugs as well as Placebo are included.\n")



#delete single arm studies

#Create 3 index variables to exlcude studies per outcome (response, dropout, dropoutAE) that do not contribute to the dose-response (e.g. 0 events, different doses etc)
DOSEj=cleandosresdata.fun(DOSEj,Study_No,logRR,Responders,No_randomised,jakubovski_ddd,"exc")
DOSEj=cleandosresdata.fun(DOSEj,Study_No,logRRdrop,Dropouts_total,No_randomised,jakubovski_ddd,"excdrop")
DOSEj=cleandosresdata.fun(DOSEj,Study_No,logRRdropAE,Dropouts_sideeffects,No_randomised,jakubovski_ddd,"excdropAE")

##REPORTING
cat("\n", paste("There are", length(unique(DOSEj$Study_No)), "studies comparing all doses .", "\n"))
cat("which include the drugs:", unique(DOSEj$Drug), "\n")
cat("\nThe knots I used in the splines are at doses 10,20,50 mg")
knots=c(10,20,50)
################
#1. response
###############

cat("\n-----------------------------------------------\n")
cat("\n-------- RESPONSE -----------------------------\n")

mymoredata=DOSEj[DOSEj$exc==F,]

cat("\n-------- Linear response -----------------------------\n")
cat(paste("******For the linear model there are", length(unique(mymoredata$Study_No)), "studies", "\n"))
text=paste(length(unique(mymoredata$Study_No)),"studies comparing all drugs and doses for response")

#linear
doseresRR=dosresmeta(formula=logRR~jakubovski_ddd, id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata,method = "reml")
print(summary(doseresRR))
cat(paste("RESULT: The RR for response from dose 10 to dose 30 is",round(predict(doseresRR,delta=30,exp=T)[2]/predict(doseresRR,delta=10,exp=T)[2],3), "\n"))
with(predict(doseresRR, expo = TRUE, order = TRUE), {
  plot(jakubovski_ddd, pred, log = "y", type = "l",
       xlim = c(0, 80), ylim = c(.75, 2.5),xlab="Dose",ylab="RR",main=c("Linear",text))
  lines(jakubovski_ddd,  ci.lb, lty = 2)
  lines(jakubovski_ddd, ci.ub, lty = 2)
  rug(jakubovski_ddd, quiet = TRUE) })
#with(mymoredata,points(jakubovski_ddd[logRR!=0],exp(logRR[logRR!=0])))
with(mymoredata,rug(jakubovski_ddd, quiet = TRUE))

cat("\n-------- Splines response -----------------------------\n")
#cubic splines

cat("\n******For the spline model we have in total",length(unique(mymoredata$Study_No)),"studies")

  doseresRR=dosresmeta(formula=logRR~rcs(jakubovski_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(jakubovski_ddd=seq(0,80,1))
  xref=min(mymoredata$jakubovski_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(jakubovski_ddd, knots)jakubovski_ddd"),pred, log = "y", type = "l",
         xlim = c(0, 80), ylim = c(.75, 2.5),xlab="Dose",ylab="RR",main=c("Splines",text))
    matlines(get("rcs(jakubovski_ddd, knots)jakubovski_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
  #with(mymoredata,points(jakubovski_ddd[logRR!=0],exp(logRR[logRR!=0])))
  with(mymoredata,rug(jakubovski_ddd, quiet = TRUE))

################
#2. dropout
###############
cat("\n-----------------------------------------------\n")
cat("\n-------- DROPOUT  -----------------------------\n")

mymoredata=DOSEj[DOSEj$excdrop==F,] 


cat("\n-------- Linear dropout -----------------------------\n")
cat(paste("\n******For the linear model there are", length(unique(mymoredata$Study_No)), "studies", "\n"))
text=paste(length(unique(mymoredata$Study_No)),"studies comparing all drugs and doses for dropout")

#linear
doseresRR=dosresmeta(formula=logRRdrop~jakubovski_ddd, id=Study_No, type=type,cases=Dropouts_total,n=No_randomised,se=selogRRdrop,data=mymoredata )
print(summary(doseresRR))
cat(paste("RESULT: The RR for dropout from dose 10 to dose 30  is",round(predict(doseresRR,delta=30,exp=T)[2]/predict(doseresRR,delta=10,exp=T)[2],3), "\n"))

with(predict(doseresRR, expo = TRUE, order = TRUE), {
  plot(jakubovski_ddd, pred, log = "y", type = "l",
       xlim = c(0, 80), ylim = c(.75, 2.5),xlab="Dose",ylab="RR",main=c("Linear",text))
  lines(jakubovski_ddd,  ci.lb, lty = 2)
  lines(jakubovski_ddd, ci.ub, lty = 2)
  rug(jakubovski_ddd, quiet = TRUE) })
#with(mymoredata,points(jakubovski_ddd[logRR!=0],exp(logRR[logRR!=0])))
with(mymoredata,rug(jakubovski_ddd, quiet = TRUE))

  
cat("\n-------- Splines dropout -----------------------------\n")

cat("******For the splines model we have in total",length(unique(mymoredata$Study_No)),"studies")

  doseresRR=dosresmeta(formula=logRRdrop~rcs(jakubovski_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_total,n=No_randomised,se=selogRRdrop,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(jakubovski_ddd=seq(0,80,1))
  xref=min(mymoredata$jakubovski_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(jakubovski_ddd, knots)jakubovski_ddd"),pred, log = "y", type = "l",
         xlim = c(0, 80), ylim = c(.75, 2.5),xlab="Dose",ylab="RR",main=c("Splines",text))
    matlines(get("rcs(jakubovski_ddd, knots)jakubovski_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
  #with(mymoredata,points(jakubovski_ddd[logRRdrop!=0],exp(logRRdrop[logRRdrop!=0])))
  with(mymoredata,rug(jakubovski_ddd, quiet = TRUE))


################################
#3. dropout due to AE
###############################
cat("\n-----------------------------------------------\n")
cat("\n-------- DROPOUT DUE TO AE --------------------\n")
mymoredata=DOSEj[DOSEj$excdropAE==F,] 

cat("\n-------- Linear dropout AE -----------------------------\n")
cat(paste("**** For the linear model", length(unique(mymoredata$Study_No)), "studies", "\n"))
text=paste(length(unique(mymoredata$Study_No)),"studies comparing all drugs and doses for dropout due to AE")

#linear
doseresRR=dosresmeta(formula=logRRdropAE~jakubovski_ddd, id=Study_No, type=type,cases=Dropouts_sideeffects,n=No_randomised,se=selogRRdropAE,data=mymoredata)
print(summary(doseresRR))
cat(paste("RESULT: The RR for AEdropout from dose 30 to dose 80  is",round(predict(doseresRR,delta=80,exp=T)[2]/predict(doseresRR,delta=30,exp=T)[2],3), "\n"))

with(predict(doseresRR, expo = TRUE, order = TRUE), {
  plot(jakubovski_ddd, pred, log = "y", type = "l",
       xlim = c(0, 80), ylim = c(.5, 5),xlab="Dose",ylab="RR",main=c("Linear",text))
  lines(jakubovski_ddd,  ci.lb, lty = 2)
  lines(jakubovski_ddd, ci.ub, lty = 2)
  rug(jakubovski_ddd, quiet = TRUE) })
#with(mymoredata,points(jakubovski_ddd[logRRdropAE!=0],exp(logRRdropAE[logRRdropAE!=0])))
with(mymoredata,rug(jakubovski_ddd, quiet = TRUE))

cat("\n-------- Splines dropout AE -----------------------------\n")
#cubic splines

cat("\n******For the splines model we have in total",length(unique(mymoredata$Study_No)),"studies\n")

  doseresRR=dosresmeta(formula=logRRdropAE~rcs(jakubovski_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_sideeffects,n=No_randomised,se=selogRRdropAE,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(jakubovski_ddd=seq(0,80,1))
  xref=min(mymoredata$jakubovski_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(jakubovski_ddd, knots)jakubovski_ddd"),pred, log = "y", type = "l",
         xlim = c(0, 80), ylim = c(.5, 5),xlab="Dose",ylab="RR",main=c("Splines",text))
    matlines(get("rcs(jakubovski_ddd, knots)jakubovski_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
  #with(mymoredata,points(jakubovski_ddd[logRRdropAE!=0],exp(logRRdropAE[logRRdropAE!=0])))
  with(mymoredata,rug(jakubovski_ddd, quiet = TRUE))

dev.off()
sink()

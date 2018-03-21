


##################################################################
#     ANALYSIS INCLUDING DOSES BETWEEN 0.75 AND 30 MG
#################################################################

pdf("Meta-analytic dose plots for all drugs and for 075 to 30.pdf")
sink("Meta-analytic dose-response analysis for all drugs and for 075 to 30.txt")

cat("\n \n \n DOSE RESPONSE ANALYSIS OF ALL ACTIVE DRUGS GIVEN AT SUB-THERAPEUTIC DOSES (LESS THAN 30mg) \n \n \n")
cat("From all analyses I excluded studies with zero events and studies with less than 2 different doses evaluated.\n 
Only active drugs are included, so zero dose (Placebo) is out.
\n The minimum observed dose is 0.75 mg.")


#####CLEAN THE DATA####

#Create 3 index variables to exlcude studies per outcome (response, dropout, dropoutAE) that do not contribute to the dose-response (e.g. 0 events, different doses etc)
DOSEless30=cleandosresdata.fun(DOSEless30,Study_No,logRR,Responders,No_randomised,hayasaka_ddd,"exc")
DOSEless30=cleandosresdata.fun(DOSEless30,Study_No,logRRdrop,Dropouts_total,No_randomised,hayasaka_ddd,"excdrop")
DOSEless30=cleandosresdata.fun(DOSEless30,Study_No,logRRdropAE,Dropouts_sideeffects,No_randomised,hayasaka_ddd,"excdropAE")

##REPORTING
cat("\n", paste("There are", length(unique(DOSEless30$Study_No)), "studies comparing doses between 0.75 and 30 mg.", "\n"))
cat("which include the drugs:", unique(DOSEless30$Drug), "\n")
cat("\nThe knots I used in the splines are at doses 5, 15 and 25 mg")
################
#1. response
###############

cat("\n-----------------------------------------------\n")
cat("\n-------- RESPONSE -----------------------------\n")

mymoredata=DOSEless30[DOSEless30$exc==F,]
mindose=min(mymoredata$hayasaka_ddd)
text=paste(length(unique(mymoredata$Study_No)),"studies comparing doses between 0.75 and 30mg")

cat("\n-------- Linear response -----------------------------\n")
cat(paste("******For the linear model there are", length(unique(mymoredata$Study_No)), "studies", "\n"))
text=paste(length(unique(mymoredata$Study_No)),"studies comparing doses between 0.75 and 30mg for response")

#linear
doseresRR=dosresmeta(formula=logRR~hayasaka_ddd, id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata,method = "reml")
print(summary(doseresRR))
predict(doseresRR,delta=1,exp=T)
predict(doseresRR,delta=30,exp=T)
cat(paste("RESULT: The RR for response between doses 1 mg and 30 mg is",round(predict(doseresRR,delta=30,exp=T)[2]/predict(doseresRR,delta=1,exp=T)[2],3), "\n"))
with(predict(doseresRR, expo = TRUE, order = TRUE), {
  plot(hayasaka_ddd, pred, log = "y", type = "l",
       xlim = c(0, 35), ylim = c(.75, 2.5),xlab="Dose",ylab="RR",main=c("Linear",text))
  lines(hayasaka_ddd,  ci.lb, lty = 2)
  lines(hayasaka_ddd, ci.ub, lty = 2)
  rug(hayasaka_ddd, quiet = TRUE) })
with(mymoredata,points(hayasaka_ddd,exp(logRR)))


cat("\n-------- Splines response -----------------------------\n")
#cubic splines
knots=c(5,15,25)
cat("\n******For the spline model we have in total",length(unique(mymoredata$Study_No)),"studies")

  doseresRR=dosresmeta(formula=logRR~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(hayasaka_ddd=seq(0.75,30,0.25))
  xref=min(mymoredata$hayasaka_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, log = "y", type = "l",
         xlim = c(0, 30), ylim = c(.75, 2.5),xlab="Dose",ylab="RR",main=c("Splines",text))
    matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
  with(mymoredata,points(hayasaka_ddd,exp(logRR)))
  with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))

################
#2. dropout
###############
cat("\n-----------------------------------------------\n")
cat("\n-------- DROPOUT  -----------------------------\n")

mymoredata=DOSEless30[DOSEless30$excdrop==F,]


cat("\n-------- Linear dropout -----------------------------\n")
cat(paste("\n******For the linear model there are", length(unique(mymoredata$Study_No)), "studies", "\n"))
text=paste(length(unique(mymoredata$Study_No)),"studies comparing doses between 0.75 and 30 for dropout")

#linear
doseresRR=dosresmeta(formula=logRRdrop~hayasaka_ddd, id=Study_No, type=type,cases=Dropouts_total,n=No_randomised,se=selogRRdrop,data=mymoredata )
print(summary(doseresRR))
cat(paste("RESULT: The RR for dropout between doses 1 mg and 30 mg is",round(predict(doseresRR,delta=30,exp=T)[2]/predict(doseresRR,delta=1,exp=T)[2],3), "\n"))
with(predict(doseresRR, expo = TRUE, order = TRUE), {
  plot(hayasaka_ddd, pred, log = "y", type = "l",
       xlim = c(0.75, 30), ylim = c(.75, 2.5),xlab="Dose",ylab="RR",main=c("Linear",text))
  lines(hayasaka_ddd,  ci.lb, lty = 2)
  lines(hayasaka_ddd, ci.ub, lty = 2)
  rug(hayasaka_ddd, quiet = TRUE) })
with(mymoredata,points(hayasaka_ddd,exp(logRR)))

cat("\n-------- Splines dropout -----------------------------\n")
knots=c(5,15,25)
cat("******For the splines model we have in total",length(unique(mymoredata$Study_No)),"studies")

  doseresRR=dosresmeta(formula=logRRdrop~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_total,n=No_randomised,se=selogRRdrop,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(hayasaka_ddd=seq(0.75,30,0.25))
  xref=min(mymoredata$hayasaka_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, log = "y", type = "l",
         xlim = c(0,30), ylim = c(.75, 2.5),xlab="Dose",ylab="RR",main=c("Splines",text))
    matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
  with(mymoredata,points(hayasaka_ddd,exp(logRRdrop)))
  with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))


################################
#3. dropout due to AE
###############################
cat("\n-----------------------------------------------\n")
cat("\n-------- DROPOUT DUE TO AE --------------------\n")
mymoredata=DOSEless30[DOSEless30$excdropAE==F,] 

cat("\n-------- Linear dropout AE -----------------------------\n")
cat(paste("**** For the linear model", length(unique(mymoredata$Study_No)), "studies", "\n"))
text=paste(length(unique(mymoredata$Study_No)),"studies comparing doses 0.75 to 30 mg for dropout due to AE")

#linear
doseresRR=dosresmeta(formula=logRRdropAE~hayasaka_ddd, id=Study_No, type=type,cases=Dropouts_sideeffects,n=No_randomised,se=selogRRdropAE,data=mymoredata)
print(summary(doseresRR))
cat(paste("RESULT: The RR for dropout due to AE between doses 1 mg and 30 mg is",round(predict(doseresRR,delta=30,exp=T)[2]/predict(doseresRR,delta=1,exp=T)[2],3), "\n"))
with(predict(doseresRR, expo = TRUE, order = TRUE), {
  plot(hayasaka_ddd, pred, log = "y", type = "l",
       xlim = c(0, 30), ylim = c(.5, 5),xlab="Dose",ylab="RR",main=c("Linear",text))
  lines(hayasaka_ddd,  ci.lb, lty = 2)
  lines(hayasaka_ddd, ci.ub, lty = 2)
  rug(hayasaka_ddd, quiet = TRUE) })
with(mymoredata,points(hayasaka_ddd,exp(logRRdropAE)))


cat("\n-------- Splines dropout AE -----------------------------\n")
#cubic splines
knots=c(5,15,25)
cat("\n******For the splines model we have in total",length(unique(mymoredata$Study_No)),"studies\n")

  doseresRR=dosresmeta(formula=logRRdropAE~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_sideeffects,n=No_randomised,se=selogRRdropAE,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(hayasaka_ddd=seq(0.75,30,0.25))
  xref=min(mymoredata$hayasaka_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, log = "y", type = "l",
         xlim = c(0, 30), ylim = c(.5, 5),xlab="Dose",ylab="RR",main=c("Splines",text))
    matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
  with(mymoredata,points(hayasaka_ddd,exp(logRRdropAE)))
  with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))

dev.off()
sink()




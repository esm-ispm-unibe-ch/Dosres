

##################################################################
#     Linear analysis of 12 studies DOSES BETWEEN 20mg AND 60mg
#################################################################

pdf("Meta-analytic dose plots for 12 studies.pdf")
sink("Meta-analytic dose-response analysis for 7 studies with dose 20 to 60mg.txt")

cat("\n \n \n DOSE RESPONSE ANALYSIS OF 12 studies with dose between 20 and 60 \n \n ")

#####CLEAN THE DATA####


#Create 3 index variables to exlcude studies per outcome (response, dropout, dropoutAE) that do not contribute to the dose-response (e.g. 0 events, different doses etc)
DOSEtheur=cleandosresdata.fun(DOSEtheur,Study_No,logRR,Responders,No_randomised,hayasaka_ddd,"exc")
DOSEtheur=cleandosresdata.fun(DOSEtheur,Study_No,logRRdrop,Dropouts_total,No_randomised,hayasaka_ddd,"excdrop")
DOSEtheur=cleandosresdata.fun(DOSEtheur,Study_No,logRRdropAE,Dropouts_sideeffects,No_randomised,hayasaka_ddd,"excdropAE")



mymoredata=DOSEtheur[DOSEtheur$exc==F,]
text=paste(12,"studies comparing doses 20 to 50 for response \n")
print(text)

#doseres for the 12 studies
mymoredata1=mymoredata[1:24,]
doseresRR=dosresmeta(formula=logRR~hayasaka_ddd, id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata1,method = "reml")
cat(" \n *****************Doseres with the 12 first studies******************** \n" )
print(summary(doseresRR))

with(predict(doseresRR, expo = TRUE, order = TRUE), {
  plot(hayasaka_ddd, pred, log = "y", type = "l",
        xlim = c(20, 60), ylim = c(.75, 2),xlab="Dose",ylab="RR",col="red")
  lines(hayasaka_ddd,  ci.lb, lty = 3, col="grey")
  lines(hayasaka_ddd, ci.ub, lty = 3, col="grey")
   })

title(main=c("Linear",text),sub="7 studies are shown with bubbles proportional to the sample size")
abline(h=1, col="green")

#doseres shown for 7 studies
for(i in c(1,3,5,7,9,11,13)){
mymoredata1=mymoredata[i:c(i+1),]
with(mymoredata1,text(hayasaka_ddd[2], exp(logRR[2]+0.02),paste(Study_No[1])))
#linear
doseresRR=dosresmeta(formula=logRR~hayasaka_ddd, id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata1,method = "reml")
cat("\n",paste("************Dosres within study with ID",mymoredata1$Study_No[1]),"****************\n")
print(summary(doseresRR))
with(predict(doseresRR, expo = TRUE, order = TRUE), {
  lines(hayasaka_ddd, pred,  type = "l",
       xlim = c(20, 60), ylim = c(.75, 2),xlab="Dose",ylab="RR")
  #lines(hayasaka_ddd,  ci.lb, lty = 3, col="grey")
  #lines(hayasaka_ddd, ci.ub, lty = 3, col="grey")
  rug(hayasaka_ddd, quiet = TRUE) })
with(mymoredata1,points(hayasaka_ddd,exp(logRR),cex=No_randomised/50))
}

dev.off()
sink()

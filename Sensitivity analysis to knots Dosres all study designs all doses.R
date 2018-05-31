

##################################################################
#     SENSITIVITY ANALYSIS INCLUDING all dose and all drugs
#     choosing different number and density of knots
#################################################################

pdf("Sensitivity analyses to knots Meta-analytic dose plots for all drugs and doses.pdf")

#matrix with each row being a different set of knots
#knotsmatrix=rbind(c(10,20,50),c(20,30,60),c(20,60,78))
knotsmatrix=rbind(c(10,20,30),c(10,40,79),c(5,10,50),c(10,20,50))

for(j in 1:dim(knotsmatrix)[1]){
  knots=as.vector(knotsmatrix[j,])
  text=paste("knots at:", toString(knots))  
  text
          ################
          #1. response
          ###############
            mymoredata=DOSE[DOSE$exc==F,]
            doseresRR=dosresmeta(formula=logRR~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata)
            print(summary(doseresRR))
            newdata=data.frame(hayasaka_ddd=seq(0,80,1))
            xref=min(mymoredata$hayasaka_ddd)
            with(predict(doseresRR, newdata,xref, exp = TRUE), {
              plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, log = "y", type = "l",
                   xlim = c(0, 80), ylim = c(.75, 2.5),xlab="Dose",ylab="RR",main=c("Splines Response",text))
              matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
            #with(mymoredata,points(hayasaka_ddd[logRR!=0],exp(logRR[logRR!=0])))
            with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))
            
          
          ################
          #2. dropout
          ###############
             mymoredata=DOSE[DOSE$excdrop==F,] 
            doseresRR=dosresmeta(formula=logRRdrop~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_total,n=No_randomised,se=selogRRdrop,data=mymoredata)
            print(summary(doseresRR))
            newdata=data.frame(hayasaka_ddd=seq(0,80,1))
            xref=min(mymoredata$hayasaka_ddd)
            with(predict(doseresRR, newdata,xref, exp = TRUE), {
              plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, log = "y", type = "l",
                   xlim = c(0, 80), ylim = c(.75, 2.5),xlab="Dose",ylab="RR",main=c("Splines Dropout",text))
              matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
            #with(mymoredata,points(hayasaka_ddd[logRRdrop!=0],exp(logRRdrop[logRRdrop!=0])))
            with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))
          
          
          ################################
          #3. dropout due to AE
          ###############################
          mymoredata=DOSE[DOSE$excdropAE==F,] 
            doseresRR=dosresmeta(formula=logRRdropAE~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_sideeffects,n=No_randomised,se=selogRRdropAE,data=mymoredata)
            print(summary(doseresRR))
            newdata=data.frame(hayasaka_ddd=seq(0,80,1))
            xref=min(mymoredata$hayasaka_ddd)
            with(predict(doseresRR, newdata,xref, exp = TRUE), {
              plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, log = "y", type = "l",
                   xlim = c(0, 80), ylim = c(.5, 5),xlab="Dose",ylab="RR",main=c("Splines Dropout AE",text))
              matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
            #with(mymoredata,points(hayasaka_ddd[logRRdropAE!=0],exp(logRRdropAE[logRRdropAE!=0])))
            with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))
}

dev.off()


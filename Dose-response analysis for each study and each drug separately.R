

################################################################################################
# Analysis of studies with the same Drug at least 2 dosages
##############################################################################################


# Create the data
##################
tableDRUGsStudy=with(DOSE,table(Study_No,Drug))
idtokeep1=unique(DOSE$Study_No)[apply(tableDRUGsStudy,1,max)>=2]#keep studies with at least 2 doses of the same drug
idtokeep2=unique(DOSE$Study_No)[tableDRUGsStudy[,colnames(tableDRUGsStudy)=="placebo"]==1]#keep placebo-controlled
idtokeep=unique(c(idtokeep1,idtokeep2))
DOSE$sameDrug=with(DOSE,!is.na(match(Study_No, idtokeep)))
DOSEsameDrug=DOSE[DOSE$sameDrug,]#create a database that only has studies with the same drug

#exlude arms that are not useful

excludeirrelevantdrug=function(drugvector)
{
  drugvector1=drugvector[drugvector!="placebo"]
  a=table(drugvector1)
  if(dim(a)==1){excludedrug=rep(F,length(drugvector))}
 else{
   if(min(a)==2){excludedrug=rep(F,length(drugvector))}
  else {excludedrug=(drugvector==names(a)[a<2])}
  }
  excludedrug
}

exludearm=c()
for(i in unique(DOSEsameDrug$Study_No)){
mylittledata=DOSEsameDrug[DOSEsameDrug$Study_No==i,]
exludearm=c(exludearm,excludeirrelevantdrug(mylittledata$Drug))
}
length(exludearm)
length(DOSEsameDrug$Drug)

DOSEsameDrug=DOSEsameDrug[exludearm==F,]
#with(DOSEsameDrug,table(Study_No,Drug))

#we need to correct study 128 By hand
r=DOSEsameDrug$Responders[DOSEsameDrug$Study_No==128]
n=DOSEsameDrug$No_randomised[DOSEsameDrug$Study_No==128]
DOSEsameDrug$logRR[DOSEsameDrug$Study_No==128]=c(0,metabin(r[2],n[2],r[1],n[1],"RR")$TE)
DOSEsameDrug$selogRR[DOSEsameDrug$Study_No==128]=c(NA,metabin(r[2],n[2],r[1],n[1],"RR")$seTE)


#split study 381 that has Placebo, Paroxetinex2, Citalopram x2

a=DOSEsameDrug[DOSEsameDrug$Study_No==381,]
aParox=a[c(1,2,4),]
aParox$Study_No=381.1
aCit=a[c(1,3,5),]
aCit$Study_No=381.2
a=rbind.data.frame(DOSEsameDrug[(DOSEsameDrug$Study_No!=381),],aParox,aCit)
DOSEsameDrug=a
DOSEsameDrug=DOSEsameDrug[order(DOSEsameDrug$Study_No),]
#####

####    sort report

cat(paste("We have",length(unique(DOSEsameDrug$Study_No)),"studies comparing different doses of the same drug"))
cat("Nr of studies with multiple doses of the same drug")
apply(table(DOSEsameDrug$Drug,DOSEsameDrug$Study_No)>0,1,sum)

dis=names(apply(table(DOSEsameDrug$Drug,DOSEsameDrug$Study_No)>0,1,sum))#names of drugs with multiple doses per arm
dis=dis[dis!="placebo"]


#########################################
### Analysis per study
#########################################



pdf("Per study dose response.pdf") 
for(i in 1:length(dis))
  {#iterate in drugs
  cat("***************************","\n")
  cat(paste("Studies in",dis[i],"\n"))
  cat("***************************","\n")
  studis=unique(DOSEsameDrug$Study_No[DOSEsameDrug$Drug==dis[i]])

  for(j in 1:length(studis)){ #iterate in studies within drugs
  mylittledata=DOSEsameDrug[DOSEsameDrug$Study_No==studis[j],]
  mylittledata=mylittledata[!is.na(mylittledata$logRR),]
  cat(paste("Study in",dis[i], "with ID=", studis[j], "\n"))
  
    if(max(mylittledata$logRR,na.rm = T)==0 | dim(mylittledata)[1]<3){cat(paste("no efficacy data in study",studis[j]),"\n")}
    else{
      text=paste("Study",studis[j],"with",unique(mylittledata$Drug)[1],"vs",unique(mylittledata$Drug)[-1])

        #linear
        doseresRR=dosresmeta(formula=logRR~Dose_delivered_mean, id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mylittledata)
        summary(doseresRR)
        predict(doseresRR,delta=20,exp=T)
        with(predict(doseresRR, expo = TRUE, order = TRUE), {
        plot(Dose_delivered_mean, pred, log = "y", type = "l",
            xlim = c(0, max(Dose_delivered_mean)), ylim = c(.75, 2.5),xlab="Actual mean dose",ylab="RR",main=c("Linear",text))
       lines(Dose_delivered_mean,  ci.lb, lty = 2)
       lines(Dose_delivered_mean, ci.ub, lty = 2)
       rug(Dose_delivered_mean, quiet = TRUE) })
        with(mylittledata,points(Dose_delivered_mean,exp(logRR)))
     
        #quadratic
        doseresRR=dosresmeta(formula=logRR~Dose_delivered_mean+I(Dose_delivered_mean^2), id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mylittledata)
        summary(doseresRR)
        predict(doseresRR,exp=T)
        with(predict(doseresRR, expo = TRUE, order = TRUE), {
        plot(Dose_delivered_mean, pred, log = "y", type = "l",
            xlim = c(0, max(Dose_delivered_mean)), ylim = c(.75, 2.5),xlab="Actual mean dose",ylab="RR",main=c("Quadratic",text))
        lines(Dose_delivered_mean,  ci.lb, lty = 2)
        lines(Dose_delivered_mean, ci.ub, lty = 2)
        rug(Dose_delivered_mean, quiet = TRUE)})
        with(mylittledata,points(Dose_delivered_mean,exp(logRR)))
     
    #cubic splines
      maxdose=max(mylittledata$Dose_delivered_mean)
      knot1=maxdose/5+(maxdose-maxdose/5)/3
      knot2=maxdose/5+2*(maxdose-maxdose/5)/3
      knots=c(maxdose/5,knot1,knot2)
      tryCatch({#start tryCatch to avoid stopping with stupid errors
      doseresRR=dosresmeta(formula=logRR~rcs(Dose_delivered_mean,knots), proc="1stage",id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mylittledata)
      summary(doseresRR)
      newdata=data.frame(Dose_delivered_mean=seq(0,maxdose,1))
      xref=0
      with(predict(doseresRR, newdata,xref, exp = TRUE), {
      plot(get("rcs(Dose_delivered_mean, knots)Dose_delivered_mean"),pred, log = "y", type = "l",
           xlim = c(0, maxdose), ylim = c(.75, 2.5),xlab="Actual mean dose",ylab="RR",main=c("Splines",text))
      matlines(get("rcs(Dose_delivered_mean, knots)Dose_delivered_mean"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
      with(mylittledata,points(Dose_delivered_mean,exp(logRR)))
      with(mylittledata,rug(Dose_delivered_mean, quiet = TRUE))},error=function(e){cat("ERROR in study",studis[j],":",conditionMessage(e), "\n")})
      
      }#end else
  }#END iterate in studies within drugs
}#END iterate in drugs
dev.off()   



##################################################################################
###       Meta-analysis of each drug                                             ##
##################################################################################
sink("Per drug dose response.txt")
pdf("Per drug dose response.pdf")
for(i in 1:length(dis))
{#iterate in drugs
  cat("***************************","\n")
  cat(paste("Studies in",dis[i],"\n"))
  cat("***************************","\n")
  studis=unique(DOSEsameDrug$Study_No[DOSEsameDrug$Drug==dis[i]])
  mymoredata=DOSEsameDrug[!is.na(match(DOSEsameDrug$Study_No,studis)),]
  mymoredata=mymoredata[!is.na(mymoredata$logRR),]
  a=table(mymoredata$Study_No)
  out=names(a[a<2])
  mymoredata=mymoredata[is.na(match(mymoredata$Study_No,out)),]
  
  cat(paste("There are", length(unique(mymoredata$Study_No)), "studies", "\n"))
  if(max(mymoredata$logRR,na.rm = T)==0 | length(unique(mymoredata$Study_No))<2){cat(paste("not enough efficacy data"),"\n")}
  else{
    text=paste(length(studis),"studies with",unique(mymoredata$Drug)[1],"vs",unique(mymoredata$Drug)[-1])
    
    #linear
    doseresRR=dosresmeta(formula=logRR~Dose_delivered_mean, id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata)
    summary(doseresRR)
    predict(doseresRR,delta=20,exp=T)
    with(predict(doseresRR, expo = TRUE, order = TRUE), {
      plot(Dose_delivered_mean, pred, log = "y", type = "l",
           xlim = c(0, max(Dose_delivered_mean)), ylim = c(.75, 2.5),xlab="Actual mean dose",ylab="RR",main=c("Linear",text))
      lines(Dose_delivered_mean,  ci.lb, lty = 2)
      lines(Dose_delivered_mean, ci.ub, lty = 2)
      rug(Dose_delivered_mean, quiet = TRUE) })
    #with(mymoredata,points(Dose_delivered_mean,exp(logRR)))
    with(mymoredata,rug(Dose_delivered_mean, quiet = TRUE))
    
    #quadratic
    #because quadratic requires ar least three observations per study, we exclude studies with less data
    a=table(mymoredata$Study_No)
    out=names(a[a<3])
    if(length(out)>0)(cat("from the quadratic model I excluded",length(out),"studies because they had 2 dose levels"))
    mymoreQdata=mymoredata[is.na(match(mymoredata$Study_No,out)),]
    tryCatch({ doseresRR=dosresmeta(formula=logRR~Dose_delivered_mean+I(Dose_delivered_mean^2), id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoreQdata)
    summary(doseresRR)
    predict(doseresRR,exp=T)
    with(predict(doseresRR, expo = TRUE, order = TRUE), {
      plot(Dose_delivered_mean, pred, log = "y", type = "l",
           xlim = c(0,max(Dose_delivered_mean)), ylim = c(.75, 2.5),xlab="Actual mean dose",ylab="RR",main=c("Quadratic",text))
      lines(Dose_delivered_mean,  ci.lb, lty = 2)
      lines(Dose_delivered_mean, ci.ub, lty = 2)
      rug(Dose_delivered_mean, quiet = TRUE)})
    #with(mymoreQdata,points(Dose_delivered_mean,exp(logRR)))
    with(mymoreQdata,rug(Dose_delivered_mean, quiet = TRUE))
    },error=function(e){cat("ERROR in Quadratic",":",conditionMessage(e), "\n")})
    
    #cubic splines
    maxdose=max(mymoredata$Dose_delivered_mean)
    mindose=min(mymoredata$Dose_delivered_mean)
    knot0=max(mindose+maxdose/8,maxdose/5)
    knot1=knot0+(maxdose-knot0)/3
    knot2=knot0+2*(maxdose-knot0)/3
    knots=c(knot0,knot1,knot2)
    tryCatch({#start tryCatch to avoid stopping with stupid errors
      doseresRR=dosresmeta(formula=logRR~rcs(Dose_delivered_mean,knots), proc="1stage",id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata)
      summary(doseresRR)
      newdata=data.frame(Dose_delivered_mean=seq(0,maxdose,1))
      xref=min(mymoredata$Dose_delivered_mean)
      with(predict(doseresRR, newdata,xref, exp = TRUE), {
        plot(get("rcs(Dose_delivered_mean, knots)Dose_delivered_mean"),pred, log = "y", type = "l",
             xlim = c(0, maxdose), ylim = c(.75, 2.5),xlab="Actual mean dose",ylab="RR",main=c("Splines",text))
        matlines(get("rcs(Dose_delivered_mean, knots)Dose_delivered_mean"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
      #with(mymoredata,points(Dose_delivered_mean,exp(logRR)))
      with(mymoredata,rug(Dose_delivered_mean, quiet = TRUE))},error=function(e){cat("ERROR in Spline",":",conditionMessage(e), "\n")})
  }#end else
  
}#END iterate in drugs

dev.off() 
sink()

################################################################
### Meta-analysis accross drugs using only the DOSEsameDrug ####
################################################################


pdf("Dose-response using the DOSEsameDrug data.pdf")

################
#1. efficacy 
###############

mymoredata=DOSEsameDrug
mymoredata=mymoredata[!is.na(mymoredata$logRR),]
a=table(mymoredata$Study_No)
out=names(a[a<2])
mymoredata=mymoredata[is.na(match(mymoredata$Study_No,out)),]
cat(paste("There are", length(unique(mymoredata$Study_No)), "studies", "\n"))
  text=paste(length(unique(mymoredata$Study_No)),"studies comparing all drugs and all doses for response")
  
      #linear
      doseresRR=dosresmeta(formula=logRR~Dose_delivered_mean, id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata)
      summary(doseresRR)
      predict(doseresRR,delta=20,exp=T)
      with(predict(doseresRR, expo = TRUE, order = TRUE), {
        plot(Dose_delivered_mean, pred, log = "y", type = "l",
             xlim = c(0, 80), ylim = c(.75, 2.5),xlab="Dose",ylab="RR",main=c("Linear",text))
        lines(Dose_delivered_mean,  ci.lb, lty = 2)
        lines(Dose_delivered_mean, ci.ub, lty = 2)
        rug(Dose_delivered_mean, quiet = TRUE) })
      with(mymoredata,points(Dose_delivered_mean,exp(logRR)))
      
      #quadratic
      #because quadratic requires ar least three observations per study, we exclude studies with less data
      a=table(mymoredata$Study_No)
      out=names(a[a<3])
      if(length(out)>0)(cat("from the quadratic model I excluded",length(out),"studies because they had 2 dose levels"))
      mymoreQdata=mymoredata[is.na(match(mymoredata$Study_No,out)),]
      tryCatch({ doseresRR=dosresmeta(formula=logRR~Dose_delivered_mean+I(Dose_delivered_mean^2), id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoreQdata)
      summary(doseresRR)
      predict(doseresRR,exp=T)
      with(predict(doseresRR, expo = TRUE, order = TRUE), {
        plot(Dose_delivered_mean, pred, log = "y", type = "l",
             xlim = c(0, 80), ylim = c(.75, 2.5),xlab="Dose",ylab="RR",main=c("Quadratic",text))
        lines(Dose_delivered_mean,  ci.lb, lty = 2)
        lines(Dose_delivered_mean, ci.ub, lty = 2)
        rug(Dose_delivered_mean, quiet = TRUE)})
      with(mymoreQdata,points(Dose_delivered_mean,exp(logRR)))},error=function(e){cat("ERROR in Quadratic",":",conditionMessage(e), "\n")})
      
      #cubic splines
      knots=c(10,20,40)
      tryCatch({#start tryCatch to avoid stopping with stupid errors
        doseresRR=dosresmeta(formula=logRR~rcs(Dose_delivered_mean,knots), proc="1stage",id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata)
        summary(doseresRR)
        newdata=data.frame(Dose_delivered_mean=seq(0,80,1))
        xref=min(mymoredata$Dose_delivered_mean)
        with(predict(doseresRR, newdata,xref, exp = TRUE), {
          plot(get("rcs(Dose_delivered_mean, knots)Dose_delivered_mean"),pred, log = "y", type = "l",
               xlim = c(0, 80), ylim = c(.75, 2.5),xlab="Dose",ylab="RR",main=c("Splines",text))
          matlines(get("rcs(Dose_delivered_mean, knots)Dose_delivered_mean"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
        with(mymoredata,points(Dose_delivered_mean,exp(logRR)))
        with(mymoredata,rug(Dose_delivered_mean, quiet = TRUE))},error=function(e){cat("ERROR in Spline",":",conditionMessage(e), "\n")})


dev.off()   




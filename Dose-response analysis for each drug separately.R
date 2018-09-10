

################################################################################################
# Analysis of studies with the same Drug 
##############################################################################################
# Create the data
##################
tableDRUGsStudy=with(DOSE,table(Study_No,Drug))
idtokeep1=unique(DOSE$Study_No)[apply(tableDRUGsStudy,1,max)>=2]#keep studies with at least 2 doses of the same drug
idtokeep2=unique(DOSE$Study_No)[tableDRUGsStudy[,colnames(tableDRUGsStudy)=="placebo"]==1]#keep placebo-controlled
idtokeep=unique(c(idtokeep1,idtokeep2))
DOSE$sameDrug=with(DOSE,!is.na(match(Study_No, idtokeep)))
DOSEsameDrug=DOSE[DOSE$sameDrug,]#create a database that only has studies with the same drug


####    sort report
cat(paste("We have",length(unique(DOSEsameDrug$Study_No)),"studies comparing different doses of the same drug"))
cat("Nr of studies with multiple doses of the same drug")
apply(table(DOSEsameDrug$Drug,DOSEsameDrug$Study_No)>0,1,sum)

dis=names(apply(table(DOSEsameDrug$Drug,DOSEsameDrug$Study_No)>0,1,sum))#names of drugs with multiple doses per arm
dis=dis[dis!="placebo"]

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
  mymoredata=mymoredata[!is.na(match(mymoredata$Drug, c(dis[i],"placebo"))),]

  a=table(mymoredata$Study_No)
  out=names(a[a<2])
  mymoredata=mymoredata[is.na(match(mymoredata$Study_No,out)),]
  
  cat(paste("There are", length(unique(mymoredata$Study_No)), "studies", "\n"))
  

  if(max(mymoredata$logRR,na.rm = T)==0 | length(unique(mymoredata$Study_No))<2){cat(paste("not enough efficacy data"),"\n")}
  else{
    maxdose=max(mymoredata$Dose_delivered_mean)
    mindose=min(mymoredata$Dose_delivered_mean)
   
   
  if(dis[i]=="agomelatine") knots=c(10,20,30)/0.75
    if(dis[i]=="amitriptyline")knots=c(10,20,30)/0.33
      if(dis[i]=="bupropion")knots=c(10,20,30)/0.11
        if(dis[i]=="citalopram")knots=c(10,20,30)/1
          if(dis[i]=="clomipramine")knots=c(10,20,30)/0.34
            if(dis[i]=="desvenlafaxine")knots=c(10,20,30)/0.40
              if(dis[i]=="duloxetine")knots=c(10,20,30)/0.33
                if(dis[i]=="escitalopram")knots=c(10,20,30)/2.22
                  if(dis[i]=="fluoxetine")knots=c(10,20,30)/1
                    if(dis[i]=="fluvoxamine")knots=c(10,20,30)/0.28
                      if(dis[i]=="levomilnacipran")knots=c(10,20,30)/0.67
                        if(dis[i]=="milnacipran")knots=c(10,20,30)/0.20
                          if(dis[i]=="mirtazapine")knots=c(10,20,30)/0.79
                            if(dis[i]=="nefazodone")knots=c(10,20,30)/0.07
                              if(dis[i]=="paroxetine")knots=c(10,20,30)/1.18
                                if(dis[i]=="reboxetine")knots=c(10,20,30)/3.48
                                 if(dis[i]=="sertraline") knots=c(10,20,30)/0.41
                                    if(dis[i]=="trazodone")knots=c(10,20,30)/0.1
                                      if(dis[i]=="venlafaxine")knots=c(10,20,30)/0.27
                                        if(dis[i]=="vilazodone")knots=c(10,20,30)/2
                                          if(dis[i]=="vortioxetine")knots=c(10,20,30)/2
    
    cat(paste("The knots for i=", i, "are:",round(knots), "\n"))
    text=paste(length(studis),"studies with",unique(mymoredata$Drug)[1],"vs",unique(mymoredata$Drug)[-1])

        if(dis[i]=="venlafaxine"){
          mymoredata$Study_No==128
        mymoredata$logRR[1]<-0
        mymoredata$logRR[2]<-mymoredata$logRR[2]-mymoredata$logRR[1]
        mymoredata$selogRR[1]<-NA
        }
    
    #cubic splines
    tryCatch({#start tryCatch to avoid stopping with stupid errors
      doseresRR=dosresmeta(formula=logRR~rcs(Dose_delivered_mean,knots), proc="1stage",id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata)
      summary(doseresRR)
      newdata=data.frame(Dose_delivered_mean=seq(0,maxdose,1))
      xref=min(mymoredata$Dose_delivered_mean)
      with(predict(doseresRR, newdata,xref, exp = TRUE), {
        plot(get("rcs(Dose_delivered_mean, knots)Dose_delivered_mean"),pred, log = "y", type = "l",
             xlim = c(0, maxdose), ylim = c(.75, 2.5),xlab="Actual mean dose",ylab="RR",main=c("Splines",text))
        matlines(get("rcs(Dose_delivered_mean, knots)Dose_delivered_mean"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
      with(mymoredata,rug(Dose_delivered_mean, quiet = TRUE))},error=function(e){cat("ERROR in Spline",":",conditionMessage(e), "\n")})
  
  }#end else
}#END iterate in drugs

dev.off() 
sink()

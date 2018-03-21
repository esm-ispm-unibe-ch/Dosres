#####################  

#create various dosages
myreplace=function(targetvector, replacementvector,selectionvector){
  #targetvector of lenght l has values some of which we want to replace
  #replacementvector of lenght l has the values that you want to copy in a new vector
  #selectionvector of lenght l has T or F indicating which values should be kept
  out=targetvector
  out[selectionvector]=replacementvector[selectionvector]
  out
}


DOSE$bollini_ddd=DOSE$Dose_delivered_mean
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.8,DOSE$Drug=="agomelatine")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.2,DOSE$Drug=="amitriptyline")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.12,DOSE$Drug=="bupropion")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.666,DOSE$Drug=="citalopram")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.2,DOSE$Drug=="clomipramine")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.40,DOSE$Drug=="desvenlafaxine")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.33,DOSE$Drug=="duloxetine")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*1.332,DOSE$Drug=="escitalopram")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*1,DOSE$Drug=="fluoxetine")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.2,DOSE$Drug=="fluvoxamine")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.67,DOSE$Drug=="levomilnacipran")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.20,DOSE$Drug=="milnacipran")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.67,DOSE$Drug=="mirtazapine")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.066,DOSE$Drug=="nefazodone")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*1,DOSE$Drug=="paroxetine")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*2.5,DOSE$Drug=="reboxetine")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.24,DOSE$Drug=="sertraline")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.067,DOSE$Drug=="trazodone")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.2,DOSE$Drug=="venlafaxine")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*2,DOSE$Drug=="vilazodone")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*2,DOSE$Drug=="vortioxetine")


DOSE$hayasaka_ddd=DOSE$Dose_delivered_mean
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*0.75,DOSE$Drug=="agomelatine")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*0.33,DOSE$Drug=="amitriptyline")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*0.11,DOSE$Drug=="bupropion")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*1,DOSE$Drug=="citalopram")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*0.34,DOSE$Drug=="clomipramine")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*0.40,DOSE$Drug=="desvenlafaxine")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*0.33,DOSE$Drug=="duloxetine")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*2.22,DOSE$Drug=="escitalopram")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*1,DOSE$Drug=="fluoxetine")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*0.28,DOSE$Drug=="fluvoxamine")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*0.67,DOSE$Drug=="levomilnacipran")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*0.20,DOSE$Drug=="milnacipran")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*0.79,DOSE$Drug=="mirtazapine")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*0.07,DOSE$Drug=="nefazodone")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*1.18,DOSE$Drug=="paroxetine")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*3.48,DOSE$Drug=="reboxetine")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*0.41,DOSE$Drug=="sertraline")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*0.1,DOSE$Drug=="trazodone")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*0.27,DOSE$Drug=="venlafaxine")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*2,DOSE$Drug=="vilazodone")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*2,DOSE$Drug=="vortioxetine")


DOSE$ddd=DOSE$Dose_delivered_mean
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*0.8,DOSE$Drug=="agomelatine")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*0.27,DOSE$Drug=="amitriptyline")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*0.067,DOSE$Drug=="bupropion")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*1,DOSE$Drug=="citalopram")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*0.20,DOSE$Drug=="clomipramine")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*0.40,DOSE$Drug=="desvenlafaxine")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*0.33,DOSE$Drug=="duloxetine")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*2,DOSE$Drug=="escitalopram")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*1,DOSE$Drug=="fluoxetine")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*0.20,DOSE$Drug=="fluvoxamine")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*0.67,DOSE$Drug=="levomilnacipran")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*0.20,DOSE$Drug=="milnacipran")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*0.67,DOSE$Drug=="mirtazapine")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*0.05,DOSE$Drug=="nefazodone")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*1,DOSE$Drug=="paroxetine")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*2.5,DOSE$Drug=="reboxetine")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*0.40,DOSE$Drug=="sertraline")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*0.067,DOSE$Drug=="trazodone")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*0.20,DOSE$Drug=="venlafaxine")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*2,DOSE$Drug=="vilazodone")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*2,DOSE$Drug=="vortioxetine")



### KEEP ONLY USEFUL VARIABLES AND ORDER THE DATABASE SO THAT WE HAVE WITHIN EACH STUDY PLACEBO OR LEAST DOSE FIRST
DOSE=DOSE[,c("Study_No","No of arms","Study_year","Drug","Dose_range","No_randomised","Responders","Dropouts_total","Dropouts_sideeffects","N compimputed","Mean","SD","hayasaka_ddd","ddd", "bollini_ddd")]
DOSE=DOSE[with(DOSE,order(Study_No,hayasaka_ddd)),]
#exclude single-arm studies
DOSE=exludesinglearmsdata.fun(DOSE,Study_No)

#CREATE STUDY TYPE
DOSE$type="cc"
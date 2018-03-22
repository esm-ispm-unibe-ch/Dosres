            ############################################################
            #         Master analysis for Dose-response in Griselda
            ############################################################
            
#load libraries
library(netmeta)
library(meta)
library(metafor)
library(readxl)
library(dosresmeta)
library(rms)
            
#Load the functions needed
source("Functions needed in the dose-response analysis.R")
            
            #########################
            #load data and prepare
            GRISELDAdose <- read_excel("GRISELDAdose.xlsx",  na = "NA")
            GRISELDAdose<-as.data.frame(GRISELDAdose)
            #delete studies that have missing dose 
            GRISELDAdose=GRISELDAdose[!is.na(GRISELDAdose$Dose_delivered_mean),]
            #delete single arm studies
            DOSE=GRISELDAdose
            DOSE=exludesinglearmsdata.fun(DOSE,Study_No)
            
            #now we need to re-create the dose (according to scripts) in the datafile DOSE
            source("Create dose in Griselda datafile DOSE.R")
            
            
            # Create two other datasets according to the hayasaka_ddd dose 
            DOSEless30=DOSE[with(DOSE,c(hayasaka_ddd<30 & hayasaka_ddd>0)),] #doses only very low, below 30
            DOSEless30=exludesinglearmsdata.fun(DOSEless30,Study_No)
            DOSEtheur=DOSE[with(DOSE,c(hayasaka_ddd>=20  & hayasaka_ddd<=80)),]#doses between 20 and 80
            DOSEtheur=exludesinglearmsdata.fun(DOSEtheur,Study_No)
            
            
            ################################################################################################
            #  Produce per arm LOG RR for RESPONSE, DROPOUT AND DROPOUT DUE TO AE
            ###############################################################################################
            
            DOSE=createdatasetdoseresponse.fun(DOSE,Responders,No_randomised,Study_No,hayasaka_ddd,nameoflogRR="logRR",nameofselogRR="selogRR")
            DOSE=createdatasetdoseresponse.fun(DOSE,Dropouts_total,No_randomised,Study_No,hayasaka_ddd,nameoflogRR="logRRdrop",nameofselogRR="selogRRdrop")
            DOSE=createdatasetdoseresponse.fun(DOSE,Dropouts_sideeffects,No_randomised,Study_No,hayasaka_ddd,nameoflogRR="logRRdropAE",nameofselogRR="selogRRdropAE")
            
            DOSEless30=createdatasetdoseresponse.fun(DOSEless30,Responders,No_randomised,Study_No,hayasaka_ddd,nameoflogRR="logRR",nameofselogRR="selogRR")
            DOSEless30=createdatasetdoseresponse.fun(DOSEless30,Dropouts_total,No_randomised,Study_No,hayasaka_ddd,nameoflogRR="logRRdrop",nameofselogRR="selogRRdrop")
            DOSEless30=createdatasetdoseresponse.fun(DOSEless30,Dropouts_sideeffects,No_randomised,Study_No,hayasaka_ddd,nameoflogRR="logRRdropAE",nameofselogRR="selogRRdropAE")
            
            DOSEtheur=createdatasetdoseresponse.fun(DOSEtheur,Responders,No_randomised,Study_No,hayasaka_ddd,nameoflogRR="logRR",nameofselogRR="selogRR")
            DOSEtheur=createdatasetdoseresponse.fun(DOSEtheur,Dropouts_total,No_randomised,Study_No,hayasaka_ddd,nameoflogRR="logRRdrop",nameofselogRR="selogRRdrop")
            DOSEtheur=createdatasetdoseresponse.fun(DOSEtheur,Dropouts_sideeffects,No_randomised,Study_No,hayasaka_ddd,nameoflogRR="logRRdropAE",nameofselogRR="selogRRdropAE")
            
            ################################################################################################
            #  Analyses
            ###############################################################################################
            
            # to produce graphs for each study and drug separately run
            source("Dose-response analysis for each study and each drug separately.R")
            # to produce graphs for all drugs and doses together including placebo
            source("Dosres all study designs all doses.R")
            # to produce graphs for all drugs at theurapeutic doses
            source("Dosres all study designs for therapeutic doses.R")
            # to produce graphs for all drugs given between 0.75 and 30 mg
            source("Dosres all study designs 075 to 30.R")
            
            rm(list=ls())
            
            
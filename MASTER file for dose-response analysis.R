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
            #Exclude flexible doses studies
            DOSE=GRISELDAdose[GRISELDAdose$Dosing_schedule=="Fixed",]
            #delete single arm studies
            DOSE=exludesinglearmsdata.fun(DOSE,Study_No)
            DOSE1=DOSE
            
            #now we need to re-create the dose (according to scripts) in the datafile DOSE
            #it also creates the databases needed for subgroup analysis DOSEless30 and DOSEtheur according to the hayasaka_ddd dose 
            # and a database DOSEj 
            source("Create dose in Griselda datafile DOSE.R")
           
           
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
            
            #keep the old database with many characteristics
            keepit=!is.na(match((DOSE1$Study_No),unique(DOSE$Study_No)))
            DOSE1=DOSE1[ keepit, ]
            write.csv(DOSE1,"Data to be used only for study reporting.csv")
            
            ####SENSITIVITY ANALYSES
            
            #Sensitivity in number and position of knots 
            source("Sensitivity analysis to knots Dosres all study designs all doses.R")
            
            #Sensitivity in dose transformation using Jakubovski_ddd
            source("Sensitivity analysis Jakubovski Dosres all study designs all doses.R")
            
            #Subgroup analysis SSRIs 
            source("Sensitivity analysis SSRIs Dosres all study designs all doses.R")
            
            #Subgroup analysis SNRIs 
            source("Sensitivity analysis SNRIs Dosres all study designs all doses.R")
            
            # an educational plot and summary for 12 studies with dose between 20 and 60 for response
            source("Educational analysis Dosres 7 study for therapeutic doses.R")
            rm(list=ls())
            
            
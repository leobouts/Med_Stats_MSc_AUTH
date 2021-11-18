#Load package
library(TCGAbiolinks) 

#Summary of TCGA-COAD project
TCGAbiolinks:::getProjectSummary("TCGA-COAD")

#Get molecular subtype per patient
mol.subtype <- TCGAquery_subtype(tumor = "COAD")

## coad subtype information from:doi:10.1038/nature11252


#Load package
library(survminer)

## Loading required package: ggplot2

## Loading required package: ggpubr

#Get clinical data per patient
clinical.coad <- GDCquery_clinic("TCGA-COAD", "clinical")
TCGAanalyze_survival(clinical.coad,
                     clusterCol = "gender",
                     main = "Gender survival analysis")

## File saved as: survival.pdf

sub.clinical.coad <- subset(clinical.coad, tissue_or_organ_of_origin %in% c("Cecum", "Ascending colon", "Descending colon","Sigmoid colon")) 

sub.clinical.coad$side <- ifelse(sub.clinical.coad$tissue_or_organ_of_origin %in% c("Cecum", "Ascending colon"), "right side", "left side") 

TCGAanalyze_survival(sub.clinical.coad, "side", main = "TCGA-COAD Survival analysis")

## File saved as: survival.pdf

sub.clinical.coad <- subset(clinical.coad, tissue_or_organ_of_origin %in% c("Cecum", "Ascending colon", "Descending colon","Sigmoid colon")) 

sub.clinical.coad$side <- ifelse(sub.clinical.coad$tissue_or_organ_of_origin %in% c("Cecum", "Ascending colon"), "right side", "left side") 

TCGAanalyze_survival(sub.clinical.coad, "side", main = "TCGA-COAD Survival analysis")




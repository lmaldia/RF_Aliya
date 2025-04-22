
#Install the packages, IF YOU NEED TO :)
install.packages("tidyverse")
install.packages("vegan")
install.packages("devtools")
library(devtools)
devtools::install_github("jbisanz/qiime2R")

#Load the packages. Everyone needs to do this.
library(tidyverse)
library(vegan)
library(qiime2R)
library(stringr)


##############################################
#Set UP
#
#These are the things that  we need from Qiime:
#
#sample-metadata.tsv
#core-metrics-results/bray_curtis_pcoa_results.qza
#core-metrics-results/weighted_unifrac_pcoa_results.qza
#core-metrics-results/rarefied_table.qza
#rooted-tree.qza
#taxonomy.qza
#core-metrics-results/evenness_vector.qza
#core-metrics-results/faith_pd_vector.qza
#core-metrics-results/observed_otus_vector.qza
#core-metrics-results/shannon_vector.qza
#
# These files are already in the ANSC516-repo
##############################################

#getwd("/data")
###Set your working directory path/to/ANSC516/ANSC-repo/data/moving-pictures
setwd("C:/Users/lilly/OneDrive - purdue.edu/spring '25/ansc516/qiime2R_tutorial/ANSC516-master/ANSC516-master/data/moving-pictures")

list.files()

if(!dir.exists("output"))
  dir.create("output")



#How to load a file into R
metadata2 <- read.delim("sample-metadata.tsv", sep = "\t", header = T, quote = "", stringsAsFactors = F)
metadata2[1,]
metadata2[,1]


# When subsetting, the first number is the row and after the comma is the column
metadata2 <- metadata2[-1,]

#Now the qiime2R method
metadata<-read_q2metadata("sample-metadata.tsv")
str(metadata)
#levels(metadata$`body-site`)
#colnames(metadata)[3] <- "body.site"
#colnames(metadata)[8] <- "reported.antibiotic.usage"
#colnames(metadata)[9] <- "days.since.experiment.start"
str(metadata)

row.names(metadata) <- metadata[,1]
row.names(metadata) <- metadata$SampleID
metadata <- metadata[,-1]
row.names(metadata)

bc_PCoA<-read_qza("core-metrics-results/bray_curtis_pcoa_results.qza")
wUF_PCoA <- read_qza("core-metrics-results/weighted_unifrac_pcoa_results.qza")

my_colors <- c('#f4a582','#b2182b','#edf8b1','#7fcdbb','#253494','#081d58','#41b6c4',
               '#000','#1d91c0','#c7e9b4','#225ea8','#dfc27d','#8c510a')
my_column <- row.names(bc_meta_final)
my_data = c('Aliya')

#W2 (green to blue)

#'#f4a582','#dfc27d','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#b2182b','#081d58','#8c510a','#000'

#W4

'#8c510a'
'#dfc27d'

#influent(red)

'#b2182b'
'#f4a582'

bc_meta <- bc_PCoA$data$Vectors %>%
  select(SampleID, PC1, PC2, PC3)

row.names(bc_meta) <- bc_meta[,1]
row.names(bc_meta) <- bc_meta$SampleID
bc_meta <- bc_meta[,-1]

## Because Selecting Data takes so long I reused all of the bc_meta variables 
## for all beta diversity plots

## Rearrange bc_meta to have RF samples only

bc_meta2 <- bc_meta
bc_meta2 <- Jac_meta
bc_meta2 <- UWuni_meta
bc_meta2 <- Wuni_meta

#Remove Unnecessary Walls

bc_meta3 <- bc_meta2[!str_detect(bc_meta2$SampleID,"W1."),]
bc_meta4 <- bc_meta3[!str_detect(bc_meta3$SampleID,"W3."),]

# Remove Pipes (Keep only water heaters)

bc_meta5 <- bc_meta4[-1,]
bc_meta6 <- bc_meta5[!str_detect(bc_meta5$SampleID,"A"),]
bc_meta7 <- bc_meta6[!str_detect(bc_meta6$SampleID,"B"),]
bc_meta8 <- bc_meta7[!str_detect(bc_meta7$SampleID,"C"),]

#Select the Right Dates

bc_meta9 <- bc_meta8[str_detect(bc_meta8$SampleID,"22"),]
bc_meta10 <- bc_meta9[!str_detect(bc_meta9$SampleID,"07"),]
bc_meta11 <- bc_meta10[!str_detect(bc_meta10$SampleID,"2201"),]
bc_meta12 <- bc_meta11[!str_detect(bc_meta11$SampleID,"2202"),]
bc_meta13 <- bc_meta12[!str_detect(bc_meta12$SampleID,"2203"),]
bc_meta14 <- bc_meta13[!str_detect(bc_meta13$SampleID,"2204"),]
bc_meta15 <- bc_meta14[!str_detect(bc_meta14$SampleID,"220608"),]
bc_meta16 <- bc_meta15[!str_detect(bc_meta15$SampleID,"220615"),]
bc_meta17 <- bc_meta16[!str_detect(bc_meta16$SampleID,"220622"),]
bc_meta18 <- bc_meta17[!str_detect(bc_meta17$SampleID,"220628"),]
bc_meta19 <- bc_meta18[!str_detect(bc_meta18$SampleID,"220629"),]
bc_meta20 <- bc_meta19[!str_detect(bc_meta19$SampleID,"HE.2205"),]

#Put Sample Names First

row.names(bc_meta20) <- bc_meta20[,1]
row.names(bc_meta20) <- bc_meta20$SampleID
bc_meta21 <- bc_meta20[,-1]

bc_meta_final <- bc_meta21[c(1,3,12,10,4,7,9,11,5,6,2,8,13),]
Jac_meta_final <- bc_meta21[c(1,3,12,10,4,7,9,11,5,6,2,8,13),]
UWuni_meta_final <- bc_meta21[c(1,3,12,10,4,7,9,11,5,6,2,8,13),]
Wuni_meta_final <- bc_meta21[c(1,3,12,10,4,7,9,11,5,6,2,8,13),]

# Now we are going to make an ordination plot
ggplot(bc_meta_final, aes(x=PC1, y=PC2, color=my_column)) +
  geom_point(size = 3) + #alpha controls transparency and helps when points are overlapping
  xlab(paste0("PC1 (", round(100*bc_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*bc_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme_minimal() +
  scale_color_manual(values=my_colors, name = 'Sample')
ggsave(paste0("output/BC_", my_data,".pdf"), height=4.5, width=5.5, device="pdf") # save a PDF 3 inches by 4 inches

#'BF.Influent','BF.W2.HE','BF.W4.HE','RF.5Gal','RF.10Gal','RF.20Gal','RF.40Gal','RF.60Gal','RF.120Gal','RF.180Gal','AF.W2.HE','AF.W4.HE','AF.Influent'


##################################################################################
## SAME thing but with Jaccard

Jac_PCoA<-read_qza("core-metrics-results/jaccard_pcoa_results.qza")

Jac_meta <- Jac_PCoA$data$Vectors %>%
  select(SampleID, PC1, PC2) 


ggplot(Jac_meta_final, aes(x=PC1, y=PC2, color=my_column)) +
  geom_point(size = 3) + #alpha controls transparency and helps when points are overlapping
  #geom_point(data=centroids, size = 3) +
  xlab(paste0("PC1 (", round(100*Jac_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*Jac_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme_minimal() +
  scale_color_manual(values=my_colors, name = 'Sample')
ggsave(paste0("output/Jac_", my_data,".pdf"), height=4.5, width=5.5, device="pdf") # save a PDF 3 inches by 4 inches


##################################################################################
## SAME thing but with Unweighted Unifrac

UWuni_PCoA<-read_qza("core-metrics-results/unweighted_unifrac_pcoa_results.qza")

UWuni_meta <- UWuni_PCoA$data$Vectors %>%
  select(SampleID, PC1, PC2)

ggplot(UWuni_meta_final, aes(x=PC1, y=PC2, color=my_column)) +
  geom_point(size = 3) + #alpha controls transparency and helps when points are overlapping
  #geom_point(data=centroids, size = 3) +
  xlab(paste0("PC1 (", round(100*UWuni_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*UWuni_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme_minimal() +
  scale_color_manual(values=my_colors, name = 'Sample')
ggsave(paste0("output/UWuni_", my_data,".pdf"), height=4.5, width=5.5, device="pdf")# save a PDF 3 inches by 4 inches

##################################################################################
## SAME thing but with weighted UniFrac

Wuni_PCoA<-read_qza("core-metrics-results/weighted_unifrac_pcoa_results.qza")

Wuni_meta <- Wuni_PCoA$data$Vectors %>%
  select(SampleID, PC1, PC2)

ggplot(Wuni_meta_final, aes(x=PC1, y=PC2, color=my_column)) +
  geom_point(size = 3) + #alpha controls transparency and helps when points are overlapping
  #geom_point(data=centroids, size = 3) +
  xlab(paste0("PC1 (", round(100*Wuni_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*Wuni_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme_minimal() +
  scale_color_manual(values=my_colors, name = 'Sample')
ggsave(paste0("output/Wuni_", my_data,".pdf"), height=4.5, width=5.5, device="pdf") # save a PDF 3 inches by 4 inches


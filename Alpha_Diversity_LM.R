#############################################################
# title: "Alpha diversity in R - qiime2 output"
# author: "ANSC595"
# date: "March 16, 2021"
##############################################################

#The first step is very important. You need to set your working 
#directory. Just like in unix we have to `cd` to where our data is, the 
#same thing is true for R.
##############################################

#So where `~/Desktop/ANSC595/moving-pictures` is in the code below, you 
#need to enter the path to where you saved the tutorial or your data.

setwd("~/Desktop/ANSC516/2023/ANSC516-repo/data/moving-pictures")
list.files()

# Modified from the original online version available at 
# http://rpubs.com/dillmcfarlan/R_microbiotaSOP

# and Tutorial: Integrating QIIME2 and R for data visualization 
# and analysis using qiime2R
# https://forum.qiime2.org/t/tutorial-integrating-qiime2-and-r-for-data-visualization-and-analysis-using-qiime2r/4121

##Goal
# The goal of this tutorial is to demonstrate basic analyses of microbiota data to determine if and how communities differ by variables of interest. In general, this pipeline can be used for any microbiota data set that has been clustered into operational taxonomic units (OTUs).
#
# This tutorial assumes some basic statistical knowledge. Please consider if your data fit the assumptions of each test (normality? equal sampling? Etc.). If you are not familiar with statistics at this level, we strongly recommend collaborating with someone who is. The incorrect use of statistics is a pervasive and serious problem in the sciences so don't become part of the problem! That said, this is an introductory tutorial and there are many, many further analyses that can be done with microbiota data. Hopefully, this is just the start for your data!

##Data
# The data used here are from the qiime2 moving pictures tutorial. 
# Please see their online tutorial for an explanation of the dataset.

##Files
# We will use the following files created using the qiime2 moving pictures tutorial.

# core-metrics-results/evenness_vector.qza (alpha diversity)
# core-metrics-results/faith_pd_vector.qza (alpha diversity)
# core-metrics-results/observed_features_vector.qza (alpha diversity)
# core-metrics-results/shannon_vector.qza (alpha diversity)
# sample-metadata.tsv (metadata)


# Data manipulation
## Load Packages

library(tidyverse)
library(qiime2R)
library(ggpubr)

##Load Data
# In the code, the text before = is what the file will be called in R. 
# Make this short but unique as this is how you will tell R to use this 
# file in later commands.

# header: tells R that the first row is column names, not data
# row.names: tells R that the first column is row names, not data
# sep: tells R that the data are tab-delimited. 
# If you had a comma-delimited file, you would us sep=","

# Load data

meta<-read_q2metadata("sample-metadata.tsv")
str(meta)
colnames(meta)[3] <- "body.site"
colnames(meta)[8] <- "reported.antibiotic.usage"
colnames(meta)[9] <- "days.since.experiment.start"
str(meta)

evenness = read_qza("core-metrics-results/evenness_vector.qza")
evenness<-evenness$data %>% rownames_to_column("SampleID") # this moves the sample names to a new column that matches the metadata and allows them to be merged

observed_features = read_qza("core-metrics-results/observed_features_vector.qza")
observed_features<-observed_features$data %>% rownames_to_column("SampleID") # this moves the sample names to a new column that matches the metadata and allows them to be merged

shannon = read_qza("core-metrics-results/shannon_vector.qza")
shannon<-shannon$data %>% rownames_to_column("SampleID") # this moves the sample names to a new column that matches the metadata and allows them to be merged

faith_pd = read_qza("core-metrics-results/faith_pd_vector.qza")
faith_pd<-faith_pd$data %>% rownames_to_column("SampleID") # this moves the sample names to a new column that matches the metadata and allows them to be merged\

## Clean up the data
# You can look at your data by clicking on it in the upper-right 
# quadrant "Environment"

# You always need to check the data types in your tables to make 
# sure they are what you want. We will now change some data types 
# in the meta now

str(meta)
#observed_features$observed_features_num <- lapply(observed_features$observed_features, as.numeric)
#observed_features$observed_features <- as.numeric(observed_features$observed_features)
str(observed_features)



###Alpha Diversity tables
# These tables will be merged for convenience and added to the 
# metadata table as the original tutorial was organized.

alpha_diversity = merge(x=faith_pd, y=evenness, by.x = "SampleID", by.y = "SampleID")
alpha_diversity = merge(alpha_diversity, observed_features, by.x = "SampleID", by.y = "SampleID")
alpha_diversity = merge(alpha_diversity, shannon, by.x = "SampleID", by.y = "SampleID")
meta = merge(meta, alpha_diversity, by.x = "SampleID", by.y = "SampleID")


bc_meta2 <- meta

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

meta <- bc_meta21[c(1,3,12,10,4,7,9,11,5,6,2,8,13),]

meta[1,19] <- c('BF.Influent')

#meta = meta[,-1]
str(meta)

my_colors <- c('#f4a582','#b2182b','#edf8b1','#7fcdbb','#253494','#081d58','#41b6c4',
               '#000','#1d91c0','#c7e9b4','#225ea8','#dfc27d','#8c510a')
my_column <- row.names(meta)

#Alpha-diversity
# Alpha-diversity is within sample diversity. It is how many 
# different species (OTUs) are in each sample (richness) and how 
# evenly they are distributed (evenness), which together are diversity. 
# Each sample has one value for each metric.


##Explore alpha metrics
# Now we will start to look at our data. We will first start with 
# alpha-diversity and richness. 
#
# You want the data to be roughly normal so that you can run ANOVA 
# or t-tests. If it is not normally distributed, you will need to 
# consider if you should normalize the data or usenon-parametric 
# tests such as Kruskal-Wallis.

# Here, we see that none of the data are normally distributed, 
# with the exception of "Faith" and "Observed Features".


#Plots
hist(meta$shannon_entropy, main="Shannon diversity", xlab="", breaks=10)
hist(meta$faith_pd, main="Faith phylogenetic diversity", xlab="", breaks=10)
hist(meta$pielou_e, main="Evenness", xlab="", breaks=10)
hist(as.numeric(meta$observed_features), main="Observed Features", xlab="", breaks=10)

#Plots the qq-plot for residuals
ggqqplot(meta$shannon_entropy, title = "Shannon")
ggqqplot(meta$faith_pd, title = "Faith PD")
ggqqplot(meta$pielou_e, title = "Evenness")
ggqqplot(meta$observed_features, title = "Observed Features")

install.packages("ggpubr")
library("ggpubr")

# To test for normalcy statistically, we can run the Shapiro-Wilk 
# test of normality.

shapiro.test(meta$shannon)              #W = 0.88489, p-value = 0.08309
shapiro.test(meta$faith_pd)             #W = 0.7724, p-value = 0.003264
shapiro.test(meta$pielou_e)             #W = 0.93759, p-value = 0.4265
shapiro.test(meta$observed_features)    #W = 0.73928, p-value = 0.001412

# The null hypothesis of these tests is that “sample distribution 
# is normal”. If the test is significant, the distribution is non-normal.

# Now, the above graph is kind of not correct. Our test and our graphic do not exactly match. ANOVA and Tukey are tests based on the mean, but the boxplot plots the median. Its not wrong, its just not the best method. Unfortunately plotting the average and standard deviation is a little complicated.

evenness_summary <- meta %>% # the names of the new data frame and the data frame to be summarised
  group_by(RF.Sort) %>%   # the grouping variable
  summarise(mean_evenness = mean(pielou_evenness),  # calculates the mean of each group
            sd_evenness = sd(pielou_evenness), # calculates the standard deviation of each group
            n_evenness = n(),  # calculates the sample size per group
            se_evenness = sd(pielou_evenness)/sqrt(n())) # calculates the standard error of each group

# We can now make a bar plot of means vs body site, with standard 
# deviations or standard errors as the error bar. The following code 
# uses the standard deviations.

evenness_se <- ggplot(evenness_summary, aes(my_column, mean_evenness, fill =my_column)) + 
  geom_col() + 
  geom_errorbar(aes(ymin = mean_evenness - se_evenness, ymax = mean_evenness + se_evenness), width=0.2) + 
  theme_q2r() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y="Pielou's evenness  ± s.e.", x = "Sample") +
  scale_fill_manual(values=my_colors, name = 'Sample')

ggsave("output/evenness_se.png", evenness_se, height = 5, width = 5)

###################################################################################

#Faiths


faith_summary <- meta %>% # the names of the new data frame and the data frame to be summarised
  group_by(RF.Sort) %>%   # the grouping variable
  summarise(mean_faith = mean(faith_pd),  # calculates the mean of each group
            sd_faith = sd(faith_pd), # calculates the standard deviation of each group
            n_faith = n(),  # calculates the sample size per group
            se_faith = sd(faith_pd)/sqrt(n())) # calculates the standard error of each group

# We can now make a bar plot of means vs body site, with standard 
# deviations or standard errors as the error bar. The following code 
# uses the standard deviations.

faith_se <- ggplot(faith_summary, aes(my_column, mean_faith, fill =my_column)) + 
  geom_col() + 
  geom_errorbar(aes(ymin = mean_faith - se_faith, ymax = mean_faith + se_faith), width=0.2) + 
  theme_q2r() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y="Faith's PD  ± s.e.", x = "Sample") +
  scale_fill_manual(values=my_colors, name = 'Sample')

ggsave("output/faith_se.png", faith_se, height = 5, width = 5)

###################################################################################

#Shannon


shannon_summary <- meta %>% # the names of the new data frame and the data frame to be summarised
  group_by(RF.Sort) %>%   # the grouping variable
  summarise(mean_shannon = mean(shannon_entropy),  # calculates the mean of each group
            sd_shannon = sd(shannon_entropy), # calculates the standard deviation of each group
            n_shannon = n(),  # calculates the sample size per group
            se_shannon = sd(shannon_entropy)/sqrt(n())) # calculates the standard error of each group

# We can now make a bar plot of means vs body site, with standard 
# deviations or standard errors as the error bar. The following code 
# uses the standard deviations.

shannon_se <- ggplot(shannon_summary, aes(my_column, mean_shannon, fill =my_column)) + 
  geom_col() + 
  geom_errorbar(aes(ymin = mean_shannon - se_shannon, ymax = mean_shannon + se_shannon), width=0.2) + 
  theme_q2r() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y="Shannon's Entropy  ± s.e.", x = "Sample") +
  scale_fill_manual(values=my_colors, name = 'Sample')

ggsave("output/shannon_se.png", shannon_se, height = 5, width = 5)

###################################################################################

#features


features_summary <- meta %>% # the names of the new data frame and the data frame to be summarised
  group_by(RF.Sort) %>%   # the grouping variable
  summarise(mean_features = mean(observed_features),  # calculates the mean of each group
            sd_features = sd(observed_features), # calculates the standard deviation of each group
            n_features = n(),  # calculates the sample size per group
            se_features = sd(observed_features)/sqrt(n())) # calculates the standard error of each group

# We can now make a bar plot of means vs body site, with standard 
# deviations or standard errors as the error bar. The following code 
# uses the standard deviations.

features_se <- ggplot(features_summary, aes(my_column, mean_features, fill =my_column)) + 
  geom_col() + 
  geom_errorbar(aes(ymin = mean_features - se_features, ymax = mean_features + se_features), width=0.2) + 
  theme_q2r() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y="Observed Features  ± s.e.", x = "Sample") +
  scale_fill_manual(values=my_colors, name = 'Sample')

ggsave("output/features_se.png", features_se, height = 5, width = 5)


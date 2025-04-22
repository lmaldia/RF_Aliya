#!/bin/sh -l

#SBATCH --nodes=4                    ##number of requested nodes (a node is a group of CPUs)
#SBATCH -n 60		             ##number of requested CPUs
#SBATCH --time=08:00:00              ##time requested
#SBATCH --job-name Lilly-Water       ##name of job
#SBATCH -A microbiome                ##name of the queue you are using. Could be scholar or microbiome if you are on snyder

#########################################################################

echo "Start time"
date +"%d %B %Y %H:%M:%S"

#step 1, cd into the proper directory. This directory must already exist

cd $RCAC_SCRATCH
cd raw_data/qiime_combined

pwd

#step 2, Load Modules

#echo "loading bioinfo"
#module load bioinfo

#echo "loading qiime2-2023.9"
#module load Qiime/2-2023.9

## Demux was done in a different slurm file

#------------------------------------------------------------

#echo "Begin DADA2"

#qiime dada2 denoise-paired \
# 	--i-demultiplexed-seqs demux-full-new.qza \
#  	--p-trim-left-f 13 \
#  	--p-trim-left-r 13 \
# 	--p-trunc-len-f 150 \
#  	--p-trunc-len-r 150 \
#  	--o-table table.qza \
#  	--o-representative-sequences rep-seqs.qza \
#	--o-denoising-stats stats-dada2.qza

#------------------------------------------------------------

#echo "Feature Table Summarization"

#qiime feature-table summarize \
#	--i-table table.qza \
# 	--o-visualization table.qzv \
#	--m-sample-metadata-file Walls_MetaData_2025.txt


#qiime feature-table tabulate-seqs \
# 	--i-data rep-seqs.qza \
#	--o-visualization rep-seqs.qzv

#------------------------------------------------------------

#echo "Visualize Denoising Stats"

#qiime metadata tabulate \
# 	--m-input-file stats_dada2.qza \
#	--o-visualization stats_dada2.qzv

#------------------------------------------------------------

#echo "Generate a Tree"

#qiime phylogeny align-to-tree-mafft-fasttree \
#	--i-sequences rep-seqs.qza \
#	--o-alignment aligned-rep-seqs.qza \
#	--o-masked-alignment masked-aligned-rep-seqs.qza \
#	--o-tree unrooted-tree.qza \
#	--o-rooted-tree rooted-tree.qza

#echo "Alpha and Beta Diversity"

#qiime diversity core-metrics-phylogenetic \
#	--i-phylogeny rooted-tree.qza \
#	--i-table table.qza \
#	--p-sampling-depth 1103 \
#	--m-metadata-file Walls_MetaData_2025.txt \
#	--output-dir core-metrics-results

#qiime diversity alpha-group-significance \
#	--i-alpha-diversity core-metrics-results/faith_pd_vector.qza \
#	--m-metadata-file Walls_MetaData_2025.txt \
#	--o-visualization core-metrics-results/faith-pd-group-significance.qzv

#qiime diversity alpha-group-significance \
#	--i-alpha-diversity core-metrics-results/evenness_vector.qza \
#	--m-metadata-file Walls_MetaData_2025.txt \
#	--o-visualization core-metrics-results/evenness-group-significance.qzv

#qiime diversity beta-group-significance \
#	--i-distance-matrix core-metrics-results/unweighted_unifrac_distance_matrix.qza \
#	--m-metadata-file Walls_MetaData_2025.txt \
#	--m-metadata-column Event \
#	--o-visualization core-metrics-results/unweighted-unifrac-event-significance.qzv \
#	--p-pairwise

#qiime diversity beta-group-significance \
#	--i-distance-matrix core-metrics-results/unweighted_unifrac_distance_matrix.qza \
#	--m-metadata-file Walls_MetaData_2025.txt \
#	--m-metadata-column Exp \
#	--o-visualization core-metrics-results/unweighted-unifrac-Exp-significance.qzv \
#	--p-pairwise

#----------------------------------------------------

wget \
  -O "gg-13-8-99-515-806-nb-classifier.qza" \
  "https://data.qiime2.org/classifiers/sklearn-1.4.2/greengenes/gg-13-8-99-515-806-nb-classifier.qza"

echo "Classify and get .qzv"
qiime feature-classifier classify-sklearn \
  --i-classifier gg-13-8-99-515-806-nb-classifier.qza \
  --i-reads rep-seqs.qza \
  --o-classification taxonomy.qza

qiime metadata tabulate \
  --m-input-file taxonomy.qza \
  --o-visualization taxonomy.qzv

qiime taxa barplot \
  --i-table table.qza \
  --i-taxonomy taxonomy.qza \
  --m-metadata-file Walls_MetaData_2025.txt \
  --o-visualization taxa-bar-plots.qzv

echo "End time"
date +"%d %B %Y %H:%M:%S"

#to run: sbatch qiime_lmaldia_full.slurm

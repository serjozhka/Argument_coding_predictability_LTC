## This is the R code used to analyze the BivalTyp data for the paper
## entitled "Cross-linguistic argument-coding predictability and the lexical meaning of the verb"
## submitted to "Linguistic Typology at the Crossroads"

# PART 1. Some preparatory actions 

# Chapter 1.1. Install packages
## Install packages for measuring entropy and MI
install.packages('infotheo')
library(infotheo)

## Install a package for nice plots
install.packages('ggpubr')
library(ggpubr)

## Install a package for nice maps
install.packages('lingtypology', dependencies = TRUE)
library(lingtypology)

## Install packages with nice palettes
install.packages('colorspace')
library(colorspace)
install.packages('RColorBrewer')
library(RColorBrewer)

## Install a package for data manipulation
install.packages("dplyr")
library(dplyr)

## Install packages for reading and writing .xlsx files
install.packages("readxl")
library(readxl)

install.packages("writexl")
library(writexl)

## install a package to save html objects
install.packages('htmlwidgets')
library(htmlwidgets) 

## Install "leaflet"
install.packages('leaflet')
library(leaflet)

## Install packages to avoid intersections between labels
install.packages('ggplot2')
library(ggplot2)
install.packages('ggrepel')
library(ggrepel)

# Chapter 1.2. Read the data from files 

## Read the languages_preprocessed.xlsx file: info on the lgs in the BivalTyp dataset
## While pre-processing, I manually erase all columns except 
## language_no, language, language_external, expert, macroarea, family_WALS, genus_WALS, latitude, longitude, number_nominal_cases, glottocode, WO_WALS

languages <- read_excel(file.choose())
languages <- as.data.frame(languages)

no_languages <- nrow(languages)

## Read the data_for_download_preprocessed.xlsx file: 
## info valency patterns in the BivalTyp dataset
## While pre-processing, I manually erase all columns except 
## language_no, predicate_no, X, Y, locus, valency_pattern

# data <- read.table(file = 'clipboard', sep = '\t', header = T)
data <- read_excel(file.choose())
data <- as.data.frame(data)

## Read the file with predicates and their numbers (predicates.xlsx). 
## Before that, manually replace all "#" with "_"

predicates <- read_excel(file.choose())
predicates <- as.data.frame(predicates)
no_predicates <- nrow(predicates)

## Read the language_stats.xlsx file with the basic statistics on languages 
language_stats <- read_excel(file.choose())
language_stats <- as.data.frame(language_stats)

## Create backup copies of all raw files for the case something goes wrong

backup_data <- data
backup_languages <- languages
backup_predicates <- predicates
backup_language_stats <- language_stats

# Chapter 1.3. Streamline and beautify the dataframes

## Replace blanks and asterisks with 'NA's
data[data == ''] <- NA
data[data == '*'] <- NA

## add language names and predicate labels to data
data$language <- NA
data$predicate_label_en <- NA

for (i in 1:nrow(data)) 
{data$language[i] <- languages$language[which(languages$language_no == data$language_no[i])]}

for (i in 1:nrow(data)) 
{data$predicate_label_en[i] <- predicates$predicate_label_en[which(predicates$predicate_no == data$predicate_no[i])]}

# Chapter 1.4. Create a dataframe with an overview of patterns

patterns <- matrix (data = NA, nrow = no_predicates, ncol = no_languages)
rownames(patterns) <- predicates$predicate_label_en
colnames(patterns) <- languages$language_external

for (i in 1:nrow(data)) {
  patterns[data$predicate_no[i], data$language_no[i]] <- data$valency_pattern[i]
  }

write_xlsx(as.data.frame(patterns), path = "patterns.xlsx") 

## Create a backup copy of the patterns overview for the case something goes wrong

backup_patterns <- patterns

# PART 2. Predictability

# Arriving at predictability scores for each predicate in each language

## Chapter 2.1. Create many subsamples of data 
## where only one language per genus is allowed

# Find the genus with the highest number of languages
no_genera <- length(unique(languages$genus_WALS))
genus_counts <- languages %>% count(genus_WALS, sort = TRUE)
max_lgs_in_genus <- max(genus_counts$n)

# Create random subsamples (total number is the maximum number of languages in the same genus)
genera <- unique(languages$genus_WALS)

subsamples <- as.data.frame(matrix(NA, nrow = no_genera, ncol = max_lgs_in_genus))
colnames(subsamples) <- paste0("sample_", 1:max_lgs_in_genus)

for (i in 1:no_genera) {
  current_genus_name <- genera[i]
  current_genus <- languages %>% filter(genus_WALS == current_genus_name) %>% select(language) %>% pull()
  set.seed(123)
  current_genus_random <- sample(rep(current_genus, length.out = max_lgs_in_genus))
  subsamples[i,] <- current_genus_random
}

subsamples_overview <- languages %>% select(language)
for (i in 1:max_lgs_in_genus) {
  subsamples_overview <- subsamples_overview %>% 
  mutate(!!paste0("sample_", i) := ifelse(language %in% subsamples[,i], 1, 0))
}

write.table(subsamples_overview, file = 'subsamples_overview.txt', row.names = T, col.names = T, sep = '\t')

## Chapter 2.2. Main part on the predictability 

## The code below is based on the subsampling procedure as implemented in 2.1. 

## Preparatory actions to create the relevant lists and dataframes
predictability_by_subsamples_liberal <- vector("list", max_lgs_in_genus)
predictability_by_subsamples_conservative <- vector("list", max_lgs_in_genus)
empty_df <- data.frame(matrix(ncol = no_genera, nrow = no_predicates))
predictability_by_subsamples_liberal <- lapply(predictability_by_subsamples_liberal, function(x) empty_df)
predictability_by_subsamples_conservative <- lapply(predictability_by_subsamples_conservative, function(x) empty_df)

transitivity_ratio_by_subsample <- as.data.frame(matrix(NA, nrow = no_predicates, ncol = max_lgs_in_genus))
colnames(transitivity_ratio_by_subsample) <- paste0("sample_", 1:max_lgs_in_genus)
rownames(transitivity_ratio_by_subsample) <- predicates$predicate_label_en

## The great loop going from one subsample to the next

for (s in 1:max_lgs_in_genus) {
  print(s)
  sample_title <- paste0("sample_",s)
  languages <- languages[which(subsamples_overview[[sample_title]]  == 1),]
  patterns <- patterns[,which(subsamples_overview[[sample_title]] == 1)]
  no_languages <- nrow(languages)

## Create two tables to eventually write obtained values in them
  predicate_lg_predictability_liberal <- matrix (data = 0, nrow = no_predicates, ncol = no_languages)
  rownames(predicate_lg_predictability_liberal) <- predicates$predicate_label_en
  colnames(predicate_lg_predictability_liberal) <- languages$language_external

  predicate_lg_predictability_conservative <- matrix (data = 0, nrow = no_predicates, ncol = no_languages)
  rownames(predicate_lg_predictability_conservative) <- predicates$predicate_label_en
  colnames(predicate_lg_predictability_conservative) <- languages$language_external

## In order to fill the data to the resultant tables, we should run 
## many cycles that end up with data on the predictability of individual 
## languages

  for (l in 1:no_languages) {
  
## Create temporary matrices for identifying predictability of
## predicate-specific patterns in a certain language 
## given all other languages separately
  
    targetlg_liberal <- matrix (data = 0, nrow = no_predicates, ncol = no_languages)
    targetlg_conservative <- matrix (data = 0, nrow = no_predicates, ncol = no_languages)
  
## Create an object (twolanguages) where we explore pairwise
## predictability in one lg (e.g. Abaza)
## based on another lg (e.g. Adyghe)
  
    for (j in 1:no_languages) {
      L1 <- patterns[,j]
      L2 <- patterns[,l]
      twolanguages <- data.frame(matrix(data = 0, nrow = no_predicates, ncol = 7))
      twolanguages[,1] <- L1
      twolanguages[,2] <- L2
      twolanguages[,3] <- as.character(twolanguages[,3])
      twolanguages[,4] <- as.numeric (twolanguages[,4])
      twolanguages[,5] <- as.numeric (twolanguages[,5])
      twolanguages[,6] <- as.numeric (twolanguages[,6])
      twolanguages[,7] <- as.numeric (twolanguages[,7])
    
## put the combined data from the 2 lgs, 
## to the third row, unless there are NAs
      for (i in 1:no_predicates) { 
        if (is.na (twolanguages[i,1]) == "TRUE" || is.na (twolanguages[i,2]) == "TRUE") 
        { twolanguages [i,1:3] <- NA 
        }
        else  
        {twolanguages [i,3] <- paste(twolanguages[i,1], twolanguages [i,2])
        }
      }
    
## put the number of occurrences of the same string in L1 to column 4
      for (i in 1:no_predicates) { 
        if (is.na(twolanguages[i,3]) == "TRUE")
        { twolanguages [i,4] <- NA } 
        else 
        { temp <- na.omit(twolanguages)
        occur <- length(which(temp[,1] == twolanguages[i,1]))
        twolanguages [i,4] <- occur
        }
      }
    
## put the number of occurrences of the same 
## string in the "combined" lg to column 5
      for (i in 1:no_predicates) {
        if (is.na(twolanguages[i,3]) == "TRUE") 
        { twolanguages [i,5] <- NA } 
        else 
        { temp <- na.omit(twolanguages)
        occur <- length(which(temp[,3] == twolanguages[i,3]))
        twolanguages [i,5] <- occur
        }
      }  
    
## calculate the ratio, i.e. predictability of L2 given L1
## and write it to the temporary language-specific table with results
## There are two methods of calculating predictability: a liberal method and a conservative one. 
## The liberal method is where the size of intersection is divided by the size of the relevant 
## class in the predictor-language (L2L1/L1). It gives higher figures for very small classes. 
## The conservative method  is to use ((L2L1-1)/(L1-1)), which makes very small classes unpredictable.  

## Here is the liberal method.
      twolanguages[,6] <- twolanguages[,5]/twolanguages[,4]
## And here is the conservative method. 
      twolanguages[,7] <- ifelse(twolanguages[,4] == 1, 0, (twolanguages[,5]-1)/(twolanguages[,4]-1))
      targetlg_liberal[,j] <- twolanguages[,6]
      targetlg_conservative[,j] <- twolanguages[,7]
    }
  
## Now we can write the language-specific means to the final 
## predictability tables and finish the cycle
  
    for (v in 1:no_predicates)
    {predicate_lg_predictability_liberal[v, l] <- mean(targetlg_liberal[v,], na.rm = TRUE)
    }

    for (v in 1:no_predicates)
    {predicate_lg_predictability_conservative[v, l] <- mean(targetlg_conservative[v,], na.rm = TRUE)
    }
  }

## At this point predicate_lg_predictability_liberal and predicate_lg_predictability_conservative contain 
## predictability values for all individual target languages

  predictability_by_subsamples_liberal[[s]] <- predicate_lg_predictability_liberal
  predictability_by_subsamples_conservative[[s]] <- predicate_lg_predictability_conservative

## Additional section used to store verbs' transitivity ratio in subsamples

  for (v in 1:no_predicates) {
    total_non_na <- sum(!is.na(patterns[v,])) 
    count_tr <- sum(patterns[v,] == "TR", na.rm = TRUE) 
    transitivity_ratio_by_subsample[v,s] <- if (total_non_na > 0) count_tr / total_non_na else NA
  }

## Restore the original data and finish the great loop
  languages <- backup_languages
  patterns <- backup_patterns
}

no_languages <- nrow(languages)
write.table(transitivity_ratio_by_subsample, file = 'transitivity_ratio_by_subsample.txt', row.names = T, col.names = T, sep = '\t')

## Build a dataframe showing basic characteristics of verbs in terms of their transitivity 
## ratio and predictability

verbs_predictability_overview <- data.frame(matrix(ncol = 3, nrow = no_predicates))
rownames(verbs_predictability_overview) <- predicates$predicate_label_en
colnames(verbs_predictability_overview) <- c('TR_ratio','predictability_liberal','predictability_conservative')

temp_sample_means <- numeric(max_lgs_in_genus)

for (v in 1:no_predicates) {
  verbs_predictability_overview$TR_ratio[v] <- mean(as.numeric(transitivity_ratio_by_subsample[v,]), na.rm = TRUE)
  
  for (s in 1:max_lgs_in_genus) {
  temp_verb_predictability_liberal_in_subsample <- predictability_by_subsamples_liberal[[s]]
  temp_mean_verb_predictability_liberal_in_subsample <- mean(temp_verb_predictability_liberal_in_subsample[v,], na.rm = TRUE)
  temp_sample_means[s] <- temp_mean_verb_predictability_liberal_in_subsample
  }
  verbs_predictability_overview$predictability_liberal[v] <- mean(temp_sample_means, na.rm = TRUE)

  for (s in 1:max_lgs_in_genus) {
    temp_verb_predictability_conservative_in_subsample <- predictability_by_subsamples_conservative[[s]]
    temp_mean_verb_predictability_conservative_in_subsample <- mean(temp_verb_predictability_conservative_in_subsample[v,], na.rm = TRUE)
    temp_sample_means[s] <- temp_mean_verb_predictability_conservative_in_subsample
  }
  verbs_predictability_overview$predictability_conservative[v] <- mean(temp_sample_means, na.rm = TRUE)
}

write.table(verbs_predictability_overview, file = 'verbs_predictability_overview.txt', row.names = T, col.names = T, sep = '\t')

## Build a dataframe showing liberal (!) predictability values of specific verbs in individual languages
## by way of calculating these values as the mean of values observed in all subsamples 
## where the language in question is included

predicate_lg_predictability_liberal_final <- data.frame(matrix(ncol = no_languages, nrow = no_predicates))
rownames(predicate_lg_predictability_liberal_final) <- predicates$predicate_label_en
colnames(predicate_lg_predictability_liberal_final) <- languages$language_external

language_predictability_liberal_by_subsamples <- data.frame(matrix(nrow = no_predicates, ncol = max_lgs_in_genus))
colnames(language_predictability_liberal_by_subsamples) <- paste0("sample_", 1:max_lgs_in_genus)

for (l in 1:no_languages) {
  current_language <- languages$language_external[l]
  for (s in 1:max_lgs_in_genus) {
    temp <- as.data.frame(predictability_by_subsamples_liberal[[s]])
    temp_predictability <- if (current_language %in% names(temp)) { temp[[current_language]] } else {as.numeric(rep(NA, no_predicates))}
    language_predictability_liberal_by_subsamples[,s] <- temp_predictability
  }

  for (v in 1:no_predicates) {
    predicate_lg_predictability_liberal_final[v,l] <- mean(as.numeric(language_predictability_liberal_by_subsamples[v,]), na.rm = TRUE)
  }
}

write.table(predicate_lg_predictability_liberal_final, file = 'predicate_lg_predictability_liberal_final.txt', row.names = T, col.names = T, sep = '\t')

## Build a dataframe showing conservative (!) predictability values of specific verbs in individual languages
## by way of calculating these values as the mean of values observed in all subsamples 
## where the language in question is included

predicate_lg_predictability_conservative_final <- data.frame(matrix(ncol = no_languages, nrow = no_predicates))
rownames(predicate_lg_predictability_conservative_final) <- predicates$predicate_label_en
colnames(predicate_lg_predictability_conservative_final) <- languages$language_external

language_predictability_conservative_by_subsamples <- data.frame(matrix(nrow = no_predicates, ncol = max_lgs_in_genus))
colnames(language_predictability_conservative_by_subsamples) <- paste0("sample_", 1:max_lgs_in_genus)

for (l in 1:no_languages) {
  current_language <- languages$language_external[l]
  for (s in 1:max_lgs_in_genus) {
    temp <- as.data.frame(predictability_by_subsamples_conservative[[s]])
    temp_predictability <- if (current_language %in% names(temp)) { temp[[current_language]] } else {as.numeric(rep(NA, no_predicates))}
    language_predictability_conservative_by_subsamples[,s] <- temp_predictability
  }
  
  for (v in 1:no_predicates) {
    predicate_lg_predictability_conservative_final[v,l] <- mean(as.numeric(language_predictability_conservative_by_subsamples[v,]), na.rm = TRUE)
  }
}

write.table(predicate_lg_predictability_conservative_final, file = 'predicate_lg_predictability_conservative_final.txt', row.names = T, col.names = T, sep = '\t')

## Chapter 2.3. Some visualizations related to predictability of predicates
## Create a plot showing the relationships between PredLib, PredCons and transitivity prominence

jpeg("Figure_S1_PredLib_vs_PredCons.jpg", width = 5, height = 5, units = "in", res = 600)
par(mar = c(4, 4, 1, 1))
plot(
  xlim = c(0, 0.85),
  ylim = c(0, 0.85),
  x = verbs_predictability_overview$predictability_liberal,
  y = verbs_predictability_overview$predictability_conservative,
  xlab = "PredLib",
  ylab = "PredCons",
  pch = 1,
  cex = 0.5 + verbs_predictability_overview$TR_ratio/2,
  asp = 1   # ensures equal scaling of x and y axes
)
dev.off()

## Create a plot showing the relationships between PredLib and transitivity prominence
jpeg("Figure_1_TrProminence_vs_PredLib.jpg", width = 5, height = 5, units = "in", res = 600)
par(mar = c(4, 4, 1, 1))
plot(
  x = verbs_predictability_overview$TR_ratio,
  y = verbs_predictability_overview$predictability_liberal,
  xlab = "Transitivity prominence",
  ylab = "PredLib",
  pch = 1
)
dev.off()

## Correlation between Transitivity prominence and PredLib
cor_test <- cor.test(
  verbs_predictability_overview$TR_ratio,
  verbs_predictability_overview$predictability_liberal,
  method = "pearson"
)
cat("Correlation between Transitivity prominence and PredLib:\n")
cat("r =", round(cor_test$estimate, 3), "\n")
cat("95% CI:", round(cor_test$conf.int[1], 3), "–", round(cor_test$conf.int[2], 3), "\n")
cat("p =", format.pval(cor_test$p.value, digits = 3), "\n")

## Create a plot showing the relationships between PredLib and transitivity prominence
## and zooming in on low transitivity verbs with high PredLib
ggsave(
  "Figure_2_Low_TP_High_PredLib.jpg",
  plot = ggplot(verbs_predictability_overview, aes(
    x = TR_ratio,
    y = predictability_liberal
  )) +
    geom_point(aes(size = 1 + predictability_conservative * 4), shape = 19) +
    geom_text_repel(
      aes(label = rownames(verbs_predictability_overview)),
      size = 4,
      max.overlaps = Inf,
      box.padding = 0.4,
      point.padding = 0.2,
      min.segment.length = 0
    ) +
    scale_size_identity() +
    xlim(0, 0.23) +
    ylim(0.45, 0.80) +
    labs(x = "Transitivity prominence", y = "PredLib") +
    theme_bw(base_size = 12) +   # removes grey background
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      text = element_text(size = 12)
    ),
  width = 6.5, height = 4.8, units = "in", dpi = 600
)

## Create a plot showing the relationships between PredLib and transitivity prominence
## and zooming in on low transitivity verbs with high PredLib
ggsave(
  "Figure_3_Low_TP_Low_PredLib.jpg",
  plot = ggplot(verbs_predictability_overview, aes(
    x = TR_ratio,
    y = predictability_liberal
  )) +
    geom_point(aes(size = 1 + predictability_conservative * 4), shape = 19) +
    geom_text_repel(
      aes(label = rownames(verbs_predictability_overview)),
      size = 4,
      max.overlaps = Inf,
      box.padding = 0.4,
      point.padding = 0.2,
      min.segment.length = 0
    ) +
    scale_size_identity() +
    xlim(0, 0.23) +
    ylim(0.3, 0.45) +
    labs(x = "Transitivity prominence", y = "PredLib") +
    theme_bw(base_size = 12) +   # removes grey background
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      text = element_text(size = 12)
    ),
  width = 6.5, height = 4.8, units = "in", dpi = 600
)

## Create a plot showing the relationships between PredLib and transitivity prominence
## and zooming in on mid-range verbs
ggsave(
  "Figure_4_Mid_range_verbs.jpg",
  plot = ggplot(verbs_predictability_overview, aes(
    x = TR_ratio,
    y = predictability_liberal
  )) +
    geom_point(aes(size = 1 + predictability_conservative * 4), shape = 19) +
    geom_text_repel(
      aes(label = rownames(verbs_predictability_overview)),
      size = 4,
      max.overlaps = Inf,
      box.padding = 0.4,
      point.padding = 0.2,
      min.segment.length = 0
    ) +
    scale_size_identity() +
    xlim(0.23, 0.64) +
    ylim(0.38, 0.58) +
    labs(x = "Transitivity prominence", y = "PredLib") +
    theme_bw(base_size = 12) +   # removes grey background
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      text = element_text(size = 12)
    ),
  width = 6.5, height = 4.8, units = "in", dpi = 600
)

## Chapter 2.4. Calculations related to predictability of languages

languages_predictability <- languages[, c("language_no", "language", "language_external")]

for (i in 1:no_languages) {
  current_language <- languages_predictability$language_external[i]
  languages_predictability$PredLib[i] <- mean(predicate_lg_predictability_liberal_final[[current_language]], na.rm = TRUE)
  languages_predictability$PredCons[i] <- mean(predicate_lg_predictability_conservative_final[[current_language]], na.rm = TRUE)  
}

write_xlsx(languages_predictability, path = "languages_predictability.xlsx") 

# PART 3. Entropy as a typological parameter

# Chapter 3.1. Prepare a full spreadsheet with data on languages
# Calculate (corrected) entropy manually (NB: not download from BivalTyp)
 
## Merge the dataframes with basic given infos on lgs and basic BivalTyp stats

languages_everything <- merge(x = languages, y = language_stats, by = "language_no")

# Remove redundant and unnecessary columns
languages_everything <- subset(languages_everything, select = - number_of_nominal_cases)
languages_everything <- subset(languages_everything, select = - normalised_entropy)

# Add family-based colours
family_colours <- data.frame(unique(languages_everything$family_WALS))
colnames(family_colours) <- "family_WALS"
family_colours$colours <- rainbow_hcl(nrow(family_colours))
languages_everything <- merge(x = languages_everything, y = family_colours, by = "family_WALS")
languages_everything <- languages_everything[order(languages_everything$language_no), ]

# Remove languages with many NAs, prepare raw data accordingly
limit <- 90
languages_everything <- languages_everything[-which(languages_everything$overallN < limit), ]
data <- data[data$language %in% languages_everything$language, ]

# Rename columns with raw (downloaded) entropy values
names(languages_everything)[names(languages_everything) == "entropy_nat"] <- "entropy_raw"
names(languages_everything)[names(languages_everything) == "entropy_of_intransitives_nat"] <- "entropy_intr_raw"

# Replace entropy values with corrected values calculated for subsets of fixed size 
# In this part of the code, I also set seed to make the random aspect of my data 
# generating process fully reproducible

temp2 <- vector(length = 100)
set.seed(321)

for (j in 1:nrow(languages_everything)) {
  temp1 <- data$valency_pattern[which(data$language == languages_everything$language[j])]
  temp1 <- temp1[!is.na(temp1)]
  for (i in 1:100) {
    temp2[i] <- entropy(temp1[sample(1:length(temp1), limit, replace = FALSE)])
    }
  languages_everything$entropy_corr[j] <- mean(temp2)
  }

# Replace entropy of intransitives values with corrected values 
# calculated for subsets of fixed (minimal) size 

temp2 <- vector(length = 100)
minintr <- min(languages_everything$intransitives)

set.seed(321)
for (j in 1:nrow(languages_everything)) {
  temp1 <- data$valency_pattern[which(data$language == languages_everything$language[j])]
  temp1 <- temp1[!is.na(temp1)]
  temp1 <- temp1[!temp1 == "TR"]
  for (i in 1:100) {
    temp2[i] <- entropy(temp1[sample(1:length(temp1), minintr, replace = FALSE)])
  }
  languages_everything$entropy_intr_corr[j] <- mean(temp2)
}

# Backup and write the resultant table
backup_languages_everything <- languages_everything

# Chapter 3.2. Addition of predictability of languages, some analyses and visualizations

## Adding PredLib and PredCons values to the summary table on languages
languages_predictability_temp <- languages_predictability[
  languages_predictability$language_no %in% languages_everything$language_no,
]
languages_predictability_temp <- languages_predictability_temp[
  , !names(languages_predictability_temp) %in% c("language", "language_external")
]
languages_everything <- merge(
  languages_everything,
  languages_predictability_temp,
  by = "language_no",
  all.x = TRUE
)

write_xlsx(languages_everything, path = "languages_everything.xlsx") 

## Testing correlations between various parameters related to predictability and informativity
cor_test <- cor.test(
  languages_everything$transitivity_ratio,
  languages_everything$PredCons,
  method = "pearson"
)
cat("Correlation between transitivity_ratio and PredCons:\n")
cat("r =", round(cor_test$estimate, 3), "\n")
cat("95% CI:", round(cor_test$conf.int[1], 3), "–", round(cor_test$conf.int[2], 3), "\n")
cat("p =", format.pval(cor_test$p.value, digits = 3), "\n")

## Create a map showing the distribution of PredCons across the lgs of the world
map_PredCons <- map.feature(languages = languages_everything$language, features = languages_everything$PredCons, longitude = languages_everything$longitude, latitude = languages_everything$latitude, tile = "CartoDB.DarkMatter")
saveWidget(map_PredCons, file="PredCons_world.html")

## Build a visualization on the relationship between TP and PredCons
jpeg('Figure_6_TP_PredCons.jpg', 
     width = 6.5, height = 4.5, units = "in", res = 600, pointsize = 4)
ggscatter(data = languages_everything, 
          x = "transitivity_ratio", 
          y = "PredCons", 
          xlab = "Transitivity prominence", 
          ylab = "PredCons",
          shape = 1)
dev.off()

## Same as above but focusing on the area with mid-range values of TP
languages_everything_temp <- languages_everything[
  languages_everything$transitivity_ratio >= 0.4 & 
    languages_everything$transitivity_ratio <= 0.55, 
]
jpeg('Figure_7_TP_PredCons_selected_languages.jpg', 
     width = 6.5, height = 4.5, units = "in", res = 600, pointsize = 4)
ggscatter(data = languages_everything_temp, 
          x = "transitivity_ratio", 
          y = "PredCons", 
          xlab = "Transitivity prominence", 
          ylab = "PredCons",
          color = "family_WALS") + 
  labs(color = "Family") +
  scale_color_brewer(palette = "Set1") +  # bright and distinct colors
  geom_text_repel(aes(label = language_external), 
                  size = 2.5,     # smaller font for many languages
                  max.overlaps = 50) +  # allow some overlapping labels if necessary
  theme(legend.position = "right",
        legend.justification = "top",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))
dev.off()
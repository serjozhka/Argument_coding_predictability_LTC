# Argument_coding_predictability_LTC
This archive contains supplementary materials for the paper entitled “Cross-linguistic argument-coding predictability and the lexical meaning of the verb” submitted to “Linguistic typology at the crossroads”. This archive is organized into four folders with the files as described below.

This repository is licensed under the Creative Commons Attribution 4.0 International License. You are free to share and adapt the material, provided appropriate credit is given.

# 1_Input_data
1. Several spreadsheets containing data used as input for the analysis

## 1.1. predicates.xlsx
This file contains an overview of predicates used in BivalTyp's questionnaire. It is based on the file predicates.csv in the BivalTyp repository (https://github.com/macleginn/bivaltyp/tree/master/data).

## 1.2 languages.xlsx
This file contains basic information about the 149 languages in the BivalTyp sample. It is the .xlsx version of the “languages.csv” file in the BivalTyp repository (https://github.com/macleginn/bivaltyp/tree/master/data).

## 1.3. languages_preprocessed.xlsx
This file is based on the “languages.xlsx” file above but includes some preprocessing steps as discussed in the code.

## 1.4. data_for_download.xlsx 
This file contains the main body of the BivalTyp data. It is the .xlsx version of the “data_for_download.csv” file in the BivalTyp repository (https://github.com/macleginn/bivaltyp/tree/master/data).

## 1.5. data_for_download_preprocessed.xlsx
This file is based on the “data_for_download.xlsx” file above but includes some preprocessing steps as discussed in the code.

## 1.6. language_stats.xlsx
This file contains an overview of BivalTyp-internal properties of the languages in the sample. It is the .xlsx version of the "language_stats.cs" file in the BilvalTyp repository (https://github.com/macleginn/bivaltyp/tree/master/data).

# 2_Code
Code_valency_predictability_LTaC.R
This is the R code used for data analysis.

# 3_Output_data
This folder containg three summary spreadsheets with the main empirical results of the study (a fourth summary spreadsheets is provided in the appendix of the paper).

## 3.1. patterns.xls
This spreadsheet contains on overview of valency patterns for each language + predicate combination.

## 3.2. verbs_predictability_overview.xlsx
This spreadsheet contains mean PredLib and PredCons values as well as the transitivity prominence values for each predicate (the values are the means of the means observed across subsamples). This files serves as the basis for Table 1A in the Appendix.

## 3.3. predicate_lg_predictability_liberal_final.xlsx
This spreadsheet contain PredLib2 values for each predicate-language combination.

## 3.4. predicate_lg_predictability_conservative_final.xlsx
This spreadsheet contain PredCons2 values for each predicate-language combination.

## 3.5. languages_everything.xlsx
This spreadsheet contains a summary of all aggregate quantitative parameters for 145 languages from the sample, including mean PredLib and PredCons values.

# 4_Additional_figures

## 4.1 Figure_S1_PredLib_vs_PredCons.jpg
This figure shows a scatterplot illustrating the relationship between PredLib and PredCons.

4.2. PredCons_world.html
This is a Leaflet-based interactive map displaying the distribution of PredCons values across the languages in the sample. It serves as the basis for Figure 5 in the paper.

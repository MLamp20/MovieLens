# Movielens
Movie Recommendation System MovieLens 10M dataset

Movie Recommendation Algorithm File Listing (not all uploaded to GitHub)

Project uses MovieLens 10M file


edxsetup.R
Creates the edx set and validation set(final hold-out test set)
/

eda.R

Exploratory data analysis on edx before split into train_edx/test_edx subsets


edxsplittraintest.R

Splits the edx data into train and test sets


vidmodel.R

Sets up baseline model (mean) as well as movie user model(not regularized)


movieusergenremodel.R

Model with movie user and genre bias


movieusergenreregmodel.R

Model with movie user and genre bias regularized


postregchart.R

Plots to evaluate impact on regularization on train_edx


movieusergenreregmodelfulledx.R

Final Movie Recommendation Model Proposal


recommendationsystemcompiledscript.R

Complete script above - submission for grading


MovieLens.rmd

R Markdown document  - submission for grading


MovieLens.pdf

PDF document - submission for grading

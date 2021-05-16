### LATEST SCORE
Area under the curve: 0.9601
Area under the curve: 0.963
Area under the curve: 0.963
Area under the curve: 0.9635
Area under the curve: 0.9626

### https://piazza.com/class/kjvsp15j2g07ac?cid=375

### Project 3 Assigned

Rubric: There are 12 total points for this project.

### 6pt for model performance (measured by the minimal value of AUC over the five test datasets)

2pt: worse than 0.90
4pt: [0.90, 0.94)
5pt: [0.94, 0.96)
6pt: 0.96 or better

### 2pt for vocabulary size: A model based on a smaller vocabulary size is easier to interpret.

0.5pt: vocabulary size in (2,000 3,000]
1pt: vocabulary size in (1,000, 2,000]
2pt: vocabulary size <= 1,000
Submit your customized vocabulary as a txt file as well as a Markdown file (HTML) explainhing how you construct this customized vocabulary.

4pt for report. Your report should contain at least the following sections
Overview of your model: background, problem statement, objective, input and output of your model, ect.
It is up to you whether you want to briefly describe the construction of your customized vocabulary.
Technical details: type of algorithms/models used/explored, pre-processing, training process, tuning parameters, etc.
Model validation: performance on the five test datasets, discussion on model limitations, explanation on errors, any interesting findings and future steps you could take to improve your model. Check some sample reports from prior semesters.
Add a paragraph or a subsection discussing interpretability of your algorithm. If given a review, can you explain why your algorithm assigns this particular score (i.e., probability of being positive here) to this review? Interpretation is easy for simple models like logistic regression or LDA or NaiveBayes. For XGBoost, check an application of the package Lime.

It is okay to conclude that you find it difficult to make your algorithm explainable. Do some search online and include relevant discussion/resources, e.g., potential approaches that can help you to unfold your black-box algorithm, in your report.
Report the following in your report: performance and running time for the five datasets, and the computer system you use (e.g., Macbook Pro, 2.53 GHz, 4GB memory, or AWS t2.large). No need to report the running time for vocab construction.

### Sample Reports: TBA


Project 3 Material
Project Description: [https://liangfgithub.github.io/S21/Project3_S21.nb.html]
Two data sets on Resources Page: [splits_S21.csv] [alldata.tsv]
Kaggle Page "Bag of Words Meets Bags of Popcorn"
https://www.kaggle.com/c/word2vec-nlp-tutorial
[text2vec] R vignettes:
https://cran.r-project.org/web/packages/text2vec/vignettes/text-vectorization.html
A Quick R Demonstration on Sentiment Analysis and Textmining
https://drive.google.com/file/d/0B_sqyEYBKc1wVm4xN0NvQlJlNWc/view
An incomplete list of relevant links
https://www.kaggle.com/c/word2vec-nlp-tutorial/discussion/11261
https://www.kaggle.com/abhishek/approaching-almost-any-nlp-problem-on-kaggle/
https://www.kaggle.com/c/word2vec-nlp-tutorial/discussion/14966
https://github.com/wush978/FeatureHashing/blob/master/vignettes/SentimentAnalysis.Rmd


#### R packages:

dplyr, tidyr, reshape2, tidyverse, tidytext,
glmnet, randomForest, XGBoost,
pROC,
text2vec, tm, stopwords
slam,
SnowballC
kernlab, e1071 (be careful with SVM. Note that SVM computation scales with sample size n, not dimension. So a vanilla SVM implementation with 25K samples would be very time-consuming.)
naivebayes


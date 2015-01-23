#+ setup_chunk, echo=FALSE, results='hide'
setwd('E:/GitHub/PracticalMachineLearning-Project/')
opts_knit$set(upload.fun = image_uri)
opts_knit$set(width = 142L)
opts_chunk$set(tidy = FALSE)
opts_chunk$set(fig.width=12, fig.height=12)
opts_chunk$set(error=FALSE)

#' ## Course Project for [Practical Machine Learning](https://www.coursera.org/course/predmachlearn)
#'   * by Shea Parkes
#'
#' ### Objective
#' 
#' We are provided with personal activity data collected from study participants while they performed a series of actions.
#' The action they performed is captured in the `classe` variable.  We have been requested to build a predictive model that
#' uses the personal activity data to predict the performed action.  We will inspect the provided training data, train a useful
#' model, and then predict the outcomes of the testing data.  We will also do some cross-validation to provide an anticipated
#' accuracy for the testing data.

library(magrittr)
library(randomForestSRC)
set.seed(42)
#' ----

#' ### Read in the data into R nicely
df.all <- lapply(
  c('training', 'testing')
  ,. %>%{
    i.df <- read.csv(
      paste0('pml-',.,'.csv')
      ,na.strings = c('NA','#DIV/0!','')
      ,stringsAsFactors = FALSE
      )
    i.df$X <- NULL
    if(. == 'testing'){
      i.problems <- i.df$problem_id
      i.df$problem_id <- NULL
      i.df$classe <- as.character(NA)
      i.df$train <- FALSE
      i.df$problem_id <- i.problems
    } else {
      i.df$train <- TRUE
      i.df$problem_id <- as.integer(NA)
    }
    i.df
  }) %>%
  {do.call(rbind, .)} %>%
  {rownames(.) <- NULL; .}

df.all %>% str(list.len=length(.))

#' Convert character vectors to factors after stacking so levels will be consistent
df.all %<>% sapply(is.character) %>%
  {
    df.all[.] <- lapply(df.all[.], as.factor)
    print(str(df.all[.]))
    df.all
  }

#' Move the non-feature data out into isolated vectors
classe <- df.all$classe
df.all$classe <- NULL

problem_id <- df.all$problem_id
df.all$problem_id <- NULL

ind.train <- df.all$train
df.all$train <- NULL


#' ### Inspect the blatantly obvious relationships
#'   * Focus on time and user because they were likely asked to do the different actions in a certain sequence
#'   * The `NA` values below for `classe` represent the testing data
table(df.all$cvtd_timestamp, df.all$user_name)
table(df.all$cvtd_timestamp, classe, useNA = 'ifany')
table(df.all$user_name, classe, useNA = 'ifany')


#' ### Isolate and remove the almost-never-populated features
#'   * There are a chunk of features that are so-seldom populated as to provide no information
#'   * Neither are they populated on the testing data, so they are extra worthless
pct.na <- df.all %>%
  sapply(. %>% is.na() %>% mean()) %T>%
  {sort(.) %>% dotchart()} %T>%
  {sort(.) %>% tail(., 20) %>% print()}

df.all %<>% '['(pct.na < 0.8)

#' ### Do a simple RandomForest fit to predict `classe`
#'   * Use a random forest because:
#'     * Results are not very sensitive to the choice of hyperparameters
#'     * It does natural feature selection
#'     * It produces highly accurate models with little intervention
#'     * It can naturally handle multi-class responses
#'     * It is not prone to overfitting as long as the training data is representative of the testing data
#'       * Per the inspection tables above, the testing data is most likely just a simple random sample of the training data
myfit <- rfsrc(
  classe~.
  ,data=data.frame(classe,df.all)[ind.train,]
  ,ntree=500 ## This is an easy enough problem there is no need to use a lot of trees
  ,nsplit=12 ## No need to exhaustively search for splits with this much data.  This will speed up training and actually improve accuracy.
  )
print(myfit)
plot(myfit)

#' ### Make (and save) the predictions
mypreds <- predict(myfit,newdata=df.all[!ind.train,])$class %T>% print()
dummy.capture <- lapply(
  1:length(mypreds)
  ,. %>% {cat(as.character(mypreds[.]), file=paste0('mypred_',.,'.txt'))}
  )

#' ### Do a quick cross-validation to estimate prediction error
#'   * Given that the testing data looks like a random draw from the training data,
#'     this is redundant with the built-in OOB error, but I'm doing it because it was requested
#'   * Doing this manually because it's easy enough and I don't like the overhead of something like `caret()`
n.fold <- 5L
ind.fold <- rep_len(1:n.fold, length.out=sum(ind.train)) %>%
  sample() %T>%
  {table(.) %>% print()}

cv.results.sloppy <- lapply(
  1:n.fold
  ,function(i.fold){
    fit.fold <- rfsrc(
      classe~.
      ,data=data.frame(classe,df.all)[ind.train,][ind.fold != i.fold,]
      ,ntree=500
      ,nsplit=12
      )
    pred.fold <- rep(as.character(NA),sum(ind.train))
    pred.fold[ind.fold == i.fold] <- predict(
      fit.fold
      ,newdata=df.all[ind.train,][ind.fold == i.fold,]
      ) %$%
      class %>%
      as.character()
    return(pred.fold)
  })
cv.results <- cv.results.sloppy %>%
  {do.call(cbind,.)} %>%
  apply(1, . %>% {.[!is.na(.)]}) %T>%
  {table(., useNA='always') %>% print()}

#' Cross-validated confusion matrix
table(classe[ind.train],cv.results)

#' Cross-validated accuracy
mean(classe[ind.train] %>% as.character() == cv.results)

#' ## Conclusions
#'   * The Cross Validated and OOB accuracy estimates both suggest an accuracy of ~99%
#'   * The testing data looks to be from the same sample as the training data, so I tend to believe these estimates
#'   * Although an accuract model, it's not that insightful since the user and time are only artificially correlated with the outcomes
#'     due to the design of the experiment
#'     
#' ----
#' ### Session information
sessionInfo()

#' ### Bibliography
#+ bibliography, echo=FALSE, results='asis'
print(citation(),style='html')
print(citation('magrittr'),style='html')
print(citation('randomForestSRC'),style='html')

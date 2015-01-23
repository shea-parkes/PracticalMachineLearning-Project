#+ setup_chunk, echo=FALSE, results='hide'
setwd('E:/GitHub/PracticalMachineLearning-Project/')
opts_knit$set(upload.fun = image_uri)
opts_knit$set(width = 142L)
opts_chunk$set(tidy = FALSE)
opts_chunk$set(fig.width=12, fig.height=12)
opts_chunk$set(error=FALSE)

#' ## Course Project for [Practical Machine Learning](https://www.coursera.org/course/predmachlearn)
#'   * by Shea Parkes

library(magrittr)
library(dplyr)
library(randomForestSRC)
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

#' Convert to factor after stacking so levels will be consistent
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
table(df.all$user_name, df.all$cvtd_timestamp, ind.train)
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

#' ### Do a simple RandomForest fit to understand modeling space
myfit <- rfsrc(
  classe~.
  ,data=data.frame(classe,df.all)[ind.train,]
  ,ntree=50 ## Keep the number of trees small here
  ,nsplit=12 ## No need to exhaustively search for splits with this much data
  )
print(myfit)
plot(myfit)
predict(myfit,newdata=df.all[!ind.train,])$class


#' ### Session information
sessionInfo()

#' ### Bibliography
#+ bibliography, echo=FALSE, results='asis'
print(citation(),style='html')
print(citation('magrittr'),style='html')
print(citation('randomForestSRC'),style='html')

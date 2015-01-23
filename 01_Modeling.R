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
#' ----


#' ### Session information
sessionInfo()

#' ### Bibliography
#+ bibliography, echo=FALSE, results='asis'
print(citation(),style='html')
print(citation('randomForest'),style='html')

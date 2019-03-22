source('Code/00_00_Preamble.R', echo=TRUE)
# Testing h2o 
library(h2o)
# Start H2O on your local machine using all available cores
# (By default, CRAN policies limit use to only 2 cores)
h2o.init(nthreads = -1)
#Get help
?h2o.glm
?h2o.gbm

# Show a demo
demo(h2o.glm)
demo(h2o.gbm)

h2o.shutdown(prompt = TRUE)

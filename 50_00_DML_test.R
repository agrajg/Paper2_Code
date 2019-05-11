# NOTES : 
cat('* Begin Running file : ', '50_00_DML_test.R ', " ...", '\n')
cat('* --------------------------------------------------', '\n')
cat('* R Setup ... ', '\n')
source(file = '50_00_R_Setup.R')

cat('* Feeding Inputs ... ', '\n')
{
  cat('* H2O port ... ', '\n')
  {  
    n.threads = 66
    port.num = 11111
    max.mem = '550G'
    force.DL = TRUE
  }
  
  {
    max_text_features = 10000
  }
}

cat('* Setting up data ...', '\n')
source(file = "50_00_Setting_up_data.R")
h2o.ls()

cat('* Feeding Inputs ... ','\n')
{
  cat('* Setting R seed ', '\n')
  {
    Seed_R = 9238928
    cat('* Seed = ', Seed_R, '\n')
    set.seed(Seed_R)
  }
  
  cat('* For DML algorithm ...', '\n')     
  {
    times.split.dml = 1                           # *** Change this in the final program
    crossfit.K = 2                                # *** Change this in the final program
  }
  
  cat('* Model Selection hyper parameters ... ', '\n')
  {
    # need alpha and lambda
    hparams_GLM = list(alpha = c(0.01, 0.1, 0.5, 0.9, 1), lambda = c(0.0001, 0.001, 0.01, 0.1, 1, 10))   # *** Change this in the final program
    # need number of trees, max depth - how many variables to break on. 
    # https://medium.com/all-things-ai/in-depth-parameter-tuning-for-random-forest-d67bb7e920d
    hparams_DRF = list(ntrees = c(50, 100, 500), 
                       max_depth = c(20, 40 , 80), 
                       min_rows = c(100, 1000, 2000),
                       nbins = c(20, 40, 80))    
    hparams_GBM = list(learn_rate = c(0.1, 0.3, 0.7),
                       ntrees = c(50, 100, 500), 
                       max_depth = c(20, 40 , 80), 
                       min_rows = c(100, 1000, 2000),
                       nbins = c(20, 40, 80))    
    # https://htmlpreview.github.io/?https://github.com/ledell/sldm4-h2o/blob/master/sldm4-deeplearning-h2o.html
    hparams_DL = list(hidden = list(c(10,20,10), c(10, 20, 10), c(50,20)), 
                      activation = c("Rectifier", "Maxout", "Tanh"), 
                      l1 = c(0, 0.00001, 0.0001, 0.001, 0.01), 
                      l2 = c(0, 0.00001, 0.0001, 0.001, 0.01))
  }
  
  cat('* For ML algorithms ...', '\n')
  {
    method_list = c("GLM", "DRF", "GBM", "DL")
    # validation.split.frac = (4/5)                     # ***Either do validation or cross validation
    cv.num.folds = 5                                    # *** Change this in the final program
    search_criteria <- list(strategy = "RandomDiscrete", max_models = 2, stopping_tolerance = 0.0001)
  }

}

cat('* MAIN PROGRAM ... ', '\n')
{
  cat('* Define predictors for all outcomes ... ')
  {
    predictor_list = X                # Change this in final program
    cat('* Define target list ... ', '\n')
    target_list = c(Y[1], D[1], Z[1:2])       # Change this in final program
  }
  
  cat('* Model Selection Proocess and DML ... ', '\n')
  for(i in 1:times.split.dml){
    #  i=1
    cat('* Running Grid search and DML process - ', i , ' - time.','\n')
    {
      cat('* Model selection ...', '\n')
      source(file = '50_00_Model selection.R')
      
      cat('* Actual DML ...', '\n')
      # source(file = '50_00_Actual_DML.R')
    }
  }
}


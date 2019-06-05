cat('* Define main DML imputs ... ', '\n')
{
  cat('- Seed for DML Folds = ', Seed_R, '\n')
  cat('- Number of times to perform DML = ', times.split.dml, '\n')
  cat('- Number of folds for CV and crossfitting = ', K.folds, '\n')
}

cat('* Define ML models hyper parameters and parmeters ... ', '\n')
{
  cat('* Define GLM parameters', '\n')
  {
    cat('- hparams_GLM = ', '\n')
    print(hparams_GLM)
    cat('- standardize_param_GLM = ', '\n')
    print(standardize_param_GLM)
    cat('- lambda_search_param_GLM = ', '\n')
    print(lambda_search_param_GLM)
    cat('- interaction_pairs_param_GLM = ', '\n')
    print(interaction_pairs_param_GLM)
    cat('- interactions_param_GLM = ', '\n')
    print(interactions_param_GLM)
  }
  
  cat('* Define DRF parameters', '\n')
  {
    # need number of trees, max depth - how many variables to break on. 
    # https://medium.com/all-things-ai/in-depth-parameter-tuning-for-random-forest-d67bb7e920d
    cat('- hparams_DRF = ', '\n')
    print(hparams_DRF)
    cat('- stopping_rounds_param_DRF = ', '\n')
    print(stopping_rounds_param_DRF)
    cat('- stopping_metric_param_DRF = ', '\n')
    print(stopping_metric_param_DRF)
    cat('- stopping_tolerance_param_DRF = ', '\n')
    print(stopping_tolerance_param_DRF)
    cat('- min_split_improvement_param_DRF = ', '\n')
    print(min_split_improvement_param_DRF)
  }
  
  cat('* Define GBM parameters', '\n')
  {
    cat('- hparams_GBM = ', '\n')
    print(hparams_GBM)
    cat('- learn_rate_annealing_param_GBM = ', '\n')
    print(learn_rate_annealing_param_GBM)
    cat('- stopping_rounds_param_GBM = ', '\n')
    print(stopping_rounds_param_GBM)
    cat('- stopping_metric_param_GBM = ', '\n')
    print(stopping_metric_param_GBM)
    cat('- stopping_tolerance_param_GBM = ', '\n')
    print(stopping_tolerance_param_GBM)
    cat('- min_split_improvement_param_GBM = ', '\n')
    print(min_split_improvement_param_GBM)
  }
  
  cat('* Define DL parameters', '\n')
  {
    cat('- hparams_DL = ', '\n')
    print(hparams_DL)
    cat('- epochs_param_DL = ', '\n')
    print(epochs_param_DL)
    cat('- distribution_param_DL = ', '\n')
    print(distribution_param_DL)
    cat('- score_interval_param_DL = ', '\n')
    print(score_interval_param_DL)
    cat('- score_training_samples_param_DL = ', '\n')
    print(score_training_samples_param_DL)
    cat('- score_validation_samples_param_DL = ', '\n')
    print(score_validation_samples_param_DL)
    cat('- score_duty_cycle_param_DL = ', '\n')
    print(score_duty_cycle_param_DL)
    cat('- classification_stop_param_DL = ', '\n')
    print(classification_stop_param_DL)
    cat('- regression_stop_param_DL = ', '\n')
    print(regression_stop_param_DL)
    cat('- stopping_rounds_param_DL = ', '\n')
    print(stopping_rounds_param_DL)
    cat('- stopping_metric_param_DL = ', '\n')
    print(stopping_metric_param_DL)
    cat('- stopping_tolerance_param_DL = ', '\n')
    print(stopping_tolerance_param_DL)
    cat('- max_runtime_secs_param_DL = ', '\n')
    print(max_runtime_secs_param_DL)
    cat('- sparse_param_DL = ', '\n')
    print(sparse_param_DL)
  }
}
cat('* For other ML algorithms ...', '\n')
{
  cat('- method_list = ', '\n')
  print(method_list)
  cat('- search_criteria = ', '\n')
  print(search_criteria)
}
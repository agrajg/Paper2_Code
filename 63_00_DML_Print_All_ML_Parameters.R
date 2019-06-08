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
}
cat('* For other ML algorithms ...', '\n')
{
  cat('- method_list = ', '\n')
  print(method_list)
  cat('- search_criteria = ', '\n')
  print(search_criteria)
}
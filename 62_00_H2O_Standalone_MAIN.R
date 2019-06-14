cat('* Clear all work space ... ', '\n')
rm(list = ls())

cat('* Specify and begin h2o on stand alone machine ... ', '\n')
{  
  n.threads = -1
  port.num = 11113
  max.mem = '4096G'
  force.DL = FALSE
  library(h2o)
  h2o.init(port = port.num, nthreads = n.threads, max_mem_size = max.mem, forceDL = force.DL)
  cat('* clean H2O slate - just in case the cluster was already running', '\n')
  h2o.removeAll() 
  h2o.ls()
}

cat('* Call the DML main program ... ', '\n')
{
  source(file = '62_00_DML_Main.R')
}

cat('* H2O shutdown ... ','\n')
h2o.shutdown(prompt = FALSE)

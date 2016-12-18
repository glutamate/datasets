import Numeric.Datasets (getDataset)
import Numeric.Datasets.Iris (iris)
import Numeric.Datasets.Abalone (abalone)
import Numeric.Datasets.Coal (coal)

main = do

  -- The Iris data set is embedded
  print (length iris)
  print (head iris)

  -- The Abalone dataset is fetched
  abas <- getDataset abalone
  print (length abas)
  print (head abas)

  -- The Coal dataset is fetched
  cs <- getDataset coal
  print (length cs)
  print (head cs)


# Load German credit data and create subset
data(germancredit)
df <- sdata

# Bin all variables of the data frame (apart from the target variable)
# with default parameter settings
binning <- woe.binning(df, 'T_TARGET', 'Q_EXT_SOURCE_3', min.perc.total = 0.01)

# Plot the binned variables
woe.binning.plot(binning)

# Tabulate the binned variables
tabulate.binning <- woe.binning.table(binning)
tabulate.binning

# Deploy the binning solution to the data frame
# (i.e. add binned variables and corresponding WOE variables)
df.with.binned.vars.added <- woe.binning.deploy(df, binning,
                                                add.woe.or.dum.var='woe')		


library(doParallel)
source('_utils.R')

nCores  <- detectCores() - 1
cl      <- makeCluster(nCores)
registerDoParallel(cl)

qnames  <- colnames(sdata_q)[colnames(sdata_q) != "T_TARGET"]
qnames  <- qnames[1:7]
results <- list()

#result <- doSmBinning('Q_EXT_SOURCE_3',sdata_q, 0.05)

# single-node
start.time <- Sys.time()
  results_s <- foreach(i = 1:length(qnames)) %do% doSmBinning(qnames[[i]], sdata_q, pVal)
end.time <- Sys.time()
print(paste0('Single node: ',end.time - start.time))

# multi-node
start.time <- Sys.time()
  results_m <- foreach(i = 1:length(qnames), .packages = c('smbinning')) %dopar% doSmBinning(qnames[[i]], sdata_q, pVal)
end.time <- Sys.time()
print(paste0('Multi node: ',end.time - start.time))

stopCluster(cl)

ivContainer <- list()
for(res in results_m){
  if(res$iv >= IVtresh){
    ivContainer[[colnames(sdata_q)[res$col_id]]]$IV <- res$iv
    ivContainer[[colnames(sdata_q)[res$col_id]]]$cuts <- res$cuts
  }
}





 



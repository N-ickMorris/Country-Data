# -----------------------------------------------------------------------------------
# ---- Input Information ------------------------------------------------------------
# -----------------------------------------------------------------------------------

# set the path of where the input files are
mywd = "C:/Users/Nick Morris/Downloads/Data-Science/Gender-Data/Human-Development"

# -----------------------------------------------------------------------------------
# ---- Packages ---------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# data handling
require(data.table)
require(gtools)

# plotting
require(ggplot2)
require(scales)
require(GGally)

}

# -----------------------------------------------------------------------------------
# ---- Functions --------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# these are functions i like to use

# ---- prints the data types of each column in a data frame -------------------------

types = function(dat)
{
  # make dat into a data.frame
  dat = data.frame(dat)
  
  # get the column names
  column = colnames(dat)
  
  # get the class of the columns
  data.type = sapply(1:ncol(dat), function(i) class(dat[,i]))
  
  # compute the number of levels for each column
  levels = sapply(1:ncol(dat), function(i) ifelse(data.type[i] == "factor", length(levels(droplevels(dat[,i]))), 0))
  
  return(data.frame(column, data.type, levels))
}

# ---- converts all columns to a character data type --------------------------------

tochar = function(dat)
{
  # make dat into a data.frame
  dat = data.frame(dat)
  
  # get the column names
  column = colnames(dat)
  
  # get the values in the columns and convert them to character data types
  values = lapply(1:ncol(dat), function(i) as.character(dat[,i]))
  
  # combine the values back into a data.frame
  dat = data.frame(do.call("cbind", values), stringsAsFactors = FALSE)
  
  # give dat its column names
  colnames(dat) = column
  
  return(dat)
}

}

# -----------------------------------------------------------------------------------
# ---- Prepare Data -----------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# set the work directory
setwd(mywd)

# read in the data
pop = data.table(tochar(read.csv("Population-Millions.csv", stringsAsFactors = FALSE)))
gni.female = data.table(tochar(read.csv("Female-GNI.csv", stringsAsFactors = FALSE)))
gni.male = data.table(tochar(read.csv("Male-GNI.csv", stringsAsFactors = FALSE)))
schooling.female = data.table(tochar(read.csv("Female-Years-of-Schooling.csv", stringsAsFactors = FALSE)))
schooling.male = data.table(tochar(read.csv("Male-Years-of-Schooling.csv", stringsAsFactors = FALSE)))
unemp.ratio = data.table(tochar(read.csv("Female-to-Male-Unemployment-Ratio.csv", stringsAsFactors = FALSE)))
gii = data.table(tochar(read.csv("Gender-Inequality-Index.csv", stringsAsFactors = FALSE)))
life.expectancy = data.table(tochar(read.csv("Life-Expectancy-Years.csv", stringsAsFactors = FALSE)))
hdi = data.table(tochar(read.csv("Human-Development-Index.csv", stringsAsFactors = FALSE)))
births = data.table(tochar(read.csv("Adolescent-Births-Per-1000-Women-Ages-15-to-19.csv", stringsAsFactors = FALSE)))

# lets build a master table of all these tables
# get the countries from pop becuase it has the most countries
countries = unique(pop$Country)

# get the years
years = 1990:2015

# build the first two columns of our table
dat = data.table(expand.grid(Country = countries, Year = as.numeric(years)))
dat[, Country := as.character(Country)]

# lets put all of our tables into long format
pop = melt(pop, id.vars = "Country", variable.name = "Year", value.name = "Population")
gni.female = melt(gni.female, id.vars = "Country", variable.name = "Year", value.name = "Female.GNI")
gni.male = melt(gni.male, id.vars = "Country", variable.name = "Year", value.name = "Male.GNI")
schooling.female = melt(schooling.female, id.vars = "Country", variable.name = "Year", value.name = "Female.Schooling")
schooling.male = melt(schooling.male, id.vars = "Country", variable.name = "Year", value.name = "Male.Schooling")
unemp.ratio = melt(unemp.ratio, id.vars = "Country", variable.name = "Year", value.name = "Unemployement.Ratio")
gii = melt(gii, id.vars = "Country", variable.name = "Year", value.name = "Gender.Inequality.Index")
life.expectancy = melt(life.expectancy, id.vars = "Country", variable.name = "Year", value.name = "Life.Expectancy")
hdi = melt(hdi, id.vars = "Country", variable.name = "Year", value.name = "Human.Development.Index")
births = melt(births, id.vars = "Country", variable.name = "Year", value.name = "Adolescent.Births")

# remove all letters from the Year and value columns in each table
pop[, Year := as.numeric(gsub("[A-z]", "", Year))]
pop[, Population := as.numeric(gsub("[A-z]", "", Population))]

gni.female[, Year := as.numeric(gsub("[A-z]", "", Year))]
gni.female[, Female.GNI := as.numeric(gsub("[A-z]", "", Female.GNI))]

gni.male[, Year := as.numeric(gsub("[A-z]", "", Year))]
gni.male[, Male.GNI := as.numeric(gsub("[A-z]", "", Male.GNI))]

schooling.female[, Year := as.numeric(gsub("[A-z]", "", Year))]
schooling.female[, Female.Schooling := as.numeric(gsub("[A-z]", "", Female.Schooling))]

schooling.male[, Year := as.numeric(gsub("[A-z]", "", Year))]
schooling.male[, Male.Schooling := as.numeric(gsub("[A-z]", "", Male.Schooling))]

unemp.ratio[, Year := as.numeric(gsub("[A-z]", "", Year))]
unemp.ratio[, Unemployement.Ratio := as.numeric(gsub("[A-z]", "", Unemployement.Ratio))]

gii[, Year := as.numeric(gsub("[A-z]", "", Year))]
gii[, Gender.Inequality.Index := as.numeric(gsub("[A-z]", "", Gender.Inequality.Index))]

life.expectancy[, Year := as.numeric(gsub("[A-z]", "", Year))]
life.expectancy[, Life.Expectancy := as.numeric(gsub("[A-z]", "", Life.Expectancy))]

hdi[, Year := as.numeric(gsub("[A-z]", "", Year))]
hdi[, Human.Development.Index := as.numeric(gsub("[A-z]", "", Human.Development.Index))]

births[, Year := as.numeric(gsub("[A-z]", "", Year))]
births[, Adolescent.Births := as.numeric(gsub("[A-z]", "", Adolescent.Births))]

# lets join all of our tables into dat by Country and Year
setkey(dat, Country, Year)
setkey(pop, Country, Year)
dat = pop[dat]

setkey(dat, Country, Year)
setkey(gni.female, Country, Year)
dat = gni.female[dat]

setkey(dat, Country, Year)
setkey(gni.male, Country, Year)
dat = gni.male[dat]

setkey(dat, Country, Year)
setkey(schooling.female, Country, Year)
dat = schooling.female[dat]

setkey(dat, Country, Year)
setkey(schooling.male, Country, Year)
dat = schooling.male[dat]

setkey(dat, Country, Year)
setkey(unemp.ratio, Country, Year)
dat = unemp.ratio[dat]

setkey(dat, Country, Year)
setkey(gii, Country, Year)
dat = gii[dat]

setkey(dat, Country, Year)
setkey(life.expectancy, Country, Year)
dat = life.expectancy[dat]

setkey(dat, Country, Year)
setkey(hdi, Country, Year)
dat = hdi[dat]

setkey(dat, Country, Year)
setkey(births, Country, Year)
dat = births[dat]

# remove objects we no longer need
rm(countries, years, pop, gni.female, gni.male, schooling.male, 
   schooling.female, unemp.ratio, mywd, gii, life.expectancy, hdi, births)

# free memory
gc()

}

# -----------------------------------------------------------------------------------
# ---- Plot Data --------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# plot the pairwise relationships in our data set
pairs.plot = ggpairs(na.omit(dat[,.(Female.Schooling, Adolescent.Births, Gender.Inequality.Index, Female.GNI)]), 
                     title = "Pairwise Relationships",
#                     mapping = aes(fill = Year, color = Year),
                     axisLabels = "internal",
                     upper = list(continuous = wrap("density", alpha = 1), combo = "box"),
                     lower = list(continuous = wrap("points", alpha = 1/3), combo = wrap("dot")),
                     diag = list(continuous = wrap("diagAxis"))) +
  theme_bw(30) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

pairs.plot

# 
dat.change = data.table(dat[, .(Year = Year, 
                                Population.Change = Population - shift(Population),
                                Female.Schooling.Change = Female.Schooling - shift(Female.Schooling)), 
                            by = Country])

# plot the pairwise relationships in our data set
pairs.plot = ggpairs(na.omit(dat.change[,.(Female.Schooling.Change, Population.Change)]), 
                     title = "Pairwise Relationships",
#                     mapping = aes(fill = Year, color = Year),
                     axisLabels = "internal",
                     upper = list(continuous = wrap("density", alpha = 1), combo = "box"),
                     lower = list(continuous = wrap("points"), combo = wrap("dot")),
                     diag = list(continuous = wrap("diagAxis"))) +
  theme_bw(30) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

pairs.plot

}




















# -----------------------------------------------------------------------------------
# ---- Export Results ---------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# export gbm.cv
write.csv(gbm.cv, "gbm-cross-validation-performance.csv", row.names = FALSE)

# summarize performance metrics for every model in doe
stat = c("Min", "Q1", "Median", "Mean", "Q3", "Max")

# get the key columns from mod.id
keys = names(mod.id[,!"mod"])

# summarize each of the performance metrics
# decide which type of model to run
if(Y.type == "Binary")
{
  # compute diagnostic errors
  gbm.diag = gbm.cv[,.(stat = factor(stat, levels = stat),
                       Accuracy = as.vector(summary(na.omit(Accuracy))), 
                       Sensitivity = as.vector(summary(na.omit(Sensitivity))),
                       Specificity = as.vector(summary(na.omit(Specificity))),
                       AUC = as.vector(summary(na.omit(AUC))),
                       Odds.Ratio = as.vector(summary(na.omit(Odds.Ratio))),
                       Log.Loss = as.vector(summary(na.omit(Log.Loss))),
                       Cutoff = as.vector(summary(na.omit(Cutoff)))),
                    by = keys]
  
} else if(Y.type == "Numeric")
{
  # compute diagnostic errors
  gbm.diag = gbm.cv[,.(stat = factor(stat, levels = stat),
                       ME = as.vector(summary(na.omit(ME))), 
                       RMSE = as.vector(summary(na.omit(RMSE))),
                       MAPE = as.vector(summary(na.omit(MAPE))),
                       Rsquared = as.vector(summary(na.omit(Rsquared)))),
                    by = keys]
  
  
} else
{
  # compute diagnostic errors
  gbm.diag = gbm.cv[,.(stat = factor(stat, levels = stat),
                       Multi.Log.Loss = as.vector(summary(na.omit(Multi.Log.Loss))),
                       Kappa = as.vector(summary(na.omit(Kappa))),
                       Macro.Accuracy = as.vector(summary(na.omit(Macro.Accuracy))),
                       Micro.Accuracy = as.vector(summary(na.omit(Micro.Accuracy)))),
                    by = keys]
}

# set the keys in mod.id and gbm.diag
setkeyv(mod.id, keys)
setkeyv(gbm.diag, keys)

# join mod.id onto gbm.diag
gbm.diag = mod.id[gbm.diag]

# lets rank each model
# decide which type of model to run
if(Y.type == "Binary")
{
  # extract the median AUC & Log Loss of each model in gbm.diag
  gbm.rank = data.table(gbm.diag[stat == "Median", .(mod, AUC, Log.Loss)])
  
  # rescale AUC and Log Loss to a [0, 1] range
  gbm.rank[, AUC := rescale(AUC, to = c(0, 1))]
  gbm.rank[, Log.Loss := 1 - rescale(Log.Loss, to = c(0, 1))]
  
  # compute the model score
  gbm.rank[, score := AUC + Log.Loss]
  
  # sort gbm.rank by score
  gbm.rank = gbm.rank[order(-score)]
  
  # define the model rank
  gbm.rank[, rank := 1:nrow(gbm.rank)]
  
  # just keep mod and rank in gbm.rank
  gbm.rank = gbm.rank[,.(mod, rank)]
  
} else if(Y.type == "Numeric")
{
  # extract the median Rsquared & RMSE of each model in gbm.diag
  gbm.rank = data.table(gbm.diag[stat == "Median", .(mod, Rsquared, RMSE)])
  
  # rescale Rsquared and RMSE to a [0, 1] range
  gbm.rank[, Rsquared := rescale(Rsquared, to = c(0, 1))]
  gbm.rank[, RMSE := 1 - rescale(RMSE, to = c(0, 1))]
  
  # compute the model score
  gbm.rank[, score := Rsquared + RMSE]
  
  # sort gbm.rank by score
  gbm.rank = gbm.rank[order(-score)]
  
  # define the model rank
  gbm.rank[, rank := 1:nrow(gbm.rank)]
  
  # just keep mod and rank in gbm.rank
  gbm.rank = gbm.rank[,.(mod, rank)]
  
} else
{
  # extract the median Micro.Accuracy & Multi.Log.Loss of each model in gbm.diag
  gbm.rank = data.table(gbm.diag[stat == "Median", .(mod, Micro.Accuracy, Multi.Log.Loss)])
  
  # rescale Micro.Accuracy and Multi.Log.Loss to a [0, 1] range
  gbm.rank[, Micro.Accuracy := rescale(Micro.Accuracy, to = c(0, 1))]
  gbm.rank[, Multi.Log.Loss := 1 - rescale(Multi.Log.Loss, to = c(0, 1))]
  
  # compute the model score
  gbm.rank[, score := Micro.Accuracy + Multi.Log.Loss]
  
  # sort gbm.rank by score
  gbm.rank = gbm.rank[order(-score)]
  
  # define the model rank
  gbm.rank[, rank := 1:nrow(gbm.rank)]
  
  # just keep mod and rank in gbm.rank
  gbm.rank = gbm.rank[,.(mod, rank)]
}

# make mod the key column in gbm.rank and gbm.diag
setkey(gbm.diag, mod)
setkey(gbm.rank, mod)

# join gbm.rank onto gbm.diag
gbm.diag = gbm.rank[gbm.diag]

# order gbm.diag by rank
gbm.diag = gbm.diag[order(rank)]

# export gbm.diag
write.csv(gbm.diag, "gbm-summary.csv", row.names = FALSE)

# add rank to gbm.cv
gbm.cv[, rank := gbm.rank$rank]

# add a Model identity column
gbm.cv[, Model := factor(ifelse(rank == 1, "Yes", "No"), levels = c("Yes", "No"))]

# plot our models performance metrics
# decide which type of model to run
if(Y.type == "Binary")
{
  gbm.plot = ggpairs(gbm.cv[,.(Accuracy, AUC, Log.Loss, Model)],
                     mapping = aes(color = Model, fill = Model),
                     upper = list(continuous = wrap("density", alpha = 1/4), combo = "box"),
                     lower = list(continuous = wrap("points"), combo = wrap("dot")),
                     diag = list(continuous = wrap("densityDiag", alpha = 1/3))) + 
    theme_bw(base_size = 20)
  
} else if(Y.type == "Numeric")
{
  gbm.plot = ggpairs(gbm.cv[,.(Rsquared, RMSE, MAPE, Model)],
                     mapping = aes(color = Model, fill = Model),
                     upper = list(continuous = wrap("density", alpha = 1/4), combo = "box"),
                     lower = list(continuous = wrap("points"), combo = wrap("dot")),
                     diag = list(continuous = wrap("densityDiag", alpha = 1/3))) + 
    theme_bw(base_size = 20)
  
} else
{
  gbm.plot = ggpairs(gbm.cv[,.(Macro.Accuracy, Micro.Accuracy, Multi.Log.Loss, Model)],
                     mapping = aes(color = Model, fill = Model),
                     upper = list(continuous = wrap("density", alpha = 1/4), combo = "box"),
                     lower = list(continuous = wrap("points"), combo = wrap("dot")),
                     diag = list(continuous = wrap("densityDiag", alpha = 1/3))) + 
    theme_bw(base_size = 20)
}

# set up a graphics window
# windows()

# set up a pdf file to capture the next graphic
pdf("gbm-plot.pdf", width = 16, height = 10, paper = "special") 

# call the plot
print(gbm.plot)

# close off the connection
dev.off()

# extract the model parameters of the best model
gbm.best = data.table(mod.id[mod == gbm.rank[rank == 1, mod]])

# predict the test set with the best model
# decide which type of model to run
if(Y.type == "Binary")
{
  # build the training model
  set.seed(42)
  mod = xgboost(label = Y, data = X,
                objective = objective, eval_metric = eval_metric,
                eta = gbm.best$eta, max_depth = gbm.best$max_depth, nthread = nthread,
                nrounds = gbm.best$nrounds, min_child_weight = gbm.best$min_child_weight,
                gamma = gbm.best$gamma, verbose = 0, scale.pos.weight = scale.pos.weight,
                subsample = gbm.best$subsample, colsample_bytree = gbm.best$colsample_bytree)
  
  # make predictions with the model on the test set
  ynew = as.numeric(as.numeric(predict(mod, newdata = as.matrix(test))) >= gbm.diag[mod == gbm.best$mod & stat == "Median", Cutoff])
  
  # free memory
  gc()
  
  # update data for re-training the model
  X = rbind(X, as.matrix(test))
  Y = c(Y, ynew)
  
  # build the training model
  set.seed(42)
  mod = xgboost(label = Y, data = X,
                objective = objective, eval_metric = eval_metric,
                eta = gbm.best$eta, max_depth = gbm.best$max_depth, nthread = nthread,
                nrounds = gbm.best$nrounds, min_child_weight = gbm.best$min_child_weight,
                gamma = gbm.best$gamma, verbose = 0, scale.pos.weight = scale.pos.weight,
                subsample = gbm.best$subsample, colsample_bytree = gbm.best$colsample_bytree)
  
  # make predictions with the model on the test set
  ynew = as.numeric(as.numeric(predict(mod, newdata = as.matrix(test))) >= gbm.diag[mod == gbm.best$mod & stat == "Median", Cutoff])
  
  # free memory
  gc()
  
  # give ynew an ID column to preserve its order
  ynew = data.table(ID = 1:length(ynew),
                    value = ynew)
  
  # set value as key column in ynew, and new.level as the key column in Y.mapping
  setkey(ynew, value)
  setkey(Y.mapping, new.level)
  
  # join Y.mapping onto ynew
  ynew = Y.mapping[ynew]
  
  # order ynew by ID
  ynew = ynew[order(ID)]
  
  # add the predictions in ynew to test
  test[, Predictions := ynew$old.level]
  
} else if(Y.type == "Numeric")
{
  # build the training model
  set.seed(42)
  mod = xgboost(label = Y, data = X,
                objective = objective, eval_metric = eval_metric,
                eta = gbm.best$eta, max_depth = gbm.best$max_depth, nthread = nthread,
                nrounds = gbm.best$nrounds, min_child_weight = gbm.best$min_child_weight,
                gamma = gbm.best$gamma, verbose = 0,
                subsample = gbm.best$subsample, colsample_bytree = gbm.best$colsample_bytree)
  
  # make predictions with the model on the test set
  ynew = as.numeric(predict(mod, newdata = as.matrix(test)))
  
  # free memory
  gc()
  
  # update data for re-training the model
  X = rbind(X, as.matrix(test))
  Y = c(Y, ynew)
  
  # build the training model
  set.seed(42)
  mod = xgboost(label = Y, data = X,
                objective = objective, eval_metric = eval_metric,
                eta = gbm.best$eta, max_depth = gbm.best$max_depth, nthread = nthread,
                nrounds = gbm.best$nrounds, min_child_weight = gbm.best$min_child_weight,
                gamma = gbm.best$gamma, verbose = 0,
                subsample = gbm.best$subsample, colsample_bytree = gbm.best$colsample_bytree)
  
  # make predictions with the model on the test set
  ynew = as.numeric(predict(mod, newdata = as.matrix(test)))
  
  # free memory
  gc()
  
  # add the predictions in ynew to test
  test[, Predictions := ynew]
  
} else
{
  # build the training model
  set.seed(42)
  mod = xgboost(label = Y, data = X, num_class = num_class,
                objective = objective, eval_metric = eval_metric,
                eta = gbm.best$eta, max_depth = gbm.best$max_depth, nthread = nthread,
                nrounds = gbm.best$nrounds, min_child_weight = gbm.best$min_child_weight,
                gamma = gbm.best$gamma, verbose = 0, weight = weight,
                subsample = gbm.best$subsample, colsample_bytree = gbm.best$colsample_bytree)
  
  # make predictions with the model on the test set
  ynew = predict(mod, newdata = as.matrix(test), reshape = TRUE)
  
  # extract the class with the highest probability as the prediction
  ynew = apply(X = ynew, MARGIN = 1, FUN = which.max) - 1
  
  # free memory
  gc()
  
  # update data for re-training the model
  X = rbind(X, as.matrix(test))
  Y = c(Y, ynew)
  
  # build the training model
  set.seed(42)
  mod = xgboost(label = Y, data = X, num_class = num_class,
                objective = objective, eval_metric = eval_metric,
                eta = gbm.best$eta, max_depth = gbm.best$max_depth, nthread = nthread,
                nrounds = gbm.best$nrounds, min_child_weight = gbm.best$min_child_weight,
                gamma = gbm.best$gamma, verbose = 0, weight = weight,
                subsample = gbm.best$subsample, colsample_bytree = gbm.best$colsample_bytree)
  
  # make predictions with the model on the test set
  ynew = predict(mod, newdata = as.matrix(test), reshape = TRUE)
  
  # extract the class with the highest probability as the prediction
  ynew = apply(X = ynew, MARGIN = 1, FUN = which.max) - 1
  
  # free memory
  gc()
  
  # give ynew an ID column to preserve its order
  ynew = data.table(ID = 1:length(ynew),
                    value = ynew)
  
  # set value as key column in ynew, and new.level as the key column in Y.mapping
  setkey(ynew, value)
  setkey(Y.mapping, new.level)
  
  # join Y.mapping onto ynew
  ynew = Y.mapping[ynew]
  
  # order ynew by ID
  ynew = ynew[order(ID)]
  
  # add the predictions in ynew to test
  test[, Predictions := ynew$old.level]
}

# extract importance information for each variable
var.data = xgb.importance(model = mod, feature_names = colnames(X))

# determine if there are any variables that weren't even used
unused.vars = setdiff(colnames(X), var.data$Feature)

# if there were any unused variables then add them to gbm.imp with 0 values for each column
if(length(unused.vars) > 0)
{
  # make unused.vars into a data table
  unused.vars = data.table(Feature = unused.vars, 
                           Gain = rep(0, length(unused.vars)), 
                           Cover = rep(0, length(unused.vars)), 
                           Frequency = rep(0, length(unused.vars)))
  
  # append unused.vars to var.data
  var.data = rbind(var.data, unused.vars)
}

# rename the columns of var.data
setnames(var.data, c("Variable", names(var.data)[-1]))

# order var.data by Gain
var.data = var.data[order(Gain, decreasing = TRUE)]

# export test
write.csv(test, "gbm-test-predictions.csv", row.names = FALSE)

# export var.data
write.csv(var.data, "gbm-variable-data.csv", row.names = FALSE)

# free memory
gc()

}
























library(magrittr)
library(caret)
library(tidyr)
library(dplyr)
library(h2o)

data("Insurance", package = "MASS")
nFolds <- 3
kFoldIndices <- createFolds(Insurance$Claims, k = nFolds, list = FALSE)
Insurance2 <- Insurance %>%
  mutate(Group = factor(Group, ordered = FALSE),
         Age = factor(Age, ordered = FALSE),
         logHolders = log(Holders),
         fold = kFoldIndices)
localH2O <- h2o.init(nthreads = -1)
Insurance_h2o <- as.h2o(Insurance2, destination_frame = "Insurance_h2o")

getGLMResults <- function(x, y, training_frame, family, link, lambda, offset_column, fold) {
  model <- h2o.glm(x = x, y = y, 
                   training_frame = training_frame[! training_frame$fold == fold,],
                   family = family, link = link, offset_column = offset_column)
  predicted <- h2o.predict(model, training_frame[training_frame$fold == fold,])
  actual <- training_frame[training_frame$fold == fold, y]
  coefficients <- model@model$coefficients %>%
    data.frame(variable = names(.), coefficient = ., row.names = NULL)
  actualAndPredicted <- as.data.frame(h2o.cbind(actual, predicted))
  names(actualAndPredicted) <- c("actual", "predicted")
  list(model = model,
       actualAndPredicted = actualAndPredicted,
       coefficients = coefficients)
}

predictors <- c("District", "Group", "Age")
response <- "Claims"
xvalResults <- 1:nFolds %>%
  lapply((function(x) getGLMResults(x = predictors,
                                    y = response,
                                    training_frame = Insurance_h2o,
                                    family = "poisson",
                                    link = "log",
                                    lambda = 0,
                                    offset_column = "logHolders",
                                    fold = x)))

relativities <- xvalResults %>%
  lapply((. %>% extract2("coefficients"))) %>%
  Reduce(function(x, y) left_join(x, y, by = "variable"), .) %>%
  lapply(function(x) if(is.numeric(x)) exp(x) else x) %>%
  as_data_frame
names(relativities) <- c("variable", paste0("fold", 1:nFolds, "of", nFolds))

# library(DT)
# relativities %>% 
#   datatable %>% 
#   formatRound(2:(nFolds + 1), digits = 2)

relativitiesLong <- relativities %>%
  filter_(~ ! variable %in% c(response, "Intercept")) %>%
  separate(variable, into = c("variable", "level"), sep = "\\.", extra = "merge") %>%
  gather(fold, relativity, -variable, -level)

plotRelativities <- function(x) {
  variable <- unique(x$variable)
  ggplot(x, aes(x = level, y = relativity)) +
    geom_boxplot(alpha = 0.5) + 
    geom_point(aes(group = fold, colour = fold, shape = fold), size = 4, position = "jitter") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(label = variable)
}

relativityPlots <- relativitiesLong %>%
  split(.$variable) %>%
  lapply(plotRelativities)
# relativityPlots

## predicted vs actual

liftChartData <- xvalResults[[1]]$actualAndPredicted %>%
  mutate(bin = findInterval(predicted, quantile(predicted, probs = seq(0, 1, 0.2)),
                            
                            rightmost.closed = TRUE) %>% as.factor ) %>%
  group_by(bin) %>%
  summarize(avgPredicted = mean(predicted),
            avgActual = mean(actual)) %>%
  gather(variable, value, avgPredicted:avgActual)

# ggplot(liftChartData, aes(bin, value)) + geom_bar(stat = "identity", aes(fill = variable), 
#                                          position = "dodge")

# library(rcdimple)
# liftChartData %>%
#   dimple(x = c("bin", "variable"), y = "value", type = "bar", groups = "variable") %>%
#   xAxis(type = "addCategoryAxis", title = "Predicted quantile") %>%
#   yAxis(type = "addMeasureAxis", title = "Value") %>%
#   add_legend() %>%
#   add_title("Out of sample lift chart")

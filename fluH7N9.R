# Load Data and Libraries
library(outbreaks)
dataset <- fluH7N9.china.2013

summary(dataset)
str(dataset)

# convert age ? to NAs
dataset$age[which(dataset$age == "?")] <- NA

# create datset without NA outcome
dataset_gather <- dataset[!is.na(dataset$outcome),]

# scatterplots of all pairs of attributes
library(GGally)
pairs(dataset_gather)


# Plot gender
# add a level for unknown gender
levels(dataset_gather$gender) <- c(levels(dataset_gather$gender), "unknown")
dataset_gather$gender[is.na(dataset_gather$gender)] <- "unknown"

# rename gender
library(plyr)
dataset_gather$gender <- mapvalues(dataset_gather$gender, from = c("m", "f", "unknown"),
                                   to = c("Male", "Female", "Unknown gender"))

library(ggplot2)
ggplot(data = dataset_gather, aes(x = gender, fill = outcome, color = outcome))+
    geom_bar(position = "dodge", alpha = 0.7, size = 1) +
    scale_fill_brewer(palette="Set1", na.value = "grey50") +
    scale_color_brewer(palette="Set1", na.value = "grey50") +
    labs(
        y = "Count",
        title = "2013 Influenza A H7N9 cases in China",
        subtitle = "Gender and Province numbers of flu cases"
    )


# Plot age
ggplot(data = dataset_gather[!is.na(dataset_gather$age), ], aes(x = as.numeric(age), fill = outcome, color = outcome)) +
    geom_density(alpha = 0.3, size = 1) +
    geom_rug() +
    scale_color_brewer(palette="Set1", na.value = "grey50") +
    scale_fill_brewer(palette="Set1", na.value = "grey50") +
    labs(
        x = "Age",
        y = "Density",
        subtitle = "Age distribution of flu cases"
    )



# Plot province
# rename provinces
dataset_gather$province <- mapvalues(dataset_gather$province,
                                     from = c("Anhui", "Beijing", "Fujian", "Guangdong", "Hebei", "Henan", "Hunan", "Jiangxi", "Shandong", "Taiwan"),
                                     to = rep("Other province", 10))

# rearrange province order so that Other is the last
dataset_gather$province <- factor(dataset_gather$province, levels = c("Jiangsu",  "Shanghai", "Zhejiang", "Other province"))

ggplot(data = dataset_gather, aes(x = province, fill = outcome, color = outcome))+
    geom_bar(position = "dodge", alpha = 0.7, size = 1) +
    scale_fill_brewer(palette="Set1", na.value = "grey50") +
    scale_color_brewer(palette="Set1", na.value = "grey50") +
    labs(
        y = "Count",
        title = "2013 Influenza A H7N9 cases in China",
        subtitle = "Gender and Province numbers of flu cases"
    )



# Plot date
# gather for plotting with ggplot2
library(tidyr)
dataset_gather_2 <- dataset_gather %>%
    gather(Group, Date, date.of.onset:date.of.outcome)

# rearrange group order
dataset_gather_2$Group <- factor(dataset_gather_2$Group, levels = c("date.of.onset", "date.of.hospitalisation", "date.of.outcome"))

# rename groups
dataset_gather_2$Group <- mapvalues(dataset_gather_2$Group, from = c("date.of.onset", "date.of.hospitalisation", "date.of.outcome"),
                                    to = c("Date of onset", "Date of hospitalisation", "Date of outcome"))


ggplot(data = dataset_gather_2, aes(x = Date, y = as.numeric(age), color = outcome)) +
    geom_point(aes(shape = gender), size = 1.5, alpha = 0.6) +
    geom_path(aes(group = case.ID)) +
    facet_wrap( ~ province, ncol = 2) +
    scale_shape_manual(values = c(15, 16, 17)) +
    scale_color_brewer(palette="Set1", na.value = "grey50") +
    scale_fill_brewer(palette="Set1") +
    labs(
        color = "Outcome",
        shape = "Gender",
        x = "Month in 2013",
        y = "Age",
        title = "2013 Influenza A H7N9 cases in China",
        subtitle = "Dataset from 'outbreaks' package (Kucharski et al. 2014)",
        caption = "\nTime from onset of flu to outcome."
    )


# Plot days between onset and outcome and between onset and hospitalisation
# calculate days_onset_to_outcome
dataset_gather$days_onset_to_outcome <- as.numeric(as.character(gsub(" days", "",
                                                                     as.Date(as.character(dataset_gather$date.of.outcome), format = "%Y-%m-%d") -
                                                                         as.Date(as.character(dataset_gather$date.of.onset), format = "%Y-%m-%d"))))
# calculate days_onset_to_hospital
dataset_gather$days_onset_to_hospital <- as.numeric(as.character(gsub(" days", "",
                                                                      as.Date(as.character(dataset_gather$date.of.hospitalisation), format = "%Y-%m-%d") -
                                                                          as.Date(as.character(dataset_gather$date.of.onset), format = "%Y-%m-%d"))))

p1 <- ggplot(data = dataset_gather[!is.na(dataset_gather$days_onset_to_outcome), ], aes(x = days_onset_to_outcome, fill = outcome, color = outcome)) +
    geom_density(alpha = 0.3, size = 1) +
    geom_rug() +
    scale_color_brewer(palette="Set1", na.value = "grey50") +
    scale_fill_brewer(palette="Set1", na.value = "grey50") +
    labs(
        x = "Days_onset_to_outcome",
        y = "Density"
    )

p2 <- ggplot(data = dataset_gather[!is.na(dataset_gather$days_onset_to_hospital), ], aes(x = days_onset_to_hospital, fill = outcome, color = outcome)) +
    geom_density(alpha = 0.3, size = 1) +
    geom_rug() +
    scale_color_brewer(palette="Set1", na.value = "grey50") +
    scale_fill_brewer(palette="Set1", na.value = "grey50") +
    labs(
        x = "Days_onset_to_hospital",
        y = "Density"
    )

library(gridExtra)
library(grid)

grid.arrange(p1, p2, ncol = 2)


# Plot early onset patients and outcome
dataset_gather$early_onset = as.factor(ifelse(dataset_gather$date.of.onset < summary(dataset_gather$date.of.onset)[[3]], 'early onset', 'not early onset'))

ggplot(data = dataset_gather[!is.na(dataset_gather$early_onset), ], aes(x = early_onset, fill = outcome, color = outcome))+
    geom_bar(position = "dodge", alpha = 0.7, size = 1) +
    scale_fill_brewer(palette="Set1", na.value = "grey50") +
    scale_color_brewer(palette="Set1", na.value = "grey50") +
    labs(
        x = "Coset type",
        y = "Count",
        title = "2013 Influenza A H7N9 cases in China"
    )


# Data wrangling
# convert gender into numeric values with 1 for female and 0 for male
dataset$gender_f <- as.factor(ifelse(dataset$gender == "f", 1, 0))

# convert provinces to binary classifiers (yes == 1, no == 0) for Shanghai, Zhejiang, Jiangsu and other provinces
dataset$province_Jiangsu = as.factor(ifelse(dataset$province == "Jiangsu", 1, 0))
dataset$province_Shanghai = as.factor(ifelse(dataset$province == "Shanghai", 1, 0))
dataset$province_Zhejiang = as.factor(ifelse(dataset$province == "Zhejiang", 1, 0))
dataset$province_other = as.factor(ifelse(dataset$province == "Zhejiang" | dataset$province == "Jiangsu" | dataset$province == "Shanghai", 0, 1))

# calculate days_onset_to_outcome
dataset$days_onset_to_outcome <- as.numeric(as.character(gsub(" days", "",
                                                              as.Date(as.character(dataset$date.of.outcome), format = "%Y-%m-%d") -
                                                                  as.Date(as.character(dataset$date.of.onset), format = "%Y-%m-%d"))))

# calculate days_onset_to_hospital
dataset$days_onset_to_hospital <- as.numeric(as.character(gsub(" days", "",
                                                               as.Date(as.character(dataset$date.of.hospitalisation), format = "%Y-%m-%d") -
                                                                   as.Date(as.character(dataset$date.of.onset), format = "%Y-%m-%d"))))


# the same binary classification is given for whether a case was hospitalised, and whether they had an early onset or early outcome
dataset$hospital = as.factor(ifelse(is.na(dataset$date.of.hospitalisation), 0, 1))

dataset$early_onset = as.factor(ifelse(dataset$date.of.onset < summary(dataset$date.of.onset)[[3]], 1, 0))

dataset$early_outcome = as.factor(ifelse(dataset$date.of.outcome < summary(dataset$date.of.outcome)[[3]], 1, 0))

# change age data type
dataset$age = as.numeric(dataset$age)

dataset <- subset(dataset, select = -c(2:4, 6, 8))


# Imputing missing data
# check all missing values
sapply(dataset,function(x) sum(is.na(x)))

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(dataset,2,pMiss)

library(mice)
md.pattern(dataset[, -c(2)])

dataset_impute <- mice(dataset[, -c(2)], print = FALSE, seed=500)
summary(dataset_impute)

dataset_complete <- complete(dataset_impute,1)
dataset_complete <- merge(dataset[, c(1,2), drop = FALSE], dataset_complete, by = "case.ID", all = TRUE)

# remove case.ID attribute
dataset_complete <- dataset_complete[, -1]

summary(dataset_complete$days_onset_to_outcome)


# Build Models
# split the dataset into train and test
train_index <- which(is.na(dataset_complete$outcome))
train_data <- dataset_complete[-train_index, ]
test_data  <- dataset_complete[train_index, -1]

library(caret)
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)

# Classification and Regression Trees
set.seed(27)
fit.cart <- train(outcome ~ .,
                  data=train_data,
                  method="rpart",
                  trControl=control)

library(rattle)
fancyRpartPlot(fit.cart$finalModel)

# Random Forest
set.seed(27)
fit.rf <- train(outcome ~ .,
                data=train_data,
                method="rf",
                trControl=control)

# Plot importance
# estimate variable importance
importance <- varImp(fit.rf, scale=TRUE)

# plot importance
importance_df <- importance$importance
importance_df$group <- rownames(importance_df)

library(plyr)
importance_df$group <- mapvalues(importance_df$group,
                                 from = c("age", "hospital2", "gender_f2", "province_Jiangsu2", "province_Shanghai2", "province_Zhejiang2", "province_other2", "days_onset_to_outcome", "days_onset_to_hospital", "early_onset2", "early_outcome2" ),
                                 to = c("Age", "Hospital", "gender", "Jiangsu", "Shanghai", "Zhejiang", "Other province", "Days onset to outcome", "Days onset to hospital", "Early onset", "Early outcome" ))


ggplot() +
    geom_point(data = importance_df, aes(x=Overall, y=reorder(group, Overall))) +
    labs(
        x = "Importance",
        title = "2013 Influenza A H7N9 cases in China",
        subtitle = "Scaled feature importance",
        caption = "\nDetermined with Random Forest and
    repeated cross validation (10 repeats, 10 times)"
    )

# Naive Bayes
set.seed(27)
fit.nb <- train(outcome ~ .,
                data=train_data,
                method="nb",
                trControl=control)

# Boosting
set.seed(27)
fit.bs <- train(outcome ~ .,
                data=train_data,
                method="ada",
                trControl=control)


# Select Best Model
# summarize accuracy of models
results <- resamples(list(cart=fit.cart, rf=fit.rf, rf=fit.rf, nb=fit.nb, bs=fit.bs))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize Best Model
fit.rf$results

# confusion matrix
fit.rf$finalModel$confusion


# predict on test data
Prediction <- predict(fit.rf, test_data)
test_data$outcome <- Prediction

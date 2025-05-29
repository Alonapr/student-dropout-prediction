library(tidyverse) 
library(e1071)
library(ggplot2)
library(caret)
library(DMwR)
library(rpart)
library(rpart.plot)
library(kernlab)
library(mlbench)
library(randomForest)
library(pROC)
library(MLmetrics)
library(iml)

ds <- read.csv("data.csv", sep = ";")
dim(ds)
str(ds)
attach(ds)
old_names <- c("Daytime.evening.attendance.", "Nacionality", 
               "Previous.qualification..grade.", 
               "Mother.s.qualification","Father.s.qualification", 
               "Mother.s.occupation", "Father.s.occupation",
               "Curricular.units.1st.sem..credited.", 
               "Curricular.units.1st.sem..enrolled.",
               "Curricular.units.1st.sem..evaluations.", 
               "Curricular.units.1st.sem..approved.",
               "Curricular.units.1st.sem..grade.", 
               "Curricular.units.1st.sem..without.evaluations.",
               "Curricular.units.2nd.sem..credited.", 
               "Curricular.units.2nd.sem..enrolled.",
               "Curricular.units.2nd.sem..evaluations.", 
               "Curricular.units.2nd.sem..approved.",
               "Curricular.units.2nd.sem..grade.", 
               "Curricular.units.2nd.sem..without.evaluations.")
new_names <- c("Day.evening.attendance", "Nationality", 
               "Prev.qualification.grade", 
               "Mother.qualification", "Father.qualification", 
               "Mother.occupation", "Father.occupation",
               "Cur.units.1sem.credited", "Cur.units.1sem.enrolled",
               "Cur.units.1sem.eval.", "Cur.units.1sem.approved", 
               "Cur.units.1sem.grade", "Cur.units.1sem.without.eval.",
               "Cur.units.2sem.credited", "Cur.units.2sem.enrolled",
               "Cur.units.2sem.eval.", "Cur.units.2sem.approved",
               "Cur.units.2sem.grade", "Cur.units.2sem.without.eval.")
for (i in seq_along(old_names)) {
  names(ds)[names(ds) == old_names[i]] <- new_names[i]
}

ds <- ds %>% 
  mutate(Marital.status = case_when(
    Marital.status == 1 ~ ".одинокий",
    Marital.status %in% c(2, 5) ~ ".офіц_неофіц.шлюб",
    Marital.status %in% c(3, 4, 6) ~ ".розлучений_вдівець",
  ),
  Application.mode = case_when(
    Application.mode %in% c(1, 17, 18) ~ ".загальний.контингент",
    Application.mode %in% c(2, 10, 26, 27) ~ ".за.нормативними.актами",
    Application.mode %in% c(5, 16) ~ ".за.регіоном",
    Application.mode == 7 ~ ".володарі.інших.вищих.курсів",
    Application.mode == 15 ~ ".іноземний.студент",
    Application.mode == 39 ~ ".старше23",
    Application.mode == 44 ~ ".володарі.диплома.технолог.спеціаліста",
    Application.mode == 53 ~ ".володарі.диплома.короткого.циклу",
    Application.mode %in% c(42, 43, 51, 57) ~ ".зміна.спеціальності_установи",
  ),
  Application.order = case_when(
    Application.order %in% c(0, 1) ~ "1_2",
    Application.order == 2 ~ "3",
    Application.order == 3 ~ "4",
    Application.order == 4 ~ "5",
    Application.order == 5 ~ "6",
    Application.order %in% c(6, 9) ~ "7_9",
  ),
  Course = case_when(
    Course == 33 ~ "Технології.виробництва.біопалива",
    Course == 171 ~ "Анімація.та.мультимедійний.дизайн",
    Course == 8014 ~ "Соціальна.служба.вечірня.форма",
    Course == 9003 ~ "Агрономія",
    Course == 9070 ~ "Комунікаційний.дизайн",
    Course == 9085 ~ "Ветеринарна.сестринська.справа",
    Course == 9119 ~ "Інженерія.інформатики",
    Course == 9130 ~ "Еквікультура",
    Course == 9147 ~ "Менеджмент",
    Course == 9238 ~ "Соціальна.служба",
    Course == 9254 ~ "Туризм",
    Course == 9500 ~ "Медична.сестринська.справа",
    Course == 9556 ~ "Гігієна.порожнини.рота",
    Course == 9670 ~ "Менеджмент.реклами.та.маркетингу",
    Course == 9773 ~ "Журналістика.та.комунікації",
    Course == 9853 ~ "Базова.освіта",
    Course == 9991 ~ "Менеджмент.вечірня.форма",
  ),
  Previous.qualification = case_when(
    Previous.qualification == 1 ~ ".Середня.освіта.12.класів",
    Previous.qualification == 19 ~ ".Середня.освіта.11.класів", 
    Previous.qualification %in% c(2, 3, 6, 40) ~ ".Ступінь.бакалавра",
    Previous.qualification %in% c(4, 5, 43) ~ ".Ступінь.магістра_докторська",
    Previous.qualification == 12 ~ ".Альтернатива.середній.освіті",
    Previous.qualification %in% c(9, 10, 14, 15, 38) ~ 
      ".Незакінчена.середня.освіта",
    Previous.qualification == 39 ~ ".Спеціалізований.технолог.курс",
    Previous.qualification == 42 ~ ".Професійний.технічний.курс",
  ),
  Mother.qualification = case_when(
    Mother.qualification == 1 ~ ".Середня.освіта.12.класів",
    Mother.qualification == 19 ~ ".Середня.освіта.11.класів", 
    Mother.qualification %in% c(12, 18) ~ ".Альтернатива.середній.освіті",
    Mother.qualification %in% c(2, 3, 6, 40) ~ ".Ступінь.бакалавра",
    Mother.qualification %in% c(4, 43) ~ ".Ступінь.магістра",
    Mother.qualification %in% c(5, 44) ~ ".Докторська.ступінь",
    Mother.qualification %in% c(22, 39, 41, 42) ~ ".Професійні.курси",
    Mother.qualification %in% c(9, 10, 14) ~ ".Незакінчена.середня.освіта",
    Mother.qualification %in% c(11, 26, 27, 29, 30, 38) ~ ".Закінчені.7_8.класів",
    Mother.qualification == 37 ~ ".Закінчені.5.класів",
    Mother.qualification %in% c(35, 36) ~ ".Незакінчені.5.класів",
    Mother.qualification == 34 ~ ".Невідомо",
  ),
  Father.qualification = case_when(
    Father.qualification == 1 ~ ".Середня.освіта.12.класів",
    Father.qualification == 19 ~ ".Середня.освіта.11.класів", 
    Father.qualification %in% c(12, 13, 18, 20) ~ ".Альтернатива.середній.освіті",
    Father.qualification %in% c(2, 3, 6, 40) ~ ".Ступінь.бакалавра",
    Father.qualification %in% c(4, 43) ~ ".Ступінь.магістра",
    Father.qualification %in% c(5, 44) ~ ".Докторська.ступінь",
    Father.qualification %in% c(22, 31, 33, 39, 41, 42) ~ ".Професійні.курси",
    Father.qualification %in% c(9, 10, 14, 25) ~ ".Незакінчена.середня.освіта",
    Father.qualification %in% c(11, 26, 27, 29, 30, 38) ~ ".Закінчені.7_8.класів",
    Father.qualification == 37 ~ ".Закінчені.5.класів",
    Father.qualification %in% c(35, 36) ~ ".Незакінчені.5.класів",
    Father.qualification == 34 ~ ".Невідомо",
  ),
  Mother.occupation = case_when(
    Mother.occupation == 0 ~ ".Студент",
    Mother.occupation == 1 ~ ".Представники.законодавчої.влади_директори",
    Mother.occupation %in% c(2, 125) ~ ".Спеціалісти.інтелектуальні_наукові.сфери",
    Mother.occupation == 3 ~ ".Технічні.фахівці.середнього.рівня",
    Mother.occupation == 4 ~ ".Адміністративний.персонал",
    Mother.occupation %in% c(5, 151:153) ~ 
      ".Персонал.сфери.послуг_особистого.обслуговування",
    Mother.occupation %in% c(6,175) ~ ".Фермери_ремесло_деревообробка",
    Mother.occupation %in% c(7, 171, 173) ~ 
      ".Кваліфіковані.робітники.промисловість_друкарство_ювеліри",
    Mother.occupation == 8 ~ ".Оператори.машин_монтажники",
    Mother.occupation %in% c(9, 192, 193) ~ ".Некваліфіковані.робітники",
    Mother.occupation %in% c(10, 90, 122, 123, 131, 132, 134, 194) ~ ".Інше",
    Mother.occupation %in% c(141, 143, 144) ~ 
      ".Фахівці.фінанси.облік_офісний.персонал",
    Mother.occupation == 191 ~ ".Працівники.прибирання",
    Mother.occupation == 99 ~ ".Невідомо",
  ),
  Father.occupation = case_when(
    Father.occupation == 0 ~ ".Студент",
    Father.occupation %in% c(1, 112, 114) ~ 
      ".Представники.законодавчої.влади_директори",
    Father.occupation %in% c(2, 121, 135) ~ 
      ".Спеціалісти.інтелектуальні_наукові.сфери",
    Father.occupation == 3 ~ ".Технічні.фахівці.середнього.рівня",
    Father.occupation == 4 ~ ".Адміністративний.персонал",
    Father.occupation %in% c(5, 151:154, 195) ~ 
      ".Персонал.сфери.послуг_особистого.обслуговування",
    Father.occupation %in% c(6, 161, 163, 175) ~ ".Фермери_ремесло_деревообробка",
    Father.occupation %in% c(7, 171, 172, 174) ~ 
      ".Кваліфіковані.робітники.промисловість_металургія",
    Father.occupation %in% c(8, 181:183) ~ ".Оператори.машин_монтажники_водії",
    Father.occupation %in% c(9, 192, 193) ~ ".Некваліфіковані.робітники",
    Father.occupation %in% c(10, 101:103) ~ ".Професії.збройних.сил",
    Father.occupation %in% c(90, 122, 123, 132, 131, 134, 135, 194) ~ ".Інше",
    Father.occupation %in% c(124, 141, 143, 144) ~ 
      ".Фахівці.фінанси.облік_офісний.персонал",
    Father.occupation == 99 ~ ".Невідомо",
  ),
  Target = case_when(
    Target %in% c("Enrolled", "Graduate") ~ 0,
    Target == "Dropout" ~ 1,
  ))
attach(ds)

cat_bin <- c(1:6, 8:12, 14:19, 21)
num <- c(7, 13, 20, 22:36)
num_names <- colnames(ds)[num]
cat_bin_names <- colnames(ds)[cat_bin]

#missing values
sum(colSums(is.na(ds)))

#Univariate analysis of categorical vars
ggplot(as.data.frame(table(Course)), aes(x = Course, y = Freq, fill = Course)) +
  geom_bar(stat = "identity") + theme_minimal() +
  labs(title = "", x = "", y = "Кількість") +
  coord_flip() +
  guides(fill = FALSE)

per <- round(table(Target)/nrow(ds)*100,2)
barplot(table(Target), col = c("#FFA07A", "yellow"), xlab = "Target", ylab = "Кількість")
text(x = barplot(table(Target), beside = TRUE, plot = FALSE), 
     y = table(Target) + 0.5, label = paste0(table(Target), " - ", as.vector(per), "%"), 
     pos = 1, cex = 0.8)

par(mfrow = c(3,3))
sapply(cat_bin, function(x)barplot(table(ds[,x]), xlab = colnames(ds)[x], main = ""))
sapply(colnames(ds)[c(1:3, 6, 9:12)], function(x)table(ds[[x]]))

#Univariate analysis of numerical vars
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
num_stat <- function(data) {
  result <- matrix(NA, nrow = length(data), ncol = 11)
  colnames(result) <- c("variable", "min", "max", "q0.25", "q0.75","median", 
                        "moda", "mean", "var", "skewness", "kurtosis")
  for(i in 1:length(data)) {
    var_name <- names(data)[i]  
    var_data <- data[[i]]
    Max <- max(var_data)
    Min <- min(var_data)
    q1 <- quantile(var_data, 0.25)
    q3 <- quantile(var_data, 0.75)
    result[i, 1] <- var_name
    result[i, 2] <- round(Min, 2)
    result[i, 3] <- round(Max,2)
    result[i, 4] <- round(q1,2)
    result[i, 5] <- round(q3,2)
    result[i, 6] <- round(median(var_data),2)
    result[i, 7] <- round(moda(var_data),2)
    result[i, 8] <- round(mean(var_data),2)
    result[i, 9] <- round(var(var_data),2)
    result[i, 10] <- round(skewness(var_data),2)
    result[i, 11] <- round(kurtosis(var_data),2)
  } 
  return(as.data.frame(result))
}
num_stat(ds[,num])

#Testing for normality
check_normality <- function(data, vars){
  norm_var <- character()
  alpha <- 0.5
  for (col in vars){
    p_value <- ks.test(data[, col], "pnorm")$p.value
    if (p_value > alpha){
      norm_var <- c(norm_var, colnames(data)[x])
    }
  }
  return(norm_var)
}
check_normality(data = ds, vars = num)

#Selection of significant variables
feature_selection <- function(data, target, cat, num) {
  type_specification <- list(categorical = character(), numeric = character())
  test_results <- list(categorical = list(), numeric = list())
  target_values <- data[[target]]
  alpha <- 0.05
  for (col in names(data)) {
    if (col %in% cat) {
      contingency_table <- table(data[[col]], target_values)
      chi_result <- chisq.test(contingency_table)
      test_results$categorical[[col]] <- list(chi_result$statistic, chi_result$p.value)
      if (chi_result$p.value <= alpha) {
        type_specification$categorical <- c(type_specification$categorical, col)
      }
    } else if (col %in% num){
      spearman_corr <- cor.test(data[[col]], target_values, method = "spearman")
      test_results$numeric[[col]] <- list(spearman_corr$estimate, spearman_corr$p.value)
      if (spearman_corr$p.value <= alpha) {
        type_specification$numeric <- c(type_specification$numeric, col)
      }
    }
  }
  return(type_specification)
}
selected_var <- feature_selection(data = ds, target = "Target", 
                                  cat = cat_bin_names, num = num_names)
num_not_in_sel <- setdiff(cat_bin_names, selected_var$categorical)
cat_not_in_sel <- setdiff(num_names, selected_var$numeric)
ds <- ds[, !(names(ds) %in% c(num_not_in_sel, cat_not_in_sel))]

#Correlation analysis for the remaining variables
cor_heatmap_num <- function(data, vars){
  cor_matrix <- cor(data[, vars], method = "spearman")
  cor_df <- as.data.frame(as.table(cor_matrix))
  names(cor_df) <- c("Var1", "Var2", "Correlation")
  ggplot(cor_df, aes(Var1, Var2, fill = Correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Correlation, 2)), color = "black", size = 2.3) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                         limits = c(-1, 1), breaks = seq(-1, 1, by = 0.2),
                         name = "Correlation") + theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8)) + coord_fixed() +
    labs(title = "Correlation Heatmap", x = NULL, y = NULL)
}
num_vars <- c(7, 12, 18:29)
cor_heatmap_num(data = ds, vars = num_vars)

#Elimination of multicollinearity
ds$Cur.units.enrolled <- rowSums(ds[, c("Cur.units.1sem.enrolled", 
                                        "Cur.units.2sem.enrolled")])
ds$Cur.units.approved <- rowSums(ds[, c("Cur.units.1sem.approved", 
                                        "Cur.units.2sem.approved")])
ds$Cur.units.grade <- rowMeans(ds[, c("Cur.units.1sem.grade", "Cur.units.2sem.grade")])
ds$Cur.units.eval <- rowSums(ds[, c("Cur.units.1sem.eval.", "Cur.units.2sem.eval.")])
ds$Cur.units.without.eval <- rowSums(ds[, c("Cur.units.1sem.without.eval.", 
                                            "Cur.units.2sem.without.eval.")])
ds <- ds[, !(names(ds) %in% c("Cur.units.1sem.enrolled", "Cur.units.2sem.enrolled",
                              "Cur.units.1sem.approved", "Cur.units.2sem.approved",
                              "Cur.units.1sem.grade", "Cur.units.2sem.grade",
                              "Cur.units.1sem.eval.", "Cur.units.2sem.eval.",
                              "Cur.units.1sem.without.eval.", 
                              "Cur.units.2sem.without.eval."))]
num_vars <- c(7, 12, 18:19, 21:25)
cor_heatmap_num(data = ds, vars = num_vars)

#one-hot encoding
ds$Target <- as.factor(Target)
cat <- c(1:4, 6, 8:11)
cat_names <- colnames(ds)[cat]
ds[,cat] <- lapply(ds[,cat], as.factor)
bin <- c(5, 13:17)
ds[,bin] <- lapply(ds[,bin], as.factor)
one_hot_encode <- function(data, vars){
  for (col in vars) {
    dummies <- model.matrix(~ . - 1, data = data[, col, drop = FALSE])
    data <- cbind(data, dummies)
  }
  return(data)
}
ds <- one_hot_encode(data = ds, vars = cat_names)
ds <- ds[, -cat]
ds[,17:ncol(ds)] <- lapply(ds[,17:ncol(ds)], as.factor)

#splitting into test, training, validation ds
set.seed(1234)
split_index <- createDataPartition(ds$Target, p = .6, list = FALSE)
train <- ds[split_index,]
t <- as.data.frame(table(train$Target))
per <- round(table(train$Target)/nrow(train)*100,2)
names(t) <- c("Target", "Freq")
pie_chart <- ggplot(t, aes(x = "", y = Freq, fill = Target)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(Freq, " - ", as.vector(per), "%")), 
            position = position_stack(vjust = 0.5),size = 5) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("#FFA07A", "yellow"), name = "Target") +
  theme(legend.text = element_text(size = 12)) 
print(pie_chart)

remain <- ds[-split_index,]
split_index2 <- createDataPartition(remain$Target, p = .5, list = FALSE)
validation <- remain[split_index2,]
X_valid <- validation[,-which(colnames(validation) == "Target")]
y_valid <- validation$Target
table(y_valid)
test <- remain[-split_index2,]
X_test <- test[,-which(colnames(test) == "Target")]
y_test <- test$Target
table(y_test)

#balancing training ds
train_balanced <- SMOTE(Target ~ ., train, perc.over = 100, perc.under = 200, k = 5) 
table(train_balanced$Target)
X_train <- train_balanced[,-which(colnames(train_balanced) == "Target")]
y_train <- train_balanced$Target

#prediction, evaluation
logit <- glm(y_train ~ ., data = X_train, family = binomial)
nb_model <- naiveBayes(y_train ~ ., data = X_train, usekernel = TRUE)
dt_model <- rpart(y_train ~ ., data = X_train)
rf_model <- randomForest(y_train ~ ., data = X_train)
svm_model <- svm(y_train ~ ., data = X_train, probability = TRUE)

prediction_evaluation <- function(X_test, y_test, model_list) {
  confusion_matrices <- list()
  metrics <- data.frame(model = names(model_list), 
                        accuracy = numeric(length(model_list)), 
                        precision = numeric(length(model_list)), 
                        recall = numeric(length(model_list)), 
                        F1 = numeric(length(model_list)), 
                        AUC = numeric(length(model_list)))
  plot(NULL, xlim = c(1, 0), ylim = c(0, 1), 
       xlab = "FPR", ylab = "TPR", legend = TRUE)
  segments(1, 0, 0, 1)
  for (model_name in names(model_list)) {
    model <- model_list[[model_name]]
    if (model_name == "LR") {
      predicted_probabilities <- predict(model, newdata = X_test, type = "response")
      threshold <- 0.5
      predictions <- ifelse(predicted_probabilities > threshold, 1, 0)
    } else if (model_name == "NB") {
      predicted_probabilities <- predict(model, X_test, type = "raw")[,2]
      predictions <- predict(model, newdata = X_test, type = "class")
    } else if (model_name == "DT") {
      predicted_probabilities <- predict(model, X_test, type = "prob")[,2]
      predictions <- predict(model, newdata = X_test, type = "class")
    } else if (model_name == "RF") {
      predicted_probabilities <- predict(model, X_test, type = "prob")[,2]
      predictions <- predict(model, newdata = X_test)
    } else if (model_name == "SVM") {
      predicted_probabilities <-  attr(predict(model, X_test, probability = TRUE), 
                                       "probabilities")[, 2]
      predictions <- predict(model, X_test)
    }
    conf_matrix <- table(predictions, y_test)
    confusion_matrices[[model_name]] <- conf_matrix
    TN <- conf_matrix[1,1]
    FN <- conf_matrix[1,2]
    FP <- conf_matrix[2,1]
    TP <- conf_matrix[2,2]
    accuracy <- (TP+TN)/(TP+TN+FP+FN)
    precision <- TP/(TP+FP)
    recall <- TP/(TP+FN)
    F1 <- (2*precision*recall)/(precision+recall)
    roc_object <- roc(y_test, predicted_probabilities)
    auc_val <- auc(roc_object)
    idx <- which(metrics$model == model_name)
    metrics[idx, "accuracy"] <- round(accuracy, 4)
    metrics[idx, "precision"] <- round(precision, 4)
    metrics[idx, "recall"] <- round(recall, 4)
    metrics[idx, "F1"] <- round(F1, 4)
    metrics[idx, "AUC"] <- round(auc_val, 4)
    lines(roc_object, col = rainbow(length(model_list))[which(names(model_list) == model_name)])
  }
  legend("bottomright", legend = names(model_list), col = rainbow(length(model_list)), lty = 1)
  for (model_name in names(confusion_matrices)) {
    cat("Confusion matrix for", model_name, ":\n")
    print(confusion_matrices[[model_name]])
  }
  cat("Metrics:\n")
  print(metrics)
  cat("\n")
}
model_list <- list(SVM = svm_model, LR = logit, RF = rf_model, DT = dt_model, NB = nb_model)
prediction_evaluation(X_test, y_test, model_list)

#setting hyperparameters SVM
tuning_svm <- function(params, y_train, X_train, y_valid, X_valid) {
  if (params$kernel == "linear") {
    results <- data.frame(cost = params$C, F1 = numeric(length(params$C)))
    for (p in seq_along(params$C)){
      model <- svm(y_train ~ ., data = X_train, kernel = params$kernel,
                   cost = params$C[p])
      predictions <- predict(model, newdata = X_valid)
      f1 <- F1_Score(y_valid, predictions, positive = 1)
      results$F1[p] <- f1
    }
    print(results)
    max_F1_index <- which.max(results$F1)
    max_F1_value <- results$F1[which.max(results$F1)]
    best_C <- results$cost[max_F1_index]
    cat("\n", "Максимальне значення F1:", max_F1_value, "для cost =", best_C, "\n")
    best_model <- svm(y_train ~ ., data = X_train, kernel = params$kernel,
                      cost = best_C, probability = TRUE)
    return(list("best_model" = best_model, "results" = results))
  } else if (params$kernel == "radial") {
    results <- expand.grid(cost = params$C, gamma = params$gamma, F1 = numeric(1))
    for (p in seq_along(params$C)){
      for (g in seq_along(params$gamma)){
        index <- (p - 1) * length(params$gamma) + g
        model <- svm(y_train ~ ., data = X_train, kernel = params$kernel,
                     gamma = params$gamma[g],
                     cost = params$C[p])
        predictions <- predict(model, newdata = X_valid)
        F1 <- F1_Score(y_valid, predictions, positive = 1)
        results$F1[index] <- F1
      }
    }
    print(results)
    max_F1_index <- which.max(results$F1)
    max_F1_value <- results$F1[max_F1_index]
    best_C <- results$cost[max_F1_index]
    best_gamma <- results$gamma[max_F1_index]
    cat("\n", "Максимальне значення F1:", max_F1_value, "для cost =", best_C,"gamma =",
        best_gamma, "\n")
    best_model <- svm(y_train ~ ., data = X_train, kernel = params$kernel,
                      cost = best_C, gamma = best_gamma, probability = TRUE)
    return(list("best_model" = best_model, "results" = results))
  } else if (params$kernel == "polynomial"){
    results <- expand.grid(cost = params$C, gamma = params$gamma, degree = params$degree,
                           kernel = params$kernel, F1 = numeric(1))
    for (p in seq_along(params$C)){
      for (g in seq_along(params$gamma)){
        for (k in seq_along(params$degree)){
          index <- ((p - 1) * length(params$gamma) * length(params$degree)) + 
            ((g - 1) * length(params$degree)) + k
          model <- svm(y_train ~ ., data = X_train, kernel = params$kernel,
                       gamma = params$gamma[g], degree = params$degree[k],
                       cost = params$C[p])
          predictions <- predict(model, newdata = X_valid)
          F1 <- F1_Score(y_valid, predictions, positive = 1)
          results$F1[index] <- F1
        }
      }
    }
    print(results)
    max_F1_index <- which.max(results$F1)
    max_F1_value <- results$F1[max_F1_index]
    best_C <- results$cost[max_F1_index]
    best_gamma <- results$gamma[max_F1_index]
    best_degree <- results$degree[max_F1_index]
    cat("\n", "Максимальне значення F1:", max_F1_value, "для cost =", best_C,
        ", gamma =", best_gamma, ", degree =", best_degree, "\n")
    best_model <- svm(y_train ~ ., data = X_train, kernel = params$kernel, 
                      degree = best_degree,cost = best_C, 
                      gamma = best_gamma, probability = TRUE)
    return(list("best_model" = best_model, "results" = results))
  }
}
#linear kernel
param_grid_l <- list(kernel = "linear", C = 10^(-1:3))  
l_svm <- tuning_svm(params = param_grid_l, y_train, X_train, y_valid, X_valid)
model_list <- list(SVM = l_svm$best_model)

#radial kernel
param_grid_r <- list(kernel = "radial", C = 10^(-1:3), gamma = 10^(-3:0)) 
r_svm <- tuning_svm(params = param_grid_r, y_train, X_train, y_valid, X_valid)
model_list <- list(SVM = r_svm$best_model)

#polynomial kernel
param_grid_p <- list(kernel = "polynomial", C = 10^(-1:3), gamma = 10^(-3:1),
                     degree = 1:5)
p_svm <- tuning_svm(params = param_grid_p, y_train, X_train, y_valid, X_valid)
model_list <- list(SVM = p_svm$best_model)

prediction_evaluation(X_test, y_test, model_list)

#setting hyperparameters RF
ncol_X_train <- ncol(X_train)
param_grid_rf <- list(ntree = seq(100, 1000, by = 100),
                      mtry = c(floor(sqrt(ncol_X_train) / 2),
                               floor(sqrt(ncol_X_train)), 
                               ceiling(sqrt(ncol_X_train) * 2))) 

tuning_rf <- function(params, y_train, X_train, y_valid, X_valid){
  results <- expand.grid(ntree = params$ntree, mtry = params$mtry, F1 = numeric(1))
  for (n in seq_along(params$ntree)){
    for (m in seq_along(params$mtry)){
      index <- (n-1)*length(params$mtry) + m
      model <- randomForest(y_train ~ ., data = X_train, 
                            ntree = params$ntree[n], mtry = params$mtry[m])
      predictions <- predict(model, newdata = X_valid)
      F1 <- F1_Score(y_valid, predictions, positive = 1)
      results$F1[index] <- F1
    }
  }
  print(results)
  max_F1_index <- which.max(results$F1)
  max_F1_value <- results$F1[which.max(results$F1)]
  best_ntree <- results$ntree[max_F1_index]
  best_mtry <- results$mtry[max_F1_index]
  cat("\n", "Максимальне значення F1:", max_F1_value, 
      "для ntree =", best_ntree, ", mtry =", best_mtry,"\n")
  best_model <- randomForest(y_train ~ ., data = X_train, ntree = best_ntree,
                             mtry = best_mtry)
  return(list("best_model" = best_model, "results" = results))
}
t_rf <- tuning_rf(params = param_grid_rf, y_train, X_train, y_valid, X_valid)
model_list <- list(RF = t_rf$best_model)
prediction_evaluation(X_test, y_test, model_list)

#Evaluate variable importance on models
F1 <- function(actual, predicted) {
  return(F1_Score(actual, predicted, positive = 1))
}

#SVM
mod_svm <- Predictor$new(model = l_svm$best_model, data = X_test, y = y_test, 
                         type = "prob")
imp_svm <- FeatureImp$new(mod_svm, loss = F1, compare = "difference", 
                          n.repetitions = 5)
top10_results <- imp_svm$results[order(imp_svm$results$importance), ][1:10, ]
ggplot(top10_results, aes(x = reorder(feature, importance), y = abs(importance))) +
  geom_bar(stat = "identity") + coord_flip() +
  labs(title = "SVM", x = "Feature", y = "Importance") + theme_minimal()

#RF
mod_rf <- Predictor$new(model = t_rf$best_model, data = X_test, y = y_test, 
                        type = "prob")
imp_rf <- FeatureImp$new(mod_rf, loss = F1, compare = "difference", 
                         n.repetitions = 5)
top10_results <- imp_rf$results[order(imp_rf$results$importance), ][1:10, ]
ggplot(top10_results, aes(x = reorder(feature, importance), y = abs(importance))) +
  geom_bar(stat = "identity") + coord_flip() + 
  labs(title = "RF", x = "Feature", y = "Importance") + theme_minimal()
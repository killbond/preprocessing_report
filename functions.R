library(ROCR)
library(randomForest)
library(e1071)

# Задачей данной функции является определение количество частей, на которое
# необходимо поделить выборку, в зависимости от указанного метода
#
# (integer) nfold - величина пропорции в зависимости от выбранного метода разделения
#
# (string) method - определяет метод разделения выборки. В случае 'number' - это
# общее количество частей во всей выборке, в случае 'percent' - это процент
# данных от всей выборки, выбираемый для обучения
getNparts <- function(nfold, method) {
  if(method != 'number') {
    nparts <- 100 %/% (100 - nfold)
  } else {
    nparts <- nfold
  }
  return(nparts)
}

# Задачей данной функции является разделение всей выборки данных на
# части для обучения и контрольной проверки
#
# (integer) nfold - величина пропорции в зависимости от выбранного метода разделения
#
# (integer) trainPart - порядковый номер части из всей выборки, предназначенный для
# контрольной проверки
#
# (data frame) data - полный пул исходных данных
#
# (string) method - определяет метод разделения выборки. В случае 'number' - это
# общее количество частей во всей выборке, в случае 'percent' - это процент
# данных от всей выборки, выбираемый для обучения
nfoldSubset <- function(nfold, trainPart, data, method = 'number') {
  
  # Определим общее число частей, в зависимости от метода
  nparts <- getNparts(nfold, method)
  
  # Проверим, правильный ли указан номер контрольной выборки
  if(nparts < trainPart) {
    return(FALSE)
  }
  
  # Определим количество строк в одной части выборки
  length <- nrow(data) %/% nparts
  
  # Определим, в какой части отрезка всей выборки
  # находится часть для контрольной проверки
  if(trainPart != 1 && trainPart != nparts) {
    # Контрольная часть в середине
    result <- list(
      # |xxx---xxx> Часть для обучения
      'learn' <- rbind(data[1:((trainPart - 1) * length),], data[(trainPart * length + 1):nrow(data),]),
      # |---xxx---> Часть для контрольной проверки
      'train' <- data[((trainPart - 1) * length + 1):((trainPart - 1) * length + length),]
    )
  } else {
    # Контрольная часть в начале либо в конце
    if(trainPart == 1) {
      result <- list(
        # |---xxxxxx> Часть для обучения
        'learn' = data[(length + 1):(nrow(data)),],
        # |xxx------> Часть для контрольной проверки
        'train' = data[(nrow(data) - length + 1):(nrow(data)),]
      )
    } else {
      result <- list(
        # |xxxxxx---> Часть для обучения
        'learn' = data[1:(nrow(data) - length),],
        # |------xxx> Часть для контрольной проверки
        'tain' = data[(nrow(data) - length + 1):nrow(data),]
      )
    }
  }
  return(result)
}

# Задача данной функции является сбор
# статистических данных по кросс-валидации конкретной модели
# 
# (integer) nfold - величина пропорции в зависимости от выбранного метода разделения
#
# (data frame) data - полный пул исходных данных
#
# (string) method - определяет метод разделения выборки. В случае 'number' - это
# общее количество частей во всей выборке, в случае 'percent' - это процент
# данных от всей выборки, выбираемый для обучения
crossValidate <- function(nfold, data, method = 'number', model = 'glm', use.storage = FALSE) {
  
  # Определим общее число частей, в зависимости от метода
  nparts <- getNparts(nfold, method)
  
  # Определим до начала испытаний является ли текущий вызов leave-one-out кросс-валидацией
  loocv <- nrow(data) == nfold
  
  # Заранее подготовим вектор для накапливания откликов модели
  storage <- c()
  
  # Счетчик порядкового номера контрольной выборки
  trainPart <- 1
  result <- list()
  while(trainPart <= nparts) {
    # Разделим выборку на части
    prepared <- nfoldSubset(nfold, trainPart, data, method)
    learn <- prepared[[1]]
    train <- prepared[[2]]
    
    if(model == 'glm') {
      fit <- glm(class ~ ., learn, family = "binomial")
    } else if(model == 'svm') {
      fit <- svm(class ~ ., data = learn)
    } else if(model == 'rf') {
      fit <- randomForest(class ~., data = learn, ntree = 100, proximity = TRUE)
    }
    
    # Предсказываем вероятность в масштабе предсказуемого значения (class), т.е. от 0 до 1
    prob <- predict(object = fit, newdata = train, type = "response")
    
    # В случае leave-on-out накапливаем отклик
    if(loocv || use.storage) {
      storage[trainPart] <- prob
    } else {
      # В вектор результата запишем среднее число правильных ответов модели и AUC
      result[[trainPart]] <- modelSummary(prob, train$class)
    }
    
    # Увеличиваем счетчик номера контрольной выборки
    trainPart <- trainPart + 1
  }
  
  # В случае leave-one-out кросс-валидации сравнительные характеристики рассчитываем по заранее 
  # по заранее накопленному вектору откликов модели
  if(loocv || use.storage) {
    return(modelSummary(storage, data$class))
  } else {
    return(list(
      # Среднее количество правильных ответов для всех моделей
      'mean' = mean(unlist(sapply(result, "[", 'mean'))),
      # Средняя площадь ROC кривой для всех моделей
      'AUC' = mean(unlist(sapply(result, "[", 'AUC')))
    ))
  }
}

# Задачей данной функции является подсчет сравнительных характеристик по рассчитанной модели
#
# (vector) prob - вектор вероятностей принадлежности наблюдений тому или иному классу
#
# (vector) actual - вектор действительных классов наблюдений
modelSummary <- function(prob, actual) {
  
  # Для дальнейших вычислений необходимо, чтобы данные хранились в виде объекта, используемого библиотекой ROCR
  # следующей командой создаем этот объект (предсказанные вероятности + требуемые результирующие значения)
  pred_fit <- prediction(prob, factor(actual, levels = c(0, 1)))
  
  # Находим специфичность решений
  # Это вектор зависимостей между порогом классификации и числом правильных ответов для класса "1", т.к. у нас бинарная модель
  specificity  <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
  
  # Находим чувствительность решений
  # Это вектор зависимостей между порогом классификации и числом правильных ответов для класса "0", т.к. у нас бинарная модель
  sensitivity  <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
  
  # Отобразим обе зависимости на графике
  plot(specificity, col = "red", lwd = 1)
  plot(add = T, sensitivity , col = "blue", lwd = 1)
  plot(add = T, performance(pred_fit, x.measure = "cutoff", measure = "rch") , col = "green", lwd = 2)
  
  # Для определения порога классификации посчитаем абсолютную разницу между значениями кривых специфичности и чувствительности
  diff <- abs(unlist(specificity@y.values) - unlist(sensitivity@y.values))
  
  # Получаем порог классификации по наименьшей из величин вектора разниц
  threshold <- unlist(sensitivity@x.values)[match(min(diff), diff)]
  
  # Отобразим порог на графике
  abline(v = threshold, lwd = 2)
  
  # Используя полученную вероятность и порог классификации, составим вектор ответов для полученной модели
  pred_resp  <- factor(ifelse(prob > threshold, 1, 0))
  
  # и вектор учета правильных ответов относительно значений в контрольной выборке
  correct  <- ifelse(pred_resp == actual, 1, 0)
  
  # Вычислим площадь под ROC кривой
  auc <- performance(pred_fit, x.measure = "cutoff", measure = "auc")@y.values[[1]]
  
  return(list(
    # Среднее число правильных ответов
    'mean' = mean(correct),
    # Площадь под ROC-кривой
    'AUC' = auc)
    )
}
source('functions.r')

library(XLConnect)

dataset <- function(N, formula = '') {
  
  mzs <- list()
  minimals <- c()
  maximals <- c()
  
  files_names <- list.files(path = "Healthy/", full.names = TRUE)
  xlsx <- files_names[grep('.xlsx', files_names)]
  healthlyEnd <- length(xlsx)
  for(item in 1:length(xlsx)) {
    file <- read.xlsx(xlsx[item], 1)
    mz <- sapply(as.vector(file[[1]][2:nrow(file)]), as.numeric)
    minimals[[length(minimals)+1]] <- min(mz)
    maximals[[length(maximals)+1]] <- max(mz)
    mzs[[length(mzs)+1]] <- mz
  }
  
  files_names <- list.files(path = "NYK/", full.names = TRUE)
  xlsx <- files_names[grep('.xlsx', files_names)]
  for(item in 1:length(xlsx)) {
    file <- read.xlsx(xlsx[item], 1)
    mz <- sapply(as.vector(file[[1]][2:nrow(file)]), as.numeric)
    minimals[[length(minimals)+1]] <- min(mz)
    maximals[[length(maximals)+1]] <- max(mz)
    mzs[[length(mzs)+1]] <- mz
  }
  
  MIN <- min(minimals)
  MAX <- max(maximals)
  if(formula == 'log') {
    MIN <- log10(MIN)
    MAX <- log10(MAX)
  } else if(formula == 'logShifted') {
    MIN <- 0
    MAX <- log10(MAX)
  }
  
  dataset <- data.frame(matrix(0, ncol = (N + 1)))
  sequences <- seq(from = MIN, to = MAX, length.out = (N + 1))
  for(item in 1:length(mzs)) {
    mz <- mzs[[item]]
    if(formula == 'log') {
      mz <- log10(mz)
    } else if(formula == 'logShifted') {
      mz <- log10(mz)
      minMZ <- min(mz)
      maxMZ <- max(mz)
      sapply(mz, function(x) { ((x - minMZ) * MAX) / (maxMZ - minMZ) })
    }
    spades <- vector(length = N)
    for(peak in 1:length(mz)) {
      for(interval in 1:N) {
        if(sequences[interval] <= mz[peak] && sequences[interval + 1] >= mz[peak]) {
          spades[interval] <- spades[interval] + 1
          break
        }
      }
    }
    class <- 1
    if(item <= healthlyEnd) {
      class <- 0
    }
    dataset[item, ] <- append(spades, class, after = 0)
  } 
  
  names(dataset) <- c('class', paste("predict", 1:N, sep=""))
  return(dataset)
}

nominal <- dataset(35)
logaryphmic <- dataset(35, 'log')
shifted <- dataset(35, 'logShifted')

crossValidate(nrow(nominal), nominal, model = 'glm')
crossValidate(nrow(nominal), logaryphmic, model = 'glm')
crossValidate(nrow(nominal), shifted, model = 'glm')

crossValidate(nrow(nominal), nominal, model = 'svm')
crossValidate(nrow(nominal), logaryphmic, model = 'svm')
crossValidate(nrow(nominal), shifted, model = 'svm')

crossValidate(nrow(nominal), nominal, model = 'rf')
crossValidate(nrow(nominal), logaryphmic, model = 'rf')
crossValidate(nrow(nominal), shifted, model = 'rf')



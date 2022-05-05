library(here)

load_data <- function(year){
  file_name <- here("data", paste0("OxIS ", year, " public.sav"))
  data <- read_sav(file_name)
  
  questions <- lapply(data, function(ques){
    attr(ques, "label")
  })
  answers <- lapply(data, function(ques){
    attr(ques, "labels")
  })
  
  covariates <- read_csv(here("data", "variables.csv"))[, 1:3]
  names(covariates) <- c("Q_ID", "value_type", "combine")
  covariates$Q_ID <- str_replace_all(covariates$Q_ID, "\\. ", "_")
  covariates$Q_ID <- str_replace_all(covariates$Q_ID, " ", "_")
  covariates$Q_ID <- str_replace_all(covariates$Q_ID, "\\.", "_")
  
  covariates <- covariates %>% filter(value_type %in% c("ordinal", "binary", "numerical", "categorical"))
  covariates$combine[is.na(covariates$combine)] <- -1
  
  outcomes <- read_csv(here("data", "outcomes.csv"))[, 1:4]
  names(outcomes) <- c("Q_ID", "category", "value_type", "reverse")
  
  return(list(raw_data = data,
              questions = questions, answers = answers,
              covariates = covariates, outcomes = outcomes))
}

preselect_variables <- function(data){
  covar_indeces <- sapply(names(data$questions), function(Q_ID){
    any(sapply(data$covariates$Q_ID, function(selected){
      grepl(paste0("^", selected), Q_ID, ignore.case = T)
    }))
  })
  covar_questions <- data$questions[covar_indeces]
  
  unmatched_indeces <- !sapply(data$covariates$Q_ID, function(selected){
    any(sapply(names(data$questions), function(Q_ID){
      grepl(paste0("^", selected), Q_ID, ignore.case = T)
    }))
  })
  unmatched <- c(data$covariates[unmatched_indeces, 1])
  
  outcome_indeces <- sapply(names(data$questions), function(Q_ID){
    any(sapply(data$outcomes$Q_ID, function(selected){
      grepl(paste0("^", selected), Q_ID, ignore.case = T)
    }))
  })
  outcome_questions <- data$questions[outcome_indeces]
  
  unmatched_indeces <- !sapply(data$outcomes$Q_ID, function(selected){
    any(sapply(names(data$questions), function(Q_ID){
      grepl(paste0("^", selected), Q_ID, ignore.case = T)
    }))
  })
  unmatched <- c(unmatched, data$outcomes[unmatched_indeces, 1])
  
  return(list(covar_indeces = covar_indeces, covar_questions = covar_questions,
              outcome_indeces = outcome_indeces, outcome_questions = outcome_questions,
              unmatched = unmatched))
}

preprocess_covars <- function(data, var_considered, reverse_order, exclude, value_maps){
  covariates <- data$covariates
  questions <- data$questions
  data <- data$raw_data
  covar_indeces <- var_considered$covar_indeces
  
  data.selected <- data[c(names(which(covar_indeces)), "weight")]
  data.selected[data.selected < 0] <- NA
  write_csv(data.selected, here("data", "temp.csv"))
  data.selected <- read_csv(here("data", "temp.csv"))
  for(col_i in 1:NCOL(data.selected)){
    data.selected[[col_i]] <- map_response(names(data.selected)[col_i],
                                           data.selected[[col_i]], value_maps)
  }
  
  covars.data <- data.frame(weight = data.selected$weight)
  final_vars <- c()
  
  for(row_i in 1:NROW(covariates)){
    Q_ID <- covariates$Q_ID[row_i]
    value_type <- covariates$value_type[row_i]
    combine <- covariates$combine[row_i]
    
    subques <- sapply(names(questions), function(ques){
      grepl(paste0("^", Q_ID), ques, ignore.case = T)
    }) %>% which() %>% names()
    subques <- subques[!(sapply(subques, function(subque){
      subque %in% exclude
    }))]
    reverse <- sapply(subques, function(subque){
      subque %in% reverse_order
    })
    
    if(value_type == "categorical"){
      for(subque in subques){
        cats <- unique(data.selected[[subque]])
        cats[is.na(cats)] <- "NA"
        cat_data <- data.selected[[subque]]
        cat_data[is.na(cat_data)] <- "NA"
        for(cat in cats){
          if(cat != "NA"){
            covars.data[[paste0(subque, "-", cat)]] <- (cat_data == cat)
          }
        }
        final_vars <- c(final_vars, subque)
      }
    } else if(combine == 1){
      X_data <- sapply(subques, function(subque){
        data.selected[[subque]] * (1-reverse[subque]*2)
      }) %>% rowMeans(na.rm = T)
      if(miss_too_much(X_data)){
        next
      } else{
        covars.data[[Q_ID]] <- X_data
        final_vars <- c(final_vars, Q_ID)
      }
      
    } else{
      for(subque in subques){
        if(miss_too_much(data.selected[[subque]])){
          next
        } else{
          covars.data[[subque]] <- data.selected[[subque]]
          final_vars <- c(final_vars, subque)
        }
      }
    }
  }
  
  weights <- covars.data[, 1]
  covars.data <- covars.data[, -1]
  
  return(list(covars.data = covars.data,
              final_vars = final_vars, weights = weights))
}

map_response <- function(Q_ID, value_vec, value_maps){
  ind <- sapply(names(value_maps), function(pattern){
    grepl(paste0("^", pattern), Q_ID, ignore.case = T)
  }) %>% which()
  
  if(length(ind) > 0){
    ind <- min(ind)
    replace_ind <- which(as.character(value_vec) %in% names(value_maps[[ind]]))
    value_vec[replace_ind] <- value_maps[[ind]][as.character(value_vec[replace_ind])]
  }
  
  return(value_vec)
}

miss_too_much <- function(vec, thr = 0.3){
  sum(is.na(vec)) / length(vec) > thr
}

show_proxies <- function(covars.data){
  covars.data <- as.matrix(covars.data)
  Sigma <- t(covars.data) %*% covars.data
  p <- NCOL(Sigma)
  names <- colnames(Sigma)
  high_corr <- 0.7
  
  proxies <- c()
  for(i in 1:p){
    for(j in i:p){
      if(i != j && abs(Sigma[i, j]) > high_corr){
        proxies <- rbind(proxies, c(names[i], names[j], round(Sigma[i, j], 2)))
      }
    }
  }
  colnames(proxies) <- c("cov1", "cov2", "corr")
    
  return(proxies)
}

preprocess_outcomes <- function(data, var_considered){
  outcomes <- data$outcomes
  data <- data$raw_data
  outcome_indeces <- var_considered$outcome_indeces
  
  data.selected <- data[c(names(which(outcome_indeces)), "weight")]
  data.selected[data.selected < 0] <- NA
  write_csv(data.selected, here("data", "temp.csv"))
  data.selected <- read_csv(here("data", "temp.csv"))
  
  covars.data <- data.frame(weight = data.selected$weight)
  categories <- unique(outcomes$category)
  
  outcome.data <- sapply(categories, function(category){
    cat_indeces <- which(outcomes$category == category)
    values <- sapply(cat_indeces, function(cat_index){
      data.selected[[outcomes$Q_ID[cat_index]]] * (1-outcomes$reverse[cat_index]*2)
    }) %>% rowMeans(na.rm = T)
    return(values)
  })
  names(outcome.data) <- categories
  
  return(outcome.data)
}

preprocess_data <- function(covars.data, outcome.data, weights){
  not_na <- (rowSums(is.na(outcome.data)) == 0)
  covars.data <- covars.data[not_na, ]
  outcome.data <- outcome.data[not_na, ]
  weights <- weights[not_na]
  
  is_outlier <- sapply(as.data.frame(outcome.data), function(outcome){
    (outcome - mean(outcome)) / sqrt(var(outcome)) > 2
  })
  not_outlier <- (rowSums(is_outlier) == 0)
  covars.data <- covars.data[not_outlier, ]
  outcome.data <- outcome.data[not_outlier, ]
  weights <- weights[not_outlier]
  
  covars.data <- lapply(covars.data, function(covar){
    covar[is.na(covar)] <- median(covar, na.rm = T)
    return(covar)
  })
  covars.data <- as.data.frame(covars.data)
  
  covars.data <- scale(covars.data, center = T, scale = F)
  covars.data <- scale(covars.data, center = F, scale = sqrt(colSums(covars.data^2)))
  
  outcome.data <- scale(outcome.data, center = T, scale = F)
  
  return(list(covars.data = covars.data, outcome.data = outcome.data, weights = weights))
}




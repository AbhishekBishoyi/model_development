
fun_idLinearComb <- function(dt){
  dt_matrix <-as.matrix(dt)
  rankifremoved <- sapply(1:ncol(dt_matrix), function (x) qr(dt_matrix[,-x])$rank)
  which(rankifremoved == max(rankifremoved))  
}


fun_checkNumericColumns <- function(dt){
  all_col <- sapply(dt,is.numeric)  
  which(all_col == FALSE)
}


fun_ismissing <- function(dt){
  
  if(is.atomic(dt)){
    df_return <- paste("count_missing: ",sum(is.na(dt)))
  }
  else{
    col_name <- colnames(dt)[colSums(is.na(dt)) > 0]
    if(length(col_name)>1)
      count_missing <- colSums(is.na(dt[,col_name]))
    else
      count_missing <- sum(is.na(dt[,col_name]))
    
    df_return <- cbind(col_name,count_missing)
    rownames(df_return) <- NULL  
  }
  
  df_return
}

fun_addpoly <- function(dt,target_var,degree){
  if(degree<=1)
    dt
  else{
    for(i in 2:degree){
      dt[,paste0(target_var,"_",i)] <- dt[,target_var]^i
    }
    dt  
  }
  
}

fun_backwardElimination <-  function(object){
    all_terms <- names(model.frame(object))
    x_terms <- all_terms[-1]
    
    object$aic <- tweedie::AICtweedie(object)
    aic_complex <- extractAIC(object)[2]
    aic_simple <- aic_complex - 1
    x_terms_temp1 <- x_terms
    
    for(i in 1:length(x_terms)){
      
      for(j in 1:length(x_terms_temp1)){
        
        x_terms_temp2 <- x_terms_temp1[-j]
        
        paste("AIC for ",object$formula," = ",aic_complex)  
      }
      
    }
}

fun_tweedie_profile <-  function(dt,target_var){
  fit_tweedie <- glm(fmla, data = dt, family = statmod::tweedie(var.power=1.1, link.power=0))
  p_tweedie<-predict.glm(fit_tweedie,newdata = dt,se.fit = TRUE,type = "response")
  min_mse <- sqrt(mean((dt[,target_var] - p_tweedie$fit)^2))     #tweedie::AICtweedie(fit_tweedie) 
  min_index <- 1.1
  rang <- seq(1.2,1.9,0.1)
  pb <- txtProgressBar(min = 1, max = length(rang), style = 3)
  k <- 1
    for(i in rang){
      
    error_msg <- try(fit_new <- glm(fmla, data = dt, family = statmod::tweedie(var.power=i, link.power=0)))
    if(class(error_msg) !="try-error"){
      p_new<-predict.glm(fit_new,newdata = dt,se.fit = TRUE,type = "response")
      new_mse <- sqrt(mean((dt[,target_var] - p_new$fit)^2)) 

      if(new_mse<min_mse){
        min_index <- i
        min_mse <- new_mse
      }
        
        
      }
      
    
    print(min_index)
    k<-k+1
    setTxtProgressBar(pb,k)
  }
    
  min_index
}
# object <- fit_tweedie

fun_findVariantCols <- function(dt){
  len_unique <- sapply(dt,function(x)length(unique(x)))
  rm_col_index <- which(len_unique == 1) 
  all_cols<- colnames(dt)
  cols <- all_cols 
  if(length(rm_col_index)>0){
    cols <- cols[-rm_col_index]
  }
  out <- NULL
  out$variant_cols <-cols
  
  "%ni%" <- Negate("%in%")
  out$invariant_cols <- all_cols[all_cols %ni% cols]
  
  out
}


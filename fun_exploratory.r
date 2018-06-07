#' Exploratory analysis functions
#' 

exploratory_lineplots <- function(dt,target_var,cat_var,operator){
  
  avg_target <- mean(dt[,target_var])
  
  if(is.numeric(dt[,cat_var]) & (length(unique(dt[,cat_var])))>10)
  {
    define_cuts <- seq(floor(min(dt[which(!is.na(dt[,cat_var])),cat_var])), 
                       ceiling(max(dt[which(!is.na(dt[,cat_var])),cat_var])), length.out = 10)
    
    dt[,cat_var]<-cut(dt[,cat_var],breaks = define_cuts,
                              include.lowest=TRUE)
  }else if (is.numeric(dt[,cat_var])){
    dt[,cat_var] <- factor(dt[,cat_var])
  }
  
  
  if(sum(is.na(dt[,cat_var]))>0)
    dt[,cat_var] <- addNA(dt[,cat_var])
  
  # Handle missing data in target variable. Code missing target var to 0.
  dt[is.na(dt[,target_var]),target_var] <- 0 
  
  aggr_table <- aggregate(dt[, target_var], list(dt[,cat_var]), paste(operator))
  colnames(aggr_table) <- c(cat_var,paste0(operator,"_",target_var))
  assign("yvar1",paste0(operator,"_",target_var))
  assign("yvar_box",target_var)
  ggplot2::theme_set(ggplot2::theme_gray(base_size = 18))
  plot1<-ggplot2::ggplot(data=aggr_table, ggplot2::aes_string(x=cat_var, y=yvar1, group=1)) +
    ggplot2::geom_line(color="red")+ggplot2::geom_hline(yintercept = avg_target,color = "orange",linetype="dashed")+
    ggplot2::geom_point()+ggplot2::xlab(cat_var)+ggplot2::ylab(paste0(operator,"_",target_var))+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  
 
  x<-dt[,cat_var]
  xtext<-as.data.frame(table(x))
  
  
  plot2<-ggplot2::ggplot(data=xtext, ggplot2::aes(x=xtext$x, y=xtext$Freq)) +
    ggplot2::geom_bar(stat="identity", color="black", fill="yellow")+
    ggplot2::xlab(cat_var)+ggplot2::ylab("Count of cases")+
    ggplot2::geom_text(ggplot2::aes(label=xtext$Freq), vjust=-0.5,
                       color="black", size=3.5)+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  
  gridExtra::grid.arrange(plot1, plot2, nrow=2)
}

exploratory_boxplots <- function(dt,target_var,cat_var,operator){
  
  
  if(is.numeric(dt[,cat_var]) & (length(unique(dt[,cat_var])))>10)
  {
    define_cuts <- seq(floor(min(dt[which(!is.na(dt[,cat_var])),cat_var])), 
                       ceiling(max(dt[which(!is.na(dt[,cat_var])),cat_var])), length.out = 10)
    
    dt[,cat_var]<-cut(dt[,cat_var],breaks = define_cuts,
                      include.lowest=TRUE)
  }else if (is.numeric(dt[,cat_var])){
    dt[,cat_var] <- factor(dt[,cat_var])
  }
  if(sum(is.na(dt[,cat_var]))>0)
    dt[,cat_var] <- addNA(dt[,cat_var])
  
  # Handle missing data in target variable. Code missing target var to 0.
  dt[is.na(dt[,target_var]),target_var] <- 0 
  
  aggr_table <- aggregate(dt[, target_var], list(dt[,cat_var]), paste(operator))
  colnames(aggr_table) <- c(cat_var,paste0(operator,"_",target_var))
  assign("yvar1",paste0(operator,"_",target_var))
  assign("yvar_box",target_var)
  ggplot2::theme_set(ggplot2::theme_gray(base_size = 18))

  plot1<- ggplot2::ggplot(data = dt,ggplot2::aes_string(x = cat_var,y = yvar_box))+
    ggplot2::geom_boxplot(color = "black")+
    ggplot2::geom_line(data=aggr_table, ggplot2::aes_string(x=cat_var, y=yvar1, group=1),color="red")+
    ggplot2::xlab(cat_var)+ggplot2::ylab(paste0(operator,"_",target_var))+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  
  x<-dt[,cat_var]
  xtext<-as.data.frame(table(x))
  
  
  plot2<-ggplot2::ggplot(data=xtext, ggplot2::aes(x=xtext$x, y=xtext$Freq)) +
    ggplot2::geom_bar(stat="identity", color="black", fill="yellow")+
    ggplot2::xlab(cat_var)+ggplot2::ylab("Count of cases")+
    ggplot2::geom_text(ggplot2::aes(label=xtext$Freq), vjust=-0.5,
                       color="black", size=3.5)+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  
  gridExtra::grid.arrange(plot1, plot2, nrow=2)
}



# head(cars)
# dt<- cars
# target_var <- "dist"
# cat_var <- "speed"
# operator <- "mean"
# exploratory_lineplots(cars,target_var,cat_var,operator)
# exploratory_boxplots(cars,target_var,cat_var,operator)
# # 

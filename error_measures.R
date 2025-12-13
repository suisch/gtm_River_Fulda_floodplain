

error_measures <- function(data_regr, x, y) {
  
  namesdata_regr_prior <- names(data_regr) 
  namesdata_regr_prior_x <- which(namesdata_regr_prior == x)
  names(data_regr)[namesdata_regr_prior_x] <- "x"
  namesdata_regr_prior_y <- which(namesdata_regr_prior == y)
  names(data_regr)[namesdata_regr_prior_y] <- "y"
  
  mod <- lm(formula = y ~ x, data = data_regr  )
  
  N = as.numeric(nrow(data_regr))
  
  if(N>2){
    #define the number of decimal places based on the value
    R2 <- 
      case_when(#"Each case is evaluated sequentially and the first 
        #match for each element determines the corresponding value in the 
        #output vector." thus, the sequence of the cases is decisive
        summary(mod)$r.squared < 0.00001 ~ round(summary(mod)$r.squared ,6),
        summary(mod)$r.squared < 0.0001 ~ round(summary(mod)$r.squared ,5),
        summary(mod)$r.squared < 0.001 ~ round(summary(mod)$r.squared ,4),
        summary(mod)$r.squared < 0.01 ~ round(summary(mod)$r.squared ,3),
        summary(mod)$r.squared < 0.1 ~ round(summary(mod)$r.squared ,2),
        .default = round(summary(mod)$r.squared ,1)
      )
  }else{
    R2 <- NA
  }
  
  
  if(N>2){
    #define the number of decimal places based on the value
    MAE <- 
      case_when(#"Each case is evaluated sequentially and the first match for each element determines the corresponding value in the output vector." thus, the sequence of the cases is decisive
        mean(abs(data_regr$y - data_regr$x)) < 0.00000001 ~ round(mean(abs(data_regr$y - data_regr$x)) ,9),
        mean(abs(data_regr$y - data_regr$x)) < 0.0000001 ~ round(mean(abs(data_regr$y - data_regr$x)) ,8),
        mean(abs(data_regr$y - data_regr$x)) < 0.000001 ~ round(mean(abs(data_regr$y - data_regr$x)) ,7),
        mean(abs(data_regr$y - data_regr$x)) < 0.00001 ~ round(mean(abs(data_regr$y - data_regr$x)) ,6),
        mean(abs(data_regr$y - data_regr$x)) < 0.0001 ~ round(mean(abs(data_regr$y - data_regr$x)) ,5),
        mean(abs(data_regr$y - data_regr$x)) < 0.001 ~ round(mean(abs(data_regr$y - data_regr$x)) ,4),
        mean(abs(data_regr$y - data_regr$x)) < 0.01 ~ round(mean(abs(data_regr$y - data_regr$x)) ,3),
        mean(abs(data_regr$y - data_regr$x)) < 0.1 ~ round(mean(abs(data_regr$y - data_regr$x)) ,2),    .default = round(mean(abs(data_regr$y - data_regr$x)) ,2)
      )
  }else{
    MAE <- NA
  }
  
  
  
  if(N>2){
    #define the number of decimal places based on the value
    MB <- 
      case_when(#"Each case is evaluated sequentially and the first match for each element determines the corresponding value in the output vector." thus, the sequence of the cases is decisive
        abs(mean(data_regr$y - data_regr$x)) < 0.000001 ~ round(mean(data_regr$y - data_regr$x) ,7),
        abs(mean(data_regr$y - data_regr$x)) < 0.00001 ~ round(mean(data_regr$y - data_regr$x) ,6),
        abs(mean(data_regr$y - data_regr$x)) < 0.0001 ~ round(mean(data_regr$y - data_regr$x) ,5),
        abs(mean(data_regr$y - data_regr$x)) < 0.001 ~ round(mean(data_regr$y - data_regr$x) ,4),
        abs(mean(data_regr$y - data_regr$x)) < 0.01 ~ round(mean(data_regr$y - data_regr$x) ,3),
        abs(mean(data_regr$y - data_regr$x)) < 0.1 ~ round(mean(data_regr$y - data_regr$x) ,2),    .default = round(mean(data_regr$y - data_regr$x) ,2)
      )
  }else{
    MB <- NA
  }
  
  
  
  NSE_term <- 1- sum((data_regr$x - data_regr$y)^2)/sum((data_regr$x-mean(data_regr$x))^2)
  
  if(N>1) {
    #define the number of decimal places based on the value
    NSE <- 
      case_when(#"Each case is evaluated sequentially and the first match for each element determines the corresponding value in the output vector." thus, the sequence of the cases is decisive
        abs(NSE_term) < 0.0001 ~ round(NSE_term ,5),
        abs(NSE_term) < 0.001 ~ round(NSE_term ,4),
        abs(NSE_term) < 0.01 ~ round(NSE_term ,3),
        abs(NSE_term) < 0.1 ~ round(NSE_term ,2),   
        .default = round(NSE_term ,2)
      )
  }else{
    NSE = NA
  }
  
  if(N>2){
    #define the number of decimal places based on the value
    RMSE <- 
      case_when(#"Each case is evaluated sequentially and the first match for each element determines the corresponding value in the output vector." thus, the sequence of the cases is decisive
        sqrt(mean((data_regr$y - data_regr$x)^2)) < 0.00000001 ~ round(sqrt(mean((data_regr$y - data_regr$x)^2)) ,9),
        sqrt(mean((data_regr$y - data_regr$x)^2)) < 0.0000001 ~ round(sqrt(mean((data_regr$y - data_regr$x)^2)) ,8),
        sqrt(mean((data_regr$y - data_regr$x)^2)) < 0.000001 ~ round(sqrt(mean((data_regr$y - data_regr$x)^2)) ,7),
        sqrt(mean((data_regr$y - data_regr$x)^2)) < 0.00001 ~ round(sqrt(mean((data_regr$y - data_regr$x)^2)) ,6),
        sqrt(mean((data_regr$y - data_regr$x)^2)) < 0.0001 ~ round(sqrt(mean((data_regr$y - data_regr$x)^2)) ,5),
        sqrt(mean((data_regr$y - data_regr$x)^2)) < 0.001 ~ round(sqrt(mean((data_regr$y - data_regr$x)^2)) ,4),
        sqrt(mean((data_regr$y - data_regr$x)^2)) < 0.01 ~ round(sqrt(mean((data_regr$y - data_regr$x)^2)) ,3),
        sqrt(mean((data_regr$y - data_regr$x)^2)) < 0.1 ~ round(sqrt(mean((data_regr$y - data_regr$x)^2)) ,2),    .default = round(sqrt(mean((data_regr$y - data_regr$x)^2)) ,2)
      )
  }else{
    RMSE <- NA
  }
  
  
  
  
  return(data.frame(
    run = run, group = NA, variable = NA,
    R2 = R2, 
    MAE = MAE, RMSE = RMSE,  MB = MB, NSE = NSE, N = N))
  
}

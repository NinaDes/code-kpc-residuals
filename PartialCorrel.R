PartialCorrel <- function(x, # I want to test the cond indip of x and y given z
                          y, 
                          z){ # z is a matrix. Its colnum is the number of var from wich I want the conditionament
  
  
  z = as.matrix(z)
    
  z = as.matrix(z)
  
  n = length(y)
  
  
  regr = data.frame(x, y, z)
  nc = ncol(regr)
  colnames(regr) = paste("x",1:nc,sep="")
  
  formx = create_formula(1, 3:nc, "x")
  formy = create_formula(2, 3:nc, "x")
  
  fitx  = gam(formx, dat = regr)
  fity  = gam(formy, dat = regr)
  
  resx   = fitx$residuals
  resy   = fity$residuals
  
  return(list(resx = resx, resy = resy))
  
 
  
}
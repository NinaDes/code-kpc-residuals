condIndipTestQ <- function (x,        # the first variable for cond ind
                            y,        # the second variable for cond ind
                            z,        # the set of variables to wich I condition
                            sig,      # the width of gaussian kernel
                            p,        # number of permutation of 1-alpha level test
                            test){    # 1 for Permut test, 2 for Gamma test   

  
  ParCor = PartialCorrel(x, y, z)
  resx   = ParCor$resx
  resy   = ParCor$resy
  
  pval = HSIC(as.matrix(resx), as.matrix(resy), sig, p, test)
  
  return (pval)
  
  
  
  
  
  
  
  
}
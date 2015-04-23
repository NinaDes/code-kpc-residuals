kPC_Residuals <- function( data, # the available data
                           alpha,# level of the test
                           sig,  # width gaussian kernel 
                           p,    # number of permutation in hsic
                           test){# type of test in hsic (1 = perm, 2 = gamma)
  
  

  
  source('Source_kPC_Resid.R')
  Source_kPC_Resid()
  
  
  # BE CAREFULL
  # G      adjac matr with all the doublearrow edge   for step A B C
  # G_dir  adjac matrix with only the directed edge   for step D, I create it at the end of step C
  # G_ad   adjac matrix: the RIGHT ONE
  # H      
  
  ## STEP A and B
  
  Sk      = Skeleton_R(data, alpha, sig, p, test )
  sepstep = Sk$sepstep
  G       = Sk$G 
  pval    = Sk$pval
  
  ## STEP C 
  
  Co = Collider(G, sepstep)
  
  G_dir = Co$G_dir
  G_ad  = Co$G_ad   
  Coll  = Co$collider
  
  ##STEP D
  undiEdge = updateUE(G_ad, NULL, NULL)
  s = 1
  
  SecSta  = SecondStage(data, G_ad, G_dir, undiEdge, s, alpha, sig, p, test)
  
  Adjacency_Matrix = SecSta
  
  
  par(mfrow=c(3,1))
  plot( as(G,'graphNEL'), main='Skeleton Residual' )
  plot( as(G_ad,'graphNEL'), main='Collider' )
  plot( as(Adjacency_Matrix,'graphNEL'), main='Final graph' )
  
  
  
  list(mat = Adjacency_Matrix, pval = pval, Coll = Coll, Gskel = G, Gcoll = G_ad)
  
} 

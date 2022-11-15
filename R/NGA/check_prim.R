# Check for primitivity and irreducibility ---------------------------------------------------

check_matrix = function(A){
  #checks numerically for non-negativity primitivity
  # and irreducibility (see Caswell 2005 chapter 7)  
  
  #INPUT:  matrix A
  #OUTPUT: string that evaluates A
  nrow=nrow(A)
  ncol=ncol(A)
  I=diag(1,nrow,ncol)
  
  if(nrow==ncol) {
    if(all(A>=0)){
      aux = (I+A^(nrow-1))
      if (all(aux>0)==T) {
        A_ = A^(nrow^2-2*nrow+2)
        bol= all(A_>0)
        if(bol==T){
          return("Matrix is primitive")
        }else{
          print(A_)
          return("Matrix is not primitive")
        }
      }else{
        print("I+A^(nrow-1)")
        print(aux)
        return("Matrix is not irreducible")
      }
      
    }else{
      print(A)
      stop("Matrix has negative entries")
    }
    
  }
  else {
    print(A)
    stop("Matrix is not square")
  }
}


# example: test for non-square
# set.seed(1)
# m=matrix(rpois(100,2),nrow = 25,ncol=4)
# m
# check_matrix(m)

# example: test for negative entries
# set.seed(1)
# m=matrix(rnorm(100,0,1),nrow = 10,ncol=10)
# m
# check_matrix(m)

# example: test for non irreducible

# set.seed(1)
# m=matrix(rpois(100,4),nrow = 10,ncol=10)
# m
# check_matrix(m)

# example: test for primitivity

# set.seed(1)
# m=matrix(rpois(100,3),nrow = 10,ncol=10)
# m
# check_matrix(m)


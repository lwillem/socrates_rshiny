#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => ADAPTED contact_matrix() function
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

# override socialmirx::contact_matrix function with small edits
# TODO include in socialmixr package
contact_matrix <- function(...){

  # print statement
  #print("Use modified contact_matrix() function")
  
  # run social_mixr function
  matrix_out <- socialmixr::contact_matrix(...)
  
  # fix (when symmetric)
  dimnames(matrix_out$matrix)[[1]] <- dimnames(matrix_out$matrix)[[2]]
  names(dimnames(matrix_out$matrix))[1] <- 'age.group'
  
  # remove population (for now)
  matrix_out$demography <- NULL
  
  # return
  matrix_out
}



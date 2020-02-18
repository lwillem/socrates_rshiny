#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => ADAPTED contact_matrix() function
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

# load adapted socialmirx::contact_matrix function
source('R_socialmixr/check.r')
source('R_socialmixr/contact_matrix.r')
source('R_socialmixr/survey.r')


# # override socialmirx::contact_matrix function with small edits
# # TODO include in socialmixr package
# contact_matrix <- function(...){
# 
#   # print statement
#   #print("Use modified contact_matrix() function")
#   
#   # run social_mixr function
#   matrix_out <- socialmixr::contact_matrix(...)
#   
#   
#   # fix (when symmetric)
#   dimnames(matrix_out$matrix)[[1]] <- dimnames(matrix_out$matrix)[[2]]
#   names(dimnames(matrix_out$matrix))[1] <- 'age.group'
#   
#   # remove population (for now)
#   matrix_out$demography <- NULL
#   
#   # # set age.groups as rownames instead of a column
#   age.groups <- matrix_out$participants$age.group
#   matrix_out$participants <- data.frame(as.matrix(matrix_out$participants[,-1]))
#   row.names(matrix_out$participants) <- age.groups
#   
#   # return
#   matrix_out
# }



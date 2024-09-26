#J=t(matrix(c(5.426261, 5.143364, 0.7721236,1.863814, 9.382832, 1.5404856,0.723321, 3.982414, 3.1749014),nrow=3,ncol=3))

# J=t(matrix(c(0.52, 0.11, 0.02, 0.17, 0.43, 0.16, 0.29, 0.14, 0.08,
#              0.11, 1.00, 0.14, 0.27, 0.57, 0.38, 0.17, 0.16, 0.07,
#              0.02, 0.14, 0.96, 0.71, 0.68, 0.48, 0.19, 0.07, 0.07,
#              0.04, 0.07, 0.17, 1.30, 0.69, 0.64, 0.59, 0.17, 0.13,
#              0.11, 0.15, 0.16, 0.70, 1.12, 0.90, 0.70, 0.370, 0.19,
#              0.04, 0.10, 0.13, 0.69, 0.94, 1.04, 0.73, 0.41, 0.29,
#              0.07, 0.04, 0.04, 0.58, 0.63, 0.62, 0.83, 0.38, 0.22,
#              0.04, 0.05, 0.02, 0.16, 0.35, 0.34, 0.38, 0.54, 0.27,
#              0.02, 0.02, 0.02, 0.15, 0.20, 0.27, 0.23, 0.28, 0.45
#              ),nrow=9,ncol=9))
#load("NGM.R")
#J=NGM
#J=t(matrix(c(5.426261, 5.143364, 0.7721236,1.863814),nrow=2,ncol=2))
# J=t(matrix(c(1, 3, 7,10,
#              10,2, 4, 35,
#              15, 3, 1, 17,
#              100, 15, 12, 2),nrow=4,ncol=4))
# J=t(matrix(c( 1, 10, 55,100,
#              10,  1,  4, 75,
#              55,  4,  1, 50,
#             100, 75, 50,  1),nrow=4,ncol=4))
J=t(matrix(c( 4, 0.1, 0.25,0.55,
             0.27,  2,  0.05, 0.11,
             0.87,  0.3,  3, 0.96,
            0.95, 0.77, 0.69,  5),nrow=4,ncol=4))
# eigen_test=eigen(diag(c(1,1,0),nrow = 3,ncol=3)%*%J)
# 
# stbl_dist=eigen_test$vectors[,1]/(sum(eigen_test$vectors[,1]));stbl_dist
# 
# p=0.2
# J_ = cbind(J,J)
# J_ = rbind((1-p)*J_,p*J_)
# 
# eigen_test2=eigen(J_)
# 
# stbl_dist2=eigen_test2$vectors[,1]/(sum(eigen_test2$vectors[,1]));stbl_dist2
# 
# 
# -0.0300431*((p)/(1-p))
# 
# eigen_test3=eigen(t(J_))
# eigen_test3

#J_%*%eigen_test2$vectors[,6]

# test the orthagonality of eigenvectors and the elasticity
ei_J=eigen(J)
ei_J$vectors

## 2 ways
ei_Jt=eigen(t(J)); ei_Jt$vectors                           # Leonardo's way
L=qr.solve(ei_J$vectors%*%t(ei_J$vectors),ei_J$vectors);L  # my way
copy_vectors=ei_J$vectors

for (i in seq(1,ncol(J))) {
  ei_J$vectors[,i]=ei_J$vectors[,i]/as.numeric((t(ei_J$vectors[,i])%*%ei_Jt$vectors[,i]))
  
}

ei_J$vectors
ei_Jt$vectors

ei_Jt$vectors/L # yup they are linear dependent

ei_Jt$vectors %*% t(ei_J$vectors) # yup they are orthogonal

A=(ei_J$vectors[,1] %*% t(ei_Jt$vectors[,1]))*J/ei_Jt$values[1];A # Leonardo's way?
B=(ei_Jt$vectors[,1] %*% t(ei_J$vectors[,1]))*J/ei_Jt$values[1];B # my way (Leos method for scaling vectors)
C=(L[,1] %*% t(copy_vectors[,1]))*J/ei_Jt$values[1];C             # my way

sum(A)
sum(B)
sum(C)

t(ei_J$vectors[,1]) %*% ei_Jt$vectors[,1]
t(copy_vectors[,1]) %*% L[,1]

# Further testing
J1=J
J1[2,3]=J[2,3]*1.1

J2=J
J2[3,2]=J[3,2]*1.1

eigen(J1)$value[1]
eigen(J2)$value[1]

## 

#test_f=eigen_(J)
#sensi_f=sens(test_f)
#elasti_f=elasti(sensi_f)

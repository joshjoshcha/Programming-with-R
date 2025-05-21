set.seed(3100)  
M <- matrix(rexp(100, rate = 1), nrow = 10, ncol = 10) 
M

# a
M_transpose <- t(M)  
M_transpose

# b 
M_trans_M <- M_transpose %*% M
M_trans_M

# c
I_10 <- diag(10) 
sum <- (2.5 * M) + (5.2 * I_10)  
sum

# d
# Let's first check if M has an inverse by calculating its determinant
det_M <- det(M) 
det_M # As long as det_M is non-zero, it will have an inverse

M_inverse <- solve(M)
M_inverse

# e
overall_mean <- mean(M)  
overall_mean

# f
row_sums <- apply(M, 1, sum)
row_w_largest_sum <- which.max(row_sums)
row_w_largest_sum

# g
percentage_above_1 <- apply(M, 2, function(x) sum(x > 1) / length(x) * 100)
percentage_above_1

#h
rownames(M) <- paste0("n", 1:10)
colnames(M) <- paste0("v", 1:10)
rownames(M)
colnames(M)

# i 
sub_matrix <- M[c("n3", "n4", "n5"), c(1, 3, 5:10)]
sub_matrix

# j
L <- diag(diag(M))
L

# k
L_add_row <- rbind(L, rep(0, ncol(L))) 
L_add_row

# l 
last_column <- M[, ncol(M), drop = FALSE]  
last_row <- M[nrow(M), , drop = FALSE] 
last_column
last_row

H <- last_column %*% last_row
H



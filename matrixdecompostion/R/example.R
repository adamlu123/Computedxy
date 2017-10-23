dt <- read.csv("Assignment1.csv")
beta <- get_lse(dt[1:150,1:5], dt[1:150,6],algorithm = 'Jacobi',rep=20)
beta_new <- get_lse(dt[,1:5], dt[,6],algorithm = 'Jacobi',rep=20)

updated_lse(dt[1:150,1:5],dt[151:200,1:5], dt[1:150,6],dt[151:200,6], beta )

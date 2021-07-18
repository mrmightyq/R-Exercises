procrustes <- function(A, B){
  # center and normalize A 
  A.centered <- t(scale(t(A), center = TRUE, scale = FALSE))
  A.size <- norm(A.centered, type = "F") / (ncol(A) * nrow(A))
  A.normalized <- A.centered / A.size
  
  # center and normalize B
  B.centered <- t(scale(t(B), center = TRUE, scale = FALSE))
  B.size <- norm(B.centered, type = "F") / (ncol(B) * nrow(B))
  B.normalized <- B.centered / B.size
  
  # Rotation matrix T 
  svd.results <- svd(B.normalized %*% t(A.normalized))
  U <- svd.results$u
  V <- svd.results$v
  T <- V %*% t(U)
  
  # B transformed
  B.transformed <- T %*% B.normalized
  
  # Error after superimposition
  RSS <- norm(A.normalized - B.transformed,  type = "F")
  
  # Return
  return(list(A.normalized = A.normalized, B.normalized = B.normalized, rotation.mtx = T, B.transformed = B.transformed, RSS = RSS))
}


# number of data points
k = 1000

# x
set.seed(7)
x <- rnorm(n = k, mean = 0, sd = 1)

# random noise
set.seed(9)
e <- rnorm(n = k, mean = 0, sd = 1)

# y
y <- abs(x) + e

# Matrix A
# Store x and y into a 2 x k matrix
A <- matrix(c(x, y), byrow = TRUE, nrow = 2)

# matrix B
# Arbitrarily rotate, scale, and translate A 
theta <- pi / 4
rot.mtx <- matrix(c(sin(theta), -cos(theta), cos(theta), sin(theta)), ncol=2)
B <- 0.5*rot.mtx %*% A - 6

procrustes.results <- procrustes(A, B)
A.df <- as_data_frame(t(A))
B.df <- as_data_frame(t(B))

A.normalized.df <- as_data_frame(t(procrustes.results$A.normalized))
B.transformed.df <- as_data_frame(t(procrustes.results$B.transformed))


data.df <- rbind(A.df, B.df, A.normalized.df, B.transformed.df)
colnames(data.df) <- c('x', 'y')
data.df$matrix <- rep(c('A', 'B', 'A', 'B'), each = k)
data.df$treatment <- rep(c('Original', 'Superimposed'), each = 2*k)


data.plot <- ggplot(data.df, aes(x = x, y = y, color = matrix)) +
  geom_point(aes(shape = matrix), size = 4, alpha = 0.5) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  facet_wrap(~treatment, scales = 'free') +
  labs(title = 'Procrustes analysis')

data.plot
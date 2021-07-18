setwd("C:/Users/KnudseQ/Desktop/R Desktop/Bee")
library(ggplot2)
library(png)
library(grid)
library(ggimage)
bees <- data.frame(distance = c(0.5, 1, 1.5, 2, 2.5, 3),
                   number = c(40, 34, 32, 22,18, 10))
img <- readPNG("comb.png")
bees$image <- "bee.png"


ggplot(data = bees, aes(x = distance, y = number)) +
  annotation_custom(rasterGrob(img, 
                               width = unit(1,"npc"),
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_image(aes(image = image), size = 0.15) +
  xlab("Distance (km)") +
  ylab("Number of Bees") +
  ylim(0, 45) +
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"))



sales <- data.frame(Sales= c(0.5, 1, 1.5, 2, 2.5, 3),
                   Time = c(4, 24, 36, 64,112, 210))
img2 <- readPNG("dc.png")
sales$image <- "scott.png"


ggplot(data = sales, aes(x = Sales, y = Time)) +
  annotation_custom(rasterGrob(img2, 
                               width = unit(1,"npc"),
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_image(aes(image = image), size = 0.10) +
  xlab("Time") +
  ylab("Sales") +
  ylim(0, 220) +
  labs(title="Scott Sales Projections", subtitle = "An unstoppable force.")+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"))


library(corrplot)
COR.SALES <- cor(sales)

# Using different color spectrum
col<- colorRampPalette(c("red", "white", "green"))(20)
corrplot(COR.SALES , type="upper", order="hclust", col=col)


library(RColorBrewer)

corrplot(COR.SALES, type="upper", order="hclust",
         col=brewer.pal(n=8, name="PuOr"))

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(sales)
head(p.mat[, 1:5])

corrplot(COR.SALES, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.05, insig = "blank")

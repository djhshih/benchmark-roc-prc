library(MASS)
library(stats)
library(ggplot2)
library(predtools)

set.seed(123456)

sim_data <- function(N, skew=0.5) {
	N1 <- round(N * skew);
	N0 <- N - N1;

	K <- 3;

	S <- matrix(
		c(1.0, 0.1, 0.3,
			0.1, 1.0, -0.2,
			0.3, -0.2, 1.0),
		nrow = K, byrow = TRUE
	);

	Sigma0 <- rWishart(1, K + 1, Sigma = S)[, , 1];
	mu0 <- c(-1, 0, 4);

	Sigma1 <- rWishart(1, K + 1, Sigma = S)[, , 1];
	mu1 <- c( 1, 0, 5);

	X0 <- mvrnorm(N0, mu0, Sigma0);
	X1 <- mvrnorm(N1, mu1, Sigma1);

	X <- rbind(X0, X1);
	y <- c(rep(0, N0), rep(1, N1));

	list(X = X, y = y, N0 = N0, N1 = N1, mu0 = mu0, mu1 = mu1, Sigma0 = Sigma0, Sigma1)
}

data1 <- sim_data(200000, 0.5);
data2 <- downsample_pos(data1, fraction=0.1);
data3 <- downsample_pos(data1, fraction=0.01);

skew1 <- prop.table(table(data1$y))[2];
skew2 <- prop.table(table(data2$y))[2];
skew3 <- prop.table(table(data3$y))[2];

train_model <- function(data) {
	d <- data.frame(y = data$y, data$X);
	mod <- glm(y ~ ., data = d, family="binomial");
}

predict_model <- function(mod, data, type="link") {
	d <- data.frame(y = data$y, data$X);
	predict.glm(mod, d, type=type)
}

mod1 <- train_model(data1);
summary(mod1)
score1 <- predict_model(mod1, data1);
prob1 <- predict_model(mod1, data1, type="response");

d.eval1 <- data.frame(
	score = score1,
	prob = prob1,
	y = data$y,
	skew = skew1
);

downsample_pos <- function(data, fraction) {
	N1 <- sum(data$y == 1);
	idx <- c(which(data$y == 0), sample(which(data$y == 1), round(N1*fraction)));
	data2 <- data;
	data2$X <- data2$X[idx, ];
	data2$y <- data2$y[idx];
	data2
}

mod2 <- train_model(data2);
summary(mod2)
score2 <- predict_model(mod2, data2);
prob2 <- predict_model(mod2, data2, type="response");

d.eval2 <- data.frame(
	score = score2,
	prob = prob2,
	y = data2$y,
	skew = round(skew2, digits=2)
);

mod3 <- train_model(data3);
summary(mod3)
score3 <- predict_model(mod3, data3);
prob3 <- predict_model(mod3, data3, type="response");


d.eval3 <- data.frame(
	score = score3,
	prob = prob3,
	y = data3$y,
	skew = round(skew3, digits=2)
);

d.eval <- rbind(d.eval1, d.eval2, d.eval3);

ggplot(d.eval, aes(x=score, fill=factor(y))) +
	theme_classic() +
	geom_density(alpha=0.5) +
	facet_grid(skew ~ .)

library(precrec)

eval1 <- evalmod(scores = d.eval1$score, labels = d.eval1$y);
auc(eval1)

eval2 <- evalmod(scores = d.eval2$score, labels = d.eval2$y);
auc(eval2)

eval3 <- evalmod(scores = d.eval3$score, labels = d.eval3$y);
auc(eval3)

plot(eval1)
plot(eval2)
plot(eval3)

calibration_plot(d.eval1, obs = "y", pred = "prob")
calibration_plot(d.eval2, obs = "y", pred = "prob")
calibration_plot(d.eval3, obs = "y", pred = "prob")


# SF2980 Project 1
# Part 2, Empirical ES

# load the library needed
require(MASS)

# ES function
ES <- function(X, R0 = 1, p = 0.05)
{
	n = length(X);
	L = -X/R0;
	Ln = sort(L, decreasing = TRUE);
	i = floor(n*p);
	ES = mean(Ln[1:i]) + (p - i/n)*Ln[i+1];
	return(ES);
}

# mean
mu = c(0, 0, 0);

# covariance matrix
U = as.matrix(data.frame(c(1, 0.8, 0.1),c(0.8, 1, 0.2),c(0.1, 0.2, 1)))
colnames(U) = NULL;

# sample size
N = 100000;

Z = mvrnorm(N, mu, U);

W = c(1, 1, 1)/3;
R1 = exp(0.03 + 0.01*Z[,1]);
R2 = exp(0.04 + 0.015*Z[,2]);
R3 = exp(0.05 + 0.020*Z[,3]);
R = as.matrix(data.frame(R1, R2, R3));

V = R%*%W;

# return
R0 = exp(0.03);
X = V - exp(0.03);

ES(X, R0);


# independent random variables

# sample size
N = 100000;
mu = c(0, 0, 0);
U = diag(1, nrow = 3);

Z = mvrnorm(N, mu, U);

W = c(1, 1, 1)/3;
R1 = exp(0.03 + 0.01*Z[,1]);
R2 = exp(0.04 + 0.015*Z[,2]);
R3 = exp(0.05 + 0.020*Z[,3]);
R = as.matrix(data.frame(R1, R2, R3));

V = R%*%W;

# return
R0 = exp(0.03);
X = V - exp(0.03);

ES(X, R0);




# SF2980 Project 1
# Part 2, Empirical ES

# load the library needed
require(MASS)

# ES function
ES <- function(X, R0 = 1, p = 0.05)
{
	n = length(X);
	L = -X/R0;
	Ln = sort(L, decreasing = TRUE);
	i = floor(n*p);
	ES = mean(Ln[1:i]) + (p - i/n)*Ln[i+1];
	return(ES);
}

# mean
mu = c(0, 0, 0);

# covariance matrix
U = as.matrix(data.frame(c(1, 0.8, 0.1),c(0.8, 1, 0.2),c(0.1, 0.2, 1)))
colnames(U) = NULL;

# sample size
N = 10000;

step = 0.01;
n = floor(1/step);
ESw = matrix(1, nrow = n, ncol = n);

w1 = 0;
while(w1 <= 1)
{
	w2 = 0;
	while(w2 <= 1 - w1)
	{
		w3 = 1 - w1 - w2;

		Z = mvrnorm(N, mu, U)

		W = c(w1, w2, w3);
		R1 = exp(0.03 + 0.01*Z[,1]);
		R2 = exp(0.04 + 0.015*Z[,2]);
		R3 = exp(0.05 + 0.020*Z[,3]);
		R = as.matrix(data.frame(R1, R2, R3));

		V = R%*%W;

		# return
		R0 = exp(0.03);
		X = V - exp(0.03);

		ESw[n*w1+1,n*w2+1] = ES(X, R0);
		w2 = w2 + step;
	}
	w1 = w1 + step;
}

index = which(ESw == min(ESw), arr.ind = TRUE)

w1 = (index[1] - 1)/n;
w2 = (index[2] - 1)/n;
w3 = 1 - w1 - w2;
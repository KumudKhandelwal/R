*********************************************************
Statistical Analysis of Massive and High Dimensional Data
*********************************************************
03-07-2020:


# Now let's try the learning model with 6 variables in complex model


X = as.data.frame(x + x^2 + x^3 + x^4 + x^5 + x^6)

train = sample(1:100, 75)

f6 = lm(y ~ ., data = X, subset = train)

# Predict for some data (here the learning data)
xVal = x[-train]
yhat = predict(f6, newdata=xVal)
ErrV = sum((y[-train] - yhat)^2) / length(yhat)
ErrV

## The overfitting effect

So, we are going to explore models ranging from very simple to very complex ones:


x = runif(100,0,10)



X = as.data.frame(x)
ErrL = ErrV = rep(NA,20)
for (c in 1:20){
	X = as.data.frame(X, x^c)
	f = lm(y ~ ., data = X)
	yhat = predict(f, newdata=X)
	ErrL[c] = sum((y - yhat)^2) / length(yhat)
	
	
	train = sample(1:100, 75)
	f2 = lm(y ~ ,, data = X, subset = train)
	xval = as.data.frame(X[-train])
	yhat = predict(f2, newdata = xval)
	ErrV[c] = sum((y[-train] - yhat)^2) / length(yhat)
}

plot(ErrL, type='b')
lines(ErrV, type='b', col='red')


# This will print on the console the best value for which the error is small
which.min(ErrL)
which.min(ErrV)

*************************
Classification Methods:
*************************

--------------------------
K-Nearest Neighbors (KNN):
--------------------------
K signifies the complexity of the model being optimized
For each x*,
-> find in X the k nearest neighbors of x* (we generally use Euclidean distance to find the neighbors)
-> look at the corresponding value of y and count the number of neighbors in each class
-> assign x* to the class which is the most represented(largest) amongst the neighbors

Pros and Cons:
+ Simple
- the complexity can be large if N is large
- the choice of K (usually thanks to CV) is sensitive
- We have to keep the learning data all the time (if it is a very big data set, then providing the data everytime to everyone working within the same company on some model individually would not be efficient in terms of storage)

* In R, use knn() in class package or knn3() in caret package

--------------------
Logistic Regression:
--------------------
-> It turn the classification method into a linear regression one with the help of logistic function
-> The logistic regression is based on the linear model for regression and the logistic function transforms the regression problem in a classification one.
-> Involves 2 steps: Learning step and Classification step
The learning step estimates beta hat from the data by maximum likelihood using an optimization technique over the learned data
The classification step computes the new probabaility

Pros and Cons:
+ very efficient for most of the times. Infact it is a reference method (after when you do some project, you have to test this mthod before goin into deep learning technique. First compare reference method, if it is giving good results, then go ahead with the other models. In deep learning, you get better results may be in 2 days whereas you get the gist of solution with logistic approach in 15 mins.. That is why it is recommended to use this technique before moving to machine learing/ deep learning).
+ like for lm(), you can interpret the coefficients to see which variables are useful
+ no tuning parameter is required (the threshold is fixed at 0.5)
- as for lm(), logistic regression is sensitive to high dimensions.

* In R, use glm() in the base package

---------------------------------------------------------------
Linear Discrimant Analysis (LDA): formalized by Fisher in 1936:
---------------------------------------------------------------
-> A generative classification method (as most of the "xxDA" methods)
-> Designed for multi-class classification : 2 or more classes
-> Like Logistic Regression, it is also a reference method.

KNN if single class
LR if binary(2) classes
LDA if binary(2) or more classes

-> The idea behind LDA is that each class can be modeled by a Gaussian distribution

2 Step learning procedure:
Step 1: Learn: Estimation of all parameters (mu1, mu2,... sigma, pi1, pi2, ...)
Step 2: Classification

Pros and Cons:
+ It has a model which is easy to understand
+ We have probabilities as output: Risk calculation becomes easy
+ It is also reference method: good upto 50 variables and works well for any dimension
+ No parameters to tune
- Does not perform well in very high dimension


#Implementation of KNN in R:

library(MBCbook)


N = nrow(wine27)
X = wine27[1,27]
Y = wine27$Type
train = sample(1:N, 150)

# use knn to classify
library(class)
out = knn(X[train], X[-train], Y[train], k=3)

# Error calculation
sum(out != Y[-train]) / length(out)



# Let's try now the LOO-CV to get a better result estimate of the error of KNN on these data:
ErrCV = rep(NA, N)
# Estimation of the error with LOO-CV
for (i in 1:nrow(X)){
	# Split between train and validation
	train = seq(1,nrow(X))[-i]
	# Learning step
	out = knn(X[train,], X[-train,], Y[train], k=3)

	# compute the error
	ErrCV[i] = sum(out != Y[-train]) / length(out)
}

mean(ErrCV)

We can observe that the 3-NN produces an avergae classification error around 20% which is not very satisfying. A way to improve it is to test other values of k and to use CV for pick the most appropriate k for those data.

ErrCV = matrix(NA,N,25)

for (k in 1:25){
for(i in 1:nrow(X)){
# Split between train and validation
	train = seq(1,nrow(X))[-i]
	# Learning step
	out = knn(X[train,], X[-train,], Y[train], k=k)

	# compute the error
	ErrCV[i, k] = sum(out != Y[-train]) / length(out)
}
}
plot(colMeans(ErrCV), type='b')
which.min(colMeans(ErrCV))
# boxplot(ErrCV)

It turns out that the best solution that we can have with KNN is with k=9.

=> Let us now use LDA to classify the same data.

N = nrow(wine27)
X = wine27
Y = win27$Type
train = sample(1:N,150)

# LDA learning step
f = lda(x[train,],Y[train])

# LDA prediction step
yhat = predict(clf, X[-train])

# Validation Error
sum(yhat != yhat$????) / length(yhat)
# sum(yhat$class != Y[-train]) / length(yhat$class)

> Note: the estimated classifier 'f' contains the maximum likelihood estimates for model parameters (f$prior = pi, f$means = mu, f$scaling = sigma)








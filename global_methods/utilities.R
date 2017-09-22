sumSquaredDist.code <-
"#include <Rcpp.h>

using namespace Rcpp;

double squaredDiff(NumericVector x, NumericVector y){
	double d = sum(pow(x - y, 2));
	return d;
}

// [[Rcpp::export]]
double calcSumSquaredDist(NumericMatrix x, NumericVector center){
	int out_length = x.nrow();

	double summed = 0.0;
	for (int i = 0 ; i < out_length; i++){
			NumericVector v1 = x.row(i);
			summed += squaredDiff(v1, center);
	}

	return summed;
}"

Rcpp::sourceCpp(code = sumSquaredDist.code)

euclideanDist.code <-
"#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double euclideanDist(NumericVector x, NumericVector y){
	double d = sqrt(sum(pow(x - y, 2)));
	return d;
}

// [[Rcpp::export]]
numericVector calcEuclideanDist(NumericMatrix x, NumericVector centroid){
	int out_length = x.nrow();
	NumericVector out(out_length);

	for (int i = 0 ; i < out_length; i++){
			NumericVector v1 = x.row(i);
			out[i] = euclideanDist(v1, centroid);
	}

	return out;
}"

Rcpp::sourceCpp(code = euclideanDist.code)

# Normalize data in the [0,1] range
normalize <- function(data) {
	invisible(data[, names(data) := lapply(.SD, as.numeric)])

	maxs <- as.numeric(data[, lapply(.SD, max)])
	mins <- as.numeric(data[, lapply(.SD, min)])

	data <- as.data.table(scale(data, center = mins, scale = maxs - mins))
	return(data)
}

# Get node's mean target values
prototype <- function(Y) {
	colMeans(Y)
}

variance <- function(Y) {
	sum(colVars(as.matrix(Y)))
}

homogeneity <- function(Y) {
	c.mean <- prototype(Y)
	calcSumSquaredDist(as.matrix(Y), c.mean)
}

predict <- function(model, new.data, parallel = FALSE) {
	switch(model$type,
		MTRT = {
			predictMTRT(model, new.data)
		},
		KCRT = {
			predictKCRT(model, new.data)
		},
		MORF = {

		},
		KCRTRF = {
			
		}
	)
}

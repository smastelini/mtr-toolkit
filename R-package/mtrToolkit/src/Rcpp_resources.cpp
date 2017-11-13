#include <Rcpp.h>
using namespace Rcpp;

double squaredDiff(NumericVector x, NumericVector y){
	double d = sum(pow(x - y, 2));
	return d;
}

// [[Rcpp::export]]
NumericVector prototype(NumericMatrix X) {
	// return Rcpp::colMeans(X);
	Rcpp::Environment base("package:base");
	Rcpp::Function col_means = base["colMeans"];
	Rcpp::NumericVector res = col_means(Rcpp::_["x"] = X);

	return res;
}

// [[Rcpp::export]]
double variance(NumericMatrix X) {
	if(X.nrow() <= 1 || X.ncol() == 0)
		return 0.0;

	double result = 0.0;
	int n_col = X.ncol();
	int n_row = X.nrow();

	NumericVector m = prototype(X);

	for(int j = 0; j < n_col; j++) {
		double mnv = m(j);
		double varS = 0.0;
		for(int i = 0; i < n_row; i++) {
			varS += pow(X(i,j) - mnv, 2);
		}

		result += varS/(n_row-1);
	}

	return result;
}

// [[Rcpp::export]]
double homogeneity(NumericMatrix X){
	if(X.nrow() == 0 && X.ncol() == 0)
		return 0.0;

	int n_rows = X.nrow();

	NumericVector center = prototype(X);

	double summed = 0.0;
	for (int i = 0 ; i < n_rows; i++){
			NumericVector v1 = X.row(i);
			summed += squaredDiff(v1, center);
	}

	return summed;
}

// [[Rcpp::export]]
double euclideanDist(NumericVector x, NumericVector y){
	double d = sqrt(sum(pow(x - y, 2)));
	return d;
}

// [[Rcpp::export]]
NumericVector calcEuclideanDist(NumericMatrix x, NumericVector centroid){
	int out_length = x.nrow();
	NumericVector out(out_length);

	for (int i = 0 ; i < out_length; i++){
			NumericVector v1 = x.row(i);
			out[i] = euclideanDist(v1, centroid);
	}

	return out;
}

// [[Rcpp::export]]
NumericVector col_vars(NumericMatrix X) {
	int n_col = X.ncol();
	int n_row = X.nrow();
	NumericVector result = no_init(n_col);

	NumericVector m = prototype(X);

	for(int j = 0; j < n_col; j++) {
		double mnv = m(j);
		double varS = 0.0;
		for(int i = 0; i < n_row; i++) {
			varS += pow(X(i,j) - mnv, 2);
		}

		result(j) = varS/(n_row-1);
	}

	return result;
}

std::list<int> whichRcpp(LogicalVector x) {
    std::list<int> idxs;
    for(int i = 0; i < x.size(); i++) {
        if(x[i] == TRUE) {
            idxs.push_back(i);
        }
    }

		return idxs;
}

NumericMatrix subset_matrix(NumericMatrix X, std::list<int> idxs) {
  int n_elem = idxs.size();
  if(n_elem == 0) {
    NumericMatrix emptyM(0,0);
    return emptyM;
  }

  NumericMatrix out = no_init(n_elem, X.ncol());

	int i = 0;
	for (std::list<int>::iterator it = idxs.begin(); it != idxs.end(); it++) {
    out(i,_) = X(*it,_);
		i++;
  }

  return out;
}

// [[Rcpp::export]]
List best_split(NumericVector attr, NumericMatrix Y, double actual_var, double actual_ss, double ftest_signf = 0.05) {
  NumericVector to_eval = sort_unique(attr);

  List retr;
  int n_row = Y.nrow();

  // None split could brings some improvement => Homogeneous data
  if(to_eval.length() == 1) {
    retr["split"] = NumericVector::get_na();
    retr["heur"] = 0;
    return retr;
  }

  double best_h = 0;
  double best_s = NumericVector::get_na();
  double split_p, h, var_p1, var_p2, sum_ss, f_test;
  std::list<int> part1, part2;

  NumericMatrix sub_part1, sub_part2;

  for(int p = 0; p < to_eval.length() - 1; p++) {

    split_p = to_eval[p];

    part1 = whichRcpp(attr <= split_p);
    part2 = whichRcpp(attr > split_p);

    sub_part1 = subset_matrix(Y, part1);
    sub_part2 = subset_matrix(Y, part2);

    var_p1 = variance(sub_part1);
    var_p2 = variance(sub_part2);

    double branch1 = var_p1 == 0.0 ? 0.0 : ((double) part1.size())/n_row*var_p1;
    double branch2 = var_p2 == 0.0 ? 0.0 : ((double) part2.size())/n_row*var_p2;

    // Heuristic calculation
    h = actual_var - (branch1 + branch2);

    if(h > best_h) {
      best_h = h;
      best_s = split_p;
    }
  }

  // Gains weren't observed
  if(best_h == 0) {
    retr["split"] = NumericVector::get_na();
    retr["heur"] = 0;
    return retr;
  }

  // Perform F Test once
  part1 = whichRcpp(attr <= best_s);
  part2 = whichRcpp(attr > best_s);

  sub_part1 = subset_matrix(Y, part1);
  sub_part2 = subset_matrix(Y, part2);

  sum_ss = homogeneity(sub_part1) + homogeneity(sub_part2);

  f_test = (n_row-2)*(actual_ss-sum_ss)/sum_ss;

  // It have passed the F-test
  if(f_test > R::qf(1 - ftest_signf, 1, n_row-2, true, false)) {
    retr["split"] = best_s;
    retr["heur"] = best_h;

  } else {// It didn't make through this
    retr["split"] = NumericVector::get_na();
    retr["heur"] = 0;
  }
  return retr;
}

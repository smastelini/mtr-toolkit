rm(list = ls())

set.seed(1973)

output.path <- "~/Desktop"
output.name <- "teste7"

n.targets <- 3
n.samples <- 500
n.features <- 30
n.gen.groups <- 1
perc.samples.noise <- 0.05

mtr.dataset <- TRUE
TARGETS.DEPS <- new.env()
TARGETS.DEPS$forms <- list()

# Correlation formulas
TARGETS.DEPS$forms[["Y1"]] <- function() sin(0.3*Y2*Y3) - runif(1, min = -5, max = 5)*(Y3-Y1)
TARGETS.DEPS$forms[["Y2"]] <- function() runif(1, min = -2, max = 2)*Y2^2 - runif(1, min = -5, max = 5)*Y3
TARGETS.DEPS$forms[["Y3"]] <- function() cos(0.01*Y1)*exp(0.05*Y2) - 3*Y3^3

dataset <- matrix(nrow=n.samples, ncol=n.features + n.targets, data=NA)
t.indexes <- seq(n.targets) + n.features

colnames(dataset) <- c(paste0("X", seq(n.features)), paste0("Y", seq(n.targets)))

# Features generation
for(i in seq(n.samples)) {
	dataset[i, -t.indexes] <- runif(n.features, min = -1, max = 1)
}

perc.size.groups <- sample(10, n.gen.groups)
perc.size.groups <- perc.size.groups/sum(perc.size.groups)

lim.sup.gen.groups <- round(n.samples*perc.size.groups)
lim.sup.gen.groups <- cumsum(lim.sup.gen.groups)
lim.sup.gen.groups[n.gen.groups] <- n.samples

lim.inf.gen.groups <- rep(1, n.gen.groups)
if(n.gen.groups > 1) {
	for(k in seq(2,n.gen.groups))
		lim.inf.gen.groups[k] <- lim.sup.gen.groups[k-1] + 1	
}

len.gen.groups <- round(n.samples/n.gen.groups)
for(k in seq(n.gen.groups)) {
	lim.inf <- lim.inf.gen.groups[k]
	lim.sup <- lim.sup.gen.groups[k]
	
	# Weights to generate Y
	x2y.w <- list()
	for(j in seq(n.targets)) {
		# aux <- rnorm(n.features, mean = 0,
		# 						 sd = runif(1, min = 0.05, max = 5))
		n.rel <- round(runif(1, 3, round(0.5*n.features)))
		f.rel <- sample(n.features, n.rel)
		
		aux <- rep(0, n.features)
		aux[f.rel] <- rnorm(n.rel, mean = 0, 
												sd = runif(1, min = 0.05, max = 5))
		aux[-f.rel] <- runif(n.features - n.rel, min = -0.01, max = 0.01)

		x2y.w[[j]] <- aux
	}
	
	# Generate simple linear relation: X -> Y
	for(j in seq(n.targets)) {
		dataset[lim.inf:lim.sup,t.indexes[j]] <- as.numeric(dataset[lim.inf:lim.sup, -t.indexes] %*% x2y.w[[j]])
	}
}

noisy.samples <- sample(n.samples)[1:round(n.samples*perc.samples.noise)]

# Noise in X
dataset[noisy.samples, -t.indexes] <- apply(dataset[noisy.samples, -t.indexes], 1, function(l, n.f) {
	noise <- rep(0, n.f)
	n.degraded <- sample(n.f, 1)
	degraded <- sample(n.f, n.degraded)
	noise[degraded] <- runif(n.degraded, min = -0.01, max = 0.01)
	l <- l + noise
	l
}, n.f = n.features)

# Add dependencies among the targets
if(mtr.dataset) {
	for(t in seq(n.targets)) {
		assign(paste0("Y", t), dataset[, t.indexes[t]], envir = TARGETS.DEPS)
	}
	
	for(t in seq(n.targets)) {
		if(length(TARGETS.DEPS$forms[[paste0("Y", t)]]) > 0) {
			environment(TARGETS.DEPS$forms[[paste0("Y", t)]]) <- TARGETS.DEPS
			dataset[, t.indexes[t]] <- TARGETS.DEPS$forms[[paste0("Y", t)]]()
		}
	}
	rm(TARGETS.DEPS)
}

# Normalize values between [0,1]
max <- apply(dataset, 2, max)
min <- apply(dataset, 2, min)

dataset <- scale(dataset, center = min, scale = max - min)

dataset <- data.frame(dataset)
dataset <- dataset[sample(nrow(dataset)),]
write.csv(dataset, paste0(output.path, "/", output.name, ".csv"), row.names = FALSE)
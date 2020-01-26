# R functions


# location 
	# mean(average, expectation)
	mymean <- function(x) 
	{
		return (sum(x) / length(x))
	}

	# median
	mymedian <- function(x) {
		sorted <- sort(x)
		len <- length(x)

		if (len %% 2 == 0) {
			return (mean(sorted[len/2 + c(0, 1)]))
		}
		else {
			return (sorted[len / 2])
		}
	}

	#mode
	mymode <- function(x) {
		t <- table(x)
		return (names(t)[t == max(t)])
	}

	mymode2 <- function(x) {
		uniqx <- unique(x)
		uniqx[which.max(tabulate(match(x, uniqx)))]
	}

	# variance
	myvariation <- function(x) {
		x_mean = sum(x) / length(x)
    	sum((x - x_mean)^2) / (length(x) - 1)
	}

	# standard deviation
	mysd <- function(x) {
		sqrt(myvariation(x))
	}

	# mad (median absolute difference)
	mymad <- function(x) {
		x_median = mymedian(x)

		mymedian(abs(x - x_median)) * 1.4826
	}

	myiqr <- function(x) {
		quantile(x, 0.75) - quantile(x, 0.25)
	}


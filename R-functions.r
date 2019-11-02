# R functions


# location 
	# mean(average, expectation)
	meanFunction <- function(x) 
	{
		sum(x) / length(x)
	}

	# median
	medianFunction <- function(x) 
	{
		sorted <- sort(x)
		len <- length(x)

		if (len %% 2 != 0)
		{
			return sorted[round(len / 2)]
		}
		else
		{
			return sorted[len / 2 + c(0, 1)]
		}
	}

	#mode
	modeFunction <- function(x) 
	{
		t <- table(x)
		return (names(t)[t == max(t)])
	}
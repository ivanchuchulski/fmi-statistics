	# task 1
	get_prices_F <- function(companyName) 
	{
		prices <- as.data.frame(getSymbols(companyName, auto.assign = FALSE, from = "2019-01-01", to = "2019-10-01"))
		prices <- data.frame(date = row.names(prices), prices[, 6])

		prices$date <- as.character(prices$date)
		names(prices)[2] <- companyName

		prices
	}

	# sample output
	# head(getSymbols("MSFT", auto.assign = FALSE, from = "2019-01-01", to = "2019-10-01"))

	# ??
	# prices <- lapply(companies, FUN=get_prices_F)

	companies <- c("MSFT", "FT", "APPL")
	prices_DF <- get_prices_F("APPL")

	x1 <- prices_DF$MSFT[-1]
	xN <- prices_DF$MSFT[-length(prices_DF)]

	x_log <- log(x1 / xN)

	# to check if data is normal distribution
	hist(x_log, col="red", main="Histogram", xlab="Apple", ylab="Freq", breaks=20)
	qqnorm(x_log)
	qqline(x_log)

	#if it is
	# location(center) <- mean()
	# expectation(variance) <- sd()

	# if it's not
	# location(center) <- median
	# expectation(variance) <- mad(mead absolute deviation)
	median(x_log)
	mad(x_log)
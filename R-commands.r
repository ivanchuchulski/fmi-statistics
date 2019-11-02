R commands

# setting names to a vector
test_scores <- setNames(c(87, 82, 99), c("Alice", "Bob", "Shirley"))

# ignore the NA values
sum(hip_cost, na.rm = TRUE)

# scan user input or text
input <- scan("input.txt") 

# generate numbers
seq(1, 100, by = 1)
seq(1, 100, length.out = 11)) # specify the sequence length

# for looping in a vector 
seq_along(vectorName)

# repeating numbers
rep(5, times = 10)
rep(1:5, times = 10)
rep(c(1, 2, 3), times = c(3, 2, 1)) # or rep(c(1, 2, 3), 3:1) 

# get indices in a named vector
match(c("Alice", "Bob"), names(testScores))

# "recycling"
n <- 1? # get 1? elements of x
x[1 + ?:(n-1) %% length(x)] # use remainder for indices
## [1] 1 2 3 1 2 3 1 2 3 1

sprintf("X%s", 1:1?)
## [1] "X1" "X2" "X3" "X4" "X5" "X6" "X7" "X8" "X9" "X1?"

sprintf("%8d", c(1, 12, 123, 1234, 12345))
## [1] " 1" " 12" " 123" " 1234" " 12345"

paste("X", 1:1?, sep="")
## [1] "X1" "X2" "X3" "X4" "X5" "X6" "X7" "X8" "X9" "X1?"

paste("The", "quick", "brown", "fox", "...", sep="_")
## [1] "The_quick_brown_fox_..."

# near equality
isTRUE(all.equal(sqrt(2) * sqrt(2), 2))

sum(whale > 200)

whale <- c(74, 122, 235, 111, 292, 111, 211, 133, 156, 79)
whale[whale > mean(whale)]
## [1] 235 292 211 156

whale[whale < mean(whale)-sd(whale) | whale > mean(whale)+sd(whale)]
## [1] 74 235 292 79

# to remove NA values
hip_cost_without_na <- hip_cost[!is.na(hip_cost)]

# or inplace with ??
#range(x, na.rm=TRUE)




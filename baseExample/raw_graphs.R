number_of_bins <- 10 # ultimately an input from user
x <- faithful[, 2]
bins <- seq(min(x), max(x), length.out = number_of_bins + 1)

# draw the histogram with the specified number of bins
hist(x, breaks = bins, col = 'darkgray', border = 'white')
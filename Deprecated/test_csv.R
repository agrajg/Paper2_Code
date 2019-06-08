# Start writing to an output file
sink('analysis-output.txt')

set.seed(12345)
x <-rnorm(10,10,1)
y <-rnorm(10,11,1)
# Do some stuff here
cat(sprintf("x has %d elements:\n", length(x)))
print(x)
cat("y =", y, "\n")

cat("=============================\n")
cat("T-test between x and y\n")
cat("=============================\n")
t.test(x,y)

# Stop writing to the file
sink()

print(x)
# Append to the file
sink('analysis-output.txt', append=FALSE)
cat("Some more stuff here...\n")
sink()

corr <- function(directory, threshold = 0) {
        corr_vector <- numeric()
        files <- list.files(directory, full.names=TRUE)
        j <- 1
        for (i in seq_along(files)) {
                current_file <- read.csv(files[i])
                sum_cases <- sum(complete.cases(current_file))
                nit <- current_file[ ,2]
                sulf <- current_file[ ,3]
                
                if (sum_cases > threshold) {
                    corr_vector[j] <- cor(nit, sulf, use="complete.obs")  
                    j <- j + 1
                }
        }

        print(corr_vector)
}
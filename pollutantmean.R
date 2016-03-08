pollutantmean <- function(directory, pollutant, id = 1:332) {
        mean_vector <- numeric()
        j <- 1
        for (i in seq_along(id)) {
                if (id[i] < 10){
                        file_id <- paste0("00",id[i])
                }
                if (id[i] >= 10 & id[i] < 100) {
                        file_id <- paste0("0",id[i])
                } 
                if (id[i] >= 100) {
                        file_id <- id[i]
                }
                file_to_read <- read.csv(paste0(directory,"/",file_id,".csv"))
               
                bad <- is.na(file_to_read[ , pollutant])
                clean_vector <- file_to_read[!bad, pollutant]
                end <- j+length(clean_vector)-1
                mean_vector[j:end] <- clean_vector
                j=j+length(clean_vector)
        }
        print(mean(mean_vector))
}
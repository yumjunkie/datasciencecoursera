complete <- function(directory, id = 1:332) {
        complete_matrix <- matrix(nrow=length(id),ncol=2)
 
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
                
                sum_cases <- sum(complete.cases(file_to_read))
                complete_matrix[i,1] <- id[i]
                complete_matrix[i,2] <- sum_cases
                
        }
        
        name <- c("id","nobs")
        colnames(complete_matrix) <- name
        print(complete_matrix)
}
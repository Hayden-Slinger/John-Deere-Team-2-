rm(list = ls())

data <- read.csv("ui_capstone_2024_sample.csv")

#Making NA Values into the data
data[data == ""] <- NA

colnames(data)



#doing the same but for the PassFail_6
data$PassFail_5 <- ifelse(is.na(data$PassFail_5) & !is.na(data$PassFail_6), data$PassFail_6, data$PassFail_5)

#deleting left over PassFail_6
data$PassFail_6 <- ifelse(!is.na(data$PassFail_5), NA, data$PassFail_6)

#get rid of data columns with no values
data <- data[, colSums(is.na(data)) <nrow(data)]

#Grabs all the columns we want into a new datbase called final_output
#1

final_output <- data.frame(data$uut_result)

#2
final_output$start_date_time <-data $start_date_time

#3 keep batch serial number
final_output$batch_serial_number <-data$batch_serial_number

#4 keep station ID
final_output$station_id <-data$station_id

#5 Keep/create utt_status: passed/fail
final_output$uut_status<- data$uut_status

#6 Keep utt_serial_number
final_output$utt_serial_mask<- data$uut_serial_mask

#7 keep product
final_output$product_line<-data$product_line

#8 Keep path_name_1 – path_name_9
final_output$path_name_1<- data$path_name_1
final_output$path_name_2<- data$path_name_2
final_output$path_name_3<- data$path_name_3
final_output$path_name_4<- data$path_name_4
final_output$path_name_5<- data$path_name_5
final_output$path_name_6<- data$path_name_6
final_output$path_name_7<- data$path_name_7
final_output$path_name_8<- data$path_name_8
final_output$path_name_9<- data$path_name_9

#9 compute step_order
#tbd

#10 compute sub_sq_name
final_output$sub_seq_name <- #ifelse(grepl("\\.seq$", final_output$path_name_9), final_output$path_name_9,
                                   # ifelse(grepl("\\.seq$", final_output$path_name_8), final_output$path_name_8,
                                           #ifelse(grepl("\\.seq$", final_output$path_name_7), final_output$path_name_7,
                                                  ifelse(grepl("\\.seq$", final_output$path_name_6), final_output$path_name_6,
                                                         ifelse(grepl("\\.seq$", final_output$path_name_5), final_output$path_name_5,
                                                                ifelse(grepl("\\.seq$", final_output$path_name_4), final_output$path_name_4,
                                                                       ifelse(grepl("\\.seq$", final_output$path_name_3), final_output$path_name_3,
                                                                              ifelse(grepl("\\.seq$", final_output$path_name_2), final_output$path_name_2,
                                                                                     ifelse(grepl("\\.seq$", final_output$path_name_1), final_output$path_name_1, NA))))))
#)))

#11 compute test step name|| not needed
#12 compute step_created_date_time|| not needed

#13 identify step type|| not needed

#14 status: passed/fail
final_output$status<-data$status_6 
final_output$status<-ifelse(is.na(final_output$status), data$status_5, final_output$status) 



#15 max/ min / result
final_output$max_value = data$max_value_5
final_output$min_value = data$min_value_5
final_output$result_value = data$returned_value_5

#16 pass/fail column

final_output$passfail <- ifelse(final_output$result_value < final_output$max_value & final_output$result_value > final_output$min_value, 1, 0)

# Remove rows with NA in min_value, max_value, or result_value columns
final_output2 <- final_output[!is.na(final_output$min_value) & !is.na(final_output$max_value) & !is.na(final_output$result_value), ]
final_output2 <- subset(final_output2, result_value >= 0)
final_output2 <- subset(final_output2, result_value > 0)

# Load the dplyr package for data manipulation
library(dplyr)

#Creating Unique Path by joining Path_name 1-5 
final_output2$path <- paste(final_output2$path_name_1,final_output2$path_name_2, final_output2$path_name_3, 
                            final_output2$path_name_4, final_output2$path_name_5, sep="-")

final_output2<-subset(final_output2, select = -c(path_name_1,path_name_2, path_name_3, 
                                                path_name_4, path_name_5,path_name_6))

# Group the data by path
grouped_data <- group_by(final_output2, path)


#drop paths with less than 5 occurances 
final_output3 <- final_output2 %>%
  group_by(path) %>%
  filter(n()>5)
table(final_output3$path)
# Group the data by path_name_5
grouped_data <- group_by(final_output3, path)




# calculates IQR and creates column
IQR<-final_output3 %>% group_by(path) %>%
  summarize(first=quantile(result_value,probs=0.25),
            second=quantile(result_value,probs=0.5),
            third=quantile(result_value,probs=0.75))
final_output4 <- left_join(final_output3, IQR, by = "path")
final_output4$IQR<- final_output4$third-final_output4$first


#drop values that are outside the IQR 
final_output5<-subset(final_output4, result_value>(first-(1.5*IQR)) | result_value<(third+(1.5*IQR)))

#New PPK Values
grouped_data2 <- group_by(final_output5, path)
ppk_values2 <- summarise(grouped_data2, ppk = {
  mean_data <- mean(result_value, na.rm = TRUE)
  std_dev <- sd(result_value, na.rm = TRUE)
  USL <- max_value  # Upper Specification Limit for each group
  LSL <- min_value  # Lower Specification Limit for each group
  ppk <- min(((USL - mean_data) / (3*std_dev)), ((mean_data - LSL) / (3*std_dev)))})

final_output6<- left_join(final_output5, ppk_values2, by = "path")
write.csv(final_output5, file = "final_output_4.csv", row.names = FALSE)

#Break up by week
final_output6$week_num <- strftime(final_output6$start_date_time, format = "%V")

#CPK By Week 
cpk<-final_output6 %>%
  group_by(path, week_num) %>%
  summarise(cpk = {
  mean_data <- mean(result_value, na.rm = TRUE)
  std_dev <- sd(result_value, na.rm = TRUE)
  USL <- max_value  # Upper Specification Limit for each group
  LSL <- min_value  # Lower Specification Limit for each group
  cpk <- min(((USL - mean_data) / (3*std_dev)), ((mean_data - LSL) / (3*std_dev)))
})

#saves final ouput and creates CSV
final_output7<- left_join(final_output6, cpk, join_by(path, week_num))
write.csv(final_output7, file = "final_output_7.csv", row.names = FALSE)




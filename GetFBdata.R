#####################################
#	GetFBdata					              	#
#	Scarpes and retreives Fitbit    	#
#	data based on login credentials	  #
#	dmaslove (2015)     		        	#
#####################################

# This uses functions from the 'fitbitScraper' package for R
# by corynissen (https://github.com/corynissen/fitbitScraper)

library(devtools)
source_gist('https://gist.github.com/DMconduit/11980df9fd663cbd14eb')

args<-commandArgs(TRUE)
# Rscript myscript.R arg1 arg2 arg3

fb_name <- args[[1]]
fb_start_date <- args[[2]]
# add one day to start date to get the end date
fb_end_date <- strsplit(as.character(as.POSIXct(fb_start_date, format='%Y-%m-%d') + 60*60*24), ' ')[[1]]
fb_start_time <- as.POSIXct(paste(args[[2]], args[[3]]), format='%Y-%m-%d %H:%M')
# add 24 hours to the start time to get end time
fb_end_time <- fb_start_time + 60*60*24
patient_id <- args[[4]]


# Create the appropriate cookie
if (fb_name == 'Shirley') {
	cookie <- login(email="shirley@conduitlab.org", password="wearitin1")
} else if (fb_name == 'Eunice') {
	cookie <- login(email="eunice@conduitlab.org", password="wearitin2")	
} else if (fb_name == 'Gertrude') {
	cookie <- login(email="gertrude@conduitlab.org", password="wearitin3")
} else if (fb_name == 'Stewart') {
	cookie <- login(email="stewart@conduitlab.org", password="wearitin4")
} else if (fb_name == 'George') {
	cookie <- login(email="george@conduitlab.org", password="wearitin5")
} else if (fb_name == 'test_one') {
	cookie <- login(email="test_one@conduitlab.org", password="Conduit1")
}

print('Collecting data...')

# HR data for the first day
hr_1 <- get_intraday_data(cookie, what='heart-rate', date=fb_start_date)
# HR data for the second day
hr_2 <- get_intraday_data(cookie, what='heart-rate', date=fb_end_date)
# bind these
hr_data <- rbind(hr_1, hr_2)
# select the entries between start and end times
# This requires we find the time stamp in the data frame that is closest to fb_start_time and fb_end_time
closest_start <- which(abs(hr_data$time-fb_start_time)==min(abs(hr_data$time-fb_start_time)))
closest_end <- which(abs(hr_data$time-fb_end_time)==min(abs(hr_data$time-fb_end_time)))
hr_output <- hr_data[closest_start:closest_end,]
# hr output file name
hr_out_file <- paste(patient_id, '_hr_', fb_start_date, '.csv', sep='')

# steps data is like HR data
steps_1 <- get_intraday_data(cookie, what = "steps", fb_start_date)
steps_2 <- get_intraday_data(cookie, what = "steps", fb_end_date)
steps_data <- rbind(steps_1, steps_2)
closest_start <- which(abs(steps_data$time-fb_start_time)==min(abs(steps_data$time-fb_start_time)))
closest_end <- which(abs(steps_data$time-fb_end_time)==min(abs(steps_data$time-fb_end_time)))
steps_output <- steps_data[closest_start:closest_end,]

# steps_output <- steps_data[which(steps_data$time == fb_start_time):which(steps_data$time == fb_end_time),]
steps_out_file <- paste(patient_id, '_steps_', fb_start_date, '.csv', sep='')



# sleep data
sleep <- get_sleep_data(cookie, start_date = fb_start_date, end_date = fb_end_date)
# summary table (one row)
sleep_summary <- data.frame(sleep$summary)
# data table 
sleep_table <- apply(sleep$df, 1, unlist)
# output file names
sleep_summary_out <- paste(patient_id, '_sleep_summary_', fb_start_date, '.csv', sep='')
sleep_data_out <- paste(patient_id, '_sleep_data_', fb_start_date, '.csv', sep='')


print('Writing results...')

# Export results
setwd("/Users/dmaslove/Dropbox/PROJECTS/Fitbit/fb_output")
write.csv(hr_output, file=hr_out_file, row.names=FALSE)
write.csv(sleep_summary, file=sleep_summary_out, row.names=FALSE)
write.csv(sleep_table, file=sleep_data_out, row.names=TRUE)
write.csv(steps_output, file=steps_out_file, row.names=TRUE)


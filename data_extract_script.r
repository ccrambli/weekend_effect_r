library(stringi)

stock_files = list.files(pattern="*.csv")

for (stock_file in stock_files) {
  stock_data <- read.csv(stock_file,stringsAsFactors=FALSE);

  # convert date field to Y-m-d, and add day of week into data frame
  stock_data$Date = as.Date(stock_data$Date, format="%d-%b-%y");
  stock_data$DOW = weekdays(as.Date(stock_data$Date,"%d-%b-%y"));

  # reverse the order so we have data going from jan to dec, instead of dec to jan
  stock_data = stock_data[order(as.Date(stock_data$Date, format="%Y-%m-%d")),]

  # we have no log return for the first date, since we do not know prior day, so add in 0 value
  stock_data$log_returns = c(0,diff(log(stock_data$Close)))

  # remove first day since we have no return data
  stock_data = stock_data[-1,]

  # only get data for Fridays or Monday
  # TODO - make this fancier and only get Friday/Monday pair differences
  # The weekend effect (also known as the Monday effect, the day-of-the-week effect or the Monday seasonal) 
  # refers to the tendency of stocks to exhibit relatively large returns on Fridays compared to those on Mondays.
  friday_data = subset(stock_data,DOW=='Friday')
  monday_data = subset(stock_data,DOW=='Monday')

  # make new file names
  friday_filename = paste(str_extract(stock_file, '.*(?=\\.csv)'),"_friday.csv", sep="")
  monday_filename = paste(str_extract(stock_file, '.*(?=\\.csv)'),"_monday.csv", sep="")

  # write our new data to individual csvs
  write.csv(friday_data, file = friday_filename,row.names=FALSE)
  write.csv(monday_data, file = monday_filename,row.names=FALSE)
}
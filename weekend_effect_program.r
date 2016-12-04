# use for multi histograms on the same panel
cat("Welcome to Corey's project for 4150. \n\nWe will analyze stock data for Mondays and Fridays for various stocks \nin order to see if we can reject or fail to reject \nthe possibility of a weekend effect.")
cat("\n\nWhat is the weekend effect?")
cat("\n\nThe weekend effect refers to the notion that stock returns on Monday\nare significantly less than the returns on Friday.\n\n")

# Hard coded for now, eventually generalize to all stocks in work directory
stocks = c("alk", "arlp", "bac", "cvs",
                     "cnsl", "cvx", "cvs","gel", "hd",
                     "jpm", "t", "tgt", "unp", "ups",
                     "vz", "wltw","all_stocks")

# main operating menu - option for info, multi stock, exit, and multi stock mode
mainMenu <- function () {
  stock.current = "";
  while (stock.current != 'e') {
    stock.current <- readline(prompt="Enter a stock symbol to view or type 'multi' to analyze two stocks at the same time\nSymbols are alk, arlp, bac, cvs, cnsl, cvx, gel, hd, jpm, t, tgt, unp, ups, vz, wltw, or all_stocks to consolidate all data.\nType i for more info, or e to exit: ")
    stock.current = tolower(stock.current);
    switch(stock.current,
      i={
        infoMenu();
      },
      multi={
        multiStockMenu();
      },
      e={
        #do nothing here but we will exit loop
      },
      # in the default case, make sure we match a stock, otherwise show error message
      {
        if (is.element(stock.current,stocks)) {
          singleStockSubMenu(stock.current);
        } else {
          menuError();
        }
      }
    )
  }
  cat("\nGoodbye.")
}

infoMenu <- function() {
  cat ("Stocks analyzed in this program come from five different sectors. Sectors and stocks included are\n")
  cat("\nTransportation: \n\tUPS - United Parcel Service, Inc.\n\tALK - Alaskan Trans\n\tUNP - Union Pacific Corporation")
  cat("\nRetail:\n\tTGT - Target Corporation\n\tCVS - CVS Health Corp\n\tHD  - Home Depot Inc")
  cat("\nEnergy:\n\tGEL - Genesis Energy, L.P. common stock\n\tCVX - Chevron Corporation\n\tARLP - Alliance Resource Partners, L.P.")
  cat("\nFinancials:\n\tJPM - JPMorgan Chase & Co.\n\tBAC - Bank of America Corp\n\tWLTW - Willis Towers Watson PLC")
  cat("\nCommunication:\n\tVZ - Verizon Communications Inc.\n\tCNSL - Consolidated Communications Holdings Inc\n\tT - AT&T Inc\n")
  cat("\nHisorical Data was captured using google finance API and data used is from Jan 1 2011 to Jan 1 2016.\n\n")
}

menuError <- function() {
  cat("\nOption is invalid, please try again or type e to exit.\n")
}

singleStockSubMenu <- function(stock) {
  singleStockOption = "";
  #load in our csv
  if (stock != "all_stocks") {
    monday_data = read.csv(paste(stock,"_5_year_monday.csv",sep=""))
    friday_data = read.csv(paste(stock,"_5_year_friday.csv",sep=""))
  } else {
    monday_stock_files = list.files(pattern="*monday.csv")
    friday_stock_files = list.files(pattern="*friday.csv")
    monday_data =  do.call(rbind,lapply(monday_stock_files,read.csv))
    friday_data =  do.call(rbind,lapply(friday_stock_files,read.csv))
  }

  while (singleStockOption != 'e') {
    #options for a single stock (or all together)
    #option 1. display histogram for data
    #option 2. display a normal probability plot for data
    #option 3. create approximate confidence interval for mean given a confidence level
    #option 4. create approximate confidence interval for variance given a confidence level
    #option 5. draw regression
    cat(paste("\nPlease select from the following options for stock ",stock,", or press e to exit."))
    singleStockOption <- readline(prompt=paste("
       \t1. View Histograms
       \t2. View normal probability plots
       \t3. CI for mean
       \t4. CI for variance
       \t5. Regression of Log Returns on Time
       \t6. Test Monday Mean <= Friday Mean \n",sep=""));

    #Since we will use these variables down below, calculate now to reduce workload
    # calculate the means
    friday_sample_mean = mean(friday_data$log_returns)
    monday_sample_mean = mean(monday_data$log_returns)

    #this can be safely used to calculate the sample variance.
    friday_sample_sd = sd(friday_data$log_returns)
    monday_sample_sd = sd(monday_data$log_returns)

    #get the number of results in our data
    friday_sample_results = length(friday_data$log_returns)
    monday_sample_results = length(monday_data$log_returns)

    switch(singleStockOption,
      '1'={
        #for now,
        par(mfrow = c(2,1))
        hist(monday_data$log_returns,main=paste("Monday Log Returns for",stock),xlab="Log Return", right=FALSE,breaks=20)
        hist(friday_data$log_returns,main=paste("Friday Log Returns for",stock),xlab="Log Return",right=FALSE,breaks=20)
      },
      '2'={
        par(mfrow = c(2,1))
        qqplot(monday_data$Date, monday_data$log_returns, ylab=paste("Monday Log Returns for",stock), xlab="Date")
        qqplot(friday_data$Date, friday_data$log_returns, ylab=paste("Friday Log Returns for",stock), xlab="Date")
      },
      '3'={
        valid_input = FALSE;
        while (valid_input == FALSE) {
          confidence_level = as.numeric(readline(prompt="Please select a level of confidence between 0 and 1\n"))
          if (confidence_level > 0 & confidence_level < 1) {
            valid_input = TRUE;
            #compute right and left confidence interval in this scenario
            alpha_level = (1-confidence_level)

            friday_error = abs(qt(alpha_level/2,df=friday_sample_results-1)*friday_sample_sd/sqrt(friday_sample_results))
            friday_left_interval = -friday_error + friday_sample_mean;
            friday_right_interval = friday_error + friday_sample_mean;

            monday_error = abs(qt(alpha_level/2,df=monday_sample_results-1)*monday_sample_sd/sqrt(monday_sample_results))
            monday_left_interval = -monday_error +monday_sample_mean
            monday_right_interval = monday_error + monday_sample_mean

            # display results
            cat(paste(confidence_level*100,"% confidence interval for μ for Friday: [", format(friday_left_interval,digits=5),", ", format(friday_right_interval,digits=5), "]\n", sep=""))
            cat(paste(confidence_level*100,"% confidence interval for μ for Monday: [", format(monday_left_interval,digits=5),", ", format(monday_right_interval,digits=5), "]\n", sep=""))
          }
        }
      },
      '4'={
        valid_input = FALSE;
        while (valid_input == FALSE) {
          confidence_level = as.numeric(readline(prompt="Please select a level of confidence between 0 and 1\n"))
          if (confidence_level > 0 & confidence_level < 1) {
            valid_input = TRUE;
            alpha_level = (1-confidence_level)

            #compute right and left confidence interval in this scenario
            friday_right_interval = sqrt((friday_sample_results-1)*friday_sample_sd^2/qchisq((1-alpha_level)/2,df=friday_sample_results-1));
            friday_left_interval = sqrt((friday_sample_results-1)*friday_sample_sd^2/qchisq(1-(1-alpha_level)/2,df=friday_sample_results-1));

            monday_right_interval = sqrt((monday_sample_results-1)*monday_sample_sd^2/qchisq((1-alpha_level)/2,df=monday_sample_results-1));
            monday_left_interval = sqrt((monday_sample_results-1)*monday_sample_sd^2/qchisq(1-(1-alpha_level)/2,df=monday_sample_results-1));

            # display results
            cat(paste(confidence_level*100,"% confidence interval for σ for Friday: [", format(friday_left_interval,digits=5),", ", format(friday_right_interval,digits=5), "]\n", sep=""))
            cat(paste(confidence_level*100,"% confidence interval for σ for Monday: [", format(monday_left_interval,digits=5),", ", format(monday_right_interval,digits=5), "]\n", sep=""))
          }
        }
      },
      '5'={#seq.int(nrow(data))
        par(mfrow = c(2,1))
        #since we can' regress on an actual date
        
        plot(monday_data$log_returns ~ seq.int(length(monday_data$Date)),bty="l",pch=20,main="Monday Log Returns", xlab="Time value",ylab="Return")
        monday_regression<-lm(monday_data$log_returns ~ seq.int(length(monday_data$Date)))
        abline(monday_regression, lty=1, lwd=2)
        monday_coefficients = round(coef(monday_regression), 5) 
        monday_r_squared = format(summary(monday_regression)$r.squared, digits = 5)

        monday_equation = paste0("Monday ",stock, " = ", monday_coefficients[1],
                     ifelse(sign(monday_coefficients[2])==1, " + ", " - "), abs(monday_coefficients[2])," Time From 0 ","\nR^2 = ",monday_r_squared)
        ## printing of the equation
        mtext(monday_equation, 3, line=-1)
        pauseProgram()
        plot(fitted(monday_regression),resid(monday_regression),main="Monday Residuals",ylab="Residual",xlab="Time")
        pauseProgram()
        plot(friday_data$log_returns ~ seq.int(length(friday_data$Date)),bty="l",pch=20,main="Friday Log Returns", xlab="Time value",ylab="Return")
        friday_regression<-lm(friday_data$log_returns ~ seq.int(length(friday_data$Date)))
        abline(friday_regression, lty=1, lwd=2)
        friday_coefficients = round(coef(friday_regression), 5) 
        friday_r_squared = format(summary(friday_regression)$r.squared, digits = 5)

        friday_equation = paste0("Friday ",stock, " = ", friday_coefficients[1],
                     ifelse(sign(friday_coefficients[2])==1, " + ", " - "), abs(friday_coefficients[2])," Time From 0 ","\nR^2 = ",friday_r_squared)
        ## printing of the equation
        mtext(friday_equation, 3, line=-1)
        pauseProgram()
        plot(fitted(friday_regression),resid(friday_regression),main="Friday Residuals",ylab="Residual",xlab="Time")
        pauseProgram()
      },
      '6'={
        # we will use t test here like when estimating sample mean...
        valid_input = FALSE;
        while (valid_input == FALSE) {
          confidence_level = as.numeric(readline(prompt="Please select a level of confidence between 0 and 1\n"))
          if (confidence_level > 0 & confidence_level < 1) {
            valid_input = TRUE;
            # we will only need Test Statistic 
            alpha_level = (1-confidence_level)

            test_statistic = (monday_sample_mean - friday_sample_mean)/sqrt((monday_sample_sd^2/monday_sample_results) + (friday_sample_sd^2/friday_sample_results))
            z_score = qnorm((1-confidence_level));

            cat(paste("Sample mean for Friday ",format(friday_sample_mean,digits=5),"\n"))
            cat(paste("Sample mean for Monday ",format(monday_sample_mean,digits=5),"\n"))
            cat(paste("One sided Z-score for ",confidence_level,":",format(z_score,digits=5),"\n"))
            cat(paste("The test statistic value is ", format(test_statistic,digits=5),"\n"))
            cat(paste("The p-value here is ",format(pnorm(-abs(test_statistic),lower.tail=FALSE),digits=5),"\n"))
            if (test_statistic > z_score) {
              cat(paste("We reject the hypothesis that the Monday mean of ", stock, " is <= the Friday mean of ",stock,"\n"))
            } else {
              cat(paste("We refuse to reject the hypothesis that the Monday mean of ", stock, " is <= the Friday mean of ",stock,"\n"))
            }
            pauseProgram();
          }
        }
      },
      e={
        return;
      },
      # in the default case, show error message since option is invalid
      {
        menuError();
      }
    )
  }
  return;
}

multiStockMenu <- function() {
  multiStockOption = "";
  # ask for first stock

  # ask for second stock to compare it to

  # ask of user wants option 1, or 2 for 
  #load in our csv
  stock_one_valid=FALSE;

  while (stock_one_valid==FALSE) {
    stock_one = readline(prompt="Please enter stock one: alk, arlp, bac, cvs, cnsl, cvx, gel, hd, jpm, t, tgt, unp, ups, vz, wltw, or all_stocks.\n")
    if (is.element(stock_one,stocks)) {
      stock_one_valid = TRUE;
    }
  }

  stock_two_valid=FALSE;
  while (stock_two_valid==FALSE) {
    stock_two = readline(prompt="Please enter stock two: alk, arlp, bac, cvs, cnsl, cvx, gel, hd, jpm, t, tgt, unp, ups, vz, wltw, or all_stocks.\n")
    if (stock_two != stock_one) {
      if (is.element(stock_one,stocks)) {
        stock_two_valid = TRUE;
      }
    } else {
      cat(paste("Please make sure your stock is different than ",stock_one))
    }
  }

  monday_data_stock_one = read.csv(paste(stock_one,"_5_year_monday.csv",sep=""))
  friday_data_stock_one = read.csv(paste(stock_one,"_5_year_friday.csv",sep=""))
  monday_data_stock_two = read.csv(paste(stock_two,"_5_year_monday.csv",sep=""))
  friday_data_stock_two = read.csv(paste(stock_two,"_5_year_friday.csv",sep=""))

  while (multiStockOption != 'e') {
    cat("\nPlease select from the following options for stock ",stock_one," and ",stock_two,
      ", or press e to exit.")
    multiStockOption <- readline(prompt=paste("
      \t1. Test Equality of Means for Mondays and Fridays of each stock, assuming n,m are large.
      \t2. Perform a regression of one log return on the other on each day for stocks\n",sep=""))


    switch(multiStockOption,
      '1'={
        #we will just compare stock 1 monday to stock 2 monday and stock 1 friday to stock 2 friday, using Z score because we assume n,m large

        # get means
        monday_data_stock_one_mean = mean(monday_data_stock_one$log_returns)
        monday_data_stock_two_mean = mean(monday_data_stock_two$log_returns)

        friday_data_stock_one_mean = mean(friday_data_stock_one$log_returns)
        friday_data_stock_two_mean = mean(friday_data_stock_two$log_returns)

        # get sds
        monday_data_stock_one_sd = sd(monday_data_stock_one$log_returns)
        monday_data_stock_two_sd = sd(monday_data_stock_two$log_returns)

        friday_data_stock_one_sd = sd(friday_data_stock_one$log_returns)
        friday_data_stock_two_sd = sd(friday_data_stock_two$log_returns)
               
        # get sample size
        monday_data_stock_one_length = length(monday_data_stock_one$log_returns)
        monday_data_stock_two_length = length(monday_data_stock_two$log_returns)

        friday_data_stock_one_length = length(friday_data_stock_one$log_returns)
        friday_data_stock_two_length = length(friday_data_stock_two$log_returns)

        valid_input = FALSE;
        while (valid_input == FALSE) {
          confidence_level = as.numeric(readline(prompt="Please select a level of confidence between 0 and 1:\n"))
          if (confidence_level > 0 & confidence_level < 1) {
            valid_input = TRUE;
            #compute right and left confidence interval in this scenario
            # remember, we are asking if stock_one = stock_two
            monday_mean_difference = abs(monday_data_stock_one_mean - monday_data_stock_two_mean)
            friday_mean_difference = abs(friday_data_stock_one_mean - friday_data_stock_two_mean)
            monday_test_statistic = abs((monday_data_stock_one_mean - monday_data_stock_two_mean)/sqrt((monday_data_stock_one_sd^2/monday_data_stock_one_length) + (monday_data_stock_two_sd^2/monday_data_stock_two_length)))
            friday_test_statistic = abs((friday_data_stock_one_mean - friday_data_stock_two_mean)/sqrt((friday_data_stock_one_sd^2/friday_data_stock_one_length) + (friday_data_stock_two_sd^2/friday_data_stock_two_length)))
            # display results
            cat(paste("Monday difference in means is: ",monday_mean_difference,"\n"))
            cat(paste("Friday difference in means is: ",friday_mean_difference,"\n"))
            cat(paste("Monday test statistic is: ",monday_test_statistic,"\n"))
            cat(paste("Friday test statistic is: ",friday_test_statistic,"\n"))
            # %confidence = (1-alpha)
            # (1-confidence)/2 = alpha/2
            z_score = abs(qnorm((1-confidence_level)/2))
            cat(paste("Double sided Z-score for ",confidence_level,":",z_score,"\n"))

            if (monday_test_statistic > z_score) {
              cat(paste("\nWe reject the hypothesis that the Monday mean of ", stock_one, " is equal to the Monday mean of ", stock_two,"\n","\n"))
            } else {
              cat(paste("We refuse to reject the hypothesis that the Monday mean of ", stock_one, " is equal to the Monday mean of ", stock_two,"\n"))
            }

            if (friday_test_statistic > z_score) {
              cat(paste("We reject the hypothesis that the Friday mean of ", stock_one, " is equal to the Friday mean of ", stock_two,"\n"))
            } else {
              cat(paste("We refuse to reject the hypothesis that the Friday mean of ", stock_one, " is equal to the Friday mean of ", stock_two,"\n"))
            }
            pauseProgram();
          }
        }

      },
      '2'={
        #here, we will display two plots, one for regression of monday to monday and the other for regression of friday to friday
        par(mfrow = c(2,1))
        plot(monday_data_stock_one$log_returns ~ monday_data_stock_two$log_returns,bty="l",pch=20,ylab=paste("Log Return ",stock_one),xlab=paste("Log Return ", stock_two),main="Monday Log Regression")
        monday_regression<-lm(monday_data_stock_one$log_returns ~ monday_data_stock_two$log_returns)
        abline(monday_regression, lty=1, lwd=2)
        monday_coefficients = round(coef(monday_regression), 5) 
        monday_r_squared = format(summary(monday_regression)$r.squared, digits = 5)
        monday_equation = paste0("Monday ",stock_one, " = ", monday_coefficients[1],
                     ifelse(sign(monday_coefficients[2])==1, " + ", " - "), abs(monday_coefficients[2])," Monday ",stock_two,"\nR^2 = ",monday_r_squared)
        ## printing of the equation
        mtext(monday_equation, 3,line=-1)


        pauseProgram();
        plot(friday_data_stock_one$log_returns ~ friday_data_stock_two$log_returns,bty="l",pch=21,ylab=paste("Log Return ",stock_one),xlab=paste("Log Return ", stock_two),main="Friday Log Regression")
        friday_regression<-lm(friday_data_stock_one$log_returns ~ friday_data_stock_two$log_returns)
        abline(friday_regression, lty=1, lwd=2)
        friday_coefficients = round(coef(friday_regression), 5) 
        friday_r_squared = format(summary(friday_regression)$r.squared, digits = 5)

        friday_equation = paste0("Friday ",stock_one, " = ", friday_coefficients[1],
                     ifelse(sign(friday_coefficients[2])==1, " + ", " - "), abs(friday_coefficients[2])," Friday ",stock_two,"\nR^2 = ",friday_r_squared)
        ## printing of the equation
        mtext(friday_equation, 3, line=-1)
        pauseProgram();
        plot(fitted(friday_regression),resid(friday_regression),main="Friday Residuals",ylab="Residual",xlab="Time")
        plot(fitted(monday_regression),resid(monday_regression),main="Monday Residuals",ylab="Residual",xlab="Time")
      },
      e={
        #mainMenu();
        return;
      },
      # in the default case, make sure we match a stock, otherwise show error message
      {
        menuError();
      }
    )
  }
}

pauseProgram <- function() {
  cat ("Press enter to continue.")
  line <- readline()
}

mainMenu();


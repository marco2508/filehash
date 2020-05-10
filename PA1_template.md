Loading and preprocessing the data
----------------------------------

All work is done in the current working directory

    #get the data
    dest = "./activity.zip"
    if( !file.exists( dest))
    {
      fileurl = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
      download.file( fileurl, destfile = dest)
      unzip( dest )
    }
    file = "./activity.csv"
    df = read.csv( file=file)
    head( df )

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

What is mean total number of steps taken per day?
-------------------------------------------------

For this part of the assignment, you can ignore the missing values in
the dataset.

    #remove the NA's'
    df_new <- na.omit( df )

    #aggregate per day
    days <-group_by( df_new, date ) 
    days <- summarize( days, steps=sum(steps))
    hist( days$steps,
          breaks=30,
          col="green",
          main = "Steps/Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    # mean & median for the figures per day
    oldmean <- mean( days$steps)
    oldmedian <- median( days$steps)
    paste0( "Mean is ", oldmean, " Median is ", oldmedian)

    ## [1] "Mean is 10766.1886792453 Median is 10765"

What is the average daily activity pattern?
-------------------------------------------

And which 5 minute interval has the highest number?

    #average daily pattern; days on y-axis, 5 minute pattern
    #aggregate per interval
    interval <-group_by( df_new, interval )
    interval <- summarize( interval, steps=mean(steps))

    ggplot( interval, type="l", 
            aes(interval, steps) ) +
            geom_line()

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    paste0( "Interval with the highest number: ", interval[ max <- which.max( interval$steps ), 1 ] )

    ## [1] "Interval with the highest number: 835"

    paste0( "Steps for Interval with the highest number: ", interval[ max <- which.max( interval$steps ), 2 ] )

    ## [1] "Steps for Interval with the highest number: 206.169811320755"

Imputing missing values
-----------------------

How many do we have? And we fill them with the means per interval.  
Because on day level we dont always see values, on interval level we
do.  
We do this by an extra function.

Further new mean and median values are asked.

    paste0( "Nbr missing values: ", nrow( df ) - nrow( df_new ) )

    ## [1] "Nbr missing values: 2304"

    #therefore we create this funtion
    replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

    #and chain this into again df_new
    df_new <- df %>% 
                group_by(interval) %>% 
                mutate(steps= replacewithmean(steps))

    #again aggregate per day
    #shorter; but have to assign column names
    days <- aggregate( df_new$steps, by=list(df_new$date), sum)
    names(days)[1]  <- "date"
    names( days) [2] <- "steps"
    # and create a new histogram
    hist( days$steps,
          breaks=30,
          col="green",
          main = "Steps/Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    # mean & median for the figures per day
    newmean <- mean( days$steps)
    newmedian <- median( days$steps)
    paste0( "OLD: Mean is ", oldmean, " Median is ", oldmedian)

    ## [1] "OLD: Mean is 10766.1886792453 Median is 10765"

    paste0( "NEW: Mean is ", newmean, " Median is ", newmedian)

    ## [1] "NEW: Mean is 10766.1886792453 Median is 10766.1886792453"

Finally Do we see activity pattern difference in the current dataset?
---------------------------------------------------------------------

transform the dates to real dates and determine if they are weekend or
not

    df_new$date <- as.Date( df_new$date)
    df_new$day <- weekdays( df_new$date)
    df_new$weekend <- ifelse( df_new$day=="zondag"|df_new$day=="zaterdag", "weekend", "werkdag")

    #and aggregate the data
    interval <-aggregate( df_new$steps, by=list(df_new$weekend, df_new$interval), sum)
    names(interval)[1] <- "weekend"
    names(interval)[2] <- "interval"
    names(interval)[3] <- "steps"
    head( interval )

    ##   weekend interval       steps
    ## 1 weekend        0   3.4339623
    ## 2 werkdag        0 101.3018868
    ## 3 weekend        5   0.6792453
    ## 4 werkdag        5  20.0377358
    ## 5 weekend       10   0.2641509
    ## 6 werkdag       10   7.7924528

    ggplot( interval, aes(x=interval, y=steps, color=weekend)) +
      geom_line() +
      ggtitle( "Weekend vs workday comparison")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

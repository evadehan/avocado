#tibble is a nicer way of printing data in a "tbl_df" 
#or tibble diff rather than a traditional data frame

# install.packages("tibble")
# library("tibble")

##dplyr is a powerful R-package to transform and
#summarize tabular data with rows and columns
##lubridate(close to lubricate) is part of tidyverse
#lubricate is used for date-time data
##set.seed means, "the seed is the number with which Stata program starts
#its algorithm to generate the pseudo-random numbers.by default"123456789"

# library(dplyr)
# library(lubridate)
# set.seed(2017)
# options(digits=4)

## Create a tibble: data_frame()
## Convert your data to a tibble: as_data_frame()
## Change default printing appearance of a tibble: options(tibble.print_max = 20, tibble.print_min = 6)
#the following three lines, created a tibble, and then define two colomms
#as.Date function is used for Date Conversion to and from character
#seq is used for generating regular sequences
#eg:seq(from, to, by= )
#rgamma is a density distribution function with parameters shape and scale
#eg:rgamma(n, shape, rate = 1, scale = 1/rate, alpha = shape, beta = scale)



# my_data<-read.csv("Desktop/640/avocado/conventional.csv")
# glimpse(my_data)
# summary(my_data)
# 
# consumption <- data_frame(
#   date=seq(as.Date("2015-01-01"), as.Date("2018-03-01"), 1),
#   #volumn=rgamma(length(date), shape = 2, scale = 20))
#   volumn=my_data$Total.Volume)



#"group_by takes an existing tbl and converts it into a grouped tbl"
#"floor_date takes a date-time object and rounds it down to the nearest integer value of the specified time unit"
#"users can specify to the nearest second, minute, hour, day, week, month or year"
#%>% is similar to a pipe

# consumption %>% group_by(month=floor_date(date, "month")) %>%
#   summarize(t.volum=sum(volumn))



install.packages("ggpubr")

library('ggpubr')

#pearson correlation#
#parametric correlation

#Kendall tau/Spearman rho
#non-parametric

cor(x,y,method = 'pearson', use = 'complete.obs')
cor.test(x,y,'pearson')

my_data<-read.csv("Desktop/640/avocado/avocadonewyork.csv")

my_data2<-read.csv("Desktop/640/avocado/nytavocadosortedchart.csv")

head(my_data)
head(my_data2)


library('ggpubr')

ggscatter(my_data, x = 'Date', y ='Total.Volume',
          add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson',
          xlab = 'Time', ylab = 'volume'
          )
 
ggscatter(my_data, x = 'Date', y ='AveragePrice',
          add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson',
          xlab = 'Time', ylab = 'price'
          )



res<-cor.test(my_data$AveragePrice, my_data$Total.Volume,
              method = 'pearson')
res

res<-cor.test(my_data$AveragePrice, my_data2$no_of_article,
              method = 'pearson')
res


res2<-cor.test(my_data$AveragePrice, my_data$Total.Volume,
              method = 'kendall')
res2

res3<-cor.test(my_data$AveragePrice, my_data$Total.Volume,
               method = 'spearman')
res3


################

#pearson correlation#
#parametric correlation

#Kendall tau/Spearman rho
#non-parametric

cor(x,y,method = 'pearson', use = 'complete.obs')
cor.test(x,y,'pearson')

nytcounts<-read.csv("Desktop/640/avocado/nytcounts.csv")
head(nytcounts)

library('ggpubr')

ggscatter(nytcounts, x = 'month', y ='no_of_article',
          add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson',
          xlab = 'month', ylab = 'no_of_article'
)
ggscatter(nytcounts, x = 'month', y ='mvolume',
          add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson',
          xlab = 'month', ylab = 'mvolume'
)
ggscatter(nytcounts, x = 'month', y ='mprice',
          add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson',
          xlab = 'month', ylab = 'mprice'
)
res<-cor.test(nytcounts$mvolume, nytcounts$mprice,
              method = 'pearson')
res

res2<-cor.test(nytcounts$mvolume, nytcounts$no_of_article,
              method = 'pearson')
res2

res4<-cor.test(nytcounts$mprice, nytcounts$no_of_article,
              method = 'pearson')
res4

res5<-cor.test(nytcounts$mprice, nytcounts$no_of_article,
               method = 'kendall')
res5

res6<-cor.test(nytcounts$mprice, nytcounts$no_of_article,
               method = 'spearman')
res6

res7<-cor.test(nytcounts$mvolume, nytcounts$no_of_article,
               method = 'kendall')
res7

res8<-cor.test(nytcounts$mvolume, nytcounts$no_of_article,
               method = 'spearman')
res8


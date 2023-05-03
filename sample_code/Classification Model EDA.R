library(ggplot2)
library(ISLR2)

table(OJ$Purchase) # look at how target is balanced

# EDA for numerical/categorical relationship
ggplot(data = OJ, aes(x = PriceDiff)) + geom_histogram() + facet_grid(.~Purchase)
quantile(filter(OJ, Purchase == 'MM')$PriceDiff, seq(0,1, by=0.1))
quantile(filter(OJ, Purchase == 'CH')$PriceDiff, seq(0,1, by=0.1))

ggplot(data = OJ, aes(x = PriceDiff)) + geom_histogram() + facet_grid(.~Purchase)

# EDA for categorical/categorical relationship
ggplot(data = OJ, aes(x = Purchase)) + geom_bar() + facet_grid(.~StoreID)

# faceting with multiple variables
ggplot(data = Default, aes(x = balance)) + geom_histogram() +facet_grid(default~student)

# average values over many different grouping
frac_by_week <- sqldf("SELECT WeekofPurchase, PURCHASE, Count(StoreID) AS Count
                      FROM OJ
                      GROUP BY WeekofPurchase, PURCHASE")
frac_pivot <- pivot_wider(data = frac_by_week, names_from = Purchase, values_from = Count)
ggplot(frac_pivot, aes(x = WeekofPurchase)) + geom_line(aes(y = MM), color = 'blue') + geom_line(aes(y=CH), color = 'red')
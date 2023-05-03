oh_un <- read.csv("ProductOrderUndated.csv")
order_wide <- pivot_wider(oh_un, names_from = ProductID, values_from = Quantity)
order_long <- pivot_longer(order_wide, cols = c('1','2','3'), names_to = 'Product_ID', values_to = 'Quantity')
#join example
full_orders <- sqldf("SELECT * 
                     FROM orders 
                     INNER JOIN order_details 
                     WHERE orders.orderID = order_details.orderID")

employees$report <- as.numeric((employees$reportsTo))
employee2 <- employees

employee_rep <- sqldf("SELECT employees.id AS worker, employee2.id AS boss 
                      FROM employees
                      INNER JOIN employee2
                      WHERE employees.report = employee2.ID")

employee_rep <- sqldf("SELECT employees.id AS worker, employee2.id AS boss 
                      FROM employees
                      LEFT JOIN employee2
                      ON employees.report = employee2.ID")

employee_rep <- sqldf("SELECT employees.id AS worker, employee2.id AS boss 
                      FROM employees
                      RIGHT JOIN employee2
                      ON employees.report = employee2.ID")

# Cartesian join
everything <- sqldf("SELECT *
                    FROM shippers
                    INNER JOIN regions")

order_history2 <- order_history
order_by_month <- sqldf("SELECT order_history.CustomerID, order_history.CustomerID AS Customer_ID2, order_history.Product_ID, order_history.Product_ID AS Product_ID2 
                        FROM order_history
                        INNER JOIN order_history2
                        WHERE order_history.Month = order_history2.Month AND order_history.Year = order_history2.Year")

employee_region <- sqldf("SELECT employee_territories.employeeID, territories.territoryDescription
                         FROM territories
                         INNER JOIN regions
                         INNER JOIN employee_territories
                         ON territories.territoryID =employee_territories.territoryID
                         AND territories.regionID = regions.regionID")
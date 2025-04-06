library(tidyverse)


elec_sales <- read_csv("C:/Users/alida/OneDrive/Documents/electronic/Electronic_sales_Sep2023-Sep2024.csv")
summary(elec_sales)

sum(duplicated(elec_sales))

numeric_data <- elec_sales %>%
  select(where(is.numeric))

summary(numeric_data) 

glimpse(elec_sales)

elec_sales %>%
  select(Gender, `Loyalty Member`,`Product Type`, `Order Status`, 
         `Payment Method`, `Shipping Type`, `Add-ons Purchased`) %>% 
  map(unique)

sum(is.na(elec_sales))


# Cleaning Data

elec_sales <- elec_sales %>%
  mutate(Gender = str_replace(Gender, "#N/A", "Male"),
         `Payment Method` = str_replace(`Payment Method`, "Paypal", "PayPal"),
         `Add-ons Purchased` = replace_na(`Add-ons Purchased`, "No Add-on"))
  
sum(is.na(elec_sales)) # checking if there is any NA left

# Exploratory Data Analysis

## Age

age <- elec_sales %>%
  group_by(Age) %>%
  count() # checking min and max age - optimal bins for age distribution plot

ggplot(elec_sales, aes(x = Age)) +
  geom_histogram(bins = 63, aes(fill = ..count..), color = "white") +
  theme_minimal() +
  scale_fill_gradient2() +
  labs(title = "Distribution by Age", x = "Age", y = "Count")

##Gender

gender <- elec_sales %>%
  count(Gender) %>%
  mutate(Proportion = round(n/sum(n) * 100, 1))

ggplot(gender, aes(x = Gender, y = n, fill = Gender)) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values = c("red", "blue")) +
  geom_text(aes(label = Proportion), vjust = -0.6, size = 3) +
  labs(title = "Gender Distribution", x = "Gender", y = "Count")


## Loyalty

loyalty <- elec_sales %>%
  count(`Loyalty Member`) %>%
  mutate(Proportion = round(n/sum(n) * 100, 1))

ggplot(loyalty, aes(x = `Loyalty Member`, y = n, fill = `Loyalty Member`)) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values = c("purple", "green")) +
  geom_text(aes(label = Proportion), vjust = -0.6, size = 3) +
  labs(title = "Loyalty Member Distrbution", x = "Loyalty Member", y = "Count")

## Product Type

product <- elec_sales %>%
  count(`Product Type`) %>%
  mutate(Proportion = round(n/sum(n) * 100, 1))

ggplot(product, aes(x = `Product Type`, y = n, fill = `Product Type`)) +
  geom_col() +
  theme_minimal() +
  geom_text(aes(label = Proportion), vjust = -0.6, size = 3) +
  labs(title = "Product Type Preference", x = "Product Type", y = "Count")

## Product Range

ggplot(elec_sales, aes(y = SKU, x = `Product Type`, size = `Unit Price`, color = `Product Type`)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Product Range")

elec_sales <- elec_sales %>%
  mutate(`New SKU` = str_c(`Product Type`, SKU, sep = " "))

ggplot(elec_sales, aes(y = `New SKU`, x = `Product Type`, size = `Unit Price`, color = `Product Type`)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Product Range")

## Rating

rating <- elec_sales %>%
  group_by(Rating) %>%
  count() %>%
  ungroup() %>%
  mutate(Proportion = round(n / sum(n) * 100, 1))

ggplot(rating, aes(x = Rating, y = n, fill = "Rating")) +
  geom_col(position = "dodge") +
  scale_fill_ordinal() +
  geom_text(aes(label = Proportion), vjust = -0.8, size = 3) + 
  labs(title = "Rating", x = "Rating", y = "Count") +
  theme_minimal()

## Order Status

order_status <- elec_sales %>%
  count(`Order Status`) %>%
  mutate(Proportion = round(n / sum(n) * 100, 1))

ggplot(order_status, aes(x = `Order Status`, y = n, fill = `Order Status`)) +
  geom_col() +
  geom_text(aes(label = Proportion), vjust = -0.8, size = 3) + 
  theme_minimal() +
  labs(title = "Order Status", y = "Count")


## Payment

payment_method <- elec_sales%>%
  group_by(`Payment Method`) %>%
  count() %>%
  ungroup() %>%
  mutate(Proportion = round(n / sum(n) * 100, 1))

ggplot(payment_method, aes(x = `Payment Method`, y = n, fill = `Payment Method`)) +
  geom_col() + 
  theme_minimal() +
  geom_text(aes(label = Proportion), vjust = -0.8, size = 3) + 
  labs(title = "Payment Method", y = "Count")

ggplot(elec_sales, aes(x = `Payment Method`, y = `Total Price`, fill = `Payment Method`)) +
  geom_violin() +
  labs(title = "Payment method vs Total Price Paid") +
  theme_minimal()



## Shipping

shipping <- elec_sales %>%
  count(`Shipping Type`) %>%
  mutate(Proportion = round(n / sum(n) * 100, 1))

ggplot(shipping, aes(x = `Shipping Type`, y = n, fill = `Shipping Type`)) +
  geom_col() + 
  theme_minimal() +
  geom_text(aes(label = Proportion), vjust = -0.8, size = 3) + 
  labs(title = "Shipping Type Distribution", y = "Count")

ggplot(elec_sales, aes(x = `Shipping Type`, y = `Total Price`, fill = `Shipping Type`)) +
  geom_violin() +
  labs(title = "Payment method vs Total Price Paid") +
  theme_minimal()


## Purchase Date

purchase_date <- elec_sales %>%
  group_by(`Purchase Date`) %>%
  count()

ggplot(purchase_date, aes(x = `Purchase Date`, y = n)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Purchase Timeline", y = "Count")

elec_sales <- elec_sales %>%
  mutate(Month = format(`Purchase Date`, "%Y-%m")) # creating new Month variable

purchase_month <- elec_sales %>%
  group_by(Month) %>%
  count()

ggplot(purchase_month, aes(x = Month, y = n, fill = Month)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.8, size = 3) + 
  labs(title = "Sales by Months", x= "Month", y = "Count") +
  theme_minimal()

revenue_month <- elec_sales %>%
  group_by(Month) %>%
  summarise(Revenue = sum(`Total Price`)/1000)

ggplot(revenue_month, aes(x = Month, y = Revenue, fill = Month)) +
  geom_col() +
  labs(title = "Revenue by Months", x= "Month", y = "Revenue in 1000s") +
  geom_text(aes(label = round(Revenue)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.8, size = 3) + 
  theme_minimal()


elec_sales %>%
  summarise(sum(`Total Price`))

## Add-on

elec_sales <- elec_sales %>%
  mutate(`No Add-on` = str_count(`Add-ons Purchased`, fixed("No Add-on")), 
         Accessory = str_count(`Add-ons Purchased`, fixed("Accessory")),
         `Impulse Item`= str_count(`Add-ons Purchased`, fixed("Impulse Item")), 
         `Extended Warranty`= str_count(`Add-ons Purchased`, fixed("Extended Warranty")))

elec_sales_add <- elec_sales %>%  
  summarise(`Total No Add-on` = sum(`No Add-on`),
            `Total Accessory` = sum(Accessory),
            `Total Impulse` = sum(`Impulse Item`),
            `Total Warranty` = sum(`Extended Warranty`))

elec_sales_war <- elec_sales %>%
  group_by(`Product Type`, `Extended Warranty`) %>%
  count() %>%
  mutate(Total = sum(`Extended Warranty` * n))

ggplot(elec_sales_war, aes(x = `Product Type`, y = Total, fill = `Product Type`)) +
  geom_col() +
  labs(title = "Extended Warranty Sold") +
  theme_minimal()

 
elec_sales_no <- elec_sales %>%
  group_by(`Product Type`, `No Add-on`) %>%
  count() %>%
  mutate(Total = sum(`No Add-on` * n))

ggplot(elec_sales_no, aes(x = `Product Type`, y = Total, fill = `Product Type`)) +
  geom_col() +
  labs(title = "No Add-ons Sold") +
  theme_minimal()

elec_sales_ac <- elec_sales %>%
  group_by(`Product Type`, `Accessory`) %>%
  count() %>%
  mutate(Total = sum(`Accessory` * n))

ggplot(elec_sales_ac, aes(x = `Product Type`, y = Total, fill = `Product Type`)) +
  geom_col() +
  labs(title = "Accessory Sold") +
  theme_minimal()

elec_sales_im <- elec_sales %>%
  group_by(`Product Type`, `Impulse Item`) %>%
  count() %>%
  mutate(Total = sum(`Impulse Item` * n))

ggplot(elec_sales_im, aes(x = `Product Type`, y = Total, fill = `Product Type`)) +
  geom_col() +
  labs(title = "Impulse Item Sold") +
  theme_minimal()

total_add <- elec_sales %>%
  summarise(`Total Add-ons` = sum(`Add-on Total`))


############ Cancellation

ratingst <- elec_sales %>%
  group_by(Rating) %>%  # Group by Rating
  count(`Order Status`) %>%
  mutate(percentage = round(n / sum(n) *100, 1)) %>%
  ungroup()

ggplot(ratingst, aes(x = Rating, y = n, fill = `Order Status`)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = percentage), 
            position = position_dodge(width = 0.9), 
            vjust = -0.8, size = 3) +  # Adjust vjust to move labels above bars
  theme_minimal() +
  labs(title = "Order Status by Rating", x = "Rating", y = "Count")


order_product <- elec_sales %>%
  group_by( `Product Type`) %>%
  count(`Order Status`) %>%
  mutate(percentage = round(n / sum(n) *100, 1))

ggplot(order_product, aes(x = `Product Type`, y = n, fill = `Order Status`)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = percentage), 
            position = position_dodge(width = 0.9), 
            vjust = -0.8, size = 3) + 
  labs(title = "Order Status by Product Type", y = "Count") +
  theme_minimal()


order_shipping <- elec_sales %>%
  group_by( `Shipping Type`) %>%
  count(`Order Status`) %>%
  mutate(percentage = round(n / sum(n) *100, 1))

ggplot(order_shipping, aes(x = `Shipping Type`, y = n, fill = `Order Status`)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = percentage), 
            position = position_dodge(width = 0.9), 
            vjust = -0.8, size = 3) + 
  labs(title = "Order Status by Shipping Type", y = "Count") +
  theme_minimal()


cancellation_month <- elec_sales %>%
  group_by(Month) %>%
  count(`Order Status`) %>%
  mutate(percentage = round(n / sum(n) *100, 1))

ggplot(cancellation_month, aes(x = Month, y = n, fill = `Order Status`)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = percentage), 
            position = position_dodge(width = 0.9), 
            vjust = -0.8, size = 3) + 
  labs(title = "Order Status by Month", y = "Count") +
  theme_minimal()


order_age <- elec_sales %>%
  group_by(`Order Status`, `Age`) %>%
  count()

ggplot(order_age, aes(x = `Age`, y = n, fill = `Order Status`)) +
  geom_col() +
  labs(title = "Order Status by Age", y = "Count") +
  theme_minimal()


### Interesting (Product First Appearance)

product_first_appearance <- elec_sales %>%
  select(`New SKU`, `Purchase Date`) %>%
  group_by(`New SKU`) %>%
  summarise(`First Purchase` = min(`Purchase Date`))  %>%
  arrange(`First Purchase`)

product_life <- elec_sales%>%
  select(`Product Type`, `Purchase Date`, `Unit Price`, `Quantity`, SKU, `New SKU`, `Total Price`)
  filter(`Product Type` == "Smartphone") # Optional filter, hence no %>% applied above :)

ggplot(product_life, aes(x = `Purchase Date`, y = `Unit Price`, color = `Product Type`)) +
  geom_point(size = 2) +
  labs(title = "Purchase Date vs Unit Price") +
  theme_minimal()

revenue_month2 <- elec_sales %>%
  group_by(Month, `New SKU`) %>%
  mutate(Revenue = sum(`Total Price`)) %>%
  filter(`Product Type` == "Headphones") # change `Product Type` for each plot

ggplot(revenue_month2, aes(x = `Month`, y = Revenue, fill = `New SKU`)) +
  geom_col() +
  labs(title = "Revenue by Months - Headphones", x= "Month", y = "Revenue") +
  theme_minimal()


######## Rating insight

avg_rating <- elec_sales %>%
  select(`Product Type`, Rating)  %>%
  group_by(`Product Type`) %>%
  mutate(Avg = round(mean(Rating), 2))


rating_product <- elec_sales %>%
  group_by(`Product Type`) %>%
  count(Rating) %>%
  mutate(Proportion = round(n / sum(n) * 100, 1))


ggplot(rating_product, aes(x = `Product Type`, y = n, fill = factor(Rating ))) +
  geom_col() +
  scale_fill_ordinal() +
  labs(title = "Rating by Product Type", y = "Count", fill = "Rating") +
  theme_minimal()

rating_month <- elec_sales %>%
  group_by(Month) %>%
  count(`Rating`) %>%
  mutate(Avg = mean(sum(round(Rating * n/sum(n), 1)))) %>%
  select(Month, Avg) %>%
  distinct()

ggplot(rating_month, aes(x = Month, y = Avg)) +
  geom_point(size = 5, color = "orange") +
  theme_minimal() +
  scale_y_continuous(limits = c(1, 5)) +
  labs(title = "Average Rating by Months", y = "Avegare Rating")

rating_SKU <- elec_sales %>%
  group_by(`New SKU`) %>%
  count(Rating) %>%
  mutate(Perc = round(n / sum(n) * 100))

ggplot(rating_SKU, aes(x = `New SKU`, y = n, fill = factor(Rating ))) +
  geom_col() +
  scale_fill_ordinal() +
  labs(title = "Rating by SKU", y = "Count", fill = "Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Loyalty insight

loyalty <- elec_sales %>%
  group_by(Month) %>%
  count(`Loyalty Member`) %>%
  mutate(percentage = round(n / sum(n) *100, 1)) 

ggplot(loyalty, aes(x = Month, y = n, fill = `Loyalty Member`)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = percentage), position = position_dodge(width = 1), 
            vjust = -0.8, size = 3) +
  scale_fill_manual(values = c("purple", "green")) +
  labs(title = "Order Status by Month", y = "Count") +
  theme_minimal()

loyalty2 <- elec_sales %>%
  select(`Customer ID`,`Loyalty Member`,`Total Price`,`Purchase Date`, `Order Status`) %>%
  filter(`Customer ID` >= 1000 & `Customer ID` < 4000) %>%
  group_by(`Customer ID`) %>%
  filter(n() >= 5) # change number of purchases and the Customer ID to discover more
  filter(`Order Status` == "Completed") # optional filter, I left it off now


ggplot(loyalty2, aes(x = `Customer ID`, y = `Purchase Date`, color = `Loyalty Member`, size = `Total Price`)) +
  geom_point() +
  scale_color_manual(values = c("purple", "green")) +
  labs(title = "Loyalty changing over time") +
  theme_minimal()


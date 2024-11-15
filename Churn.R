---
  title: 'Lab 3: Predicting Telecom Churn with tidymodels'
author: "Boutelba Houssem"
output:
  pdf_document: default
html_notebook: default
---
  
  # **1. Import Library & data**
  
  ```{r}
library(tidyverse)
library(tidymodels)
library(janitor)
library(broom)
library(gridExtra)
library(gtExtras)
library(readxl)

churn_data <- read.csv("C:\\Users\\Hp\\Desktop\\Machine Learning\\Customer Churn\\Telco-Customer-Churn.csv")

```

# 2. Taking a look at the data

```{r}
summary(churn_data)
```

```{r}
head(churn_data) %>% 
  gt() %>% 
  gt_theme_excel()
```

#### - The churn dataset has 11 missing values.

```{r}
glimpse(churn_data)

```

```{r}
dim(churn_data)
```

-   this dataset has 7043 rows and 21 columns

# **3. Cleaning Data**

```{r}
churn_data <- churn_data %>%
  select(-customerID) %>%
  mutate(SeniorCitizen = as.factor(ifelse(churn_data$SeniorCitizen==1, 'Yes', 'No'))) %>%
  clean_names()%>%
  mutate_if(is.character , as.factor) %>%
  na.omit()

glimpse(churn_data)

nrow(churn_data)
```

# **4. Explanatory Data Analysis (EDA)**

a- distribution of categorical variables

```{r}
churn_percent <- churn_data %>%
  group_by(churn) %>%
  count()%>%
  summarise(percent = n / nrow(churn_data) * 100 )

churn_percent
```

```{r}
churn_pie <- churn_percent %>%
  ggplot( aes(x = "", y = percent , fill = churn)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Percentage Pie Chart") +
  geom_text(aes(label = paste0(churn, "\n", round(percent, 1), "%")),
            position = position_stack(vjust = 0.5))

churn_pie
```

-   we see that that the majority of customers didn't opt to lay off the services of the company.

```{r}
graph1 <- ggplot(churn_data, aes(x=gender,fill=churn ))+
  geom_bar(color="white")
graph2 <- ggplot(churn_data, aes(x=senior_citizen,fill=churn))+
  geom_bar()
graph3 <- ggplot(churn_data, aes(x=dependents,fill=churn))+
  geom_bar()
graph4 <- ggplot(churn_data, aes(x=partner,fill=churn))+
  geom_bar()
grid.arrange(graph1,graph2,graph3,graph4,ncol=2)

```

-   customer churn rate is higher within senior citizens than non-senior citizens

-   customer churn rate is higher within clients who don't have dependents or partners than those who don't , most likely due to the financial conditions of these clients

```{r}
graph5 <- ggplot(churn_data, aes(x=streaming_tv,fill=churn))+
  geom_bar()
graph6 <- ggplot(churn_data, aes(x=streaming_movies,fill=churn))+
  geom_bar()
graph7 <- ggplot(churn_data, aes(x=contract,fill=churn))+
  geom_bar()
graph8 <- ggplot(churn_data, aes(x=paperless_billing,fill=churn))+
  geom_bar()
grid.arrange(graph1,graph2,graph3,graph4,ncol=2)
```

-   **Streaming Services**: Customers who use streaming services (e.g., TV, movies) are less likely to churn.

-   **Phone Services**: Phone service alone does not significantly impact churn.

```{r}
plot1 <- ggplot(churn_data, aes(x=payment_method,fill=churn))+
  geom_bar()+
  coord_flip()
plot4 <- ggplot(churn_data, aes(x=churn,fill=gender))+
  geom_bar()+
  coord_flip()
grid.arrange(plot1,plot4)
```

-   **Electronic Check**: Customers using electronic checks have a higher churn rate.

-   **Automatic Payment**: Encouraging automatic payment methods may reduce churn.

```{r}
ggplot(data = churn_data, aes(x = churn,fill = internet_service)) +
  geom_bar(stat = "count",position = position_dodge()) +
  geom_text(stat = "count" , aes(label = paste( formatC(..count..))),vjust = -0.5 , position = position_dodge(0.9)) +
  ggtitle("Customer Churn by Internet Services") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
```

-   clients who opted for the fiber optic service have the highest rate of churn within those who have internet service , indicating their dissatisfaction with the services provided by the company. Although the majority of these clients use this service.

```{r}
ggplot(data = churn_data, aes(x = churn,fill = contract)) +
  geom_bar(stat = "count",position = position_dodge()) 

```

-   **Contract**: Month-to-month contracts have a significantly higher churn rate compared to one-year or two-year contracts.

<!-- -->

-   **Contract Length**: Encouraging longer contract commitments may reduce churn

# 5 - Distribution of numerical variables.

```{r}
churn_summary <- churn_data %>%
  group_by(churn) %>%
  summarize(mean_tenure = mean(tenure),
            mean_monthlyCharges = mean(monthly_charges))
```

```{r}
combined_plot <- ggplot(churn_summary, aes(x = churn)) +
  geom_bar(aes(y = mean_tenure, fill = churn), stat = "identity", alpha = 0.6) +
  geom_text(aes(y = mean_tenure, label = paste(round(mean_tenure, 0), "months")),
            size = 3.5, vjust = -0.5) +
  labs(title = "Average Tenure") +
  theme_minimal()

combined_plot
```

```{r}
churn_data %>%
  ggplot(aes(x = "", y = tenure, fill = churn)) +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Tenure")


```

#### Calculate the mean tenure and monthly charges according to churn

```{r}
monthlyCharges_plot <- ggplot(churn_summary, aes(x = churn)) +
  geom_bar(aes(y = mean_monthlyCharges, fill = churn), stat = "identity", alpha = 0.6) +
  geom_text(aes(y = mean_monthlyCharges, label = paste(round(mean_monthlyCharges, 0))),
            size = 3.5, vjust = -0.5) +
  labs(title = "Average Monthly Charges") +
  theme_minimal()

monthlyCharges_plot
```

#### - The average monthly charges for churn customers are more than no-churn customers.

```{r}
library(gridExtra)
combined_plots <- grid.arrange(combined_plot, monthlyCharges_plot, ncol = 2)

```

```{r}
g1 <- churn_data %>%
  ggplot(aes(x=monthly_charges,fill=churn ))+
  geom_histogram(color="red")
g2 <- churn_data %>%
  ggplot(aes(x=total_charges,fill=churn ))+
  geom_histogram(color="white")
g3 <- churn_data %>%
  ggplot(aes(x=tenure,fill=churn ))+
  geom_histogram(color="white")
grid.arrange(g1,g2,g3,ncol=2)
```

-   from the graphs we notice that :

    -   Shorter tenure correlates with higher churn rates. New customers are more likely to leave.

    -   Customers with longer tenure (e.g., more than 60 months) are loyal and less likely to churn.

    -   Higher monthly charges are associated with higher churn rates.

# 6. the relationship between Churn and the numerical variables.

```{r}
churn_data %>%
  ggplot(aes(x = "", y = tenure, fill = churn)) +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Tenure")

```

```{r}
churn_data %>%
  ggplot(aes(x = "", y = monthly_charges, fill = churn)) +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Monthly Charges")

```

```{r}

churn_data %>%
  ggplot(aes(x = "", y = total_charges, fill = churn)) +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Total Charges")

```

## Data Preprocessing

# treating the target variables

```{r}
churn_data <- churn_data %>% mutate(churn= as.factor(ifelse(churn=="Yes",1,0)))

head(churn_data)%>% 
  gt() %>% 
  gt_theme_excel()
```

# Spliting the data

```{r}
set.seed(123)
churn_split <-initial_split(churn_data,
                              prop = 0.8,
                              strata ="churn" )
churn_train <- training(churn_split)
churn_test <- testing(churn_split)
churn_split
```

# recipe

```{r}
rec_churn <- recipe(churn~.,churn_train)
```

```{r}
churn_rec <- rec_churn %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>%
  prep() 
  churn_rec
```

## - baking the data in the recipe

```{r}
churn_train_process <-bake(churn_rec,churn_train)
head(churn_train_process)%>% 
  gt() %>% 
  gt_theme_excel()
```

-   setting the engine to :

##1- logistic regression

```{r}
logic_specification <- logistic_reg() %>%
 set_engine("glm") %>%
  set_mode("classification")
```

-   model training

```{r}
logit_fit <- logic_specification %>% 
  fit(churn  ~. , churn_train_process)
```

-   baking the testing data

```{r}
test_train_process <-bake(churn_rec,churn_test)
head(test_train_process)%>% 
  gt() %>% 
  gt_theme_excel()
```

-   prediction

    ```{r}
    churn_pred <- predict(logit_fit, test_train_process)
    churn_pred
    ```

```{r}
churn_test_proc_results <- test_train_process %>% 
     dplyr::bind_cols(churn_pred)
churn_test_proc_results
```

-   evaluation of the model

```{r}
yardstick::accuracy(churn_test_proc_results,churn,.pred_class)
```

##2- KNN(K-nearest neighbors)

-   set the specification :

```{r}
knn_spec <- nearest_neighbor() %>%
 set_engine("kknn") %>%
  set_mode("classification")
```

-   set the model :

```{r}
knn_fit <- knn_spec %>% 
  fit(churn ~. , churn_train_process)
```

-   prediction :

```{r}
knn_churn_pred <- predict(knn_fit, test_train_process)
knn_churn_pred
```

```{r}
knn_churn_test_results <- test_train_process %>% 
     dplyr::bind_cols(knn_churn_pred)
knn_churn_test_results
```

-   evaluation of the model

```{r}
yardstick::accuracy(knn_churn_test_results,churn,.pred_class)
```

### 3. Decision Tree

#### set the model :
```{r}
decision_spec <- decision_tree() %>%
 set_engine("rpart") %>%
  set_mode("classification")
```
## training the model
```{r}
decision_fit <- decision_spec %>% 
  fit(churn  ~. , churn_train_process)
```
## prediction
```{r}
decision_churn_pred <- predict(decision_fit, test_train_process)
head(decision_churn_pred)%>% 
  gt() %>% 
  gt_theme_excel()
```
## 
```{r}
decision_churn_test_results <- test_train_process %>% 
     dplyr::bind_cols(decision_churn_pred)
head(decision_churn_test_results)%>% 
  gt() %>% 
  gt_theme_excel()
```

## accuracy
```{r}
yardstick::accuracy(decision_churn_test_results,churn,.pred_class)
```

# 4. Random Forest

## setting the model 
```{r}
rand_forest_spec <- rand_forest() %>%
 set_engine("ranger") %>%
  set_mode("classification")
```
## training the model
```{r}
random_fit <- rand_forest_spec %>% 
  fit(churn  ~. , churn_train_process)
```
## prediction
```{r}
random_churn_pred <- predict(random_fit, test_train_process)
random_churn_pred
```

## the most accurate model is that of logical regresion with 0.8066809	

## 6-new_customers_data 

```{r}
new_churn_data <- read_xlsx("C:\\Users\\Hp\\Desktop\\Machine Learning\\Customer Churn\\new_customers_data.xlsx")
new_churn_data
```
## taking a look at the data
```{r}
glimpse(new_churn_data)
```

```{r}
summary(new_churn_data)
```
## cleaning the data
```{r}
new_churn_data <- new_churn_data %>%
  select(-customerID) %>%
  mutate(SeniorCitizen = as.factor(ifelse(new_churn_data$SeniorCitizen==1, 'Yes', 'No'))) %>%
  clean_names()%>%
  mutate_if(is.character , as.factor) %>%
  na.omit()
head(new_churn_data)%>% 
  gt() %>% 
  gt_theme_excel()
```
## spliting the data 
```{r}
new_churn_split <-initial_split(new_churn_data,
                              prop = 0.8 )
new_churn_train <- training(new_churn_split)
new_churn_test <- testing(new_churn_split)
new_churn_split

```
## baking the data
```{r}
new_churn_train_process <-bake(churn_rec,new_churn_train)
head(new_churn_train_process) %>% 
  gt() %>% 
  gt_theme_excel()
```
## predictions 
```{r}
new_churn_pred <- predict(logit_fit, new_churn_train_process)
head(new_churn_pred)%>% 
  gt() %>% 
  gt_theme_excel()
```
## results
```{r}
new_churn_results <- new_churn_train_process %>% 
     dplyr::bind_cols(new_churn_pred)
```



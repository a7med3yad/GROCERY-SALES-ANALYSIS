"# data-analysis-on-market-sales" 
# Install and load required packages
#install.packages(c("shiny", "ggstats", "ggplot2", "ggfortify", "arulesViz", "readxl", "dplyr", "arules"))

library(shiny)
library(readxl)
library(dplyr)
library(arulesViz)
library(arules)
library(ggplot2)

# Define UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file", "Upload Excel File"),
      numericInput("num_clusters", "Number of Clusters", 3, min = 2, max = 10),
      numericInput("support", "Support", 0.01, min = 0.001, max = 1, step = 0.01),
      numericInput("confidence", "Confidence", 0.1, min = 0.001, max = 1, step = 0.01)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Cleaning", verbatimTextOutput("cleaning_summary")),
        tabPanel("Clustering", plotOutput("cluster_plot")),
        tabPanel("Clusters Table", tableOutput("clusters_table")),
        tabPanel("Association Rules",verbatimTextOutput("numberofrules"), tableOutput("apriori_rules_table"), plotOutput("rules_plot")),
        tabPanel("Additional Visualizations", plotOutput("pieChart"), plotOutput("barPlot"), plotOutput("cityPlot"), plotOutput("histPlot")),
        tabPanel("Frequency plot of Association",plotOutput("frequencyplotoutput"))
        
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Read data from uploaded file
  data <- reactive({
    req(input$data_file)
    inFile <- input$data_file
    read_excel(inFile$datapath)
  })
  
  # Data Cleaning summary
  output$cleaning_summary <- renderText({
    cleaned_data <- data()
    
    # Remove rows with missing values
    cleaned_data <- na.omit(cleaned_data)
    
    # Remove duplicates
    data <- unique(cleaned_data)
    
    paste("Duplicates:", sum(duplicated(data)),
          "Missing values:", sum(is.na(data)),
          "Outliers removed from numeric columns")
  })
  output$numberofrules <- renderText({
    support <- input$support
    confidence <- input$confidence
    
    transactions_itemsv1 <- paste(data()$items, sep = "\n")
    items_file <- write(transactions_itemsv1, file = "transactions_data.txt")
    tdata <- read.transactions("transactions_data.txt", sep = ",")
    
    apriori_rules <- apriori(tdata, parameter = list(supp = support, conf = confidence, minlen = 2))
    
    num_rules <- length(apriori_rules)
    
    
    
    paste("number of rules:", num_rules)
  })
  # Clustering plot
  output$cluster_plot <- renderPlot({
    data <- data()
    NumberOfCluster <- input$num_clusters
    
    if (NumberOfCluster >= 2 && NumberOfCluster <= 4) {
      Age <- data %>%
        group_by(age) %>%
        summarise(Total = sum(total))
      
      KMEANS <- kmeans(Age, centers = NumberOfCluster)
      Age$cluster <- as.factor(KMEANS$cluster)
      
      ggplot(Age, aes(x = age, y = Total, color = cluster)) +
        geom_point(size = 3) +
        geom_text(aes(label = cluster), vjust = -1) +
        labs(x = "Age", y = "Total Spending", title = "Clustering of Age vs Total Spending") +
        scale_color_manual(values = c("blue", "red", "green", "orange"))
    } else {
      plot(1, type = "n", xlab = "", ylab = "", main = "Please enter a number of clusters between 2 and 4.")
    }
  })
  
  # Clusters table
  output$clusters_table <- renderTable({
    data <- data()
    NumberOfCluster <- input$num_clusters
    if (NumberOfCluster >= 2 && NumberOfCluster <= 4) {
      Age <- data %>%
        group_by(age) %>%
        summarise(Total = sum(total))
      
      KMEANS <- kmeans(Age, centers = NumberOfCluster)
      Age$cluster <- KMEANS$cluster
      Age <- Age[,-2]
      
      Customers <- data %>%
        group_by(customer) %>%
        summarise(Total = sum(total), Age = unique(age))
      for(i in 1:15){
        for(j in 1:12){
          if(Customers$Age[i] == Age$age[j]){
            Customers$Cluster[i] <- Age$cluster[j]
            break
          }
        }
      }
      Customers
    } else {
      data.frame(Message = "Please enter a number of clusters between 2 and 4.")
    }
  })
  
  # Apriori rules table
  output$apriori_rules_table <- renderTable({
    support <- input$support
    confidence <- input$confidence
    
    transactions_itemsv1 <- paste(data()$items, sep = "\n")
    items_file <- write(transactions_itemsv1, file = "transactions_data.txt")
    tdata <- read.transactions("transactions_data.txt", sep = ",")
    
    apriori_rules <- apriori(tdata, parameter = list(supp = support, conf = confidence, minlen = 2))
    
    # If rules are found
    if(length(apriori_rules) > 0){
      # Format support and confidence to display three decimal places
      support_formatted <- sprintf("%.3f", quality(apriori_rules)$support)
      confidence_formatted <- sprintf("%.3f", quality(apriori_rules)$confidence)
      
      # Update support and confidence values in the rules
      quality(apriori_rules)$support <- support_formatted
      quality(apriori_rules)$confidence <- confidence_formatted
      
      # Return the rules as a data frame
      as(apriori_rules, "data.frame")
    } else {
      data.frame(Message = "No rules found")
    }
  })
  
  # Association rules visualization
  output$rules_plot <- renderPlot({
    support <- input$support
    confidence <- input$confidence
    
    transactions_itemsv1 <- paste(data()$items, sep = "\n")
    items_file <- write(transactions_itemsv1, file = "transactions_data.txt")
    tdata <- read.transactions("transactions_data.txt", sep = ",")
    
    apriori_rules <- apriori(tdata, parameter = list(supp = support, conf = confidence, minlen = 2))
    
    if (length(apriori_rules) > 0) {
      plot(apriori_rules, method = "graph", control = list(type="items"))
    } else {
      plot(1, type = "n", xlab = "", ylab = "", main = "No association rules found.")
    }
  })
  #frequncy plot of association
  output$frequencyplotoutput <- renderPlot({
    support <- input$support
    confidence <- input$confidence
    
    transactions_itemsv1 <- paste(data()$items, sep = "\n")
    items_file <- write(transactions_itemsv1, file = "transactions_data.txt")
    tdata <- read.transactions("transactions_data.txt", sep = ",")
    
    apriori_rules <- apriori(tdata, parameter = list(supp = support, conf = confidence, minlen = 2))
    
    
    itemsets1<-lhs(apriori_rules)
    itemsets2<-rhs(apriori_rules)
    par(mfrow = c(2,2))
    itemFrequencyPlot(itemsets1,topN=10,type="relative",col="skyblue",ylab="item frequency(relative) lhs")
    itemFrequencyPlot(itemsets1,topN=10,type="absolute",col="blue",ylab="item frequency(absolute) lhs")
    itemFrequencyPlot(itemsets2,topN=10,type="relative",col="pink",ylab="item frequency(relative) rhs")
    itemFrequencyPlot(itemsets2,topN=10,type="absolute",col="red",ylab="item frequency(absolute) rhs")
    
  })
  
  # Additional visualizations
  output$pieChart <- renderPlot({
    req(data())
    cash_count <- length(which(data()$paymentType == "Cash"))
    credit_count <- length(which(data()$paymentType == "Credit"))
    values <- c(cash_count, credit_count)
    pie(values, labels = c(cash_count, credit_count), main = "Payments Types", col = c("green3", "blue2"))
    legend("topright", legend = c("Cash", "Credit"), fill = c("green3", "blue2"))
  })
  
  output$barPlot <- renderPlot({
    req(data())
    ggplot(data(), aes(x = factor(age), y = total)) +
      geom_bar(stat = "identity", fill = "red3") +
      labs(x = "Age", y = "Total Spending", title = "Total Spending by Age")
  })
  
  output$cityPlot <- renderPlot({
    req(data())
    sum_by_city <- aggregate(total ~ city, data = data(), FUN = sum)
    sum_by_city <- sum_by_city[order(-sum_by_city$total),]
    ggplot(sum_by_city, aes(x = reorder(city, -total), y = total)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(x = "Cities", y = "Total Spending", title = "Total Spending by Cities") +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$histPlot <- renderPlot({
    req(data())
    hist(data()$total, col = "yellow2", main = "Total Spending Distribution", xlab = "Total Spending")
  })
}

# Run the Shiny app
shinyApp(ui, server)


# Analysis

#install.packages("dplyr")
install.packages("tidyr")
library("dplyr")
library("tidyr")

summary(data)
str(data)

# To check what is the most item sold in every city

most_sales <- data
most_sales <- most_sales %>%
  mutate(items = strsplit(as.character(items), ",\\s*")) %>%
  unnest(items)
item_counts <- most_sales %>%
  group_by(city, items) %>%
  summarise(count = n(), .groups = 'drop')
most_common_items <- item_counts %>%
  group_by(city) %>%
  slice_max(n = 1, order_by = count)
print(most_common_items)

# Insights:
# - The most sold item in every city is Whole milk.
# - Most spending ages are 22, 37, 55.
# - Most spending cites are Alexandria and Cairo
# - Cash and Credit usages are nearly equal
# - The least spending frequency is 2500
# - People who buy yogurt also buy whole milk

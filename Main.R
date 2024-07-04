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
      numericInput("num_clusters", "Number of Clusters", 3, min = 2, max = 10), # Adjust max value as needed
      numericInput("support", "Support", 0.01, min = 0.001, max = 1, step = 0.01),
      numericInput("confidence", "Confidence", 0.1, min = 0.001, max = 1, step = 0.01)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Cleaning", verbatimTextOutput("cleaning_summary")),
        tabPanel("Clustering", plotOutput("cluster_plot"), plotOutput("cluster_centers"), plotOutput("cluster_visualization")),
        tabPanel("Association Rules", tableOutput("rules_table"), plotOutput("rules_plot1"), plotOutput("rules_plot2"), plotOutput("rules_plot3"), plotOutput("rules_plot4"),plotOutput("rules_plot5"),plotOutput("rules_plot6")),
        tabPanel("Additional Visualizations", plotOutput("pieChart"), plotOutput("barPlot"), plotOutput("cityPlot"), plotOutput("histPlot"))
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
    cleaned_data <- unique(cleaned_data)



    paste(" duplicates:", sum(duplicated(cleaned_data)),
          "Missing values:", sum(is.na(cleaned_data)),
          "Outliers removed from numeric columns")
  })

  # Define function for K-means Clustering

  kmeansClustering <- function(data, num_clusters) {
    tryCatch({
      # Print out structure of data for debugging
      print(str(data))

      # Extract necessary columns for clustering
      kmdata <- data.frame(total = data$total, age = data$age)

      # Perform K-means clustering
      km <- kmeans(kmdata, num_clusters)

      # Check for errors or warnings in kmeans
      if (!is.null(km$cluster) && !is.null(km$centers)) {
        customer <- data.frame( km$cluster, data)
        centers <- km$centers

        return(list(customer = customer, km = kmdata, centers = centers))

      } else {
        # Return NULL if clustering failed
        return(NULL)
      }
    }, error = function(e) {
      # Print error message
      print(paste("Error in kmeansClustering:", e))
      return(NULL)
    })
  }



  # Clustering plot
  output$cluster_plot <- renderPlot({
    req(input$num_clusters)
    kmeans_result <- kmeansClustering(data(), input$num_clusters)
    if (!is.null(kmeans_result)) {
      plot(kmeans_result$km$centers, col = 1:input$num_clusters, pch = 19, xlab = "Total", ylab = "Age", main = "Cluster Plot")
      points(kmeans_result$customer$total, kmeans_result$customer$age, col = kmeans_result$customer$cluster, pch = 1)
    } else {
      NULL
    }
  })

  # Clustering centers
  output$cluster_centers <- renderPlot({
    req(input$num_clusters)
    kmeans_results <- kmeansClustering(data(), input$num_clusters)
    if (!is.null(kmeans_results)) {
      centers_df <- data.frame(cluster = 1:input$num_clusters, centers = kmeans_results$centers[,1])
      ggplot(centers_df, aes(x = cluster, y = centers)) +
        geom_col(fill = "skyblue") +
        labs(title = "Cluster Centers", x = "Cluster", y = "Center Value")
    } else {
      NULL
    }
  })

  # Clustering visualization
  output$cluster_visualization <- renderPlot({
    req(input$num_clusters)
    kmeans_result <- kmeansClustering(data(), input$num_clusters)
    if (!is.null(kmeans_result)) {
      data_clustered <- cbind(data(), Cluster = kmeans_result$customer[,1])
      ggplot(data_clustered, aes(x = total, y = age, color = factor(Cluster))) +
        geom_point() +
        labs(title = "Cluster Visualization", x = "Total", y = "Age") +
        scale_color_manual(values = c("blue", "red", "green", "orange"))
    } else {
      NULL
    }
  })

  # Association Rules table
  output$rules_table <- renderTable({
    req(input$support, input$confidence)
    trans <- as(as(strsplit(data()$items, ","), "transactions"), "ngCMatrix")
    apriori_result <- apriori(trans, parameter = list(supp = input$support, conf = input$confidence, minlen = 2))

    # Check if any rules are generated
    if (length(apriori_result) == 0) {
      return(data.frame("Message" = "No association rules were found based on the provided thresholds"))
    } else {
      inspect(apriori_result)
    }
  })

  # Association Rules plot 1
  output$rules_plot1 <- renderPlot({
    req(input$support, input$confidence)
    trans <- as(as(strsplit(data()$items, ","), "transactions"), "ngCMatrix")
    apriori_result <- apriori(trans, parameter = list(supp = input$support, conf = input$confidence, minlen = 2))
    plot(apriori_result, jitter = 0)
  })

  # Association Rules plot 2
  output$rules_plot2 <- renderPlot({
    req(input$support, input$confidence)
    trans <- as(as(strsplit(data()$items, ","), "transactions"), "ngCMatrix")
    apriori_result <- apriori(trans, parameter = list(supp = input$support, conf = input$confidence, minlen = 2))
    plot(apriori_result, method = "grouped")
  })

  # Association Rules plot 3
  output$rules_plot3 <- renderPlot({
    req(input$support, input$confidence)
    trans <- as(as(strsplit(data()$items, ","), "transactions"), "ngCMatrix")
    apriori_result <- apriori(trans, parameter = list(supp = input$support, conf = input$confidence, minlen = 2))
    itemsets1 <- lhs(apriori_result)
    itemsets1_trans <- as(itemsets1, "transactions")
    itemFrequencyPlot(itemsets1_trans, topN = 10, type = "relative", col = "skyblue", ylab = "item frequency(relative) lhs", xlim = c(0, 1), ylim = c(0, 1))
  })
  # Association Rules plot 4
  output$rules_plot4 <- renderPlot({
    req(input$support, input$confidence)
    trans <- as(as(strsplit(data()$items, ","), "transactions"), "ngCMatrix")
    apriori_result <- apriori(trans, parameter = list(supp = input$support, conf = input$confidence, minlen = 2))
    itemsets2 <- rhs(apriori_result)
    itemsets2_trans <- as(itemsets2, "transactions")
    itemFrequencyPlot(itemsets2_trans, topN = 10, type = "relative", col = "red", ylab = "item frequency(relative) rhs", xlim = c(0, 1), ylim = c(0, 1))
  })# Association Rules plot 5
  output$rules_plot5 <- renderPlot({
    req(input$support, input$confidence)
    trans <- as(as(strsplit(data()$items, ","), "transactions"), "ngCMatrix")
    apriori_result <- apriori(trans, parameter = list(supp = input$support, conf = input$confidence, minlen = 2))
    itemsets1 <- rhs(apriori_result)
    itemsets1_trans <- as(itemsets1, "transactions")
    itemFrequencyPlot(itemsets1_trans, topN = 10, type = "absolute", col = "blue", ylab = "item frequency(absolute) lhs", xlim = c(0, 1), ylim = c(0, 1))
  })

  # Association Rules plot 6
  output$rules_plot6 <- renderPlot({
    req(input$support, input$confidence)
    trans <- as(as(strsplit(data()$items, ","), "transactions"), "ngCMatrix")
    apriori_result <- apriori(trans, parameter = list(supp = input$support, conf = input$confidence, minlen = 2))
    itemsets2 <- rhs(apriori_result)
    itemsets2_trans <- as(itemsets2, "transactions")
    itemFrequencyPlot(itemsets2_trans, topN = 10, type = "absolute", col = "pink", ylab = "item frequency(absolute) rhs", xlim = c(0, 1), ylim = c(0, 1))
  })

  # Pie Chart for Payment Types
  output$pieChart <- renderPlot({
    req(data())
    cash_count <- length(which(data()$paymentType == "Cash"))
    credit_count <- length(which(data()$paymentType == "Credit"))
    values <- c(cash_count, credit_count)
    pie(values, labels = c(cash_count, credit_count), main = "Payments Types", col = c("green3", "blue2"))
    legend("topright", legend = c("Cash", "Credit"), fill = c("green3", "blue2"))
  })

  # Bar Plot for Total Spending by Age
  output$barPlot <- renderPlot({
    req(data())
    ggplot(data(), aes(x = factor(age), y = total)) +
      geom_bar(stat = "identity", fill = "red3") +
      labs(x = "Age", y = "Total Spending", title = "Total Spending by Age")
  })

  # Bar Plot for Total Spending by Cities
  output$cityPlot <- renderPlot({
    req(data())
    sum_by_city <- aggregate(total ~ city, data = data(), FUN = sum)
    sum_by_city <- sum_by_city[order(-sum_by_city$total),]
    ggplot(sum_by_city, aes(x = reorder(city, -total), y = total)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(x = "Cities", y = "Total Spending", title = "Total Spending by Cities") +
      scale_y_continuous(labels = scales::comma)
  })

  # Histogram for Total Spending Distribution
  output$histPlot <- renderPlot({
    req(data())
    hist(data()$total, col = "yellow2", main = "Total Spending Distribution", xlab = "Total Spending")
  })
}

# Run the Shiny app
shinyApp(ui, server)













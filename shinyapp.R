library(shiny)
library(shinydashboard)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)
library(ggplot2)
library(DT)
df <- read.csv('USA_Housing.csv')
print(head(df))

num.cols <- sapply(df,is.numeric) #get only numeric cols in my df for the linear regression
df <- subset(df,select=-c(Address)) # removing the strtr col in my df, of no use.
cor.data <- cor(df[,num.cols]) #dataframe of correlation between each numeric parameter
dd <- as.data.frame(cor.data)
#split up the sample
sample <- sample.split(df$Price,SplitRatio = 0.7)
# 70% of the data goes to training
train <- subset(df,sample==T)
# 30% will be test
test <- subset(df,sample==F)

# TRAIN & BUILD THE MODEL

model <- lm(Price ~ .,data=df)
print(summary(model))
eff_df <- coef(summary(model))[,1:2]
eff_df <- as.data.frame(eff_df)
eff_df <- eff_df[-c(1),]
print(head(eff_df))
print(ggplot(aes(x=row.names(eff_df),y=Estimate),data=eff_df)+geom_bar(stat='identity')+theme(axis.test.x=element_text(angle=90)))
res <- residuals(model)
res <- as.data.frame(res)
print(ggplot(res,aes(res))+geom_histogram(fill='blue',alpha=0.5))
Price.predictions <- predict(model,test)
results <- cbind(Price.predictions,test$Price)
colnames(results) <- c('predicted','actual')
results <- as.data.frame(results)
print(head(results))
to_zero <- function(x){
  if(x<0){
    return(0)
  }else{
    return(x)
  }
}

results$predicted <- sapply(results$predicted,to_zero)

ui <- dashboardPage(
  dashboardHeader(title= "House Price Prediction"),
  dashboardSidebar(
    menuItem("Distribution",tabName='distribution'),
    menuItem("Correlation",tabName='correlations'),
    menuItem("Prediction",tabName='predictions')
  ),
  dashboardBody(
    tabItems(
      tabItem("distribution",
              box(selectInput("distribution","Distribution",c=colnames(df),width=NULL)
              ),
              box(plotOutput('distribution_plot'),width = 8)
      ),
      tabItem('correlations',fluidPage(
        h1("Correlations")),
              box(selectInput("correlations","Correlations",c=colnames(df),width=NULL)),
              box(plotOutput("correlation_plot"),width=8)
      ),
      tabItem('predictions',fluidPage(
        h1('Predictions'),
        dataTableOutput('eff_df_price'),
        h1('Regression Model Validity'),
        box(plotOutput('scatter_predict'),width=8),
        box(plotOutput('predicted_actual'),width=8)

      ))
    )))
server <- function(input,output){
  output$distribution_plot <- renderPlot({
    hist(df[[input$distribution]])
  })
  output$correlation_plot <- renderPlot({
    plot(df$Price,df[[input$correlations]])
  })
  output$eff_df_price <- renderDataTable(eff_df)
  output$scatter_predict <- renderPlot({
    plot(results$actual,results$predicted)
  })
  output$predicted_actual <- renderPlot({
    hist((results$predicted - results$actual))
  })
}

shinyApp(ui,server)


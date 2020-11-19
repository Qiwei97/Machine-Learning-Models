library(shiny)
library(shinyjs)
library(shinyBS)
library(leaflet)
library(ggplot2)
library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
library(shinythemes)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(ggthemes)
library(sf)
library(shinydashboard)
library(sp)
library(mapview)
library(ggmap)
library(imager)
library(keras)
library(tensorflow)
library(countrycode)
library(rgdal)
library(caret)
library(randomForest)
library(RANN)
library(shinyWidgets)
library(shinyalert)
library(reticulate)
library(sentimentr)
library(tm)
library(FactoMineR)
library(factoextra)
library(corrplot)
# install_keras()


#Load the model
model <- load_model_tf("image_classifier")

WorldCountry <-  rgdal::readOGR("https://raw.githubusercontent.com/datasets/geo-countries/master/data/countries.geojson")

#Data is used for everything except word cloud
originaldata <- read.csv('Final Data.csv')
originaldata$Rating <- as.numeric(originaldata$Rating)
originaldata$Rating <- ifelse(originaldata$Rating == 0, NA, originaldata$Rating)
originaldata$Product <- as.factor(originaldata$Product)
originaldata$Country.of.origin <- as.factor(originaldata$Country.of.origin)
originaldata$Platform <- as.factor(originaldata$Platform)



#Creating a copy of data from original data so that we can modify data set with Bagging subsequently and revert to original
data <- originaldata


#Data2 is used for wordcloud only
data2 <- read.csv('Final reviews.csv')

#Vector of ecommerces
ecommerce <- c("QOO10", "EZBUY","Lazada","Shopee","all")
ecommerce2 <- c("QOO10", "EZBUY","Lazada","Shopee","Combined", "Facet View")
ecommerce3 <- c("QOO10", "EZBUY","Lazada","Shopee")

#vector of english
english <- c("you","i","the","for","where","yes","lol","haha",'buy','and','so','though','buyer','seller', 'with','came','from','was','ordered','try','this','got','find','after','have')

ui <- tagList(
  useShinyjs(),
  
  useShinyalert(),
  
  navbarPage(title = tags$a(href= "", id = "logo", icon("home", lib = "glyphicon")), 
             id = "inTabset",
             
             #PRODUCT SEARCH TAB
             tabPanel("Product Search", value = "panel1",
                      setBackgroundImage(
                        src = "https://images.pexels.com/photos/515414/pexels-photo-515414.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=750&w=1260"
                      ), 
                      
                      fluidPage(theme = shinytheme("superhero"),
                                titlePanel(
                                  fluidRow( 
                                    column(4, p("Koogle+", style = "font-family: 'Source Sans Pro'; color:gold"), 
                                           align="center", offset=4)
                                  )
                                ),
                                
                                fluidRow(
                                  br(),
                                  column(9,
                                         fileInput("image_path", label = NULL, width='70%', 
                                                   buttonLabel="Upload an image", accept=c("image/*")), #upload image
                                         offset=3)
                                ),
                                
                                fluidRow(
                                  column(4, 
                                         actionBttn(inputId='start',label='Submit', style="fill", color = "danger"), #upload image
                                         actionBttn(inputId='jumptoP2',icon=icon('fas fa-chevron-right'), 
                                                    style="fill", color = "danger"),
                                         offset=4, align="center")
                                ),
                                
                                
                                fluidRow(
                                  br(),
                                  column(4, 
                                         textOutput(outputId = "prediction"), #upload image
                                         offset=4, align="center")
                                ),
                                br(),
                                plotOutput(outputId = "image")
                      )
             ), 
             
             
             
             #FACTORS TAB
             tabPanel("Factors", value = "panel2",
                      setBackgroundImage(
                        src = "https://images.pexels.com/photos/515414/pexels-photo-515414.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=750&w=1260"
                      ),
                      
                      tags$head(tags$style(HTML(".shiny-output-error-validation {color: orange;font-weight: bold;text-align: center}")) # Change colour of validation error
                      ),
                      
                      fluidPage(theme = shinytheme("superhero"),
                                fluidRow(column(12,"Please select how important these factors are to you (10 is utmost importance, 0 is not important)",
                                                fluidRow(
                                                  
                                                  column(12,
                                                         
                                                         hr(),
                                                         
                                                         chooseSliderSkin("Flat"),
                                                         
                                                         sliderInput(inputId = "Price", label="Price:",
                                                                     min = 0, max = 10, value = 5),
                                                         
                                                         #slider for ratings preference
                                                         sliderInput(inputId = "Ratings", "Ratings:",
                                                                     min = 0, max = 10, value = 5),
                                                         
                                                         #slider for amount sold preference
                                                         sliderInput(inputId = "Popularity", "Popularity:",
                                                                     min = 0, max = 10, value = 5)
                                                  ),
                                                  
                                                  column(12, 
                                                         actionBttn(inputId='jumptoP1',icon=icon('fas fa-chevron-left'), 
                                                                    style="fill", color = "default"),
                                                         actionBttn(inputId='jumptoP3', icon=icon('fas fa-chevron-right'), 
                                                                    style="fill", color = "default"), #next button
                                                         offset=4, align="center")
                                                )
                                )))
             ),
             
             
             
             #search results tab
             tabPanel("Search Results", value = "panel3",
                      setBackgroundImage(
                        src = "https://images.pexels.com/photos/515414/pexels-photo-515414.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=750&w=1260"
                      ),
                      fluidPage(h4("Best merchants available"),
                                sliderInput(inputId = 'numberofobs', 'Number of results to view', 
                                            min=1, max=50, value=10),
                                materialSwitch(inputId='changedata',label='Enable Data Imputation', 
                                               value = FALSE, status ='danger', width='400px'),
                                actionBttn(inputId='backtoP2',icon=icon('fas fa-chevron-left'), 
                                           style="fill", color = "default"),
                                actionBttn(inputId='jumptoP4',icon=icon('fas fa-chevron-right'), 
                                           style="fill", color = "default"),
                                hr(),
                                tableOutput("table")
                      )
             ),
             
             #THE VISUALISATIONS TAB
             tabPanel("Visualisations", value = "panel4",
                      setBackgroundImage(
                        src = "https://images.pexels.com/photos/515414/pexels-photo-515414.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=750&w=1260"
                      ),
                      fluidPage(value = "panel4",
                                navbarPage(id = "navbar2", "Select a chart:",
                                           
                                           #tab for the price graph
                                           tabPanel("Price Graph",
                                                    plotOutput('priceplot'),
                                                    
                                                    br(),
                                                    
                                                    actionBttn(inputId='backtoP3',icon=icon('fas fa-chevron-left'), 
                                                               style="fill", color = "default"),
                                           ),
                                           
                                           
                                           #tab for the word cloud and sentiment analysis
                                           tabPanel("Word Cloud",
                                                    wordcloud2Output('cloud'),
                                                    hr(),
                                                    sliderInput(inputId = "num", "Maximum number of words",
                                                                min=10, max=100, value=30
                                                    ),
                                                    hr(),
                                                    selectInput("col", "Background color", c("white","black")
                                                    ),
                                                    hr(),
                                                    selectInput("cloudweb", "E-commerce", ecommerce3)
                                           ),
                                           
                                           #tab for ratings graph
                                           tabPanel("Ratings Distribution",
                                                    plotOutput('ratingplot'),
                                                    selectInput("ratingweb", "E-commerce", ecommerce2)
                                           ),
                                           
                                           
                                           #tab for map graph
                                           tabPanel("Map",
                                                    leafletOutput("countryleaflet")
                                           ),
                                           
                                           
                                           #tab for ratings graph
                                           tabPanel("Predictive Analytics",
                                                    
                                                    radioGroupButtons(
                                                     inputId = "pca",
                                                     label = "Principal Components Analysis",
                                                     choices = c("Variance", "Contribution", "Correlation"),
                                                     status = "primary",
                                                     size = "sm",
                                                     checkIcon = list(
                                                       yes = icon("ok", 
                                                                  lib = "glyphicon"))),
                                                    
                                                    plotOutput('mlplot'),
                                                    
                                                    textOutput('info'),
                                                    
                                                    br(),
                                                    
                                                    selectInput('mlplottype','Graph',c("Feature Importance Plot", "Principal Component Analysis"))),
                                           
                                           
                                           
                                           #tab for ratings graph
                                           tabPanel("Descriptive Analytics",
                                                    
                                                    radioGroupButtons(
                                                      inputId = "heattype",
                                                      label = "HeatMap Type",
                                                      choices = c("Price", "Units Sold", "Rating"),
                                                      status = "primary",
                                                      size = "sm",
                                                      checkIcon = list(
                                                        yes = icon("ok", 
                                                                   lib = "glyphicon"))
                                                    ),
                                                    
                                                    plotOutput('ratingplot2'),
                                                    selectInput('ratingplottype','Graph',c("Price vs Ratings", "Price vs Units Sold", "Sentiment Analysis", "HeatMap"))
                                           )
                                )
                                
                                
                      )
             )
  )
)   


server <- function(input, output, session){
  
  observe({
    hide(selector = "#inTabset") #hide navbar
  })
  
  # Go to Next/Previous Tab
  observeEvent(input$jumptoP1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
  })
  
  observeEvent(input$jumptoP2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  observeEvent(input$backtoP2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  observeEvent(input$jumptoP3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel3")
  })
  
  observeEvent(input$backtoP3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel3")
  })
  
  observeEvent(input$jumptoP4, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel4")
  })
  
  # Hide Next Button
  shinyjs::hide("jumptoP2")
  shinyjs::hide("heattype")
  shinyjs::hide("pca")
  
  # Prepare image for prediction
  image <- reactive({
    req(input$image_path)
    image_load(input$image_path$datapath, target_size = c(224L,224L)) # Resize image for prediction
  })
  
  
  display <- reactive({
    req(input$image_path)
    jpeg::readJPEG(input$image_path$datapath) # Read image for display
  })
  
  # Prediction
  
  item <- reactive({
    
    img <- image()
    
    x <- image_to_array(img)
    x <- array_reshape(x, c(1, dim(x)))
    x <- imagenet_preprocess_input(x)
    pred <- model %>% predict(x)
    
    product_list <- list()
    
    product_list[[1]] <- "Amazon Echo Gen 3"
    
    product_list[[2]] <- "Apple Watch 5"
    
    product_list[[3]] <- "Dyson HD03"
    
    product_list[[4]] <- "Epson L3150"
    
    product_list[[5]] <- "JBL Charge 4"
    
    product_list[[6]] <- "Kindle Paperwhite"
    
    product_list[[7]] <- "Nintendo Switch Gen 2"
    
    product_list[[8]] <- "Samsung Note 20"
    
    product_list[[9]] <- "Sony WH-1000xm4"
    
    product_list[[10]] <- "Sony WF-1000xm3"
    
    item <- product_list[[which.max(pred)]]
    
    item
  })
  
  # Submit Button
  
  observeEvent(input$start, {active(TRUE)})
  
  active <- reactiveVal(FALSE)
  
  observe({
    if(active())
    {
      output$prediction <- renderText({
        paste0("You uploaded an image of ", item())
      })
      
      output$image <- renderPlot({
        ext <- tools::file_ext(input$image_path$datapath)
        validate(need(ext == "jpg", "Please upload a JPEG Image"))  # Enforce JPG
        
        observe({
          if (length(item() != 0)) {
            delay(500, shinyjs::show("jumptoP2"))
          }
        })
        
        plot(as.raster(display()))
      })
    }
  })
  
  findscore <- function(data, item) {
    
    filtered_data <- data %>% filter(Product == as.character(item))
    
    filtered_data$Rating <- ifelse(is.na(filtered_data$Rating), 0, filtered_data$Rating) # Replace NA with 0 for Score Calculation
    
    max_price <- max(filtered_data$Price, na.rm=T)
    min_price <- min(filtered_data$Price, na.rm=T)
    
    max_ratings <- max(filtered_data$Rating, na.rm=T)
    min_ratings <- min(filtered_data$Rating, na.rm=T)
    
    max_popularity <- max(filtered_data$Units.Sold, na.rm=T)
    min_popularity <- min(filtered_data$Units.Sold, na.rm=T)
    
    filtered_data <- filtered_data %>% 
      mutate(Score = input$Price * (1 - ((Price - min_price)/(max_price - min_price))) +
               input$Ratings * ((Rating - min_ratings)/(max_ratings - min_ratings)) +
               input$Popularity * ((Units.Sold - min_popularity)/(max_popularity - min_popularity)))
    
    filtered_data$Rating <- ifelse(filtered_data$Rating == 0, NA, filtered_data$Rating) # Replace 0 back to NA
    
    filtered_data
  }
  
  #change data set if selected
  observe({
    if(input$changedata)
    {
      shinyalert("Missing fields are now replaced by our in-house AI", type = "success")
      data <- data %>% subset(select =-c(Product.url, Title))
      impute_model <- preProcess(data, method='bagImpute')
      data <- predict(impute_model, newdata = data)
      test <- originaldata %>% subset(select = c(Index, Title, Product.url))
      data <- merge(x = data, y = test, by = 'Index', all.x=TRUE)
      
      data$Stock <- ifelse(data$Stock < 0, 0, data$Stock)
      data$Stock <- format(floor(data$Stock), nsmall = 0)
      
      data$Rating <- ifelse(data$Rating < 0, 0, data$Rating)
      data$Rating <- ifelse(data$Rating > 5, 5, data$Rating)
      data$Rating <- round(data$Rating, digits = 2)
      
      data <- data %>% select(Index, Product, Platform, Title, Price, Rating,
                              Country.of.origin, Stock, Units.Sold, Product.url)
      data$Country.of.origin <- as.character(data$Country.of.origin)
      data$Stock <- as.numeric(data$Stock)
      data <<- data
      
      # Reactive value for selected dataset ----
      datasetInput <- reactive({
        findscore(data, item())
      })
      
      # Table of selected dataset ----
      output$table <- renderTable({
        head(datasetInput()[order(-datasetInput()$Score),], input$numberofobs)
      })
    }
    if(!input$changedata){
      data <- originaldata
      data <<- data
      
      # Reactive value for selected dataset ----
      datasetInput <- reactive({
        findscore(data, item())
      })
      
      # Table of selected dataset ----
      output$table <- renderTable({
        head(datasetInput()[order(-datasetInput()$Score),], input$numberofobs)
      })
    }
  })
  
  
  #function for creating word cloud
  create_wordcloud <- function(data, background = "white", max_words = 100) {
    corpus <- Corpus(VectorSource(data))
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, english)
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    data <- sort(rowSums(tdm), decreasing = TRUE)
    data <- data.frame(word = names(data), freq = as.numeric(data))
    data <- data[order(-data$freq), ]
    
    # Grab the top n most common words
    data <- head(data, ifelse(max_words >= nrow(data), nrow(data), max_words))
    
    #create the wordcloud
    wordcloud2(data, shape = 'cardiod', backgroundColor = background, size = 1)
  }
  
  #reactive value for slicing data for word cloud
  wordcloudInput <- reactive({
      create_wordcloud(data2 %>% filter(Product == as.character(item())) %>% 
                         filter(Platform == as.character(input$cloudweb)) %>% 
                         select(Review), 
                       max_words = input$num, background = input$col)
  })
  
  #Cloud based on the sliced data
  output$cloud <- renderWordcloud2({
    wordcloudInput()
  })
  
  
  
  #reactive value for slicing data for price plot
  priceplotInput <- reactive({
    data %>% filter(Product == as.character(item()))
  })
  
  #boxplot function
  boxplot_price <- function(name) {
    
    names <- c("Amazon Echo Gen 3","Apple Watch 5","Dyson HD03", 
               "Epson L3150",  "JBL Charge 4",
               "Kindle Paperwhite",
               "Nintendo Switch Gen 2",
               "Samsung Note 20",
               "Sony WF-1000xm3",
               "Sony WH-1000xm4")
    
    number <- 1
    
    for (i in names){
      p <- plots$plots[number]
      number <- number + 1
      
      if (name == i){
        return(p)
        break
      }
    }
    
  }
  
  plot <- data %>% ggplot(aes(x = Platform, y = Price, color = Platform)) + geom_boxplot() + theme_economist()
  
  plots = data %>%
    group_by(Product) %>%
    do(plots = plot %+% . + facet_wrap(~Product))
  
  
  #Price Plot based on the sliced data
  output$priceplot <- renderPlot({
    boxplot_price(item())
  })
  
 
  #Reactive Rating Plot slice data
  rating <- reactive({
    
    data3 <- data %>% filter(Product == as.character(item()))
    
    if (as.character(input$ratingweb) == 'Combined'){
      
      ggplot(data=data3) + geom_density(aes(x=Rating, y=..scaled.., fill = Platform), alpha=0.3) + 
                                          theme_economist() +
                                          ylab('Probability') + xlab('Rating') + 
                                          ggtitle('Rating distribution')
      
    } else if (as.character(input$ratingweb) == 'Facet View'){
      
      ggplot(data=data3, aes(x =Rating, y=..scaled.., fill = Platform)) + geom_density() + facet_wrap(~Platform) + 
        theme_economist() + ylab('Probability') + 
        xlab('Rating')  + ggtitle('Rating distribution')
      
    } else {
      
      data3 <- data3 %>% filter(Platform == as.character(input$ratingweb))
      
      ggplot(data=data3) + geom_density(aes(x =Rating, y=..scaled..), fill = "dark red") + theme_economist() + ylab('Probability') + xlab('Rating')  + ggtitle('Rating distribution')
      
    }
    
  })
  
  #Rating Plot based on the sliced data
  output$ratingplot <- renderPlot({
    rating()
  })
  
  
  # Perform Sentiment Analysis
  sentiment_score <- function(data) {
    
    bounded_sentences <- data %>% 
      unnest %>% 
      sentimentr::get_sentences() %>% 
      sentimentr::sentiment() %>% 
      mutate(characters = nchar(stripWhitespace(Review))) %>% 
      filter(characters > 1)
    
    bounded_sentences <- bounded_sentences %>% filter(between(sentiment,-1,1))
    
    dat <- with(density(bounded_sentences$sentiment), data.frame(x, y))
    
    ggplot(dat, aes(x = x, y = y)) +
      geom_line() +
      geom_area(mapping = aes(x = ifelse(x >=0 & x<=1 , x, 0)), fill = "green") +
      geom_area(mapping = aes(x = ifelse(x <=0 & x>=-1 , x, 0)), fill = "red") +
      scale_y_continuous(limits = c(0,7.5)) +
      theme_minimal(base_size = 16) +
      labs(x = "Sentiment Score", 
           y = "", 
           title = "Distribution of Sentiments") +
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.y=element_blank())
    
  }
  
  #Reactive Rating Plot 2 slice data
  ratingplot <- reactive({
    
    data3 <- data %>% filter(Product == as.character(item())) %>% filter(Rating != 0)
    
    if (input$ratingplottype == 'Price vs Ratings'){
      shinyjs::hide("heattype")
      ggplot(data=data) + geom_point(aes(x=Price, y=Rating)) + 
        geom_smooth(aes(x=Price,y=Rating), method = "lm", color= 'red')
    
    } else if (input$ratingplottype == 'Price vs Units Sold'){
      shinyjs::hide("heattype")
      ggplot(data=data, aes(x=Price, y=Units.Sold, fill=Rating)) + geom_point(colour='blue', aes(size=Rating)) + geom_smooth(method = "lm", color= 'red')
    
    } else if (input$ratingplottype == 'HeatMap'){
      shinyjs::show("heattype")
      if (input$heattype == "Price") {
        ggplot(data, aes(Product, Platform)) + 
          geom_tile(aes(fill= Price)) + scale_fill_gradient(low='white',high='blue') + theme(axis.text.x=element_text(angle=90))
      } else if (input$heattype == "Units Sold") {
        ggplot(data, aes(Product, Platform)) + 
          geom_tile(aes(fill= Units.Sold)) + scale_fill_gradient(low='white',high='blue') + theme(axis.text.x=element_text(angle=90))
      } else {
        ggplot(data, aes(Product, Platform)) + 
          geom_tile(aes(fill= Rating**3.5)) + 
          scale_fill_gradient(low='white',high='blue',name = "Ratings", labels = (function(x) format((x**(1/3.5)), scientific = F, digits = 3))) + 
          theme(axis.text.x=element_text(angle=90))
      }
      
    } else {
      shinyjs::hide("heattype")
      data_words <- data2 %>% filter(Product == as.character(item()))
      sentiment_score(data_words)
    }
  })
  
  
  
  #Rating Plot 2 based on the sliced data
  output$ratingplot2 <- renderPlot({
    ratingplot()
  })
  
  #Feature Importance Plot + PCA
  predplot <- reactive({
    
    df <- data %>% filter(Product == as.character(item())) %>% 
      select(Platform, Price, Rating, Country.of.origin, Stock, Units.Sold)
    
    df <- df[complete.cases(df),]
    
    df$Platform <- as.factor(df$Platform)
    df$Country.of.origin <- as.factor(df$Country.of.origin)
    
    if (input$mlplottype == "Feature Importance Plot") {
      
      shinyjs::hide("pca")
      
      # Data Proprocessing
      clean_model <- preProcess(df, method = c("center", "scale"))
      df_train <- predict(clean_model, newdata = df)
      
      
      rf <- randomForest(Rating ~ ., 
                         data = df_train,
                         ntree = 700)
      
      varimp <- varImp(rf)
      
      varimp %>% ggplot(aes(x=row.names(varimp), y=Overall)) + 
        geom_bar(stat='identity', width = 0.5, fill='dark red') + 
        xlab("Variables") + 
        ylab("Feature Importance") + 
        ggtitle("Feature Importance Plot") +
        theme_economist() + labs(caption = "This feature importance plot is generated by our in-house AI.\nIt shows the relative importance of each variable in predicting the ratings of a particular product.\nIt is expected that different products will have different plots.") +
        theme(
        plot.caption = element_text(hjust = 0, size = 14))
      
    } else {
      
      shinyjs::show("pca")
      
      x <- df %>% select(Rating, Price, Stock, Units.Sold)
      
      res.pca <- PCA(x, scale.unit = TRUE, ncp = 5, graph = FALSE)
      
      if (input$pca == "Variance") {
        
        fviz_eig(res.pca, addlabels = TRUE)
        
      } else if (input$pca == "Correlation") {
        
        fviz_pca_var(res.pca,
                     col.var = "contrib", # Color by contributions to the PC
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = TRUE     # Avoid text overlapping
        ) 
        
      } else {
        
        var <- get_pca_var(res.pca)
        
        corrplot(var$contrib, is.corr=FALSE, mar=c(2,0,1,1), method='circle', tl.col = "black", cl.ratio = 0.2, cl.align = "r") # Darker values means contribute more to PC
        
      }
    }
  })
  
  
  #Feature Importance Plot
  output$mlplot <- renderPlot({
    predplot()
  })
  
  #PCA Explanation Messages for Users

  pca_explain <- reactive({
    
    if (input$mlplottype != "Feature Importance Plot") {
    
      if (input$pca == "Variance") {
        
        print("Above shows how much variance is contributed by each Principal Component.")
    
      } else if (input$pca == "Correlation") {
        
        print("Positively correlated variables are grouped together.\nNegatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).")
    
      } else if (input$pca == "Contribution") {
        
        print("Above shows the composition of each Principal Component as well as the percentage contributed by each variable.")
    
      }
      
    } else {
      
      print("")
      
    }
  })
      
  output$info <- renderText({
    pca_explain()
  })
  
  
  #Function for leaflet
  createmap <- function(prod){
    prod.data <- data %>% filter(Product == prod) %>% drop_na(Stock) %>%
      group_by(Country.of.origin) %>% summarise(test=sum(Stock)) %>%
      mutate(code=countrycode(Country.of.origin, origin="country.name", destination="iso3c"))
    
    #Subsetting relevant countries
    Countrydata <- WorldCountry[WorldCountry$ISO_A3 %in% prod.data$code, ]
    
    #Reordering the data
    idx <- match(prod.data$code, Countrydata$ISO_A3)
    Countrydata <- Countrydata[idx,]
    
    #colour
    pal <- colorBin("YlOrRd", 4, domain = prod.data$test, na.color="transparent")
    
    #For labels
    mylabel <- paste("Country: ", prod.data$Country.of.origin, 
                     "|",
                     "Stock Available: ", prod.data$test, sep=" ")
    #Generate
    graph <- leaflet(Countrydata) %>% 
      addTiles() %>%
      addPolygons(fillColor = ~pal(prod.data$test), 
                  stroke = TRUE,
                  smoothFactor = 0.2,
                  #dashArray = 3,
                  weight=0.5, 
                  opacity=1, 
                  color="red", 
                  fillOpacity = 0.7, 
                  label=mylabel, 
                  labelOptions = labelOptions(style =list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto"),
                  highlightOptions = highlightOptions(color="white", weight=2, bringToFront = TRUE)) %>%
      addLegend(pal=pal, values=prod.data$test, opacity=0.9, title= "Total Stock Available", position="bottomleft")
    
    return(graph)
  }
  
  
  
  #plotting the map
  output$countryleaflet <- renderLeaflet({
    createmap(as.character(item()))
  })
  
  
}


shinyApp(ui=ui, server=server)

























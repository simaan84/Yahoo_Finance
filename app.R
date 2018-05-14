library(shiny)
library(plyr)
library(lubridate)
library(quantmod)
library(curl)
library(ggplot2)
library(plotly)

rm(list = ls())

# some usefull functions
ret.f <- function(x) c(NA,x[2:length(x)]/x[1:(length(x)-1)] - 1)

lag.f <- function(x) c(NA,x[1:(length(x)-1)])

# get the prices function
get.prices <- function(v,p,t1,price) {
  
  ds.list <- lapply(v, function(tic) get(getSymbols(tic, from=t1)) )
  names(ds.list) <- v
  
  # aggregate data respectively
  if(p == "w") {
    ds.list <- lapply(ds.list,function(x) apply.weekly(x, function(y) y[nrow(y),])   )
    }
  
  if(p == "m") {
    ds.list <- lapply(ds.list,function(x) apply.monthly(x, function(y) y[nrow(y),])   )
  }
  
  
  # do some adjustment to the data list before merging all together
  ds.list <- lapply(ds.list, function(x) data.frame(x[,grep(price,names(x))]) )
  ds.list <- lapply(ds.list, function(x) data.frame(Date = rownames(x), Price = x[,1]) )
  ds.list <- lapply(v, function(v.i) { names(ds.list[[v.i]])[2] <- v.i ; return(ds.list[[v.i]])}  )
  ds <- Reduce(function(...) merge(...,by = "Date", all = T), ds.list)
  ds$Date <- as.Date(ds$Date)
  
  # if( !is.null(t2) ) {
  #   ds <- ds[ds$Date <= t2 ,]
  # }
  
  return(ds)
}


#### ui ############
ui <- fluidPage(
  titlePanel("Download Data from Yahoo Finance by Majeed Simaan" ),
  h4("This app uses the 'quantmod' package maintained by Joshua M. Ulrich"),

  sidebarLayout(
    sidebarPanel(
      numericInput("m0", label = "choose a starting month", value = 1),
      numericInput("d0", label = "choose a starting day", value = 1),
      numericInput("y0", label = "choose a starting year", value = 2010),
      selectInput("frequency", "choose frequency:",choices = c("daily","weekly","monthly") ),
      selectInput("price", "choose data type:",choices = c("Adjusted","Open","High","Low","Close","Volume")),
      textInput("v", label = "choose tickers separated by spaces", value = c("SPY XLF XLU")  ),
      fileInput("file1", "choose text file for tickers instead",
                accept=c('text-separated-values'))
    ),
    
    mainPanel(
      plotlyOutput('plot1'),
      tableOutput("table.ret"),
      downloadButton('downloadData', 'Download')
    )
  )
)





##### server #########
server <- function(input,output) {
  
  f1 <- function(v,p,y0,m0,d0,price) {
    
    v <- as.character(v)
    v <- unlist(strsplit(v, " "))
    p <- substr(p,1,1)
    
    if(nchar(d0) < 2) {
      d0 <- paste(0,d0,sep = "")
    }
    
    if(nchar(m0) < 2) {
      m0 <- paste(0,m0,sep = "")
    }
    
    # month day year
    t1 <- paste(c(y0,m0,d0),collapse = "-")
    # today's date
    t2 <- as.character(today())
    
    P <- get.prices(v,p,t1,price)
    
    
    P$Date <- as.character(P$Date)
    return(P)
  }

  
  DataInput <- reactive({ 
    
    
    if(is.null(input$file1[[1]]) )
      return(f1(input$v,input$frequency,input$y0,input$m0,input$d0,input$price))
    
    else {
      req(input$file1)
      tics <- scan(input$file1$datapath,what = character())
      if(length(tics) == 0) {
        cat("empty text file. I am returning data for the above tickers")
        return(f1(input$v,input$frequency,input$y0,input$m0,input$d0,input$price))
      }
      else
        return(f1(tics,input$frequency,input$y0,input$m0,input$d0,input$price))
    }
  
    
  })
  
  output$table.ret <- renderTable( {
    tail(DataInput())
  } )
  
  output$downloadData <- downloadHandler(
    filename = "yahoo_returns.csv",
    content = function(file) {
      write.csv(DataInput(), file,row.names = F)
    }
  )
  
      output$plot1 <- renderPlotly( {
      ds <- na.omit(DataInput())
      ds.list <- lapply(2:ncol(ds),function(i) data.frame(Date = ds[,1], Price = ds[,i], Tic = as.factor(names(ds)[i])  ) )
      ds.list <- lapply(ds.list, function(ds.i) { ds.i$Price <- ds.i$Price/ds.i$Price[1]; return(ds.i) }  )
      ds <- Reduce(rbind,ds.list)
      my.plot <- ggplot(ds, aes(x = Date, y = Price, group = Tic , colour = Tic) ) + geom_line(alpha = 0.5, size=0.7) + theme(legend.text=element_text(size=16)  )  + scale_x_discrete(breaks = ds$Date[(1:7)*floor(nrow(ds)/7)])
      my.plot <- ggplotly(my.plot)
      return(my.plot)
      } )
      
    
      
      
  
}


shinyApp(ui = ui, server = server)




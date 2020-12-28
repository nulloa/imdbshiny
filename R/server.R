server <- function(input, output) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    req(input$seriesName)
    
    imdbSeries(seriesname = input$seriesName, seasons = 1:input$nseason) %>% 
        left_join(enrichIMDB(.) %>% select(imdbID, director, runtime, votes)) %>% 
        mutate(runtime = readr::parse_number(runtime))
  })
  
  clusters <- reactive({
    
    if(input$nseason==1){
      kmeans(selectedData() %>% 
               select_if(colSums(!is.na(.)) > nrow(.)*.75) %>% 
               select(-director, -imdbID, -Title, -Season, -Released) %>% 
               na.omit() %>% scale(), 
             centers=5, nstart = 25)
    }else{
      kmeans(selectedData() %>% 
               select_if(colSums(!is.na(.)) > nrow(.)*.75) %>% 
               select(-director, -imdbID, -Title, -Released) %>% 
               na.omit() %>% scale(), 
             centers=5, nstart = 25)
    }
    
  })
  
  modData <- reactive({
    
    tmp <- selectedData() %>%
      left_join(
        selectedData() %>% 
          select_if(colSums(!is.na(.)) > nrow(.)*.75) %>%
          na.omit() %>%
          ungroup() %>%
          mutate(clust_score = clusters()$cluster)
      )
    
    
    bnds = quantile(tmp$imdbRating, 0:5/5)
    
    if(any(is.na(tmp$clust_score))){
      grps = c("Garbage", "Bad", "Average", "Great", "Perfect", NA)
    }else{
      c("Garbage", "Bad", "Average", "Great", "Perfect")
    }
    
    mmclustrat <- tmp %>%
      group_by(clust_score) %>%
      summarise(mnrat = mean(imdbRating)) %>%
      arrange(mnrat) %>%
      cbind(., grps)
    
    tmp %>%
      left_join(mmclustrat) %>%
      mutate(clust_score = grps) %>%
      select(-grps, -mnrat) %>%
      mutate(fixed_score = get_rating_score(imdbRating, bnd=bnds))
    
  })
  
  output$plot1 <- renderPlotly({
    p <- modData() %>%
      select(-imdbID, -director) %>%
      gather(clustertype, Rating, -Season, -Episode, -Released, -imdbRating, -votes, -runtime, -Title) %>%
      mutate(Rating = factor(Rating, c("Perfect", "Great", "Average", "Bad", "Garbage")),
             clustertype = recode_factor(clustertype,
                                         'clust_score' = 'Clusters',
                                         'fixed_score' = 'Quantiles')) %>%
      ggplot(aes(x=Released, y=imdbRating)) + 
      geom_point(aes(text=paste("Title:", Title), color=Rating, size=votes)) + theme_bw() + 
      facet_grid(clustertype~.) + 
      labs(x="Airdate", y="Rating", color="Rating Group") + 
      theme(legend.position="bottom")
    
    plotly::ggplotly(p)
  })
  
}


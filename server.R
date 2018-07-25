
function(input, output, session) {
  
  ## Background tab: line graph ##
  
  output$total_line <-renderPlot({
    ggplot(bike_line, aes(x = date, y=n)) +
      geom_line(color = "#33ffcc") +
      geom_smooth(method = 'lm') +
      ylab("Number of Accidents per Day") +
      xlab("") +
      themes
  })
  
  ## Background tab: area graph ##
  
  total<-reactive({
    validate(
      need(input$seasoncheckbox != "", "Please select season"))
    # if (input$seasoncheckbox=='all'){
    #   bike_total
    #   }
    # else{
      bike_total %>%
      filter(., season %in% input$seasoncheckbox)
    
  })
  
  output$total <-renderPlot({
    ggplot(total(), aes(x=year,y = n))+ 
    geom_area(aes(fill = season)) +
    labs(y = "Number of Accidents per Season",
         x = "") +
    themes +
    scale_color_manual(values = c("fall" = "#ffa789", "summer" = "#ffff66", "spring" = "#00ff7f", "winter" = "#0099ff"), aesthetics = "fill") +
    coord_cartesian(ylim=c(0,6000))
  })

  
  # outcome <-reactive({
  #   validate(
  #     need(input$outcomecheckbox != "", "Please select category"))
  #   if (input$outcomecheckbox=='injured'){
  #     bike_outcome%>%
  #       filter(outcome == "injured")
  #   }
  #   else{
  #     bike_outcome%>%
  #       filter(outcome == "killed")
  #   }
  # })
  # 
  # output$outcome <-renderPlot({
  #   ggplot(outcome(), aes(x=year, y=number, group = season)) +
  #     geom_area(aes(fill = season), alpha = 0.7) +
  #     labs(title = "Severity of cyclist collisions",
  #          x = "",
  #          y = "Number of cyclists affected")  +
  #     themes +
  #     scale_fill_manual(values = c("fall" = "#ffcccc", "summer" = "#ffff66", "spring" = "#99ffcc", "winter" = "#ccccff"))
  # })
  
  ## Background tab: hour slider ##
  
  output$hour <- renderPlot({
    ggplot(bike_hour, 
      aes(x = HOUR, y = n, group = season, color = season)) + geom_line() +
      coord_cartesian(xlim = round(c(0,input$hourslider))) +
      labs(x = "Hour",
           y = "Number of Accidents")  +
      annotate("text", x= c(9,18), y= c(850, 850), label = c("rush hour", "rush hour"), size = 5, color = "white") +
      annotate("rect", xmin = 8, xmax = 10, ymin = 0, ymax = 800, alpha = 0.08, fill = "#ff0099") +
      annotate("rect", xmin = 16, xmax = 20, ymin = 0, ymax = 800, alpha = 0.08, fill = "#ff0099") +
      themes +
      scale_color_manual(values = c("fall" = "#ffcccc", "summer" = "#ffff66", "spring" = "#99ffcc", "winter" = "#ccccff"))
  })
  
  ## Explore map tab: maps ## 
  
  output$mymap <- renderLeaflet({
    leaflet(bike_map) %>% 
    addTiles() %>%
    addProviderTiles("Wikimedia") %>%
    setView(lat = 40.7545215,lng = -73.9785482, zoom = 12) %>%
    addMarkers(clusterOptions = markerClusterOptions())
  })
  
  output$hot_spots <- renderLeaflet({
    leaflet(hot_spots) %>% 
      addTiles() %>%
      addProviderTiles("Wikimedia") %>%
      setView(lat = 40.7545215,lng = -73.9785482, zoom = 12) %>%
      addCircles(radius = 25, color = "#3300cc")
  })
  
  ## Explore causes tab: Sankey diagram ##
  
  output$diagram <- renderGvis({
    Sank <- gvisSankey(bike_combo, from="contributing.factor.vehicle.1", to="vehicle.type.code.1", weight="freq",
                       options=list(title= 'Main Categories and causes',width= 800, height = 520, sankey= "{link: {color: { fill: '#99ffcc' } },
                                    node: { color: { fill: '#ffccff' },
                                    label: { color: '#ffcc33', size:'15'}} }" ))
    
    return(Sank)
  })
  
  ## Explore causes tab: heatmap ##
  
  output$heatmap <- renderPlot({
    ggplot(bike_heatmap, aes(HOUR, contributing.factor.vehicle.1))+
      geom_tile(aes(fill = count), colour = "white") +
      scale_fill_gradient(low = "#b6fcd5",high = "#ff0033") +
      theme(
        plot.background = element_rect(
          fill = '#282B30', colour = '#282B30'),
        panel.background = element_rect(
          fill = "#ffffff", colour = '#ffffff'),
        panel.grid.major = element_line(colour = '#ffffff', size = 0.07, linetype = "solid"),
        panel.grid.minor = element_line(colour = '#ffffff', size = 0.05, linetype = "solid"),
        axis.title = element_text(color = '#ffffff', size = 18), 
        axis.text = element_text(color = '#ffffff', size = 10),
        plot.title = element_text(
          color = '#ffffff', size = 18,hjust = 0.5), 
        legend.background = element_rect(
          fill = '#282B30', colour = '#282B30'),
        legend.text = element_text(color = '#ffffff', size = 15),
        legend.title = element_text(color = "#ffffff",size = 15),
        text = element_text(size=20)) +
      labs(title = "Causes of Accidents over 24 hours",
           x = "Hour",
           y = "") })
  
  ## Data table tab: tables ##
  
    output$table_high <- renderDataTable(high_freq)
    output$table_low <- renderDataTable(low_freq)
  
  ## Case study tab: bar plot ##
  
    output$lanes <-renderPlot({
      ggplot(bike_lanes, aes(x = freq, y = perc)) + geom_col(fill = "#ff4040") + themes +
        labs(x = "Frequency of accidents",
             y = "Unprotected bike lanes (%)")+
        coord_cartesian(ylim = c(0,80))+
        geom_text(aes(label = round(perc), vjust = -0.5), color = "white", show.legend = FALSE)})
  
}


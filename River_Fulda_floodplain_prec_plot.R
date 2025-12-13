Fulda_prec_plot <- function(enddate){
  Fulda_daily_temp_ <- Fulda_daily_temp_ %>%
    dplyr::filter(lubridate::year(dateRi) > 1977 & lubridate::year(dateRi) <= lubridate::year(as.Date(enddate)))
  
  Fulda_daily_prec <- Fulda_daily_prec %>%
    dplyr::filter(RS > -999) %>% 
    dplyr::filter(lubridate::year(dateRi) > 1977 & lubridate::year(dateRi) <= lubridate::year(as.Date(enddate)))
  
  # precipitaiton plot requires coeff.
  coeff <- 2
  
  #with precipitation and temperature 
  maxRange= 1.1*(max(Fulda_daily_prec$RS) + max(Fulda_daily_temp_$TT_TER 
  ))
  
  #a slimmer version for ggarrange
  (Fulda_daily_prec_plot <- 
      ggplot(data = Fulda_daily_prec ) +
      geom_tile(data = Fulda_daily_prec, aes(x=dateRi, y = -1*(RS/coeff-maxRange
      ), # y = the center point of each bar
      height = RS,   width = 1),
      fill = "blue",
      color = NA) +#https://stackoverflow.com/questions/42057832/how-to-draw-rainfall-runoff-graph-in-r-using-ggplot
      #https://stackoverflow.com/questions/73181349/create-climate-diagram-in-r-with-temperature-and-precipitation
      geom_line(data = Fulda_daily_temp_, aes(x = dateRi, y = TT_TER))+
      
      scale_y_continuous(
        expression("Air temperature " ( degree*C)), 
        #returns warning  sec.axis = sec_axis(transform = ~  -1*(.-maxRange ), 
        #                      name = "Precipitation (mm)")
        #changed 10 dec 2025:
        sec.axis = sec_axis( ~  -1*(.-maxRange ), name = "Precipitation (mm)")
      )+ #https://stackoverflow.com/questions/73181349/create-climate-diagram-in-r-with-temperature-and-precipitation
      #https://stackoverflow.com/questions/42057832/how-to-draw-rainfall-runoff-graph-in-r-using-ggplot
      labs (x = "Date"#,   y = "Precipitation [mm / day]"
      )+
      scale_x_date(limits = c(t_0, t_max))+
      theme(panel.background = element_rect(fill = "white",  colour = "black", #size = 0.5, 
                                            linetype = "solid" ),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.y.right = element_line(color = "blue"), #https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales
            axis.ticks.y.right = element_line(color = "blue"),
            axis.text.y.right = element_text(color = "blue"), 
            axis.title.y.right = element_text(color = "blue"),
            axis.text = element_text(colour = "black"), 
            axis.text.x = element_text(),
            legend.key = element_blank()#, #get rid of boxes around line - point
      )
  )
}

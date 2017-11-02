#Build Function to Return Element Text Object
rotatedAxisElementText = function(angle,position='x'){
  angle     = angle[1]; 
  position  = position[1]
  positions = list(x=0,y=90,top=180,right=270)
  if(!position %in% names(positions))
    stop(sprintf("'position' must be one of [%s]",paste(names(positions),collapse=", ")),call.=FALSE)
  if(!is.numeric(angle))
    stop("'angle' must be numeric",call.=FALSE)
  rads  = (angle - positions[[ position ]])*pi/180
  hjust = 0.5*(1 - sin(rads))
  vjust = 0.5*(1 + cos(rads))
  element_text(angle=angle,vjust=vjust,hjust=hjust)
}

# Helper functions --------------------------------------------------------
classify_wui <-  function(x) {
  # break out fires into small, med, large
  # input: 
  #   - x: vector of fire sizes
  # output: 
  #   - y: vector (same length) of classified fire sizes ----- Km2
  as.factor(ifelse(x == "Low_Dens_Intermix", "WUI",
                   ifelse(x == "Low_Dens_Interface", "WUI",
                          ifelse(x == "Med_Dens_Intermix", "WUI",
                                 ifelse(x == "Med_Dens_Interface", "WUI",
                                        ifelse(x == "High_Dens_Interface", "WUI",
                                               ifelse(x == "High_Dens_Intermix", "WUI",
                                                      ifelse(x == "Very_Low_Dens_Veg", "VLD", 
                                                             ifelse(x == "Uninhabited_Veg", "Wildlands",
                                                                    ifelse(x == "Med_Dens_NoVeg", "Urban",
                                                                           ifelse(x == "Low_Dens_NoVeg", "Urban",
                                                                                  ifelse(x == "High_Dens_NoVeg", "Urban",
                                                                                         "Other"))))))))))))
}

# GGPLOT Theme ------------------------------------------------------------
theme_pub <- function(base_size=11, base_family="") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(hjust = 0.05, size = 13),
            
            panel.border = element_rect(colour = NA),
            panel.background = element_rect(colour = NA),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(colour = NA),
            
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            
            legend.title = element_text(size=11),
            legend.position = "right",
            legend.text = element_text(size=11),
            legend.direction = "vertical",
            legend.key = element_rect(colour = "transparent", fill = "white"),
            
            strip.background=element_rect(colour=NA),
            strip.text.x = element_text(size = 10),
            
            axis.title = element_text(size = 11),
            axis.text.x = element_text(size = 10, angle = 65, hjust = 1),
            axis.text.y = element_text(size = 11)))
}
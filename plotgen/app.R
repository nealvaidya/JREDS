#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)          #reading the spreadsheet
library(lubridate)       #useful for dealing with dates, times
library(tidyr)           #reshape data
library(dplyr)           #manipulate data
library(splitstackshape) #Used for the 'Gear' data
library(dtplyr) 
library(ggplot2)         #Creating Graphics
library(viridis)         #Colors for graphs
library(cowplot)         #Combine multiple plots into one image
library(gridExtra)
library(grid)
library(ggrepel)
library(tools)
library(grDevices)

activity.area = function(ecol) {
ggplot(data = ecol, aes(x = reorder(Fishing_Area, Fishing_Area, function(x) length(x)), 
                        label = 1)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#0066CC") + 
  scale_y_continuous(labels = scales::percent) + labs(y = "% of all Trips", 
                                                      title = "Percent of Total Activity by Location", x = NULL, subtitle = "Numbers indicate CPUE for each site") + 
  annotate("text", x = (1:length(unique(ecol$Fishing_Area))), y = 0.075, 
           label = sprintf("%.2f", round(tapply(ecol$Weight_Kg, reorder(ecol$Fishing_Area, 
                                                                        ecol$Fishing_Area, function(x) length(x)), mean), 2)), hjust = 1) + 
  geom_hline(yintercept = 0, size = 1, colour = "#535353") + coord_flip() + 
  theme538 + theme(plot.margin = unit(c(1, 1, 0.5, 0.3), "cm"), axis.text.x = element_text(size = 11, 
                                                                                           colour = "#C8C8C8", face = "bold", hjust = 0.3))
}
cpue.activity = function(ecol) {
  ecol = ecol %>% filter(Fishing_Area != "Sudaseyat")
  CPUE <- tapply(ecol$Weight_Kg, ecol$Fishing_Area, mean)  
  Freq <- table(ecol$Fishing_Area)
  cpueloc <- data.frame(CPUE, Freq)
  locmod <- (lm(log(Freq, exp(1)) ~ CPUE, cpueloc))
  predf1 <- data.frame(CPUE = seq(0, max(CPUE)))
  predf1$Freq <- predict(locmod, predf1)
  ggplot(cpueloc, aes(CPUE, Freq)) + geom_point(size = 3, color = "#9A9A9A") + 
    geom_smooth(inherit.aes = F, data = predf1, aes(CPUE, exp(Freq)), color = "red") + 
    geom_hline(yintercept = 0, size = 1, colour = "#535353") + 
    geom_text_repel(aes(label = Var1), nudge_y = 2, color = "#535353", fontface = "bold") + 
    labs(x = "Catch per Unit Effort (kg/trip)",  y = "Number of trips", title = "CPUE vs Activity") + 
    theme538
}
weight.gear   = function(ecol) {
  line_catch <- c(
    sum(ecol$Weight_Kg < 1 & ecol$Gear_line == 1),
    sum(between(ecol$Weight_Kg, 1, 10) & ecol$Gear_line == 1),
    sum(between(ecol$Weight_Kg, 11, 20) & ecol$Gear_line == 1),   #Splits along the quartiles of Weight_Kg
    sum(between(ecol$Weight_Kg, 21, 40) & ecol$Gear_line == 1),
    sum(ecol$Weight_Kg > 40 & ecol$Gear_line == 1)
  )
  
  net_catch <- c(
    sum(ecol$Weight_Kg < 1 & ecol$Gear_net == 1),
    sum(between(ecol$Weight_Kg, 1, 10) & ecol$Gear_net == 1),
    sum(between(ecol$Weight_Kg, 11, 20) & ecol$Gear_net == 1),
    sum(between(ecol$Weight_Kg, 21, 40) & ecol$Gear_net == 1),
    sum(ecol$Weight_Kg > 40 & ecol$Gear_net == 1)
  )
  
  cage_catch <- c(
    sum(ecol$Weight_Kg < 1 & ecol$Gear_net == 1),
    sum(between(ecol$Weight_Kg, 1, 10) & ecol$Gear_cage == 1),
    sum(between(ecol$Weight_Kg, 11, 20) & ecol$Gear_cage == 1),
    sum(between(ecol$Weight_Kg, 21, 40) & ecol$Gear_cage == 1),
    sum(ecol$Weight_Kg > 40 & ecol$Gear_cage == 1)
  )
  
  bin <- c("No Catch", "1 to 10", "11 to 20", "21 to 40", "> 40")
  
  
  gear_catch <- data_frame(line_catch, net_catch, cage_catch) %>% #all in one frame
    gather(V1) #gathers it so theyre all in the same column, not row
  
  gear_catch$cat <- rep(bin, 3) #Which category
  
  ggplot(gear_catch, 
         aes(x = factor(V1, levels = c("line_catch", "net_catch", "cage_catch")), 
             y = value, 
             fill = factor(cat, levels = c("> 40", "21 to 40", "11 to 20", "1 to 10", "No Catch")))) +
    geom_bar(stat = "identity", 
             position = "fill", #Fills the whole length so we see relative comparisons, not absolute
             colour = "black") + 
    coord_flip() + 
    scale_fill_brewer(name = "kg", 
                      palette = "YlGnBu", 
                      direction = -1) + 
    scale_y_continuous(labels = scales::percent) + 
    labs(x = NULL, y = NULL, title = "Catch by Gear") + 
    scale_x_discrete(labels = c("Line", "Net", "Cage")) +
    theme538 +
    theme(
      axis.line.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      panel.background = element_blank())
}
weight.family = function(ecol) {
  ecol$Gear <- unlist(ecol$Gear)
  gear_fam <- ecol %>% group_by(family2, Gear) %>% summarise(weight = sum(Weight_Kg), 
                                                             num = sum(Number_of_Specimens))
  gear_fam$family2 <- relevel(factor(gear_fam$family2), "Other")
  plot_weight <- ggplot(gear_fam, aes(x = family2, y = weight, fill = Gear)) + 
    geom_bar(stat = "identity") + coord_flip() + labs(x = "Family", y = "Weight (Kg)") + 
    theme538 + theme(legend.position = "none")
  plot_num <- ggplot(gear_fam, aes(x = family2, y = num, fill = Gear)) + 
    geom_bar(stat = "identity") + coord_flip() + labs(y = "Number of Specimens") + 
    theme538 + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  grid.arrange(plot_weight, plot_num, ncol = 2)
#  plot_grid(plot_weight, plot_num, align = "h")
}
month.weight  = function(ecol) {
  ecol$family2 <- factor(ecol$family2, levels(factor(ecol$family2))[c(5, 
                                                                      2, 3, 4, 1, 7, 6)])
  ggplot(data = ecol, aes(x = ecol$ym, y = Weight_Kg)) + geom_col(aes(fill = family2), 
                                                                  position = "stack") + scale_fill_brewer(name = "Specimen Family", palette = "Set2") + 
    scale_x_date(date_breaks = "1 months", date_labels = "%b '%y") + theme(axis.text.x = element_text(angle = 45, 
                                                                                                      hjust = 1, vjust = 1)) + labs(y = "Total Weight (kg)", x = NULL, title = "Catch Weight by Month and Specimen Family") + 
    geom_hline(yintercept = 0, size = 1, colour = "#535353") + theme538
}
heat.map      = function(ecol) {
  heatdf <- ecol %>% filter(Family != "No Catch") %>% group_by(ym, Family) %>% 
    summarise(net_weight = sum(Weight_Kg))
  ggplot(heatdf, aes(x = ym, y = Family)) + geom_tile(aes(fill = (net_weight)^(1/3)), 
                                                      width = 31) + coord_equal(ratio = 20) + scale_fill_viridis(option = "D", 
                                                                                                                 name = "Weight (kg)") + geom_text(size = 4, aes(label = net_weight, 
                                                                                                                                                                 color = net_weight, fontface = "bold")) + scale_x_date(expand = c(0, 
                                                                                                                                                                                                                                   0), date_breaks = "1 month", date_labels = "%b '%y", position = "top") + 
    scale_y_discrete(expand = c(0, 0)) + labs(x = NULL, y = "Specimen Family", 
                                              title = "Catch Weight (kg) by Month and Specimen Family", caption = "Blank cell indicates 0") + 
    scale_color_gradientn(colours = c("White", "white", "white", "Black", 
                                      "Black"), guide = FALSE) + theme538 + theme(panel.background = element_rect(fill = "#440154", 
                                                                                                                  colour = "#471260"), panel.grid = element_blank(), plot.margin = unit(c(1, 
                                                                                                                                                                                          1, 0.5, 0.3), "cm"), axis.text.x = element_text(size = 9, colour = "#3C3C3C", 
                                                                                                                                                                                                                                          hjust = 0.3), axis.text.y = element_text(colour = "#3C3C3C", size = 9, 
                                                                                                                                                                                                                                                                                   vjust = 0.3), legend.position = "none")
}
time.weight   = function(ecol) {
  plottot <- ggplot(ecol, aes(x = floor_date(datetime, unit = "day"))) + 
    geom_histogram(bins = 365, position = "identity", aes(y = ..density..), 
                   fill = "#0066CC") + geom_density(color = "#008950", size = 1) + 
    labs(x = NULL, y = NULL, title = "Relative Frequency of Trips") + geom_hline(yintercept = 0, 
                                                                                 size = 1, colour = "#535353") + theme_bw() + theme(panel.border = element_blank(), 
                                                                                                                                    panel.background = element_rect(fill = "#F0F0F0"), plot.background = element_rect(fill = "#F0F0F0"), 
                                                                                                                                    panel.grid.major = element_line(colour = "#D0D0D0", size = 0.5), axis.ticks = element_blank(), 
                                                                                                                                    plot.margin = unit(c(0.5, 1, 0.5, 0.3), "cm"), axis.text.x = element_text(colour = "#3C3C3C", 
                                                                                                                                                                                                              size = 8, hjust = 0.3), axis.text.y = element_blank(), plot.title = element_text(face = "bold", 
                                                                                                                                                                                                                                                                                               hjust = 0, vjust = 2, colour = "#3C3C3C", size = 20))
  plotwday <- ggplot(ecol, aes(x = lubridate::wday(datetime, label = T))) + 
    stat_count(fill = "#0066CC") + theme_bw() + geom_hline(yintercept = 0, 
                                                           size = 1, colour = "#535353") + labs(title = "Day of Week", x = NULL, 
                                                                                                y = NULL) + theme(panel.border = element_blank(), panel.background = element_rect(fill = "#F0F0F0"), 
                                                                                                                  plot.background = element_rect(fill = "#F0F0F0"), panel.grid.major = element_line(colour = "#D0D0D0", 
                                                                                                                                                                                                    size = 0.5), axis.ticks = element_blank(), plot.margin = unit(c(0.1, 
                                                                                                                                                                                                                                                                    0.1, 0.5, 0.3), "cm"), axis.text.x = element_text(colour = "#3C3C3C", 
                                                                                                                                                                                                                                                                                                                      size = 8, hjust = 0.5), axis.text.y = element_blank(), plot.title = element_text(face = "bold", 
                                                                                                                                                                                                                                                                                                                                                                                                       hjust = 0, vjust = 2, colour = "#3C3C3C", size = 12))
  plotmonth <- ggplot(ecol, aes(x = ym)) + stat_count(fill = "#0066CC") + 
    theme_bw() + scale_x_date(date_breaks = "3 months", date_labels = "%m/%y") + 
    geom_hline(yintercept = 0, size = 1, colour = "#535353") + labs(title = "Month", 
                                                                    x = NULL, y = NULL) + theme(panel.border = element_blank(), panel.background = element_rect(fill = "#F0F0F0"), 
                                                                                                plot.background = element_rect(fill = "#F0F0F0"), panel.grid.major = element_line(colour = "#D0D0D0", 
                                                                                                                                                                                  size = 0.5), axis.ticks = element_blank(), plot.margin = unit(c(0.1, 
                                                                                                                                                                                                                                                  0.1, 0.5, 0.3), "cm"), axis.text.x = element_text(colour = "#3C3C3C", 
                                                                                                                                                                                                                                                                                                    size = 8, hjust = 0.5), axis.text.y = element_blank(), plot.title = element_text(face = "bold", 
                                                                                                                                                                                                                                                                                                                                                                                     hjust = 0, vjust = 2, colour = "#3C3C3C", size = 12))
  plothour <- ggplot(ecol, aes(x = hour(datetime))) + stat_count(fill = "#0066CC") + 
    scale_x_continuous(limits = c(1, 24)) + theme_bw() + geom_hline(yintercept = 0, 
                                                                    size = 1, colour = "#535353") + labs(title = "Hour", y = NULL, x = NULL, 
                                                                                                         caption = "By hour of return") + theme(panel.border = element_blank(), 
                                                                                                                                                panel.background = element_rect(fill = "#F0F0F0"), plot.background = element_rect(fill = "#F0F0F0"), 
                                                                                                                                                panel.grid.major = element_line(colour = "#D0D0D0", size = 0.5), axis.ticks = element_blank(), 
                                                                                                                                                plot.margin = unit(c(0.1, 0.1, 0.5, 0.3), "cm"), axis.text.x = element_text(colour = "#3C3C3C", 
                                                                                                                                                                                                                            size = 8, hjust = 0.3), axis.text.y = element_blank(), plot.title = element_text(face = "bold", 
                                                                                                                                                                                                                                                                                                             hjust = 0, vjust = 2, colour = "#3C3C3C", size = 12))
  grid.newpage()
  lay = rbind(c(1,1),
              c(2,3))
  grid.arrange(plottot, plotmonth, plotwday, layout_matrix = lay)
#  ggdraw() + draw_plot(plottot, 0, 0.5, 1, 0.5) + draw_plot(plot_grid(plotmonth, 
#                                                                      plotwday, align = "h", ncol = 2), 0, 0, 1, 0.5)
}
time.cpue     = function(ecol) {
  plot1 <- ecol %>% group_by(ym) %>% summarise(psucc = mean(catch), activity = n()) %>% 
    ggplot(aes(x = ym, y = psucc)) + geom_point(aes(size = activity), color = "#535353") + 
    scale_size_area(name = "Number of trips", max_size = 9) + geom_line(color = "#535353", size = 1) + 
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) + 
    scale_x_date(breaks = date_breaks(width = "1 month"), labels = date_format("%b '%y")) + 
    theme538 + labs(x = NULL, y = NULL, title = "Percent of Trips With Succesful Catch By Month", subtitle = "Size Indicates number of Trips") + 
    geom_hline(yintercept = 0, size = 1, colour = "#535353") + 
    geom_hline(yintercept = mean(ecol$catch), linetype = "dashed") + 
    annotate("label", x = date(min(ecol$datetime)), y = mean(ecol$catch), label = "Mean = 67.9%", vjust = -0.2, hjust = -0.1, fontface = "bold", alpha = 0.5) + 
    theme(plot.margin = unit(c(1, 1, 0.1, 0.5), "cm"), plot.title = element_text(face = "bold", hjust = -0.08, vjust = 2, colour = "#3C3C3C", size = 20), 
          legend.position = "none", legend.direction = "horizontal", legend.key = element_blank(),legend.background = element_rect(fill = "#f0f0f0", color = "#E0E0E0"),axis.text.x = element_blank())
  
    plot2 <- ecol %>% group_by(ym) %>% summarise(cpue = mean(Weight_Kg), activity = n()) %>% 
    ggplot(aes(x = ym, y = cpue)) + geom_point(aes(size = activity), color = "#535353") + 
    scale_size_area(max_size = 9) + geom_line(color = "#535353", size = 1) + 
    scale_y_continuous(limits = c(0, 40)) + scale_x_date(breaks = date_breaks(width = "1 month"), 
                                                         labels = date_format("%b '%y")) + labs(x = NULL, y = NULL, title = "CPUE by month") + 
    geom_hline(yintercept = 0, size = 1, colour = "#535353") + geom_hline(yintercept = mean(ecol$Weight_Kg), 
                                                                          linetype = "dashed") + annotate("label", x= date(min(ecol$datetime)), y = mean(ecol$Weight_Kg), 
                                                                                                          label = "Mean = 23.3 kg/trip", vjust = -0.3, hjust = -0.1, alpha = 0.5) + theme538 + 
    theme(plot.margin = unit(c(0.1, 1, 0.5, 0.5), "cm"), plot.title = element_text(face = "bold", 
                                                                                   hjust = 0, vjust = 2, colour = "#3C3C3C", size = 20), legend.position = "none")
  grid.newpage()
  grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))
}

#ppit = system("wmic desktopmonitor get PixelsPerXLogicalInch", intern = T)
#ppi = as.numeric(ppit[2])
ppi = 144

plotType <- function(x, type) {
  switch(type,
         A = activity.area(x),
         B = cpue.activity(x),
         C = weight.gear(x),
         D = weight.family(x),
         E = month.weight(x),
        "F"= heat.map(x),
         G = time.weight(x),
         H = time.cpue(x))
}


theme538 = theme_grey() + 
  theme(
    plot.margin = unit(c(1, 0.5, .5, .3), "cm"), #pads the borders (top, right, bottom, left)
    plot.title = element_text(  #Features of the plot title
      face = "bold", 
      hjust = 0,    #alginment of text, horizontally
      vjust = 2,      #alignment of text, vertically
      colour = "#3C3C3C", 
      size = 20
      ),
    plot.subtitle = element_text( #Features of plot subtitle
      hjust = 0, 
      vjust = 2, 
      colour = "#535353", 
      size = 15
      ),
    plot.background = element_rect( #background of the whole image
      fill = "#F0F0F0"), 
    panel.border = element_blank(), #Border of the graphed portion
    panel.background = element_rect(
      fill = "#F0F0F0"),            #just the background of the graphed portion
    panel.grid.major = element_line(#The gridlines on the graph
      colour = "#D0D0D0", 
      size = .5
      ),
    axis.ticks = element_blank(), #No tick marks
    axis.text.x = element_text( #text on the x axis
      colour = "#3C3C3C", 
      size = 8, 
      hjust = 0.3),
    axis.text.y = element_text( #text on the y axis
      colour = "#3C3C3C", 
      size = 9, 
      vjust = 0.3),
    axis.title.y = element_text( #label on the y axis
      size = 11, 
      colour = "#535353", 
      face = "bold"),
    legend.background = element_blank(), #legend is transparent
    legend.title = element_text( #label for the legend
      size = 11, 
      colour = "#535353", 
      face = "bold")
  )


importData <- function(filepath) {
  ecol = read_xlsx(filepath)
  names(ecol) <- gsub(" ", "_", names(ecol))
  ecol$Gear <- gsub("s", "", ecol$Gear)
  ecol$Gear <- lapply(ecol$Gear, tolower)
  
  if (is.character(ecol$Month))
    ecol$Month = match(ecol$Month, month.name)
  
  ecol$datetime <- make_datetime(year = ecol$Year, month = ecol$Month, day = ecol$Day)
  
  # readDatetime = function(df) {
  #   out = tryCatch(
  #     make_datetime(year = ecol$Year, day = ecol$Day, month = match(ecol$Month, month.name), hour =  hour(ecol$Time), tz = "Asia/Amman"),
  #     error = function(cond) {
  #       return(make_datetime(year = ecol$Year, month = ecol$Month, day = ecol$Day))
  #     },
  #     warning = function(cond) {
  #       return(make_datetime(year = ecol$Year, month = ecol$Month, day = ecol$Day))
  #     }
  #   )
  # }

  ecol$Number_of_Specimens = as.numeric(ecol$Number_of_Specimens)
  ecol$Family = gsub(ecol$Family, pattern =  "\\s+", replacement =  "")
  ecol$Common_Name = gsub(ecol$Common_Name, pattern = "[[:cntrl:]]+", replacement = "")
  ecol$Fishing_Area = gsub(x = ecol$Fishing_Area, pattern = "[oO]pposite\\s+(to\\s)?", replacement = "")
  
  #splits each gear type into its own column
  ecol <- concat.split.expanded(ecol, split.col = "Gear", sep = " and |\\,|\\+", type = "character", fill = 0, fixed = F)
  
  #ecol <- tbl_df(ecol)
  
  #remove spaces
  #ecol[c(3, 9, 10, 12, 13, 14)] <- lapply(ecol[c(3, 9, 10, 12, 13, 14)], function(x) gsub("[[:cntrl:]]", "", x))
  
  #Remove data from Novmeber 2014 as it throws off monthly averages/analyses
  ecol <- filter(ecol, datetime > ymd("2014/12/01"))
  
  #Year and Month
  ecol$ym <- make_date(year = year(ecol$datetime), month = month(ecol$datetime))
  
  #column that indicates if there was any fish caught
  ecol$catch <- rep(0)
  ecol$catch[ecol$Number_of_Specimens > 0] <- 1
  
  ecol$Family[ecol$Number_of_Specimens == 0] = "No Catch"
  
  #Match cases
  ecol$Family = toTitleCase(tolower(ecol$Family))
  ecol$Common_Name = toTitleCase(tolower(ecol$Common_Name))
  ecol$Fishing_Area = toTitleCase(tolower(ecol$Fishing_Area))
  
  #Common names messed up
  ecol$Common_Name[ecol$Common_Name == "Tuna (Tuna)"] = "Tuna"
  ecol$Common_Name[ecol$Common_Name == "Gamberon (Tuna)"] = "Tuna"
  ecol$Common_Name[ecol$Common_Name == "Amiya (Sardine)"] = "Sardine"
  ecol$Common_Name[ecol$Common_Name == "Soldierfishes"] = "Soldierfish"
  
  ecol$Common_Name[ecol$Common_Name == "Sharak"] = "Shark"
  ecol$Family[ecol$Common_Name == "Sharak"] = "Chondrichthyes"
  
  ecol$Common_Name[ecol$Common_Name == "Shak Normal"] = "Shark"
  ecol$Family[ecol$Common_Name == "Shak Normal"] = "Chondrichthyes"
  
  #family2 only includes major families
  ecol$family2 <- "Other"
  ecol$family2[ecol$Family == "Clupeidae"]      <- "Clupeidae"
  ecol$family2[ecol$Family == "Carangidae"]     <- "Carangidae"
  ecol$family2[ecol$Family == "Scombridae"]     <- "Scombridae"
  ecol$family2[ecol$Family == "Xiphiidae"]      <- "Xiphiidae"
  ecol$family2[ecol$Family == "Istiophoridae"]  <- "Istiophoridae"
  ecol$family2[ecol$Family == "Chondrichthyes"] <- "Chondrichthyes"
  return(ecol)
}

ui <- fluidPage(
  titlePanel("Generate Plots"),
  sidebarLayout(
    sidebarPanel(
      fileInput("data", "Data Source"),
      selectInput("pType", "Plot", choices = c(
        "Activity by Area" = "A",
        "Activity by CPUE" = "B",
        "Weight by Gear" = "C",
        "Weight and Gear by Family" = "D",
        "Weight and Family by Month" = "E",
        "Heatmap" = "F",
        "Weight by Time" = "G",
        "CPUE and % Success by Time" = "H"
      )),
      numericInput("width", "Width", 5),
      numericInput("height", "Height", 3),
      radioButtons("units", "units", choices = c(
        "Inches" = "in",
        "Centimeters" = "cm",
        "Millimeters" = "mm"
      )),
      downloadButton("saveplot", "Save Plot"),
      radioButtons("fType", "file type", choices = c(
        "png", "svg"
      ))
    ),
    mainPanel(
      plotOutput("out")
    )
  )
)

findlen <- function(len, unit) {
  switch(unit,
         "in" = round(len * ppi),
         "cm" = round(len * ppi/2.54),
         "mm" = round(len * ppi/25.4))
}

server <- function(input, output) {
  observe({ 
    output$out <- renderPlot({
      validate(
        need(input$data != "", "Please select a data set")
      )
      inData = input$data
      ecol = importData(inData$datapath)
      plotType(ecol, input$pType)
    }, width = findlen(input$width, input$units),
    height = findlen(input$height, input$units))
  })
  output$saveplot <- downloadHandler(
    filename = function() { paste("plot", input$fType, sep='.') },
    content = function(file) {
      inData = input$data
      ecol = importData(inData$datapath)
      ggsave(file, 
             plot = plotType(ecol, input$pType), 
             device = input$fType,
             width = {
               if (input$fType == "svg")
                 input$width * 1.5
               else
                 input$width * 2
             },
             height = {
               if (input$fType == "svg")
                 input$height * 1.5
               else
                 input$height * 2
             },
             units = input$units,
             dpi = 72,
             pointsize = 9
      )
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)

# load data (move later in the same folder)
setwd('C:/Users/vince/OneDrive/Documents/WD/Programming/Shiny/data/PM')
load('PM_prod.RData')
load('d_cr_area.RData')
load('d_cr_prod.RData')
load('d_poly.RData')
load('dist_list.RData')
load('dist_list_name.RData')
load('data_CM.RData')
# load('d_pred_acc.RData')

# functions
m_f <- function(x) mean(x, na.rm = TRUE)
tr_f <- function(x, y){ m <- lm(y ~ x); coef(m)[2]}


ui <- fluidPage(
  
  # tags$style('.container-fluid {background-color: #77D17F;}'),
  
  titlePanel("Pearl millet agronomy and crop model prediction in India"),
  
  navlistPanel(
    "Description",
    
    #### App description ####
    
    tabPanel("Content",
             
             # tags$style(".well {background-color: #F1B077;}"),
    
             'This app provides interactive statistics and crop model predictions
             about pearl millet production in India.',
             
             tags$br(),
             tags$br(),
             
             'All data are related to Kharif (rainy) season (June-September) for a
             selection of 62 districts representing 90% of the total cultivated area over the period 1998-2017.',
             
             tags$br(),
             tags$br(),
             
             'It uses data from the ICRISAT district level data (DLD) database',
             tags$a('ICRISAT DLD', href = 'http://data.icrisat.org/dld/'),
             
             'The crop model predictions were calculated using the APSIM software',
             
             tags$a('APSIM', href = 'https://www.apsim.info/'),
             
             tags$br(),
             tags$br(),
             tags$strong('The app is distributed with absolutely no warranty.'),
             tags$br(),
             tags$br(),
             
             tags$img(height = 300, width = 375, src = 'PM_field_1.jpg'),
             
             tags$br(),
             
             'Copyright - Vincent Garin - ICRISAT GEMS team'
             
             ),
    "General",
    tabPanel("Area",
             
    #### General-area ####
    
    # Description
    tags$strong('General description:'),
    'Plots, map and descriptive statistics about pearl millet cultivated area in selected districts for a certain time range.',
    tags$br(),
    tags$br(),
    
    selectInput(inputId = 'A_s_dist', label = 'Select district(s)',
                       choices = d_list_nm, multiple = TRUE, selected = 'all'),
    
    sliderInput("A_y_range", label = 'Select year range', min = 1998,
                max = 2017, value = c(1998, 2017), step = 1, ticks = FALSE),
    
    sliderInput("A_y_range_sh", label = 'Select year range for share statistics', min = 1998,
                max = 2015, value = c(1998, 2015), step = 1, ticks = FALSE),
    
    'Select among the following options the statistic that will be ploted on the map below:',
    tags$br(),
    tags$br(),
    'A) average: average area [1000*ha - Kha] over the selected years', tags$br(),
    'B) average share: proportion of area allocated to pearl millet [%] compared
    to other crops averaged over the selected years', tags$br(),
    'C) trend: linear trend or rate of change of the cultivated pearl
    millet area [Kha/year] over the selected years', tags$br(),
    'D) share trend: linear trend or rate of change of the pearl millet share
    area [%/year] over the selected years', tags$br(),
    tags$br(),
    
    selectInput(inputId = 'A_s_var', label = '',
                choices = list(average = 'av', `average share` = 'av_sh',
                               trend = 'tr', `share trend` = 'sh_tr'), selected = 'av_sh'),
    
    hr(),
    tags$strong('Scatter plot of the averaged area [Kha] over year with linear trend.'),
    tags$br(),
    plotOutput('plot_A1', width = '500px'),
    hr(),
    tags$strong('Scatter plot of the averaged proportion of pearl millet area [%] over year with linear trend.'),
    tags$br(),
    plotOutput('plot_A2', width = '500px'),
    hr(),
    plotOutput('p_map_A', width = '400px', height = '400px'),
    hr(),
    'Descriptive statistics of pearl millet cultivated area per district. For the definition of each column see
    the above descriptions of each statistics.',
    tags$br(),
    tableOutput('res_tab_A')
             
             
             ),
    tabPanel("Production",
             
    #### General-production ####
             
    # Description
    tags$strong('General description:'),
    'Plots, map and descriptive statistics about pearl millet production in selected districts for a certain time range.',
    tags$br(),
    tags$br(),
    
    selectInput(inputId = 'P_s_dist', label = 'Select district(s)',
                choices = d_list_nm, multiple = TRUE, selected = 'all'),
    
    sliderInput("P_y_range", label = 'Select year range', min = 1998,
                max = 2017, value = c(1998, 2017), step = 1, ticks = FALSE),
    
    sliderInput("P_y_range_sh", label = 'Select year range for share statistics', min = 1998,
                max = 2015, value = c(1998, 2015), step = 1, ticks = FALSE),
    
    'Select among the following options the statistic that will be ploted on the map below:',
    tags$br(),
    tags$br(),
    'A) average: average production [1000*tons - Ktons] over the selected years', tags$br(),
    'B) average share: proportion of pearl millet production [%] compared
    to other crops averaged over the selected years', tags$br(),
    'C) trend: linear trend or rate of change of the pearl millet production
    [Ktons/year] over the selected years', tags$br(),
    'D) share trend: linear trend or rate of change of the pearl millet share
    production [%/year] over the selected years', tags$br(),
    tags$br(),
    
    selectInput(inputId = 'P_s_var', label = '',
                choices = list(average = 'av', `average share` = 'av_sh',
                               trend = 'tr', `share trend` = 'sh_tr'), selected = 'av_sh'),
    
    hr(),
    tags$strong('Scatter plot of the averaged production [Ktons] over year with linear trend.'),
    tags$br(),
    plotOutput('plot_P1', width = '500px'),
    hr(),
    tags$strong('Scatter plot of the averaged proportion of pearl millet production [%] over year with linear trend.'),
    tags$br(),
    plotOutput('plot_P2', width = '500px'),
    hr(),
    plotOutput('p_map_P', width = '400px', height = '400px'),
    hr(),
    'Descriptive statistics of pearl millet production per district. For the definition of each column see
    the above descriptions of each statistics.',
    tags$br(),
    tableOutput('res_tab_P')
             
             ),
    tabPanel("Yield",
             
    #### General-yield ####
             
    # Description
    tags$strong('General description:'),
    'Plots, map and descriptive statistics about pearl millet yield in selected districts for a certain time range.',
    tags$br(),
    tags$br(),
             
      selectInput(inputId = 'Y_s_dist', label = 'Select district(s)',
                         choices = d_list_nm, multiple = TRUE, selected = 'all'),
             
      sliderInput("Y_y_range", label = 'Select year range', min = 1998,
                         max = 2017, value = c(1998, 2017), step = 1, ticks = FALSE),
            
    'Select among the following options the statistic that will be ploted on the map below:',
    tags$br(),
    tags$br(),
    'A) average: average yield [kg/ha] over the selected years', tags$br(),
    'B) trend: linear trend or rate of change of the pearl millet yield
    [kg/ha/year] over the selected years', tags$br(),
           
      selectInput(inputId = 'Y_s_var', label = '',
                         choices = list(average = 'av', trend = 'tr'), selected = 'tr'),
             
      hr(),
      tags$strong('Scatter plot of the averaged yield [kg/ha] over years with linear trend.'),
      tags$br(),
      plotOutput('plot_Y', width = '500px'),
      hr(),
      plotOutput('p_map_Y', width = '400px', height = '400px'),
      hr(),
    
      'Descriptive statistics of pearl millet yield per districts. For the definition of each column see
      the above descriptions of each statistics.',
      tags$br(),
    
      tableOutput('res_tab_Y')
             
             ),
    "Comparison other crops",
    
    #### Area share ####
    
    tabPanel("Area",
             
    # Description
    tags$strong('General description:'),
    'Pie chart of pearl millet area proportion with other crops for selected districts over a selected period of time.',
    tags$br(),
    tags$br(),
             
    selectInput(inputId = 'AS_s_dist', label = 'Select district(s)',
                         choices = d_list_nm, multiple = TRUE, selected = 'all'),
             
    sliderInput("AS_y_range", label = 'Select year range', min = 1998,
                         max = 2015, value = c(1998, 2015), step = 1, ticks = FALSE),
             
    sliderInput("AS_min_sh", label = 'Select crop min. share [%]', min = 1,
                         max = 20, value = 3, step = 1, ticks = FALSE),
             
    hr(),
    splitLayout(plotOutput('p_dist_AS', width = '210px', height = '250px'),
                plotOutput('pie_AS', width = '350px', height = '250px'), cellWidths = c("40%", "60%"))
             
    
             ),
    
    #### Area competitor ####
    
    tabPanel("Area competitors",
             
             # Description
             tags$strong('General description:'),
             'Time series plot of the main crop cultivated area compared to pearl
             millet in selected districts for selected time frame.',
             tags$br(),
             tags$br(),
             
             selectInput(inputId = 'AC_s_dist', label = 'Select district(s)',
                         choices = d_list_nm, multiple = TRUE, selected = 'all'),
             
             sliderInput("AC_y_range", label = 'Select year range', min = 1998,
                         max = 2015, value = c(1998, 2015), step = 1, ticks = FALSE),
             
             sliderInput("AC_crop_N", label = 'Number of crops', min = 2,
                         max = 6, value = 6, step = 1, ticks = FALSE),
             
             hr(),
             plotOutput('facet_AC'),
             # tableOutput('d_AC') Possibility to add table with values later
             hr(),
             plotOutput('p_dist_AC', width = '210px', height = '250px')
            
             ),
    
    
    
    'Environment',
    
    #### Soil type ####
    
    tabPanel("Soil",
             
             # Description
             tags$strong('General description:'),
             'Map of the majority type of soil per district. For some districts the information is not available.',
             tags$br(),
             tags$br(),
             
             selectInput(inputId = 'S_s_dist', label = 'Select district(s)',
                         choices = d_list_nm, multiple = TRUE, selected = 'all'),
             
             hr(),
             plotOutput('p_soil', width = '470px', height = '450px')
             
             ),
    
    #### Weather rain ####
    
    tabPanel("Weather - rain",
             
             # Description
             tags$strong('General description:'),
             'Map of the average, variance and trend of cumulated rain during the Kharif (rainy) season in
             selected districts for a selected time range.',
             tags$br(),
             tags$br(),
             
             selectInput(inputId = 'R_s_dist', label = 'Select district(s)',
                         choices = d_list_nm, multiple = TRUE, selected = 'all'),
             
             sliderInput("R_y_range", label = 'Select year range', min = 1998,
                         max = 2017, value = c(1998, 2017), step = 1, ticks = FALSE),
             
             
             hr(),
             splitLayout(plotOutput('p_R_av', width = '350px', height = '350px'),
                         plotOutput('p_R_var', width = '350px', height = '350px'),
                         plotOutput('p_R_tr', width = '350px', height = '350px'),
                         cellWidths = c("33%", "33%", "34%")),
             
             tags$br(),
             tableOutput('res_tab_rain')
             
             ),
    
    #### Mgt: irrigation ####
    
    "Management",
    tabPanel("Irrigation",
             
             # Description
             tags$strong('General description:'),
             'Map of the avarage and trend of pearl millet surface under irrigation.',
             tags$br(),
             tags$br(),
             
             selectInput(inputId = 'I_s_dist', label = 'Select district(s)',
                         choices = d_list_nm, multiple = TRUE, selected = 'all'),
             
             sliderInput("I_y_range", label = 'Select year range', min = 1998,
                         max = 2017, value = c(1998, 2017), step = 1, ticks = FALSE),
             
             
             hr(),
             splitLayout(plotOutput('p_I_av', width = '450px', height = '450px'),
                         plotOutput('p_I_tr', width = '450px', height = '450px'),
                         cellWidths = c("50%", "50%")),
             
             tags$br(),
             tableOutput('res_tab_irrig')
             
             ),
    
    #### Crop model comparison ####
    
    "Crop model Prediction",
    tabPanel("Predicted yield and income comparison",
    
    # Description
    tags$strong('General description:'),
    'Comparison of crop model prediction outputs (yield or expected income) for a selection of
    districts averaged over a selected period of time. The results presented are an
    average between two crop models: the reference crop model from APSIM and an updated
    version that integrates more recent biological knowledge about leaf area development and tillering.',
    tags$br(),
    tags$br(),
    
    'The user can constrast two scenarios. For each scenario, he/she can select 
    among the following parameters options:',
    tags$br(),
    tags$br(),
    'A) Soil water content in mm of water per cm of soil depth: 0.6 (~sandy), 0.9 (~loam), or 1.3 (~clay)', tags$br(),
    'B) Soil depth in cm: 60 (shallow), 120 (medium), or 180 (deep)', tags$br(),
    'C) Type of plant variety used: landrace type or commercial hybrid type', tags$br(),
    'D) Sowing date window: 16-30 June (early), 1-15 July (average), 16-30 July (late)', tags$br(),
    'E) Plant density in plant/m2: 12, 18, or 24', tags$br(),
    'F) Irrigation: automatic, intermediate, no irrigation', tags$br(),
    'G) Fertilisation basal dose/20 days after sowing [kg N/ha]:, 0/0, 30/30, 50/50',
    tags$br(),
    tags$br(),
    'The user can also select with crop model output should be ploted on the map:',
    tags$br(),
    tags$br(),
    'A) Expected yield [kg/ha]', tags$br(),
    'B) Expected income [INR/ha]. The expected income is the multiplication of the
    expected yield and the average price per 100 kg at harvest for the five most
    recent years where information is available [2013-2017].
    For some district the price information is not available.', tags$br(),
    tags$br(),
    tags$br(),
    
    # input
    wellPanel('General parameters',
              
              selectInput(inputId = 'CM_s_dist', label = 'Select district(s)',
                          choices = d_list_nm, multiple = TRUE, selected = 'all'),
              
              sliderInput("CM_y_range", label = 'Select year range', min = 1998,
                          max = 2017, value = c(1998, 2017), step = 1, ticks = FALSE),
              
              selectInput(inputId = 'CM_s_var', label = 'Select parameter to plot on the map',
                          choices = list(`Expected yield` = 'exp_yield', `Expected income` = 'exp_income'),
                          selected = 'exp_yield')
              
              
              ),
    
    splitLayout(wellPanel("Option1",
              
              sliderTextInput(inputId = 's_text1', label = 'Soil water content [Wat. mm/cm soil depth]',
                              choices = c('0.6 (~sandy)', '0.9 (~loam)', '1.3 (~clay)'), selected = '0.9 (~loam)'),
              
              sliderTextInput(inputId = 's_depth1', label = 'Soil depth',
                              choices = c('60 cm (shallow)', '120 cm (medium)',
                                          '180 cm (deep)'), selected = '120 cm (medium)'),
              
              radioButtons(inputId = 'variety1', label = 'Variety',
                           choices = list(landrace = 'wrajpop', `commerical hybrid (9444)` = 'PM9444')),
              
              sliderTextInput(inputId = 'sow_d1', label = 'Sowing date',
                              choices = c('early (16-30 June)', 'average (1-15 July)', 'late (16-30 July)'),
                              selected = 'average (1-15 July)'),
              
              sliderTextInput(inputId = 'dens1', label = 'Plant density [plant/m2]',
                              choices = c('12', '18', '24'),
                              selected = '18'),
              
              sliderTextInput(inputId = 'irrig1', label = 'Irrigation',
                              choices = c('no irrigation', 'partial irrigation',
                                          'full irrigation'), selected = 'partial irrigation'),
              
              sliderTextInput(inputId = 'fert1', label = 'Fertilisation',
                              choices = c('no fertilisation', '30/30 kg N', '50/50 kg N'),
                              selected = '30/30 kg N')
              
              ),
    
    wellPanel("Option2",
              
              sliderTextInput(inputId = 's_text2', label = 'Soil water content [Wat. mm/cm soil depth]',
                              choices = c('0.6 (~sandy)', '0.9 (~loam)', '1.3 (~clay)'), selected = '0.9 (~loam)'),
              
              sliderTextInput(inputId = 's_depth2', label = 'Soil depth',
                              choices = c('60 cm (shallow)', '120 cm (medium)',
                                          '180 cm (deep)'), selected = '120 cm (medium)'),
              
              radioButtons(inputId = 'variety2', label = 'Variety',
                           choices = list(landrace = 'wrajpop', `commerical hybrid (9444)` = 'PM9444')),
              
              sliderTextInput(inputId = 'sow_d2', label = 'Sowing date',
                              choices = c('early (16-30 June)', 'average (1-15 July)', 'late (16-30 July)'),
                              selected = 'average (1-15 July)'),
              
              sliderTextInput(inputId = 'dens2', label = 'Plant density [plant/m2]',
                              choices = c('12', '18', '24'),
                              selected = '18'),
              
              sliderTextInput(inputId = 'irrig2', label = 'Irrigation',
                              choices = c('no irrigation', 'partial irrigation',
                                          'full irrigation'), selected = 'partial irrigation'),
              
              sliderTextInput(inputId = 'fert2', label = 'Fertilisation',
                              choices = c('no fertilisation', '30/30 kg N', '50/50 kg N'),
                              selected = '30/30 kg N')
              
              )),
  
    # output
    splitLayout(plotOutput('p_map_CM_o1', width = '300px', height = '300px'),
                plotOutput('p_map_CM_o2', width = '300px', height = '300px')),
    hr(),
    plotOutput('p_map_CM_diff', width = '300px', height = '300px'),
    tableOutput("pred_tab")
             
             )
    
) # end navlist panel

) # end fluid page

    #### server ####

server <- function(input, output) {
  
        #### General area ####
  
  # get the input
  sel_dist_A <- reactive({  unique(unlist(dist_list[input$A_s_dist]))  })
  y_r_A <- reactive({input$A_y_range})
  y_r_cr_A <- reactive({input$A_y_range_sh})
  
  # abs average and trend
  s1_A <- reactive({PM_prod %>% filter(dist_code %in% sel_dist_A(), year %in% y_r_A()[1]:y_r_A()[2]) %>%
    group_by(state_name, dist_code, dist_name) %>%
    summarise(av = m_f(area), tr = tr_f(x = year, y = area))})

  # share average and trend
  s2_A <- reactive({d_cr_area %>% filter(dist_code %in% sel_dist_A(), year %in% y_r_cr_A()[1]:y_r_cr_A()[2]) %>%
    rowwise() %>% mutate(tot_s = sum(c_across(8:ncol(d_cr_area)))) %>%
    mutate(sh = 100*(PearlMillet/tot_s)) %>%
    ungroup() %>% group_by(state_name, dist_code, dist_name) %>%
    summarise(av = m_f(sh), tr = tr_f(x = year, y = sh))})
  
  # Absolute trend plot
  dA <- reactive({PM_prod %>% filter(dist_code %in% sel_dist_A(), year %in% y_r_A()[1]:y_r_A()[2]) %>%
    group_by(year) %>% summarise(y = sum(area, na.rm = TRUE))})
  
  pA <- reactive({ggplot(dA(), aes(x = year, y = y)) + geom_point() +
    geom_smooth(method='lm', formula= y~x) + labs(y = 'area [Kha]',
                                                  title = paste('area', 'trend'))})
  
  # share plot
  
  dA2 <- reactive({
    
    d_i <- d_cr_area %>% filter(dist_code %in% sel_dist_A(), year %in% y_r_cr_A()[1]:y_r_cr_A()[2])
    d_i <- rowsum(x = d_i[, 8:ncol(d_i)], group = d_i$year)
    d_i <- d_i %>% rowwise() %>% mutate(tot_s = sum(c_across(everything()))) %>%
      mutate(sh = 100*(PearlMillet/tot_s))
    d_i <- data.frame(year = y_r_cr_A()[1]:y_r_cr_A()[2], y = d_i$sh)
    return(d_i)
    
  })
  
  pA2 <- reactive({ggplot(dA2(), aes(x = year, y = y)) + geom_point() +
      geom_smooth(method='lm', formula= y~x) + labs(y = paste('PM', 'area', 'share [%]'),
                                                    title = paste('area', 'share trend'))})

  # map plot
  p_map_A <- reactive({
    
    ptype <- input$A_s_var
    
    if(ptype == 'av'){
      
      p_lk <- s1_A()$av
      names(p_lk) <- s1_A()$dist_code
      d_poly$param <- p_lk[as.character(d_poly$dist_code)]
      
      p <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
        geom_polygon(colour='black', aes(fill=param, group = dist)) +
        scale_fill_gradient(low = "white", high = "darkgreen", name = 'area [Kha]') +
        labs(title = paste('Average PM', 'area'), x = 'lon')
      
    } else if (ptype == 'tr') {
      
      p_lk <- s1_A()$tr
      names(p_lk) <- s1_A()$dist_code
      d_poly$param <- p_lk[as.character(d_poly$dist_code)]
      
      p <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
      geom_polygon(colour='black', aes(fill=param, group = dist)) +
      scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue', name = 'trend [Kha/y]') +
      labs(title = paste('area', 'trend'), x = 'lon')
      
      
    } else if(ptype == 'av_sh'){
      
      p_lk <- s2_A()$av
      names(p_lk) <- s2_A()$dist_code
      d_poly$param <- p_lk[as.character(d_poly$dist_code)]
      
      p <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
        geom_polygon(colour='black', aes(fill=param, group = dist)) +
        scale_fill_gradient(low = "white", high = "darkgreen", name = 'area share [%]') +
        labs(title = paste('average PM', 'area', 'share'), x = 'lon')
      
    } else if(ptype == 'sh_tr'){
      
      p_lk <- s2_A()$tr
      names(p_lk) <- s2_A()$dist_code
      d_poly$param <- p_lk[as.character(d_poly$dist_code)]
      
      p <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
        geom_polygon(colour='black', aes(fill=param, group = dist)) +
        scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue', name = 'trend [%/y]') +
        labs(title = paste('area', 'share trend'), x = 'lon')
     
    }
    
    return(p)
    
  })
  
  
  # plot
  output$plot_A1 <- renderPlot({pA()})
  output$plot_A2 <- renderPlot({pA2()})
  output$p_map_A <- renderPlot({p_map_A()})
  
  # table
  res_tab_A <- reactive({left_join(s1_A(), s2_A(), "dist_code") %>% ungroup() %>%
    select(state_name.x, dist_name.x, av.x, tr.x, av.y, tr.y) %>%
      rename(state = state_name.x, district = dist_name.x, `average [Kha]` = av.x,
             `trend [Kha/y]` = tr.x, `av. share [%]` = av.y, `share trend [%/y]` = tr.y)})

  output$res_tab_A <- renderTable({res_tab_A()})
  
        #### General production ####
  
  # get the input
  sel_dist_P <- reactive({  unique(unlist(dist_list[input$P_s_dist]))  })
  y_r_P <- reactive({input$P_y_range})
  y_r_cr_P <- reactive({input$P_y_range_sh})
  
  # abs average and trend
  s1_P <- reactive({PM_prod %>% filter(dist_code %in% sel_dist_P(), year %in% y_r_P()[1]:y_r_P()[2]) %>%
      group_by(state_name, dist_code, dist_name) %>%
      summarise(av = m_f(prod), tr = tr_f(x = year, y = prod))})
  
  # share average and trend
  s2_P <- reactive({d_cr_prod %>% filter(dist_code %in% sel_dist_P(), year %in% y_r_cr_P()[1]:y_r_cr_P()[2]) %>%
      rowwise() %>% mutate(tot_s = sum(c_across(8:ncol(d_cr_prod)))) %>%
      mutate(sh = 100*(PearlMillet/tot_s)) %>%
      ungroup() %>% group_by(state_name, dist_code, dist_name) %>%
      summarise(av = m_f(sh), tr = tr_f(x = year, y = sh))})
  
  # Absolute trend plot
  dP <- reactive({PM_prod %>% filter(dist_code %in% sel_dist_P(), year %in% y_r_P()[1]:y_r_P()[2]) %>%
      group_by(year) %>% summarise(y = sum(prod, na.rm = TRUE))})
  
  pP <- reactive({ggplot(dP(), aes(x = year, y = y)) + geom_point() +
      geom_smooth(method='lm', formula= y~x) + labs(y = 'prod [Ktons]',
                                                    title = 'prod trend')})
  
  # share plot
  
  dP2 <- reactive({
    
    d_i <- d_cr_prod %>% filter(dist_code %in% sel_dist_P(), year %in% y_r_cr_P()[1]:y_r_cr_P()[2])
    d_i <- rowsum(x = d_i[, 8:ncol(d_i)], group = d_i$year)
    d_i <- d_i %>% rowwise() %>% mutate(tot_s = sum(c_across(everything()))) %>%
      mutate(sh = 100*(PearlMillet/tot_s))
    d_i <- data.frame(year = y_r_cr_P()[1]:y_r_cr_P()[2], y = d_i$sh)
    return(d_i)
    
  })
  
  pP2 <- reactive({ggplot(dP2(), aes(x = year, y = y)) + geom_point() +
      geom_smooth(method='lm', formula= y~x) + labs(y = 'PM prod share [%]',
                                                    title = 'prod share trend')})
  
  # map plot
  p_map_P <- reactive({
    
    ptype <- input$P_s_var
    
    if(ptype == 'av'){
      
      p_lk <- s1_P()$av
      names(p_lk) <- s1_P()$dist_code
      d_poly$param <- p_lk[as.character(d_poly$dist_code)]
      
      p <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
        geom_polygon(colour='black', aes(fill=param, group = dist)) +
        scale_fill_gradient(low = "white", high = "darkgreen", name = 'prod [Ktons]') +
        labs(title = 'Average PM prod', x = 'lon')
      
    } else if (ptype == 'tr') {
      
      p_lk <- s1_P()$tr
      names(p_lk) <- s1_P()$dist_code
      d_poly$param <- p_lk[as.character(d_poly$dist_code)]
      
      p <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
        geom_polygon(colour='black', aes(fill=param, group = dist)) +
        scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue', name = 'trend [Ktons/y]') +
        labs(title = 'prod trend', x = 'lon')
      
      
    } else if(ptype == 'av_sh'){
      
      p_lk <- s2_P()$av
      names(p_lk) <- s2_P()$dist_code
      d_poly$param <- p_lk[as.character(d_poly$dist_code)]
      
      p <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
        geom_polygon(colour='black', aes(fill=param, group = dist)) +
        scale_fill_gradient(low = "white", high = "darkgreen", name = 'prod share [%]') +
        labs(title = 'average PM prod share', x = 'lon')
      
    } else if(ptype == 'sh_tr'){
      
      p_lk <- s2_P()$tr
      names(p_lk) <- s2_P()$dist_code
      d_poly$param <- p_lk[as.character(d_poly$dist_code)]
      
      p <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
        geom_polygon(colour='black', aes(fill=param, group = dist)) +
        scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue', name = 'trend [%/y]') +
        labs(title = 'prod share trend', x = 'lon')
      
    }
    
    return(p)
    
  })
  
  
  # plot
  output$plot_P1 <- renderPlot({pP()})
  output$plot_P2 <- renderPlot({pP2()})
  output$p_map_P <- renderPlot({p_map_P()})
  
  # table
  res_tab_P <- reactive({left_join(s1_P(), s2_P(), "dist_code") %>% ungroup() %>%
      select(state_name.x, dist_name.x, av.x, tr.x, av.y, tr.y) %>%
      rename(state = state_name.x, district = dist_name.x, `average [Ktons]` = av.x,
             `trend [Ktons/y]` = tr.x, `av. share [%]` = av.y, `share trend [%/y]` = tr.y)})
  
  output$res_tab_P <- renderTable({res_tab_P()})
  
        #### General yield ####
  
  # get the input
  sel_dist_Y <- reactive({unique(unlist(dist_list[input$Y_s_dist]))  })
  y_r <- reactive({input$Y_y_range})
  
  # abs average and trend
  s1_Y <- reactive({PM_prod %>% filter(dist_code %in% sel_dist_Y(), year %in% y_r()[1]:y_r()[2]) %>%
      group_by(state_name, dist_code, dist_name) %>%
      summarise(av = m_f(yield), tr = tr_f(x = year, y = yield)) %>% ungroup()}) 
  
  
  # Absolute trend plot
  dY <- reactive({PM_prod %>% filter(dist_code %in% sel_dist_Y(), year %in% y_r()[1]:y_r()[2]) %>%
      group_by(year) %>% summarise(y = mean(yield, na.rm = TRUE))})
  
  pY <- reactive({ggplot(dY(), aes(x = year, y = y)) + geom_point() +
      geom_smooth(method='lm', formula= y~x) + labs(y = 'yield [kg/ha]',
                                                    title = 'yield trend')})
  
  output$plot_Y <- renderPlot({pY()})
  
  # map plot
  p_map_Y <- reactive({
    
    ptype <- input$Y_s_var
    
    if(ptype == 'av'){
      
      p_lk <- s1_Y()$av
      names(p_lk) <- s1_Y()$dist_code
      d_poly$param <- p_lk[as.character(d_poly$dist_code)]
      
      p <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
        geom_polygon(colour='black', aes(fill=param, group = dist)) +
        scale_fill_gradient(low = "white", high = "chocolate4",
                            name = 'yield [kg/ha]') +
        labs(title = 'Average PM yield', x = 'lon')
      
    } else if (ptype == 'tr') {
      
      p_lk <- s1_Y()$tr
      names(p_lk) <- s1_Y()$dist_code
      d_poly$param <- p_lk[as.character(d_poly$dist_code)]
      
      p <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
        geom_polygon(colour='black', aes(fill=param, group = dist)) +
        scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue',
                             name = 'trend [kg/ha/y]') +
        labs(title = 'PM yield trend', x = 'lon')
      
    } 
    
    return(p)
    
  })
  
  
  output$p_map_Y <- renderPlot({p_map_Y()})
  
  # table
  res_tab_Y <- reactive({ s1_Y() %>% select(state_name, dist_name, av, tr) %>%
      rename(state = state_name, district = dist_name, `average [kg/ha]` = av,
             `trend [kg/ha/y]` = tr)})

  output$res_tab_Y <- renderTable({res_tab_Y()})
  
        #### Area share ####
  
  # get the input
  sel_dist_AS <- reactive({  unique(unlist(dist_list[input$AS_s_dist]))  })
  y_r_AS <- reactive({input$AS_y_range})
  min_share_AS <- reactive({input$AS_min_sh})
  
  p_AS <- reactive({
    
    sh_d <- d_cr_area %>% filter(dist_code %in% sel_dist_AS(), year %in% y_r_AS()[1]:y_r_AS()[2]) %>% 
      summarise(across(.cols = 8:ncol(d_cr_area), ~ sum(., is.na(.), 0)))
    
    sh_d <- data.frame(crop = colnames(sh_d), tot = unlist(sh_d[1, ])) %>%
      arrange(desc(tot)) %>% mutate(c_per = round(tot/sum(tot)*100, 1)) %>%
      filter(c_per > min_share_AS()) %>% select(crop, c_per)
    
    res_info <- data.frame('Others', round(100 - sum(sh_d$c_per), 1))
    names(res_info) <- colnames(sh_d)
    sh_d <- rbind(sh_d, res_info)
    sh_d$crop <- paste0(sh_d$crop, ' (', sh_d$c_per, '%)')
    sh_d$crop <- factor(sh_d$crop, levels = sh_d$crop)
    
    # Plot selected distircts
    d_poly$param[d_poly$dist_code %in% sel_dist_AS()] <- 'sel.'
    
    p_dist_AS <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
      geom_polygon(colour='black', aes(fill=param, group = dist)) +
      labs(x = 'lon') +
      ggtitle('Selected districts') + theme(legend.position = 'none')
    
    # Pie chart
    bp <- ggplot(sh_d, aes(x="", y=c_per, fill=crop)) + 
      geom_bar(width = 1, stat = "identity")
    
    pie_AS <- bp + coord_polar("y", start=0) + ggtitle(paste('Crop surface share')) +
      theme(axis.text.x=element_blank()) + labs(x = '', y = '') +
      theme(title = element_text(size = 14),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12))
    
    return(list(pie_AS = pie_AS, p_dist_AS = p_dist_AS))
    
  })
  
  output$pie_AS <- renderPlot({p_AS()$pie_AS})
  output$p_dist_AS <- renderPlot({p_AS()$p_dist_AS})
  
  
        #### Area competitors ####
  
  # get the input
  sel_dist_AC <- reactive({  unique(unlist(dist_list[input$AC_s_dist]))  })
  y_r_AC <- reactive({input$AC_y_range})
  n_cr_plot <- reactive({input$AC_crop_N})
  
  p_AC <- reactive({
    
    y_range <- y_r_AC()[1]:y_r_AC()[2]
    n_year <- length(y_range)
    
    data_i <- d_cr_area %>% filter(dist_code %in% sel_dist_AC(), year %in% y_range)
    cr_av <- colMeans(data_i[, 8:ncol(data_i)])
    data_i <- data_i[, c(rep(T, 7), cr_av > 1)]
    
    data_s_y <- c()
    n_crop <- dim(data_i)[2] - 7
    
    for(j in 1:n_crop){
      
      tr_i_av <- tapply(data_i[, j+7], data_i$year, m_f)
      data_s_y <- cbind(data_s_y, tr_i_av)
      
    }
    
    colnames(data_s_y) <- colnames(data_i[8:dim(data_i)[2]])
    
    # determine most important crop
    sc <- cr_av[cr_av > 1]
    sc_nm <- unlist(lapply(strsplit(x = colnames(data_s_y), split =  '_'), `[[`, 1))
    dt <- data.frame(Group = factor(sc_nm), sc = sc)
    
    data_s_cr <- data_s_y
    colnames(data_s_cr) <- sc_nm
    
    dt2 <- dt[order(dt$sc, decreasing = T), ]
    data_s_cr <- data_s_cr[, as.character(dt2$Group)[1:n_cr_plot()]]
    data_s_cr <- data.frame(data_s_cr) %>% select(PearlMillet, everything())
    data_s_cr <- as.matrix(data_s_cr)
    
    cr_lab <- factor(rep(colnames(data_s_cr), each = n_year),
                     levels = colnames(data_s_cr))
    
    dt <- data.frame(crop = cr_lab,
                     year = rep(y_range, n_cr_plot()),
                     sc = c(data_s_cr))
    
    facet_AC <- ggplot(data = dt, aes(x = year, y = sc)) + geom_line(size = 1) +
      facet_wrap(~crop, ncol = 3) + ylab("1000 ha") + ggtitle('Area')
    
    # Plot selected districts
    d_poly$param[d_poly$dist_code %in% sel_dist_AC()] <- 'sel.'
    
    p_dist_AC <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
      geom_polygon(colour='black', aes(fill=param, group = dist)) +
      labs(x = 'lon') +
      ggtitle('Selected districts') + theme(legend.position = 'none')
    
    return(list(facet_AC = facet_AC, p_dist_AC = p_dist_AC))
    
  })
  
  # output$d_AC <- renderTable({p_AC()}) # possibility to also return the values
  output$facet_AC <- renderPlot({p_AC()$facet_AC})
  output$p_dist_AC <- renderPlot({p_AC()$p_dist_AC})
  
        
        #### Soil majority ####
  
  # get the input
  sel_dist_soil <- reactive({  unique(unlist(dist_list[input$S_s_dist]))  })
  
  p_soil <- reactive({
    
    d_p <- PM_prod %>% filter(!is.na(env), dist_code %in% sel_dist_soil()) %>% group_by(dist_name2) %>%
      summarise(soil = unique(soil))
    
    soil_id <- unique(d_p$soil)
    soil_id <- c("VERTISOLS", "UDUPTS/UDALFS", "PSSAMENTS", "ORTHIDS", "USTALF/USTOLLS",
                 "INCEPTISOLS")
    # soil_labs <- c('vert.', 'udup.', 'pssa.', 'orth.', 'usta.', 'incep.')
    n_soil <- length(soil_id)
    
    col_palette <- c("#FFFFFF", "#6B6100", "#9D964B", "#FDAE6B", "#FD8D3C", "#E6550D", "#A63603")
    
    c_id <- paste0('c', 1:n_soil)
    
    d_poly$soil <- rep('', nrow(d_poly))
    
    for(j in 1:n_soil){
      
      d_sel_j <- d_p %>% filter(soil == soil_id[j]) %>% select(dist_name2)
      d_poly$soil[d_poly$dist %in% unlist(d_sel_j[, 1])] <- c_id[j]
      
    }
    
    p_soil <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
      geom_polygon(colour='black', aes(fill=soil, group = dist)) +
      scale_fill_manual(breaks = c("", c_id), values=col_palette,
                        name = "Soil", labels =  c('', soil_id)) +
      ggtitle('Soil type')
    
    return(p_soil)
    
  })
  
  output$p_soil <- renderPlot({p_soil()})
  
  
        #### rain weather ####
  
  # get the input
  sel_dist_R <- reactive({  unique(unlist(dist_list[input$R_s_dist]))  })
  y_r_R <- reactive({input$R_y_range})
  
  p_rain <- reactive({
    
    s_rain <- PM_prod %>% filter(!is.na(env), dist_code %in% sel_dist_R(),
              year %in% y_r_R()[1]:y_r_R()[2]) %>% group_by(dist_name2) %>%
      summarise(sate = unique(state_name),
                dist_name = unique(dist_name),
                av_s = mean(rain, na.rm = TRUE),
                v_d = var(rain, na.rm = TRUE),
                trend = tr_f(x = year, y = rain))
    
    p_caption <- c('Rain average', 'Rain variance', 'Rain trend')
    f_title <- c('rain [mm]', 'rain [mm]', 'rain [mm/y]')
    
    for(i in 1:3){
      
      p_lk <- unlist(s_rain[, i+3])
      names(p_lk) <- s_rain$dist_name2
      d_poly$param <- p_lk[as.character(d_poly$dist)]
      d_poly$param[is.na(d_poly$param)] <- 0
      d_poly$param[!(d_poly$dist_code %in% sel_dist_R())] <- NA
      
      # plot
      
      p <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
        geom_polygon(colour='black', aes(fill=param, group = dist)) +
        scale_fill_gradient(low = "white", high = "darkblue", name = f_title[i]) +
        labs(title = p_caption[i], x = 'lon')
      
      assign(x = paste0('p', i), value = p)
      
      # further process table of values
      s_rain2 <- s_rain[, 2:6]
      colnames(s_rain2) <- c('state', 'district', 'rain av. [mm]', 'rain var. [mm]', 'rain trend [mm/y]')
      s_rain2 <- s_rain2 %>% arrange(state)
      
    }
    
    return(list(p_av = p1, p_var = p2, p_tr = p3, tab = s_rain2))
    
  })
  
  output$p_R_av <- renderPlot({p_rain()$p_av})
  output$p_R_var <- renderPlot({p_rain()$p_var})
  output$p_R_tr <- renderPlot({p_rain()$p_tr})
  output$res_tab_rain <- renderTable({p_rain()$tab})
  
        #### mgt:irrigation ####
  
  # get the input
  sel_dist_I <- reactive({  unique(unlist(dist_list[input$I_s_dist]))  })
  y_r_I <- reactive({input$I_y_range})
  
  p_irrig <- reactive({
    
    s_irrig <- PM_prod %>% filter(!is.na(env), dist_code %in% sel_dist_I(),
                                 year %in% y_r_I()[1]:y_r_I()[2]) %>% group_by(dist_name2) %>%
      summarise(sate = unique(state_name),
                dist_name = unique(dist_name),
                av_s = mean(irrig, na.rm = TRUE),
                trend = tr_f(x = year, y = irrig))
    
    p_caption <- c('Irrig. average', 'Irrig. trend')
    f_title <- c('irrig [%]', 'irrig [%/y]')
    
    for(j in 1:2){
      
      p_lk <- unlist(s_irrig[, j+3])
      names(p_lk) <- s_irrig$dist_name2
      d_poly$param <- p_lk[as.character(d_poly$dist)]
      # d_poly$param[is.na(d_poly$param)] <- 0
      d_poly$param[!(d_poly$dist_code %in% sel_dist_R())] <- NA
      d_poly$param[d_poly$dist_name2 == "SAMBHAL"] <- NA
      
      
      # plot
      
      if(j == 1){
        
        p <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
          geom_polygon(colour='black', aes(fill=param, group = dist)) +
          scale_fill_gradient(low = "white", high = "darkblue", name = f_title[j]) +
          labs(title = p_caption[j], x = 'lon')
        
      } else if (j == 2){
        
        p <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
          geom_polygon(colour='black', aes(fill=param, group = dist)) +
          scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue',
                               name = f_title[j]) +
          labs(title = p_caption[j], x = 'lon')
        
      }
      
      assign(x = paste0('p', j), value = p)
      
    }
    
    
    
    # further process table of values
    s_irrig2 <- s_irrig[, 2:5]
    colnames(s_irrig2) <- c('state', 'district', 'irrig av. [%]', 'irrig trend [%/y]')
    s_irrig2 <- s_irrig2 %>% arrange(state)
    
    return(list(p_av = p1, p_tr = p2, tab = s_irrig2))
    
  })
  
  output$p_I_av <- renderPlot({p_irrig()$p_av})
  output$p_I_tr <- renderPlot({p_irrig()$p_tr})
  output$res_tab_irrig <- renderTable({p_irrig()$tab})
  
        #### CM prediction ####
  
  # get the input
  sel_dist_CM <- reactive({  unique(unlist(dist_list[input$CM_s_dist]))  })
  y_r_CM <- reactive({input$CM_y_range})
  last_year <- 2013:2017
  
  output$sel_dist_CM <- renderText({sel_dist_CM()})
  
  # expected yield option 1
  y_p_opt1 <- reactive({
    
    # get the input
    s_text1 <- unlist(list(`0.6 (~sandy)` = 'sand',
                           `0.9 (~loam)` = 'loam',
                            `1.3 (~clay)` = 'clay')[input$s_text1])
    
    s_depth1 <- unlist(list(`60 cm (shallow)` = 'shallow',
                              `120 cm (medium)` = 'medium',
                              `180 cm (deep)` = 'deep')[input$s_depth1])
    
    irrig1 <- unlist(list(`no irrigation` = 'off', `partial irrigation` = 'int',
                           `full irrigation` = 'on')[input$irrig1])
    
    variety1 <- input$variety1
    
    fert1 <- unlist(list(`no fertilisation` = 'f_0', `30/30 kg N` = 'f_30',
                         `50/50 kg N` = 'f_50')[input$fert1])
    
    sow_d1 <- unlist(list(`early (16-30 June)` = 'early',
                          `average (1-15 July)` = 'average',
                          `late (16-30 July)` = 'late')[input$sow_d1])
    
    dens1 <- unlist(list(`12` = 12,
                         `18` = 18,
                         `24` = 24)[input$dens1])
    
    d <- data_CM %>% filter(dist_code %in% sel_dist_CM(), year %in% y_r_CM()[1]:y_r_CM()[2],
                            s_text == s_text1, s_depth == s_depth1,
                            irrig == irrig1, variety == variety1,
                            fert == fert1, sow_d == sow_d1, dens == dens1) %>%
      group_by(dist_code, dist) %>% summarise(av_yld = mean(y_est, na.rm = TRUE))

    av_price <- PM_prod %>% filter(dist_code %in% sel_dist_CM(), year %in% last_year) %>%
      group_by(dist_code, dist_name) %>% summarise(av_price = mean(price, na.rm = TRUE))

    d <- left_join(x = d, y = av_price, "dist_code") %>%
      mutate(exp_inc = av_yld * av_price/100) %>%
      select(dist_code, dist_name, av_yld, exp_inc) %>% rename(exp_yld = av_yld)

    # d <- d %>% select(dist_code, dist_name, exp_yld, exp_inc)
    
    return(d)
    
  })
  
  # expected yield option 1
  y_p_opt2 <- reactive({

    # get the input
    s_text2 <- unlist(list(`0.6 (~sandy)` = 'sand',
                           `0.9 (~loam)` = 'loam',
                           `1.3 (~clay)` = 'clay')[input$s_text2])
    
    s_depth2 <- unlist(list(`60 cm (shallow)` = 'shallow',
                            `120 cm (medium)` = 'medium',
                            `180 cm (deep)` = 'deep')[input$s_depth2])
    
    irrig2 <- unlist(list(`no irrigation` = 'off', `partial irrigation` = 'int',
                          `full irrigation` = 'on')[input$irrig2])
    
    variety2 <- input$variety2
    
    fert2 <- unlist(list(`no fertilisation` = 'f_0', `30/30 kg N` = 'f_30',
                         `50/50 kg N` = 'f_50')[input$fert2])
    
    sow_d2 <- unlist(list(`early (16-30 June)` = 'early',
                          `average (1-15 July)` = 'average',
                          `late (16-30 July)` = 'late')[input$sow_d2])
    
    dens2 <- unlist(list(`12` = 12,
                         `18` = 18,
                         `24` = 24)[input$dens2])

    d <- data_CM %>% filter(dist_code %in% sel_dist_CM(), year %in% y_r_CM()[1]:y_r_CM()[2],
                            s_text == s_text2, s_depth == s_depth2,
                            irrig == irrig2, variety == variety2,
                            fert == fert2, sow_d == sow_d2, dens == dens2) %>%
      group_by(dist_code, dist) %>% summarise(av_yld = mean(y_est, na.rm = TRUE))

    av_price <- PM_prod %>% filter(dist_code %in% sel_dist_CM(), year %in% last_year) %>%
      group_by(dist_code, dist_name) %>% summarise(av_price = mean(price, na.rm = TRUE))

    d <- left_join(x = d, y = av_price, "dist_code") %>%
      mutate(exp_inc = av_yld * av_price/100) %>%
      select(dist_code, dist_name, av_yld, exp_inc) %>% rename(exp_yld = av_yld)

    # d <- d %>% select(dist_code, dist_name, exp_yld, exp_inc)

    return(d)

  })

  # Combine the two table
  res_tab_CM <- reactive({

    r_tab <- inner_join(x = y_p_opt1(), y = y_p_opt2(), "dist_code")

  # combine and calculate the difference in yield and expected income
  r_tab <- r_tab %>%
    select(dist_code, dist_name.x, exp_yld.x, exp_yld.y, exp_inc.x, exp_inc.y) %>%
    mutate(yld_diff = exp_yld.y - exp_yld.x, inc_diff = exp_inc.y - exp_inc.x) %>%
    relocate(yld_diff, .after = exp_yld.y) %>% relocate(inc_diff, .after = exp_inc.y)

  return(r_tab)
  # return(sel_dist_x)

  })
  
  res_tab_CM_final <- reactive({
    
    state <- PM_prod %>% filter(dist_code %in% res_tab_CM()$dist_code) %>%
      group_by(dist_code) %>% summarise(state = unique(state_name))
    res_tab <- left_join(x = res_tab_CM(), y = state, "dist_code") %>% arrange(state) %>%
      relocate(state, .before = dist_name.x)
    res_tab <- res_tab[, -1]
    colnames(res_tab) <- c('state', 'district', 'exp. yield opt1 [kg/ha]',
                           'exp. yield opt2 [kg/ha]', 'yield diff (o2-o1) [kg/ha]',
                           'exp. inc. opt1 [INR]', 'exp. inc. opt2 [INR]', 
                           'inc. diff (o2-o1) [INR]')
    
    return(res_tab)
    
  })

  output$pred_tab <- renderTable({res_tab_CM_final()})
  
  # map plot
  p_map_CM <- reactive({
    
    ptype <- input$CM_s_var
    
    if(ptype == 'exp_yield'){
      
      # option 1
      
      p_lk <- res_tab_CM()$exp_yld.x
      names(p_lk) <- res_tab_CM()$dist_code
      d_poly$param <- p_lk[as.character(d_poly$dist_code)]
      
      p_o1 <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
        geom_polygon(colour='black', aes(fill=param, group = dist)) +
        scale_fill_gradient(low = "white", high = "chocolate4", name = 'yield') +
        labs(title = 'Predicted yield option 1', x = 'lon')
      
      # option 2
      
      p_lk <- res_tab_CM()$exp_yld.y
      names(p_lk) <- res_tab_CM()$dist_code
      d_poly$param <- p_lk[as.character(d_poly$dist_code)]
      
      p_o2 <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
        geom_polygon(colour='black', aes(fill=param, group = dist)) +
        scale_fill_gradient(low = "white", high = "chocolate4", name = 'yield') +
        labs(title = 'Predicted yield option 2', x = 'lon')
      
      # difference
      
      p_lk <- res_tab_CM()$yld_diff
      names(p_lk) <- res_tab_CM()$dist_code
      d_poly$param <- p_lk[as.character(d_poly$dist_code)]
      
      p_diff <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
        geom_polygon(colour='black', aes(fill=param, group = dist)) +
        scale_fill_gradient2(low = 'red', mid = 'white', high = 'green', name = 'yield diff') +
        labs(title = 'Predicted yield difference option 2 - option 1', x = 'lon')
      
    } else if(ptype == 'exp_income'){
      
      # option 1
      
      p_lk <- res_tab_CM()$exp_inc.x
      names(p_lk) <-res_tab_CM()$dist_code
      d_poly$param <- p_lk[as.character(d_poly$dist_code)]
      
      p_o1 <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
        geom_polygon(colour='black', aes(fill=param, group = dist)) +
        scale_fill_gradient(low = "white", high = "chocolate4", name = 'INR') +
        labs(title = 'Expected income option 1', x = 'lon')
      p_o1
      
      # option 2
      
      p_lk <- res_tab_CM()$exp_inc.y
      names(p_lk) <-res_tab_CM()$dist_code
      d_poly$param <- p_lk[as.character(d_poly$dist_code)]
      
      p_o2 <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
        geom_polygon(colour='black', aes(fill=param, group = dist)) +
        scale_fill_gradient(low = "white", high = "chocolate4", name = 'INR') +
        labs(title = 'Expected income option 2', x = 'lon')
      p_o2
      
      # difference
      
      p_lk <- res_tab_CM()$inc_diff
      names(p_lk) <-res_tab_CM()$dist_code
      d_poly$param <- p_lk[as.character(d_poly$dist_code)]
      
      p_diff <- ggplot(d_poly, aes(x = long, y = lat, group = dist)) + 
        geom_polygon(colour='black', aes(fill=param, group = dist)) +
        scale_fill_gradient2(low = 'red', mid = 'white', high = 'green', name = 'inc. diff') +
        labs(title = 'Income difference option 2 - option 1', x = 'lon')
      p_diff
      
    }
    
    return(list(p_o1 = p_o1, p_o2 = p_o2, p_diff = p_diff))
    
  })
  
  output$p_map_CM_o1 <- renderPlot({p_map_CM()$p_o1})
  output$p_map_CM_o2 <- renderPlot({p_map_CM()$p_o2})
  output$p_map_CM_diff <- renderPlot({p_map_CM()$p_diff})
  
  
}

      #### shiny app execution ####

shinyApp(ui = ui, server = server)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)

# load data (move later in the same folder)
setwd('C:/Users/vince/OneDrive/Documents/WD/Programming/Shiny/data/PM')
load('PM_prod.RData')
load('d_cr_area.RData')
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
    
    selectInput(inputId = 'A_s_dist', label = 'Select district(s)',
                       choices = d_list_nm, multiple = TRUE, selected = 'all'),
    
    sliderInput("A_y_range", label = 'Select year range', min = 1998,
                max = 2017, value = c(1998, 2017), step = 1, ticks = FALSE),
    
    sliderInput("A_y_range_sh", label = 'Select year range for share statistics', min = 1998,
                max = 2015, value = c(1998, 2015), step = 1, ticks = FALSE),
    
    selectInput(inputId = 'A_s_var', label = 'Select parameter to plot on the map',
                choices = list(average = 'av', `average share` = 'av_sh',
                               trend = 'tr', `share trend` = 'sh_tr'), selected = 'av_sh'),
    
    hr(),
    plotOutput('plot_A1', width = '500px'),
    hr(),
    plotOutput('plot_A2', width = '500px'),
    hr(),
    plotOutput('p_map_A', width = '400px', height = '400px'),
    hr(),
    tableOutput('res_tab_A')
             
             
             ),
    tabPanel("Production"),
    tabPanel("Yield",
             
    #### General-yield ####
             
    # Description
             
      selectInput(inputId = 'Y_s_dist', label = 'Select district(s)',
                         choices = d_list_nm, multiple = TRUE, selected = 'all'),
             
      sliderInput("Y_y_range", label = 'Select year range', min = 1998,
                         max = 2017, value = c(1998, 2017), step = 1, ticks = FALSE),
            
             
      selectInput(inputId = 'Y_s_var', label = 'Select parameter to plot on the map',
                         choices = list(average = 'av', trend = 'tr'), selected = 'tr'),
             
      hr(),
      plotOutput('plot_Y', width = '500px'),
      hr(),
      plotOutput('p_map_Y', width = '400px', height = '400px'),
      hr(),
      tableOutput('res_tab_Y')
             
             ),
    "Comparison other crops",
    tabPanel("Area",
             
    #### Area share ####
             
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
    tabPanel("Production"),
    tabPanel("Yield"),
    "Crop model Prediction",
    tabPanel("Predicted yield and income comparison",
             
    #### Crop model comparison ####
    
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
              
              sliderTextInput(inputId = 's_text1', label = 'Soil texture',
                              choices = c('sandy (PAWC/cm = 0.6)', 'loam (PAWC/cm = 0.9)',
                              'clay (PAWC/cm = 1.3)'), selected = 'loam (PAWC/cm = 0.9)'),
              
              sliderTextInput(inputId = 's_depth1', label = 'Soil depth',
                              choices = c('shallow (60 cm)', 'medium (120 cm)',
                                          'deep (180 cm)'), selected = 'medium (120 cm)'),
              
              sliderTextInput(inputId = 'irrig1', label = 'Irrigation',
                              choices = c('no irrigation', 'partial irrigation',
                                          'full irrigation'), selected = 'partial irrigation'),
              
              radioButtons(inputId = 'variety1', label = 'Variety',
                              choices = list(landrace = 'wrajpop', `commerical hybrid (9444)` = 'PM9444')),
              
              sliderTextInput(inputId = 'fert1', label = 'Fertilisation',
                              choices = c('no fertilisation', '30/30 kg N', '50/50 kg N'),
                              selected = '30/30 kg N'),
              
              sliderTextInput(inputId = 'sow_d1', label = 'Sowing date',
                              choices = c('early (16-30 June)', 'average (1-15 July)', 'late (16-30 July)'),
                              selected = 'average (1-15 July)'),
              
              sliderTextInput(inputId = 'dens1', label = 'Plant density',
                              choices = c('12 plant/m2', '18 plant/m2', '24 plant/m2'),
                              selected = '18 plant/m2'),
              
              
              ),
    
    wellPanel("Option2",
              
              sliderTextInput(inputId = 's_text2', label = 'Soil texture',
                              choices = c('sandy (PAWC/cm = 0.6)', 'loam (PAWC/cm = 0.9)',
                                          'clay (PAWC/cm = 1.3)'), selected = 'loam (PAWC/cm = 0.9)'),
              
              sliderTextInput(inputId = 's_depth2', label = 'Soil depth',
                              choices = c('shallow (60 cm)', 'medium (120 cm)',
                                          'deep (180 cm)'), selected = 'medium (120 cm)'),
              
              sliderTextInput(inputId = 'irrig2', label = 'Irrigation',
                              choices = c('no irrigation', 'partial irrigation',
                                          'full irrigation'), selected = 'partial irrigation'),
              
              radioButtons(inputId = 'variety2', label = 'Variety',
                           choices = list(landrace = 'wrajpop', `commerical hybrid (9444)` = 'PM9444')),
              
              sliderTextInput(inputId = 'fert2', label = 'Fertilisation',
                              choices = c('no fertilisation', '30/30 kg N', '50/50 kg N'),
                              selected = '30/30 kg N'),
              
              sliderTextInput(inputId = 'sow_d2', label = 'Sowing date',
                              choices = c('early (16-30 June)', 'average (1-15 July)', 'late (16-30 July)'),
                              selected = 'average (1-15 July)'),
              
              sliderTextInput(inputId = 'dens2', label = 'Plant density',
                              choices = c('12 plant/m2', '18 plant/m2', '24 plant/m2'),
                              selected = '18 plant/m2')
              
              )),
  
    # output
    splitLayout(plotOutput('p_map_CM_o1', width = '300px', height = '300px'),
                plotOutput('p_map_CM_o2', width = '300px', height = '300px')),
    hr(),
    plotOutput('p_map_CM_diff', width = '300px', height = '300px'),
    tableOutput("pred_tab")
             
             ),
  )
)

    #### server ####

server <- function(input, output) {
  
        #### General area ####
  
  # get the input
  sel_dist_A <- reactive({  unique(unlist(dist_list[input$A_s_dist]))  })
  y_r <- reactive({input$A_y_range})
  y_r_cr <- reactive({input$A_y_range_sh})
  # sel_var <- reactive({input$A_s_var})
  
  # abs average and trend
  s1_A <- reactive({PM_prod %>% filter(dist_code %in% sel_dist_A(), year %in% y_r()[1]:y_r()[2]) %>%
    group_by(state_name, dist_code, dist_name) %>%
    summarise(av = m_f(area), tr = tr_f(x = year, y = area))})

  # share average and trend
  s2_A <- reactive({d_cr_area %>% filter(dist_code %in% sel_dist_A(), year %in% y_r_cr()[1]:y_r_cr()[2]) %>%
    rowwise() %>% mutate(tot_s = sum(c_across(8:ncol(d_cr_area)))) %>%
    mutate(sh = 100*(PearlMillet/tot_s)) %>%
    ungroup() %>% group_by(state_name, dist_code, dist_name) %>%
    summarise(av = m_f(sh), tr = tr_f(x = year, y = sh))})
  
  # Absolute trend plot
  dA <- reactive({PM_prod %>% filter(dist_code %in% sel_dist_A(), year %in% y_r()[1]:y_r()[2]) %>%
    group_by(year) %>% summarise(y = sum(area, na.rm = TRUE))})
  
  pA <- reactive({ggplot(dA(), aes(x = year, y = y)) + geom_point() +
    geom_smooth(method='lm', formula= y~x) + labs(y = 'area [Kha]',
                                                  title = paste('area', 'trend'))})
  
  # share plot
  
  dA2 <- reactive({
    
    d_i <- d_cr_area %>% filter(dist_code %in% sel_dist_A(), year %in% y_r_cr()[1]:y_r_cr()[2])
    d_i <- rowsum(x = d_i[, 8:ncol(d_i)], group = d_i$year)
    d_i <- d_i %>% rowwise() %>% mutate(tot_s = sum(c_across(everything()))) %>%
      mutate(sh = 100*(PearlMillet/tot_s))
    d_i <- data.frame(year = y_r_cr()[1]:y_r_cr()[2], y = d_i$sh)
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
  
        #### CM prediction ####
  
  # get the input
  sel_dist_CM <- reactive({  unique(unlist(dist_list[input$CM_s_dist]))  })
  y_r_CM <- reactive({input$CM_y_range})
  last_year <- 2013:2017
  
  output$sel_dist_CM <- renderText({sel_dist_CM()})
  
  # expected yield option 1
  y_p_opt1 <- reactive({
    
    # get the input
    s_text1 <- unlist(list(`sandy (PAWC/cm = 0.6)` = 'sand',
                           `loam (PAWC/cm = 0.9)` = 'loam',
                            `clay (PAWC/cm = 1.3)` = 'clay')[input$s_text1])
    
    s_depth1 <- unlist(list(`shallow (60 cm)` = 'shallow',
                              `medium (120 cm)` = 'medium',
                              `deep (180 cm)` = 'deep')[input$s_depth1])
    
    irrig1 <- unlist(list(`no irrigation` = 'off', `partial irrigation` = 'int',
                           `full irrigation` = 'on')[input$irrig1])
    
    variety1 <- input$variety1
    
    fert1 <- unlist(list(`no fertilisation` = 'f_0', `30/30 kg N` = 'f_30',
                         `50/50 kg N` = 'f_50')[input$fert1])
    
    sow_d1 <- unlist(list(`early (16-30 June)` = 'early',
                          `average (1-15 July)` = 'average',
                          `late (16-30 July)` = 'late')[input$sow_d1])
    
    dens1 <- unlist(list(`12 plant/m2` = 12,
                         `18 plant/m2` = 18,
                         `24 plant/m2` = 24)[input$dens1])
    
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
    s_text2 <- unlist(list(`sandy (PAWC/cm = 0.6)` = 'sand',
                           `loam (PAWC/cm = 0.9)` = 'loam',
                           `clay (PAWC/cm = 1.3)` = 'clay')[input$s_text2])

    s_depth2 <- unlist(list(`shallow (60 cm)` = 'shallow',
                            `medium (120 cm)` = 'medium',
                            `deep (180 cm)` = 'deep')[input$s_depth2])

    irrig2 <- unlist(list(`no irrigation` = 'off', `partial irrigation` = 'int',
                          `full irrigation` = 'on')[input$irrig2])

    variety2 <- input$variety2

    fert2 <- unlist(list(`no fertilisation` = 'f_0', `30/30 kg N` = 'f_30',
                         `50/50 kg N` = 'f_50')[input$fert2])

    sow_d2 <- unlist(list(`early (16-30 June)` = 'early',
                          `average (1-15 July)` = 'average',
                          `late (16-30 July)` = 'late')[input$sow_d2])

    dens2 <- unlist(list(`12 plant/m2` = 12,
                         `18 plant/m2` = 18,
                         `24 plant/m2` = 24)[input$dens2])

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
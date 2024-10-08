ternary_afm = function(output = c('ggplot','plotly'), 
               language = c('en','es')) {
  
  # library(ggplot2)
  # library(ggtern)
  library(magrittr)
  
  # load('data/all_tibbles.rdata')
  
  tb.AFM = tibble::tribble(
    ~IDPoint,    ~F,    ~A,    ~M,
    1L, 31.37, 64.81,  3.82,
    2L, 33.19, 61.92,  4.89,
    3L, 35.03, 59.03,  5.95,
    4L, 36.84, 56.15,  7.02,
    5L,  38.7, 53.24,  8.06,
    6L, 40.49, 50.37,  9.14,
    7L, 42.33, 47.47,  10.2,
    8L, 44.14, 44.59, 11.27,
    9L, 45.98,  41.7, 12.32,
    10L, 47.79, 38.82, 13.39,
    11L,  49.6, 35.94, 14.46,
    12L, 51.45, 33.04, 15.51,
    13L, 53.03, 30.27,  16.7,
    14L, 54.27, 27.68, 18.05,
    15L, 55.07,  25.3, 19.63,
    16L, 55.35, 23.19, 21.46,
    17L, 55.26, 21.26, 23.48,
    18L, 54.61, 19.61, 25.78,
    19L,  53.6, 18.14, 28.26,
    20L, 52.27, 16.82,  30.9,
    21L, 50.71, 15.63, 33.66,
    22L, 48.96, 14.53, 36.51,
    23L, 47.11, 13.48, 39.41,
    24L, 45.05, 12.53, 42.41,
    25L, 42.77,  11.7, 45.53,
    26L, 41.16, 11.07, 47.77
  )
  
  
  # reusable function for creating annotation object
  label <- function(txt) {
    list(
      text = txt, 
      x = 0.1, y = 1,
      ax = 0, ay = 0,
      xref = "paper", yref = "paper", 
      align = "center",
      font = list(size = 15, color = "white"),
      bgcolor = "#b3b3b3", bordercolor = "black", borderwidth = 2
    )
  }
  
  # reusable function for axis formatting
  axis <- function(txt) {
    list(
      title = txt, tickformat = ".0%", tickfont = list(size = 10)
    )
  }
  
  AFM.ternaryAxes <- list(
    aaxis = axis("F"), 
    baxis = axis("A"), 
    caxis = axis("M")
  )

  
  if (any(output == 'ggplot' & language == 'en')) {
    AFM <- ggtern::ggtern(data=tb.AFM,ggtern::aes(A,F,M)) +
      ggplot2::geom_path(color = 'darkred') +
      ggtern::annotate('text', label = 'Tholeiitic', 
                       x=20, y=70, z=15, col='darkred',size=3) + 
      ggtern::annotate('text', label = 'Calc-Alkaline', 
                       x=40, y=20, z=30, col='darkred',size=3) +
      ggplot2::theme_bw() + 
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::labs(T="F",
                    L="A",
                    R="M")
  } else if (any(output == 'ggplot' & language == 'es')) {
    AFM <- ggtern::ggtern(data=tb.AFM,ggtern::aes(A,F,M)) +
      ggplot2::geom_path(color = 'darkred') +
      ggtern::annotate('text', label = 'Toleítica', 
                       x=20, y=70, z=15, col='darkred',size=3) + 
      ggtern::annotate('text', label = 'Calco-Alcalina', 
                       x=40, y=20, z=30, col='darkred',size=3) +
      ggplot2::theme_bw() + 
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::labs(T="F",
                    L="A",
                    R="M")
  } else if (any(output == 'plotly' & language == 'en')) {
    AFM = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.AFM,
        a = ~F, b = ~A, c = ~M, 
        colors = 'transparent',
        type = "scatterternary",
        mode = "lines",
        line = list(color = "darkred"),
        hoverinfo = 'none',
        showlegend = F
      ) %>%
      plotly::add_trace(
        a = c(70,20), b = c(20,40), c = c(15,30),
        text = c('Tholeiitic','Calc-Alkaline'),
        type = "scatterternary",
        mode = "text",
        hoverinfo = 'none',
        textfont = list(family= "Arial", size= 12, color= "darkred"),
        showlegend = F
      ) %>% 
      plotly::layout(
        ternary = AFM.ternaryAxes,
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'AFM',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    AFM = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.AFM,
        a = ~F, b = ~A, c = ~M, 
        colors = 'transparent',
        type = "scatterternary",
        mode = "lines",
        line = list(color = "darkred"),
        hoverinfo = 'none',
        showlegend = F
      ) %>%
      plotly::add_trace(
        a = c(70,20), b = c(20,40), c = c(15,30),
        text = c('Toleítica','Calco-Alcalina'),
        type = "scatterternary",
        mode = "text",
        hoverinfo = 'none',
        textfont = list(family= "Arial", size= 12, color= "darkred"),
        showlegend = F
      ) %>% 
      plotly::layout(
        ternary = AFM.ternaryAxes,
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'AFM',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }
  
  return(AFM)
  
}

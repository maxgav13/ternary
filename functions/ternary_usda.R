ternary_usda = function(output = c('ggplot','plotly'), 
                        language = c('en','es')) {
  
  # library(ggplot2)
  # library(ggtern)
  library(magrittr)
  
  # load('data/all_tibbles.rdata')
  
  tb.usda = data.frame(
    Clay = c(1,0.55,0.4,0.4,0.6,0.55,0.35,0.35,
             0.35,0.2,0.2,0.275,0.35,0.2,0.15,0,0,0.05,0.05,0.2,
             0.15,0.1,0,0,0.1,0,0,0.4,0.275,0.275,0.4,0.275,0.2,
             0.05,0.05,0.275,0.275,0,0,0.125,0.125,0.275,0.6,0.4,
             0.4,0.4,0.275,0.275,0.4,0.125,0,0,0.125),
    Sand = c(0,0.45,0.45,0.2,0,0.45,0.65,0.45,
             0.65,0.8,0.525,0.45,0.45,0.8,0.85,0.7,0.5,0.45,0.525,
             0.525,0.85,0.9,0.85,0.7,0.9,1,0.85,0.45,0.45,0.2,0.2,
             0.45,0.525,0.525,0.45,0.225,0.225,0.5,0.2,0.075,0,0,
             0,0.2,0,0.2,0.2,0,0,0.075,0.2,0,0),
    Silt = c(0,0,0.15,0.4,0.4,0,0,0.2,0,0,
             0.275,0.275,0.2,0,0,0.3,0.5,0.5,0.425,0.275,0,0,0.15,
             0.3,0,0,0.15,0.15,0.275,0.525,0.4,0.275,0.275,0.425,
             0.5,0.5,0.5,0.5,0.8,0.8,0.875,0.725,0.4,0.4,0.6,0.4,
             0.525,0.725,0.6,0.8,0.8,1,0.875),
    Label = c("Clay","Clay","Clay",
              "Clay","Clay","Sandy Clay","Sandy Clay",
              "Sandy Clay","Sandy Clay Loam","Sandy Clay Loam",
              "Sandy Clay Loam","Sandy Clay Loam","Sandy Clay Loam",
              "Sandy Loam","Sandy Loam","Sandy Loam","Sandy Loam",
              "Sandy Loam","Sandy Loam","Sandy Loam","Loamy Sand",
              "Loamy Sand","Loamy Sand","Loamy Sand","Sand",
              "Sand","Sand","Clay Loam","Clay Loam","Clay Loam",
              "Clay Loam","Loam","Loam","Loam","Loam","Loam",
              "Silt Loam","Silt Loam","Silt Loam","Silt Loam",
              "Silt Loam","Silt Loam","Silty Clay","Silty Clay",
              "Silty Clay","Silty Clay Loam","Silty Clay Loam",
              "Silty Clay Loam","Silty Clay Loam","Silt","Silt",
              "Silt","Silt")
  ) %>% 
    dplyr::mutate(Label.es = 
                    dplyr::case_when(
                      Label == 'Clay' ~ "Arcilla",
                      Label == 'Sandy Clay' ~ "Arcilla arenosa",
                      Label == 'Sandy Clay Loam' ~ "Franco arcillo arenoso",
                      Label == 'Sandy Loam' ~ "Franco arenoso",
                      Label == 'Loamy Sand' ~ "Arena franca",
                      Label == 'Sand' ~ "Arena",
                      Label == 'Clay Loam' ~ "Franco arcilloso",
                      Label == 'Loam' ~ "Franco",
                      Label == 'Silt Loam' ~ "Franco limoso",
                      Label == 'Silty Clay' ~ "Arcilla limosa",
                      Label == 'Silty Clay Loam' ~ "Franco arcillo limoso",
                      Label == 'Silt' ~ "Limo"),
                  dplyr::across(Label:Label.es,forcats::as_factor))
  
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
  
  usda.ternaryAxes.en <- list(
    aaxis = axis("Clay"), 
    baxis = axis("Sand"), 
    caxis = axis("Silt")
  )
  
  usda.ternaryAxes.es <- list(
    aaxis = axis("Arcilla"), 
    baxis = axis("Arena"), 
    caxis = axis("Limo")
  )

  
  if (any(output == 'ggplot' & language == 'en')) {
    usda <- ggtern::ggtern(data=tb.usda,ggtern::aes(Sand,Clay,Silt)) +
      ggplot2::geom_polygon(aes(fill=Label,color=Label,group=Label),
                            alpha=0.5) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_brewer(palette = 'Set3') + 
      ggplot2::scale_color_brewer(palette = 'Set3') + 
      ggplot2::labs(title="USDA",
                    fill = "Soil",
                    color = "Soil",
                    T="Clay",
                    L="Sand",
                    R="Silt")
  } else if (any(output == 'ggplot' & language == 'es')) {
    usda <- ggtern::ggtern(data=tb.usda,ggtern::aes(Sand,Clay,Silt)) +
      ggplot2::geom_polygon(aes(fill=Label.es,color=Label.es,group=Label.es),
                            alpha=0.5) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_brewer(palette = 'Set3') + 
      ggplot2::scale_color_brewer(palette = 'Set3') + 
      ggplot2::labs(title="USDA",
                    fill = "Suelo",
                    color = "Suelo",
                    T="Arcilla",
                    L="Arena",
                    R="Limo")
  } else if (any(output == 'plotly' & language == 'en')) {
    usda = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.usda,
        a = ~Clay, b = ~Sand, c = ~Silt, 
        color = ~Label, 
        colors = 'Set3',
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        annotations = label("USDA"), ternary = usda.ternaryAxes.en,
        legend = list(title=list(text='<b> Soil </b>'))
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'usda',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    usda = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.usda,
        a = ~Clay, b = ~Sand, c = ~Silt, 
        color = ~Label.es, 
        colors = 'Set3',
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        annotations = label("USDA"), ternary = usda.ternaryAxes.es,
        legend = list(title=list(text='<b> Suelo </b>'))
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'usda',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }
  
  return(usda)
  
}

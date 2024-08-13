ternary_qap = function(type = c('plutonic','volcanic'),
                       output = c('ggplot','plotly'), 
                       language = c('en','es')) {
  
  opacity = .05
  
  # library(ggplot2)
  # library(ggtern)
  library(magrittr)
  
  # load('data/all_tibbles.rdata')
  
  tb.QAP = tibble::tribble(
    ~Q,   ~A,   ~P, ~Label,                                              ~Plut.en,                                                  ~Plut.es,                                        ~Volc.en,                                       ~Volc.es,
    100,    0,    0,   "1a",                                         "Quartzolite",                                              "Cuarzolita",                                              NA_character_,                                             NA_character_,
    90,   10,    0,   "1a",                                         "Quartzolite",                                              "Cuarzolita",                                              NA_character_,                                             NA_character_,
    90,    0,   10,   "1a",                                         "Quartzolite",                                              "Cuarzolita",                                              NA_character_,                                             NA_character_,
    100,    0,    0,   "1a",                                         "Quartzolite",                                              "Cuarzolita",                                              NA_character_,                                             NA_character_,
    90,   10,    0,   "1b",                               "Quartz-rich granitoid",                               "Granitoide rico en cuarzo",                                              NA_character_,                                             NA_character_,
    60,   40,    0,   "1b",                               "Quartz-rich granitoid",                               "Granitoide rico en cuarzo",                                              NA_character_,                                             NA_character_,
    60,    0,   40,   "1b",                               "Quartz-rich granitoid",                               "Granitoide rico en cuarzo",                                              NA_character_,                                             NA_character_,
    90,    0,   10,   "1b",                               "Quartz-rich granitoid",                               "Granitoide rico en cuarzo",                                              NA_character_,                                             NA_character_,
    90,   10,    0,   "1b",                               "Quartz-rich granitoid",                               "Granitoide rico en cuarzo",                                              NA_character_,                                             NA_character_,
    60,   40,    0,    "2",                             "Alkali feldspar granite",                              "Granito alcalifeldespático",                      "Alkali feldspar rhyolite",                   "Riolita alcalifeldespática",
    20,   80,    0,    "2",                             "Alkali feldspar granite",                              "Granito alcalifeldespático",                      "Alkali feldspar rhyolite",                   "Riolita alcalifeldespática",
    20,   72,    8,    "2",                             "Alkali feldspar granite",                              "Granito alcalifeldespático",                      "Alkali feldspar rhyolite",                   "Riolita alcalifeldespática",
    60,   36,    4,    "2",                             "Alkali feldspar granite",                              "Granito alcalifeldespático",                      "Alkali feldspar rhyolite",                   "Riolita alcalifeldespática",
    60,   40,    0,    "2",                             "Alkali feldspar granite",                              "Granito alcalifeldespático",                      "Alkali feldspar rhyolite",                   "Riolita alcalifeldespática",
    60,   36,    4,   "3a",                                       "Syeno-granite",                                           "Sieno-granito",                                      "Rhyolite",                           "Riolita (Liparita)",
    20,   72,    8,   "3a",                                       "Syeno-granite",                                           "Sieno-granito",                                      "Rhyolite",                           "Riolita (Liparita)",
    20,   52,   28,   "3a",                                       "Syeno-granite",                                           "Sieno-granito",                                      "Rhyolite",                           "Riolita (Liparita)",
    60,   26,   14,   "3a",                                       "Syeno-granite",                                           "Sieno-granito",                                      "Rhyolite",                           "Riolita (Liparita)",
    60,   36,    4,   "3a",                                       "Syeno-granite",                                           "Sieno-granito",                                      "Rhyolite",                           "Riolita (Liparita)",
    60,   26,   14,   "3b",                                       "Monzo-granite",                                           "Monzo-granito",                                    "Rhyodacite",                                    "Riodacita",
    20,   52,   28,   "3b",                                       "Monzo-granite",                                           "Monzo-granito",                                    "Rhyodacite",                                    "Riodacita",
    20,   28,   52,   "3b",                                       "Monzo-granite",                                           "Monzo-granito",                                    "Rhyodacite",                                    "Riodacita",
    60,   14,   26,   "3b",                                       "Monzo-granite",                                           "Monzo-granito",                                    "Rhyodacite",                                    "Riodacita",
    60,   26,   14,   "3b",                                       "Monzo-granite",                                           "Monzo-granito",                                    "Rhyodacite",                                    "Riodacita",
    60,   14,   26,    "4",                                        "Granodiorite",                                            "Granodiorita",                                        "Dacite",                                       "Dacita",
    20,   28,   52,    "4",                                        "Granodiorite",                                            "Granodiorita",                                        "Dacite",                                       "Dacita",
    20,    8,   72,    "4",                                        "Granodiorite",                                            "Granodiorita",                                        "Dacite",                                       "Dacita",
    60,    4,   36,    "4",                                        "Granodiorite",                                            "Granodiorita",                                        "Dacite",                                       "Dacita",
    60,   14,   26,    "4",                                        "Granodiorite",                                            "Granodiorita",                                        "Dacite",                                       "Dacita",
    60,    4,   36,    "5",                                            "Tonalite",                                                "Tonalita",                                 "Plagio-dacite",                                  "Plagidacita",
    20,    8,   72,    "5",                                            "Tonalite",                                                "Tonalita",                                 "Plagio-dacite",                                  "Plagidacita",
    20,    0,   80,    "5",                                            "Tonalite",                                                "Tonalita",                                 "Plagio-dacite",                                  "Plagidacita",
    60,    0,   40,    "5",                                            "Tonalite",                                                "Tonalita",                                 "Plagio-dacite",                                  "Plagidacita",
    60,    4,   36,    "5",                                            "Tonalite",                                                "Tonalita",                                 "Plagio-dacite",                                  "Plagidacita",
    20,   80,    0,   "6*",                      "Quartz alkali feldspar syenite",                     "Sienita alcalifeldespática cuarzosa",               "Quartz alkali feldspar trachyte",           "Traquita cuarzo-alcalifeldespática",
    5,   95,    0,   "6*",                      "Quartz alkali feldspar syenite",                     "Sienita alcalifeldespática cuarzosa",               "Quartz alkali feldspar trachyte",           "Traquita cuarzo-alcalifeldespática",
    5, 85.5,  9.5,   "6*",                      "Quartz alkali feldspar syenite",                     "Sienita alcalifeldespática cuarzosa",               "Quartz alkali feldspar trachyte",           "Traquita cuarzo-alcalifeldespática",
    20,   72,    8,   "6*",                      "Quartz alkali feldspar syenite",                     "Sienita alcalifeldespática cuarzosa",               "Quartz alkali feldspar trachyte",           "Traquita cuarzo-alcalifeldespática",
    20,   80,    0,   "6*",                      "Quartz alkali feldspar syenite",                     "Sienita alcalifeldespática cuarzosa",               "Quartz alkali feldspar trachyte",           "Traquita cuarzo-alcalifeldespática",
    20,   72,    8,   "7*",                                      "Quartz syenite",                                        "Sienita cuarzosa",                               "Quartz trachyte",                            "Traquita cuarzosa",
    5, 85.5,  9.5,   "7*",                                      "Quartz syenite",                                        "Sienita cuarzosa",                               "Quartz trachyte",                            "Traquita cuarzosa",
    5,   61.7,   33.4,   "7*",                                      "Quartz syenite",                                        "Sienita cuarzosa",                               "Quartz trachyte",                            "Traquita cuarzosa",
    20,   52,   28,   "7*",                                      "Quartz syenite",                                        "Sienita cuarzosa",                               "Quartz trachyte",                            "Traquita cuarzosa",
    20,   72,    8,   "7*",                                      "Quartz syenite",                                        "Sienita cuarzosa",                               "Quartz trachyte",                            "Traquita cuarzosa",
    20,   52,   28,   "8*",                                    "Quartz monzonite",                                      "Monzonita cuarzosa",                                 "Quartz latite",                              "Latita cuarzosa",
    5,   61.7,   33.3,   "8*",                                    "Quartz monzonite",                                      "Monzonita cuarzosa",                                 "Quartz latite",                              "Latita cuarzosa",
    5,   33.3,   61.7,   "8*",                                    "Quartz monzonite",                                      "Monzonita cuarzosa",                                 "Quartz latite",                              "Latita cuarzosa",
    20,   28,   52,   "8*",                                    "Quartz monzonite",                                      "Monzonita cuarzosa",                                 "Quartz latite",                              "Latita cuarzosa",
    20,   52,   28,   "8*",                                    "Quartz monzonite",                                      "Monzonita cuarzosa",                                 "Quartz latite",                              "Latita cuarzosa",
    20,   28,   52,   "9*",            "Quartz monzodiorite/monzogabbro",             "Monzodiorita/Monzogabro cuarzoso", "Quartz latite andesite/basalt", "Latiandesita/Latibasalto cuarzoso",
    5,   33.3,   61.7,   "9*",            "Quartz monzodiorite/monzogabbro",             "Monzodiorita/Monzogabro cuarzoso", "Quartz latite andesite/basalt", "Latiandesita/Latibasalto cuarzoso",
    5,  9.5, 85.5,   "9*",            "Quartz monzodiorite/monzogabbro",             "Monzodiorita/Monzogabro cuarzoso", "Quartz latite andesite/basalt", "Latiandesita/Latibasalto cuarzoso",
    20,    8,   72,   "9*",            "Quartz monzodiorite/monzogabbro",             "Monzodiorita/Monzogabro cuarzoso", "Quartz latite andesite/basalt", "Latiandesita/Latibasalto cuarzoso",
    20,   28,   52,   "9*",            "Quartz monzodiorite/monzogabbro",             "Monzodiorita/Monzogabro cuarzoso", "Quartz latite andesite/basalt", "Latiandesita/Latibasalto cuarzoso",
    20,    8,   72,  "10*", "Quartz diorite/gabbro/anorthosite", "Diorita/Gabro/Anortosita cuarzosa",           "Quartz andesite/Tholeiitic basalt",        "Andesita/Basalto toleítico",
    5,  9.5, 85.5,  "10*", "Quartz diorite/gabbro/anorthosite", "Diorita/Gabro/Anortosita cuarzosa",           "Quartz andesite/Tholeiitic basalt",        "Andesita/Basalto toleítico",
    5,    0,   95,  "10*", "Quartz diorite/gabbro/anorthosite", "Diorita/Gabro/Anortosita cuarzosa",           "Quartz andesite/Tholeiitic basalt",        "Andesita/Basalto toleítico",
    20,    0,   80,  "10*", "Quartz diorite/gabbro/anorthosite", "Diorita/Gabro/Anortosita cuarzosa",           "Quartz andesite/Tholeiitic basalt",        "Andesita/Basalto toleítico",
    20,    8,   72,  "10*", "Quartz diorite/gabbro/anorthosite", "Diorita/Gabro/Anortosita cuarzosa",           "Quartz andesite/Tholeiitic basalt",        "Andesita/Basalto toleítico",
    5,   95,    0,    "6",                             "Alkali feldspar syenite",                              "Sienita alcalifeldespática",                      "Alkali feldspar trachyte",                  "Traquita alcalifeldespática",
    0,  100,    0,    "6",                             "Alkali feldspar syenite",                              "Sienita alcalifeldespática",                      "Alkali feldspar trachyte",                  "Traquita alcalifeldespática",
    0,   90,   10,    "6",                             "Alkali feldspar syenite",                              "Sienita alcalifeldespática",                      "Alkali feldspar trachyte",                  "Traquita alcalifeldespática",
    5, 85.5,  9.5,    "6",                             "Alkali feldspar syenite",                              "Sienita alcalifeldespática",                      "Alkali feldspar trachyte",                  "Traquita alcalifeldespática",
    5,   95,    0,    "6",                             "Alkali feldspar syenite",                              "Sienita alcalifeldespática",                      "Alkali feldspar trachyte",                  "Traquita alcalifeldespática",
    5, 85.5,  9.5,    "7",                                             "Syenite",                                                 "Sienita",                                      "Trachyte",                                     "Traquita",
    0,   90,   10,    "7",                                             "Syenite",                                                 "Sienita",                                      "Trachyte",                                     "Traquita",
    0,   65,   35,    "7",                                             "Syenite",                                                 "Sienita",                                      "Trachyte",                                     "Traquita",
    5,   61.7,   33.3,    "7",                                             "Syenite",                                                 "Sienita",                                      "Trachyte",                                     "Traquita",
    5, 85.5,  9.5,    "7",                                             "Syenite",                                                 "Sienita",                                      "Trachyte",                                     "Traquita",
    5,   61.7,   33.3,    "8",                                           "Monzonite",                                               "Monzonita",                                        "Latite",                                       "Latita",
    0,   65,   35,    "8",                                           "Monzonite",                                               "Monzonita",                                        "Latite",                                       "Latita",
    0,   35,   65,    "8",                                           "Monzonite",                                               "Monzonita",                                        "Latite",                                       "Latita",
    5,   33.3,   61.7,    "8",                                           "Monzonite",                                               "Monzonita",                                        "Latite",                                       "Latita",
    5,   61.7,   33.3,    "8",                                           "Monzonite",                                               "Monzonita",                                        "Latite",                                       "Latita",
    5,   33.3,   61.7,    "9",                         "Monzodiorite/Monzograbbro",                              "Monzodiorita/Monzograbro",               "Latite andesite/basalt",                   "Latiandesita/Latibasalto",
    0,   35,   65,    "9",                         "Monzodiorite/Monzograbbro",                              "Monzodiorita/Monzograbro",               "Latite andesite/basalt",                   "Latiandesita/Latibasalto",
    0,   10,   90,    "9",                         "Monzodiorite/Monzograbbro",                              "Monzodiorita/Monzograbro",               "Latite andesite/basalt",                   "Latiandesita/Latibasalto",
    5,  9.5, 85.5,    "9",                         "Monzodiorite/Monzograbbro",                              "Monzodiorita/Monzograbro",               "Latite andesite/basalt",                   "Latiandesita/Latibasalto",
    5,   33.3,   61.7,    "9",                         "Monzodiorite/Monzograbbro",                              "Monzodiorita/Monzograbro",               "Latite andesite/Latite basalt",                   "Latiandesita/Latibasalto",
    5,  9.5, 85.5,   "10",                      "Diorite/Gabbro/Anorthosite",                            "Diorita/Gabro/Anortosita",                             "Andesite/Basalt",                           "Andesita/Basalto",
    0,   10,   90,   "10",                      "Diorite/Gabbro/Anorthosite",                            "Diorita/Gabro/Anortosita",                             "Andesite/Basalt",                           "Andesita/Basalto",
    0,    0,  100,   "10",                      "Diorite/Gabbro/Anorthosite",                            "Diorita/Gabro/Anortosita",                             "Andesite/Basalt",                           "Andesita/Basalto",
    5,    0,   95,   "10",                      "Diorite/Gabbro/Anorthosite",                            "Diorita/Gabro/Anortosita",                             "Andesite/Basalt",                           "Andesita/Basalto",
    5,  9.5, 85.5,   "10",                      "Diorite/Gabbro/Anorthosite",                            "Diorita/Gabro/Anortosita",                             "Andesite/Basalt",                           "Andesita/Basalto"
  ) %>% 
    dplyr::mutate(dplyr::across(Label:last_col(),forcats::as_factor))
  
  Labs.QAP = data.frame(
    Q = c(93.3333333333333,75,40,40,40,40,40,
          12.5,12.5,12.5,12.5,12.5,2.5,2.5,2.5,2.5,2.5),
    A = c(3.33333333333333,12.5,57,46.5,30,13.5,
          3,83.125,67.875,43.75,19.625,4.375,92.625,75.625,48.75,
          21.875,4.875),
    P = c(3.33333333333333,12.5,3,13.5,30,46.5,
          57,4.375,19.625,43.75,67.875,83.125,4.875,21.875,48.75,
          75.625,92.625),
    Label = as.factor(c("1a","1b","2","3a",
                        "3b","4","5","6*","7*","8*","9*","10*","6",
                        "7","8","9","10"))
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
  
  QAP.ternaryAxes <- list(
    aaxis = axis("Q"), 
    baxis = axis("A"), 
    caxis = axis("P")
  )
  
  QAP.pal = c('#D2CBC1','#D0BCA1','#F2A37D','#EDAA8D','#DDAB95','#CFB4A9',
              '#C4BDBA','#EE7F51','#E58D6C','#D2927C','#B9998F','#A59997',
              '#EC6A50','#DB7766','#BF8378','#A98984','#8A8280')
  
  if (any(output == 'ggplot')) {
    QAP <- ggtern::ggtern(data=tb.QAP,ggtern::aes(A,Q,P)) +
      ggplot2::geom_polygon(aes(group=Label),
                            fill='white',
                            color="black",alpha=opacity) +
      ggplot2::geom_text(data=Labs.QAP,aes(label=Label),
                         size=2.5,color="black",show.legend = T) +
      ggplot2::theme_bw() +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      # custom_percent("Percent") +
      # ggplot2::scale_fill_manual(values = QAP.pal) +
      # ggplot2::scale_color_manual(values = QAP.pal) +
      ggplot2::labs(T="Q",
                    L="A",
                    R="P")
  } else if (any(type == 'plutonic' & output == 'plotly' & language == 'en')) {
    QAP = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.QAP,
        a = ~Q, b = ~A, c = ~P, 
        color = ~Label,
        colors = 'transparent',
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        text = ~paste('</br>', Label,
                      '</br>', Plut.en),
        hoveron = 'fills'
      ) %>%
      plotly::add_trace(
        data = Labs.QAP,
        a = ~Q, b = ~A, c = ~P,
        text = ~Label,
        type = "scatterternary",
        mode = "text",
        name = 'ID',
        hoverinfo = 'none',
        textfont = list(family= "Arial", size= 12, color= "black"),
        showlegend = T
      ) %>% 
      plotly::layout(
        ternary = QAP.ternaryAxes
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'QAP',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(type == 'plutonic' & output == 'plotly' & language == 'es')) {
    QAP = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.QAP,
        a = ~Q, b = ~A, c = ~P, 
        color = ~Label,
        colors = 'transparent',
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        text = ~paste('</br>', Label,
                      '</br>', Plut.es),
        hoveron = 'fills'
      ) %>%
      plotly::add_trace(
        data = Labs.QAP,
        a = ~Q, b = ~A, c = ~P,
        text = ~Label,
        type = "scatterternary",
        mode = "text",
        name = 'ID',
        hoverinfo = 'none',
        textfont = list(family= "Arial", size= 12, color= "black"),
        showlegend = T
      ) %>% 
      plotly::layout(
        ternary = QAP.ternaryAxes
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'QAP',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(type == 'volcanic' & output == 'plotly' & language == 'en')) {
    QAP = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.QAP,
        a = ~Q, b = ~A, c = ~P, 
        color = ~Label,
        colors = 'transparent',
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        text = ~paste('</br>', Label,
                      '</br>', Volc.en),
        hoveron = 'fills'
      ) %>%
      plotly::add_trace(
        data = Labs.QAP,
        a = ~Q, b = ~A, c = ~P,
        text = ~Label,
        type = "scatterternary",
        mode = "text",
        name = 'ID',
        hoverinfo = 'none',
        textfont = list(family= "Arial", size= 12, color= "black"),
        showlegend = T
      ) %>% 
      plotly::layout(
        ternary = QAP.ternaryAxes,
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'QAP',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(type == 'volcanic' & output == 'plotly' & language == 'es')) {
    QAP = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.QAP,
        a = ~Q, b = ~A, c = ~P, 
        color = ~Label,
        colors = 'transparent',
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        text = ~paste('</br>', Label,
                      '</br>', Volc.es),
        hoveron = 'fills'
      ) %>%
      plotly::add_trace(
        data = Labs.QAP,
        a = ~Q, b = ~A, c = ~P,
        text = ~Label,
        type = "scatterternary",
        mode = "text",
        name = 'ID',
        hoverinfo = 'none',
        textfont = list(family= "Arial", size= 12, color= "black"),
        showlegend = T
      ) %>% 
      plotly::layout(
        ternary = QAP.ternaryAxes,
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'QAP',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }
  
  return(QAP)
  
}

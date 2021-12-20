# UI elements for References

my_references <- tabItem(
  tabName = "references",
  h2(strong("References")),
  br(),
  tags$ol(
    tags$li(
      p(HTML(
        '<div>',
          'Bates D, Maechler M, Bolker B, Walker S (2015).',
          '<strong>Fitting linear mixed-effects models using lme4.</strong>',
          '<i>Journal of Statistical Software</i>, 67(1), 1-48.',
        '</div>'
      ))
    ),
    tags$li(
      p(HTML(
        '<div>',
          'Hedeker D, Gibbons RD (2006).',
          '<strong>',
            'Longitudinal Data Analysis.',
          '</strong>',
          '<i>John Wiley and Sons, Inc.</i>, Hoboken, New Jersey.',
        '</div>'
      ))
    ),
    tags$li(
      p(HTML(
        '<div>',
          'Jarvis B, Wilrich C, Wilrich P-T (2019).',
          '<strong>',
            'Estimation of the POD function and the LOD of a binary',
            'microbiological measurement method from an interlaboratory experiment.',
          '</strong>',
          '<i>Journal of AOAC International</i>, 102(5), 1617-1623.',
        '</div>'
      ))
    ),
    tags$li(
      p(HTML(
        '<div>',
          'Nakagawa S, Johnson PCD, Schielzeth H (2017).',
          '<strong>',
            'The coefficient of determination R<sup>2</sup> and intra-class',
            'correlation coefficient from generalized linear mixed-effects',
            'models revisited and expanded.',
          '</strong>',
          '<i>Journal of the Royal Society Interface</i>, 14, 20170213.',
        '</div>'
      ))
    ),
    tags$li(
      p(HTML(
        '<div>',
          'Stanish WM, Taylor N (1983).',
          '<strong>',
            'Estimation of the intraclass correlation coefficient for the',
            'analysis of covariance model.',
          '</strong>',
          '<i>The American Statistician</i>, 37(3), 221–224.',
        '</div>'
      ))
    ),
    tags$li(
      p(HTML(
        '<div>',
          'Wang S, Ihrie J (2021).',
          '<strong>',
            'On the estimation of POD and LOD of qualitative microbiological',
            'assays from a multi-laboratory validation study.',
          '</strong>',
          '<i>Journal of AOAC International</i>.'
      ))
    ),
  class = "references-body"
  )
)


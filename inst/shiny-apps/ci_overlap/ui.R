library(shiny)
library(markdown)

navbarPage(
  "Confidence Intervals and p-values",
  #theme = "superhero.bcd2.css",
  tabPanel("Introduction",
#           withMathJax(),
           div("This app permits visualization of the relationship between Confidence intervals and p values.  
             There is a well-known inaccurate perception among students/researchers that in 
             comparing two conditions, if CI’s overlap that means that a test of the mean difference 
             in the two conditions will not be significant with NHST procedures at the same alpha level 
             as the CI is specified.   The misunderstanding is often applied in situations where 
             comparisons between pairs of groups are done visually using a graph such as this 
             bar graph displaying means and CI’s.",br()
           ),
           br(),
           div(img(src="threegroup.PNG", width=400)),
           br(),
           div(
             "\\(\\bullet\\) The misunderstanding probably arises because such a relationship Does exist in tests of means in the standard 1-sample t-test.",br(),
             "\\(\\bullet\\) Comparisons of CI overlap and p values can be made for many different statistics, but here, the focus is on tests of means, the most common application where this confusion arises.",br(),
             "\\(\\bullet\\) In this app, we can visualize the CI-overlap idea in a one-sample test of means (the standard one-sample t-test) and in the independent samples t-test (pooled variance form).  A related plot, based on suggestions from Gardner and Altman is also provided.",br(),
             "\\(\\bullet\\) A literature on which the app is based is found in the references tab.  The two-sample depiction was motivated by plots and ideas from Cumming and Baguley and a literature on “inference by eye”.  The Gardner Altman plots use the “dabestr” package in R.",
             br()
             ),
           div("The user is advised to work through each tab, in order."),
           br(),
           div("Shiny app by", 
               a(href="http://www.albany.edu/psychology/20869.php",target="_blank", 
                 "Bruce Dudek"),align="left", style = "font-size: 9pt")
           ),  # end tabpanel1
  navbarMenu(
    "One Sample t-test: CIs and P values",
    tabPanel(
      "Background",
#      withMathJax(),
      div(
        "\\(\\bullet\\) When the mean of a single sample is compared to a null hypothesis value, the standard NHST inferential method is the one-sample t-test.",br(),
        "\\(\\bullet\\) In this limited 1-sample situation, overlap of the CI with the null hypothesis mean value always coincides with a non-significant t-test.",br(),
        "\\(\\bullet\\) The overlap/pvalue relationship occurs when the CI level is \\(1-\\alpha\\) for the two-tailed t-test.",br(),
        "\\(\\bullet\\) The plot permits comparison of various hypothetical sample mean values with a fixed null hypothesis value.",br(),
        "\\(\\bullet\\) The simulated data comprise a random sample of 15 scores with a standard deviaiton of 15 and the sample mean is chosen in the plot controls.",br(),
        br()
      )
    ), # end tabpanel for 1-sample background info
  tabPanel(
    "1-sample Plots",
    sidebarLayout(sidebarPanel(
      radioButtons(
        "cilevelone",
        "Choose Confidence Interval Level:",
        c("95%" = .95,
          "99%" = .99)
      ),
      radioButtons(
        "samplemeanval",
        "Choose Size of Sample Mean:",
        c("112" = 1,
          "109" = 2,
          "105" = 3,
          "To produce p=.05" = 4,
          "To produce p=.01"= 5)
      ),
      HTML("Show graph elements"),
      checkboxInput("descrone", "Show descriptive info", FALSE),
      checkboxInput("showdata1", "Show data set", FALSE)
    ), # end sidebarPanel
    mainPanel(
      fluidRow(
        align = "left",
        HTML("One Sample T-test (two-sided)"),
        plotOutput("plot1")
      ),
      fluidRow(
        conditionalPanel(
          condition="input.showdata1",
          tableOutput('table1')
        )
      ),
      width=5
    ) # end mainpanel) # end sidebarlayout
    ) # end sidebarLayout
  ) # end tabpanel for 1-sample plots
  ), # end navbarmenu for 1-sample tabs
  
  navbarMenu(
    "Two Sample t-test: CI Overlap and P Values",
    tabPanel(
      "Background",
      div(
        "\\(\\bullet\\) Comparison of means from two independent samples is traditionally done with the independent samples t-test.",br(),
        "\\(\\bullet\\) The data for this plot are simulated so that the std deviations within each of the two groups are exactly 15 (homogeneity of variance).",br(),
        "\\(\\bullet\\) The t-test employed is the 'pooled variance' form but produces the same test outcome as the Welch form since n's are equl in the two groups (15) and within group variances are identical.",br(),
        "\\(\\bullet\\) The user can manipulate the CI level and the size of the mean difference between the two groups.",br(),
        "\\(\\bullet\\) CI levels can be chosen as one of three fixed values - the .83 level produces CIS that just abut when the p value is exactly .05",br(),
        "\\(\\bullet\\) Caveat:  When heterogeneity of variance or unequal sample sizes are present, the relationship betwen p values and CI overlap as visualized here may be slightly different.",
        br()
      )
    ),
  tabPanel(
    "2-sample plots",
    sidebarLayout(sidebarPanel(
      checkboxInput("text2", "Show explanatory text", FALSE),
      radioButtons(
        "cilevel1",
        "Choose Confidence Interval Level:",
        c("95%" = .95,
          "99%" = .99,
          "83% (zero overlap at p=.05)" = .8304815)
      ),
      radioButtons(
        "meandifference",
        "Choose Size of Group Mean Difference:",
        c("10" = 1,
          "15 (cohen's d=1.0)" = 2,
          "To produce p=.05" = 3,
        "So 95% CIs abut each other"=4,
        "So CIs touch means"=5,
        "To Produce p=.01"= 6,
        "To Produce Hedges g=1.0"=7)
      ),
      HTML("Show graph elements"),
      checkboxInput("groupamean", "Group A mean and Difference zero", FALSE),
      checkboxInput("edges", "Show lines at CI edges", FALSE),
      checkboxInput("descr", "Show descriptive info", FALSE),
      checkboxInput("showdata2", "Show data set", FALSE)
      ), # end sidebarPanel
    mainPanel(
      conditionalPanel(
        condition = "input.text2",
        div(
          "\\(\\bullet\\) Explore this plot to visualize the relationship between CI overlap and p value.",br(),
          "\\(\\bullet\\) Choose the CI level and mean difference value to explore the relationship.",br(),
          "\\(\\bullet\\) Add graph elements with the checkboxes to include more visualizations and numeric information.",br(),
          "\\(\\bullet\\) The right hand pane displays the value of the difference between the two group means and the CI displayed there is the CI of the difference, derived from the standard error of the difference.",br(),
          "\\(\\bullet\\) The CI of the difference behaves with a relationsip to p values in a manner identical to a 1-sample test, but the CIs for each individual mean have overlap patterns that don't show that same relationship.",
          br(),br()
        )
      ),
      fluidRow(
        align = "left",
        HTML("Two Sample T-test (two-sided):"),
        plotOutput("plot2")
      ),
      fluidRow(
        conditionalPanel(
          condition="input.showdata2",
          tableOutput('table2')
                  )
      ),
      width=6
      ) # end mainpanel) # end sidebarlayout
    ) # end sidebarLayout
    )# end tabpanel for 2-sample plots
  ), # end navbarmenu for 2-sample example
 
   navbarMenu(
     "Gardner & Altman Style Plots",
    tabPanel(
      "Background",
      div(
        "\\(\\bullet\\) A style of graph recommended by Gardner and Altman (see references).",br(),
        "\\(\\bullet\\) This visualization also examines the two independent samples situation.",br(),
        "\\(\\bullet\\) It focuses on the difference value between the group means or an effect size coefficient.",br(),
        "\\(\\bullet\\) Overlaps of the CI with the zero difference value or zero effect size are not perfectly coincident with p value significance since the CIs here are bootstrapped.",br(),
        "\\(\\bullet\\) This type of plot forces an emphasis on estimation over 'significance'.",
        br()
      )
    ),
    tabPanel(
      "Plots",
      sidebarLayout(sidebarPanel(
        checkboxInput("text3", "Show explanatory text", FALSE),
        radioButtons(
          "ptype",
          "Choose Type of Plot:",
          c("Scale to the Difference in Means" = 1,
            "Scale to Cohen's d" = 2,
            "Scale to the adjusted Hedges g" = 3)
        ),
        radioButtons(
          "meandifferencega",
          "Choose Size of Group Mean Difference:",
          c("10" = 1,
            "15 (cohen's d=1.0)" = 2,
            "To produce p=.05" = 3,
            "So 95% CIs abut each other"=4,
            "So CIs touch means"=5,
            "To Produce p=.01"= 6,
            "To Produce Hedges g=1.0"=7)
        )
      ), # end sidebarPanel
      mainPanel(
          conditionalPanel(
            condition="input.text3",
            div(
              "\\(\\bullet\\)  Gardner and Altman style plot (see references).",br(),
              "\\(\\bullet\\) The left panel shows the raw data for two independent groups - same simulation as the CI visualization in the 2-sample tab.",br(),
              "\\(\\bullet\\) The right had panel is in the scale of the difference between the two means or one of two effect sizes.",br(),
              "\\(\\bullet\\) The CI in the right hand panel is a CI for the mean difference, but it is bootstrapped, not the asymptotic estimate of the two-sample t-test.",br(),
              "\\(\\bullet\\) Note that bootstrapped CI's may not give same visual outcome as shown in the 2-sample panel plots.",br(),
              "\\(\\bullet\\) The gray shaded distribution is a simulted sampling distribution of the difference, based on the bootstrapped SD.",br(),
              "\\(\\bullet\\) The plots may load somewhat slowly due to the bootstrapping methodology",br(),
              "\\(\\bullet\\) This plot uses the 'dabestr' package in R.",
              br()
            )
          ),
       plotOutput("plot3")
        ) # end mainpanel
      ) # end sidebarLayout
    )# end tabpanel for gardner-altman plots
  ), # end navbar menu for gardner-altman plots
  tabPanel(
    "References",
    includeMarkdown('references.md')
  ),
  tabPanel(
    "About",
    includeMarkdown('about.md')
  )
  ) # end navbarpage




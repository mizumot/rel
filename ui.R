library(shiny)
library(shinyAce)



shinyUI(bootstrapPage(

    headerPanel("Cronbach's Coefficient Alpha"),

    mainPanel(
        tabsetPanel(

        tabPanel("Main",

            strong('Option:'),

            checkboxInput("colname", label = strong("The input data includes variable names (header)."), value = T),

            br(),

            p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

            aceEditor("text", value="Item1\tItem2\tItem3\tItem4\n2\t3\t3\t3\n3\t3\t4\t4\n4\t4\t3\t4\n5\t4\t3\t4\n3\t4\t2\t4\n3\t3\t4\t3\n4\t3\t4\t4\n3\t3\t2\t2\n4\t5\t5\t5\n2\t2\t1\t2\n4\t3\t4\t3\n3\t4\t3\t3\n3\t4\t4\t3\n3\t4\t3\t4\n5\t5\t5\t4",
                mode="r", theme="cobalt"),

            br(),

            h3("Basic statistics of the scale (test)"),
            verbatimTextOutput("textarea.out"),

            br(),

            h3("Cronbach's coefficient alpha"),
            verbatimTextOutput("alpha.result.out"),

            br(),

            h3("Histogram"),
            radioButtons("meantotal1", "",
                list("Average" = "mean1",
                     "Total" = "total1"), selected = "mean1"),
            plotOutput("distPlot"),

            br(),

            h3("Box plot with individual data points"),

            radioButtons("meantotal2", "",
                list("Average" = "mean2",
                     "Total" = "total2"), selected = "mean2"),
            plotOutput("boxPlot"),

            br(),

            h3("Test of normality"),
            verbatimTextOutput("testnorm.out"),

            br(),

            h3("Q-Q plot"),
            plotOutput("qqPlot", width="70%"),

            br(),
            br(),

            strong('R session info'),
            verbatimTextOutput("info.out")

            ),


        tabPanel("About",

            strong('Note'),
            p('This web application is developed with',
            a("Shiny.", href="http://shiny.rstudio.com/", target="_blank"),
            ''),

            br(),

            strong('List of Packages Used'), br(),
            code('library(shiny)'),br(),
            code('library(shinyAce)'),br(),
            code('library(psych)'),br(),
            code('library(beeswarm)'),br(),


            br(),

            strong('Code'),
            p('Source code for this application is based on',
            a('"The handbook of Research in Foreign Language Learning and Teaching" (Takeuchi & Mizumoto, 2012).', href='http://mizumot.com/handbook/', target="_blank")),

            p('The code for this web application is available at',
            a('GitHub.', href='https://github.com/mizumot/rel', target="_blank")),

            p('If you want to run this code on your computer (in a local R session), run the code below:',
            br(),
            code('library(shiny)'),br(),
            code('runGitHub("rel","mizumot")')
            ),

            br(),

            strong('Citation in Publications'),
            p('Mizumoto, A. (2015). Langtest (Version 1.0) [Web application]. Retrieved from http://langtest.jp'),

            br(),

            strong('Article'),
            p('Mizumoto, A., & Plonsky, L. (2015).', a("R as a lingua franca: Advantages of using R for quantitative research in applied linguistics.", href='http://applij.oxfordjournals.org/content/early/2015/06/24/applin.amv025.abstract', target="_blank"), em('Applied Linguistics,'), 'Advance online publication. doi:10.1093/applin/amv025'),

            br(),

            strong('Recommended'),
            p('To learn more about R, I suggest this excellent and free e-book (pdf),',
            a("A Guide to Doing Statistics in Second Language Research Using R,", href="http://cw.routledge.com/textbooks/9780805861853/guide-to-R.asp", target="_blank"),
            'written by Dr. Jenifer Larson-Hall.'),

            p('Also, if you are a cool Mac user and want to use R with GUI,',
            a("MacR", href="https://sites.google.com/site/casualmacr/", target="_blank"),
            'is defenitely the way to go!'),

            br(),

            strong('Author'),
            p(a("Atsushi MIZUMOTO,", href="http://mizumot.com", target="_blank"),' Ph.D.',br(),
            'Associate Professor of Applied Linguistics',br(),
            'Faculty of Foreign Language Studies /',br(),
            'Graduate School of Foreign Language Education and Research,',br(),
            'Kansai University, Osaka, Japan'),

            br(),

            a(img(src="http://i.creativecommons.org/p/mark/1.0/80x15.png"), target="_blank", href="http://creativecommons.org/publicdomain/mark/1.0/"),

            p(br())

            )

))
))
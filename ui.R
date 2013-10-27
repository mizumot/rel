

library(shiny)

shinyUI(bootstrapPage(

#shinyUI(pageWithSidebar(

    headerPanel("Cronbach's Coefficient Alpha"),

    mainPanel(
        tabsetPanel(

        tabPanel("Main",

            p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

            aceEditor("text", value="2\t3\t3\t3\n3\t3\t4\t4\n4\t4\t3\t4\n5\t4\t3\t4\n3\t4\t2\t4\n3\t3\t4\t3\n4\t3\t4\t4\n3\t3\t2\t2\n4\t5\t5\t5\n2\t2\t1\t2\n4\t3\t4\t3\n3\t4\t3\t3\n3\t4\t4\t3\n3\t4\t3\t4\n5\t5\t5\t4",
                mode="r", theme="cobalt"),

            br(),

            strong('Option:'),

            checkboxInput("colname", label = strong("Input item names"), value = FALSE),

                # Display this only if the item names are to be shown
                conditionalPanel(condition = "input.colname == true",
                                tags$textarea(id="colname.text", rows=5, "Item1\tItem2\tItem3\tItem4")
                                ),

            br(),

            h3("Basic statistics of the scale (test)"),
            verbatimTextOutput("textarea.out"),

            br(),

            h3("Cronbach's coefficient alpha"),
            verbatimTextOutput("alpha.result.out"),

            br(),

            h3("Histogram"),
            plotOutput("distPlot"),

            br(),

            h3("Box plot with individual data points"),
            plotOutput("boxPlot"),

            br(),

            h3("Test of normality"),
            verbatimTextOutput("testnorm.out"),

            br(),

            h3("Q-Q plot"),
            plotOutput("qqPlot", width="70%")

            ),


        tabPanel("About",

            strong('Note'),
            p('This web application is developed with',
            a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),
            ''),

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

            strong('Recommended'),
            p('To learn more about R, I suggest this excellent and free e-book (pdf),',
            a("A Guide to Doing Statistics in Second Language Research Using R,", href="http://cw.routledge.com/textbooks/9780805861853/guide-to-R.asp", target="_blank"),
            'written by Dr. Jenifer Larson-Hall.'),

            p('Also, if you are a cool Mac user and want to use R with GUI,',
            a("MacR", href="http://www.urano-ken.com/blog/2013/02/25/installing-and-using-macr/", target="_blank"),
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
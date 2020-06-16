###  Load functions required in this app

# first, the function that permits the use of markdown in the "about" tab
# no longer needed
#renderRmd <- function(path, input){
#  return(renderText( {
#    if (!require(knitr))
#      stop("knitr package is not installed")
#    if (!require(markdown))
#      stop("Markdown package is not installed")
#    # shiny:::dependsOnFile(path)
#    contents <- paste(readLines(path, warn = FALSE), collapse = '\n')
#    myenvir <- new.env() # Perhaps use parent.frame() ?!
#    assign('input', input, envir=myenvir)
#    html <- knitr::knit2html(text = contents, fragment.only = TRUE, envir=myenvir)
#    Encoding(html) <- 'UTF-8'
#    return(HTML(html))
#  }))
#}


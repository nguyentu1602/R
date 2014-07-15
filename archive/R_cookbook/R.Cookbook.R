# R Cookbook by Paul Teetor

# Chapter 1: Get started --------------------------------------------------
# To get help with function:
args(pnorm)      # Remind all arguments
example(pnorm())   # Give an example
?pnorm

# To search the supplied documentation
help.search("pattern") # Give all the related topics
??pattern              # Give the same
help.search("adf.test")
help.search("dickey-fuller")

# To get help on a package
help(package='cluster')

# To extract vignette of some packages, use: (a pdf will pop up at the end)
vignette(package='zoo')
vignette('zoo-read')

# To search the web for R sites:
RSiteSearch("canonical correlation")
# http://rseek.org
# http://crantastic.org     - offer summaries of packages


# Chapter 2: Some basics --------------------------------------------------

# To list variables, use ls.str()
ls.str()
ls()
# To remove all variables:
rm(list=ls())

# To get basic stats, we can use mean(), sd(), var(), cor(), cov() with data frames
# To create sequences, use seq() and rep()
seq(1,10,by=2)
seq(from=0, to=20, length.out=5)
rep(c(1,'2'), times=5)

# To compare 2 vectors:

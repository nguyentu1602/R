# Chapter 1: Data, Modes & classes ----------------------------------------
mode()
class()
typeof()
# Matrices or other arrays require all the data contained to be in the same mode
# Lists and dataframes allow for multiple modes within an object
# The most primitive mode of data is char
x = c(1,2,5,10, TRUE)
mode(x)
y = c(1,2,"cat", 5)
mode(y)
z = c(x, y) #You can combine vectors to make a new vactor
# To name the elements of vectors, there are 2 ways:
x = c(one=1, two=2, three=3)
y = c(1,2,3)
names(y) = c('one','two', 'three')
# To make a matrix:
rmat = matrix(rnorm(15), 5, 3, dimnames= list(NULL, c('A', 'B', 'C')))
# To change the names of columns in matrix
dimnames(rmat) = list(NULL,c('A','B','C'))    
# To call a column in a matrix
rmat[,'A'] 
# To make a list
mylist = list(c(1,4,6),"dog",3,"cat",TRUE,c(9,10,11))
# To call/change a element in a list
mylist[1]
mylist[4] = 'catttt'
# To name list's elements: 2 ways
mylist = list(first=c(1,3,5), second=c('one','three','five'),third='end')
  # or
mylist = list(c(1,3,5),c(’one’,’three’,’five’),’end’)
names(mylist) = c(’first’,’second’,’third’)

# A data frame is a list that each element (the variables) must be of the same length as every other element
# Some function to test mode:   is.list, is.factor, is.numeric, is.data.frame, and is.character

# To view structure of a nested list, use str()
nestlist = list(a=list(matrix(rnorm(10),5,2),val=3),
                b=list(sample(letters,10),values=runif(5)),
                c=list(list(1:10,1:20),list(1:5,1:10)))
str(nestlist)

# To convert types of objects, use as.
nums = c(12,10,8,12,10,12,8,10,12,8)
tt = table(nums)
names(tt)
sum(as.numeric(names(tt)) * tt)

# To test missing values, use is.na


# Chapter 2: Reading and Writing Data -------------------------------------
# 2.1 Reading Vectors and Matrices

# To use standard input:
names = scan(what="") # A prompt will show up
# To entry a record:
names = scan(what=list(a=0,b="",c=0))
1 dog 3

# To read into a matrix through a prompt, use a nested scan()
mymat = matrix(scan(),ncol=3,byrow=TRUE) # Notice the byrow = TRUE

# To skip fields while reading in data with scan(), use NULL to pass on 'what'
# Suppose we only need to read the contents of the first, third, and tenth fields:
toskip = c(f1=0,NULL,f3=0,rep(list(NULL),6),f10=0)
values = scan(filename, what=toskip) # Code will not run btw
result = cbind(values$f1,values$f3,values$f10)

# 2.2 Data frame: read.table(), which always returns a data frame and automatically convert data
  # To avoid conversion, use as.is = TRUE

# 2.3 Comma and tab-delimited input files: use read.csv(), read.csv2(), read.delim()
# 2.4 Fixed-width input files: use read.fwf()
# 2.5 Extracting from R objects: use a set of appropriate functions with each class
  # use apropos() to find all such function, for example:
apropos('.*\\.lm$')
apropos('.*\\.anova$')
  # use names() to get all elements in each objects, then extract using & or []:
  #   slm$df.residual is equivalent to slm['df.residual']
# 2.6 & 2.7 Connections: read from files or server - page 23. Will come back
# 2.8 Generating data
  # Sequences
1:10
seq(1,10)
1:10 - seq(1,10)
seq(10,100,5)
seq(10, by=5, length=10)
  # Generating levels: use gl():
thelevels = data.frame(group=gl(3,10,length=30),
                       subgroup=gl(5,2,length=30),
                       obs=gl(2,1,length=30))
  # expand.grid() - very cool!
oe = expand.grid(odd=seq(1,5,by=2), even=seq(2,5,by=2))

  # Random numbers: eg: rbinom() or rlogis() etc... Full table page 29
# 2.9 Permutations: use sample() and factorial(n)
# 2.10 Working with sequences: 
# 2.11 Spreadsheets: RODBC (win only) and gdata packages
# 2.12 Saving into .RData or .rda files: use save() and load()
save(list=c('x','y','z'),file='mydata.rda')
load('mydata.rda')
# 2.14 Writing into files in ASCII format: write(), write.table() and write.csv()
  # The write function accepts an R object and the name of a file or connection object, 
  # and writes an ASCII representation of the object to the appropriate destination. 
a = c(one=1,two=2,three=3)
write(a, file ='haha.txt')
write.csv(t(a), file='haha.csv', row.names=FALSE) # Transpose first and exclude row.names


# Chapter 5: Factors ------------------------------------------------------
# 5.1 Using factors:
  # Factors represent a very efficient way to store character values, 
  # because each unique character value is stored only once, 
  # and the data itself is stored as a vector of integers.
# To create a factor, use factor()
# To see the levels of the factor, use levels()
data = c(1,2,2,3,1,2,3,3,1,2,3,3,1)
fdata = factor(data)
# To assign roman numerals
rdata = factor(data, labels = c('I', 'II', 'III'))
# To convert default factor fdata to roman numerals, use levels()
levels(fdata) = c('I', 'II', 'III')
# To convert ordinary vector to factor, use factor():
mons = c('Mar','Apr','Jan','Feb','Mar','May','Jan','Mar')
mons = factor(mons)
table(mons)
# To create an ordered factor, use field ordered=TRUE. Example on page 69
# 5.2 Numeric Factors:
  # To convert the factor back to its original numeric values will enable arimethic
fert = c(10,20,20,50,10,20,10,50)
fert = factor(fert, levels=c(10,20,50), ordered=TRUE)
levels(fert)[fert] # Call the levels, then the [] part makes it into the long list
mean(as.numeric(levels(fert)[fert])) # Either way works
mean(as.numeric(as.character(fert)))

# 5.3 Manipulating factors
lets = sample(letters,size=100,replace=TRUE) # The letters vector is part of R base
lets = factor(lets)
table(lets[1:15])
# To drop levels without any count, use drop=TRUE
table(lets[1:15, drop=TRUE])
  # To combine factors, they should first be converted back to their original values (through the levels function), 
  # then catenated and converted to a new factor, because c() will interpret the factors as integers
fact1 = factor(sample(letters,size=10,replace=TRUE))
fact2 = factor(sample(letters,size=10,replace=TRUE))
fact12 = factor(c(levels(fact1)[fact1], levels(fact2)[fact2])) # Combining

# 5.4 Creating factors from continuous var
# 5.5 Factors based on Dates and Times
# 5.6 Interactions


# Chapter 6: Subscripting -------------------------------------------------
# 6.1-6.2: Numeric subscripts: use to access the elements of a vector, array or list
  # Negative subscripts in R extract all of the elements of an object except the ones 
  # specified in the negative subscript; thus, when using numeric subscripts,   
  # subscripts must be either all positive (or zero) or all negative (or zero).

# 6.3 Character Subscripts
# 6.4 Logical Subscripts
nums = c(12,9,8,14,7,16,3,2,9)
nums > 10
# To get all the elements > 10:
nums[nums > 10]
# To get all the subscripts of the elements for which > 10
which(nums>10)
seq(along=nums)[nums > 10]
# To modify using subscript
nums[nums > 10] = 0

# 6.5 Subscripting Matrices and Arrays
A = matrix(1:12,4,3)
# To transpose a matrix use t()
B = t(A)
# To treat a matrix like a vector
B[10] - B[1,4]
# To get a sub-matrix
B[2:3,1:2]
# To use do.call: do exercise page 81

# 6.6 Specialized functions for matrices
method1 = c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4)
method2 = c(1,2,2,3,2,2,1,3,3,3,2,4,1,4,4,3)
# To make a contigency table:
tt = table(method1, method2)
# To extract all the off-diagonal elements, need several steps
offd = row(tt) != col(tt)
tt[offd]
sum(tt[offd])    # E.g: sum of the off-diagonal elements
# To extract the upper and lower triangular elements of the matrix
A[lower.tri(A)] <- NA
A[upper.tri(A)] <- 0
C = matrix(1:16,nrow=4,ncol=4)
C[lower.tri(C,diag=TRUE)] <- NA

# 6.7 Lists: most general way to store a collection of objects in R
simple = list(a=c('fred','sam','harry'),b=c(24,17,19,22))
simple$a[-2]
# Note: simple[2] is not a vector, it's a list containing the vector
simple[2]
mode(simple[2])
# To do calculation, there are 2 ways to fix this:
  # 1st: $ opperator
mode(simple$b)
mean(simple$b)
  # 2nd: [[]] operator
mean(simple[[2]])
mean(simple[['b']])
  # Note: single brackets will always return a list containing the selected elements,
    # While [[]] returns the actual contents of selected list elements: E.g.
simple[1]
simple[[1]]

# 6.8 Subscripting data frames
# Data frames are a cross between a list and a matrix - so both technique applied
  # However, when a single subscript is used with a data frame, it behaves like a list rather than a vector
dd = data.frame(a=c(5,9,12,15,17,11),b=c(8,NA,12,10,NA,15))
dd[dd$b > 10,]
# To extract a subset in a dataframe - 1st way
dd[!is.na(dd$b) & dd$b > 10,]

# To extract a subset in a dataframe - 2nd way
subset(dd, b>10)
# To extract only certain values of certain columns: use select=
some = subset(LifeCycleSavings,sr>10,select=c(pop15,pop75))
# To extract from a range of column, use the : operator:
life1 = subset(LifeCycleSavings,select=pop15:dpi)
life1 = subset(LifeCycleSavings,select=1:3)

# To extract from a range of columns, use:
life2 = subset(LifeCycleSavings, select=c(-pop15,-pop75))
life2 = subset(LifeCycleSavings, select=c(2,3))
  ## Note: subset() always returns a new data frame, so it's not suited for modifying existing old frames


# Chapter 7: Character Manipulation ---------------------------------------
# 7.1 Basics of Character data
# To find the lengths of each string elements
state.name
nchar(state.name)
# 7.2 Displaying and concatenating character strings
# To combine character values and print them to the screen or a file directly
x = 7
y = 10
cat('x should be >= y, but x=',x,'and y=',y,'\n')

# To automatically insert \n into the output string, use field=
cat('Long strings can','be displayed over',
    'several lines using','the fill= argument', fill=40)

# To print into a specific file, use file=, then use append=TRUE to append existing file
# To join together several strings in specific ways, use paste()
paste('one',2,'three',4,'five')
paste('one',2,'three',4,'five', sep='-|-')
paste(c('one','two','there','four'), collapse = ' ') # sep= will not work with vector here
paste(c('one','two','there','four'), collapse = ' hahuhe ')

# To generate variable names with a common prefix: (recycling shorter elements)
paste('X',1:5,sep='') # This gives a vector of strings
paste(c('X','Y'),1:5, sep='')
  # Can use both sep= and collapse=
paste(c('X','Y'),1:5,sep='_',collapse='|')

# 7.3 Working with Parts of Character Values
# To extract or to change the values of parts of character strings, use substring()
substring(state.name,2,6)

mystring = 'dog cat duck'
substring(mystring,c(1,5,9),c(3,7,12))
substring(mystring,5,7) = 'feline'
substring(mystring,5,7) = 'a'

# 7.4 Regular Expressions in R:
  # Regular expressions are a method of expressing patterns in character values 
  # which can then be used to extract parts of strings or to modify those strings.

  # Regular expressions are supported in the R functions: strsplit, grep, sub, gsub, regexpr, gregexpr
  # E.g:
expr = '.*\\.txt'
nchar(expr)
cat(expr, '\n')

# To avoid the use of double backlashes, use readline()
expr = readline()
nchar(expr)

# 7.5 Basics of regular expressions: [page 92]
# Regular expressions are composed of three components: 
  #   literal characters, which are matched by a single character; 
  #   character classes, which can be matched by any of a number of characters, and
  #   modifiers, which operate on literal characters or character classes.
    #Table 7.1 summarize all the modifiers

# 7.6 Breaking apart character values
  # To divide a character string into smaller pieces: use strsplit()
sentence = 'R is a free software envi for statistical computing'
parts = strsplit(sentence,split=' ') # The result is a list
parts[[1]]
parts[[1]][2]
length(parts)
length(parts[[1]])

# To use strsplit() with a vector of strings, use sapply()
more = c('R is a free software environment for statistical computing',
         'It compiles and runs on a wide variety of UNIX platforms')
result = strsplit(more, ' ')
sapply(result, length)

# To turn the outputs from list of lists, use 
result.vec = unlist(result)
length(result.vec)

# To deal with extra spaces in strings, twist the split='' a little bit
str = 'one  two   three four'
strsplit(str,split=' +')   # The plus sign represents one or more blanks

# To split words into characters:
words =c('one two','three four')
strsplit(words,'')

# 7.7 Using regular expression in R
# To extract a set of variables from a data frame based on their names
grep('^pop',names(LifeCycleSavings))   # The '^pop' part serves as the pattern to search
grep('^pop',names(LifeCycleSavings), value=TRUE)  # Extract the real names
LifeCycleSavings[,grep('^pop',names(LifeCycleSavings))]  # Combine with subscript - extract columns

# To find regular expressions without regard to the case of the inputs, use ignore.case= TRUE
inp = c('run dog run','work doggedly','CAT AND DOG')
grep('\\<dog\\>',inp,ignore.case=TRUE,value=TRUE)  
    # Surrounding a string with '\\<    \\>' restrict matches to the case where the string is surrounded
    # by either white space, punctuation or a line ending or beginning





x = c (1,2,3)
y = c (1,2,"cat",3)
mode(y)
class(y)
typeof(y)
# Assign names
z <- c(one = 1, two =2, three =3)
names(x) = c ('one', 'two', 'three')
# Matrix
rmat = matrix(rnorm(15), 5, 3, 
              dimnames = list(NULL, c('A','B', 'C')))
dimnames(rmat) = list(NULL, c('CA','BU', 'EC'))

# List
mylist = list(c(1,2,4), 'dog', 3, 'cat', TRUE, c (4,5,10))
sapply(mylist, mode)
names(mylist) = c ('first', 'second', 'third', 'baha', 'opps')

mylist1 = list(a=c(1,2,3),b=c("cat","dog","duck"),
               d=factor("a","b","a"))
# Use summary is one of the best way to understand a data object
# Nested list might create trouble, which then solved with str
nestlist = list(a=list(matrix(rnorm(10),5,2),val=3),
                b=list(sample(letters,10),values=runif(5)),
                c=list(list(1:10,1:20),list(1:5,1:10)))
str(nestlist)
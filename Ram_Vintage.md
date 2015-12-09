# ***Vintage Data Analysis by Ramakrishna B***
##1. First we need to load the excell data into R.
### Install and load package readxl.


```r
library("readxl");
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(stringr);
library(ggplot2);
```

```
## 
## Attaching package: 'ggplot2'
## 
## The following object is masked _by_ '.GlobalEnv':
## 
##     diamonds
```

##2. Now define the path and load the vintage file into R.


```r
path = "C:/Users/rbellana/Desktop";
setwd(path);
getwd();
```

```
## [1] "C:/Users/rbellana/Desktop"
```

```r
vintage <- read_excel("Vintage Art V2.1.xlsx",sheet = "Art-i-stick");
```

```
## Error: 'Vintage Art V2.1.xlsx' does not exist in current working directory ('C:/Users/rbellana/Desktop').
```

```r
head(vintage);
```

```
## Error in head(vintage): object 'vintage' not found
```

##3. Now studying the data.

```r
dim(vintage);   
```

```
## Error in eval(expr, envir, enclos): object 'vintage' not found
```

```r
names(vintage); 
```

```
## Error in eval(expr, envir, enclos): object 'vintage' not found
```

```r
str(vintage);  
```

```
## Error in str(vintage): object 'vintage' not found
```

```r
head(vintage);
```

```
## Error in head(vintage): object 'vintage' not found
```

##4. Now we want to split the height and widht of the Art Piece size.

```r
s = str_split(vintage$"Art Piece Size ", "X");
```

```
## Error in stri_split_regex(string, pattern, n = n, simplify = FALSE, opts_regex = attr(pattern, : object 'vintage' not found
```

```r
head(s);
```

```
## Error in head(s): object 's' not found
```
			 #####Sample for spliting.
                  #a = "2 X 3";      
                  #s = str_split(a,"X")
				  
				  
##5. Now extracting width and height elements of the column separately. So we use User Defined functions.

```r
get_x= function(ele){
              ele[1]
}
get_y= function(ele){
              ele[2]
}
```

### Applying user defined function for the entire column.

```r
x=sapply(s,FUN=get_x);
```

```
## Error in lapply(X = X, FUN = FUN, ...): object 's' not found
```

```r
y=sapply(s,FUN=get_y);
```

```
## Error in lapply(X = X, FUN = FUN, ...): object 's' not found
```

```r
head(x)
```

```
##             [,1]       [,2]       [,3]        [,4]       [,5]        [,6]
## [1,] -0.19139217         NA -0.3340366 -0.37723765 -0.8473501          NA
## [2,] -0.78190665  0.2527501         NA  0.09761946 -0.2606394  0.04324404
## [3,]          NA -1.1719483  0.6366744          NA -0.4144197 -0.33265732
## [4,]  0.75050145  0.6687143 -0.1084317 -0.87559247 -0.1830508 -1.82223542
## [5,]          NA -1.6501009  0.5137628  0.12176000  0.4070561          NA
## [6,]  0.08005964 -0.3658522  0.3992718          NA  0.6246331 -0.83758243
##             [,7]       [,8]      [,9]      [,10]
## [1,] -2.73221952 -0.5380708 0.4852268  0.4180578
## [2,] -0.09979059 -2.8557587 0.6967688 -0.4002352
## [3,]  0.97603173 -0.7896469 0.1855139         NA
## [4,]  0.41386892  0.4878146 0.7007335 -1.6070809
## [5,]  0.91232216         NA 0.3116810 -0.4157518
## [6,]          NA  0.5006946 0.7604624  0.4220084
```

```r
head(y)
```

```
## [1] 0.0002 0.0012 0.0060 0.0300 0.0900 0.2700
```

## 6. We are getting as "17in. ",where "in." and space is not required.

```r
x = str_replace(x,"in. "," ");
y = str_replace(y,"in."," ");
```

##7. Now remove the spaces using trim function.

```r
x = str_trim(x);
y = str_trim(y);
```

##8.Now change into integer from character form.

```r
vintage$x = as.integer(x)
```

```
## Error in vintage$x = as.integer(x): object 'vintage' not found
```

```r
vintage$Y = as.integer(y)
```

```
## Error in vintage$Y = as.integer(y): object 'vintage' not found
```

##9. Column names should be like one word.So replace space with "_".

```r
names(vintage) = str_trim(names(vintage));
```

```
## Error in stri_trim_both(string): object 'vintage' not found
```

```r
names(vintage) = str_replace_all(names(vintage)," ","_");
```

```
## Error in stri_replace_all_regex(string, pattern, replacement, vectorize_all = vec, : object 'vintage' not found
```


##10. Now arranging the data with X on X axis and y on Y axis

```r
vintage = arrange(vintage,x,y);
```

```
## Error in arrange_(.data, .dots = lazyeval::lazy_dots(...)): object 'vintage' not found
```

##11. Now analyzing the height and width of the paints done on basis of brush size and painter.

```r
g3 = ggplot(data= vintage,aes(x=x,y=y,size=vintage$Brush_Size,color=vintage$Brush_Size));
```

```
## Error in ggplot(data = vintage, aes(x = x, y = y, size = vintage$Brush_Size, : object 'vintage' not found
```

```r
g3 = g3 + geom_point();
```

```
## Error in eval(expr, envir, enclos): object 'g3' not found
```

```r
g3 = g3 + facet_grid(.~Top_3_artists);  
```

```
## Error in eval(expr, envir, enclos): object 'g3' not found
```

```r
print(g3);
```

```
## Error in print(g3): object 'g3' not found
```

##12.

```r
names(vintage)
```

```
## Error in eval(expr, envir, enclos): object 'vintage' not found
```

```r
print(g4)
```

```
## Error in print(g4): object 'g4' not found
```


##13. Top 3 Artistist

```r
unique(vintage$"Top_3_artists")
```

```
## Error in unique(vintage$Top_3_artists): object 'vintage' not found
```

```r
t = table(vintage$"Top_3_artists")
```

```
## Error in table(vintage$Top_3_artists): object 'vintage' not found
```

```r
barplot(sort(t),horiz = T)
```

```
## Error in sort.int(x, na.last = na.last, decreasing = decreasing, ...): 'x' must be atomic
```

```r
##14.Count of art by Artists:
### default "stat" for "bars" is 'bin" that gives the count value
```

```r
g = ggplot(data=vintage,aes(x=vintage$"Top_3_artists"))
```

```
## Error in ggplot(data = vintage, aes(x = vintage$Top_3_artists)): object 'vintage' not found
```

```r
g = g +  geom_bar(stat="bin",fill= "blue",position ="stack",color = "red");
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
g = g + geom_bar(width = 0.5,fill= topo.colors(5),color="darkgreen");
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
g = g + coord_flip();
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
print(g)
```

```
## Error in print(g): object 'g' not found
```

##15.when the data contains y values in a column, use stat="identity".

```r
df = data.frame(x=1:3,y=11:13)
g = g + ggplot(df,aes(x=x,y=y));
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
g = g + geom_bar(stat = "identity")
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
print(g)
```

```
## Error in print(g): object 'g' not found
```

##16. Count the "IsGood_Purchase" per Artist.
### weighted column should be numeric to that value, so that we can sum up together.

```r
g1 = ggplot(data=vintage,aes(x=vintage$"Top_3_artists"),weight = vintage$"IsGood_Purchase");
```

```
## Error in ggplot(data = vintage, aes(x = vintage$Top_3_artists), weight = vintage$IsGood_Purchase): object 'vintage' not found
```

```r
g1 = g1 + geom_bar(width=0.5);
```

```
## Error in eval(expr, envir, enclos): object 'g1' not found
```

```r
g1 = g1 + coord_flip();
```

```
## Error in eval(expr, envir, enclos): object 'g1' not found
```

```r
print(g1);
```

```
## Error in print(g1): object 'g1' not found
```

##17.Art Nationality

```r
g2 = ggplot(data=vintage,aes(x=vintage$"Art_Nationality"));
```

```
## Error in ggplot(data = vintage, aes(x = vintage$Art_Nationality)): object 'vintage' not found
```

```r
g2 = g2 + geom_bar(width=0.5,stat = "bin",fill=topo.colors(5));
```

```
## Error in eval(expr, envir, enclos): object 'g2' not found
```

```r
g2 = g2 + coord_flip();
```

```
## Error in eval(expr, envir, enclos): object 'g2' not found
```

```r
print(g2);
```

```
## Error in print(g2): object 'g2' not found
```

### Bin data--> Missing values are currently sliently dropped.

##18.count of arts per prominent colour with data labels

```r
freq = table(vintage$"Prominent_Color")
```

```
## Error in table(vintage$Prominent_Color): object 'vintage' not found
```

```r
names(freq)
```

```
## Error in eval(expr, envir, enclos): object 'freq' not found
```

```r
str(freq)
```

```
## Error in str(freq): object 'freq' not found
```

```r
pos = freq + 200
```

```
## Error in eval(expr, envir, enclos): object 'freq' not found
```

```r
df = data.frame(Poss);
```

```
## Error in data.frame(Poss): object 'Poss' not found
```

```r
t = data.frame(vintage$"prominent_Color");
```

```
## Error in data.frame(vintage$prominent_Color): object 'vintage' not found
```

```r
head(vintage$"Prominent_Color")
```

```
## Error in head(vintage$Prominent_Color): object 'vintage' not found
```


```r
names(iris);
```

```
## [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width" 
## [5] "Species"
```

```r
iris$"Species"
```

```
##   [1] setosa     setosa     setosa     setosa     setosa     setosa    
##   [7] setosa     setosa     setosa     setosa     setosa     setosa    
##  [13] setosa     setosa     setosa     setosa     setosa     setosa    
##  [19] setosa     setosa     setosa     setosa     setosa     setosa    
##  [25] setosa     setosa     setosa     setosa     setosa     setosa    
##  [31] setosa     setosa     setosa     setosa     setosa     setosa    
##  [37] setosa     setosa     setosa     setosa     setosa     setosa    
##  [43] setosa     setosa     setosa     setosa     setosa     setosa    
##  [49] setosa     setosa     versicolor versicolor versicolor versicolor
##  [55] versicolor versicolor versicolor versicolor versicolor versicolor
##  [61] versicolor versicolor versicolor versicolor versicolor versicolor
##  [67] versicolor versicolor versicolor versicolor versicolor versicolor
##  [73] versicolor versicolor versicolor versicolor versicolor versicolor
##  [79] versicolor versicolor versicolor versicolor versicolor versicolor
##  [85] versicolor versicolor versicolor versicolor versicolor versicolor
##  [91] versicolor versicolor versicolor versicolor versicolor versicolor
##  [97] versicolor versicolor versicolor versicolor virginica  virginica 
## [103] virginica  virginica  virginica  virginica  virginica  virginica 
## [109] virginica  virginica  virginica  virginica  virginica  virginica 
## [115] virginica  virginica  virginica  virginica  virginica  virginica 
## [121] virginica  virginica  virginica  virginica  virginica  virginica 
## [127] virginica  virginica  virginica  virginica  virginica  virginica 
## [133] virginica  virginica  virginica  virginica  virginica  virginica 
## [139] virginica  virginica  virginica  virginica  virginica  virginica 
## [145] virginica  virginica  virginica  virginica  virginica  virginica 
## Levels: setosa versicolor virginica
```

```r
Poss= table(iris$"Species") + 400;
d = merge(vintage, df, by.x="prominent_Color",by.y= "Var1")
```

```
## Error in merge(vintage, df, by.x = "prominent_Color", by.y = "Var1"): object 'vintage' not found
```






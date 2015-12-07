# Vintage Data Analysis:
#1. First we need to load the excell data into R.
# Install and load package readxl.
install.packages("readxl");
library("readxl");
install.packages("dplyr");
library(dplyr)
install.packages("stringr");
library(stringr);
install.packages("ggplot2");
library(ggplot2);

#2. Now define the path and load the vintage file into R.
path = "C:/Users/rbellana/Desktop";
setwd(path);
getwd();
vintage <- read_excel("Vintage Art V2.1.xlsx",sheet = "Art-i-stick");
head(vintage);
#3. Now studying the data.
dim(vintage);   # There are 72983 Rows and 26 variables
names(vintage); # we find variable names in string format where there are 2 or 3 words in a string
                # We need to replace the space between words with "_" symbol. 
str(vintage);  # we observve Art Piece size is maintained in a single column (lenght and Breadth).
head(vintage);

#4. Now we want to split the height and widht of the Art Piece size.
s = str_split(vintage$"Art Piece Size ", "X");
head(s);

                  #a = "2 X 3";       Sample for spliting.
                  #s = str_split(a,"X")
#5. Now extracting width and height elements of the column separately. So we use User Defined functions.
get_x= function(ele){
              ele[1]
}
get_y= function(ele){
              ele[2]
}

# Applying user defined function for the entire column.
x=sapply(s,FUN=get_x);
y=sapply(s,FUN=get_y);
head(x)
head(y)

# 6. We are getting as "17in. ",where "in." and space is not required.
x = str_replace(x,"in. "," ");
y = str_replace(y,"in."," ");

#7. Now remove the spaces using trim function.
x = str_trim(x);
y = str_trim(y);

#8.Now change into integer from character form.
vintage$x = as.integer(x)
vintage$Y = as.integer(y)

#9. Column names should be like one word.So replace space with "_".
names(vintage) = str_trim(names(vintage));
names(vintage) = str_replace_all(names(vintage)," ","_");


#10. Now arranging the data with X on X axis and y on Y axis
vintage = arrange(vintage,x,y);

#11. Now analyzing the height and width of the paints done on basis of brush size and painter.
g3 = ggplot(data= vintage,aes(x=x,y=y,size=vintage$Brush_Size,color=vintage$Brush_Size));
g3 = g3 + geom_point();
g3 = g3 + facet_grid(.~Top_3_artists);  #Dividing the data based on artist.Separate graph for 
                                          #separate artist.
print(g3);

#12. My Analysis:
# The plot shows that maximum artist used picture size of height 30 inches and width is 35 inches.
# Graph is downloaded in PDF Format which is attached in mail.
# By this we can do analysis on different variables like Art purchase date Versus changing measure-calc.
# We can analyze between different measuring variables according to the Business requirement.

names(vintage)

"Top_3_artists" 
"Art_Purchase_Date"
print(g4)

##################################################################################################
#13. Top 3 Artistist
unique(vintage$"Top_3_artists")

t = table(vintage$"Top_3_artists")
barplot(sort(t),horiz = T)

#14.Count of art by Artists:
# default "stat" for "bars" is 'bin" that gives the count value
g = ggplot(data=vintage,aes(x=vintage$"Top_3_artists"))
#g = g +  geom_bar(stat="bin",fill= "blue",position ="stack",color = "red");
g = g + geom_bar(width = 0.5,fill= topo.colors(5),color="darkgreen");
g = g + coord_flip();
print(g)

#15.when the data contains y values in a column, use stat="identity".
df = data.frame(x=1:3,y=11:13)
g = g + ggplot(df,aes(x=x,y=y));
g = g + geom_bar(stat = "identity")
print(g)

#16. Count the "IsGood_Purchase" per Artist.
# weighted column should be numeric to that value, so that we can sum up together.
g1 = ggplot(data=vintage,aes(x=vintage$"Top_3_artists"),weight = vintage$"IsGood_Purchase");
g1 = g1 + geom_bar(width=0.5);
g1 = g1 + coord_flip();
print(g1);

#17.Art Nationality
g2 = ggplot(data=vintage,aes(x=vintage$"Art_Nationality"));
g2 = g2 + geom_bar(width=0.5,stat = "bin",fill=topo.colors(5));
g2 = g2 + coord_flip();
print(g2);

# Bin data--> Missing values are currently sliently dropped.

#18.count of arts per prominent colour with data labels
freq = table(vintage$"Prominent_Color")
names(freq)
str(freq)
pos = freq + 200
df = data.frame(Poss);
t = data.frame(vintage$"prominent_Color");
head(vintage$"Prominent_Color")
names(iris);
iris$"Species"
Poss= table(iris$"Species") + 400;
d = merge(vintage, df, by.x="prominent_Color",by.y= "Var1")






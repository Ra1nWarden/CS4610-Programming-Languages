This is my first program written in Haskell. Due to the functional nature of the language, I defined some data types for this project:
1. Program, which consists of Classes. It has all the information from the abstract syntax tree.
2. Class, which consists of Features. Each class variable has all the attriubutes and methods of that class. 
3. Feature, a data type that can be either an Attriute or a Method. The names of these two data types are self-explanatory. 
4. Expression, a data type that is divided into all the types defined in the cool reference manual. Their umbrella term is called Expression. 
For PA4c, I have written some functions to check for type errors that do not involve type-checking of expressions. Basically, I scan through the .cl-ast file and build a Program variable. After that, I will look for inheritance cycle, presence of main class, etc. 
For the main part of the project, I have written more functions to type check expressions. I followed the instructions given in class by building an object map, a method map and the current working class. To evaluate the type of each expression, I passed all of these as arguments. The type rules are taken from the cool reference manual.
Remarks:
This is a much longer project compared to the previous ones and my code seems to be too long (over 1400 lines). If I had more time, I definitely want to make it shorter as many functions have similar structures and logic and it should not be hard to simplify those. 
Hakell IO seems very confusing so I have chosen to write a lot of indepent functions and glue all of them together into one big function. This may also be another reason for the code being too long.  

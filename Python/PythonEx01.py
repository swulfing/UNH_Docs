print("Hello World")

# A Python script file
# Comments in Python start with #
# The next line assigns the string "Data Carpentry" to the variable "text".
text = "Data Carpentry"

# The next line does nothing!
text

# The next line uses the print function to print out the value we assigned to "text"
print(text)

# Other Notes-operator
2 ** 16 # power
13 % 5  # mod

3 > 4 # False
True and True # True
True or False # True
True and False # False

# lists
numbers = [1, 2, 3]
numbers[0] #1
for num in numbers:
	print(num)

numbers.append(4)
print(numbers)

#help(numbers) # A bumch of info about numbers

#Tuples-A tuple is similar to a list in that it’s an ordered sequence of elements. However, tuples can not be changed once created (they are “immutable”). Tuples are created by placing comma-separated values inside parentheses ().

a_tuple = (1, 2, 3)
another_tuple = ('blue', 'green', 'red')

# Note: lists use square brackets
a_list = [1, 2, 3]

#What does the following output?
a_list[1] = 5 #replaces 2 in list w 5
print(a_list)
#a_tuple[2] = 5 #error tuple does not support item assignment
print(a_tuple)

type(a_tuple) #just says type tuple

#Dictionaries
translation = {'one': 'first', 'two': 'second'}
translation['one']# Pick up here

rev = {'first': 'one', 'second': 'two'}
rev['first']
rev['third'] = 'three'

for key, value in rev.items():
    print(key, '->', value)

for key in rev.keys():
    print(key, '->', rev[key])

def add_function(a, b):
    result = a + b
    return result

z = add_function(20, 22)
print(z)



# import sys, cStringIO
# import re
# zen = cStringIO.StringIO()
# old_stdout = sys.stdout
# sys.stdout = zen
# import this
# sys.stdout = old_stdout
# poem= zen.getvalue()
# author=" - The Zen of Python, by Tim Peters"
# def count_words(input_list):
#     #input_line=' '.join(input_line)
#     count = {}
#     for line in input_list:
#         words = re.sub(r'\"|\*|\-','',line.lower()).split()
#         for word in words:
#             if word in count:
#                 count[word] += 1
#             else:
#                 count[word] = 1
#     return count
#
# selected_lines=map(lambda (number,line): '\"%s\"'%(line.replace(r'\n\n',' ').strip()),[(number,line) for number,line in enumerate(poem.split('.')) if number%2!=0 ])
# print( "Answer to question 1-3:\n\nThe user has selected these lines:\n\n\n\n%s\n\nWith a %s word count total\n\nWord Count:%s " % (selected_lines,sum(count_words(selected_lines).values()),count_words(selected_lines) ))
# selected_lines=map(lambda (number,line): '\"%s\"%s'%(line.replace(r'\n\n',' ').strip(),author),[(number,line) for number,line in enumerate(poem.split('.')) if number%2!=0 ])
# print( "Answer to question 4:\n\nThe user has selected these lines:\n\n\n\n%s\n\nWith a %s word count total\n\nWord Count:%s " % (selected_lines,sum(count_words(selected_lines).values()),count_words(selected_lines) ))

list=["Apple","BananA","CherRy","dates"]
# newlist=[]
# newlist.append(word.title() for word in list)
# del newlist[0]
# newlist.pop()
# newlist.remove('dates')
# newlist.sort()
# newlist.sort(reverse=True)
# newlist.reverse()
# len(newlist)

animals=["chicken","fish","pig"]
for animal in animals:
    if animal == "chicken" :
        print("A %s has wings" %animal)
    elif animal=="fish":
        print ("A %s swims"% animal)
    else:
        print("A %s has four legs"%animal)
print("They all taste DELICIOUS!!")
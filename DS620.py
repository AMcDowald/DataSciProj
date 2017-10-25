
person_name="Anthony mcDowald"#This is used in message variable
famous_person= 'Juvenal'# Author to our quote
quote='All wish to possess knowledge, but few, comparatively speaking, are willing to pay the price.'
name_with_whitspace=" Tony\t McD\n *"#this has the white spaces

#inserting string variables into Python string
message="Welcome %s, This is Python!" % person_name
message2='title: %s\ncaps:%s\nlower:%s' % (person_name.title(), person_name.upper(),person_name.lower())
message3='The author %s once said, \"%s\"' % (famous_person, quote)
message4='with white space: %s\nlstrip:%s\nrstrip:%s\nstrip:%s' % (name_with_whitspace,name_with_whitspace.lstrip(), name_with_whitspace.rstrip(),name_with_whitspace.strip())
array=[message,message2,message3,message4] #Creating an array of strings

print '\n\n'.join([message for message in array]) #printing an array of strings joined by two new lines

c = get_config()

#Export all the notebooks in the current directory to the sphinx_howto format.
c.NbConvertApp.notebooks = ['*.ipynb']
c.NbConvertApp.export_format = 'latex'
c.Exporter.template_file = 'article'
c.NbConvertApp.postprocessor_class = 'PDF'
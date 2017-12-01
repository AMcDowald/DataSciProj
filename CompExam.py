from prjlib import *
import operator


if __name__ == '__main__':
    file_location = input('Please input the folder where the files for this project are located\nexample:\t\"/home/amcdowald/Downloads/PythonComprehensive/test/\"\n'
                          'This function will get all the files in this format Cuartos_Tarifa.*.txt and return a dictionary with all the pandas data frames from 0 to N files.\n->')
    list_of_files=[file for file in getfiles(file_location) if re.match(r'^.*Cuartos_Tarifa.*.txt',file)]
    data_frame_dict={}
    map(lambda line: operator.setitem(data_frame_dict,line[0],data_munger(line[1])),[line for line in enumerate(list_of_files)])
    print(data_frame_dict.viewkeys())
    #print first data frame
    print(data_frame_dict[0])
    DATAFRAME_DICTIONARY=data_frame_dict
import pandas as pd
import re
import collections
from os import listdir
from os.path import isfile, join

def getfiles(mypath):
  onlyfiles = [f for f in listdir(mypath) if isfile(join(mypath, f))]
  names = []
  for i_file in onlyfiles:
    names.append('{path}{i_file}'.format(path=mypath,i_file=i_file))
  return names

def data_munger(file_path):
    raw_data=open(file_path,'r+')
    col=re.compile(r'(\d+)\D+(\d+)\D+(\d{2}-\d{2}-\d{4}\D\d{2}:\d{2}:\d{2}).*(\d{2}-\d{2}-\d{4}\D\d{2}:\d{2}:\d{2})\D+(\w)\D+(\d+)\D+(\d+)\D(\w+\D\w+)')
    content= [line for line in raw_data.readlines() if not re.match(r'(\-{2,}|\s-\s|\r\n$)',line)]
    initial_rows=len(content)
    header=content[0].split()
    column_dt=collections.defaultdict(list)
    for line in content[1:]:
        try:
            column=col.match(line).groups()
        except:
            continue
        for number,name in enumerate(header):
            column_dt["%s" % name].append(column[number])
    dataframe=pd.DataFrame.from_dict(column_dt, orient='columns')
    print("FileName: %s\nStarting Row Count: %s\nDataFrame Row Count: %s\nRow Loss: %s\n\n"%(file_path,initial_rows,dataframe.shape[0],(initial_rows-dataframe.shape[0])))
    return dataframe

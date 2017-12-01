from PIL import Image
import numpy as np
import matplotlib.pyplot as plt
import time
from collections import Counter

from matplotlib import style

style.use("ggplot")

#from https://pythonprogramming.net/automated-image-thresholding-python/?completed=/thresholding-python-function/
def createExamples():
    numberArrayExamples = open('numArEx.txt', 'a')
    numbersWeHave = range(1, 10)
    for eachNum in numbersWeHave:
        for furtherNum in numbersWeHave:
            imgFilePath = '/home/amcdowald/Downloads/images/numbers/' + str(eachNum) + '.' + str(furtherNum) + '.png'
            ei = Image.open(imgFilePath)
            eiar = np.array(ei)
            eiarl = str(eiar.tolist())

            lineToWrite = str(eachNum) + '::' + eiarl + '\n'
            numberArrayExamples.write(lineToWrite)



#from https://pythonprogramming.net/automated-image-thresholding-python/?completed=/thresholding-python-function/
def threshold(imageArray):
    balanceAr = []
    newAr = imageArray
    from statistics import mean
    for eachRow in imageArray:
        for eachPix in eachRow:
            avgNum = mean(eachPix[:3])
            balanceAr.append(avgNum)
    balance = mean(balanceAr)
    print balance
    print newAr
    for eachRow in newAr:
        for eachPix in eachRow:
            if mean(eachPix[:newAr.shape[2]-1]) > balance:
                  for x in range(newAr.shape[2]-1):
                      eachPix[x] = 255
            else:
                  for x in range(newAr.shape[2] - 1):
                    eachPix[x] = 0
            eachPix[newAr.shape[2]-1] = 255
    return newAr

#from https://pythonprogramming.net/automated-image-thresholding-python/?completed=/thresholding-python-function/
def whatNumIsThis(filePath):
    matchedAr = []
    loadExamps = open('/home/amcdowald/Downloads/images/test/numArEx.txt', 'r').read()
    loadExamps = loadExamps.split('\n')
    i = Image.open(filePath)
    iar = np.array(i)
    iarl = iar.tolist()
    inQuestion = str(iarl)
    for eachExample in loadExamps:
        try:
            splitEx = eachExample.split('::')
            currentNum = splitEx[0]
            currentAr = splitEx[1]
            eachPixEx = currentAr.split('],')
            eachPixInQ = inQuestion.split('],')
            x = 0
            while x < len(eachPixEx):
                if eachPixEx[x] == eachPixInQ[x]:
                    matchedAr.append(int(currentNum))

                x += 1
        except Exception as e:
            print('hey')
            print(str(e))

    x = Counter(matchedAr)
    print(x)
    graphX = []
    graphY = []
    ylimi = 0
    for eachThing in x:
        graphX.append(eachThing)
        graphY.append(x[eachThing])
        ylimi = x[eachThing]

    fig = plt.figure()
    ax1 = plt.subplot2grid((4, 4), (0, 0), rowspan=1, colspan=4)
    ax2 = plt.subplot2grid((4, 4), (1, 0), rowspan=3, colspan=4)

    ax1.imshow(iar)
    ax2.bar(graphX, graphY, align='center')
    plt.ylim(400)

    xloc = plt.MaxNLocator(12)
    ax2.xaxis.set_major_locator(xloc)
    # plt.show()
    return fig

whatNumIsThis('/home/amcdowald/Downloads/images/numbers/1.3.png')


# # i = Image.open('/home/amcdowald/Downloads/images/dot.png')
# #
# # iar = np.asarray(i)
# # threshold(iar)
# #
# # plt.imshow(iar)
# # print(iar.shape)
# # plt.show()
#
#
# i = Image.open('/home/amcdowald/Downloads/images/test/1.2.png')
# iar = np.array(i)
# i2 = Image.open('/home/amcdowald/Downloads/images/test/test.png')
# iar2 = np.array(i2)
# # i3 = Image.open('/home/amcdowald/Downloads/images/numbers/y0.5.png')
# # iar3 = np.array(i3)
# # i4 = Image.open('/home/amcdowald/Downloads/images/sentdex.png')
# # iar4 = np.array(i4)
#
# #
# # iar = threshold(iar)
# iar2 = threshold(iar2)
# # iar3 = threshold(iar3)
# # iar4 = threshold(iar4)
# # print(iar.shape)
# print(iar2)
# plt.imshow(iar2)
# plt.show()
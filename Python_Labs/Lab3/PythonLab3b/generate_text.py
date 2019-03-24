import sys
from text_stats import TextStats
import random

'''
this class inherits from the previous created TextStats class. It uses the read function from that class to read
the data into a dictionary, with its next word. 
This file requires 3  arguments to execute. 
File name, Start Word, Length of sequence

You can even import this class and use it to generate text. You will have to call the read file function manually for the
program to work.
'''
class GenerateText(TextStats):
    def __init__(self):
        super().__init__()

    def getNextWord(self, curWord):
        if self.uniqWords.get(curWord) != None:
            return random.choices(list(self.uniqWords[curWord].keys()), list(self.uniqWords[curWord].values()))[0]
        else:
            return random.choice(list(self.uniqWords.keys()))

    def genText(self, startWord, seqLength):
        txt = startWord + " "
        curWord = startWord
        for i in range(seqLength):
            nxtWord = self.getNextWord(curWord)
            txt += nxtWord + " "
            curWord = nxtWord
        return txt


if __name__ == "__main__":
    textGen = GenerateText()
    if len(sys.argv) > 3:
        if textGen.readText(sys.argv[1]):
            print(textGen.genText(sys.argv[2], int(sys.argv[3])))
    else:
        print("A file name, a starting word and the length of sequence is required!")
import sys

'''
This class has a read function that reads data from a file into a dictionary. It stores that uniques word as a key
and the values to it is also a dictionary of the words that come next to it. All of this is done using the function 
convTextToDict, which is automatically called by the read function if you are using this program from the command
line. If you want to import the class and use it to get stats about your text, you will have to call the convTextToDict
function to create a dictionary first, then you can call the getStats function to get the stats about your text.

if you are running this program from the command line, this program requires a file name to read text from. You can also 
provide a file name to write the stats to.
'''

class TextStats:
    def __init__(self):
        self.total = 0
        self.freqLetters = {}
        for i in range(97, 123):
            self.freqLetters[chr(i)] = 0
        self.uniqWords = {}
        self.topFive = []
        self.sortedAlp = []

    def readText(self, fileName):
        try:
            with open(fileName, 'r', encoding="utf8") as f:
                txt = f.readlines()
            self.convTextToDict(txt)
        except:
            print("File does not exist!!")
            return False
        return True

    def convTextToDict(self, text):
        try:
            prev = ""
            for line in text:
                line = line.lower()
                word = ""
                for ch in line:
                    if ch.isalpha():
                        word += ch
                        try:
                            self.freqLetters[ch] += 1
                        except:
                            # print("Unable to process : ", ch)
                            pass
                    else:
                        if prev != "" and word != "":
                            if self.uniqWords[prev].get(word) != None:
                                self.uniqWords[prev][word] += 1
                            else:
                                self.uniqWords[prev][word] = 1
                                if self.uniqWords.get(word) == None:
                                    self.uniqWords[word] = {}
                            prev = word[:]
                            word = ""
                            self.total += 1
                        elif word != "":
                            if self.uniqWords.get(word) == None:
                                self.uniqWords[word] = {}
                            prev = word[:]
                            word = ""
                            self.total += 1
                        else:
                            word = ""
            self.uniqWords[prev]["_end_"] = 1
            return True
        except:
            print("No Text assigned to work with!")

    def topFiveWords(self):
        sorted_by_len = sorted(self.uniqWords.items(), key=lambda kv: sum(kv[1].values()), reverse=True)[:5]
        for i in range(5):
            vals = sorted_by_len[i]
            sorted_by_count_top = sorted(vals[1].items(), key=lambda kv: kv[1], reverse=True)[:3]
            self.topFive.append((vals[0], sum(vals[1].values()), sorted_by_count_top))

    def getStats(self, fileNameRead="", fileNameWrite="", text=None):
        if text and fileNameRead == "":
            self.convTextToList(text)
            self.sortedAlp = sorted(self.freqLetters.items(), key=lambda kv: kv[1], reverse=True)
            self.topFiveWords()
            self.printStats()
            if fileNameWrite != "":
                self.writeToFile(fileNameWrite)
        elif fileNameRead != "":
            if self.readText(fileNameRead):
                self.sortedAlp = sorted(self.freqLetters.items(), key=lambda kv: kv[1], reverse=True)
                self.topFiveWords()
                self.printStats()
                if fileNameWrite != "":
                    self.writeToFile(fileNameWrite)
        else:
            print("No data to compute stats for!")

    def printStats(self):
        print("Alphabets Sorted by count :\n", self.sortedAlp)
        print("\n\n")
        print("Total number of words in the text : ", self.total)
        print("Total number of unique words in the text : ", len(self.uniqWords), "\n\n")
        for vals in self.topFive:
            print(vals[0], "(", vals[1], "occurrences )")
            for cons in vals[2]:
                print("-- ", cons[0], ",", cons[1])

    def writeToFile(self, fileName):
        try:
            with open(fileName, 'w', encoding="utf8") as f:
                f.write("Alphabets Sorted by count :\n")
                for alps in self.sortedAlp:
                    f.write(alps[0] + "  " + str(alps[1]) + "\n")
                f.write("\n\n")
                f.write("Total number of words in the text : " + str(self.total) + "\n")
                f.write("Total number of unique words in the text : " + str(len(self.uniqWords)) + "\n\n")
                for vals in self.topFive:
                    f.write(str(vals[0]) + " ( " + str(vals[1]) + " occurrences )\n")
                    for cons in vals[2]:
                        f.write("-- " + str(cons[0]) + ", " + str(cons[1]) + "\n")
            print("Successfully written the stats to the file!")

        except:
            print("Some error occurred, unable to write to file!")

    def clearAll(self):
        self.freqLetters = {}
        for i in range(97, 123):
            self.freqLetters[chr(i)] = 0
        self.uniqWords = {}
        self.topFive = []
        self.sortedAlp = []

if __name__ == "__main__":
    stats = TextStats()
    if len(sys.argv) > 1:
        if len(sys.argv) == 2:
            stats.getStats(sys.argv[1])
        elif len(sys.argv) == 3:
            stats.getStats(sys.argv[1], sys.argv[2])
    else:
        print("A file name is required!")
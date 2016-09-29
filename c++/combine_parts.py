#!/usr/bin/python
import sys

s= str(sys.argv[1])
print s
#sketchFile = open(s, 'r')
with open(s, 'r') as sketchFile:
    sketchText = sketchFile.read()
    print sketchText
    originalFile = open("test/out1.txt", "r")
    originalText = originalFile.read()
    # print originalText
    print "DONE"
# backFile = open("background.sk", "r") 

# sketchText = sketchFile.read()
# backText = backFile.read()


# backText = backText.replace("SKETCH", sketchText)
# backText = backText.replace("ORIGINAL", originalText)

# print backText
            
    

import sys
import re

f = open('commentest.cl', 'r')
data = f.read()

print re.findall(r'\n', data)

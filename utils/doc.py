#!/usr/bin/python3
import json
import sys

itsum = 0

if (len(sys.argv) <= 1):
    print("Usage: doc,py <filename> (without .json extension)")
    exit(-1)
else:
    fn = sys.argv[1]
       
with open(fn+'.json', 'r') as file:
    data = json.load(file)

with open(fn+".tex", "w") as wf:
    wf.write('\\documentclass[12pt, landscape, a4papaer]{article}\n')
    wf.write("\\usepackage{savetrees}\n")
    wf.write("\\usepackage[svgnames]{xcolor}\n")
    wf.write("\\usepackage{gb4e}\n")
    wf.write("\\usepackage{courier}\n")
    wf.write("\\renewcommand*\\familydefault{\\ttdefault}\n")
    wf.write("\\usepackage[T1]{fontenc}\n")
    

    cola = ['DarkGreen', 'DarkBlue']
    wf.write('\\begin{document}\n')
    wf.write('\\section*{Material from file '+fn + '.json}\n')
    wf.write('Green distractors will be presented on the left side and Blue distractors on the right side \n')
    for it in data:
        tar = it['reps'][0]['target'] 
        num = it['item']
        wf.write('\\begin{exe}\n')
        wf.write('\t\\setcounter{xnumi}{'+str(it['item']-1)+'}\n')
        wf.write('\\ex \\begin{tabbing}')
        wf.write('\t'+tar.replace(' ',r' \= ')+' \\kill \n')
        wf.write('\t'+tar.replace(' ',r' \> ')+' \\\\ \n')
        for rep in it['reps']:
            dw = rep['distractor'].split()
            sw = rep['side'].split()
            wf.write('\t')
            for i in range(0,len(dw)):
                wf.write('\\color{'+cola[int(sw[i])]+'}{'+dw[i]+'}') 
                if i< len(dw)-1 :
                    wf.write(' \> ')
            wf.write(' \\\\ \n')
        wf.write('\t \end{tabbing}')
        wf.write('\\end{exe}\n')
       

    wf.write('\\end{document}\n')
    

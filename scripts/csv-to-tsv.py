import csv
 
with open("./public/latest.csv",'rt') as csvin, open("./public/latest.tsv", 'w', newline='\n') as tsvout:
    tsvout = csv.writer(tsvout, delimiter='\t')
    for row in csv.reader(csvin):
        tsvout.writerow([x.replace('\n', ' ') for x in row])



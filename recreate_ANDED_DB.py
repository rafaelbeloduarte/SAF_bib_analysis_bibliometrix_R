import pyreadr
import csv
import difflib

def similar(seq1, seq2, conf):
    return difflib.SequenceMatcher(a=seq1.lower(), b=seq2.lower()).ratio() > conf

# load AND database
AND_DB = []
ID_list = []
with open('AND_refined.csv', newline='') as csvfile:
	AND_data = csv.reader(csvfile, delimiter=',', quotechar='"')
	for row in AND_data:
		# row[0] = row[0].replace(".", "")
		AND_DB.append(row)
		ID_list.append(row[3] + row[5] + row[6] + row[7] + row[14])

# load WOS dataset (converted to RData in R)
# result is a dictionary where keys are the name of objects and the values python
# objects
result = pyreadr.read_r('complete_dataset.RData') # also works for Rds
M = result["M"] # extract the pandas data frame for object M

# replace names
a = 1
for i in range(len(M)):
	
	# split authors in each line
	authors_tmp = []
	
	authors_tmp = M.AU[i]
	
	authors_tmp = authors_tmp.split(";")
	
	# loop trhough all authors replacing names
	for j in range(len(authors_tmp)):
		# AND index
		# ['', 'authorID', 'groupID', 'author_name', 'author_order', 'address', 'university', 'department', 'postal_code', 'city', 'state', 'country', 'RP_address', 'RI', 'OI', 'UT', 'refID', 'PT', 'PY', 'PU']
		for k in range(len(AND_DB)):
			AND_wos_code = AND_DB[k][15].replace(":","")
			# if name strings are 50% similar and WOS code is equal then name is replaced
			if similar(AND_DB[k][3],authors_tmp[j], 0.5) and AND_wos_code == M.UT[i]:
				M.AU[i] = M.AU[i].replace(authors_tmp[j],AND_DB[k][3] + " " + AND_DB[k][2])
				print(AND_DB[k][3] + " " + AND_DB[k][2], " = ", authors_tmp[j])
				print(AND_DB[k][15], " = ", M.UT[i])
				print("")
				a = a + 1
				print(a)

# write the new R dataframe
pyreadr.write_rdata("complete_dataset_ANDED.rds", M, df_name="dataset_ANDED")

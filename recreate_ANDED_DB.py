# Program to insert an ANDed list created by refsplitr
# into a rds bibliometrix file
# Copyright (C) 2023  Rafael Belo Duarte
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# contact me at rafaelbeloduarte@pm.me

import pyreadr
import csv
import difflib

# load AND database
AND_DB = []
with open('AND_refined.csv', newline='') as csvfile:
    AND_data = csv.reader(csvfile, delimiter=',', quotechar='"')
    for row in AND_data:
        AND_DB.append(row)

# load WOS dataset (converted to RData in R first)
# result is a dictionary where keys are the name of objects and the values python
# objects
result = pyreadr.read_r('complete_dataset.RData') # also works for Rds
M = result["M"] # extract the pandas data frame for object M
# display column names 
# print(list(M.columns))

# refsplitr gives a list with papers WOS codes and the respective author orders
# so it is easy to look through the original dataset, which also has WOS codes,
# and replace all the names

# removing the ":" characther from AND_DB first because
# in the original database there is no ":"
AND_wos_codes = []
for j in range(len(AND_DB)):
    AND_wos_codes.append(AND_DB[j][15].replace(":",""))
print("WOS codes count: ", len(AND_wos_codes))

# AND index:
# ['', 'authorID', 'groupID', 'author_name', 'author_order', 'address', 'university', 'department', 'postal_code', 'city', 'state', 'country', 'RP_address', 'RI', 'OI', 'UT', 'refID', 'PT', 'PY', 'PU']
# loop to replace names
a = 1
for row in M.index:
    print("Paper index: ", row)
    original_DB_wos_code = M.loc[row, 'UT']
    print("WOS code: ", original_DB_wos_code)
    print("Original names: ", M.loc[row, 'AU'])

    indices = [index for index, item in enumerate(AND_wos_codes) if item == original_DB_wos_code]

    authors_tmp = []
    
    for k in indices:
        # create a list of lists with author orders and names
        authors_tmp.append([AND_DB[k][4],AND_DB[k][3]])
    # sort the list
    authors_tmp.sort()
    # remove duplicates
    authors_tmp = [list(item) for item in set(tuple(row) for row in authors_tmp)]
    
    # remove the order information from the list
    authors = []
    for i in range(len(authors_tmp)):
        authors.append(authors_tmp[i][1])

    # convert list back into str
    authors = ';'.join(str(x) for x in authors)

    M.loc[row, 'AU'] = authors

    print("New names: ", M.loc[row, 'AU'])
    print("")

# save the new bibliometrix rds file
pyreadr.write_rds("complete_dataset_ANDED.rds", M)

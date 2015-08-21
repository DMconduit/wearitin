#################################
#	bmXMLparser					#
#	BedMasterEX XML parser		#
#	c. dmaslove (2015)			#
#################################

# This script will take the XML otput from Bedmaster and
# parse the XML into individual vital signs readings with corresponding
# time stamp. Outputs a CSV where each row is ('time', 'parameter', 'value')

import sys
import csv
import xml.etree.cElementTree as ET 	# the faster C implementation of ElementTree


# Functions
def oneTimepoint(timepoint):
	"""Takes an individual timepoint from the XML doc
	(i.e. a VitalSigns node) and extracts all the vitals,
	Combines with timestamp and value to generate list of tuples"""
	t = []
	for vs in timepoint:
		t.append((timepoint.attrib.get('CollectionTime'), vs[0].text, vs[1].text))
	return(t)	



subject_id = sys.argv[1]
read_file = subject_id + '.xml'
write_file = subject_id + '_out.csv'

# Open the XML file
with open(read_file, 'r') as f:
    tree = ET.parse(f)

# a list of all the timpoints
timepoints = tree.findall('.//VitalSigns')

# create an empty list
# thew loop will fill in one row for each vital sign
all = []
for each in timepoints:
	all.append(oneTimepoint(each))

# unpack 'all' which is a list of lists
all_out = [item for sublist in all for item in sublist] 

# write out to CSV
with open(write_file, 'wb') as myfile:
	wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
	for each in all_out:
		wr.writerow(each)


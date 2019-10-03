#!/usr/bin/env python
'''
combine subject files into one data file
-- assumes there are 9 pieces of info on each (useful) line, 
and that the first one is an int
'''

import sys, os
DATA_DIR = "data"

def build_line(line=[]):
    string = ""
    for i in line:
        string += str(i)+'\t'
    return (string.strip('\t') + '\n')

def main():
	fname = "exp1_NissBul_traj_data.txt"
	try:
		f = open(fname, 'r')
		f.close()
		print(fname + " already exists! Let's not overwrite it.")
		sys.exit()
	except:
		pass
	
	files = os.listdir(os.path.join(os.curdir, DATA_DIR))
	outfile = open(fname, 'w')
	header = 'subject\ttrial\tcond\ttotalTime\ttrialTime\tprevTarget\tcurTarget\tnextTarget\thitTargetPos\ttimeTargetHit\tobjTouched\tCorrect\txPos\tyPos\n'
	outfile.write(header)
	ncols = len(header.split())
	for file in files:
		if file[0:4]=="data":
			sub_file = open(os.path.join(os.curdir, DATA_DIR, file), 'r')
			for line in sub_file:
				trial = line.strip().split()
				if len(trial)==ncols:
					try:
						int(trial[0])
						if file[6:8]=='21': # special case for screwed up subj number
							trial[0] = '21'

						outfile.write(build_line(trial))
					except:
						pass
	
	outfile.close()

if __name__ == '__main__':
	main()

#! /usr/bin/env python
Usage = '''
SI_GPSConverter.py - version 1.0
Script for 20Sp_PCfB Hackathon Group1
Convert a series of GPS coordinates to Deg.decimal format
and quality check them against a smithsonian database file
Usage:
	SI_GPSConverter.py OriginalData.csv SmithsonianDatabase.csv Outfile.txt
'''
import re, sys, csv

if len(sys.argv)<2:
	print(Usage)
else:
	def GPSConvertCalc(GPSString):
		if GPSString.count(' ')==2:
			SearchStr = '(\d+) ([\d]+) (\d+)'
			Result = re.search(SearchStr, GPSString)
			Degrees = float(Result.group(1))
			Minutes = float(Result.group(2))
			Seconds = float(Result.group(3))
			DecimalDegrees = Degrees + Minutes/60 + Seconds/3600
		elif GPSString.count(' ')==1:
			SearchStr = '(\d+) ([\d\.\d]+)'
			Result = re.search(SearchStr, GPSString)
			Degrees = float(Result.group(1))
			Minutes = float(Result.group(2))
			DecimalDegrees = Degrees + Minutes/60
		elif GPSString.count(' ')==0:
			DecimalDegrees=float(GPSString)
		return DecimalDegrees

	OrigData = open(sys.argv[1],'r')
	SI_Data = open(sys.argv[2],'r')
	OutFile = open(sys.argv[3],'w')
	
#Making a dictionary called SI_Dict['Field Number']=['Centroid Latitude','Centroid Longitude']
	SI_Dict={}
	SILineCount=0
	for Line in csv.reader(SI_Data):
		if SILineCount>0:
			SIList = Line
#			print('%s\t%s\t%s\t%s'%(SIList[0],SIList[26],SIList[23],SIList[24]))
			try:
				SI_Dict[SIList[26]]
			except KeyError:
				SI_Dict[SIList[26]]=[SIList[23],SIList[24]]
		SILineCount+=1
#	print(SI_Dict)
	Header = 'Field-Numbers\tOriginal_Latitude\tOriginal_Longitude\tdd_lat\tdd_lon\tsmith_lat\tsmith_lon\tagree'
	OutFile.write('%s\n'%(Header))
	OLineCount=0
	for Line in OrigData:
		Line=Line.strip('\n').strip('\r')
		if OLineCount>0:
			ElementList = Line.split(',')
			FieldNum = ElementList[0]
			DDLat = GPSConvertCalc(ElementList[1])
			DDLong = GPSConvertCalc(ElementList[2])
			Agreement = 'Yes'
			Lat=''
			Long=''
			try:
				if SI_Dict[FieldNum][0]:
					Lat=float(SI_Dict[FieldNum][0])
					Long=float(SI_Dict[FieldNum][1])
				else:
					Lat=SI_Dict[FieldNum][0]
					Long=SI_Dict[FieldNum][1]
				if DDLat != Lat:
					Agreement = 'LatWrong'
				if DDLong != Long:
					if Agreement == 'LatWrong':
						Agreement = 'LatLongWrong'
					else:
						Agreement = 'LongWrong'
			except KeyError:
				Agreement = 'FieldNumWrong'
			OutFile.write('%s\t%s\t%s\t%.5f\t%.5f\t%s\t%s\t%s\n' %(FieldNum, ElementList[1], ElementList[2],DDLat, DDLong, Lat, Long, Agreement))
		OLineCount+=1
	OrigData.close()
	SI_Data.close()
	OutFile.close()
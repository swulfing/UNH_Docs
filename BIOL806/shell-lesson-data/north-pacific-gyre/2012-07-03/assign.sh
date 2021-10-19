# cd shell-lesson-data/north-pacific-gyre/2012-07-03
for datafile in NENE*A.txt NENE*B.txt
do
	#echo $datafile
	bash goostats.sh $datafile stats-$datafile
done

cd north-pacific-gyre/2012-07-03/
for datafile in "$@"
do
	echo $datafile
	bash goostats.sh $datafile stats-$datafile
done

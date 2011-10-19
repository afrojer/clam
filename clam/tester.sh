divider='==============================================='
for file in tests/*
do
	if [ -f "${file}.res" ] 
	then
		echo $divider 
		echo "Testing file $file"
		./clam < $file > temp
		if [ $? -ne 0 ] 
		then
			echo "Could not compile"
			cat temp
			continue
		else
			echo "Compiled"
		fi
		
		./bin.clam | diff - "$file.res" > temp
		if [ $? -ne 0 ]
		then
			echo "Output not as expected:"
			cat temp
		else
			echo "Output as expected"
		fi
		rm temp
	fi
done
echo $divider 

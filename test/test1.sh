if not set -q argv[1] 
	exit 1
end

set type $argv[1]

switch $type 
case sat
case unsat
case '*'
	exit 1
end

if not set -q argv[2] 
	exit 1
else
	set file $argv[2]
	set outfile (path change-extension .out $file)

	if not set -q NUM_CONFLICTS
	   # set output (string lower (cabal run -v0 sat $file -- -t 10.0 -n))
	   set -x NUM_CONFLICTS 256
	# else
	#    set output (string lower (cabal run -v0 sat $file -- -t 10.0 -n -c $NUM_CONFLICTS))
	end

	if not set -q GROW_FACTOR
	   set -x GROW_FACTOR 2
	end

	set output (./_build/default/bin/main.exe $file -t 10.0 -c $NUM_CONFLICTS -g $GROW_FACTOR -p $outfile) #) -t 10.0 -n -c $NUM_CONFLICTS -g $GROW_FACTOR))
	# set output (string lower (cabal run -v0 sat $file -- -t 10.0 -n))
	set result (echo $output | head -n 1)

	if test $status -gt 0
	   	set_color -o red
		echo "FATAL ERROR"
	   	set_color normal
		exit 1
	end

	echo -n $file "- "
	if test $result = "s UNKNOWN"
	   	set_color -o yellow
		echo "TIME OUT"
	   	set_color normal
		# TODO race condition
		# set -Ux timeout (math $timeout + 1)
	# else if test $type != $output
	#    	set_color -o red
	# 	echo -n "FAIL: "
	#    	set_color normal
	# 	echo "Expected" $type "but got" $output
	# 	# TODO race condition
	# 	# set -Ux failed (math $failed + 1)
	else
		if [ $type = "sat" ]
		   set satfile (path change-extension .sat $file)
		   echo $output | tail -n 1 | sed -e 's/s SATISFIABLE v //' > $satfile
		   gratchk sat $file $satfile | grep 'VERIFIED SAT' > /dev/null 2>&1
		   if [ $status -gt 0 ]
				set_color -o red
				echo -n "FAIL: "
				set_color normal
				echo "Expected SAT but got UNSAT"
		   else
				set_color -o green
				echo "OK"
				set_color normal
		   end
		   rm $satfile
		else
		   set gratfile (path change-extension .grat $file)
		   gratgen $file $outfile -o $gratfile > /dev/null 2>&1
		   gratchk unsat $file $gratfile | grep 'VERIFIED' > /dev/null 2>&1
		   if [ $status -gt 0 ]
				set_color -o red
				echo -n "FAIL: "
				set_color normal
				echo "Expected UNSAT but got SAT"
		   else
				set_color -o green
				echo "OK"
				set_color normal
		   end
		   rm $outfile
		   rm $gratfile
		end
	end
end

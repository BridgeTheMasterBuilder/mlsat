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

	if not set -q NUM_CONFLICTS
	   # set output (string lower (cabal run -v0 sat $file -- -t 10.0 -n))
	   set -x NUM_CONFLICTS 2
	# else
	#    set output (string lower (cabal run -v0 sat $file -- -t 10.0 -n -c $NUM_CONFLICTS))
	end

	if not set -q GROW_FACTOR
	   set -x GROW_FACTOR 8
	end

	set output (string lower (./_build/default/bin/main.exe -- $file | tail -n 1)) #) -t 10.0 -n -c $NUM_CONFLICTS -g $GROW_FACTOR))
	# set output (string lower (cabal run -v0 sat $file -- -t 10.0 -n))

	if test $status -gt 0
	   	set_color -o red
		echo "FATAL ERROR"
	   	set_color normal
		exit 1
	end

	echo -n $file "- "
	if test $output = "solver timed out"
	   	set_color -o yellow
		echo "TIME OUT"
	   	set_color normal
		# TODO race condition
		# set -Ux timeout (math $timeout + 1)
	else if test $type != $output
	   	set_color -o red
		echo -n "FAIL: "
	   	set_color normal
		echo "Expected" $type "but got" $output
		# TODO race condition
		# set -Ux failed (math $failed + 1)
	else
	   	set_color -o green
		echo "OK"
	   	set_color normal
	end
end

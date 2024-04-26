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
	set -Ux failed 0
	set -Ux timeout 0
	set files $argv[2..-1]

	set_color -o white

	if test (count $argv) = 2
	   	echo "Running one test"
	else
		echo "Running" (math (count $argv) - 1) "tests"
	end

	set_color normal
	/usr/bin/time -f"Took %e seconds (user:%U system:%S) %P CPU usage" parallel --halt-on-error now,fail=1 'fish test/test1.sh {} {}' ::: $type ::: $files
	# for file in $files
	# 			fish test/test1.sh $type $file
	# end

	if test $failed -gt 0
	   	set_color -o white
		echo $failed "failures"
		set_color normal
	end

	if test $timeout -gt 0
	   	set_color -o white
		echo $timeout "timed out"
		set_color normal
	end

	set temp $failed
	set -Ux failed 0
	set -Ux timeout 0

	exit $temp
end

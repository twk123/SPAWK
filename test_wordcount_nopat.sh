#!/bin/sh
# script to compile and test spawk with 

TEST="no pattern word count program"

SignalError() {
    if [ $error -eq 0 ] ; then
	echo "FAILED"
	error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    #generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
	SignalError "$1 differs"
	echo "FAILED $1 differs from $2" 1>&2
	exit
    }
    echo "Test PASS for $TEST"
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}


# run the control python script through spark , sort needed for random reducer order from spark parallel system
spark-submit test_nopat_wc.py ../wordcount_test.txt 2> /dev/null | sort > wordcount_nopat_test.out 

# compile and run generated python through spark
rm -f spawk.native
rm -f wordcount_nopat.py
ocamlbuild -use-menhir -yaccflag --trace -use-ocamlfind spawk.native
./spawk.native wordcount_nopat.spawk ../wordcount_test.txt > /dev/null 2>&1 
spark-submit wordcount_nopat.py 2> /dev/null | sort > spawk_output.out

Compare spawk_output.out wordcount_nopat_test.out diffFile


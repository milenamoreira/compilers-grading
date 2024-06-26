#!/usr/bin/perl -w
#
# CS143gradesingle grading script
#
# Usage:
# 143gradesingle assignname assignmentdir
#
# where ppnumber is the programming project number to grade
# (ex. "1" for pp1, "3check" for pp3check)
# and assignment dir is the directory containing the students 
# assignment.
# 
# this script is responsible for grading a single submission of
# of a single assignment.
#
# this script should, however, be invoked automatically by
# 143grading, which handles grading all submissions of a 
# particular assignment.
#


# by default, run only automated tests
$mode=2; #do interactive
$forceoverwrite=0;
$comments="RESULTS";
$showdiff=1; #show diff on interactive

while (@ARGV > 2) {
    if ($ARGV[0] eq "-i") {
	$mode=2;	
	shift;
    } elsif ($ARGV[0] eq "-a") {
	$mode=3;
	shift;
    } elsif ($ARGV[0] eq "-o") {
	$forceoverwrite=1;
	shift; 
    } elsif ($ARGV[0] eq "-f") {
	$comments=$ARGV[1];
	shift;
	shift;
    } elsif ($ARGV[0] eq "-d") {
	$showdiff=1;
	shift;
    } else {
	last;
    }
}


if ( @ARGV lt 2 ) {
    print STDERR "Usage: 143gradesingle [options] assignment-name assignment-dir\n\n";
    print STDERR "where options may be the following:\n";
    print STDERR "\t-a\t\trun all tests (interactive and automated)\n";
    print STDERR "\t-d\t\twhen running interactive, show the diff output\n";
    print STDERR "\t-i\t\trun only interactive tests\n";
    print STDERR "\t-o\t\toverwrite COMMENTS file w/o prompt\n";
    print STDERR "\t-f filename\tsave COMMENTS to filename instead\n\n";
    print STDERR "note: by default, only automated tests are run\n";
    exit;
}

$assignname=$ARGV[0];
$assigndir=$ARGV[1];
#$needsSpim=($ppnumber eq '4' or $ppnumber eq '4check');
#$needsOutErrSplit=0;   # ($ppnumber eq '3');

# ---------------------------------------------------------------------------
# Path variables

# base class directory
#$CLASS_DIR="/usr/coolc";

# directory containing cases file, test files, and solution output files
$TESTS_DIR=".";

$BIN_DIR=".";

$TEST_OUTPUT_DIR="test-output";

if ($assignname eq "PA2") {
  $executable="lexer";
}
if ($assignname eq "PA3") {
  $executable="myparser";
}
if ($assignname eq "PA4") {
  $executable="mysemant";
}
if ($assignname eq "PA5") {
  $executable="mycoolc";
}
if ($assignname eq "PAX1") {
  $executable="mycoolc";
}

#$flags="-d pp$ppnumber";

shift;
shift;

# if test cases specified, assume we want to run them all
if (@ARGV > 0) {
    $mode=3;
}

print "Grading $assigndir...\n";

my @cases = ();

open CASESFILE, "$TESTS_DIR/cases" or die "Could not open cases file: $TESTS_DIR/cases";

my $linenum = 0;

while (defined($line = <CASESFILE>)) {
    chomp $line;
    $linenum++;

    # ignore blank lines and lines starting with #
    if ($line =~ /^#/) {
	next;
    } elsif ($line =~ /^\s+$/) {
	next;
    } elsif ($line eq "") {
	next;
    }

    # look for maxscore line
    my @scoreline = split /=/, $line;
    if ($scoreline[0] =~ /maxscore/i) {
	if (!defined($maxscore)) {
	    $maxscore = $scoreline[1];
	    $maxscore =~ s/ //g;
	    next;
	} else {
	    die "on line $linenum, already had maxscore: $maxscore, but got line: $line";
	}
    }
    @case = split /;/, $line;

    # 1 is not interactive
    $interactive=1;  
    # 1 is don't filter
    #$filter = 2; #filter by default

    if (defined($case[3]) && $case[3] =~ /^\s*I\s*$/) {
	# two is interactive
	$interactive = 2;
    }
    if (defined($case[4]))
      {
        $filter = $case[4];
        $filter =~ s/ //g
      }
    else
      {
        $filter = $assignname . "-filter";
      }
    if (defined($case[5]))
      {
        $addedflags = $case[5];
      }
    else
      {
        $addedflags = "";
      }
    ($prefix) = split /\./, $case[0];
    # did someone forget the .cool?
    #if ($prefix eq $case[0]) {
    #    $case[0] = $case[0] . ".cool";
    #}
    $case[3] = $case[0] . ".out";
    $case[4] = $interactive;
    $case[5] = $filter;
    $case[6] = $addedflags;
    # check syntax
    if (!defined($prefix) || !defined($case[1]) || !defined($case[2]) ||
	!defined($case[3]) || !defined($case[4]) || !defined($case[5])) {
	die "on line $linenum, bad syntax in cases file: $line";
    }

    # remove the space(s) from the point value
    $case[1] =~ s/ //g;

    # if have argv elments, check and see if this case if one of them
    # if it's not, don't add it.
    if (@ARGV > 0) {
	foreach (@ARGV) {
	    if ( ($_ eq $prefix) || ($_ eq $case[0])) {
		print "adding: $_\n";
		# always interacvtive
		$case[4] = 2;  
		push @cases, [ @case ] ;
	    }
	}

    } else {
	push @cases, [ @case ] ;
    }
}
close CASESFILE;

if (!defined($maxscore)) {
    die "maxscore was never defined in cases file";
}

$totscore = $maxscore;

if (@ARGV > @cases) {
    die "Couldn't find one of these case files: @ARGV";
}

# ---------------------------------------------------------------------------
# Builds the students submission

#make the binary
if ( $assignname eq "PA2") {
  system("make -C $assigndir lexer");
  if (! -x "$assigndir/lexer") {
    print "Couldn't build submission!\n";
    exit 1;
  }
}
if ( $assignname eq "PA3") {
  system("make -C $assigndir parser");
  if (! -x "$assigndir/parser") {
    print "Couldn't build submission!\n";
    exit 1;
  }
}
if ( $assignname eq "PA4") {
  system("make -C $assigndir");
  system("make -C $assigndir semant");
  if (! -x "$assigndir/semant") {
    printf "Couldn't build submission!\n";
    exit 1;
  }
  
}
if ($assignname eq "PA5") {
  system("make -C $assigndir");
  system("make -C $assigndir cgen");
  if (! -x "$assigndir/cgen") {
    printf "Couldn't build submission!\n";
    exit 1;
  }
}
if ($assignname eq "PAX1") {
  system ("make -C $assigndir");
  system("make -C $assigndir cgen");
  if (! -x "$assigndir/cgen") {
    printf "Couldn't build submission!\n";
    exit 1;
  }
}


$testout = "test.out";
$testerr = "test.err";

# only remove stale files when running automated
#if ( $mode == 1) {
# remove old diffs/out filesi
system("mkdir -p $TEST_OUTPUT_DIR");
system("rm -f $TEST_OUTPUT_DIR/*.diff $TEST_OUTPUT_DIR/*.out $TEST_OUTPUT_DIR/*.err $TEST_OUTPUT_DIR/*.unfilt *.s $testerr $testout");
#}

# ---------------------------------------------------------------------------
# Grades the submission

# the COMMENTS will contain all of the script generated 
# comments.  if a submission's output does not match the
# solution output, the comments in the cases file will be
# added to the end of the COMMENTS file.

# append or overwrite?
$AO=">";

if ((! $forceoverwrite) and (open COMMENTS, $comments)) {
    close COMMENTS;
    $ans="";
    while ($ans ne "a" and $ans ne "o") {
	printf "$comments file exists.  append or overwrite ([a]/o)? ";
	chomp ($ans = <STDIN>);
	if ($ans eq "") {
	    $ans="a";
	}	
    }
    if ($ans eq "o") {
	$AO=">";
    } else {
	$AO=">>";
    }
    
}


open COMMENTS, "$AO $comments" or die "Could not open $comments";
if ($assignname eq "PAX1") {
  system("echo '' > SPEEDS");
}
$passedall=1;

if ($mode == 2)
{
    # don't print congratulatory message for passing all interactive tests
#    $passedall = 0;
}

print "=====================================================================\n";
print "submission: $assigndir\n\n";

foreach my $caseref (@cases) {
    ($casefile, $points, $comment, $goodout, $interactive, $filter, $addedflags) = @$caseref;

    ($prefix) = split (/\./, $casefile);
    
    eval {
	local $SIG{ALRM} = sub
	{
	    # if the submission times out, run this block
	    alarm 0;
	    $retval = 0;
	    $bad = 1;
	    # need to actually kill child o_O
	    $gpid = getpgrp 0;
	    
	    # kill the errant process(es)
	    system("ps -g $gpid | grep -i $executable | awk '{print \$1}' | xargs kill -9 > /dev/null 2>&1");
	    #system("ps -g $gpid | grep -i $preprocessor | awk '{print \$1}' | xargs kill -9 > /dev/null 2>&1");
	    system("ps -g $gpid | grep -i spim | awk '{print \$1}' | xargs kill -9 > /dev/null 2>&1");
	    system("ps -g $gpid | grep -i java | awk '{print \$1}' | xargs kill -9 > /dev/null 2>&1");
	    system("ps -g $gpid | grep -i cgen | awk '{print \$1}' | xargs kill -9 > /dev/null 2>&1");
	    system("ps -g $gpid | grep -i lexer | awk '{print \$1}' | xargs kill -9 > /dev/null 2>&1");
	    system("ps -g $gpid | grep -i parser | awk '{print \$1}' | xargs kill -9 > /dev/null 2>&1");
	    system("ps -g $gpid | grep -i semant | awk '{print \$1}' | xargs kill -9 > /dev/null 2>&1");
	 
	    die "Timed out waiting for assignment.  infinite loop?\n";
	};
	
	# set the alarm for 60 seconds
	alarm 60;
	
	# this redirects, in two steps, stdout to testout and stderr to stdout.
	# note, something like 
	# ./$executable < $TESTS_DIR/$casefile 2>&1 testout
	# will *not* work with classic sh versions (it will work with bash).
	# the following should work in both sh and bash derivatives.
	
	system("rm -f $testout $testerr tmp.asm");
	$bad = 0;
	
	if ( $assignname eq "PA2" ) {
	    # $retval = system("./$executable < $TESTS_DIR/$casefile > $testout 2>&1");
	    $retval = system("$assigndir/$executable $TESTS_DIR/$casefile > $testout 2>&1");
	} elsif ( $assignname eq "PA3" ) {
	    $retval = system("./$executable $TESTS_DIR/$casefile > $testout 2>&1");
	    $retval = 0; #if there were errors, the parser returns an
	    # error value
	} elsif ( $assignname eq "PA4" ) {
	    $retval = system("$assigndir/$executable $TESTS_DIR/$casefile > $testout 2>&1");
	    $retval = 0;
	} elsif ( $assignname eq "PA5" ) {
	    # modif by Pierre Pont - 12/03/04
	    # if the case name contains "-gc", run coolc with Garbage Collection enabled
	    if ($casefile =~ m/-gc/) {
		print "[GC Test Case: $casefile]\n";
		$retval = system("$assigndir/$executable $addedflags -g -o $casefile.s $TESTS_DIR/$casefile");
	    } else {
		$retval = system("$assigndir/$executable $addedflags -o $casefile.s $TESTS_DIR/$casefile");
	    } 
	    if (($retval != 0)) {
		system("mv $casefile.s $testout");
	    } else {
		$retval = system("$BIN_DIR/spim -file $casefile.s > $testout 2>&1");
		# system("rm $casefile.s");
	    }
	} elsif ( $assignname eq "PAX1" ) {
	    #send output to /dev/null because some people have _insane_ amounts
	    #of debug output. 
	    $retval = system("$assigndir/$executable $addedflags -g -O -o $casefile.s $TESTS_DIR/$casefile > /dev/null 2>&1 ");
	    if (($retval != 0)) {
		system("mv $casefile.s $testout");
	    } else {
			if(!open(TRAP, "<trap.handler")) {
		$retval = system("$BIN_DIR/spim -trap_file /usr/class/cs143/admin/bin/trap.handler.quiet -keepstats -ldata 20000000 -stext 262144 -file $casefile.s > $testout 2>&1");
		} else {
			close TRAP;
		$retval = system("$BIN_DIR/spim -trap_file trap.handler -keepstats -ldata 20000000 -stext 262144 -file $casefile.s > $testout 2>&1");
		}
		#system("rm $casefile.s");
	    }
	}
	# unset the alarm
	alarm 0;
    }; # end of eval block
    
    warn $@ if $@;
    # 255 for -1 return
    #$retval = ($retval >> 8);
    
    $origdiffout = "";
    $origdifferr = "";

    if ($bad == 1)
    {
	# bad is set by infinite loop alarm if run
	$comment .= ' [submission looped infinitely on this input]';
	
    }
    elsif (($retval != 0) && ($retval != 255))
    {
	# if the submission crashes, run this block
	print "non-zero exit value: " . $retval . "\n";
	$comment .= ' [submission crashed on this input]';
	$bad = 1;
    } 
    else 
    {
	if ($assignname eq "PAX1") {
	    system("echo '($prefix)' >> SPEEDS");
	    system("cat $testout | grep -o -E '#instructions : [0-9]+' | grep -o -E '[0-9]+' >> SPEEDS");
	    system("cat $testout | grep -o -E '#reads : [0-9]+' | grep -o -E '[0-9]+' >> SPEEDS");
	    system("cat $testout | grep -o -E '#writes [0-9]+' | grep -o -E '[0-9]+' >> SPEEDS");
	}
	
	$origdifferr = "";
	$origdiffout = &diffFile($testout, $goodout, $filter);
	
	#print "HERE!!!\n\n\n";
	#print $filter;
	#print "\n\n\n\n\n";
	
	if ($needsOutErrSplit) {
	    $gooderr = $goodout;
	    $gooderr =~ s/.out/.err/;
	    $origdifferr = &diffFile($testerr, $gooderr, $filter);
	}
	
	if (($origdiffout eq "") and ($origdifferr eq "")) {
	    $bad = 0;
	} else {
	    $bad = 1;
	}
    } 

    if ( $interactive == 2 ) {
	print "---------------------------------------------------------------------\n";
	print "test case: $casefile; $comment\n";
	print "---------------------------------------------------------------------\n\n";
	# do interactive grading here
	#print "interactive\n";

	if ($showdiff == 0) {
	    &printFile($testout);
	    if (($needsOutErrSplit)) {
		print "-----------------";
		&printFile($testerr);
	    }
	} else {
	    if ($bad) {
		print $origdiffout;
		if (($needsOutErrSplit)) {
		    print "-----------------";
		    print $origdifferr;
		}
	    } else {
		print "there were no differences between submission and solution\n";
	    }
	}
	
	$bad=0;
	
	$pass="0";
	while ( $pass ne "y" and $pass ne "n") {
	    print "\nDid the submission pass the test ([y]/n)?\n";
	    chomp ($pass=<STDIN>);
	    if ($pass eq "" ) {
		$pass="y";
	    }
	}

	if ( $pass eq "n" ) {
	    $bad=1;
	}
    }

    # the submission did not pass the test
    if ($bad)
    {
	$totscore -= $points;
	$passedall=0;
	
	if ($points != 0) {
	    print "\t-$points ($prefix)\t$comment\n";
	    print COMMENTS "\t-$points ($prefix)\t$comment\n";
	} else {
	    print "\t($prefix)\t$comment\n";
	    print COMMENTS "\t($prefix)\t$comment\n";
	}
    
	if (($interactive == 1) || (($interactive == 2) && ($showdiff == 1)))
	{
    	    ($diffFile) = split (/\./, $casefile);
	    $outDiffFile = "$TEST_OUTPUT_DIR/$prefix.diff";

	    if ($origdiffout ne "") {
		&writeFile($outDiffFile, $origdiffout);
	    }
	    if ($needsOutErrSplit) {
		$errDiffFile = "$TEST_OUTPUT_DIR/$prefix.err.diff";
		if ($origdifferr ne "") {
		    &writeFile($errDiffFile, $origdifferr);
		}
	    }
      
	    # save the students output
	    system("mv -f $testout $TEST_OUTPUT_DIR/$goodout > /dev/null 2>&1");
	    system("mv -f $testout.unfilt $TEST_OUTPUT_DIR/$goodout.unfilt > /dev/null 2>&1");
	    if ($needsOutErrSplit) {
		system("mv -f $testerr $gooderr");
	    }
	}
    }
    else # didn't fail test
    {
	# don't save any output
	#system("rm -f $casefile.s");
    }
}

# ---------------------------------------------------------------------------
# Write the score to the score file.

# writes the score file, called SCORE, which is just a number 
# indicating the student's numerical grade.
open SCORE, ">SCORE" or die "Could not open score.";
print SCORE "$totscore\n";
close SCORE;
# don't generate score file.

if ($passedall == 1) {
    print COMMENTS "\t(our tests)\tPassed all tests -- congratulations!\n";
}

close COMMENTS;

# appending will be done in 143makescores
#if (open TEST, "COMMENTS.ta") {
#    close TEST;
#    print "appending COMMENTS.ta to COMMENTS file\n";
#    system("cat COMMENTS.ta >> $comments");
#}


#if (open TEST, "$BIN_DIR/143gradesingle.pp$ppnumber") {
#    close TEST;
#    print "running supplmental script\n";
#    system("$BIN_DIR/143gradesingle.pp$ppnumber >> $comments");
#}

print "=====================================================================\n";

exit;




# arg 1: student file getting diffed
# arg 2: solution file with which to diff

# returns: string with diff results.  empty if diffed cleanly.
sub diffFile() {

    my ($test, $curout) = @_;
    my $bad = 1;
    my $outnum = 1;

    my $goodout = $curout;

    my $origdiffout = "";
    my $diffout = "";

    system("mv -f $test " . $test . ".tmp");
    system("$BIN_DIR/chomp < $test.tmp > $test");
    system("rm -f " . $test . ".tmp");
  
    &runPPCmd($test);
    
    # loop over the output files.  try, in order, test.out, test.out1
    # test.out2, etc.  if any match, then the submission passes the test.

    while ( $bad ) {

	$bad = 0;
	
	# see if the output file exists
	unless (open NEWOUT, "$TESTS_DIR/$curout") {
	    $bad = 1;
	    last;
	}
	
	# print "testing: $curout\n";
	close NEWOUT;
	open DIFF, "$BIN_DIR/chomp < $TESTS_DIR/$curout | $BIN_DIR/$filter | diff -w -i - $test |";
	$diffout = "";

	# searches the diff output for problems.  it seems that this while
	# loop checks the diff output for non-blank lines
	# other white space characters are handled by the -w diff flag
	while (defined($line = <DIFF>)) {
	    $diffout .= $line;
	    chomp $line;
	    if ($line !~ /^[<>]/ ) {
		#print "non-diff line: $line\n";
		next
		} 
	    elsif ($line =~ /([<>][\s]+$)/ ) {
		#print "diff line w/ only ^ or \' \': $line\n";
		next;
	    } else {
		#print "bad: $line\n";
		$bad=1;
	    }
	}
	
	# save the diff output for the first file, in case the submission
	# failes the test and we need to write test.diff to the
	# submission directory
	if ($curout eq $goodout) {
	    $origdiffout = $diffout;
	}
	
	$curout = $goodout . $outnum;
	$outnum++;
    }

    if ($bad) {
	return ($origdiffout);
    } else {
	return ("");
    }
}


# arg 1: file to print to screen
sub printFile() {
    open FILE, "$_[0]" or die "Could not open: $_[0].";
    while (defined($line = <FILE>)) {
	print $line;
    }
    close FILE;
}


# arg 1: filename to write to
# arg 2: string to write
sub writeFile() {
    open DIFFOUT, ">$_[0]" or 
	die "Could not open $_[0] for writing";
    print DIFFOUT $_[1];
    close DIFFOUT;
}


sub runPPCmd()
{
    my $test = $_[0];
    if (open(PPCMD, "$BIN_DIR/$filter")) {
       close PPCMD;
       # print "I AM FILTERING!\n\n\n";
       system("cp -f $test $test.unfilt"); 
       system("mv -f $test " . $test . ".tmp");
       system("$BIN_DIR/$filter  < $test.tmp > $test");
       system("rm -f " . $test. ".tmp");
    }
    else
    {
#	$filter = 1;
    }

}

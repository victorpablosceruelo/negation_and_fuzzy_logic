#!/usr/bin/perl -w 

use Time::Local;
use File::Temp;
use strict;


# Figure out where we were executed from and add that directory to the
# include path.  This is unwise in that it opens the possibility of
# sucking in anything included in the directory we're in, not just the
# stuff we want to allow sucking in..

sub BEGIN {
    # Seal us up a bit for living la vida tainted
    $ENV{'PATH'} = "/bin:/usr/bin";
    delete @ENV{'IFS', 'CDPATH', 'ENV', 'BASH_ENV'};

    my $dir = ($0 =~ m!^(.*)/!);
    if ($dir !~ m!^/!) {my $tmp = `pwd`; chop($tmp);}
    @INC=($dir, @INC) unless $INC[0] eq $dir;
}
#----------------------------------------------------------------------------------       
my $configfilepath = "/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tmp";                 
my $zchaffdir =      "/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/low_level_ops";
my $tmpfiledir =     "$configfilepath/SAT";  # not cleaned, default SAT subdir also has zchaff
#----------------------------------------------------------------------------------       
my $LEN = 0;
my $BINFLAG = 0;
my $filename = "RNDB.txt";
my $debug = 0;
my $admin = 0;   # 0 outputs only solutions; 1 gives extra info to parse

# Sun Jul 17 08:16:24 2005 reads, converts and writes a record at a time
# Mon Jul 31 08:56:12 2006 added filename as arg like crack_cnf has.
# Tue Jun  5 09:29:15 2007 optional arg directory to put temp files
# Wed Jun  6 11:08:01 2007 added help
# Wed Jul  4 08:11:04 2007 added $admin to quiet output

&main(@ARGV);

sub usage_abort 
{
    die "Usage: % ./crack_ndb.pl [-b <binary>] filename [-d temp file directory]\n";
}


sub command_args
{
    my $cmdlineflag = @_;  #number of args
    my @args = @_;
    my $i = 0;

    if($cmdlineflag == 0){
        &usage_abort();
    }
    
    if($cmdlineflag > 0) {
        if ($args[0] =~ /^[?]$/ ) {
            &usage_abort();
        }
    }

    while($i < $cmdlineflag) {
        if ( $args[$i] =~ /^-([BbDd])$/ ) {
            my $c = uc $1;
            
            if ( $c =~ /B/ ) {
                $BINFLAG = 1;
#               $admin && print("binary input\n");
                $i++;
            } elsif ($c =~ /D/) {
                $tmpfiledir = $args[$i+1];
                if($tmpfiledir !~ /^[\w\d\-\\\/\._]+$/ ) {
                    die "crack_ndb: -d arg <$tmpfiledir> is invalid\n";
                }
                $i+=2;
            } else {
                &usage_abort;
            }
        } else {
            $filename = $args[$i];
            if($filename !~ /^[\w\d\-\\\/\._]+$/ ) {
                die "crack_ndb: filename <$filename> is invalid\n";
            }
            $admin && print("using file $filename\n");
            $i++;
        }
    }	# end while
    return;
}


# i noticed that this may take awhile for gigantic files
sub wc_lines {
    my $fn = $_[0];
    $admin && print "patiently getting line count..\n";
    my $wc = `wc -l $fn`;
    my $sz = 0;
    $admin && print $wc;
    if ( $wc =~ /^(\d+)/ ) {
        $sz = $1;
    }
    return $sz;
}


sub check_alldontcares {
    my $r = $_[0];
    my $l = $_[1];
    my $dccount = ($r =~ tr/\*//); 
    return $dccount == $l;
}


sub convert_alldontcares2cnf {
    my $len = $_[0];
    my $i = 0;
    my $s = "";

# for each position output a line with both +/-
# each line ends with a "0"

    while ($i < $len ) { 
        my $z = $i+1;
        $s = $s . "-$z 0 \n ";
        $s = $s . "$z 0 \n ";
        $i++;
    }
    return $s;
}


sub convert2cnf {
    my $y = $_[0];
    my $s = "";
    my $i = 0;

    while ($i < $LEN ) { 
        my $z = $i+1;
        my $c = substr($y,$i,1);
        if ( $c =~ /1/) {         # complement the output v.23
            $s = $s . "-$z ";
        } elsif ( $c =~ /0/ ) {
            $s = $s . "$z ";
        }
        $i++;
    }
    $s = $s . "0 \n";  # each line ends with a "0"
    return $s;
}

    
sub print_rndb_dimacs_header {
    my $cl = $_[0];
    my $va = $LEN;
    my $gl = $cl/$va;

    print "c  cnf \#var \#clause\nc \#var is RN, \#clauses is DB reclength\n";
    print "c guideline ratio is around 4.3: <$gl>\n";
    print "p cnf $va $cl\n";
    return;
}


sub read_rndb {
    my $l = 0;
    my $n = 0;
    my $explicits = 0;
    my $fn = "$configfilepath/$filename";
    stat($fn);

# no RNDB exists, die
    if (! -e _) {   
	die("$filename does not exist\n\n");
    }
    my $sz = &wc_lines($fn);

    open IRNDB,$fn;

    if (! -e "$tmpfiledir" ) {
	mkdir "$tmpfiledir" || die("can't create tmp directory");
	-w "$tmpfiledir" || die("can't write to tmp directory");
    }

    open OUTRNDB,">$tmpfiledir/RNDB.dimacs";
    select OUTRNDB;

    while(<IRNDB>){
	if( /^([01\*]+)$/ ){
	    my $rec = $1;
	    $debug && print STDOUT "read record <$rec>\n";
	    $n++;
	    my $l = length $rec;
	    if ( !$LEN ) { 
                $LEN = $l; 
                &print_rndb_dimacs_header($sz); # needs $LEN too
            }
#            print STDOUT "here's rec <$n> <$rec>\n";
	    $l > $LEN && die("records are not the same length $l $LEN\n");
            my $c;
            if (&check_alldontcares($rec,$l)) {
                $c = &convert_alldontcares2cnf($l);
            } else {
                $c = &convert2cnf($rec);
            }
	    print OUTRNDB $c;    
	} elsif ( /^[ab]([1-9][0-9]*)$/ ){
            $LEN = $1;
            &print_rndb_dimacs_header($sz); # needs $LEN too
            print OUTRNDB "0\n";            # empty clause, no constraint
        }
    }

    close IRNDB;
    close OUTRNDB;
    select STDOUT;

    $admin && print "db size is $sz; read $n records\n";
    return $sz;
}


sub check_string {
    my $sref = $_[0];
    my $l = $_[1];
    my $x = 0;
    my $c = 1;
    my $rec = "";

    while ($c <= $LEN && $x < $l) {
	my $s = $$sref[$x];
	my $n;
	if ($s !~ /^-?(\d+)/) {
	    last;
	}
	$x++;
	$c++;
    }
    return $x;
}

# convert cnf to rndb format (0,1,*)
sub verify_solution {
    my $sref = $_[0];
    my $l = $_[1];
    my $x = 0;
    my $c = 1;
    my $rec = "";

    while ($c <= $LEN && $x < $l) {
	my $s = $$sref[$x];
	my $n = abs($s);
        if ($n == $c) {
            if ($s < 0) {
                $rec = $rec . "0";
            } else {
                $rec = $rec . "1";
            }
        } else {
            $rec = $rec . ".";
        }
        $c++;
        $x++;
    }
#    while ($c < $LEN) {
#	$rec = $rec . ".";
#	$c++;
#    }

#    print "$rec\n";
    return $rec;
}


# append complement of solution to RNDB.dimacs 
sub complement_solution_to_file {
    my $sref = $_[0];
    my $l = $_[1];
    my $x;
    my $s;
    my $fn = "$tmpfiledir/RNDB.dimacs";
    open DIMACS,">>$fn" || die("no rndb dimacs for append");

    for ( $x = 0; $x < $l; $x++) {
	$s = $$sref[$x];
        $s = -$s;
#	    print "$$sref[$x] $s\n";
	print DIMACS "$s ";
    }
    print DIMACS "0\n";

    close DIMACS;
    return;
}


# take an 8 digit binary string and convert it to a decimal number
sub convert_to_number {
    my $s = $_[0];
    my $i = 0;
    my $d = 0;

    while ($i < 8) {
	my $t = substr($s,7-$i,1);
	$d = $d + ($t * (2**$i));
	$i++;
    }

    $debug && print "converted <$_[0]> now <$s> to <$d>\n";
    return $d;
}


# take a binary string and convert groups of eight digits to letters,
# leave remainders as 0's and 1's
sub convert_to_letters {
    my $n = $_[0];
    my $l = length $n;
    my $i=8;
    my $r = "";

    $debug && print "starting with <$n> length <$l>\n";

    while ($i <= $l) {
	my $s = substr($n,$i-8,8);
	my $d = &convert_to_number($s);
	my $a = chr($d);
	$debug && print "found letter <$a> from <$s> to <$d> \n";
	$r = $r.$a;
	$debug && print "string now <$r> for <$n>\n";
	$i = $i+8;
    }

# get the rest of it
    $i = $i - 8;
    if ($i < $l) {
	my $e = $l - $i;
	my $s = substr($n,$i,$e);
	my $s0 = $s . ("0"x (8 - $e));
	my $s1 = $s . ("1"x (8 - $e));
	my $t0 = &convert_to_number($s0);
	my $t1 = &convert_to_number($s1);
	if ($t0 > 126) { $t0 = 62;}  # use '>' to indicate over ascii table
	if ($t1 > 126) { $t1 = 62;}  # use '>' to indicate over ascii table
	if ($t0 < 32) { $t0 = 60;}  # use '<' to indicate under ascii table
	if ($t1 < 32) { $t1 = 60;}  # use '<' to indicate under ascii table
	my $a0 = chr($t0);
	my $a1 = chr($t1);
	$r = $r. "[" . $a0 . "-". $a1 ."]";
	$debug && print "the rest of it stays bits <$s> <$r>\n";
    }
    return $r;
}


# redirect zchaff results into temp file for satplay
sub find_satsolution
{
    `$zchaffdir/zchaff $tmpfiledir/RNDB.dimacs 900 > $tmpfiledir/satresults`;
}


# append complement of sat solution to RNDB.dimacs
# convert positive solution to rndb format
# convert non-binary database to text
sub satplay {
#    print "in satplay..";

    my $rec;
    my $foundflag = 0;
    my $fn = "$tmpfiledir/satresults";
    open SATR,$fn || die("no satresults");

    while (<SATR>) {
	my $i = $_;
	chop $i;
	my @string;
	my $l;
	my $x;
	my $s;

	if ( $i =~ /^(-?\d+)/) {        # the solution
	    @string = split(/ /, $i);
	    $l = @string;
#	    print "string length is $l\n";
            $foundflag = 1;
	    $l = &check_string(\@string,$l);
	    &complement_solution_to_file(\@string,$l);  
	    $rec = &verify_solution(\@string,$l);
            if(!$BINFLAG) {
                $rec = &convert_to_letters($rec);
            }
	    print "$rec\n";            # this is a DB record!!
	}
    }
    close SATR;
    return $foundflag;
}


sub main {
    &command_args(@_);
    $admin && print `date`;

    my $rtn = &read_rndb();
    $admin && print "read $rtn ";
    $admin && $BINFLAG && print "binary ";
    $admin && print "negative database records\n";

    my $dbrec_count = 0;
    $admin && print "starting sat solving..\n";
    $admin && print `date`;

    while(1) {
        &find_satsolution();
        if (! &satplay() ) {    
            last;               # instance unsatisfiable
        }
        $dbrec_count++;
    }
    
    $admin && print "$dbrec_count POSITIVE RECORDS FOUND\n";
    $admin && print `date`;
    return; 
}

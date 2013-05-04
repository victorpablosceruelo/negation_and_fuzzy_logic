#!/usr/bin/perl -w 

use Time::Local;
use File::Temp;
use strict;

# This file is part of the Negative Set Sharing (NSH) application of
# the Online Negative Database (NDB), Copyright (C) 2007-2008 elena s
# ackley and the Regents of the University of New Mexico This program
# is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free
# Software Foundation; either version 2 of the License, or (at your
# option) any later version.  This program is distributed in the hope
# that it will be useful, but WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
# PURPOSE.  See the GNU General Public License for more details.  You
# should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

# -------------------------------------------------------------------------
# Filename      : SH2CSH.pl
# Language      : Perl 
# Purpose : builds a compressed database representing positive strings
#           precisely
# 
# Creator       : E.S. Ackley
# Creation Date : Sat Apr 14 11:39:02 2007
# Updated       : Tue Apr 22 14:28:20 2008 
# -------------------------------------------------------------------------
#
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

my $version = "v.25.9";
my $negdbpath = "nice -19 /home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/low_level_ops/c_code";     
my $configfilepath =  "/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tmp";                 
my $tmpfilepath    = "";     # set by script
my $cshfilename    = "NSH0"; # default negative database name
my $dbfilename     = "SH0";  # default positive input database name

my $KNOWN_MISSING=0;        # negative length argument to do additive way
my $CALC_MISSING=1;         # 1=calculates missing, if LEN le 32, and SH gt
                            # CUTOVER_PERCENT of powerset, o.w. "removes" known

my $CUTOVER_PERCENT=1.0;    # 1 essentially disables this feature
                            # (needs <1.00), 
                            # except for full powerset special case;

my $USEGRAYCODE=1;          # 1=uses graycode sort; 0=no sort  
my $USENEGDB_SCRIPTOPT=1;   # 1=uses -w negdb script option; 0=atomic transactions
my $WRITEAFTERN=1000;       # for scripting, save after this many operations

my $BTMODE=1;               # 0 for binary (no *'s); 1 for ternary
my $SANITY=1;               # 1 will die when insanity happens; 0 will be silent

my $LEN=0;
my $MAXL=100;
my $INT_SIZE=32;
my $N=0;
my $MAX_KEY=1;              # updated later to be len/2  v.17 and BTMODE
my $C=2;                    # constant for max_key as a log of LE
my $G=1;                    # additive constant for max_key

my $ln2 = log(2);           # natural log of 2 constant

my $cleanuppercent=0;       # 0 for no post cleanups; ow. percent of ndbsize
my $sleeptime=0;            # after adds, sleep 2 or 3 allows processor remain cool 

my $debug=0;
my $debug2=0;               # debug graycode
my $admin=0;

my $stderrors = "2> /dev/null";
my $stdoutput = "> /dev/null";


exit(&main(@ARGV));

sub usage_abort 
{
    die "Usage: % ./SH2CSH.pl [-]len [SHfilename [CSHfilename]] mode\n";
}

# Usage 1: ./SH2CSH.pl len SHfilename CSHfilename mode

sub command_args
{
    my $argc = @_;   #number of args
    my @args = @_;

    if($argc==0) {
        &usage_abort();
    }
    
    if($argc > 0){
    # print "args 0 is <$args[0]>\n";
        if ($args[0] =~ /^[Z?|help]/ ) {    # unquoted ? looks like a capital z
            &usage_abort();
        }
    }
 
    # VALIDATE LEN
    if( $args[0] =~ /-?[1-9][0-9]*/ ) {
        $LEN = $args[0];
        if ($LEN < 0) {             # v.14.2: records in sh file are known missing
            $KNOWN_MISSING = 1;
            $LEN = -1 * $LEN;
        }

        if ($LEN > $MAXL || $LEN == 0 ) {
            die "SH2CSH: Record length (first arg) is INVALID, can't be greater than $MAXL\n";
        }
    } else {
        die "SH2CSH: Record length (first arg) must be an integer\n";
    }

    if ($argc == 2){    
        $BTMODE = $args[1];
    }
    elsif ($argc >= 3){	
        if ( $args[1] =~ /^[a-zA-Z0-9\.\_\/\\-]{1,255}$/ ) {  # alt sh file
            $dbfilename = $args[1];
        } else {
            die "SH2CSH: positive input filename must be alphanumeric and less than 255 letters long>\n";
        }
	if ($argc == 3){
	    $BTMODE = $args[2];
        }  elsif ($argc == 4) {
            if ( $args[2] =~ /^[a-zA-Z0-9\.\_\/\\-]{1,255}$/ ) {  # alt csh file
                $cshfilename = $args[2];
            }else {
                die "SH2CSH: csh output filename must be alphanumeric and less than 255 letters long>\n";
            }
	    $BTMODE = $args[3];
        }
    }
    # VALIDATE MODE
    if ($BTMODE == 0 || $BTMODE == 1) {
    	 if ($BTMODE == 0){
	    $debug && print "SH2CSH using bSH\n";
         }
         else{
            $debug && print "SH2CSH using tSH\n";
         }     			    
    } else{
            die "CSH mode can be only 0 (bSH) or 1 (tSH) \n";
          }

    if($BTMODE==1) {
     #$MAX_KEY = int($LEN/2+.5);    # v.17 round up
        $MAX_KEY = &logbase2($LEN) * $C + $G;     # try v.25.6, v.26.7 
    } else {                          # fully specified; no *'s; bSH.
        $MAX_KEY = $LEN;
    }

    ($admin || $debug) && print "MAX_KEY is $MAX_KEY\n";
}


# when positive record count is too big, figure out and remove those
# that are missing instead by printing them to tmp db file; limited to
# 32-bit lengths.
sub read_db_sort_for_missing {
    my $dbfn = $_[0];
    my $mdbfn = $_[1];
    my $fn = "$configfilepath/$dbfn";
    my $total = 0;
    my $rej = 0;
    my $endloop = (2 ** $LEN) - 1;
    my $num = 0;       # current record number
    
    open SORTDB,"sort -g $fn |" || die "find missing failed to sort $fn";
    open(FULLCSH,">$mdbfn") || die "Unable to create new missing sh $mdbfn\n";

    while(<SORTDB>) {
        my $s;
        my $l;
        my $okp = 0;
        my $i = 0;
        my $d = 0;

	if(/^([01]+)$/ ){
	    $s = $1;
	    $l = length $s;
	    $debug && print "($total) read SH record <$s>\n";
            if ($l > $LEN || $l < 0) {
                print("rejecting SH record: <$s> due to bad length, $l, expecting $LEN\n");
                $rej++;
            } else {
                $okp = 1;
            }
	} else {
	    print("rejecting SH record: $_\n");
	    $rej++;
	}

        $debug && print "read_db_sort_for_missing: rec is <$s>\n";
	if ($okp){
            $total++;
            while($i < $LEN) { $d += ((chop $s) << $i++); }  # limited to 32-bit integers

            $debug && print "read_db_sort_for_missing: as num <$d>\n";
        
            while($num < $d) {
                my $rec = sprintf("%b",$num);
                $rec = pad_left_length($rec,"0",$LEN);
                print FULLCSH "$rec\n"; 
                $debug && print "writemissing rec ($num): <$rec>\n";
                $num++;
            }
            $num++; # get past d
        } # done with this rec
    } #end while loop
    
    close SORTDB;

# completes the rest of the powerset not in DB, here 
    $debug && print "completing end loop <$endloop> for length $LEN, starting at <$num>\n";

    while($num <= $endloop) {
        my $rec = sprintf("%b",$num);
        $rec = pad_left_length($rec,"0",$LEN);
        print FULLCSH "$rec\n";   
        $debug && print "writemissing rec ($num): <$rec>\n";
        $num++;
    }

    close FULLCSH;
    $debug && print("SH2CSH read_db_sort_for_missing: read in $total records, rejected $rej, saved $num\n");

    return $num;
}


#read in binary positive strings of the same length that are MISSING
#from SH; return number of records;
sub build_csh_from_missing {
    my $dbfn = $_[0];        # full path input here
    my $rej = 0;
    my $total = 0;

    open INDB,$dbfn || die "Unable to open positive $dbfn";

    if($USENEGDB_SCRIPTOPT) {
        $sleeptime = &script_header_select();
    }

    while(<INDB>){
	my $rec="";
        my $l;
	my $okp=0;

	if(/^([01]+)$/ ){
	    $rec = $1;
	    $l = length $rec;
	    $debug && print STDOUT "($total) read SH record <$rec>\n";
            if ($l > $LEN || $l < 0) {
                print("#rejecting SH record: $rec due to bad length, $l, expecting $LEN\n");
                $rej++;
            } elsif ($l != $LEN) {
                $rec = &pad_left_length($rec,0,$LEN);
            }  # o.w. nothing to do
	    $okp=1;
	} else {
	    print("#rejecting SH record: $_\n");
	    $rej++;
	}
	
	if ($okp){
            $total++;
            if (&add_db_rec($rec,$total) == 0) {
                $N++;
                sleep $sleeptime;
            }
        }
    }
    close INDB;

    if($USENEGDB_SCRIPTOPT) {
        &script_done_deselect_run();
    }

    $debug && print("SH2CSH build_csh_from_missing: read in $total missing records, rejected $rej, added $N\n");
    
    return $N;
}

            
#read in binary positive strings of the same length; return
#number of records;
sub read_db_and_build_csh {
    my $dbfn = $_[0];
    my $rej = 0;
    my $fn = "$configfilepath/$dbfn";
    my $total = 0;

    open INDB,"$fn" || die "Unable to open positive $fn";

    if($USENEGDB_SCRIPTOPT) {
        $sleeptime = &script_header_select();
    }

    while(<INDB>){
	my $rec="";
        my $l;
	my $okp=0;

	if(/^([01]+)$/ ){
	    $rec = $1;
	    $l = length $rec;
	    $debug && print STDOUT "($total) read SH record <$rec>\n";
            if ($l > $LEN || $l < 0) {
                print("#rejecting SH record: $rec due to bad length, $l, expecting $LEN\n");
                $rej++;
            } elsif ($l != $LEN) {
                $rec = &pad_left_length($rec,0,$LEN);
            }  # o.w. nothing to do
	    $okp=1;
	} else {
	    print("#rejecting SH record: $_\n");
	    $rej++;
	}
	
	if ($okp){
            $total++;
            if (&remove_db_rec($rec,$total) == 0) {
                $N++;
                sleep $sleeptime;
            }
        }
    }
    close INDB;

    if($USENEGDB_SCRIPTOPT) {
        &script_done_deselect_run();
    }

    $debug && print("SH2CSH read_db_and_build_csh: read in $total records, rejected $rej, removed $N\n");

    return $N;
}


#read in binary positive strings of the same length; interpret as
#graycode; sort; and convert back to original before "adding" or
#"removing" to the compressed db; return number of records (v.16)
sub read_db_as_graycode {
    my $dbfn = $_[0];     # full path to db here
    my $rej = 0;
    my $tmpgrayfn = "$tmpfilepath/dbgray.tmp";
    my $total = 0;

    $debug && print "read_db_as_graycode: dbfn is <$dbfn>\n";

    open INDB,"$dbfn" || die "Unable to open positive $dbfn";
    open(GRAYDB,">$tmpgrayfn") || die "Unable to create tmp gray db $tmpgrayfn\n";

    $debug && print "read_db_as_graycode: here before the while!\n";

    while(<INDB>){
	my $rec="";
        my $l;
	my $okp=0;

	if(/^([01]+)$/ ){
	    $rec = $1;
	    $l = length $rec;
	    $debug && print "($total) read SH record <$rec>\n";
            if ($l > $LEN || $l < 0) {
                print("rejecting SH record: $rec due to bad length, $l, expecting $LEN\n");
                $rej++;
            } elsif ($l != $LEN) {
                $rec = &pad_left_length($rec,0,$LEN);
            }  # o.w. nothing to do
	    $okp=1;
	} else {
	    print("rejecting SH record: $_\n");
	    $rej++;
	}
	
	if ($okp){
            my $g = &gray2binary($rec);
            print GRAYDB "$g\n";
            $debug && print "printing gray rec: <$g>\n";
            $total++;
        }
    }

    close GRAYDB;
    close INDB;
    $debug && print("SH2CSH read_db_as_graycode: read in $total records, rejected $rej\n");
    return $total;
}

 
sub build_csh_from_graycode
{   
    my $missingflag = $_[0];     # 1="add"; =0 "remove"
    my $tmpgrayfn = "$tmpfilepath/dbgray.tmp";
    my $total = 0;

    open INGRAYDB,"sort -g $tmpgrayfn |" || die "Unable to open sorted tmp gray db $tmpgrayfn";

    if($USENEGDB_SCRIPTOPT) {
        $sleeptime = &script_header_select();
    }

    while (<INGRAYDB>) {
	my $grec="";
        
	if(/^([01]+)$/ ){
	    $grec = $1;
            $total++;
	    $debug && print STDOUT "($total) read GRAY record <$grec>\n";
            my $rec = &binary2gray($grec);
            if($missingflag) {
                if (&add_db_rec($rec,$total) == 0) {
                    $N++;
                }
            } else {
                if (&remove_db_rec($rec,$total) == 0) {
                    $N++;
                }
            }
            sleep $sleeptime;
        }
    }

    close INGRAYDB;
    (!$debug && !$admin) && unlink($tmpgrayfn);

    if($USENEGDB_SCRIPTOPT) {
        &script_done_deselect_run();
    }

    $debug && print("SH2CSH build_csh_from_graycode: added/removed <$N> recs\n");
    return $N;
}


sub gray2binary{
    my $cg = $_[0];
    my $cb= "";
    my $len = length $cg;
    my $j;

    $cb = substr($cg,0,1);

    for($j=1; $j<$len; $j++) {
        my $x = &xor(substr($cb,$j-1,1),substr($cg,$j,1)); 
        $cb = $cb . $x;
    }
    return $cb;
}


sub binary2gray{
    my $cb = $_[0];
    my $cg= "";
    my $len = length $cb;
    my $j;

    $debug2 && print "b2g: cb is <$cb>, cg is <$cg>\n";
    $cg = substr($cb,0,1);
    $debug2 && print "b2g: cb is <$cb>, cg is <$cg>\n";
    for($j=1; $j<$len; $j++) {
        my $x = &xor(substr($cb,$j-1,1),substr($cb,$j,1)); 
        $cg = $cg . $x;
        $debug2 && print "b2g: cb is <$cb>, cg is <$cg>\n";
    }
    return $cg;
}


sub xor{
    my $x = $_[0];
    my $y = $_[1];
    if($x eq '0' && $y eq '1' || $y eq '0' && $x eq '1') {
        return '1';
    } else {
        return '0';
    }
}


sub remove_db_rec {
    my $rec = $_[0];
    my $zit = $_[1];
    my $fn = "$configfilepath/$cshfilename";
    my $l = length $rec;
    my $rtn = 0;
    my $ans;

    if($USENEGDB_SCRIPTOPT) {
        return &script_a_line($rec,$zit,"R");
    }

    my $cmd  = "$negdbpath/negdb -r \"$rec\" -f \"$fn\" -b -m $MAX_KEY -z $zit -D \"$tmpfilepath\" -T $cleanuppercent";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "SH2CSH remove_db_rec: ($zit) failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0) {
            ($debug || $admin) && print("SH2CSH remove_db_rec: ($zit) removed [$rec]\n");
            return $ans;
        }
            if($ans > 1) {
                print("SH2CSH remove_db_rec: ($zit) found error [$ans]\n");
            }
    }
    $debug && print "args to remove_db_rec: rec is <$rec>, zit is <$zit>, ans is <$ans>\n";
    $SANITY && die "SH2CSH FAILURE: args to remove_db_rec: rec is <$rec>, zit is <$zit>, ans is <$ans>\n";
    return $ans;
}


# note: compile negdb with NPG_DETERMINISTIC option 
sub add_db_rec {
    my $rec = $_[0];
    my $zit = $_[1];
    my $fn = "$configfilepath/$cshfilename";
    my $l = length $rec;
    my $rtn = 0;
    my $ans;

    if($USENEGDB_SCRIPTOPT) {
        return &script_a_line($rec,$zit,"A");
    }

    my $cmd  = "$negdbpath/negdb -a \"$rec\" -f \"$fn\" -b -m $MAX_KEY -z $zit -D \"$tmpfilepath\" -T $cleanuppercent";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "SH2CSH add_db_rec: ($zit) failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0) {
            ($debug || $admin) && print("SH2CSH add_db_rec: ($zit) added [$rec]\n");
            return $ans;
        }
            if($ans > 1) {
                print("SH2CSH add_db_rec: ($zit) found error [$ans]\n");
            }
    }
    $debug && print "args to add_db_rec: rec is <$rec>, zit is <$zit>, ans is <$ans>\n";
    $SANITY && die "SH2CSH FAILURE: args to add_db_rec: rec is <$rec>, zit is <$zit>, ans is <$ans>\n";
    return $ans;
}


sub new_empty {
    my $mode = $_[0];  # 0 is "empty DB", 2 is "full DB"
    my $rtn = 1;
    my $fn = "$configfilepath/$cshfilename";
    my $cmd = "$negdbpath/negdb -f \"$fn\" -l $LEN -N $mode -b -D \"$tmpfilepath\" -m $MAX_KEY ";
    $debug && print("new_empty: cmd is [$cmd]\n");
    `$cmd $stdoutput $stderrors`;
    
    if ($? == -1) {
        print "new_empty: failed to execute [$!]\n";
        $rtn = $?;
    } else {
        $rtn = ($? >> 8); 
    }
    
    if($rtn == 0) {
        $admin && print("Created an empty CSH");
        $admin && print(" [$fn]");
        $admin && print "\n";
        return $rtn;
    } 
    print("New Empty CSH FAILED");
    print(" ($rtn)");
    print "\n";
    $SANITY && die "SH2CSH FAILURE: new_empty failed, mode is <$mode>\n";
    return $rtn;
}


sub logbase2 {
    my $x = $_[0];
    my $y = log($x)/$ln2;
    my $yint = int($y);
    
    $debug && print "The  base-2 log of <$x> is <$y> ";
    
#    if ($yint < $y) { 
#	$yint++;
#	$debug && print ", and the next highest int is <$yint>";
#    }

    $debug && print " using <$yint>\n";
    return $yint;
}


sub pad_left_length {
    my $s = $_[0];
    my $c = $_[1];
    my $l = $_[2];
    my $sl = length $s;
    my $ns = ($c x ($l - $sl)) . $s;   # pad left with zeros
    return $ns;
}


sub dbsize_stat {
    my $fn = $_[0];         # full path input
    my $len = $_[1];
    my $llen = length $len;

    $debug && print "record len is <$len>, number of bytes is <$llen>\n";        
    $debug && print "dbsize_stat: fn is <$fn>\n";

    my @statfn = stat($fn);
    if (! -e _) {
        die "SH2CSH: dbsize_stat on <$fn> failed\n";
    }
    my $lines = $statfn[7];       # file size in bytes
    $debug && print "dbsize_stat: lines stat is <$lines>\n";
    $lines = $lines - (1+1+ $llen); # subtract bytes in header
    $debug && print "dbsize_stat: lines stat is adjust to <$lines>\n";
    $lines = int(($lines + 1) /($len+1));  # don't forget newline byte
    $debug && print "dbsize_stat: lines is calculated <$lines>\n";
    return $lines;
}


# return TRUE (1) if csh size is greater than $CUTOVER_PERCENT of a
# full powerset for length LEN, LEN <= 32, and special case (2) for
# the powerset (v.16)
sub check_dbsize {
    my $dbfn = $_[0];
    my $rtn = 0;
    
    if($LEN <= $INT_SIZE){
        my $ps = 2 ** ($LEN);
        my $fn = "$configfilepath/$dbfn";
        my $lines = &dbsize_stat($fn,$LEN);
        $lines++;       # compensate for no header
        $admin && print "check_dbsize: <$fn> lines is calculated <$lines>\n";
        if($lines == $ps){
            $rtn = 2;
        } elsif ($lines >= $ps * $CUTOVER_PERCENT){
            $rtn = 1;
        }
    }
    return $rtn;
}


# bkup last CSH to bkup; remove old CSH incremental saves, and
# initialize Z to 1 for future incremental saves
sub bkupCSH {
   my $fn = "$configfilepath/$cshfilename";
   my $bkup = "$configfilepath/bkup-$cshfilename";

   stat($fn);
   if ( -e _ ) {
       $debug && print("filename: $fn exists, moving to $bkup\n");
       rename($fn,$bkup);
   }

#   unlink("$configfilepath/$fn.*");   doesn't work with *'s
   `rm -f $configfilepath/$fn.*`;
   
# initialize Z steps, remove old one first
   my $zfile = $configfilepath . "/Z";
   stat($zfile);
   if ( -e _ ) {
       unlink($zfile);
   }
   open(ZSTEPS,">$zfile") || die "Unable to open $zfile\n";
   print ZSTEPS "1"; 
   close(ZSTEPS);
}


sub cleanup {
    if(!$debug && !$admin){
        unlink("$tmpfilepath/adds.dat");
        unlink("$tmpfilepath/clean.dat");
        unlink("$tmpfilepath/sizes.dat");
        unlink("$tmpfilepath/distrib7-RNDB-0.dat");
        unlink("$tmpfilepath/missing.db");
        if($USENEGDB_SCRIPTOPT) {
            unlink("$tmpfilepath/$cshfilename.script");
        }
        rmdir($tmpfilepath) || die "SH2CSH: can't clean up $!";
    }
}


# neg len was used which means the input file contains the missing sg's
sub known_missing{
    my $dbfn = $_[0];
    my $fn = "$configfilepath/$dbfn";
    &new_empty(0);
    $debug && print "SH2CSH: known missing..reading the missing from $dbfilename.\n";
    if($USEGRAYCODE) {
        &read_db_as_graycode($fn) && &build_csh_from_graycode(1); # 1 means missing
    } else {
        &build_csh_from_missing($fn);    # no sorting
    }
}


# only upto 32 bit length, cause we don't have years to count
sub calc_missing{
    my $dbfn = $_[0];
    my $dbszchk = &check_dbsize($dbfn);
    if($dbszchk == 2) {       # power set in db
        &new_empty(0);

    } elsif ($dbszchk == 1) {

        $debug && print "SH2CSH: calc_missing is determining the missing..\n";
        my $mdbfn = "$tmpfilepath/missing.db";
        &read_db_sort_for_missing($dbfn,$mdbfn);

        &new_empty(0);
        if($USEGRAYCODE) {
            &read_db_as_graycode($mdbfn) && &build_csh_from_graycode(1); # 1 means missing
        } else {
            &build_csh_from_missing($mdbfn);    # no sorting
        }
    } else {
        &subtractive_way($dbfn);
    }
}


# creates a compressed representation by "removing" each positive element
# in db from a blank negative database (v.20); uses graycode sorting (v.23)
sub subtractive_way {
    my $dbfn = $_[0];
    my $fn = "$configfilepath/$dbfn";

    &new_empty(2);
    if($USEGRAYCODE) {
        &read_db_as_graycode($fn) && &build_csh_from_graycode(0); # 0 means not missing
    } else {
        &read_db_and_build_csh($dbfn);
    }
}


sub script_header_select {
    my $script = "$tmpfilepath/$cshfilename.script";
    open(NEGDBSCRIPT,">$script") || die "Unable to create negdb script $script\n";
    select NEGDBSCRIPT;    
    print "#script to create $configfilepath/$cshfilename\n";
    print "B1\n";
    print "Z1\n";
    print "L$LEN\n";
    print "K$MAX_KEY\n";
    print "D0$configfilepath\n";
    print "D1$configfilepath/$cshfilename\n";
    print "O\n";
    return 0;
}


sub script_a_line {
    my $rec = $_[0];
    my $zit = $_[1];
    my $op = $_[2];

    print "$op$rec\n";
    if( ($zit % $WRITEAFTERN)==0) {
        print "W\n";
    }
    return 0;
}


sub script_done_deselect_run {
    my $handle = select;
    print "W\n";
    print "#done\n";
    select STDOUT;
    close $handle;

    if($debug) {
        my $script = "$tmpfilepath/$cshfilename.script";
        print "CAT SCRIPT <$script>\n";
        open SCRIPT,"$script" || die "can't open script to cat\n";
        while (<SCRIPT>) {
            print $_;
        }
    }

    &script_run();
}


# note: sizes.dat will have the results as they happen.
sub script_run {
    my $ans;
    my $script = "$tmpfilepath/$cshfilename.script";
    my $cmd  = "$negdbpath/negdb -w \"$script\" ";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "SH2CSH script_run failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0) {
            ($debug || $admin) && print("SH2CSH script_run completed\n");
            return $ans;
        }
        if($ans > 1) {
            print("SH2CSH script_run found error [$ans]\n");
        }
    }
    $SANITY && die "SH2CSH FAILURE: script run <$script> failed, ans <$ans>\n";
    return $ans;
}


sub main
{
    &command_args(@_);

    &bkupCSH();

# make tmp dir for temporary ndb files
    $tmpfilepath = mkdtemp("$configfilepath/cshtmpSH2CSH_XXXXX");
    ($tmpfilepath =~ /undef/ ) && die "SH2CSH: fail to make temporary data directory\n"; 
    ($admin || $debug) && print "tmp dir is: $tmpfilepath\n";
        
    if($admin || $debug) {
        $stderrors = "2>> $tmpfilepath/errlog.txt";
        $stdoutput = ">> $tmpfilepath/log.txt";
    }
  
    if($KNOWN_MISSING) {                      # negative length arg 
        &known_missing($dbfilename);
    } elsif($CALC_MISSING && $LEN <= $INT_SIZE) {
        &calc_missing($dbfilename);
    } else {
        &subtractive_way($dbfilename);
    }

    if($debug || $admin) {
        my $fsz = &dbsize_stat("$configfilepath/$cshfilename",$LEN);
        print "SH2CSH: final file size is <$fsz>\n";
    }

    &cleanup();    
    return $N;
}

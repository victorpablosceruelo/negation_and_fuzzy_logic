#!/usr/bin/perl -w 

use Time::Local;
use File::Temp;
use File::Copy;
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
# Filename      : NSHamgu.pl
# Language      : Perl 
# Purpose       : a negative implementation of the amgu function
# 
# Creator       : E.S. Ackley
# Creation Date : Sat Apr 14 15:08:30 2007 
# Updated       : Thu Apr 24 06:57:52 2008 
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

my $version = "v.19.15";
my $configfilepath = "/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tmp";
my $tmpfilepath;   # set by script
my $negdbpath = "nice -19 /home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/low_level_ops/c_code";     
my $crackpath = "/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/low_level_ops";
my $nshfilename    = "NSH0";    #default negative database

my $LEN=0;
my $MAXL=1000;
my $MAX_KEY=1;             # determined by BTMODE for bu 
my $C=2;                   # constant for max_key as a log of LEN
my $G=1;                   # additive constant for max_key

my $ln2 = log(2);        # natural log of 2 constant

my $cleanuppercent=0;      # 0 for no post cleanups; ow. percent of ndbsize

my $debug=0;
my $admin=0;

my $stderrors = "2> /dev/null";
my $stdoutput = "> /dev/null";

my $SAVE_THE_OLD_NSH = 0;    # 1= saves NSH at each step (per file Z); 0 doesn't

my $BU_STEP_BY_STEP = 0; # 1 performs each B step separately (for testing
                         # purposes); 0 uses efficient negdb operation S

my $BTMODE=1;            # 0 for binary (no *'s); 1 for ternary
my $SANITY=1;            # 1 will die when insanity happens; 0 will be silent

exit(&main(@ARGV));

sub usage_abort 
{
    die "Usage: % ./NSHamgu.pl len v t [nshfilename] | v t\n";
}


sub command_args
{
    my $argc = @_;   #number of args
    my @args = @_;
    my $varg;
    my $targ;
    my $v;
    my $vcount;
    my $t;
    my $tcount;
    
    if($argc < 2 || $argc > 4) {
        &usage_abort();
    }

    if($argc > 0){
# print "args 0 is <$args[0]>\n";
        if ($args[0] =~ /^[Z?|help]/ ) {    # unquoted ? looks like a capital z
            &usage_abort();
        }
    }

# alternate NSH file name, assumes last of four args
    if($argc == 4) {
        if( $args[3] =~ /^[0-9a-zA-Z\-\/\\\._]{1,255}$/ ) {
            $nshfilename = $args[3];
        } else {
            die "NSHamgu: nsh filename must be alphanumeric and less than 255 letters long>\n";
        }
    }

    $LEN = &get_NSHlength();
    if ($LEN > $MAXL || $LEN == 0 ) {
        die "NSHamgu: NSH Record length is INVALID, can't be greater than $MAXL or less than 1\n";
    }
    
    if ($argc >= 3) {
        my $nshlen = 0;
# VALIDATE first arg, LENGTH
        if( $args[0] =~ /[1-9][0-9]*/ ) {
            $nshlen = $args[0];
            if ($nshlen > $MAXL) {
                die "NSHamgu: Record length (first arg) is INVALID, can't be greater than $MAXL\n";
            }
        } else {
            die "NSHamgu: Record length (first arg) must be greater than 0\n";
        }
        if ($nshlen != $LEN) {
            die "NSHamgu: Record length (first arg) must match NSH length ($nshlen)\n";
        }
        $varg = $args[1];
        $targ = $args[2];
    } else {             # allow for first length arg to be optional
        $varg = $args[0];
        $targ = $args[1];
    }

# VALIDATE AND PREPARE v arg: string of 0's and a single 1
    if($varg =~ /^([0]*1[0]*)$/ ){
        $v = $varg;
        $vcount = ($v =~ tr/1//); 
        if ($vcount != 1) {
            die "NSHamgu: error, v has $vcount 1's\n";
        }
        $v =~ s/0/\*/g;
        my $vlen = length $v;
        if ($vlen < $LEN) {
            $v = &pad_left_length($v,'*',$LEN);
        } elsif($vlen > $LEN) {
            die "NSHamgu: v argument cannot have more than $LEN digits\n";
        }
    } else {
        die "NSHamgu: v argument must have only one 1 and the rest 0's\n";
    }
    

# VALIDATE AND PREPARE t arg: string of 0's and 1's
    if($targ =~ /^([01]+)$/ ){
        $t = $targ;
        $tcount = ($t =~ tr/1//); 
        if ($tcount < 0 || $tcount > $LEN) {
            die "NSHamgu: error, t has $tcount 1's\n";
        }
        $t =~ s/0/\*/g;
        my $tlen = length $t;
        if ($tlen < $LEN) {
            $t = &pad_left_length($t,'*',$LEN);
        } elsif ($tlen > $LEN) {
            die "NSHamgu: t argument must have only $LEN 1's and 0's\n";
        }
    } else {
        die "NSHamgu: t argument must have $LEN 1's and 0's\n";
    }
    if($BTMODE==0) {
        $MAX_KEY = $LEN;           # no *'s
    } else {
#    $MAX_KEY=int($LEN/2);
    }
    $debug && print "nshamgu: max key is <$MAX_KEY>\n";
    return ($v,$vcount,$t,$tcount);
}


# negUnion two files, name1 and name2 
sub combine_two {
    my $name1 = $_[0];
    my $name2 = $_[1];

    $debug && print "combine_two input is <$name1> and <$name2>\n";

    my $fn = "$tmpfilepath/$name1.nsh";
    my $jn = "$tmpfilepath/$name2.nsh";
    my $outfn = "$tmpfilepath/$name1$name2".".nsh";

    my $rtn = &negRelOp($fn,$jn,$outfn,"U",$MAX_KEY);
    if($rtn != 0) {
        die "NSHamgu combine_two failed <$fn> <$jn> with rtn code $rtn\n";
    }

    my $l = length $outfn;
    $outfn = substr($outfn,0,$l - 4);   # remove the .nsh suffix
    $outfn = substr($outfn,rindex($outfn,"/")+1); # remove the tmpdir and /
    $debug && print "combine_two: output is in <$outfn>\n";
    return $outfn;      #final file name without .nsh suffix
}


# one rec with 0's instead of 1's and the rest *'s;
sub make_easy_singleton {
    my $rec = $_[0];
    my $fname = $_[1];
    my $len = length $rec;

    $rec =~ s/1/0/g;

    open SINGLE,">$fname" || die "cannot create $fname for making an easy singleton";

    print SINGLE "b$len\n";
    print SINGLE "$rec\n";
    close SINGLE;
}


# for each set bit in rec, create a pattern and append to a set
sub make_easy_singleton_patterns {
    my $rec = $_[0];
    my $fname = $_[1];
    my $len = length $rec;
    my $i = 0;
    my $qpattern = "*";
    $qpattern = &pad_left_length($qpattern,"*",$len);

    open SINGLE,">$fname" || die "cannot create $fname for making an easy singleton";

    print SINGLE "b$len\n";
    while($i < $len){
        my $idx = index($rec,"1",$i);
        if ($idx == -1) {last;}
        my $qrec = $qpattern;
        substr($qrec,$idx,1,"1");
        $debug && print "make_easy_singleton: qrec is <$qrec>, i is <$i>, idx is <$idx>\n";
        print SINGLE "$qrec\n";
        $i = $idx + 1;
    }
    close SINGLE;
}

# creates a single negative database that represents the complement;
# returns filename if successful.
sub create_complement2 {
    my $v = $_[0];
    my $vcount = $_[1];
    my $t = $_[2];
    my $tcount = $_[3];
    my $nvtcount = $LEN - $vcount - $tcount;

    if($nvtcount == 0) {          # short-circuit is no bits left
        $admin && print "NSHamgu: complement2 file size is 0\n";
        return "0";
    }

    my $jn = "$tmpfilepath/vt.nsh";
    my $fn = "$configfilepath/$nshfilename";
    my $nvtname = "nvt1-$nvtcount";
    my $outfn  = "$tmpfilepath/$nvtname.nsh";

# make a separate ndb containing a record with a pattern for each bit of v and t
    my $vtrec = $t;
    my $vidx = index($v,"1",0);
    ($vidx == -1) && die "NSHamgu: complement2 bad v <$v>\n";
    substr($vtrec,$vidx,1,"1");
    &make_easy_singleton_patterns($vtrec,$jn);

# get the negIntersection of the bit patterns in vt.nsh and NSH
# which is the complement

    my $rtn = &negRelOp($fn,$jn,$outfn,"I",$MAX_KEY);
    if(!($rtn == 0 || $rtn == 1)) {
        die "NSHamgu create complement 2 failed with rtn code $rtn\n";
    }

    if ($debug) {
        print "CRACK COMPLEMENT:\n";
        my $log = `$crackpath/crack_ndb.pl $tmpfilepath/$nvtname.nsh -b -d $tmpfilepath`;
        print "<$log>";
    }

    if($admin || $debug) {
        my $csz = &dbsize_stat("$tmpfilepath/$nvtname.nsh",$LEN);
        print "NSHamgu: nvt complement file size is <$csz>\n";
    }

    return $nvtname;
}


sub bu_subsume {
    my $name1 = $_[0];
    my $name2 = $_[1];
    my $op = "S";
    my $k;

    if($BTMODE==0) {
        $k = $MAX_KEY;                   # same as $LEN, no *'s
    } else {
#    (note: k = 1 is worse in the negative, unlike csh)
#    $k = int($LEN/2);
#    $k = $LEN - &logbase2($LEN);         # try len - log(len) for k.
        $k = &logbase2($LEN) * $C + $G;     # try v.19.11, v.19.12, v.19.13
    }

#combine the one $name1 (fn) with the one newname2 (jn) into outfn
    my $fn = "$tmpfilepath/$name1" . "1.nsh";
    my $jn = "$tmpfilepath/$name2.nsh";
    my $final = $name1 . "1BU" . $name2;   # old way
    my $outfn = "$tmpfilepath/$final.nsh";
    my $rtn = &negRelOp($fn,$jn,$outfn,$op,$k);
    if($rtn != 0) {
        die "NSHAMGU bu_subsume failed with rtn code $rtn\n";
    }

    if($admin || $debug) {    
        my $busz = &dbsize_stat($outfn,$LEN);
        print "NSHamgu: BU STAR file size is <$busz>\n";
    }

    if ($debug) {
        print "CRACK BU of vt:\n";
        my $log = `$crackpath/crack_ndb.pl $outfn -b -d $tmpfilepath`;
        print "<$log>";
    }

# return new final name without suffix and tmpdir
    $debug && print "bu_subsume: output is in <$outfn>, return <$final>\n";
    return $final;      #final file name without .nsh suffix
}



sub bu_subsume_step_by_step {
    my $name1 = $_[0];
    my $name2 = $_[1];
    my $op = "B";
    my $fn = "$tmpfilepath/$name1" . "1.nsh";
    my $jn = "$tmpfilepath/$name2.nsh";
    my $rtn;
    my $k;

    if($BTMODE==0) {
        $k = $MAX_KEY;                   # same as $LEN, no *'s
    } else {
#    $k = int($LEN/2);
#    $k = $LEN - &logbase2($LEN);         # try len - log(len) for k.
        $k = &logbase2($LEN) * $C + $G;    # try v.19.11, v.19.12, v.19.13
    }

# Step 1: complement name1 and name2 
    my $fnc = "$tmpfilepath/un_" . $name1 . "1.nsh";
    my $jnc = "$tmpfilepath/un_" . $name2 . ".nsh";

    $rtn = &unnegate($fn,$fnc,$k);
    if($rtn != 0) {
        die "NSHAMGU bu_subsume_step_by_step failed to unnegate <$fn> into <$fnc> with rtn code $rtn\n";
    }

    if($admin || $debug){
        my $vcsz = &dbsize_stat("$fnc",$LEN);
        print "NSHamgu: un_$name1 file size is <$vcsz>\n";
    }

    $rtn = &unnegate($jn,$jnc,$k);
    if($rtn != 0) {
        die "NSHAMGU bu_subsume_step_by_step failed to unnegate <$jn> into <$jnc> with rtn code $rtn\n";
    }
    
    if($admin || $debug){
        my $tcsz = &dbsize_stat("$jnc",$LEN);
        print "NSHamgu: un_$name2 file size is <$tcsz>\n";
    }
    
# Step 2: binary union the two complements using -x B
#combine the one $name1 (fn) with the one newname2 (jn) into outfn for bu
    my $finalc = "un_" . $name1 . "1BU" . $name2;   # old way
    my $outfnc = "$tmpfilepath/$finalc.nsh";
    $rtn = &negRelOp($fnc,$jnc,$outfnc,$op,$MAX_KEY);
    if(!($rtn == 0 || $rtn == 1)) {
        die "NSHAMGU bu_subsume_step_by_step failed $op with rtn code $rtn\n";
    }

    if ($debug) {
        print "CAT BU of vt before STAR:\n";
        my $log = `cat $outfnc`;
        print "<$log>";
    }

    if($admin || $debug) {    
        my $busz = &dbsize_stat($outfnc,$LEN);
        print "NSHamgu: BU file size is <$busz>\n";
    }


# Step 3: bu star - closure repeat until no change remains
    my $eqv = 0;
    my $cnt = 1;
    while($eqv==0) {
        my $cpyfnc = "$outfnc.cpy";
        copy($outfnc,$cpyfnc);
        ($admin || $debug) && print "NSHamgu: star closure count <$cnt>\n";
        $eqv = &negRelOp($outfnc,$cpyfnc,$outfnc,$op,0);  # no compression use k=0 
    }

    if ($debug) {
        print "CAT BU STAR of vt:\n";
        my $log = `cat $tmpfilepath/$outfnc`;
        print "<$log>";
    }

    if($admin || $debug) {    
        my $busz = &dbsize_stat($outfnc,$LEN);
        print "NSHamgu: BU STAR file size is <$busz>\n";   
    }


# Step 4: complement the result
#combine the one $name1 (fn) with the one newname2 (jn) into outfn for final
    my $final = $name1 . "1BU" . $name2;   # old way
    my $outfn = "$tmpfilepath/$final.nsh";

    $rtn = &unnegate($outfnc,$outfn,$k);  # use original k like internal way(or keep k value small?)
    if($rtn != 0) {
        die "NSHAMGU bu_subsume_step_by_step failed to unnegate <$outfnc> into <$outfn> with rtn code $rtn\n";
    }

    if ($debug) {
        print "CRACK BU STAR of vt:\n";
        my $log = `$crackpath/crack_ndb.pl $outfn -b -d $tmpfilepath`;
        print "<$log>";
    }

    if($admin || $debug) {    
        my $bustarsz = &dbsize_stat($outfn,$LEN);
        print "NSHamgu: BU STAR neg file size is <$bustarsz>\n";   
    }

# return new final name without suffix and tmpdir
    $debug && print "bu_subsume: output is in <$outfn>, return <$final>\n";
    return $final;      #final file name without .nsh suffix
}


sub negOp {
    my $s = $_[0];
    my $jn = $_[1];
    my $op = $_[2];
    my $ans;
    my $cmd  = "$negdbpath/negdb $op \"$s\" -f \"$jn\" -D \"$tmpfilepath\"";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "NSHamgu negOp: $op failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0 || $ans == 1) {
            ($debug || $admin) && print("NSHamgu negOp $op completed\n");
            return $ans;
        } 
        if ($ans > 1) {
            print("NSHamgu negOp $op found error [$ans]\n");
        }
    }
    $debug && print "args to negOp: op is <$op>, s is <$s>, jn is <$jn>, and ans is <$ans>\n";
    $SANITY && die "NSHamgu: args to negOp: op is <$op>, s is <$s>, jn is <$jn>, and ans is <$ans>\n";
    return $ans;
}


sub negRelOp {
    my $fn = $_[0];
    my $jn = $_[1];
    my $outfn = $_[2];
    my $op = $_[3];
    my $k = $_[4];
    my $ans;
    my $cmd  = "$negdbpath/negdb -x $op -f \"$fn\" -j \"$jn\" -o \"$outfn\" -D \"$tmpfilepath\" -T $cleanuppercent -k $k";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "NSHamgu negRelOp: $op failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0 || $ans == 1) {
            ($debug || $admin) && print("NSHamgu negRelOp $op completed\n");
            return $ans;
        } 
        if ($ans > 1) {
            print("NSHamgu negRelOp $op found error [$ans]\n");
        }
    }
    $debug && print "args to negRelOp: op is <$op>, fn is <$fn>, jn is <$jn>, outfn is <$outfn>, and ans is <$ans>\n";
    $SANITY && die "NSHamgu:args to negRelOp: op is <$op>, fn is <$fn>, jn is <$jn>, outfn is <$outfn>, and ans is <$ans>\n";
    return $ans;
}


# instead of doing separate queries for each bit and combining them,
# do them all at once with negRelOp NegI and 0's for each bit
sub select_relevant {
    my $rec = $_[0];
    my $name = $_[1];
    my $outname = $_[2];
    my $jn = "$tmpfilepath/$name.nsh";
    my $fn = "$configfilepath/$nshfilename";
    my $outfn = "$tmpfilepath/$outname.nsh";
    my $op = "I";

    &make_easy_singleton($rec,$jn);
    
    my $rtn = &negRelOp($fn,$jn,$outfn,$op,$MAX_KEY);
    if(!($rtn == 0 || $rtn == 1)) {
        die "NSHamgu select relevant failed with rtn code $rtn\n";
    }

    return $rtn;
}


sub partial_query {
    my $q = $_[0];
    my $fn = $_[1];
    my $outfn = $_[2];
    my $ans;
    my $cmd  = "$negdbpath/negdb -q \"$q\" -f \"$fn\" -o \"$outfn\" -D \"$tmpfilepath\" -k $MAX_KEY";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "NSHamgu partial_query: failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0) {
            ($debug || $admin) && print("NSHamgu partial query completed\n");
            return $ans;
        }
        if($ans > 1) {
            print("NSHamgu partial query found error [$ans]\n");
        }
    }
    $debug && print "args to partial query: q is <$q>, ans is <$ans>\n";
    $SANITY && die "NSHamgu:args to partial query: q is <$q>, ans is <$ans>\n";
    return $ans;
}


# for bu step by step
sub unnegate {
    my $fn = $_[0];
    my $outfn = $_[1];
    my $k = $_[2];
    my $ans;
    my $cmd  = "$negdbpath/negdb -u -f \"$fn\" -o \"$outfn\" -D \"$tmpfilepath\" -k $k";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "NSHamgu unnegate: failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0) {
            ($debug || $admin) && print("NSHamgu unnegate completed\n");
            return $ans;
        } 
        if ($ans > 1) {
            print("NSHamgu unnegate found error [$ans]\n");
        }
    }
    $debug && print "args to unnegate: fn is <$fn>, outfn is <$outfn>, k is <$k>, and ans is <$ans>\n";
    $SANITY && die "NSHamgu:args to unnegate: fn is <$fn>, outfn is <$outfn>, k is <$k>, and ans is <$ans>\n";
    return $ans;
}


# for bu step by step
sub compare {
    my $fn = $_[0];
    my $jn = $_[1];
    my $ans;
    my $zmode = 3;        # try mode 2 for ternary; mode 1 was default v.19.14 
    if($BTMODE==0) {
        $zmode = 2;       # use mode 2 if fully specified 
    }
    my $cmd  = "$negdbpath/negdb -Z $zmode -f \"$fn\" -j \"$jn\"";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "NSHamgu comparison: failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0 || $ans == 1) {
            ($debug || $admin) && print("NSHamgu comparison completed <$ans>\n");
            return $ans;
        }
        if($ans > 1) {
            print("NSHamgu compare found error [$ans]\n");
        }
    }
    $debug && print "args to compare: ans is <$ans>\n";
    $SANITY && die "NSHamgu:args to compare: ans is <$ans>\n";
    return $ans;
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
    my $ns = ($c x ($l - $sl)) . $s;   # pad left with $c's (e.g. zeros, *'s)
    return $ns;
}

	
sub get_NSHlength {
    my $nshlen=0;
    my $fnsh = "$configfilepath/$nshfilename";

    stat($fnsh);
    if (! -e _ ) {
        die "NSHamgu: <$fnsh> file not found\n";
    }

    open NSHFILE,"$fnsh" || die "cannot open $fnsh for reading the length";
    my $raw = <NSHFILE>;
    close NSHFILE;
    $debug && print "NSHamgu raw length of NSH is <$raw>\n";        

    if( $raw =~ /^[ab]([1-9][0-9]*)$/){
        $nshlen = $1;
    }
    return $nshlen;
}


sub dbsize_stat {
    my $fn = $_[0];
    my $len = $_[1];
    my $llen = length $len;

    $debug && print "record len is <$len>, number of bytes is <$llen>\n";        
    $debug && print "dbsize_stat: fn is <$fn>\n";

    my @statfn = stat($fn);
    if (! -e _) {
        die "NSHamgu: dbsize_stat on <$fn> failed\n";
    }
    my $lines = $statfn[7];       # file size in bytes
    $debug && print "dbsize_stat: lines stat is <$lines>\n";
    $lines = $lines - (1+1+ $llen); # subtract bytes in header
    $debug && print "dbsize_stat: lines stat is adjust to <$lines>\n";
    $lines = int(($lines + 1) /($len+1));  # don't forget newline byte
    $debug && print "dbsize_stat: lines is calculated <$lines>\n";
    return $lines;
}


sub compress_reduction{
    my $len = $_[0];
    my $fn = $_[1];
    my $outfn = $_[2];
    my $q = "";
    $q = &pad_left_length($q,"*",$len);
    &partial_query($q,$fn,$outfn);
}


# save last NSH and increment Z step for next time (if testing); 
sub bkupNSH {
    if($SAVE_THE_OLD_NSH==1){
        my $fnsh = "$configfilepath/$nshfilename";
        my $zfile = $configfilepath . "/Z";
        open ZFILE,"$zfile" || die "cannot open zfile";
        my $raw = <ZFILE>;
        close ZFILE;
        $debug && print "NSHamgu bkup NSH using raw z of <$raw>\n";
        
        my $it="";
        if( $raw =~ /^([1-9][0-9]*)$/){
            $it = $1;
            copy($fnsh, $fnsh.$it) || die "NSHamgu: copy failed $!";
            $it++;
            open ZFILE,">$zfile" || die "cannot open zfile";
            print ZFILE "$it";
            close ZFILE;
        } else {
            print "NSHamgu: problems with bkup, Z contains junk!!\n";
        }
    }
}


# save final file as new NSH;
# remove all the intermediate files
sub cleanup {
    my $final = $_[0];
    my $fnsh = "$configfilepath/$nshfilename";
    copy("$tmpfilepath/$final.nsh", $fnsh) || die "NSHamgu: copy failed $!";
    &cleanup2();
}


sub cleanup2 {
    if(! ($debug || $admin) ) {                    # after final copy
#        unlink("$tmpfilepath/clean.dat");
#        unlink("$tmpfilepath/sizes.dat");
#        unlink("$tmpfilepath/distrib7-RNDB-0.dat");
#        unlink("$tmpfilepath/*.nsh");
#        rmdir($tmpfilepath) || die "NSHamgu: can't clean up $!";;
        `rm -rf $tmpfilepath`;
    }
}


sub main
{
    my $v;
    my $vcount;
    my $t;
    my $tcount;
    my $filecount;
    my $final;

    ($v,$vcount,$t,$tcount) = &command_args(@_);  # aborts if error

    &bkupNSH();

    if($admin || $debug){
        my $fsz = &dbsize_stat("$configfilepath/$nshfilename",$LEN);
        print "NSHamgu: input file size is <$fsz>\n";
    }
        
#make temporary directory for .nsh files
    $tmpfilepath = mkdtemp("$configfilepath/nshtmpAMGU_XXXXX");
    ($tmpfilepath =~ /undef/ ) && die "NSHadd: fail to make temporary data directory\n"; 
    ($debug || $admin) && print "tmp dir is <$tmpfilepath>\n";

    if($admin || $debug) {
        $stderrors = "2>> $tmpfilepath/errlog.txt";
        $stdoutput = ">> $tmpfilepath/log.txt";
    }

# special case, v is a constant (ground), so t has no 1's
# just remove v from NSH
    if($tcount == 0) {
        my $fnsh = "$configfilepath/$nshfilename";
        my $rtn = &negOp($v,$fnsh,"-r");
        if(!($rtn != 0 || $rtn != 1)) {
            print "NSHAMGU remove_v <$v> from <$fnsh> failed with rtn code $rtn\n";
        }
        &cleanup2();
        return $rtn;
    }

# OKAY TO BEGIN
    &select_relevant($v,"v","v1"); # produces ndb files starting with v1.nsh
    
    if($admin || $debug){
        my $vsz = &dbsize_stat("$tmpfilepath/v1.nsh",$LEN);
        print "NSHamgu: v1.nsh file size is <$vsz>\n";
    }

    my $newnamet = "t1-$tcount";
    &select_relevant($t,"t",$newnamet);      # produces one ndb file named t1-$tcount.nsh

    $debug && print "NSHamgu: new name for t's is <$newnamet>\n";
    if($admin || $debug){
        my $tsz = &dbsize_stat("$tmpfilepath/$newnamet.nsh",$LEN);
        print "NSHamgu: <$newnamet> file size is <$tsz>\n";
    }

    if($BU_STEP_BY_STEP) {
        $final = &bu_subsume_step_by_step("v",$newnamet);
    } else {
        $final = &bu_subsume("v",$newnamet);
    }

# CREATE COMPLEMENT string of v and t: it has 1's where v and t have 0's
    my $compname = &create_complement2($v,$vcount,$t,$tcount);

# at this point, we have the complement and the bU* of v and t
# so combine them (vt and nvt) with negUnion
    $debug && print "compname is <$compname>\n";
    if($compname !~ /^0$/) {
        $final = &combine_two($final,$compname);      # new nsh to-be created
    } else {
        my $finalcompress = "compress-$final";
        &compress_reduction($LEN,"$tmpfilepath/$final.nsh","$tmpfilepath/$finalcompress.nsh");
        $final = $finalcompress;
    }

    $debug &&  print "final file was <$final>\n";
    if($admin || $debug){
        my $fsz = &dbsize_stat("$tmpfilepath/$final.nsh",$LEN);
        print "NSHamgu: final result file size is <$fsz>\n";
    }

    &cleanup($final);
    return 0;
}

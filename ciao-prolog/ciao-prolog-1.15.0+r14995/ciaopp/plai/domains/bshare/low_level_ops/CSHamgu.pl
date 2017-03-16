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
# Filename      : CSHamgu.pl
# Language      : Perl 
# Purpose       : a negative implementation of the amgu function
# 
# Creator       : E.S. Ackley
# Creation Date : Sat Apr 14 15:08:30 2007 
# Updated       : Thu Apr 24 07:01:43 2008 
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

my $version   = "v.25.9";
my $negdbpath = "nice -19 /home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/low_level_ops/c_code";     
my $configfilepath =  "/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tmp";                 
my $tmpfilepath;                # set by script
my $cshfilename    = "NSH0";    # default negative database

my $LEN=0;
my $MAXL=1000;
my $MAX_KEY=1;             # determined by BTMODE
my $cleanuppercent=0;      # 0 for no post cleanups; ow. percent of ndbsize
my $debug=0;
my $admin=0;

my $stderrors = "2> /dev/null";
my $stdoutput = "> /dev/null";
my $SAVE_THE_OLD_CSH = 0;    # 1= saves CSH at each step (per file Z); 0 doesn't

my $BTMODE=1;               # 0 for binary (no *'s); 1 for ternary
my $SANITY=1;               # 1 will die when insanity happens; 0 will be silent

exit(&main(@ARGV));

sub usage_abort 
{
    die "Usage: % ./CSHamgu.pl len v t [cshfilename] | v t mode \n";
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

# alternate CSH file name, assumes last of four args
    if($argc == 4) {
        if( $args[3] =~ /^[0-9a-zA-Z\-\/\\\._]{1,255}$/ ) {
            $cshfilename = $args[3];
        } else {
            die "CSHamgu: CSH filename must be alphanumeric and less than 255 letters long>\n";
        }
    }

    $LEN = &get_CSHlength();
    if ($LEN > $MAXL || $LEN == 0 ) {
        die "CSHamgu: CSH Record length is INVALID, can't be greater than $MAXL or less than 1\n";
    }
    
     if ($argc > 3) {
        my $cshlen = 0;
        # VALIDATE first arg, LENGTH
        if( $args[0] =~ /[1-9][0-9]*/ ) {
            $cshlen = $args[0];
            if ($cshlen > $MAXL) {
                die "CSHamgu: Record length (first arg) is INVALID, can't be greater than $MAXL\n";
            }
        } else {
            die "CSHamgu: Record length (first arg) must be greater than 0\n";
        }
        if ($cshlen != $LEN) {
            die "CSHamgu: Record length (first arg) must match CSH length ($cshlen)\n";
        }
        $varg = $args[1];
        $targ = $args[2];
    } else {             
        # allow for first length arg to be optional	   
        $varg = $args[0];
        $targ = $args[1];
        # VALIDATE third argument (MODE)
	if ($args[2] == 0 || $args[2] == 1) {
	    $BTMODE = $args[2];  
	    if ($BTMODE == 0){
		$debug && print "CSHamgu using bSH\n";
	    }
	    else{
		$debug && print "CSHamgu using tSH\n";
            }    			    
        }
	else{
            die "CSH mode can be only 0 (bSH) or 1 (tSH) \n";
        }
    }

    # VALIDATE AND PREPARE v arg: string of 0's and a single 1
    if($varg =~ /^([0]*1[0]*)$/ ){
        $v = $varg;
        $vcount = ($v =~ tr/1//); 
        if ($vcount != 1) {
            die "CSHamgu: error, v has $vcount 1's\n";
        }
        $v =~ s/0/\*/g;
        my $vlen = length $v;
        if ($vlen < $LEN) {
            $v = &pad_left_length($v,'*',$LEN);
        } elsif($vlen > $LEN) {
            die "CSHamgu: v argument cannot have more than $LEN digits\n";
        }
    } else {
        die "CSHamgu: v argument must have only one 1 and the rest 0's\n";
    }
    

    # VALIDATE AND PREPARE t arg: string of 0's and 1's
    if($targ =~ /^([01]+)$/ ){
        $t = $targ;
        $tcount = ($t =~ tr/1//); 
        if ($tcount < 0 || $tcount >= $LEN) {
            die "CSHamgu: error, t has $tcount 1's\n";
        }
        $t =~ s/0/\*/g;
        my $tlen = length $t;
        if ($tlen < $LEN) {
            $t = &pad_left_length($t,'*',$LEN);
        } elsif ($tlen > $LEN) {
            die "CSHamgu: t argument must have only $LEN 1's and 0's\n";
        }
    } else {
        die "CSHamgu: t argument must have $LEN 1's and 0's\n";
    }

    if($BTMODE==0) {
        $MAX_KEY = $LEN;           # no *'s
    } else {
        # $MAX_KEY=int($LEN/2);        
    }
    $debug && print "cshamgu: max key is <$MAX_KEY>\n";
    return ($v,$vcount,$t,$tcount);
}


# negUnion two files, name1 and name2 
sub combine_two {
    my $name1 = $_[0];
    my $name2 = $_[1];

    $debug && print "combine_two input is <$name1> and <$name2>\n";

    my $fn = "$tmpfilepath/$name1.csh";
    my $jn = "$tmpfilepath/$name2.csh";
    my $outfn = "$tmpfilepath/$name1$name2".".csh";

    my $rtn = &negRelOp($fn,$jn,$outfn,"I",$MAX_KEY);
    if(!($rtn == 0 || $rtn == 1)) {
        die "CSHamgu combine_two failed <$fn> <$jn> with rtn code $rtn\n";
    }

    my $l = length $outfn;
    $outfn = substr($outfn,0,$l - 4);   # remove the .csh suffix
    $outfn = substr($outfn,rindex($outfn,"/")+1); # remove the tmpdir and /
    $debug && print "combine_two: output is in <$outfn>\n";
    return $outfn;      #final file name without .csh suffix
}


# creates a single negative database that represents the complement;
# returns filename if successful.
sub create_complement {
    my $notvt = $_[0];
    my $nvtcount = ($notvt =~ tr/1//); 

    if($nvtcount==0) {
        $admin && print "CSHamgu: complement file size is 0\n";
        return "0";
    }

# make one ndb for notvt from CSH using and's;
# remove vt bits as individual neg records;
    my $nvtname = "nvt1-$nvtcount";
    my $nvtnamefull = "$tmpfilepath/$nvtname.csh";

    $debug && print "create_complement:  <$nvtname>\n";
    
    &select_relevant($notvt,"nvt",$nvtname);

    if ($debug) {
        print "CAT COMPLEMENT (before remove vt):\n";
        my $log = `cat $nvtnamefull`;
        print "<$log>";
    }
 
    if(&remove_vt($notvt,$nvtnamefull) != 0) {   # improve precision
        die "CSHamgu: create_complement failed_remove_vt from $nvtnamefull\n";
    }
    
    if ($debug) {
        print "CAT COMPLEMENT:\n";
        my $log = `cat $nvtnamefull`;
        print "<$log>";
    }
    
    if($admin || $debug) {
        my $csz = &dbsize_stat($nvtnamefull,$LEN);
        print "CSHamgu: complement file size is <$csz>\n";
    }
    
    return $nvtname;
}


# creates a single negative database that represents the complement;
# negU CSH and a singleton with 0's in irrel positions (*'s everywhere else) 
# returns filename if successful.
sub create_complement2 {
    my $nvt = $_[0];

    my $nvtcount = ($nvt =~ tr/1//); 

    if($nvtcount==0) {
        $admin && print "CSHamgu: complement file size is 0\n";
        return "0";
    }


# negU nvt bits as one combined neg records;
    my $nvtname = "nvt1-$nvtcount";
    my $outfn = "$tmpfilepath/$nvtname.csh";
    my $fn = "$configfilepath/$cshfilename";
    my $jn = "$tmpfilepath/nvt.csh";

    $debug && print "create_complement2:  <$nvtname>\n";

    &make_easy_singleton_patterns($nvt,$jn);  
   
# get the "intersect" (negUnion) of the bit patterns in nvt.csh and CSH
# nvt.csh contains all possible combos with the irrel positions = 1.

    my $rtn = &negRelOp($fn,$jn,$outfn,"U",$MAX_KEY);
    if(!($rtn == 0 || $rtn == 1)) {
        die "CSHamgu create complement 2 failed with rtn code $rtn\n";
    }

    $jn = "$tmpfilepath/nvt0.csh";
    $fn = "$tmpfilepath/$nvtname.csh.cpy";
    copy($outfn,$fn);       # so we can see it.

    &make_easy_singleton2($nvt,$jn);  # one rec with 0's in the rel positions
   
# get the "intersection" (negUnion) of the bit patterns in nvt0.csh and CSH
# nvt0.csh has all combos of the irrel's with the rel positions 0.
# result is the complement

    $rtn = &negRelOp($fn,$jn,$outfn,"U",$MAX_KEY);
    if(!($rtn == 0 || $rtn == 1)) {
        die "CSHamgu create complement 2 second negU failed with rtn code $rtn\n";
    }

    if ($debug) {
        print "CAT COMPLEMENT:\n";
        my $log = `cat $outfn`;
        print "<$log>";
    }
    
    if($admin || $debug) {
        my $csz = &dbsize_stat($outfn,$LEN);
        print "CSHamgu: complement file size is <$csz>\n";
    }
    
    return $nvtname;
}


# creates a single negative database that represents the complement;
# negU CSH and a singleton with 0's in irrel positions (*'s everywhere else) 
# returns filename if successful.
sub create_complement3 {
    my $vfn = $_[0];
    my $vcount = $_[1];
    my $tfn = $_[2];
    my $tcount = $_[3];


    my $nvtcount = $LEN - $vcount - $tcount;

    if($nvtcount==0) {
        $admin && print "CSHamgu: complement file size is 0\n";
        return "0";
    }


# union (using negIntersect) the two relevant set.
    my $jn = "$tmpfilepath/relvt.csh";
    my $rtn = &negRelOp($vfn,$tfn,$jn,"I",$MAX_KEY);
    if(!($rtn == 0 || $rtn == 1)) {
        die "CSHamgu create complement 3 failed unioning the relevant sets with rtn code $rtn\n";
    }


# get the Ternary Set Diff between CSH and the relevant ones

    my $nvtname = "nvt1-$nvtcount";
    my $outfn = "$tmpfilepath/$nvtname.csh";
    my $fn = "$configfilepath/$cshfilename";

    $debug && print "create_complement3:  <$nvtname>\n";

    $rtn = &negRelOp($fn,$jn,$outfn,"O",$MAX_KEY);
    if(!($rtn == 0 || $rtn == 1)) {
        die "CSHamgu create complement 3 failed Set Diff with rtn code $rtn\n";
    }

    if ($debug) {
        print "CAT COMPLEMENT:\n";
        my $log = `cat $outfn`;
        print "<$log>";
    }
    
    if($admin || $debug) {
        my $csz = &dbsize_stat($outfn,$LEN);
        print "CSHamgu: complement file size is <$csz>\n";
    }
    
    return $nvtname;
}


# remove each bit pattern in vt (*'s in the complement) from each
# partial nvt, before combining with vt result
sub remove_vt {
    my $nvt = $_[0];
    my $fn = $_[1];               # full path name
    my $j = 1;
    my $op = "-a";
    my $idx = rindex($nvt,"*");

    while($j <= $LEN && $idx >= 0){
        my $rtn;
        my $q = "*";
        $q = &pad_left_length($q,"*",$LEN);
        substr($q,$idx,1,"1");
        $debug && print "remove_vt: q is <$q>\n";
        $rtn = &negOp($q,$fn,$op);
        if(!($rtn != 0 || $rtn != 1)) {
            print "CSHamgu remove_vt <$q> from <$fn> failed with rtn code $rtn\n";
            return $rtn;
        }
        if($idx==0) { last; }
        $idx = rindex($nvt,"*",$idx-1);
        $j++;
    }
    return 0;
}


sub bu_subsume {
    my $name1 = $_[0];
    my $name2 = $_[1];
    my $op = "B";
    my $k;

    if($BTMODE==0) {
        $k = $MAX_KEY;                   # same as $LEN, no *'s
    } else {
        $k = 1;
    }

#combine the one $name1 (fn) with the one newname2 (jn) into outfn
    my $fn = "$tmpfilepath/$name1" . "1.csh";
    my $jn = "$tmpfilepath/$name2.csh";
    my $final = $name1 . "1BU" . $name2;   # old way
    my $outfn = "$tmpfilepath/$final.csh";
    my $rtn = &negRelOp($fn,$jn,$outfn,$op,$k);
    if(!($rtn == 0 || $rtn == 1)) {
        die "CSHamgu bu_subsume failed with rtn code $rtn\n";
    }

    if ($debug) {
        print "CAT BU of vt before STAR:\n";
        my $log = `cat $outfn`;
        print "<$log>";
    }
    
    if($admin || $debug) {    
        my $busz = &dbsize_stat($outfn,$LEN);
        print "CSHamgu: BU file size is <$busz>\n";
    }

# bu star - closure repeat until no change remains
# test for same number of records (rtn == 1)
    my $eqv = 0;
    my $cnt = 1;
    while($eqv==0) {
        my $cpyfn = "$outfn.cpy";
        copy($outfn,$cpyfn);
        $debug && print "CSHamgu: star closure count <$cnt>\n";
        $eqv = &negRelOp($outfn,$cpyfn,$outfn,$op,0);   # no compression use k=0 
    }

    if ($debug) {
        print "CAT BU STAR of vt:\n";
        my $log = `cat $outfn`;
        print "<$log>";
    }

    if($admin || $debug) {    
        my $busz = &dbsize_stat($outfn,$LEN);
        print "CSHamgu: BU STAR file size is <$busz>\n";   
    }

# return new final name without suffix and tmpdir
    $debug && print "bu_subsume: output is in <$outfn>, return <$final>\n";
    return $final;      #final file name without .csh suffix
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
        print "CSHamgu negOp: $op failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0 || $ans == 1) {
            ($debug || $admin) && print("CSHamgu negOp $op completed\n");
            return $ans;
        } 
        if ($ans > 1) {
            print("CSHamgu negOp $op found error [$ans]\n");
        }
    }
    $debug && print "args to negOp: op is <$op>, s is <$s>, jn is <$jn>, and ans is <$ans>\n";
    $SANITY && die "CSHamgu FAILURE: args to negOp: op is <$op>, s is <$s>, jn is <$jn>, and ans is <$ans>\n";
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
        print "CSHamgu negRelOp: $op failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0 || $ans == 1) {
            ($debug || $admin) && print("CSHamgu negRelOp $op completed\n");
            return $ans;
        } 
        if ($ans > 1) {
            print("CSHamgu negRelOp $op found error [$ans]\n");
        }
    }
    $debug && print "args to negRelOp: op is <$op>, fn is <$fn>, jn is <$jn>, outfn is <$outfn>, and ans is <$ans>\n";
    $SANITY && die "CSHamgu FAILURE: args to negRelOp: op is <$op>, fn is <$fn>, jn is <$jn>, outfn is <$outfn>, and ans is <$ans>\n";
    return $ans;
}


# no longer used for bu*.
sub compare {
    my $fn = $_[0];
    my $jn = $_[1];
    my $ans;
    my $zmode = 3;        # try mode 2 for ternary; mode 1 was default v.25.8 
    if($BTMODE==0) {
        $zmode = 2;       # use mode 2 if fully specified 
    }
    my $cmd  = "$negdbpath/negdb -Z $zmode -f \"$fn\" -j \"$jn\"";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "CSHamgu comparison: failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0 || $ans == 1) {
            ($debug || $admin) && print("CSHamgu comparison completed\n");
            return $ans;
        }
        if($ans > 1) {
            print("CSHamgu compare found error [$ans]\n");
        }
    }
    $debug && print "args to compare: ans is <$ans>\n";
    $SANITY && die "CSHamgu FAILURE: args to compare: <$fn> <$jn>, ans is <$ans>\n";
    return $ans;
}


# one rec with 0's instead of *'s; used for binary mode
sub make_easy_singleton {
    my $rec = $_[0];
    my $fname = $_[1];
    my $len = length $rec;

    $rec =~ s/\*/0/g;

    open SINGLE,">$fname" || die "cannot create $fname for making an easy singleton";

    print SINGLE "b$len\n";
    print SINGLE "$rec\n";
    close SINGLE;
}


# one rec with 0's instead of *'s, and *'s instead of 1's; use for irrel
sub make_easy_singleton2 {
    my $rec = $_[0];
    my $fname = $_[1];
    my $len = length $rec;

    $rec =~ s/\*/0/g;
    $rec =~ s/1/\*/g;

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


# instead of doing separate queries for each bit and combining them,
# do them all at once with negRelOp Relevance (requires v.71)
sub select_relevant {
    my $rec = $_[0];
    my $name = $_[1];
    my $outname = $_[2];
    my $jn = "$tmpfilepath/$name.csh";
    my $fn = "$configfilepath/$cshfilename";
    my $outfn = "$tmpfilepath/$outname.csh";
    my $op = "";

    if($BTMODE == 1) {
        &make_easy_singleton_patterns($rec,$jn);   
        $op = "U";
    } else {
        &make_easy_singleton($rec,$jn);      # BINARY MODE
        $op = "R";
    }

    my $rtn = &negRelOp($fn,$jn,$outfn,$op,$MAX_KEY);
    if(!($rtn == 0 || $rtn == 1)) {
        die "CSHamgu select relevant failed with rtn code $rtn\n";
    }
    return $rtn;
}


sub partial_query {
    my $q = $_[0];
    my $fn = $_[1];
    my $outfn = $_[2];
    my $ans;
    my $cmd  = "$negdbpath/negdb -C \"$q\" -f \"$fn\" -o \"$outfn\" -D \"$tmpfilepath\" -k $MAX_KEY";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "CSHamgu partial_query: failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0) {
            ($debug || $admin) && print("CSHamgu partial query completed\n");
            return $ans;
        }
        if($ans > 1) {
            print("CSHamgu partial query found error [$ans]\n");
        }
    }
    $debug && print "args to partial query: q is <$q>, ans is <$ans>\n";
    $SANITY && die "CSHamgu FAILURE: args to partial query: q is <$q>, ans is <$ans>\n";
    return $ans;
}


sub pad_left_length {
    my $s = $_[0];
    my $c = $_[1];
    my $l = $_[2];
    my $sl = length $s;
    my $ns = ($c x ($l - $sl)) . $s;   # pad left with $c's (e.g. zeros, *'s)
    return $ns;
}


# return a string that has 1's in the positions not set in v or t
sub build_complement_vt {
    my $v = $_[0];
    my $t = $_[1];
    my $notvt = $t;
    $debug && print "build_complement_vt: v is <$v>. t is <$t>\n";
    my $vidx = index($v,"1");
    substr($notvt,$vidx,1,"1");    
    $debug && print "build_complement_vt: vt is <$notvt>, vidx is <$vidx>\n";
    $notvt =~ s/\*/t/g;     # change all the *'s to t's temporarily
    $notvt =~ s/1/\*/g;     # change all the 1's to *'s
    $notvt =~ s/t/1/g;      # change all the t's to 1's
    $debug && print "build_complement_vt: nvt is <$notvt>\n";
    return $notvt;
}

	
sub get_CSHlength {
    my $cshlen=0;
    my $fcsh = "$configfilepath/$cshfilename";

    stat($fcsh);
    if (! -e _ ) {
        die "CSHamgu: <$fcsh> file not found\n";
    }

    open CSHFILE,"$fcsh" || die "cannot open $fcsh for reading the length";
    my $raw = <CSHFILE>;
    close CSHFILE;
    $debug && print "CSHamgu raw length of CSH is <$raw>\n";        

    if( $raw =~ /^[ab]([1-9][0-9]*)$/){
        $cshlen = $1;
    }
    return $cshlen;
}


sub dbsize_stat {
    my $fn = $_[0];         # full path input
    my $len = $_[1];
    my $llen = length $len;

    $debug && print "record len is <$len>, number of bytes is <$llen>\n";        
    $debug && print "dbsize_stat: fn is <$fn>\n";

    my @statfn = stat($fn);
    if (! -e _) {
        die "CSHamgu: dbsize_stat on <$fn> failed\n";
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



# save last CSH and increment Z step for next time (if testing); 
sub bkupCSH {
    if($SAVE_THE_OLD_CSH==1){
        my $fcsh = "$configfilepath/$cshfilename";
        my $zfile = $configfilepath . "/Z";
        open ZFILE,"$zfile" || die "cannot open zfile";
        my $raw = <ZFILE>;
        close ZFILE;
        $debug && print "CSHamgu bkup CSH using raw z of <$raw>\n";
        
        my $it="";
        if( $raw =~ /^([1-9][0-9]*)$/){
            $it = $1;
            copy($fcsh, $fcsh.$it) || die "CSHamgu: copy failed $!";
            $it++;
            open ZFILE,">$zfile" || die "cannot open zfile";
            print ZFILE "$it";
            close ZFILE;
        } else {
            print "CSHamgu: problems with bkup, Z contains junk!!\n";
        }
    }
}


# save final file as new CSH;
# remove all the intermediate files
sub cleanup {
    my $final = $_[0];
    my $fcsh = "$configfilepath/$cshfilename";
    copy("$tmpfilepath/$final.csh", $fcsh) || die "CSHamgu: copy failed $!";
    &cleanup2();
}


sub cleanup2 {
    if(! ($debug || $admin) ) {                    # after final copy
#        unlink("$tmpfilepath/clean.dat");
#        unlink("$tmpfilepath/sizes.dat");
#        unlink("$tmpfilepath/distrib7-RNDB-0.dat");
#        unlink("$tmpfilepath/*.csh");
#        rmdir($tmpfilepath) || die "CSHamgu: can't clean up $!";;
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

    &bkupCSH();

    if($admin || $debug){
        my $fsz = &dbsize_stat("$configfilepath/$cshfilename",$LEN);
        print "CSHamgu: input file size is <$fsz>\n";
    }
        
#make temporary directory for .csh files
    $tmpfilepath = mkdtemp("$configfilepath/cshtmpAMGU_XXXXX");
    ($tmpfilepath =~ /undef/ ) && die "CSHamgu: fail to make temporary data directory\n"; 
    ($debug || $admin) && print "tmp dir is <$tmpfilepath>\n";

    if($admin || $debug) {
        $stderrors = "2>> $tmpfilepath/errlog.txt";
        $stdoutput = ">> $tmpfilepath/log.txt";
    }

# special case, v is a constant (ground), so t has no 1's
# just add v to CSH
    if($tcount == 0) {
        my $fcsh = "$configfilepath/$cshfilename";
        my $rtn = &negOp($v,$fcsh,"-a");
        if(!($rtn != 0 || $rtn != 1)) {
            print "CSHamgu add_v <$v> to <$fcsh> failed with rtn code $rtn\n";
        }
        &cleanup2();
        return $rtn;
    }


# OKAY TO BEGIN
    &select_relevant($v,"v","v1"); # produces ndb files starting with v1.csh
    
    if($admin || $debug){
        my $vsz = &dbsize_stat("$tmpfilepath/v1.csh",$LEN);
        print "CSHamgu: v1.csh file size is <$vsz>\n";
    }

    &select_relevant($t,"t","t1-$tcount");      # produces one ndb file named t1-$tcount.csh
    
    my $newnamet = "$tmpfilepath/t1-$tcount.csh";

    $debug && print "CSHamgu: new name for t's is <$newnamet>\n";
    if($admin || $debug){
        my $tsz = &dbsize_stat($newnamet,$LEN);
        print "CSHamgu: t1-$tcount file size is <$tsz>\n";
    }

    $final = &bu_subsume("v","t1-$tcount");

# CREATE COMPLEMENT string of v and t: it has 1's where v and t have 0's
# three ways, 1 and 2 work well; use 3 for binary mode.
    my $compname;
#    $compname = &create_complement(&build_complement_vt($v,$t));
    if($BTMODE==1) {
        $compname = &create_complement2(&build_complement_vt($v,$t));
    } else {         # BINARY MODE
        $compname = &create_complement3("$tmpfilepath/v1.csh",1,$newnamet,$tcount);
    }
# at this point, we have the complement and the bU* of v and t
# so combine them (vt and nvt) with negUnion
    $debug && print "compname is <$compname>\n";
    if($compname !~ /^0$/) {
        $final = &combine_two($final,$compname);      # new CSH to-be created
    } elsif ($BTMODE == 1) {
        my $finalcompress = "compress-$final";
        &compress_reduction($LEN,"$tmpfilepath/$final.csh","$tmpfilepath/$finalcompress.csh");
        $final = $finalcompress;
    }

    $debug &&  print "final file was <$final>\n";
    if($admin || $debug){
        my $fsz = &dbsize_stat("$tmpfilepath/$final.csh",$LEN);
        print "CSHamgu: final file size is <$fsz>\n";
    }

    &cleanup($final);
    return 0;
}

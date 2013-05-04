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
# Filename      : NSHreduceone.pl
# Language      : Perl 
# Purpose       : reduce the length of the negative database record by one
# 
# Creator       : E.S. Ackley
# Creation Date : Tue May  1 15:04:51 2007
# Updated       : Mon Mar 24 12:12:08 2008 
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
my $configfilepath    = "/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tmp";                 
my $tmpfilepath;      # set by script
my $negdbpath = "nice -19 /home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/low_level_ops/c_code";     
my $nshfilename       = "NSH0";    #default negative database
my $resultnshfilename = "NSH0";

my $LEN=0;
my $MAXL=1000;
my $MAX_KEY=1;
my $cleanuppercent=0;      # 0 for no post cleanups; ow. percent of ndbsize
my $debug=0;
my $admin=0;

my $stderrors = "2> /dev/null";
my $stdoutput = "> /dev/null";

my $SANITY=1;               # 1 will die when insanity happens; 0 will be silent

exit(&main(@ARGV));

sub usage_abort 
{
    die "Usage: % ./NSHreduceone.pl len x [nshfilename [resultnshfilename]] | x, where x is a string that specifies the single bit position to be eliminated; nshfilename and resultnshfilename default to NSH\n";
}


sub command_args
{
    my $argc = @_;   #number of args
    my @args = @_;
    my $xarg;
    my $x;
    my $xcount;

    if($argc < 1 || $argc > 4) {
        &usage_abort();
    }

    if($argc > 0){
# print "args 0 is <$args[0]>\n";
        if ($args[0] =~ /^[Z\?|help]/ ) {    # unquoted ? looks like a capital z
            &usage_abort();
        }
    }

# alternate NSH file name, assumes last of four args
    if($argc >= 3) {
        if( $args[2] =~ /^[0-9a-zA-Z\-\/\\\._]{1,255}$/ ) {
            $nshfilename = $args[2];
        } else {
            die "NSHreduceone: nsh filename must be alphanumeric and less than 255 letters long>\n";
        }
        if($argc > 3) {
            if( $args[3] =~ /^[0-9a-zA-Z\-\/\\\._]{1,255}$/ ) {
                $resultnshfilename = $args[3];
            } else {
                die "NSHreduceone: nsh result filename must be alphanumeric and less than 255 letters long>\n";
            }
        } else {
            $resultnshfilename = $nshfilename;
        }
    }

    $LEN = &get_NSHlength();
    if ($LEN > $MAXL || $LEN == 0 ) {
        die "NSHreduceone: NSH Record length is INVALID, can't be greater than $MAXL or less than 1\n";
    }

    if($argc >= 2) {
        my $nshlen = 0;
# VALIDATE first arg, LENGTH
        if( $args[0] =~ /[1-9][0-9]*/ ) {
            $nshlen = $args[0];
            if ($nshlen > $MAXL) {
                die "NSHreduceone: Record length (first arg) is INVALID, can't be greater than $MAXL\n";
            }
        } elsif ($nshlen < 2) {
            die "NSHreduceone: Record length (first arg) must be greater than 1\n";
        }
        if ($nshlen != $LEN) {
            die "NSHreduceone: Record length (first arg) must match NSH length ($LEN)\n";
        }
        $xarg = $args[1];
    } else {
        $xarg = $args[0];    # allow for first length arg to be optional
    }

# VALIDATE AND PREPARE x arg
    if($xarg =~ /^([01]+)$/ ){
        $x = $xarg;
        $xcount = ($x =~ tr/1//); 
        if ($xcount != 1) {
            die "NSHreduceone: error, x has $xcount 1's, only one allowed at this time\n";
        }
        $x =~ s/0/\*/g;
        my $xlen = length $x;
        if ($xlen < $LEN) {
            $x = &pad_left_length($x,'*',$LEN);
        }
    } else {
        die "NSHreduceone: x argument <$xarg> must have $LEN 1's and 0's\n";
    }
    return ($x,$xcount);
}


sub combine_both {
    my $name1 = $_[0];
    my $count1 = $_[1];
    my $outfn = "$tmpfilepath/$name1$count1.nsh";
    my $fn = "$tmpfilepath/$name1$count1-1.nsh";
    my $jn = "$tmpfilepath/$name1$count1-0.nsh";
    my $rtn = &negRelOp($fn,$jn,$outfn,"U");
    if(!($rtn == 0 || $rtn == 1)) {
        print "NSHreduceone combine both <$name1> <$count1> failed with rtn code $rtn\n";
        return $rtn;
    }
    return 0;
}


sub negRelOp {
    my $fn = $_[0];
    my $jn = $_[1];
    my $outfn = $_[2];
    my $op = $_[3];
    my $ans;
    my $cmd = "$negdbpath/negdb -x $op -f \"$fn\" -j \"$jn\" -o \"$outfn\" -D \"$tmpfilepath\" -T $cleanuppercent -k $MAX_KEY";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "NSHreduceone negRelOp: $op failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0 || $ans == 1) {
            ($debug || $admin) && print("NSHreduceone negRelOp $op completed\n");
            return $ans;
        } 
        if ($ans > 1) {
            print("NSHreduceone negRelOp $op found error [$ans]\n");
        }
    }
    $debug && print "args to negRelOp: op is <$op>, fn is <$fn>, jn is <$jn>, outfn is <$outfn>, and ans is <$ans>\n";
    $SANITY && die "NSHreduceone: args to negRelOp: op is <$op>, fn is <$fn>, jn is <$jn>, outfn is <$outfn>, and ans is <$ans>\n";
    return $ans;
}


sub select_eq_both {
    my $rec = $_[0];
    my $name = $_[1];
    my $fn = "$configfilepath/$nshfilename";
    my $l = length $rec;
    my $rtn = 0;
    my $ans;
    my $count = 1;
    my $idx = rindex($rec,"1");

    while ($count <= $LEN && $idx != -1) {
        my $outfn = "$tmpfilepath/$name$count-1.nsh";
        my $q = "*";
        $q = &pad_left_length($q,"*",$LEN);
        my $rtn;

        substr($q,$idx,1,"1");
        $debug && print "partial query with <$q> into <$outfn>\n";
        $rtn = &partial_query($q,$fn,$outfn); 
        if ( $rtn != 0) {
            last;
        }

        $outfn = "$tmpfilepath/$name$count-0.nsh";
        $q =~ s/1/0/g;
        $debug && print "partial query with <$q> into <$outfn>\n";
        $rtn = &partial_query($q,$fn,$outfn); 
        if ( $rtn != 0) {
            last;
        }

        $count++;
        if($idx==0) { last; }
        $idx = rindex($rec,"1",$idx-1);
    }
    return $count-1;
}


# with project option
sub partial_query {
    my $q = $_[0];
    my $fn = $_[1];
    my $outfn = $_[2];
    my $ans;
    my $cmd = "$negdbpath/negdb -q \"$q\" -f \"$fn\" -o \"$outfn\" -P -D \"$tmpfilepath\" -m $MAX_KEY";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "NSHreduceone partial_query: failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0 || $ans == 1) {
            ($debug || $admin) && print("NSHreduceone partial query completed\n");
            return $ans;
        }
        if($ans > 1) {
            print("NSHreduceone partial query found error [$ans]\n");
        }
    }
    $debug && print "args to partial query: q is <$q>, ans is <$ans>\n";
    $SANITY && die "NSHreduceone: args to partial query: q is <$q>, ans is <$ans>\n";
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


sub get_NSHlength {
    my $nshlen=0;
    my $fnsh = "$configfilepath/$nshfilename";

    stat($fnsh);
    if (! -e _ ) {
        die "NSHreduceone: <$fnsh> file not found\n";
    }

    open NSHFILE,"$fnsh" || die "cannot open $fnsh for reading the length";
    my $raw = <NSHFILE>;
    close NSHFILE;
    $debug && print "NSHreduceone raw length of NSH is <$raw>\n";        

    if( $raw =~ /^b([1-9][0-9]*)$/){
        $nshlen = $1;
    }
    return $nshlen;
}


# save final file as new NSH;
# remove all the intermediate files
sub cleanup {
    my $final = $_[0];
    my $fnsh = "$configfilepath/$resultnshfilename";
    copy("$tmpfilepath/$final", $fnsh) || die "NSHreduceone: copy failed $!";

    if(!$debug) {                    # after final copy
#        unlink("$tmpfilepath/clean.dat");
#        unlink("$tmpfilepath/sizes.dat");
#        unlink("$tmpfilepath/distrib7-RNDB-0.dat");
#        unlink("$tmpfilepath/*.nsh");
#        rmdir($tmpfilepath) || die "NSHreduceone: can't clean up $!";
        `rm -rf $tmpfilepath`;
    }
}


sub main{
    my $x;
    my $xcount;

    ($x, $xcount) = &command_args(@_);  # aborts if error

# make tmp dir for temporary ndb files
        $tmpfilepath = mkdtemp("$configfilepath/nshtmpREDUCEONE_XXXXX");
        ($tmpfilepath =~ /undef/ ) && die "NSHreduceone: fail to make temporary data directory\n"; 
        $debug && print "tmp dir is <$tmpfilepath>\n";

# OKAY TO CONTINUE
    my $filecount;
    my $final;

    $filecount = &select_eq_both($x,"x"); # produces ndb file starting with x1-0.nsh and x1-1
    if ($filecount != $xcount) {
        die("NSHreduceone: problem with select on x, $filecount files, not expected $xcount\n");
    }
    
    &combine_both("x",$xcount);         # new nsh to-be (called final) created
    $final = "x1.nsh";

    $debug &&  print "final file was <$final>\n";
    &cleanup($final);
    return 0;
}

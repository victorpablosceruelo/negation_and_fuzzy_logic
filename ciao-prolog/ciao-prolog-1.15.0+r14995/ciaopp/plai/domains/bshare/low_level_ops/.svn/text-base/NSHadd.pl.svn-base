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
# Filename      : NSHadd.pl
# Language      : Perl 
# Purpose       : Increase the length of negative database records
#                 with zero values at the beginning or end
# Creator       : E.S. Ackley
# Creation Date : Mon Apr 16 07:31:14 2007
# Updated       : Mon Mar 24 12:07:56 2008 
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
my $negdbpath = "nice -19 /home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/low_level_ops/c_code";     
my $crackpath = "/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/low_level_ops";
my $tmpfilepath;                # set by script
my $nshfilename    = "NSH0";    #default negative database

my $LEN=0;
my $MAXL=1000;
my $MAX_KEY=1;
my $PADLEFT=0;
my $cleanuppercent=0;      # 0 for no post cleanups; ow. percent of ndbsize
my $debug=0;
my $admin=0;

my $stderrors = "2> /dev/null";
my $stdoutput = "> /dev/null";
my $SAVE_THE_OLD_NSH = 0;    # 1= saves NSH at each step (per file Z); 0 doesn't

my $SANITY=1;               # 1 will die when insanity happens; 0 will be silent

exit(&main(@ARGV));

sub usage_abort 
{
    die "Usage: % ./NSHadd.pl len n [nshfilename] | [n], where n is the number of columns of zero to add (negative n prepends to the beginning), default is 1 appended at the end\n";
}


sub command_args
{
    my $argc = @_;   #number of args
    my @args = @_;
    my $n = 0;

    if($argc > 3) {
        &usage_abort();
    }
    
    if($argc > 0){
# print "args 0 is <$args[0]>\n";
        if ($args[0] =~ /^[Z?|help]/ ) {    # unquoted ? looks like a capital z
            &usage_abort();
        }
    }

# alternate NSH file name, assumes last of three args
    if($argc == 3) {
        if( $args[2] =~ /^[0-9a-zA-Z\-\/\\\._]{1,255}$/ ) {
            $nshfilename = $args[2];
        } else {
            die "NSHadd.pl: nsh filename must be alphanumeric and less than 255 letters long>\n";
        }
    }

    $LEN = &get_NSHlength();
    if ($LEN > $MAXL || $LEN < 0 ) {
        die "NSHadd: NSH Record length is INVALID, can't be greater than $MAXL\n";
    }
    
    if($argc > 0) {
        my $narg = 1;
        if($argc >= 2) {
            my $nshlen = 0;
# VALIDATE first arg, LENGTH
            if( $args[0] =~ /[0-9]+/ ) {
                $nshlen = $args[0];
                
                if ($nshlen > $MAXL) {
                    die "NSHadd: Record length (first arg) is INVALID, can't be greater than $MAXL\n";
                }
                
                if ($nshlen != $LEN) {
                    die "NSHadd: Record length (first arg) must match NSH length ($LEN)\n";
                }
                
                $narg = $args[1];
                
            }else {
                die "NSHadd: Record length (first arg) must be an integer\n";
            }
            
        } else {
            
            $narg = $args[0];    # allow for first length arg to be optional
            
        } # default value is one 
        
# VALIDATE second arg, NUMBER TO ADD TO LENGTH
        if( $narg =~ /-?[1-9][0-9]*/ ) {
            $n = $narg;
            if ($n < 0) {                       # new to v.9.5
                $PADLEFT = 1;
                $n = -1 * $n;
                $debug && print "padright is <$PADLEFT>\n";
            }
        } else {
            die "NSHadd: length to add (second arg) must be an integer\n";
        }
        $debug && print "NSHadd: arg validated len is <$LEN>, n is <$n>\n";
    } else {
        $n = 1;
    }

    if ($LEN + $n > $MAXL || $LEN + $n == 0 ) {
        die "NSHadd: Number of columns to add is INVALID, total can't be greater than $MAXL\n";
    }
    return $n;
}


# add each bit of rec separately
sub add2NSH {
    my $c = $_[0];     # bit value
    my $n = $_[1];     # extra length
    my $len = $_[2];   # total length   
    my $name = $_[3];    # filename
    my $fn = "$configfilepath/$name";
    my $pos = 0;
    my $padch = "0";              # no patterns

    $debug && print "char is <$c>, padding is <$padch>\n";

    while($pos < $n){
        my $rtn;
        my $q = $padch;
        $q = &pad_left_length($q,$padch,$len);  # all *'s initially
        substr($q,$len - $pos - 1,1,$c);        # set the bit value at pos in q
        $debug && print "adding with <$q>\n";
        $rtn = &negOp($q,$fn,"-a");
        if ( $rtn != 0) {
            last;
        }
        $pos++;
    }
    return $pos;
}


sub negOp {
    my $s = $_[0];
    my $jn = $_[1];
    my $op = $_[2];
    my $ans;
    my $cmd  = "$negdbpath/negdb $op \"$s\" -f \"$jn\" -m $MAX_KEY -D \"$tmpfilepath\" -T $cleanuppercent";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "NSHadd negOp: $op failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0 || $ans == 1) {
            ($debug || $admin) && print("NSHadd negOp $op completed\n");
            return $ans;
        } 
        if ($ans > 1) {
            print("NSHadd negOp $op found error [$ans]\n");
        }
    }
    $debug && print "args to negOp: op is <$op>, s is <$s>, jn is <$jn>, and ans is <$ans>\n";
    $SANITY && die "NSHadd: args to negOp: op is <$op>, s is <$s>, jn is <$jn>, and ans is <$ans>\n";
    return $ans;
}


# do the negative CrossProduct on two ndb's fn and jn, and save as fn
# to pad to the right the argument name is first parameter to negRelOp v.9.5 
sub extendNSH {
    my $newlen = $_[0];
    my $name = $_[1];
    my $fn = "$configfilepath/$nshfilename";
    my $jn = "$name";
    my $rtn;
    if ($PADLEFT) {
        $rtn = &negRelOp($jn,$fn,$fn,'C');
    } else {
        $rtn = &negRelOp($fn,$jn,$fn,'C');
    }
    if($rtn != 0) {
        print "NSHadd extend NSH failed with rtn code $rtn\n";
        return $rtn;
    }
    return 0;
}


# do the negative relational operation op, on ndb files fn and jn, 
# and output to outfn; normal completion returns 0.
sub negRelOp {
    my $fn = $_[0];
    my $jn = $_[1];
    my $outfn = $_[2];
    my $op = $_[3];
    my $ans;
    my $cmd="$negdbpath/negdb -x $op -f \"$fn\" -j \"$jn\" -o \"$outfn\" -D \"$tmpfilepath\"";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "NSHadd negRelOp: $op failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0 || $ans == 1) {
            ($debug || $admin) && print("NSHadd negRelOp $op completed\n");
            return $ans;
        } 
        if ($ans > 1) {
            print("NSHadd negRelOp $op found error [$ans]\n");
        }
    }
    $debug && print "args to negRelOp: op is <$op>, fn is <$fn>, jn is <$jn>, outfn is <$outfn>, and ans is <$ans>\n";
    $SANITY && die "NSHadd:args to negRelOp: op is <$op>, fn is <$fn>, jn is <$jn>, outfn is <$outfn>, and ans is <$ans>\n";
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


# builds a string with n c's, and the rest of length l padded with don't cares
sub build_newrec {
    my $n = $_[0];
    my $c = $_[1];
    my $l = $_[2];
    my $rec = $c x $n;
    $debug && print "build_newrec: rec is <$rec>\n";
    $rec = &pad_left_length($rec,"*",$l);
    $debug && print "build_newrec: after padding, rec is <$rec>\n";
    return $rec;
}


# create a new empty negative database of length n
sub new_empty {
    my $n = $_[0];
    my $name = $_[1];
    my $rtn = 1;
    my $fn = $name;
    my $cmd = "$negdbpath/negdb -f \"$fn\" -l $n -N 4 -b -D \"$tmpfilepath\"";
    $debug && print("new_empty: cmd is [$cmd]\n");
    `$cmd $stdoutput $stderrors`;
    
    if ($? == -1) {
        print "new_empty: failed to execute [$!]\n";
        $rtn = $?;
    } else {
        $rtn = ($? >> 8); 
    }
    
    if($rtn == 0) {
        $admin && print("Created an empty NSH");
        $admin && print(" [$fn]");
        $admin && print "\n";
    } else {
        print("New Empty NSH FAILED");
        $admin && print(" ($rtn)");
        $admin && print "\n";
    }
    return $rtn;
}


sub get_NSHlength {
    my $nshlen=0;
    my $fnsh = "$configfilepath/$nshfilename";

    stat($fnsh);
    if (! -e _ ) {
        die "NSHadd: <$nshfilename> file not found\n";
    }

    open NSHFILE,"$fnsh" || die "cannot open $fnsh for reading the length";
    my $raw = <NSHFILE>;
    close NSHFILE;
    $debug && print "NSHadd raw length of NSH is <$raw>\n";        

    if( $raw =~ /^b([1-9][0-9]*)$/){
        $nshlen = $1;
    }
    return $nshlen;
}


# save last NSH and increment Z step for next time (if testing); 
# save final file as new NSH;
# remove all the intermediate files
sub bkupNSH {
    my $fnsh = "$configfilepath/$nshfilename";
 
    if($SAVE_THE_OLD_NSH==1){
        my $zfile = $configfilepath . "/Z";
        open ZFILE,"$zfile" || die "cannot open zfile";
        my $raw = <ZFILE>;
        close ZFILE;
        $debug && print "NSHadd cleanup read raw from z <$raw>\n";

        my $it="";
        if( $raw =~ /^([1-9][0-9]*)$/){
            $it = $1;
            copy($fnsh, $fnsh.$it) || die "NSHadd: copy failed $!";
            $it++;
            open ZFILE,">$zfile" || die "NSHadd: cannot open zfile";
            print ZFILE "$it";
            close ZFILE;
        } else {
            print "NSHadd: problems with bkup, Z contains junk!!\n";
        }
    }
}


sub cleanup {
    if(! $debug) {
        unlink("$tmpfilepath/new.nsh") || die "NSHadd: can't cleanup $!";
        unlink("$tmpfilepath/clean.dat");
        unlink("$tmpfilepath/sizes.dat");
        unlink("$tmpfilepath/distrib7-RNDB-0.dat");
        rmdir($tmpfilepath) || die "NSHadd: can't clean up $!";
    }
}


sub main
{
    my $n = &command_args(@_);  # aborts if error
    my $newfn = "new.nsh";

    if ($LEN == 0) {            # special case: nsh has zero length, no records v.4.2
        $newfn = "$configfilepath/$nshfilename";
    } else {
        $tmpfilepath = mkdtemp("$configfilepath/nshtmpADD_XXXXX");
        ($tmpfilepath =~ /undef/ ) && die "NSHadd: fail to make temporary data directory\n"; 
        $debug && print "tmp dir is <$tmpfilepath>\n";
        $newfn = "$tmpfilepath/$newfn";
    }

# OKAY TO BEGIN
    &bkupNSH();

    &new_empty($n,$newfn);

# add all zeros record to new empty ndb,
# in order to extend the existing values of NSH 
# with pure zeros and does not change their semantics
    my $rec = &build_newrec($n,"0",$n);
    &negOp($rec,$newfn,"-a");

    if($LEN > 0) {              # special case: nsh has zero length, no records v.4.2
        &extendNSH($n,$newfn);
        if ($debug) {
            print "CRACK AFTER CP:\n";
            my $log = `$crackpath/crack_ndb.pl $nshfilename -b -d $tmpfilepath`;
            print "<$log>";
        }
    }

# adds the n-bits individually as patterns, after the CP in order to
# get the new single bit records (the rest all zeros) added to NSH
#    &add2NSH("1",$n,$LEN+$n,$nshfilename);

    &cleanup();
    return 0;
}

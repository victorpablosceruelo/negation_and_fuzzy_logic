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
# Filename      : CSHproject.pl
# Language      : Perl 
# Purpose       : reduce the length of the negative database record
# 
# Creator       : E.S. Ackley
# Creation Date : Thu May 10 19:13:38 2007
# Updated       : Sat Mar 22 14:46:14 2008 
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
my $configfilepath    = "/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tmp";                 
my $tmpfilepath;
my $cshfilename       = "NSH0";    #default negative database
my $resultcshfilename = "NSH0";    #default negative database

my $SAME_NAME = 1;    # 0=input & result cshfilenames are different, default same
my $LEN=0;
my $MAXL=1000;
my $MAX_KEY=1;
my $debug=0;
my $admin=0;

my $stderrors = "2> /dev/null";
my $stdoutput = "> /dev/null";

my $SANITY=1;               # 1 will die when insanity happens; 0 will be silent

exit(&main(@ARGV));

sub usage_abort 
{
    die "Usage: % ./CSHproject.pl len x [cshfilename [resultcshfilename]] | x, where x is a string that specifies the zero bit positions to be eliminated; cshfilename and resultcshfilename default to CSH\n";
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
        if ($args[0] =~ /^[Z?|help]/ ) {    # unquoted ? looks like a capital z
            &usage_abort();
        }
    }

# alternate CSH file name, assumes last of four args
    if($argc >= 3) {
        if( $args[2] =~ /^[0-9a-zA-Z\-\/\\\._]{1,255}$/ ) {
            $cshfilename = $args[2];
        } else {
            die "CSHproject.pl: csh filename must be alphanumeric and less than 255 letters long>\n";
        }

        if($argc > 3) {
            if( $args[3] =~ /^[0-9a-zA-Z\-\/\\\._]{1,255}$/ ) {
                $resultcshfilename = $args[3];
                $SAME_NAME = 0;
            } else {
                die "CSHproject: csh result filename must be alphanumeric and less than 255 letters long>\n";
            }
        } else {
            $resultcshfilename = $cshfilename;
        }
    }
    
    $LEN = &get_CSHlength();
    if ($LEN > $MAXL || $LEN == 0 ) {
        die "CSHproject: CSH Record length is INVALID, can't be greater than $MAXL or less than 1\n";
    }
    
    if($argc >= 2) {
        my $cshlen = 0;
# VALIDATE first arg, LENGTH
        if( $args[0] =~ /[1-9][0-9]*/ ) {
            $cshlen = $args[0];
            if ($cshlen > $MAXL) {
                die "CSHproject: Record length (first arg) is INVALID, can't be greater than $MAXL \n";
            }
            if ($cshlen == 1) {
                die "CSHproject: Record length (first arg) must be greater than 1\n";
            }
        } else {
            die "CSHproject: Record length (first arg) is an invalid number\n";
        }
        if ($cshlen != $LEN) {
            die "CSHproject: Record length (first arg) must match CSH length ($LEN)\n";
        }
        $xarg = $args[1];
    } else {
        $xarg = $args[0];    # allow for first length arg to be optional
    }
    
# VALIDATE AND PREPARE x arg
    if($xarg =~ /^([01]+)$/ ){
        $x = $xarg;
        $xcount = ($x =~ tr/1//); 
        if ($xcount == 0) {
            die "CSHproject: error, x has <$xcount> no 1's\n";
        }
        my $xlen = length $x;
        if ($xlen < $LEN) {
            $x = &pad_left_length($x,'0',$LEN);
        } elsif ($xlen > $LEN) {
            die "CSHproject: x argument <$xarg> must have only $LEN 1's and 0's\n";
        }
    } else {
        die "CSHproject: x argument <$xarg> must have $LEN 1's and 0's\n";
    }
    return ($x,$xcount);
}


sub pad_left_length {
    my $s = $_[0];
    my $c = $_[1];
    my $l = $_[2];
    my $sl = length $s;
    my $ns = ($c x ($l - $sl)) . $s;   # pad left with $c's (e.g. zeros, *'s)
    return $ns;
}


sub get_CSHlength {
    my $cshlen=0;
    my $fcsh = "$configfilepath/$cshfilename";

    stat($fcsh);
    if (! -e _ ) {
        die "CSHproject: <$fcsh> file not found\n";
    }

    open CSHFILE,"$fcsh" || die "cannot open $fcsh for reading the length";
    my $raw = <CSHFILE>;
    close CSHFILE;
    $debug && print "CSHproject raw length of CSH is <$raw>\n";        

    if( $raw =~ /^b([1-9][0-9]*)$/){
        $cshlen = $1;
    }
    return $cshlen;
}


# keep only set bit positions; use hash to eliminate duplicates;
sub reduce {
    my $x = $_[0];
    my $xcnt = $_[1];
    my $fn = "$configfilepath/$cshfilename";
    my @idxarray = {};
    my %reducehash = ();
    
    $debug && print "x is <$x> and xcnt is $xcnt\n";

    my $idx = $LEN + 1;
    my $j = 0;
    while($j < $xcnt) {
        $idx = rindex($x,"1",$idx-1);   # next bit position
        $debug && print "$j th index is <$idx>\n";
        $idxarray[$j]=$idx; 
        $j++;
    }

    $debug && print "array looks like <@idxarray>\n";

    open INCSH,"$fn" || die "CSHadd: could not open csh <$fn>\n";

    while(<INCSH>) {
        if(/^([01\*]+)$/){
            my $s = $1;
            my $w = "";
            my $i=0;
            while($i < $xcnt) {
                $w = substr($s,$idxarray[$i],1) . $w;
                $debug && print "new w is <$w>, i is <$i>, idx is $idxarray[$i]\n";
                $i++;
            }
            $reducehash{$w} = 1;
        }
    }
    close INCSH;
    return \%reducehash;
}


sub save_reduction{
    my $href = $_[0];
    my $len = $_[1];
    my $outfn = $_[2];
    my $r;

    open(NEWCSH,">$outfn") || die "Unable to create new csh $outfn\n";
    print NEWCSH "b$len\n"; 

    foreach $r (keys %$href) {
        print NEWCSH "$r\n";
    }
    
    close NEWCSH;
    return;
}


sub partial_query {
    my $q = $_[0];
    my $fn = $_[1];
    my $outfn = $_[2];
    my $ans;
    my $cmd  = "$negdbpath/negdb -C \"$q\" -f \"$fn\" -o \"$outfn\" -D \"$tmpfilepath\" -m $MAX_KEY";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "CSHproject partial_query: failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0 || $ans == 1) {
            ($debug || $admin) && print("CSHproject partial query completed\n");
            return $ans;
        }
        if($ans > 1) {
            print("CSHproject partial query found error [$ans]\n");
        }
    }
    $debug && print "args to partial query: q is <$q>, ans is <$ans>\n";
    $SANITY && die "CSHproject FAILURE: args to partial query: q is <$q>, ans is <$ans>\n";
    return $ans;
}


# used to get sorted output order only any more.
sub compress_reduction{
    my $len = $_[0];
    my $fn = $_[1];
    my $outfn = $_[2];
    my $q = "";
    $q = &pad_left_length($q,"*",$len);
    &partial_query($q,$fn,$outfn);
}


# save final file as new CSH;
# remove all the intermediate files
sub cleanup {
    my $final = $_[0];
    if(! $debug) {    
        unlink("$tmpfilepath/sizes.dat");
        unlink($final);
        rmdir($tmpfilepath) || die "CSHproject: can't clean up $!";;
#        `rm -rf $tmpfilepath`;
    }
}


sub main{
    my ($x, $xcount) = &command_args(@_);  # aborts if error

#make temporary directory for .csh files
    $tmpfilepath = mkdtemp("$configfilepath/cshtmpPROJECT_XXXXX");
    ($tmpfilepath =~ /undef/ ) && die "CSHproject: fail to make temporary data directory\n"; 
    $debug && print "tmp dir is <$tmpfilepath>\n";

    my $reducehashref = &reduce($x,$xcount);
    my $newtmpname = "$tmpfilepath/newcsh.tmp";

    &save_reduction($reducehashref,$xcount,$newtmpname);

    &compress_reduction($xcount,$newtmpname,"$configfilepath/$resultcshfilename");

    &cleanup($newtmpname);
    return 0;
}

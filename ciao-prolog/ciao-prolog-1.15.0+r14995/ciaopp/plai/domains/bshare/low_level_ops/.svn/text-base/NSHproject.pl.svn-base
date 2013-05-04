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
# Filename      : NSHproject.pl
# Language      : Perl 
# Purpose       : reduce the length of the negative database record
# 
# Creator       : E.S. Ackley
# Creation Date : Thu May 10 19:13:38 2007
# Updated       : Thu Mar  6 13:13:35 2008 
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
my $crackpath = "/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/low_level_ops";
my $reduceonepath     = $crackpath;
my $nshfilename       = "NSH0";    #default negative database
my $resultnshfilename = "NSH0";    #default negative database

my $SAME_NAME = 1;    # 0=input & result nshfilenames are different, default same
my $LEN=0;
my $MAXL=1000;
my $debug=0;
my $admin=0;

my $stderrors = "2> /dev/null";
my $stdoutput = "> /dev/null";

exit(&main(@ARGV));

sub usage_abort 
{
    die "Usage: % ./NSHproject.pl len x [nshfilename [resultnshfilename]] | x, where x is a string that specifies the zero bit positions to be eliminated; nshfilename and resultnshfilename default to NSH\n";
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

# alternate NSH file name, assumes last of four args
    if($argc >= 3) {
        if( $args[2] =~ /^[0-9a-zA-Z\-\/\\\._]{1,255}$/ ) {
            $nshfilename = $args[2];
        } else {
            die "NSHproject.pl: nsh filename must be alphanumeric and less than 255 letters long>\n";
        }

        if($argc > 3) {
            if( $args[3] =~ /^[0-9a-zA-Z\-\/\\\._]{1,255}$/ ) {
                $resultnshfilename = $args[3];
                $SAME_NAME = 0;
            } else {
                die "NSHprojectone: nsh result filename must be alphanumeric and less than 255 letters long>\n";
            }
        } else {
            $resultnshfilename = $nshfilename;
        }
    }
    
    $LEN = &get_NSHlength();
    if ($LEN > $MAXL || $LEN == 0 ) {
        die "NSHproject: NSH Record length is INVALID, can't be greater than $MAXL or less than 1\n";
    }
    
    if($argc >= 2) {
        my $nshlen = 0;
# VALIDATE first arg, LENGTH
        if( $args[0] =~ /[1-9][0-9]*/ ) {
            $nshlen = $args[0];
            if ($nshlen > $MAXL) {
                die "NSHproject: Record length (first arg) is INVALID, can't be greater than $MAXL \n";
            }
            if ($nshlen == 1) {
                die "NSHproject: Record length (first arg) must be greater than 1\n";
            }
        } else {
            die "NSHproject: Record length (first arg) is an invalid number\n";
        }
        if ($nshlen != $LEN) {
            die "NSHproject: Record length (first arg) must match NSH length ($LEN)\n";
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
            die "NSHproject: error, x has <$xcount> no 1's\n";
        }
        my $xlen = length $x;
        if ($xlen < $LEN) {
            $x = &pad_left_length($x,'0',$LEN);
        }  elsif ($xlen > $LEN) {
            die "NSHproject: x argument <$xarg> must have only $LEN 1's and 0's\n";
        }
        $xcount = $LEN - $xcount;  # we want the number of zeros now
    } else {
        die "NSHproject: x argument <$xarg> must have $LEN 1's and 0's\n";
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


sub get_NSHlength {
    my $nshlen=0;
    my $fnsh = "$configfilepath/$nshfilename";

    stat($fnsh);
    if (! -e _ ) {
        die "NSHproject: <$fnsh> file not found\n";
    }

    open NSHFILE,"$fnsh" || die "cannot open $fnsh for reading the length";
    my $raw = <NSHFILE>;
    close NSHFILE;
    $debug && print "NSHproject raw length of NSH is <$raw>\n";        

    if( $raw =~ /^b([1-9][0-9]*)$/){
        $nshlen = $1;
    }
    return $nshlen;
}


# reduce each zero bit position; iterates directly on result nsh,
# reducing the record length by one each time. v.7.3
sub reduce {
    my $x = $_[0];
    my $xcnt = $_[1];
    my $idx = $LEN + 1;
    my $i = 0;

    $debug && print "x is <$x> and xcnt is $xcnt, i is $i\n";

# before starting, copy input to output filename, if different.
#    if(! $SAME_NAME) {
#        copy("$configfilepath/$nshfilename","$configfilepath/$resultnshfilename") || die "NSHproject: copy failed $!\n";
#    }

    while($i < $xcnt) {
        my $len = $LEN - $i;
        my $newx = "";
        $newx = &pad_left_length($newx,"0",$len);
        $idx = rindex($x,"0",$idx-1);   # next zero bit position
        substr($newx,$idx,1,"1");       # set to one in query string
        $debug && print "newx is <$newx>, len is <$len>, idx is <$idx>\n";
        my $cmd = "$reduceonepath/NSHreduceone.pl $len $newx $resultnshfilename";
        $debug && print "$cmd\n";
        my $results = `$cmd`;
        $debug && print "$results";
        if($debug) {
            print "CRACK PROJECT ($i):\n";
            my $log = `$crackpath/crack_ndb.pl $resultnshfilename -b`;
            print "<$log>";
        }
        $i++;
    }
    return $i;
}


# append all zero record to the negative database v.15.3
sub remove_allzero_record {
    my $newlen = $_[0];
    my $nshfn = "$configfilepath/$resultnshfilename";
    my $rec = "0" x $newlen;

    open(NSH,">>$nshfn") || die "Unable to open nsh $nshfn for writing all zero record\n";
    print NSH "$rec\n"; 
    close NSH;
    return 0;
}


sub main{
    my $x;
    my $xcount;

    ($x, $xcount) = &command_args(@_);  # aborts if error

    &reduce($x,$xcount);

# for program verification application, all zeros is meaningless
#    &remove_allzero_record($LEN-$xcount);

    return 0;
}

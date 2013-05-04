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
# Filename      : NSHcompare.pl
# Language      : Perl 
# Purpose       : comparison of two negative databases, using complements
# 
# Creator       : E.S. Ackley
# Creation Date : Wed Jun  6 11:35:05 2007 
# Updated       : Sat Mar 22 13:59:24 2008 
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
my $negdbpath = "nice -19 /home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/low_level_ops/c_code";     
my $configfilepath = "/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tmp";                 
my $nshfilename2   = "NSH1";    # default negative database
my $nshfilename1   = "NSH2";       # the other negative database

my $LEN=0;
my $MAXL=1000;
my $debug=0;
my $admin=0;

my $stderrors = "2> /dev/null";
my $stdoutput = "> /dev/null";

my $SANITY=1;               # 1 will die when insanity happens; 0 will be silent

exit(&main(@ARGV));

sub usage_abort 
{
    die "Usage: % ./NSHcompare.pl [len] nshfilename1 [nshfilename2], where nshfilename2 default is NSH\n";
}


sub command_args
{
    my $argc = @_;   #number of args
    my @args = @_;
    my $n1arg;
    my $n2arg;
    my $lenarg;
    my $len2;

    if($argc > 3 || $argc == 0) {
        &usage_abort();
    }

    if($argc > 0){
#print "args 0 is <$args[0]>\n";
        if ($args[0] =~ /^[Z?|help]/ ) {    # unquoted ? looks like a capital z
            &usage_abort();
        }
    }

# alternate NSH file name, assumes last of three args
    if($argc > 1) {
        if ($argc == 2) {
            $n2arg = $args[1];
            $n1arg = $args[0];
        } else {
            $n2arg = $args[2];
            $n1arg = $args[1];
            $lenarg = $args[0];
        }
        
        if( $n2arg =~ /^[0-9a-zA-Z\-\/\\\._]{1,255}$/ ) {
            $nshfilename2 = $n2arg;
        } else {
            die "NSHcompare: nsh filename2 must be alphanumeric and less than 255 letters long>\n";
        } 
    } else {
        $n1arg = $args[0];
    }

    if( $n1arg =~ /^[0-9a-zA-Z\-\/\\\._]{1,255}$/ ) {
        $nshfilename1 = $n1arg;
    } else {
        die "NSHcompare: nsh filename1 must be alphanumeric and less than 255 letters long>\n";
    }

    $LEN = &get_NSHlength($nshfilename1);
    if ($LEN < 0 || $LEN > $MAXL) {
        die "NSHcompare: nsh <$nshfilename1> record length must be positive and less than $MAXL\n";
    }
    
    $len2 = &get_NSHlength($nshfilename2);
    if ($len2 != $LEN) {
        die "NSHcompare: NSH record lengths must be the same ($LEN,$len2)\n";
    }

# VALIDATE first arg, LENGTH ??? do we even need this?
    if ($argc == 3) {
        if( $lenarg =~ /[0-9]+/ ) {
            my $l = $lenarg;
            if ($l != $LEN) {
                die "NSHcompare: Record length (first arg) is INVALID\n";
            }
        } else {
            die "NSHcompare: Record length (first arg) must be numeric\n";
        }
    }
}


sub compare {
    my $fn = $_[0];
    my $jn = $_[1];
    my $ans;
    my $cmd  = "$negdbpath/negdb -Z 3 -f \"$configfilepath/$fn\" -j \"$configfilepath/$jn\"";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "NSHcompare comparison: failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0 || $ans == 1) {
            ($debug || $admin) && print("NSHcompare comparison completed\n");
            return $ans;
        }
        if($ans > 1) {
            print("NSHcompare query found error [$ans]\n");
        }
    }
    $debug && print "args to query: ans is <$ans>\n";
    $SANITY && die "NSHcompare: args to query: ans is <$ans>\n";
    return $ans;
}


sub get_NSHlength {
    my $name = $_[0];
    my $fnsh = "$configfilepath/$name";
    my $raw;
    my $l;

    stat($fnsh);
    if (! -e _ ) {
        die "NSHcompare: <$fnsh> file not found\n";
    }

    open INDB,$fnsh || die "Unable to open $fnsh";

    $raw = <INDB>;
    close INDB;

    if( $raw =~ /^[ab]([1-9][0-9]*)$/ ){
        $l = $1;
    } else {
        return 0;
    }
    return $l;
}


sub main
{
    &command_args(@_);  # aborts if error

    my $ans = &compare($nshfilename1,$nshfilename2);
    if ($ans == 1) {     # flip the answer
        ($admin || $debug) && print "DIFFERENT\n";
    #    print "0\n";
        write_result("0\n");
        return 0;
    }
    
    ($admin || $debug) && print "SAME\n";
    #print "1\n";         # flip the answer
    write_result("1\n");
    return 1;
}

sub write_result
{
        my $result = $_[0];

	open  (MYFILE,">$configfilepath/compare.txt") || die("Cannot Open File to print result");
	print MYFILE $result;
	close (MYFILE); 

}

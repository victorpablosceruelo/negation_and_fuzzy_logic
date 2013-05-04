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
# Filename      : NSHunion.pl
# Language      : Perl 
# Purpose       : Negative union of two negative databases
# 
# Creator       : E.S. Ackley
# Creation Date : Sat Jun  2 10:42:55 2007 
# Updated       : Mon Mar 24 12:14:55 2008 
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
my $tmpfilepath;      # set by script
my $negdbpath = "nice -19 /home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/low_level_ops/c_code";     
my $nshfilename3   = "NSH0";       # default result negative database
my $nshfilename2   = "NSH2";       # default negative database
my $nshfilename1   = "NSH1"; # the other negative database

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
    die "Usage: % ./NSHunion.pl len nshfilename1 nshfilename2 resultnshfilename | nshfilename1 [nshfilename2 resultnshfilename], where nshfilename2 and resultnshfilename default to NSH\n";
}


sub command_args
{
    my $argc = @_;   #number of args
    my @args = @_;
    my $n1arg;
    my $n2arg;
    my $n3arg;
    my $lenarg;
    my $len2;

    if($argc > 4 || $argc == 0) {
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
        if($argc == 4) {
            $n3arg = $args[3];
            $n2arg = $args[2];
            $n1arg = $args[1];
            $lenarg = $args[0];
        } elsif ($argc == 3) {
            $n3arg = $args[2];
            $n2arg = $args[1];
            $n1arg = $args[0];
        } else {
            &usage_abort();
        }

        if( $n2arg =~ /^[0-9a-zA-Z\-\/\\\._]{1,255}$/ ) {
            $nshfilename2 = $n2arg;
        } else {
            die "NSHunion: nsh filename2 must be alphanumeric and less than 255 letters long>\n";
        } 
        
        if( $n3arg =~ /^[0-9a-zA-Z\-\/\\\._]{1,255}$/ ) {
            $nshfilename3 = $n3arg;
        } else {
            die "NSHunion: nsh result filename must be alphanumeric and less than 255 letters long>\n";
        } 
    } else {
        $n1arg = $args[0];
    }

    if( $n1arg =~ /^[0-9a-zA-Z\-\/\\\._]{1,255}$/ ) {
        $nshfilename1 = $n1arg;
    } else {
        die "NSHunion: nsh filename1 must be alphanumeric and less than 255 letters long>\n";
    }
    
    $LEN = &get_NSHlength($nshfilename1);
    if ($LEN < 0 || $LEN > $MAXL) {
        die "NSHunion: nsh <$nshfilename1> record length must be positive and less than $MAXL\n";
    }
    
    $len2 = &get_NSHlength($nshfilename2);
    if ($len2 != $LEN) {
        die "NSHunion: NSH record lengths must be the same ($LEN,$len2)\n";
    }

# VALIDATE first arg, LENGTH ??? do we even need this?
    if ($argc == 4) {
        if( $lenarg =~ /[0-9]+/ ) {
            my $l = $lenarg;
            if ($l != $LEN) {
                die "NSHunion: Record length (first arg) is INVALID\n";
            }
        } else {
            die "NSHunion: Record length (first arg) must be numeric\n";
        }
    }
}


sub get_NSHlength {
    my $name = $_[0];
    my $fnsh = "$configfilepath/$name";
    my $raw;
    my $l;

    stat($fnsh);
    if (! -e _ ) {
        die "NSHunion: <$fnsh> file not found\n";
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


sub negRelOp {
    my $fn = $_[0];
    my $jn = $_[1];
    my $outfn = $_[2];
    my $op = $_[3];
    my $ans;
    my $cmd  = "$negdbpath/negdb -x $op -f \"$fn\" -j \"$jn\" -o \"$outfn\" -D \"$tmpfilepath\" -T $cleanuppercent -k $MAX_KEY";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "NSHunion negRelOp: $op failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0 || $ans == 1) {
            ($debug || $admin) && print("NSHunion negRelOp $op completed\n");
            return $ans;
        } 
        if ($ans > 1) {
            print("NSHunion negRelOp $op found error [$ans]\n");
        }
    }
    $debug && print "args to negRelOp: op is <$op>, fn is <$fn>, jn is <$jn>, outfn is <$outfn>, and ans is <$ans>\n";
    $SANITY && die "NSHunion: args to negRelOp: op is <$op>, fn is <$fn>, jn is <$jn>, outfn is <$outfn>, and ans is <$ans>\n";
    return $ans;
}


sub cleanup {
    if(! $debug) {
        unlink("$tmpfilepath/clean.dat");
        unlink("$tmpfilepath/sizes.dat");
        unlink("$tmpfilepath/distrib7-RNDB-0.dat");
        rmdir($tmpfilepath) || die "NSHunion: can't clean up $!";
    }
}


sub main
{
    &command_args(@_);  # aborts if error

# make tmp dir for temporary ndb files
        $tmpfilepath = mkdtemp("$configfilepath/nshtmpUNION_XXXXX");
        ($tmpfilepath =~ /undef/ ) && die "NSHunion: fail to make temporary data directory\n"; 
        $debug && print "tmp dir is <$tmpfilepath>\n";

# OKAY TO BEGIN THE REAL WORK
    my $fnsh = "$configfilepath/$nshfilename1";
    my $jnsh = "$configfilepath/$nshfilename2";
    my $outnsh = "$configfilepath/$nshfilename3";
    my $rtn = &negRelOp($fnsh,$jnsh,$outnsh,"U");
    if($rtn != 0) {
        die "NSHunion negative union operation on <$fnsh> and <$jnsh> failed with rtn code $rtn\n";
    }
    &cleanup();
    return 0;
}

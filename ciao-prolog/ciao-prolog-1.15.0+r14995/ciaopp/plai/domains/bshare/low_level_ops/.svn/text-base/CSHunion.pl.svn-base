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
# Filename      : CSHunion.pl
# Language      : Perl 
# Purpose       : Union of two compressed databases (Negative Intersection)
# 
# Creator       : E.S. Ackley
# Creation Date : Fri Dec  7 12:00:33 2007 
# Updated       : Wed Mar 26 19:33:15 2008 
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
my $tmpfilepath;           # set by script
my $cshfilename3   = "NSH0"; # default result negative database
my $cshfilename2   = "NSH2"; # default negative database
my $cshfilename1   = "NSH1"; # the other negative database

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
    die "Usage: % ./CSHunion.pl len cshfilename1 cshfilename2 resultcshfilename | cshfilename1 [cshfilename2 resultcshfilename], where cshfilename2 and resultcshfilename default to CSH\n";
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

# alternate CSH file name, assumes last of three args
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
            $cshfilename2 = $n2arg;
        } else {
            die "CSHunion: csh filename2 must be alphanumeric and less than 255 letters long>\n";
        } 
        
        if( $n3arg =~ /^[0-9a-zA-Z\-\/\\\._]{1,255}$/ ) {
            $cshfilename3 = $n3arg;
        } else {
            die "CSHunion: csh result filename must be alphanumeric and less than 255 letters long>\n";
        } 
    } else {
        $n1arg = $args[0];
    }

    if( $n1arg =~ /^[0-9a-zA-Z\-\/\\\._]{1,255}$/ ) {
        $cshfilename1 = $n1arg;
    } else {
        die "CSHunion: csh filename1 must be alphanumeric and less than 255 letters long>\n";
    }
    
    $LEN = &get_CSHlength($cshfilename1);
    if ($LEN < 0 || $LEN > $MAXL) {
        die "CSHunion: csh <$cshfilename1> record length must be positive and less than $MAXL\n";
    }
    
    $len2 = &get_CSHlength($cshfilename2);
    if ($len2 != $LEN) {
        die "CSHunion: CSH record lengths must be the same ($LEN,$len2)\n";
    }

# VALIDATE first arg, LENGTH ??? do we even need this?
    if ($argc == 4) {
        if( $lenarg =~ /[0-9]+/ ) {
            my $l = $lenarg;
            if ($l != $LEN) {
                die "CSHunion: Record length (first arg) is INVALID\n";
            }
        } else {
            die "CSHunion: Record length (first arg) must be numeric\n";
        }
    }
}


sub get_CSHlength {
    my $name = $_[0];
    my $fcsh = "$configfilepath/$name";
    my $raw;
    my $l;

    stat($fcsh);
    if (! -e _ ) {
        die "CSHunion: <$fcsh> file not found\n";
    }

    open INDB,$fcsh || die "Unable to open $fcsh";

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
    my $cmd  = "$negdbpath/negdb -x $op -f \"$fn\" -j \"$jn\" -o \"$outfn\" -D \"$tmpfilepath\" -T $cleanuppercent -m $MAX_KEY";
    $debug && print "$cmd\n";
    `$cmd $stdoutput $stderrors`;
    if ($? == -1) {
        print "CSHunion negRelOp: $op failed to execute [$!]\n";
        $ans = -1;
    } else {
        $ans = $? >> 8; 
        if($ans == 0 || $ans == 1) {
            ($debug || $admin) && print("CSHunion negRelOp $op completed\n");
            return $ans;
        } 
        if ($ans > 1) {
            print("CSHunion negRelOp $op found error [$ans]\n");
        }
    }
    $debug && print "args to negRelOp: op is <$op>, fn is <$fn>, jn is <$jn>, outfn is <$outfn>, and ans is <$ans>\n";
    $SANITY && die "CSHunion FAILURE: args to negRelOp: op is <$op>, fn is <$fn>, jn is <$jn>, outfn is <$outfn>, and ans is <$ans>\n";
    return $ans;
}


sub cleanup {
    if(! $debug) {
        unlink("$tmpfilepath/clean.dat");
        unlink("$tmpfilepath/sizes.dat");
        unlink("$tmpfilepath/distrib7-RNDB-0.dat");
        rmdir($tmpfilepath) || die "CSHunion: can't clean up $!";
    }
}


sub main
{
    &command_args(@_);  # aborts if error

# make tmp dir for temporary ndb files
        $tmpfilepath = mkdtemp("$configfilepath/cshtmpUNION_XXXXX");
        ($tmpfilepath =~ /undef/ ) && die "CSHunion: fail to make temporary data directory\n"; 
        $debug && print "tmp dir is <$tmpfilepath>\n";

# OKAY TO BEGIN THE REAL WORK
    my $fcsh = "$configfilepath/$cshfilename1";
    my $jcsh = "$configfilepath/$cshfilename2";
    my $outcsh = "$configfilepath/$cshfilename3";
    my $rtn = &negRelOp($fcsh,$jcsh,$outcsh,'I');
    if(!($rtn == 0 || $rtn == 1)) {
        die "CSHunion negative intersection operation on <$fcsh> and <$jcsh> failed with rtn code $rtn\n";
    }
    &cleanup();
    return 0;
}

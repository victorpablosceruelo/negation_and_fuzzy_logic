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
# Filename      : CSHadd.pl
# Language      : Perl 
# Purpose       : Increase the length of negative database records
#                 with zero values at the beginning or end
# Creator       : E.S. Ackley
# Creation Date : Mon Apr 16 07:31:14 2007
# Updated       : Thu Mar  6 09:43:47 2008 
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
my $configfilepath =  "/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tmp";                
my $tmpfilepath;
my $cshfilename = "NSH0";    #default negative database

my $LEN=0;
my $MAXL=1000;
my $PADLEFT=0;
my $cleanuppercent=0;      # 0 for no post cleanups; ow. percent of ndbsize
my $debug=0;
my $admin=0;

my $stderrors = "2> /dev/null";
my $stdoutput = "> /dev/null";
my $SAVE_THE_OLD_CSH = 0;    # 1= saves CSH at each step (per file Z); 0 doesn't

exit(&main(@ARGV));

sub usage_abort 
{
    die "Usage: % ./CSHadd.pl len n [cshfilename] | [n], where n is the number of columns of zero to add (negative n prepends to the beginning), default is 1 appended at the end\n";
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

# alternate CSH file name, assumes last of three args
    if($argc == 3) {
        if( $args[2] =~ /^[0-9a-zA-Z\-\/\\\._]{1,255}$/ ) {
            $cshfilename = $args[2];
        } else {
            die "CSHadd.pl: csh filename must be alphanumeric and less than 255 letters long>\n";
        }
    }

    $LEN = &get_CSHlength();
    if ($LEN > $MAXL || $LEN < 0 ) {
        die "CSHadd: CSH Record length is INVALID, can't be greater than $MAXL\n";
    }
    
    if($argc > 0) {
        my $narg = 1;
        if($argc >= 2) {
            my $cshlen = 0;
# VALIDATE first arg, LENGTH
            if( $args[0] =~ /[0-9]+/ ) {
                $cshlen = $args[0];
                
                if ($cshlen > $MAXL) {
                    die "CSHadd: Record length (first arg) is INVALID, can't be greater than $MAXL\n";
                }
                
                if ($cshlen != $LEN) {
                    die "CSHadd: Record length (first arg) must match CSH length ($LEN)\n";
                }
                
                $narg = $args[1];
                
            }else {
                die "CSHadd: Record length (first arg) must be an integer\n";
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
                $debug && print "padleft is <$PADLEFT>\n";
            }
        } else {
            die "CSHadd: length to add (second arg) must be an integer\n";
        }
        $debug && print "CSHadd: arg validated len is <$LEN>, n is <$n>\n";
    } else {
        $n = 1;
    }

    if ($LEN + $n > $MAXL || $LEN + $n == 0 ) {
        die "CSHadd: Number of columns to add is INVALID, total can't be greater than $MAXL\n";
    }
    return $n;
}


# extend each record with newlen 0's
sub extendCSH {
    my $newlen = $_[0];
    my $name = $_[1];
    my $fn = "$configfilepath/$cshfilename";
    my $outfn = "$name";
    my $rtn;
    my $n = $newlen + $LEN;

    open INCSH,"$fn" || die "CSHadd: could not open csh <$fn>\n";

    open(NEWCSH,">$outfn") || die "Unable to create new csh $outfn\n";
    print NEWCSH "b$n\n"; 

    my $rec = &build_newrec($newlen,"0",$newlen);

    while(<INCSH>){
        if(/^([01\*]+)$/){
            my $s = $1;
#        chop $s;

            if ($PADLEFT) {
                $s = $rec . $s;
            } else {
                $s = $s . $rec;
            }
            print NEWCSH "$s\n";
        }
    }
    close INCSH;
    close NEWCSH;
    return 0;
}


# extend each record with newlen 0's
sub extendnewCSH {
    my $newlen = $_[0];
    my $name = $_[1];
    my $outfn = "$name";
    my $rtn;
    my $n = $newlen + $LEN;

    open(NEWCSH,">$outfn") || die "Unable to create new csh $outfn\n";
    print NEWCSH "b$n\n"; 

    my $rec = &build_newrec($newlen,"0",$newlen);

    print NEWCSH "$rec\n";

    close NEWCSH;
    return 0;
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


sub get_CSHlength {
    my $cshlen=0;
    my $fcsh = "$configfilepath/$cshfilename";

    stat($fcsh);
    if (! -e _ ) {
        die "CSHadd: <$cshfilename> file not found\n";
    }

    open CSHFILE,"$fcsh" || die "cannot open $fcsh for reading the length";
    my $raw = <CSHFILE>;
    close CSHFILE;
    $debug && print "CSHadd raw length of CSH is <$raw>\n";        

    if( $raw =~ /^b([1-9][0-9]*)$/){
        $cshlen = $1;
    }
    return $cshlen;
}


# save last CSH and increment Z step for next time (if testing); 
# save final file as new CSH;
# remove all the intermediate files
sub bkupCSH {
    my $fcsh = "$configfilepath/$cshfilename";
 
    if($SAVE_THE_OLD_CSH==1){
        my $zfile = $configfilepath . "/Z";
        open ZFILE,"$zfile" || die "cannot open zfile";
        my $raw = <ZFILE>;
        close ZFILE;
        $debug && print "CSHadd cleanup read raw from z <$raw>\n";

        my $it="";
        if( $raw =~ /^([1-9][0-9]*)$/){
            $it = $1;
            copy($fcsh, $fcsh.$it) || die "CSHadd: copy failed $!";
            $it++;
            open ZFILE,">$zfile" || die "CSHadd: cannot open zfile";
            print ZFILE "$it";
            close ZFILE;
        } else {
            print "CSHadd: problems with bkup, Z contains junk!!\n";
        }
    }
}


sub cleanup {
    my $newcsh = $_[0];
    my $fcsh = "$configfilepath/$cshfilename";
    copy($newcsh,$fcsh);
    if(! $debug) {
        unlink($newcsh) || die "CSHadd: can't cleanup $!";
        rmdir($tmpfilepath) || die "CSHadd: can't clean up $!";
    }
}


sub main
{
    my $n = &command_args(@_);  # aborts if error
    my $newfn = "new.csh";

    if ($LEN == 0) {            # special case: csh has zero length, no records v.4.2
        $newfn = "$configfilepath/$cshfilename";
    } else {
        $tmpfilepath = mkdtemp("$configfilepath/cshtmpADD_XXXXX");
        ($tmpfilepath =~ /undef/ ) && die "CSHadd: fail to make temporary data directory\n"; 
        $debug && print "tmp dir is <$tmpfilepath>\n";
        $newfn = "$tmpfilepath/$newfn";
    }

# OKAY TO BEGIN
    &bkupCSH();

    if($LEN > 0) {              
        &extendCSH($n,$newfn);
        if ($debug) {
            print "CAT AFTER extend:\n";
            my $log = `cat $newfn`;
            print "<$log>";
        }
        &cleanup($newfn);
    } else {
        &extendnewCSH($n,$newfn); # special case: csh has zero length, no records v.4.2
    }
    return 0;
}

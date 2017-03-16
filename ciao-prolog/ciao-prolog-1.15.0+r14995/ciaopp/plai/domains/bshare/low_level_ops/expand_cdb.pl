#!/usr/bin/perl -w 

use Time::Local;
use File::Temp;
use strict;


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

my $debug = 0;
my $configfilepath = "/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tmp";
my $stderrors = "2> /dev/null";
my $stdoutput = "> /dev/null";


exit(&main(@ARGV));

sub read_db {
    my $fn = $_[0];
    my $dbref = $_[1];
    my @dbtmp= `cat $configfilepath/$fn`;
    my $len = shift @dbtmp;       # get rid of blen
    my $ll = length $len;
    $len = substr($len,1,$ll - 2); # subtract one for b, and one for newline
#    print "len is <$len>\n";
    my $r;
    foreach $r (@dbtmp) {
        chop $r;
        push(@$dbref,&substitute_dots_and_dontcares($r));
    }
#    print "first rec is: <$$dbref[0]>\n";
    return $len;
}


sub substitute_dots_and_dontcares {
    my $s = $_[0];
    $debug && print " sub dots and dontcares in <$s>, now ";
    $s =~ (s/\*/\./g);
    $debug && print "<$s>\n";
    return $s;
}

sub query {
    my $qrec = $_[0];
    my $dbref = $_[1];
    my $r;
    my $ans = 0;
    foreach $r (@$dbref) {
        my $hatr = "^"."$r";
        $debug && print "<$qrec> query <$hatr>\n";
        if( $qrec =~ /$hatr/ ) {
            $debug && print "<$qrec> matches <$r>\n";
            $ans = 1;
            last;
        }
    }
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

sub find_all {
    my $l = $_[0];
    my $dbref = $_[1];
    my $endloop = 2**$l;
    my $i = 0;
    while($i < $endloop) {
        my $rec = sprintf("%b",$i);
        $rec = &pad_left_length($rec,"0",$l);
        if( &query($rec,$dbref) == 1 ) {
            print "$rec\n";
        }
        $i++;
    }
}


sub main {
    my $filename = $_[0];
    my @db;
    my $len = &read_db($filename,\@db);
    &find_all($len,\@db);
}

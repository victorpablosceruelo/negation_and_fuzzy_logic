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

my $version = "0.03";
my $debug = 0;
my $debugcmd = 0;
my $configfilepath = "/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tmp";
my $n = 1000;
my $len = 32;
my $seed = 0;
my $TYPE = 1; # 1 is random unique DB, 2 is related DB
my $WEBDEMO = 1;
my $UNIQUESONLY = 0;

#Thu Jul  5 10:03:21 2007  allow configfilepath as argument
# Sat Jul 28 19:34:24 2007 option -u for no duplicates 

&main(@ARGV);

sub usage_abort{
    die "Usage: % perl genDB4.pl [-s <seed>][-l <record length in bits>][-n <number of records>][-t <1 = random (default)|2 = related>|3 = words][-d <directory>][-u uniques only]\n";
}

sub command_args{
    my $cmdlineflag = @_;  #number of args
    my @args = @_;
    $debugcmd && print "got <$cmdlineflag> args\n";

	my $i = 0;

	while($i < $cmdlineflag) {
	    $debugcmd && print "find this arg <$args[$i]>\n";

	    if( $args[$i] =~ /\-([SsLlNnTtDdUu\?])$/ ) {
		my $c = uc $1;
                
		$debugcmd && print "arg is <$c>\n";
                
		# two arg cmds 
                if($c =~ /D/ ) {
                    if( ($i+1 >= $cmdlineflag) || ($args[$i+1] =~ /^-/ )) {
                        &usage_abort;
                    } else {              # here parse bit length
                        $debugcmd && print "2nd arg for D is <$args[$i+1]>\n";
                        $configfilepath = $args[$i+1];
                        $i+=2;
                    }
                } elsif($c =~ /L/ ) {
                    if( ($i+1 >= $cmdlineflag) || ($args[$i+1] =~ /^-/ )) {
                        &usage_abort;
                    } else {              # here parse bit length
                        $debugcmd && print "2nd arg for L is <$args[$i+1]>\n";
                        $len = $args[$i+1];
                        $i+=2;
                    }
                } elsif($c =~ /N/ ) {
                    if( ($i+1 >= $cmdlineflag) || ($args[$i+1] =~ /^-/ )) {
                        &usage_abort;
                    } else {              # here parse bit length
                        $debugcmd && print "2nd arg for N is <$args[$i+1]>\n";
                        $n = $args[$i+1];
                        $i+=2;
                    }
                } elsif($c =~ /S/ ) {
                    if( ($i+1 >= $cmdlineflag) || ($args[$i+1] =~ /^-/ )) {
                        &usage_abort;
                    } else {              # here parse seed
                        $debugcmd && print "2nd arg for S is <$args[$i+1]>\n";
                        $seed = $args[$i+1];
                        $i+=2;
                    }
                } elsif ($c =~ /T/ ) {
                    if( ($i+1 >= $cmdlineflag) || ($args[$i+1] =~ /^-/ )) {
                        &usage_abort;
                    } else {              # here parse bit length
                        $debugcmd && print "2nd arg for T is <$args[$i+1]>\n";
                        $TYPE = $args[$i+1];
                        $i+=2;
                    }
                } elsif ($c =~ /U/ ) {
                    $debugcmd && print "NO DUPS\n";
                    $UNIQUESONLY = 1;
                    $i++;
                } else {
                    &usage_abort;
                }
            } else {
                &usage_abort;
            }
        } # end while
    return;
}


sub seedrand {
    my $tmp = $_[0];
    if (!$tmp) {
	$seed = (time ^ $$ ^ unpack "%L*", `ps axwwww | gzip`);
	} 
    $debug &&    print "seed is <$seed>\n";
    srand($seed);
}


sub create_randREC {
    my @rec;
    my $j = 0;

    while ($j < $len) {
	my $c = int(rand(2));
	$rec[$j] = $c;
	$j++;
    }
    
    return @rec;
}


sub print_rec {
    my @rec = @_;
    my $j = 0;

    $j = 0;
    while ($j < $len) {
#	my $s = sprintf("%b",$rec[$j]);
        my $s = $rec[$j];
	print OUTDB "$s";
	$j++;
    }
    print OUTDB "\n";

    return;
}


sub print_rec_join {
    my @rec = @_;
    my $s = join("",@rec);
    print OUTDB "$s\n";
    return;
}


sub create_randDB{
    my $i = 0;
    my @REC = ();
    my %hashdb = ();

    while ($i < $n) {
	@REC = &create_randREC();
        if($UNIQUESONLY){
            my $r = join("",@REC);
            $debug && print "joined r is <$r>\n";
            if(! defined $hashdb{$r}){
                $hashdb{$r}=1;
                print OUTDB "$r\n";
                $i++;
            } else {
                $debug && print "dup found\n";
            }
        } else {
            &print_rec_join(@REC);
            $i++;
        }
    }
    return;
}


sub create_relDB(){
    my $i = 0;
    my @r = create_randREC();
    my %hashdb = ();

    while ($i < $n) {	
	my @b = create_randREC();
	my @d = create_randREC();
	my @rec = ();
	my $j = 0;

	while ($j < $len){
	    $rec[$j] = $r[$j] ^ ($b[$j] & $d[$j]);
	    $j++;
	}

        if($UNIQUESONLY){
            my $r = join("",@rec);
            print "joined r is <$r>\n";
            if(! defined $hashdb{$r}){
                $hashdb{$r}=1;
                print OUTDB "$r\n";
                $i++;
            } else {
                $debug && print "dup found\n";
            }
        } else {
            &print_rec_join(@rec);
            $i++;
        }
    }

    return;
}


sub create_selrandwords {
    my $n2 = 0;
    my @PICKS;
    my $i;
    my $wfile = "$configfilepath/words.txt";

    stat($wfile);
    if ( ! -e _ ) {
        die "genDB4: $wfile required for type $TYPE option\n";
    }

    open INWORDS,$wfile || die "genDB4: unable to open $wfile\n";
    while (<INWORDS>){
	if ( /^([a-z]{1,$len})$/) {
	    my $s = $1;
	    $debug && print "found a word: <$s>\n";
	    my $r = int(rand($n2));  # pick a random number
	    if ($r < $n) {
		if ($n2 < $n) {
		    $PICKS[$n2] = $s;
		} else {
		    my $r2 = int(rand($n-1));
		    $PICKS[$r2] = $s;
		}
	    }
	    $n2++;
	}
    }
    close INWORDS;

    for ($i = 0; $i < $n ; $i++) {
	print OUTDB "$PICKS[$i]\n";
    }
}


sub create_DB {

    if($UNIQUESONLY && ($n > 2**$len)){
        die "genDB4: impossible to create $n unique records of length $len\n";
    }

    open OUTDB, "> $configfilepath/DB.txt";

#    print "perl genDB4.pl -l $len -n $n -t $TYPE -s $seed\n";

    if ($TYPE == 1) {
	&create_randDB();
	print "Random Binary DB: ";
    } elsif ($TYPE == 2) {
	&create_relDB();
	print "Related Random Binary DB: ";
    } elsif ($TYPE == 3) {
	&create_selrandwords();
	print "Randomly Selected Words DB: ";
    }

    print "record length $len, size $n, seed $seed\n";
    write_seed("$seed");
    close OUTDB;
    return;
}


sub PrintBR {
    if(!$WEBDEMO)
    {
	print "\n";
    }
    else
    {
	print "<BR>";
    }
    return;
}


sub main {
    &command_args(@_);
    &seedrand($seed);
    &create_DB();
}

sub write_seed
{
        my $result = $_[0];

	open  (MYFILE,"> $configfilepath/seed.txt") || die("Cannot Open File to print result");
	print MYFILE $result;
	close (MYFILE); 

}


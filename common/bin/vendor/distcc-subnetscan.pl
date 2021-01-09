#!/usr/bin/perl
# $Header: /home/cvsroot/distcc/distcc-subnetscan.pl,v 1.8 2003/07/24 18:49:23 lisa Exp $
# Author Info: Lisa Seelye <lisa@gentoo.org>
# Purpose:  This script is designed to detect distccd servers on any given 
# netblock.
# Program info:  This script is admittedly a hack job.  It can scan a block of
# 256 IPs in about 25 seconds on a 100mbit network.
#
# Tere is a ton of room for expansion and growth in this script.  Feel free to
# add or modify this script.  Email patches to me, if you don't mind. :)
#
# For usage, use perl distcc-subnetscan.pl --help
#
# This source is released into the public domain.
# Depends on:
# Getopt::Long (dev-perl/Getopt-Long)
# IO::Socket
# Net::Netmask (dev-perl/Net-Netmask)
#Important Note:  This script will not link files and it will not use ccache.

# TODO:  There has to be a better way to get the IP of this box
# TODO:  Flag to discard unlinked files, or to link them??
# TODO:  Let the user specify their own CHOST for cross compiler testing
# TODO:  Flag to decide what network device to use? Or a better way to get the \
#         IP of `this' computer (IO::Interface ???)
# TODO:  Flag to let the user specify their own source file
# TODO:  Let the user specify a compiler
# TODO:  Do something with $remote in checkHost so interpreter don't complain \
#         with -w or `use strict'

use IO::Socket;     # To get the IP of `this' box
use Net::Netmask;   # Matching and managing the list of IPs to netmasks
use Getopt::Long;   # To parse options


my $skipmyip = 0;  # 0 = do not skip, 1 = skip.  Set via --exclude-self.
my $netrange = ""; # Not set means it will do your block, ie 192.168.0.0/24
				   # Use typical netmasks
my $port = 3632;   # Default port. Overwrite with --port [n]
my $outfile = "";  # Default place to send data.  Overwrite with --outfile [path]
my $showHelp = 0;

# Prototype subroutines
sub printUsage();
sub checkHost;
sub getMyIP();
sub buildIPList();
sub writeCSourceFile();

GetOptions ('exclude-self' => \$skipmyip,
			'help' => \$showHelp,
			'port=s' => \$port,
			'net=s' => \$netrange,
			'outfile=s' => \$outfile,
			);
printUsage() if $showHelp;

# TODO:  Move this stuff into buildIPList, mangle $block to fit
my $ip = getMyIP();
my @segments = split /\./, $ip;
my $ip2 = $segments[0] . "." . $segments[1] . "." . $segments[2] . ".";
$netrange = $ip2 . "0/24" if !$netrange;   #user didnt specify with --net
my $block = new Net::Netmask($netrange);


my @goodHosts; #List of good hosts


sub printUsage() {
	print <<"USAGE";
distcc-subnetscan.pl's Usage --- lisa\@gentoo.org

distcc-subnetscan.pl [--exclude-self] [--port [N]] --net [aaa.bbb.ccc.ddd/N]
                     [--outfile N]

distcc-subnetscan.pl [--help]

--help  Show this usage
--exclude-self : Exclude your IP from the list of hosts to check
--port [N] : Use a non-standard port to check against. Defaults to port 3632
--net [aaa.bbb.ccc.ddd/N] : Use a netmask to scan.  Defaults to your block, eg 192.168.0.0\/24
--outfile [N] : Send program output to a file.  Defaults to STDOUT
USAGE
exit(0);
}


sub checkHost {
	my $host = shift @_;
	$remote = IO::Socket::INET->new(
		Proto    => "tcp",
		PeerAddr => $host,
		PeerPort => "$port",
		Timeout  => ".10",
    ) || return 0; #false  Timeout of .10 seconds
	return 1;
}
sub getMyIP() {
	#return the ip of `this' computer
	local @f = gethostbyname($ENV{HOSTNAME});
	local @addrs = $f[4];
	($a, $b, $c, $d) = unpack("C4",$addrs[0]);
	$ip = "$a.$b.$c.$d";
	return $ip;
}

sub buildIPList() {
	my @iplist;
	my $j = 0;   #index into good list of ips
	for $blockip ($block->enumerate()) {
		#next two lines:  Skip broadcast stuff
		@dotted_list = split /\./, $ip;
		next if (($dotted_list[3] == "0") || ($dotted_list[3] == "255"));

		if ($ip eq $blockip && $skipmyip) {
		 #Skip my host
			next;
		}
		if (! checkHost($blockip)) {
			#it isn't listening on $port, so skip this host
			next;
		}
		$iplist[$j++] = $blockip;
	}
	return @iplist
}
sub writeCSourceFile() {
	open(FILEHAND,">ctest.c") || die "Could not open file: $!\n";
	print FILEHAND <<"CSRC";
#include <stdio.h>
int main()
{
    int x;
    x = 3;
    float n;
    n = x * 30.21f;
    printf(\"Result: %f\",n);
    return 0;
}
CSRC
	close(FILEHAND);
}


################################################################################
###                           Program Space                                  ###
################################################################################

writeCSourceFile();
foreach $ip (buildIPList()) {
	$command = sprintf("CCACHE_DISABLE='1' DISTCC_HOSTS='%s/3' DISTCC_FALLBACK='0' distcc -c ctest.c -o ctest-%s \&>//dev//null",$ip,$ip);
	#print "Passing command: $command\n";
	if (system($command) == 0) {
		#good host
		$goodHosts[$#goodHosts + 1] = $ip;
	}
}
if ($outfile) {
	open(FHAND,">$outfile");
	foreach $ip (@goodHosts) {
		print FHAND "$ip ";
	}
	close(FHAND);
}
else {
	foreach $ip (@goodHosts) {
		print "$ip ";
	}
	print "\n";
}

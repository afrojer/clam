#!/usr/bin/perl
use strict;

open(GITSL, "git shortlog -n -e |") or die "Can't execute git shortlog: $!";
my $user = "";
my $num;
my $cdata;
print "\\lstset{ language=make}\n";
print "\\begin{lstlisting}\n";
while (<GITSL>) {
	if (/^([a-zA-Z].*<.*>)\s*\((\d+)\):\s*$/) {
		if (!($user eq "")) { print "$user ($num):\n$cdata\n"; }
		$user=$1; $num=$2; $cdata="";
		next;
	}
	if (/^\s*[mM]erged* branch .*/) {
		$num -= 1;
		next;
	}
	$cdata .= $_;
}

print "$user ($num):\n$cdata\n";
print "\\end{lstlisting}\n";

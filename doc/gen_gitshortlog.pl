#!/usr/bin/perl
use strict;

open(GITSL, "git shortlog -n -e |") or die "Can't execute git shortlog: $!";
my $user = "";
my $num;
my $cdata;
print "\\section{Project Repository \\texttt{git} 'shortlog'}\n";
print "\\lstset{numbers=none,frame=none}\n";
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
close GITSL;
print "$user ($num):\n$cdata\n";
print "\\end{lstlisting}\n\n";

my $gitlogcmd = "git log --color --stat --no-merges --pretty=format:\"\%h: \%Cblue\%aN <\%aE>\%Creset\%nDate: \%aD\%nSubject: \%s\%nContent: \%b\"";
print "\\clearpage\n\\section{Full \\texttt{git} Log}\n";
print "The following log was generated using the command:\n";
#print "\\lstset{language=make}\n";
print "\\begin{lstlisting}\n";
print $gitlogcmd . "\n";
print "\\end{lstlisting}\n";

print "\\lstset{escapeinside=`'}\n";
print "\\begin{lstlisting}\n";
open(GITLOG, "$gitlogcmd |") or die "$!";
my $curcolor = "none";
while (<GITLOG>) {
	my $line = $_;
	my $ctrlChar = "\\033\\[([0-9]*m)";
	my $ctrlSub = 0;
	while ($line =~ /$ctrlChar/) {
		my $newcolor = "";
		my $colorstr = "";
		if ($1 eq "34m") { $newcolor = "blue"; $colorstr = "`{\\color{blue}"; }
		elsif ($1 eq "32m") { $newcolor = "green";  $colorstr = "`{\\color{green}"; }
		elsif ($1 eq "31m") { $newcolor = "red"; $colorstr = "`{\\color{red}"; }
		elsif ($1 eq "m") { $colorstr = "'"; $newcolor = "none" }
		if ( !("$curcolor" eq "none") and !($newcolor eq $curcolor) ) {
			$colorstr = "}$colorstr"; }
		$curcolor = $newcolor;
		$line =~ s/$ctrlChar/$colorstr/;
		$ctrlSub++;
	}
	# force the end of the color
	if (!("$curcolor" eq "none")) { $curcolor = "none"; $line =~ s/([^\r\n])[\r\n]*$/$1"}'"\n/; }
# Don't need it right now...
#	if ($ctrlSub != 0 ) {
#		my $safeline = $line;
#		while ($safeline =~ /`{\\color{[\w]+}(.*?)}'/) {
#			my $esc = $1;
#			my $start = index($line, $esc);
#			my $end = $start + length($esc) - 2;
#			(my $safeTeX = $esc) =~ s/([#{}&%_])/\\$1/g;
#			#$line =~ s/`{$esc}'/`{$safeTeX}'/;
#			$esc = substr($line, $start, $end, $safeTeX);
#			$safeline =~ s/`{(.*?)}'//;
#		}
#	}
	print $line;
}
print "\\end{lstlisting}\n";

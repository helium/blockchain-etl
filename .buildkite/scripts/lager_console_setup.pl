#!/usr/bin/env perl

$done = 0;
while (<>) {

	if (/lager_file_backend/ and not $done) {
		$done = 1;
		print "     {lager_console_backend, [{level, debug}]}\n";
	}
	elsif (/lager_file_backend/ and $done) {
	}
	elsif (/colored/) {
		print "   {colored, false},\n";
	}
	else {
		print "$_";
	}
}

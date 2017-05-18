#! /usr/bin/env perl
################################################################################
# Created: Wednesday, April 27 2011

package Slurp;

use Exporter;
@ISA    = qw(Exporter);
@EXPORT = qw(slurp trim);

use strict;
use diagnostics;

sub slurp {
    my $file = shift;
    die "No such file" unless -f $file;
    open IN,$file;
    if (wantarray) {
	my @content = <IN>;
	close IN;
	return @content;
    } else {
	my $old = $/;
	undef $/;
	my $content = <IN>;
	close IN;
	$/ = $old;
	return $content;
    }
}

sub trim {
    $_ = shift;
    s/^\s+//mg; # leading space
    s/\s+$//mg; # trailing space
    s/\s+/ /g;  # multiple space
    return $_;
}

# End of file
1;
################################################################################
# Local Variables:
# comment-column: 60
# End:
################################################################################

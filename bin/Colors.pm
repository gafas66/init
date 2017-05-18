#!/usr/bin/perl
################################################################################
# Created: Monday, June 21 2010

package Colors;

use Exporter;
@ISA    = qw(Exporter);
@EXPORT = qw(red green yellow blue magenta cyan);

use strict;
use diagnostics;

my ($red,$green,$yellow,$blue,$magenta,$cyan,$white) = (31,32,33,34,35,36,37);

my $end = "[0m";

sub red     {"[0;$red;40m@_$end"}
sub green   {"[0;$green;40m@_$end"}
sub yellow  {"[0;$yellow;40m@_$end"}
sub blue    {"[0;$blue;40m@_$end"}
sub magenta {"[0;$magenta;40m@_$end"}
sub cyan    {"[0;$cyan;40m@_$end"}

#
1;
# End of file
################################################################################
# Local Variables:
# comment-column: 60
# End:
################################################################################

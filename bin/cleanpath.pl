#!/usr/bin/perl
################################################################################
# Taken from Martyn

my @PATHS = grep(/(PATH|LICENSE_FILE|LIC_FILE)/,keys %ENV);

# Keep PATH twice (at end) so high/low,path matches this
my %path;
foreach $p (@PATHS,"PATH") {

  %path = ();
  my ($high, $low) = (0,0);
  for (split /:/, $ENV{$p} ) {
    next if exists $path{$_};   # remove duplicates
    $path{$_} = $high;
    ++$high;
  }
  # Will print to screen so need to make setenv PATH externally.
  print "setenv $p ",join ":", sort {$path{$a} <=> $path{$b} }  keys %path unless ($p eq "PATH");
  print "\n";
}

# Simple command line args so you can:
# remove entries:       e.g -/usr/bin
# add entries at end:   e.g +/home/mbrown/myNewPathAtEnd
# add entries at start: e.g ++/home/mbrown/myNewPathAtStart
for (@ARGV){
  /^-(.*)/    and do {delete   $path{$1}           if exists $path{$1}; next;};
  /^\+\+(.*)/ and do {--$low;  $path{$1}=$low  unless exists $path{$1}; next;};
  /^\+(.*)/   and do {++$high; $path{$1}=$high unless exists $path{$1}; next;};
}

print "setenv PATH ",join ":", sort {$path{$a} <=> $path{$b} } keys %path;
print "\n";

# End
################################################################################

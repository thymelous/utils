#!/usr/bin/env perl

use strict;
use IPC::Open2;
use IPC::Open3;
use List::MoreUtils q{natatime};

my $srcpath = shift or die "Usage: $0 source\n";
open my $src, '<', $srcpath or die "Can't open $srcpath $!\n";

open2(\*PP_OUT, \*PP_IN, 'gcc -E -') or die "Failed to start gcc: $!\n";

while (<$src>) {
  chomp;
  my $line = /^[^#\n]\s*\S/ ? "$_ #__LINE__#\n" : "$_\n";
  print PP_IN $line;
}
close PP_IN;

open3(\*CC_IN, \*CC_OUT, \*CC_ERR, 'clang -x c -') or die "Failed to start clang: $!\n";

my @linemap;
my $line = 0;
while (<PP_OUT>) {
  next if /^#/;
  $line += 1;

  if (/(.+)#(\d+)#$/) {
    push @linemap, $line;
    push @linemap, $2;
    print CC_IN "$1\n";
  }
  else {
    print CC_IN "$_";
  }
}
close CC_IN;

while (<CC_ERR>) {
  if (/^<stdin>:(\d+):(.+)$/) {
    my ($line, $origline) = (1, 1);
    my $it = natatime 2, @linemap;
    while (my ($l, $ol) = $it->()) {
      last if $l > $1;
      $line = $l;
      $origline = $ol;
    }
    print STDERR "$srcpath:$origline:$2\n";
  }
  else {
    print STDERR;
  }
}


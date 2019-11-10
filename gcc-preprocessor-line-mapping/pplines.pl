#!/usr/bin/env perl

use strict;
use IPC::Open2;
use IPC::Open3;

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

while (<PP_OUT>) {
  if (/(.+)#(\d+)#$/) {
    print CC_IN "$1\n";
  }
  else {
    print CC_IN "$_\n";
  }
}
close CC_IN;

while (<CC_ERR>) {
  print;
}

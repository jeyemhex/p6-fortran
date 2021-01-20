#!/usr/bin/env perl6
use lib 'lib';
use Fortran::Grammar::F2018;
#EJH# use trace;
use Grammar::Debugger;

sub MAIN (Str $expr) {
    say "a = " ~ $expr ~ "\nend";
    say "-" x 80;
    say Fortran2018.parse("a = " ~ $expr ~ "\nend\n") or die "didn't work";
}


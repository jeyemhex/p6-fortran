#!/usr/bin/env perl6
use lib 'lib';
use Fortran::Grammar::F2018;
#EJH# use trace;
use Grammar::Debugger;

sub MAIN ($file) {
    my $fortran = $file.IO.slurp;
    $fortran .= subst(/\!.*?$$/, '', :g);
    $fortran .= subst(/^^\h*$$\n/, '', :g);
    say $fortran;
    say "-" x 80;
    say Fortran2018.parse($fortran) or die "didn't work";
}


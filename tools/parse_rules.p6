#!/usr/bin/env perl6

grammar Standard {
    token TOP                   { [<syntax-rule> | <syntax-constraint>]+ % "\n" }

    token syntax-rule           { <rule-id> <ws> <rule-name> <ws> 
                                    is <ws> <rule-value-list>+ % [<ws> "|" <ws>] }

    token syntax-constraint     { <constraint-id> <ws> <constraint-text> }

    token rule-value-list       { <rule-value>+ % <ws> }

    token rule-value            { <repeated-rule-value> | <optional-rule-value> | <base-rule-value> }

    token base-rule-value       { <rule-token> }

    token optional-rule-value   { "["<ws> <rule-value-list> <ws> "]" }

    token repeated-rule-value   { <optional-rule-value> <ws> "..." }

    token rule-token            { <rule-name> | <literal> }

    token rule-id               { R\d+ }

    regex rule-name             { <lower> [<lower> | <digit> | "-"]* <lower> | <lower> }

    token constraint-id         { C\d+ }

    token constraint-text       { .* $$ }

    token literal               { <fortran-character>+ }

    token fortran-character     { <upper> 
                                | <digit> 
                                | <[ \_ \= \+ \- \* \/ \\ \( \) \{ \} \, \. \:
                                     \; \! \" \% \& \~ \< \> \? \' \` \^ \$ \# \@ ]> }
}

class Standard-Actions {
    method TOP($/) {
        if $<syntax-rule> {make $<syntax-rule>[0].made }
        else              {make $<syntax-constraint>[0].made }
    }

    method syntax-rule($/) {
        make { id      => $<rule-id>.Str,
               name    => $<rule-name>.Str,
               values  => $<rule-value-list>>>.made };
    }

    method syntax-constraint($/) {
        make { id   => $<constraint-id>.Str,
               text => $<constraint-text>.Str };
    }

    method rule-value-list($/) {
        make $<rule-value>>>.made;
    }

    method rule-value($/) {
        if    $<repeated-rule-value> { make $<repeated-rule-value>.made }
        elsif $<optional-rule-value> { make $<optional-rule-value>.made }
        else                         { make $<base-rule-value>.made     }
    }

    method base-rule-value($/) {
        make $<rule-token>.made;
    }

    method optional-rule-value($/) {
        make {  type => "optional", value => $<rule-value-list>.made };
    }

    method repeated-rule-value($/) {
        my $made = $<optional-rule-value>.made;
        $made<type> = "repeated";
        make $made;
    }

    method rule-token($/) {
        if $<rule-name> { make {type => "rule",    value => $<rule-name>.Str } }
        else            { make {type => "literal", value => $<literal>.Str   } }
    }
}

sub generate-rule($tree) {
    my $rule;

    if $tree<id>  ~~ /^R/ {
        $rule = "    #`[{ $tree<id> }]";
        $rule ~= " token $tree<name> \{\n";
        for $tree<values>.values -> $isa {
            $rule ~= "        |";
            for $isa.values -> $elem {
                $rule ~= " " ~ get-subrule($elem);
            }
            $rule ~= "\n"
        }
        $rule ~= "    \}";
    } else {
        $rule = "    #`[{ $tree<id> }: {$tree<text>}]";
    }

    return $rule;
}

sub get-subrule($isa) {
    my Str $subrule;
    given $isa<type> {
        when /repeated/ { 
            $subrule = " [ ";
            for $isa<value>.values -> $elem {
                $subrule ~= get-subrule($elem);
            }
            $subrule ~= " ]*";
        }

        when /optional/ {
            $subrule = " [ ";
            for $isa<value>.values -> $elem {
                $subrule ~= get-subrule($elem);
            }
            $subrule ~= " ]?";
        }

        when /rule/     {
            $subrule = ' $<' ~ $isa<value> ~ '>';
        }

        when /literal/  {
            $subrule = ' "'  ~ $isa<value> ~ '"';
        }
    }
    return $subrule;
}

#| A program to parse a Fortran standard to a perl6 grammar
sub MAIN($std_file) {
    my Bool %generated;
    my @std = $std_file.IO.lines;

    say 'grammar Fortran2018 {';

    for @std -> $line {
        my $tree = Standard.parse($line, actions => Standard-Actions).made
            or fail "Unable to parse rule:\n  $line";
#EJH#         say $tree.gist;
        if not %generated{$tree<id>} {
            say generate-rule($tree) ~ "\n";
            %generated{$tree<id>} = True;
        }
    }
    say '}';
}


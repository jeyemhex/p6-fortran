#!/usr/bin/env perl6

sub denumify(Str $str_in) {
    my $string = $str_in;
    if ($str_in ~~ /\d/) {
        $string .= subst(/0/, "zero", :g);
        $string .= subst(/1/, "one", :g);
        $string .= subst(/2/, "two", :g);
        $string .= subst(/3/, "three", :g);
        $string .= subst(/4/, "four", :g);
        $string .= subst(/5/, "five", :g);
        $string .= subst(/6/, "six", :g);
        $string .= subst(/7/, "seven", :g);
        $string .= subst(/8/, "eight", :g);
        $string .= subst(/9/, "nine", :g);
    }
    return $string;
}

# Grammar defining the subset of the standards in ./rules/*.rules
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

# Actions required to compile syntax rules into a easier-to-handle tree
class Standard-Actions {
    method TOP($/) {
        if $<syntax-rule> {make $<syntax-rule>[0].made }
        else              {make $<syntax-constraint>[0].made }
    }

    method syntax-rule($/) {
        make { id      => $<rule-id>.Str,
               name    => denumify( $<rule-name>.Str ),
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
        if $<rule-name> { make {type => "rule",    value => denumify( $<rule-name>.Str ) } }
        else            { make {type => "literal", value => $<literal>.Str   } }
    }
}


# Generate a perl6 grammar string from a compiled rule tree
sub generate-rule($tree) {
    my $rule;

    # If the tree is a rule...
    if $tree<id>  ~~ /^R/ {

        # Add an inline reference to the rule in the standard
        $rule = "    #`[{ $tree<id> }]";

        $rule ~= " rule $tree<name> \{:i [\n";

        for $tree<values>.values -> $isa {
            $rule ~= "        |";
            for $isa.values -> $elem {
                $rule ~= " " ~ get-subrule($elem);
            }
            $rule ~= "\n"
        }

        if ($tree<name> ~~ / "-stmt" $/) { $rule ~= "    \] <.stmt-separator> \}" }
        else { $rule ~= "    \] \}" }

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
            given $isa<value> {
                when / "-name" $/           { $subrule = ' <name>'          }
                when /(.*?) "-list" $/      { $subrule = " <$0> [ <$0> ]*"  }
                when /^ "scalar-" (.*) $/   { $subrule = " <$0>"            }
                default                     { $subrule = " <$isa<value>>"   }
            }
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
    my Str @std = $std_file.IO.lines;

    say 'grammar Fortran2018 {';

    say '    rule TOP { <program> }';
    say '    token ws { \h* }';
    say '    token stmt-separator { [ "\n"+ | ";" \n* ] }';
    say '    token letter { <[A..Z a..z]> }';

    for @std -> $line {
        my $tree = Standard.parse($line, actions => Standard-Actions).made
            or fail "Unable to parse rule:\n  $line";

        unless defined $tree<name> {$*ERR.say($line)}

        unless defined %generated{$tree<name>} {
            say generate-rule($tree) ~ "\n";
            %generated{$tree<name>} = True;
        }
    }
    say '}';
}


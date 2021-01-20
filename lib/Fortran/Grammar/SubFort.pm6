use Grammar::Debugger;

grammar Fortran2018 {
    rule TOP { <program> }
    token ws { \h* }
    token stmt-separator { [ "\n"+ | ";" \n* ] }
    token letter { <[A..Z a..z]> }

    #`[R501] rule program { [
        |  <program-unit>  [  <program-unit> ]*
    ] }

    #`[R502] rule program-unit { [
        |  <main-program>
    ] }

    #`[R509] rule execution-part { [
        |  <executable-construct>
    ] }

    #`[R514] rule executable-construct { [
        |  <action-stmt>
    ] }

    #`[R515] rule action-stmt { [
        |  <assignment-stmt>
    ] }

    #`[R601] token alphanumeric-character { [
        |  <letter>
        |  <digit>
        |  <underscore>
    ] }

    #`[R602] rule underscore { [
        |  "_"
    ] }

    #`[R603] token name { [
        |  <letter>  [  <alphanumeric-character> ]*
    ] }

    #`[R605] rule literal-constant { [
        |  <int-literal-constant>
    ] }

    #`[R708] rule int-literal-constant { [
        |  <digit-string>  [  "_" <kind-param> ]?
    ] }

    #`[R709] rule kind-param { [
        |  <digit-string>
        |  <name>
    ] }

    #`[R711] rule digit-string { [
        |  <digit>  [  <digit> ]*
    ] }

    #`[R901] rule designator { [
        |  <name>
    ] }

    #`[R902] rule variable { [
        |  <designator>
    ] }

    #`[R1001] rule primary { [
        |  <literal-constant>
        |  <name>
        |  "("  <expr>  ")"
    ] }

    #`[R1002] rule level-one-expr { [
        |  [  <defined-unary-op> ]?  <primary>
    ] }

    #`[R1003] token defined-unary-op { [
        |  "."  <letter>  [  <letter> ]*  "."
    ] }

    #`[R1004] rule mult-operand { [
        |  <level-one-expr>  [  <power-op> <mult-operand> ]?
    ] }

    #`[R1005] rule add-operand { [
        |  [  <add-operand> <mult-op> ]?  <mult-operand>
    ] }

    #`[R1006] rule level-two-expr {
        [  [  <level-two-expr> ]? <add-op> ]?  <add-operand>
    }

    #`[R1007] token power-op { [
        |  "**"
    ] }

    #`[R1008] token mult-op { [
        |  "*"
        |  "/"
    ] }

    #`[R1009] token add-op { [
        |  "+"
        |  "-"
    ] }

    #`[R1022] rule expr { [
        |  <level-two-expr>
    ] }

    #`[R1032] rule assignment-stmt { [
        |  <variable>  "="  <expr>
    ] <stmt-separator> }

    #`[R1401] rule main-program { [
        |  [  <program-stmt> ]? [ <execution-part> ]? <end-program-stmt>
    ] }

    #`[R1402] rule program-stmt {:i [
        |  "PROGRAM"  <name>
    ] <stmt-separator> }

    #`[R1403] rule end-program-stmt {:i [
        |  "END"  [  "PROGRAM" [  <name> ]? ]?
    ] <stmt-separator> }

}

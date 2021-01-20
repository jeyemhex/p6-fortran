grammar Fortran2018 {
    rule TOP { <program> }
    token ws { \h* }
    token stmt-separator { [ "\n"+ | ";" \n* ] }
    token letter { <[A..Z a..z]> }
    #`[R401] rule xyz-list {:i [
        |  <xyz>  [  "," <xyz> ]*
    ] }

    #`[R402] rule xyz-name {:i [
        |  <name>
    ] }

    #`[R403] rule scalar-xyz {:i [
        |  <xyz>
    ] }

    #`[C401: (R403) scalar-xyz shall be scalar.]

    #`[R501] rule program {:i [
        |  <program-unit>  [  <program-unit> ]*
    ] }

    #`[R502] rule program-unit {:i [
        |  <main-program>
        |  <external-subprogram>
        |  <module>
        |  <submodule>
        |  <block-data>
    ] }

    #`[R1401] rule main-program {:i [
        |  [  <program-stmt> ]?  [  <specification-part> ]?  [  <execution-part> ]?  [  <internal-subprogram-part> ]?  <end-program-stmt>
    ] }

    #`[R503] rule external-subprogram {:i [
        |  <function-subprogram>
        |  <subroutine-subprogram>
    ] }

    #`[R1529] rule function-subprogram {:i [
        |  <function-stmt>  [  <specification-part> ]?  [  <execution-part> ]?  [  <internal-subprogram-part> ]?  <end-function-stmt>
    ] }

    #`[R1534] rule subroutine-subprogram {:i [
        |  <subroutine-stmt>  [  <specification-part> ]?  [  <execution-part> ]?  [  <internal-subprogram-part> ]?  <end-subroutine-stmt>
    ] }

    #`[R1404] rule module {:i [
        |  <module-stmt>  [  <specification-part> ]?  [  <module-subprogram-part> ]?  <end-module-stmt>
    ] }

    #`[R1416] rule submodule {:i [
        |  <submodule-stmt>  [  <specification-part> ]?  [  <module-subprogram-part> ]?  <end-submodule-stmt>
    ] }

    #`[R1420] rule block-data {:i [
        |  <block-data-stmt>  [  <specification-part> ]?  <end-block-data-stmt>
    ] }

    #`[R504] rule specification-part {:i [
        |  [  <use-stmt> ]*  [  <import-stmt> ]*  [  <implicit-part> ]?  [  <declaration-construct> ]*
    ] }

    #`[R505] rule implicit-part {:i [
        |  [  <implicit-part-stmt> ]*  <implicit-stmt>
    ] }

    #`[R506] rule implicit-part-stmt {:i [
        |  <implicit-stmt>
        |  <parameter-stmt>
        |  <format-stmt>
        |  <entry-stmt>
    ] <.stmt-separator> }

    #`[R507] rule declaration-construct {:i [
        |  <specification-construct>
        |  <data-stmt>
        |  <format-stmt>
        |  <entry-stmt>
        |  <stmt-function-stmt>
    ] }

    #`[R508] rule specification-construct {:i [
        |  <derived-type-def>
        |  <enum-def>
        |  <generic-stmt>
        |  <interface-block>
        |  <parameter-stmt>
        |  <procedure-declaration-stmt>
        |  <other-specification-stmt>
        |  <type-declaration-stmt>
    ] }

    #`[R509] rule execution-part {:i [
        |  <executable-construct>  [  <execution-part-construct> ]*
    ] }

    #`[R510] rule execution-part-construct {:i [
        |  <executable-construct>
        |  <format-stmt>
        |  <entry-stmt>
        |  <data-stmt>
    ] }

    #`[R511] rule internal-subprogram-part {:i [
        |  <contains-stmt>  [  <internal-subprogram> ]*
    ] }

    #`[R512] rule internal-subprogram {:i [
        |  <function-subprogram>
        |  <subroutine-subprogram>
    ] }

    #`[R1407] rule module-subprogram-part {:i [
        |  <contains-stmt>  [  <module-subprogram> ]*
    ] }

    #`[R1408] rule module-subprogram {:i [
        |  <function-subprogram>
        |  <subroutine-subprogram>
        |  <separate-module-subprogram>
    ] }

    #`[R1538] rule separate-module-subprogram {:i [
        |  <mp-subprogram-stmt>  [  <specification-part> ]?  [  <execution-part> ]?  [  <internal-subprogram-part> ]?  <end-mp-subprogram-stmt>
    ] }

    #`[R513] rule other-specification-stmt {:i [
        |  <access-stmt>
        |  <allocatable-stmt>
        |  <asynchronous-stmt>
        |  <bind-stmt>
        |  <codimension-stmt>
        |  <contiguous-stmt>
        |  <dimension-stmt>
        |  <external-stmt>
        |  <intent-stmt>
        |  <intrinsic-stmt>
        |  <namelist-stmt>
        |  <optional-stmt>
        |  <pointer-stmt>
        |  <protected-stmt>
        |  <save-stmt>
        |  <target-stmt>
        |  <volatile-stmt>
        |  <value-stmt>
        |  <common-stmt>
        |  <equivalence-stmt>
    ] <.stmt-separator> }

    #`[R514] rule executable-construct {:i [
        |  <action-stmt>
        |  <associate-construct>
        |  <block-construct>
        |  <case-construct>
        |  <change-team-construct>
        |  <critical-construct>
        |  <do-construct>
        |  <if-construct>
        |  <select-rank-construct>
        |  <select-type-construct>
        |  <where-construct>
        |  <forall-construct>
    ] }

    #`[R515] rule action-stmt {:i [
        |  <allocate-stmt>
        |  <assignment-stmt>
        |  <backspace-stmt>
        |  <call-stmt>
        |  <close-stmt>
        |  <continue-stmt>
        |  <cycle-stmt>
        |  <deallocate-stmt>
        |  <endfile-stmt>
        |  <error-stop-stmt>
        |  <event-post-stmt>
        |  <event-wait-stmt>
        |  <exit-stmt>
        |  <fail-image-stmt>
        |  <flush-stmt>
        |  <form-team-stmt>
        |  <goto-stmt>
        |  <if-stmt>
        |  <inquire-stmt>
        |  <lock-stmt>
        |  <nullify-stmt>
        |  <open-stmt>
        |  <pointer-assignment-stmt>
        |  <print-stmt>
        |  <read-stmt>
        |  <return-stmt>
        |  <rewind-stmt>
        |  <stop-stmt>
        |  <sync-all-stmt>
        |  <sync-images-stmt>
        |  <sync-memory-stmt>
        |  <sync-team-stmt>
        |  <unlock-stmt>
        |  <wait-stmt>
        |  <where-stmt>
        |  <write-stmt>
        |  <computed-goto-stmt>
        |  <forall-stmt>
    ] <.stmt-separator> }

    #`[R516] rule keyword {:i [
        |  <name>
    ] }

    #`[R601] rule alphanumeric-character {:i [
        |  <letter>
        |  <digit>
        |  <underscore>
    ] }

    #`[R602] rule underscore {:i [
        |  "_"
    ] }

    #`[R603] rule name {:i [
        |  <letter>  [  <alphanumeric-character> ]*
    ] }

    #`[R604] rule constant {:i [
        |  <literal-constant>
        |  <named-constant>
    ] }

    #`[R605] rule literal-constant {:i [
        |  <int-literal-constant>
        |  <real-literal-constant>
        |  <complex-literal-constant>
        |  <logical-literal-constant>
        |  <char-literal-constant>
        |  <boz-literal-constant>
    ] }

    #`[R606] rule named-constant {:i [
        |  <name>
    ] }

    #`[R607] rule int-constant {:i [
        |  <constant>
    ] }

    #`[R608] rule intrinsic-operator {:i [
        |  <power-op>
        |  <mult-op>
        |  <add-op>
        |  <concat-op>
        |  <rel-op>
        |  <not-op>
        |  <and-op>
        |  <or-op>
        |  <equiv-op>
    ] }

    #`[R1007] rule power-op {:i [
        |  "**"
    ] }

    #`[R1008] rule mult-op {:i [
        |  "*"
        |  "/"
    ] }

    #`[R1009] rule add-op {:i [
        |  "+"
        |  "-"
    ] }

    #`[R1011] rule concat-op {:i [
        |  "//"
    ] }

    #`[R1013] rule rel-op {:i [
        |  ".EQ."
        |  ".NE."
        |  ".LT."
        |  ".LE."
        |  ".GT."
        |  ".GE."
        |  "=="
        |  "/="
        |  "<"
        |  "<="
        |  ">"
        |  ">="
    ] }

    #`[R1018] rule not-op {:i [
        |  ".NOT."
    ] }

    #`[R1019] rule and-op {:i [
        |  ".AND."
    ] }

    #`[R1020] rule or-op {:i [
        |  ".OR."
    ] }

    #`[R1021] rule equiv-op {:i [
        |  ".EQV."
        |  ".NEQV."
    ] }

    #`[R609] rule defined-operator {:i [
        |  <defined-unary-op>
        |  <defined-binary-op>
        |  <extended-intrinsic-op>
    ] }

    #`[R1003] rule defined-unary-op {:i [
        |  "."  <letter>  [  <letter> ]*  "."
    ] }

    #`[R1023] rule defined-binary-op {:i [
        |  "."  <letter>  [  <letter> ]*  "."
    ] }

    #`[R610] rule extended-intrinsic-op {:i [
        |  <intrinsic-operator>
    ] }

    #`[R611] rule label {:i [
        |  <digit>  [  <digit> [  <digit> [  <digit> [  <digit> ]? ]? ]? ]?
    ] }

    #`[R701] rule type-param-value {:i [
        |  <int-expr>
        |  "*"
        |  ":"
    ] }

    #`[R702] rule type-spec {:i [
        |  <intrinsic-type-spec>
        |  <derived-type-spec>
    ] }

    #`[R703] rule declaration-type-spec {:i [
        |  <intrinsic-type-spec>
        |  "TYPE"  "("  <intrinsic-type-spec>  ")"
        |  "TYPE"  "("  <derived-type-spec>  ")"
        |  "CLASS"  "("  <derived-type-spec>  ")"
        |  "CLASS"  "("  "*"  ")"
        |  "TYPE"  "("  "*"  ")"
    ] }

    #`[R704] rule intrinsic-type-spec {:i [
        |  <integer-type-spec>
        |  "REAL"  [  <kind-selector> ]?
        |  "DOUBLE"  "PRECISION"
        |  "COMPLEX"  [  <kind-selector> ]?
        |  "CHARACTER"  [  <char-selector> ]?
        |  "LOGICAL"  [  <kind-selector> ]?
    ] }

    #`[R705] rule integer-type-spec {:i [
        |  "INTEGER"  [  <kind-selector> ]?
    ] }

    #`[R706] rule kind-selector {:i [
        |  "("  [  "KIND" "=" ]?  <int-constant-expr>  ")"
    ] }

    #`[R707] rule signed-int-literal-constant {:i [
        |  [  <sign> ]?  <int-literal-constant>
    ] }

    #`[R708] rule int-literal-constant {:i [
        |  <digit-string>  [  "_" <kind-param> ]?
    ] }

    #`[R709] rule kind-param {:i [
        |  <digit-string>
        |  <name>
    ] }

    #`[R710] rule signed-digit-string {:i [
        |  [  <sign> ]?  <digit-string>
    ] }

    #`[R711] rule digit-string {:i [
        |  <digit>  [  <digit> ]*
    ] }

    #`[R712] rule sign {:i [
        |  "+"
        |  "-"
    ] }

    #`[R713] rule signed-real-literal-constant {:i [
        |  [  <sign> ]?  <real-literal-constant>
    ] }

    #`[R714] rule real-literal-constant {:i [
        |  <significand>  [  <exponent-letter> <exponent> ]?  [  "_" <kind-param> ]?
        |  <digit-string>  <exponent-letter>  <exponent>  [  "_" <kind-param> ]?
    ] }

    #`[R715] rule significand {:i [
        |  <digit-string>  "."  [  <digit-string> ]?
        |  "."  <digit-string>
    ] }

    #`[R716] rule exponent-letter {:i [
        |  "E"
        |  "D"
    ] }

    #`[R717] rule exponent {:i [
        |  <signed-digit-string>
    ] }

    #`[R718] rule complex-literal-constant {:i [
        |  "("  <real-part>  ","  <imag-part>  ")"
    ] }

    #`[R719] rule real-part {:i [
        |  <signed-int-literal-constant>
        |  <signed-real-literal-constant>
        |  <named-constant>
    ] }

    #`[R720] rule imag-part {:i [
        |  <signed-int-literal-constant>
        |  <signed-real-literal-constant>
        |  <named-constant>
    ] }

    #`[R721] rule char-selector {:i [
        |  <length-selector>
        |  "("  "LEN"  "="  <type-param-value>  ","  "KIND"  "="  <int-constant-expr>  ")"
        |  "("  <type-param-value>  ","  [  "KIND" "=" ]?  <int-constant-expr>  ")"
        |  "("  "KIND"  "="  <int-constant-expr>  [  "," "LEN" "=" <type-param-value> ]?  ")"
    ] }

    #`[R722] rule length-selector {:i [
        |  "("  [  "LEN" "=" ]?  <type-param-value>  ")"
        |  "*"  <char-length>  [  "," ]?
    ] }

    #`[R723] rule char-length {:i [
        |  "("  <type-param-value>  ")"
        |  <int-literal-constant>
    ] }

    #`[R724] rule char-literal-constant {:i [
        |  [  <kind-param> "_" ]?  "'"  [  <rep-char> ]*  "'"
        |  [  <kind-param> "_" ]?  """  [  <rep-char> ]*  """
    ] }

    #`[R725] rule logical-literal-constant {:i [
        |  ".TRUE."  [  "_" <kind-param> ]?
        |  ".FALSE."  [  "_" <kind-param> ]?
    ] }

    #`[R726] rule derived-type-def {:i [
        |  <derived-type-stmt>  [  <type-param-def-stmt> ]*  [  <private-or-sequence> ]*  [  <component-part> ]?  [  <type-bound-procedure-part> ]?  <end-type-stmt>
    ] }

    #`[R727] rule derived-type-stmt {:i [
        |  "TYPE"  [  [  "," <type-attr-spec> [ <type-attr-spec> ]* ]? "::" ]?  <name>  [  "(" <type-param-name> [ <type-param-name> ]* ")" ]?
    ] <.stmt-separator> }

    #`[R728] rule type-attr-spec {:i [
        |  "ABSTRACT"
        |  <access-spec>
        |  "BIND"  "(C)"
        |  "EXTENDS("  <name>  ")"
    ] }

    #`[R729] rule private-or-sequence {:i [
        |  <private-components-stmt>
        |  <sequence-stmt>
    ] }

    #`[R730] rule end-type-stmt {:i [
        |  "ENDTYPE"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R731] rule sequence-stmt {:i [
        |  "SEQUENCE"
    ] <.stmt-separator> }

    #`[R732] rule type-param-def-stmt {:i [
        |  <integer-type-spec>  ","  <type-param-attr-spec>  "::"  <type-param-decl> [ <type-param-decl> ]*
    ] <.stmt-separator> }

    #`[R733] rule type-param-decl {:i [
        |  <name>  [  "=" <int-constant-expr> ]?
    ] }

    #`[R734] rule type-param-attr-spec {:i [
        |  "KIND"
        |  "LEN"
    ] }

    #`[R735] rule component-part {:i [
        |  [  <component-def-stmt> ]*
    ] }

    #`[R736] rule component-def-stmt {:i [
        |  <data-component-def-stmt>
        |  <proc-component-def-stmt>
    ] <.stmt-separator> }

    #`[R737] rule data-component-def-stmt {:i [
        |  <declaration-type-spec>  [  [  "," <component-attr-spec> [ <component-attr-spec> ]* ]? "::" ]?  <component-decl> [ <component-decl> ]*
    ] <.stmt-separator> }

    #`[R738] rule component-attr-spec {:i [
        |  <access-spec>
        |  "ALLOCATABLE"
        |  "CODIMENSION"  <lbracket>  <coarray-spec>  <rbracket>
        |  "CONTIGUOUS"
        |  "DIMENSION"  "("  <component-array-spec>  ")"
        |  "POINTER"
    ] }

    #`[R739] rule component-decl {:i [
        |  <name>  [  "(" <component-array-spec> ")" ]?  [  <lbracket> <coarray-spec> <rbracket> ]?  [  "*" <char-length> ]?  [  <component-initialization> ]?
    ] }

    #`[R740] rule component-array-spec {:i [
        |  <explicit-shape-spec> [ <explicit-shape-spec> ]*
        |  <deferred-shape-spec> [ <deferred-shape-spec> ]*
    ] }

    #`[R741] rule proc-component-def-stmt {:i [
        |  "PROCEDURE("  [  <proc-interface> ]?  ")"  ","  <proc-component-attr-spec> [ <proc-component-attr-spec> ]*  "::"  <proc-decl> [ <proc-decl> ]*
    ] <.stmt-separator> }

    #`[R742] rule proc-component-attr-spec {:i [
        |  <access-spec>
        |  "NOPASS"
        |  "PASS"  [  "(" <name> ")" ]?
        |  "POINTER"
    ] }

    #`[R743] rule component-initialization {:i [
        |  "="  <constant-expr>
        |  "=>"  <null-init>
        |  "=>"  <initial-data-target>
    ] }

    #`[R744] rule initial-data-target {:i [
        |  <designator>
    ] }

    #`[R745] rule private-components-stmt {:i [
        |  "PRIVATE"
    ] <.stmt-separator> }

    #`[R746] rule type-bound-procedure-part {:i [
        |  <contains-stmt>  [  <binding-private-stmt> ]?  [  <type-bound-proc-binding> ]*
    ] }

    #`[R747] rule binding-private-stmt {:i [
        |  "PRIVATE"
    ] <.stmt-separator> }

    #`[R748] rule type-bound-proc-binding {:i [
        |  <type-bound-procedure-stmt>
        |  <type-bound-generic-stmt>
        |  <final-procedure-stmt>
    ] }

    #`[R749] rule type-bound-procedure-stmt {:i [
        |  "PROCEDURE"  [  [  "," <binding-attr> [ <binding-attr> ]* ]? "::" ]?  <type-bound-proc-decl> [ <type-bound-proc-decl> ]*
        |  "PROCEDURE("  <name>  "),"  <binding-attr> [ <binding-attr> ]*  "::"  <binding-name> [ <binding-name> ]*
    ] <.stmt-separator> }

    #`[R750] rule type-bound-proc-decl {:i [
        |  <name>  [  "=>" <name> ]?
    ] }

    #`[R751] rule type-bound-generic-stmt {:i [
        |  "GENERIC"  [  "," <access-spec> ]?  "::"  <generic-spec>  "=>"  <binding-name> [ <binding-name> ]*
    ] <.stmt-separator> }

    #`[R752] rule binding-attr {:i [
        |  <access-spec>
        |  "DEFERRED"
        |  "NON_OVERRIDABLE"
        |  "NOPASS"
        |  "PASS"  [  "(" <name> ")" ]?
    ] }

    #`[R753] rule final-procedure-stmt {:i [
        |  "FINAL"  [  "::" ]?  <final-subroutine-name> [ <final-subroutine-name> ]*
    ] <.stmt-separator> }

    #`[R754] rule derived-type-spec {:i [
        |  <name>  [  "(" <type-param-spec> [ <type-param-spec> ]* ")" ]?
    ] }

    #`[R755] rule type-param-spec {:i [
        |  [  <keyword> "=" ]?  <type-param-value>
    ] }

    #`[R756] rule structure-constructor {:i [
        |  <derived-type-spec>  "("  [  <component-spec> [ <component-spec> ]* ]?  ")"
    ] }

    #`[R757] rule component-spec {:i [
        |  [  <keyword> "=" ]?  <component-data-source>
    ] }

    #`[R758] rule component-data-source {:i [
        |  <expr>
        |  <data-target>
        |  <proc-target>
    ] }

    #`[R759] rule enum-def {:i [
        |  <enum-def-stmt>  <enumerator-def-stmt>  [  <enumerator-def-stmt> ]*  <end-enum-stmt>
    ] }

    #`[R760] rule enum-def-stmt {:i [
        |  "ENUM,BIND(C)"
    ] <.stmt-separator> }

    #`[R761] rule enumerator-def-stmt {:i [
        |  "ENUMERATOR"  [  "::" ]?  <enumerator> [ <enumerator> ]*
    ] <.stmt-separator> }

    #`[R762] rule enumerator {:i [
        |  <named-constant>  [  "=" <int-constant-expr> ]?
    ] }

    #`[R763] rule end-enum-stmt {:i [
        |  "ENDENUM"
    ] <.stmt-separator> }

    #`[R764] rule boz-literal-constant {:i [
        |  <binary-constant>
        |  <octal-constant>
        |  <hex-constant>
    ] }

    #`[R765] rule binary-constant {:i [
        |  "B'"  <digit>  [  <digit> ]*  "'"
        |  "B"  """  <digit>  [  <digit> ]*  """
    ] }

    #`[R766] rule octal-constant {:i [
        |  "O'"  <digit>  [  <digit> ]*  "'"
        |  "O"  """  <digit>  [  <digit> ]*  """
    ] }

    #`[R767] rule hex-constant {:i [
        |  "Z"  "'"  <hex-digit>  [  <hex-digit> ]*  "'"
        |  "Z"  """  <hex-digit>  [  <hex-digit> ]*  """
    ] }

    #`[R768] rule hex-digit {:i [
        |  <digit>
        |  "A"
        |  "B"
        |  "C"
        |  "D"
        |  "E"
        |  "F"
    ] }

    #`[R769] rule array-constructor {:i [
        |  "(/"  <ac-spec>  "/)"
        |  <lbracket>  <ac-spec>  <rbracket>
    ] }

    #`[R770] rule ac-spec {:i [
        |  <type-spec>  "::"
        |  [  <type-spec> "::" ]?  <ac-value> [ <ac-value> ]*
    ] }

    #`[R773] rule ac-value {:i [
        |  <expr>
        |  <ac-implied-do>
    ] }

    #`[R774] rule ac-implied-do {:i [
        |  "("  <ac-value> [ <ac-value> ]*  ","  <ac-implied-do-control>  ")"
    ] }

    #`[R775] rule ac-implied-do-control {:i [
        |  [  <integer-type-spec> "::" ]?  <ac-do-variable>  "="  <int-expr>  ","  <int-expr>  [  "," <int-expr> ]?
    ] }

    #`[R776] rule ac-do-variable {:i [
        |  <do-variable>
    ] }

    #`[R801] rule type-declaration-stmt {:i [
        |  <declaration-type-spec>  [  [  "," <attr-spec> ]* "::" ]?  <entity-decl> [ <entity-decl> ]*
    ] <.stmt-separator> }

    #`[R802] rule attr-spec {:i [
        |  <access-spec>
        |  "ALLOCATABLE"
        |  "ASYNCHRONOUS"
        |  "CODIMENSION"  <lbracket>  <coarray-spec>  <rbracket>
        |  "CONTIGUOUS"
        |  "DIMENSION"  "("  <array-spec>  ")"
        |  "EXTERNAL"
        |  "INTENT"  "("  <intent-spec>  ")"
        |  "INTRINSIC"
        |  <language-binding-spec>
        |  "OPTIONAL"
        |  "PARAMETER"
        |  "POINTER"
        |  "PROTECTED"
        |  "SAVE"
        |  "TARGET"
        |  "VALUE"
        |  "VOLATILE"
    ] }

    #`[R803] rule entity-decl {:i [
        |  <name>  [  "(" <array-spec> ")" ]?  [  <lbracket> <coarray-spec> <rbracket> ]?  [  "*" <char-length> ]?  [  <initialization> ]?
        |  <name>  [  "*" <char-length> ]?
    ] }

    #`[R804] rule object-name {:i [
        |  <name>
    ] }

    #`[R805] rule initialization {:i [
        |  "="  <constant-expr>
        |  "=>"  <null-init>
        |  "=>"  <initial-data-target>
    ] }

    #`[R806] rule null-init {:i [
        |  <function-reference>
    ] }

    #`[R807] rule access-spec {:i [
        |  "PUBLIC"
        |  "PRIVATE"
    ] }

    #`[R808] rule language-binding-spec {:i [
        |  "BIND"  "(C"  [  "," "NAME" "=" <default-char-constant-expr> ]?  ")"
    ] }

    #`[R809] rule coarray-spec {:i [
        |  <deferred-coshape-spec> [ <deferred-coshape-spec> ]*
        |  <explicit-coshape-spec>
    ] }

    #`[R810] rule deferred-coshape-spec {:i [
        |  ":"
    ] }

    #`[R811] rule explicit-coshape-spec {:i [
        |  [  [  <lower-cobound> ":" ]? <upper-cobound> "," ]*  [  <lower-cobound> ":" ]?  "*"
    ] }

    #`[R812] rule lower-cobound {:i [
        |  <specification-expr>
    ] }

    #`[R813] rule upper-cobound {:i [
        |  <specification-expr>
    ] }

    #`[R814] rule dimension-spec {:i [
        |  "DIMENSION"  "("  <array-spec>  ")"
    ] }

    #`[R815] rule array-spec {:i [
        |  <explicit-shape-spec> [ <explicit-shape-spec> ]*
        |  <assumed-shape-spec> [ <assumed-shape-spec> ]*
        |  <deferred-shape-spec> [ <deferred-shape-spec> ]*
        |  <assumed-size-spec>
        |  <implied-shape-spec>
        |  <implied-shape-or-assumed-size-spec>
        |  <assumed-rank-spec>
    ] }

    #`[R816] rule explicit-shape-spec {:i [
        |  [  <lower-bound> ":" ]?  <upper-bound>
    ] }

    #`[R817] rule lower-bound {:i [
        |  <specification-expr>
    ] }

    #`[R818] rule upper-bound {:i [
        |  <specification-expr>
    ] }

    #`[R819] rule assumed-shape-spec {:i [
        |  [  <lower-bound> ]?  ":"
    ] }

    #`[R820] rule deferred-shape-spec {:i [
        |  ":"
    ] }

    #`[R821] rule assumed-implied-spec {:i [
        |  [  <lower-bound> ":" ]?  "*"
    ] }

    #`[R822] rule assumed-size-spec {:i [
        |  <explicit-shape-spec> [ <explicit-shape-spec> ]*  ","  <assumed-implied-spec>
    ] }

    #`[R823] rule implied-shape-or-assumed-size-spec {:i [
        |  <assumed-implied-spec>
    ] }

    #`[R824] rule implied-shape-spec {:i [
        |  <assumed-implied-spec>  ","  <assumed-implied-spec> [ <assumed-implied-spec> ]*
    ] }

    #`[R825] rule assumed-rank-spec {:i [
        |  ".."
    ] }

    #`[R826] rule intent-spec {:i [
        |  "IN"
        |  "OUT"
        |  "INOUT"
    ] }

    #`[R827] rule access-stmt {:i [
        |  <access-spec>  [  [  "::" ]? <access-id> [ <access-id> ]* ]?
    ] <.stmt-separator> }

    #`[R828] rule access-id {:i [
        |  <name>
        |  <generic-spec>
    ] }

    #`[R829] rule allocatable-stmt {:i [
        |  "ALLOCATABLE"  [  "::" ]?  <allocatable-decl> [ <allocatable-decl> ]*
    ] <.stmt-separator> }

    #`[R830] rule allocatable-decl {:i [
        |  <name>  [  "(" <array-spec> ")" ]?  [  <lbracket> <coarray-spec> <rbracket> ]?
    ] }

    #`[R831] rule asynchronous-stmt {:i [
        |  "ASYNCHRONOUS"  [  "::" ]?  <object-name> [ <object-name> ]*
    ] <.stmt-separator> }

    #`[R832] rule bind-stmt {:i [
        |  <language-binding-spec>  [  "::" ]?  <bind-entity> [ <bind-entity> ]*
    ] <.stmt-separator> }

    #`[R833] rule bind-entity {:i [
        |  <name>
        |  "/"  <name>  "/"
    ] }

    #`[R834] rule codimension-stmt {:i [
        |  "CODIMENSION"  [  "::" ]?  <codimension-decl> [ <codimension-decl> ]*
    ] <.stmt-separator> }

    #`[R835] rule codimension-decl {:i [
        |  <name>  <lbracket>  <coarray-spec>  <rbracket>
    ] }

    #`[R836] rule contiguous-stmt {:i [
        |  "CONTIGUOUS"  [  "::" ]?  <object-name> [ <object-name> ]*
    ] <.stmt-separator> }

    #`[R837] rule data-stmt {:i [
        |  "DATA"  <data-stmt-set>  [  [  "," ]? <data-stmt-set> ]*
    ] <.stmt-separator> }

    #`[R838] rule data-stmt-set {:i [
        |  <data-stmt-object> [ <data-stmt-object> ]*  "/"  <data-stmt-value> [ <data-stmt-value> ]*  "/"
    ] }

    #`[R839] rule data-stmt-object {:i [
        |  <variable>
        |  <data-implied-do>
    ] }

    #`[R840] rule data-implied-do {:i [
        |  "("  <data-i-do-object> [ <data-i-do-object> ]*  ","  [  <integer-type-spec> "::" ]?  <data-i-do-variable>  "="  <int-constant-expr>  ","  <int-constant-expr>  [  "," <int-constant-expr> ]?  ")"
    ] }

    #`[R841] rule data-i-do-object {:i [
        |  <array-element>
        |  <structure-component>
        |  <data-implied-do>
    ] }

    #`[R842] rule data-i-do-variable {:i [
        |  <do-variable>
    ] }

    #`[R843] rule data-stmt-value {:i [
        |  [  <data-stmt-repeat> "*" ]?  <data-stmt-constant>
    ] }

    #`[R844] rule data-stmt-repeat {:i [
        |  <int-constant>
        |  <int-constant-subobject>
    ] }

    #`[R845] rule data-stmt-constant {:i [
        |  <constant>
        |  <constant-subobject>
        |  <signed-int-literal-constant>
        |  <signed-real-literal-constant>
        |  <null-init>
        |  <initial-data-target>
        |  <structure-constructor>
    ] }

    #`[R846] rule int-constant-subobject {:i [
        |  <constant-subobject>
    ] }

    #`[R847] rule constant-subobject {:i [
        |  <designator>
    ] }

    #`[R848] rule dimension-stmt {:i [
        |  "DIMENSION"  [  "::" ]?  <name>  "("  <array-spec>  ")"  [  "," <name> "(" <array-spec> ")" ]*
    ] <.stmt-separator> }

    #`[R849] rule intent-stmt {:i [
        |  "INTENT"  "("  <intent-spec>  ")"  [  "::" ]?  <dummy-arg-name> [ <dummy-arg-name> ]*
    ] <.stmt-separator> }

    #`[R850] rule optional-stmt {:i [
        |  "OPTIONAL"  [  "::" ]?  <dummy-arg-name> [ <dummy-arg-name> ]*
    ] <.stmt-separator> }

    #`[R851] rule parameter-stmt {:i [
        |  "PARAMETER("  <named-constant-def> [ <named-constant-def> ]*  ")"
    ] <.stmt-separator> }

    #`[R852] rule named-constant-def {:i [
        |  <named-constant>  "="  <constant-expr>
    ] }

    #`[R853] rule pointer-stmt {:i [
        |  "POINTER"  [  "::" ]?  <pointer-decl> [ <pointer-decl> ]*
    ] <.stmt-separator> }

    #`[R854] rule pointer-decl {:i [
        |  <name>  [  "(" <deferred-shape-spec> [ <deferred-shape-spec> ]* ")" ]?
        |  <name>
    ] }

    #`[R855] rule protected-stmt {:i [
        |  "PROTECTED"  [  "::" ]?  <entity-name> [ <entity-name> ]*
    ] <.stmt-separator> }

    #`[R856] rule save-stmt {:i [
        |  "SAVE"  [  [  "::" ]? <saved-entity> [ <saved-entity> ]* ]?
    ] <.stmt-separator> }

    #`[R857] rule saved-entity {:i [
        |  <name>
        |  <name>
        |  "/"  <name>  "/"
    ] }

    #`[R858] rule proc-pointer-name {:i [
        |  <name>
    ] }

    #`[R859] rule target-stmt {:i [
        |  "TARGET"  [  "::" ]?  <target-decl> [ <target-decl> ]*
    ] <.stmt-separator> }

    #`[R860] rule target-decl {:i [
        |  <name>  [  "(" <array-spec> ")" ]?  [  <lbracket> <coarray-spec> <rbracket> ]?
    ] }

    #`[R861] rule value-stmt {:i [
        |  "VALUE"  [  "::" ]?  <dummy-arg-name> [ <dummy-arg-name> ]*
    ] <.stmt-separator> }

    #`[R862] rule volatile-stmt {:i [
        |  "VOLATILE"  [  "::" ]?  <object-name> [ <object-name> ]*
    ] <.stmt-separator> }

    #`[R863] rule implicit-stmt {:i [
        |  "IMPLICIT"  <implicit-spec> [ <implicit-spec> ]*
        |  "IMPLICIT"  "NONE"  [  "(" [  <implicit-none-spec> [ <implicit-none-spec> ]* ]? ")" ]?
    ] <.stmt-separator> }

    #`[R864] rule implicit-spec {:i [
        |  <declaration-type-spec>  "("  <letter-spec> [ <letter-spec> ]*  ")"
    ] }

    #`[R865] rule letter-spec {:i [
        |  <letter>  [  "-" <letter> ]?
    ] }

    #`[R866] rule implicit-none-spec {:i [
        |  "EXTERNAL"
        |  "TYPE"
    ] }

    #`[R867] rule import-stmt {:i [
        |  "IMPORT"  [  [  "::" ]? <import-name> [ <import-name> ]* ]?
        |  "IMPORT,"  "ONLY"  ":"  <import-name> [ <import-name> ]*
        |  "IMPORT,"  "NONE"
        |  "IMPORT,"  "ALL"
    ] <.stmt-separator> }

    #`[R868] rule namelist-stmt {:i [
        |  "NAMELIST"  "/"  <name>  "/"  <namelist-group-object> [ <namelist-group-object> ]*  [  [  "," ]? "/" <name> "/" <namelist-group-object> [ <namelist-group-object> ]* ]*
    ] <.stmt-separator> }

    #`[R869] rule namelist-group-object {:i [
        |  <name>
    ] }

    #`[R870] rule equivalence-stmt {:i [
        |  "EQUIVALENCE"  <equivalence-set> [ <equivalence-set> ]*
    ] <.stmt-separator> }

    #`[R871] rule equivalence-set {:i [
        |  "("  <equivalence-object>  ","  <equivalence-object> [ <equivalence-object> ]*  ")"
    ] }

    #`[R872] rule equivalence-object {:i [
        |  <name>
        |  <array-element>
        |  <substring>
    ] }

    #`[R873] rule common-stmt {:i [
        |  "COMMON"  [  "/" [  <name> ]? "/" ]?  <common-block-object> [ <common-block-object> ]*  [  [  "," ]? "/" [  <name> ]? "/" <common-block-object> [ <common-block-object> ]* ]*
    ] <.stmt-separator> }

    #`[R874] rule common-block-object {:i [
        |  <name>  [  "(" <array-spec> ")" ]?
    ] }

    #`[R901] rule designator {:i [
        |  <name>
        |  <array-element>
        |  <array-section>
        |  <coindexed-named-object>
        |  <complex-part-designator>
        |  <structure-component>
        |  <substring>
    ] }

    #`[R902] rule variable {:i [
        |  <designator>
        |  <function-reference>
    ] }

    #`[R903] rule variable-name {:i [
        |  <name>
    ] }

    #`[R904] rule logical-variable {:i [
        |  <variable>
    ] }

    #`[R905] rule char-variable {:i [
        |  <variable>
    ] }

    #`[R906] rule default-char-variable {:i [
        |  <variable>
    ] }

    #`[R907] rule int-variable {:i [
        |  <variable>
    ] }

    #`[R908] rule substring {:i [
        |  <parent-string>  "("  <substring-range>  ")"
    ] }

    #`[R909] rule parent-string {:i [
        |  <name>
        |  <array-element>
        |  <coindexed-named-object>
        |  <structure-component>
        |  <constant>
    ] }

    #`[R910] rule substring-range {:i [
        |  [  <int-expr> ]?  ":"  [  <int-expr> ]?
    ] }

    #`[R911] rule data-ref {:i [
        |  <part-ref>  [  "%" <part-ref> ]*
    ] }

    #`[R912] rule part-ref {:i [
        |  <name>  [  "(" <section-subscript> [ <section-subscript> ]* ")" ]?  [  <image-selector> ]?
    ] }

    #`[R913] rule structure-component {:i [
        |  <data-ref>
    ] }

    #`[R914] rule coindexed-named-object {:i [
        |  <data-ref>
    ] }

    #`[R915] rule complex-part-designator {:i [
        |  <designator>  "%"  "RE"
        |  <designator>  "%"  "IM"
    ] }

    #`[R916] rule type-param-inquiry {:i [
        |  <designator>  "%"  <name>
    ] }

    #`[R917] rule array-element {:i [
        |  <data-ref>
    ] }

    #`[R918] rule array-section {:i [
        |  <data-ref>  [  "(" <substring-range> ")" ]?
        |  <complex-part-designator>
    ] }

    #`[R919] rule subscript {:i [
        |  <int-expr>
    ] }

    #`[R920] rule section-subscript {:i [
        |  <subscript>
        |  <subscript-triplet>
        |  <vector-subscript>
    ] }

    #`[R921] rule subscript-triplet {:i [
        |  [  <subscript> ]?  ":"  [  <subscript> ]?  [  ":" <stride> ]?
    ] }

    #`[R922] rule stride {:i [
        |  <int-expr>
    ] }

    #`[R923] rule vector-subscript {:i [
        |  <int-expr>
    ] }

    #`[R924] rule image-selector {:i [
        |  <lbracket>  <cosubscript> [ <cosubscript> ]*  [  "," <image-selector-spec> [ <image-selector-spec> ]* ]?  <rbracket>
    ] }

    #`[R925] rule cosubscript {:i [
        |  <int-expr>
    ] }

    #`[R926] rule image-selector-spec {:i [
        |  "STAT"  "="  <stat-variable>
        |  "TEAM="  <team-value>
        |  "TEAM_NUMBER="  <int-expr>
    ] }

    #`[R927] rule allocate-stmt {:i [
        |  "ALLOCATE("  [  <type-spec> "::" ]?  <allocation> [ <allocation> ]*  [  "," <alloc-opt> [ <alloc-opt> ]* ]?  ")"
    ] <.stmt-separator> }

    #`[R928] rule alloc-opt {:i [
        |  "ERRMSG="  <errmsg-variable>
        |  "MOLD="  <source-expr>
        |  "SOURCE="  <source-expr>
        |  "STAT"  "="  <stat-variable>
    ] }

    #`[R929] rule errmsg-variable {:i [
        |  <default-char-variable>
    ] }

    #`[R930] rule source-expr {:i [
        |  <expr>
    ] }

    #`[R931] rule allocation {:i [
        |  <allocate-object>  [  "(" <allocate-shape-spec> [ <allocate-shape-spec> ]* ")" ]?  [  <lbracket> <allocate-coarray-spec> <rbracket> ]?
    ] }

    #`[R932] rule allocate-object {:i [
        |  <name>
        |  <structure-component>
    ] }

    #`[R933] rule allocate-shape-spec {:i [
        |  [  <lower-bound-expr> ":" ]?  <upper-bound-expr>
    ] }

    #`[R934] rule lower-bound-expr {:i [
        |  <int-expr>
    ] }

    #`[R935] rule upper-bound-expr {:i [
        |  <int-expr>
    ] }

    #`[R936] rule allocate-coarray-spec {:i [
        |  [  <allocate-coshape-spec> [ <allocate-coshape-spec> ]* "," ]?  [  <lower-bound-expr> ":" ]?  "*"
    ] }

    #`[R937] rule allocate-coshape-spec {:i [
        |  [  <lower-bound-expr> ":" ]?  <upper-bound-expr>
    ] }

    #`[R938] rule nullify-stmt {:i [
        |  "NULLIFY"  "("  <pointer-object> [ <pointer-object> ]*  ")"
    ] <.stmt-separator> }

    #`[R939] rule pointer-object {:i [
        |  <name>
        |  <structure-component>
        |  <name>
    ] }

    #`[R940] rule deallocate-stmt {:i [
        |  "DEALLOCATE("  <allocate-object> [ <allocate-object> ]*  [  "," <dealloc-opt> [ <dealloc-opt> ]* ]?  ")"
    ] <.stmt-separator> }

    #`[R941] rule dealloc-opt {:i [
        |  "STAT"  "="  <stat-variable>
        |  "ERRMSG="  <errmsg-variable>
    ] }

    #`[R942] rule stat-variable {:i [
        |  <int-variable>
    ] }

    #`[R1001] rule primary {:i [
        |  <literal-constant>
        |  <designator>
        |  <array-constructor>
        |  <structure-constructor>
        |  <function-reference>
        |  <type-param-inquiry>
        |  <name>
        |  "("  <expr>  ")"
    ] }

    #`[R1002] rule level-one-expr {:i [
        |  [  <defined-unary-op> ]?  <primary>
    ] }

    #`[R1004] rule mult-operand {:i [
        |  <level-one-expr>  [  <power-op> <mult-operand> ]?
    ] }

    #`[R1005] rule add-operand {:i [
        |  [  <add-operand> <mult-op> ]?  <mult-operand>
    ] }

    #`[R1006] rule level-two-expr {:i [
        |  [  [  <level-two-expr> ]? <add-op> ]?  <add-operand>
    ] }

    #`[R1010] rule level-three-expr {:i [
        |  [  <level-three-expr> <concat-op> ]?  <level-two-expr>
    ] }

    #`[R1012] rule level-four-expr {:i [
        |  [  <level-three-expr> <rel-op> ]?  <level-three-expr>
    ] }

    #`[R1014] rule and-operand {:i [
        |  [  <not-op> ]?  <level-four-expr>
    ] }

    #`[R1015] rule or-operand {:i [
        |  [  <or-operand> <and-op> ]?  <and-operand>
    ] }

    #`[R1016] rule equiv-operand {:i [
        |  [  <equiv-operand> <or-op> ]?  <or-operand>
    ] }

    #`[R1017] rule level-five-expr {:i [
        |  [  <level-five-expr> <equiv-op> ]?  <equiv-operand>
    ] }

    #`[R1022] rule expr {:i [
        |  [  <expr> <defined-binary-op> ]?  <level-five-expr>
    ] }

    #`[R1024] rule logical-expr {:i [
        |  <expr>
    ] }

    #`[R1025] rule default-char-expr {:i [
        |  <expr>
    ] }

    #`[R1026] rule int-expr {:i [
        |  <expr>
    ] }

    #`[R1027] rule numeric-expr {:i [
        |  <expr>
    ] }

    #`[R1028] rule specification-expr {:i [
        |  <int-expr>
    ] }

    #`[R1029] rule constant-expr {:i [
        |  <expr>
    ] }

    #`[R1030] rule default-char-constant-expr {:i [
        |  <default-char-expr>
    ] }

    #`[R1031] rule int-constant-expr {:i [
        |  <int-expr>
    ] }

    #`[R1032] rule assignment-stmt {:i [
        |  <variable>  "="  <expr>
    ] <.stmt-separator> }

    #`[R1033] rule pointer-assignment-stmt {:i [
        |  <data-pointer-object>  [  "(" <bounds-spec> [ <bounds-spec> ]* ")" ]?  "=>"  <data-target>
        |  <data-pointer-object>  "("  <bounds-remapping> [ <bounds-remapping> ]*  ")"  "=>"  <data-target>
        |  <proc-pointer-object>  "=>"  <proc-target>
    ] <.stmt-separator> }

    #`[R1034] rule data-pointer-object {:i [
        |  <name>
        |  <variable>  "%"  <name>
    ] }

    #`[R1035] rule bounds-spec {:i [
        |  <lower-bound-expr>  ":"
    ] }

    #`[R1036] rule bounds-remapping {:i [
        |  <lower-bound-expr>  ":"  <upper-bound-expr>
    ] }

    #`[R1037] rule data-target {:i [
        |  <expr>
    ] }

    #`[R1038] rule proc-pointer-object {:i [
        |  <name>
        |  <proc-component-ref>
    ] }

    #`[R1039] rule proc-component-ref {:i [
        |  <variable>  "%"  <name>
    ] }

    #`[R1040] rule proc-target {:i [
        |  <expr>
        |  <name>
        |  <proc-component-ref>
    ] }

    #`[R1041] rule where-stmt {:i [
        |  "WHERE("  <mask-expr>  ")"  <where-assignment-stmt>
    ] <.stmt-separator> }

    #`[R1042] rule where-construct {:i [
        |  <where-construct-stmt>  [  <where-body-construct> ]*  [  <masked-elsewhere-stmt> [  <where-body-construct> ]* ]*  [  <elsewhere-stmt> [  <where-body-construct> ]* ]?  <end-where-stmt>
    ] }

    #`[R1043] rule where-construct-stmt {:i [
        |  [  <name> ":" ]?  "WHERE"  "("  <mask-expr>  ")"
    ] <.stmt-separator> }

    #`[R1044] rule where-body-construct {:i [
        |  <where-assignment-stmt>
        |  <where-stmt>
        |  <where-construct>
    ] }

    #`[R1045] rule where-assignment-stmt {:i [
        |  <assignment-stmt>
    ] <.stmt-separator> }

    #`[R1046] rule mask-expr {:i [
        |  <logical-expr>
    ] }

    #`[R1047] rule masked-elsewhere-stmt {:i [
        |  "ELSEWHERE("  <mask-expr>  ")"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1048] rule elsewhere-stmt {:i [
        |  "ELSEWHERE"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1049] rule end-where-stmt {:i [
        |  "ENDWHERE"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1050] rule forall-construct {:i [
        |  <forall-construct-stmt>  [  <forall-body-construct> ]*  <end-forall-stmt>
    ] }

    #`[R1051] rule forall-construct-stmt {:i [
        |  [  <name> ":" ]?  "FORALL"  <concurrent-header>
    ] <.stmt-separator> }

    #`[R1052] rule forall-body-construct {:i [
        |  <forall-assignment-stmt>
        |  <where-stmt>
        |  <where-construct>
        |  <forall-construct>
        |  <forall-stmt>
    ] }

    #`[R1053] rule forall-assignment-stmt {:i [
        |  <assignment-stmt>
        |  <pointer-assignment-stmt>
    ] <.stmt-separator> }

    #`[R1054] rule end-forall-stmt {:i [
        |  "ENDFORALL"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1055] rule forall-stmt {:i [
        |  "FORALL"  <concurrent-header>  <forall-assignment-stmt>
    ] <.stmt-separator> }

    #`[R1101] rule block {:i [
        |  [  <execution-part-construct> ]*
    ] }

    #`[R1102] rule associate-construct {:i [
        |  <associate-stmt>  <block>  <end-associate-stmt>
    ] }

    #`[R1103] rule associate-stmt {:i [
        |  [  <name> ":" ]?  "ASSOCIATE"  "("  <association> [ <association> ]*  ")"
    ] <.stmt-separator> }

    #`[R1104] rule association {:i [
        |  <name>  "=>"  <selector>
    ] }

    #`[R1105] rule selector {:i [
        |  <expr>
        |  <variable>
    ] }

    #`[R1106] rule end-associate-stmt {:i [
        |  "ENDASSOCIATE"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1107] rule block-construct {:i [
        |  <block-stmt>  [  <block-specification-part> ]?  <block>  <end-block-stmt>
    ] }

    #`[R1108] rule block-stmt {:i [
        |  [  <name> ":" ]?  "BLOCK"
    ] <.stmt-separator> }

    #`[R1109] rule block-specification-part {:i [
        |  [  <use-stmt> ]*  [  <import-stmt> ]*  [  [  <declaration-construct> ]* <specification-construct> ]?
    ] }

    #`[R1110] rule end-block-stmt {:i [
        |  "ENDBLOCK"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1111] rule change-team-construct {:i [
        |  <change-team-stmt>  <block>  <end-change-team-stmt>
    ] }

    #`[R1112] rule change-team-stmt {:i [
        |  [  <name> ":" ]?  "CHANGE"  "TEAM"  "("  <team-value>  [  "," <coarray-association> [ <coarray-association> ]* ]?  [  "," <sync-stat> [ <sync-stat> ]* ]?  ")"
    ] <.stmt-separator> }

    #`[R1113] rule coarray-association {:i [
        |  <codimension-decl>  "=>"  <selector>
    ] }

    #`[R1114] rule end-change-team-stmt {:i [
        |  "ENDTEAM"  [  "(" [  <sync-stat> [ <sync-stat> ]* ]? ")" ]?  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1115] rule team-value {:i [
        |  <expr>
    ] }

    #`[R1116] rule critical-construct {:i [
        |  <critical-stmt>  <block>  <end-critical-stmt>
    ] }

    #`[R1117] rule critical-stmt {:i [
        |  [  <name> ":" ]?  "CRITICAL"  [  "(" [  <sync-stat> [ <sync-stat> ]* ]? ")" ]?
    ] <.stmt-separator> }

    #`[R1118] rule end-critical-stmt {:i [
        |  "ENDCRITICAL"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1119] rule do-construct {:i [
        |  <do-stmt>  <block>  <end-do>
    ] }

    #`[R1120] rule do-stmt {:i [
        |  <nonlabel-do-stmt>
        |  <label-do-stmt>
    ] <.stmt-separator> }

    #`[R1121] rule label-do-stmt {:i [
        |  [  <name> ":" ]?  "DO"  <label>  [  <loop-control> ]?
    ] <.stmt-separator> }

    #`[R1122] rule nonlabel-do-stmt {:i [
        |  [  <name> ":" ]?  "DO"  [  <loop-control> ]?
    ] <.stmt-separator> }

    #`[R1123] rule loop-control {:i [
        |  [  "," ]?  <do-variable>  "="  <int-expr>  ","  <int-expr>  [  "," <int-expr> ]?
        |  [  "," ]?  "WHILE"  "("  <logical-expr>  ")"
        |  [  "," ]?  "CONCURRENT"  <concurrent-header>  <concurrent-locality>
    ] }

    #`[R1124] rule do-variable {:i [
        |  <name>
    ] }

    #`[R1125] rule concurrent-header {:i [
        |  "("  [  <integer-type-spec> "::" ]?  <concurrent-control> [ <concurrent-control> ]*  [  "," <mask-expr> ]?  ")"
    ] }

    #`[R1126] rule concurrent-control {:i [
        |  <name>  "="  <concurrent-limit>  ":"  <concurrent-limit>  [  ":" <concurrent-step> ]?
    ] }

    #`[R1127] rule concurrent-limit {:i [
        |  <int-expr>
    ] }

    #`[R1128] rule concurrent-step {:i [
        |  <int-expr>
    ] }

    #`[R1129] rule concurrent-locality {:i [
        |  [  <locality-spec> ]*
    ] }

    #`[R1130] rule locality-spec {:i [
        |  "LOCAL("  <variable-name> [ <variable-name> ]*  ")"
        |  "LOCAL_INIT"  "("  <variable-name> [ <variable-name> ]*  ")"
        |  "SHARED("  <variable-name> [ <variable-name> ]*  ")"
        |  "DEFAULT"  "("  "NONE"  ")"
    ] }

    #`[R1131] rule end-do {:i [
        |  <end-do-stmt>
        |  <continue-stmt>
    ] }

    #`[R1132] rule end-do-stmt {:i [
        |  "ENDDO"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1133] rule cycle-stmt {:i [
        |  "CYCLE"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1134] rule if-construct {:i [
        |  <if-then-stmt>  <block>  [  <else-if-stmt> <block> ]*  [  <else-stmt> <block> ]?  <end-if-stmt>
    ] }

    #`[R1135] rule if-then-stmt {:i [
        |  [  <name> ":" ]?  "IF"  "("  <logical-expr>  ")"  "THEN"
    ] <.stmt-separator> }

    #`[R1136] rule else-if-stmt {:i [
        |  "ELSE"  "IF"  "("  <logical-expr>  ")"  "THEN"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1137] rule else-stmt {:i [
        |  "ELSE"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1138] rule end-if-stmt {:i [
        |  "ENDIF"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1139] rule if-stmt {:i [
        |  "IF"  "("  <logical-expr>  ")"  <action-stmt>
    ] <.stmt-separator> }

    #`[R1140] rule case-construct {:i [
        |  <select-case-stmt>  [  <case-stmt> <block> ]*  <end-select-stmt>
    ] }

    #`[R1141] rule select-case-stmt {:i [
        |  [  <name> ":" ]?  "SELECT"  "CASE"  "("  <case-expr>  ")"
    ] <.stmt-separator> }

    #`[R1142] rule case-stmt {:i [
        |  "CASE"  <case-selector>  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1143] rule end-select-stmt {:i [
        |  "ENDSELECT"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1144] rule case-expr {:i [
        |  <expr>
    ] }

    #`[R1145] rule case-selector {:i [
        |  "("  <case-value-range> [ <case-value-range> ]*  ")"
        |  "DEFAULT"
    ] }

    #`[R1146] rule case-value-range {:i [
        |  <case-value>
        |  <case-value>  ":"
        |  ":"  <case-value>
        |  <case-value>  ":"  <case-value>
    ] }

    #`[R1147] rule case-value {:i [
        |  <constant-expr>
    ] }

    #`[R1148] rule select-rank-construct {:i [
        |  <select-rank-stmt>  [  <select-rank-case-stmt> <block> ]*  <end-select-rank-stmt>
    ] }

    #`[R1149] rule select-rank-stmt {:i [
        |  [  <name> ":" ]?  "SELECT"  "RANK"  "("  [  <name> "=>" ]?  <selector>  ")"
    ] <.stmt-separator> }

    #`[R1150] rule select-rank-case-stmt {:i [
        |  "RANK("  <int-constant-expr>  ")"  [  <name> ]?
        |  "RANK(*)"  [  <name> ]?
        |  "RANKDEFAULT"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1151] rule end-select-rank-stmt {:i [
        |  "ENDSELECT"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1152] rule select-type-construct {:i [
        |  <select-type-stmt>  [  <type-guard-stmt> <block> ]*  <end-select-type-stmt>
    ] }

    #`[R1153] rule select-type-stmt {:i [
        |  [  <name> ":" ]?  "SELECT"  "TYPE"  "("  [  <name> "=>" ]?  <selector>  ")"
    ] <.stmt-separator> }

    #`[R1154] rule type-guard-stmt {:i [
        |  "TYPEIS("  <type-spec>  ")"  [  <name> ]?
        |  "CLASS"  "IS"  "("  <derived-type-spec>  ")"  [  <name> ]?
        |  "CLASS"  "DEFAULT"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1155] rule end-select-type-stmt {:i [
        |  "ENDSELECT"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1156] rule exit-stmt {:i [
        |  "EXIT"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1157] rule goto-stmt {:i [
        |  "GOTO"  <label>
    ] <.stmt-separator> }

    #`[R1158] rule computed-goto-stmt {:i [
        |  "GOTO("  <label> [ <label> ]*  ")"  [  "," ]?  <int-expr>
    ] <.stmt-separator> }

    #`[R1159] rule continue-stmt {:i [
        |  "CONTINUE"
    ] <.stmt-separator> }

    #`[R1160] rule stop-stmt {:i [
        |  "STOP"  [  <stop-code> ]?  [  "," "QUIET" "=" <logical-expr> ]?
    ] <.stmt-separator> }

    #`[R1161] rule error-stop-stmt {:i [
        |  "ERRORSTOP"  [  <stop-code> ]?  [  "," "QUIET" "=" <logical-expr> ]?
    ] <.stmt-separator> }

    #`[R1162] rule stop-code {:i [
        |  <default-char-expr>
        |  <int-expr>
    ] }

    #`[R1163] rule fail-image-stmt {:i [
        |  "FAIL"  "IMAGE"
    ] <.stmt-separator> }

    #`[R1164] rule sync-all-stmt {:i [
        |  "SYNCALL"  [  "(" [  <sync-stat> [ <sync-stat> ]* ]? ")" ]?
    ] <.stmt-separator> }

    #`[R1165] rule sync-stat {:i [
        |  "STAT"  "="  <stat-variable>
        |  "ERRMSG="  <errmsg-variable>
    ] }

    #`[R1166] rule sync-images-stmt {:i [
        |  "SYNCIMAGES("  <image-set>  [  "," <sync-stat> [ <sync-stat> ]* ]?  ")"
    ] <.stmt-separator> }

    #`[R1167] rule image-set {:i [
        |  <int-expr>
        |  "*"
    ] }

    #`[R1168] rule sync-memory-stmt {:i [
        |  "SYNCMEMORY"  [  "(" [  <sync-stat> [ <sync-stat> ]* ]? ")" ]?
    ] <.stmt-separator> }

    #`[R1169] rule sync-team-stmt {:i [
        |  "SYNCTEAM("  <team-value>  [  "," <sync-stat> [ <sync-stat> ]* ]?  ")"
    ] <.stmt-separator> }

    #`[R1170] rule event-post-stmt {:i [
        |  "EVENTPOST("  <event-variable>  [  "," <sync-stat> [ <sync-stat> ]* ]?  ")"
    ] <.stmt-separator> }

    #`[R1171] rule event-variable {:i [
        |  <variable>
    ] }

    #`[R1172] rule event-wait-stmt {:i [
        |  "EVENTWAIT("  <event-variable>  [  "," <event-wait-spec> [ <event-wait-spec> ]* ]?  ")"
    ] <.stmt-separator> }

    #`[R1173] rule event-wait-spec {:i [
        |  <until-spec>
        |  <sync-stat>
    ] }

    #`[R1174] rule until-spec {:i [
        |  "UNTIL_COUNT="  <int-expr>
    ] }

    #`[R1175] rule form-team-stmt {:i [
        |  "FORMTEAM("  <team-number>  ","  <team-variable>  [  "," <form-team-spec> [ <form-team-spec> ]* ]?  ")"
    ] <.stmt-separator> }

    #`[R1176] rule team-number {:i [
        |  <int-expr>
    ] }

    #`[R1177] rule team-variable {:i [
        |  <variable>
    ] }

    #`[R1178] rule form-team-spec {:i [
        |  "NEW_INDEX="  <int-expr>
        |  <sync-stat>
    ] }

    #`[R1179] rule lock-stmt {:i [
        |  "LOCK("  <lock-variable>  [  "," <lock-stat> [ <lock-stat> ]* ]?  ")"
    ] <.stmt-separator> }

    #`[R1180] rule lock-stat {:i [
        |  "ACQUIRED_LOCK="  <logical-variable>
        |  <sync-stat>
    ] }

    #`[R1181] rule unlock-stmt {:i [
        |  "UNLOCK("  <lock-variable>  [  "," <sync-stat> [ <sync-stat> ]* ]?  ")"
    ] <.stmt-separator> }

    #`[R1182] rule lock-variable {:i [
        |  <variable>
    ] }

    #`[R1201] rule io-unit {:i [
        |  <file-unit-number>
        |  "*"
        |  <internal-file-variable>
    ] }

    #`[R1202] rule file-unit-number {:i [
        |  <int-expr>
    ] }

    #`[R1203] rule internal-file-variable {:i [
        |  <char-variable>
    ] }

    #`[R1204] rule open-stmt {:i [
        |  "OPEN("  <connect-spec> [ <connect-spec> ]*  ")"
    ] <.stmt-separator> }

    #`[R1205] rule connect-spec {:i [
        |  [  "UNIT" "=" ]?  <file-unit-number>
        |  "ACCESS"  "="  <default-char-expr>
        |  "ACTION"  "="  <default-char-expr>
        |  "ASYNCHRONOUS="  <default-char-expr>
        |  "BLANK="  <default-char-expr>
        |  "DECIMAL"  "="  <default-char-expr>
        |  "DELIM"  "="  <default-char-expr>
        |  "ENCODING="  <default-char-expr>
        |  "ERR"  "="  <label>
        |  "FILE"  "="  <file-name-expr>
        |  "FORM="  <default-char-expr>
        |  "IOMSG"  "="  <iomsg-variable>
        |  "IOSTAT"  "="  <stat-variable>
        |  "NEWUNIT="  <int-variable>
        |  "PAD"  "="  <default-char-expr>
        |  "POSITION"  "="  <default-char-expr>
        |  "RECL"  "="  <int-expr>
        |  "ROUND="  <default-char-expr>
        |  "SIGN"  "="  <default-char-expr>
        |  "STATUS"  "="  <default-char-expr>
    ] }

    #`[R1206] rule file-name-expr {:i [
        |  <default-char-expr>
    ] }

    #`[R1207] rule iomsg-variable {:i [
        |  <default-char-variable>
    ] }

    #`[R1208] rule close-stmt {:i [
        |  "CLOSE("  <close-spec> [ <close-spec> ]*  ")"
    ] <.stmt-separator> }

    #`[R1209] rule close-spec {:i [
        |  [  "UNIT" "=" ]?  <file-unit-number>
        |  "IOSTAT"  "="  <stat-variable>
        |  "IOMSG"  "="  <iomsg-variable>
        |  "ERR"  "="  <label>
        |  "STATUS"  "="  <default-char-expr>
    ] }

    #`[R1210] rule read-stmt {:i [
        |  "READ("  <io-control-spec> [ <io-control-spec> ]*  ")"  [  <input-item> [ <input-item> ]* ]?
        |  "READ"  <format>  [  "," <input-item> [ <input-item> ]* ]?
    ] <.stmt-separator> }

    #`[R1211] rule write-stmt {:i [
        |  "WRITE("  <io-control-spec> [ <io-control-spec> ]*  ")"  [  <output-item> [ <output-item> ]* ]?
    ] <.stmt-separator> }

    #`[R1212] rule print-stmt {:i [
        |  "PRINT"  <format>  [  "," <output-item> [ <output-item> ]* ]?
    ] <.stmt-separator> }

    #`[R1213] rule io-control-spec {:i [
        |  [  "UNIT" "=" ]?  <io-unit>
        |  [  "FMT" "=" ]?  <format>
        |  [  "NML" "=" ]?  <name>
        |  "ADVANCE="  <default-char-expr>
        |  "ASYNCHRONOUS="  <default-char-constant-expr>
        |  "BLANK="  <default-char-expr>
        |  "DECIMAL"  "="  <default-char-expr>
        |  "DELIM"  "="  <default-char-expr>
        |  "END="  <label>
        |  "EOR="  <label>
        |  "ERR"  "="  <label>
        |  "ID"  "="  <id-variable>
        |  "IOMSG"  "="  <iomsg-variable>
        |  "IOSTAT"  "="  <stat-variable>
        |  "PAD"  "="  <default-char-expr>
        |  "POS"  "="  <int-expr>
        |  "REC"  "="  <int-expr>
        |  "ROUND="  <default-char-expr>
        |  "SIGN"  "="  <default-char-expr>
        |  "SIZE"  "="  <int-variable>
    ] }

    #`[R1214] rule id-variable {:i [
        |  <int-variable>
    ] }

    #`[R1215] rule format {:i [
        |  <default-char-expr>
        |  <label>
        |  "*"
    ] }

    #`[R1216] rule input-item {:i [
        |  <variable>
        |  <io-implied-do>
    ] }

    #`[R1217] rule output-item {:i [
        |  <expr>
        |  <io-implied-do>
    ] }

    #`[R1218] rule io-implied-do {:i [
        |  "("  <io-implied-do-object> [ <io-implied-do-object> ]*  ","  <io-implied-do-control>  ")"
    ] }

    #`[R1219] rule io-implied-do-object {:i [
        |  <input-item>
        |  <output-item>
    ] }

    #`[R1220] rule io-implied-do-control {:i [
        |  <do-variable>  "="  <int-expr>  ","  <int-expr>  [  "," <int-expr> ]?
    ] }

    #`[R1221] rule dtv-type-spec {:i [
        |  "TYPE("  <derived-type-spec>  ")"
        |  "CLASS("  <derived-type-spec>  ")"
    ] }

    #`[R1222] rule wait-stmt {:i [
        |  "WAIT("  <wait-spec> [ <wait-spec> ]*  ")"
    ] <.stmt-separator> }

    #`[R1223] rule wait-spec {:i [
        |  [  "UNIT" "=" ]?  <file-unit-number>
        |  "END="  <label>
        |  "EOR="  <label>
        |  "ERR"  "="  <label>
        |  "ID"  "="  <int-expr>
        |  "IOMSG"  "="  <iomsg-variable>
        |  "IOSTAT"  "="  <stat-variable>
    ] }

    #`[R1224] rule backspace-stmt {:i [
        |  "BACKSPACE"  <file-unit-number>
        |  "BACKSPACE("  <position-spec> [ <position-spec> ]*  ")"
    ] <.stmt-separator> }

    #`[R1225] rule endfile-stmt {:i [
        |  "ENDFILE"  <file-unit-number>
        |  "ENDFILE"  "("  <position-spec> [ <position-spec> ]*  ")"
    ] <.stmt-separator> }

    #`[R1226] rule rewind-stmt {:i [
        |  "REWIND"  <file-unit-number>
        |  "REWIND("  <position-spec> [ <position-spec> ]*  ")"
    ] <.stmt-separator> }

    #`[R1227] rule position-spec {:i [
        |  [  "UNIT" "=" ]?  <file-unit-number>
        |  "IOMSG"  "="  <iomsg-variable>
        |  "IOSTAT"  "="  <stat-variable>
        |  "ERR"  "="  <label>
    ] }

    #`[R1228] rule flush-stmt {:i [
        |  "FLUSH"  <file-unit-number>
        |  "FLUSH"  "("  <flush-spec> [ <flush-spec> ]*  ")"
    ] <.stmt-separator> }

    #`[R1229] rule flush-spec {:i [
        |  [  "UNIT" "=" ]?  <file-unit-number>
        |  "IOSTAT"  "="  <stat-variable>
        |  "IOMSG"  "="  <iomsg-variable>
        |  "ERR"  "="  <label>
    ] }

    #`[R1230] rule inquire-stmt {:i [
        |  "INQUIRE"  "("  <inquire-spec> [ <inquire-spec> ]*  ")"
        |  "INQUIRE"  "("  "IOLENGTH"  "="  <int-variable>  ")"  <output-item> [ <output-item> ]*
    ] <.stmt-separator> }

    #`[R1231] rule inquire-spec {:i [
        |  [  "UNIT" "=" ]?  <file-unit-number>
        |  "FILE"  "="  <file-name-expr>
        |  "ACCESS"  "="  <default-char-variable>
        |  "ACTION"  "="  <default-char-variable>
        |  "ASYNCHRONOUS="  <default-char-variable>
        |  "BLANK="  <default-char-variable>
        |  "DECIMAL"  "="  <default-char-variable>
        |  "DELIM"  "="  <default-char-variable>
        |  "DIRECT"  "="  <default-char-variable>
        |  "ENCODING="  <default-char-variable>
        |  "ERR"  "="  <label>
        |  "EXIST"  "="  <logical-variable>
        |  "FORM="  <default-char-variable>
        |  "FORMATTED="  <default-char-variable>
        |  "ID"  "="  <int-expr>
        |  "IOMSG"  "="  <iomsg-variable>
        |  "IOSTAT"  "="  <stat-variable>
        |  "NAME="  <default-char-variable>
        |  "NAMED="  <logical-variable>
        |  "NEXTREC="  <int-variable>
        |  "NUMBER="  <int-variable>
        |  "OPENED="  <logical-variable>
        |  "PAD"  "="  <default-char-variable>
        |  "PENDING"  "="  <logical-variable>
        |  "POS"  "="  <int-variable>
        |  "POSITION"  "="  <default-char-variable>
        |  "READ="  <default-char-variable>
        |  "READWRITE="  <default-char-variable>
        |  "RECL"  "="  <int-variable>
        |  "ROUND="  <default-char-variable>
        |  "SEQUENTIAL"  "="  <default-char-variable>
        |  "SIGN"  "="  <default-char-variable>
        |  "SIZE"  "="  <int-variable>
        |  "STREAM="  <default-char-variable>
        |  "UNFORMATTED="  <default-char-variable>
        |  "WRITE="  <default-char-variable>
    ] }

    #`[R1301] rule format-stmt {:i [
        |  "FORMAT"  <format-specification>
    ] <.stmt-separator> }

    #`[R1302] rule format-specification {:i [
        |  "("  [  <format-items> ]?  ")"
        |  "("  [  <format-items> "," ]?  <unlimited-format-item>  ")"
    ] }

    #`[R1303] rule format-items {:i [
        |  <format-item>  [  [  "," ]? <format-item> ]*
    ] }

    #`[R1304] rule format-item {:i [
        |  [  <r> ]?  <data-edit-desc>
        |  <control-edit-desc>
        |  <char-string-edit-desc>
        |  [  <r> ]?  "("  <format-items>  ")"
    ] }

    #`[R1305] rule unlimited-format-item {:i [
        |  "*"  "("  <format-items>  ")"
    ] }

    #`[R1306] rule r {:i [
        |  <int-literal-constant>
    ] }

    #`[R1307] rule data-edit-desc {:i [
        |  "I"  <w>  [  "." <m> ]?
        |  "B"  <w>  [  "." <m> ]?
        |  "O"  <w>  [  "." <m> ]?
        |  "Z"  <w>  [  "." <m> ]?
        |  "F"  <w>  "."  <d>
        |  "E"  <w>  "."  <d>  [  "E" <e> ]?
        |  "EN"  <w>  "."  <d>  [  "E" <e> ]?
        |  "ES"  <w>  "."  <d>  [  "E" <e> ]?
        |  "EX"  <w>  "."  <d>  [  "E" <e> ]?
        |  "G"  <w>  [  "." <d> [  "E" <e> ]? ]?
        |  "L"  <w>
        |  "A"  [  <w> ]?
        |  "D"  <w>  "."  <d>
        |  "DT"  [  <char-literal-constant> ]?  [  "(" <v> [ <v> ]* ")" ]?
    ] }

    #`[R1308] rule w {:i [
        |  <int-literal-constant>
    ] }

    #`[R1309] rule m {:i [
        |  <int-literal-constant>
    ] }

    #`[R1310] rule d {:i [
        |  <int-literal-constant>
    ] }

    #`[R1311] rule e {:i [
        |  <int-literal-constant>
    ] }

    #`[R1312] rule v {:i [
        |  <signed-int-literal-constant>
    ] }

    #`[R1313] rule control-edit-desc {:i [
        |  <position-edit-desc>
        |  [  <r> ]?  "/"
        |  ":"
        |  <sign-edit-desc>
        |  <k>  "P"
        |  <blank-interp-edit-desc>
        |  <round-edit-desc>
        |  <decimal-edit-desc>
    ] }

    #`[R1314] rule k {:i [
        |  <signed-int-literal-constant>
    ] }

    #`[R1315] rule position-edit-desc {:i [
        |  "T"  <n>
        |  "TL"  <n>
        |  "TR"  <n>
        |  <n>  "X"
    ] }

    #`[R1316] rule n {:i [
        |  <int-literal-constant>
    ] }

    #`[R1317] rule sign-edit-desc {:i [
        |  "SS"
        |  "SP"
        |  "S"
    ] }

    #`[R1318] rule blank-interp-edit-desc {:i [
        |  "BN"
        |  "BZ"
    ] }

    #`[R1319] rule round-edit-desc {:i [
        |  "RU"
        |  "RD"
        |  "RZ"
        |  "RN"
        |  "RC"
        |  "RP"
    ] }

    #`[R1320] rule decimal-edit-desc {:i [
        |  "DC"
        |  "DP"
    ] }

    #`[R1321] rule char-string-edit-desc {:i [
        |  <char-literal-constant>
    ] }

    #`[R1322] rule hex-digit-string {:i [
        |  <hex-digit>  [  <hex-digit> ]*
    ] }

    #`[R1402] rule program-stmt {:i [
        |  "PROGRAM"  <name>
    ] <.stmt-separator> }

    #`[R1403] rule end-program-stmt {:i [
        |  "END"  [  "PROGRAM" [  <name> ]? ]?
    ] <.stmt-separator> }

    #`[R1405] rule module-stmt {:i [
        |  "MODULE"  <name>
    ] <.stmt-separator> }

    #`[R1406] rule end-module-stmt {:i [
        |  "END"  [  "MODULE" [  <name> ]? ]?
    ] <.stmt-separator> }

    #`[R1409] rule use-stmt {:i [
        |  "USE"  [  [  "," <module-nature> ]? "::" ]?  <name>  [  "," <rename> [ <rename> ]* ]?
        |  "USE"  [  [  "," <module-nature> ]? "::" ]?  <name>  ","  "ONLY:"  [  <only> [ <only> ]* ]?
    ] <.stmt-separator> }

    #`[R1410] rule module-nature {:i [
        |  "INTRINSIC"
        |  "NON_INTRINSIC"
    ] }

    #`[R1411] rule rename {:i [
        |  <name>  "=>"  <name>
        |  "OPERATOR("  <local-defined-operator>  ")"  "=>"  "OPERATOR("  <use-defined-operator>  ")"
    ] }

    #`[R1412] rule only {:i [
        |  <generic-spec>
        |  <name>
        |  <rename>
    ] }

    #`[R1413] rule only-use-name {:i [
        |  <name>
    ] }

    #`[R1414] rule local-defined-operator {:i [
        |  <defined-unary-op>
        |  <defined-binary-op>
    ] }

    #`[R1415] rule use-defined-operator {:i [
        |  <defined-unary-op>
        |  <defined-binary-op>
    ] }

    #`[R1417] rule submodule-stmt {:i [
        |  "SUBMODULE("  <parent-identifier>  ")"  <name>
    ] <.stmt-separator> }

    #`[R1418] rule parent-identifier {:i [
        |  <name>  [  ":" <name> ]?
    ] }

    #`[R1419] rule end-submodule-stmt {:i [
        |  "END"  [  "SUBMODULE" [  <name> ]? ]?
    ] <.stmt-separator> }

    #`[R1421] rule block-data-stmt {:i [
        |  "BLOCKDATA"  [  <name> ]?
    ] <.stmt-separator> }

    #`[R1422] rule end-block-data-stmt {:i [
        |  "END"  [  "BLOCKDATA" [  <name> ]? ]?
    ] <.stmt-separator> }

    #`[R1501] rule interface-block {:i [
        |  <interface-stmt>  [  <interface-specification> ]*  <end-interface-stmt>
    ] }

    #`[R1502] rule interface-specification {:i [
        |  <interface-body>
        |  <procedure-stmt>
    ] }

    #`[R1503] rule interface-stmt {:i [
        |  "INTERFACE"  [  <generic-spec> ]?
        |  "ABSTRACTINTERFACE"
    ] <.stmt-separator> }

    #`[R1504] rule end-interface-stmt {:i [
        |  "ENDINTERFACE"  [  <generic-spec> ]?
    ] <.stmt-separator> }

    #`[R1505] rule interface-body {:i [
        |  <function-stmt>  [  <specification-part> ]?  <end-function-stmt>
        |  <subroutine-stmt>  [  <specification-part> ]?  <end-subroutine-stmt>
    ] }

    #`[R1506] rule procedure-stmt {:i [
        |  [  "MODULE" ]?  "PROCEDURE"  [  "::" ]?  <specific-procedure> [ <specific-procedure> ]*
    ] <.stmt-separator> }

    #`[R1507] rule specific-procedure {:i [
        |  <name>
    ] }

    #`[R1508] rule generic-spec {:i [
        |  <name>
        |  "OPERATOR("  <defined-operator>  ")"
        |  "ASSIGNMENT"  "("  "="  ")"
        |  <defined-io-generic-spec>
    ] }

    #`[R1509] rule defined-io-generic-spec {:i [
        |  "READ(FORMATTED)"
        |  "READ(UNFORMATTED)"
        |  "WRITE(FORMATTED)"
        |  "WRITE(UNFORMATTED)"
    ] }

    #`[R1510] rule generic-stmt {:i [
        |  "GENERIC"  [  "," <access-spec> ]?  "::"  <generic-spec>  "=>"  <specific-procedure> [ <specific-procedure> ]*
    ] <.stmt-separator> }

    #`[R1511] rule external-stmt {:i [
        |  "EXTERNAL"  [  "::" ]?  <external-name> [ <external-name> ]*
    ] <.stmt-separator> }

    #`[R1512] rule procedure-declaration-stmt {:i [
        |  "PROCEDURE("  [  <proc-interface> ]?  ")"  [  [  "," <proc-attr-spec> ]* "::" ]?  <proc-decl> [ <proc-decl> ]*
    ] <.stmt-separator> }

    #`[R1513] rule proc-interface {:i [
        |  <name>
        |  <declaration-type-spec>
    ] }

    #`[R1514] rule proc-attr-spec {:i [
        |  <access-spec>
        |  <proc-language-binding-spec>
        |  "INTENT"  "("  <intent-spec>  ")"
        |  "OPTIONAL"
        |  "POINTER"
        |  "PROTECTED"
        |  "SAVE"
    ] }

    #`[R1515] rule proc-decl {:i [
        |  <name>  [  "=>" <proc-pointer-init> ]?
    ] }

    #`[R1516] rule interface-name {:i [
        |  <name>
    ] }

    #`[R1517] rule proc-pointer-init {:i [
        |  <null-init>
        |  <initial-proc-target>
    ] }

    #`[R1518] rule initial-proc-target {:i [
        |  <name>
    ] }

    #`[R1519] rule intrinsic-stmt {:i [
        |  "INTRINSIC"  [  "::" ]?  <intrinsic-procedure-name> [ <intrinsic-procedure-name> ]*
    ] <.stmt-separator> }

    #`[R1520] rule function-reference {:i [
        |  <procedure-designator>  "("  [  <actual-arg-spec> [ <actual-arg-spec> ]* ]?  ")"
    ] }

    #`[R1521] rule call-stmt {:i [
        |  "CALL"  <procedure-designator>  [  "(" [  <actual-arg-spec> [ <actual-arg-spec> ]* ]? ")" ]?
    ] <.stmt-separator> }

    #`[R1522] rule procedure-designator {:i [
        |  <name>
        |  <proc-component-ref>
        |  <data-ref>  "%"  <name>
    ] }

    #`[R1523] rule actual-arg-spec {:i [
        |  [  <keyword> "=" ]?  <actual-arg>
    ] }

    #`[R1524] rule actual-arg {:i [
        |  <expr>
        |  <variable>
        |  <name>
        |  <proc-component-ref>
        |  <alt-return-spec>
    ] }

    #`[R1525] rule alt-return-spec {:i [
        |  "*"  <label>
    ] }

    #`[R1526] rule prefix {:i [
        |  <prefix-spec>  [  <prefix-spec> ]*
    ] }

    #`[R1527] rule prefix-spec {:i [
        |  <declaration-type-spec>
        |  "ELEMENTAL"
        |  "IMPURE"
        |  "MODULE"
        |  "NON_RECURSIVE"
        |  "PURE"
        |  "RECURSIVE"
    ] }

    #`[R1528] rule proc-language-binding-spec {:i [
        |  <language-binding-spec>
    ] }

    #`[R1530] rule function-stmt {:i [
        |  [  <prefix> ]?  "FUNCTION"  <name>  "("  [  <dummy-arg-name> [ <dummy-arg-name> ]* ]?  ")"  [  <suffix> ]?
    ] <.stmt-separator> }

    #`[R1531] rule dummy-arg-name {:i [
        |  <name>
    ] }

    #`[R1532] rule suffix {:i [
        |  <proc-language-binding-spec>  [  "RESULT" "(" <name> ")" ]?
        |  "RESULT"  "("  <name>  ")"  [  <proc-language-binding-spec> ]?
    ] }

    #`[R1533] rule end-function-stmt {:i [
        |  "END"  [  "FUNCTION" [  <name> ]? ]?
    ] <.stmt-separator> }

    #`[R1535] rule subroutine-stmt {:i [
        |  [  <prefix> ]?  "SUBROUTINE"  <name>  [  "(" [  <dummy-arg> [ <dummy-arg> ]* ]? ")" [  <proc-language-binding-spec> ]? ]?
    ] <.stmt-separator> }

    #`[R1536] rule dummy-arg {:i [
        |  <name>
        |  "*"
    ] }

    #`[R1537] rule end-subroutine-stmt {:i [
        |  "END"  [  "SUBROUTINE" [  <name> ]? ]?
    ] <.stmt-separator> }

    #`[R1539] rule mp-subprogram-stmt {:i [
        |  "MODULE"  "PROCEDURE"  <name>
    ] <.stmt-separator> }

    #`[R1540] rule end-mp-subprogram-stmt {:i [
        |  "END"  [  "PROCEDURE" [  <name> ]? ]?
    ] <.stmt-separator> }

    #`[R1541] rule entry-stmt {:i [
        |  "ENTRY"  <name>  [  "(" [  <dummy-arg> [ <dummy-arg> ]* ]? ")" [  <suffix> ]? ]?
    ] <.stmt-separator> }

    #`[R1542] rule return-stmt {:i [
        |  "RETURN"  [  <int-expr> ]?
    ] <.stmt-separator> }

    #`[R1543] rule contains-stmt {:i [
        |  "CONTAINS"
    ] <.stmt-separator> }

    #`[R1544] rule stmt-function-stmt {:i [
        |  <name>  "("  [  <dummy-arg-name> [ <dummy-arg-name> ]* ]?  ")"  "="  <expr>
    ] <.stmt-separator> }

}

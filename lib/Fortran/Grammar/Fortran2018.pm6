grammar Fortran2018 {
    token TOP { $<program> }

    #`[R401] token xyz-list {
        |  $<xyz>  [  "," $<xyz> ]*
    }

    #`[R402] token xyz-name {
        |  $<name>
    }

    #`[R403] token scalar-xyz {
        |  $<xyz>
    }

    #`[C401: (R403) scalar-xyz shall be scalar.]

    #`[R501] token program {
        |  $<program-unit>  [  $<program-unit> ]*
    }

    #`[R502] token program-unit {
        |  $<main-program>
        |  $<external-subprogram>
        |  $<module>
        |  $<submodule>
        |  $<block-data>
    }

    #`[R1401] token main-program {
        |  [  $<program-stmt> ]?  [  $<specification-part> ]?  [  $<execution-part> ]?  [  $<internal-subprogram-part> ]?  $<end-program-stmt>
    }

    #`[R503] token external-subprogram {
        |  $<function-subprogram>
        |  $<subroutine-subprogram>
    }

    #`[R1529] token function-subprogram {
        |  $<function-stmt>  [  $<specification-part> ]?  [  $<execution-part> ]?  [  $<internal-subprogram-part> ]?  $<end-function-stmt>
    }

    #`[R1534] token subroutine-subprogram {
        |  $<subroutine-stmt>  [  $<specification-part> ]?  [  $<execution-part> ]?  [  $<internal-subprogram-part> ]?  $<end-subroutine-stmt>
    }

    #`[R1404] token module {
        |  $<module-stmt>  [  $<specification-part> ]?  [  $<module-subprogram-part> ]?  $<end-module-stmt>
    }

    #`[R1416] token submodule {
        |  $<submodule-stmt>  [  $<specification-part> ]?  [  $<module-subprogram-part> ]?  $<end-submodule-stmt>
    }

    #`[R1420] token block-data {
        |  $<block-data-stmt>  [  $<specification-part> ]?  $<end-block-data-stmt>
    }

    #`[R504] token specification-part {
        |  [  $<use-stmt> ]*  [  $<import-stmt> ]*  [  $<implicit-part> ]?  [  $<declaration-construct> ]*
    }

    #`[R505] token implicit-part {
        |  [  $<implicit-part-stmt> ]*  $<implicit-stmt>
    }

    #`[R506] token implicit-part-stmt {
        |  $<implicit-stmt>
        |  $<parameter-stmt>
        |  $<format-stmt>
        |  $<entry-stmt>
    }

    #`[R507] token declaration-construct {
        |  $<specification-construct>
        |  $<data-stmt>
        |  $<format-stmt>
        |  $<entry-stmt>
        |  $<stmt-function-stmt>
    }

    #`[R508] token specification-construct {
        |  $<derived-type-def>
        |  $<enum-def>
        |  $<generic-stmt>
        |  $<interface-block>
        |  $<parameter-stmt>
        |  $<procedure-declaration-stmt>
        |  $<other-specification-stmt>
        |  $<type-declaration-stmt>
    }

    #`[R509] token execution-part {
        |  $<executable-construct>  [  $<execution-part-construct> ]*
    }

    #`[R510] token execution-part-construct {
        |  $<executable-construct>
        |  $<format-stmt>
        |  $<entry-stmt>
        |  $<data-stmt>
    }

    #`[R511] token internal-subprogram-part {
        |  $<contains-stmt>  [  $<internal-subprogram> ]*
    }

    #`[R512] token internal-subprogram {
        |  $<function-subprogram>
        |  $<subroutine-subprogram>
    }

    #`[R1407] token module-subprogram-part {
        |  $<contains-stmt>  [  $<module-subprogram> ]*
    }

    #`[R1408] token module-subprogram {
        |  $<function-subprogram>
        |  $<subroutine-subprogram>
        |  $<separate-module-subprogram>
    }

    #`[R1538] token separate-module-subprogram {
        |  $<mp-subprogram-stmt>  [  $<specification-part> ]?  [  $<execution-part> ]?  [  $<internal-subprogram-part> ]?  $<end-mp-subprogram-stmt>
    }

    #`[R513] token other-specification-stmt {
        |  $<access-stmt>
        |  $<allocatable-stmt>
        |  $<asynchronous-stmt>
        |  $<bind-stmt>
        |  $<codimension-stmt>
        |  $<contiguous-stmt>
        |  $<dimension-stmt>
        |  $<external-stmt>
        |  $<intent-stmt>
        |  $<intrinsic-stmt>
        |  $<namelist-stmt>
        |  $<optional-stmt>
        |  $<pointer-stmt>
        |  $<protected-stmt>
        |  $<save-stmt>
        |  $<target-stmt>
        |  $<volatile-stmt>
        |  $<value-stmt>
        |  $<common-stmt>
        |  $<equivalence-stmt>
    }

    #`[R514] token executable-construct {
        |  $<action-stmt>
        |  $<associate-construct>
        |  $<block-construct>
        |  $<case-construct>
        |  $<change-team-construct>
        |  $<critical-construct>
        |  $<do-construct>
        |  $<if-construct>
        |  $<select-rank-construct>
        |  $<select-type-construct>
        |  $<where-construct>
        |  $<forall-construct>
    }

    #`[R515] token action-stmt {
        |  $<allocate-stmt>
        |  $<assignment-stmt>
        |  $<backspace-stmt>
        |  $<call-stmt>
        |  $<close-stmt>
        |  $<continue-stmt>
        |  $<cycle-stmt>
        |  $<deallocate-stmt>
        |  $<endfile-stmt>
        |  $<error-stop-stmt>
        |  $<event-post-stmt>
        |  $<event-wait-stmt>
        |  $<exit-stmt>
        |  $<fail-image-stmt>
        |  $<flush-stmt>
        |  $<form-team-stmt>
        |  $<goto-stmt>
        |  $<if-stmt>
        |  $<inquire-stmt>
        |  $<lock-stmt>
        |  $<nullify-stmt>
        |  $<open-stmt>
        |  $<pointer-assignment-stmt>
        |  $<print-stmt>
        |  $<read-stmt>
        |  $<return-stmt>
        |  $<rewind-stmt>
        |  $<stop-stmt>
        |  $<sync-all-stmt>
        |  $<sync-images-stmt>
        |  $<sync-memory-stmt>
        |  $<sync-team-stmt>
        |  $<unlock-stmt>
        |  $<wait-stmt>
        |  $<where-stmt>
        |  $<write-stmt>
        |  $<computed-goto-stmt>
        |  $<forall-stmt>
    }

    #`[R516] token keyword {
        |  $<name>
    }

    #`[R601] token alphanumeric-character {
        |  $<letter>
        |  $<digit>
        |  $<underscore>
    }

    #`[R602] token underscore {
        |  "_"
    }

    #`[R603] token name {
        |  $<letter>  [  $<alphanumeric-character> ]*
    }

    #`[C601: (R603) The maximum length of a name is 63 characters.]

    #`[R604] token constant {
        |  $<literal-constant>
        |  $<named-constant>
    }

    #`[R605] token literal-constant {
        |  $<int-literal-constant>
        |  $<real-literal-constant>
        |  $<complex-literal-constant>
        |  $<logical-literal-constant>
        |  $<char-literal-constant>
        |  $<boz-literal-constant>
    }

    #`[R606] token named-constant {
        |  $<name>
    }

    #`[R607] token int-constant {
        |  $<constant>
    }

    #`[C602: (R607) int-constant shall be of type integer.]

    #`[R608] token intrinsic-operator {
        |  $<power-op>
        |  $<mult-op>
        |  $<add-op>
        |  $<concat-op>
        |  $<rel-op>
        |  $<not-op>
        |  $<and-op>
        |  $<or-op>
        |  $<equiv-op>
    }

    #`[R1007] token power-op {
        |  "**"
    }

    #`[R1008] token mult-op {
        |  "*"
        |  "/"
    }

    #`[R1009] token add-op {
        |  "+"
        |  "-"
    }

    #`[R1011] token concat-op {
        |  "//"
    }

    #`[R1013] token rel-op {
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
    }

    #`[R1018] token not-op {
        |  ".NOT."
    }

    #`[R1019] token and-op {
        |  ".AND."
    }

    #`[R1020] token or-op {
        |  ".OR."
    }

    #`[R1021] token equiv-op {
        |  ".EQV."
        |  ".NEQV."
    }

    #`[R609] token defined-operator {
        |  $<defined-unary-op>
        |  $<defined-binary-op>
        |  $<extended-intrinsic-op>
    }

    #`[R1003] token defined-unary-op {
        |  "."  $<letter>  [  $<letter> ]*  "."
    }

    #`[R1023] token defined-binary-op {
        |  "."  $<letter>  [  $<letter> ]*  "."
    }

    #`[R610] token extended-intrinsic-op {
        |  $<intrinsic-operator>
    }

    #`[R611] token label {
        |  $<digit>  [  $<digit> [  $<digit> [  $<digit> [  $<digit> ]? ]? ]? ]?
    }

    #`[C603: (R611) At least one digit in a label shall be nonzero.]

    #`[R701] token type-param-value {
        |  $<scalar-int-expr>
        |  "*"
        |  ":"
    }

    #`[C701: (R701) The type-param-value for a kind type parameter shall be a constant expression.]

    #`[C702: (R701) A colon shall not be used as a type-param-value except in the declaration of an entity that has the POINTER | ALLOCATABLE attribute.]

    #`[R702] token type-spec {
        |  $<intrinsic-type-spec>
        |  $<derived-type-spec>
    }

    #`[C703: (R702) The derived-type-spec shall not specify an abstract type (7.5.7).]

    #`[R703] token declaration-type-spec {
        |  $<intrinsic-type-spec>
        |  "TYPE"  "("  $<intrinsic-type-spec>  ")"
        |  "TYPE"  "("  $<derived-type-spec>  ")"
        |  "CLASS"  "("  $<derived-type-spec>  ")"
        |  "CLASS"  "("  "*"  ")"
        |  "TYPE"  "("  "*"  ")"
    }

    #`[C704: (R703) In a declaration-type-spec, every type-param-value that is not a colon | an asterisk shall be a specification-expr.]

    #`[C705: (R703) In a declaration-type-spec that uses the CLASS keyword, derived-type-spec shall specify an ex- tensible type (7.5.7).]

    #`[C706: (R703) TYPE(derived-type-spec) shall not specify an abstract type (7.5.7).]

    #`[C707: (R702) In TYPE(intrinsic-type-spec) the intrinsic-type-spec shall not end with a comma.]

    #`[C708: An entity declared with the CLASS keyword shall be a dummy argument | have the ALLOCATABLE | POINTER attribute.]

    #`[C709: An assumed-type entity shall be a dummy data object that does not have the ALLOCATABLE, CODI- MENSION, INTENT (OUT), POINTER, | VALUE attribute and is not an explicit-shape array.]

    #`[C710: An assumed-type variable name shall not appear in a designator | expression except as an actual argument corresponding to a dummy argument that is assumed-type, | as the first argument to the intrinsic function IS_CONTIGUOUS, LBOUND, PRESENT, RANK, SHAPE, SIZE, | UBOUND, | the function C_LOC from the intrinsic module ISO_C_BINDING.]

    #`[C711: An assumed-type actual argument that corresponds to an assumed-rank dummy argument shall be assumed-shape | assumed-rank.]

    #`[R704] token intrinsic-type-spec {
        |  $<integer-type-spec>
        |  "REAL"  [  $<kind-selector> ]?
        |  "DOUBLE"  "PRECISION"
        |  "COMPLEX"  [  $<kind-selector> ]?
        |  "CHARACTER"  [  $<char-selector> ]?
        |  "LOGICAL"  [  $<kind-selector> ]?
    }

    #`[R705] token integer-type-spec {
        |  "INTEGER"  [  $<kind-selector> ]?
    }

    #`[R706] token kind-selector {
        |  "("  [  "KIND" "=" ]?  $<scalar-int-constant-expr>  ")"
    }

    #`[C712: (R706) The value of scalar-int-constant-expr shall be nonnegative and shall specify a representation method that exists on the processor.]

    #`[R707] token signed-int-literal-constant {
        |  [  $<sign> ]?  $<int-literal-constant>
    }

    #`[R708] token int-literal-constant {
        |  $<digit-string>  [  "_" $<kind-param> ]?
    }

    #`[R709] token kind-param {
        |  $<digit-string>
        |  $<scalar-int-constant-name>
    }

    #`[R710] token signed-digit-string {
        |  [  $<sign> ]?  $<digit-string>
    }

    #`[R711] token digit-string {
        |  $<digit>  [  $<digit> ]*
    }

    #`[R712] token sign {
        |  "+"
        |  "-"
    }

    #`[C713: (R709) A scalar-int-constant-name shall be a named constant of type integer.]

    #`[C714: (R709) The value of kind-param shall be nonnegative.]

    #`[C715: (R708) The value of kind-param shall specify a representation method that exists on the processor.]

    #`[R713] token signed-real-literal-constant {
        |  [  $<sign> ]?  $<real-literal-constant>
    }

    #`[R714] token real-literal-constant {
        |  $<significand>  [  $<exponent-letter> $<exponent> ]?  [  "_" $<kind-param> ]?
        |  $<digit-string>  $<exponent-letter>  $<exponent>  [  "_" $<kind-param> ]?
    }

    #`[R715] token significand {
        |  $<digit-string>  "."  [  $<digit-string> ]?
        |  "."  $<digit-string>
    }

    #`[R716] token exponent-letter {
        |  "E"
        |  "D"
    }

    #`[R717] token exponent {
        |  $<signed-digit-string>
    }

    #`[C716: (R714) If both kind-param and exponent-letter appear, exponent-letter shall be E.]

    #`[C717: (R714) The value of kind-param shall specify an approximation method that exists on the processor.]

    #`[R718] token complex-literal-constant {
        |  "("  $<real-part>  ","  $<imag-part>  ")"
    }

    #`[R719] token real-part {
        |  $<signed-int-literal-constant>
        |  $<signed-real-literal-constant>
        |  $<named-constant>
    }

    #`[R720] token imag-part {
        |  $<signed-int-literal-constant>
        |  $<signed-real-literal-constant>
        |  $<named-constant>
    }

    #`[C718: (R718) Each named constant in a complex literal constant shall be of type integer | real.]

    #`[R721] token char-selector {
        |  $<length-selector>
        |  "("  "LEN"  "="  $<type-param-value>  ","  "KIND"  "="  $<scalar-int-constant-expr>  ")"
        |  "("  $<type-param-value>  ","  [  "KIND" "=" ]?  $<scalar-int-constant-expr>  ")"
        |  "("  "KIND"  "="  $<scalar-int-constant-expr>  [  "," "LEN" "=" $<type-param-value> ]?  ")"
    }

    #`[R722] token length-selector {
        |  "("  [  "LEN" "=" ]?  $<type-param-value>  ")"
        |  "*"  $<char-length>  [  "," ]?
    }

    #`[R723] token char-length {
        |  "("  $<type-param-value>  ")"
        |  $<int-literal-constant>
    }

    #`[C719: (R721) The value of scalar-int-constant-expr shall be nonnegative and shall specify a representation method that exists on the processor.]

    #`[C720: (R723) The int-literal-constant shall not include a kind-param.]

    #`[C721: (R723) A type-param-value in a char-length shall be a colon, asterisk, | specification-expr.]

    #`[C722: (R721 R722 R723) A type-param-value of * shall be used only *> to declare a dummy argument, *> to declare a named constant, *> in the type-spec of an ALLOCATE statement wherein each allocate-object is a dummy argument of type CHARACTER with an assumed character length, *> in the type-spec | derived-type-spec of a type guard statement (11.1.11), | *> in an external function, to declare the character length parameter of the function result.]

    #`[C723: Afunctionnameshallnotbedeclaredwithanasterisktype-param-value unlessitisoftypeCHARACTER and is the name of a dummy function | the name of the result of an external function.]

    #`[C724: Afunction name declared with an asterisk type-param-value shall not be an array, a pointer, elemental, | pure. A function name declared with an asterisk type-param-value shall not have the RECURSIVE attribute.]

    #`[C725: (R722) The optional comma in a length-selector is permitted only in a declaration-type-spec in a type-declaration-stmt.]

    #`[C726: (R722) The optional comma in a length-selector is permitted only if no double-colon separator appears in the type- declaration-stmt.]

    #`[C727: (R721) The length specified for a character statement function | for a statement function dummy argument of type character shall be a constant expression.]

    #`[R724] token char-literal-constant {
        |  [  $<kind-param> "_" ]?  "'"  [  $<rep-char> ]*  "'"
        |  [  $<kind-param> "_" ]?  """  [  $<rep-char> ]*  """
    }

    #`[C728: (R724) The value of kind-param shall specify a representation method that exists on the processor.]

    #`[R725] token logical-literal-constant {
        |  ".TRUE."  [  "_" $<kind-param> ]?
        |  ".FALSE."  [  "_" $<kind-param> ]?
    }

    #`[C729: (R725) The value of kind-param shall specify a representation method that exists on the processor.]

    #`[R726] token derived-type-def {
        |  $<derived-type-stmt>  [  $<type-param-def-stmt> ]*  [  $<private-or-sequence> ]*  [  $<component-part> ]?  [  $<type-bound-procedure-part> ]?  $<end-type-stmt>
    }

    #`[R727] token derived-type-stmt {
        |  "TYPE"  [  [  "," $<type-attr-spec-list> ]? "::" ]?  $<type-name>  [  "(" $<type-param-name-list> ")" ]?
    }

    #`[R728] token type-attr-spec {
        |  "ABSTRACT"
        |  $<access-spec>
        |  "BIND"  "(C)"
        |  "EXTENDS("  $<parent-type-name>  ")"
    }

    #`[C730: (R727) A derived type type-name shall not be DOUBLEPRECISION | the same as the name of any intrinsic type defined in this document.]

    #`[C731: (R727) The same type-attr-spec shall not appear more than once in a given derived-type-stmt.]

    #`[C732: The same type-param-name shall not appear more than once in a given derived-type-stmt.]

    #`[C733: (R728) A parent-type-name shall be the name of a previously defined extensible type (7.5.7).]

    #`[C734: (R726) If the type definition contains | inherits (7.5.7.2) a deferred type-bound procedure (7.5.5), AB- STRACTshall appear.]

    #`[C735: (R726) If ABSTRACT appears, the type shall be extensible.]

    #`[C736: (R726) If EXTENDS appears, SEQUENCE shall not appear.]

    #`[C737: (R726) If EXTENDS appears and the type being defined has a coarray ultimate component, its parent type shall have a coarray ultimate component.]

    #`[C738: (R726) If EXTENDS appears and the type being defined has a potential subobject component of type EVENT_TYPE | LOCK_TYPE from the intrinsic module ISO_FORTRAN_ENV, its parent type shall be EVENT_TYPE | LOCK_TYPE | have a potential subobject component of type EVENT_- TYPEorLOCK_TYPE.]

    #`[R729] token private-or-sequence {
        |  $<private-components-stmt>
        |  $<sequence-stmt>
    }

    #`[C739: (R726) The same private-or-sequence shall not appear more than once in a given derived-type-def.]

    #`[R730] token end-type-stmt {
        |  "ENDTYPE"  [  $<type-name> ]?
    }

    #`[C740: (R730) If END TYPE is followed by a type-name, the type-name shall be the same as that in the]

    #`[R731] token sequence-stmt {
        |  "SEQUENCE"
    }

    #`[C741: (R726) If SEQUENCE appears, the type shall have at least one component, each data component shall be declared to be of an intrinsic type | of a sequence type, the derived type shall not have any type parameter, and a type-bound-procedure-part shall not appear.]

    #`[R732] token type-param-def-stmt {
        |  $<integer-type-spec>  ","  $<type-param-attr-spec>  "::"  $<type-param-decl-list>
    }

    #`[R733] token type-param-decl {
        |  $<type-param-name>  [  "=" $<scalar-int-constant-expr> ]?
    }

    #`[C742: (R732) A type-param-name in a type-param-def-stmt in a derived-type-def shall be one of the type-param- names in the derived-type-stmt of that derived-type-def.]

    #`[C743: (R732) Each type-param-name in the derived-type-stmt in a derived-type-def shall appear exactly once as a type-param-name in a type-param-def-stmt in that derived-type-def.]

    #`[R734] token type-param-attr-spec {
        |  "KIND"
        |  "LEN"
    }

    #`[R735] token component-part {
        |  [  $<component-def-stmt> ]*
    }

    #`[R736] token component-def-stmt {
        |  $<data-component-def-stmt>
        |  $<proc-component-def-stmt>
    }

    #`[R737] token data-component-def-stmt {
        |  $<declaration-type-spec>  [  [  "," $<component-attr-spec-list> ]? "::" ]?  $<component-decl-list>
    }

    #`[R738] token component-attr-spec {
        |  $<access-spec>
        |  "ALLOCATABLE"
        |  "CODIMENSION"  $<lbracket>  $<coarray-spec>  $<rbracket>
        |  "CONTIGUOUS"
        |  "DIMENSION"  "("  $<component-array-spec>  ")"
        |  "POINTER"
    }

    #`[R739] token component-decl {
        |  $<component-name>  [  "(" $<component-array-spec> ")" ]?  [  $<lbracket> $<coarray-spec> $<rbracket> ]?  [  "*" $<char-length> ]?  [  $<component-initialization> ]?
    }

    #`[R740] token component-array-spec {
        |  $<explicit-shape-spec-list>
        |  $<deferred-shape-spec-list>
    }

    #`[C744: (R737) No component-attr-spec shall appear more than once in a given component-def-stmt.]

    #`[C745: (R737) If neither the POINTER nor the ALLOCATABLE attribute is specified, the declaration-type-spec in the component-def-stmt shall specify an intrinsic type | a previously defined derived type.]

    #`[C746: (R737) If the POINTER | ALLOCATABLE attribute is specified, each component-array-spec shall be a deferred-shape-spec-list.]

    #`[C747: (R737) If a coarray-spec appears, it shall be a deferred-coshape-spec-list and the component shall have the ALLOCATABLE attribute.]

    #`[C748: (R737) If a coarray-spec appears, the component shall not be of type C_PTR | C_FUNPTR from the intrinsic module ISO_C_BINDING (18.2), | of type TEAM_TYPE from the intrinsic module ISO_FORTRAN_ENV(16.10.2).]

    #`[C749: A data component whose type has a coarray ultimate component shall be a nonpointer nonallocatable scalar and shall not be a coarray.]

    #`[C750: (R737) If neither the POINTER nor the ALLOCATABLE attribute is specified, each component-array- spec shall be an explicit-shape-spec-list.]

    #`[C751: (R740) Each bound in the explicit-shape-spec shall be a specification expression in which there are no ref- erences to specification functions | the intrinsic functions ALLOCATED, ASSOCIATED, EXTENDS_- TYPE_OF, PRESENT, | SAME_TYPE_AS, every specification inquiry reference is a constant ex- pression, and the value does not depend on the value of a variable.]

    #`[C752: (R737) A component shall not have both the ALLOCATABLE and POINTER attributes.]

    #`[C753: (R737) If the CONTIGUOUS attribute is specified, the component shall be an array with the POINTER attribute.]

    #`[C754: (R739) The * char-length option is permitted only if the component is of type character.]

    #`[C755: (R736) Each type-param-value within a component-def-stmt shall be a colon | a specification expres- sion in which there are no references to specification functions | the intrinsic functions ALLOCATED, ASSOCIATED,EXTENDS_TYPE_OF,PRESENT,orSAME_TYPE_AS,everyspecificationinquiry reference is a constant expression, and the value does not depend on the value of a variable.  NOTE7.24 Because a type parameter is not an object, a type-param-value | a bound in an explicit-shape-spec can contain a type-param-name.]

    #`[R741] token proc-component-def-stmt {
        |  "PROCEDURE("  [  $<proc-interface> ]?  ")"  ","  $<proc-component-attr-spec-list>  "::"  $<proc-decl-list>
    }

    #`[R742] token proc-component-attr-spec {
        |  $<access-spec>
        |  "NOPASS"
        |  "PASS"  [  "(" $<arg-name> ")" ]?
        |  "POINTER"
    }

    #`[C756: (R741) The same proc-component-attr-spec shall not appear more than once in a given proc-component- def-stmt.]

    #`[C757: (R741) POINTER shall appear in each proc-component-attr-spec-list.]

    #`[C758: (R741) If the procedure pointer component has an implicit interface | has no arguments, NOPASS shall be specified.]

    #`[C759: (R741)IfPASS(arg-name)appears,theinterfaceoftheprocedurepointercomponentshallhaveadummy argument named arg-name.]

    #`[C760: (R741) PASS and NOPASS shall not both appear in the same proc-component-attr-spec-list.]

    #`[C761: The passed-object dummy argument shall be a scalar, nonpointer, nonallocatable dummy data object with the same declared type as the type being defined; all of its length type parameters shall be assumed; it shall be polymorphic (7.3.2.3) if and only if the type being defined is extensible (7.5.7). It shall not have the VALUE attribute.]

    #`[R743] token component-initialization {
        |  "="  $<constant-expr>
        |  "=>"  $<null-init>
        |  "=>"  $<initial-data-target>
    }

    #`[R744] token initial-data-target {
        |  $<designator>
    }

    #`[C762: (R737) If component-initialization appears, a double-colon separator shall appear before the component- decl-list.]

    #`[C763: (R737) If component-initialization appears, every type parameter and array bound of the component shall be a colon | constant expression.]

    #`[C764: (R737) If => appears in component-initialization, POINTER shall appear in the component-attr-spec- list. If = appears in component-initialization, neither POINTER nor ALLOCATABLE shall appear in the component-attr-spec-list.]

    #`[C765: If initial-data-target appears in a component-initialization in a component-decl, component-name shall be data-pointer-initialization compatible with it.]

    #`[C766: A designator that is an initial-data-target shall designate a nonallocatable, noncoindexed variable that has the TARGET and SAVE attributes and does not have a vector subscript. Every subscript, sec- tion subscript, substring starting point, and substring ending point in designator shall be a constant expression.]

    #`[R745] token private-components-stmt {
        |  "PRIVATE"
    }

    #`[C767: (R745) A private-components-stmt is permitted only if the type definition is within the specification part of a module.]

    #`[R746] token type-bound-procedure-part {
        |  $<contains-stmt>  [  $<binding-private-stmt> ]?  [  $<type-bound-proc-binding> ]*
    }

    #`[R747] token binding-private-stmt {
        |  "PRIVATE"
    }

    #`[C768: (R746) A binding-private-stmt is permitted only if the type definition is within the specification part of a module.]

    #`[R748] token type-bound-proc-binding {
        |  $<type-bound-procedure-stmt>
        |  $<type-bound-generic-stmt>
        |  $<final-procedure-stmt>
    }

    #`[R749] token type-bound-procedure-stmt {
        |  "PROCEDURE"  [  [  "," $<binding-attr-list> ]? "::" ]?  $<type-bound-proc-decl-list>
        |  "PROCEDURE("  $<interface-name>  "),"  $<binding-attr-list>  "::"  $<binding-name-list>
    }

    #`[R750] token type-bound-proc-decl {
        |  $<binding-name>  [  "=>" $<procedure-name> ]?
    }

    #`[C769: (R749) If => procedure-name appears in a type-bound-proc-decl, the double-colon separator shall appear.]

    #`[C770: (R750) The procedure-name shall be the name of an accessible module procedure | an external procedure that has an explicit interface.]

    #`[C771: Abinding-name in a type-bound-proc-decl in a derived type definition shall not be the same as any other binding-name within that derived type definition.]

    #`[R751] token type-bound-generic-stmt {
        |  "GENERIC"  [  "," $<access-spec> ]?  "::"  $<generic-spec>  "=>"  $<binding-name-list>
    }

    #`[C772: (R751) Within the specification-part of a module, each type-bound-generic-stmt shall specify, either im- plicitly | explicitly, the same accessibility as every other type-bound-generic-stmt with that generic-spec in the same derived type.]

    #`[C773: (R751) Each binding-name in binding-name-list shall be the name of a specific binding of the type.]

    #`[C774: A binding-name in a type-bound GENERIC statement shall not specify a specific binding that was inherited | specified previously for the same generic identifier in that derived type definition.]

    #`[C775: (R751) If generic-spec is not generic-name, each of its specific bindings shall have a passed-object dummy argument (7.5.4.5).]

    #`[C776: (R751) If generic-spec is OPERATOR ( defined-operator ), the interface of each binding shall be as specified in 15.4.3.4.2.]

    #`[C777: (R751) If generic-spec is ASSIGNMENT ( = ), the interface of each binding shall be as specified in 15.4.3.4.3.]

    #`[C778: (R751) If generic-spec is defined-io-generic-spec, the interface of each binding shall be as specified in 12.6.4.8. The type of the dtv argument shall be type-name.]

    #`[R752] token binding-attr {
        |  $<access-spec>
        |  "DEFERRED"
        |  "NON_OVERRIDABLE"
        |  "NOPASS"
        |  "PASS"  [  "(" $<arg-name> ")" ]?
    }

    #`[C779: (R752) The same binding-attr shall not appear more than once in a given binding-attr-list.]

    #`[C780: (R749) If the interface of the binding has no dummy argument of the type being defined, NOPASS shall appear.]

    #`[C781: (R749) If PASS (arg-name) appears, the interface of the binding shall have a dummy argument named arg-name.]

    #`[C782: (R752) PASS and NOPASS shall not both appear in the same binding-attr-list.]

    #`[C783: (R752) NON_OVERRIDABLE and DEFERRED shall not both appear in the same binding-attr-list.]

    #`[C784: (R752) DEFERRED shall appear if and only if interface-name appears.]

    #`[C785: (R749) An overriding binding (7.5.7.3) shall have the DEFERRED attribute only if the binding it over- rides is deferred.]

    #`[C786: (R749) A binding shall not override an inherited binding (7.5.7.2) that has the NON_OVERRIDABLE attribute.]

    #`[R753] token final-procedure-stmt {
        |  "FINAL"  [  "::" ]?  $<final-subroutine-name-list>
    }

    #`[C787: (R753) A final-subroutine-name shall be the name of a module procedure with exactly one dummy argument. That argument shall be nonoptional and shall be a noncoarray, nonpointer, nonallocatable, nonpolymorphic variable of the derived type being defined. All length type parameters of the dummy argument shall be assumed. The dummy argument shall not have the INTENT (OUT) | VALUE attribute.]

    #`[C788: (R753) A final-subroutine-name shall not be one previously specified as a final subroutine for that type.]

    #`[C789: (R753) A final subroutine shall not have a dummy argument with the same kind type parameters and rank as the dummy argument of another final subroutine of that type.]

    #`[C790: (R753) If a final subroutine has an assumed-rank dummy argument, no other final subroutine of that type shall have a dummy argument with the same kind type parameters.]

    #`[R754] token derived-type-spec {
        |  $<type-name>  [  "(" $<type-param-spec-list> ")" ]?
    }

    #`[R755] token type-param-spec {
        |  [  $<keyword> "=" ]?  $<type-param-value>
    }

    #`[C791: (R754) type-name shall be the name of an accessible derived type.]

    #`[C792: (R754) type-param-spec-list shall appear only if the type is parameterized.]

    #`[C793: (R754) There shall be at most one type-param-spec corresponding to each parameter of the type. If a type parameter does not have a default value, there shall be a type-param-spec corresponding to that type parameter.]

    #`[C794: (R755) The keyword= may be omitted from a type-param-spec only if the keyword= has been omitted from each preceding type-param-spec in the type-param-spec-list.]

    #`[C795: (R755) Each keyword shall be the name of a parameter of the type.]

    #`[C796: (R755) An asterisk may be used as a type-param-value in a type-param-spec only in the declaration of a dummy argument | associate name | in the allocation of a dummy argument.]

    #`[R756] token structure-constructor {
        |  $<derived-type-spec>  "("  [  $<component-spec-list> ]?  ")"
    }

    #`[R757] token component-spec {
        |  [  $<keyword> "=" ]?  $<component-data-source>
    }

    #`[R758] token component-data-source {
        |  $<expr>
        |  $<data-target>
        |  $<proc-target>
    }

    #`[C797: (R756) The derived-type-spec shall not specify an abstract type (7.5.7).]

    #`[C798: (R756) At most one component-spec shall be provided for a component.]

    #`[C799: (R756)Ifacomponent-spec isprovidedforanancestorcomponent, acomponent-spec shallnotbeprovided for any component that is inheritance associated with a subcomponent of that ancestor component.]

    #`[C7100: (R756) A component-spec shall be provided for a nonallocatable component unless it has default initializ- ation | is inheritance associated with a subcomponent of another component for which a component-spec is provided.]

    #`[C7101: (R757) The keyword= may be omitted from a component-spec only if the keyword= has been omitted from each preceding component-spec in the constructor.]

    #`[C7102: (R757) Each keyword shall be the name of a component of the type.]

    #`[C7103: (R756) The type name and all components of the type for which a component-spec appears shall be accessible in the scoping unit containing the structure constructor.]

    #`[C7104: (R756) If derived-type-spec is a type name that is the same as a generic name, the component-spec-list shall not be a valid actual-arg-spec-list for a function reference that is resolvable as a generic reference to that name (15.5.5.2).]

    #`[C7105: (R758) A data-target shall correspond to a data pointer component; a proc-target shall correspond to a procedure pointer component.]

    #`[C7106: (R758) A data-target shall have the same rank as its corresponding component.]

    #`[R759] token enum-def {
        |  $<enum-def-stmt>  $<enumerator-def-stmt>  [  $<enumerator-def-stmt> ]*  $<end-enum-stmt>
    }

    #`[R760] token enum-def-stmt {
        |  "ENUM,BIND(C)"
    }

    #`[R761] token enumerator-def-stmt {
        |  "ENUMERATOR"  [  "::" ]?  $<enumerator-list>
    }

    #`[R762] token enumerator {
        |  $<named-constant>  [  "=" $<scalar-int-constant-expr> ]?
    }

    #`[R763] token end-enum-stmt {
        |  "ENDENUM"
    }

    #`[C7107: (R761) If = appears in an enumerator, a double-colon separator shall appear before the enumerator-list.]

    #`[R764] token boz-literal-constant {
        |  $<binary-constant>
        |  $<octal-constant>
        |  $<hex-constant>
    }

    #`[R765] token binary-constant {
        |  "B'"  $<digit>  [  $<digit> ]*  "'"
        |  "B"  """  $<digit>  [  $<digit> ]*  """
    }

    #`[C7108: (R765) digit shall have one of the values 0 | 1.]

    #`[R766] token octal-constant {
        |  "O'"  $<digit>  [  $<digit> ]*  "'"
        |  "O"  """  $<digit>  [  $<digit> ]*  """
    }

    #`[C7109: (R766) digit shall have one of the values 0 through 7.]

    #`[R767] token hex-constant {
        |  "Z"  "'"  $<hex-digit>  [  $<hex-digit> ]*  "'"
        |  "Z"  """  $<hex-digit>  [  $<hex-digit> ]*  """
    }

    #`[R768] token hex-digit {
        |  $<digit>
        |  "A"
        |  "B"
        |  "C"
        |  "D"
        |  "E"
        |  "F"
    }

    #`[C7110: (R764) A boz-literal-constant shall appear only as a data-stmt-constant in a DATA statement, | where explicitly allowed in subclause 16.9 as an actual argument of an intrinsic procedure.]

    #`[R769] token array-constructor {
        |  "(/"  $<ac-spec>  "/)"
        |  $<lbracket>  $<ac-spec>  $<rbracket>
    }

    #`[R770] token ac-spec {
        |  $<type-spec>  "::"
        |  [  $<type-spec> "::" ]?  $<ac-value-list>
    }

    #`[R773] token ac-value {
        |  $<expr>
        |  $<ac-implied-do>
    }

    #`[R774] token ac-implied-do {
        |  "("  $<ac-value-list>  ","  $<ac-implied-do-control>  ")"
    }

    #`[R775] token ac-implied-do-control {
        |  [  $<integer-type-spec> "::" ]?  $<ac-do-variable>  "="  $<scalar-int-expr>  ","  $<scalar-int-expr>  [  "," $<scalar-int-expr> ]?
    }

    #`[R776] token ac-do-variable {
        |  $<do-variable>
    }

    #`[C7111: (R770) If type-spec is omitted, each ac-value expression in the array-constructor shall have the same declared type and kind type parameters.]

    #`[C7112: (R770) If type-spec specifies an intrinsic type, each ac-value expression in the array-constructor shall be of an intrinsic type that is in type conformance with a variable of type type-spec as specified in Table 10.8.]

    #`[C7113: (R770) If type-spec specifies a derived type, the declared type of each ac-value expression in the array- constructor shall be that derived type and shall have the same kind type parameter values as specified by type-spec.]

    #`[C7114: (R773) An ac-value shall not be unlimited polymorphic.]

    #`[C7115: (R773) The declared type of an ac-value shall not be abstract.]

    #`[C7116: (R774) The ac-do-variable of an ac-implied-do that is in another ac-implied-do shall not appear as the ac-do-variable of the containing ac-implied-do.]

    #`[R801] token type-declaration-stmt {
        |  $<declaration-type-spec>  [  [  "," $<attr-spec> ]* "::" ]?  $<entity-decl-list>
    }

    #`[R802] token attr-spec {
        |  $<access-spec>
        |  "ALLOCATABLE"
        |  "ASYNCHRONOUS"
        |  "CODIMENSION"  $<lbracket>  $<coarray-spec>  $<rbracket>
        |  "CONTIGUOUS"
        |  "DIMENSION"  "("  $<array-spec>  ")"
        |  "EXTERNAL"
        |  "INTENT"  "("  $<intent-spec>  ")"
        |  "INTRINSIC"
        |  $<language-binding-spec>
        |  "OPTIONAL"
        |  "PARAMETER"
        |  "POINTER"
        |  "PROTECTED"
        |  "SAVE"
        |  "TARGET"
        |  "VALUE"
        |  "VOLATILE"
    }

    #`[C801: (R801) The same attr-spec shall not appear more than once in a given type-declaration-stmt.]

    #`[C802: (R801) If a language-binding-spec with a NAME= specifier appears, the entity-decl-list shall consist of a single entity-decl.]

    #`[C803: (R801) If a language-binding-spec is specified, the entity-decl-list shall not contain any procedure names.]

    #`[R803] token entity-decl {
        |  $<object-name>  [  "(" $<array-spec> ")" ]?  [  $<lbracket> $<coarray-spec> $<rbracket> ]?  [  "*" $<char-length> ]?  [  $<initialization> ]?
        |  $<function-name>  [  "*" $<char-length> ]?
    }

    #`[C804: (R803) If the entity is not of type character, * char-length shall not appear.]

    #`[C805: (R801) If initialization appears, a double-colon separator shall appear before the entity-decl-list.]

    #`[C806: (R801) If the PARAMETER keyword appears, initialization shall appear in each entity-decl.]

    #`[C807: (R803) An initialization shall not appear if object-name is a dummy argument, a function result, an object in a named common block unless the type declaration is in a block data program unit, an object in blank common, an allocatable variable, | an automatic data object.]

    #`[C808: (R803) The function-name shall be the name of an external function, an intrinsic function, a dummy function, a procedure pointer, | a statement function.]

    #`[R804] token object-name {
        |  $<name>
    }

    #`[C809: (R804) The object-name shall be the name of a data object.]

    #`[R805] token initialization {
        |  "="  $<constant-expr>
        |  "=>"  $<null-init>
        |  "=>"  $<initial-data-target>
    }

    #`[R806] token null-init {
        |  $<function-reference>
    }

    #`[C810: (R803) If => appears in initialization, the entity shall have the POINTER attribute. If = appears in initialization, the entity shall not have the POINTER attribute.]

    #`[C811: (R803) If initial-data-target appears, object-name shall be data-pointer-initialization compatible with it (7.5.4.6).]

    #`[C812: (R806) The function-reference shall be a reference to the intrinsic function NULL with no arguments.]

    #`[C813: An automatic data object shall not have the SAVE attribute.]

    #`[C814: An entity shall not be explicitly given any attribute more than once in a scoping unit.]

    #`[C815: An array-spec for a nonallocatable nonpointer function result shall be an explicit-shape-spec-list.]

    #`[R807] token access-spec {
        |  "PUBLIC"
        |  "PRIVATE"
    }

    #`[C816: An access-spec shall appear only in the specification-part of a module.]

    #`[R808] token language-binding-spec {
        |  "BIND"  "(C"  [  "," "NAME" "=" $<scalar-default-char-constant-expr> ]?  ")"
    }

    #`[C817: An entity with the BIND attribute shall be a common block, variable, type, | procedure.]

    #`[C818: Avariable with the BIND attribute shall be declared in the specification part of a module.]

    #`[C819: Avariable with the BIND attribute shall be interoperable (18.3).]

    #`[C820: Each variable of a common block with the BIND attribute shall be interoperable.]

    #`[R809] token coarray-spec {
        |  $<deferred-coshape-spec-list>
        |  $<explicit-coshape-spec>
    }

    #`[C821: The sum of the rank and corank of an entity shall not exceed fifteen.]

    #`[C822: Acoarray shall be a component | a variable that is not a function result.]

    #`[C823: A coarray shall not be of type C_PTR | C_FUNPTR from the intrinsic module ISO_C_BINDING (18.3.3), | of type TEAM_TYPE from the intrinsic module ISO_FORTRAN_ENV (16.10.2.32).]

    #`[C824: Anentity whose type has a coarray ultimate component shall be a nonpointer nonallocatable scalar, shall not be a coarray, and shall not be a function result.]

    #`[C825: Acoarrayoranobjectwithacoarrayultimatecomponentshallbeanassociatename,adummyargument, | have the ALLOCATABLE | SAVE attribute.]

    #`[R810] token deferred-coshape-spec {
        |  ":"
    }

    #`[C826: Acoarray with the ALLOCATABLE attribute shall have a coarray-spec that is a deferred-coshape-spec- list.]

    #`[R811] token explicit-coshape-spec {
        |  [  [  $<lower-cobound> ":" ]? $<upper-cobound> "," ]*  [  $<lower-cobound> ":" ]?  "*"
    }

    #`[C827: Anonallocatable coarray shall have a coarray-spec that is an explicit-coshape-spec.]

    #`[R812] token lower-cobound {
        |  $<specification-expr>
    }

    #`[R813] token upper-cobound {
        |  $<specification-expr>
    }

    #`[C828: (R811) A lower-cobound | upper-cobound that is not a constant expression shall appear only in a sub- program, BLOCK construct, | interface body.]

    #`[C829: An entity with the CONTIGUOUS attribute shall be an array pointer, an assumed-shape array, | an assumed-rank dummy data object.]

    #`[R814] token dimension-spec {
        |  "DIMENSION"  "("  $<array-spec>  ")"
    }

    #`[R815] token array-spec {
        |  $<explicit-shape-spec-list>
        |  $<assumed-shape-spec-list>
        |  $<deferred-shape-spec-list>
        |  $<assumed-size-spec>
        |  $<implied-shape-spec>
        |  $<implied-shape-or-assumed-size-spec>
        |  $<assumed-rank-spec>
    }

    #`[R816] token explicit-shape-spec {
        |  [  $<lower-bound> ":" ]?  $<upper-bound>
    }

    #`[R817] token lower-bound {
        |  $<specification-expr>
    }

    #`[R818] token upper-bound {
        |  $<specification-expr>
    }

    #`[C830: (R816) An explicit-shape-spec whose bounds are not constant expressions shall appear only in a subpro- gram, derived type definition, BLOCK construct, | interface body.]

    #`[R819] token assumed-shape-spec {
        |  [  $<lower-bound> ]?  ":"
    }

    #`[R820] token deferred-shape-spec {
        |  ":"
    }

    #`[C831: An array with the POINTER | ALLOCATABLE attribute shall have an array-spec that is a deferred- shape-spec-list.]

    #`[R821] token assumed-implied-spec {
        |  [  $<lower-bound> ":" ]?  "*"
    }

    #`[R822] token assumed-size-spec {
        |  $<explicit-shape-spec-list>  ","  $<assumed-implied-spec>
    }

    #`[C832: An object whose array bounds are specified by an assumed-size-spec shall be a dummy data object.]

    #`[C833: An assumed-size array with the INTENT (OUT) attribute shall not be polymorphic, finalizable, of a type with an allocatable ultimate component, | of a type for which default initialization is specified.]

    #`[R823] token implied-shape-or-assumed-size-spec {
        |  $<assumed-implied-spec>
    }

    #`[C834: An object whose array bounds are specified by an implied-shape-or-assumed-size-spec shall be a dummy data object | a named constant.]

    #`[R824] token implied-shape-spec {
        |  $<assumed-implied-spec>  ","  $<assumed-implied-spec-list>
    }

    #`[C835: An implied-shape array shall be a named constant.]

    #`[R825] token assumed-rank-spec {
        |  ".."
    }

    #`[C836: Anassumed-rankentityshallbeadummydataobjectthatdoesnothavetheCODIMENSIONorVALUE attribute.]

    #`[C837: An assumed-rank variable name shall not appear in a designator | expression except as an actual argument that corresponds to a dummy argument that is assumed-rank, the argument of the function C_LOCorC_SIZEOFfromthe intrinsic module ISO_C_BINDING (18.2), the first dummy argument of an intrinsic inquiry function, | the selector of a SELECT RANK statement.]

    #`[C838: If an assumed-size | nonallocatable nonpointer assumed-rank array is an actual argument that corres- ponds to a dummy argument that is an INTENT (OUT) assumed-rank array, it shall not be polymorphic, finalizable, of a type with an allocatable ultimate component, | of a type for which default initialization is specified.]

    #`[C839: An entity shall not have both the EXTERNAL attribute and the INTRINSIC attribute.]

    #`[C840: In an external subprogram, the EXTERNAL attribute shall not be specified for a procedure defined by the subprogram.]

    #`[C841: In an interface body, the EXTERNAL attribute shall not be specified for the procedure declared by the interface body.]

    #`[R826] token intent-spec {
        |  "IN"
        |  "OUT"
        |  "INOUT"
    }

    #`[C842: An entity with the INTENT attribute shall be a dummy data object | a dummy procedure pointer.]

    #`[C843: (R826) A nonpointer object with the INTENT (IN) attribute shall not appear in a variable definition context (19.6.7).]

    #`[C844: Apointer with the INTENT (IN) attribute shall not appear in a pointer association context (19.6.8).]

    #`[C845: AnINTENT(OUT)dummyargumentofanonintrinsic procedure shall not be an allocatable coarray | have a subobject that is an allocatable coarray.]

    #`[C846: Anentity with the INTENT (OUT) attribute shall not be of, | have a subcomponent of, type EVENT_- TYPE(16.10.2.10) | LOCK_TYPE (16.10.2.19) from the intrinsic module ISO_FORTRAN_ENV.]

    #`[C847: If the generic name of an intrinsic procedure is explicitly declared to have the INTRINSIC attribute, and it is also the generic name of one | more generic interfaces (15.4.3.2) accessible in the same scoping unit, the procedures in the interfaces and the generic intrinsic procedure shall all be functions | all be subroutines.]

    #`[C848: An entity with the OPTIONAL attribute shall be a dummy argument.  NOTE8.19 Theintrinsic function PRESENT(16.9.152)canbeusedtodeterminewhetheranoptionaldummyargument has a corresponding actual argument.]

    #`[C849: An entity with the PARAMETER attribute shall not be a variable, a coarray, | a procedure.]

    #`[C850: An expression that specifies a length type parameter | array bound of a named constant shall be a constant expression.]

    #`[C851: An entity with the POINTER attribute shall not have the ALLOCATABLE, INTRINSIC, | TARGET attribute, and shall not be a coarray.]

    #`[C852: Aprocedure with the POINTER attribute shall have the EXTERNAL attribute.]

    #`[C853: The PROTECTED attribute shall be specified only in the specification part of a module.]

    #`[C854: An entity with the PROTECTED attribute shall be a procedure pointer | variable.]

    #`[C855: An entity with the PROTECTED attribute shall not be in a common block.]

    #`[C856: A nonpointer object that has the PROTECTED attribute and is accessed by use association shall not appear in a variable definition context (19.6.7) | as a data-target | initial-data-target.]

    #`[C857: Apointer that has the PROTECTED attribute and is accessed by use association shall not appear in a pointer association context (19.6.8).]

    #`[C858: An entity with the SAVE attribute shall be a common block, variable, | procedure pointer.]

    #`[C859: The SAVE attribute shall not be specified for a dummy argument, a function result, an automatic data object, | an object that is in a common block.]

    #`[C860: An entity with the TARGET attribute shall be a variable.]

    #`[C861: An entity with the TARGET attribute shall not have the POINTER attribute.]

    #`[C862: An entity with the VALUE attribute shall be a dummy data object. It shall not be an assumed-size array, a coarray, | a variable with a coarray ultimate component.]

    #`[C863: Anentity with the VALUE attribute shall not have the ALLOCATABLE, INTENT (INOUT), INTENT (OUT), POINTER, | VOLATILE attributes.]

    #`[C864: Adummy argument of a procedure with the BIND attribute shall not have both the OPTIONAL and VALUEattributes.]

    #`[C865: An entity with the VOLATILE attribute shall be a variable that is not an INTENT (IN) dummy argu- ment.]

    #`[C866: The VOLATILE attribute shall not be specified for a coarray, | a variable with a coarray ultimate component, that is accessed by use (14.2.2) | host (19.5.1.4) association.]

    #`[C867: Within a BLOCK construct (11.1.4), the VOLATILE attribute shall not be specified for a coarray, | a variable with a coarray ultimate component, that is not a construct entity (19.4) of that construct.]

    #`[R827] token access-stmt {
        |  $<access-spec>  [  [  "::" ]? $<access-id-list> ]?
    }

    #`[R828] token access-id {
        |  $<access-name>
        |  $<generic-spec>
    }

    #`[C868: (R827) An access-stmt shall appear only in the specification-part of a module. Only one accessibility statement with an omitted access-id-list is permitted in the specification-part of a module.]

    #`[C869: (R828) Each access-name shall be the name of a module, variable, procedure, derived type, named constant, | namelist group.]

    #`[C870: Amodule whose name appears in an access-stmt shall be referenced by a USE statement in the scoping unit that contains the access-stmt.]

    #`[C871: The name of a module shall appear at most once in all of the access-stmts in a module.]

    #`[R829] token allocatable-stmt {
        |  "ALLOCATABLE"  [  "::" ]?  $<allocatable-decl-list>
    }

    #`[R830] token allocatable-decl {
        |  $<object-name>  [  "(" $<array-spec> ")" ]?  [  $<lbracket> $<coarray-spec> $<rbracket> ]?
    }

    #`[R831] token asynchronous-stmt {
        |  "ASYNCHRONOUS"  [  "::" ]?  $<object-name-list>
    }

    #`[R832] token bind-stmt {
        |  $<language-binding-spec>  [  "::" ]?  $<bind-entity-list>
    }

    #`[R833] token bind-entity {
        |  $<entity-name>
        |  "/"  $<common-block-name>  "/"
    }

    #`[C872: (R832) If the language-binding-spec has a NAME= specifier, the bind-entity-list shall consist of a single bind-entity.]

    #`[R834] token codimension-stmt {
        |  "CODIMENSION"  [  "::" ]?  $<codimension-decl-list>
    }

    #`[R835] token codimension-decl {
        |  $<coarray-name>  $<lbracket>  $<coarray-spec>  $<rbracket>
    }

    #`[R836] token contiguous-stmt {
        |  "CONTIGUOUS"  [  "::" ]?  $<object-name-list>
    }

    #`[R837] token data-stmt {
        |  "DATA"  $<data-stmt-set>  [  [  "," ]? $<data-stmt-set> ]*
    }

    #`[R838] token data-stmt-set {
        |  $<data-stmt-object-list>  "/"  $<data-stmt-value-list>  "/"
    }

    #`[R839] token data-stmt-object {
        |  $<variable>
        |  $<data-implied-do>
    }

    #`[R840] token data-implied-do {
        |  "("  $<data-i-do-object-list>  ","  [  $<integer-type-spec> "::" ]?  $<data-i-do-variable>  "="  $<scalar-int-constant-expr>  ","  $<scalar-int-constant-expr>  [  "," $<scalar-int-constant-expr> ]?  ")"
    }

    #`[R841] token data-i-do-object {
        |  $<array-element>
        |  $<scalar-structure-component>
        |  $<data-implied-do>
    }

    #`[R842] token data-i-do-variable {
        |  $<do-variable>
    }

    #`[C873: Adata-stmt-object | data-i-do-object shall not be a coindexed variable.]

    #`[C874: (R839) A data-stmt-object that is a variable shall be a designator. Each subscript, section subscript, substring starting point, and substring ending point in the variable shall be a constant expression.]

    #`[C875: (R839) A variable whose designator appears as a data-stmt-object | a data-i-do-object shall not be a dummy argument, accessed by use | host association, in a named common block unless the DATA statement is in a block data program unit, in blank common, a function name, a function result name, an automatic data object, | an allocatable variable.]

    #`[C876: (R839)Adata-i-do-object oravariable that appears as a data-stmt-object shall not be an object designator in which a pointer appears other than as the entire rightmost part-ref.]

    #`[C877: (R841) The array-element shall be a variable.]

    #`[C878: (R841) The scalar-structure-component shall be a variable.]

    #`[C879: (R841) The scalar-structure-component shall contain at least one part-ref that contains a subscript-list.]

    #`[C880: (R841) In an array-element | scalar-structure-component that is a data-i-do-object, any subscript shall be a constant expression, and any primary within that subscript that is a data-i-do-variable shall be a DOvariable of this data-implied-do | of a containing data-implied-do.]

    #`[R843] token data-stmt-value {
        |  [  $<data-stmt-repeat> "*" ]?  $<data-stmt-constant>
    }

    #`[R844] token data-stmt-repeat {
        |  $<scalar-int-constant>
        |  $<scalar-int-constant-subobject>
    }

    #`[C881: (R844) The data-stmt-repeat shall be positive | zero. If the data-stmt-repeat is a named constant, it shall have been defined previously.]

    #`[R845] token data-stmt-constant {
        |  $<scalar-constant>
        |  $<scalar-constant-subobject>
        |  $<signed-int-literal-constant>
        |  $<signed-real-literal-constant>
        |  $<null-init>
        |  $<initial-data-target>
        |  $<structure-constructor>
    }

    #`[C882: (R845) If a DATA statement constant value is a named constant | a structure constructor, the named constant | derived type shall have been defined previously.]

    #`[C883: (R845) If a data-stmt-constant is a structure-constructor, it shall be a constant expression.]

    #`[R846] token int-constant-subobject {
        |  $<constant-subobject>
    }

    #`[C884: (R846) int-constant-subobject shall be of type integer.]

    #`[R847] token constant-subobject {
        |  $<designator>
    }

    #`[C885: (R847) constant-subobject shall be a subobject of a constant.]

    #`[C886: (R847) Any subscript, substring starting point, | substring ending point shall be a constant expression.]

    #`[R848] token dimension-stmt {
        |  "DIMENSION"  [  "::" ]?  $<array-name>  "("  $<array-spec>  ")"  [  "," $<array-name> "(" $<array-spec> ")" ]*
    }

    #`[R849] token intent-stmt {
        |  "INTENT"  "("  $<intent-spec>  ")"  [  "::" ]?  $<dummy-arg-name-list>
    }

    #`[R850] token optional-stmt {
        |  "OPTIONAL"  [  "::" ]?  $<dummy-arg-name-list>
    }

    #`[R851] token parameter-stmt {
        |  "PARAMETER("  $<named-constant-def-list>  ")"
    }

    #`[R852] token named-constant-def {
        |  $<named-constant>  "="  $<constant-expr>
    }

    #`[R853] token pointer-stmt {
        |  "POINTER"  [  "::" ]?  $<pointer-decl-list>
    }

    #`[R854] token pointer-decl {
        |  $<object-name>  [  "(" $<deferred-shape-spec-list> ")" ]?
        |  $<proc-entity-name>
    }

    #`[C887: Aproc-entity-name shall have the EXTERNAL attribute.]

    #`[R855] token protected-stmt {
        |  "PROTECTED"  [  "::" ]?  $<entity-name-list>
    }

    #`[R856] token save-stmt {
        |  "SAVE"  [  [  "::" ]? $<saved-entity-list> ]?
    }

    #`[R857] token saved-entity {
        |  $<object-name>
        |  $<proc-pointer-name>
        |  "/"  $<common-block-name>  "/"
    }

    #`[R858] token proc-pointer-name {
        |  $<name>
    }

    #`[C888: (R856) If a SAVE statement with an omitted saved entity list appears in a scoping unit, no other appearance of the SAVE attr-spec | SAVE statement is permitted in that scoping unit.]

    #`[C889: Aproc-pointer-name shall be the name of a procedure pointer.]

    #`[R859] token target-stmt {
        |  "TARGET"  [  "::" ]?  $<target-decl-list>
    }

    #`[R860] token target-decl {
        |  $<object-name>  [  "(" $<array-spec> ")" ]?  [  $<lbracket> $<coarray-spec> $<rbracket> ]?
    }

    #`[R861] token value-stmt {
        |  "VALUE"  [  "::" ]?  $<dummy-arg-name-list>
    }

    #`[R862] token volatile-stmt {
        |  "VOLATILE"  [  "::" ]?  $<object-name-list>
    }

    #`[R863] token implicit-stmt {
        |  "IMPLICIT"  $<implicit-spec-list>
        |  "IMPLICIT"  "NONE"  [  "(" [  $<implicit-none-spec-list> ]? ")" ]?
    }

    #`[R864] token implicit-spec {
        |  $<declaration-type-spec>  "("  $<letter-spec-list>  ")"
    }

    #`[R865] token letter-spec {
        |  $<letter>  [  "-" $<letter> ]?
    }

    #`[R866] token implicit-none-spec {
        |  "EXTERNAL"
        |  "TYPE"
    }

    #`[C890: (R863) If an IMPLICIT NONE statement appears in a scoping unit, it shall precede any PARAMETER statements that appear in the scoping unit. No more than one IMPLICIT NONE statement shall appear in a scoping unit.]

    #`[C891: The same implicit-none-spec shall not appear more than once in a given implicit-stmt.]

    #`[C892: If an IMPLICIT NONEstatementinascopingunithasanimplicit-none-spec of TYPEorhasnoimplicit- none-spec-list, there shall be no other IMPLICIT statements in the scoping unit.]

    #`[C893: (R865) If the minus and second letter appear, the second letter shall follow the first letter alphabetically.]

    #`[C894: If IMPLICIT NONE with an implicit-none-spec of EXTERNAL appears within a scoping unit, the name of an external | dummy procedure in that scoping unit | in a contained subprogram | BLOCK construct shall have an explicit interface | be explicitly declared to have the EXTERNAL attribute.]

    #`[R867] token import-stmt {
        |  "IMPORT"  [  [  "::" ]? $<import-name-list> ]?
        |  "IMPORT,"  "ONLY"  ":"  $<import-name-list>
        |  "IMPORT,"  "NONE"
        |  "IMPORT,"  "ALL"
    }

    #`[C895: (R867) An IMPORT statement shall not appear in the scoping unit of a main-program, external- subprogram, module, | block-data.]

    #`[C896: (R867) Each import-name shall be the name of an entity in the host scoping unit.]

    #`[C897: If any IMPORT statement in a scoping unit has an ONLY specifier, all IMPORT statements in that scoping unit shall have an ONLY specifier.]

    #`[C898: IMPORT, NONE shall not appear in the scoping unit of a submodule.]

    #`[C899: If an IMPORT, NONE | IMPORT, ALL statement appears in a scoping unit, no other IMPORT statement shall appear in that scoping unit.]

    #`[C8100: Within an interface body, an entity that is accessed by host association shall be accessible by host | use association within the host scoping unit, | explicitly declared prior to the interface body.]

    #`[C8101: An entity whose name appears as an import-name | which is made accessible by an IMPORT, ALL statement shall not appear in any context described in 19.5.1.4 that would cause the host entity of that name to be inaccessible.]

    #`[R868] token namelist-stmt {
        |  "NAMELIST"  "/"  $<namelist-group-name>  "/"  $<namelist-group-object-list>  [  [  "," ]? "/" $<namelist-group-name> "/" $<namelist-group-object-list> ]*
    }

    #`[C8102: (R868) The namelist-group-name shall not be a name accessed by use association.]

    #`[R869] token namelist-group-object {
        |  $<variable-name>
    }

    #`[C8103: (R869) A namelist-group-object shall not be an assumed-size array.]

    #`[C8104: (R868) A namelist-group-object shall not have the PRIVATE attribute if the namelist-group-name has the PUBLIC attribute.]

    #`[R870] token equivalence-stmt {
        |  "EQUIVALENCE"  $<equivalence-set-list>
    }

    #`[R871] token equivalence-set {
        |  "("  $<equivalence-object>  ","  $<equivalence-object-list>  ")"
    }

    #`[R872] token equivalence-object {
        |  $<variable-name>
        |  $<array-element>
        |  $<substring>
    }

    #`[C8105: (R872) An equivalence-object shall not be a designator with a base object that is a dummy argument, a function result, a pointer, an allocatable variable, a derived-type object that has an allocatable | pointer ultimate component, an object of a nonsequence derived type, an automatic data object, a coarray, a variable with the BIND attribute, a variable in a common block that has the BIND attribute, | a named constant.]

    #`[C8106: (R872) An equivalence-object shall not be a designator that has more than one part-ref.]

    #`[C8107: (R872) An equivalence-object shall not have the TARGET attribute.]

    #`[C8108: (R872) Each subscript | substring range expression in an equivalence-object shall be an integer constant expression (10.1.12).]

    #`[C8109: (R871) If an equivalence-object is default integer, default real, double precision real, default complex, default logical, | of numeric sequence type, all of the objects in the equivalence set shall be of these types and kinds.]

    #`[C8110: (R871) If an equivalence-object is default character | of character sequence type, all of the objects in the equivalence set shall be of these types and kinds.]

    #`[C8111: (R871) If an equivalence-object is of a sequence type that is not a numeric sequence | character sequence type, all of the objects in the equivalence set shall be of that type.]

    #`[C8112: (R871) If an equivalence-object is of an intrinsic type but is not default integer, default real, double precision real, default complex, default logical, | default character, all of the objects in the equivalence set shall be of the same type with the same kind type parameter value.]

    #`[C8113: (R872) If an equivalence-object has the PROTECTED attribute, all of the objects in the equivalence set shall have the PROTECTEDattribute.]

    #`[C8114: (R872) The name of an equivalence-object shall not be a name made accessible by use association.]

    #`[C8115: (R872) A substring shall not have length zero.]

    #`[R873] token common-stmt {
        |  "COMMON"  [  "/" [  $<common-block-name> ]? "/" ]?  $<common-block-object-list>  [  [  "," ]? "/" [  $<common-block-name> ]? "/" $<common-block-object-list> ]*
    }

    #`[R874] token common-block-object {
        |  $<variable-name>  [  "(" $<array-spec> ")" ]?
    }

    #`[C8116: (R874) An array-spec in a common-block-object shall be an explicit-shape-spec-list.]

    #`[C8117: (R874) Only one appearance of a given variable-name is permitted in all common-block-object-lists within a scoping unit.]

    #`[C8118: (R874) A common-block-object shall not be a dummy argument, a function result, an allocatable variable, a derived-type object with an ultimate component that is allocatable, a procedure pointer, an automatic data object, a variable with the BIND attribute, an unlimited polymorphic pointer, | a coarray.]

    #`[C8119: (R874) If a common-block-object is of a derived type, the type shall have the BIND attribute | the SEQUENCE attribute and it shall have no default initialization.]

    #`[C8120: (R874) A variable-name shall not be a name made accessible by use association.]

    #`[R901] token designator {
        |  $<object-name>
        |  $<array-element>
        |  $<array-section>
        |  $<coindexed-named-object>
        |  $<complex-part-designator>
        |  $<structure-component>
        |  $<substring>
    }

    #`[R902] token variable {
        |  $<designator>
        |  $<function-reference>
    }

    #`[C901: (R902) designator shall not be a constant | a subobject of a constant.]

    #`[C902: (R902) function-reference shall have a data pointer result.]

    #`[R903] token variable-name {
        |  $<name>
    }

    #`[C903: (R903) variable-name shall be the name of a variable.]

    #`[R904] token logical-variable {
        |  $<variable>
    }

    #`[C904: (R904) logical-variable shall be of type logical.]

    #`[R905] token char-variable {
        |  $<variable>
    }

    #`[C905: (R905) char-variable shall be of type character.]

    #`[R906] token default-char-variable {
        |  $<variable>
    }

    #`[C906: (R906) default-char-variable shall be default character.]

    #`[R907] token int-variable {
        |  $<variable>
    }

    #`[C907: (R907) int-variable shall be of type integer.]

    #`[R908] token substring {
        |  $<parent-string>  "("  $<substring-range>  ")"
    }

    #`[R909] token parent-string {
        |  $<scalar-variable-name>
        |  $<array-element>
        |  $<coindexed-named-object>
        |  $<scalar-structure-component>
        |  $<scalar-constant>
    }

    #`[R910] token substring-range {
        |  [  $<scalar-int-expr> ]?  ":"  [  $<scalar-int-expr> ]?
    }

    #`[C908: (R909) parent-string shall be of type character.]

    #`[R911] token data-ref {
        |  $<part-ref>  [  "%" $<part-ref> ]*
    }

    #`[R912] token part-ref {
        |  $<part-name>  [  "(" $<section-subscript-list> ")" ]?  [  $<image-selector> ]?
    }

    #`[C909: (R911) Each part-name except the rightmost shall be of derived type.]

    #`[C910: (R911) Each part-name except the leftmost shall be the name of a component of the declared type of the preceding part-name.]

    #`[C911: (R911) If the rightmost part-name is of abstract type, data-ref shall be polymorphic.]

    #`[C912: (R911) The leftmost part-name shall be the name of a data object.]

    #`[C913: (R912) If a section-subscript-list appears, the number of section-subscripts shall equal the rank of part- name.]

    #`[C914: (R912) If image-selector appears, the number of cosubscripts shall be equal to the corank of part-name.]

    #`[C915: A data-ref shall not be of type C_PTR | C_FUNPTR from the intrinsic module ISO_C_BIND- ING(18.2), | of type TEAM_TYPE from the intrinsic module ISO_FORTRAN_ENV (16.10.2), if one of its part-refs has an image-selector.]

    #`[C916: (R912) If image-selector appears and part-name is an array, section-subscript-list shall appear.]

    #`[C917: (R911) Except as an actual argument to an intrinsic inquiry function | as the designator in a type parameter inquiry, a data-ref shall not be a coindexed object that has a polymorphic allocatable potential subobject component.]

    #`[C918: Except as an actual argument to an intrinsic inquiry function | as the designator in a type parameter inquiry, if the rightmost part-ref is polymorphic, no other part-ref shall be coindexed.]

    #`[C919: (R911) There shall not be more than one part-ref with nonzero rank. A part-name to the right of a part-ref with nonzero rank shall not have the ALLOCATABLE | POINTER attribute.]

    #`[R913] token structure-component {
        |  $<data-ref>
    }

    #`[C920: (R913)Thereshallbemorethanonepart-ref andtherightmostpart-ref shallnothaveasection-subscript- list.]

    #`[R914] token coindexed-named-object {
        |  $<data-ref>
    }

    #`[C921: (R914) The data-ref shall contain exactly one part-ref. The part-ref shall contain an image-selector.  The part-name shall be the name of a scalar coarray.]

    #`[R915] token complex-part-designator {
        |  $<designator>  "%"  "RE"
        |  $<designator>  "%"  "IM"
    }

    #`[C922: (R915) The designator shall be of complex type.]

    #`[R916] token type-param-inquiry {
        |  $<designator>  "%"  $<type-param-name>
    }

    #`[C923: (R916) The type-param-name shall be the name of a type parameter of the declared type of the object designated by the designator.]

    #`[R917] token array-element {
        |  $<data-ref>
    }

    #`[C924: (R917) Every part-ref shall have rank zero and the last part-ref shall contain a subscript-list.]

    #`[R918] token array-section {
        |  $<data-ref>  [  "(" $<substring-range> ")" ]?
        |  $<complex-part-designator>
    }

    #`[C925: (R918) Exactly one part-ref shall have nonzero rank, and either the final part-ref shall have a section- subscript-list with nonzero rank, another part-ref shall have nonzero rank, | the complex-part-designator shall be an array.]

    #`[C926: (R918) If a substring-range appears, the rightmost part-name shall be of type character.]

    #`[R919] token subscript {
        |  $<scalar-int-expr>
    }

    #`[R920] token section-subscript {
        |  $<subscript>
        |  $<subscript-triplet>
        |  $<vector-subscript>
    }

    #`[R921] token subscript-triplet {
        |  [  $<subscript> ]?  ":"  [  $<subscript> ]?  [  ":" $<stride> ]?
    }

    #`[R922] token stride {
        |  $<scalar-int-expr>
    }

    #`[R923] token vector-subscript {
        |  $<int-expr>
    }

    #`[C927: (R923) A vector-subscript shall be an integer array expression of rank one.]

    #`[C928: (R921) The second subscript shall not be omitted from a subscript-triplet in the last dimension of an assumed-size array.]

    #`[R924] token image-selector {
        |  $<lbracket>  $<cosubscript-list>  [  "," $<image-selector-spec-list> ]?  $<rbracket>
    }

    #`[R925] token cosubscript {
        |  $<scalar-int-expr>
    }

    #`[R926] token image-selector-spec {
        |  "STAT"  "="  $<stat-variable>
        |  "TEAM="  $<team-value>
        |  "TEAM_NUMBER="  $<scalar-int-expr>
    }

    #`[C929: No specifier shall appear more than once in a given image-selector-spec-list.]

    #`[C930: TEAMandTEAM_NUMBERshallnotbothappear in the same image-selector-spec-list.]

    #`[C931: Astat-variable in an image-selector shall not be a coindexed object.]

    #`[R927] token allocate-stmt {
        |  "ALLOCATE("  [  $<type-spec> "::" ]?  $<allocation-list>  [  "," $<alloc-opt-list> ]?  ")"
    }

    #`[R928] token alloc-opt {
        |  "ERRMSG="  $<errmsg-variable>
        |  "MOLD="  $<source-expr>
        |  "SOURCE="  $<source-expr>
        |  "STAT"  "="  $<stat-variable>
    }

    #`[R929] token errmsg-variable {
        |  $<scalar-default-char-variable>
    }

    #`[R930] token source-expr {
        |  $<expr>
    }

    #`[R931] token allocation {
        |  $<allocate-object>  [  "(" $<allocate-shape-spec-list> ")" ]?  [  $<lbracket> $<allocate-coarray-spec> $<rbracket> ]?
    }

    #`[R932] token allocate-object {
        |  $<variable-name>
        |  $<structure-component>
    }

    #`[R933] token allocate-shape-spec {
        |  [  $<lower-bound-expr> ":" ]?  $<upper-bound-expr>
    }

    #`[R934] token lower-bound-expr {
        |  $<scalar-int-expr>
    }

    #`[R935] token upper-bound-expr {
        |  $<scalar-int-expr>
    }

    #`[R936] token allocate-coarray-spec {
        |  [  $<allocate-coshape-spec-list> "," ]?  [  $<lower-bound-expr> ":" ]?  "*"
    }

    #`[R937] token allocate-coshape-spec {
        |  [  $<lower-bound-expr> ":" ]?  $<upper-bound-expr>
    }

    #`[C932: (R932) Each allocate-object shall be a data pointer | an allocatable variable.]

    #`[C933: (R927) If any allocate-object has a deferred type parameter, is unlimited polymorphic, | is of abstract type, either type-spec | source-expr shall appear.]

    #`[C934: (R927) If type-spec appears, it shall specify a type with which each allocate-object is type compatible.]

    #`[C935: (R927) A type-param-value in a type-spec shall be an asterisk if and only if each allocate-object is a dummy argument for which the corresponding type parameter is assumed.]

    #`[C936: (R927) If type-spec appears, the kind type parameter values of each allocate-object shall be the same as the corresponding type parameter values of the type-spec.]

    #`[C937: (R927) type-spec shall not specify a type that has a coarray ultimate component.]

    #`[C938: (R927) If an allocate-object is a coarray, type-spec shall not specify type C_PTR | C_FUNPTR from the intrinsic module ISO_C_BINDING, | type TEAM_TYPE from the intrinsic module ISO_FOR- TRAN_ENV.]

    #`[C939: (R927) If an allocate-object is an array, either allocate-shape-spec-list shall appear in its allocation, | source-expr shall appear in the ALLOCATE statement and have the same rank as the allocate-object.]

    #`[C940: (R931) If allocate-object is scalar, allocate-shape-spec-list shall not appear.]

    #`[C941: (R931) An allocate-coarray-spec shall appear if and only if the allocate-object is a coarray.]

    #`[C942: (R931) The number of allocate-shape-specs in an allocate-shape-spec-list shall be the same as the rank of the allocate-object. The number of allocate-coshape-specs in an allocate-coarray-spec shall be one less than the corank of the allocate-object.]

    #`[C943: (R928) No alloc-opt shall appear more than once in a given alloc-opt-list.]

    #`[C944: (R927) At most one of source-expr and type-spec shall appear.]

    #`[C945: (R927) Each allocate-object shall be type compatible (7.3.2.3) with source-expr. If SOURCE= appears, source-expr shall be a scalar | have the same rank as each allocate-object.]

    #`[C946: (R927) If source-expr appears, the kind type parameters of each allocate-object shall have the same values as the corresponding type parameters of source-expr.]

    #`[C947: (R927) The declared type of source-expr shall not be C_PTR | C_FUNPTR from the intrinsic module ISO_C_BINDING, | TEAM_TYPE from the intrinsic module ISO_FORTRAN_ENV, if an allocate- object is a coarray.]

    #`[C948: (R927) If SOURCE= appears, the declared type of source-expr shall not be EVENT_TYPE | LOCK_- TYPE from the intrinsic module ISO_FORTRAN_ENV, | have a potential subobject component of type EVENT_TYPE | LOCK_TYPE.]

    #`[C949: (R930) The declared type of source-expr shall not have a coarray ultimate component.]

    #`[C950: (R932) An allocate-object shall not be a coindexed object.]

    #`[R938] token nullify-stmt {
        |  "NULLIFY"  "("  $<pointer-object-list>  ")"
    }

    #`[R939] token pointer-object {
        |  $<variable-name>
        |  $<structure-component>
        |  $<proc-pointer-name>
    }

    #`[C951: (R939) Each pointer-object shall have the POINTER attribute.]

    #`[R940] token deallocate-stmt {
        |  "DEALLOCATE("  $<allocate-object-list>  [  "," $<dealloc-opt-list> ]?  ")"
    }

    #`[R941] token dealloc-opt {
        |  "STAT"  "="  $<stat-variable>
        |  "ERRMSG="  $<errmsg-variable>
    }

    #`[C952: (R941) No dealloc-opt shall appear more than once in a given dealloc-opt-list.]

    #`[R942] token stat-variable {
        |  $<scalar-int-variable>
    }

    #`[R1001] token primary {
        |  $<literal-constant>
        |  $<designator>
        |  $<array-constructor>
        |  $<structure-constructor>
        |  $<function-reference>
        |  $<type-param-inquiry>
        |  $<type-param-name>
        |  "("  $<expr>  ")"
    }

    #`[C1001: (R1001) The type-param-name shall be the name of a type parameter.]

    #`[C1002: (R1001) The designator shall not be a whole assumed-size array.]

    #`[C1003: (R1001) The expr shall not be a function reference that returns a procedure pointer.]

    #`[R1002] token level-one-expr {
        |  [  $<defined-unary-op> ]?  $<primary>
    }

    #`[C1004: (R1003) A defined-unary-op shall not contain more than 63 letters and shall not be the same as any intrinsic-operator | logical-literal-constant.]

    #`[R1004] token mult-operand {
        |  $<level-one-expr>  [  $<power-op> $<mult-operand> ]?
    }

    #`[R1005] token add-operand {
        |  [  $<add-operand> $<mult-op> ]?  $<mult-operand>
    }

    #`[R1006] token level-two-expr {
        |  [  [  $<level-two-expr> ]? $<add-op> ]?  $<add-operand>
    }

    #`[R1010] token level-three-expr {
        |  [  $<level-three-expr> $<concat-op> ]?  $<level-two-expr>
    }

    #`[R1012] token level-four-expr {
        |  [  $<level-three-expr> $<rel-op> ]?  $<level-three-expr>
    }

    #`[R1014] token and-operand {
        |  [  $<not-op> ]?  $<level-four-expr>
    }

    #`[R1015] token or-operand {
        |  [  $<or-operand> $<and-op> ]?  $<and-operand>
    }

    #`[R1016] token equiv-operand {
        |  [  $<equiv-operand> $<or-op> ]?  $<or-operand>
    }

    #`[R1017] token level-five-expr {
        |  [  $<level-five-expr> $<equiv-op> ]?  $<equiv-operand>
    }

    #`[R1022] token expr {
        |  [  $<expr> $<defined-binary-op> ]?  $<level-five-expr>
    }

    #`[C1005: (R1023) A defined-binary-op shall not contain more than 63 letters and shall not be the same as any intrinsic-operator | logical-literal-constant.]

    #`[R1024] token logical-expr {
        |  $<expr>
    }

    #`[C1006: (R1024) logical-expr shall be of type logical.]

    #`[R1025] token default-char-expr {
        |  $<expr>
    }

    #`[C1007: (R1025) default-char-expr shall be default character.]

    #`[R1026] token int-expr {
        |  $<expr>
    }

    #`[C1008: (R1026) int-expr shall be of type integer.]

    #`[R1027] token numeric-expr {
        |  $<expr>
    }

    #`[C1009: (R1027) numeric-expr shall be of type integer, real, | complex.]

    #`[R1028] token specification-expr {
        |  $<scalar-int-expr>
    }

    #`[C1010: (R1028) The scalar-int-expr shall be a restricted expression.]

    #`[R1029] token constant-expr {
        |  $<expr>
    }

    #`[C1011: (R1029) constant-expr shall be a constant expression.]

    #`[R1030] token default-char-constant-expr {
        |  $<default-char-expr>
    }

    #`[C1012: (R1030) default-char-constant-expr shall be a constant expression.]

    #`[R1031] token int-constant-expr {
        |  $<int-expr>
    }

    #`[C1013: (R1031) int-constant-expr shall be a constant expression.]

    #`[R1032] token assignment-stmt {
        |  $<variable>  "="  $<expr>
    }

    #`[C1014: (R1032) The variable shall not be a whole assumed-size array.]

    #`[R1033] token pointer-assignment-stmt {
        |  $<data-pointer-object>  [  "(" $<bounds-spec-list> ")" ]?  "=>"  $<data-target>
        |  $<data-pointer-object>  "("  $<bounds-remapping-list>  ")"  "=>"  $<data-target>
        |  $<proc-pointer-object>  "=>"  $<proc-target>
    }

    #`[R1034] token data-pointer-object {
        |  $<variable-name>
        |  $<scalar-variable>  "%"  $<data-pointer-component-name>
    }

    #`[C1015: (R1033)Ifdata-target is not unlimited polymorphic, data-pointer-object shall be type compatible (7.3.2.3) with it and the corresponding kind type parameters shall be equal.]

    #`[C1016: (R1033) If data-target is unlimited polymorphic, data-pointer-object shall be unlimited polymorphic, | of a type with the BIND attribute | the SEQUENCE attribute.]

    #`[C1017: (R1033) If bounds-spec-list is specified, the number of bounds-specs shall equal the rank of data-pointer- object.]

    #`[C1018: (R1033) If bounds-remapping-list is specified, the number of bounds-remappings shall equal the rank of data-pointer-object.]

    #`[C1019: (R1033) If bounds-remapping-list is not specified, the ranks of data-pointer-object and data-target shall be the same.]

    #`[C1020: (R1033) A coarray data-target shall have the VOLATILE attribute if and only if the data-pointer-object has the VOLATILE attribute.]

    #`[C1021: (R1034) A variable-name shall have the POINTER attribute.]

    #`[C1022: (R1034) A scalar-variable shall be a data-ref.]

    #`[C1023: (R1034) A data-pointer-component-name shall be the name of a component of scalar-variable that is a data pointer.]

    #`[C1024: (R1034) A data-pointer-object shall not be a coindexed object.]

    #`[R1035] token bounds-spec {
        |  $<lower-bound-expr>  ":"
    }

    #`[R1036] token bounds-remapping {
        |  $<lower-bound-expr>  ":"  $<upper-bound-expr>
    }

    #`[R1037] token data-target {
        |  $<expr>
    }

    #`[C1025: (R1037)Theexpr shall be a designator that designates a variable with either the TARGET | POINTER attribute and is not an array section with a vector subscript, | it shall be a reference to a function that returns a data pointer.]

    #`[C1026: (R1037) A data-target shall not be a coindexed object.]

    #`[R1038] token proc-pointer-object {
        |  $<proc-pointer-name>
        |  $<proc-component-ref>
    }

    #`[R1039] token proc-component-ref {
        |  $<scalar-variable>  "%"  $<procedure-component-name>
    }

    #`[C1027: (R1039) The scalar-variable shall be a data-ref that is not a coindexed object.]

    #`[C1028: (R1039) The procedure-component-name shall be the name of a procedure pointer component of the declared type of scalar-variable.]

    #`[R1040] token proc-target {
        |  $<expr>
        |  $<procedure-name>
        |  $<proc-component-ref>
    }

    #`[C1029: (R1040) An expr shall be a reference to a function whose result is a procedure pointer.]

    #`[C1030: (R1040) A procedure-name shall be the name of an internal, module, | dummy procedure, a procedure pointer, a specific intrinsic function listed in Table 16.2, | an external procedure that is accessed by use | host association, referenced in the scoping unit as a procedure, | that has the EXTERNAL attribute.]

    #`[C1031: (R1040) The proc-target shall not be a nonintrinsic elemental procedure.]

    #`[R1041] token where-stmt {
        |  "WHERE("  $<mask-expr>  ")"  $<where-assignment-stmt>
    }

    #`[R1042] token where-construct {
        |  $<where-construct-stmt>  [  $<where-body-construct> ]*  [  $<masked-elsewhere-stmt> [  $<where-body-construct> ]* ]*  [  $<elsewhere-stmt> [  $<where-body-construct> ]* ]?  $<end-where-stmt>
    }

    #`[R1043] token where-construct-stmt {
        |  [  $<where-construct-name> ":" ]?  "WHERE"  "("  $<mask-expr>  ")"
    }

    #`[R1044] token where-body-construct {
        |  $<where-assignment-stmt>
        |  $<where-stmt>
        |  $<where-construct>
    }

    #`[R1045] token where-assignment-stmt {
        |  $<assignment-stmt>
    }

    #`[R1046] token mask-expr {
        |  $<logical-expr>
    }

    #`[R1047] token masked-elsewhere-stmt {
        |  "ELSEWHERE("  $<mask-expr>  ")"  [  $<where-construct-name> ]?
    }

    #`[R1048] token elsewhere-stmt {
        |  "ELSEWHERE"  [  $<where-construct-name> ]?
    }

    #`[R1049] token end-where-stmt {
        |  "ENDWHERE"  [  $<where-construct-name> ]?
    }

    #`[C1032: (R1045) A where-assignment-stmt that is a defined assignment shall be elemental.]

    #`[C1033: (R1042) If the where-construct-stmt is identified by a where-construct-name, the corresponding end- where-stmt shall specify the same where-construct-name. If the where-construct-stmt is not identified by a where-construct-name, the corresponding end-where-stmt shall not specify a where-construct-name. If an elsewhere-stmt | a masked-elsewhere-stmt is identified by a where-construct-name, the corresponding where-construct-stmt shall specify the same where-construct-name.]

    #`[C1034: (R1044) A statement that is part of a where-body-construct shall not be a branch target statement.]

    #`[R1050] token forall-construct {
        |  $<forall-construct-stmt>  [  $<forall-body-construct> ]*  $<end-forall-stmt>
    }

    #`[R1051] token forall-construct-stmt {
        |  [  $<forall-construct-name> ":" ]?  "FORALL"  $<concurrent-header>
    }

    #`[R1052] token forall-body-construct {
        |  $<forall-assignment-stmt>
        |  $<where-stmt>
        |  $<where-construct>
        |  $<forall-construct>
        |  $<forall-stmt>
    }

    #`[R1053] token forall-assignment-stmt {
        |  $<assignment-stmt>
        |  $<pointer-assignment-stmt>
    }

    #`[R1054] token end-forall-stmt {
        |  "ENDFORALL"  [  $<forall-construct-name> ]?
    }

    #`[C1035: (R1054) If the forall-construct-stmt has a forall-construct-name, the end-forall-stmt shall have the same forall-construct- name. If the end-forall-stmt has a forall-construct-name, the forall-construct-stmt shall have the same forall-construct- name.]

    #`[C1036: (R1052) A statement in a forall-body-construct shall not define an index-name of the forall-construct.]

    #`[C1037: (R1052) Any procedure referenced in a forall-body-construct, including one referenced by a defined operation, assignment, | finalization, shall be a pure procedure.]

    #`[C1038: (R1052) A forall-body-construct shall not be a branch target.]

    #`[R1055] token forall-stmt {
        |  "FORALL"  $<concurrent-header>  $<forall-assignment-stmt>
    }

    #`[R1101] token block {
        |  [  $<execution-part-construct> ]*
    }

    #`[R1102] token associate-construct {
        |  $<associate-stmt>  $<block>  $<end-associate-stmt>
    }

    #`[R1103] token associate-stmt {
        |  [  $<associate-construct-name> ":" ]?  "ASSOCIATE"  "("  $<association-list>  ")"
    }

    #`[R1104] token association {
        |  $<associate-name>  "=>"  $<selector>
    }

    #`[R1105] token selector {
        |  $<expr>
        |  $<variable>
    }

    #`[C1101: (R1104) If selector is not a variable | is a variable that has a vector subscript, neither associate-name nor any subobject thereof shall appear in a variable definition context (19.6.7).]

    #`[C1102: (R1104) An associate-name shall not be the same as another associate-name in the same associate-stmt.]

    #`[C1103: (R1105) variable shall not be a coindexed object.]

    #`[C1104: (R1105) expr shall not be a variable.]

    #`[C1105: (R1105) expr shall not be a designator of a procedure pointer | a function reference that returns a procedure pointer.]

    #`[R1106] token end-associate-stmt {
        |  "ENDASSOCIATE"  [  $<associate-construct-name> ]?
    }

    #`[C1106: (R1106) If the associate-stmt of an associate-construct specifies an associate-construct-name, the corres- ponding end-associate-stmt shall specify the same associate-construct-name. If the associate-stmt of an associate-construct does not specify an associate-construct-name, the corresponding end-associate-stmt shall not specify an associate-construct-name.]

    #`[R1107] token block-construct {
        |  $<block-stmt>  [  $<block-specification-part> ]?  $<block>  $<end-block-stmt>
    }

    #`[R1108] token block-stmt {
        |  [  $<block-construct-name> ":" ]?  "BLOCK"
    }

    #`[R1109] token block-specification-part {
        |  [  $<use-stmt> ]*  [  $<import-stmt> ]*  [  [  $<declaration-construct> ]* $<specification-construct> ]?
    }

    #`[R1110] token end-block-stmt {
        |  "ENDBLOCK"  [  $<block-construct-name> ]?
    }

    #`[C1107: (R1107) A block-specification-part shall not contain a COMMON, EQUIVALENCE, INTENT, NAMELIST, OPTIONAL, statement function, | VALUE statement.]

    #`[C1108: (R1107) A SAVE statement in a BLOCK construct shall contain a saved-entity-list that does not specify a common-block-name.]

    #`[C1109: (R1107) If the block-stmt of a block-construct specifies a block-construct-name, the corresponding end- block-stmt shall specify the same block-construct-name.             If the block-stmt does not specify a block- construct-name, the corresponding end-block-stmt shall not specify a block-construct-name.]

    #`[R1111] token change-team-construct {
        |  $<change-team-stmt>  $<block>  $<end-change-team-stmt>
    }

    #`[R1112] token change-team-stmt {
        |  [  $<team-construct-name> ":" ]?  "CHANGE"  "TEAM"  "("  $<team-value>  [  "," $<coarray-association-list> ]?  [  "," $<sync-stat-list> ]?  ")"
    }

    #`[R1113] token coarray-association {
        |  $<codimension-decl>  "=>"  $<selector>
    }

    #`[R1114] token end-change-team-stmt {
        |  "ENDTEAM"  [  "(" [  $<sync-stat-list> ]? ")" ]?  [  $<team-construct-name> ]?
    }

    #`[R1115] token team-value {
        |  $<scalar-expr>
    }

    #`[C1110: A branch (11.2) within a CHANGE TEAM construct shall not have a branch target that is outside the construct.]

    #`[C1111: A RETURN statement shall not appear within a CHANGE TEAM construct.]

    #`[C1112: If the change-team-stmt of a change-team-construct specifies a team-construct-name, the corresponding end-change-team-stmt shall specify the same team-construct-name. If the change-team-stmt of a change- team-construct does not specify a team-construct-name, the corresponding end-change-team-stmt shall not specify a team-construct-name.]

    #`[C1113: In a change-team-stmt, a coarray-name in a codimension-decl shall not be the same as a selector, | another coarray-name, in that statement.]

    #`[C1114: A team-value shall be of type TEAM_TYPE from the intrinsic module ISO_FORTRAN_ENV.]

    #`[C1115: No selector shall appear more than once in a given change-team-stmt.]

    #`[C1116: A selector in a coarray-association shall be a named coarray.]

    #`[R1116] token critical-construct {
        |  $<critical-stmt>  $<block>  $<end-critical-stmt>
    }

    #`[R1117] token critical-stmt {
        |  [  $<critical-construct-name> ":" ]?  "CRITICAL"  [  "(" [  $<sync-stat-list> ]? ")" ]?
    }

    #`[R1118] token end-critical-stmt {
        |  "ENDCRITICAL"  [  $<critical-construct-name> ]?
    }

    #`[C1117: (R1116) If the critical-stmt of a critical-construct specifies a critical-construct-name, the corresponding end-critical-stmt shall specify the same critical-construct-name. If the critical-stmt of a critical-construct does not specify a critical-construct-name, the corresponding end-critical-stmt shall not specify a critical- construct-name.]

    #`[C1118: (R1116) The block of a critical-construct shall not contain a RETURN statement | an image control statement.]

    #`[C1119: Abranch(11.2)withinaCRITICALconstructshallnothaveabranchtargetthatisoutsidetheconstruct.]

    #`[R1119] token do-construct {
        |  $<do-stmt>  $<block>  $<end-do>
    }

    #`[R1120] token do-stmt {
        |  $<nonlabel-do-stmt>
        |  $<label-do-stmt>
    }

    #`[R1121] token label-do-stmt {
        |  [  $<do-construct-name> ":" ]?  "DO"  $<label>  [  $<loop-control> ]?
    }

    #`[R1122] token nonlabel-do-stmt {
        |  [  $<do-construct-name> ":" ]?  "DO"  [  $<loop-control> ]?
    }

    #`[R1123] token loop-control {
        |  [  "," ]?  $<do-variable>  "="  $<scalar-int-expr>  ","  $<scalar-int-expr>  [  "," $<scalar-int-expr> ]?
        |  [  "," ]?  "WHILE"  "("  $<scalar-logical-expr>  ")"
        |  [  "," ]?  "CONCURRENT"  $<concurrent-header>  $<concurrent-locality>
    }

    #`[R1124] token do-variable {
        |  $<scalar-int-variable-name>
    }

    #`[C1120: (R1124) The do-variable shall be a variable of type integer.]

    #`[R1125] token concurrent-header {
        |  "("  [  $<integer-type-spec> "::" ]?  $<concurrent-control-list>  [  "," $<scalar-mask-expr> ]?  ")"
    }

    #`[R1126] token concurrent-control {
        |  $<index-name>  "="  $<concurrent-limit>  ":"  $<concurrent-limit>  [  ":" $<concurrent-step> ]?
    }

    #`[R1127] token concurrent-limit {
        |  $<scalar-int-expr>
    }

    #`[R1128] token concurrent-step {
        |  $<scalar-int-expr>
    }

    #`[R1129] token concurrent-locality {
        |  [  $<locality-spec> ]*
    }

    #`[R1130] token locality-spec {
        |  "LOCAL("  $<variable-name-list>  ")"
        |  "LOCAL_INIT"  "("  $<variable-name-list>  ")"
        |  "SHARED("  $<variable-name-list>  ")"
        |  "DEFAULT"  "("  "NONE"  ")"
    }

    #`[C1121: (R1125) Any procedure referenced in the scalar-mask-expr, including one referenced by a defined opera- tion, shall be a pure procedure (15.7).]

    #`[C1122: (R1126) The index-name shall be a named scalar variable of type integer.]

    #`[C1123: (R1126) A concurrent-limit | concurrent-step in a concurrent-control shall not contain a reference to any index-name in the concurrent-control-list in which it appears.]

    #`[C1124: A variable-name in a locality-spec shall be the name of a variable in the innermost executable construct | scoping unit that includes the DO CONCURRENT statement.]

    #`[C1125: A variable-name in a locality-spec shall not be the same as an index-name in the concurrent-header of the same DO CONCURRENT statement.]

    #`[C1126: The name of a variable shall not appear in more than one variable-name-list, | more than once in a variable-name-list, in a given concurrent-locality.]

    #`[C1127: The DEFAULT ( NONE ) locality-spec shall not appear more than once in a given concurrent-locality.]

    #`[C1128: Avariable-name that appears in a LOCAL | LOCAL_INIT locality-spec shall not have the ALLOCAT- ABLE,INTENT(IN),orOPTIONALattribute,shallnotbeoffinalizabletype, shallnotbeanonpointer polymorphicdummyargument,andshallnotbeacoarrayoranassumed-sizearray. Avariable-name that is not permitted to appear in a variable definition context shall not appear in a LOCAL | LOCAL_INIT locality-spec.]

    #`[C1129: A variable that is referenced by the scalar-mask-expr of a concurrent-header | by any concurrent-limit | concurrent-step in that concurrent-header shall not appear in a LOCAL locality-spec in the same DO CONCURRENTstatement.]

    #`[C1130: If the locality-spec DEFAULT ( NONE ) appears in a DO CONCURRENT statement, a variable that is a local | construct entity of a scope containing the DO CONCURRENT construct, and that appears in the block of the construct, shall have its locality explicitly specified by that statement.]

    #`[R1131] token end-do {
        |  $<end-do-stmt>
        |  $<continue-stmt>
    }

    #`[R1132] token end-do-stmt {
        |  "ENDDO"  [  $<do-construct-name> ]?
    }

    #`[C1131: (R1119) If the do-stmt of a do-construct specifies a do-construct-name, the corresponding end-do shall be an end-do-stmt specifying the same do-construct-name. If the do-stmt of a do-construct does not specify a do-construct-name, the corresponding end-do shall not specify a do-construct-name.]

    #`[C1132: (R1119) If the do-stmt is a nonlabel-do-stmt, the corresponding end-do shall be an end-do-stmt.]

    #`[C1133: (R1119) If the do-stmt is a label-do-stmt, the corresponding end-do shall be identified with the same label.]

    #`[R1133] token cycle-stmt {
        |  "CYCLE"  [  $<do-construct-name> ]?
    }

    #`[C1134: If a do-construct-name appears on a CYCLE statement, the CYCLE statement shall be within that do-construct; otherwise, it shall be within at least one do-construct.]

    #`[C1135: Acycle-stmt shall not appear within a CHANGE TEAM, CRITICAL, | DO CONCURRENT construct if it belongs to an outer construct.]

    #`[C1136: A RETURN statement shall not appear within a DO CONCURRENT construct.]

    #`[C1137: An image control statement shall not appear within a DO CONCURRENT construct.]

    #`[C1138: A branch (11.2) within a DO CONCURRENT construct shall not have a branch target that is outside the construct.]

    #`[C1139: A reference to an impure procedure shall not appear within a DO CONCURRENT construct.]

    #`[C1140: A statement that might result in the deallocation of a polymorphic entity shall not appear within a DO CONCURRENTconstruct.]

    #`[C1141: A reference to the procedure IEEE_GET_FLAG, IEEE_SET_HALTING_MODE, | IEEE_GET_- HALTING_MODE from the intrinsic module IEEE_EXCEPTIONS, shall not appear within a DO CONCURRENTconstruct.]

    #`[R1134] token if-construct {
        |  $<if-then-stmt>  $<block>  [  $<else-if-stmt> $<block> ]*  [  $<else-stmt> $<block> ]?  $<end-if-stmt>
    }

    #`[R1135] token if-then-stmt {
        |  [  $<if-construct-name> ":" ]?  "IF"  "("  $<scalar-logical-expr>  ")"  "THEN"
    }

    #`[R1136] token else-if-stmt {
        |  "ELSE"  "IF"  "("  $<scalar-logical-expr>  ")"  "THEN"  [  $<if-construct-name> ]?
    }

    #`[R1137] token else-stmt {
        |  "ELSE"  [  $<if-construct-name> ]?
    }

    #`[R1138] token end-if-stmt {
        |  "ENDIF"  [  $<if-construct-name> ]?
    }

    #`[C1142: (R1134) If the if-then-stmt of an if-construct specifies an if-construct-name, the corresponding end-if- stmt shall specify the same if-construct-name. If the if-then-stmt of an if-construct does not specify an if-construct-name, the corresponding end-if-stmt shall not specify an if-construct-name. If an else-if- stmt | else-stmt specifies an if-construct-name, the corresponding if-then-stmt shall specify the same if-construct-name.]

    #`[R1139] token if-stmt {
        |  "IF"  "("  $<scalar-logical-expr>  ")"  $<action-stmt>
    }

    #`[C1143: (R1139) The action-stmt in the if-stmt shall not be an if-stmt.]

    #`[R1140] token case-construct {
        |  $<select-case-stmt>  [  $<case-stmt> $<block> ]*  $<end-select-stmt>
    }

    #`[R1141] token select-case-stmt {
        |  [  $<case-construct-name> ":" ]?  "SELECT"  "CASE"  "("  $<case-expr>  ")"
    }

    #`[R1142] token case-stmt {
        |  "CASE"  $<case-selector>  [  $<case-construct-name> ]?
    }

    #`[R1143] token end-select-stmt {
        |  "ENDSELECT"  [  $<case-construct-name> ]?
    }

    #`[C1144: (R1140)Iftheselect-case-stmt of a case-construct specifies a case-construct-name, the corresponding end- select-stmt shall specify the same case-construct-name. If the select-case-stmt of a case-construct does not specify a case-construct-name, the corresponding end-select-stmt shall not specify a case-construct- name. If a case-stmt specifies a case-construct-name, the corresponding select-case-stmt shall specify the same case-construct-name.]

    #`[R1144] token case-expr {
        |  $<scalar-expr>
    }

    #`[C1145: case-expr shall be of type character, integer, | logical.]

    #`[R1145] token case-selector {
        |  "("  $<case-value-range-list>  ")"
        |  "DEFAULT"
    }

    #`[C1146: (R1140) No more than one of the selectors of one of the CASE statements shall be DEFAULT.]

    #`[R1146] token case-value-range {
        |  $<case-value>
        |  $<case-value>  ":"
        |  ":"  $<case-value>
        |  $<case-value>  ":"  $<case-value>
    }

    #`[R1147] token case-value {
        |  $<scalar-constant-expr>
    }

    #`[C1147: (R1140) For a given case-construct, each case-value shall be of the same type as case-expr. For character type, the kind type parameters shall be the same; character length differences are allowed.]

    #`[C1148: (R1140) A case-value-range using a colon shall not be used if case-expr is of type logical.]

    #`[C1149: (R1140) For a given case-construct, there shall be no possible value of the case-expr that matches more than one case-value-range.]

    #`[R1148] token select-rank-construct {
        |  $<select-rank-stmt>  [  $<select-rank-case-stmt> $<block> ]*  $<end-select-rank-stmt>
    }

    #`[R1149] token select-rank-stmt {
        |  [  $<select-construct-name> ":" ]?  "SELECT"  "RANK"  "("  [  $<associate-name> "=>" ]?  $<selector>  ")"
    }

    #`[C1150: The selector in a select-rank-stmt shall be the name of an assumed-rank array.]

    #`[R1150] token select-rank-case-stmt {
        |  "RANK("  $<scalar-int-constant-expr>  ")"  [  $<select-construct-name> ]?
        |  "RANK(*)"  [  $<select-construct-name> ]?
        |  "RANKDEFAULT"  [  $<select-construct-name> ]?
    }

    #`[C1151: A scalar-int-constant-expr in a select-rank-case-stmt shall be nonnegative and less than | equal to the maximum possible rank of selector.]

    #`[C1152: For a given select-rank-construct, the same rank value shall not be specified in more than one select-rank- case-stmt.]

    #`[C1153: For a given select-rank-construct, there shall be at most one RANK ( * ) select-rank-case-stmt and at most one RANK DEFAULT select-rank-case-stmt.]

    #`[C1154: If select-construct-name appears on a select-rank-case-stmt the corresponding select-rank-stmt shall spe- cify the same select-construct-name.]

    #`[C1155: A SELECT RANK construct shall not have a select-rank-case-stmt that is RANK ( * ) if the selector has the ALLOCATABLE | POINTER attribute.]

    #`[R1151] token end-select-rank-stmt {
        |  "ENDSELECT"  [  $<select-construct-name> ]?
    }

    #`[C1156: If the select-rank-stmt of a select-rank-construct specifies a select-construct-name, the corresponding end-select-rank-stmt shall specify the same select-construct-name. If the select-rank-stmt of a select- rank-construct does not specify a select-construct-name, the corresponding end-select-rank-stmt shall not specify a select-construct-name.]

    #`[R1152] token select-type-construct {
        |  $<select-type-stmt>  [  $<type-guard-stmt> $<block> ]*  $<end-select-type-stmt>
    }

    #`[R1153] token select-type-stmt {
        |  [  $<select-construct-name> ":" ]?  "SELECT"  "TYPE"  "("  [  $<associate-name> "=>" ]?  $<selector>  ")"
    }

    #`[C1157: (R1153) If selector is not a named variable, associate-name => shall appear.]

    #`[C1158: (R1153) If selector is not a variable | is a variable that has a vector subscript, neither associate-name nor any subobject thereof shall appear in a variable definition context (19.6.7).]

    #`[C1159: (R1153) The selector in a select-type-stmt shall be polymorphic.]

    #`[R1154] token type-guard-stmt {
        |  "TYPEIS("  $<type-spec>  ")"  [  $<select-construct-name> ]?
        |  "CLASS"  "IS"  "("  $<derived-type-spec>  ")"  [  $<select-construct-name> ]?
        |  "CLASS"  "DEFAULT"  [  $<select-construct-name> ]?
    }

    #`[C1160: (R1154) The type-spec | derived-type-spec shall specify that each length type parameter is assumed.]

    #`[C1161: (R1154) The type-spec | derived-type-spec shall not specify a type with the BIND attribute | the SEQUENCEattribute.]

    #`[C1162: (R1152) If selector is not unlimited polymorphic, each TYPE IS | CLASS IS type-guard-stmt shall specify an extension of the declared type of selector.]

    #`[C1163: (R1152) For a given select-type-construct, the same type and kind type parameter values shall not be specified in more than one TYPE IS type-guard-stmt and shall not be specified in more than one CLASS IS type-guard-stmt.]

    #`[C1164: (R1152) For a given select-type-construct, there shall be at most one CLASS DEFAULT type-guard-stmt.]

    #`[R1155] token end-select-type-stmt {
        |  "ENDSELECT"  [  $<select-construct-name> ]?
    }

    #`[C1165: (R1152)Iftheselect-type-stmt of a select-type-construct specifies a select-construct-name, the correspond- ing end-select-type-stmt shall specify the same select-construct-name. If the select-type-stmt of a select- type-construct does not specify a select-construct-name, the corresponding end-select-type-stmt shall not specify a select-construct-name. If a type-guard-stmt specifies a select-construct-name, the corresponding select-type-stmt shall specify the same select-construct-name.]

    #`[R1156] token exit-stmt {
        |  "EXIT"  [  $<construct-name> ]?
    }

    #`[C1166: If a construct-name appears on an EXIT statement, the EXIT statement shall be within that construct; otherwise, it shall be within at least one do-construct.]

    #`[C1167: Anexit-stmt shall not appear within a CHANGE TEAM, CRITICAL, | DO CONCURRENT construct if it belongs to that construct | an outer construct.]

    #`[R1157] token goto-stmt {
        |  "GOTO"  $<label>
    }

    #`[C1168: (R1157) The label shall be the statement label of a branch target statement that appears in the same inclusive scope as the goto-stmt.]

    #`[R1158] token computed-goto-stmt {
        |  "GOTO("  $<label-list>  ")"  [  "," ]?  $<scalar-int-expr>
    }

    #`[C1169: (R1158) Each label in label-list shall be the statement label of a branch target statement that appears in the same inclusive scope as the computed-goto-stmt.]

    #`[R1159] token continue-stmt {
        |  "CONTINUE"
    }

    #`[R1160] token stop-stmt {
        |  "STOP"  [  $<stop-code> ]?  [  "," "QUIET" "=" $<scalar-logical-expr> ]?
    }

    #`[R1161] token error-stop-stmt {
        |  "ERRORSTOP"  [  $<stop-code> ]?  [  "," "QUIET" "=" $<scalar-logical-expr> ]?
    }

    #`[R1162] token stop-code {
        |  $<scalar-default-char-expr>
        |  $<scalar-int-expr>
    }

    #`[C1170: (R1162) The scalar-int-expr shall be of default kind.]

    #`[R1163] token fail-image-stmt {
        |  "FAIL"  "IMAGE"
    }

    #`[R1164] token sync-all-stmt {
        |  "SYNCALL"  [  "(" [  $<sync-stat-list> ]? ")" ]?
    }

    #`[R1165] token sync-stat {
        |  "STAT"  "="  $<stat-variable>
        |  "ERRMSG="  $<errmsg-variable>
    }

    #`[C1171: No specifier shall appear more than once in a given sync-stat-list.]

    #`[C1172: A stat-variable | errmsg-variable in a sync-stat shall not be a coindexed object.]

    #`[R1166] token sync-images-stmt {
        |  "SYNCIMAGES("  $<image-set>  [  "," $<sync-stat-list> ]?  ")"
    }

    #`[R1167] token image-set {
        |  $<int-expr>
        |  "*"
    }

    #`[C1173: An image-set that is an int-expr shall be scalar | of rank one.]

    #`[C1174: The value of image-set shall not depend on the value of stat-variable | errmsg-variable.]

    #`[R1168] token sync-memory-stmt {
        |  "SYNCMEMORY"  [  "(" [  $<sync-stat-list> ]? ")" ]?
    }

    #`[R1169] token sync-team-stmt {
        |  "SYNCTEAM("  $<team-value>  [  "," $<sync-stat-list> ]?  ")"
    }

    #`[R1170] token event-post-stmt {
        |  "EVENTPOST("  $<event-variable>  [  "," $<sync-stat-list> ]?  ")"
    }

    #`[R1171] token event-variable {
        |  $<scalar-variable>
    }

    #`[C1175: (R1171)Anevent-variable shall be of type EVENT_TYPEfromtheintrinsicmoduleISO_FORTRAN_-]

    #`[R1172] token event-wait-stmt {
        |  "EVENTWAIT("  $<event-variable>  [  "," $<event-wait-spec-list> ]?  ")"
    }

    #`[R1173] token event-wait-spec {
        |  $<until-spec>
        |  $<sync-stat>
    }

    #`[R1174] token until-spec {
        |  "UNTIL_COUNT="  $<scalar-int-expr>
    }

    #`[C1176: (R1172) The event-variable in an event-wait-stmt shall not be coindexed.]

    #`[C1177: No specifier shall appear more than once in a given event-wait-spec-list.]

    #`[R1175] token form-team-stmt {
        |  "FORMTEAM("  $<team-number>  ","  $<team-variable>  [  "," $<form-team-spec-list> ]?  ")"
    }

    #`[R1176] token team-number {
        |  $<scalar-int-expr>
    }

    #`[R1177] token team-variable {
        |  $<scalar-variable>
    }

    #`[C1178: A team-variable shall be of type TEAM_TYPE from the intrinsic module ISO_FORTRAN_ENV.]

    #`[R1178] token form-team-spec {
        |  "NEW_INDEX="  $<scalar-int-expr>
        |  $<sync-stat>
    }

    #`[C1179: No specifier shall appear more than once in a given form-team-spec-list.]

    #`[R1179] token lock-stmt {
        |  "LOCK("  $<lock-variable>  [  "," $<lock-stat-list> ]?  ")"
    }

    #`[R1180] token lock-stat {
        |  "ACQUIRED_LOCK="  $<scalar-logical-variable>
        |  $<sync-stat>
    }

    #`[C1180: No specifier shall appear more than once in a given lock-stat-list.]

    #`[R1181] token unlock-stmt {
        |  "UNLOCK("  $<lock-variable>  [  "," $<sync-stat-list> ]?  ")"
    }

    #`[R1182] token lock-variable {
        |  $<scalar-variable>
    }

    #`[C1181: (R1182)Alock-variable shall be of type LOCK_TYPEfromtheintrinsic module ISO_FORTRAN_ENV (16.10.2.19).]

    #`[R1201] token io-unit {
        |  $<file-unit-number>
        |  "*"
        |  $<internal-file-variable>
    }

    #`[R1202] token file-unit-number {
        |  $<scalar-int-expr>
    }

    #`[R1203] token internal-file-variable {
        |  $<char-variable>
    }

    #`[C1201: (R1203) The char-variable shall not be an array section with a vector subscript.]

    #`[C1202: (R1203) The char-variable shall be default character, ASCII character, | ISO 10646 character.]

    #`[R1204] token open-stmt {
        |  "OPEN("  $<connect-spec-list>  ")"
    }

    #`[R1205] token connect-spec {
        |  [  "UNIT" "=" ]?  $<file-unit-number>
        |  "ACCESS"  "="  $<scalar-default-char-expr>
        |  "ACTION"  "="  $<scalar-default-char-expr>
        |  "ASYNCHRONOUS="  $<scalar-default-char-expr>
        |  "BLANK="  $<scalar-default-char-expr>
        |  "DECIMAL"  "="  $<scalar-default-char-expr>
        |  "DELIM"  "="  $<scalar-default-char-expr>
        |  "ENCODING="  $<scalar-default-char-expr>
        |  "ERR"  "="  $<label>
        |  "FILE"  "="  $<file-name-expr>
        |  "FORM="  $<scalar-default-char-expr>
        |  "IOMSG"  "="  $<iomsg-variable>
        |  "IOSTAT"  "="  $<stat-variable>
        |  "NEWUNIT="  $<scalar-int-variable>
        |  "PAD"  "="  $<scalar-default-char-expr>
        |  "POSITION"  "="  $<scalar-default-char-expr>
        |  "RECL"  "="  $<scalar-int-expr>
        |  "ROUND="  $<scalar-default-char-expr>
        |  "SIGN"  "="  $<scalar-default-char-expr>
        |  "STATUS"  "="  $<scalar-default-char-expr>
    }

    #`[R1206] token file-name-expr {
        |  $<scalar-default-char-expr>
    }

    #`[R1207] token iomsg-variable {
        |  $<scalar-default-char-variable>
    }

    #`[C1203: No specifier shall appear more than once in a given connect-spec-list.]

    #`[C1204: (R1204)IftheNEWUNIT=specifierdoesnotappear,afile-unit-number shallbespecified; iftheoptional characters UNIT= are omitted, the file-unit-number shall be the first item in the connect-spec-list.]

    #`[C1205: (R1204) If a NEWUNIT= specifier appears, a file-unit-number shall not appear.]

    #`[C1206: (R1204) The label used in the ERR= specifier shall be the statement label of a branch target statement that appears in the same inclusive scope as the OPEN statement.]

    #`[R1208] token close-stmt {
        |  "CLOSE("  $<close-spec-list>  ")"
    }

    #`[R1209] token close-spec {
        |  [  "UNIT" "=" ]?  $<file-unit-number>
        |  "IOSTAT"  "="  $<stat-variable>
        |  "IOMSG"  "="  $<iomsg-variable>
        |  "ERR"  "="  $<label>
        |  "STATUS"  "="  $<scalar-default-char-expr>
    }

    #`[C1207: No specifier shall appear more than once in a given close-spec-list.]

    #`[C1208: A file-unit-number shall be specified in a close-spec-list; if the optional characters UNIT= are omitted, the file-unit-number shall be the first item in the close-spec-list.]

    #`[C1209: (R1209) The label used in the ERR= specifier shall be the statement label of a branch target statement that appears in the same inclusive scope as the CLOSE statement.]

    #`[R1210] token read-stmt {
        |  "READ("  $<io-control-spec-list>  ")"  [  $<input-item-list> ]?
        |  "READ"  $<format>  [  "," $<input-item-list> ]?
    }

    #`[R1211] token write-stmt {
        |  "WRITE("  $<io-control-spec-list>  ")"  [  $<output-item-list> ]?
    }

    #`[R1212] token print-stmt {
        |  "PRINT"  $<format>  [  "," $<output-item-list> ]?
    }

    #`[R1213] token io-control-spec {
        |  [  "UNIT" "=" ]?  $<io-unit>
        |  [  "FMT" "=" ]?  $<format>
        |  [  "NML" "=" ]?  $<namelist-group-name>
        |  "ADVANCE="  $<scalar-default-char-expr>
        |  "ASYNCHRONOUS="  $<scalar-default-char-constant-expr>
        |  "BLANK="  $<scalar-default-char-expr>
        |  "DECIMAL"  "="  $<scalar-default-char-expr>
        |  "DELIM"  "="  $<scalar-default-char-expr>
        |  "END="  $<label>
        |  "EOR="  $<label>
        |  "ERR"  "="  $<label>
        |  "ID"  "="  $<id-variable>
        |  "IOMSG"  "="  $<iomsg-variable>
        |  "IOSTAT"  "="  $<stat-variable>
        |  "PAD"  "="  $<scalar-default-char-expr>
        |  "POS"  "="  $<scalar-int-expr>
        |  "REC"  "="  $<scalar-int-expr>
        |  "ROUND="  $<scalar-default-char-expr>
        |  "SIGN"  "="  $<scalar-default-char-expr>
        |  "SIZE"  "="  $<scalar-int-variable>
    }

    #`[R1214] token id-variable {
        |  $<scalar-int-variable>
    }

    #`[C1210: No specifier shall appear more than once in a given io-control-spec-list.]

    #`[C1211: Anio-unit shall be specified in an io-control-spec-list; if the optional characters UNIT= are omitted, the io-unit shall be the first item in the io-control-spec-list.]

    #`[C1212: (R1213) A DELIM= | SIGN= specifier shall not appear in a read-stmt.]

    #`[C1213: (R1213) A BLANK=, PAD=, END=, EOR=, | SIZE= specifier shall not appear in a write-stmt.]

    #`[C1214: (R1213) The label in the ERR=, EOR=, | END= specifier shall be the statement label of a branch target statement that appears in the same inclusive scope as the data transfer statement.]

    #`[C1215: (R1213) A namelist-group-name shall be the name of a namelist group.]

    #`[C1216: (R1213) A namelist-group-name shall not appear if a REC= specifier, format, input-item-list, | an output-item-list appears in the data transfer statement.]

    #`[C1217: (R1213) If format appears without a preceding FMT=, it shall be the second item in the io-control-spec-]

    #`[C1218: (R1213) If namelist-group-name appears without a preceding NML=, it shall be the second item in the io-control-spec-list and the first item shall be io-unit.]

    #`[C1219: (R1213) If io-unit is not a file-unit-number, the io-control-spec-list shall not contain a REC= specifier | a POS= specifier.]

    #`[C1220: (R1213) If the REC= specifier appears, an END= specifier shall not appear, and the format, if any, shall not be an asterisk.]

    #`[C1221: (R1213) An ADVANCE= specifier may appear only in a formatted sequential | stream data transfer statement with explicit format specification (13.2) whose io-control-spec-list does not contain an internal- file-variable as the io-unit.]

    #`[C1222: (R1213) If an EOR= specifier appears, an ADVANCE= specifier also shall appear.]

    #`[C1223: (R1213) The scalar-default-char-constant-expr in an ASYNCHRONOUS= specifier shall have the value YES | NO.]

    #`[C1224: (R1213) An ASYNCHRONOUS= specifier with a value YES shall not appear unless io-unit is a file- unit-number.]

    #`[C1225: (R1213) If an ID= specifier appears, an ASYNCHRONOUS= specifier with the value YES shall also appear.]

    #`[C1226: (R1213) If a POS= specifier appears, the io-control-spec-list shall not contain a REC= specifier.]

    #`[C1227: (R1213) If a DECIMAL=, BLANK=, PAD=, SIGN=, | ROUND= specifier appears, a format | namelist-group-name shall also appear.]

    #`[C1228: (R1213) If a DELIM= specifier appears, either format shall be an asterisk | namelist-group-name shall appear.]

    #`[C1229: (R1214) The scalar-int-variable shall have a decimal exponent range no smaller than that of default integer.]

    #`[R1215] token format {
        |  $<default-char-expr>
        |  $<label>
        |  "*"
    }

    #`[C1230: (R1215) The label shall be the label of a FORMAT statement that appears in the same inclusive scope as the statement containing the FMT= specifier.]

    #`[R1216] token input-item {
        |  $<variable>
        |  $<io-implied-do>
    }

    #`[R1217] token output-item {
        |  $<expr>
        |  $<io-implied-do>
    }

    #`[R1218] token io-implied-do {
        |  "("  $<io-implied-do-object-list>  ","  $<io-implied-do-control>  ")"
    }

    #`[R1219] token io-implied-do-object {
        |  $<input-item>
        |  $<output-item>
    }

    #`[R1220] token io-implied-do-control {
        |  $<do-variable>  "="  $<scalar-int-expr>  ","  $<scalar-int-expr>  [  "," $<scalar-int-expr> ]?
    }

    #`[C1231: (R1216) A variable that is an input-item shall not be a whole assumed-size array.]

    #`[C1232: (R1219) In an input-item-list, an io-implied-do-object shall be an input-item. In an output-item-list, an io-implied-do-object shall be an output-item.]

    #`[C1233: (R1217) An expression that is an output-item shall not have a value that is a procedure pointer.]

    #`[R1221] token dtv-type-spec {
        |  "TYPE("  $<derived-type-spec>  ")"
        |  "CLASS("  $<derived-type-spec>  ")"
    }

    #`[C1234: (R1221) If derived-type-spec specifies an extensible type, the CLASS keyword shall be used; otherwise, the TYPE keyword shall be used.]

    #`[C1235: (R1221) All length type parameters of derived-type-spec shall be assumed.]

    #`[R1222] token wait-stmt {
        |  "WAIT("  $<wait-spec-list>  ")"
    }

    #`[R1223] token wait-spec {
        |  [  "UNIT" "=" ]?  $<file-unit-number>
        |  "END="  $<label>
        |  "EOR="  $<label>
        |  "ERR"  "="  $<label>
        |  "ID"  "="  $<scalar-int-expr>
        |  "IOMSG"  "="  $<iomsg-variable>
        |  "IOSTAT"  "="  $<stat-variable>
    }

    #`[C1236: No specifier shall appear more than once in a given wait-spec-list.]

    #`[C1237: A file-unit-number shall be specified in a wait-spec-list; if the optional characters UNIT= are omitted, the file-unit-number shall be the first item in the wait-spec-list.]

    #`[C1238: (R1223) The label in the ERR=, EOR=, | END= specifier shall be the statement label of a branch target statement that appears in the same inclusive scope as the WAIT statement.]

    #`[R1224] token backspace-stmt {
        |  "BACKSPACE"  $<file-unit-number>
        |  "BACKSPACE("  $<position-spec-list>  ")"
    }

    #`[R1225] token endfile-stmt {
        |  "ENDFILE"  $<file-unit-number>
        |  "ENDFILE"  "("  $<position-spec-list>  ")"
    }

    #`[R1226] token rewind-stmt {
        |  "REWIND"  $<file-unit-number>
        |  "REWIND("  $<position-spec-list>  ")"
    }

    #`[R1227] token position-spec {
        |  [  "UNIT" "=" ]?  $<file-unit-number>
        |  "IOMSG"  "="  $<iomsg-variable>
        |  "IOSTAT"  "="  $<stat-variable>
        |  "ERR"  "="  $<label>
    }

    #`[C1239: No specifier shall appear more than once in a given position-spec-list.]

    #`[C1240: Afile-unit-number shall be specified in a position-spec-list; if the optional characters UNIT= are omitted, the file-unit-number shall be the first item in the position-spec-list.]

    #`[C1241: (R1227) The label in the ERR= specifier shall be the statement label of a branch target statement that appears in the same inclusive scope as the file positioning statement.]

    #`[R1228] token flush-stmt {
        |  "FLUSH"  $<file-unit-number>
        |  "FLUSH"  "("  $<flush-spec-list>  ")"
    }

    #`[R1229] token flush-spec {
        |  [  "UNIT" "=" ]?  $<file-unit-number>
        |  "IOSTAT"  "="  $<stat-variable>
        |  "IOMSG"  "="  $<iomsg-variable>
        |  "ERR"  "="  $<label>
    }

    #`[C1242: No specifier shall appear more than once in a given flush-spec-list.]

    #`[C1243: A file-unit-number shall be specified in a flush-spec-list; if the optional characters UNIT= are omitted from the unit specifier, the file-unit-number shall be the first item in the flush-spec-list.]

    #`[C1244: (R1229) The label in the ERR= specifier shall be the statement label of a branch target statement that appears in the same inclusive scope as the FLUSH statement.]

    #`[R1230] token inquire-stmt {
        |  "INQUIRE"  "("  $<inquire-spec-list>  ")"
        |  "INQUIRE"  "("  "IOLENGTH"  "="  $<scalar-int-variable>  ")"  $<output-item-list>
    }

    #`[R1231] token inquire-spec {
        |  [  "UNIT" "=" ]?  $<file-unit-number>
        |  "FILE"  "="  $<file-name-expr>
        |  "ACCESS"  "="  $<scalar-default-char-variable>
        |  "ACTION"  "="  $<scalar-default-char-variable>
        |  "ASYNCHRONOUS="  $<scalar-default-char-variable>
        |  "BLANK="  $<scalar-default-char-variable>
        |  "DECIMAL"  "="  $<scalar-default-char-variable>
        |  "DELIM"  "="  $<scalar-default-char-variable>
        |  "DIRECT"  "="  $<scalar-default-char-variable>
        |  "ENCODING="  $<scalar-default-char-variable>
        |  "ERR"  "="  $<label>
        |  "EXIST"  "="  $<scalar-logical-variable>
        |  "FORM="  $<scalar-default-char-variable>
        |  "FORMATTED="  $<scalar-default-char-variable>
        |  "ID"  "="  $<scalar-int-expr>
        |  "IOMSG"  "="  $<iomsg-variable>
        |  "IOSTAT"  "="  $<stat-variable>
        |  "NAME="  $<scalar-default-char-variable>
        |  "NAMED="  $<scalar-logical-variable>
        |  "NEXTREC="  $<scalar-int-variable>
        |  "NUMBER="  $<scalar-int-variable>
        |  "OPENED="  $<scalar-logical-variable>
        |  "PAD"  "="  $<scalar-default-char-variable>
        |  "PENDING"  "="  $<scalar-logical-variable>
        |  "POS"  "="  $<scalar-int-variable>
        |  "POSITION"  "="  $<scalar-default-char-variable>
        |  "READ="  $<scalar-default-char-variable>
        |  "READWRITE="  $<scalar-default-char-variable>
        |  "RECL"  "="  $<scalar-int-variable>
        |  "ROUND="  $<scalar-default-char-variable>
        |  "SEQUENTIAL"  "="  $<scalar-default-char-variable>
        |  "SIGN"  "="  $<scalar-default-char-variable>
        |  "SIZE"  "="  $<scalar-int-variable>
        |  "STREAM="  $<scalar-default-char-variable>
        |  "UNFORMATTED="  $<scalar-default-char-variable>
        |  "WRITE="  $<scalar-default-char-variable>
    }

    #`[C1245: No specifier shall appear more than once in a given inquire-spec-list.]

    #`[C1246: An inquire-spec-list shall contain one FILE= specifier | one file-unit-number, but not both.]

    #`[C1247: In the inquire by unit form of the INQUIRE statement, if the optional characters UNIT= are omitted, the file-unit-number shall be the first item in the inquire-spec-list.]

    #`[C1248: If an ID= specifier appears in an inquire-spec-list, a PENDING= specifier shall also appear.]

    #`[C1249: (R1229) The label in the ERR= specifier shall be the statement label of a branch target statement that appears in the same inclusive scope as the INQUIRE statement.]

    #`[R1301] token format-stmt {
        |  "FORMAT"  $<format-specification>
    }

    #`[R1302] token format-specification {
        |  "("  [  $<format-items> ]?  ")"
        |  "("  [  $<format-items> "," ]?  $<unlimited-format-item>  ")"
    }

    #`[C1301: (R1301) The format-stmt shall be labeled.]

    #`[R1303] token format-items {
        |  $<format-item>  [  [  "," ]? $<format-item> ]*
    }

    #`[R1304] token format-item {
        |  [  $<r> ]?  $<data-edit-desc>
        |  $<control-edit-desc>
        |  $<char-string-edit-desc>
        |  [  $<r> ]?  "("  $<format-items>  ")"
    }

    #`[R1305] token unlimited-format-item {
        |  "*"  "("  $<format-items>  ")"
    }

    #`[R1306] token r {
        |  $<int-literal-constant>
    }

    #`[C1302: (R1303) The optional comma shall not be omitted except *> between a P edit descriptor and an immediately following F, E, EN, ES, D, | G edit descriptor (13.8.5), possibly preceded by a repeat specification, *> before a slash edit descriptor when the optional repeat specification does not appear (13.8.2), *> after a slash edit descriptor, | *> before | after a colon edit descriptor (13.8.3)]

    #`[C1303: (R1305) An unlimited-format-item shall contain at least one data edit descriptor.]

    #`[C1304: (R1306) r shall be positive.]

    #`[C1305: (R1306) A kind parameter shall not be specified for r.]

    #`[R1307] token data-edit-desc {
        |  "I"  $<w>  [  "." $<m> ]?
        |  "B"  $<w>  [  "." $<m> ]?
        |  "O"  $<w>  [  "." $<m> ]?
        |  "Z"  $<w>  [  "." $<m> ]?
        |  "F"  $<w>  "."  $<d>
        |  "E"  $<w>  "."  $<d>  [  "E" $<e> ]?
        |  "EN"  $<w>  "."  $<d>  [  "E" $<e> ]?
        |  "ES"  $<w>  "."  $<d>  [  "E" $<e> ]?
        |  "EX"  $<w>  "."  $<d>  [  "E" $<e> ]?
        |  "G"  $<w>  [  "." $<d> [  "E" $<e> ]? ]?
        |  "L"  $<w>
        |  "A"  [  $<w> ]?
        |  "D"  $<w>  "."  $<d>
        |  "DT"  [  $<char-literal-constant> ]?  [  "(" $<v-list> ")" ]?
    }

    #`[R1308] token w {
        |  $<int-literal-constant>
    }

    #`[R1309] token m {
        |  $<int-literal-constant>
    }

    #`[R1310] token d {
        |  $<int-literal-constant>
    }

    #`[R1311] token e {
        |  $<int-literal-constant>
    }

    #`[R1312] token v {
        |  $<signed-int-literal-constant>
    }

    #`[C1306: (R1308) w shall be zero | positive for the I, B, O, Z, D, E, EN, ES, EX, F, and G edit descriptors. w shall be positive for all other edit descriptors.]

    #`[C1307: (R1307) For the G edit descriptor, d shall be specified if w is not zero.]

    #`[C1308: (R1307) For the G edit descriptor, e shall not be specified if w is zero.]

    #`[C1309: (R1307) A kind parameter shall not be specified for the char-literal-constant in the DT edit descriptor, | for w, m, d, e, and v.]

    #`[R1313] token control-edit-desc {
        |  $<position-edit-desc>
        |  [  $<r> ]?  "/"
        |  ":"
        |  $<sign-edit-desc>
        |  $<k>  "P"
        |  $<blank-interp-edit-desc>
        |  $<round-edit-desc>
        |  $<decimal-edit-desc>
    }

    #`[R1314] token k {
        |  $<signed-int-literal-constant>
    }

    #`[C1310: (R1314) A kind parameter shall not be specified for k.]

    #`[R1315] token position-edit-desc {
        |  "T"  $<n>
        |  "TL"  $<n>
        |  "TR"  $<n>
        |  $<n>  "X"
    }

    #`[R1316] token n {
        |  $<int-literal-constant>
    }

    #`[C1311: (R1316) n shall be positive.]

    #`[C1312: (R1316) A kind parameter shall not be specified for n.]

    #`[R1317] token sign-edit-desc {
        |  "SS"
        |  "SP"
        |  "S"
    }

    #`[R1318] token blank-interp-edit-desc {
        |  "BN"
        |  "BZ"
    }

    #`[R1319] token round-edit-desc {
        |  "RU"
        |  "RD"
        |  "RZ"
        |  "RN"
        |  "RC"
        |  "RP"
    }

    #`[R1320] token decimal-edit-desc {
        |  "DC"
        |  "DP"
    }

    #`[R1321] token char-string-edit-desc {
        |  $<char-literal-constant>
    }

    #`[C1313: (R1321) A kind parameter shall not be specified for the char-literal-constant.]

    #`[R1322] token hex-digit-string {
        |  $<hex-digit>  [  $<hex-digit> ]*
    }

    #`[R1402] token program-stmt {
        |  "PROGRAM"  $<program-name>
    }

    #`[R1403] token end-program-stmt {
        |  "END"  [  "PROGRAM" [  $<program-name> ]? ]?
    }

    #`[C1401: (R1401) The program-name may be included in the end-program-stmt only if the optional program-stmt is used and, if included, shall be identical to the program-name specified in the program-stmt.]

    #`[R1405] token module-stmt {
        |  "MODULE"  $<module-name>
    }

    #`[R1406] token end-module-stmt {
        |  "END"  [  "MODULE" [  $<module-name> ]? ]?
    }

    #`[C1402: (R1404) If the module-name is specified in the end-module-stmt, it shall be identical to the module-name specified in the module-stmt.]

    #`[C1403: (R1404) A module specification-part shall not contain a stmt-function-stmt, an entry-stmt, | a format-stmt.]

    #`[R1409] token use-stmt {
        |  "USE"  [  [  "," $<module-nature> ]? "::" ]?  $<module-name>  [  "," $<rename-list> ]?
        |  "USE"  [  [  "," $<module-nature> ]? "::" ]?  $<module-name>  ","  "ONLY:"  [  $<only-list> ]?
    }

    #`[R1410] token module-nature {
        |  "INTRINSIC"
        |  "NON_INTRINSIC"
    }

    #`[R1411] token rename {
        |  $<local-name>  "=>"  $<use-name>
        |  "OPERATOR("  $<local-defined-operator>  ")"  "=>"  "OPERATOR("  $<use-defined-operator>  ")"
    }

    #`[R1412] token only {
        |  $<generic-spec>
        |  $<only-use-name>
        |  $<rename>
    }

    #`[R1413] token only-use-name {
        |  $<use-name>
    }

    #`[C1404: (R1409) If module-nature is INTRINSIC, module-name shall be the name of an intrinsic module.]

    #`[C1405: (R1409)Ifmodule-nature is NON_INTRINSIC,module-name shallbethenameofanonintrinsicmodule.]

    #`[C1406: (R1409) A scoping unit shall not directly reference an intrinsic module and a nonintrinsic module of the same name.]

    #`[C1407: (R1411) OPERATOR (use-defined-operator) shall not identify a type-bound generic interface.]

    #`[C1408: (R1412) The generic-spec shall not identify a type-bound generic interface.]

    #`[C1409: Each generic-spec, use-name, and use-defined-operator in a USE statement shall be a public identifier of the module.]

    #`[C1410: An only-use-name shall be a nongeneric name.]

    #`[R1414] token local-defined-operator {
        |  $<defined-unary-op>
        |  $<defined-binary-op>
    }

    #`[R1415] token use-defined-operator {
        |  $<defined-unary-op>
        |  $<defined-binary-op>
    }

    #`[R1417] token submodule-stmt {
        |  "SUBMODULE("  $<parent-identifier>  ")"  $<submodule-name>
    }

    #`[R1418] token parent-identifier {
        |  $<ancestor-module-name>  [  ":" $<parent-submodule-name> ]?
    }

    #`[R1419] token end-submodule-stmt {
        |  "END"  [  "SUBMODULE" [  $<submodule-name> ]? ]?
    }

    #`[C1411: (R1416) A submodule specification-part shall not contain a format-stmt, entry-stmt, | stmt-function-stmt.]

    #`[C1412: (R1418) The ancestor-module-name shall be the name of a nonintrinsic module that declares a separate module procedure; the parent-submodule-name shall be the name of a descendant of that module.]

    #`[C1413: (R1416) If a submodule-name appears in the end-submodule-stmt, it shall be identical to the one in the submodule-stmt.]

    #`[R1421] token block-data-stmt {
        |  "BLOCKDATA"  [  $<block-data-name> ]?
    }

    #`[R1422] token end-block-data-stmt {
        |  "END"  [  "BLOCKDATA" [  $<block-data-name> ]? ]?
    }

    #`[C1414: (R1420) The block-data-name shall be included in the end-block-data-stmt only if it was provided in the block-data-stmt and, if included, shall be identical to the block-data-name in the block-data-stmt.]

    #`[C1415: (R1420) A block-data specification-part shall contain only derived-type definitions and ASYNCHRONOUS, BIND, COM- MON, DATA, DIMENSION, EQUIVALENCE, IMPLICIT, INTRINSIC, PARAMETER, POINTER, SAVE, TARGET, USE, VOLATILE, and type declaration statements.]

    #`[C1416: (R1420) A type declaration statement in a block-data specification-part shall not contain ALLOCATABLE, EXTERNAL, | BIND attribute specifiers.]

    #`[R1501] token interface-block {
        |  $<interface-stmt>  [  $<interface-specification> ]*  $<end-interface-stmt>
    }

    #`[R1502] token interface-specification {
        |  $<interface-body>
        |  $<procedure-stmt>
    }

    #`[R1503] token interface-stmt {
        |  "INTERFACE"  [  $<generic-spec> ]?
        |  "ABSTRACTINTERFACE"
    }

    #`[R1504] token end-interface-stmt {
        |  "ENDINTERFACE"  [  $<generic-spec> ]?
    }

    #`[R1505] token interface-body {
        |  $<function-stmt>  [  $<specification-part> ]?  $<end-function-stmt>
        |  $<subroutine-stmt>  [  $<specification-part> ]?  $<end-subroutine-stmt>
    }

    #`[R1506] token procedure-stmt {
        |  [  "MODULE" ]?  "PROCEDURE"  [  "::" ]?  $<specific-procedure-list>
    }

    #`[R1507] token specific-procedure {
        |  $<procedure-name>
    }

    #`[R1508] token generic-spec {
        |  $<generic-name>
        |  "OPERATOR("  $<defined-operator>  ")"
        |  "ASSIGNMENT"  "("  "="  ")"
        |  $<defined-io-generic-spec>
    }

    #`[R1509] token defined-io-generic-spec {
        |  "READ(FORMATTED)"
        |  "READ(UNFORMATTED)"
        |  "WRITE(FORMATTED)"
        |  "WRITE(UNFORMATTED)"
    }

    #`[C1501: (R1501) An interface-block in a subprogram shall not contain an interface-body for a procedure defined by that subprogram.]

    #`[C1502: (R1501) If the end-interface-stmt includes a generic-spec, the interface-stmt shall specify the same generic-spec, except that if one generic-spec has a defined-operator that is .LT., .LE., .GT., .GE., .EQ., | .NE., the other generic-spec may have a defined-operator that is the corresponding operator <, <=, >, >=, ==, | /=.]

    #`[C1503: (R1503) If the interface-stmt is ABSTRACT INTERFACE, then the function-name in the function-stmt | the subroutine-name in the subroutine-stmt shall not be the same as a keyword that specifies an intrinsic type.]

    #`[C1504: (R1502) A procedure-stmt is allowed only in an interface block that has a generic-spec.]

    #`[C1505: (R1505) An interface-body of a pure procedure shall specify the intents of all dummy arguments except alternate return indicators, dummy procedures, and arguments with the POINTER | VALUE attribute.]

    #`[C1506: (R1505) An interface-body shall not contain a data-stmt, format-stmt, entry-stmt, | stmt-function-stmt.]

    #`[C1507: (R1506) If MODULE appears in a procedure-stmt, each procedure-name in that statement shall denote a module procedure.]

    #`[C1508: (R1507) A procedure-name shall denote a nonintrinsic procedure that has an explicit interface.]

    #`[C1509: (R1501) An interface-specification in a generic interface block shall not specify a procedure that was specified previously in any accessible interface with the same generic identifier.]

    #`[R1510] token generic-stmt {
        |  "GENERIC"  [  "," $<access-spec> ]?  "::"  $<generic-spec>  "=>"  $<specific-procedure-list>
    }

    #`[C1510: (R1510) A specific-procedure in a GENERIC statement shall not specify a procedure that was specified previously in any accessible interface with the same generic identifier.]

    #`[C1511: Within the scope of a generic operator, if two procedures with that identifier have the same number of arguments, one shall have a dummy argument that corresponds by position in the argument list to a dummy argument of the other that is distinguishable from it.]

    #`[C1512: Within the scope of the generic ASSIGNMENT (=) identifier, if two procedures have that identifier, one shall have a dummy argument that corresponds by position in the argument list to a dummy argument of the other that is distinguishable from it.]

    #`[C1513: Within the scope of a defined-io-generic-spec, if two procedures have that generic identifier, their dtv arguments (12.6.4.8.2) shall be distinguishable.]

    #`[C1514: Within the scope of a generic name, each pair of procedures identified by that name shall both be subroutines | both be functions, and (1)    there is a non-passed-object dummy data object in one | the other of them such that (a)   the number of dummy data objects in one that are nonoptional, are not passed-object, and with which that dummy data object is TKR compatible, possibly including that dummy data object itself, exceeds (b)   the number of non-passed-object dummy data objects, both optional and nonoptional, in the other that are not distinguishable from that dummy data object, (2)   the number of nonoptional dummy procedures in one of them exceeds the number of dummy procedures in the other, (3)   both have passed-object dummy arguments and the passed-object dummy arguments are distin- guishable, | (4)   at least one of them shall have both (a)    a nonoptional non-passed-object dummy argument at an effective position such that either the other procedure has no dummy argument at that effective position | the dummy argu- ment at that position is distinguishable from it, and (b)    a nonoptional non-passed-object dummy argument whose name is such that either the other procedure has no dummy argument with that name | the dummy argument with that name is distinguishable from it, and the dummy argument that disambiguates by position shall either be the same as | occur earlier in the argument list than the one that disambiguates by name.]

    #`[R1511] token external-stmt {
        |  "EXTERNAL"  [  "::" ]?  $<external-name-list>
    }

    #`[R1512] token procedure-declaration-stmt {
        |  "PROCEDURE("  [  $<proc-interface> ]?  ")"  [  [  "," $<proc-attr-spec> ]* "::" ]?  $<proc-decl-list>
    }

    #`[R1513] token proc-interface {
        |  $<interface-name>
        |  $<declaration-type-spec>
    }

    #`[R1514] token proc-attr-spec {
        |  $<access-spec>
        |  $<proc-language-binding-spec>
        |  "INTENT"  "("  $<intent-spec>  ")"
        |  "OPTIONAL"
        |  "POINTER"
        |  "PROTECTED"
        |  "SAVE"
    }

    #`[R1515] token proc-decl {
        |  $<procedure-entity-name>  [  "=>" $<proc-pointer-init> ]?
    }

    #`[R1516] token interface-name {
        |  $<name>
    }

    #`[R1517] token proc-pointer-init {
        |  $<null-init>
        |  $<initial-proc-target>
    }

    #`[R1518] token initial-proc-target {
        |  $<procedure-name>
    }

    #`[C1515: (R1516) The name shall be the name of an abstract interface | of a procedure that has an explicit interface. If name is declared by a procedure-declaration-stmt it shall be previously declared. If name denotes an intrinsic procedure it shall be one that is listed in Table 16.2.]

    #`[C1516: (R1516) The name shall not be the same as a keyword that specifies an intrinsic type.]

    #`[C1517: (R1512) If a proc-interface describes an elemental procedure, each procedure-entity-name shall specify an external procedure.]

    #`[C1518: (R1515) If => appears in proc-decl, the procedure entity shall have the POINTER attribute.]

    #`[C1519: (R1518) The procedure-name shall be the name of a nonelemental external | module procedure, | a specific intrinsic function listed in Table 16.2.]

    #`[C1520: (R1512) If proc-language-binding-spec with NAME= is specified, then proc-decl-list shall contain exactly one proc-decl, which shall neither have the POINTER attribute nor be a dummy procedure.]

    #`[C1521: (R1512)If proc-language-binding-spec is specified, the proc-interface shall appear, it shall be an interface- name, and interface-name shall be declared with a proc-language-binding-spec.]

    #`[R1519] token intrinsic-stmt {
        |  "INTRINSIC"  [  "::" ]?  $<intrinsic-procedure-name-list>
    }

    #`[C1522: (R1519) Each intrinsic-procedure-name shall be the name of an intrinsic procedure.]

    #`[R1520] token function-reference {
        |  $<procedure-designator>  "("  [  $<actual-arg-spec-list> ]?  ")"
    }

    #`[C1523: (R1520) The procedure-designator shall designate a function.]

    #`[C1524: (R1520) The actual-arg-spec-list shall not contain an alt-return-spec.]

    #`[R1521] token call-stmt {
        |  "CALL"  $<procedure-designator>  [  "(" [  $<actual-arg-spec-list> ]? ")" ]?
    }

    #`[C1525: (R1521) The procedure-designator shall designate a subroutine.]

    #`[R1522] token procedure-designator {
        |  $<procedure-name>
        |  $<proc-component-ref>
        |  $<data-ref>  "%"  $<binding-name>
    }

    #`[C1526: (R1522) A procedure-name shall be a generic name | the name of a procedure.]

    #`[C1527: (R1522) A binding-name shall be a binding name (7.5.5) of the declared type of data-ref.]

    #`[C1528: (R1522) A data-ref shall not be a polymorphic subobject of a coindexed object.]

    #`[C1529: (R1522) If data-ref is an array, the referenced type-bound procedure shall have the PASS attribute.]

    #`[R1523] token actual-arg-spec {
        |  [  $<keyword> "=" ]?  $<actual-arg>
    }

    #`[R1524] token actual-arg {
        |  $<expr>
        |  $<variable>
        |  $<procedure-name>
        |  $<proc-component-ref>
        |  $<alt-return-spec>
    }

    #`[R1525] token alt-return-spec {
        |  "*"  $<label>
    }

    #`[C1530: (R1523) The keyword = shall not appear if the interface of the procedure is implicit.]

    #`[C1531: (R1523) The keyword = shall not be omitted from an actual-arg-spec unless it has been omitted from each preceding actual-arg-spec in the argument list.]

    #`[C1532: (R1523) Each keyword shall be the name of a dummy argument in the explicit interface of the procedure.]

    #`[C1533: (R1524) A nonintrinsic elemental procedure shall not be used as an actual argument.]

    #`[C1534: (R1524) A procedure-name shall be the name of an external, internal, module, | dummy procedure, a specific intrinsic function listed in Table 16.2, | a procedure pointer.]

    #`[C1535: (R1524) expr shall not be a variable.]

    #`[C1536: (R1525) The label shall be the statement label of a branch target statement that appears in the same inclusive scope as the call-stmt.]

    #`[C1537: An actual argument that is a coindexed object shall not have a pointer ultimate component.]

    #`[C1538: AnactualargumentthatisacoindexedobjectwiththeASYNCHRONOUSorVOLATILEattributeshall not correspond to a dummy argument that has either the ASYNCHRONOUS | VOLATILE attribute, unless the dummy argument has the VALUE attribute.]

    #`[C1539: (R1524) If an actual argument is a nonpointer array that has the ASYNCHRONOUS | VOLATILE attribute but is not simply contiguous (9.5.4), and the corresponding dummy argument has either the ASYNCHRONOUS | VOLATILE attribute, but does not have the VALUE attribute, that dummy argument shall be assumed-shape | assumed-rank and shall not have the CONTIGUOUS attribute.]

    #`[C1540: (R1524) If an actual argument is an array pointer that has the ASYNCHRONOUS | VOLATILE attribute but does not have the CONTIGUOUS attribute, and the corresponding dummy argument has either the ASYNCHRONOUS | VOLATILE attribute, but does not have the VALUE attribute, that dummy argument shall be an array pointer, an assumed-shape array without the CONTIGUOUS attribute, | an assumed-rank entity without the CONTIGUOUS attribute.]

    #`[C1541: The actual argument corresponding to a dummy pointer with the CONTIGUOUS attribute shall be simply contiguous (9.5.4).]

    #`[C1542: The actual argument corresponding to a dummy pointer shall not be a coindexed object.]

    #`[R1526] token prefix {
        |  $<prefix-spec>  [  $<prefix-spec> ]*
    }

    #`[R1527] token prefix-spec {
        |  $<declaration-type-spec>
        |  "ELEMENTAL"
        |  "IMPURE"
        |  "MODULE"
        |  "NON_RECURSIVE"
        |  "PURE"
        |  "RECURSIVE"
    }

    #`[C1543: (R1526) A prefix shall contain at most one of each prefix-spec.]

    #`[C1544: (R1526) A prefix shall not specify both PURE and IMPURE.]

    #`[C1545: (R1526) A prefix shall not specify both NON_RECURSIVE and RECURSIVE.]

    #`[C1546: An elemental procedure shall not have the BIND attribute.]

    #`[C1547: (R1526) MODULE shall appear only in the function-stmt | subroutine-stmt of a module subprogram | of a nonabstract interface body that is declared in the scoping unit of a module | submodule.]

    #`[C1548: (R1526) If MODULE appears in the prefix of a module subprogram, it shall have been declared to be a separate module procedure in the containing program unit | an ancestor of that program unit.]

    #`[C1549: (R1526) If MODULE appears in the prefix of a module subprogram, the subprogram shall specify the same characteristics and dummy argument names as its corresponding module procedure interface body.]

    #`[C1550: (R1526) If MODULE appears in the prefix of a module subprogram and a binding label is specified, it shall be the same as the binding label specified in the corresponding module procedure interface body.]

    #`[C1551: (R1526) If MODULE appears in the prefix of a module subprogram, NON_RECURSIVE shall appear if and only if NON_RECURSIVE appears in the prefix in the corresponding module procedure interface body.]

    #`[R1528] token proc-language-binding-spec {
        |  $<language-binding-spec>
    }

    #`[C1552: A proc-language-binding-spec with a NAME= specifier shall not be specified in the function-stmt | subroutine-stmt of an internal procedure, | of an interface body for an abstract interface | a dummy procedure.]

    #`[C1553: Ifproc-language-binding-spec is specified for a function, the function result shall be an interoperable scalar variable.]

    #`[C1554: If proc-language-binding-spec is specified for a procedure, each of its dummy arguments shall be an inter- operable procedure (18.3.7) | a variable that is interoperable (18.3.5, 18.3.6), assumed-shape, assumed- rank, assumed-type, of type CHARACTER with assumed length, | that has the ALLOCATABLE | POINTERattribute.]

    #`[C1555: A variable that is a dummy argument of a procedure that has a proc-language-binding-spec shall be assumed-type | of interoperable type and kind type parameters.]

    #`[C1556: If proc-language-binding-spec is specified for a procedure, it shall not have a default-initialized dummy argument with the ALLOCATABLE | POINTER attribute.]

    #`[C1557: If proc-language-binding-spec is specified for a procedure, it shall not have a dummy argument that is a coarray.]

    #`[C1558: If proc-language-binding-spec is specified for a procedure, it shall not have an array dummy argument with the VALUE attribute.]

    #`[R1530] token function-stmt {
        |  [  $<prefix> ]?  "FUNCTION"  $<function-name>  "("  [  $<dummy-arg-name-list> ]?  ")"  [  $<suffix> ]?
    }

    #`[C1559: (R1530) If RESULT appears, result-name shall not be the same as function-name and shall not be the same as the entry-name in any ENTRY statement in the subprogram.]

    #`[C1560: (R1530) If RESULT appears, the function-name shall not appear in any specification statement in the scoping unit of the function subprogram.]

    #`[R1531] token dummy-arg-name {
        |  $<name>
    }

    #`[C1561: (R1531) A dummy-arg-name shall be the name of a dummy argument.]

    #`[R1532] token suffix {
        |  $<proc-language-binding-spec>  [  "RESULT" "(" $<result-name> ")" ]?
        |  "RESULT"  "("  $<result-name>  ")"  [  $<proc-language-binding-spec> ]?
    }

    #`[R1533] token end-function-stmt {
        |  "END"  [  "FUNCTION" [  $<function-name> ]? ]?
    }

    #`[C1562: (R1529) An internal function subprogram shall not contain an internal-subprogram-part.]

    #`[C1563: (R1533) If a function-name appears in the end-function-stmt, it shall be identical to the function-name specified in the function-stmt.]

    #`[R1535] token subroutine-stmt {
        |  [  $<prefix> ]?  "SUBROUTINE"  $<subroutine-name>  [  "(" [  $<dummy-arg-list> ]? ")" [  $<proc-language-binding-spec> ]? ]?
    }

    #`[C1564: (R1535) The prefix of a subroutine-stmt shall not contain a declaration-type-spec.]

    #`[R1536] token dummy-arg {
        |  $<dummy-arg-name>
        |  "*"
    }

    #`[R1537] token end-subroutine-stmt {
        |  "END"  [  "SUBROUTINE" [  $<subroutine-name> ]? ]?
    }

    #`[C1565: (R1534) An internal subroutine subprogram shall not contain an internal-subprogram-part.]

    #`[C1566: (R1537) If a subroutine-name appears in the end-subroutine-stmt, it shall be identical to the subroutine- name specified in the subroutine-stmt.]

    #`[R1539] token mp-subprogram-stmt {
        |  "MODULE"  "PROCEDURE"  $<procedure-name>
    }

    #`[R1540] token end-mp-subprogram-stmt {
        |  "END"  [  "PROCEDURE" [  $<procedure-name> ]? ]?
    }

    #`[C1567: (R1538)Theprocedure-name shallhavebeendeclaredtobeaseparatemoduleprocedureinthecontaining program unit | an ancestor of that program unit.]

    #`[C1568: (R1540)Ifaprocedure-name appearsintheend-mp-subprogram-stmt,itshallbeidenticaltotheprocedure- name in the mp-subprogram-stmt.]

    #`[R1541] token entry-stmt {
        |  "ENTRY"  $<entry-name>  [  "(" [  $<dummy-arg-list> ]? ")" [  $<suffix> ]? ]?
    }

    #`[C1569: (R1541) If RESULT appears, the entry-name shall not appear in any specification | type declaration statement in the scoping unit of the function program.]

    #`[C1570: (R1541) An entry-stmt shall appear only in an external-subprogram | a module-subprogram that does not define a separate module procedure. An entry-stmt shall not appear within an executable-construct.]

    #`[C1571: (R1541) RESULT shall appear only if the entry-stmt is in a function subprogram.]

    #`[C1572: (R1541) A dummy-arg shall not be an alternate return indicator if the ENTRY statement is in a function subprogram.]

    #`[C1573: (R1541) If RESULT appears, result-name shall not be the same as the function-name in the FUNCTION statement and shall not be the same as the entry-name in any ENTRY statement in the subprogram.]

    #`[R1542] token return-stmt {
        |  "RETURN"  [  $<scalar-int-expr> ]?
    }

    #`[C1574: (R1542) The return-stmt shall be in the inclusive scope of a function | subroutine subprogram.]

    #`[C1575: (R1542) The scalar-int-expr is allowed only in the inclusive scope of a subroutine subprogram.]

    #`[R1543] token contains-stmt {
        |  "CONTAINS"
    }

    #`[R1544] token stmt-function-stmt {
        |  $<function-name>  "("  [  $<dummy-arg-name-list> ]?  ")"  "="  $<scalar-expr>
    }

    #`[C1576: (R1544) Each primary in scalar-expr shall be a constant (literal | named), a reference to a variable, a reference to a function, | an expression in parentheses. Each operation shall be intrinsic. If scalar-expr contains a reference to a function, the reference shall not require an explicit interface, the function shall not require an explicit interface unless it is an intrinsic function, the function shall not be a transformational intrinsic, and the result shall be scalar. If an argument to a function is an array, it shall be an array name. If a reference to a statement function appears in scalar-expr, its definition shall have been provided earlier in the scoping unit and shall not be the name of the statement function being defined.]

    #`[C1577: (R1544) Named constants in scalar-expr shall have been declared earlier in the scoping unit | made accessible by use | host association. If array elements appear in scalar-expr, the array shall have been declared as an array earlier in the scoping unit | made accessible by use | host association.]

    #`[C1578: (R1544) If a dummy-arg-name, variable, function reference, | dummy function reference is typed by the implicit typing rules, its appearance in any subsequent type declaration statement shall confirm this implied type and the values of any implied type parameters.]

    #`[C1579: (R1544) The function-name and each dummy-arg-name shall be specified, explicitly | implicitly, to be scalar.]

    #`[C1580: (R1544) A given dummy-arg-name shall not appear more than once in a given dummy-arg-name-list.]

    #`[C1581: Astatement function shall not be of a parameterized derived type.]

    #`[C1582: The specification-part of a pure function subprogram shall specify that all its nonpointer dummy data objects have the INTENT (IN) | the VALUE attribute.]

    #`[C1583: The function result of a pure function shall not be such that finalization of a reference to the function would reference an impure procedure.]

    #`[C1584: The function result of a pure function shall not be both polymorphic and allocatable, | have a poly- morphic allocatable ultimate component.]

    #`[C1585: The specification-part of a pure subroutine subprogram shall specify the intents of all its nonpointer dummy data objects that do not have the VALUE attribute.]

    #`[C1586: An INTENT (OUT) dummy argument of a pure procedure shall not be such that finalization of the actual argument would reference an impure procedure.]

    #`[C1587: An INTENT (OUT) dummy argument of a pure procedure shall not be polymorphic | have a poly- morphic allocatable ultimate component.]

    #`[C1588: A local variable of a pure subprogram, | of a BLOCK construct within a pure subprogram, shall not have the SAVE | VOLATILE attribute.]

    #`[C1589: The specification-part of a pure subprogram shall specify that all its dummy procedures are pure.]

    #`[C1590: Ifaprocedurethatisneitheranintrinsic procedure norastatementfunction is used in a context that requires it to be pure, then its interface shall be explicit in the scope of that use. The interface shall specify that the procedure is pure.]

    #`[C1591: All internal subprograms in a pure subprogram shall be pure.]

    #`[C1592: A designator of a variable with the VOLATILE attribute shall not appear in a pure subprogram.]

    #`[C1593: In a pure subprogram any designator with a base object that is in common | accessed by host | use association, is a pointer dummy argument of a pure function, is a dummy argument with the INTENT (IN) attribute, is a coindexed object, | an object that is storage associated with any such variable, shall not be used (1) in a variable definition context (19.6.7), (2) in a pointer association context (19.6.8), (3) as the data-target in a pointer-assignment-stmt, (4) as the expr corresponding to a component in a structure-constructor if the component has the POINTERattribute | has a pointer component at any level of component selection, (5) as the expr of an intrinsic assignment statement in which the variable is of a derived type if the derived type has a pointer component at any level of component selection, (6) as the source-expr in a SOURCE= specifier if the designator is of a derived type that has a pointer component at any level of component selection, (7) as an actual argument corresponding to a dummy argument with the POINTER attribute, | (8) as the actual argument to the function C_LOC from the intrinsic module ISO_C_BINDING.]

    #`[C1594: Anyprocedurereferenced in a pure subprogram, including one referenced via a defined operation, defined assignment, defined input/output, | finalization, shall be pure.]

    #`[C1595: A statement that might result in the deallocation of a polymorphic entity is not permitted in a pure procedure.]

    #`[C1596: A pure subprogram shall not contain a print-stmt, open-stmt, close-stmt, backspace-stmt, endfile-stmt, rewind-stmt, flush-stmt, wait-stmt, | inquire-stmt.]

    #`[C1597: A pure subprogram shall not contain a read-stmt | write-stmt whose io-unit is a file-unit-number | *.]

    #`[C1598: A pure subprogram shall not contain an image control statement (11.6.1).]

    #`[C1599: All dummy arguments of an elemental procedure shall be scalar noncoarray dummy data objects and shall not have the POINTER | ALLOCATABLE attribute.]

    #`[C15100: Theresultofanelementalfunctionshallbescalar, andshallnothavethePOINTERorALLOCATABLE attribute.]

    #`[C15101: Thespecification-part of an elemental subprogram shall specify the intents of all of its dummy arguments that do not have the VALUE attribute.]

    #`[C15102: In the specification-expr that specifies a type parameter value of the result of an elemental function, an object designator with a dummy argument of the function as the base object shall appear only as the subject of a specification inquiry (10.1.11), and that specification inquiry shall not depend on a property that is deferred.]

    #`[C1601: If a boz-literal-constant is truncated as an argument to the intrinsic function REAL, the discarded bits shall all be zero.]

    #`[C1602: A reference to a collective subroutine shall not appear in a context where an image control statement is not permitted to appear.]

    #`[C1603: A named entity with declared type EVENT_TYPE, | which has a noncoarray potential subobject component with declared type EVENT_TYPE, shall be a variable. A component that is of such a type shall be a data component.]

    #`[C1604: A named variable with declared type EVENT_TYPE shall be a coarray. A named variable with a noncoarray potential subobject component of type EVENT_TYPE shall be a coarray.]

    #`[C1605: An event variable shall not appear in a variable definition context except as the event-variable in an EVENTPOSTorEVENTWAITstatement,asanallocate-object,orasanactualargumentinareference to a procedure with an explicit interface if the corresponding dummy argument has INTENT (INOUT).]

    #`[C1606: A variable with a nonpointer subobject of type EVENT_TYPE shall not appear in a variable definition context except as an allocate-object in an ALLOCATE statement without a SOURCE= specifier, as an allocate-object in a DEALLOCATE statement, | as an actual argument in a reference to a procedure with an explicit interface if the corresponding dummy argument has INTENT (INOUT).]

    #`[C1607: A named entity with declared type LOCK_TYPE, | which has a noncoarray potential subobject com- ponent with declared type LOCK_TYPE, shall be a variable. A component that is of such a type shall be a data component.]

    #`[C1608: A named variable with declared type LOCK_TYPE shall be a coarray. A named variable with a noncoarray potential subobject component of type LOCK_TYPE shall be a coarray.]

    #`[C1609: Alock variable shall not appear in a variable definition context except as the lock-variable in a LOCK | UNLOCKstatement, as an allocate-object, | as an actual argument in a reference to a procedure with an explicit interface where the corresponding dummy argument has INTENT (INOUT).]

    #`[C1610: AvariablewithasubobjectoftypeLOCK_TYPEshallnotappearinavariabledefinitioncontextexcept as an allocate-object | as an actual argument in a reference to a procedure with an explicit interface where the corresponding dummy argument has INTENT (INOUT).]

    #`[C1801: (R726) A derived type with the BIND attribute shall not have the SEQUENCE attribute.]

    #`[C1802: (R726) A derived type with the BIND attribute shall not have type parameters.]

    #`[C1803: (R726) A derived type with the BIND attribute shall not have the EXTENDS attribute.]

    #`[C1804: (R726) A derived-type-def that defines a derived type with the BIND attribute shall not have a type- bound-procedure-part.]

    #`[C1805: (R726) A derived type with the BIND attribute shall have at least one component.]

    #`[C1806: (R726) Each component of a derived type with the BIND attribute shall be a nonpointer, nonallocatable data component with interoperable type and type parameters.]

    #`[C1807: A procedure defined in a submodule shall not have a binding label unless its interface is declared in the ancestor module.]

}

grammar F2008 {
#`(R201)    rule TOP { <program-unit>+ }

#`(R202)    rule program-unit               { <main-program>
                                            | <external-subprogram>
                                            | <module>
                                            | <block-data> }

#`(R1101)   rule main-program               { <program-stmt>?
                                              <specification-part>?
                                              <execution-part>?
                                              <internal-subprogram-part>?
                                              <end-program-stmt> }

#`(R203)    rule external-subprogram        { <function-subprogram>
                                            | <subroutine-subprogram> }

#`(R1227)   rule function-subprogram        { <function-stmt>
                                              <specification-part>?
                                              <execution-part>?
                                              <internal-subprogram-part>?
                                              <end-function-stmt> }

#`(R1233)   rule subroutine-subprogram      { <subroutine-stmt>
                                              <specification-part>?
                                              <execution-part>?
                                              <internal-subprogram-part>?
                                              <end-subroutine-stmt> }

#`(R1104)   rule module                     { <module-stmt>
                                              <specification-part>?
                                              <module-subprogram-part>?
                                              <end-module-stmt> }

#`(R1116)   rule submodule                  { <submodule-stmt>
                                              <specification-part>?
                                              <module-subprogram-part>?
                                              <end-module-stmt> }

#`(R1120)   rule block-data                 { <block-data-stmt> 
                                              <specification-part>?
                                              <end-block-data-stmt> }

#`(R204)    rule specification-part         { <use-stmt>*
                                              <import-stmt>*
                                              <implicit-part>?
                                              <declaration-construct>* }

#`(R205)    rule implicit-part              { <implicit-part-stmt>*
                                              <implicit-stmt> }

#`(R206)    rule implicit-part-stmt         { <implicit-stmt>
                                            | <parameter-stmt>
                                            | <format-stmt>
                                            | <entry-stmt> }

#`(R207)    rule declaration-construct      { <derived-type-def>
                                            | <entry-stmt>
                                            | <enum-def>
                                            | <format-stmt>
                                            | <interface-block>
                                            | <parameter-stmt>
                                            | <procedure-declaration-stmt>
                                            | <other-specification-stmt>
                                            | <type-declaration-stmt>
                                            | <stmt-function-stmt> }

#`(R208)    rule execution-part             { <executable-construct>
                                              <execution-part-construct>* }

#`(R209)    rule execution-part-construct   { <executable-construct>
                                            | <format-stmt>
                                            | <entry-stmt>
                                            | <data-stmt> }

#`(R210)    rule internal-subprogram-part   { <contains-stmt>
                                              <internal-subprogram>* }

#`(R211)    rule internal-subprogram        { <function-subprogram>
                                            | <subroutine-subprogram> }

#`(R1107)   rule module-subprogram-part     { <contains-stmt>
                                              <module-subprogram>* }

#`(R1108)   rule module-subprogram          { <function-subprogram>
                                            | <subroutine-subprogram>
                                            | <separate-module-subprogram> }

#`(R1237)   rule separate-module-subprogram { <mp-subprogram-stmt>
                                              <specification-part>?
                                              <execution-part>?
                                              <internal-subprogram-part>?
                                              <end-mp-subprogram-stmt> }

#`(R212)    rule other-specification-stmt   { <access-stmt>
                                            | <allocatable-stmt>
                                            | <asynchronous-stmt>
                                            | <bind-stmt>
                                            | <codimension-stmt>
                                            | <common-stmt>
                                            | <data-stmt>
                                            | <dimension-stmt>
                                            | <equivalence-stmt>
                                            | <external-stmt>
                                            | <intent-stmt>
                                            | <intrinsic-stmt>
                                            | <namelist-stmt>
                                            | <optional-stmt>
                                            | <pointer-stmt>
                                            | <protected-stmt>
                                            | <save-stmt>
                                            | <target-stmt>
                                            | <volatile-stmt>
                                            | <value-stmt> }

#`(R213)    rule executable-construct       { <action-stmt>
                                            | <associate-construct>
                                            | <block-construct>
                                            | <case-construct>
                                            | <critical-construct>
                                            | <do-construct>
                                            | <forall-construct>
                                            | <if-construct>
                                            | <select-type-construct>
                                            | <where-construct> }

#`(R214)    rule action-stmt                { <allocate-stmt>
                                            | <assignment-stmt>
                                            | <backspace-stmt>
                                            | <call-stmt>
                                            | <close-stmt>
                                            | <continue-stmt>
                                            | <cycle-stmt>
                                            | <deallocate-stmt>
                                            | <end-function-stmt>
                                            | <end-mp-subprogram-stmt>
                                            | <end-program-stmt>
                                            | <end-subroutine-stmt>
                                            | <endfile-stmt>
                                            | <error-stop-stmt>
                                            | <exit-stmt>
                                            | <flush-stmt>
                                            | <forall-stmt>
                                            | <goto-stmt>
                                            | <if-stmt>
                                            | <enquire-stmt>
                                            | <lock-stmt>
                                            | <nullify-stmt>
                                            | <open-stmt>
                                            | <pointer-assignment-stmt>
                                            | <print-stmt>
                                            | <read-stmt>
                                            | <return-stmt>
                                            | <rewind-stmt>
                                            | <stop-stmt>
                                            | <sync-all-stmt>
                                            | <sync-images-stmt>
                                            | <sync-memory-stmt>
                                            | <unlock-stmt>
                                            | <wait-stmt>
                                            | <where-stmt>
                                            | <write-stmt>
                                            | <arithmetic-if-stmt>
                                            | <computed-goto-stmt> }
}

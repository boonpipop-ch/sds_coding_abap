*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE MC_BDC_DYNPRO.
  APPEND VALUE bdcdata( program = &1
                        dynpro  = &2
                        dynbegin = 'X' )
         TO lt_bdcdata.
END-OF-DEFINITION.

DEFINE MC_BDC_FIELD.
  APPEND VALUE bdcdata( fnam = &1
                        fval = &2 )
         TO lt_bdcdata.
END-OF-DEFINITION.

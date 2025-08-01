*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE MACRO_ASSIGN_RESP.
  IF &1 IS ASSIGNED.
    ASSIGN COMPONENT &2 OF STRUCTURE &1 TO <l_field>.
    IF sy-subrc EQ 0.
      EF_ASSIGNED = 'X'.
      <l_field> = &3.
    ENDIF.
  ENDIF.
END-OF-DEFINITION.

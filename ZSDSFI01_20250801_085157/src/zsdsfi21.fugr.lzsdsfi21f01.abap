*----------------------------------------------------------------------*
***INCLUDE LAALSMEXF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SEPARATED_TO_INTERN_CONVERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEPARATED_TO_INTERN_CONVERT TABLES I_TAB       TYPE TY_T_SENDER
                                        I_INTERN    TYPE ZSDSFIS190_TT
                                 USING  I_SEPARATOR TYPE C.
  DATA: L_SIC_TABIX LIKE SY-TABIX,
        L_SIC_COL   TYPE KCD_EX_COL.
  DATA: L_FDPOS     LIKE SY-FDPOS.

  REFRESH I_INTERN.

  LOOP AT I_TAB.
    L_SIC_TABIX = SY-TABIX.
    L_SIC_COL = 0.
    WHILE I_TAB CA I_SEPARATOR.
      L_FDPOS = SY-FDPOS.
      L_SIC_COL = L_SIC_COL + 1.
      PERFORM LINE_TO_CELL_SEPARAT TABLES I_INTERN
                                   USING  I_TAB L_SIC_TABIX L_SIC_COL
                                          I_SEPARATOR L_FDPOS.
    ENDWHILE.
    IF I_TAB <> SPACE.
      CLEAR I_INTERN.
      I_INTERN-ROW = L_SIC_TABIX.
      I_INTERN-COL = L_SIC_COL + 1.
      I_INTERN-VALUE = I_TAB.
      APPEND I_INTERN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SEPARATED_TO_INTERN_CONVERT
*---------------------------------------------------------------------*
FORM LINE_TO_CELL_SEPARAT TABLES I_INTERN    TYPE ZSDSFIS190_TT
                          USING  I_LINE
                                 I_ROW       LIKE SY-TABIX
                                 CH_CELL_COL TYPE KCD_EX_COL
                                 I_SEPARATOR TYPE C
                                 I_FDPOS     LIKE SY-FDPOS.
  DATA: L_STRING   TYPE TY_S_SENDERLINE.
  DATA  L_SIC_INT  TYPE I.

  CLEAR I_INTERN.
  L_SIC_INT = I_FDPOS.
  I_INTERN-ROW = I_ROW.
  L_STRING = I_LINE.
  I_INTERN-COL = CH_CELL_COL.
* csv Dateien mit separator in Zelle: --> ;"abc;cd";
  IF ( I_SEPARATOR = ';' OR  I_SEPARATOR = ',' ) AND
       L_STRING(1) = GC_ESC.
    PERFORM LINE_TO_CELL_ESC_SEP USING L_STRING
                                       L_SIC_INT
                                       I_SEPARATOR
                                       I_INTERN-VALUE.
  ELSE.
    IF L_SIC_INT > 0.
      I_INTERN-VALUE = I_LINE(L_SIC_INT).
    ENDIF.
  ENDIF.
  IF L_SIC_INT > 0.
    APPEND I_INTERN.
  ENDIF.
  L_SIC_INT = L_SIC_INT + 1.
  I_LINE = I_LINE+L_SIC_INT.
ENDFORM.

*---------------------------------------------------------------------*
FORM LINE_TO_CELL_ESC_SEP USING I_STRING
                                I_SIC_INT      TYPE I
                                I_SEPARATOR    TYPE C
                                I_INTERN_VALUE TYPE ZSDSFIS190-VALUE.
  DATA: L_INT         TYPE I,
        L_CELL_END(2).
  FIELD-SYMBOLS: <L_CELL>.
  L_CELL_END = GC_ESC.
  L_CELL_END+1 = I_SEPARATOR .

  IF I_STRING CS GC_ESC.
    I_STRING = I_STRING+1.
    IF I_STRING CS L_CELL_END.
      L_INT = SY-FDPOS.
      ASSIGN I_STRING(L_INT) TO <L_CELL>.
      I_INTERN_VALUE = <L_CELL>.
      L_INT = L_INT + 2.
      I_SIC_INT = L_INT.
      I_STRING = I_STRING+L_INT.
    ELSEIF I_STRING CS GC_ESC.
*     letzte Celle
      L_INT = SY-FDPOS.
      ASSIGN I_STRING(L_INT) TO <L_CELL>.
      I_INTERN_VALUE = <L_CELL>.
      L_INT = L_INT + 1.
      I_SIC_INT = L_INT.
      I_STRING = I_STRING+L_INT.
      L_INT = STRLEN( I_STRING ).
      IF L_INT > 0 . MESSAGE X001(KX) . ENDIF.
    ELSE.
      MESSAGE X001(KX) . "was ist mit csv-Format
    ENDIF.
  ENDIF.

ENDFORM.

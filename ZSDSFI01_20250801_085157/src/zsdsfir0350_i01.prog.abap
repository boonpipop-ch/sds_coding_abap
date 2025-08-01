*&---------------------------------------------------------------------*
*& Include ZSDSFIR0350_I01
*&---------------------------------------------------------------------*
MODULE TABSTRIP_CTRL_ACTIVE_TAB_GET INPUT.
  GF_OKCODE = SY-UCOMM.
  CASE GF_OKCODE.
    WHEN C_TABSTRIP_CTRL-TAB1.
      G_TABSTRIP_CTRL-PRESSED_TAB = C_TABSTRIP_CTRL-TAB1.
    WHEN C_TABSTRIP_CTRL-TAB2.
      G_TABSTRIP_CTRL-PRESSED_TAB = C_TABSTRIP_CTRL-TAB2.
    WHEN C_TABSTRIP_CTRL-TAB3.
      G_TABSTRIP_CTRL-PRESSED_TAB = C_TABSTRIP_CTRL-TAB3.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
MODULE TABLE_CTRL_MAIN_MODIFY INPUT.

  MOVE-CORRESPONDING ZSDSFIS075 TO GS_MAINITEM.
  MODIFY GT_MAINITEM
    FROM GS_MAINITEM
    INDEX TABLE_CTRL_MAIN-CURRENT_LINE.

ENDMODULE.
MODULE TABLE_CTRL_MAIN_USER_COMMAND INPUT.

  GF_OKCODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TABLE_CTRL_MAIN'
                              'GT_MAINITEM'
                              'FLAG'
                     CHANGING GF_OKCODE.
  SY-UCOMM = GF_OKCODE.
ENDMODULE.
MODULE TABLE_CTRL_ACCA_MODIFY INPUT.

  MOVE-CORRESPONDING ZSDSFIS076 TO GS_ACCA.
  MODIFY GT_ACCA
    FROM GS_ACCA
    INDEX TABLE_CTRL_ACCA-CURRENT_LINE.
  IF SY-SUBRC EQ 4.
    APPEND INITIAL LINE TO GT_ACCA ASSIGNING FIELD-SYMBOL(<L_ACCA>) ##NEEDED.
    MOVE-CORRESPONDING GS_ACCA TO <L_ACCA>.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

  PERFORM F_USER_COMMAND_9000.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PERIOD_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PERIOD_CHECK INPUT.

  PERFORM F_PERIOD_CHECK.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_TXT20  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_TXT20 INPUT.

  PERFORM F_GET_ACC_DESC USING ZSDSFIS075-HKONT_DR
                          CHANGING ZSDSFIS075-TXT20_DR.

  IF ZSDSFIS075-HKONT_DR IS NOT INITIAL AND ZSDSFIS075-TXT20_DR IS INITIAL.
    MESSAGE TEXT-E03 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT INPUT.

  IF ZSDSFIS076-ALLKT IS INITIAL AND
    NOT ( ZSDSFIS076-KOSTL IS INITIAL AND ZSDSFIS076-AUFNR IS INITIAL AND
          ZSDSFIS076-PS_POSID IS INITIAL AND ZSDSFIS076-PRCTR IS INITIAL ).
    MESSAGE 'Allocation is required'(016) TYPE 'E'.
  ELSEIF ( ZSDSFIS076-KOSTL IS INITIAL AND ZSDSFIS076-AUFNR IS INITIAL AND
          ZSDSFIS076-PS_POSID IS INITIAL AND ZSDSFIS076-PRCTR IS INITIAL ).
    CLEAR ZSDSFIS076-ALLKT.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_KTEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_KTEXT INPUT.

  PERFORM F_GET_CC_DESC USING ZSDSFIS076-KOSTL
                     CHANGING ZSDSFIS076-LTEXT.

  IF ZSDSFIS076-KOSTL IS NOT INITIAL AND ZSDSFIS076-LTEXT IS INITIAL.
    MESSAGE TEXT-E03 TYPE 'E'.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  9003_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
##WARN_OK
MODULE 9003_PAI INPUT.

  GR_POSTITEM_TABLE->REFRESH(
*  S_STABLE     =
*  REFRESH_MODE = IF_SALV_C_REFRESH=>SOFT
  ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CHECK_CHAIN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_CHAIN INPUT.
  ##NEEDED ##DECL_MODUL
  DATA: LT_MESSAGES TYPE STANDARD TABLE OF BAPIRETURN1,
        ##NEEDED ##DECL_MODUL
        LS_COBL     TYPE COBL.

  CLEAR LS_COBL.
  FREE LT_MESSAGES.

  LS_COBL-BUKRS    = ZSDSFIS074-BUKRS.
  LS_COBL-GLVOR    = 'RFBU'.
  LS_COBL-VORGN    = 'RFBU'.
  LS_COBL-HKONT    = ZSDSFIS075-HKONT_DR.
  LS_COBL-BUDAT    = SY-DATUM.
  LS_COBL-BLDAT    = SY-DATUM.
  LS_COBL-KOSTL    = ZSDSFIS076-KOSTL.
  LS_COBL-AUFNR    = ZSDSFIS076-AUFNR.
  LS_COBL-PS_POSID = ZSDSFIS076-PS_POSID.
  LS_COBL-PRCTR    = ZSDSFIS076-PRCTR.
  LS_COBL-PROCESS  = 'BELEGPOS'.
  LS_COBL-EVENT    = 'PRUEFEN'.

  CALL FUNCTION 'COBL_CODINGBLOCK_CHECK'
    EXPORTING
      CHECK_COBL              = LS_COBL
*     PBO_COBL                =
      CUST_FIELDS_DYNP_CHECKS = 'X'
      COLLECT_MESSAGES        = 'X'
*     EXTERNAL_FIELDS_USED    = ''
* IMPORTING
*     CHECKED_COBL            =
    TABLES
      T_MESSAGES              = LT_MESSAGES.
  ##NEEDED
  READ TABLE LT_MESSAGES INTO DATA(LS_MESSAGES) WITH KEY TYPE = 'E'.
  IF SY-SUBRC EQ 0.
    MESSAGE LS_MESSAGES-MESSAGE TYPE LS_MESSAGES-TYPE.
  ELSE.
    READ TABLE LT_MESSAGES INTO  LS_MESSAGES INDEX 1.
    IF SY-SUBRC EQ 0.
      MESSAGE LS_MESSAGES-MESSAGE TYPE LS_MESSAGES-TYPE.
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000_EXIT INPUT.
  CASE SY-UCOMM.
    WHEN '&F15'.
      LEAVE TO SCREEN 0.
    WHEN '&F12'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9002 INPUT.
  CASE SY-UCOMM.
    WHEN 'BT_COPY'.
      LCL_DATA=>COPY_DATA( ).
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0120_COMMAND
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN '&BACK' OR '&LEAVE' OR '&EXIT'.
      LEAVE TO SCREEN 0.

    WHEN '&SIM'.
      PERFORM F_SIMULATE.
    WHEN '&REF'.
      I_DETAIL[] = I_DETAIL_OLD[].
      CLEAR : TXT_AMT_INC_VAT,
              TXT_AMT_INC_WHT,
              TXT_AMT_INC_VAT_O,
              TXT_AMT_INC_WHT_O,
              TXT_TOTAL_AMT1,
              TXT_TOTAL_AMT1_O,
              TXT_TOTAL_AMT2,
              TXT_TOTAL_AMT2_O,
              TXT_TOTAL_QTY,
              TXT_TOTAL_QTY_O,
              TXT_GRANDTOTAL,
              TXT_GRANDTOTAL_O,
              TXT_ADV,
              TXT_ADV_O.

      TXT_PRCNT = 100.
      CHK_VAT = 'X'.
    WHEN '&CLR'.
      PERFORM F_CLEAR_SCREEN.

    WHEN '&SALL'.
      W_DETAIL-SEL = 'X'.
      MODIFY I_DETAIL FROM W_DETAIL TRANSPORTING SEL WHERE MATNR IS NOT INITIAL.
      CLEAR W_DETAIL.
    WHEN '&DALL'.
      W_DETAIL-SEL = SPACE.
      MODIFY I_DETAIL FROM W_DETAIL TRANSPORTING SEL WHERE MATNR IS NOT INITIAL.
      CLEAR W_DETAIL.

    WHEN '&CDOC'.
      PERFORM F_ASSIGN_TOPIC USING LST_DOCTYPE TXT_PRCNT CHANGING TXT_TOPIC TXT_ATTEND.

    WHEN '&CVAT' OR '&CWHT'.
      PERFORM F_SIMULATE.

    WHEN '&CMGR'.
      PERFORM F_ASSIGN_POS USING LST_MGRNR CHANGING TXT_POS.

    WHEN '&ADD'.
      PERFORM F_ADD_REMARK TABLES I_DETAIL.

    WHEN '&DEL'.
      PERFORM F_DEL_REMARK TABLES I_DETAIL.

    WHEN '&CRE' OR '&SAVE'.

      PERFORM F_CHEK_REQUIRE_FIELD CHANGING R_FIELD.
      CHECK R_FIELD EQ 0.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          TEXTLINE1      = 'Are you sure create billing document ?'
          TITEL          = 'Confirm to create'
          CANCEL_DISPLAY = SPACE
        IMPORTING
          ANSWER         = GV_ANSWER.
      IF GV_ANSWER = 'J'.
        PERFORM F_SIMULATE.
        PERFORM F_CREATE_BILL TABLES I_DETAIL.
      ENDIF.

  ENDCASE.


  CLEAR SY-UCOMM.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0130_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  TC_MODIFY_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_MODIFY_0100 INPUT.

  READ TABLE I_DETAIL_OLD WITH KEY VBELN = W_DETAIL-VBELN
                             POSNR = W_DETAIL-POSNR
               INTO W_DETAIL_OLD.

  IF W_DETAIL-POSNR NE '999999'.
    IF SY-SUBRC EQ 0.
      IF W_DETAIL_OLD-KWMENG LT W_DETAIL-KWMENG.
        MESSAGE I000(38) WITH 'Quantity more than original!' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ELSE.
      MESSAGE I000(38) WITH 'Quantity more than original!' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE I_DETAIL INTO W_DETAIL_OLD INDEX TC_DETAIL-CURRENT_LINE.

*  Quantity Changed
  W_DETAIL-NETWR = W_DETAIL-NETPR * W_DETAIL-KWMENG.

* Calculate Adv
  W_DETAIL-KWERT = 0.
  LOOP AT I_KONV INTO W_KONV WHERE KNUMV = W_DETAIL-KNUMV AND KPOSN = W_DETAIL-POSNR.
    W_DETAIL-KWERT = W_DETAIL-KWERT + W_KONV-KWERT.
  ENDLOOP.

  READ TABLE I_VBAP INTO W_VBAP WITH KEY VBELN = W_DETAIL-VBELN
                                         POSNR = W_DETAIL-POSNR.
  IF SY-SUBRC EQ 0.
    W_DETAIL-KWERT = W_DETAIL-KWERT * ( W_DETAIL-KWMENG / W_VBAP-KWMENG ).
    W_DETAIL-KWERT = ( W_DETAIL-KWERT * TXT_PRCNT ) / 100.
  ENDIF.

  CLEAR : W_KONV,W_VBAP.
  MODIFY I_DETAIL FROM W_DETAIL INDEX TC_DETAIL-CURRENT_LINE.

  IF W_DETAIL_OLD-MEINH EQ 'SET'. " BOM
    PERFORM F_MODIFY_LINE_ITEMS TABLES I_DETAIL
                                USING W_DETAIL-VBELN
                                      W_DETAIL-POSNR
                                      W_DETAIL_OLD-SEL.

    PERFORM F_SIMULATE.
  ENDIF.

  IF W_DETAIL_OLD-MEINH NE 'SET'. "ITEM
    PERFORM F_MODIFY_BOM TABLES I_DETAIL
                                    USING W_DETAIL-VBELN
                                          W_DETAIL-POSNR
                                          W_DETAIL-KWMENG
                                   .
    PERFORM F_CAL_AMT TABLES I_DETAIL[].
    PERFORM F_CAL_AMT_BOM TABLES I_DETAIL[].
  ENDIF.


ENDMODULE.                 " TC_MODIFY_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_MODIFY_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_MODIFY_0110 INPUT.
  PERFORM F_SIMULATE.
ENDMODULE.                 " TC_MODIFY_0110  INPUT
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

    WHEN '&SALL'.
      W_DETAIL-SEL = 'X'.
      MODIFY I_DETAIL FROM W_DETAIL TRANSPORTING SEL WHERE MATNR IS NOT INITIAL.
      CLEAR W_DETAIL.
    WHEN '&DALL'.
      W_DETAIL-SEL = SPACE.
      MODIFY I_DETAIL FROM W_DETAIL TRANSPORTING SEL WHERE MATNR IS NOT INITIAL.
      CLEAR W_DETAIL.

    WHEN '&CDOC'.
      PERFORM F_ASSIGN_TOPIC IN PROGRAM ZAR_MSBILL_CRED IF FOUND USING LST_DOCTYPE TXT_PRCNT CHANGING TXT_TOPIC TXT_ATTEND.

    WHEN '&CVAT' OR '&CWHT'.
      PERFORM F_SIMULATE.

    WHEN '&CMGR'.
      PERFORM F_ASSIGN_POS USING LST_MGRNR CHANGING TXT_POS.

    WHEN '&ADD'.
      PERFORM F_ADD_REMARK TABLES I_DETAIL.

    WHEN '&DEL'.
      PERFORM F_DEL_REMARK  TABLES I_DETAIL.

    WHEN '&DELH'.
      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          TEXTLINE1      = 'Are you sure to delete billing document ?'
          TITEL          = 'Confirm to delete'
          CANCEL_DISPLAY = SPACE
        IMPORTING
          ANSWER         = GV_ANSWER.
      IF GV_ANSWER = 'J'.
        PERFORM F_DEL_BILLING_DOCUMENT USING TXT_DOCNR.
      ENDIF.

      CLEAR GV_ANSWER.

    WHEN '&CRE' OR '&SAVE'.
      PERFORM F_CHEK_REQUIRE_FIELD CHANGING R_FIELD.
      CHECK R_FIELD EQ 0.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          TEXTLINE1      = 'Are you sure save changed billing document ?'
          TITEL          = 'Confirm to change'
          CANCEL_DISPLAY = SPACE
        IMPORTING
          ANSWER         = GV_ANSWER.
      IF GV_ANSWER = 'J'.
        PERFORM F_SIMULATE.
        PERFORM F_SAVE_CHANGED_BILL TABLES I_DETAIL.
      ENDIF.

      CLEAR GV_ANSWER.
    WHEN '&AMAT'.
      PERFORM F_GET_VBAP_POP TABLES I_VBAP_POP USING TXT_VBELN.
      IF I_VBAP_POP[] IS NOT INITIAL.
        CALL SCREEN 0300 STARTING AT 5 2 ENDING AT 125 18.
      ELSE.
        MESSAGE I000(38) WITH 'No sale order items to added!' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

  ENDCASE.


  CLEAR SY-UCOMM.



ENDMODULE.                 " USER_COMMAND_0100  INPUT


*&---------------------------------------------------------------------*
*&      Module  TC_MODIFY_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_MODIFY_0200 INPUT.
*  Quantity Changed
  W_DETAIL_200-NETWR = W_DETAIL_200-NETPR * W_DETAIL_200-KWMENG.

*  Material changed
*  PERFORM f_matnr_desc USING w_detail_200-matnr CHANGING w_detail_200-maktx.

* Material description changed
  IF W_DETAIL_200-MATNR IS INITIAL.
    PERFORM F_MAKTX_DESC USING W_DETAIL_200-MAKTX CHANGING W_DETAIL_200-MATNR W_DETAIL_200-MAKTX.
  ENDIF.

  READ TABLE I_DETAIL_200 INTO W_DETAIL_ADD INDEX  TC_DETAIL_200-CURRENT_LINE.
  IF SY-SUBRC EQ 0.
    MODIFY I_DETAIL_200 FROM W_DETAIL_200 INDEX TC_DETAIL_200-CURRENT_LINE.
  ELSE.
    APPEND W_DETAIL_200 TO I_DETAIL_200.
  ENDIF.

  PERFORM F_CAL_AMT_200 TABLES I_DETAIL_200[].
  PERFORM F_TC_DETAIL_200_LINES.
ENDMODULE.                 " TC_MODIFY_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_MODIFY_0210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_MODIFY_0210 INPUT.
  PERFORM F_SIMULATE_200.
ENDMODULE.                 " TC_MODIFY_0210  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE SY-UCOMM.
    WHEN '&BACK' OR '&LEAVE' OR '&EXIT'.
      LEAVE TO SCREEN 0.
    WHEN '&SIM'.
      PERFORM F_SIMULATE_200.

    WHEN '&SALL'.
      W_DETAIL_200-SEL = 'X'.
      MODIFY I_DETAIL_200 FROM W_DETAIL_200 TRANSPORTING SEL WHERE MAKTX IS NOT INITIAL.
      CLEAR W_DETAIL_200.
    WHEN '&DALL'.
      W_DETAIL_200-SEL = SPACE.
      MODIFY I_DETAIL_200 FROM W_DETAIL_200 TRANSPORTING SEL WHERE MAKTX IS NOT INITIAL.
      CLEAR W_DETAIL_200.

    WHEN '&CDOC'.
      PERFORM F_ASSIGN_TOPIC IN PROGRAM ZAR_MSBILL_CRED IF FOUND  USING LST_DOCTYPE TXT_PRCNT CHANGING TXT_TOPIC TXT_ATTEND.

    WHEN '&CVAT' OR '&CWHT'.
      PERFORM F_SIMULATE_200.

    WHEN '&CMGR'.
      PERFORM F_ASSIGN_POS USING LST_MGRNR CHANGING TXT_POS.

    WHEN '&CRE' OR '&SAVE'.

      PERFORM F_CHEK_REQUIRE_FIELD_200 CHANGING R_FIELD.
      CHECK R_FIELD EQ 0.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          TEXTLINE1      = 'Are you sure to save changed billing document ?'
          TITEL          = 'Confirm to change'
          CANCEL_DISPLAY = SPACE
        IMPORTING
          ANSWER         = GV_ANSWER.
      IF GV_ANSWER = 'J'.

        PERFORM F_SIMULATE_200.
        PERFORM F_SAVE_CHANGED_BILL_200 TABLES I_DETAIL_200[].
      ENDIF.

    WHEN '&DEL'.
      PERFORM F_DEL_ROWS TABLES I_DETAIL_200[].

    WHEN '&DELH'.
      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          TEXTLINE1      = 'Are you sure to delete billing document ?'
          TITEL          = 'Confirm to delete'
          CANCEL_DISPLAY = SPACE
        IMPORTING
          ANSWER         = GV_ANSWER.
      IF GV_ANSWER = 'J'.
        PERFORM F_DEL_BILLING_DOCUMENT USING TXT_DOCNR.
      ENDIF.

      CLEAR GV_ANSWER.
    WHEN OTHERS.
  ENDCASE.

  CLEAR SY-UCOMM.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.
  CASE SY-UCOMM.
    WHEN '&OK'.
      PERFORM F_ADD_LINE_ITEMS TABLES I_VBAP_POP I_DETAIL I_DETAIL_OLD I_VBAP.
      LEAVE TO SCREEN 0.
    WHEN '&CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CLEAR SY-UCOMM.
ENDMODULE.                 " USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_MODIFY_300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_MODIFY_300 INPUT.
  MODIFY I_VBAP_POP FROM W_VBAP_POP INDEX TC_VBAP_300-CURRENT_LINE.
ENDMODULE.                 " TC_MODIFY_300  INPUT

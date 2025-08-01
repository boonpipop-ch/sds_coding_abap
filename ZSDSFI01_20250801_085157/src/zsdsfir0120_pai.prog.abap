*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0120_PAI
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
    W_DETAIL-KWERT = W_DETAIL-KWERT * ( W_DETAIL-KWMENG / W_VBAP-KWMENG ). "weight value adv
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
                                    CHANGING
                                      GV_CORRECT
                                      GV_POSNR.
  ENDIF.

  PERFORM F_CAL_AMT TABLES I_DETAIL[].
  PERFORM F_CAL_AMT_BOM TABLES I_DETAIL[].

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
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  CASE SY-UCOMM.
    WHEN '&RFF'.
      PERFORM F_ASSIGN_MANAGER.
      CALL SCREEN 100.
    WHEN '&UKI'.
      PERFORM F_ASSIGN_MANAGER.
      CALL SCREEN 200.
    WHEN '&BACK' OR '&LEAVE' OR '&EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR SY-UCOMM.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
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
    WHEN '&CLR'.
      PERFORM F_CLEAR_SCREEN.
    WHEN '&SALL'.
      W_DETAIL_200-SEL = 'X'.
      MODIFY I_DETAIL_200 FROM W_DETAIL_200 TRANSPORTING SEL WHERE MAKTX IS NOT INITIAL.
      CLEAR W_DETAIL_200.
    WHEN '&DALL'.
      W_DETAIL_200-SEL = SPACE.
      MODIFY I_DETAIL_200 FROM W_DETAIL_200 TRANSPORTING SEL WHERE MAKTX IS NOT INITIAL.
      CLEAR W_DETAIL_200.

    WHEN '&CDOC'.
      PERFORM F_ASSIGN_TOPIC USING LST_DOCTYPE TXT_PRCNT CHANGING TXT_TOPIC TXT_ATTEND.

    WHEN '&CVAT' OR '&CWHT'.
      PERFORM F_SIMULATE_200.

    WHEN '&CMGR'.
      PERFORM F_ASSIGN_POS USING LST_MGRNR CHANGING TXT_POS.

    WHEN '&CRE' OR '&SAVE'.

      PERFORM F_CHEK_REQUIRE_FIELD_200 CHANGING R_FIELD.
      CHECK R_FIELD EQ 0.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          TEXTLINE1      = 'Are you sure create billing document ?'
          TITEL          = 'Confirm to create'
          CANCEL_DISPLAY = SPACE
        IMPORTING
          ANSWER         = GV_ANSWER.
      IF GV_ANSWER = 'J'.

        PERFORM F_SIMULATE_200.
        PERFORM F_CREATE_BILL_200 TABLES I_DETAIL_200[].
      ENDIF.

    WHEN '&DEL'.
      PERFORM F_DEL_ROWS TABLES I_DETAIL_200[].
    WHEN '&ADD'.
      PERFORM F_ADD_ROWS TABLES I_DETAIL_200[].


    WHEN OTHERS.
  ENDCASE.

  CLEAR SY-UCOMM.
ENDMODULE.                 " USER_COMMAND_0200  INPUT

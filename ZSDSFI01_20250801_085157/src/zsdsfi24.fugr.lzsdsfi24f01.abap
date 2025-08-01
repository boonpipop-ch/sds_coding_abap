*----------------------------------------------------------------------*
***INCLUDE LZSDSFI24F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_COMMIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_commit .
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.
ENDFORM.                    " F_COMMIT
*&---------------------------------------------------------------------*
*&      Form  F_GET_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_RETURN  text
*      <--P_GV_MESSAGE  text
*----------------------------------------------------------------------*
FORM f_get_message  TABLES lt_return STRUCTURE bapiret2
                  CHANGING gv_message.

  DATA : lv_id          TYPE bapiret2-id,
         lv_number      TYPE bapiret2-number,
         lv_language    TYPE bapitga-langu,
         lv_textformat  TYPE bapitga-textformat,
         lv_linkpattern TYPE bapitga-linkmask,
         lv_message_v1  TYPE bapiret2-message_v1,
         lv_message_v2  TYPE bapiret2-message_v2,
         lv_message_v3  TYPE bapiret2-message_v3,
         lv_message_v4  TYPE bapiret2-message_v4,
         lv_message     TYPE bapiret2-message.

  DATA ls_return TYPE bapiret2.

  READ TABLE lt_return INTO ls_return
  WITH KEY type = 'E'.
  IF sy-subrc = 0.
    lv_id          = ls_return-id.
    lv_number      = ls_return-number.
    lv_language    = sy-langu.
    lv_textformat  = 'ASC'.
    lv_message_v1  = ls_return-message_v1.
    lv_message_v2  = ls_return-message_v2.
    lv_message_v3  = ls_return-message_v3.
    lv_message_v4  = ls_return-message_v4.

    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = lv_id
        number     = lv_number
        language   = lv_language
        textformat = lv_textformat
        message_v1 = lv_message_v1
        message_v2 = lv_message_v2
        message_v3 = lv_message_v3
        message_v4 = lv_message_v4
      IMPORTING
        message    = lv_message.

    gv_message = lv_message.
  ENDIF.

ENDFORM.                    " F_GET_MESSAGE

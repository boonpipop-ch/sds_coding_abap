*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0490_I01
*&---------------------------------------------------------------------*
*&SPWIZARD: INPUT MODULE FOR TC 'TABLE_CONTROL01'. DO NOT CHANGE THIS LI
*&SPWIZARD: MODIFY TABLE
MODULE TABLE_CONTROL01_MODIFY INPUT.

  IF ITAB_TABLE01-MENGE < ITAB_TABLE01-I_QTY.
    MESSAGE E398(00) WITH TEXT-T01.      "'invalid values: Split Qty'
  ENDIF.

  IF ITAB_TABLE01-DABMG < ITAB_TABLE01-I_QTY. " T41K939875
    MESSAGE E398(00) WITH TEXT-T01.      "'invalid values: Split Qty'
  ENDIF.

  IF NOT ITAB_TABLE01-I_DAT IS INITIAL AND
     ITAB_TABLE01-I_DAT < G_TODAY.
    MESSAGE W398(00) WITH TEXT-T09.      "'Date of past'
  ENDIF.

  IF ITAB_TABLE01-I_DAT IS INITIAL AND
     ITAB_TABLE01-EINDT < G_TODAY.
    MESSAGE W398(00) WITH TEXT-T09.      "'Date of past'
  ENDIF.

  MODIFY ITAB_TABLE01 INDEX TABLE_CONTROL01-CURRENT_LINE.

ENDMODULE.                    "TABLE_CONTROL01_MODIFY INPUT

**&SPWIZARD: INPUT MODULE FOR TC 'TABLE_CONTROL01'. DO NOT CHANGE THIS LI
**&SPWIZARD: PROCESS USER COMMAND
*MODULE table_control01_user_command INPUT.
*  ok_code = sy-ucomm.
*  PERFORM user_ok_tc USING    'TABLE_CONTROL01'
*                              'ITAB_TABLE01'
*                              ' '
*                     CHANGING ok_code.
*  sy-ucomm = ok_code.
*ENDMODULE.                    "TABLE_CONTROL01_USER_COMMAND INPUT
*&---------------------------------------------------------------------*
*&      Module  D0100_PAI_CHECK01  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_PAI_CHECK01 INPUT.

  DATA: LWA_SCHLIN TYPE I,
        LWA_MENGE  TYPE EKPO-MENGE,
        LWA_DABMG  TYPE EKES-MENGE.

  CHECK D0100-EBELN <> G_EBELN.

* DB Lock
  IF NOT D0100-EBELN IS INITIAL.
    CALL FUNCTION 'DEQUEUE_ALL'.

    CALL FUNCTION 'ENQUEUE_EMEKKOE'
      EXPORTING
        MODE_EKKO      = 'E'
        EBELN          = D0100-EBELN
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.
    CASE SY-SUBRC.
      WHEN 0.
      WHEN 1.                          "purchase order ist just locked
        SY-MSGV2 = SY-MSGV1.
*       message e272 with d0100-ebeln sy-msgv1.
        MESSAGE E006(ME) WITH SY-MSGV1 SPACE D0100-EBELN  SPACE.
      WHEN OTHERS.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDCASE.
  ENDIF.

  G_EBELN = D0100-EBELN.
  CLEAR:   D0100, ITAB_EKKOPO, ITAB_TABLE01,
           LWA_SCHLIN, LWA_MENGE, LWA_DABMG.
  REFRESH: ITAB_EKKOPO, ITAB_TABLE01.
  REFRESH CONTROL 'TABLE_CONTROL01' FROM SCREEN '0100'.

  CHECK NOT G_EBELN IS INITIAL.

* Get PO Data
* Get Header Data
  SELECT  SINGLE
          EBELN LIFNR
          EKORG EKGRP
          BEDAT
    INTO  (D0100-EBELN, D0100-LIFNR,
           D0100-EKORG, D0100-EKGRP,
           D0100-BEDAT)
    FROM  EKKO
    WHERE EBELN = G_EBELN.
* No Data
  IF SY-SUBRC <> 0.
    MESSAGE S019(06) WITH G_EBELN
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*   Vedor name
  SELECT SINGLE NAME1
      INTO D0100-VNAME
      FROM  LFA1
      WHERE LIFNR = D0100-LIFNR.

* Get Item Data
  SELECT  K~EBELN K~BSART K~LIFNR K~EKORG K~EKGRP K~BEDAT
          P~EBELP P~LOEKZ P~MATNR P~WERKS P~LGORT P~MENGE P~MEINS P~NETWR
    INTO  CORRESPONDING FIELDS OF TABLE ITAB_EKKOPO
    FROM  EKKO AS K
    INNER JOIN EKPO AS P
    ON    K~EBELN EQ P~EBELN
    WHERE K~EBELN EQ G_EBELN.
*      AND p~elikz EQ space
*      AND p~loekz EQ space.

  LOOP AT ITAB_EKKOPO.
    CLEAR ITAB_TABLE01.

*   Item data
    MOVE-CORRESPONDING ITAB_EKKOPO TO ITAB_TABLE01.
    ITAB_TABLE01-EBELP2 = ITAB_EKKOPO-EBELP.

*   Display DELETE-ICON on deleted item
    IF ITAB_EKKOPO-LOEKZ IS INITIAL.
      CLEAR ITAB_TABLE01-DELETE.
    ELSE.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          NAME       = 'ICON_DELETE'
          TEXT       = ''
          INFO       = 'Deleted'
          ADD_STDINF = 'X'
        IMPORTING
          RESULT     = ITAB_TABLE01-DELETE.
    ENDIF.

*   Delivery Date on Schedule Lines.
    SELECT COUNT(*) INTO LWA_SCHLIN
      FROM EKET
      WHERE EBELN = ITAB_EKKOPO-EBELN
      AND   EBELP = ITAB_EKKOPO-EBELP.
*   How many lines ?
    IF LWA_SCHLIN <> 1.
      ITAB_TABLE01-MULTI = 'X'.  "Can not split
*   Split OK
    ELSE.
      CLEAR ITAB_TABLE01-MULTI.
      SELECT SINGLE ETENR EINDT
      INTO (ITAB_TABLE01-ETENR, ITAB_TABLE01-EINDT)
      FROM EKET
      WHERE EBELN = ITAB_EKKOPO-EBELN
      AND   EBELP = ITAB_EKKOPO-EBELP.
    ENDIF.

*   Description of Storage Location
    SELECT  SINGLE LGOBE
      INTO  ITAB_TABLE01-LGOBE
      FROM  T001L
      WHERE LGORT = ITAB_EKKOPO-LGORT.

*   Already Delivered, Delivery Data
    SELECT SUM( MENGE )
      INTO LWA_DABMG
      FROM  EKES
      WHERE EBELN = ITAB_EKKOPO-EBELN
      AND   EBELP = ITAB_EKKOPO-EBELP
      AND   EBTYP = 'LA'.              "Inbound Deliver

*     If not selected, DeliveredQTY = 0.
*     OpenQTY            = RequestQTY - DeliveredQTY
    LWA_MENGE          = ITAB_TABLE01-MENGE.
    ITAB_TABLE01-DABMG = LWA_MENGE - LWA_DABMG.
    IF ITAB_TABLE01-DABMG < 0.
      ITAB_TABLE01-DABMG = 0.
    ENDIF.

    ITAB_TABLE01-FLG     = 'U'.
    ITAB_TABLE01-I_LGORT = ITAB_TABLE01-LGORT.
    ITAB_TABLE01-NETWR   = ITAB_EKKOPO-NETWR.

    APPEND ITAB_TABLE01.

  ENDLOOP.

ENDMODULE.                 " D0100_PAI_CHECK01  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'SPLIT'.
      PERFORM SUB_UCOMM_SPLIT.
    WHEN 'UNDO'.
      PERFORM SUB_UCOMM_UNDO.
    WHEN 'CLEAR'.
      G_CLEAR = 'X'.
    WHEN 'UPDDL'.
      PERFORM F_GOTO_DELIVERY_PO.
    WHEN 'SAVE'.
      PERFORM SUB_UCOMM_SAVE.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.

  PERFORM SUB_LEAVE_PROGRAM.

ENDMODULE.                 " USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL01_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL01_USER_COMMAND INPUT.

  DATA: FLDNAME(100) TYPE C,
        HELP(100)    TYPE C,
        COL          TYPE CXTAB_COLUMN,
        L_SUBRC      TYPE SY-SUBRC.

  OK_CODE = SY-UCOMM.

  READ TABLE TABLE_CONTROL01-COLS INTO COL WITH KEY SELECTED = 'X'.
  L_SUBRC = SY-SUBRC.

  SPLIT COL-SCREEN-NAME AT '-' INTO HELP FLDNAME.

  CASE OK_CODE.

*   Sort Up
    WHEN 'TABLE_SU'.
      IF L_SUBRC <> 0.
        MESSAGE S398(00) WITH TEXT-T08.
        EXIT.
      ENDIF.
      SORT ITAB_TABLE01 BY (FLDNAME).

*   Sort Down
    WHEN 'TABLE_SD'.
      IF L_SUBRC <> 0.
        MESSAGE S398(00) WITH TEXT-T08.
        EXIT.
      ENDIF.
      SORT ITAB_TABLE01 BY (FLDNAME) DESCENDING.

*   Search
    WHEN 'TABLE_SR'.
      IF L_SUBRC <> 0.
        MESSAGE S398(00) WITH TEXT-T08.
        EXIT.
      ENDIF.
      IF D0100-TEXT20 IS INITIAL.
        EXIT.
      ELSE.
        PERFORM SUB_SEARCH_SCROOL USING COL-SCREEN-NAME
                                        D0100-TEXT20
                                        0.
      ENDIF.
**   Search Next
*    WHEN 'TABLE_SN'.
*      IF l_subrc <> 0.
*        EXIT.
*      ENDIF.
*      IF g_text20 IS INITIAL.
*        EXIT.
*      ELSE.
*        PERFORM sub_search_scrool USING col-screen-name
*                                        g_text20
*                                        g_srch_line.
*      ENDIF.

  ENDCASE.

  SY-UCOMM = OK_CODE.
  CLEAR OK_CODE.

ENDMODULE.                 " TABLE_CONTROL01_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODULE_TEXT20  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODULE_TEXT20 INPUT.
  SET CURSOR FIELD 'd0100-text20'.
  IF G_TEXT20 <> D0100-TEXT20.
    LOOP AT ITAB_TABLE01.
      CLEAR ITAB_TABLE01-MARK.
      MODIFY ITAB_TABLE01.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " MODULE_TEXT20  INPUT

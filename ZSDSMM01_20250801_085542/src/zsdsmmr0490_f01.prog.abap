*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0490_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZI_MM_SPLIT_PO_NEW_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SUB_UCOMM_SPLIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUB_UCOMM_SPLIT .

  DATA: L_FLG      TYPE C,
        L_REFCHK   TYPE I,
        L_BUFF     TYPE T_TABLE01,
        L_QTY      TYPE EKPO-MENGE,
        L_NEWITM   TYPE EKPO-EBELP,
        L_ITABBUFF TYPE TABLE OF T_TABLE01 WITH HEADER LINE.

  LOOP AT ITAB_TABLE01.

    CHECK ITAB_TABLE01-I_QTY > 0.

*   From Document
    IF ITAB_TABLE01-FLG IS INITIAL.
      ITAB_TABLE01-FLG    = 'U'.
    ENDIF.
    L_QTY  = ITAB_TABLE01-DABMG - ITAB_TABLE01-I_QTY.
    IF L_QTY <= 0 AND ITAB_TABLE01-FLG = 'I'.
      IF NOT ITAB_TABLE01-I_DAT IS INITIAL.
        ITAB_TABLE01-EINDT = ITAB_TABLE01-I_DAT.
      ENDIF.
      CLEAR: ITAB_TABLE01-I_QTY, ITAB_TABLE01-I_DAT.
      ITAB_TABLE01-I_LGORT = ITAB_TABLE01-LGORT.

      SELECT SINGLE LGOBE
      FROM T001L
      INTO ITAB_TABLE01-LGOBE
      WHERE WERKS EQ '1000'
        AND LGORT EQ ITAB_TABLE01-I_LGORT.

      MODIFY ITAB_TABLE01 INDEX SY-TABIX.
      CONTINUE.
    ELSE.
      ITAB_TABLE01-DABMG = L_QTY.
      IF ITAB_TABLE01-FLG = 'I'.
        ITAB_TABLE01-MENGE = ITAB_TABLE01-DABMG.
      ELSE.
        ITAB_TABLE01-MENGE = ITAB_TABLE01-MENGE - ITAB_TABLE01-I_QTY.
      ENDIF.
      MOVE-CORRESPONDING ITAB_TABLE01 TO L_BUFF.
      CLEAR: ITAB_TABLE01-I_QTY, ITAB_TABLE01-I_DAT.
      ITAB_TABLE01-I_LGORT = ITAB_TABLE01-LGORT.

      SELECT SINGLE LGOBE
      FROM T001L
      INTO ITAB_TABLE01-LGOBE
      WHERE WERKS EQ '1000'
        AND LGORT EQ ITAB_TABLE01-I_LGORT.

      MODIFY ITAB_TABLE01 INDEX SY-TABIX.
    ENDIF.

*   To   Document
    CLEAR L_ITABBUFF. REFRESH L_ITABBUFF.
    L_ITABBUFF[] = ITAB_TABLE01[].
    SORT L_ITABBUFF DESCENDING BY EBELP.
    READ TABLE L_ITABBUFF INDEX 1.

    IF ITAB_TABLE01-BSART = 'NB'.
      L_NEWITM = 10.
    ELSE.
      L_NEWITM = 1.
    ENDIF.

    L_BUFF-FLG    = 'I'.
    L_BUFF-EBELP  = L_ITABBUFF-EBELP + L_NEWITM.
    L_BUFF-EBELP2 = ITAB_TABLE01-EBELP.
    L_BUFF-MENGE  = L_BUFF-I_QTY.
    L_BUFF-DABMG  = L_BUFF-I_QTY.
    IF NOT L_BUFF-I_DAT IS INITIAL.
      L_BUFF-EINDT = L_BUFF-I_DAT.
    ENDIF.

    CLEAR: L_BUFF-I_QTY, L_BUFF-I_DAT.

    SELECT SINGLE LGOBE
      FROM T001L
      INTO L_BUFF-LGOBE
      WHERE WERKS EQ '1000'
        AND LGORT EQ L_BUFF-I_LGORT.

    L_BUFF-LGORT = L_BUFF-I_LGORT.
    APPEND L_BUFF TO ITAB_TABLE01.

  ENDLOOP.

ENDFORM.                    " SUB_UCOMM_SPLIT
*&---------------------------------------------------------------------*
*&      Form  SUB_UCOMM_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUB_UCOMM_SAVE .

  DATA: L_YESNO     TYPE C,
        LWA_TESTRUN TYPE BAPIFLAG,
        LWA_MSGCNT  TYPE I,
        LWA_ERRCNT  TYPE I,
        L_RC        TYPE SY-SUBRC.

  DATA: LV_STATUS TYPE ZSDSMMT002-STATU,
        LV_SPE    TYPE C.

  CLEAR ITAB_MSG.
  REFRESH ITAB_MSG.

  PERFORM F_UNRELEASE_PO USING LV_STATUS
                               LV_SPE.

  LOOP AT ITAB_TABLE01 WHERE FLG = 'I' OR FLG = 'U'.
    EXIT.
  ENDLOOP.
  CHECK SY-SUBRC = 0.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TEXT_QUESTION         = TEXT-T02     "'Do you wish to saved ata ?'
      TEXT_BUTTON_1         = TEXT-T03     "'Save'
      ICON_BUTTON_1         = 'ICON_CHECKED'
      TEXT_BUTTON_2         = TEXT-T04     "'Cancel'
      ICON_BUTTON_2         = 'ICON_INCOMPLETE'
      DISPLAY_CANCEL_BUTTON = ' '
    IMPORTING
      ANSWER                = L_YESNO.
  CHECK L_YESNO = 1.

* Display refresh
  G_CLEAR = 'X'.

  PERFORM SUB_CHANGE_PO.

  CLEAR: LWA_MSGCNT, LWA_ERRCNT.
  DESCRIBE TABLE ITAB_MSG LINES LWA_MSGCNT.
  IF NOT LWA_MSGCNT IS INITIAL.
    LOOP AT ITAB_MSG.
      IF ITAB_MSG-ID   <> 'BAPI'.
        MESSAGE ID     ITAB_MSG-ID
                TYPE   ITAB_MSG-TYPE
                NUMBER ITAB_MSG-NUMBER
                WITH   ITAB_MSG-MESSAGE_V1 ITAB_MSG-MESSAGE_V2
                       ITAB_MSG-MESSAGE_V3 ITAB_MSG-MESSAGE_V4.
      ENDIF.
      IF ITAB_MSG-TYPE <> 'W'.
        LWA_ERRCNT = LWA_ERRCNT + 1.
      ENDIF.
    ENDLOOP.
    CALL FUNCTION 'DEQUEUE_ALL'.
    IF NOT LWA_ERRCNT IS INITIAL.
      EXIT.
    ENDIF.
  ENDIF.

  LWA_TESTRUN = D0100-TESTRUN.
  CHECK LWA_TESTRUN IS INITIAL.

* Commit database
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = 'X'.

  CALL FUNCTION 'DEQUEUE_ALL'.

  COMMIT WORK AND WAIT.

  MESSAGE S440(ME) WITH G_EBELN TEXT-T05. "'Split PO'
  PERFORM F_RELEASE_PO USING LV_STATUS
                             LV_SPE.

ENDFORM.                    " SUB_UCOMM_SAVE
*&---------------------------------------------------------------------*
*&      Form  SUB_LEAVE_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUB_LEAVE_PROGRAM .

  DATA: L_YESNO TYPE C.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TEXT_QUESTION         = TEXT-T06    "'Do you want to exit?'
      TEXT_BUTTON_1         = TEXT-T07    "'Exit'
*     icon_button_1         = 'ICON_CHECKED'
      TEXT_BUTTON_2         = TEXT-T04    "'Cancel'
*     icon_button_2         = 'ICON_INCOMPLETE'
      DISPLAY_CANCEL_BUTTON = ' '
    IMPORTING
      ANSWER                = L_YESNO.
  CHECK L_YESNO = 1.

  LEAVE PROGRAM.

ENDFORM.                    " SUB_LEAVE_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  SUB_CHANGE_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SUB_CHANGE_PO .

  DATA : LWA_EBELN    LIKE BAPIMEPOHEADER-PO_NUMBER,
         LS_POHEADER  LIKE BAPIMEPOHEADER,
         LS_POHEADERX LIKE BAPIMEPOHEADERX,
         LWA_PO       LIKE BAPIMEPOITEM,
         LWA_SP_PO    LIKE BAPIMEPOITEM,
         LWA_PS       LIKE BAPIMEPOSCHEDULE,
         LWA_SP_PS    LIKE BAPIMEPOSCHEDULE,
         LWA_TESTRUN  LIKE BAPIFLAG,
         LWA_MSGCNT   LIKE SY-DBCNT,
         LWA_NEWITM   TYPE I,
         LWA_INDEX    TYPE I,
         LWA_ITMCNT   TYPE I.

  DATA : LIT_RETURN LIKE BAPIRET2         OCCURS 0 WITH HEADER LINE,
         LIT_PO     LIKE BAPIMEPOITEM     OCCURS 0 WITH HEADER LINE,
         LIT_SP_PO  LIKE BAPIMEPOITEM     OCCURS 0 WITH HEADER LINE,
         LIT_POX    LIKE BAPIMEPOITEMX    OCCURS 0 WITH HEADER LINE,
         LIT_PS     LIKE BAPIMEPOSCHEDULE OCCURS 0 WITH HEADER LINE,
         LIT_SP_PS  LIKE BAPIMEPOSCHEDULE OCCURS 0 WITH HEADER LINE,
         LIT_PSX    LIKE BAPIMEPOSCHEDULX OCCURS 0 WITH HEADER LINE.

  DATA : NO_PRICE_FROM_PO	 TYPE BAPIFLAG-BAPIFLAG.

* Clear Work Area
  CLEAR : LWA_EBELN, LWA_TESTRUN,
          LWA_PO, LWA_SP_PO, LWA_PS, LWA_SP_PS,
          LS_POHEADER, LS_POHEADERX, LIT_RETURN,
          LIT_PO, LIT_SP_PO, LIT_POX,
          LIT_PS, LIT_SP_PS, LIT_PSX.
  REFRESH : LIT_PO, LIT_SP_PO, LIT_POX,
            LIT_PS, LIT_SP_PS, LIT_PSX.

  DATA : BEGIN OF LS_EKPO,
           EBELN TYPE EKPO-EBELN,
           EBELP TYPE EKPO-EBELP,
           NETPR TYPE EKPO-NETPR,
         END OF LS_EKPO.
  DATA LT_EKPO LIKE TABLE OF LS_EKPO.

  IF D0100-EBELN IS NOT INITIAL.
    PERFORM F_UNRELEASE.
  ENDIF.

* Get PO processing now
  CALL FUNCTION 'BAPI_PO_GETDETAIL1'
    EXPORTING
      PURCHASEORDER = G_EBELN
    IMPORTING
      POHEADER      = LS_POHEADER
    TABLES
      RETURN        = LIT_RETURN
      POITEM        = LIT_PO
      POSCHEDULE    = LIT_PS.

  SORT LIT_PO BY PO_ITEM.
  SORT LIT_PS BY PO_ITEM.

  IF ITAB_TABLE01[] IS NOT INITIAL.
    SELECT EBELN
           EBELP
           NETPR
      FROM EKPO
      INTO TABLE LT_EKPO
      FOR ALL ENTRIES IN ITAB_TABLE01
      WHERE EBELN EQ D0100-EBELN
        AND EBELP EQ ITAB_TABLE01-EBELP.
  ENDIF.

* Transfer splitted item to BAPI poitem
  LOOP AT ITAB_TABLE01 WHERE FLG = 'I' OR FLG = 'U'.

*   Get original item before split
    READ TABLE LIT_PO INTO LWA_SP_PO
         WITH KEY PO_ITEM = ITAB_TABLE01-EBELP2.
    LWA_SP_PO-PO_ITEM  = ITAB_TABLE01-EBELP.
    LWA_SP_PO-QUANTITY = ITAB_TABLE01-MENGE.

    READ TABLE LT_EKPO INTO LS_EKPO
    WITH KEY EBELN = ITAB_TABLE01-EBELN
             EBELP = ITAB_TABLE01-EBELP2.
    IF SY-SUBRC EQ 0.
      LWA_SP_PO-NET_PRICE = LS_EKPO-NETPR.
      LWA_SP_PO-PO_PRICE  = '1'.
    ENDIF.

    LWA_SP_PO-STGE_LOC = ITAB_TABLE01-I_LGORT.

*   Set split BAPI item
    APPEND LWA_SP_PO TO LIT_SP_PO.
    CLEAR : LS_EKPO.
  ENDLOOP.

* Transfer splitted item to BAPI schedule
  LOOP AT ITAB_TABLE01 WHERE FLG = 'I' OR FLG = 'U'.

*   Get original item before split
    READ TABLE LIT_PS INTO LWA_SP_PS
         WITH KEY PO_ITEM = ITAB_TABLE01-EBELP2.
    LWA_SP_PS-PO_ITEM       = ITAB_TABLE01-EBELP.
    LWA_SP_PS-SCHED_LINE    = ITAB_TABLE01-ETENR.
    LWA_SP_PS-QUANTITY      = ITAB_TABLE01-MENGE.

*    lwa_sp_ps-quantity      = itab_table01-dabmg.
    WRITE ITAB_TABLE01-EINDT TO LWA_SP_PS-DELIVERY_DATE. "Must be date
*   Set split BAPI schedule
    APPEND LWA_SP_PS TO LIT_SP_PS.
  ENDLOOP.

* PO headerx
  LS_POHEADERX-PO_NUMBER = 'X'.

* PO itemx
  LOOP AT LIT_SP_PO.
    LIT_POX-PO_ITEM   = LIT_SP_PO-PO_ITEM.
    LIT_POX-PO_ITEMX  = 'X'.
    LIT_POX-MATERIAL  = 'X'.
    LIT_POX-QUANTITY  = 'X'.
    LIT_POX-PLANT     = 'X'.
    LIT_POX-STGE_LOC  = 'X'.
    LIT_POX-NET_PRICE = 'X'.
    LIT_POX-PO_PRICE  = 'X'.
    APPEND LIT_POX.
    CLEAR  LIT_POX.
  ENDLOOP.

* PO schedulx
  LOOP AT LIT_SP_PS.
    LIT_PSX-PO_ITEM       = LIT_SP_PS-PO_ITEM.
    LIT_PSX-SCHED_LINE    = LIT_SP_PS-SCHED_LINE.
    LIT_PSX-PO_ITEMX      = 'X'.
    LIT_PSX-SCHED_LINEX   = 'X'.
    LIT_PSX-DELIVERY_DATE = 'X'.
    LIT_PSX-QUANTITY      = 'X'.
    APPEND LIT_PSX.
    CLEAR  LIT_PSX.
  ENDLOOP.

  CLEAR ITAB_MSG.
  SORT LIT_SP_PO BY PO_ITEM.
  SORT LIT_SP_PS BY PO_ITEM.

  NO_PRICE_FROM_PO = 'X'.

* Call function change PO
  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      PURCHASEORDER    = G_EBELN
      POHEADER         = LS_POHEADER
      POHEADERX        = LS_POHEADERX
      TESTRUN          = LWA_TESTRUN
      NO_PRICE_FROM_PO = NO_PRICE_FROM_PO
    TABLES
      RETURN           = LIT_RETURN
      POITEM           = LIT_SP_PO
      POITEMX          = LIT_POX
      POSCHEDULE       = LIT_SP_PS
      POSCHEDULEX      = LIT_PSX.

  IF SY-SUBRC = 0.
* SY-SUBRC = 0, but BAPI reuturns error messages
* Check error message
    LOOP AT LIT_RETURN  WHERE TYPE = 'E'.
      ITAB_MSG-TYPE       = LIT_RETURN-TYPE.
      ITAB_MSG-ID         = LIT_RETURN-ID.
      ITAB_MSG-NUMBER     = LIT_RETURN-NUMBER.
      ITAB_MSG-MESSAGE    = LIT_RETURN-MESSAGE.
      ITAB_MSG-MESSAGE_V1 = LIT_RETURN-MESSAGE_V1.
      ITAB_MSG-MESSAGE_V2 = LIT_RETURN-MESSAGE_V2.
      ITAB_MSG-MESSAGE_V3 = LIT_RETURN-MESSAGE_V3.
      ITAB_MSG-MESSAGE_V4 = LIT_RETURN-MESSAGE_V4.
      APPEND ITAB_MSG.
      CLEAR ITAB_MSG.
    ENDLOOP.

** Warning messgae "Can delivery date be met?"
*    LOOP AT lit_return  WHERE type   = 'W'  AND
*                              id     = '06' AND
*                              number = '245'.
*      itab_msg-type       = lit_return-type.
*      itab_msg-id         = lit_return-id.
*      itab_msg-number     = lit_return-number.
*      itab_msg-message    = lit_return-message.
*      itab_msg-message_v1 = lit_return-message_v1.
*      itab_msg-message_v2 = lit_return-message_v2.
*      itab_msg-message_v3 = lit_return-message_v3.
*      itab_msg-message_v4 = lit_return-message_v4.
*      APPEND itab_msg.
*      CLEAR itab_msg.
*    ENDLOOP.

  ELSE.
* SY-SUBRC <> 0
* Check error message
    LOOP AT LIT_RETURN.
      ITAB_MSG-TYPE       = LIT_RETURN-TYPE.
      ITAB_MSG-ID         = LIT_RETURN-ID.
      ITAB_MSG-NUMBER     = LIT_RETURN-NUMBER.
      ITAB_MSG-MESSAGE    = LIT_RETURN-MESSAGE.
      ITAB_MSG-MESSAGE_V1 = LIT_RETURN-MESSAGE_V1.
      ITAB_MSG-MESSAGE_V2 = LIT_RETURN-MESSAGE_V2.
      ITAB_MSG-MESSAGE_V3 = LIT_RETURN-MESSAGE_V3.
      ITAB_MSG-MESSAGE_V4 = LIT_RETURN-MESSAGE_V4.
      APPEND ITAB_MSG.
      CLEAR ITAB_MSG.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " SUB_CHANGE_PO
*&---------------------------------------------------------------------*
*&      Form  SUB_SEARCH_SCROOL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FLDNAM  text
*      -->P_TEXT20  text
*----------------------------------------------------------------------*
FORM SUB_SEARCH_SCROOL  USING    P_FLDNAM
                                 P_TEXT20
                                 P_LINE.

  DATA: L_TC_NAME       LIKE FELD-NAME,
        L_TC_LINES_NAME LIKE FELD-NAME,
        L_TC_FIELD_NAME LIKE FELD-NAME.

  DATA: L_TC_COL TYPE CXTAB_COLUMN.

  DATA: L_LINES       TYPE I,
        L_CURR_LINE   TYPE I,
        L_CURR_PAGE   TYPE I,
        L_NEW_TOPLINE TYPE I,
        L_JUMP_PAGE   TYPE I,
        L_JUMP_TEMP   TYPE P DECIMALS 1,
        L_QTY_CHR(20) TYPE C,
        L_FIND        TYPE C.

  CLEAR: L_FIND.
  DESCRIBE TABLE ITAB_TABLE01 LINES L_LINES.

  SET CURSOR FIELD D0100-TEXT20.

  LOOP AT ITAB_TABLE01.
    L_CURR_LINE = SY-TABIX.
    IF L_CURR_LINE > P_LINE.

      CASE P_FLDNAM.
        WHEN 'ITAB_TABLE01-EBELP'.
          IF ITAB_TABLE01-EBELP CS P_TEXT20.
            L_FIND = 'X'.
            ITAB_TABLE01-MARK = 'X'.
            MODIFY ITAB_TABLE01 INDEX L_CURR_LINE.
          ENDIF.
        WHEN 'ITAB_TABLE01-MATNR'.
          IF ITAB_TABLE01-MATNR CS P_TEXT20.
            L_FIND = 'X'.
            ITAB_TABLE01-MARK = 'X'.
            MODIFY ITAB_TABLE01 INDEX L_CURR_LINE.
          ENDIF.
        WHEN 'ITAB_TABLE01-MENGE'.
          WRITE ITAB_TABLE01-MENGE TO L_QTY_CHR
                UNIT ITAB_TABLE01-MEINS.
          IF L_QTY_CHR CS P_TEXT20.
            L_FIND = 'X'.
            ITAB_TABLE01-MARK = 'X'.
            MODIFY ITAB_TABLE01 INDEX L_CURR_LINE.
          ENDIF.
        WHEN 'ITAB_TABLE01-DABMG'.
          WRITE ITAB_TABLE01-DABMG TO L_QTY_CHR
                UNIT ITAB_TABLE01-MEINS.
          IF L_QTY_CHR CS P_TEXT20.
            L_FIND = 'X'.
            ITAB_TABLE01-MARK = 'X'.
            MODIFY ITAB_TABLE01 INDEX L_CURR_LINE.
          ENDIF.
        WHEN 'ITAB_TABLE01-EINDT'.
          IF ITAB_TABLE01-EINDT CS P_TEXT20.
            L_FIND = 'X'.
            ITAB_TABLE01-MARK = 'X'.
            MODIFY ITAB_TABLE01 INDEX L_CURR_LINE.
          ENDIF.
        WHEN 'ITAB_TABLE01-LGOBE'.
          IF ITAB_TABLE01-LGOBE CS P_TEXT20.
            L_FIND = 'X'.
            ITAB_TABLE01-MARK = 'X'.
            MODIFY ITAB_TABLE01 INDEX L_CURR_LINE.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDLOOP.

  CLEAR L_CURR_LINE.
  LOOP AT ITAB_TABLE01.
    IF ITAB_TABLE01-MARK = 'X'.
      L_CURR_LINE = SY-TABIX.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF L_FIND IS INITIAL.
    CLEAR: G_SRCH_FLD,  G_SRCH_LINE.
    LOOP AT TABLE_CONTROL01-COLS INTO L_TC_COL.
      CLEAR L_TC_COL-SELECTED.
      MODIFY TABLE_CONTROL01-COLS FROM L_TC_COL.
    ENDLOOP.
    MESSAGE S398(00) WITH TEXT-T10.
    EXIT.
  ENDIF.

  MODIFY ITAB_TABLE01 INDEX L_CURR_LINE.
  L_JUMP_TEMP = L_CURR_LINE / 15.
  L_JUMP_TEMP = FLOOR( L_JUMP_TEMP ).
  L_JUMP_PAGE = L_JUMP_TEMP + 1.

  G_SRCH_LINE = L_CURR_LINE.
  G_TEXT20    = P_TEXT20.
  G_SRCH_FLD  = P_FLDNAM.

  IF NOT L_FIND IS INITIAL.
    TABLE_CONTROL01-TOP_LINE = G_SRCH_LINE.
  ENDIF.

  CALL FUNCTION 'SCROLLING_IN_TABLE'
    EXPORTING
      ENTRY_ACT      = TABLE_CONTROL01-TOP_LINE
      ENTRY_FROM     = 1
      ENTRY_TO       = TABLE_CONTROL01-LINES
      LAST_PAGE_FULL = 'X'
      LOOPS          = 15
*     ok_code        = p_ok
      OVERLAPPING    = 'X'
      PAGE_GO        = L_JUMP_PAGE
    IMPORTING
      ENTRY_NEW      = L_NEW_TOPLINE
    EXCEPTIONS
*     NO_ENTRY_OR_PAGE_ACT  = 01
*     NO_ENTRY_TO    = 02
*     NO_OK_CODE_OR_PAGE_GO = 03
      OTHERS         = 0.

  TABLE_CONTROL01-TOP_LINE = L_NEW_TOPLINE.

  LOOP AT TABLE_CONTROL01-COLS INTO L_TC_COL.
    CLEAR L_TC_COL-SELECTED.
    MODIFY TABLE_CONTROL01-COLS FROM L_TC_COL.
  ENDLOOP.


ENDFORM.                    " SUB_SEARCH_SCROOL
*&---------------------------------------------------------------------*
*&      Form  SUB_UCOMM_UNDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUB_UCOMM_UNDO .

  DATA: L_TABLE    TYPE T_TABLE01.

  DATA: L_MARKLINE TYPE I,
        L_MARKCNT  TYPE I,
        L_UNDOLINE TYPE I,
        L_SUBRC    TYPE SY-SUBRC,
        L_NO       TYPE C.

  CLEAR: ITAB_UNDOLIN.
  REFRESH: ITAB_UNDOLIN.
  CLEAR: L_MARKLINE, L_MARKCNT, L_UNDOLINE, L_SUBRC, L_NO.

  LOOP AT ITAB_TABLE01 WHERE FLG = 'I'.
  ENDLOOP.
  L_SUBRC = SY-SUBRC.
  CHECK L_SUBRC = 0.

  LOOP AT ITAB_TABLE01 WHERE MARK = 'X'.
    IF ITAB_TABLE01-FLG = 'U' AND
       ITAB_TABLE01-EBELP = ITAB_TABLE01-EBELP2.
      MESSAGE S398(00) WITH TEXT-T13.
      L_NO = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
  CHECK L_NO IS INITIAL.

  LOOP AT ITAB_TABLE01 WHERE MARK = 'X'.
    L_MARKLINE = SY-TABIX.
    L_MARKCNT  = L_MARKCNT + 1.
    MOVE: ITAB_TABLE01-FLG    TO ITAB_UNDOLIN-FLG,
          ITAB_TABLE01-DELETE TO ITAB_UNDOLIN-DELETE,
          ITAB_TABLE01-MULTI  TO ITAB_UNDOLIN-MULTI,
          ITAB_TABLE01-EBELN  TO ITAB_UNDOLIN-EBELN,
          ITAB_TABLE01-EBELP  TO ITAB_UNDOLIN-EBELP,
          ITAB_TABLE01-EBELP2 TO ITAB_UNDOLIN-EBELP2,
          ITAB_TABLE01-MATNR  TO ITAB_UNDOLIN-MATNR,
          ITAB_TABLE01-LGORT  TO ITAB_UNDOLIN-LGORT,
          ITAB_TABLE01-MENGE  TO ITAB_UNDOLIN-MENGE,
          ITAB_TABLE01-DABMG  TO ITAB_UNDOLIN-DABMG.
    APPEND ITAB_UNDOLIN.
    DELETE ITAB_TABLE01.
  ENDLOOP.

  LOOP AT ITAB_UNDOLIN.
    READ TABLE ITAB_TABLE01 INTO L_TABLE
         WITH KEY EBELN = ITAB_UNDOLIN-EBELN
                  EBELP = ITAB_UNDOLIN-EBELP2
                  MATNR = ITAB_UNDOLIN-MATNR
                  LGORT = ITAB_UNDOLIN-LGORT.
    IF SY-SUBRC = 0.
      L_TABLE-MENGE = L_TABLE-MENGE + ITAB_UNDOLIN-MENGE.
      L_TABLE-DABMG = L_TABLE-DABMG + ITAB_UNDOLIN-DABMG.
      MODIFY ITAB_TABLE01 FROM L_TABLE
             INDEX SY-TABIX
             TRANSPORTING MENGE DABMG.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " SUB_UCOMM_UNDO
*&---------------------------------------------------------------------*
*&      Form  F_UNRELEASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_UNRELEASE .

  RANGES : S_EBELN FOR EKPO-EBELN.

  CALL FUNCTION 'DEQUEUE_ALL'.

  CLEAR S_EBELN.
  S_EBELN-SIGN   = 'I'.
  S_EBELN-OPTION = 'EQ'.
  S_EBELN-LOW    = D0100-EBELN.
  APPEND S_EBELN.

  SUBMIT ZSDSMMR0020 USING SELECTION-SCREEN 1000
                       WITH S_EBELN IN S_EBELN[]
                       WITH P_NOALV EQ ABAP_TRUE
                       AND RETURN.

*  CALL FUNCTION 'ENQUEUE_EMEKKOE'
*    EXPORTING
*      mode_ekko      = 'E'
*      ebeln          = d0100-ebeln
*    EXCEPTIONS
*      foreign_lock   = 1
*      system_failure = 2
*      OTHERS         = 3.
*  CASE sy-subrc.
*    WHEN 0.
*    WHEN 1.                          "purchase order ist just locked
*      sy-msgv2 = sy-msgv1.
**       message e272 with d0100-ebeln sy-msgv1.
*      MESSAGE e006(me) WITH sy-msgv1 space d0100-ebeln  space.
*    WHEN OTHERS.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDCASE.

ENDFORM.                    " F_UNRELEASE
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_STORAGE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CHECK_STORAGE INPUT.

  SELECT SINGLE LGOBE
    FROM T001L
    INTO ITAB_TABLE01-LGOBE
    WHERE WERKS EQ '1000'
      AND LGORT EQ ITAB_TABLE01-I_LGORT.

  MODIFY ITAB_TABLE01 INDEX TABLE_CONTROL01-CURRENT_LINE
               TRANSPORTING LGOBE.

ENDMODULE.                 " M_CHECK_STORAGE  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_RELEASE_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_RELEASE_PO USING LV_STATUS
                        LV_SPE.


*    SUBMIT ZP_PO_RELEASE USING SELECTION-SCREEN 1000
*                         WITH S_EBELN IN S_EBELN
*                         AND RETURN.
*
*    COMMIT WORK AND WAIT.




*  DATA : LV_EBELN TYPE EKKO-EBELN.
*
*  RANGES : S_EBELN FOR EKKO-EBELN.
*
**  IF lv_status EQ 'COMP'.
*  UPDATE ZTMM_STATUS_PO SET STATUS = LV_STATUS
*                      WHERE EBELN EQ D0100-EBELN.
*  COMMIT WORK AND WAIT.
**  ENDIF.
*
*  SELECT SINGLE EBELN
*    FROM ZTMM_STATUS_PO
*    INTO LV_EBELN
*    WHERE EBELN  EQ D0100-EBELN
*      AND STATUS EQ 'COMP'.
*  IF SY-SUBRC EQ 0.
*    UPDATE ZTMM_PO_RELEASE SET STATU = SPACE
*                               ERNAM = SY-UNAME
*                               ERDAT = SY-DATUM
*                         WHERE EBELN EQ D0100-EBELN.
*
*    UPDATE ZTMM_STATUS_PO SET STATUS = 'COMP'
*                        WHERE EBELN EQ D0100-EBELN.
*
*    CLEAR : S_EBELN,S_EBELN[].
*    S_EBELN-SIGN   = 'I'.
*    S_EBELN-OPTION = 'EQ'.
*    S_EBELN-LOW    = D0100-EBELN.
*    APPEND S_EBELN.
*
*    SUBMIT ZP_PO_RELEASE USING SELECTION-SCREEN 1000
*                         WITH S_EBELN IN S_EBELN
*                         AND RETURN.
*
*    COMMIT WORK AND WAIT.
*  ELSE.
*    SELECT SINGLE EBELN
*    FROM ZTMM_PO_RELEASE
*    INTO LV_EBELN
*    WHERE EBELN EQ D0100-EBELN
*      AND STATU EQ 'APP'.
*    IF SY-SUBRC EQ 0.
*      UPDATE ZTMM_PO_RELEASE SET STATU = SPACE
*                                 ERNAM = SY-UNAME
*                                 ERDAT = SY-DATUM
*                           WHERE EBELN EQ D0100-EBELN.
*
*      UPDATE ZTMM_STATUS_PO SET STATUS = 'COMP'
*                          WHERE EBELN EQ D0100-EBELN.
*
*      CLEAR : S_EBELN,S_EBELN[].
*      S_EBELN-SIGN   = 'I'.
*      S_EBELN-OPTION = 'EQ'.
*      S_EBELN-LOW    = D0100-EBELN.
*      APPEND S_EBELN.
*
*      SUBMIT ZP_PO_RELEASE USING SELECTION-SCREEN 1000
*                           WITH S_EBELN IN S_EBELN
*                           AND RETURN.
*
*      COMMIT WORK AND WAIT.
*    ELSE.
*      IF LV_SPE EQ 'X'.
*        UPDATE ZTMM_STATUS_PO SET STATUS = 'COMP'
*                            WHERE EBELN EQ D0100-EBELN.
*        COMMIT WORK AND WAIT.
*
*        CLEAR : S_EBELN,S_EBELN[].
*        S_EBELN-SIGN   = 'I'.
*        S_EBELN-OPTION = 'EQ'.
*        S_EBELN-LOW    = D0100-EBELN.
*        APPEND S_EBELN.
*
*        SUBMIT ZP_PO_RELEASE USING SELECTION-SCREEN 1000
*                           WITH S_EBELN IN S_EBELN
*                           AND RETURN.
*
*        COMMIT WORK AND WAIT.
*
*        UPDATE ZTMM_STATUS_PO SET STATUS = LV_STATUS
*                      WHERE EBELN EQ D0100-EBELN.
*        COMMIT WORK AND WAIT.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  CALL FUNCTION 'DEQUEUE_ALL'.
*  COMMIT WORK AND WAIT.

ENDFORM.                    " F_RELEASE_PO
*&---------------------------------------------------------------------*
*&      Form  F_GOTO_DELIVERY_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GOTO_DELIVERY_PO.

  CALL TRANSACTION 'ZT_MM_UPD_DELIVER_PO'.


ENDFORM.                    " F_GOTO_DELIVERY_PO
*&---------------------------------------------------------------------*
*&      Form  F_UNRELEASE_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_UNRELEASE_PO USING LV_STATUS
                          LV_SPE.

*  DATA : P_EBELN  TYPE EKKO-EBELN,
*         LV_EBELN TYPE EKKO-EBELN.
*  RANGES : S_EBELN FOR EKKO-EBELN.
*
*  SELECT SINGLE STATU
*    FROM ZSDSMMT002
*    INTO LV_STATUS
*    WHERE EBELN EQ D0100-EBELN
*      AND RUNNG EQ ( SELECT MAX( RUNNG )
*                       FROM ZSDSMMT002
*                      WHERE EBELN EQ D0100-EBELN ).
*
*
*
*
*  DATA : I_EBELN TYPE  EKKO-EBELN,
*         I_EBELP TYPE  EKPO-EBELP,
*         I_BSTYP TYPE  EKKO-BSTYP,
*         I_FRGCO TYPE  FRGCO.
*
*  DATA : PURCHASEORDER  TYPE  BAPIMMPARA-PO_NUMBER,
*         PO_REL_CODE    TYPE  BAPIMMPARA-PO_REL_COD,
*         USE_EXCEPTIONS TYPE  BAPIMMPARA-SELECTION,
*         NO_COMMIT      TYPE  BAPIMMPARA-SELECTION.
*
*  DATA : E_FRGKZ TYPE  EBAN-FRGKZ,
*         E_FRGZU TYPE  EBAN-FRGZU,
*         E_FRGRL TYPE  EBAN-FRGRL.
*
*  DATA : BEGIN OF LS_T16FS,
*           FRGGR TYPE EKKO-FRGGR,
**    frgsx TYPE ekko-frgsx,
*           BSTYP TYPE EKKO-BSTYP,
*           FRGC1 TYPE T16FS-FRGC1,
*         END OF LS_T16FS.
*
*  DATA : LS_ZTMM_PO_RELEASE TYPE ZTMM_PO_RELEASE,
*         LT_ZTMM_PO_RELEASE TYPE TABLE OF ZTMM_PO_RELEASE.
*
*  RANGES : S_FRGGR  FOR EKKO-FRGGR,
*          S_BSTYP  FOR EKKO-BSTYP,
*          LS_EBELN FOR EKKO-EBELN.
*
*  P_EBELN = D0100-EBELN.
*
*  CLEAR : S_EBELN[].
*  SELECT SINGLE EBELN
*      FROM ZTMM_STATUS_PO
*      INTO LV_EBELN
*      WHERE EBELN  EQ P_EBELN
*        AND STATUS EQ 'COMP'.
*  IF LV_STATUS eq 'COM'.
*    SELECT SINGLE *
*      FROM ZTMM_PO_RELEASE
*      INTO LS_ZTMM_PO_RELEASE
*      WHERE EBELN EQ P_EBELN.
*
*    SELECT SINGLE EKKO~FRGGR
*                  EKKO~BSTYP
*                  FRGC1
*      FROM EKKO
*      INNER JOIN T16FS ON EKKO~FRGGR EQ T16FS~FRGGR AND
*                          EKKO~FRGSX EQ T16FS~FRGSX
*      INTO LS_T16FS
*      WHERE EBELN EQ P_EBELN.
*
*    PURCHASEORDER  = P_EBELN.
*    PO_REL_CODE    = LS_T16FS-FRGC1.
*
*    CLEAR S_FRGGR.
*    S_FRGGR-SIGN   = 'I'.
*    S_FRGGR-OPTION = 'EQ'.
*    S_FRGGR-LOW    = LS_T16FS-FRGGR.
*    APPEND S_FRGGR.
*
*    CLEAR S_BSTYP.
*    S_BSTYP-SIGN   = 'I'.
*    S_BSTYP-OPTION = 'EQ'.
*    S_BSTYP-LOW    = LS_T16FS-BSTYP.
*    APPEND S_BSTYP.
*
*    CLEAR S_EBELN.
*    S_EBELN-SIGN   = 'I'.
*    S_EBELN-OPTION = 'EQ'.
*    S_EBELN-LOW    = P_EBELN.
*    APPEND S_EBELN.
*
*    CALL FUNCTION 'BAPI_PO_RESET_RELEASE'
*      EXPORTING
*        PURCHASEORDER            = PURCHASEORDER
*        PO_REL_CODE              = PO_REL_CODE
*      EXCEPTIONS
*        AUTHORITY_CHECK_FAIL     = 1
*        DOCUMENT_NOT_FOUND       = 2
*        ENQUEUE_FAIL             = 3
*        PREREQUISITE_FAIL        = 4
*        RELEASE_ALREADY_POSTED   = 5
*        RESPONSIBILITY_FAIL      = 6
*        NO_RELEASE_ALREADY       = 7
*        NO_NEW_RELEASE_INDICATOR = 8
*        OTHERS                   = 9.
*    IF SY-SUBRC <> 0.
*      IF LS_ZTMM_PO_RELEASE IS NOT INITIAL.
*
*        UPDATE ZTMM_PO_RELEASE SET STATU = 'APP'
*                                   ERNAM = SY-UNAME
*                                   ERDAT = SY-DATUM
*                             WHERE EBELN EQ P_EBELN.
*
*      ELSE.
*
*        LS_ZTMM_PO_RELEASE-EBELN = P_EBELN.
*        LS_ZTMM_PO_RELEASE-STATU = 'APP'.
*        LS_ZTMM_PO_RELEASE-ERNAM = SY-UNAME.
*        LS_ZTMM_PO_RELEASE-ERDAT = SY-DATUM.
*
*        INSERT ZTMM_PO_RELEASE FROM LS_ZTMM_PO_RELEASE.
*      ENDIF.
*
*      UPDATE ZTMM_STATUS_PO SET STATUS = 'SAVE'
*                          WHERE EBELN EQ P_EBELN.
*
*      COMMIT WORK AND WAIT.
*
*      SUBMIT RM06EF00 USING SELECTION-SCREEN 1000
*                      WITH FRGCO   EQ LS_T16FS-FRGC1
*                      WITH S_FRGGR IN S_FRGGR
*                      WITH P_FRGRS EQ 'X'
*                      WITH LISTU   EQ 'BEST'
*                      WITH S_BSTYP IN S_BSTYP
*                      WITH S_EBELN IN S_EBELN
*                      AND RETURN VIA SELECTION-SCREEN.
*
*      COMMIT WORK AND WAIT.
*
*      MESSAGE S001(Z_MM) WITH 'PO has been unreleased already'.
*    ELSE.
*
*      IF LS_ZTMM_PO_RELEASE IS NOT INITIAL.
*
*        UPDATE ZTMM_PO_RELEASE SET STATU = 'APP'
*                                   ERNAM = SY-UNAME
*                                   ERDAT = SY-DATUM
*                             WHERE EBELN EQ P_EBELN.
*
*      ELSE.
*
*        LS_ZTMM_PO_RELEASE-EBELN = P_EBELN.
*        LS_ZTMM_PO_RELEASE-STATU = 'APP'.
*        LS_ZTMM_PO_RELEASE-ERNAM = SY-UNAME.
*        LS_ZTMM_PO_RELEASE-ERDAT = SY-DATUM.
*
*        INSERT ZTMM_PO_RELEASE FROM LS_ZTMM_PO_RELEASE.
*      ENDIF.
*
*      UPDATE ZTMM_STATUS_PO SET STATUS = 'SAVE'
*                          WHERE EBELN EQ P_EBELN.
*
*      COMMIT WORK AND WAIT.
*
*      MESSAGE S001(Z_MM) WITH 'PO has been unreleased already'.
*
*    ENDIF.
*
*  ELSE.
*    DATA : LV_FRGKE TYPE EKKO.
*
*    SELECT SINGLE FRGKE
*      FROM EKKO
*      INTO LV_FRGKE
*      WHERE EBELN EQ P_EBELN
*        AND FRGKE EQ 'G'.
*    IF SY-SUBRC EQ 0.
*      LV_SPE = 'X'.
*      SELECT SINGLE EKKO~FRGGR
*                    EKKO~BSTYP
*                    FRGC1
*      FROM EKKO
*      INNER JOIN T16FS ON EKKO~FRGGR EQ T16FS~FRGGR AND
*                          EKKO~FRGSX EQ T16FS~FRGSX
*      INTO LS_T16FS
*      WHERE EBELN EQ P_EBELN.
*
*      PURCHASEORDER  = P_EBELN.
*      PO_REL_CODE    = LS_T16FS-FRGC1.
*
*      CALL FUNCTION 'BAPI_PO_RESET_RELEASE'
*        EXPORTING
*          PURCHASEORDER            = PURCHASEORDER
*          PO_REL_CODE              = PO_REL_CODE
*        EXCEPTIONS
*          AUTHORITY_CHECK_FAIL     = 1
*          DOCUMENT_NOT_FOUND       = 2
*          ENQUEUE_FAIL             = 3
*          PREREQUISITE_FAIL        = 4
*          RELEASE_ALREADY_POSTED   = 5
*          RESPONSIBILITY_FAIL      = 6
*          NO_RELEASE_ALREADY       = 7
*          NO_NEW_RELEASE_INDICATOR = 8
*          OTHERS                   = 9.
*      IF SY-SUBRC <> 0.
*        SUBMIT RM06EF00 USING SELECTION-SCREEN 1000
*                      WITH FRGCO   EQ LS_T16FS-FRGC1
*                      WITH S_FRGGR IN S_FRGGR
*                      WITH P_FRGRS EQ 'X'
*                      WITH LISTU   EQ 'BEST'
*                      WITH S_BSTYP IN S_BSTYP
*                      WITH S_EBELN IN S_EBELN
*                      AND RETURN VIA SELECTION-SCREEN.
*      ELSE.
*        COMMIT WORK AND WAIT.
*      ENDIF.
*
*    ELSE.
*      MESSAGE S001(Z_MM) WITH 'Cannot release after submit' DISPLAY LIKE 'E'.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " F_UNRELEASE_PO
*&---------------------------------------------------------------------*
*&      Form  F_UNLOCK_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_UNLOCK_PO.

  CALL FUNCTION 'DEQUEUE_EMEKKOE'
    EXPORTING
      MODE_EKKO = 'E'
      MODE_EKPO = 'E'
      MANDT     = SY-MANDT
      EBELN     = D0100-EBELN.

  COMMIT WORK AND WAIT.

ENDFORM.                    " F_UNLOCK_PO

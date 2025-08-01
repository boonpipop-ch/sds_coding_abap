*&---------------------------------------------------------------------*
*& Report ZSDSMMR0630
*  Author             : Wantanee Prateep na thalang
*  Add-on ID          :
*  Description        : Program upload serial
*  Purpose            :
*  Copied from        : ZMM_UPLOAD_SERIAL2
*  Restriction        :
*&---------------------------------------------------------------------*
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
REPORT ZSDSMMR0630.
************************************************************************
*      T A B L E S                                                     *
************************************************************************
TABLES: SER01,   "SERIAL NUMBER
        LIKP,    "SD DOCUMENT: DELIVERY HEADER DATA
        LIPS,    "SD DOCUMENT: DELIVERY ITEM DATA
        VBUK,    "SALES DOCUMENT: HEADER STATUS AND ADMINISTRATIVE DATA
        EQUI,    "EQUIPMENT MASTER DATA <SERIAL>
        SOMLRECI1. "SEND MAIL.

************************************************************************
*      I N T E R N A L     T A B L E S                                 *
************************************************************************
DATA: BEGIN OF I_TAB OCCURS 0,
        TXT(200),
*        DELIVERY(10), "INBOUND DELIVERY
*        ITEM(6),
*        SERIAL(18),
      END OF I_TAB.

DATA: BEGIN OF IN_TAB OCCURS 0,
        EX_ID(10),    "EXTERNAL ID
        VBELN(10),
        ITEM(6),
        SERIAL(18),
        MATNR(18),
        LFART(4),
      END OF IN_TAB.

DATA: BEGIN OF IN_CHK OCCURS 0,
        DELIVERY(10), "INBOUND DELIVERY
        ITEM(6),
        SERIAL(18),
        LFART(4),
        MATNR(18),
        FLAG(1),
        PICK(1),
        BLOCK(1),
      END OF IN_CHK.

DATA: BEGIN OF IN_PICK OCCURS 0.
        INCLUDE STRUCTURE IN_CHK.
DATA: END OF IN_PICK.

DATA: BEGIN OF I_CHK OCCURS 0,
        VBELN  LIKE LIKP-VBELN,
        LFART  LIKE LIKP-LFART,
        POSNR  LIKE LIPS-POSNR,
      END OF I_CHK.

DATA: BEGIN OF GT_SER01 OCCURS 0,
      LIEF_NR  LIKE SER01-LIEF_NR,
      POSNR    LIKE SER01-POSNR,
      END OF GT_SER01.

DATA: BEGIN OF TLINES OCCURS 1.
        INCLUDE STRUCTURE TLINE.
DATA: END OF TLINES.

DATA: BEGIN OF HEADER.
        INCLUDE STRUCTURE THEAD.
DATA: END OF HEADER.

DATA: BDCDATA  LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.

DATA : GV_CHECK TYPE C,
       GV_TAXT  TYPE C LENGTH 255.

DATA: MSG(100)      TYPE C,
      GV_FLAG       TYPE CHAR1,
      GV_ITEM(6)    TYPE C,
      GV_MATNR(18)  TYPE C,
      GV_SERIAL(18) TYPE C.

DATA:
*- STRUCTURE TO HOLD OUTPUT DETAILS IN DELIMITED FORMAT
         GF_OUTPUT_SOLI TYPE  SOLI,
*- STRUCTURE TO HOLD IMPORTED OBJECT COMPONENTS
         GF_OBJPACK     TYPE SOPCKLSTI1,
*- STRUCTURE TO HOLD DATA IN SINGLE LINE FORMAT
         GF_OBJHEAD     TYPE SOLISTI1,
*- STRUCTURE TO HOLD DATA IN SINGLE LINE FORMAT
         GF_OBJTXT      TYPE SOLISTI1,
*- STRUCTURE TO HOLD DATA FOR API RECIPIENT LIST
         GF_RECLIST     TYPE SOMLRECI1,
*- STRUCTURE TO HOLD  EMAIL DATA
         GF_DOC_CHNG    TYPE SODOCCHGI1,
*- INTERNAL TABLE TO HOLD OUTPUT DETAILS IN DELIMITED FORMAT
         GT_OUTPUT_SOLI TYPE TABLE OF SOLI,
*- INTERNAL TABLE TO HOLD IMPORTED OBJECT COMPONENTS
         GT_OBJPACK     TYPE TABLE OF SOPCKLSTI1,
*- INTERNAL TABLE TO HOLD DATA IN SINGLE LINE FORMAT
         GT_OBJHEAD     TYPE TABLE OF SOLISTI1,
*- INTERNAL TABLE TO HOLD DATA IN SINGLE LINE FORMAT
         GT_OBJTXT      TYPE TABLE OF SOLISTI1,
*- INTERNAL TABLE TO HOLD DATA FOR API RECIPIENT LIST
         GT_RECLIST     TYPE TABLE OF SOMLRECI1.

DATA: G_LINES     TYPE SY-TABIX,  "TO HOLD NUMBER OF RECORDS
      G_MSG_LINES TYPE SY-TABIX,  "TO HOLD NUMBER OF RECORDS
      G_SENT_ALL(1) TYPE C.

************************************************************************
*      S E L E C T I O N     S C R E E N                               *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-B01.
PARAMETERS: P_FILE LIKE RLGRAP-FILENAME DEFAULT 'C:\' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-B02.
PARAMETERS: P_INB   RADIOBUTTON GROUP B2
                    DEFAULT 'X' USER-COMMAND UCOM.
PARAMETERS: P_OUTB  RADIOBUTTON GROUP B2.
PARAMETERS: P_1700  AS CHECKBOX.  "ADD BY WANTANEE 20200115 T41K934530
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-B03.
SELECTION-SCREEN COMMENT 3(41) TEXT-C03 FOR FIELD P_GI.
PARAMETERS: P_GI AS CHECKBOX DEFAULT '' MODIF ID SAA.
SELECTION-SCREEN END OF BLOCK B3.

*SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-B04.
SELECT-OPTIONS: S_RECEI FOR SOMLRECI1-RECEIVER NO INTERVALS MODIF ID MAL.
*SELECTION-SCREEN END OF BLOCK B4.

PARAMETERS: P_CHECK AS CHECKBOX DEFAULT SPACE.
*&---------------------------------------------------------------------*
*  CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS : C_INT(3)          TYPE C VALUE 'INT',   "INTERNET MAIL ADDRESS
            C_DOC_TYPE_HTM(3) TYPE C VALUE 'HTM',   "CODE FOR DOCUMENT CLASS
            C_REC_TYPE        TYPE C VALUE 'U',     "RECIPIENT TYPE
            C_EXPRESS         TYPE C VALUE 'X'.     "SEND EXPRESS

************************************************************************
*      A T     S E L E C T I O N     S C R E E N                       *
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  PERFORM MODIFY_SCREEN.
  PERFORM F_GET_SELECTION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM GET_PATH_NAME CHANGING P_FILE.

************************************************************************
*      B E G I N     S E L E C T I O N                                 *
************************************************************************
START-OF-SELECTION .
* GET DATA
  IF P_CHECK NE 'X'.
    PERFORM UPLOAD_DATA.
    CLEAR GV_CHECK.
  ELSE.
    PERFORM F_IMPORT_DATA.
    CLEAR GV_CHECK.
  ENDIF.

  IF I_TAB[] IS INITIAL.
*    MESSAGE I000.
  ELSE.
*   CHECK INBOUND OR OUTBOUND

    PERFORM CHECK_DATA.
    PERFORM F_CHECK_DATA_ZSDSSDT001.


*   PROCESS DATA
    READ TABLE IN_CHK WITH KEY FLAG = 'X'.
    IF SY-SUBRC = 0.
      IF P_CHECK EQ 'X'.
        GV_CHECK = 'X'.
        PERFORM F_UPDATE_ERROR.
        GV_TAXT = 'INCORRECT SERIAL NO. PLEASE, CHECK DATA'.
        PERFORM F_SEND_MAIL USING GV_TAXT
                                  'F'.
      ELSE.
        "MESSAGE I001 WITH 'INCORRECT SERIAL NO. PLEASE, CHECK DATA'.
        WRITE: / 'INCORRECT SERIAL NO. PLEASE, CHECK DATA'.
        SKIP 1.
        WRITE: /2 'ITEM',
               10 'MODEL NO',
               30 'SERIAL NO'.
        WRITE: SY-ULINE.
        LOOP AT IN_CHK WHERE FLAG = 'X'.
          WRITE: /2 IN_CHK-ITEM,
                 10 IN_CHK-MATNR,
                 30 IN_CHK-SERIAL.
        ENDLOOP.
      ENDIF.
    ELSE.

*      PERFORM PROCESS_DATA.   "ISS DEL - 12135

*     ISS START INS - 12135
      READ TABLE IN_CHK WITH KEY PICK = 'X'.
      IF SY-SUBRC = 0.
        IF P_CHECK EQ 'X'.
          GV_CHECK = 'X'.
          PERFORM F_UPDATE_ERROR.
          GV_TAXT = 'SERIAL NO. MISMATCH DELIV.QTY : PLEASE, CHECK DATA'.
          PERFORM F_SEND_MAIL USING GV_TAXT
                                    'P'.
        ELSE.
          "MESSAGE I001 WITH 'INCORRECT SERIAL NO. PLEASE, CHECK DATA'.
          WRITE: / 'SERIAL NO. MISMATCH DELIV.QTY : PLEASE, CHECK DATA'.
          SKIP 1.
          WRITE: /2 'ITEM',
                 10 'MODEL NO',
                 30 'SERIAL NO'.
          WRITE: SY-ULINE.
          LOOP AT IN_CHK WHERE PICK = 'X'.
            WRITE: /2 IN_CHK-ITEM,
                   10 IN_CHK-MATNR,
                   30 IN_CHK-SERIAL.
          ENDLOOP.
        ENDIF.
      ELSE.

        READ TABLE IN_CHK WITH KEY BLOCK = 'X'.
        IF SY-SUBRC = 0.
          IF P_CHECK EQ 'X'.
            GV_CHECK = 'X'.
            PERFORM F_UPDATE_ERROR.
            GV_TAXT = 'SERIAL NO. WAS BLOCKED STOCK : PLEASE, CHECK DATA'.
            PERFORM F_SEND_MAIL USING GV_TAXT
                                      'B'.
          ELSE.
            "MESSAGE I001 WITH 'INCORRECT SERIAL NO. PLEASE, CHECK DATA'.
            WRITE: / 'SERIAL NO. WAS BLOCKED STOCK : PLEASE, CHECK DATA'.
            SKIP 1.
            WRITE: /2 'ITEM',
                   10 'MODEL NO',
                   30 'SERIAL NO'.
            WRITE: SY-ULINE.
            LOOP AT IN_CHK WHERE BLOCK = 'X'.
              WRITE: /2 IN_CHK-ITEM,
                     10 IN_CHK-MATNR,
                     30 IN_CHK-SERIAL.
            ENDLOOP.
            MESSAGE S000(38) WITH 'SERIAL NO. WAS BLOCKED STOCK : PLEASE, CHECK DATA !' DISPLAY LIKE 'E'.
          ENDIF.
        ELSE.

          IF IN_CHK[] IS NOT INITIAL.
            IF P_CHECK EQ 'X'.
              IF GV_CHECK = 'X'.
                PERFORM F_UPDATE_ERROR.
              ELSE.
                PERFORM PROCESS_DATA.
              ENDIF.
            ELSE.
              PERFORM PROCESS_DATA.
            ENDIF.
          ELSE.
            IF P_CHECK EQ 'X'.
              PERFORM F_UPDATE_ERROR.
              GV_TAXT = 'NO DATA FOR UPLOADING'.
              PERFORM F_SEND_MAIL USING GV_TAXT
                                        SPACE.
            ELSE.
              MESSAGE S000(38) WITH 'NO DATA FOR UPLOADING'.
            ENDIF.

          ENDIF.
*       ISS END INS - 12135
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&      FORM  GET_PATH_NAME
*&---------------------------------------------------------------------*
FORM GET_PATH_NAME  CHANGING PATH.
  DATA: L_LENGTH TYPE I.
  DATA: L_MASK(20) TYPE C.

* S = SAVE, O = OPEN
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      MASK             = ',*.TXT,*.TXT.'
      MODE             = 'O'
    IMPORTING
      FILENAME         = PATH
    EXCEPTIONS
      INV_WINSYS       = 01
      NO_BATCH         = 02
      SELECTION_CANCEL = 03
      SELECTION_ERROR  = 04.

ENDFORM.                    " GET_PATH_NAME
*&---------------------------------------------------------------------*
*&      FORM  UPLOAD_DATA
*&---------------------------------------------------------------------*
FORM UPLOAD_DATA .
  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
      FILENAME        = P_FILE
      FILETYPE        = 'DAT'
    TABLES
      DATA_TAB        = I_TAB
    EXCEPTIONS
      FILE_OPEN_ERROR = 1
      FILE_READ_ERROR = 2.

ENDFORM.                    " UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      FORM  PROCESS_DATA
*&---------------------------------------------------------------------*
FORM PROCESS_DATA .
  DATA: LT_SERIAL  LIKE BAPIDLVITMSERNO OCCURS 0 WITH HEADER LINE,
        LT_RET     LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.

  IF NOT P_INB IS INITIAL.
    DATA: L_IDOH      LIKE BAPIIBDLVHDRCHG,
          L_IDOHC     LIKE BAPIIBDLVHDRCTRLCHG,
          L_IDELIVERY LIKE BAPIIBDLVHDRCHG-DELIV_NUMB.

    LOOP AT IN_CHK.
      IF SY-TABIX = '1'.
        L_IDOH-DELIV_NUMB  = IN_CHK-DELIVERY.
        L_IDOHC-DELIV_NUMB = IN_CHK-DELIVERY.
        L_IDELIVERY        = IN_CHK-DELIVERY.
      ENDIF.
*
      LT_SERIAL-DELIV_NUMB = IN_CHK-DELIVERY.
      LT_SERIAL-ITM_NUMBER = IN_CHK-ITEM.
      LT_SERIAL-SERIALNO   = IN_CHK-SERIAL.
      APPEND LT_SERIAL.
      CLEAR LT_SERIAL.
    ENDLOOP.

    CALL FUNCTION 'BAPI_INB_DELIVERY_CHANGE'
      EXPORTING
        HEADER_DATA    = L_IDOH
        HEADER_CONTROL = L_IDOHC
        DELIVERY       = L_IDELIVERY
      TABLES
        ITEM_SERIAL_NO = LT_SERIAL
        RETURN         = LT_RET.

    READ TABLE LT_RET WITH KEY TYPE = 'E'.
    IF SY-SUBRC <> 0.
*     NO ERROR
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      WRITE: 'UPLOAD COMPLETE'.
    ELSE.
      WRITE: 'UPLOAD NOT COMPLETE'.
    ENDIF.
  ELSE.
    DATA: L_ODOH      LIKE BAPIOBDLVHDRCHG,
          L_ODOHC     LIKE BAPIOBDLVHDRCTRLCHG,
          L_ODELIVERY LIKE BAPIOBDLVHDRCHG-DELIV_NUMB.

    LOOP AT IN_CHK.
      IF SY-TABIX = '1'.
        L_ODOH-DELIV_NUMB  = IN_CHK-DELIVERY.
        L_ODOHC-DELIV_NUMB = IN_CHK-DELIVERY.
        L_ODELIVERY        = IN_CHK-DELIVERY.
      ENDIF.
*
      LT_SERIAL-DELIV_NUMB = IN_CHK-DELIVERY.
      LT_SERIAL-ITM_NUMBER = IN_CHK-ITEM.
      LT_SERIAL-SERIALNO   = IN_CHK-SERIAL.
      APPEND LT_SERIAL.
      CLEAR LT_SERIAL.
    ENDLOOP.

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
      EXPORTING
        HEADER_DATA    = L_ODOH
        HEADER_CONTROL = L_ODOHC
        DELIVERY       = L_ODELIVERY
      TABLES
        ITEM_SERIAL_NO = LT_SERIAL
        RETURN         = LT_RET.

    READ TABLE LT_RET WITH KEY TYPE = 'E'.
    IF SY-SUBRC <> 0.
*     NO ERROR
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      SELECT LIEF_NR POSNR INTO TABLE GT_SER01 FROM SER01
        FOR ALL ENTRIES IN LT_SERIAL
        WHERE LIEF_NR EQ LT_SERIAL-DELIV_NUMB AND
              POSNR EQ LT_SERIAL-ITM_NUMBER.
      IF GT_SER01[] IS NOT INITIAL.
        IF P_CHECK EQ 'X'.
          UPDATE ZSDSSDT001 SET SERNF = 'X'
                     WHERE DONUM EQ L_ODOH-DELIV_NUMB.
        ELSE.
          WRITE: 'UPLOAD COMPLETE'.
        ENDIF.
      ELSE.
        IF P_CHECK EQ 'X'.
          UPDATE ZSDSSDT001 SET SERNF = 'E'
                     WHERE DONUM EQ L_ODOH-DELIV_NUMB.
        ELSE.
          WRITE: 'UPLOAD NOT COMPLETE, PLEASE CHECK TEXT FILES!'.
        ENDIF.
      ENDIF.
    ELSE.
      IF P_CHECK EQ 'X'.
        UPDATE ZSDSSDT001 SET SERNF = 'E'
                     WHERE DONUM EQ L_ODOH-DELIV_NUMB.
      ELSE.
        WRITE: 'UPLOAD NOT COMPLETE'.
      ENDIF.
    ENDIF.

*   POST GI
    IF P_GI IS NOT INITIAL.
      CLEAR: VBUK.
      SELECT SINGLE WBSTK
        INTO VBUK-WBSTK
        FROM VBUK
       WHERE VBELN = L_ODELIVERY
         AND WBSTK = 'C'.

      IF SY-SUBRC = 0.
*        MESSAGE I001 WITH 'CANNOT DUPLICATE POST GOODS ISSUE'.
      ELSE.
*       ASSIGN BDC DATA
        CLEAR: BDCDATA[], BDCDATA.
        PERFORM BDC_DYNPRO   USING 'SAPMV50A'      '4004'.
        PERFORM BDC_FIELD    USING 'BDC_CURSOR'    'LIKP-VBELN'.
        PERFORM BDC_FIELD    USING 'LIKP-VBELN'    L_ODELIVERY.
        PERFORM BDC_FIELD    USING 'BDC_OKCODE'    '/00'.
        PERFORM BDC_DYNPRO   USING 'SAPMV50A'      '1000'.
        PERFORM BDC_FIELD    USING 'BDC_OKCODE'    '=WABU_T'.

*       CHANGE OUTBOUND DELIVERY > POST GOODS ISSUE
*       MODE
*       'A' DISPLAY SCREEN
*       'E' DISPLAY SCREEN ONLY IF AN ERROR OCCURS
*       'N' NO DISPLAY

        CALL TRANSACTION 'VL02N' USING BDCDATA MODE 'N'.
        IF SY-SUBRC = 0.
*         NO ERROR
          CONCATENATE 'OUTBOUND DELIVERY FG' L_ODELIVERY 'HAS BEEN SAVED'
                 INTO MSG SEPARATED BY ' '.
*          MESSAGE S001 WITH MSG.
        ELSE.
*          MESSAGE I001 WITH 'OUTBOUND DELIVERY HAVE ERROR. PLEASE CHECK'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      FORM  CHECK_DATA
*&---------------------------------------------------------------------*
FORM CHECK_DATA .
* INBOUND [DIT -> EX_ID,
*          SONY -> เลขที่ INBOUND เหมือนกัน แต่ใช้อีกโปรแกรมนึง]

* OUTBOUND [SONY -> เลขที่ OUTBOUND เหมือนกัน]
  CLEAR: IN_TAB[], IN_PICK[].

  IF NOT P_INB IS INITIAL.
    LOOP AT I_TAB.
      SPLIT I_TAB-TXT AT ',' INTO IN_TAB-EX_ID
                                  IN_TAB-VBELN
                                  IN_TAB-ITEM
                                  IN_TAB-SERIAL
                                  IN_TAB-MATNR.
      APPEND IN_TAB.
    ENDLOOP.
  ELSE.
    LOOP AT I_TAB.
      IN_TAB-EX_ID    = I_TAB-TXT(10).
      IN_TAB-ITEM     = I_TAB-TXT+15(4).
      IN_TAB-SERIAL   = I_TAB-TXT+19(18).
      APPEND IN_TAB.
    ENDLOOP.
  ENDIF.

  CLEAR: IN_CHK[], IN_PICK[].

* DELIVERY TYPE <EL = INBOUND, LF = OUTBOUND, NL = REPLENISHMENT DLV.>
  IF NOT P_INB IS INITIAL.
    LOOP AT IN_TAB.
      CLEAR: LIKP, LIPS.

*      SELECT SINGLE *
*        FROM LIKP
*       WHERE LIFEX = IN_TAB-EX_ID.
      CLEAR: I_CHK[].
      SELECT A~VBELN A~LFART B~POSNR
        INTO TABLE I_CHK
        FROM LIKP AS A INNER JOIN LIPS AS B
          ON A~VBELN = B~VBELN
       WHERE A~LIFEX = IN_TAB-EX_ID
         AND B~VGPOS = IN_TAB-ITEM.
*
      IF SY-SUBRC = 0.
        READ TABLE I_CHK INDEX 1.
        IN_CHK-DELIVERY = I_CHK-VBELN.
        IN_CHK-ITEM     = I_CHK-POSNR.
        IN_CHK-SERIAL   = IN_TAB-SERIAL.
        IN_CHK-LFART    = I_CHK-LFART.
        IN_CHK-MATNR    = IN_TAB-MATNR.
        APPEND IN_CHK.
      ENDIF.
    ENDLOOP.
* OUTBOUND DELIVERY
  ELSE.
    LOOP AT IN_TAB.
      CLEAR: LIKP.

      IF P_GI IS NOT INITIAL.
        SELECT SINGLE *
          FROM LIKP
         WHERE VBELN = IN_TAB-EX_ID
           AND LFART = 'NL'. "REPLENISHMENT DLV.
      ELSE.
        SELECT SINGLE *
          FROM LIKP
         WHERE VBELN = IN_TAB-EX_ID.
      ENDIF.
*
      IF SY-SUBRC = 0.
        IN_CHK-DELIVERY = LIKP-VBELN.
        IN_CHK-ITEM     = IN_TAB-ITEM.
        IN_CHK-SERIAL   = IN_TAB-SERIAL.
        IN_CHK-LFART    = LIKP-LFART.
        APPEND IN_CHK.
      ENDIF.
    ENDLOOP.
  ENDIF.
*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
  PERFORM F_UPDATE_STORAGE_FOR_PROJ_DDD.
*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*

  IF NOT P_OUTB IS INITIAL.
    LOOP AT IN_CHK.
      CLEAR: LIPS.

      SELECT SINGLE *
        FROM LIPS
       WHERE VBELN = IN_CHK-DELIVERY
         AND POSNR = IN_CHK-ITEM.

      IF SY-SUBRC = 0.
        IN_CHK-MATNR = LIPS-MATNR.
        MODIFY IN_CHK.
      ENDIF.
    ENDLOOP.


    DATA: LV_DELIV     TYPE VBELN_VL,
          LV_MATERIAL  LIKE BAPIEKPO-MATERIAL,
          LV_SERIAL    LIKE EQUI-SERNR,
          LV_QUEST     TYPE STRING,
          LV_ANSWER    TYPE C,
          LV_FLAG      TYPE CHAR1,
          LV_INVALID   TYPE CHAR1,
          LV_INPICK    TYPE CHAR1,
          LV_INBLOCK   TYPE CHAR1.

    CLEAR: GV_FLAG.
    IN_PICK[] = IN_CHK[].
    LOOP AT IN_CHK.
      CLEAR: LV_DELIV, LV_MATERIAL, LV_SERIAL, LV_FLAG.
      LV_DELIV    = IN_CHK-DELIVERY.
      LV_MATERIAL = IN_CHK-MATNR.
      LV_SERIAL   = IN_CHK-SERIAL.

*      CALL FUNCTION 'ZFM_MM_CHECK_MAT_SERIAL'
*        EXPORTING
*          MATERIAL        = LV_MATERIAL
*          SERIAL          = LV_SERIAL
*        IMPORTING
*          FLAG            = LV_FLAG.

*      CALL FUNCTION 'ZFM_SD_CHECK_SERIAL'
*        EXPORTING
*          DELIVERY        = LV_DELIV
*          MATERIAL        = LV_MATERIAL
*          SERIAL          = LV_SERIAL
*        IMPORTING
*          FLAG            = LV_FLAG.

*     ISS START INS - 12135
*     CHECK STATUS AND SLOC
      CLEAR: LV_INVALID, LV_INPICK, LV_INBLOCK.
      PERFORM VALIDATE_SERIAL USING IN_CHK-DELIVERY
                                    IN_CHK-ITEM
                                    IN_CHK-MATNR
                                    IN_CHK-SERIAL
                           CHANGING LV_INVALID
                                    LV_INPICK
                                    LV_INBLOCK.
      IF LV_INVALID = 'X'.
        IN_CHK-FLAG = 'X'.
        MODIFY IN_CHK.
      ELSEIF LV_INPICK IS NOT INITIAL.
        IN_CHK-PICK = 'X'.
        MODIFY IN_CHK.
      ELSEIF LV_INBLOCK IS NOT INITIAL.
        IN_CHK-BLOCK = 'X'.
        MODIFY IN_CHK.
      ENDIF.
*     ISS END INS - 12135

      SELECT SINGLE WERK LAGER
        INTO (EQUI-WERK, EQUI-LAGER)
        FROM EQUI
       WHERE MATNR = LV_MATERIAL
         AND SERNR = LV_SERIAL.

      IF SY-SUBRC <> 0. "IF EQUI-WERK IS INITIAL AND EQUI-LAGER IS INITIAL.
*       IN_CHK-FLAG = 'X'.
        MODIFY IN_CHK.
      ENDIF.
    ENDLOOP.

*   ISS START INS - 12135
*   CHECK DUPLICATED UPLOADING
    DATA : LT_DO   LIKE TABLE OF IN_CHK WITH HEADER LINE,
           LV_ITEM TYPE CHAR50.

    READ TABLE IN_CHK WITH KEY FLAG = 'X'.
    IF SY-SUBRC <> 0.

      LT_DO[] = IN_CHK[].

      SORT LT_DO BY DELIVERY ITEM.
      DELETE ADJACENT DUPLICATES FROM LT_DO COMPARING DELIVERY ITEM.

      CLEAR LV_INVALID.
      LOOP AT LT_DO.

        PERFORM CHECK_DUP_UPLOAD USING LT_DO-DELIVERY
                                       LT_DO-ITEM
                              CHANGING LV_INVALID.
        IF  LV_INVALID = 'X'.

          IF LV_ITEM IS INITIAL.
            LV_ITEM = LT_DO-ITEM.
          ELSE.
            CONCATENATE LV_ITEM LT_DO-ITEM
                   INTO LV_ITEM
            SEPARATED BY ','.
          ENDIF.
*          EXIT.

        ENDIF.

      ENDLOOP.

      IF LV_INVALID = 'X'.

        CONCATENATE 'DELIVERY'
                    LT_DO-DELIVERY
*                    'ITEM'
*                    LV_ITEM
                    'ALREADY HAS SERIAL NUMBER.'
                    'DO YOU CONFIRM TO CONTINUE?'
               INTO LV_QUEST
        SEPARATED BY SPACE.

        CLEAR LV_ANSWER.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            TITLEBAR              = 'POPUP TO CONFIRM'
            TEXT_QUESTION         = LV_QUEST
            DISPLAY_CANCEL_BUTTON = SPACE
          IMPORTING
            ANSWER                = LV_ANSWER
          EXCEPTIONS
            TEXT_NOT_FOUND        = 1
            OTHERS                = 2.
        IF SY-SUBRC = 0.

          IF LV_ANSWER = '2'.       "NO

            LEAVE TO CURRENT TRANSACTION.

*          ELSEIF LV_ANSWER = '1'.   "YES
*
*            DELETE IN_CHK WHERE DELIVERY = LT_DO-DELIVERY
*                            AND ITEM     = LT_DO-ITEM.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.
*   ISS END INS - 12135

  ENDIF.

ENDFORM.                    " CHECK_DATA
*&---------------------------------------------------------------------*
*&      FORM  BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.                    " BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      FORM  BDC_FIELD
*&---------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.                    " BDC_FIELD
*&---------------------------------------------------------------------*
*&      FORM  MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM MODIFY_SCREEN .
  LOOP AT SCREEN.
    IF P_INB = 'X'.
      IF SCREEN-GROUP1 = 'SAA'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      FORM  VALIDATE_SERIAL
*&---------------------------------------------------------------------*
FORM VALIDATE_SERIAL USING  PLV_DELIVERY
                            PLV_ITEM
                            PLV_MATNR
                            PLV_SERIAL
                   CHANGING CLV_INVALID
                            CLV_INPICK
                            CLV_INBLOCK.

  DATA : LV_STATUS  TYPE BSVX-STTXT.

  DATA : LV_SERNR   TYPE EQUI-SERNR.

  DATA : LWA_LIPS         TYPE LIPS,
         LWA_MARC         TYPE MARC,
         LWA_EQUI         TYPE EQUI,
         LWA_SER01        TYPE SER01,
         LWA_OBJK         TYPE OBJK,
         LWA_V_EQUI_EQBS  TYPE V_EQUI_EQBS_SML,
         LWA_COUNT        TYPE I.

  CALL FUNCTION 'CONVERSION_EXIT_GERNR_INPUT'
    EXPORTING
      INPUT  = PLV_SERIAL
    IMPORTING
      OUTPUT = LV_SERNR.

  SELECT SINGLE *
    FROM LIPS
    INTO LWA_LIPS
   WHERE VBELN = PLV_DELIVERY
     AND POSNR = PLV_ITEM.

  CLEAR LWA_COUNT.
  LOOP AT IN_PICK WHERE DELIVERY = LWA_LIPS-VBELN AND
                        ITEM = LWA_LIPS-POSNR.

    ADD 1 TO LWA_COUNT.
  ENDLOOP.

  IF LWA_LIPS-LFIMG NE LWA_COUNT.
    CLV_INPICK = 'X'.
  ENDIF.

* CHECK MAT MASTER HAS SERIAL PROFILE ?
  SELECT SINGLE *
    FROM MARC
    INTO LWA_MARC
   WHERE WERKS = LWA_LIPS-WERKS
     AND MATNR = LWA_LIPS-MATNR.

* SERIAL PROFILE IS NOT INITAIL
  CHECK LWA_MARC-SERNP IS NOT INITIAL.

* CHECK SERIAL EXIST IN SYSTEM ?
  SELECT SINGLE *
    FROM EQUI
    INTO LWA_EQUI
   WHERE MATNR = LWA_LIPS-MATNR
     AND SERNR = LV_SERNR.

  IF SY-SUBRC = 0.

*   CHECK NEW SERIAL IS THE SAME EXISTING SERIAL IN DO ITEM ?
    SELECT SINGLE *
      FROM SER01
      INTO LWA_SER01
     WHERE LIEF_NR = LWA_LIPS-VBELN
       AND POSNR   = LWA_LIPS-POSNR.

    IF SY-SUBRC = 0.

      SELECT SINGLE *
        FROM OBJK
        INTO LWA_OBJK
       WHERE OBKNR = LWA_SER01-OBKNR
         AND MATNR = LWA_LIPS-MATNR
         AND SERNR = LV_SERNR.

*     NEW SERIAL
      IF SY-SUBRC <> 0.

*       CHECK SERIAL STATUS
        CLEAR LV_STATUS.
        CALL FUNCTION 'STATUS_TEXT_EDIT'
          EXPORTING
            CLIENT           = SY-MANDT
            FLG_USER_STAT    = 'X'
            OBJNR            = LWA_EQUI-OBJNR
            SPRAS            = 'E'
          IMPORTING
            LINE             = LV_STATUS
          EXCEPTIONS
            OBJECT_NOT_FOUND = 1
            OTHERS           = 2.

        IF SY-SUBRC = 0.

          CONDENSE LV_STATUS.
          IF LV_STATUS+0(4) <> 'ESTO'.

            CLV_INVALID = 'X'.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

*   CHECK SERIAL NUMBER WITH SLOC
    CLEAR LWA_V_EQUI_EQBS.
    SELECT SINGLE * INTO LWA_V_EQUI_EQBS FROM V_EQUI_EQBS_SML
      WHERE MATNR   = LWA_LIPS-MATNR
        AND SERNR   = LV_SERNR
        AND B_WERK  = LWA_LIPS-WERKS
        AND B_LAGER = LWA_LIPS-LGORT.

    IF SY-SUBRC <> 0.

      CLV_INVALID = 'X'.

    ENDIF.

    SELECT SINGLE * INTO LWA_V_EQUI_EQBS FROM V_EQUI_EQBS_SML
      WHERE MATNR   = LWA_LIPS-MATNR
        AND SERNR   = LV_SERNR
        AND B_WERK  = LWA_LIPS-WERKS
        AND B_LAGER = LWA_LIPS-LGORT
        AND LBBSA   = '07'.
    IF SY-SUBRC EQ 0.
      CLV_INBLOCK = 'X'.
    ENDIF.

  ELSE.

    CLV_INVALID = 'X'.

  ENDIF.

ENDFORM.                    " VALIDATE_SERIAL
*&---------------------------------------------------------------------*
*&      FORM  CHECK_DUP_UPLOAD
*&---------------------------------------------------------------------*
FORM CHECK_DUP_UPLOAD  USING    PLV_CHK_DELIVERY
                                PLV_CHK_ITEM
                       CHANGING CLV_INVALID.

  DATA : LWA_SER01 TYPE SER01.

  SELECT SINGLE *
    FROM SER01
    INTO LWA_SER01
   WHERE LIEF_NR = PLV_CHK_DELIVERY
     AND POSNR   = PLV_CHK_ITEM.

  IF SY-SUBRC = 0.

    CLV_INVALID = 'X'.

  ENDIF.

ENDFORM.                    " CHECK_DUP_UPLOAD
*&---------------------------------------------------------------------*
*&      FORM  F_GET_SELECTION
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM F_GET_SELECTION .
  LOOP AT SCREEN.
    IF SCREEN-NAME       = 'P_CHECK'.
      SCREEN-INTENSIFIED = '1'.
      SCREEN-ACTIVE      = '0'.
      SCREEN-DISPLAY_3D  = '1'.
      MODIFY SCREEN.
    ENDIF.

    IF SCREEN-GROUP1     = 'MAL'.
      SCREEN-INTENSIFIED = '1'.
      SCREEN-ACTIVE      = '0'.
      SCREEN-DISPLAY_3D  = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " F_GET_SELECTION
*&---------------------------------------------------------------------*
*&      FORM  F_IMPORT_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM F_IMPORT_DATA .
  IMPORT LT_SERIAL_TMP TO I_TAB[] FROM MEMORY ID 'SER'. " FROM PROGRAM ZP_MM_RETRIEVE_STATUS_DO.
  FREE MEMORY ID 'SER'.
ENDFORM.                    " F_IMPORT_DATA
*&---------------------------------------------------------------------*
*&      FORM  F_UPDATE_ERROR
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM F_UPDATE_ERROR .
  READ TABLE IN_TAB INTO IN_TAB INDEX 1.
  IF SY-SUBRC = 0.
    UPDATE ZSDSSDT001 SET SERNF = 'E'
               WHERE DONUM EQ IN_TAB-EX_ID.
  ENDIF.

ENDFORM.                    " F_UPDATE_ERROR
*&---------------------------------------------------------------------*
*&      FORM  F_SEND_MAIL
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->P_GV_TAXT  TEXT
*----------------------------------------------------------------------*
FORM F_SEND_MAIL  USING LV_TEXT
                        LV_CHECK.
  DATA LV_AMOUNT TYPE C LENGTH 255.

  DATA LV_SENDER_ADDRESS TYPE SOEXTRECI1-RECEIVER.

  LV_SENDER_ADDRESS     = SY-UNAME.
  GF_DOC_CHNG-OBJ_NAME  = 'ERROR UPLOAD SERIAL FROM SONY'.
  GF_DOC_CHNG-OBJ_DESCR = 'ERROR UPLOAD SERIAL FROM SONY'.
*SET THE BODY BACKGROUND COLOUR
  GF_OBJTXT-LINE = '<BODY BGCOLOR = "#E6E6FA">'.
*- APPEND
  APPEND GF_OBJTXT TO GT_OBJTXT.
*- CLEAR
  CLEAR GF_OBJTXT.
*SET FONT COLOR AND ITS TYPE
  CONCATENATE '<FONT COLOR = "#191970" FACE="GARAMOND">' '<B>' INTO GF_OBJTXT-LINE.
*- APPEND
  APPEND GF_OBJTXT TO GT_OBJTXT.
*- CLEAR
  CLEAR GF_OBJTXT.
*PRIPARE MAIL BODY
  CONCATENATE '<P>' 'DEAR SIR/MADAM,' '</P>'
              INTO GF_OBJTXT-LINE.
*- APPEND
  APPEND GF_OBJTXT TO GT_OBJTXT.
*- CLEAR
  CLEAR GF_OBJTXT.
  GF_OBJTXT-LINE = SPACE.
*- APPEND
  APPEND GF_OBJTXT TO GT_OBJTXT.
*- CLEAR
  CLEAR GF_OBJTXT.
  CONCATENATE '<P>'
              'PROGRAM CANNOT UPLOAD DATA FORM TEXT FILE. KINDLY CHECK BELOW DETAIL.'
              '</P>'
                 INTO GF_OBJTXT-LINE SEPARATED BY SPACE.
  APPEND GF_OBJTXT TO GT_OBJTXT.
  CLEAR  GF_OBJTXT.
*--------------------------------------------------------------------*
*  CONCATENATE '<P>' ' 1. ลูกค้ารายใดที่ยังมีความประสงค์จะดำเนินการซื้อสินค้าต่อเนื่องกับทาง SDS แต่ BG'
*                       'หมดอายุหรือใกล้จะหมดอายุ ขอให้ฝ่ายขายแจ้งทางลูกค้าให้ต่ออายุ BG ให้แล้วเสร็จก่อนที่จะออก SALES ORDER โดยแบ่ง ดังนี้'
*                 '</P>'
*                 INTO GF_OBJTXT-LINE SEPARATED BY SPACE.
*  APPEND GF_OBJTXT TO GT_OBJTXT.
*  CLEAR  GF_OBJTXT.
*
*  CONCATENATE '<P>' '&NBSP;' '&NBSP;' '1.1 BG ค้ำวงเงิน PERMANENT อายุของ BG ต้องไม่น้อยกว่า 1 ปี หรือไม่ต้องระบุวันหมดอายุ BG (เป็น BG ปลายเปิด)'
*                 '</P>'
*                 INTO GF_OBJTXT-LINE SEPARATED BY SPACE.
*  APPEND GF_OBJTXT TO GT_OBJTXT.
*  CLEAR GF_OBJTXT.
*
*  CONCATENATE '<P>' '&NBSP;' '&NBSP;' '1.2 BG ค้ำวงเงิน TEMPORARY(PROJECT) อายุของ BG'
*                      ' ต้องครอบคลุมถึงวันที่ชำระหนี้ของงวดสุดท้ายของ PROJECT'
*                 '</P>'
*                 INTO GF_OBJTXT-LINE SEPARATED BY SPACE.
*  APPEND GF_OBJTXT TO GT_OBJTXT.
*  CLEAR GF_OBJTXT.
*
*  CONCATENATE '<P>' '2. เมื่อโครงการสิ้นสุดให้ฝ่ายขายดำเนินการขอคืน BG โดยเร็ว'
*                 '</P>'
*                 INTO GF_OBJTXT-LINE SEPARATED BY SPACE.
*  APPEND GF_OBJTXT TO GT_OBJTXT.
*  CLEAR GF_OBJTXT.
*--------------------------------------------------------------------*
  GF_OBJTXT-LINE = '<CENTER>'.
  APPEND GF_OBJTXT TO GT_OBJTXT.
  CLEAR  GF_OBJTXT.


*  PRIPARE EMPLOYEE DATA IN TABLE FORMAT TO DISPAY IN MAIL BODY
  GF_OBJTXT-LINE = '<TABLE  WIDTH= "100%" BORDER="1">'.
  APPEND GF_OBJTXT TO GT_OBJTXT.
*--------------------------------------------------------------------*
* HEADER TABLE
*--------------------------------------------------------------------*
  CLEAR  GF_OBJTXT.
  CONCATENATE '<TR ><TD ALIGN = "LEFT" BGCOLOR = "#708090">'
              '<FONT COLOR = "BLUE"><B>DO NO.</B> </FONT>'
                '</TD>'  INTO GF_OBJTXT-LINE.
  APPEND GF_OBJTXT TO GT_OBJTXT.
  CLEAR  GF_OBJTXT.


  CONCATENATE '<TD ALIGN = "LEFT" BGCOLOR = "#708090">'
              '<FONT COLOR = "BLUE"><B>ITEM</B> </FONT>'
                '</TD>'  INTO GF_OBJTXT-LINE.
  APPEND GF_OBJTXT TO GT_OBJTXT.
  CLEAR  GF_OBJTXT.

  CONCATENATE '<TD ALIGN = "LEFT" BGCOLOR = "#708090">'
              '<FONT COLOR = "BLUE"><B>MODEL</B> </FONT>'
                '</TD>'  INTO GF_OBJTXT-LINE.
  APPEND GF_OBJTXT TO GT_OBJTXT.
  CLEAR  GF_OBJTXT.

  CONCATENATE '<TD ALIGN = "LEFT" BGCOLOR = "#708090">'
              '<FONT COLOR = "BLUE"><B>SERIAL</B> </FONT>'
                '</TD>'  INTO GF_OBJTXT-LINE.
  APPEND GF_OBJTXT TO GT_OBJTXT.
  CLEAR  GF_OBJTXT.

  CONCATENATE '<TD ALIGN = "LEFT"  BGCOLOR = "#708090">'
                '<FONT COLOR = "BLUE"> <B>MESSAGE</B> </FONT>'
                '</TD></TR>'  INTO GF_OBJTXT-LINE.
  APPEND GF_OBJTXT TO GT_OBJTXT.
  CLEAR  GF_OBJTXT.

*  CONCATENATE '<TD ALIGN = "LEFT"  BGCOLOR = "#708090">'
*                   ' <FONT COLOR = "BLUE"><B>BG START DATE</B> </FONT>'
*                   '</TD>'  INTO GF_OBJTXT-LINE.
*  APPEND GF_OBJTXT TO GT_OBJTXT.
*  CLEAR  GF_OBJTXT.
*
*  CONCATENATE '<TD ALIGN = "LEFT"  BGCOLOR = "#708090">'
*                   '<FONT COLOR = "BLUE"><B>BG EXPIRY DATE</B> </FONT>'
*                   '</TD>'  INTO GF_OBJTXT-LINE.
*  APPEND GF_OBJTXT TO GT_OBJTXT.
*  CLEAR  GF_OBJTXT.
*
*  CONCATENATE '<TD ALIGN = "LEFT"  BGCOLOR = "#708090">'
*                '<FONT COLOR = "BLUE"><B>BANK NAME</B> </FONT>'
*                '</TD>'  INTO GF_OBJTXT-LINE.
*  APPEND GF_OBJTXT TO GT_OBJTXT.
*  CLEAR  GF_OBJTXT.
*
*  CONCATENATE '<TD ALIGN = "LEFT"  BGCOLOR = "#708090">'
*                   ' <FONT COLOR = "BLUE"><B>CATEGORY</B> </FONT>'
*                   '</TD>'  INTO GF_OBJTXT-LINE.
*  APPEND GF_OBJTXT TO GT_OBJTXT.
*  CLEAR  GF_OBJTXT.
*
*  CONCATENATE '<TD ALIGN = "LEFT"  BGCOLOR = "#708090">'
*                   ' <FONT COLOR = "BLUE"><B>PROJECT</B> </FONT>'
*                   '</TD>'  INTO GF_OBJTXT-LINE.
*  APPEND GF_OBJTXT TO GT_OBJTXT.
*  CLEAR  GF_OBJTXT.
*
*  CONCATENATE '<TD ALIGN = "LEFT"  BGCOLOR = "#708090">'
*                   '<FONT COLOR = "BLUE"><B>BG AMOUNT</B> </FONT>'
*                   '</TD></TR>'  INTO GF_OBJTXT-LINE.
*  APPEND GF_OBJTXT TO GT_OBJTXT.
*  CLEAR  GF_OBJTXT.
*--------------------------------------------------------------------*
* DETAIL TABLE
*--------------------------------------------------------------------*
  LOOP AT IN_CHK .
    IF     LV_CHECK EQ 'F'.
      IF IN_CHK-FLAG NE 'X'.
        CONTINUE.
      ENDIF.
    ELSEIF LV_CHECK EQ 'P'.
      IF IN_CHK-PICK NE 'X'.
        CONTINUE.
      ENDIF.
    ELSEIF LV_CHECK EQ 'B'.
      IF IN_CHK-BLOCK NE 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.

    CONCATENATE '<TR><TD ALIGN = "LEFT">'
                '<FONT COLOR = "BLUE">' IN_CHK-DELIVERY '</FONT>'
                '</TD>'  INTO GF_OBJTXT-LINE.
    APPEND GF_OBJTXT TO GT_OBJTXT.
    CLEAR  GF_OBJTXT.

    CONCATENATE '<TD ALIGN = "LEFT">'
                '<FONT COLOR = "BLUE">' IN_CHK-ITEM '</FONT>'
                '</TD>'  INTO GF_OBJTXT-LINE.
    APPEND GF_OBJTXT TO GT_OBJTXT.
    CLEAR  GF_OBJTXT.

    CONCATENATE '<TD ALIGN = "LEFT">'
                '<FONT COLOR = "BLUE">' IN_CHK-MATNR '</FONT>'
                '</TD>'  INTO GF_OBJTXT-LINE.
    APPEND GF_OBJTXT TO GT_OBJTXT.
    CLEAR  GF_OBJTXT.

    CONCATENATE '<TD ALIGN = "LEFT">'
                '<FONT COLOR = "BLUE">' IN_CHK-SERIAL '</FONT>'
                '</TD>'  INTO GF_OBJTXT-LINE.
    APPEND GF_OBJTXT TO GT_OBJTXT.
    CLEAR  GF_OBJTXT.

    CONCATENATE '<TD ALIGN = "LEFT">'
                '<FONT COLOR = "BLUE">' LV_TEXT '</FONT>'
                 '</TD>'  INTO GF_OBJTXT-LINE.
    APPEND GF_OBJTXT TO GT_OBJTXT.
    CLEAR  GF_OBJTXT.

*
*    CONCATENATE '<TD ALIGN = "LEFT">'
*                '<FONT COLOR = "BLUE">' L_BEGDA '</FONT>'
*                 '</TD>'  INTO GF_OBJTXT-LINE.
*    APPEND GF_OBJTXT TO GT_OBJTXT.
*    CLEAR  GF_OBJTXT.
*
*    CONCATENATE '<TD ALIGN = "LEFT">'
*                '<FONT COLOR = "BLUE">' L_ENDDA '</FONT>'
*                  '</TD>'  INTO GF_OBJTXT-LINE.
*    APPEND GF_OBJTXT TO GT_OBJTXT.
*
*    CONCATENATE '<TD ALIGN = "LEFT">'
*                '<FONT COLOR = "BLUE">' GS_RESULT-BANKN '</FONT>'
*                 '</TD>'  INTO GF_OBJTXT-LINE.
*    APPEND GF_OBJTXT TO GT_OBJTXT.
*    CLEAR  GF_OBJTXT.
*
*    CONCATENATE '<TD ALIGN = "LEFT">'
*                '<FONT COLOR = "BLUE">' GS_RESULT-CATAGORY '</FONT>'
*                  '</TD>'  INTO GF_OBJTXT-LINE.
*    APPEND GF_OBJTXT TO GT_OBJTXT.
*    CLEAR  GF_OBJTXT.
*
*    CONCATENATE '<TD ALIGN = "LEFT">'
*                '<FONT COLOR = "BLUE">' GS_RESULT-PROJEC '</FONT>'
*                  '</TD>'  INTO GF_OBJTXT-LINE.
*    APPEND GF_OBJTXT TO GT_OBJTXT.
*    CLEAR  GF_OBJTXT.
*
*    WRITE GS_RESULT-NETWR TO LV_AMOUNT.
*    REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN LV_AMOUNT WITH ''.
*
*    CONCATENATE '<TD ALIGN = "LEFT">'
*                '<FONT COLOR = "BLUE">' LV_AMOUNT '</FONT>'
*                  '</TD>'  INTO GF_OBJTXT-LINE.
*    APPEND GF_OBJTXT TO GT_OBJTXT.
*    CLEAR  GF_OBJTXT.
  ENDLOOP.
*--------------------------------------------------------------------*
* FOOTER
*--------------------------------------------------------------------*
  GF_OBJTXT-LINE = '</TABLE>'.
  APPEND GF_OBJTXT TO GT_OBJTXT.
  CLEAR  GF_OBJTXT.
  GF_OBJTXT-LINE = '</CENTER>'.
  APPEND GF_OBJTXT TO GT_OBJTXT.
  CLEAR  GF_OBJTXT.

*  CONCATENATE '<P>' ' 1. ลูกค้ารายใดที่ยังมีความประสงค์จะดำเนินการซื้อสินค้าต่อเนื่องกับทาง SDS แต่ BG'
*                       'หมดอายุหรือใกล้จะหมดอายุ ขอให้ฝ่ายขายแจ้งทางลูกค้าให้ต่ออายุ BG ให้แล้วเสร็จก่อนที่จะออก SALES ORDER โดยแบ่ง ดังนี้'
*                 '</P>'
*                 INTO GF_OBJTXT-LINE SEPARATED BY SPACE.
*  APPEND GF_OBJTXT TO GT_OBJTXT.
*  CLEAR  GF_OBJTXT.
*
*  CONCATENATE '<P>' '&NBSP;' '&NBSP;' '1.1 BG ค้ำวงเงิน PERMANENT อายุของ BG ต้องไม่น้อยกว่า 1 ปี หรือไม่ต้องระบุวันหมดอายุ BG (เป็น BG ปลายเปิด)'
*                 '</P>'
*                 INTO GF_OBJTXT-LINE SEPARATED BY SPACE.
*  APPEND GF_OBJTXT TO GT_OBJTXT.
*  CLEAR GF_OBJTXT.
*
*  CONCATENATE '<P>' '&NBSP;' '&NBSP;' '1.2 BG ค้ำวงเงิน TEMPORARY(PROJECT) อายุของ BG'
*                      ' ต้องครอบคลุมถึงวันที่ชำระหนี้ของงวดสุดท้ายของ PROJECT'
*                 '</P>'
*                 INTO GF_OBJTXT-LINE SEPARATED BY SPACE.
*  APPEND GF_OBJTXT TO GT_OBJTXT.
*  CLEAR GF_OBJTXT.
*
*  CONCATENATE '<P>' '2. เมื่อโครงการสิ้นสุดให้ฝ่ายขายดำเนินการขอคืน BG โดยเร็ว'
*                 '</P>'
*                 INTO GF_OBJTXT-LINE SEPARATED BY SPACE.
*  APPEND GF_OBJTXT TO GT_OBJTXT.
*  CLEAR GF_OBJTXT.


  GF_OBJTXT-LINE =  '<BR>REGARDS,<BR />'.
  APPEND GF_OBJTXT TO GT_OBJTXT.
  CLEAR GF_OBJTXT.
  CONCATENATE LV_SENDER_ADDRESS '<BR />' INTO GF_OBJTXT-LINE.
*  GF_OBJTXT-LINE =   'VAMSI<BR />'.
  APPEND GF_OBJTXT TO GT_OBJTXT.
  CLEAR GF_OBJTXT.
  GF_OBJTXT-LINE = '<BR><BR><B><CENTER><I><FONT COLOR = "RED">THIS IS AN AUTO GENERATED EMAIL.'.
  APPEND GF_OBJTXT TO GT_OBJTXT.
  CLEAR GF_OBJTXT.
  GF_OBJTXT-LINE = '</FONT></BODY>'.
*- APPEND
  APPEND GF_OBJTXT TO GT_OBJTXT.
*- CLEAR
  CLEAR GF_OBJTXT.
*--------------------------------------------------------------------*
*END FOOTER
*--------------------------------------------------------------------*
  DESCRIBE TABLE GT_OBJTXT LINES G_MSG_LINES.
  READ TABLE GT_OBJTXT INTO GF_OBJTXT INDEX G_MSG_LINES.
  GF_DOC_CHNG-DOC_SIZE = ( G_MSG_LINES - 1 ) * 255 + STRLEN( GF_OBJTXT ).
*- CREATION OF THE ENTRY FOR THE COMPRESSED DOCUMENT
  GF_OBJPACK-TRANSF_BIN = ' '.
  GF_OBJPACK-HEAD_START = 1.
  GF_OBJPACK-HEAD_NUM   = 0.
  GF_OBJPACK-BODY_START = 1.
  GF_OBJPACK-BODY_NUM   = G_MSG_LINES.
  GF_OBJPACK-DOC_TYPE   = C_DOC_TYPE_HTM.
*- APPEND
  APPEND GF_OBJPACK TO GT_OBJPACK.
*- CLEAR
  CLEAR GF_OBJPACK.
*- CREATION OF THE DOCUMENT ATTACHMENT
*- DESCRIBE
  DESCRIBE TABLE GT_OUTPUT_SOLI LINES G_LINES.
*- DON'T CREATE ATTACHMENT IF NO DATA IS PRESENT
  IF G_LINES <> 0.
    LOOP AT GT_OUTPUT_SOLI INTO GF_OUTPUT_SOLI.
      GF_OBJTXT = GF_OUTPUT_SOLI.
*- APPEND
      APPEND GF_OBJTXT TO GT_OBJTXT.
*- CLEAR
      CLEAR GF_OBJTXT.
    ENDLOOP.
  ENDIF.

  LOOP AT S_RECEI.
*- COMPLETING THE RECIPIENT LIST
    GF_RECLIST-RECEIVER = S_RECEI-LOW.
    GF_RECLIST-REC_TYPE = C_REC_TYPE.
    GF_RECLIST-EXPRESS  = C_EXPRESS.
*- APPEND
    APPEND GF_RECLIST TO GT_RECLIST.
  ENDLOOP.

  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      DOCUMENT_DATA              = GF_DOC_CHNG
      PUT_IN_OUTBOX              = ''
      SENDER_ADDRESS             = LV_SENDER_ADDRESS
      SENDER_ADDRESS_TYPE        = 'B'
      COMMIT_WORK                = 'X'
    IMPORTING
      SENT_TO_ALL                = G_SENT_ALL
    TABLES
      PACKING_LIST               = GT_OBJPACK
      OBJECT_HEADER              = GT_OBJHEAD
      CONTENTS_TXT               = GT_OBJTXT
      RECEIVERS                  = GT_RECLIST
    EXCEPTIONS
      TOO_MANY_RECEIVERS         = 1
      DOCUMENT_NOT_SENT          = 2
      DOCUMENT_TYPE_NOT_EXIST    = 3
      OPERATION_NO_AUTHORIZATION = 4
      PARAMETER_ERROR            = 5
      X_ERROR                    = 6
      ENQUEUE_ERROR              = 7
      OTHERS                     = 8.
  IF SY-SUBRC = 0.
    CL_OS_TRANSACTION_END_NOTIFIER=>RAISE_COMMIT_REQUESTED( ).
    CALL FUNCTION 'DB_COMMIT'.
    CL_OS_TRANSACTION_END_NOTIFIER=>RAISE_COMMIT_FINISHED( ).
  ENDIF.

  SUBMIT RSCONN01 WITH MODE = 'INT' AND RETURN.

ENDFORM.                    " F_SEND_MAIL
*&---------------------------------------------------------------------*
*&      FORM  F_CHECK_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM F_CHECK_DATA_ZSDSSDT001.

  DATA : BEGIN OF LS_LIKP,
    VBELN TYPE LIKP-VBELN,
  END OF LS_LIKP.
  DATA LT_LIKP LIKE TABLE OF LS_LIKP.

  DATA : BEGIN OF LS_CHECK,
    DONUM TYPE ZSDSSDT001-DONUM,
  END OF LS_CHECK.
  DATA LT_CHECK LIKE TABLE OF LS_CHECK.

  DATA LS_ZSDSSDT001 TYPE ZSDSSDT001.

*  DATA LS_CHK LIKE LINE OF IN_CHK.

  IF IN_CHK[] IS NOT INITIAL.
    SELECT VBELN
      FROM LIKP
      INTO TABLE LT_LIKP
      FOR ALL ENTRIES IN IN_CHK
      WHERE VBELN EQ IN_CHK-DELIVERY.

    SELECT DONUM
      FROM ZSDSSDT001
      INTO TABLE LT_CHECK
      FOR ALL ENTRIES IN IN_CHK
      WHERE DONUM EQ IN_CHK-DELIVERY.

    LOOP AT LT_LIKP INTO LS_LIKP.
      READ TABLE LT_CHECK INTO LS_CHECK
      WITH KEY DONUM = LS_LIKP-VBELN.
      IF SY-SUBRC NE 0.
        LS_ZSDSSDT001-DONUM = LS_LIKP-VBELN.
        INSERT ZSDSSDT001 FROM LS_ZSDSSDT001.
        COMMIT WORK AND WAIT.
      ENDIF.

      CLEAR : LS_LIKP,LS_ZSDSSDT001,LS_CHECK.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " F_CHECK_DATA
*&---------------------------------------------------------------------*
*&      FORM  F_UPDATE_STORAGE_FOR_PROJ_DDD
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM F_UPDATE_STORAGE_FOR_PROJ_DDD .

  DATA : LT_LIPS TYPE TABLE OF LIPS,
         LS_LIPS TYPE LIPS.

  DATA : LLTAK   LIKE LTAK,
         L_TRTYP TYPE  C.

  DATA : LLTAP TYPE TABLE OF LTAP,
         XLIPS TYPE TABLE OF LIPS,
         XVBFA TYPE TABLE OF VBFAVB,
         XVBUK TYPE TABLE OF VBUKVB,
         XVBUP TYPE TABLE OF VBUPVB,
         YVBFA TYPE TABLE OF VBFAVB,
         YVBUK TYPE TABLE OF VBUKVB,
         YVBUP TYPE TABLE OF VBUPVB.

  DATA : LS_XLIPS TYPE LIPS,
         LS_LLTAP TYPE LTAP.

  DATA : LS_VBKOK TYPE  VBKOK.
  DATA : LT_VBPOK TYPE TABLE OF  VBPOK.

  FIELD-SYMBOLS <FS_VBPOK> TYPE VBPOK.
  DATA : LV_VBELN TYPE LIKP-VBELN.

  DATA : BEGIN OF LS_LIKP,
    VBELN TYPE LIKP-VBELN,
  END OF LS_LIKP.
  DATA : LT_LIKP LIKE TABLE OF LS_LIKP.

  SELECT VBELN
    FROM LIKP
    INTO TABLE LT_LIKP
    FOR ALL ENTRIES IN IN_CHK
    WHERE VBELN EQ IN_CHK-DELIVERY
      AND LSTEL EQ 'ZD'.
  IF SY-SUBRC = 0.
    SELECT *
      FROM LIPS
      INTO TABLE LT_LIPS
      FOR ALL ENTRIES IN LT_LIKP
      WHERE VBELN EQ LT_LIKP-VBELN.

    LOOP AT LT_LIPS INTO LS_LIPS WHERE VRKME NE 'SET' AND
                                       LGORT EQ SPACE.

      APPEND INITIAL LINE TO LT_VBPOK ASSIGNING <FS_VBPOK>.
      <FS_VBPOK>-VBELN_VL = LS_LIPS-VBELN.
      <FS_VBPOK>-POSNR_VL = LS_LIPS-POSNR.
      <FS_VBPOK>-KZLGO    = 'X'.
      <FS_VBPOK>-WERKS    = LS_LIPS-WERKS.
      "ADD BY WANTANEE 20200115
      IF P_1700 IS NOT INITIAL.
          <FS_VBPOK>-LGORT    = '1700'. " HERE PUT YOUR STOR. LOC
      ELSE.
           <FS_VBPOK>-LGORT    = '1800'. " HERE PUT YOUR STOR. LOC
      ENDIF.
      "END ADD BY WANTANEE 20200115

      <FS_VBPOK>-XWMPP    = 'X'.
      <FS_VBPOK>-LGPLA    = LS_LIPS-LGPLA.
      <FS_VBPOK>-LGTYP    = LS_LIPS-LGTYP.
      <FS_VBPOK>-BWLVS    = LS_LIPS-BWLVS.

      LV_VBELN = LS_LIPS-VBELN.
    ENDLOOP.
    IF SY-SUBRC EQ 0.
      LS_VBKOK-VBELN_VL   = LV_VBELN.
      CALL FUNCTION 'WS_DELIVERY_UPDATE'
        EXPORTING
          VBKOK_WA  = LS_VBKOK
          DELIVERY  = LV_VBELN
          COMMIT    = 'X'
        TABLES
          VBPOK_TAB = LT_VBPOK
*            PROT      = LT_PROT
        EXCEPTIONS
          ERROR_MESSAGE = 1
          OTHERS        = 2.
    ENDIF.
    COMMIT WORK AND WAIT.
    WAIT UP TO 10 SECONDS.
  ENDIF.

ENDFORM.                    " F_UPDATE_STORAGE_FOR_PROJ_DDD

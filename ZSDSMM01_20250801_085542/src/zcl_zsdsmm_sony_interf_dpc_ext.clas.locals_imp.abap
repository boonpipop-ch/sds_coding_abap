*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS LCL_DATA DEFINITION.
  PUBLIC SECTION.
    TYPES : BEGIN OF GY_SERIAL,
              DELIVERY TYPE C LENGTH 15,
              ITEM     TYPE C LENGTH 4,
              SERIAL   TYPE C LENGTH 18,
              MATNR    TYPE LIPS-MATNR,
              LFART    TYPE LIKP-LFART,
              FLAG     TYPE C LENGTH 1,
              PICK     TYPE C LENGTH 1,
              BLOCK    TYPE C LENGTH 1,
            END OF GY_SERIAL.
    TYPES : GTY_SERIAL TYPE TABLE OF GY_SERIAL WITH EMPTY KEY.

    TYPES : BEGIN OF TY_S_GOSERIAL,
              SELECTED TYPE XFELD,
              SERIALNO TYPE GERNR,
              UII      TYPE UII_CHAR72,              "EHP603 IUID
              SUBRK    TYPE XFELD,                   "EHP604 TT 2007.12.21
            END OF TY_S_GOSERIAL,
            TY_T_GOSERIAL TYPE STANDARD TABLE OF TY_S_GOSERIAL WITH
                                  NON-UNIQUE DEFAULT KEY.

    TYPES : BEGIN OF TY_S_GOSERIAL_KERNEL,
              GLOBAL_COUNTER TYPE MIGO_GLOBAL_COUNTER,
              T_GOSERIAL     TYPE TY_T_GOSERIAL,
            END OF TY_S_GOSERIAL_KERNEL.
    TYPES : TTY_S_GOSERIAL_KERNEL TYPE TABLE OF TY_S_GOSERIAL_KERNEL.

    CONSTANTS : GC_I  TYPE C LENGTH 1 VALUE 'I',
                GC_EQ TYPE C LENGTH 2 VALUE 'EQ',
                GC_S  TYPE C LENGTH 1 VALUE 'S',
                GC_E  TYPE C LENGTH 1 VALUE 'E',
                GC_X  TYPE C LENGTH 1 VALUE 'X',
                GC_A  TYPE C LENGTH 1 VALUE 'A',
                GC_L  TYPE C LENGTH 1 VALUE 'L',
                GC_F  TYPE C LENGTH 1 VALUE 'f',
                GC_P  TYPE C LENGTH 1 VALUE 'p',
                GC_B  TYPE C LENGTH 1 VALUE 'b',
                GC_C  TYPE C LENGTH 1 VALUE 'C'.

    CONSTANTS: GC_VL02N TYPE C LENGTH 5 VALUE 'VL02N'.

    METHODS :
      CONSTRUCTOR.

    CLASS-METHODS :
      UPDATE_SERIAL IMPORTING I_DATA   TYPE ZSDSSDS019
                    RETURNING VALUE(R) TYPE ZSDSSDS019,
      UPDATE_SERIAL_HOLD IMPORTING I_DATA   TYPE ZSDSSDS019
                         RETURNING VALUE(R) TYPE ZSDSSDS019,
      GET_SERIAL IMPORTING I_DATA   TYPE ZSDSSDS018_TT
                 RETURNING VALUE(R) TYPE GTY_SERIAL,
      UPDATE_DO IMPORTING IT_DATA  TYPE GTY_SERIAL
                          I_DDD    TYPE FLAG
                          I_GI     TYPE FLAG
                RETURNING VALUE(R) TYPE ZSDSSDS018_TT,
      UPDATE_STORAGE_FOR_PROJ_DDD IMPORTING IT_DATA TYPE GTY_SERIAL
                                            I_DDD   TYPE FLAG,
      VALIDATE_SERIAL IMPORTING PLV_DELIVERY TYPE ANY
                                PLV_ITEM     TYPE ANY
                                PLV_MATNR    TYPE ANY
                                PLV_SERIAL   TYPE ANY
                      CHANGING  CLV_INVALID  TYPE ANY
                                CLV_INPICK   TYPE ANY
                                CLV_INBLOCK  TYPE ANY
                                IN_PICK      TYPE GTY_SERIAL,
      CHECK_DUP_UPLOAD IMPORTING PLV_CHK_DELIVERY TYPE ANY
                                 PLV_CHK_ITEM     TYPE ANY
                       RETURNING VALUE(R)         TYPE FLAG,
      UPDATE_ERROR IMPORTING I_DATA   TYPE GY_SERIAL
                             I_TEXT   TYPE ANY
                   RETURNING VALUE(R) TYPE ZSDSSDS018_tt,
      UPDATE_SUCCESS IMPORTING I_DATA   TYPE GY_SERIAL
                               I_TEXT   TYPE ANY
                     RETURNING VALUE(R) TYPE ZSDSSDS018_tt,
      SEND_MAIL IMPORTING LV_TEXT  TYPE ANY
                          LV_CHECK TYPE ANY
                          IN_CHK   TYPE GTY_SERIAL,
      GET_SUBJECT RETURNING VALUE(R) TYPE STRING,
      GET_CONTANT_TEXT IMPORTING LV_SNAME TYPE ANY
                                 IT_CHK   TYPE GTY_SERIAL
                                 LV_CHECK TYPE ANY
                                 LV_TEXT  TYPE ANY
                       RETURNING VALUE(R) TYPE SOLI_TAB,
      GET_SENDER_EMAIL IMPORTING I_SNAME  TYPE ANY
                       RETURNING VALUE(R) TYPE ADR6-SMTP_ADDR,
      GET_SENDER_NAME IMPORTING I_SNAME  TYPE ANY
                      RETURNING VALUE(R) TYPE ADR6-SMTP_ADDR,
      PROCESS_DATA IMPORTING I_DATA   TYPE GY_SERIAL
                             IT_CHK   TYPE GTY_SERIAL
                             I_GI     TYPE FLAG
                   RETURNING VALUE(R) TYPE ZSDSSDS018_tt,
      BDC_DYNPRO IMPORTING I_PROGRAM TYPE ANY
                           I_DYNPRO  TYPE ANY
                 RETURNING VALUE(R)  TYPE TAB_BDCDATA,
      BDC_FIELD IMPORTING I_FNAM   TYPE ANY
                          I_FVAL   TYPE ANY
                RETURNING VALUE(R) TYPE TAB_BDCDATA,
      COMMIT,
      GET_EMAIL RETURNING VALUE(R) TYPE STRING_T,
      UPDATE_SERIAL_HOLD_DOC IMPORTING I_DATA  TYPE CHAR10
                                       IT_DATA TYPE TTY_S_GOSERIAL_KERNEL,
      GET_GUID IMPORTING I_DATA   TYPE CHAR10
               RETURNING VALUE(R) TYPE MMIM_PRED-GUID,
      UPDATE_DO_STATUS CHANGING C_DATA TYPE ZSDSSDS116,
      UPDATE_SMART_TRACK CHANGING C_DATA TYPE ZSDSSDS130,
      AUTO_POST_INV_MHA IMPORTING I_DATA TYPE GY_SERIAL.

*    CLASS-DATA :
*      GT_SERIAL TYPE GTY_SERIAL,
*      GS_SERIAL TYPE GY_SERIAL.
ENDCLASS.
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD UPDATE_SERIAL.
    DATA : BEGIN OF LS_LIKP,
             VBELN TYPE LIKP-VBELN,
             POSNR TYPE LIPS-POSNR,
             MATNR TYPE LIPS-MATNR,
             LFART TYPE LIKP-LFART,
           END OF LS_LIKP.
    DATA : LT_LIKP LIKE TABLE OF LS_LIKP.

    DATA : LT_SERIAL TYPE GTY_SERIAL.

    FIELD-SYMBOLS : <LFS_SERIAL> TYPE GY_SERIAL.

    CONSTANTS : BEGIN OF LC_CASE,
                  DDD TYPE C LENGTH 7 VALUE 'DDD',
                END OF LC_CASE.

    LT_SERIAL = GET_SERIAL( I_DATA-DETAIL ).

    IF LT_SERIAL[] IS NOT INITIAL.
      SELECT LIKP~VBELN,
             LIPS~POSNR,
             LIPS~MATNR,
             LIKP~LFART
        FROM @LT_SERIAL AS A
        INNER JOIN LIKP ON A~DELIVERY EQ LIKP~VBELN
        INNER JOIN LIPS ON LIKP~VBELN EQ LIPS~VBELN
        INTO TABLE @LT_LIKP.
    ENDIF.

    SORT LT_SERIAL BY DELIVERY ITEM SERIAL.

    LOOP AT LT_SERIAL ASSIGNING <LFS_SERIAL>.
      <LFS_SERIAL>-ITEM = |{ <LFS_SERIAL>-ITEM ALPHA = IN }|.
      READ TABLE LT_LIKP INTO LS_LIKP
      WITH KEY VBELN = <LFS_SERIAL>-DELIVERY+0(10)
               POSNR = <LFS_SERIAL>-ITEM.
      IF SY-SUBRC NE 0.
        CONTINUE.
      ELSE.
        <LFS_SERIAL>-MATNR = LS_LIKP-MATNR.
        <LFS_SERIAL>-LFART = LS_LIKP-LFART.
      ENDIF.
    ENDLOOP.

    IF I_DATA-TYPE EQ LC_CASE-DDD.
      R-DETAIL = UPDATE_DO( IT_DATA = LT_SERIAL
                            I_DDD   = ABAP_TRUE
                            I_GI    = I_DATA-GI ).
    ELSE.
      R-DETAIL = UPDATE_DO( IT_DATA = LT_SERIAL
                            I_DDD   = ABAP_FALSE
                            I_GI    = I_DATA-GI ).
    ENDIF.

  ENDMETHOD.
  METHOD GET_SERIAL.
    DATA : LS_DATA LIKE LINE OF I_DATA.

    DATA : LS_R LIKE LINE OF R.

    LOOP AT I_DATA INTO LS_DATA.
      LS_R-DELIVERY = LS_DATA-DELIVERY.
      LS_R-ITEM     = LS_DATA-ITEM.
      LS_R-SERIAL   = LS_DATA-SERIAL.

      APPEND LS_R TO R.
      CLEAR LS_R.
    ENDLOOP.
  ENDMETHOD.
  METHOD UPDATE_DO.
    DATA : LS_DATA LIKE LINE OF IT_DATA.

    DATA : LV_DELIV    TYPE VBELN_VL,
           LV_MATERIAL TYPE BAPIEKPO-MATERIAL,
           LV_SERIAL   TYPE EQUI-SERNR,
           LV_QUEST    TYPE STRING,
           LV_ANSWER   TYPE C,
           LV_FLAG     TYPE CHAR1,
           LV_INVALID  TYPE CHAR1,
           LV_INPICK   TYPE CHAR1,
           LV_INBLOCK  TYPE CHAR1.

    DATA : LT_PICK LIKE IT_DATA.

    DATA : LT_CHK TYPE TABLE OF GY_SERIAL WITH EMPTY KEY,
           LS_CHK TYPE GY_SERIAL.

    DATA : LT_DO   LIKE LT_CHK,
           LS_DO   LIKE LINE OF LT_DO,
           LV_ITEM TYPE CHAR50.

    DATA : LV_TAXT TYPE C LENGTH 255.

    UPDATE_STORAGE_FOR_PROJ_DDD( IT_DATA = IT_DATA
                                 I_DDD   = I_DDD ).
    LT_CHK[]  = IT_DATA[].
    LT_PICK[] = IT_DATA[].
    LOOP AT LT_CHK INTO LS_DATA.
      CLEAR: LV_DELIV, LV_MATERIAL, LV_SERIAL, LV_FLAG.
      LV_DELIV    = LS_DATA-DELIVERY.
      LV_MATERIAL = LS_DATA-MATNR.
      LV_SERIAL   = LS_DATA-SERIAL.

      CLEAR: LV_INVALID, LV_INPICK, LV_INBLOCK.
      VALIDATE_SERIAL( EXPORTING PLV_DELIVERY = LS_DATA-DELIVERY
                                 PLV_ITEM     = LS_DATA-ITEM
                                 PLV_MATNR    = LS_DATA-MATNR
                                 PLV_SERIAL   = LS_DATA-SERIAL
                        CHANGING CLV_INVALID  = LV_INVALID
                                 CLV_INPICK   = LV_INPICK
                                 CLV_INBLOCK  = LV_INBLOCK
                                 IN_PICK      = LT_PICK ).

      IF LV_INVALID EQ ABAP_TRUE.
        LS_DATA-FLAG = ABAP_TRUE.
        MODIFY LT_CHK FROM LS_DATA.
      ELSEIF LV_INPICK IS NOT INITIAL.
        LS_DATA-PICK = ABAP_TRUE.
        MODIFY LT_CHK FROM LS_DATA.
      ELSEIF LV_INBLOCK IS NOT INITIAL.
        LS_DATA-BLOCK = ABAP_TRUE.
        MODIFY LT_CHK FROM LS_DATA.
      ENDIF.
    ENDLOOP.

    READ TABLE LT_CHK
    WITH KEY FLAG = ABAP_TRUE TRANSPORTING NO FIELDS.
    IF SY-SUBRC <> 0.
      LT_DO[] = LT_CHK[].

      SORT LT_DO BY DELIVERY ITEM.
      DELETE ADJACENT DUPLICATES FROM LT_DO COMPARING DELIVERY ITEM.

      CLEAR LV_INVALID.
      LOOP AT LT_DO INTO LS_DO.
        LV_INVALID = CHECK_DUP_UPLOAD( PLV_CHK_DELIVERY = LS_DO-DELIVERY
                                       PLV_CHK_ITEM     = LS_DO-ITEM ).
        IF  LV_INVALID = ABAP_TRUE.
          IF LV_ITEM IS INITIAL.
            LV_ITEM = LS_DO-ITEM.
          ELSE.
            CONCATENATE LV_ITEM LS_DO-ITEM
                   INTO LV_ITEM
            SEPARATED BY ','.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    READ TABLE LT_CHK
    WITH KEY FLAG = ABAP_TRUE TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      LV_TAXT = TEXT-105.
      R = UPDATE_ERROR( I_DATA = LS_DATA
                        I_TEXT = LV_TAXT ).
      SEND_MAIL( LV_TEXT  = LV_TAXT
                 LV_CHECK = GC_F
                 IN_CHK   = LT_CHK ).
      RETURN.
    ENDIF.

    READ TABLE LT_CHK
    WITH KEY PICK = ABAP_TRUE TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      LV_TAXT = TEXT-107.

      SEND_MAIL( LV_TEXT  = LV_TAXT
                 LV_CHECK = GC_P
                 IN_CHK   = LT_CHK ).
    ENDIF.

    READ TABLE LT_CHK
    WITH KEY BLOCK = ABAP_TRUE TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      LV_TAXT = TEXT-108.
      R = UPDATE_ERROR( I_DATA = LS_DATA
                        I_TEXT = LV_TAXT ).
      SEND_MAIL( LV_TEXT  = LV_TAXT
                 LV_CHECK = GC_B
                 IN_CHK   = LT_CHK ).
    ENDIF.

    IF LT_CHK IS INITIAL.
      LV_TAXT = TEXT-109.
      R = UPDATE_ERROR( I_DATA = LS_DATA
                        I_TEXT = LV_TAXT ).
      SEND_MAIL( LV_TEXT  = LV_TAXT
                 LV_CHECK = GC_P
                 IN_CHK   = LT_CHK ).
    ELSE.
      R = PROCESS_DATA( I_DATA = LS_DATA
                        IT_CHK = LT_CHK
                        I_GI   = I_GI ).
    ENDIF.
  ENDMETHOD.
  METHOD UPDATE_STORAGE_FOR_PROJ_DDD.
    DATA : LT_LIPS TYPE TABLE OF LIPS,
           LS_LIPS TYPE LIPS.

    DATA : LLTAK   TYPE LTAK,
           L_TRTYP TYPE	C.

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
    DATA : LT_VBPOK TYPE TABLE OF	VBPOK.

    FIELD-SYMBOLS <FS_VBPOK> TYPE VBPOK.
    DATA : LV_VBELN TYPE LIKP-VBELN.

    DATA : BEGIN OF LS_LIKP,
             VBELN TYPE LIKP-VBELN,
           END OF LS_LIKP.
    DATA : LT_LIKP LIKE TABLE OF LS_LIKP.

    CONSTANTS : BEGIN OF LC_CON,
                  ZD  TYPE C LENGTH 2 VALUE 'ZD',
                  SET TYPE C LENGTH 3 VALUE 'SET',
                  TSS TYPE C LENGTH 4 VALUE '1700',
                  TSE TYPE C LENGTH 4 VALUE '1800',
                END OF LC_CON.


    SELECT VBELN
      FROM @IT_DATA AS A
      INNER JOIN LIKP ON A~DELIVERY EQ LIKP~VBELN AND
                         LIKP~LSTEL EQ @LC_CON-ZD
      INTO TABLE @LT_LIKP.
    IF SY-SUBRC = 0.
      SELECT *
        FROM LIPS
        INTO TABLE LT_LIPS
        FOR ALL ENTRIES IN LT_LIKP
        WHERE VBELN EQ LT_LIKP-VBELN.

      LOOP AT LT_LIPS INTO LS_LIPS WHERE VRKME NE LC_CON-SET AND
                                         LGORT EQ SPACE.

        APPEND INITIAL LINE TO LT_VBPOK ASSIGNING <FS_VBPOK>.
        <FS_VBPOK>-VBELN_VL = LS_LIPS-VBELN.
        <FS_VBPOK>-POSNR_VL = LS_LIPS-POSNR.
        <FS_VBPOK>-KZLGO    = ABAP_TRUE.
        <FS_VBPOK>-WERKS    = LS_LIPS-WERKS.
        "Add by Wantanee 20200115
        IF I_DDD IS NOT INITIAL.
          <FS_VBPOK>-LGORT    = LC_CON-TSS. " Here put your stor. loc
        ELSE.
          <FS_VBPOK>-LGORT    = LC_CON-TSE. " Here put your stor. loc
        ENDIF.
        "End Add by Wantanee 20200115

        <FS_VBPOK>-XWMPP    = ABAP_TRUE.
        <FS_VBPOK>-LGPLA    = LS_LIPS-LGPLA.
        <FS_VBPOK>-LGTYP    = LS_LIPS-LGTYP.
        <FS_VBPOK>-BWLVS    = LS_LIPS-BWLVS.

        LV_VBELN = LS_LIPS-VBELN.
      ENDLOOP.
      IF SY-SUBRC EQ 0.
        LS_VBKOK-VBELN_VL   = LV_VBELN.
        CALL FUNCTION 'WS_DELIVERY_UPDATE'
          EXPORTING
            VBKOK_WA      = LS_VBKOK
            DELIVERY      = LV_VBELN
            COMMIT        = ABAP_TRUE
          TABLES
            VBPOK_TAB     = LT_VBPOK
*           prot          = lt_prot
          EXCEPTIONS
            ERROR_MESSAGE = 1
            OTHERS        = 2.
      ENDIF.
      COMMIT WORK AND WAIT.
      WAIT UP TO 10 SECONDS.
    ENDIF.

  ENDMETHOD.
  METHOD VALIDATE_SERIAL.
    DATA : LV_STATUS  TYPE BSVX-STTXT.

    DATA : LV_SERNR   TYPE EQUI-SERNR.

    DATA : LS_PICK LIKE LINE OF IN_PICK.

    DATA : LWA_LIPS        TYPE LIPS,
           LWA_MARC        TYPE MARC,
           LWA_EQUI        TYPE EQUI,
           LWA_SER01       TYPE SER01,
           LWA_OBJK        TYPE OBJK,
           LWA_V_EQUI_EQBS TYPE V_EQUI_EQBS_SML,
           LWA_COUNT       TYPE I.

    CONSTANTS : BEGIN OF LC_CON,
                  ESTO TYPE C LENGTH 4 VALUE 'ESTO',
                  OS   TYPE C LENGTH 2 VALUE '07',
                END OF LC_CON.

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
    LOOP AT IN_PICK INTO LS_PICK WHERE DELIVERY EQ LWA_LIPS-VBELN AND
                                       ITEM     EQ LWA_LIPS-POSNR.
      ADD 1 TO LWA_COUNT.
    ENDLOOP.

    IF LWA_LIPS-LFIMG NE LWA_COUNT.
      CLV_INPICK = ABAP_TRUE.
    ENDIF.

* Check Mat master has Serial Profile ?
    SELECT SINGLE *
      FROM MARC
      INTO LWA_MARC
     WHERE WERKS EQ LWA_LIPS-WERKS
       AND MATNR EQ LWA_LIPS-MATNR.
* Serial Profile is not initail
    CHECK LWA_MARC-SERNP IS NOT INITIAL.
* Check Serial exist in system ?
    SELECT SINGLE *
      FROM EQUI
      INTO LWA_EQUI
     WHERE MATNR = LWA_LIPS-MATNR
       AND SERNR = LV_SERNR.
    IF SY-SUBRC = 0.
*   Check new serial is the same existing serial in DO item ?
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
*     New Serial
        IF SY-SUBRC <> 0.
*       Check Serial Status
          CLEAR LV_STATUS.
          CALL FUNCTION 'STATUS_TEXT_EDIT'
            EXPORTING
              CLIENT           = SY-MANDT
              FLG_USER_STAT    = ABAP_TRUE
              OBJNR            = LWA_EQUI-OBJNR
              SPRAS            = GC_E
            IMPORTING
              LINE             = LV_STATUS
            EXCEPTIONS
              OBJECT_NOT_FOUND = 1
              OTHERS           = 2.
          IF SY-SUBRC = 0.
            CONDENSE LV_STATUS.
            IF LV_STATUS+0(4) <> LC_CON-ESTO.
              CLV_INVALID = ABAP_TRUE.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*   Check Serial Number with SLOC
      CLEAR LWA_V_EQUI_EQBS.
      SELECT SINGLE * INTO LWA_V_EQUI_EQBS FROM V_EQUI_EQBS_SML
        WHERE MATNR   = LWA_LIPS-MATNR
          AND SERNR   = LV_SERNR
          AND B_WERK  = LWA_LIPS-WERKS
          AND B_LAGER = LWA_LIPS-LGORT.
      IF SY-SUBRC <> 0.
        CLV_INVALID = ABAP_TRUE.
      ENDIF.
      SELECT SINGLE * INTO LWA_V_EQUI_EQBS FROM V_EQUI_EQBS_SML
        WHERE MATNR   = LWA_LIPS-MATNR
          AND SERNR   = LV_SERNR
          AND B_WERK  = LWA_LIPS-WERKS
          AND B_LAGER = LWA_LIPS-LGORT
          AND LBBSA   = LC_CON-OS.
      IF SY-SUBRC EQ 0.
        CLV_INBLOCK = ABAP_TRUE.
      ENDIF.
    ELSE.
      CLV_INVALID = ABAP_TRUE.
    ENDIF.
  ENDMETHOD.
  METHOD CHECK_DUP_UPLOAD.
*    DATA : LWA_SER01 TYPE SER01.
    SELECT COUNT(*)
      FROM SER01
     WHERE LIEF_NR EQ PLV_CHK_DELIVERY
       AND POSNR   EQ PLV_CHK_ITEM.
    IF SY-SUBRC EQ 0.
      R = ABAP_TRUE.
    ENDIF.
  ENDMETHOD.
  METHOD UPDATE_ERROR.
*    DATA : LS_SERIAL TYPE LINE OF GTY_SERIAL.
*    READ TABLE I_DATA INTO LS_SERIAL INDEX 1.
*    IF SY-SUBRC = 0.
    DATA : LS_R LIKE LINE OF R.

    LS_R-DELIVERY    = I_DATA-DELIVERY.
    LS_R-ITEM        = I_DATA-ITEM.
    LS_R-SERIAL      = I_DATA-SERIAL.
    LS_R-MESSAGETYPE = GC_E.
    LS_R-MESSAGE     = I_TEXT.
    APPEND ls_R TO R.

    UPDATE ZSDSSDT001 SET SERNF = GC_E
                   WHERE DONUM EQ I_DATA-DELIVERY.
    COMMIT( ).
*    ENDIF.
  ENDMETHOD.
  METHOD SEND_MAIL.
*    DATA: BEGIN OF ASC_FILE OCCURS 100,
*            STRING TYPE STRING,
*          END OF ASC_FILE.
*    DATA: BEGIN OF ASC_PDF OCCURS 100,
*            STRING TYPE XSTRING,
*          END OF ASC_PDF.

    DATA : LV_MONTH_TEXT TYPE C LENGTH 10.

    DATA : LS_DOCUMENT_DATA       TYPE  SODOCCHGI1,
           LV_PUT_IN_OUTBOX       TYPE  SONV-FLAG,
           LV_SENDER_ADDRESS      TYPE  SOEXTRECI1-RECEIVER,
           LV_SENDER_ADDRESS_TYPE TYPE  SOEXTRECI1-ADR_TYP,
           LV_COMMIT_WORK         TYPE  SONV-FLAG.

    DATA : LV_SENT_TO_ALL   TYPE  SONV-FLAG,
           LV_NEW_OBJECT_ID TYPE  SOFOLENTI1-OBJECT_ID,
           LV_SENDER_ID     TYPE  SOUDK.

    DATA : I_TEXT    TYPE TABLE OF SOLI,
           LV_NUM    TYPE C LENGTH 15,
           TAB_LINE1 TYPE SO_BD_NUM,
           TAB_LINE2 TYPE	SO_BD_NUM,
           LV_FIELEC TYPE SOOD-OBJDES.

    DATA : LO_DOC_BCS TYPE REF TO CL_DOCUMENT_BCS,
           LO_BCS     TYPE REF TO CL_BCS.

    DATA : LO_SENDER TYPE REF TO CL_CAM_ADDRESS_BCS.

    DATA : LO_RECIPIENT TYPE REF TO CL_CAM_ADDRESS_BCS.

    DATA : LT_SOLIX TYPE SOLIX_TAB,
           LS_SOLIX LIKE LINE OF LT_SOLIX.

    DATA : LT_SOLIX_ATTH TYPE SOLIX_TAB,
           LS_SOLIX_ATTH LIKE LINE OF LT_SOLIX.

    DATA : LT_SOLI TYPE SOLI_TAB,
           LS_SOLI LIKE LINE OF LT_SOLI.

    DATA : LT_SOLI_ATTH TYPE SOLI_TAB,
           LS_SOLI_ATTH LIKE LINE OF LT_SOLI.

    DATA : I_SUBJECT TYPE SO_OBJ_DES.

    DATA : LV_SUB TYPE STRING.

    DATA : LV_LINE TYPE SO_TEXT255.

    DATA : LS_RECEI TYPE SOMLRECI1-RECEIVER.

*    DATA : LV_CC TYPE ADR6-SMTP_ADDR.

    DATA : LV_CC    TYPE ADR6-SMTP_ADDR, " VALUE 'jakarin_s@tripetch-it.co.th',
           LV_EMAIL TYPE ADR6-SMTP_ADDR. " VALUE 'jakarin_s@tripetch-it.co.th'.

    DATA : LT_EMAIL TYPE STRING_T.

    DATA : LV_SENDER TYPE ADR6-SMTP_ADDR, " VALUE 'jakarin_s@tripetch-it.co.th',
           LV_SNAME  TYPE ADR6-SMTP_ADDR. " VALUE 'jakarin_s@tripetch-it.co.th'.

    CONSTANTS : C_CRET(2) TYPE C VALUE CL_ABAP_CHAR_UTILITIES=>CR_LF.

    CONSTANTS : BEGIN OF LC_CON,
                  HTM TYPE C LENGTH 3 VALUE 'HTM',
                END OF LC_CON.

    CLEAR : LO_BCS,LO_DOC_BCS .

    LV_SNAME  = SY-UNAME.
    LV_SENDER = GET_SENDER_EMAIL( LV_SNAME ).
    LV_SNAME  = GET_SENDER_NAME( LV_SNAME ).

    LV_SUB  = GET_SUBJECT( ).
    LT_SOLI = GET_CONTANT_TEXT( LV_SNAME = LV_SNAME
                                LV_CHECK = LV_CHECK
                                IT_CHK   = IN_CHK
                                LV_TEXT  = LV_TEXT ).

    I_SUBJECT = LV_SUB.
    TRY.
        LO_DOC_BCS = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
                            I_TYPE    = LC_CON-HTM
                            I_TEXT    = LT_SOLI
                            I_SUBJECT = I_SUBJECT ).
      CATCH CX_DOCUMENT_BCS.
    ENDTRY.
    TRY.
        LO_BCS = CL_BCS=>CREATE_PERSISTENT( ).
      CATCH CX_SEND_REQ_BCS.
    ENDTRY.
    TRY.
        LO_BCS->SET_DOCUMENT( I_DOCUMENT = LO_DOC_BCS ).
      CATCH CX_SEND_REQ_BCS.
    ENDTRY.
    TRY.
        LO_BCS->SET_MESSAGE_SUBJECT( IP_SUBJECT = LV_SUB ).
      CATCH CX_SEND_REQ_BCS.
    ENDTRY.
    TRY.
        LO_SENDER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( I_ADDRESS_STRING = LV_SENDER
                                                                 I_ADDRESS_NAME   = LV_SNAME ).
      CATCH CX_ADDRESS_BCS.
    ENDTRY.
    TRY.
        LO_BCS->SET_SENDER( EXPORTING I_SENDER = LO_SENDER ).
      CATCH CX_SEND_REQ_BCS.
    ENDTRY.
    TRY.
*        LOOP AT S_RECEI INTO LS_RECEI.
        LT_EMAIL = GET_EMAIL( ).
        LOOP AT LT_EMAIL INTO DATA(LS_EMAIL).
          LV_EMAIL = LS_EMAIL.
          LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( I_ADDRESS_STRING =  LV_EMAIL ).
        ENDLOOP.
*        ENDLOOP.
      CATCH CX_ADDRESS_BCS.
    ENDTRY.
    TRY.
        LO_BCS->ADD_RECIPIENT( I_RECIPIENT = LO_RECIPIENT ).
      CATCH CX_SEND_REQ_BCS.
    ENDTRY.

*--------------------------------------------------------------------*
* Set UTF-8 For Excel
*--------------------------------------------------------------------*
*      DATA: BINARY_CONTENT TYPE SOLIX_TAB,
*            TEXT_STRING    TYPE STRING.
*
*      DATA: XS_CONTENT   TYPE XSTRING,
*            APP_TYPE(50) .
*
*      DATA LENGTH TYPE I.
*
*      APP_TYPE = 'text/plain; charset=utf-8'.
*
*      PERFORM F_GEN_DATA_EXCEL TABLES LT_SOLIX_ATTH
*                                USING LV_LINE
*                                      C_CRET.
*
*      LV_FIELEC = TEXT-101.
*      LO_DOC_BCS->ADD_ATTACHMENT( I_ATTACHMENT_TYPE    = 'XLS'
*                                  I_ATTACHMENT_SUBJECT = LV_FIELEC
*                                  I_ATT_CONTENT_HEX    = LT_SOLIX_ATTH ).
*--------------------------------------------------------------------*
* Set Send Excel
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* CC
*--------------------------------------------------------------------*
*      IF S_CC[] IS NOT INITIAL.
*        LOOP AT S_CC.
*          LV_CC = S_CC-LOW.
*          LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( I_ADDRESS_STRING = LV_CC  ).
*          LO_BCS->ADD_RECIPIENT( I_RECIPIENT = LO_RECIPIENT
*                                 I_COPY      = GC_X ).
*        ENDLOOP.
*      ENDIF.
*--------------------------------------------------------------------*
*Call Function send mail
*--------------------------------------------------------------------*
    TRY.
        LO_BCS->SEND( ).
      CATCH CX_SEND_REQ_BCS.
    ENDTRY.

*--------------------------------------------------------------------*
* Commit Work And Wait
*--------------------------------------------------------------------*
    COMMIT( ).

  ENDMETHOD.
  METHOD GET_EMAIL.
    SELECT VALUE_LOW
      FROM ZSDSCAC001
      WHERE REPID EQ 'EMAIL_GROUP'
        AND PARAM EQ 'LOGISTIC'
      INTO TABLE @R.
  ENDMETHOD.
  METHOD COMMIT.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = ABAP_TRUE.
  ENDMETHOD.
  METHOD GET_SUBJECT.
    R = TEXT-101.
  ENDMETHOD.
  METHOD GET_CONTANT_TEXT.
    DATA : LS_SOLI TYPE SOLI,
           IN_CHK  LIKE LINE OF IT_CHK.

    CONCATENATE '<body bgcolor = "#E6E6FA">'
                '<FONT COLOR = "#191970" face="Garamond">' '<b>'
                '<p>' 'Dear Sir/Madam,' '</p>'
                '<p>'
         INTO LS_SOLI-LINE.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    CONCATENATE 'Program cannot upload data form text file. Kindly check below detail.'
                '</p>'
                '<center>'
                '<TABLE  width= "100%" border="1">'
                '<TR >'
           INTO LS_SOLI-LINE.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    CONCATENATE'<td align = "LEFT" BGCOLOR = "#708090">'
               '<FONT COLOR = "BLUE"><B>DO No.</B> </FONT>'
               '</td>'
          INTO LS_SOLI-LINE.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    CONCATENATE '<td align = "LEFT" BGCOLOR = "#708090">'
                '<FONT COLOR = "BLUE"><B>Item</B> </FONT>'
                '</td>'
           INTO LS_SOLI-LINE.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    CONCATENATE'<td align = "LEFT" BGCOLOR = "#708090">'
               '<FONT COLOR = "BLUE"><B>Model</B> </FONT>'
               '</td>'
          INTO LS_SOLI-LINE.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    CONCATENATE '<td align = "LEFT" BGCOLOR = "#708090">'
                '<FONT COLOR = "BLUE"><B>Serial</B> </FONT>'
                '</td>'
          INTO LS_SOLI-LINE.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    CONCATENATE '<td align = "LEFT"  BGCOLOR = "#708090">'
                '<FONT COLOR = "BLUE"> <B>Message</B> </FONT>'
                '</td></tr>'
           INTO LS_SOLI-LINE.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.

    LOOP AT IT_CHK INTO IN_CHK.
      IF     LV_CHECK EQ 'f'.
        IF IN_CHK-FLAG NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
      ELSEIF LV_CHECK EQ 'p'.
        IF IN_CHK-PICK NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
      ELSEIF LV_CHECK EQ 'b'.
        IF IN_CHK-BLOCK NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
      ENDIF.

      CONCATENATE '<TR><td align = "LEFT">'
                  '<FONT COLOR = "BLUE">' IN_CHK-DELIVERY '</FONT>'
                  '</td>'  INTO LS_SOLI-LINE.
      APPEND LS_SOLI TO R.
      CLEAR LS_SOLI.

      CONCATENATE '<td align = "LEFT">'
                  '<FONT COLOR = "BLUE">' IN_CHK-ITEM '</FONT>'
                  '</td>'  INTO LS_SOLI-LINE.
      APPEND LS_SOLI TO R.
      CLEAR LS_SOLI.

      CONCATENATE '<td align = "LEFT">'
                  '<FONT COLOR = "BLUE">' IN_CHK-MATNR '</FONT>'
                  '</td>'  INTO LS_SOLI-LINE.
      APPEND LS_SOLI TO R.
      CLEAR LS_SOLI.

      CONCATENATE '<td align = "LEFT">'
                  '<FONT COLOR = "BLUE">' IN_CHK-SERIAL '</FONT>'
                  '</td>'  INTO LS_SOLI-LINE.
      APPEND LS_SOLI TO R.
      CLEAR LS_SOLI.

      CONCATENATE '<td align = "LEFT">'
                  '<FONT COLOR = "BLUE">' LV_TEXT '</FONT>'
                   '</td>'  INTO LS_SOLI-LINE.
      APPEND LS_SOLI TO R.
      CLEAR LS_SOLI.
    ENDLOOP.

    LS_SOLI-LINE = '</TABLE>'.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    LS_SOLI-LINE = '</center>'.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    LS_SOLI-LINE =  '<br>Regards,<br />'.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    CONCATENATE LV_SNAME '<br />' INTO LS_SOLI-LINE.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    LS_SOLI-LINE = '<br><br><b><center><i><font color = "RED">This is an auto generated Email.'.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    LS_SOLI-LINE = '</FONT></body>'.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.

  ENDMETHOD.
  METHOD PROCESS_DATA.
    DATA: LT_SERIAL TYPE TABLE OF BAPIDLVITMSERNO WITH EMPTY KEY,
          LT_RET    TYPE TABLE OF BAPIRET2.

    DATA: LS_SERIAL TYPE  BAPIDLVITMSERNO,
          LS_RET    TYPE  BAPIRET2.

    DATA: L_ODOH      TYPE BAPIOBDLVHDRCHG,
          L_ODOHC     TYPE BAPIOBDLVHDRCTRLCHG,
          L_ODELIVERY TYPE BAPIOBDLVHDRCHG-DELIV_NUMB.

    DATA: LT_BDCDATA  TYPE TABLE OF BDCDATA.

    DATA: IN_CHK LIKE LINE OF IT_CHK.

    DATA: BEGIN OF LS_SER01,
            LIEF_NR TYPE SER01-LIEF_NR,
            POSNR   TYPE SER01-POSNR,
          END OF LS_SER01.
    DATA LT_SER01 LIKE TABLE OF LS_SER01.

    DATA : LV_MODE TYPE C LENGTH 1 VALUE 'N'.

    DATA : LV_MSG  TYPE C LENGTH 100.

    DATA : LV_TEXT TYPE C LENGTH 255.

    DATA : lV_error TYPE C.

    DATA : LS_OPT  TYPE CTU_PARAMS.

    DATA : LT_MESSTAB TYPE TABLE OF BDCMSGCOLL.

    LOOP AT IT_CHK INTO IN_CHK.
      IF SY-TABIX = '1'.
        L_ODOH-DELIV_NUMB  = IN_CHK-DELIVERY.
        L_ODOHC-DELIV_NUMB = IN_CHK-DELIVERY.
        L_ODELIVERY        = IN_CHK-DELIVERY.
      ENDIF.
*
      LS_SERIAL-DELIV_NUMB = IN_CHK-DELIVERY.
      LS_SERIAL-ITM_NUMBER = IN_CHK-ITEM.
      LS_SERIAL-SERIALNO   = IN_CHK-SERIAL.
      APPEND LS_SERIAL TO LT_SERIAL.
      CLEAR LS_SERIAL.
    ENDLOOP.

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
      EXPORTING
        HEADER_DATA    = L_ODOH
        HEADER_CONTROL = L_ODOHC
        DELIVERY       = L_ODELIVERY
      TABLES
        ITEM_SERIAL_NO = LT_SERIAL
        RETURN         = LT_RET.

    READ TABLE LT_RET
    WITH KEY TYPE = GC_E TRANSPORTING NO FIELDS.
    IF SY-SUBRC <> 0.
      COMMIT( ).

      SELECT LIEF_NR POSNR
        FROM SER01
        INTO TABLE LT_SER01
        FOR ALL ENTRIES IN LT_SERIAL
        WHERE LIEF_NR EQ LT_SERIAL-DELIV_NUMB
          AND POSNR   EQ LT_SERIAL-ITM_NUMBER.
      IF LT_SER01[] IS NOT INITIAL.
        LV_TEXT = TEXT-S01.
        R = UPDATE_SUCCESS( I_DATA = I_DATA
                            I_TEXT = LV_TEXT ).

        UPDATE ZSDSSDT001 SET SERNF = ABAP_TRUE
                        WHERE DONUM EQ L_ODOH-DELIV_NUMB.

        AUTO_POST_INV_MHA( I_DATA ).
      ELSE.
        lV_error = GC_E.
        UPDATE ZSDSSDT001 SET SERNF = GC_E
                        WHERE DONUM EQ L_ODOH-DELIV_NUMB.
      ENDIF.
    ELSE.
      lV_error = GC_E.
      LV_TEXT = TEXT-E01.
      R = UPDATE_ERROR( I_DATA = I_DATA
                        I_TEXT = LV_TEXT ).

      UPDATE ZSDSSDT001 SET SERNF = GC_E
                      WHERE DONUM EQ L_ODOH-DELIV_NUMB.
    ENDIF.

*   post GI
    IF lV_error EQ SPACE.
      IF I_GI IS NOT INITIAL.
        SELECT COUNT(*)
          FROM VBUK
         WHERE VBELN EQ L_ODELIVERY
           AND WBSTK EQ GC_C.
        IF SY-SUBRC = 0.
*        MESSAGE I001 WITH TEXT-113.
        ELSE.
          LT_BDCDATA = BDC_DYNPRO( I_PROGRAM = 'SAPMV50A'
                                   I_DYNPRO  = '4004' ).

          LT_BDCDATA = BDC_FIELD( I_FNAM = 'BDC_CURSOR'
                                  I_FVAL = 'LIKP-VBELN' ).

          LT_BDCDATA = BDC_FIELD( I_FNAM = 'LIKP-VBELN'
                                  I_FVAL = L_ODELIVERY ).

          LT_BDCDATA = BDC_FIELD( I_FNAM = 'BDC_OKCODE'
                                  I_FVAL = '/00' ).

          LT_BDCDATA = BDC_DYNPRO( I_PROGRAM = 'SAPMV50A'
                                   I_DYNPRO  = '1000' ).

          LT_BDCDATA = BDC_FIELD( I_FNAM = 'BDC_OKCODE'
                                  I_FVAL = '=WABU_T' ).

*       Change Outbound Delivery > Post Goods Issue
*       MODE
*       'A' Display screen
*       'E' Display screen only if an error occurs
*       'N' No display

          LS_OPT-DISMODE  = 'N'."'A'
          LS_OPT-UPDMODE  = 'A'.
          LS_OPT-RACOMMIT = ABAP_TRUE.
          LS_OPT-NOBINPT  = ABAP_TRUE.
          LS_OPT-NOBIEND  = ABAP_TRUE.

*          CALL TRANSACTION GC_VL02N USING LT_BDCDATA OPTIONS FROM LS_OPT
*                                                     MESSAGES INTO LT_MESSTAB.
          IF SY-SUBRC NE 0.
            LV_TEXT = TEXT-E02.
            R = UPDATE_ERROR( I_DATA = I_DATA
                              I_TEXT = LV_TEXT ).
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      LV_TEXT = TEXT-E01.
      R = UPDATE_ERROR( I_DATA = I_DATA
                        I_TEXT = LV_TEXT ).
    ENDIF.
  ENDMETHOD.
  METHOD BDC_DYNPRO.
    DATA : LS_R LIKE LINE OF R.
    LS_R-PROGRAM  = I_PROGRAM.
    LS_R-DYNPRO   = I_DYNPRO.
    LS_R-DYNBEGIN = GC_X.
    APPEND LS_R TO R.
  ENDMETHOD.
  METHOD BDC_FIELD.
    DATA : LS_R LIKE LINE OF R.
    LS_R-FNAM = I_FNAM.
    LS_R-FVAL = I_FVAL.
    APPEND LS_R TO R.
  ENDMETHOD.
  METHOD GET_SENDER_EMAIL.
    SELECT SINGLE SMTP_ADDR
      FROM USR21
      INNER JOIN ADR6 ON USR21~PERSNUMBER EQ ADR6~PERSNUMBER AND
                         USR21~ADDRNUMBER EQ ADR6~ADDRNUMBER
      INTO R
      WHERE BNAME EQ I_SNAME.
  ENDMETHOD.
  METHOD GET_SENDER_NAME.
    SELECT SINGLE NAME_TEXT
      FROM USR21
      INNER JOIN ADRP ON USR21~PERSNUMBER EQ ADRP~PERSNUMBER
      INTO R
      WHERE BNAME EQ I_SNAME.
  ENDMETHOD.
  METHOD UPDATE_SUCCESS.
    DATA : LS_R LIKE LINE OF R.

    LS_R-DELIVERY    = I_DATA-DELIVERY.
    LS_R-ITEM        = I_DATA-ITEM.
    LS_R-SERIAL      = I_DATA-SERIAL.
    LS_R-MESSAGETYPE = GC_C.
    LS_R-MESSAGE     = I_TEXT.
    APPEND ls_R TO R.

    UPDATE ZSDSSDT001 SET SERNF = ABAP_TRUE
                   WHERE DONUM EQ I_DATA-DELIVERY.
    COMMIT( ).

  ENDMETHOD.
  METHOD UPDATE_SERIAL_HOLD.

    DATA : LS_HOLD TYPE ZSDSMMT026.

    DATA : LT_SERIAL TYPE GTY_SERIAL.

    DATA : LV_DOCNO TYPE C LENGTH 10.

    DATA : LT_GOSERIAL TYPE TABLE OF TY_S_GOSERIAL_KERNEL .

    DATA : LS_DATA_SERIAL TYPE TY_S_GOSERIAL.

    DATA : LS_GOSERIAL LIKE LINE OF LT_GOSERIAL.

    DATA : LV_POSNR TYPE ZSDSMMT026-ITEM.

    DATA : S_REFID TYPE RANGE OF MMIM_PREDOC_ORG-REFID.

    LT_SERIAL = GET_SERIAL( I_DATA-DETAIL ).

    SELECT ZSDSMMT026~DELIVERY,
           ZSDSMMT026~ITEM,
           ZSDSMMT026~SERIAL,
           ZSDSMMT026~ERNAM,
           ZSDSMMT026~ERDAT,
           ZSDSMMT026~ERZET
      FROM @LT_SERIAL AS A
      INNER JOIN ZSDSMMT026 ON A~DELIVERY EQ ZSDSMMT026~DELIVERY AND
                               A~ITEM     EQ ZSDSMMT026~ITEM     AND
                               A~SERIAL   EQ ZSDSMMT026~SERIAL
      INTO TABLE @DATA(LT_TMP).

    DATA LS_TMP LIKE LINE OF LT_TMP.

    LOOP AT I_DATA-DETAIL INTO DATA(LS_DETAIL).
      LV_DOCNO = LS_DETAIL-DELIVERY.
      READ TABLE LT_TMP INTO LS_TMP
      WITH KEY DELIVERY = LS_DETAIL-DELIVERY
               ITEM     = LS_DETAIL-ITEM
               SERIAL   = LS_DETAIL-SERIAL.
      IF SY-SUBRC NE 0.
        CLEAR : LS_TMP.
      ENDIF.

      LV_POSNR = LS_DETAIL-ITEM.
      LV_POSNR = |{ LV_POSNR ALPHA = IN }|.

      MODIFY ZSDSMMT026 FROM @(
        VALUE #(  DELIVERY = LS_DETAIL-DELIVERY
                  ITEM     = LV_POSNR
                  SERIAL   = LS_DETAIL-SERIAL
                  ERNAM    = COND #( WHEN LS_TMP-ERNAM IS INITIAL THEN SY-UNAME ELSE LS_TMP-ERNAM )
                  ERDAT    = COND #( WHEN LS_TMP-ERDAT IS INITIAL THEN SY-DATUM ELSE LS_TMP-ERDAT )
                  ERZET    = COND #( WHEN LS_TMP-ERZET IS INITIAL THEN SY-UZEIT ELSE LS_TMP-ERZET )
                  AENAM    = SY-UNAME
                  AEDAT    = SY-DATUM
                  AEZET    = SY-UZEIT
                  ) ).
      COMMIT WORK AND WAIT.
*      LS_DATA_SERIAL-SERIALNO = LS_DETAIL-SERIAL.
*      COLLECT LS_DATA_SERIAL INTO LS_GOSERIAL-T_GOSERIAL.
    ENDLOOP.

    S_REFID = VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = LV_DOCNO ) ).

    SUBMIT ZSDSMMR0640 USING SELECTION-SCREEN  1000
                       WITH S_REFID    IN S_REFID[]
                       AND RETURN.

*    IF LS_GOSERIAL-T_GOSERIAL IS NOT INITIAL.
*      LS_GOSERIAL-GLOBAL_COUNTER = 1.
*      APPEND LS_GOSERIAL TO LT_GOSERIAL.
*
*      UPDATE_SERIAL_HOLD_DOC( I_DATA    = LV_DOCNO
*                              IT_DATA   = LT_GOSERIAL ).
*    ENDIF.

  ENDMETHOD.
  METHOD UPDATE_SERIAL_HOLD_DOC.

    DATA : LV_GUID TYPE MMIM_PRED-GUID.

    CONSTANTS : BEGIN OF LC_CON,
                  GO_SERI TYPE CHAR30 VALUE 'PT_GOSERIAL_KERNEL',
                END OF LC_CON.

    DATA: LOREF_PREDOC   TYPE REF TO CL_MMIM_DATASTORE.
    IF LOREF_PREDOC IS NOT BOUND.
      CREATE OBJECT LOREF_PREDOC.
    ENDIF.

    LV_GUID = GET_GUID( I_DATA ).

    CALL METHOD LOREF_PREDOC->OPEN(
        I_GUID = LV_GUID
        I_MODE = CL_MMIM_DATASTORE=>C_APPEND ).
    COMMIT WORK AND WAIT.

    CALL METHOD LOREF_PREDOC->WRITE
      EXPORTING
        I_NAME = LC_CON-GO_SERI
        I_DATA = IT_DATA.

    CALL METHOD LOREF_PREDOC->CLOSE.
    COMMIT WORK AND WAIT.

  ENDMETHOD.
  METHOD GET_GUID.
    SELECT SINGLE MMIM_PREDOC_ORG~GUID
        FROM MMIM_PREDOC_ORG
        WHERE REFID = @I_DATA
        INTO @R.
  ENDMETHOD.
  METHOD UPDATE_DO_STATUS.
    DATA : LS_DO_DATA LIKE LINE OF C_DATA-DO_DATA.
    DATA : LS_ZSDSSDT001 TYPE ZSDSSDT001.
    DATA : LV_TIME TYPE C LENGTH 6.

*    DATA : LS_STRING TYPE C LENGTH 2500.

    LOOP AT C_DATA-DO_DATA INTO LS_DO_DATA.
      CLEAR: LV_TIME.
      LS_ZSDSSDT001-DONUM = LS_DO_DATA+0(10).
      LS_ZSDSSDT001-WMSDT = LS_DO_DATA+10(8).
      LS_ZSDSSDT001-WMSTM = LS_DO_DATA+18(4).
      LS_ZSDSSDT001-STAPC = LS_DO_DATA+22(8).
      LS_ZSDSSDT001-STAPT = LS_DO_DATA+30(4).
      LS_ZSDSSDT001-LOADD = LS_DO_DATA+34(8).
      LS_ZSDSSDT001-LOADT = LS_DO_DATA+42(4).
      LS_ZSDSSDT001-CONTD = LS_DO_DATA+46(8).
      LS_ZSDSSDT001-CONTS = LS_DO_DATA+54(4).
      LS_ZSDSSDT001-CONTP = LS_DO_DATA+58(60).
      LS_ZSDSSDT001-CONTT = LS_DO_DATA+118(120).
      LS_ZSDSSDT001-DOSIG = LS_DO_DATA+238(8).
      LS_ZSDSSDT001-PODDA = LS_DO_DATA+246(12).
      LS_ZSDSSDT001-PODRE = LS_DO_DATA+258(120).
      LS_ZSDSSDT001-DOSDS = LS_DO_DATA+378(8).
      LS_ZSDSSDT001-DOSDT = LS_DO_DATA+386(4).
      LS_ZSDSSDT001-ORDDA = LS_DO_DATA+390(8).
      LS_ZSDSSDT001-DEALC = LS_DO_DATA+398(10).
      LS_ZSDSSDT001-DEALN = LS_DO_DATA+408(40).
      LS_ZSDSSDT001-REMAK = LS_DO_DATA+448(255).
      LS_ZSDSSDT001-SHPAD = LS_DO_DATA+703(220).
      LS_ZSDSSDT001-SHPPV = LS_DO_DATA+923(30).
      LS_ZSDSSDT001-AMPMF = LS_DO_DATA+953(2).
      LS_ZSDSSDT001-LOADP = LS_DO_DATA+955(20).
      LS_ZSDSSDT001-REMAP = LS_DO_DATA+975(1).
      LS_ZSDSSDT001-REINV = LS_DO_DATA+976(1).
      LS_ZSDSSDT001-MAPST = LS_DO_DATA+977(10).
      IF LS_DO_DATA+987(4) IS NOT INITIAL.
        CONCATENATE LS_DO_DATA+987(4) '00' INTO LV_TIME.
      ELSE.
        CLEAR : LV_TIME.
      ENDIF.
      LS_ZSDSSDT001-EDOTM = LV_TIME.
      LS_ZSDSSDT001-DIVNM = LS_DO_DATA+991(60).
      LS_ZSDSSDT001-PLTNO = LS_DO_DATA+1051(20).
      LS_ZSDSSDT001-CARNA = LS_DO_DATA+1071(60).
      LS_ZSDSSDT001-TUKTY = LS_DO_DATA+1131(15).
      LS_ZSDSSDT001-SHPNO = LS_DO_DATA+1146(12).

      UPDATE ZSDSSDT001 SET DONUM = LS_ZSDSSDT001-DONUM
                            WMSDT = LS_ZSDSSDT001-WMSDT
                            WMSTM = LS_ZSDSSDT001-WMSTM
                            STAPC = LS_ZSDSSDT001-STAPC
                            STAPT = LS_ZSDSSDT001-STAPT
                            LOADD = LS_ZSDSSDT001-LOADD
                            LOADT = LS_ZSDSSDT001-LOADT
                            CONTD = LS_ZSDSSDT001-CONTD
                            CONTS = LS_ZSDSSDT001-CONTS
                            CONTP = LS_ZSDSSDT001-CONTP
                            CONTT = LS_ZSDSSDT001-CONTT
                            DOSIG = LS_ZSDSSDT001-DOSIG
                            PODDA = LS_ZSDSSDT001-PODDA
                            PODRE = LS_ZSDSSDT001-PODRE
                            DOSDS = LS_ZSDSSDT001-DOSDS
                            DOSDT = LS_ZSDSSDT001-DOSDT
                            ORDDA = LS_ZSDSSDT001-ORDDA
                            DEALC = LS_ZSDSSDT001-DEALC
                            DEALN = LS_ZSDSSDT001-DEALN
                            REMAK = LS_ZSDSSDT001-REMAK
                            SHPAD = LS_ZSDSSDT001-SHPAD
                            SHPPV = LS_ZSDSSDT001-SHPPV
                            AMPMF = LS_ZSDSSDT001-AMPMF
                            LOADP = LS_ZSDSSDT001-LOADP
                            REMAP = LS_ZSDSSDT001-REMAP
                            REINV = LS_ZSDSSDT001-REINV
                            MAPST = LS_ZSDSSDT001-MAPST
                            EDOTM = LS_ZSDSSDT001-EDOTM
                            DIVNM = LS_ZSDSSDT001-DIVNM
                            PLTNO = LS_ZSDSSDT001-PLTNO
                            CARNA = LS_ZSDSSDT001-CARNA
                            TUKTY = LS_ZSDSSDT001-TUKTY
                            SHPNO = LS_ZSDSSDT001-SHPNO
                            AENAM = SY-UNAME
                            AEDAT = SY-DATUM
                            AEZET = SY-UZEIT
                      WHERE DONUM = LS_ZSDSSDT001-DONUM.
      IF SY-SUBRC NE 0.
        LS_ZSDSSDT001-ERNAM = SY-UNAME.
        LS_ZSDSSDT001-ERDAT = SY-DATUM.
        LS_ZSDSSDT001-ERZET = SY-UZEIT.
        LS_ZSDSSDT001-AENAM = SY-UNAME.
        LS_ZSDSSDT001-AEDAT = SY-DATUM.
        LS_ZSDSSDT001-AEZET = SY-UZEIT.
        MODIFY ZSDSSDT001 FROM LS_ZSDSSDT001.
      ENDIF.
      CLEAR : LS_ZSDSSDT001.
    ENDLOOP.
    IF SY-SUBRC EQ 0.
      C_DATA-MESTYPE = 'Data Updated'.
    ELSE.
      C_DATA-MESTYPE = 'Cannot Update'.
    ENDIF.
    COMMIT WORK AND WAIT.
  ENDMETHOD.
  METHOD UPDATE_SMART_TRACK.
    DATA : LS_DATA_SMART LIKE LINE OF C_DATA-DATA_SMART.
    DATA : LS_ZSDSSDT025 TYPE ZSDSSDT025.

    LOOP AT C_DATA-DATA_SMART INTO LS_DATA_SMART.
      SPLIT LS_DATA_SMART AT '|' INTO LS_ZSDSSDT025-DO_NO
                                      LS_ZSDSSDT025-SMART_TRACKING_DATE
                                      LS_ZSDSSDT025-SMART_TRACKING_TIME.

      DATA(LV_LEN) = STRLEN( LS_ZSDSSDT025-SMART_TRACKING_TIME ).
      IF LV_LEN EQ 4.
        CONCATENATE LS_ZSDSSDT025-SMART_TRACKING_TIME '00' INTO LS_ZSDSSDT025-SMART_TRACKING_TIME.
      ENDIF.

      LS_ZSDSSDT025-ERNAM = SY-UNAME.
      LS_ZSDSSDT025-ERDAT = SY-DATUM.
      LS_ZSDSSDT025-ERZET = SY-UZEIT.
      LS_ZSDSSDT025-AENAM = SY-UNAME.
      LS_ZSDSSDT025-AEDAT = SY-DATUM.
      LS_ZSDSSDT025-AEZET = SY-UZEIT.

      MODIFY ZSDSSDT025 FROM LS_ZSDSSDT025.
    ENDLOOP.

    IF SY-SUBRC EQ 0.
      C_DATA-MESTYPE = 'Data Updated'.
    ELSE.
      C_DATA-MESTYPE = 'Cannot Update'.
    ENDIF.
    COMMIT WORK AND WAIT.

  ENDMETHOD.
  METHOD AUTO_POST_INV_MHA.

    DATA: LV_JOBNAME  TYPE TBTCO-JOBNAME,
          LV_JOBCOUNT TYPE TBTCO-JOBCOUNT.

    DATA : S_WERKS TYPE RANGE OF MARD-WERKS.

    DATA : S_VBELN TYPE RANGE OF LIKP-VBELN.
    DATA : S_ZZPOB TYPE RANGE OF VBAK-ZZPOB.

    DATA : LS_PRINT_PARAMETERS TYPE PRI_PARAMS.
    DATA : LS_SELTAB           TYPE TABLE OF RSPARAMS.

    DATA : LV_USER LIKE SY-UNAME.

    CONSTANTS : BEGIN OF LC_CON,
                  I_REPID             TYPE CHAR20 VALUE 'USER_JOB_POST_DO',
                  I_SINGLE_VALUE_FLAG TYPE FLAG   VALUE 'X',
                  I_PARAM             TYPE CHAR20 VALUE 'USER',
                END OF LC_CON.

    DATA : CR_RETURN     TYPE RANGE OF VBAK-VBELN,
           C_RETURN      TYPE C LENGTH 255,
           C_RETURN_HIGH TYPE C LENGTH 255.

    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID              = LC_CON-I_REPID
                                                  I_SINGLE_VALUE_FLAG  = LC_CON-I_SINGLE_VALUE_FLAG
                                                  I_PARAM              = LC_CON-I_PARAM
                                         CHANGING C_RETURN             = LV_USER
                             ).

    IF I_DATA-DELIVERY IS NOT INITIAL.
      S_VBELN =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = I_DATA-DELIVERY ) ).
*      S_ZZPOB =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = 'Z1' ) ).

      CONCATENATE 'MHA' '_' I_DATA-DELIVERY '_' SY-DATUM '_' SY-UZEIT INTO LV_JOBNAME.

      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          JOBNAME          = LV_JOBNAME
        IMPORTING
          JOBCOUNT         = LV_JOBCOUNT
        EXCEPTIONS
          CANT_CREATE_JOB  = 1
          INVALID_JOB_DATA = 2
          JOBNAME_MISSING  = 3
          OTHERS           = 4.
      CASE SY-SUBRC.
        WHEN 0.
        WHEN OTHERS.
          MESSAGE E208(00) WITH 'Error'.
      ENDCASE.

      CALL FUNCTION 'GET_PRINT_PARAMETERS'
        EXPORTING
*         immediately            = 'X'
          NO_DIALOG              = 'X'
          USER                   = LV_USER
        IMPORTING
          OUT_PARAMETERS         = LS_PRINT_PARAMETERS
        EXCEPTIONS
          ARCHIVE_INFO_NOT_FOUND = 1
          INVALID_PRINT_PARAMS   = 2
          INVALID_ARCHIVE_PARAMS = 3
          OTHERS                 = 4.

      SUBMIT ZSDSSDR0630 TO SAP-SPOOL AND RETURN
                                   WITH SELECTION-TABLE LS_SELTAB
                                                   USER LV_USER
                                       SPOOL PARAMETERS LS_PRINT_PARAMETERS
                                   WITHOUT SPOOL DYNPRO
                                                VIA JOB LV_JOBNAME
                                                 NUMBER LV_JOBCOUNT
                        USING SELECTION-SCREEN  1000
                              WITH P_KSCHL EQ 'ZIV5'
                              WITH S_VBELN IN S_VBELN[]
                              WITH S_ZZPOB IN S_ZZPOB[]
                              WITH P_AUTO  EQ ABAP_TRUE
                              WITH P_MHA   EQ ABAP_TRUE.

      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          JOBCOUNT             = LV_JOBCOUNT
          JOBNAME              = LV_JOBNAME
          STRTIMMED            = ABAP_TRUE
        EXCEPTIONS
          CANT_START_IMMEDIATE = 1
          INVALID_STARTDATE    = 2
          JOBNAME_MISSING      = 3
          JOB_CLOSE_FAILED     = 4
          JOB_NOSTEPS          = 5
          JOB_NOTEX            = 6
          LOCK_FAILED          = 7
          OTHERS               = 8.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0700
*  Creation Date      : 24.02.2025
*  Author             : Boonpipop Ch.(SDS)
*  Add-on ID          : N/A
*  Description        : AP Invoice Aging Report
*  Purpose            : N/A
*  Copied from        : ZR_FIAP_INVOICE_AGING_REP_FYC (ECC)
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSFIR0700 MESSAGE-ID ZSDSFI01.

*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : LFA1,
         LFB1,
         BSIK,
         BSEG,
         BSEC,
         BKPF,
         BSEGA.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF TY_OUTPUT,
          BREIF(50)      TYPE C,
          LIFNR          TYPE LIFNR,
          NAME1          TYPE NAME1,
          BELNR          TYPE BELNR_D,
          BUKRS          TYPE BUKRS,
          GJAHR          TYPE GJAHR,
          BLART          TYPE BLART,
          XBLNR          TYPE XBLNR,
          SGTXT          TYPE SGTXT,
          ZBD1T          TYPE CHAR3,
          PSTDT          TYPE BUDAT,
          INVDT          TYPE BLDAT,
          NETDT          TYPE BLDAT,
          WAERS          TYPE WAERS,
          HWAERS         TYPE WAERS,
          INVDY          TYPE CHAR8,
          NETDY          TYPE CHAR8,
          WRBTR          TYPE WRBTR,
          KURSF          TYPE CHAR10,
          DMBTR          TYPE DMBTR,
          HWBAS          TYPE HWBAS,
          HWSTE          TYPE HWSTE,
          REMAIN_AMT     TYPE DMBTR,
          REMAIN_AMT_DOC TYPE WRBTR,
          AMT_1          TYPE WRBTR,
          AMT_2          TYPE WRBTR,
          AMT_3          TYPE WRBTR,
          AMT_4          TYPE WRBTR,
          AMT_5          TYPE WRBTR,
          AUFNR          TYPE AUFK-AUFNR,
          KTEXT          TYPE AUFK-KTEXT,
          USNAM          TYPE BKPF-USNAM,
        END OF TY_OUTPUT.

TYPES : BEGIN OF TY_VENDOR,
          LIFNR TYPE LIFNR,
          BUKRS TYPE BUKRS,
          NAME1 TYPE NAME1,
          AKONT TYPE AKONT,
          KTOKK TYPE KTOKK,
        END OF TY_VENDOR.

TYPES : BEGIN OF TY_BSET,
          BUKRS     TYPE BUKRS,
          BELNR     TYPE BELNR_D,
          GJAHR     TYPE GJAHR,
          HWBAS     TYPE HWBAS,
          HWSTE     TYPE HWSTE,
          FLAG_READ TYPE C,
        END OF TY_BSET.

TYPES : BEGIN OF TY_BSIK2,
          BUKRS TYPE BUKRS,
          LIFNR TYPE BSIK-LIFNR,
          GJAHR TYPE GJAHR,
          BELNR TYPE BELNR_D,
          BUZEI TYPE BUZEI,
          REBZG TYPE REBZG,
          REBZJ TYPE REBZJ,
          SHKZG TYPE SHKZG,
          DMBTR TYPE DMBTR,
          WRBTR TYPE WRBTR,
          FLAG  TYPE C,
        END OF TY_BSIK2.

TYPES : BEGIN OF TYP_BKPF,
          BELNR TYPE BKPF-BELNR,
          GJAHR TYPE BKPF-GJAHR,
          USNAM TYPE BKPF-USNAM,
        END OF TYP_BKPF.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GV_CHK    TYPE C,
       GV_FLAG   TYPE C,
       GV_GJAHR  TYPE GJAHR,
       GV_PERIV  TYPE PERIV,
       GV_HWAERS TYPE WAERS.

TYPE-POOLS: SLIS.
DATA : GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
       GT_LAYOUT   TYPE SLIS_LAYOUT_ALV,
       GT_EVENTS   TYPE SLIS_T_EVENT,
       GT_HEADING  TYPE SLIS_T_LISTHEADER.

DATA : V_POS TYPE I .

DATA : R_BSTAT   TYPE RANGE OF BSIK-BSTAT,
       R_DATE1   TYPE RANGE OF RFPDO1-ALLGROGR,
       R_DATE2   TYPE RANGE OF RFPDO1-ALLGROGR,
       R_DATE3   TYPE RANGE OF RFPDO1-ALLGROGR,
       R_DATE4   TYPE RANGE OF RFPDO1-ALLGROGR,
       R_DATE5   TYPE RANGE OF RFPDO1-ALLGROGR,
       R_MR8M    TYPE RANGE OF BSEG-BELNR,
       R_PARTIAL TYPE RANGE OF BSIK-BLART,
       R_GJAHR   TYPE RANGE OF BSIK-GJAHR.
DATA: GW_BSIK    TYPE BSIK,
      GW_BSIK_BK TYPE BSIK,
      GW_OUTPUT  TYPE TY_OUTPUT,
      GW_VENDOR  TYPE TY_VENDOR,
      GW_BSET    TYPE TY_BSET,
      GW_BSIK2   TYPE TY_BSIK2.
DATA: GT_BSIK     TYPE SORTED TABLE OF BSIK WITH
                    UNIQUE KEY BUKRS LIFNR GJAHR BELNR BUZEI,
      GT_BSIK_BK  TYPE STANDARD TABLE OF BSIK,
      GT_OUTPUT   TYPE STANDARD TABLE OF TY_OUTPUT,
      GT_VENDOR   TYPE STANDARD TABLE OF TY_VENDOR,
      GT_BSET     TYPE STANDARD TABLE OF TY_BSET,
      GT_BSET_TAB TYPE STANDARD TABLE OF TY_BSET,
      GT_BSIK2    TYPE STANDARD TABLE OF TY_BSIK2.

DATA: GT_BKPF TYPE STANDARD TABLE OF TYP_BKPF,
      GW_BKPF TYPE TYP_BKPF.
*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS : GC_TRUE      TYPE C                 VALUE 'X',
            GC_REPID     TYPE SY-REPID          VALUE 'ZSDSFIR0700',
            GC_PARTIAL   TYPE ZSDSCAC001-PARAM  VALUE 'PARTIAL_BLART_*',
            GC_MASK_DATE TYPE CHAR10            VALUE '__.__.____',
            GC_MASK_TIME TYPE CHAR8             VALUE '__:__:__'.

*-----------------------------------------------------------------------
* M A C R O S
*-----------------------------------------------------------------------
*DEFINE.
*END-OF-DEFINITION.

*-----------------------------------------------------------------------
* F I E L D – S Y M B O L S
*-----------------------------------------------------------------------
*FIELD-SYMBOLS:

*-----------------------------------------------------------------------
* F I E L D – G R O U P S
*-----------------------------------------------------------------------
*FIELD-GROUPS:

*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK0 WITH FRAME TITLE TEXT-SE0.
  PARAMETER : P_BUKRS TYPE BUKRS OBLIGATORY DEFAULT '1000'.
  SELECT-OPTIONS : S_KTOKK FOR LFA1-KTOKK,         "Vendor Group
                   S_AKONT FOR LFB1-AKONT,         "Reconcile Account
                   S_LIFNR FOR LFA1-LIFNR,         "Vendor
                   S_BELNR FOR BSIK-BELNR.         "Document number
  PARAMETER : P_DATUM TYPE BLDAT OBLIGATORY DEFAULT SY-DATUM . "As of Date

* for option Due Date
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETER : R_NETDT RADIOBUTTON GROUP TYP DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 05(24) TEXT-001.
    SELECT-OPTIONS : S_NETDT FOR BSEGA-NETDT.
  SELECTION-SCREEN END OF LINE.
* for option Invoice Date
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETER : R_INVDT RADIOBUTTON GROUP TYP .
    SELECTION-SCREEN COMMENT 05(24) TEXT-002 .
    SELECT-OPTIONS : S_INVDT FOR BSEGA-NETDT.
  SELECTION-SCREEN END OF LINE.

  SELECT-OPTIONS : S_BUDAT FOR BSIK-BUDAT.               "Posting Date
  SELECT-OPTIONS : S_WAERS FOR BSIK-WAERS NO INTERVALS.  "Currency

SELECTION-SCREEN END OF BLOCK BLOCK0.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-SE1.
  PARAMETER : P_NORMAL AS CHECKBOX DEFAULT 'X'.          "Normal Item
  SELECT-OPTIONS : S_UMSKZ FOR BSEG-UMSKZ.                "Special GL Transaction
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-SE2.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 01(31) TEXT-003 FOR FIELD P_D1.
    PARAMETERS: P_D1 LIKE RFPDO1-ALLGROGR DEFAULT '000'.
    PARAMETERS: P_D2 LIKE RFPDO1-ALLGROGR DEFAULT '030'.
    PARAMETERS: P_D3 LIKE RFPDO1-ALLGROGR DEFAULT '060'.
    PARAMETERS: P_D4 LIKE RFPDO1-ALLGROGR DEFAULT '090'.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BLOCK2.

*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.
  CLEAR GV_CHK.
*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
AT SELECTION-SCREEN.

  IF P_D4 IS INITIAL AND P_D3 IS INITIAL AND
     P_D2 IS INITIAL AND P_D1 IS INITIAL.
    MESSAGE E999 WITH 'Please input Aging Days'.
  ENDIF.

  IF NOT P_D4 IS INITIAL.
    IF  P_D4 GT P_D3
    AND P_D3 GT P_D2
    AND P_D2 GT P_D1.
    ELSE.
      MESSAGE E003.
    ENDIF.
  ELSE.
    IF NOT P_D3 IS INITIAL.
      IF  P_D3 GT P_D2
      AND P_D2 GT P_D1.
      ELSE.
        MESSAGE E003.
      ENDIF.
    ELSE.
      IF NOT P_D2 IS INITIAL.
        IF  P_D2 GT P_D1.
        ELSE.
          MESSAGE E003.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
  CHECK GV_CHK IS INITIAL.
  PERFORM GET_GENC.
  PERFORM CHECK_FLAG.
  PERFORM CHECK_DAY.
  PERFORM GET_CURRENCY.
  PERFORM GET_RANGE.
  PERFORM GET_YEAR.
  PERFORM GET_DATA.
  PERFORM MAP_DATA.

*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
*-> process data
  IF NOT GT_OUTPUT[] IS INITIAL.
    PERFORM DISPLAY_REPROT.
  ELSE.
    MESSAGE I004.
    EXIT.
  ENDIF.
*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
*TOP-OF-PAGE.

*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
*AT LINE-SELECTION.

*-----------------------------------------------------------------------
* F O R M S
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .
  REFRESH : GT_BSIK ,GT_BSET_TAB , GT_BSET .
  CLEAR GW_BSIK.

  DATA: LS_MR8M LIKE LINE OF R_MR8M.

* get data from BSIK
  SELECT *
  INTO TABLE GT_BSIK
  FROM BSIK
  WHERE BUKRS EQ P_BUKRS
    AND LIFNR IN S_LIFNR
    AND UMSKZ IN S_UMSKZ
    AND GJAHR IN R_GJAHR
    AND ( BUDAT IN S_BUDAT AND
          BUDAT LE P_DATUM )
    AND BELNR IN S_BELNR
    AND WAERS IN S_WAERS
    AND SAKNR IN S_AKONT.

* get data from BSAK
  SELECT *
  APPENDING TABLE GT_BSIK
  FROM BSAK
  WHERE BUKRS EQ P_BUKRS
    AND LIFNR IN S_LIFNR
    AND UMSKZ IN S_UMSKZ
    AND GJAHR IN R_GJAHR
    AND ( BUDAT IN S_BUDAT AND
          BUDAT LE P_DATUM )
    AND AUGDT GT P_DATUM
*    AND ( budat IN s_budat and
*          budat gt p_datum )
    AND BELNR IN S_BELNR
    AND WAERS IN S_WAERS
    AND SAKNR IN S_AKONT.

  IF NOT GT_BSIK[] IS INITIAL.
    GT_BSIK_BK[] = GT_BSIK[].
    CLEAR R_MR8M[].
    LS_MR8M-SIGN   = 'I'.
    LS_MR8M-OPTION = 'EQ'.
    LOOP AT GT_BSIK_BK INTO GW_BSIK_BK.
      IF GW_BSIK_BK-SGTXT = 'MR8M'.
        LS_MR8M-LOW = GW_BSIK_BK-BELNR.
        APPEND LS_MR8M TO R_MR8M[].
        CLEAR LS_MR8M-LOW.
        IF NOT GW_BSIK_BK-REBZG IS INITIAL.
          LS_MR8M-LOW = GW_BSIK_BK-REBZG.
          APPEND LS_MR8M TO R_MR8M.
          CLEAR LS_MR8M-LOW.
        ELSE.
          READ TABLE GT_BSIK INTO GW_BSIK WITH KEY BUKRS = GW_BSIK_BK-BUKRS
                                                   REBZG = GW_BSIK_BK-BELNR
                                                   GJAHR = GW_BSIK_BK-GJAHR
                                                   BUZEI = GW_BSIK_BK-BUZEI
                                                   SGTXT = 'MR8M'.
          IF SY-SUBRC = 0.
            LS_MR8M-LOW = GW_BSIK_BK-BELNR.
            APPEND LS_MR8M TO R_MR8M.
            CLEAR R_MR8M.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  REFRESH GT_BSIK_BK.

* delete document from MR8M
  IF NOT R_MR8M[] IS INITIAL.
    DELETE GT_BSIK WHERE BELNR IN R_MR8M.
  ENDIF.

  IF NOT GT_BSIK[] IS INITIAL.
* Start of modify IMS#300000167 to add sign for amount
* get partial doc from BSIK
*    SELECT bukrs gjahr belnr buzei
*           rebzg rebzj dmbtr wrbtr
    SELECT  BUKRS
            LIFNR
            GJAHR
            BELNR
            BUZEI
            REBZG
            REBZJ
            SHKZG
            DMBTR
            WRBTR
* End of modify IMS#300000167 to add sign for amount
    INTO TABLE GT_BSIK2
    FROM BSIK
    FOR ALL ENTRIES IN GT_BSIK
    WHERE BUKRS = GT_BSIK-BUKRS
      AND REBZG = GT_BSIK-BELNR
      AND REBZJ = GT_BSIK-GJAHR
      AND ( BUDAT IN S_BUDAT AND
            BUDAT LE P_DATUM )
      AND UMSKZ   IN S_UMSKZ.
* get partial doc from BSAK
* Start of modify IMS#300000167 to add sign for amount
*    SELECT bukrs gjahr belnr buzei
*           rebzg rebzj dmbtr wrbtr
    SELECT  BUKRS
            LIFNR
            GJAHR
            BELNR
            BUZEI
            REBZG
            REBZJ
            SHKZG
            DMBTR
            WRBTR
* End of modify IMS#300000167 to add sign for amount
    APPENDING TABLE GT_BSIK2
    FROM BSAK
    FOR ALL ENTRIES IN GT_BSIK
    WHERE BUKRS = GT_BSIK-BUKRS
      AND REBZG = GT_BSIK-BELNR
      AND REBZJ = GT_BSIK-GJAHR
    AND ( BUDAT IN S_BUDAT AND
          BUDAT LE P_DATUM )
    AND AUGDT GT P_DATUM
    AND UMSKZ   IN S_UMSKZ.

    SELECT A~LIFNR BUKRS NAME1 AKONT KTOKK
    INTO TABLE GT_VENDOR
    FROM LFA1 AS A INNER JOIN LFB1 AS B
    ON  A~LIFNR = B~LIFNR
    AND B~BUKRS = P_BUKRS
    FOR ALL ENTRIES IN GT_BSIK
    WHERE A~LIFNR = GT_BSIK-LIFNR.

* Begin of change IMS 300000167
*    SELECT bukrs belnr gjahr buzei hwbas hwste
*    INTO TABLE gt_bset
    SELECT BUKRS BELNR GJAHR HWBAS HWSTE
     INTO TABLE GT_BSET_TAB
* Eng of change  IMS 300000167
     FROM BSET
     FOR ALL ENTRIES IN GT_BSIK
     WHERE BUKRS = GT_BSIK-BUKRS
       AND BELNR = GT_BSIK-BELNR
       AND GJAHR = GT_BSIK-GJAHR .
* Begin of change IMS 300000167
*      AND buzei = gt_bsik-buzei.

    LOOP AT GT_BSET_TAB INTO GW_BSET .
      COLLECT GW_BSET INTO GT_BSET.
    ENDLOOP.
* End of change IMS 300000167
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_YEAR .
  CLEAR : GV_GJAHR,
          GV_PERIV.

  SELECT SINGLE PERIV
  INTO GV_PERIV
  FROM T001
  WHERE BUKRS = P_BUKRS.

*  IF sy-subrc = 0.
*    CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
*      EXPORTING
*        i_date         = p_datum
*        i_periv        = gv_periv
*      IMPORTING
*        e_gjahr        = gv_gjahr
*      EXCEPTIONS
*        input_false    = 1
*        t009_notfound  = 2
*        t009b_notfound = 3
*        OTHERS         = 4.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*    gv_gjahr = p_gjahr.

*  ENDIF.
  REFRESH R_GJAHR.
  CLEAR R_GJAHR.

*-->> Begin Remark Parinya25Jan2012
*-> Search key : 20120131
*  IF p_gjahr IS NOT INITIAL.
*    r_gjahr-sign = 'I'.
*    r_gjahr-option = 'EQ'.
*    r_gjahr-low = p_gjahr.
*    APPEND r_gjahr.
*    CLEAR r_gjahr.
*  ENDIF.
*-->> End Remark Parinya25Jan2012

ENDFORM.                    " GET_YEAR
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPROT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_REPROT .
  PERFORM BUILD_LAYOUT.
  PERFORM BUILD_CATALOG.
  PERFORM BUILD_EVENT USING GT_EVENTS.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
      IS_LAYOUT          = GT_LAYOUT
      IT_FIELDCAT        = GT_FIELDCAT
      IT_EVENTS          = GT_EVENTS
      I_SAVE             = 'A'
    TABLES
      T_OUTTAB           = GT_OUTPUT
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " DISPLAY_REPROT
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT .
  GT_LAYOUT-WINDOW_TITLEBAR = SY-TITLE.
  GT_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_CATALOG .
  DATA : LV_TEXT1(30) TYPE C,
         LV_TEXT2(30) TYPE C,
         LV_TEXT3(30) TYPE C,
         LV_TEXT4(30) TYPE C,
         LV_TEXT5(30) TYPE C,
         LV_D1(3)     TYPE N,
         LV_D2(3)     TYPE N,
         LV_D3(3)     TYPE N,
         LV_D4(3)     TYPE N,
         LV_TEMP(3)   TYPE N.

  CLEAR : V_POS,
          LV_TEXT1,
          LV_TEXT2,
          LV_TEXT3,
          LV_TEXT4,
          LV_TEXT5.

* Concatenate vendor code and vendor name
  PERFORM APPEND_FIELDCAT USING 'BREIF'
                                 SPACE
                                 SPACE
                                 TEXT-T25
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

* Vendor
  PERFORM APPEND_FIELDCAT USING 'LIFNR'
                                'LFA1'
                                'LIFNR'
                                 TEXT-T01
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* Vendor Name
  PERFORM APPEND_FIELDCAT USING 'NAME1'
                                'LFA1'
                                'NAME1'
                                 TEXT-T02
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* Document Number
  PERFORM APPEND_FIELDCAT USING 'BELNR'
                                'BSIK'
                                'BELNR'
                                 TEXT-T04
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* Document Type
  PERFORM APPEND_FIELDCAT USING 'BLART'
                                'BSIK'
                                'BLART'
                                 TEXT-T03
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* Invoice Number
  PERFORM APPEND_FIELDCAT USING 'XBLNR'
                                'BSIK'
                                'XBLNR'
                                 TEXT-T05
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

* Document header text
  PERFORM APPEND_FIELDCAT USING 'SGTXT'
                                'BSIK'
                                'SGTXT'
                                 TEXT-T26
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

* Payment Term
  PERFORM APPEND_FIELDCAT USING 'ZBD1T'
                                'BSIK'
                                'ZBD1T'
                                 TEXT-T06
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

* Posting Date
  PERFORM APPEND_FIELDCAT USING 'PSTDT'
                                 SPACE
                                 SPACE
                                 TEXT-T00
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

* Invoice Date
  PERFORM APPEND_FIELDCAT USING 'INVDT'
                                 SPACE
                                 SPACE
                                 TEXT-T07
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  IF R_NETDT EQ GC_TRUE.
* Due Date
    PERFORM APPEND_FIELDCAT USING 'NETDT'
                                   SPACE
                                   SPACE
                                   TEXT-T08
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].
* Due Days
    PERFORM APPEND_FIELDCAT USING 'NETDY'
                                   SPACE
                                   SPACE
                                   TEXT-T10
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].
  ELSE.
* Invoice Days
    PERFORM APPEND_FIELDCAT USING 'INVDY'
                                   SPACE
                                   SPACE
                                   TEXT-T09
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].
  ENDIF.

* Amount in Document Currency
  PERFORM APPEND_FIELDCAT USING 'WRBTR'
                                'BSIK'
                                'WRBTR'
                                 TEXT-T11
                                 SPACE
                                 'WAERS'
                                 SPACE
                                 GT_FIELDCAT[].
* Document Currency
  PERFORM APPEND_FIELDCAT USING 'WAERS'
                                'BSIK'
                                'WAERS'
                                 TEXT-T12
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* Exchange Rate
  PERFORM APPEND_FIELDCAT USING 'KURSF'
                                'BKPF'
                                'KURSF'
                                 TEXT-T18
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* Amount in Local Currency
  PERFORM APPEND_FIELDCAT USING 'DMBTR'
                                'BSIK'
                                'DMBTR'
                                 TEXT-T13
                                 SPACE
                                 'HWAERS'
                                 SPACE
                                 GT_FIELDCAT[].
* Local Currency
  PERFORM APPEND_FIELDCAT USING 'HWAERS'
                                'BSIK'
                                'WAERS'
                                 TEXT-T24
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* Original Amount
  PERFORM APPEND_FIELDCAT USING 'HWBAS'
                                'BSET'
                                'HWBAS'
                                 TEXT-T14
                                 SPACE
                                 'HWAERS'
                                 SPACE
                                 GT_FIELDCAT[].
* VAT
  PERFORM APPEND_FIELDCAT USING 'HWSTE'
                                'BSET'
                                'HWSTE'
                                 TEXT-T15
                                 SPACE
                                 'HWAERS'
                                 SPACE
                                 GT_FIELDCAT[].
* Remain Amount
  PERFORM APPEND_FIELDCAT USING 'REMAIN_AMT'
                                'BSIK'
                                'DMBTR'
                                 TEXT-T16
                                 'X'
                                 'HWAERS'
                                 SPACE
                                 GT_FIELDCAT[].
*************************************************************************************
* 1 Aging
  IF P_D1 EQ 0.
    MOVE TEXT-T23 TO LV_TEXT1.
  ELSE.
    WRITE : P_D1 TO LV_D1 NO-ZERO.
    CONCATENATE TEXT-T19 TEXT-T22 LV_D1 TEXT-T17 INTO LV_TEXT1 SEPARATED BY SPACE.
  ENDIF.
  PERFORM APPEND_FIELDCAT USING 'AMT_1'
                                'BSIK'
                                'DMBTR'
                                 LV_TEXT1
                                'X'
                                 'HWAERS'
                                 SPACE
                                 GT_FIELDCAT[].
*************************************************************************************
* 2 Aging
  IF NOT P_D2 IS INITIAL.
    LV_TEMP = P_D1 + 1.
    WRITE : P_D2    TO LV_D2 NO-ZERO,
            LV_TEMP TO LV_D1 NO-ZERO.
    CONCATENATE TEXT-T19 LV_D1 TEXT-T20 LV_D2 TEXT-T17 INTO LV_TEXT2 SEPARATED BY SPACE.
    PERFORM APPEND_FIELDCAT USING 'AMT_2'
                                  'BSIK'
                                  'DMBTR'
                                   LV_TEXT2
                                  'X'
                                   'HWAERS'
                                   SPACE
                                   GT_FIELDCAT[].
  ELSE.
    LV_TEMP = P_D1 + 1.
    WRITE LV_TEMP TO LV_D1 NO-ZERO.
    CONCATENATE TEXT-T19 TEXT-T21 LV_D1 TEXT-T17 INTO LV_TEXT2 SEPARATED BY SPACE.
    PERFORM APPEND_FIELDCAT USING 'AMT_2'
                                  'BSIK'
                                  'DMBTR'
                                   LV_TEXT2
                                  'X'
                                   'HWAERS'
                                   SPACE
                                   GT_FIELDCAT[].
  ENDIF.
*************************************************************************************
* 3 Aging
  IF NOT P_D3 IS INITIAL.
    LV_TEMP = P_D2 + 1.
    WRITE : P_D3    TO LV_D3 NO-ZERO,
            LV_TEMP TO LV_D2 NO-ZERO.
    CONCATENATE TEXT-T19 LV_D2 TEXT-T20 LV_D3 TEXT-T17 INTO LV_TEXT3 SEPARATED BY SPACE.
    PERFORM APPEND_FIELDCAT USING 'AMT_3'
                                  'BSIK'
                                  'DMBTR'
                                   LV_TEXT3
                                  'X'
                                   'HWAERS'
                                   SPACE
                                   GT_FIELDCAT[].
  ELSE.
    LV_TEMP = P_D2 + 1.
    WRITE LV_TEMP TO LV_D2 NO-ZERO.
    CONCATENATE TEXT-T19 TEXT-T21 LV_D2 TEXT-T17 INTO LV_TEXT3 SEPARATED BY SPACE.
    PERFORM APPEND_FIELDCAT USING 'AMT_3'
                                  'BSIK'
                                  'DMBTR'
                                   LV_TEXT3
                                  'X'
                                   'HWAERS'
                                   SPACE
                                   GT_FIELDCAT[].
  ENDIF.
*************************************************************************************
* 4 Aging
  IF P_D4 IS INITIAL.
    IF NOT P_D3 IS INITIAL.
      LV_TEMP = P_D3 + 1.
      WRITE LV_TEMP TO LV_D3 NO-ZERO.
      CONCATENATE TEXT-T19 TEXT-T21 LV_D3 TEXT-T17 INTO LV_TEXT4 SEPARATED BY SPACE.
      PERFORM APPEND_FIELDCAT USING 'AMT_4'
                                  'BSIK'
                                  'DMBTR'
                                   LV_TEXT4
                                  'X'
                                   'HWAERS'
                                   SPACE
                                   GT_FIELDCAT[].
    ENDIF.
  ELSE.
    LV_TEMP = P_D3 + 1.
    WRITE : P_D4    TO LV_D4 NO-ZERO,
            LV_TEMP TO LV_D3 NO-ZERO.
    CONCATENATE TEXT-T19 LV_D3 TEXT-T20 LV_D4 TEXT-T17 INTO LV_TEXT4 SEPARATED BY SPACE.
    PERFORM APPEND_FIELDCAT USING 'AMT_4'
                                  'BSIK'
                                  'DMBTR'
                                   LV_TEXT4
                                  'X'
                                   'HWAERS'
                                   SPACE
                                   GT_FIELDCAT[].

*************************************************************************************
* 5 Aging
    LV_TEMP = P_D4 + 1.
    WRITE LV_TEMP TO LV_D4 NO-ZERO.
    CONCATENATE TEXT-T19 TEXT-T21 LV_D4 TEXT-T17 INTO LV_TEXT5 SEPARATED BY SPACE.
    PERFORM APPEND_FIELDCAT USING 'AMT_5'
                                  'BSIK'
                                  'DMBTR'
                                   LV_TEXT5
                                  'X'
                                   'HWAERS'
                                   SPACE
                                   GT_FIELDCAT[].
  ENDIF.
*************************************************************************************
*************************************************************************************
  "Add by Wantanee 20131207
* IO number
  PERFORM APPEND_FIELDCAT USING 'AUFNR'
                                'AUFK'
                                'AUFNR'
                                 'IO Number'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* IO Description
  PERFORM APPEND_FIELDCAT USING 'KTEXT'
                                'AUFK'
                                'KTEXT'
                                 'IO Description'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  "End Add by Wantanee 20131207
  "Add by Wantanee 20190123
* User Name
  PERFORM APPEND_FIELDCAT USING 'USNAM'
                                'BKPF'
                                'USNAM'
                                 'User Name'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  "End Add by Wantanee 20190123
ENDFORM.                    " BUILD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0630   text
*      -->P_0631   text
*      -->P_0632   text
*      -->P_TEXT_T13  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM APPEND_FIELDCAT USING   P_FIELD   "Field name
                             P_REFTABLE"Reference Table name
                             P_REFFIELD"Reference Field name
                             P_COLTXT  "Col Text(for specify)
                             P_DOSUM   "Sum total
                             P_CFIELDNAME  "  currency
                             P_NO_ZERO     " no zero
                             P_IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

  DATA: WA_INFIELDCAT   TYPE SLIS_FIELDCAT_ALV,
        V_COLTXT_LENGTH TYPE I.

  ADD 1 TO V_POS.

  WA_INFIELDCAT-FIELDNAME     = P_FIELD.
  WA_INFIELDCAT-REF_TABNAME   = P_REFTABLE.
  WA_INFIELDCAT-REF_FIELDNAME = P_REFFIELD.
  WA_INFIELDCAT-COL_POS       = V_POS .
  WA_INFIELDCAT-DO_SUM        = P_DOSUM.

  IF NOT P_NO_ZERO IS INITIAL .
    WA_INFIELDCAT-NO_ZERO = P_NO_ZERO.
  ENDIF.
  IF NOT P_CFIELDNAME IS INITIAL .
    WA_INFIELDCAT-CFIELDNAME = P_CFIELDNAME .
  ENDIF.

*If we need to specify text ,don't need to derive from data dictionary
*program will check length and define width of the colum
  IF NOT P_COLTXT IS INITIAL.
    V_COLTXT_LENGTH = STRLEN( P_COLTXT ).

    IF V_COLTXT_LENGTH > 20.
      WA_INFIELDCAT-DDICTXT = 'L'."Long text
      WA_INFIELDCAT-SELTEXT_L = P_COLTXT.
    ELSEIF V_COLTXT_LENGTH > 10.
      WA_INFIELDCAT-DDICTXT = 'M'."Medium Text
      WA_INFIELDCAT-SELTEXT_M = P_COLTXT.
    ELSE.
      WA_INFIELDCAT-DDICTXT = 'S'."Short Text
      WA_INFIELDCAT-SELTEXT_S = P_COLTXT.
    ENDIF.
    WA_INFIELDCAT-REPTEXT_DDIC = P_COLTXT  .
  ENDIF.
  APPEND WA_INFIELDCAT TO P_IT_FIELDCAT.

ENDFORM.                    " APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  GET_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_RANGE .
  IF NOT P_NORMAL IS INITIAL.
    APPEND INITIAL LINE TO R_BSTAT ASSIGNING FIELD-SYMBOL(<LFS_BSTAT>).
    <LFS_BSTAT>-SIGN   = 'I'.
    <LFS_BSTAT>-OPTION = 'NE'.
    <LFS_BSTAT>-LOW    = 'S'.
    APPEND INITIAL LINE TO R_BSTAT ASSIGNING <LFS_BSTAT>.
    <LFS_BSTAT>-LOW    = 'V'.
    APPEND INITIAL LINE TO R_BSTAT ASSIGNING <LFS_BSTAT>.
    <LFS_BSTAT>-LOW    = 'Z'.
    UNASSIGN <LFS_BSTAT>.

* for special GL
    S_UMSKZ-SIGN   =  'I'.
    S_UMSKZ-OPTION = 'EQ'.
    APPEND S_UMSKZ.
    CLEAR  S_UMSKZ.
  ENDIF.
ENDFORM.                    " GET_RANGE
*&---------------------------------------------------------------------*
*&      Form  MAP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAP_DATA .
  DATA : LV_ZFBDT      TYPE DZFBDT,
         LV_ZBD1T      TYPE DZBD1T,
         LV_REMAIN     TYPE DMBTR,
         LV_KURSF      TYPE KURSF,
         LV_NETDY      TYPE P,
         LV_INVDY      TYPE P,
         LV_EARLIEST   TYPE C,
         LV_REMAIN_DOC TYPE WRBTR.

  DATA : LV_VCOD TYPE LIFNR,
         LV_VNAM TYPE NAME1.

  DATA: LV_USNAM TYPE BKPF-USNAM.

  CLEAR : LV_KURSF,
          LV_NETDY,
          LV_INVDY.

  LOOP AT GT_BSIK INTO GW_BSIK.
    CLEAR: LV_USNAM.
    READ TABLE GT_VENDOR INTO GW_VENDOR WITH KEY LIFNR = GW_BSIK-LIFNR.
    IF SY-SUBRC = 0.
** filter for reconcile account
*      IF gw_vendor-akont IN s_akont.
*        gw_output-name1 = gw_vendor-name1.
*        CONCATENATE gw_vendor-lifnr '-' gw_vendor-name1 INTO gw_output-breif.
*      ELSE.
*        CONTINUE.
*      ENDIF.
** filter for vendor group
      GW_OUTPUT-NAME1 = GW_VENDOR-NAME1.
      IF GW_VENDOR-KTOKK IN S_KTOKK.

      ELSE.
        CONTINUE.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.
* get due date
    CALL FUNCTION 'MRM_PAYMENT_TERMS_GET'
      EXPORTING
        IF_ZTERM = GW_BSIK-ZTERM
        IF_BLDAT = GW_BSIK-BLDAT
        IF_BUDAT = GW_BSIK-BUDAT
        IF_ZFBDT = GW_BSIK-ZFBDT
      IMPORTING
        EF_ZFBDT = LV_ZFBDT
        EF_ZBD1T = LV_ZBD1T.

    IF SY-SUBRC = 0.
      GW_OUTPUT-NETDT = LV_ZFBDT + LV_ZBD1T.
*       gw_output-netdy = p_datum - gw_output-netdt.

      CALL FUNCTION '/SDF/CMO_DATETIME_DIFFERENCE'
        EXPORTING
          DATE1            = P_DATUM
          DATE2            = GW_OUTPUT-NETDT
        IMPORTING
          DATEDIFF         = LV_NETDY
          EARLIEST         = LV_EARLIEST
        EXCEPTIONS
          INVALID_DATETIME = 1
          OTHERS           = 2.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        IF LV_EARLIEST = '1'.
*          gw_output-netdy = gw_output-netdy * -1.
          LV_NETDY = LV_NETDY * -1.
        ENDIF.
      ENDIF.


    ENDIF.

*    gw_output-invdy = p_datum - gw_bsik-bldat.
    CALL FUNCTION '/SDF/CMO_DATETIME_DIFFERENCE'
      EXPORTING
        DATE1            = P_DATUM
        DATE2            = GW_BSIK-BLDAT
      IMPORTING
*       datediff         = gw_output-invdy
        DATEDIFF         = LV_INVDY
        EARLIEST         = LV_EARLIEST
      EXCEPTIONS
        INVALID_DATETIME = 1
        OTHERS           = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      IF LV_EARLIEST = '1'.
*        gw_output-invdy = gw_output-invdy * -1.
        LV_INVDY = LV_INVDY * -1.
      ENDIF.
    ENDIF.
* check for Dr/Cr Indicator
    IF GW_BSIK-SHKZG EQ 'S'.
      GW_BSIK-WRBTR = GW_BSIK-WRBTR * -1.
      GW_BSIK-DMBTR = GW_BSIK-DMBTR * -1.
    ENDIF.
    IF GW_BSIK-LIFNR(2) EQ 'OT'.
      SELECT NAME1
        INTO LV_VNAM
        FROM BSEC
       WHERE BELNR EQ GW_BSIK-BELNR
         AND GJAHR EQ GW_BSIK-GJAHR.
        IF SY-SUBRC EQ 0.
          CONCATENATE GW_BSIK-LIFNR '-' LV_VNAM INTO GW_OUTPUT-BREIF.
          GW_OUTPUT-NAME1 = LV_VNAM.
        ENDIF.
      ENDSELECT.
    ENDIF.
    MOVE : GW_BSIK-LIFNR TO GW_OUTPUT-LIFNR,
           GW_BSIK-BELNR TO GW_OUTPUT-BELNR,
           GW_BSIK-BLART TO GW_OUTPUT-BLART,
           GW_BSIK-XBLNR TO GW_OUTPUT-XBLNR,
           GW_BSIK-SGTXT TO GW_OUTPUT-SGTXT,
           GW_BSIK-BUDAT TO GW_OUTPUT-PSTDT,
           GW_BSIK-BLDAT TO GW_OUTPUT-INVDT,
           GW_BSIK-WRBTR TO GW_OUTPUT-WRBTR,
           GW_BSIK-WAERS TO GW_OUTPUT-WAERS,
           GW_BSIK-DMBTR TO GW_OUTPUT-DMBTR,
           GW_BSIK-ZBD1T TO GW_OUTPUT-ZBD1T,
           GV_HWAERS     TO GW_OUTPUT-HWAERS.

    "Add by Wantanee 20131207.

    IF GW_BSIK-UMSKZ EQ 'T'.
      PERFORM GET_IO USING GW_BSIK-SGTXT CHANGING GW_OUTPUT-AUFNR GW_OUTPUT-KTEXT.
    ELSE.
      GW_OUTPUT-AUFNR = ''.
      GW_OUTPUT-KTEXT = ''.
    ENDIF.
    "End Add by Wantanee 20131207


    IF NOT R_NETDT IS INITIAL.
      IF NOT S_NETDT[] IS INITIAL.
        CHECK GW_OUTPUT-NETDT IN S_NETDT.
      ENDIF.
    ELSE.
      IF NOT S_INVDT[] IS INITIAL.
        CHECK GW_OUTPUT-INVDT IN S_INVDT.
      ENDIF.
    ENDIF.

    IF GW_OUTPUT-WRBTR NE 0.
      LV_KURSF = GW_OUTPUT-DMBTR / GW_OUTPUT-WRBTR.
      MOVE LV_KURSF TO GW_OUTPUT-KURSF.
*      gw_output-kursf = gw_output-dmbtr / gw_output-wrbtr.
    ENDIF.
* Begin of change by IMS 300000167
*    READ TABLE gt_bset INTO gw_bset WITH KEY bukrs = gw_bsik-bukrs
*                                             belnr = gw_bsik-belnr
*                                             gjahr = gw_bsik-gjahr .
*                                             buzei = gw_bsik-buzei.
*    IF sy-subrc = 0.
** 300000167: Begin of insertion
*      IF gw_bsik-shkzg EQ 'S'.
*        gw_output-hwbas = gw_bset-hwbas * -1.
*        gw_output-hwste = gw_bset-hwste * -1.
*      ELSE.
** 300000167: End of insertion
*        gw_output-hwbas = gw_bset-hwbas.
*        gw_output-hwste = gw_bset-hwste.
** 300000167: Begin of insertion
*      ENDIF.
*    ELSE.
** 300000167: Begin of comment -> gw_bsik-dmbtr is already check dr/ cr indicator
**      IF gw_bsik-shkzg EQ 'S'.
**        gw_output-hwbas = gw_bsik-dmbtr * -1.
**      ELSE.
*      gw_output-hwbas = gw_bsik-dmbtr.
**      ENDIF.
** 300000167: End of comment
*      gw_output-hwste = 0.
** 300000167: End of insertion
*    ENDIF.
    LOOP AT GT_BSET INTO GW_BSET WHERE  BUKRS = GW_BSIK-BUKRS
                                  AND   BELNR = GW_BSIK-BELNR
                                  AND   GJAHR = GW_BSIK-GJAHR .
* read data from BSET only once per document
      IF GW_BSET-FLAG_READ IS INITIAL.
        IF GW_BSIK-SHKZG EQ 'S'.
          GW_OUTPUT-HWBAS = GW_BSET-HWBAS * -1.
          GW_OUTPUT-HWSTE = GW_BSET-HWSTE * -1.
        ELSE.
          GW_OUTPUT-HWBAS = GW_BSET-HWBAS.
          GW_OUTPUT-HWSTE = GW_BSET-HWSTE.
        ENDIF.
        GW_BSET-FLAG_READ = 'X' .
        MODIFY GT_BSET FROM GW_BSET TRANSPORTING FLAG_READ .
      ELSE.
        CLEAR : GW_OUTPUT-HWBAS , GW_OUTPUT-HWSTE.
      ENDIF.
    ENDLOOP.
    IF SY-SUBRC NE 0 .  " not found from BSET
      GW_OUTPUT-HWBAS = GW_BSIK-DMBTR.
      GW_OUTPUT-HWSTE = 0.
    ENDIF.
* End of change by IMS 300000167

* get remain amount for Partial
    CLEAR : LV_REMAIN,
            LV_REMAIN_DOC.
    LOOP AT GT_BSIK2 INTO GW_BSIK2 WHERE BUKRS = GW_BSIK-BUKRS
                                     AND REBZG = GW_BSIK-BELNR
                                     AND REBZJ = GW_BSIK-GJAHR.
* Start of modify IMS#300000167 to add sign for amount.
      IF GW_BSIK2-SHKZG EQ 'H'.
        GW_BSIK2-DMBTR = GW_BSIK2-DMBTR * -1.
        GW_BSIK2-WRBTR = GW_BSIK2-WRBTR * -1.
      ENDIF.
* End of modify IMS#300000167 to add sign for amount.
      LV_REMAIN     = LV_REMAIN + GW_BSIK2-DMBTR.
      LV_REMAIN_DOC = LV_REMAIN_DOC + GW_BSIK2-WRBTR.
      GW_BSIK2-FLAG = 'X'.
      MODIFY GT_BSIK2 FROM GW_BSIK2.
      CLEAR GW_BSIK2.
    ENDLOOP.
    GW_OUTPUT-REMAIN_AMT     = GW_OUTPUT-DMBTR - LV_REMAIN.
    GW_OUTPUT-REMAIN_AMT_DOC = GW_OUTPUT-WRBTR - LV_REMAIN_DOC.
* Raking Aging Amount
    IF R_NETDT EQ GC_TRUE.        "due date
      IF GV_FLAG EQ GC_TRUE.      "document currency
        PERFORM RAKING_AMOUNT USING    GW_OUTPUT-REMAIN_AMT_DOC
                                       LV_NETDY
                              CHANGING GW_OUTPUT-AMT_1
                                       GW_OUTPUT-AMT_2
                                       GW_OUTPUT-AMT_3
                                       GW_OUTPUT-AMT_4
                                       GW_OUTPUT-AMT_5.
      ELSE.                       "local currency
        PERFORM RAKING_AMOUNT USING    GW_OUTPUT-REMAIN_AMT
                                       LV_NETDY
                              CHANGING GW_OUTPUT-AMT_1
                                       GW_OUTPUT-AMT_2
                                       GW_OUTPUT-AMT_3
                                       GW_OUTPUT-AMT_4
                                       GW_OUTPUT-AMT_5.

      ENDIF.
    ELSE.                           "invoice date
      IF GV_FLAG EQ GC_TRUE.        "document currency
        PERFORM RAKING_AMOUNT USING    GW_OUTPUT-REMAIN_AMT_DOC
                                       LV_INVDY
                              CHANGING GW_OUTPUT-AMT_1
                                       GW_OUTPUT-AMT_2
                                       GW_OUTPUT-AMT_3
                                       GW_OUTPUT-AMT_4
                                       GW_OUTPUT-AMT_5.
      ELSE.                         "local currency
        PERFORM RAKING_AMOUNT USING    GW_OUTPUT-REMAIN_AMT
                                       LV_INVDY
                              CHANGING GW_OUTPUT-AMT_1
                                       GW_OUTPUT-AMT_2
                                       GW_OUTPUT-AMT_3
                                       GW_OUTPUT-AMT_4
                                       GW_OUTPUT-AMT_5.
      ENDIF.

    ENDIF.
    GW_OUTPUT-BUKRS = GW_BSIK-BUKRS.
    GW_OUTPUT-GJAHR = GW_BSIK-GJAHR.

    MOVE : LV_INVDY TO GW_OUTPUT-INVDY,
           LV_NETDY TO GW_OUTPUT-NETDY.

    "Add by Wantanee 20190123
    SELECT SINGLE USNAM
    INTO LV_USNAM
    FROM BKPF
    WHERE BUKRS EQ P_BUKRS
      AND BELNR EQ GW_OUTPUT-BELNR
      AND GJAHR EQ GW_OUTPUT-GJAHR.
    GW_OUTPUT-USNAM = LV_USNAM.
    "End Add by wantanee 20190123

    COLLECT GW_OUTPUT INTO GT_OUTPUT.
*    APPEND gw_output TO gt_output.
    CLEAR GW_OUTPUT.
    CLEAR : LV_KURSF,
            LV_ZBD1T,
            LV_NETDY,
            LV_INVDY.
  ENDLOOP.

*  DELETE gt_output WHERE blart IN r_partial.

  CLEAR GW_BSIK2.
*  LOOP AT gt_bsik2 INTO gw_bsik2 WHERE flag = 'X'.
*    READ TABLE gt_output INTO gw_output
*                         WITH KEY bukrs = gw_bsik2-bukrs
*                                  belnr = gw_bsik2-belnr
*                                  gjahr = gw_bsik2-gjahr
*                                  lifnr = gw_bsik2-lifnr.
*    IF sy-subrc = 0.
*      DELETE gt_output INDEX sy-tabix.
*    ENDIF.
*  ENDLOOP.

*  DELETE gt_output WHERE remain_amt = 0.

ENDFORM.                    " MAP_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_FLAG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_FLAG .
  DATA : LV_LINE TYPE N.
  CLEAR GV_FLAG.

  DELETE ADJACENT DUPLICATES FROM S_WAERS
                  COMPARING  LOW.

* one currency == document currency == gv_flag = 'X'
  DESCRIBE TABLE S_WAERS LINES LV_LINE.
  IF LV_LINE EQ 1.
    GV_FLAG = GC_TRUE.
  ENDIF.

ENDFORM.                    " CHECK_FLAG
*&---------------------------------------------------------------------*
*&      Form  RAKING_AMOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GW_BSIK_DMBTR  text
*      -->P_GW_NETDY       text
*      <--P_GW_OUTPUT_AMT_1  text
*      <--P_GW_OUTPUT_AMT_2  text
*      <--P_GW_OUTPUT_AMT_3  text
*      <--P_GW_OUTPUT_AMT_4  text
*----------------------------------------------------------------------*
FORM RAKING_AMOUNT  USING    P_AMOUNT
                             P_DAY
                    CHANGING P_AMT_1
                             P_AMT_2
                             P_AMT_3
                             P_AMT_4
                             P_AMT_5.
  IF P_DAY IN R_DATE1.
    P_AMT_1 = P_AMOUNT.
  ELSEIF P_DAY IN R_DATE2.
    P_AMT_2 = P_AMOUNT.
  ELSEIF P_DAY IN R_DATE3.
    P_AMT_3 = P_AMOUNT.
  ELSEIF P_DAY IN R_DATE4.
    P_AMT_4 = P_AMOUNT.
  ELSEIF P_DAY IN R_DATE5.
    P_AMT_5 = P_AMOUNT.
  ENDIF.

ENDFORM.                    " RAKING_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  CHECK_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_DAY .
  IF NOT P_D1 IS INITIAL.
    APPEND INITIAL LINE TO R_DATE1 ASSIGNING FIELD-SYMBOL(<LFS_DATE1>).
    <LFS_DATE1>-SIGN   = 'I'.
    <LFS_DATE1>-OPTION = 'LE'.
    <LFS_DATE1>-LOW    = P_D1.
  ELSE.
    APPEND INITIAL LINE TO R_DATE1 ASSIGNING <LFS_DATE1>.
    <LFS_DATE1>-SIGN   = 'I'.
    <LFS_DATE1>-OPTION = 'LE'.
    <LFS_DATE1>-LOW    = 0.
  ENDIF.

  IF NOT P_D2 IS INITIAL .
    APPEND INITIAL LINE TO R_DATE2 ASSIGNING FIELD-SYMBOL(<LFS_DATE2>).
    <LFS_DATE2>-SIGN   = 'I'.
    <LFS_DATE2>-OPTION = 'BT'.
    <LFS_DATE2>-LOW    = P_D1 + 1.
    <LFS_DATE2>-HIGH   = P_D2.
  ELSE.
    APPEND INITIAL LINE TO R_DATE2 ASSIGNING <LFS_DATE2>.
    <LFS_DATE2>-SIGN   = 'I'.
    <LFS_DATE2>-OPTION = 'GT'.
    <LFS_DATE2>-LOW    = P_D2.
  ENDIF.

  IF NOT P_D3 IS INITIAL .
    APPEND INITIAL LINE TO R_DATE3 ASSIGNING FIELD-SYMBOL(<LFS_DATE3>).
    <LFS_DATE3>-SIGN   = 'I'.
    <LFS_DATE3>-OPTION = 'BT'.
    <LFS_DATE3>-LOW    = P_D2 + 1.
    <LFS_DATE3>-HIGH   = P_D3.
  ELSE.
    APPEND INITIAL LINE TO R_DATE3 ASSIGNING <LFS_DATE3>.
    <LFS_DATE3>-SIGN   = 'I'.
    <LFS_DATE3>-OPTION = 'GT'.
    <LFS_DATE3>-LOW    = P_D2.
  ENDIF.

  IF NOT P_D4 IS INITIAL .
    APPEND INITIAL LINE TO R_DATE4 ASSIGNING FIELD-SYMBOL(<LFS_DATE4>).
    <LFS_DATE4>-SIGN   = 'I'.
    <LFS_DATE4>-OPTION = 'BT'.
    <LFS_DATE4>-LOW    = P_D3 + 1.
    <LFS_DATE4>-HIGH   = P_D4.
  ELSE.
    APPEND INITIAL LINE TO R_DATE4 ASSIGNING <LFS_DATE4>.
    <LFS_DATE4>-SIGN   = 'I'.
    <LFS_DATE4>-OPTION = 'GT'.
    <LFS_DATE4>-LOW    = P_D3.
  ENDIF.

  IF NOT P_D4 IS INITIAL .
    APPEND INITIAL LINE TO R_DATE5  ASSIGNING FIELD-SYMBOL(<LFS_DATE5>).
    <LFS_DATE5>-SIGN   = 'I'.
    <LFS_DATE5>-OPTION = 'GT'.
    <LFS_DATE5>-LOW    = P_D4.
  ENDIF.

ENDFORM.                    " CHECK_DAY
*&---------------------------------------------------------------------*
*&      Form  GET_CURRENCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_CURRENCY .

  CLEAR GV_HWAERS.

  SELECT SINGLE WAERS
  INTO GV_HWAERS
  FROM T001
  WHERE BUKRS = P_BUKRS.

ENDFORM.                    " GET_CURRENCY
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*----------------------------------------------------------------------*
FORM BUILD_EVENT  USING  P_GTYP_EVENT TYPE SLIS_T_EVENT.
  FIELD-SYMBOLS <FS_EVENTS> LIKE LINE OF P_GTYP_EVENT.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    IMPORTING
      ET_EVENTS       = P_GTYP_EVENT
    EXCEPTIONS
      LIST_TYPE_WRONG = 0
      OTHERS          = 0.

  LOOP AT P_GTYP_EVENT ASSIGNING <FS_EVENTS>
                    WHERE NAME = 'TOP_OF_PAGE'.
    <FS_EVENTS>-FORM = 'REPORT_HEADER'.
  ENDLOOP.

ENDFORM.                    " BUILD_EVENT

*&---------------------------------------------------------------------*
*&      Form  REPORT_HEADER
*&---------------------------------------------------------------------*

FORM REPORT_HEADER.
  DATA : LT_LISTHEAD TYPE SLIS_T_LISTHEADER,
         LW_LISTLINE TYPE SLIS_LISTHEADER.

  DATA : LV_TEXT(256)  TYPE C,
         LV_DATE(10)   TYPE C,
         LV_TIME(8)    TYPE C,
         LV_SIGN(7)    TYPE C,
         LV_OPTION(12) TYPE C,
         LV_LOW(10)    TYPE C,
         LV_HIGH(20)   TYPE C,
         LV_COUNT      TYPE I.

  DEFINE DEF_LIST_HEAD.
    clear lw_listline.
    lw_listline-typ  = &1.
    lw_listline-key  = &2.
    lw_listline-info = &3.
    append lw_listline to lt_listhead.
  END-OF-DEFINITION.
* Company Name
  CLEAR LV_TEXT.
  BREAK ATTAHORNL.
  CONCATENATE TEXT-H02 TEXT-H01 INTO LV_TEXT SEPARATED BY SPACE.
  DEF_LIST_HEAD 'H' '' LV_TEXT .

* Report Title
  CLEAR LV_TEXT.
  IF R_NETDT EQ GC_TRUE.
    CONCATENATE TEXT-H03 TEXT-H04 INTO LV_TEXT SEPARATED BY SPACE .
  ELSE.
    CONCATENATE TEXT-H03 TEXT-H05 INTO LV_TEXT SEPARATED BY SPACE .
  ENDIF.
  DEF_LIST_HEAD 'S' '' LV_TEXT  .

* Currency
  CLEAR LV_TEXT.
  LV_TEXT = TEXT-H06.
  LOOP AT S_WAERS.
    CONCATENATE LV_TEXT S_WAERS-LOW INTO LV_TEXT SEPARATED BY SPACE.
  ENDLOOP.
  DEF_LIST_HEAD 'S' '' LV_TEXT  .

* Date & Time
  CLEAR : LV_TEXT,
          LV_DATE,
          LV_TIME.

  WRITE SY-DATUM TO LV_DATE USING EDIT MASK GC_MASK_DATE.
  WRITE SY-UZEIT TO LV_TIME USING EDIT MASK GC_MASK_TIME.
  CONCATENATE TEXT-H07 LV_DATE TEXT-H08 LV_TIME INTO LV_TEXT SEPARATED BY SPACE.
  DEF_LIST_HEAD 'S' '' LV_TEXT  .

* As of Date
  CLEAR LV_TEXT.
  CLEAR LV_DATE.
  WRITE P_DATUM TO LV_DATE USING EDIT MASK GC_MASK_DATE.
  CONCATENATE TEXT-H09 LV_DATE INTO LV_TEXT SEPARATED BY SPACE.
  DEF_LIST_HEAD 'S' '' LV_TEXT  .

* Fiscal Year
  CLEAR LV_TEXT.

*-->> Begin Remark Parinya25Jan2012
*-> Search key : 20120131
*  CONCATENATE text-h10 p_gjahr INTO lv_text SEPARATED BY space.
*  def_list_head 'S' '' lv_text .
*-->> End Remark Parinya25Jan2012

* Account group
  LV_COUNT = 0.
  LOOP AT S_KTOKK.
    CLEAR: LV_TEXT,
           LV_SIGN,
           LV_OPTION,
           LV_LOW,
           LV_HIGH.
    LV_COUNT = LV_COUNT + 1.
    IF S_KTOKK-SIGN EQ 'I'.
      LV_SIGN = 'Include'.
    ELSEIF S_KTOKK-SIGN EQ  'E' .
      LV_SIGN = 'Exclude'.
    ENDIF.
    LV_OPTION = S_KTOKK-OPTION.
    LV_LOW  = S_KTOKK-LOW.
    IF S_KTOKK-HIGH IS NOT INITIAL.
      CONCATENATE 'TO' S_KTOKK-HIGH INTO LV_HIGH SEPARATED BY SPACE.
    ENDIF.
    IF LV_COUNT > 1.
      CONCATENATE '_____________' LV_SIGN LV_OPTION  LV_LOW LV_HIGH INTO LV_TEXT SEPARATED BY SPACE.
    ELSE.
      CONCATENATE TEXT-H11 LV_SIGN LV_OPTION  LV_LOW LV_HIGH INTO LV_TEXT SEPARATED BY SPACE.
    ENDIF.
    DEF_LIST_HEAD 'S' '' LV_TEXT.
  ENDLOOP.
* Reconciliation acct
  LV_COUNT = 0.
  LOOP AT S_AKONT.
    CLEAR: LV_TEXT,
           LV_SIGN,
           LV_OPTION,
           LV_LOW,
           LV_HIGH.
    LV_COUNT = LV_COUNT + 1.
    IF S_AKONT-SIGN EQ 'I'.
      LV_SIGN = 'Include'.
    ELSEIF S_AKONT-SIGN EQ  'E' .
      LV_SIGN = 'Exclude'.
    ENDIF.
    LV_OPTION = S_AKONT-OPTION.
    LV_LOW  = S_AKONT-LOW.
    IF S_AKONT-HIGH IS NOT INITIAL.
      CONCATENATE 'TO' S_AKONT-HIGH INTO LV_HIGH SEPARATED BY SPACE.
    ENDIF.
    IF LV_COUNT > 1.
      CONCATENATE '_____________' LV_SIGN LV_OPTION  LV_LOW LV_HIGH INTO LV_TEXT SEPARATED BY SPACE.
    ELSE.
      CONCATENATE TEXT-H12 LV_SIGN LV_OPTION  LV_LOW LV_HIGH INTO LV_TEXT SEPARATED BY SPACE.
    ENDIF.
    DEF_LIST_HEAD 'S' '' LV_TEXT.
  ENDLOOP.
* Vendor
  LV_COUNT = 0.
  LOOP AT S_LIFNR.
    CLEAR: LV_TEXT,
           LV_SIGN,
           LV_OPTION,
           LV_LOW,
           LV_HIGH.
    LV_COUNT = LV_COUNT + 1.
    IF S_LIFNR-SIGN EQ 'I'.
      LV_SIGN = 'Include'.
    ELSEIF S_LIFNR-SIGN EQ  'E' .
      LV_SIGN = 'Exclude'.
    ENDIF.
    LV_OPTION = S_LIFNR-OPTION.
    LV_LOW  = S_LIFNR-LOW.
    IF S_LIFNR-HIGH IS NOT INITIAL.
      CONCATENATE 'TO' S_LIFNR-HIGH INTO LV_HIGH SEPARATED BY SPACE.
    ENDIF.
    IF LV_COUNT > 1.
      CONCATENATE '_______' LV_SIGN LV_OPTION  LV_LOW LV_HIGH INTO LV_TEXT SEPARATED BY SPACE.
    ELSE.
      CONCATENATE TEXT-H13 LV_SIGN LV_OPTION  LV_LOW LV_HIGH INTO LV_TEXT SEPARATED BY SPACE.
    ENDIF.
    DEF_LIST_HEAD 'S' '' LV_TEXT.
  ENDLOOP.
* Document Number
  LV_COUNT = 0.
  LOOP AT S_BELNR.
    CLEAR: LV_TEXT,
           LV_SIGN,
           LV_OPTION,
           LV_LOW,
           LV_HIGH.
    LV_COUNT = LV_COUNT + 1.
    IF S_BELNR-SIGN EQ 'I'.
      LV_SIGN = 'Include'.
    ELSEIF S_BELNR-SIGN EQ  'E' .
      LV_SIGN = 'Exclude'.
    ENDIF.
    LV_OPTION = S_BELNR-OPTION.
    LV_LOW  = S_BELNR-LOW.
    IF S_BELNR-HIGH IS NOT INITIAL.
      CONCATENATE 'TO' S_BELNR-HIGH INTO LV_HIGH SEPARATED BY SPACE.
    ENDIF.
    IF LV_COUNT > 1.
      CONCATENATE '____________' LV_SIGN LV_OPTION  LV_LOW LV_HIGH INTO LV_TEXT SEPARATED BY SPACE.
    ELSE.
      CONCATENATE TEXT-H14 LV_SIGN LV_OPTION  LV_LOW LV_HIGH INTO LV_TEXT SEPARATED BY SPACE.
    ENDIF.
    DEF_LIST_HEAD 'S' '' LV_TEXT.
  ENDLOOP.

* Normal Item
  CLEAR LV_TEXT.
  IF P_NORMAL IS NOT INITIAL.
    LV_TEXT = 'Normal Item : Include'.
  ELSE.
    LV_TEXT = 'Normal Item : Exclude'.
  ENDIF.
  DEF_LIST_HEAD 'S' '' LV_TEXT.

* Special G/L Indicator
  IF S_UMSKZ IS NOT INITIAL.
    LV_COUNT = 0.
    LOOP AT S_UMSKZ.
      CLEAR: LV_TEXT,
             LV_SIGN,
             LV_OPTION,
             LV_LOW,
             LV_HIGH.
      LV_COUNT = LV_COUNT + 1.
      IF S_UMSKZ-SIGN EQ 'I'.
        LV_SIGN = 'Include'.
      ELSEIF S_UMSKZ-SIGN EQ  'E' .
        LV_SIGN = 'Exclude'.
      ENDIF.
      LV_OPTION = S_UMSKZ-OPTION.
      LV_LOW  = S_UMSKZ-LOW.
      IF S_UMSKZ-HIGH IS NOT INITIAL.
        CONCATENATE 'TO' S_UMSKZ-HIGH INTO LV_HIGH SEPARATED BY SPACE.
      ENDIF.
      IF LV_COUNT > 1.
        CONCATENATE '_______________________' LV_SIGN LV_OPTION  LV_LOW LV_HIGH INTO LV_TEXT SEPARATED BY SPACE.
      ELSE.
        CONCATENATE TEXT-H15 LV_SIGN LV_OPTION  LV_LOW LV_HIGH INTO LV_TEXT SEPARATED BY SPACE.
      ENDIF.
      DEF_LIST_HEAD 'S' '' LV_TEXT.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LT_LISTHEAD
    EXCEPTIONS
      OTHERS             = 0.

ENDFORM.                    " REPORT_HEADER
*&---------------------------------------------------------------------*
*&      Form  GET_GENC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_GENC .

*  DATA : LW_GENC TYPE ZSDS_GEN_C,
*         LT_GENC TYPE STANDARD TABLE OF ZSDS_GEN_C.
*
*  REFRESH : LT_GENC.
*  CLEAR LW_GENC.
*  SELECT *
*  INTO TABLE LT_GENC
*  FROM ZSDS_GEN_C
*  WHERE REPID = GC_REPID.
*
*  LOOP AT LT_GENC INTO LW_GENC.
*    IF LW_GENC-CONST CP GC_PARTIAL.
*      CLEAR R_PARTIAL.
*      R_PARTIAL-SIGN   = 'I'.
*      R_PARTIAL-OPTION = 'EQ'.
*      R_PARTIAL-LOW    = LW_GENC-VALUE.
*      APPEND R_PARTIAL.
*      CLEAR R_PARTIAL.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " GET_GENC
*&---------------------------------------------------------------------*
*&      Form  GET_IO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_IO USING P_SGTXT
            CHANGING P_AUFNR TYPE AUFK-AUFNR
                     P_KTEXT TYPE AUFK-KTEXT.

  DATA : LV_AUFNR(12) TYPE C,
         LV_KTEXT     TYPE AUFK-KTEXT,
         LV_POS       TYPE I,
         LV_OFF       TYPE I.
  DATA: RESULT_TAB TYPE MATCH_RESULT_TAB.
  DATA: RESULT_WA TYPE MATCH_RESULT.

  CLEAR : P_AUFNR,P_KTEXT,LV_AUFNR,LV_POS,LV_OFF.
  CONDENSE P_SGTXT NO-GAPS..


  FIND ALL OCCURRENCES OF REGEX '((COSTING)|(COSTING#))'  IN P_SGTXT
     RESULTS RESULT_TAB.

  LOOP AT RESULT_TAB INTO RESULT_WA.

    LV_POS = RESULT_WA-OFFSET + RESULT_WA-LENGTH.
    IF LV_POS NE 0.
      LV_POS = LV_POS.
      LV_AUFNR = P_SGTXT+LV_POS(6).
      P_AUFNR = LV_AUFNR.
    ENDIF.
  ENDLOOP.

  CLEAR: LV_POS,RESULT_TAB,RESULT_WA.
  IF P_AUFNR EQ ''.

    FIND ALL OCCURRENCES OF REGEX 'IO' IN P_SGTXT
       RESULTS RESULT_TAB.

    LOOP AT RESULT_TAB INTO RESULT_WA.

      LV_POS = RESULT_WA-OFFSET + RESULT_WA-LENGTH.
      IF LV_POS NE 0.
        LV_POS = LV_POS.
        LV_AUFNR = P_SGTXT+LV_POS(8).
        CONCATENATE '0000' LV_AUFNR INTO LV_AUFNR.
        SELECT SINGLE KTEXT
        INTO (LV_KTEXT)
        FROM AUFK
        WHERE AUFNR EQ LV_AUFNR.
        IF LV_KTEXT NE ''.
          P_AUFNR = LV_AUFNR.
          P_KTEXT = LV_KTEXT.
        ENDIF.

      ENDIF.
    ENDLOOP.

  ENDIF.



ENDFORM.                    " GET_GENC

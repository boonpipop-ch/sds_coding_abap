*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0090
*  Creation Date      : 20.03.2023
*  Author             : Jakarin S
*  Add-on ID          : ZMMF007
*  Description        : Receipt Traveler / Expenses Approval Form
*  Purpose            :
*  Copied from        : ZP_REC_EXP_APPROVE
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

REPORT ZSDSMMR0090 MESSAGE-ID ZSDSMM01.
TYPE-POOLS : TRUXS,SLIS,ICON.
*&---------------------------------------------------------------------*
*  DECLARATION
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*  TABLE
*&---------------------------------------------------------------------*
TABLES : EKPO,LFA1.
*&---------------------------------------------------------------------*
*  TYPE
*&---------------------------------------------------------------------*
TYPES : GY_YEAR TYPE I.

TYPES : BEGIN OF GY_RESULT,
          JOBNO         TYPE C LENGTH 12,
          AUFNR         TYPE AUFK-AUFNR,
          NAME1         TYPE C LENGTH 100,
          PRCTR         TYPE PRCTR,
          SUBNA         TYPE C LENGTH 255,
          MBLNR         TYPE MSEG-MBLNR,
          BUDAT         TYPE MKPF-BUDAT,
          EBELN         TYPE EKKO-EBELN,
          BEDAT         TYPE EKKO-BEDAT,
          MATNR         TYPE MARA-MATNR,
          SERNR         TYPE EQUI-SERNR,
          VKGRP         TYPE VBAK-VKGRP,
          VKBUR         TYPE VBAK-VKBUR,
          NETWR         TYPE VBAK-NETWR,
          MAKTX         TYPE MAKT-MAKTX,
          MENGE         TYPE EKPO-MENGE,
          MEINS         TYPE EKPO-MEINS,
          NAMEC         TYPE KNA1-NAME1,
          FECOD         TYPE VIQMFE-FECOD,
          USR05         TYPE AFVU-USR05,
          BANFN         LIKE EKPO-BANFN, "PR NO.
          EBELP         LIKE EKPO-EBELP, " PO Item
          LOEKZ         LIKE EKPO-LOEKZ, " DELETION
          WERKS         LIKE EKPO-WERKS, "PLANT
          VBELN         LIKE EKKN-VBELN,
          KUNNR         LIKE VBAK-KUNNR,
          VGBEL         LIKE VBAK-VGBEL,
          TEXT(20)      TYPE C, " JOB NO. FROM TEXT VBAK
          LIFNR         LIKE EKKO-LIFNR, " ACCOUNT VENDOR
          NAME2(100)    TYPE C,  "Add by Wantanee C1-20110808
          P_SALE(40)    TYPE C,
          AUART         LIKE VBAK-AUART,
          BEZEI         LIKE TVAKT-BEZEI,
          PREPARE(20)   TYPE C,
          VAT(3)        TYPE P DECIMALS 2,
          SUMQTY        LIKE EKPO-MENGE,
          GNO           LIKE EKPO-EBELP,
          ADRNR         LIKE VBPA-ADRNR,
          OBJNR         LIKE VIAUFKS-OBJNR, "modify 211209
          KUNUM         LIKE VIAUFKS-KUNUM, "modify 211209
          CUSNAME_SVO   LIKE KNA1-NAME1, "modify 211209
          ADRNR2        LIKE IHPA-ADRNR, "modify 211209
          AUART2        LIKE VIAUFKS-AUART, "modify 211209
          SVOTXT        LIKE T003P-TXT, "modify 211209
          HEADER_JOB    LIKE ZSDSMMT005-HEADER_JOB, "modify 211209 add type job for svo
          MAUFNR        LIKE AFKO-MAUFNR, "modify 211209 add Job no.(Super Order(SVO))
          AUFNR2        LIKE VIAUFKS-AUFNR, "modify 211209 add Job no.(Super Order(SVO))
          KDAUF         LIKE VIAUFKS-KDAUF, "modify 70110 add S/O for SVO MA in Job no.
          STATUS(10)    TYPE C,
          QMNUM         LIKE AFIH-QMNUM,
          AUFPL         LIKE VIAUFKS-AUFPL,
          FLAG          TYPE C,
          RECRIPT       TYPE C LENGTH 128,
          COST_SHEET    TYPE C LENGTH 128,
          PROJECT       TYPE C LENGTH 128,
          CUSTOMER      TYPE C LENGTH 128,
          AC_CODE       TYPE C LENGTH 128,
          PROFIT_CENTER TYPE C LENGTH 128,
          REMARK        TYPE C LENGTH 128,
          RECEIPT_DATE  TYPE C LENGTH 128,
          WORK_PERIOD   TYPE C LENGTH 128,
          WARRAN_PERIOD TYPE C LENGTH 128,
          ENGINEER      TYPE C LENGTH 128,
          MAT_GROUP     TYPE C LENGTH 128,
          SALE_ORDER    TYPE C LENGTH 128,
          PENALTY       TYPE C LENGTH 128,
          BSTMG         TYPE MSEG-BSTMG,
        END OF GY_RESULT.

TYPES : BEGIN OF GY_HEADER,
          EBELN  TYPE EKPO-EBELN,
          VENDOR TYPE LFA1-LIFNR,
          NAME   TYPE C LENGTH 100,
          MATD   TYPE MSEG-MBLNR,
          SALE   TYPE C LENGTH 40,
          PO     TYPE C LENGTH 10,
          DATE1  TYPE EKKO-BEDAT,
          AUFNR  TYPE AUFK-AUFNR,
*BOI CH02
          ERNAM  TYPE EKKO-ERNAM,
          WAERS  TYPE EKKO-WAERS,
*EOI CH02
        END OF GY_HEADER.

TYPES : BEGIN OF GY_DETAIL,
          VBELN TYPE VBRK-VBELN,
          POSNR TYPE VBRP-POSNR,
          MATNR TYPE VBAP-MATNR,
          UPMAT TYPE VBAP-UPMAT,
          UEPOS TYPE VBAP-UEPOS,
        END OF GY_DETAIL.

TYPES : BEGIN OF GY_MSEG,
          MBLNR      TYPE MSEG-MBLNR,
          MJAHR      TYPE MSEG-MJAHR,
          ZEILE      TYPE MSEG-ZEILE,
          EBELN      TYPE MSEG-EBELN,
          EBELP      TYPE MSEG-EBELP,
          AUFNR      TYPE MSEG-AUFNR,
          BUDAT      TYPE MKPF-BUDAT,
          KDAUF      TYPE MSEG-KDAUF,
          KDPOS      TYPE MSEG-KDPOS,
          SGTXT      TYPE MSEG-SGTXT,
          SAKTO      TYPE MSEG-SAKTO,
          BSTMG      TYPE MSEG-BSTMG,
          PS_PSP_PNR TYPE MSEG-PS_PSP_PNR,
        END OF GY_MSEG.

TYPES : BEGIN OF GY_AUFK,
          AUFNR TYPE AUFK-AUFNR,
          KTEXT TYPE AUFK-KTEXT,
        END OF GY_AUFK.

TYPES : BEGIN OF GY_MAKT,
          MATNR TYPE MAKT-MATNR,
          MAKTX TYPE MAKT-MAKTX,
        END OF GY_MAKT.

TYPES : BEGIN OF GY_EKKO,
          EBELN TYPE EKKO-EBELN,
          BEDAT TYPE EKKO-BEDAT,
          LIFNR TYPE EKKO-LIFNR,
          NAME1 TYPE LFA1-NAME1,
          NAME2 TYPE LFA1-NAME2,
          ADRNR TYPE LFA1-ADRNR,
*BOI CH02
          ERNAM TYPE EKKO-ERNAM,
          WAERS TYPE EKKO-WAERS,
*EOI CH02
        END OF GY_EKKO.

TYPES : BEGIN OF GY_MKPF,
          MBLNR TYPE MKPF-MBLNR,
          BUDAT TYPE MKPF-BUDAT,
        END OF GY_MKPF.

TYPES : BEGIN OF GY_OBJK,
          OBKNR TYPE OBJK-OBKNR,
          MATNR TYPE OBJK-MATNR,
          SERNR TYPE OBJK-SERNR,
        END OF GY_OBJK.

TYPES : BEGIN OF GY_SER01,
          OBKNR   TYPE SER01-OBKNR,
          LIEF_NR TYPE SER01-LIEF_NR,
          POSNR   TYPE SER01-POSNR,
        END OF GY_SER01.

TYPES : BEGIN OF GY_LIPS,
          VBELN TYPE LIPS-VBELN,
          POSNR TYPE LIPS-POSNR,
          PRCTR TYPE LIPS-PRCTR,
          KUNNR TYPE LIKP-KUNNR,
          NAME1 TYPE KNA1-NAME1,
        END OF GY_LIPS.

TYPES : BEGIN OF GY_LIKP,
          VBELN TYPE LIPS-VBELN,
          POSNR TYPE LIPS-POSNR,
          KUNNR TYPE LIKP-KUNNR,
          NAME1 TYPE KNA1-NAME1,
        END OF GY_LIKP.

TYPES : BEGIN OF GY_VBFA,
          VBELV TYPE VBFA-VBELV,
          POSNV TYPE VBFA-POSNV,
          VBELN TYPE VBFA-VBELN,
          POSNN TYPE VBFA-POSNN,
        END OF GY_VBFA.

TYPES : BEGIN OF GY_VBAP,
          VBELN TYPE VBAK-VBELN,
          POSNR TYPE VBAP-POSNR,
          NETWR TYPE VBAP-NETWR,
          MWSBP TYPE VBAP-MWSBP,
          VKGRP TYPE VBAK-VKGRP,
          VKBUR TYPE VBAK-VKBUR,
        END OF GY_VBAP.

TYPES : BEGIN OF GY_IHPA,
          OBJNR TYPE IHPA-OBJNR,
          ADRNR TYPE IHPA-ADRNR,
          NAME1 TYPE KNA1-NAME1,
          NAME2 TYPE KNA1-NAME1,
        END OF GY_IHPA.

TYPES : BEGIN OF GY_EKPO,
          EBELN TYPE EKPO-EBELN,
          EBELP TYPE EKPO-EBELP,
          TXZ01 TYPE EKPO-TXZ01,
          MATNR TYPE EKPO-MATNR,
          MENGE TYPE EKPO-MENGE,
          MEINS TYPE EKPO-MEINS,
          NETWR TYPE EKPO-NETWR,
          BANFN TYPE EKPO-BANFN,
          MATKL TYPE EKPO-MATKL,
          WGBEZ TYPE T023T-WGBEZ,
        END OF GY_EKPO.

TYPES : BEGIN OF GY_EKKN,
          EBELN TYPE EKKN-EBELN,
          EBELP TYPE EKKN-EBELP,
          AUFNR TYPE EKKN-AUFNR,
          PRCTR TYPE EKKN-PRCTR,
        END OF GY_EKKN.

TYPES : BEGIN OF GY_JEST,
          OBJNR TYPE JEST-OBJNR,
        END OF GY_JEST.

TYPES : BEGIN OF GY_VIQMFE,
          QMNUM TYPE VIQMFE-QMNUM,
          FECOD TYPE VIQMFE-FECOD,
        END OF GY_VIQMFE.

TYPES : BEGIN OF GY_AFVU,
          AUFPL TYPE AFVU-AUFPL,
          USR05 TYPE AFVU-USR05,
        END OF GY_AFVU.

TYPES : BEGIN OF GY_MSEG_FROM,
          MBLNR LIKE  MSEG-MBLNR,
          MJAHR LIKE  MSEG-MJAHR,
          ZEILE LIKE  MSEG-ZEILE,
          BWART LIKE  MSEG-BWART,
          AUFNR LIKE  MSEG-AUFNR,
          MATNR LIKE  MSEG-MATNR,
          MAKTX LIKE  MAKT-MAKTX,
          DMBTR LIKE  MSEG-DMBTR,
          SJAHR LIKE  MSEG-SJAHR,
          SMBLN LIKE  MSEG-SMBLN,
          SMBLP LIKE  MSEG-SMBLP,
        END OF GY_MSEG_FROM.

TYPES : BEGIN OF GY_CHECK_PROFIT,
          AUFNR TYPE AUFK-AUFNR,
          SERNR TYPE OBJK-SERNR,
          MATNR TYPE OBJK-MATNR,
          PRCTR TYPE AUFK-PRCTR,
        END OF GY_CHECK_PROFIT.

TYPES : BEGIN OF GY_CHECK_AMOUNT,
          AUFNR  TYPE AUFK-AUFNR,
          RSPOS  TYPE RESBD-RSPOS,
          GPREIS TYPE RESBD-GPREIS,
        END OF GY_CHECK_AMOUNT.

TYPES : BEGIN OF GY_SKAT,
          SAKNR TYPE SKAT-SAKNR,
          TXT20 TYPE SKAT-TXT20,
        END OF GY_SKAT.

TYPES : BEGIN OF GY_KNA1,
          VBELN TYPE VBAK-VBELN,
          NAME1 TYPE KNA1-NAME1,
          NAME2 TYPE KNA1-NAME2,
        END OF GY_KNA1.

TYPES: BEGIN OF TYP_ADRC,
         ADDRNUMBER TYPE ADRC-ADDRNUMBER,
         NAME1      TYPE ADRC-NAME1,
         NAME2      TYPE ADRC-NAME2,

       END OF TYP_ADRC.
*&---------------------------------------------------------------------*
*  VARIABLE
*&---------------------------------------------------------------------*
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_HEADER TYPE TABLE OF GY_HEADER,
       GS_HEADER TYPE GY_HEADER.

DATA : GT_MSEG TYPE TABLE OF GY_MSEG,
       GS_MSEG TYPE GY_MSEG.

DATA : GT_AUFK TYPE TABLE OF GY_AUFK,
       GS_AUFK TYPE GY_AUFK.

DATA : GT_MAKT TYPE TABLE OF GY_MAKT,
       GS_MAKT TYPE GY_MAKT.

DATA : GT_EKKO TYPE TABLE OF GY_EKKO,
       GS_EKKO TYPE GY_EKKO.

DATA : GT_MKPF TYPE TABLE OF GY_MKPF,
       GS_MKPF TYPE GY_MKPF.

DATA : GT_OBJK TYPE TABLE OF GY_OBJK,
       GS_OBJK TYPE GY_OBJK.

DATA : GT_SER01 TYPE TABLE OF GY_SER01,
       GS_SER01 TYPE GY_SER01.

DATA : GT_LIPS TYPE TABLE OF GY_LIPS,
       GS_LIPS TYPE GY_LIPS.

DATA : GT_LIKP TYPE TABLE OF GY_LIKP,
       GS_LIKP TYPE GY_LIKP.

DATA : GT_VBFA TYPE TABLE OF GY_VBFA,
       GS_VBFA TYPE GY_VBFA.

DATA : GT_VBAP TYPE TABLE OF GY_VBAP,
       GS_VBAP TYPE GY_VBAP.

DATA : GT_IHPA TYPE TABLE OF GY_IHPA,
       GS_IHPA TYPE GY_IHPA.

DATA : GT_EKPO TYPE TABLE OF GY_EKPO,
       GS_EKPO TYPE GY_EKPO.

DATA : GT_EKKN TYPE TABLE OF GY_EKKN,
       GS_EKKN TYPE GY_EKKN.

DATA : GT_CAUFVD TYPE TABLE OF CAUFVD,
       GS_CAUFVD TYPE CAUFVD.

DATA : GT_JEST TYPE TABLE OF GY_JEST,
       GS_JEST TYPE GY_JEST.

DATA : GT_VIQMFE TYPE TABLE OF GY_VIQMFE,
       GS_VIQMFE TYPE GY_VIQMFE.

DATA : GT_AFVU TYPE TABLE OF GY_AFVU,
       GS_AFVU TYPE GY_AFVU.

DATA : GT_MSEG_FORM TYPE TABLE OF GY_MSEG_FROM,
       GS_MSEG_FORM TYPE GY_MSEG_FROM.

DATA : GT_RIPW0 TYPE TABLE OF RIPW0,
       GS_RIPW0 TYPE RIPW0.

DATA : GT_CHECK_AMOUNT TYPE TABLE OF GY_CHECK_AMOUNT,
       GS_CHECK_AMOUNT TYPE GY_CHECK_AMOUNT.

DATA : GT_CHECK_PROFIT TYPE TABLE OF GY_CHECK_PROFIT,
       GS_CHECK_PROFIT TYPE GY_CHECK_PROFIT.

DATA : GT_SKAT TYPE TABLE OF GY_SKAT,
       GS_SKAT TYPE GY_SKAT.


DATA: GT_ADRC TYPE STANDARD TABLE OF TYP_ADRC,
      GS_ADRC TYPE TYP_ADRC.


DATA : GT_KNA1 TYPE TABLE OF GY_KNA1,
       GS_KNA1 TYPE GY_KNA1.

DATA : GV_PAGE         TYPE SY-TABIX,
       GV_TOTAL_LINE   TYPE I,
       GV_CHECK_PROFIT TYPE C,
       GV_CHECK_VAT    TYPE C.

DATA : GV_TMP_FILE_PATH LIKE IBIPPARMS-PATH.

DATA : GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_SORT   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT   TYPE SLIS_SORTINFO_ALV.
*BOI CH02
DATA : GV_MNG      TYPE C,
       GV_GM       TYPE C,
       GV_AGM      TYPE C,
       GV_AMD      TYPE C,
       GV_SD       TYPE C,
       GV_MD       TYPE C,
       GV_PRES     TYPE C,
       GV_NAMEMNG  TYPE C LENGTH 70,
       GV_DATEMNG  TYPE C LENGTH 10,
       GV_NAMEGM   TYPE C LENGTH 70,
       GV_NAMEAGM  TYPE C LENGTH 70,
       GV_DATEGM   TYPE C LENGTH 10,
       GV_DATEAGM  TYPE C LENGTH 10,
       GV_NAMEAMD  TYPE C LENGTH 70,
       GV_DATEAMD  TYPE C LENGTH 10,
       GV_NAMESD   TYPE C LENGTH 70,
       GV_DATESD   TYPE C LENGTH 10,
       GV_NAMEMD   TYPE C LENGTH 70,
       GV_DATEMD   TYPE C LENGTH 10,
       GV_NAMEPRES TYPE C LENGTH 70,
       GV_DATEPRES TYPE C LENGTH 10,
       GV_NAMEREQ  TYPE C LENGTH 70,
       GV_DATEREQ  TYPE C LENGTH 10,
       GV_DATUM    TYPE SY-DATUM.

TYPES TTYP_EKPO LIKE GT_EKPO[].
*EOI CH02
*&---------------------------------------------------------------------*
*  RANGES
*&---------------------------------------------------------------------*
RANGES : GR_VBELN FOR EKBE-EBELN.
*&---------------------------------------------------------------------*
*  CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS : GC_TRUE TYPE C      VALUE 'X',
            GC_FORM TYPE STRING VALUE 'ZSDSMM003'.

CONSTANTS : BEGIN OF GC_CON,
              E TYPE C LENGTH 1 VALUE 'E',
              S TYPE C LENGTH 1 VALUE 'S',
              X TYPE C LENGTH 1 VALUE 'X',
              Y TYPE C LENGTH 1 VALUE 'Y',
              L TYPE C LENGTH 1 VALUE 'L',
            END OF GC_CON.
*&---------------------------------------------------------------------*
*  SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT-B01.
  SELECT-OPTIONS: S_EBELN FOR  EKPO-EBELN.  "PO DOC.NUMBER
  PARAMETERS      P_VAT(3)  TYPE P DECIMALS 2 DEFAULT '0.07'. "VAT
  PARAMETERS      P_PRE(20) TYPE C. "SALE
  PARAMETERS      P_WERKS   LIKE EKPO-WERKS DEFAULT '1000' OBLIGATORY. "Plant
SELECTION-SCREEN END OF BLOCK B01.

*&---------------------------------------------------------------------*
*  INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.

*&---------------------------------------------------------------------*
*  AT SELECTION-SCREEN OUTPUT
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*&---------------------------------------------------------------------*
*  AT SELECTION-SCREEN OUTPUT
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

*&---------------------------------------------------------------------*
*  START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_GET_DATA.
*&---------------------------------------------------------------------*
*  END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  IF GV_CHECK_VAT EQ SPACE.
    IF GT_RESULT[] IS NOT INITIAL.
      PERFORM F_CALL_FORM.
    ELSE.
      MESSAGE S000 DISPLAY LIKE GC_CON-E.
    ENDIF.
  ELSEIF GV_CHECK_VAT EQ GC_CON-X.
    MESSAGE S001 WITH TEXT-101 DISPLAY LIKE GC_CON-E.
  ELSEIF GV_CHECK_VAT EQ GC_CON-Y.
    MESSAGE S000 DISPLAY LIKE GC_CON-E.
  ENDIF.
*----------------------------------------------------------------------*
* CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS EVENT_CLASS DEFINITION.
*Handling double click
  PUBLIC SECTION.
    METHODS:
    HANDLE_DOUBLE_CLICK
    FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS. "lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS EVENT_CLASS IMPLEMENTATION.
  METHOD HANDLE_DOUBLE_CLICK.

  ENDMETHOD. "handle_double_click
ENDCLASS. "lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_GET_DATA.

  SELECT EKPO~EBELN
         EKPO~EBELP
         EKPO~TXZ01
         EKPO~MATNR
         EKPO~MENGE
         EKPO~MEINS
         EKPO~NETWR
         EKPO~BANFN
         EKPO~MATKL
         T023T~WGBEZ
    FROM EKKO
    INNER JOIN EKPO  ON EKKO~EBELN  EQ EKPO~EBELN
    INNER JOIN T023T ON EKPO~MATKL  EQ T023T~MATKL AND
                        T023T~SPRAS EQ SY-LANGU
    INTO TABLE GT_EKPO
    WHERE EKPO~EBELN IN S_EBELN
      AND EKPO~WERKS EQ P_WERKS
      AND EKPO~LOEKZ NE GC_CON-L
    ORDER BY EKPO~EBELN EKPO~EBELP.

  IF GT_EKPO[] IS NOT INITIAL.
    PERFORM F_GET_PO_DAT_SUB_NAME_PO.
    PERFORM F_CHECK_VAT.
    IF GV_CHECK_VAT EQ SPACE.
      PERFORM F_GET_DETAIL_PO.
      PERFORM F_GET_AUFK_REPORT_PO.
      PERFORM F_GET_MATDOC.
      PERFORM F_GET_ACC_DESC.
      PERFORM F_GET_NAME_CUST.
      PERFORM F_GET_RESULT.
    ENDIF.
  ENDIF.

ENDFORM.                    "f_get_data

*&---------------------------------------------------------------------*
*&      Form  F_CALL_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CALL_FORM.
  DATA: FM_NAME     TYPE RS38L_FNAM,
        LV_FORMNAME TYPE TDSFNAME.

  DATA : LV_TOTAL_LINE TYPE I,
         COUNT         TYPE SSFCTRLOP.

  DATA : LV_TABIX TYPE SY-TABIX,
         LV_LINE  TYPE I.

  DATA : LV_OUT_PUT TYPE SSFCOMPOP.

  LV_FORMNAME = GC_FORM.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = LV_FORMNAME
    IMPORTING
      FM_NAME            = FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR LV_TOTAL_LINE.

  DATA : LS_HEADER TYPE GY_HEADER,
         LT_HEADER TYPE TABLE OF GY_HEADER.

  LOOP AT GT_HEADER INTO GS_HEADER.
    ADD 1 TO LV_TOTAL_LINE.
    CLEAR LS_HEADER.
  ENDLOOP.
  GV_TOTAL_LINE = LV_TOTAL_LINE.

  LT_HEADER[] = GT_HEADER[].

  LOOP AT LT_HEADER INTO LS_HEADER.
    LV_TABIX = SY-TABIX.
    CLEAR GT_HEADER[].
    APPEND LS_HEADER TO GT_HEADER.
*BOI CH02
    PERFORM F_CHECK_AMOUNT_LOA  USING LS_HEADER-EBELN
                                      LS_HEADER-ERNAM
                                      LS_HEADER-WAERS
                                      GT_EKPO[].
*EOI CH02
    IF LV_TOTAL_LINE GT 1.
      IF     LV_TABIX = 1.               "FISRT CALL
        COUNT-NO_OPEN   = SPACE .
        COUNT-NO_CLOSE  = 'X' .
      ELSEIF LV_TABIX   = LV_TOTAL_LINE.  "LAST CALL
        COUNT-NO_OPEN   = 'X' .
        COUNT-NO_CLOSE  = SPACE .
      ELSE.                          "OTHER CALLS
        COUNT-NO_OPEN   = 'X' .
        COUNT-NO_CLOSE  = 'X' .
      ENDIF.
    ENDIF.

    CALL FUNCTION FM_NAME
      EXPORTING
        CONTROL_PARAMETERS = COUNT
        USER_SETTINGS      = 'X'
        IV_MNG             = GV_MNG
        IV_GM              = GV_GM
        IV_AGM             = GV_AGM
        IV_AMD             = GV_AMD
        IV_SD              = GV_SD
        IV_MD              = GV_MD
        IV_PRES            = GV_PRES
        IV_NAMEMNG         = GV_NAMEMNG
        IV_DATEMNG         = GV_DATEMNG
        IV_NAMEGM          = GV_NAMEGM
        IV_NAMEAGM         = GV_NAMEAGM
        IV_DATEGM          = GV_DATEGM
        IV_DATEAGM         = GV_DATEAGM
        IV_NAMEAMD         = GV_NAMEAMD
        IV_DATEAMD         = GV_DATEAMD
        IV_NAMESD          = GV_NAMESD
        IV_DATESD          = GV_DATESD
        IV_NAMEMD          = GV_NAMEMD
        IV_DATEMD          = GV_DATEMD
        IV_NAMEPRES        = GV_NAMEPRES
        IV_DATEPRES        = GV_DATEPRES
        IV_NAMEREQ         = GV_NAMEREQ
        IV_DATEREQ         = GV_DATEREQ
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        ERROR_MESSAGE      = 5
        OTHERS             = 7.

  ENDLOOP.

ENDFORM.                    " F_CALL_FORM
*&---------------------------------------------------------------------*
*&      Form  send_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_DATA_HEADER  text
*      -->PV_DATA_DETAIL  text
*----------------------------------------------------------------------*
FORM F_SEND_DATA TABLES FT_DETAIL
                        FT_MSEG
                        FT_HEADER
                  USING FV_FLAG
                        FV_PREPARE
                        FV_VATRATE.

  FT_HEADER[] = GT_HEADER[].
  FT_DETAIL[] = GT_RESULT[].
  FT_MSEG[]   = GT_MSEG_FORM[].
  FV_FLAG     = GV_CHECK_PROFIT.
  FV_PREPARE  = P_PRE.
  FV_VATRATE  = P_VAT.
ENDFORM.                    "send_data
*&---------------------------------------------------------------------*
*&      Form  F_GET_PO_DAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_PO_DAT_SUB_NAME_PO.

  SELECT EKKO~EBELN
         EKKO~BEDAT
         EKKO~LIFNR
         LFA1~NAME1
         LFA1~NAME2
         LFA1~ADRNR
*BOI CH02
         EKKO~ERNAM
         EKKO~WAERS
*EOI CH02
    FROM EKKO
    INNER JOIN LFA1 ON EKKO~LIFNR EQ LFA1~LIFNR
    INTO TABLE GT_EKKO
    FOR ALL ENTRIES IN GT_EKPO
    WHERE EKKO~EBELN EQ GT_EKPO-EBELN.

  IF NOT GT_EKKO[] IS INITIAL.
    SELECT  ADDRNUMBER NAME1 NAME2
    INTO TABLE GT_ADRC
    FROM ADRC
    FOR ALL ENTRIES IN GT_EKKO
    WHERE ADDRNUMBER = GT_EKKO-ADRNR
      AND NATION = ' '.

  ENDIF.

ENDFORM.                    " F_GET_PO_DAT
*&---------------------------------------------------------------------*
*&      Form  F_GET_AUFNR_REPORT_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_DETAIL_PO .
  SELECT EBELN
         EBELP
         AUFNR
         PRCTR
    FROM EKKN
    INTO TABLE GT_EKKN
    FOR ALL ENTRIES IN GT_EKPO
    WHERE EBELN EQ GT_EKPO-EBELN
      AND EBELP EQ GT_EKPO-EBELP.

ENDFORM.                    " F_GET_AUFNR_REPORT_PO
*&---------------------------------------------------------------------*
*&      Form  F_GET_AUFK_REPORT_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_AUFK_REPORT_PO.
  IF GT_EKKN IS NOT INITIAL.
    SELECT AUFNR
           KTEXT
      FROM AUFK
      INTO TABLE GT_AUFK
      FOR ALL ENTRIES IN GT_EKKN
      WHERE AUFNR EQ GT_EKKN-AUFNR.
  ENDIF.
ENDFORM.                    " F_GET_AUFK_REPORT_PO
*&---------------------------------------------------------------------*
*&      Form  F_GET_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_RESULT .

  DATA : BEGIN OF LS_SUM_AUFNR,
           AUFNR TYPE AUFK-AUFNR,
           NETWR TYPE VBAP-NETWR,
         END OF LS_SUM_AUFNR.
  DATA LT_SUM_AUFNR LIKE TABLE OF LS_SUM_AUFNR.

  DATA : BEGIN OF LS_CHECK_AMOUNT,
           AUFNR  TYPE AUFK-AUFNR,
           GPREIS TYPE RESBD-GPREIS,
         END OF LS_CHECK_AMOUNT.
  DATA LT_CHECK_AMOUNT LIKE TABLE OF LS_CHECK_AMOUNT.

  DATA LV_NAME TYPE THEAD-TDNAME.

  SORT GT_MSEG BY MBLNR DESCENDING.

  LOOP AT GT_EKPO INTO GS_EKPO .

    GS_HEADER-EBELN = GS_EKPO-EBELN.
    GS_HEADER-PO    = GS_EKPO-EBELN.

    CONCATENATE GS_EKPO-EBELN GS_EKPO-EBELP INTO LV_NAME.

    PERFORM F_READ_TEXT USING 'F50'
                              'EKPO'
                              LV_NAME
                     CHANGING GS_RESULT-WORK_PERIOD.

    PERFORM F_READ_TEXT USING 'F53'
                              'EKPO'
                              LV_NAME
                     CHANGING GS_RESULT-WARRAN_PERIOD.

    PERFORM F_READ_TEXT USING 'F54'
                              'EKPO'
                              LV_NAME
                     CHANGING GS_RESULT-PENALTY.

    PERFORM F_READ_TEXT USING 'F55'
                              'EKPO'
                              LV_NAME
                     CHANGING GS_RESULT-ENGINEER.

    GS_RESULT-MAT_GROUP = GS_EKPO-WGBEZ.

    READ TABLE GT_MSEG INTO GS_MSEG
    WITH KEY EBELN = GS_EKPO-EBELN
             EBELP = GS_EKPO-EBELP.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ELSE.
      GS_HEADER-MATD         = GS_MSEG-MBLNR.
      GS_RESULT-BSTMG        = GS_MSEG-BSTMG.
      GS_RESULT-RECRIPT      = GS_MSEG-MBLNR.
      GS_RESULT-RECEIPT_DATE = GS_MSEG-BUDAT.
      IF GS_MSEG-PS_PSP_PNR  IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
          EXPORTING
            INPUT  = GS_MSEG-PS_PSP_PNR
          IMPORTING
            OUTPUT = GS_RESULT-COST_SHEET.
      ELSE.
        GS_RESULT-COST_SHEET   = GS_MSEG-AUFNR.
      ENDIF.
      PERFORM F_EXIT_ALPHA USING GS_RESULT-COST_SHEET.
      GS_RESULT-SALE_ORDER   = GS_MSEG-KDAUF.
      GS_RESULT-REMARK       = GS_MSEG-SGTXT.
      IF GS_RESULT-REMARK IS INITIAL.
        PERFORM F_READ_TEXT USING 'Z011'
                                  'VBBK'
                                  GS_RESULT-SALE_ORDER
                         CHANGING GS_RESULT-REMARK.
      ENDIF.
      PERFORM F_READ_CUS_NAME.
      PERFORM F_READ_ACC_DESC.
    ENDIF.

    READ TABLE GT_EKKN INTO GS_EKKN
    WITH KEY EBELN = GS_EKPO-EBELN
             EBELP = GS_EKPO-EBELP.
    IF SY-SUBRC = 0.
      GS_RESULT-PRCTR         = GS_EKKN-PRCTR.
      GS_RESULT-AUFNR         = GS_EKKN-AUFNR.
      GS_HEADER-AUFNR         = GS_EKKN-AUFNR.
      GS_RESULT-PROFIT_CENTER = GS_EKKN-PRCTR.
      PERFORM F_READ_AUFK.
    ENDIF.

    READ TABLE GT_EKKO INTO GS_EKKO
    WITH KEY EBELN = GS_EKPO-EBELN.
    IF SY-SUBRC = 0.
      GS_RESULT-SUBNA  = GS_EKKO-NAME1.
      GS_RESULT-BEDAT  = GS_EKKO-BEDAT.
      GS_HEADER-DATE1  = GS_EKKO-BEDAT.
      GS_HEADER-VENDOR = GS_EKKO-LIFNR.
*      CONCATENATE gs_ekko-name1 gs_ekko-name2 INTO gs_header-name SEPARATED BY space.
*BOI CH02
      GS_HEADER-ERNAM  = GS_EKKO-ERNAM.
      GS_HEADER-WAERS  = GS_EKKO-WAERS.
*EOI CH02
      READ TABLE GT_ADRC INTO GS_ADRC WITH KEY ADDRNUMBER = GS_EKKO-ADRNR.
      IF SY-SUBRC EQ 0.
        CONCATENATE GS_ADRC-NAME1 GS_ADRC-NAME2 INTO GS_HEADER-NAME SEPARATED BY SPACE.
      ELSE.
        CONCATENATE GS_EKKO-NAME1 GS_EKKO-NAME2 INTO GS_HEADER-NAME SEPARATED BY SPACE.
      ENDIF.
    ENDIF.

    GS_RESULT-NETWR = GS_EKPO-NETWR.
    GS_RESULT-EBELN = GS_EKPO-EBELN.
    GS_RESULT-MATNR = GS_EKPO-MATNR.
    GS_RESULT-MAKTX = GS_EKPO-TXZ01.
    GS_RESULT-MENGE = GS_EKPO-MENGE.
    GS_RESULT-MEINS = GS_EKPO-MEINS.
    GS_RESULT-BANFN = GS_EKPO-BANFN.

    MOVE-CORRESPONDING GS_RESULT TO LS_SUM_AUFNR.

    COLLECT LS_SUM_AUFNR INTO LT_SUM_AUFNR.
    APPEND GS_HEADER TO GT_HEADER.
    APPEND GS_RESULT TO GT_RESULT.
    CLEAR : GS_EKPO,GS_EKKN,GS_EKKO,GS_HEADER.
  ENDLOOP.

  SORT GT_HEADER.
  DELETE ADJACENT DUPLICATES FROM GT_HEADER COMPARING EBELN.
*  SORT gt_check_amount BY aufnr rspos.
*  DELETE ADJACENT DUPLICATES FROM gt_check_amount COMPARING aufnr rspos.

  LOOP AT GT_CHECK_AMOUNT INTO GS_CHECK_AMOUNT.
    MOVE-CORRESPONDING GS_CHECK_AMOUNT TO LS_CHECK_AMOUNT.
    COLLECT LS_CHECK_AMOUNT INTO LT_CHECK_AMOUNT.

    CLEAR : LS_CHECK_AMOUNT,GS_CHECK_AMOUNT.
  ENDLOOP.

*  LOOP AT lt_sum_aufnr INTO ls_sum_aufnr.
*    gs_result-netwr = ls_sum_aufnr-netwr.
*
**    READ TABLE lt_check_amount INTO ls_check_amount
**    WITH KEY aufnr = ls_sum_aufnr-aufnr.
**    IF sy-subrc = 0.
**      gs_result-netwr = gs_result-netwr - ls_check_amount-gpreis.
**    ENDIF.
*
**    IF gs_result-netwr = 0.
**      DELETE gt_result WHERE aufnr = ls_sum_aufnr-aufnr.
**    ELSE.
*    MODIFY gt_result FROM gs_result TRANSPORTING netwr
*                                           WHERE aufnr = ls_sum_aufnr-aufnr.
**    ENDIF.
*    CLEAR : ls_sum_aufnr,gs_result.
*  ENDLOOP.

ENDFORM.                    " F_GET_RESULT
*&---------------------------------------------------------------------*
*&      Form  F_GET_MATDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_MATDOC .

  IF GT_EKKN[] IS NOT INITIAL.
    SELECT MSEG~MBLNR
           MSEG~MJAHR
           MSEG~ZEILE
           MSEG~EBELN
           MSEG~EBELP
           MSEG~AUFNR
           MKPF~BUDAT
           MSEG~KDAUF
           MSEG~KDPOS
           MSEG~SGTXT
           MSEG~SAKTO
           MSEG~BSTMG
           MSEG~PS_PSP_PNR
      FROM MSEG
      INNER JOIN MKPF ON MKPF~MBLNR EQ MSEG~MBLNR
      INTO TABLE GT_MSEG
      FOR ALL ENTRIES IN GT_EKKN
      WHERE MSEG~EBELN EQ GT_EKKN-EBELN
        AND MSEG~EBELP EQ GT_EKKN-EBELP
        AND MSEG~AUFNR EQ GT_EKKN-AUFNR.


    IF GT_MSEG IS NOT INITIAL.
      SELECT MSEG~MBLNR
             MSEG~MJAHR
             MSEG~ZEILE
             MSEG~BWART
             MSEG~AUFNR
             MSEG~MATNR
             MAKT~MAKTX
             MSEG~DMBTR
             MSEG~SJAHR
             MSEG~SMBLN
             MSEG~SMBLP
        FROM MSEG
        LEFT JOIN MAKT ON MSEG~MATNR EQ MAKT~MATNR AND
                           MAKT~SPRAS EQ SY-LANGU
        INTO TABLE GT_MSEG_FORM
        FOR ALL ENTRIES IN GT_MSEG
        WHERE MSEG~EBELN EQ GT_MSEG-EBELN
          AND MSEG~EBELP EQ GT_MSEG-EBELP
          AND MSEG~AUFNR EQ GT_MSEG-AUFNR.
      IF GT_MSEG_FORM IS NOT INITIAL.
        LOOP AT GT_MSEG_FORM INTO GS_MSEG_FORM WHERE SMBLN NE SPACE AND
                                                     SMBLP NE SPACE.
          DELETE GT_MSEG_FORM WHERE MBLNR = GS_MSEG_FORM-SMBLN AND
                                    MJAHR = GS_MSEG_FORM-SJAHR AND
                                    ZEILE = GS_MSEG_FORM-SMBLP.
          CLEAR GS_MSEG_FORM.
        ENDLOOP.

        DELETE GT_MSEG_FORM WHERE SMBLN NE SPACE
                              AND SMBLP NE SPACE.
        SORT GT_MSEG_FORM BY AUFNR.
      ENDIF.

    ENDIF.
  ENDIF.
ENDFORM.                    " F_GET_MATDOC
*&---------------------------------------------------------------------*
*&      Form  F_MODIF_CUS_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MODIF_CUS_NAME .

  READ TABLE GT_IHPA INTO GS_IHPA
  WITH KEY OBJNR = GS_CAUFVD-OBJNR.
  IF SY-SUBRC = 0.
    CONCATENATE GS_IHPA-NAME1 GS_IHPA-NAME2 INTO GS_RESULT-NAME1 SEPARATED BY SPACE.
  ENDIF.

ENDFORM.                    " F_MODIF_CUS_NAME
*&---------------------------------------------------------------------*
*&      Form  F_MODIF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MODIF_STATUS .

  READ TABLE GT_JEST INTO GS_JEST
  WITH KEY OBJNR = GS_CAUFVD-OBJNR.
  IF SY-SUBRC = 0.
    GS_RESULT-STATUS = 'TECO'.
  ELSE.
    GS_RESULT-STATUS = 'NONE'.
  ENDIF.

ENDFORM.                    " F_MODIF_STATUS
*&---------------------------------------------------------------------*
*&      Form  F_MODIF_DAMAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MODIF_DAMAGE .

  READ TABLE GT_VIQMFE INTO GS_VIQMFE
  WITH KEY QMNUM = GS_CAUFVD-QMNUM.
  IF SY-SUBRC = 0.
    GS_RESULT-FECOD = GS_VIQMFE-FECOD.
  ENDIF.

ENDFORM.                    " F_MODIF_DAMAGE
*&---------------------------------------------------------------------*
*&      Form  F_MODIF_VOLUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MODIF_VOLUM .
  READ TABLE GT_AFVU INTO GS_AFVU
  WITH KEY AUFPL = GS_CAUFVD-AUFPL.
  IF SY-SUBRC = 0.
    GS_RESULT-USR05 = GS_AFVU-USR05.
  ENDIF.

ENDFORM.                    " F_MODIF_VOLUM
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_PROFIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CHECK_PROFIT .

  LOOP AT GT_CHECK_PROFIT INTO GS_CHECK_PROFIT WHERE AUFNR = GS_RESULT-AUFNR.

    IF GS_CHECK_PROFIT-PRCTR NE GS_RESULT-PRCTR.
      GS_RESULT-FLAG = '*'.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " F_CHECK_PROFIT
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_VAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CHECK_VAT .
  DATA LT_LFA1 TYPE LFB1-LIFNR OCCURS 0.

  CONSTANTS : BEGIN OF LC_CON,
                VAT  TYPE C LENGTH 3 VALUE 'VAT',
                VAT1 TYPE C LENGTH 3 VALUE 'vat',
                VAT2 TYPE C LENGTH 3 VALUE 'Vat',
              END OF LC_CON.

  SELECT LIFNR
    FROM LFB1
    INTO TABLE LT_LFA1
    FOR ALL ENTRIES IN GT_EKKO
    WHERE   LIFNR EQ  GT_EKKO-LIFNR AND
          ( KVERM EQ LC_CON-VAT  OR
            KVERM EQ LC_CON-VAT1 OR
            KVERM EQ LC_CON-VAT2 OR
            KVERM NE SPACE ).
  IF LT_LFA1[] IS INITIAL AND P_VAT IS NOT INITIAL.
    GV_CHECK_VAT = 'X'.
*  ELSEIF lt_lfa1[] IS INITIAL AND p_vat IS NOT INITIAL.
*    gv_check_vat = 'Y'.
  ENDIF.

ENDFORM.                    " F_CHECK_VAT
*&---------------------------------------------------------------------*
*&      Form  F_READ_AUFK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_READ_AUFK .

  READ TABLE GT_AUFK INTO GS_AUFK
  WITH KEY AUFNR = GS_EKKN-AUFNR.
  IF SY-SUBRC = 0.
    GS_RESULT-PROJECT = GS_AUFK-KTEXT.
  ENDIF.

ENDFORM.                    " F_READ_AUFK

*&---------------------------------------------------------------------*
*&      Form  F_READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_READ_TEXT USING FV_ID TYPE TEXT4
                       FV_OD TYPE TEXT4
                       FV_NAME
              CHANGING LV_OUTPUT.

  DATA : LV_ID       TYPE THEAD-TDID,
         LV_LANGUAGE TYPE THEAD-TDSPRAS,
         LV_NAME     TYPE THEAD-TDNAME,
         LV_OBJECT   TYPE THEAD-TDOBJECT,
         LV_TEXT     TYPE STRING.

  DATA : LT_LINES TYPE TABLE OF TLINE,
         LS_LINES TYPE TLINE.

  CONSTANTS : BEGIN OF LC_READ_TEXT,
                ID TYPE C LENGTH 4 VALUE 'Z012',
                OB TYPE C LENGTH 4 VALUE 'VBBK',
              END OF LC_READ_TEXT.
*--------------------------------------------------------------------*
  LV_ID       = FV_ID.
  LV_LANGUAGE = SY-LANGU.
  LV_NAME     = FV_NAME.
  LV_OBJECT   = FV_OD.
*--------------------------------------------------------------------*
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      CLIENT                  = SY-MANDT
      ID                      = LV_ID
      LANGUAGE                = LV_LANGUAGE
      NAME                    = LV_NAME
      OBJECT                  = LV_OBJECT
    TABLES
      LINES                   = LT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR LV_TEXT.
  IF LT_LINES[] IS NOT INITIAL.
    LOOP AT LT_LINES INTO LS_LINES.
      IF LV_TEXT IS NOT INITIAL.
        CONCATENATE LV_TEXT LS_LINES-TDLINE INTO LV_TEXT.
      ELSE.
        LV_TEXT = LS_LINES-TDLINE.
      ENDIF.
    ENDLOOP.
    LV_OUTPUT = LV_TEXT.
  ENDIF.

ENDFORM.                    " F_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  F_GET_ACC_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_ACC_DESC.
  IF GT_MSEG IS NOT INITIAL.
    SELECT SAKNR
           TXT20
      FROM SKAT
      INTO TABLE GT_SKAT
      FOR ALL ENTRIES IN GT_MSEG
      WHERE SAKNR EQ GT_MSEG-SAKTO
        AND SPRAS EQ SY-LANGU
        AND KTOPL EQ '1000'.
  ENDIF.
ENDFORM.                    " F_GET_ACC_DESC
*&---------------------------------------------------------------------*
*&      Form  F_GET_NAME_CUST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_NAME_CUST .

  IF GT_MSEG IS NOT INITIAL.
    SELECT VBAK~VBELN
           KNA1~NAME1
           KNA1~NAME2
      FROM VBAK
      INNER JOIN KNA1 ON VBAK~KUNNR EQ KNA1~KUNNR
      INTO TABLE GT_KNA1
      FOR ALL ENTRIES IN GT_MSEG
      WHERE VBELN EQ GT_MSEG-KDAUF.
  ENDIF.

ENDFORM.                    " F_GET_NAME_CUST
*&---------------------------------------------------------------------*
*&      Form  F_READ_CUS_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_READ_CUS_NAME.

  READ TABLE GT_KNA1 INTO GS_KNA1
  WITH KEY VBELN = GS_MSEG-KDAUF.
  IF SY-SUBRC = 0.
    CONCATENATE GS_KNA1-NAME1 GS_KNA1-NAME2 INTO GS_RESULT-CUSTOMER.
    CLEAR GS_KNA1.
  ENDIF.

ENDFORM.                    " F_READ_CUS_NAME
*&---------------------------------------------------------------------*
*&      Form  F_READ_ACC_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_READ_ACC_DESC .
  READ TABLE GT_SKAT INTO GS_SKAT
  WITH KEY SAKNR = GS_MSEG-SAKTO.
  IF SY-SUBRC = 0.
    PERFORM F_EXIT_ALPHA USING GS_SKAT-SAKNR.
    CONCATENATE GS_SKAT-SAKNR '(' GS_SKAT-TXT20 ')' INTO GS_RESULT-AC_CODE.
    CLEAR GS_SKAT.
  ENDIF.

ENDFORM.                    " F_READ_ACC_DESC
*&---------------------------------------------------------------------*
*&      Form  F_EXIT_ALPHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_SKAT_SAKNR  text
*      -->P_CONCATENATE  text
*      -->P_GS_SKAT_SAKNR  text
*      -->P_2694   text
*      -->P_GS_SKAT_TXT20  text
*      -->P_2696   text
*      -->P_INTO  text
*      -->P_GS_RESULT_AC_CODE  text
*----------------------------------------------------------------------*
FORM F_EXIT_ALPHA  USING LV_RESULT.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = LV_RESULT
    IMPORTING
      OUTPUT = LV_RESULT.

ENDFORM.                    " F_EXIT_ALPHA
*BOI CH02
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_AMOUNT_LOA
*&---------------------------------------------------------------------*
*       Routine for check amount LOA (Refer logic from  ZFM06PE02)
*----------------------------------------------------------------------*
FORM F_CHECK_AMOUNT_LOA USING UV_EBELN TYPE EKKO-EBELN
                              UV_UNAME TYPE EKKO-ERNAM
                              UV_WAERS TYPE EKKO-WAERS
                              UT_EKPO  TYPE TTYP_EKPO.

  "Clear all related LOA variable
*  CLEAR:
*    GV_MNG,
*    GV_GM,
*    GV_AMD,
*    GV_SD,
*    GV_MD,
*    GV_PRES,
*    GV_NAMEMNG,
*    GV_DATEMNG,
*    GV_NAMEGM,
*    GV_DATEGM,
*    GV_NAMEAMD,
*    GV_DATEAMD,
*    GV_NAMESD,
*    GV_DATESD,
*    GV_NAMEMD,
*    GV_DATEMD,
*    GV_NAMEPRES,
*    GV_DATEPRES,
*    GV_NAMEREQ,
*    GV_DATEREQ,
*    GV_DATUM.
*
*  TYPES:
*    BEGIN OF LTYP_ZTMM_MAP_LOA,
*      DIVISION TYPE ZTMM_MAP_LOA-DIVISION,
*      PROCESS  TYPE ZTMM_MAP_LOA-PROCESS,
*      AMOUNT_L TYPE ZTMM_MAP_LOA-AMOUNT_L,
*      AMOUNT_H TYPE ZTMM_MAP_LOA-AMOUNT_H,
*      OPERATOR TYPE ZTMM_MAP_LOA-OPERATOR,
*      MNG      TYPE ZTMM_MAP_LOA-MNG,
*      AGM_GM   TYPE ZTMM_MAP_LOA-AGM_GM,
*      AMD      TYPE ZTMM_MAP_LOA-AMD,
*      SD       TYPE ZTMM_MAP_LOA-SD,
*      MD       TYPE ZTMM_MAP_LOA-MD,
*      PRES     TYPE ZTMM_MAP_LOA-PRES,
*    END OF LTYP_ZTMM_MAP_LOA.
*
*  DATA: LT_ZTMM_MAP_LOA TYPE STANDARD TABLE OF LTYP_ZTMM_MAP_LOA,
*        LT_EKPO         TYPE TTYP_EKPO,
*        LS_ZTMM_MAP_LOA TYPE LTYP_ZTMM_MAP_LOA,
*        LS_EKPO         LIKE LINE OF LT_EKPO,
*        LV_EXCH_RATE    TYPE UKURSP,
*        LV_EXRATE       TYPE ZTMM_MAP_LOA-AMOUNT_L,
*        LV_AMOUNT       TYPE EKPO-NETWR.
*
*  DATA : BEGIN OF LS_ZTMM_LOG_PO,
*    EBELN  TYPE ZTMM_LOG_PO-EBELN,
*    RUNNG  TYPE ZTMM_LOG_PO-RUNNG,
*    STATUS TYPE ZTMM_LOG_PO-STATUS,
*    ADSDR  TYPE ZTMM_LOG_PO-ADSDR,
*    POSIT  TYPE ZTMM_LOG_PO-POSIT,
*    ERDAT  TYPE ZTMM_LOG_PO-ERDAT,
*    ERTIM  TYPE ZTMM_LOG_PO-ERTIM,
*  END OF LS_ZTMM_LOG_PO.
*
*  DATA :
*    LT_ZTMM_LOG_PO  LIKE TABLE OF LS_ZTMM_LOG_PO,
*    LS_TMP          LIKE LS_ZTMM_LOG_PO,
*    LS_CHK          LIKE LS_ZTMM_LOG_PO,
*    LV_ERNAM1       TYPE EKKO-ERNAM,
*    LV_TABIX        TYPE SY-TABIX,
*    LV_APPFG        TYPE ZTMM_STATUS_PO-APPFG.
*
*  RANGES : LR_AMOUNT FOR ZTMM_MAP_LOA-AMOUNT_L.
*
*  LT_EKPO[] = UT_EKPO[].
*  DELETE LT_EKPO[] WHERE EBELN <> UV_EBELN.
*
*  SELECT ZTMM_MAP_LOA~DIVISION
*         PROCESS
*         AMOUNT_L
*         AMOUNT_H
*         OPERATOR
*         MNG
*         AGM_GM
*         AMD
*         SD
*         MD
*         PRES
*    FROM ZTMM_MAP_USER
*    INNER JOIN ZTMM_MAP_LOA
*    ON ZTMM_MAP_USER~DIVISION EQ ZTMM_MAP_LOA~DIVISION
*    INTO TABLE LT_ZTMM_MAP_LOA
*    WHERE ZTMM_MAP_USER~UNAME    EQ UV_UNAME
*      AND ( ZTMM_MAP_LOA~PROCESS EQ 'PO' OR
*            ZTMM_MAP_LOA~PROCESS EQ 'FG' )
*      AND ZTMM_MAP_USER~DFALG    NE 'X'
*      AND ZTMM_MAP_LOA~DFALG     NE 'X'.
*
*
*  IF UV_WAERS NE 'THB'.
*    PERFORM F_GET_EXCHANGE_RATE USING SY-DATUM
*                                      UV_WAERS
*                             CHANGING LV_EXCH_RATE.
*
*    LV_EXRATE = LV_EXCH_RATE / 1000.
*  ENDIF.
*
*  "Summarize Net amount
*  CLEAR : LV_AMOUNT.
*  LOOP AT LT_EKPO INTO LS_EKPO.
*    IF UV_WAERS NE 'THB'.
*      LS_EKPO-NETWR = ( ( LS_EKPO-NETWR / 100 ) * LV_EXRATE ).
*    ENDIF.
*
*    ADD LS_EKPO-NETWR TO LV_AMOUNT.
*  ENDLOOP.
*
*  LOOP AT LT_ZTMM_MAP_LOA INTO LS_ZTMM_MAP_LOA.
*
*    CLEAR LR_AMOUNT.
*    LR_AMOUNT-SIGN   = 'I'.
*    LR_AMOUNT-OPTION = LS_ZTMM_MAP_LOA-OPERATOR.
*    LR_AMOUNT-LOW    = LS_ZTMM_MAP_LOA-AMOUNT_L.
*    LR_AMOUNT-HIGH   = LS_ZTMM_MAP_LOA-AMOUNT_H.
*    APPEND LR_AMOUNT.
*
*    IF LV_AMOUNT IN LR_AMOUNT.
*      GV_MNG  = LS_ZTMM_MAP_LOA-MNG.
*      GV_GM   = LS_ZTMM_MAP_LOA-AGM_GM.
*      GV_AGM  = LS_ZTMM_MAP_LOA-AGM_GM.
*      GV_AMD  = LS_ZTMM_MAP_LOA-AMD.
*      GV_SD   = LS_ZTMM_MAP_LOA-SD.
*      GV_MD   = LS_ZTMM_MAP_LOA-MD.
*      GV_PRES = LS_ZTMM_MAP_LOA-PRES.
*      EXIT.
*    ENDIF.
*
*    CLEAR : LS_ZTMM_MAP_LOA,LR_AMOUNT[].
*  ENDLOOP.
*
*  SELECT EBELN
*         RUNNG
*         STATUS
*         ADSDR
*         POSIT
*         ERDAT
*         ERTIM
*    FROM ZTMM_LOG_PO
*    INTO TABLE LT_ZTMM_LOG_PO
*    WHERE EBELN EQ UV_EBELN
*      AND FLAGR NE 'X'.
*    IF SY-SUBRC = 0.
*      SORT LT_ZTMM_LOG_PO BY EBELN RUNNG.
*    ENDIF.
*
*  SELECT SINGLE ERNAM
*  FROM EKKO
*  INTO LV_ERNAM1
*  WHERE EBELN EQ UV_EBELN.
*
*  GV_NAMEREQ = LV_ERNAM1.
*
*  READ TABLE LT_ZTMM_LOG_PO INTO LS_TMP
*  WITH KEY STATUS = 'COMP'.
*  IF SY-SUBRC = 0.
*    SELECT SINGLE APPFG
*      FROM ZTMM_STATUS_PO
*      INTO LV_APPFG
*      WHERE EBELN EQ LS_TMP-EBELN.
*    LOOP AT LT_ZTMM_LOG_PO INTO LS_ZTMM_LOG_PO.
*      LV_TABIX = SY-TABIX.
*      IF LV_APPFG+1(1) EQ 'G'.
*        IF     LS_ZTMM_LOG_PO-POSIT EQ 'MGR'.
*          GV_NAMEMNG  = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ELSEIF LS_ZTMM_LOG_PO-POSIT EQ 'AGM'.
*          GV_NAMEAGM   = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ELSEIF LS_ZTMM_LOG_PO-POSIT EQ 'GM'.
*          GV_NAMEGM   = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ELSEIF LS_ZTMM_LOG_PO-POSIT EQ 'AMD'.
*          GV_NAMEAMD  = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ELSEIF LS_ZTMM_LOG_PO-POSIT EQ 'SD'.
*          GV_NAMESD   = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ELSEIF LS_ZTMM_LOG_PO-POSIT EQ 'MD'.
*          GV_NAMEMD   = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ELSEIF LS_ZTMM_LOG_PO-POSIT EQ 'PRD'.
*          GV_NAMEPRES = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ENDIF.
*
*        READ TABLE LT_ZTMM_LOG_PO INTO LS_CHK
*        WITH KEY EBELN  = LS_ZTMM_LOG_PO-EBELN
*                 STATUS = 'COMP'.
*        IF SY-SUBRC = 0.
*          IF     LV_TABIX EQ 1.
*            GV_DATEREQ  = LS_ZTMM_LOG_PO-ERDAT.
*          ELSEIF LV_TABIX EQ 2.
*            GV_DATEMNG  = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEGM   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEAGM  = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEAMD  = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATESD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEMD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEPRES = LS_ZTMM_LOG_PO-ERDAT.
*          ELSEIF LV_TABIX EQ 3.
*            GV_DATEGM   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEAGM  = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEAMD  = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATESD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEMD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEPRES = LS_ZTMM_LOG_PO-ERDAT.
*          ELSEIF LV_TABIX EQ 4.
*            GV_DATEAMD  = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATESD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEMD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEPRES = LS_ZTMM_LOG_PO-ERDAT.
*          ELSEIF LV_TABIX EQ 5.
*            GV_DATESD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEMD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEPRES = LS_ZTMM_LOG_PO-ERDAT.
*          ELSEIF LV_TABIX EQ 6.
*            GV_DATEMD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEPRES = LS_ZTMM_LOG_PO-ERDAT.
*          ELSEIF LV_TABIX EQ 7.
*            GV_DATEPRES = LS_ZTMM_LOG_PO-ERDAT.
*          ENDIF.
*
*        ENDIF.
*      ELSEIF LV_APPFG+2(1) EQ 'G'.
*        IF LS_ZTMM_LOG_PO-POSIT EQ 'MGR'.
*          GV_NAMEMNG  = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ELSEIF LS_ZTMM_LOG_PO-POSIT EQ 'GM'.
*          GV_NAMEGM   = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ELSEIF LS_ZTMM_LOG_PO-POSIT EQ 'AGM'.
*          GV_NAMEAGM  = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ELSEIF LS_ZTMM_LOG_PO-POSIT EQ 'AMD'.
*          GV_NAMEAMD  = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ELSEIF LS_ZTMM_LOG_PO-POSIT EQ 'SD'.
*          GV_NAMESD   = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ELSEIF LS_ZTMM_LOG_PO-POSIT EQ 'MD'.
*          GV_NAMEMD   = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ELSEIF LS_ZTMM_LOG_PO-POSIT EQ 'PRD'.
*          GV_NAMEPRES = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ENDIF.
*
*        READ TABLE LT_ZTMM_LOG_PO INTO LS_CHK
*        WITH KEY EBELN  = LS_ZTMM_LOG_PO-EBELN
*                 STATUS = 'COMP'.
*        IF SY-SUBRC = 0.
*          IF LV_TABIX EQ 1.
*            GV_DATEREQ  = LS_ZTMM_LOG_PO-ERDAT.
*          ELSEIF LV_TABIX EQ 2.
*            GV_DATEMNG  = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEGM   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEAGM  = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEAMD  = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATESD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEMD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEPRES = LS_ZTMM_LOG_PO-ERDAT.
*          ELSEIF LV_TABIX EQ 4.
*            GV_DATEGM   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEAGM  = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEAMD  = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATESD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEMD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEPRES = LS_ZTMM_LOG_PO-ERDAT.
*          ELSEIF LV_TABIX EQ 5.
*            GV_DATESD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEAMD  = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEMD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEPRES = LS_ZTMM_LOG_PO-ERDAT.
*          ELSEIF LV_TABIX EQ 6.
*            GV_DATEAMD  = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEMD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEPRES = LS_ZTMM_LOG_PO-ERDAT.
*          ELSEIF LV_TABIX EQ 7.
*            GV_DATEMD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEPRES = LS_ZTMM_LOG_PO-ERDAT.
*          ELSEIF LV_TABIX EQ 8.
*            GV_DATEPRES = LS_ZTMM_LOG_PO-ERDAT.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        IF LS_ZTMM_LOG_PO-POSIT EQ 'MGR'.
*          GV_NAMEMNG  = LS_ZTMM_LOG_PO-ADSDR+10(40).
*          GV_NAMEGM   = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ELSEIF LS_ZTMM_LOG_PO-POSIT EQ 'AMD'.
*          GV_NAMEAMD  = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ELSEIF LS_ZTMM_LOG_PO-POSIT EQ 'SD'.
*          GV_NAMESD   = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ELSEIF LS_ZTMM_LOG_PO-POSIT EQ 'MD'.
*          GV_NAMEMD   = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ELSEIF LS_ZTMM_LOG_PO-POSIT EQ 'PRD'.
*          GV_NAMEPRES = LS_ZTMM_LOG_PO-ADSDR+10(40).
*        ENDIF.
*
*        READ TABLE LT_ZTMM_LOG_PO INTO LS_CHK
*        WITH KEY EBELN  = LS_ZTMM_LOG_PO-EBELN
*                 STATUS = 'COMP'.
*        IF SY-SUBRC = 0.
*          IF     LV_TABIX EQ 1.
*            GV_DATEREQ  = LS_ZTMM_LOG_PO-ERDAT.
*          ELSEIF LV_TABIX EQ 2.
*            GV_DATEMNG  = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEGM   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEAMD  = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATESD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEMD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEPRES = LS_ZTMM_LOG_PO-ERDAT.
*          ELSEIF LV_TABIX EQ 3.
*            GV_DATESD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEAMD  = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEMD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEPRES = LS_ZTMM_LOG_PO-ERDAT.
*          ELSEIF LV_TABIX EQ 4.
*            GV_DATEAMD  = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEMD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEPRES = LS_ZTMM_LOG_PO-ERDAT.
*          ELSEIF LV_TABIX EQ 5.
*            GV_DATEMD   = LS_ZTMM_LOG_PO-ERDAT.
*            GV_DATEPRES = LS_ZTMM_LOG_PO-ERDAT.
*          ELSEIF LV_TABIX EQ 6.
*            GV_DATEPRES = LS_ZTMM_LOG_PO-ERDAT.
*          ENDIF.
*
*        ENDIF.
*      ENDIF.
*      CLEAR : LS_ZTMM_LOG_PO.
*    ENDLOOP.
*  ENDIF.
*
*  IF GV_NAMEGM IS NOT INITIAL.
*    GV_AGM = 'X'.
*  ELSE.
*    GV_GM  = 'X'.
*  ENDIF.

ENDFORM.                    " F_CHECK_AMOUNT_LOA
*&---------------------------------------------------------------------*
*&      Form  F_GET_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_EXCHANGE_RATE USING UV_DATE  TYPE DATUM
                               UV_WAERS TYPE EKKO-WAERS
                      CHANGING CV_EXCHANGE TYPE UKURSP.

  DATA : LV_RATE_TYPE  TYPE BAPI1093_1-RATE_TYPE,
         LV_FROM_CURR  TYPE BAPI1093_1-FROM_CURR,
         LV_TO_CURRNCY TYPE	BAPI1093_1-TO_CURRNCY,
         LV_DATE       TYPE	BAPI1093_2-TRANS_DATE.

  DATA : LS_EXCH_RATE	TYPE BAPI1093_0,
         LS_RETURN    TYPE  BAPIRET1.

  CONSTANTS : BEGIN OF LC_EXCHANGE,
                RATE_TYPE TYPE C LENGTH 1 VALUE 'M',
                CURRNCY   TYPE C LENGTH 3 VALUE 'THB',
              END OF LC_EXCHANGE.

  LV_RATE_TYPE  = LC_EXCHANGE-RATE_TYPE.
  LV_FROM_CURR  = UV_WAERS.
  LV_TO_CURRNCY = LC_EXCHANGE-CURRNCY.

  PERFORM F_FIRST_DATE USING UV_DATE
                    CHANGING LV_DATE.

  CALL FUNCTION 'BAPI_EXCHANGERATE_GETDETAIL'
    EXPORTING
      RATE_TYPE  = LV_RATE_TYPE
      FROM_CURR  = LV_FROM_CURR
      TO_CURRNCY = LV_TO_CURRNCY
      DATE       = LV_DATE
    IMPORTING
      EXCH_RATE  = LS_EXCH_RATE
      RETURN     = LS_RETURN.

  CV_EXCHANGE = LS_EXCH_RATE-EXCH_RATE.

ENDFORM.                    " F_GET_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*&      Form  F_FIRST_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_DATE  text
*----------------------------------------------------------------------*
FORM F_FIRST_DATE USING UV_DATE_IN TYPE DATUM
               CHANGING CV_DATE    TYPE DATUM.

  DATA : LV_DATE             TYPE DATUM,
         LV_MONTH_BEGIN_DATE TYPE DATUM,
         LV_MONTH_END_DATE   TYPE DATUM.

  LV_DATE = UV_DATE_IN.

  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      IV_DATE             = LV_DATE
    IMPORTING
      EV_MONTH_BEGIN_DATE = LV_MONTH_BEGIN_DATE
      EV_MONTH_END_DATE   = LV_MONTH_END_DATE.

  CV_DATE = LV_MONTH_BEGIN_DATE.

ENDFORM.                    " F_FIRST_DATE

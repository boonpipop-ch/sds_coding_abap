*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0060
*  Creation Date      : 02.05.2024
*  Author             : Boonpipop Ch.
*  Add-on ID          : ZSDF007
*  Description        : Quotation Form
*  Copied from        : ECC ( ZR_PRINT_QT )
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Report ZSDSSDR0060
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSSDR0060.

***INCLUDE RVADTABL .
TABLES: NAST,                          "Messages
        TNAPR,                         "Programs & Forms
        ITCPO,                         "Communicationarea for Spool
        ARC_PARAMS,                    "Archive parameters
        TOA_DARA,                      "Archive parameters
        ADDR_KEY.                      "Adressnumber for ADDRESS
TABLES: KONV,
        JEST,
        TVKBT,
        TVGRT.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
* header
TYPES: BEGIN OF TYP_HEADER,
         VBELN               LIKE VBAK-VBELN,
         VSNMR_V             LIKE VBAK-VSNMR_V,
         ERDAT               LIKE VBAK-ERDAT,
         CRTEDAT(9)          TYPE C,
         VDATU               LIKE VBAK-VDATU,
         DLVDAT(9)           TYPE C,
         SHIPTOADDR(100)     TYPE C,                      "Ship to Address
         SHIPTONO            LIKE VBPA-KUNNR,          "Ship to no
         STREET              LIKE ADRC-STREET,          "Street ship to
         CITY2               LIKE ADRC-CITY2,            "City2 to ship to
         CITY1               LIKE ADRC-CITY1,            "City1 to ship to
         POST_CODE1          LIKE ADRC-POST_CODE1,   "Post cost
         CUST_ZTERM          LIKE KNB1-ZTERM,          "Customer Term
         SHIPNAME(100)       TYPE C,                       "Ship to name
         BILLTONO            LIKE VBPA-KUNNR,          "Bill to no
         BILLNAME(100)       TYPE C,                         "Bill to name
         REFERMEMO(255)      TYPE C,                 "Refer memo
         CHECK_SALES_OFF(50) TYPE C,          "Check Sales Office
         KUNNR               LIKE VBAK-KUNNR,
         AUFNR               LIKE VBAP-AUFNR,
         KTEXT               LIKE COAS-KTEXT,
         SOLDTO              LIKE ADRC-NAME1,
         SOLDTOTITLE         LIKE TSAD3T-TITLE_MEDI,
         SHIPTO              LIKE ADRC-NAME1,
         SHIPTOTITLE         LIKE TSAD3T-TITLE_MEDI,
         SHIPBY              LIKE ADRC-NAME1,
         EDATU               LIKE VBEP-EDATU,
         RCVEDAT(9)          TYPE C,
         BSTKD               LIKE VBKD-BSTKD,
         QUOTN               LIKE VBFA-VBELV,
         INQRY               LIKE VBFA-VBELV,
         ZTERM               LIKE VBKD-ZTERM,
         VTEXT(25)           TYPE C,
         KWERT               LIKE KONV-KWERT,
         SALEPR              LIKE P0001-ENAME,
         JOBNO(15)           TYPE C,
         DATNO(15)           TYPE C,
         CMDONO(15)          TYPE C,
         AUART               LIKE VBAK-AUART,
         BEZEI               LIKE TVAKT-BEZEI,
         VKORG               LIKE VBAK-VKORG,
         VTWEG               LIKE VBAK-VTWEG,
         VKBUR               LIKE VBAK-VKBUR,
         VKGRP               LIKE VBAK-VKGRP,
         OBJNR               LIKE VBAK-OBJNR,
       END OF TYP_HEADER.

* item
TYPES: BEGIN OF TYP_ITEM,
         POSNR      LIKE VBAP-POSNR,
         MATNR      LIKE VBAP-MATNR,
         UEPOS      LIKE VBAP-UEPOS,
         DESC       TYPE STRING,
         KWMENG     LIKE VBAP-KWMENG,
         KWMENG2    TYPE I,
         NETPR      LIKE VBAP-NETPR,
         NETWR      LIKE VBAP-NETWR,
         KWMENG_MOM TYPE KWMENG,
         PRICE      TYPE NETWR,
         RUNNO(2)   TYPE N,
         ZTERM      LIKE  VBKD-ZTERM,
         KWERT      LIKE  KONV-KWERT,
         FLG_PROCON TYPE C,        "Flag Check Product Control
       END OF TYP_ITEM.
TYPES: TYP_ITEM_TABLE TYPE TABLE OF TYP_ITEM.

TYPES: BEGIN OF TYP_ITEM2,
         POSNR     LIKE VBAP-POSNR,
         MATNR(21) TYPE C,
         RUNNO(2)  TYPE N,
         EDATU     TYPE VBEP-EDATU,
         BMENG(5)  TYPE C,
         ZTERM     LIKE  VBKD-ZTERM,
         ITEM      LIKE VBAP-POSNR,
         KWMENG(5) TYPE C,
         KWERT     LIKE  KONV-KWERT,
       END OF TYP_ITEM2.
TYPES: TYP_ITEM_TABLE2 TYPE TABLE OF TYP_ITEM2.
TYPES: TYP_ITEM_VBAP TYPE TABLE OF VBAP.
TYPES: BEGIN OF TYP_VBEP,
         VBELN  LIKE  VBEP-VBELN,
         ITEM   LIKE  VBEP-POSNR,
         POSNR  LIKE VBEP-POSNR,
         MATNR  LIKE VBAP-MATNR,
         EDATU  LIKE VBEP-EDATU,
         BMENG  LIKE VBEP-BMENG,
         KWMENG LIKE VBAP-KWMENG,
       END OF TYP_VBEP.

* total
TYPES: BEGIN OF TYP_TOTAL,
         TOTAL      LIKE VBAK-NETWR,
         PCENT      TYPE I,
         VAT        LIKE VBAK-NETWR,
         GTOTAL     LIKE VBAK-NETWR,
         QTY(5)     TYPE C,
         TOTAL_DOWN LIKE KONV-KWERT,  "Advance Receipt
       END OF TYP_TOTAL.

* attachment
TYPES: BEGIN OF TYP_ATTCH,
         PYMNT TYPE C,
         QUOTN TYPE C,
         MAP   TYPE C,
         PO    TYPE C,
       END OF TYP_ATTCH.

* premium
TYPES: BEGIN OF TYP_PREM,
         PICHONK TYPE C,
         QTY     TYPE I,
       END OF TYP_PREM.

* manager
TYPES: BEGIN OF TYP_MGR,
         SALES_MGR      TYPE ZSDSDE_PARAM_VALUE,
         SALES_DATE(9)  TYPE C,
         CREDIT_MGR     TYPE ZSDSDE_PARAM_VALUE,
         CREDIT_DATE(9) TYPE C,
         ACCNT_MGR      TYPE ZSDSDE_PARAM_VALUE,
         ACCNT_DATE(9)  TYPE C,
         SPA_MGR        TYPE ZSDSDE_PARAM_VALUE,
         SPA_DATE(9)    TYPE C,
       END OF TYP_MGR.

*&---------------------------------------------------------------------*
*& Structure
*&---------------------------------------------------------------------*
DATA: GWA_HEADER TYPE TYP_HEADER.
DATA: GT_ITEM    TYPE TYP_ITEM_TABLE WITH HEADER LINE.
DATA: GT_ITEM2   TYPE TYP_ITEM_TABLE2 WITH HEADER LINE.
DATA: GT_ITEM_TOTAL TYPE TYP_ITEM_TABLE WITH HEADER LINE.

*&---------------------------------------------------------------------*
*& Variables
*&---------------------------------------------------------------------*
DATA: GV_VBELN          LIKE VBAK-VBELN,
      GV_KNUMV          TYPE KNUMV,
      GV_VKBUR          TYPE VKBUR,
      GV_TXTNAME_INVRMK TYPE TDOBNAME,
      GV_SPRAS_ADRC     TYPE NA_SPRAS.

*&---------------------------------------------------------------------*
*& Constants
*&---------------------------------------------------------------------*
DATA: GC_X TYPE C VALUE 'X'.
CONSTANTS: GC_APPROVAL TYPE C LENGTH 30 VALUE 'VKBUR_&1_APP'.

*&---------------------------------------------------------------------*
*& Program Variables
*&---------------------------------------------------------------------*
DATA: XSCREEN(1)   TYPE C.                "Output on printer or screen
DATA: LV_FM        TYPE RS38L_FNAM.

*---------------------------------------------------------------------*
*       FORM ENTRY
*---------------------------------------------------------------------*
FORM ENTRY USING RETURN_CODE US_SCREEN.

  DATA: LF_RETCODE TYPE SY-SUBRC.
  XSCREEN = US_SCREEN.
  PERFORM PROCESSING USING    US_SCREEN
                     CHANGING LF_RETCODE.
  IF LF_RETCODE NE 0.
    RETURN_CODE = 1.
  ELSE.
    RETURN_CODE = 0.
  ENDIF.

ENDFORM.                    "ENTRY
*---------------------------------------------------------------------*
*       FORM PROCESSING                                    *
*---------------------------------------------------------------------*
FORM PROCESSING USING    PROC_SCREEN
                CHANGING CF_RETCODE.

  DATA: LS_PRINT_DATA_TO_READ TYPE LEDLV_PRINT_DATA_TO_READ.
  DATA: LS_DLV_DELNOTE        TYPE LEDLV_DELNOTE.
  DATA: LF_FM_NAME            TYPE RS38L_FNAM.
  DATA: LS_CONTROL_PARAM      TYPE SSFCTRLOP.
  DATA: LS_COMPOSER_PARAM     TYPE SSFCOMPOP.
  DATA: LS_RECIPIENT          TYPE SWOTOBJID.
  DATA: LS_SENDER             TYPE SWOTOBJID.
  DATA: LF_FORMNAME           TYPE TDSFNAME.
  DATA: LS_ADDR_KEY           LIKE ADDR_KEY.

* SmartForm from customizing table TNAPR
  LF_FORMNAME = TNAPR-SFORM.

* determine print data
  PERFORM SET_PRINT_DATA_TO_READ USING    LF_FORMNAME
                                 CHANGING LS_PRINT_DATA_TO_READ
                                          CF_RETCODE.

  IF CF_RETCODE = 0.
    PERFORM SET_PRINT_PARAM USING    LS_ADDR_KEY
                            CHANGING LS_CONTROL_PARAM
                                     LS_COMPOSER_PARAM
                                     LS_RECIPIENT
                                     LS_SENDER
                                     CF_RETCODE.
  ENDIF.

  IF CF_RETCODE = 0.
*   determine smartform function module
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        FORMNAME           = LF_FORMNAME
      IMPORTING
        FM_NAME            = LF_FM_NAME
      EXCEPTIONS
        NO_FORM            = 1
        NO_FUNCTION_MODULE = 2
        OTHERS             = 3.
    IF SY-SUBRC <> 0.
*     error handling
      CF_RETCODE = SY-SUBRC.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.
  ENDIF.

  IF CF_RETCODE = 0.
*   call smartform
    CALL FUNCTION LF_FM_NAME
      EXPORTING
        ARCHIVE_INDEX      = TOA_DARA
        ARCHIVE_PARAMETERS = ARC_PARAMS
        CONTROL_PARAMETERS = LS_CONTROL_PARAM
*       mail_appl_obj      =
        MAIL_RECIPIENT     = LS_RECIPIENT
        MAIL_SENDER        = LS_SENDER
        OUTPUT_OPTIONS     = LS_COMPOSER_PARAM
        USER_SETTINGS      = ' '
        IS_DLV_DELNOTE     = LS_DLV_DELNOTE
        IS_NAST            = NAST
*      IMPORTING
*       document_output_info =
*       job_output_info    =
*       job_output_options =
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        OTHERS             = 5.

    IF SY-SUBRC <> 0.
*     error handling
*     cf_retcode = sy-subrc.
      CF_RETCODE = 0.
      PERFORM PROTOCOL_UPDATE.
*     get SmartForm protocoll and store it in the NAST protocoll
      PERFORM ADD_SMFRM_PROT.                  "INS_HP_335958
    ENDIF.
  ENDIF.

* get SmartForm protocoll and store it in the NAST protocoll
* PERFORM ADD_SMFRM_PROT.                       DEL_HP_335958

ENDFORM.                    "PROCESSING
*---------------------------------------------------------------------*
*       FORM SET_PRINT_DATA_TO_READ                                   *
*---------------------------------------------------------------------*
*       General provision of data for the form                        *
*---------------------------------------------------------------------*
FORM SET_PRINT_DATA_TO_READ USING    IF_FORMNAME LIKE TNAPR-SFORM
         CHANGING CS_PRINT_DATA_TO_READ TYPE LEDLV_PRINT_DATA_TO_READ
                         CF_RETCODE.

  FIELD-SYMBOLS: <FS_PRINT_DATA_TO_READ> TYPE XFELD.
  DATA: LT_FIELDLIST TYPE TSFFIELDS.
  DATA: LS_FIELDLIST TYPE LINE OF TSFFIELDS.
  DATA: LF_FIELD1 TYPE LINE OF TSFFIELDS.
  DATA: LF_FIELD2 TYPE LINE OF TSFFIELDS.
  DATA: LF_FIELD3 TYPE LINE OF TSFFIELDS.

* get function name of smartform
  CALL FUNCTION 'SSF_FIELD_LIST'
    EXPORTING
      FORMNAME           = IF_FORMNAME
*     VARIANT            = ' '
*    IMPORTING
*     FIELDLIST          = LT_FIELDLIST
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
*  error handling
    CF_RETCODE = SY-SUBRC.
    PERFORM PROTOCOL_UPDATE.
    CLEAR LT_FIELDLIST.
  ELSE.
* set print data requirements
    LOOP AT LT_FIELDLIST INTO LS_FIELDLIST.
      SPLIT LS_FIELDLIST AT '-' INTO LF_FIELD1 LF_FIELD2 LF_FIELD3.
* <<<< START_OF_INSERTION_HP_350342 >>>>
      IF LF_FIELD1 = 'IS_DLV_DELNOTE' AND LF_FIELD2 = 'IT_SERNR'.
        LF_FIELD2 = 'IT_SERNO'.
      ENDIF.
* <<<< END_OF_INSERTION_HP_350342 >>>>
      ASSIGN COMPONENT LF_FIELD2 OF STRUCTURE
                       CS_PRINT_DATA_TO_READ TO <FS_PRINT_DATA_TO_READ>.
      IF SY-SUBRC = 0.
        <FS_PRINT_DATA_TO_READ> = 'X'.
      ENDIF.
    ENDLOOP.

* header data is always required
    CS_PRINT_DATA_TO_READ-HD_GEN = 'X'.
* adress is always required for print param
    CS_PRINT_DATA_TO_READ-HD_ADR = 'X'.
* organisational data is always required for include texts
    CS_PRINT_DATA_TO_READ-HD_ORG = 'X'.
*organisational data address is always required       "n_520906
    CS_PRINT_DATA_TO_READ-HD_ORG_ADR  = 'X'.                "n_520906

  ENDIF.

ENDFORM.                    "SET_PRINT_DATA_TO_READ


*&---------------------------------------------------------------------*
*&      Form  SET_PRINT_PARAM
*&---------------------------------------------------------------------*
FORM SET_PRINT_PARAM USING    IS_ADDR_KEY LIKE ADDR_KEY
                     CHANGING CS_CONTROL_PARAM TYPE SSFCTRLOP
                              CS_COMPOSER_PARAM TYPE SSFCOMPOP
                              CS_RECIPIENT TYPE  SWOTOBJID
                              CS_SENDER TYPE  SWOTOBJID
                              CF_RETCODE TYPE SY-SUBRC.

  DATA: LS_ITCPO     TYPE ITCPO.
  DATA: LF_REPID     TYPE SY-REPID.
  DATA: LF_DEVICE    TYPE TDDEVICE.
  DATA: LS_RECIPIENT TYPE SWOTOBJID.
  DATA: LS_SENDER    TYPE SWOTOBJID.

  LF_REPID = SY-REPID.

  CALL FUNCTION 'WFMC_PREPARE_SMART_FORM'
    EXPORTING
      PI_NAST       = NAST
      PI_ADDR_KEY   = IS_ADDR_KEY
      PI_REPID      = LF_REPID
    IMPORTING
      PE_RETURNCODE = CF_RETCODE
      PE_ITCPO      = LS_ITCPO
      PE_DEVICE     = LF_DEVICE
      PE_RECIPIENT  = CS_RECIPIENT
      PE_SENDER     = CS_SENDER.

  IF CF_RETCODE = 0.
    MOVE-CORRESPONDING LS_ITCPO TO CS_COMPOSER_PARAM.
*   CS_CONTROL_PARAM-NO_OPEN
*   CS_CONTROL_PARAM-NO_CLOSE
    CS_CONTROL_PARAM-DEVICE      = LF_DEVICE.
    CS_CONTROL_PARAM-NO_DIALOG   = 'X'.
    CS_CONTROL_PARAM-PREVIEW     = XSCREEN.
    CS_CONTROL_PARAM-GETOTF      = LS_ITCPO-TDGETOTF.
    CS_CONTROL_PARAM-LANGU       = NAST-SPRAS.
*   CS_CONTROL_PARAM-REPLANGU1
*   CS_CONTROL_PARAM-REPLANGU2
*   CS_CONTROL_PARAM-REPLANGU3
*   CS_CONTROL_PARAM-STARTPAGE
  ENDIF.

ENDFORM.                               " SET_PRINT_PARAM
*&---------------------------------------------------------------------*
*&      Form  ADD_SMFRM_PROT
*&---------------------------------------------------------------------*
FORM ADD_SMFRM_PROT.

  DATA: LT_ERRORTAB             TYPE TSFERROR.
* DATA: LF_MSGNR                TYPE SY-MSGNO.   "DEL_HP_335958
  FIELD-SYMBOLS: <FS_ERRORTAB>  TYPE LINE OF TSFERROR.

* get smart form protocoll
  CALL FUNCTION 'SSF_READ_ERRORS'
    IMPORTING
      ERRORTAB = LT_ERRORTAB.

* add smartform protocoll to nast protocoll
  LOOP AT LT_ERRORTAB ASSIGNING <FS_ERRORTAB>.
*    CLEAR LF_MSGNR.                             "DEL_HP_335958
*    LF_MSGNR = <FS_ERRORTAB>-ERRNUMBER.         "DEL_HP_335958
    CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
      EXPORTING
        MSG_ARBGB = <FS_ERRORTAB>-MSGID
*       MSG_NR    = LF_MSGNR               "DEL_HP_335958
        MSG_NR    = <FS_ERRORTAB>-MSGNO    "INS_HP_335958
        MSG_TY    = <FS_ERRORTAB>-MSGTY
        MSG_V1    = <FS_ERRORTAB>-MSGV1
        MSG_V2    = <FS_ERRORTAB>-MSGV2
        MSG_V3    = <FS_ERRORTAB>-MSGV3
        MSG_V4    = <FS_ERRORTAB>-MSGV4
      EXCEPTIONS
        OTHERS    = 1.
  ENDLOOP.

ENDFORM.                               " ADD_SMFRM_PROT
*---------------------------------------------------------------------*
*       FORM PROTOCOL_UPDATE                                          *
*---------------------------------------------------------------------*
*       The messages are collected for the processing protocol.       *
*---------------------------------------------------------------------*
FORM PROTOCOL_UPDATE.

  CHECK XSCREEN = SPACE.
  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
    EXPORTING
      MSG_ARBGB = SYST-MSGID
      MSG_NR    = SYST-MSGNO
      MSG_TY    = SYST-MSGTY
      MSG_V1    = SYST-MSGV1
      MSG_V2    = SYST-MSGV2
      MSG_V3    = SYST-MSGV3
      MSG_V4    = SYST-MSGV4
    EXCEPTIONS
      OTHERS    = 1.

ENDFORM.                    "PROTOCOL_UPDATE
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
FORM READ_TEXT_HEAD  USING    P_ID
                         P_OBJECT
                         P_VBELN
                CHANGING P_VALUE.

  DATA: IT_LINES TYPE STANDARD TABLE OF TLINE.
  DATA: WA_LINES LIKE LINE OF IT_LINES.
  DATA: V_NAME TYPE THEAD-TDNAME.

  CLEAR: P_VALUE, IT_LINES[].

  V_NAME = P_VBELN.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = P_ID
      LANGUAGE                = SY-LANGU
      NAME                    = V_NAME
      OBJECT                  = P_OBJECT
    TABLES
      LINES                   = IT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  IF SY-SUBRC = 0.
    LOOP AT IT_LINES INTO WA_LINES.
      CONCATENATE P_VALUE  WA_LINES-TDLINE INTO P_VALUE.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  CONVERT_DATE_FORMAT
*&---------------------------------------------------------------------*
FORM CONVERT_DATE_FORMAT  USING    P_EDATU P_LANGU_NAST
                          CHANGING P_DATE_CONVERTED.

  DATA: LV_MONTH             TYPE MONTH,
        LV_DATE_CONVERTED(9) TYPE C,
        LV_DATE(2)           TYPE C,
        LV_YEAR(2)           TYPE C.
  DATA: LWA_T247 TYPE T247.

  CLEAR: LV_MONTH, LV_DATE, LV_YEAR, LV_DATE_CONVERTED.
  MOVE P_EDATU+6(2) TO LV_DATE.
  MOVE P_EDATU+2(2) TO LV_YEAR.
  MOVE P_EDATU+4(2) TO LV_MONTH.

  CALL FUNCTION 'IDWT_READ_MONTH_TEXT'
    EXPORTING
      LANGU = P_LANGU_NAST
      MONTH = LV_MONTH
    IMPORTING
      T247  = LWA_T247.

  IF LWA_T247-KTX IS NOT INITIAL.
    CONCATENATE LV_DATE LWA_T247-KTX LV_YEAR INTO LV_DATE_CONVERTED SEPARATED BY '-'.
  ENDIF.

  MOVE LV_DATE_CONVERTED TO P_DATE_CONVERTED.

ENDFORM.                    " CONVERT_DATE_FORMAT
*&---------------------------------------------------------------------*
*&      Form  init_data_frm
*&---------------------------------------------------------------------*
FORM F_SEND_DATA TABLES FT_HEADER
                        FT_DETAIL
                        FT_SUM
                        FT_FOOTER.

  DATA : LT_MONTH_NAMES	TYPE TABLE OF T247,
         LS_MONTH_NAMES	TYPE T247.

  DATA : BEGIN OF LS_HEADER,
           VBELN   TYPE C LENGTH 255,
           REFNO   TYPE C LENGTH 255,
           DATE    TYPE C LENGTH 255,
           NAME1   TYPE C LENGTH 255,
           ADDE1   TYPE C LENGTH 255,
           ADDE2   TYPE C LENGTH 255,
           SUJEC   TYPE C LENGTH 255,
           LOCAT   TYPE C LENGTH 255,
           WORKP   TYPE C LENGTH 255,
           WORKC   TYPE C LENGTH 255,
           TERMP   TYPE C LENGTH 255,
           WATYC   TYPE C LENGTH 255,
           VALID   TYPE C LENGTH 255,
           DISCH   TYPE C LENGTH 255,
           KUNNR   TYPE C LENGTH 255,
           TEL     TYPE C LENGTH 255,
           CO_EMP  TYPE C LENGTH 255,
           CO_TEL  TYPE C LENGTH 255,
           SNAME   TYPE C LENGTH 255,
           QR      TYPE STRING,
           QR_DESC TYPE C LENGTH 255,
           REMARK  TYPE C LENGTH 255,
           COMMENT TYPE C LENGTH 255,
           REQ_RE  TYPE C LENGTH 255,
         END OF LS_HEADER.
  DATA LT_HEADER LIKE TABLE OF LS_HEADER.

  DATA : BEGIN OF LS_FOOTER,
           VBELN TYPE VBAK-VBELN,
           REMK1 TYPE C LENGTH 255,
           REMK2 TYPE C LENGTH 255,
           REMK3 TYPE C LENGTH 255,
           REMK4 TYPE C LENGTH 255,
           QTONA TYPE C LENGTH 255,
           APPNA TYPE C LENGTH 255,
           SIGQT TYPE C LENGTH 255,
           SIGAP TYPE C LENGTH 255,
         END OF LS_FOOTER.
  DATA LT_FOOTER LIKE TABLE OF LS_FOOTER.

  DATA : BEGIN OF LS_DETAIL,
           VBELN TYPE VBAP-VBELN,
           POSNR TYPE VBAP-POSNR,
           LINE  TYPE C LENGTH 255,
           DESC  TYPE C LENGTH 255,
           QTY   TYPE C LENGTH 255,
           UNIP  TYPE C LENGTH 255,
           DISC  TYPE C LENGTH 255,
           NETP  TYPE C LENGTH 255,
           AMOT  TYPE C LENGTH 255,
           MATNR TYPE C LENGTH 255,
           PW_SP TYPE C LENGTH 255,
           CHILD TYPE STRING,
         END OF LS_DETAIL.
  DATA LT_DETAIL LIKE TABLE OF LS_DETAIL.

  DATA : BEGIN OF LS_SUM,
           VBELN TYPE VBAK-VBELN,
           SUBTO TYPE C LENGTH 255,
           VAT   TYPE C LENGTH 255,
           GRAND TYPE C LENGTH 255,
           QTY   TYPE C LENGTH 255,
           DIS   TYPE C LENGTH 255,
         END OF LS_SUM.
  DATA LT_SUM LIKE TABLE OF LS_SUM.

  DATA : BEGIN OF LS_VBAK,
           VBELN TYPE VBAK-VBELN,
           AUDAT TYPE VBAK-AUDAT,
           VTEXT TYPE TVZBT-VTEXT,
           ZTAG1 TYPE T052-ZTAG1,
           KNUMV TYPE VBAK-KNUMV,
           ANGDT TYPE VBAK-ANGDT, "valid from
           BNDDT TYPE VBAK-BNDDT, "valid to
         END OF LS_VBAK.
  DATA LT_VBAK LIKE TABLE OF LS_VBAK.

  DATA : BEGIN OF LS_KONV,
           KNUMV TYPE KONV-KNUMV,
           KPOSN TYPE KONV-KPOSN,
           KWERT TYPE KONV-KWERT,
         END OF LS_KONV.
  DATA LT_KONV LIKE TABLE OF LS_KONV.

*  DATA : BEGIN OF LS_KNA1,
*           KUNNR      TYPE KNA1-KUNNR,
*           NAME1      TYPE ADRC-NAME1,
*           NAME2      TYPE ADRC-NAME2,
*           NAME3      TYPE ADRC-NAME3,
*           NAME4      TYPE ADRC-NAME4,
*           STREET     TYPE ADRC-STREET,
*           CITY2      TYPE ADRC-CITY2,
*           CITY1      TYPE ADRC-CITY1,
*           POST_CODE1 TYPE ADRC-POST_CODE1,
*           TEL        TYPE ADRC-TEL_NUMBER,
*           FAX        TYPE ADRC-FAX_NUMBER,
*         END OF LS_KNA1.
*  DATA LT_KNA1 LIKE TABLE OF LS_KNA1.

  DATA : LV_AMOUNT TYPE VBAK-NETWR.

  DATA : LV_VKGRP TYPE VBAK-VKGRP.

  DATA: LT_TEL     TYPE STANDARD TABLE OF BAPIADTEL,
        LT_RETURN  TYPE BAPIRET2_T,
        LS_ADDRESS TYPE BAPIADDR3.
  DATA : LV_TEXT1_QR TYPE C LENGTH 18,
         LV_TEXT2_QR TYPE C LENGTH 18,
         LV_QTY      TYPE C LENGTH 19,
         LV_DAY_DIFF TYPE I.
*--------------------------------------------------------------------*
* Get Header
*--------------------------------------------------------------------*
  "Project
  PERFORM F_READ_TEXT USING 'ZH06'
                            NAST-OBJKY
                            'VBBK'
                   CHANGING LS_HEADER-WORKP.

  "Comment
  PERFORM F_READ_TEXT USING 'ZH09'
                            NAST-OBJKY
                            'VBBK'
                   CHANGING LS_HEADER-COMMENT.

  "Remark
  PERFORM F_READ_TEXT USING 'ZH02'
                            NAST-OBJKY
                            'VBBK'
                   CHANGING LS_HEADER-REMARK.

  "Location
  PERFORM F_READ_TEXT USING 'ZH03'
                            NAST-OBJKY
                            'VBBK'
                   CHANGING LS_HEADER-LOCAT.

  "Request Remark
  PERFORM F_READ_TEXT USING 'ZH10'
                            NAST-OBJKY
                            'VBBK'
                   CHANGING LS_HEADER-REQ_RE.

  SELECT SINGLE VBAK~VBELN
                VBAK~AUDAT
                VTEXT
                ZTAG1
                KNUMV
                ANGDT
                BNDDT
    FROM VBAK
    INNER JOIN VBKD  ON VBAK~VBELN  EQ VBKD~VBELN AND
                        VBKD~POSNR  EQ SPACE
    INNER JOIN TVZBT ON VBKD~ZTERM  EQ TVZBT~ZTERM AND
                        TVZBT~SPRAS EQ SY-LANGU
    LEFT  JOIN T052  ON VBKD~ZTERM  EQ T052~ZTERM AND
                        T052~ZTAGG  EQ SPACE
    INTO LS_VBAK
    WHERE VBAK~VBELN EQ NAST-OBJKY.

  SELECT KNUMV
         KPOSN
         KWERT
    FROM KONV
    INTO TABLE LT_KONV
    WHERE KNUMV EQ LS_VBAK-KNUMV
      AND KSCHL LIKE 'ZD%' .

  SELECT SINGLE
                VBPA~KUNNR,
                ADRC~NAME1,
                ADRC~NAME2,
                ADRC~NAME3,
                ADRC~NAME4,
                ADRC~STREET,
                ADRC~STR_SUPPL3,
                ADRC~LOCATION,
                ADRC~CITY2,
                ADRC~CITY1,
                ADRC~POST_CODE1,
                ADRC~TEL_NUMBER,
                ADRC~FAX_NUMBER
    FROM VBPA
*    INNER JOIN kna1 ON vbak~kunnr  EQ kna1~kunnr
    INNER JOIN ADRC ON VBPA~ADRNR  EQ ADRC~ADDRNUMBER AND
                       ADRC~NATION EQ @SPACE
    INTO @DATA(LS_KNA1)
    WHERE VBELN EQ @LS_VBAK-VBELN
      AND POSNR EQ @SPACE
      AND PARVW EQ 'AG'.

  LS_HEADER-KUNNR = LS_KNA1-KUNNR.
  IF LS_KNA1-TEL_NUMBER IS NOT INITIAL.
    LS_HEADER-TEL   = |Tel : { LS_KNA1-TEL_NUMBER } Fax : { LS_KNA1-FAX_NUMBER }|.
  ENDIF.
  CONCATENATE LS_KNA1-NAME1 LS_KNA1-NAME2 LS_KNA1-NAME3 LS_KNA1-NAME4   INTO LS_HEADER-NAME1 SEPARATED BY SPACE.
  CONCATENATE LS_KNA1-STREET LS_KNA1-STR_SUPPL3 LS_KNA1-LOCATION        INTO LS_HEADER-ADDE1 SEPARATED BY SPACE.
  CONCATENATE LS_KNA1-CITY2 LS_KNA1-CITY1 LS_KNA1-POST_CODE1            INTO LS_HEADER-ADDE2 SEPARATED BY SPACE.

  DATA : LV_KWERT LIKE LS_KONV-KWERT.
  LOOP AT LT_KONV INTO LS_KONV.
    LS_KONV-KWERT = LS_KONV-KWERT * -1.
    ADD LS_KONV-KWERT TO LV_KWERT.
    CLEAR : LS_KONV.
  ENDLOOP.

  IF LV_KWERT NE 0.
    WRITE LV_KWERT TO LS_HEADER-DISCH.
    REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN LS_HEADER-DISCH WITH ''.
  ENDIF.

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      LANGUAGE              = '2'
    TABLES
      MONTH_NAMES           = LT_MONTH_NAMES
    EXCEPTIONS
      MONTH_NAMES_NOT_FOUND = 1
      OTHERS                = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  READ TABLE LT_MONTH_NAMES INTO LS_MONTH_NAMES
  WITH KEY MNR = LS_VBAK-AUDAT+4(2).
  IF SY-SUBRC = 0.
    DATA(LV_BE_YEAR) = LS_VBAK-AUDAT+0(4) + 543.
    LS_HEADER-DATE = |{ LS_VBAK-AUDAT+6(2) } { LS_MONTH_NAMES-LTX } { LV_BE_YEAR }|.
  ENDIF.

  SELECT SINGLE VKGRP
    FROM VBAK
    INTO LV_VKGRP
    WHERE VBELN EQ NAST-OBJKY.

  LS_HEADER-VBELN = LS_VBAK-VBELN.
  LS_HEADER-TERMP = LS_VBAK-VTEXT.

  CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
    EXPORTING
      BEGDA = LS_VBAK-ANGDT
      ENDDA = LS_VBAK-BNDDT
    IMPORTING
      DAYS  = LV_DAY_DIFF.

  LS_HEADER-VALID = LV_DAY_DIFF && ' วัน'.

  "Get Admin Data
  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      USERNAME = SY-UNAME
    IMPORTING
      ADDRESS  = LS_ADDRESS
    TABLES
      RETURN   = LT_RETURN
      ADDTEL   = LT_TEL.
  LOOP AT LT_RETURN TRANSPORTING NO FIELDS
                    WHERE TYPE = 'E'
                       OR TYPE = 'A'.
    "error case
    EXIT.
  ENDLOOP.
  IF SY-SUBRC <> 0.
    IF LT_TEL IS NOT INITIAL.
      DATA(LS_TEL) = LT_TEL[ 1 ].
      LS_HEADER-CO_TEL = LS_TEL-EXTENSION.                                  "Cordinator Tel.
    ENDIF.

    LS_HEADER-CO_EMP = |{ LS_ADDRESS-FIRSTNAME } { LS_ADDRESS-LASTNAME }|.  "Cordinator Name
  ENDIF.

  "Get Sale_emp Data
  SELECT SINGLE *
    FROM VBPA
    INTO @DATA(LS_VBPA)
    WHERE VBELN = @LS_VBAK-VBELN
      AND PARVW = 'VE'.
  IF SY-SUBRC = 0.
    SELECT SINGLE *
      FROM ZSDSSDT014
      INTO @DATA(LS_SALEEMP)
      WHERE PERNR = @LS_VBPA-PERNR.
    IF SY-SUBRC = 0.
      LS_HEADER-SNAME = |{ LS_SALEEMP-FIRST_NAME } { LS_SALEEMP-LAST_NAME }|.
    ENDIF.
  ENDIF.

  "QR
  LV_TEXT1_QR = LS_HEADER-KUNNR.

  CONCATENATE "''
               '|010552500823702' "cl_abap_char_utilities=>newline
               LV_TEXT1_QR        "cl_abap_char_utilities=>newline
               LV_TEXT2_QR        "cl_abap_char_utilities=>newline
               '0'             INTO LS_HEADER-QR SEPARATED BY '\c013\' ."'\c013\'."cl_abap_char_utilities=>cr_lf.


  APPEND LS_HEADER TO LT_HEADER.
*--------------------------------------------------------------------*
* Get Item
*--------------------------------------------------------------------*

  DATA : BEGIN OF LS_VBAP,
           VBELN  TYPE VBAP-VBELN,
           POSNR  TYPE VBAP-POSNR,
           KWMENG TYPE VBAP-KWMENG,
           NETWR  TYPE VBAP-NETWR,
           MWSBP  TYPE VBAP-MWSBP,
           ARKTX  TYPE VBAP-ARKTX,
           MATNR  TYPE VBAP-MATNR,
           UEPOS  TYPE VBAP-UEPOS,
         END OF LS_VBAP.
  DATA LT_VBAP LIKE TABLE OF LS_VBAP.

  DATA : LV_TEXT1 TYPE C LENGTH 255,
         LV_TEXT2 TYPE C LENGTH 255.

  DATA : LV_SUB   TYPE VBAK-NETWR,
         LV_VAT   TYPE VBAK-NETWR,
         LV_GAN   TYPE VBAK-NETWR,
         LV_KMPMG TYPE VBAP-KMPMG.

  DATA: LV_SUM_DIS TYPE PRCD_ELEMENTS-KWERT,
        LV_SUM_UNIT TYPE PRCD_ELEMENTS-KWERT.
  DATA: LV_INDEX TYPE N LENGTH 4.

  SELECT VBELN
         POSNR
         KWMENG
         NETWR
         MWSBP
         ARKTX
         MATNR
         UEPOS
    FROM VBAP
    INTO TABLE LT_VBAP
    WHERE VBELN EQ LS_VBAK-VBELN.

*  SELECT SINGLE knumv
*                kposn
*                kwert
*    FROM prcd_elements
*    INTO ls_konv
*    WHERE knumv EQ ls_vbak-knumv
*      AND kschl LIKE 'ZWK0'.
  IF LT_VBAP[] IS NOT INITIAL.

    SORT LT_VBAP BY VBELN POSNR.
    SELECT MARA~MATNR,
           MARA~ZZVOL,
           MARA~ZZPHA,
           MARA~ZZFRE,
           ZDSMMC028~DESCRIPTION AS ZZVOL_DESC,
           ZDSMMC028~DESCRIPTION AS ZZFRE_DESC
      FROM MARA
      LEFT OUTER JOIN ZDSMMC028
      ON MARA~ZZVOL = ZDSMMC028~ZZVOL
      LEFT OUTER JOIN ZDSMMC009
      ON MARA~ZZFRE = ZDSMMC009~ZZFRE
      INTO TABLE @DATA(LT_MARA)
      FOR ALL ENTRIES IN @LT_VBAP
      WHERE MATNR = @LT_VBAP-MATNR.
    IF SY-SUBRC = 0.
      SORT LT_MARA BY MATNR.
    ENDIF.
  ENDIF.

  IF LS_VBAK-KNUMV IS NOT INITIAL.
    SELECT
      KNUMV,
      KSCHL,
      KPOSN,
      KWERT,
      KBETR
    FROM PRCD_ELEMENTS
    INTO TABLE @DATA(LT_PRCD_ELEMENTS)
    WHERE KNUMV EQ @LS_VBAK-KNUMV.
    IF SY-SUBRC = 0.
      SORT LT_PRCD_ELEMENTS BY KSCHL KPOSN.
    ENDIF.
  ENDIF.

  "Clssify mom/child material [BOM]

  DATA: LT_VBAP_CHILD LIKE LT_VBAP.
  LT_VBAP_CHILD[] = LT_VBAP[].

  DELETE  LT_VBAP       WHERE UEPOS IS NOT INITIAL.
  DELETE  LT_VBAP_CHILD WHERE UEPOS IS INITIAL.
  SORT LT_VBAP_CHILD BY UEPOS.

  CLEAR LS_VBAP-POSNR.
  LOOP AT LT_VBAP INTO LS_VBAP.

    CLEAR: LV_SUM_DIS,
           LV_SUM_UNIT.

    LV_INDEX = LV_INDEX + 1.
    LS_DETAIL-VBELN = LS_VBAP-VBELN.
    LS_DETAIL-POSNR = LS_VBAP-POSNR.
    LS_DETAIL-LINE  = LV_INDEX.
    LS_DETAIL-DESC  = LS_VBAP-ARKTX.
    LS_DETAIL-QTY   = LS_VBAP-KWMENG.
    LS_DETAIL-MATNR = LS_VBAP-MATNR.

    SPLIT LS_DETAIL-QTY AT '.' INTO LV_TEXT1
                                    LV_TEXT2.

    LS_DETAIL-QTY = LV_TEXT1.
    REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN LS_DETAIL-QTY WITH ''.

    "Amount per unit
    READ TABLE LT_PRCD_ELEMENTS INTO DATA(LS_PRCD_ELEMENTS)
                                WITH KEY KSCHL = 'ZPR0'
                                         KPOSN = LS_VBAP-POSNR
                                         BINARY SEARCH.
    IF SY-SUBRC = 0.
      LV_SUM_UNIT = LV_SUM_UNIT + ABS( LS_PRCD_ELEMENTS-KBETR ).
    ENDIF.

    LOOP AT LT_PRCD_ELEMENTS INTO LS_PRCD_ELEMENTS
                             WHERE KSCHL+0(3) = 'ZDH'
                              AND KPOSN = LS_VBAP-POSNR.
      LV_SUM_DIS = LV_SUM_DIS + ABS( LS_PRCD_ELEMENTS-KWERT ).
    ENDLOOP.

    "BOM case
    READ TABLE LT_VBAP_CHILD TRANSPORTING NO FIELDS
                             WITH KEY UEPOS = LS_VBAP-POSNR
                             BINARY SEARCH.
    IF SY-SUBRC = 0.
      LOOP AT LT_VBAP_CHILD INTO DATA(LS_VBAP_CHILD) FROM SY-TABIX.

        IF LS_VBAP_CHILD-UEPOS <> LS_VBAP-POSNR.
          EXIT.
        ENDIF.

        LV_QTY = LS_DETAIL-QTY.
        SPLIT LV_QTY  AT '.' INTO LV_TEXT1
                                  LV_TEXT2.
        LV_QTY = LV_TEXT1.


        IF SY-TABIX = 1.
          LS_DETAIL-CHILD = |{ LS_VBAP_CHILD-MATNR }({ 1 })|. "lv_qty
        ELSE.
          LS_DETAIL-CHILD = LS_DETAIL-CHILD && '+' && |{ LS_VBAP_CHILD-MATNR }({ 1 })|.
        ENDIF.

        "Discount
        LOOP AT LT_PRCD_ELEMENTS INTO LS_PRCD_ELEMENTS
                                 WHERE KSCHL+0(3) = 'ZDH'
                                  AND KPOSN = LS_VBAP_CHILD-POSNR.
          LV_SUM_DIS = LV_SUM_DIS + ABS( LS_PRCD_ELEMENTS-KWERT ).
        ENDLOOP.

        "Unit/Price
        READ TABLE LT_PRCD_ELEMENTS INTO LS_PRCD_ELEMENTS
                                    WITH KEY KSCHL = 'ZPR0'
                                             KPOSN = LS_VBAP_CHILD-POSNR
                                             BINARY SEARCH.
        IF SY-SUBRC = 0.
          LV_SUM_UNIT = LV_SUM_UNIT + ABS( LS_PRCD_ELEMENTS-KBETR ).
        ENDIF.

        "Sum to parent item
        ADD LS_VBAP_CHILD-NETWR TO LS_VBAP-NETWR.
        ADD LS_VBAP_CHILD-MWSBP TO LS_VBAP-MWSBP.

      ENDLOOP.
    ENDIF.

    "Unit Price
    WRITE LV_SUM_UNIT TO LS_DETAIL-UNIP DECIMALS 2.
    REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN LS_DETAIL-UNIP WITH ''.

    "Discount
    LV_SUM_DIS = LV_SUM_DIS / LS_DETAIL-QTY.
    WRITE LV_SUM_DIS TO LS_DETAIL-DISC DECIMALS 2.
    REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN LS_DETAIL-DISC WITH ''.

    "Net Price
    CLEAR : LV_AMOUNT.
    LV_AMOUNT = LS_VBAP-NETWR / LS_DETAIL-QTY.
    WRITE LV_AMOUNT TO LS_DETAIL-NETP.
    REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN LS_DETAIL-NETP WITH ''.

    "Amount
    WRITE LS_VBAP-NETWR TO LS_DETAIL-AMOT DECIMALS 2.
    REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN LS_DETAIL-AMOT WITH ''.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = LS_DETAIL-LINE
      IMPORTING
        OUTPUT = LS_DETAIL-LINE.
    REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN LS_DETAIL-LINE WITH ''.


    READ TABLE LT_MARA INTO DATA(LS_MARA)
                       WITH KEY MATNR = LS_DETAIL-MATNR
                       BINARY SEARCH.
    IF SY-SUBRC = 0.
      LS_DETAIL-PW_SP = |{ LS_MARA-ZZVOL_DESC }/{ LS_MARA-ZZPHA }/{ LS_MARA-ZZFRE_DESC }|.
    ENDIF.

    ADD LS_VBAP-NETWR TO LV_SUB.
    ADD LS_VBAP-MWSBP TO LV_VAT.
    ADD LS_DETAIL-QTY TO LV_KMPMG.

    APPEND LS_DETAIL TO LT_DETAIL.
    CLEAR : LS_DETAIL, LS_VBAP.
  ENDLOOP.
*--------------------------------------------------------------------*
* Sum
*--------------------------------------------------------------------*
  LV_SUB = LV_SUB." - lv_kwert.
  LV_GAN = LV_SUB + LV_VAT.

  WRITE LV_SUB TO LS_SUM-SUBTO.
  WRITE LV_VAT TO LS_SUM-VAT.
  WRITE LV_GAN TO LS_SUM-GRAND.
  WRITE LV_KMPMG TO LS_SUM-QTY.
  SPLIT LS_SUM-QTY AT '.' INTO LV_TEXT1
                               LV_TEXT2.
  LS_SUM-QTY = LV_TEXT1.

  READ TABLE LT_PRCD_ELEMENTS INTO LS_PRCD_ELEMENTS
                             WITH KEY KSCHL(3) = 'ZDH'
                                      KPOSN    = '00000'.
  IF SY-SUBRC = 0.
    IF LS_PRCD_ELEMENTS-KBETR < 0.
      LS_PRCD_ELEMENTS-KWERT = LS_PRCD_ELEMENTS-KWERT * -1.
    ENDIF.
    WRITE LS_PRCD_ELEMENTS-KBETR TO LS_SUM-DIS.
  ENDIF.

  REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN LS_SUM-SUBTO WITH ''.
  REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN LS_SUM-VAT WITH ''.
  REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN LS_SUM-GRAND WITH ''.
  REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN LS_SUM-QTY WITH ''.
  REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN LS_SUM-DIS WITH ''.

  APPEND LS_SUM TO LT_SUM.
*--------------------------------------------------------------------*
* Footer
*--------------------------------------------------------------------*
  PERFORM F_GET_FOOTER TABLES LT_FOOTER
                        USING NAST-OBJKY.

*--------------------------------------------------------------------*
* Send Data
*--------------------------------------------------------------------*
  FT_HEADER[] = LT_HEADER[].
  FT_DETAIL[] = LT_DETAIL[].
  FT_SUM[]    = LT_SUM[].
  FT_FOOTER[] = LT_FOOTER[].


ENDFORM.                    " init_data_frm
*&---------------------------------------------------------------------*
*&      Form  F_READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_READ_TEXT USING LV_ID_IN
                       LV_NAME_IN
                       LV_OBJECT_IN
              CHANGING LV_TEXT_OUT.

  DATA : LV_LANGUAGE TYPE THEAD-TDSPRAS.

  DATA : LT_LINES TYPE TABLE OF TLINE,
         LS_LINES TYPE TLINE.

  DATA : LV_ID     TYPE THEAD-TDID,
         LV_NAME   TYPE THEAD-TDNAME,
         LV_OBJECT TYPE THEAD-TDOBJECT,
         LV_TEXT   TYPE STRING.

  CONSTANTS : BEGIN OF LC_READ_TEXT,
                ID TYPE C LENGTH 4 VALUE 'Z012',
                OB TYPE C LENGTH 4 VALUE 'VBBK',
              END OF LC_READ_TEXT.
*--------------------------------------------------------------------*
  LV_LANGUAGE = SY-LANGU.
  LV_ID       = LV_ID_IN.
  LV_NAME     = LV_NAME_IN.
  LV_OBJECT   = LV_OBJECT_IN.
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
  IF LV_ID_IN EQ 'Z020'.
    PERFORM F_GET_MONTH TABLES LT_LINES
                      CHANGING LV_TEXT.
    IF LV_TEXT IS INITIAL.
      LOOP AT LT_LINES INTO LS_LINES.
        IF LV_TEXT IS NOT INITIAL.
          CONCATENATE LV_TEXT LS_LINES-TDLINE INTO LV_TEXT.
        ELSE.
          LV_TEXT = LS_LINES-TDLINE.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSE.
    IF LT_LINES[] IS NOT INITIAL.
      LOOP AT LT_LINES INTO LS_LINES.
        IF LV_TEXT IS NOT INITIAL.
          CONCATENATE LV_TEXT LS_LINES-TDLINE INTO LV_TEXT.
        ELSE.
          LV_TEXT = LS_LINES-TDLINE.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  LV_TEXT_OUT = LV_TEXT.

ENDFORM.                    " F_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  F_GET_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_LINES  text
*      <--P_LV_TEXT  text
*      <--P_MONTH_NAMES_GET  text
*      <--P_ELSE  text
*----------------------------------------------------------------------*
FORM F_GET_MONTH  TABLES FT_LINES
                CHANGING LV_TEXT.

  DATA : LT_LINES TYPE TABLE OF TLINE,
         LS_LINES TYPE TLINE.

  DATA : LT_MONTH_NAMES	TYPE TABLE OF T247,
         LS_MONTH_NAMES	TYPE T247.

  DATA : LV_TABIX TYPE SY-TABIX.

  LT_LINES[] = FT_LINES[].


  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      LANGUAGE              = '2'
    TABLES
      MONTH_NAMES           = LT_MONTH_NAMES
    EXCEPTIONS
      MONTH_NAMES_NOT_FOUND = 1
      OTHERS                = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT LT_LINES INTO LS_LINES.
    LV_TABIX = SY-TABIX.

    IF LV_TABIX EQ 1.
      READ TABLE LT_MONTH_NAMES INTO LS_MONTH_NAMES
      WITH KEY MNR = LS_LINES+3(2).
      IF SY-SUBRC = 0.
        CONCATENATE 'เริ่ม'  LS_LINES+0(2) LS_MONTH_NAMES-LTX LS_LINES+6(4) INTO LV_TEXT SEPARATED BY SPACE.
      ENDIF.
    ELSE.
      READ TABLE LT_MONTH_NAMES INTO LS_MONTH_NAMES
      WITH KEY MNR = LS_LINES+3(2).
      IF SY-SUBRC = 0.
        CONCATENATE LV_TEXT 'ถึง'  LS_LINES+0(2) LS_MONTH_NAMES-LTX LS_LINES+6(4) INTO LV_TEXT SEPARATED BY SPACE.
      ENDIF.
    ENDIF.

    CLEAR : LS_LINES.
  ENDLOOP.

ENDFORM.                    " F_GET_MONTH
*&---------------------------------------------------------------------*
*&      Form  F_GET_REMARK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_FOOTER  text
*      -->P_NAST_OBJKY  text
*----------------------------------------------------------------------*
FORM F_GET_FOOTER TABLES FT_FOOTER
                   USING LV_VBELN.

  CONSTANTS: LC_VKGRP_AP TYPE ZSDSCAC001-PARAM VALUE 'VKGRP_AP',
             LC_REPORT_N TYPE ZSDSCAC001-REPID VALUE 'ZSDSSDR0060'.
  DATA : BEGIN OF LS_FOOTER,
           VBELN TYPE VBAK-VBELN,
           REMK1 TYPE C LENGTH 255,
           REMK2 TYPE C LENGTH 255,
           REMK3 TYPE C LENGTH 255,
           REMK4 TYPE C LENGTH 255,
           QTONA TYPE C LENGTH 255,
           APPNA TYPE C LENGTH 255,
           SIGQT TYPE C LENGTH 255,
           SIGAP TYPE C LENGTH 255,
         END OF LS_FOOTER.
  DATA LT_FOOTER LIKE TABLE OF LS_FOOTER.

  DATA : BEGIN OF LS_PA0182,
           VBELN TYPE VBRP-VBELN,
           ALNAM TYPE PA0182-ALNAM,
         END OF LS_PA0182.
  DATA LT_PA0182 LIKE TABLE OF LS_PA0182.

  DATA : LV_CONST TYPE ZSDSCAC001-PARAM,
         LV_VALUE TYPE ZSDSCAC001-VALUE_LOW.
  DATA: LV_PARAM TYPE ZSDSCAC001-PARAM,
        LR_PARAM TYPE RANGE OF ZSDSCAC001-PARAM,
        LV_AP_VKGRP TYPE ZSDSCAC001-PARAM.
  DATA: LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C,
        LR_VKGRP_AP   TYPE  RANGE OF VBAK-VKGRP.

  CONSTANTS: LC_APPROVER(5) TYPE C VALUE 'ZAPPR',
             LC_MANAGER(3)  TYPE C VALUE 'MGR'.

  SELECT SINGLE VBELN
                ALNAM
    FROM VBPA
    INNER JOIN PA0182 ON VBPA~PERNR EQ PA0182~PERNR
    INTO LS_PA0182
    WHERE VBELN EQ NAST-OBJKY
      AND PARVW EQ 'VE'
      AND POSNR EQ SPACE.

  LS_FOOTER-QTONA = LS_PA0182-ALNAM.

  SELECT SINGLE
    VKBUR,
    VKGRP
    FROM VBAK
*    INTO @DATA(LV_VKBUR)
    INTO @DATA(LS_VBAK)
    WHERE VBELN EQ @NAST-OBJKY.
  IF SY-SUBRC = 0.
    LV_PARAM = GC_APPROVAL.
    REPLACE '&1' IN LV_PARAM WITH LS_VBAK-VKBUR.

    ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID  = LC_REPORT_N
                                                    IF_PARAM = LC_VKGRP_AP
                                          IMPORTING ET_RANGE  = LR_VKGRP_AP ).

    IF LS_VBAK-VKGRP IN LR_VKGRP_AP.
      CONCATENATE LV_PARAM
                  'AP'
                  INTO LV_PARAM.
    ELSE.
      "dont do anything
      CONCATENATE LV_PARAM
                  'UP'
                  INTO LV_PARAM.
    ENDIF.

    APPEND INITIAL LINE TO LR_PARAM ASSIGNING FIELD-SYMBOL(<LFS_PARAM>).
    <LFS_PARAM>-LOW = LV_PARAM.
    <LFS_PARAM>-SIGN = 'I'.
    <LFS_PARAM>-OPTION = 'EQ'.

    ZCL_SDSCA_UTILITIES=>GET_GEN_C( EXPORTING IF_REPID  = LC_REPORT_N
                                              IRT_PARAM = LR_PARAM
                                    IMPORTING ET_GEN_C  = LT_GENC ).

    READ TABLE LT_GENC INTO DATA(LS_GENC)
                       INDEX 1.
    IF Sy-subrc = 0.
      LS_FOOTER-APPNA = LS_GENC-VALUE_LOW.
    ENDIF.
  ENDIF.

*  CONCATENATE ls_pa0002-vorna ls_pa0002-nachn INTO ls_footer-qtona SEPARATED BY space.
*

  DATA : LV_LANGUAGE TYPE THEAD-TDSPRAS.

  DATA : LT_LINES TYPE TABLE OF TLINE,
         LS_LINES TYPE TLINE.

  DATA : LV_ID     TYPE THEAD-TDID,
         LV_NAME   TYPE THEAD-TDNAME,
         LV_OBJECT TYPE THEAD-TDOBJECT,
         LV_TEXT   TYPE STRING.

*--------------------------------------------------------------------*
  LV_LANGUAGE = SY-LANGU.
  LV_ID       = 'Z021'.
  LV_NAME     = NAST-OBJKY.
  LV_OBJECT   = 'VBBK'.
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


  IF LT_LINES[] IS NOT INITIAL.
    LOOP AT LT_LINES INTO LS_LINES.

      IF     SY-TABIX EQ 1.
        LS_FOOTER-REMK1 = LS_LINES-TDLINE.
      ELSEIF SY-TABIX EQ 2.
        LS_FOOTER-REMK2 = LS_LINES-TDLINE.
      ELSEIF SY-TABIX EQ 3.
        LS_FOOTER-REMK3 = LS_LINES-TDLINE.
      ELSEIF SY-TABIX EQ 4.
        LS_FOOTER-REMK4 = LS_LINES-TDLINE.
      ELSE.
        EXIT.
      ENDIF.

      CLEAR LS_LINES.
    ENDLOOP.
  ENDIF.



  APPEND LS_FOOTER TO LT_FOOTER.


  FT_FOOTER[] = LT_FOOTER[].


ENDFORM.                    " F_GET_REMARK

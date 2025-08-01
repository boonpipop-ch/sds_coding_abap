*&---------------------------------------------------------------------*
*& Include ZSDSCAI0010
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           Z_OPT_INCLUDE_ARCHIVE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_OVERLAY_FORM
*&---------------------------------------------------------------------*
FORM GET_OVERLAY_FORM USING    P_WA_BKPF     TYPE BKPF
                               REPID         TYPE SY-REPID
                      CHANGING P_WA_TOA_DARA TYPE TOA_DARA
                               P_WA_LAYOUT   TYPE ZSDSCAC007.

  DATA: LV_DOCTYPE TYPE CHAR4.

  LV_DOCTYPE = P_WA_BKPF-BLART.


  CALL FUNCTION 'Z_SDSCA_OPT_GET_OVERLAY_FORM'
    EXPORTING
      REPID      = REPID
      COMP_CODE  = P_WA_BKPF-BUKRS
      DOC_TYPE   = LV_DOCTYPE
    CHANGING
      TOA_DARA   = P_WA_TOA_DARA
      LAYOUT_SET = P_WA_LAYOUT.


ENDFORM.                    " GET_OVERLAY_FORM
*&---------------------------------------------------------------------*
*&      Form  SET_ARCHIV_PARAM
*&---------------------------------------------------------------------*
FORM SET_ARCHIV_PARAM USING    P_WA_BKPF     TYPE BKPF
                               P_WA_LAYOUT   TYPE ZSDSCAC007
                      CHANGING P_WA_TOA_DARA TYPE TOA_DARA
                               P_WA_OPTION   TYPE ITCPO
                               P_WA_ARC_P    TYPE ARC_PARAMS.

  DATA: LWA_TOAOM TYPE TOAOM.

  SELECT SINGLE *
    INTO LWA_TOAOM
    FROM TOAOM
   WHERE SAP_OBJECT EQ P_WA_LAYOUT-SAP_OBJECT
     AND AR_OBJECT  EQ P_WA_LAYOUT-AR_OBJECT
     AND AR_STATUS  EQ 'X'.

* Set TOA_DARA
  P_WA_TOA_DARA-FUNCTION   = 'DARA'.
  P_WA_TOA_DARA-MANDANT    = SY-MANDT.             "Fixed sy-mandt
  P_WA_TOA_DARA-RESERVE    = 'COMMIT'.             "Fixed with 'COMMIT'
  P_WA_TOA_DARA-NOTIZ      = SPACE.                "Fixed with space
  P_WA_TOA_DARA-SAP_OBJECT = LWA_TOAOM-SAP_OBJECT.
  P_WA_TOA_DARA-AR_OBJECT  = LWA_TOAOM-AR_OBJECT.
  CONCATENATE P_WA_BKPF-BUKRS P_WA_BKPF-BELNR P_WA_BKPF-GJAHR
         INTO P_WA_TOA_DARA-OBJECT_ID.

* Set print option
  P_WA_OPTION-TDGETOTF = 'X'.      "Get smartforms into intern
*  P_WA_OPTION-TDARMOD  = '2'.      "Set to archive mode
  P_WA_OPTION-TDNEWID  = 'X'.      "New spool
  P_WA_OPTION-TDIMMED  = 'X'.      "Act immediately
  P_WA_OPTION-TDFINAL  = 'X'.      "Close spool
  P_WA_OPTION-TDDEST   = 'SEPS'.   "Use PDFU as default

* Set archiv parameter
  P_WA_ARC_P-SAP_OBJECT = LWA_TOAOM-SAP_OBJECT.
  P_WA_ARC_P-AR_OBJECT  = LWA_TOAOM-AR_OBJECT.
  P_WA_ARC_P-ARCHIV_ID  = LWA_TOAOM-ARCHIV_ID.

ENDFORM.                    " SET_ARCHIV_PARAM
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_DOCUMENT
*&---------------------------------------------------------------------*
FORM ARCHIVE_DOCUMENT  TABLES   P_IT_OTF      STRUCTURE ITCOO
                       USING    P_WA_TOA_DARA TYPE TOA_DARA
                                P_WA_ARC_P    TYPE ARC_PARAMS.

  DATA: LV_UPDATE TYPE SY-SUBRC.

  CALL FUNCTION 'CONVERT_OTF_AND_ARCHIVE'
    EXPORTING
      ARC_P                    = P_WA_ARC_P
      ARC_I                    = P_WA_TOA_DARA
      FORMAT                   = 'PDF'
    TABLES
      OTF                      = P_IT_OTF[]
    EXCEPTIONS
      ERROR_ARCHIV             = 1
      ERROR_COMMUNICATIONTABLE = 2
      ERROR_CONNECTIONTABLE    = 3
      ERROR_KERNEL             = 4
      ERROR_PARAMETER          = 5
      ERROR_FORMAT             = 6
      OTHERS                   = 7.
  IF SY-SUBRC EQ 0.

    CALL FUNCTION 'TH_IN_UPDATE_TASK'
      IMPORTING
        IN_UPDATE_TASK = LV_UPDATE.
    IF LV_UPDATE IS INITIAL.
      COMMIT WORK.
    ENDIF.

  ELSE.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " ARCHIVE_DOCUMENT

*&---------------------------------------------------------------------*
*&      Form  GET_OVERLAY_FORM
*&---------------------------------------------------------------------*
"Add by wantanee 20150811 edit case print receipt T41K920199
FORM GET_OVERLAY_FORM_1 USING    P_WA_BKPF     TYPE BKPF
                               REPID         TYPE SY-REPID
                               LV_CHECK_RECEIPT TYPE C
                        CHANGING P_WA_TOA_DARA TYPE TOA_DARA
                               P_WA_LAYOUT   TYPE ZSDSCAC007.

  DATA: LV_DOCTYPE TYPE CHAR4.

  LV_DOCTYPE = P_WA_BKPF-BLART.

  CALL FUNCTION 'Z_SDSCA_OPT_GET_OVERLAY_FORM'
    EXPORTING
      REPID      = REPID
      COMP_CODE  = P_WA_BKPF-BUKRS
      DOC_TYPE   = LV_DOCTYPE
      PRINT_RECEIPT = LV_CHECK_RECEIPT
    CHANGING
      TOA_DARA   = P_WA_TOA_DARA
      LAYOUT_SET = P_WA_LAYOUT.


ENDFORM.                    " GET_OVERLAY_FORM

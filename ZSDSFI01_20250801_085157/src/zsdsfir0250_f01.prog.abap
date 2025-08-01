*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0250_F01
*  Creation Date      : 21.05.2024
*  Author             : Apichat Ch. (Eviden)
*  Add-on ID          : N/A
*  Description        : This is include program routine
*  Purpose            : Include program logic - collection log
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Form F_AUTHORIZE_CHECK
*----------------------------------------------------------------------*
*  Check Authorization on t-code
*----------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK USING UF_TCODE  TYPE  SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
    ID 'TCD' FIELD UF_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH UF_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_fill_celltab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LT_CELLTAB
*&---------------------------------------------------------------------*
FORM F_FILL_CELLTAB  USING VALUE(UF_MODE) TYPE CHAR02
                     CHANGING CT_CELLTAB  TYPE LVC_T_STYL.

  DATA: LV_MODE    TYPE RAW4.

  IF UF_MODE EQ 'RW'.
    LV_MODE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  ELSE. "uf_mode eq 'RO'
    LV_MODE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
  ENDIF.

  PERFORM F_ADD_CELLTAB USING LV_MODE:
        'SEL'           CHANGING CT_CELLTAB,
        'PERNR'         CHANGING CT_CELLTAB,
        'WORK_DATE'     CHANGING CT_CELLTAB,
        'DEDUCT_AMT'    CHANGING CT_CELLTAB,
        'PAYNO'         CHANGING CT_CELLTAB,
        'BRNCH'         CHANGING CT_CELLTAB,
        'EXPS_AMT'      CHANGING CT_CELLTAB,
        'AUFNR_EXPS'    CHANGING CT_CELLTAB,
        'WHT_AMT'       CHANGING CT_CELLTAB,
        'FEE'           CHANGING CT_CELLTAB,
        'AUFNR_FEE'     CHANGING CT_CELLTAB,
        'RETENTION'     CHANGING CT_CELLTAB,
*        'REFUND_AMT'    CHANGING ct_celltab,
        'INCOME_AMT'    CHANGING CT_CELLTAB,
        'CASH_CON'      CHANGING CT_CELLTAB,
        'KOSTL'         CHANGING CT_CELLTAB,
        'PRCTR'         CHANGING CT_CELLTAB,
*        'DUE_ON'        CHANGING ct_celltab,
        'HBKID'         CHANGING CT_CELLTAB,
        'HKTID'         CHANGING CT_CELLTAB,
        'PYMT_METHOD'   CHANGING CT_CELLTAB,
        'ACTION_TYPE'   CHANGING CT_CELLTAB,
        'STATUS'        CHANGING CT_CELLTAB,
        'REMARK'        CHANGING CT_CELLTAB,
        'PAYMENT_DATE'  CHANGING CT_CELLTAB,
        'RECEIVED_DATE' CHANGING CT_CELLTAB,
        'RECEIPT_NO'    CHANGING CT_CELLTAB,
        'BANK_DATE'     CHANGING CT_CELLTAB,
        'CHEQUE_NO'     CHANGING CT_CELLTAB,
        'BANKK'         CHANGING CT_CELLTAB,
        'BANKN'         CHANGING CT_CELLTAB,
        'BANKL'         CHANGING CT_CELLTAB,
        'ZBANK_ITEM'    CHANGING CT_CELLTAB,
        'VBELN'         CHANGING CT_CELLTAB,
        'FOLLOW_DATE'   CHANGING CT_CELLTAB,
        'REMARK_VENDOR' CHANGING CT_CELLTAB.

*-Beg of INS by Jutamas Y.
  PERFORM F_ADD_CELLTAB USING LV_MODE:
        'BILLPL_NO'           CHANGING CT_CELLTAB,
        'BILLPL_DATE'         CHANGING CT_CELLTAB.
*-End of INS by Jutamas Y.


  CASE GV_DATA_TYPE.
    WHEN GC_TAB_SEL-UPDATE_BANK.

      PERFORM F_ADD_CELLTAB USING LV_MODE:
        'DOCUMENT_STATUS'   CHANGING CT_CELLTAB,
        'INV_STATUS'        CHANGING CT_CELLTAB,
        'REASON_VENDOR'     CHANGING CT_CELLTAB,
        'BILLING_CYCLE'     CHANGING CT_CELLTAB,
        'COLLECTION_CYCLE'  CHANGING CT_CELLTAB,
        'AWB_NO'            CHANGING CT_CELLTAB.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_add_celltab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_ADD_CELLTAB USING UF_MODE TYPE RAW4
                         UF_FIELD TYPE LVC_FNAME
                   CHANGING CT_CELLTAB TYPE LVC_T_STYL.

  DATA: LS_CELLTAB TYPE LVC_S_STYL.

  LS_CELLTAB-FIELDNAME = UF_FIELD.
  LS_CELLTAB-STYLE = UF_MODE.
  INSERT LS_CELLTAB INTO TABLE CT_CELLTAB.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_display_result
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_OUTPUT
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UF_VAR    TYPE DISVARIANT-VARIANT
                              UT_RESULT TYPE TT_OUTPUT.

* Set Container name
  GF_CONTAINER = 'CTL_ALV'.
* Disable Header area
  GF_ALV_HEADER = GC_TRUE.

*   No auto refresh in edit mode
  GF_NO_AUTO_REFRESH = GC_TRUE.

* ALV Layout
  PERFORM F_ALV_LAYOUT USING UF_VAR
                       CHANGING GS_LAYOUT
                                GS_VARIANT
                                GS_PRINT.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT = GC_HEADER_HEIGHT.
  GF_ALV_HEIGHT   = GC_ALV_HEIGHT.
  ASSIGN UT_RESULT TO <G_LIST>.              "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT CHANGING GT_FIELDCAT.
* Sort data
  PERFORM F_ALV_SORT_RESULT CHANGING GT_SORT.
* Call ALV Screen
  CALL SCREEN 9000.


ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT USING    UF_VARIANT TYPE  DISVARIANT-VARIANT
                  CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                           CS_VARIANT TYPE  DISVARIANT
                           CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-SEL_MODE   = 'B'. "Multiple Selection with Push Box
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.
  CS_LAYOUT-NO_ROWMARK = GC_TRUE.

  IF GV_DATA_TYPE = GC_TAB_SEL-COLLECTION_LOG OR
     GV_DATA_TYPE = GC_TAB_SEL-UPDATE_BANK OR
     GV_DATA_TYPE = GC_TAB_SEL-MEMO.
    CS_LAYOUT-STYLEFNAME = 'CELLTAB'.
  ENDIF.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.
  CS_VARIANT-VARIANT = UF_VARIANT.

  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.


  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.

* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.
    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'SEL'.
        <L_FIELDCAT>-EDIT       = GV_EDIT.
        <L_FIELDCAT>-CHECKBOX   = GC_TRUE.
        <L_FIELDCAT>-COL_POS    = 10.
        <L_FIELDCAT>-KEY        = GC_TRUE.
        CASE GV_DATA_TYPE.
          WHEN GC_TAB_SEL-COLLECTION_LOG.
            IF RB_DIS = GC_TRUE.
              <L_FIELDCAT>-TECH     = GC_TRUE.
            ENDIF.
          WHEN GC_TAB_SEL-MEMO.
            IF RB_CREM = GC_TRUE OR
               RB_DISM = GC_TRUE.
              <L_FIELDCAT>-TECH     = GC_TRUE.
            ENDIF.
        ENDCASE.

      WHEN 'DELETE_FLAG'.
        IF RB_DIS = GC_TRUE.
          <L_FIELDCAT>-COL_POS    = 15  ##NUMBER_OK.
          <L_FIELDCAT>-KEY        = GC_TRUE.
        ELSE.
          <L_FIELDCAT>-TECH     = GC_TRUE.
        ENDIF.
      WHEN 'TRANF_NO'.
        <L_FIELDCAT>-COL_POS    = 20   ##NUMBER_OK.
        <L_FIELDCAT>-KEY        = GC_TRUE.
      WHEN 'BUKRS'.
        <L_FIELDCAT>-COL_POS    = 25    ##NUMBER_OK.
        <L_FIELDCAT>-KEY        = GC_TRUE.
      WHEN 'PERNR'.
        <L_FIELDCAT>-COL_POS    = 30    ##NUMBER_OK.
        <L_FIELDCAT>-KEY        = GC_TRUE.
      WHEN 'FULL_NAME'.
        <L_FIELDCAT>-COL_POS    = 40    ##NUMBER_OK.
        <L_FIELDCAT>-KEY        = GC_TRUE.
      WHEN 'WORK_DATE'.
        <L_FIELDCAT>-COL_POS    = 50    ##NUMBER_OK.
        <L_FIELDCAT>-KEY        = GC_TRUE.
      WHEN 'KUNNR'.
        <L_FIELDCAT>-COL_POS    = 60    ##NUMBER_OK.
      WHEN 'CUST_NAME'.
        <L_FIELDCAT>-COL_POS    = 70    ##NUMBER_OK.
      WHEN 'UMSKZ'.
        <L_FIELDCAT>-COL_POS    = 80    ##NUMBER_OK.
      WHEN 'BILLPL_NO'.
        <L_FIELDCAT>-COL_POS    = 90    ##NUMBER_OK.
      WHEN 'BILLPL_DATE'.
        <L_FIELDCAT>-COL_POS    = 100   ##NUMBER_OK.
      WHEN 'XBLNR'. "Tax Invoice
        <L_FIELDCAT>-SELTEXT    = TEXT-F11.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-F11.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-F11.
        <L_FIELDCAT>-REPTEXT    = TEXT-F11.

        <L_FIELDCAT>-COL_POS    = 110   ##NUMBER_OK.
      WHEN 'BLDAT'.
        <L_FIELDCAT>-COL_POS    = 120   ##NUMBER_OK.
      WHEN 'BELNR'.
        <L_FIELDCAT>-HOTSPOT    = GC_TRUE.
        <L_FIELDCAT>-COL_POS    = 130   ##NUMBER_OK.
      WHEN 'VBELN_VF'.
        <L_FIELDCAT>-HOTSPOT    = GC_TRUE.
        <L_FIELDCAT>-COL_POS    = 140   ##NUMBER_OK.
      WHEN 'BUDAT'.
        <L_FIELDCAT>-COL_POS    = 150   ##NUMBER_OK.
      WHEN 'FAEDT'.
        <L_FIELDCAT>-COL_POS    = 160   ##NUMBER_OK.
      WHEN 'WRBTR'.
        <L_FIELDCAT>-COL_POS    = 170   ##NUMBER_OK.
        <L_FIELDCAT>-DO_SUM     = GC_TRUE.
      WHEN 'WAERS'.
        <L_FIELDCAT>-COL_POS    = 180   ##NUMBER_OK.
      WHEN 'DEDUCT_AMT'.
        <L_FIELDCAT>-COL_POS    = 190   ##NUMBER_OK.
        <L_FIELDCAT>-DO_SUM     = GC_TRUE.
*      WHEN 'PAYNO'.
*        <l_fieldcat>-col_pos    = 200   ##NUMBER_OK.
      WHEN 'BRNCH'.
        <L_FIELDCAT>-COL_POS    = 210   ##NUMBER_OK.
      WHEN 'BAL_AMT'.
        <L_FIELDCAT>-COL_POS    = 220   ##NUMBER_OK.
        <L_FIELDCAT>-DO_SUM     = GC_TRUE.
      WHEN 'EXPS_AMT'.
        <L_FIELDCAT>-COL_POS    = 230   ##NUMBER_OK.
        <L_FIELDCAT>-DO_SUM     = GC_TRUE.
      WHEN 'AUFNR_EXPS'.
        <L_FIELDCAT>-SELTEXT    = TEXT-F09.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-F09.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-F09.
        <L_FIELDCAT>-REPTEXT    = TEXT-F09.

        <L_FIELDCAT>-COL_POS    = 235   ##NUMBER_OK.
        <L_FIELDCAT>-F4AVAILABL = GC_TRUE.
      WHEN 'WHT_AMT'.
        <L_FIELDCAT>-COL_POS    = 240   ##NUMBER_OK.
        <L_FIELDCAT>-DO_SUM     = GC_TRUE.
      WHEN 'FEE'.
        <L_FIELDCAT>-COL_POS    = 250   ##NUMBER_OK.
        <L_FIELDCAT>-DO_SUM     = GC_TRUE.

      WHEN 'AUFNR_FEE'.
        <L_FIELDCAT>-SELTEXT    = TEXT-F10.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-F10.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-F10.
        <L_FIELDCAT>-REPTEXT    = TEXT-F10.

        <L_FIELDCAT>-COL_POS    = 255   ##NUMBER_OK.
        <L_FIELDCAT>-F4AVAILABL = GC_TRUE.

      WHEN 'RECEIVED_AMT'.
        <L_FIELDCAT>-COL_POS    = 290   ##NUMBER_OK.
        <L_FIELDCAT>-DO_SUM     = GC_TRUE.
      WHEN 'RETENTION'.
        <L_FIELDCAT>-COL_POS    = 300   ##NUMBER_OK.
        <L_FIELDCAT>-DO_SUM     = GC_TRUE.
*        <l_fieldcat>-tech       = gc_true.
*      WHEN 'REFUND_AMT'.
*        <l_fieldcat>-col_pos    = 310.
*        <l_fieldcat>-do_sum     = gc_true.
      WHEN 'INCOME_AMT'.
        <L_FIELDCAT>-COL_POS    = 320   ##NUMBER_OK.
        <L_FIELDCAT>-DO_SUM     = GC_TRUE.
      WHEN 'CASH_CON'.
        <L_FIELDCAT>-COL_POS    = 330   ##NUMBER_OK.
        <L_FIELDCAT>-DO_SUM     = GC_TRUE.
      WHEN 'PAYIN_AMT'.
        <L_FIELDCAT>-COL_POS    = 340   ##NUMBER_OK.
        <L_FIELDCAT>-DO_SUM     = GC_TRUE.
      WHEN 'REMAIN_AMT'.
        <L_FIELDCAT>-SELTEXT    = TEXT-F07.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-F07.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-F07.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-F07.
        <L_FIELDCAT>-REPTEXT    = TEXT-F07.

        <L_FIELDCAT>-COL_POS    = 350   ##NUMBER_OK.
        <L_FIELDCAT>-DO_SUM     = GC_TRUE.
        <L_FIELDCAT>-HOTSPOT    = GC_TRUE.
      WHEN 'REMAIN_C_AMT'.
        <L_FIELDCAT>-SELTEXT    = TEXT-F08.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-F08.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-F08.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-F08.
        <L_FIELDCAT>-REPTEXT    = TEXT-F08.

        <L_FIELDCAT>-COL_POS    = 360   ##NUMBER_OK.
        <L_FIELDCAT>-DO_SUM     = GC_TRUE.
*        <l_fieldcat>-no_out     = gc_true.
      WHEN 'KOSTL'.
        <L_FIELDCAT>-COL_POS    = 370   ##NUMBER_OK.
      WHEN 'PRCTR'.
        <L_FIELDCAT>-COL_POS    = 380   ##NUMBER_OK.
*      WHEN 'DUE_ON'.
*        <l_fieldcat>-seltext    = TEXT-f01.
*        <l_fieldcat>-scrtext_s  = TEXT-f01.
*        <l_fieldcat>-scrtext_m  = TEXT-f01.
*        <l_fieldcat>-scrtext_l  = TEXT-f01.
*        <l_fieldcat>-col_pos    = 390   ##NUMBER_OK.
      WHEN 'HBKID'.
        <L_FIELDCAT>-COL_POS    = 400   ##NUMBER_OK.
        <L_FIELDCAT>-F4AVAILABL = GC_TRUE.
      WHEN 'HKTID'.
        <L_FIELDCAT>-COL_POS    = 410   ##NUMBER_OK.
        <L_FIELDCAT>-F4AVAILABL = GC_TRUE.
      WHEN 'ZBANK_ITEM'.
        <L_FIELDCAT>-COL_POS    = 415   ##NUMBER_OK.
        <L_FIELDCAT>-F4AVAILABL = GC_TRUE.
      WHEN 'PYMT_METHOD'.
        <L_FIELDCAT>-COL_POS    = 420   ##NUMBER_OK.
      WHEN 'ACTION_TYPE'.
        <L_FIELDCAT>-COL_POS    = 430   ##NUMBER_OK.
*        <l_fieldcat>-drdn_hndl  = 1.
*        <l_fieldcat>-drdn_alias = 'X'.
      WHEN 'STATUS'.
        <L_FIELDCAT>-COL_POS    = 440   ##NUMBER_OK.
*        <l_fieldcat>-drdn_hndl  = 2.
*        <l_fieldcat>-drdn_alias = 'X'.
      WHEN 'REMARK'.
        <L_FIELDCAT>-COL_POS    = 450   ##NUMBER_OK.
      WHEN 'PAYMENT_DATE'.
        <L_FIELDCAT>-COL_POS    = 460   ##NUMBER_OK.
      WHEN 'RECEIVED_DATE'.
        <L_FIELDCAT>-COL_POS    = 470   ##NUMBER_OK.
      WHEN 'RECEIPT_NO'.
        <L_FIELDCAT>-COL_POS    = 480   ##NUMBER_OK.
      WHEN 'BANK_DATE'.
        <L_FIELDCAT>-COL_POS    = 490   ##NUMBER_OK.
      WHEN 'BANKK'.
        <L_FIELDCAT>-REPTEXT    = TEXT-F06.
        <L_FIELDCAT>-SELTEXT    = TEXT-F06.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-F05.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-F06.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-F06.
        <L_FIELDCAT>-COL_POS    = 495   ##NUMBER_OK.
        <L_FIELDCAT>-F4AVAILABL = GC_TRUE.
*        <l_fieldcat>-no_out     = gc_true.
      WHEN 'BANKN'.
        <L_FIELDCAT>-COL_POS    = 496   ##NUMBER_OK.
*        <l_fieldcat>-no_out     = gc_true.
      WHEN 'CHEQUE_NO'.
        <L_FIELDCAT>-SELTEXT    = TEXT-F02.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-F02.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-F02.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-F02.
        <L_FIELDCAT>-REPTEXT    = TEXT-F02.

        <L_FIELDCAT>-COL_POS    = 500   ##NUMBER_OK.
      WHEN 'BANKL'.
        <L_FIELDCAT>-COL_POS    = 510   ##NUMBER_OK.

      WHEN 'VBELN'.
        <L_FIELDCAT>-COL_POS    = 540   ##NUMBER_OK.
      WHEN 'DOC_SIGN_DATE'.
        <L_FIELDCAT>-COL_POS    = 550   ##NUMBER_OK.
      WHEN 'FKDAT'.
        <L_FIELDCAT>-COL_POS    = 560   ##NUMBER_OK.
      WHEN 'CHEQUE_DATE'.
        <L_FIELDCAT>-COL_POS    = 570   ##NUMBER_OK.
      WHEN 'FOLLOW_DATE'.
        <L_FIELDCAT>-COL_POS    = 580   ##NUMBER_OK.
      WHEN 'DOCUMENT_STATUS'.
        IF GV_DATA_TYPE = GC_TAB_SEL-UPDATE_BANK.
          <L_FIELDCAT>-COL_POS  = 600   ##NUMBER_OK.
        ELSE.
          <L_FIELDCAT>-NO_OUT   = GC_TRUE.
        ENDIF.
      WHEN 'INV_STATUS'.
        IF GV_DATA_TYPE = GC_TAB_SEL-UPDATE_BANK.
          <L_FIELDCAT>-COL_POS  = 610   ##NUMBER_OK.
        ELSE.
          <L_FIELDCAT>-NO_OUT   = GC_TRUE.
        ENDIF.
      WHEN 'REMARK_VENDOR'.
        <L_FIELDCAT>-COL_POS    = 620   ##NUMBER_OK.
      WHEN 'REASON_VENDOR'.
        IF GV_DATA_TYPE = GC_TAB_SEL-UPDATE_BANK.
          <L_FIELDCAT>-COL_POS  = 630   ##NUMBER_OK.
        ELSE.
          <L_FIELDCAT>-NO_OUT   = GC_TRUE.
        ENDIF.
      WHEN 'BILLING_CYCLE'.
        IF GV_DATA_TYPE = GC_TAB_SEL-UPDATE_BANK.
          <L_FIELDCAT>-COL_POS  = 640   ##NUMBER_OK.
        ELSE.
          <L_FIELDCAT>-NO_OUT   = GC_TRUE.
        ENDIF.
      WHEN 'COLLECTION_CYCLE'.
        IF GV_DATA_TYPE = GC_TAB_SEL-UPDATE_BANK.
          <L_FIELDCAT>-COL_POS  = 650   ##NUMBER_OK.
        ELSE.
          <L_FIELDCAT>-NO_OUT   = GC_TRUE.
        ENDIF.
      WHEN 'AWB_NO'.
        IF GV_DATA_TYPE = GC_TAB_SEL-UPDATE_BANK.
          <L_FIELDCAT>-COL_POS  = 660   ##NUMBER_OK.
        ELSE.
          <L_FIELDCAT>-NO_OUT   = GC_TRUE.
        ENDIF.
      WHEN OTHERS.
        <L_FIELDCAT>-NO_OUT     = GC_TRUE.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT  CHANGING CT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'SEL',
    LC_SORT2 TYPE  LVC_S_SORT-FIELDNAME VALUE 'BILLPL_NO',
    LC_SORT3 TYPE  LVC_S_SORT-FIELDNAME VALUE 'KUNNR',
    LC_SORT4 TYPE  LVC_S_SORT-FIELDNAME VALUE 'WAERS',
    LC_SORT5 TYPE  LVC_S_SORT-FIELDNAME VALUE 'GJAHR',
    LC_SORT6 TYPE  LVC_S_SORT-FIELDNAME VALUE 'BELNR',
    LC_SORT7 TYPE  LVC_S_SORT-FIELDNAME VALUE 'SEQ'.
  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: CT_SORT.

  "Sort by check box
  IF RB_DIS <> GC_TRUE.
    CLEAR LS_SORT.
    LS_SORT-SPOS      = 1.
    LS_SORT-FIELDNAME = LC_SORT1.
    LS_SORT-UP        = SPACE.
    LS_SORT-DOWN      = GC_TRUE.
    LS_SORT-SUBTOT    = GC_TRUE.
    APPEND LS_SORT TO CT_SORT.
  ENDIF.

  "Sort by bill placement no
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 2.
  LS_SORT-FIELDNAME = LC_SORT2.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-DOWN      = SPACE.
  LS_SORT-SUBTOT    = GC_TRUE.
  APPEND LS_SORT TO CT_SORT.

  "Sort by customer code
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 3.
  LS_SORT-FIELDNAME = LC_SORT3.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-DOWN      = SPACE.
  LS_SORT-SUBTOT    = GC_TRUE.
  APPEND LS_SORT TO CT_SORT.

  "Sort by currency
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 4.
  LS_SORT-FIELDNAME = LC_SORT4.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-DOWN      = SPACE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

  "Sort by document year
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 5.
  LS_SORT-FIELDNAME = LC_SORT5.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-DOWN      = SPACE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

  "Sort by document no
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 6.
  LS_SORT-FIELDNAME = LC_SORT6.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-DOWN      = SPACE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

  "Sort by document seq
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 7.
  LS_SORT-FIELDNAME = LC_SORT7.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-DOWN      = SPACE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_top_of_page
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE USING UREF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '30%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '70%'.

  DATA:
    LV_TOTAL     TYPE  ZSDSFIT029-PAYIN_AMT,
    LV_REMAIN    TYPE  ZSDSFIT029-PAYIN_AMT,
    LF_TEXT      TYPE  SDYDO_TEXT_ELEMENT,
    LREF_TABLE   TYPE  REF TO CL_DD_TABLE_ELEMENT,
    LREF_COL_KEY TYPE  REF TO CL_DD_AREA,
    LREF_COL_VAL TYPE  REF TO CL_DD_AREA.

* Create table
  CALL METHOD UREF_DYNDOC_ID->ADD_TABLE
    EXPORTING
      NO_OF_COLUMNS = 2
      BORDER        = '0'
      WIDTH         = '50%'
    IMPORTING
      TABLE         = LREF_TABLE.
* Get Column Element
  CALL METHOD LREF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_KEY
    IMPORTING
      COLUMN = LREF_COL_KEY.
* Get Column Element
  CALL METHOD LREF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_VAL
    IMPORTING
      COLUMN = LREF_COL_VAL.
* Set Key column style
  CALL METHOD LREF_TABLE->SET_COLUMN_STYLE
    EXPORTING
      COL_NO       = 1
      SAP_EMPHASIS = CL_DD_AREA=>STRONG.

  CASE GV_DATA_TYPE.
*--------------------------------------------------------------------*
* Collector log
*--------------------------------------------------------------------*
    WHEN GC_TAB_SEL-COLLECTION_LOG.

*-----------------------
* Add value in Line1
*-----------------------
      CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h01 : Bill Collector Input Data
      LF_TEXT = TEXT-H01.
      CALL METHOD LREF_COL_KEY->ADD_TEXT
        EXPORTING
          TEXT = LF_TEXT.


*-----------------------
* Add value in Line2
*-----------------------
      CALL METHOD LREF_TABLE->NEW_ROW.

* Text-h01 : Collector Name
      LF_TEXT = TEXT-H02.
      CALL METHOD LREF_COL_KEY->ADD_TEXT
        EXPORTING
          TEXT = LF_TEXT.

      LF_TEXT = |{ P_PERNR ALPHA = OUT } { GV_FULLNAME }|.
      CALL METHOD LREF_COL_VAL->ADD_TEXT
        EXPORTING
          TEXT = LF_TEXT.

      LOOP AT GT_OUTPUT INTO DATA(LS_OUTPUT) WHERE SEL = GC_TRUE  ##INTO_OK.
*        lv_total += ls_output-payin_amt.
        LV_TOTAL += LS_OUTPUT-RECEIVED_AMT + LS_OUTPUT-INCOME_AMT + LS_OUTPUT-CASH_CON.
      ENDLOOP.
      LV_REMAIN = P_RCVAMT - LV_TOTAL.

*-----------------------
* Add value in Line3
*-----------------------
      IF P_RCVAMT IS NOT INITIAL.
        CALL METHOD LREF_TABLE->NEW_ROW.

* Text-h04 : Received Amount
        LF_TEXT = |{ TEXT-H04 }|.
        CALL METHOD LREF_COL_KEY->ADD_TEXT
          EXPORTING
            TEXT = LF_TEXT.

*    LF_TEXT = |{ P_RCVAMT CURRENCY = GV_WAERS  } |.
        WRITE P_RCVAMT CURRENCY GV_WAERS TO LF_TEXT.
        CALL METHOD LREF_COL_VAL->ADD_TEXT
          EXPORTING
            TEXT = LF_TEXT.

      ENDIF.

*-----------------------
* Add value in Line4
*-----------------------
      CALL METHOD LREF_TABLE->NEW_ROW.

* Text-h05 : Invoice Amount
      LF_TEXT = |{ TEXT-H05 }|.
      CALL METHOD LREF_COL_KEY->ADD_TEXT
        EXPORTING
          TEXT = LF_TEXT.

*  LF_TEXT = |{ LV_TOTAL CURRENCY = GV_WAERS  } |.
      WRITE LV_TOTAL CURRENCY GV_WAERS TO LF_TEXT.
      CALL METHOD LREF_COL_VAL->ADD_TEXT
        EXPORTING
          TEXT = LF_TEXT.

*-----------------------
* Add value in Line5
*-----------------------
      IF P_RCVAMT IS NOT INITIAL.

        CALL METHOD LREF_TABLE->NEW_ROW.

* Text-h05 : Balance
        LF_TEXT = |{ TEXT-H06 }|.
        CALL METHOD LREF_COL_KEY->ADD_TEXT
          EXPORTING
            TEXT = LF_TEXT.

*    LF_TEXT = |{ LV_REMAIN CURRENCY = GV_WAERS } |.
        WRITE LV_REMAIN CURRENCY GV_WAERS TO LF_TEXT.
        CALL METHOD LREF_COL_VAL->ADD_TEXT
          EXPORTING
            TEXT = LF_TEXT.

      ENDIF.

*--------------------------------------------------------------------*
* New entry
*--------------------------------------------------------------------*
    WHEN GC_TAB_SEL-NEW_ENTRY.

*-----------------------
* Add value in Line1
*-----------------------
      CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h01 : Bill Collector Input Data
      LF_TEXT = TEXT-H01.
      CALL METHOD LREF_COL_KEY->ADD_TEXT
        EXPORTING
          TEXT = LF_TEXT.


*-----------------------
* Add value in Line2
*-----------------------
      CALL METHOD LREF_TABLE->NEW_ROW.

* Text-h01 : Collector Name
      LF_TEXT = TEXT-H02.
      CALL METHOD LREF_COL_KEY->ADD_TEXT
        EXPORTING
          TEXT = LF_TEXT.

      LF_TEXT = |{ P_PERNRN ALPHA = OUT } { GV_FULLNAME }|.
      CALL METHOD LREF_COL_VAL->ADD_TEXT
        EXPORTING
          TEXT = LF_TEXT.

      LOOP AT GT_OUTPUT_NEW INTO DATA(LS_OUTPUT_NEW) WHERE SEL = GC_TRUE  ##INTO_OK.
        LV_TOTAL += LS_OUTPUT_NEW-WRBTR.
      ENDLOOP.
      LV_REMAIN = P_RCVAMN - LV_TOTAL.

*-----------------------
* Add value in Line3
*-----------------------
      IF P_RCVAMN IS NOT INITIAL.
        CALL METHOD LREF_TABLE->NEW_ROW.

* Text-h04 : Received Amount
        LF_TEXT = |{ TEXT-H04 }|.
        CALL METHOD LREF_COL_KEY->ADD_TEXT
          EXPORTING
            TEXT = LF_TEXT.

        WRITE P_RCVAMN CURRENCY GV_WAERS TO LF_TEXT.
        CALL METHOD LREF_COL_VAL->ADD_TEXT
          EXPORTING
            TEXT = LF_TEXT.

      ENDIF.

*-----------------------
* Add value in Line4
*-----------------------
      CALL METHOD LREF_TABLE->NEW_ROW.

* Text-h05 : Assigned Amount
      LF_TEXT = |{ TEXT-H05 }|.
      CALL METHOD LREF_COL_KEY->ADD_TEXT
        EXPORTING
          TEXT = LF_TEXT.

*  LF_TEXT = |{ LV_TOTAL CURRENCY = GV_WAERS  } |.
      WRITE LV_TOTAL CURRENCY GV_WAERS TO LF_TEXT.
      CALL METHOD LREF_COL_VAL->ADD_TEXT
        EXPORTING
          TEXT = LF_TEXT.

*-----------------------
* Add value in Line5
*-----------------------
      IF P_RCVAMN IS NOT INITIAL.

        CALL METHOD LREF_TABLE->NEW_ROW.

* Text-h05 : Balance
        LF_TEXT = |{ TEXT-H06 }|.
        CALL METHOD LREF_COL_KEY->ADD_TEXT
          EXPORTING
            TEXT = LF_TEXT.

*    LF_TEXT = |{ LV_REMAIN CURRENCY = GV_WAERS } |.
        WRITE LV_REMAIN CURRENCY GV_WAERS TO LF_TEXT.
        CALL METHOD LREF_COL_VAL->ADD_TEXT
          EXPORTING
            TEXT = LF_TEXT.

      ENDIF.

*--------------------------------------------------------------------*
* Update Bank's vendor & Memo
*--------------------------------------------------------------------*
    WHEN GC_TAB_SEL-UPDATE_BANK OR
         GC_TAB_SEL-MEMO.

*-----------------------
* Add value in Line1
*-----------------------
      CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h01 : Bill Collector Input Data
      LF_TEXT = TEXT-H01.
      CALL METHOD LREF_COL_KEY->ADD_TEXT
        EXPORTING
          TEXT = LF_TEXT.


*-----------------------
* Add value in Line2
*-----------------------
      CALL METHOD LREF_TABLE->NEW_ROW.

* Text-h01 : Collector Name
      LF_TEXT = TEXT-H02.
      CALL METHOD LREF_COL_KEY->ADD_TEXT
        EXPORTING
          TEXT = LF_TEXT.

      LF_TEXT = |{ P_PERNRU ALPHA = OUT } { GV_FULLNAME }|.
      CALL METHOD LREF_COL_VAL->ADD_TEXT
        EXPORTING
          TEXT = LF_TEXT.


  ENDCASE.
ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_on_data_changed
*----------------------------------------------------------------------*
*       ALV ON_DATA_CHANGED Event
*----------------------------------------------------------------------*
FORM F_ON_DATA_CHANGED USING
                          UREF_DATA_CHANGED	TYPE REF TO	CL_ALV_CHANGED_DATA_PROTOCOL
                          UF_ONF4           TYPE  CHAR01    ##NEEDED
                          UF_ONF4_BEFORE    TYPE  CHAR01    ##NEEDED
                          UF_ONF4_AFTER     TYPE  CHAR01    ##NEEDED
                          UF_UCOMM          TYPE  SY-UCOMM  ##CALLED.


  CASE GV_DATA_TYPE.
    WHEN GC_TAB_SEL-COLLECTION_LOG.
      PERFORM F_VALIDATE_CHG_COLL_LOG USING UREF_DATA_CHANGED.

    WHEN GC_TAB_SEL-NEW_ENTRY.
      PERFORM F_DEFAULT_NEW_ENTRY CHANGING UREF_DATA_CHANGED.

    WHEN GC_TAB_SEL-MEMO.
      PERFORM F_DEFAULT_NEW_MEMO CHANGING UREF_DATA_CHANGED.

  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_on_data_changed_finished
*----------------------------------------------------------------------*
*       ALV ON_DATA_CHANGED_FINISHED Event
*----------------------------------------------------------------------*
FORM F_ON_DATA_CHANGED_FINISHED USING
                                  UF_MODIFIED   TYPE  CHAR01      ##NEEDED
                                  UT_GOOD_CELLS TYPE  LVC_T_MODI  ##CALLED.

  DATA: "ls_output  TYPE zsdsfis085,
    LS_STABLE    TYPE LVC_S_STBL,
    LV_REFRESH   TYPE ABAP_BOOL,
    LV_CAL_WHTAX TYPE CHAR01.

  LOOP AT UT_GOOD_CELLS INTO DATA(LS_GOOD)    ##INTO_OK.

    CASE GV_DATA_TYPE.
*--------------------------------------------------------------------*
* Collector log
*--------------------------------------------------------------------*
      WHEN GC_TAB_SEL-COLLECTION_LOG OR
           GC_TAB_SEL-UPDATE_BANK.

        READ TABLE GT_OUTPUT ASSIGNING FIELD-SYMBOL(<LS_OUTPUT>)
          INDEX LS_GOOD-ROW_ID.

        CHECK SY-SUBRC EQ 0.

        CHECK <LS_OUTPUT> IS ASSIGNED.

        CASE LS_GOOD-FIELDNAME.
          WHEN 'SEL'.
*            <LS_OUTPUT>-ZBANK_ITEM = P_ZBANK.

            LV_REFRESH = ABAP_TRUE.

          WHEN 'PERNR'.
            LV_REFRESH = ABAP_TRUE.

            PERFORM F_GET_PERNR_NAME USING <LS_OUTPUT>-PERNR
                                     CHANGING <LS_OUTPUT>-FULL_NAME.

          WHEN 'DEDUCT_AMT'.

            <LS_OUTPUT>-BAL_AMT = <LS_OUTPUT>-DEDUCT_AMT.

            "Witholding tax amount
            CLEAR LV_CAL_WHTAX.
            PERFORM IS_CAL_WHTAX USING <LS_OUTPUT>-KUNNR
                                 CHANGING LV_CAL_WHTAX.

            IF LV_CAL_WHTAX = GC_TRUE.
              IF <LS_OUTPUT>-BLART IN GR_SRV_BLART.
                IF GV_SRV_WHTAX_RATE <> 0.
                  READ TABLE GT_TAX_BASE
                    INTO DATA(LS_TAX_BASE)
                    WITH KEY BUKRS = <LS_OUTPUT>-BUKRS
                             BELNR = <LS_OUTPUT>-BELNR
                             GJAHR = <LS_OUTPUT>-GJAHR.
                  IF SY-SUBRC = 0.
*<<F36K911258 start of del
*                    <ls_output>-wht_amt = ( ( <ls_output>-bal_amt - ( <ls_output>-bal_amt * ( ls_tax_base-wrbtr / ls_tax_base-fwbas ) ) )
*                                            * ( gv_srv_whtax_rate / 100 ) ) * -1.
*<<F36K911258 end of del
*<<F36K911258 start of ins
                    "whtax = ( ( Bal amt * 100 / 107 ) * 3% ) * -1
                    <LS_OUTPUT>-WHT_AMT = ( ( <LS_OUTPUT>-BAL_AMT * 100 / ( 100 + ( LS_TAX_BASE-WRBTR / LS_TAX_BASE-FWBAS * 100 ) ) )
                                            * ( GV_SRV_WHTAX_RATE / 100 ) ) * -1.
*<<F36K911258 end of ins
                  ELSE.
                    <LS_OUTPUT>-WHT_AMT = ( <LS_OUTPUT>-BAL_AMT * ( GV_SRV_WHTAX_RATE / 100 ) ) * -1.
                  ENDIF.
                ENDIF.
              ELSE.
                READ TABLE GT_WHTAX
                  INTO DATA(LS_WHTAX)
                  WITH KEY BUKRS = <LS_OUTPUT>-BUKRS
                           BELNR = <LS_OUTPUT>-BELNR
                           GJAHR = <LS_OUTPUT>-GJAHR
                           BUZEI = <LS_OUTPUT>-BUZEI.
                IF SY-SUBRC EQ 0.
                  <LS_OUTPUT>-WHT_AMT = ( <LS_OUTPUT>-BAL_AMT * ( LS_WHTAX-QSATZ / LS_WHTAX-QPROZ ) ) * -1.
                ENDIF.
              ENDIF.
            ENDIF.

            <LS_OUTPUT>-RECEIVED_AMT = <LS_OUTPUT>-BAL_AMT +
                                       <LS_OUTPUT>-EXPS_AMT +
                                       <LS_OUTPUT>-WHT_AMT +
                                       <LS_OUTPUT>-FEE.

            <LS_OUTPUT>-PAYIN_AMT    = <LS_OUTPUT>-RECEIVED_AMT +
                                       <LS_OUTPUT>-RETENTION +
*                                   <ls_output>-refund_amt +
                                       <LS_OUTPUT>-INCOME_AMT +
                                       <LS_OUTPUT>-CASH_CON.

            <LS_OUTPUT>-REMAIN_C_AMT = <LS_OUTPUT>-REMAIN_AMT - <LS_OUTPUT>-BAL_AMT.

            LV_REFRESH = ABAP_TRUE.

          WHEN 'EXPS_AMT' OR
               'WHT_AMT'  OR
               'FEE'.

            CASE LS_GOOD-FIELDNAME.
              WHEN 'EXPS_AMT'.
                <LS_OUTPUT>-EXPS_AMT = ABS( <LS_OUTPUT>-EXPS_AMT ) * -1.

                PERFORM F_SET_DEFAULT_ORDER_EXP USING <LS_OUTPUT>-EXPS_AMT
                                                CHANGING <LS_OUTPUT>-AUFNR_EXPS.

              WHEN 'WHT_AMT'.
                <LS_OUTPUT>-WHT_AMT  = ABS( <LS_OUTPUT>-WHT_AMT ) * -1.
              WHEN 'FEE'.


                <LS_OUTPUT>-FEE      = ABS( <LS_OUTPUT>-FEE ) * -1.

                PERFORM F_SET_DEFAULT_ORDER_FEE USING <LS_OUTPUT>-FEE
                                                CHANGING <LS_OUTPUT>-AUFNR_FEE.

            ENDCASE.

            <LS_OUTPUT>-RECEIVED_AMT = <LS_OUTPUT>-BAL_AMT +
                                       <LS_OUTPUT>-EXPS_AMT +
                                       <LS_OUTPUT>-WHT_AMT +
                                       <LS_OUTPUT>-FEE.

            <LS_OUTPUT>-PAYIN_AMT    = <LS_OUTPUT>-RECEIVED_AMT +
                                       <LS_OUTPUT>-RETENTION +
*                                   <ls_output>-refund_amt +
                                       <LS_OUTPUT>-INCOME_AMT +
                                       <LS_OUTPUT>-CASH_CON.

            <LS_OUTPUT>-REMAIN_C_AMT = <LS_OUTPUT>-REMAIN_AMT - <LS_OUTPUT>-BAL_AMT.

            LV_REFRESH = ABAP_TRUE.
          WHEN 'RETENTION'  OR
*           'REFUND_AMT' OR
               'INCOME_AMT' OR
               'CASH_CON'.

            <LS_OUTPUT>-PAYIN_AMT    = <LS_OUTPUT>-RECEIVED_AMT +
                                       <LS_OUTPUT>-RETENTION +
*                                   <ls_output>-refund_amt +
                                       <LS_OUTPUT>-INCOME_AMT +
                                       <LS_OUTPUT>-CASH_CON.

            <LS_OUTPUT>-REMAIN_C_AMT = <LS_OUTPUT>-REMAIN_AMT - <LS_OUTPUT>-BAL_AMT.

            LV_REFRESH = ABAP_TRUE.

          WHEN 'BANK_DATE'.

*            <ls_output>-due_on = <ls_output>-bank_date.
            <LS_OUTPUT>-FAEDT = <LS_OUTPUT>-BANK_DATE.

        ENDCASE.

        "Mark sel = 'X' for change mode as change ind
        IF RB_CHG = GC_TRUE AND LS_GOOD-FIELDNAME <> 'SEL'.
          <LS_OUTPUT>-SEL = GC_TRUE.
        ENDIF.

        IF <LS_OUTPUT>-PYMT_METHOD IN GR_PDC AND <LS_OUTPUT>-CHEQUE_NO IS NOT INITIAL.
*          IF <ls_output>-bankk IS INITIAL.
*            <ls_output>-bankk = gv_df_pdc_bank_key.
*            lv_refresh = abap_true.
*          ENDIF.
*
          IF <LS_OUTPUT>-BANKN IS INITIAL.
            <LS_OUTPUT>-BANKN = GV_DF_PDC_BANK_ACCT.
            LV_REFRESH = ABAP_TRUE.
          ENDIF.
        ENDIF.

*--------------------------------------------------------------------*
* New entry
*--------------------------------------------------------------------*
      WHEN GC_TAB_SEL-NEW_ENTRY.
        READ TABLE GT_OUTPUT_NEW ASSIGNING FIELD-SYMBOL(<LS_OUTPUT_NEW>)
          INDEX LS_GOOD-ROW_ID.

        CHECK SY-SUBRC EQ 0.

        CHECK <LS_OUTPUT_NEW> IS ASSIGNED.

        CASE LS_GOOD-FIELDNAME.
          WHEN 'SEL'.
            LV_REFRESH = ABAP_TRUE.

          WHEN 'WRBTR'.
            LV_REFRESH = ABAP_TRUE.

          WHEN 'BLDAT'.
            IF <LS_OUTPUT_NEW>-BUDAT IS INITIAL.
              <LS_OUTPUT_NEW>-BUDAT = <LS_OUTPUT_NEW>-BLDAT.
            ENDIF.

            IF <LS_OUTPUT_NEW>-ZFBDT IS INITIAL.
              <LS_OUTPUT_NEW>-ZFBDT = <LS_OUTPUT_NEW>-BLDAT.
            ENDIF.

            LV_REFRESH = ABAP_TRUE.
          WHEN 'KUNNR'.
            SELECT SINGLE
               CASE
                 WHEN CUST~KTOKD = 'Z010' THEN
                   CONCAT_WITH_SPACE( CONCAT_WITH_SPACE( PARTNER~NAME_FIRST, PARTNER~NAME_LAST, 1 ), CONCAT_WITH_SPACE( PARTNER~NAME_ORG1, PARTNER~NAME_ORG2, 1 ), 1 )
                 WHEN CUST~KTOKD = 'Z050' THEN
                   CONCAT_WITH_SPACE( PARTNER~NAME_FIRST, PARTNER~NAME_LAST, 1 )
                 ELSE
                   CONCAT_WITH_SPACE( PARTNER~NAME_ORG1, PARTNER~NAME_ORG2, 1 )
               END AS CUST_NAME
               FROM KNA1 AS CUST
               INNER JOIN CVI_CUST_LINK AS LINK
               ON CUST~KUNNR = LINK~CUSTOMER
               INNER JOIN BUT000 AS PARTNER
               ON PARTNER~PARTNER_GUID = LINK~PARTNER_GUID
               WHERE CUST~KUNNR = @<LS_OUTPUT_NEW>-KUNNR
               INTO @<LS_OUTPUT_NEW>-CUST_NAME  ##WARN_OK.

            LV_REFRESH = ABAP_TRUE.
        ENDCASE.

*--------------------------------------------------------------------*
* Memo
*--------------------------------------------------------------------*
      WHEN GC_TAB_SEL-MEMO.
        READ TABLE GT_OUTPUT ASSIGNING FIELD-SYMBOL(<LS_OUTPUT_MEMO>)
          INDEX LS_GOOD-ROW_ID.

        CHECK SY-SUBRC EQ 0.

        CHECK <LS_OUTPUT_MEMO> IS ASSIGNED.

        CASE LS_GOOD-FIELDNAME.
          WHEN 'SEL'.
            LV_REFRESH = ABAP_TRUE.

          WHEN 'WRBTR'.
            <LS_OUTPUT_MEMO>-DEDUCT_AMT = <LS_OUTPUT_MEMO>-WRBTR.

            <LS_OUTPUT_MEMO>-BAL_AMT = <LS_OUTPUT_MEMO>-DEDUCT_AMT.

            <LS_OUTPUT_MEMO>-RECEIVED_AMT = <LS_OUTPUT_MEMO>-BAL_AMT +
                                            <LS_OUTPUT_MEMO>-EXPS_AMT +
                                            <LS_OUTPUT_MEMO>-FEE.

            <LS_OUTPUT_MEMO>-PAYIN_AMT    = <LS_OUTPUT_MEMO>-RECEIVED_AMT +
                                            <LS_OUTPUT_MEMO>-RETENTION +
                                            <LS_OUTPUT_MEMO>-INCOME_AMT +
                                            <LS_OUTPUT_MEMO>-CASH_CON.


            LV_REFRESH = ABAP_TRUE.

          WHEN 'BLDAT'.
            IF <LS_OUTPUT_MEMO>-BUDAT IS INITIAL.
              <LS_OUTPUT_MEMO>-BUDAT = <LS_OUTPUT_MEMO>-BLDAT.
            ENDIF.

            IF <LS_OUTPUT_MEMO>-ZFBDT IS INITIAL.
              <LS_OUTPUT_MEMO>-ZFBDT = <LS_OUTPUT_MEMO>-BLDAT.
            ENDIF.

            LV_REFRESH = ABAP_TRUE.
          WHEN 'KUNNR'.
            SELECT SINGLE
               CASE
                 WHEN CUST~KTOKD = 'Z010' THEN
                   CONCAT_WITH_SPACE( CONCAT_WITH_SPACE( PARTNER~NAME_FIRST, PARTNER~NAME_LAST, 1 ), CONCAT_WITH_SPACE( PARTNER~NAME_ORG1, PARTNER~NAME_ORG2, 1 ), 1 )
                 WHEN CUST~KTOKD = 'Z050' THEN
                   CONCAT_WITH_SPACE( PARTNER~NAME_FIRST, PARTNER~NAME_LAST, 1 )
                 ELSE
                   CONCAT_WITH_SPACE( PARTNER~NAME_ORG1, PARTNER~NAME_ORG2, 1 )
               END AS CUST_NAME
               FROM KNA1 AS CUST
               INNER JOIN CVI_CUST_LINK AS LINK
               ON CUST~KUNNR = LINK~CUSTOMER
               INNER JOIN BUT000 AS PARTNER
               ON PARTNER~PARTNER_GUID = LINK~PARTNER_GUID
               WHERE CUST~KUNNR = @<LS_OUTPUT_MEMO>-KUNNR
               INTO @<LS_OUTPUT_MEMO>-CUST_NAME  ##WARN_OK.

            LV_REFRESH = ABAP_TRUE.

          WHEN 'DEDUCT_AMT'.

            <LS_OUTPUT_MEMO>-BAL_AMT = <LS_OUTPUT_MEMO>-DEDUCT_AMT.

            <LS_OUTPUT_MEMO>-RECEIVED_AMT = <LS_OUTPUT_MEMO>-BAL_AMT +
                                            <LS_OUTPUT_MEMO>-EXPS_AMT +
                                            <LS_OUTPUT_MEMO>-FEE.

            <LS_OUTPUT_MEMO>-PAYIN_AMT    = <LS_OUTPUT_MEMO>-RECEIVED_AMT +
                                            <LS_OUTPUT_MEMO>-RETENTION +
                                            <LS_OUTPUT_MEMO>-INCOME_AMT +
                                            <LS_OUTPUT_MEMO>-CASH_CON.

            LV_REFRESH = ABAP_TRUE.

          WHEN 'FEE'.

            CASE LS_GOOD-FIELDNAME.
              WHEN 'FEE'.

*                <ls_output_memo>-fee      = <ls_output_memo>-fee * -1.

                PERFORM F_SET_DEFAULT_ORDER_FEE USING <LS_OUTPUT_MEMO>-FEE
                                                CHANGING <LS_OUTPUT_MEMO>-AUFNR_FEE.

            ENDCASE.

            <LS_OUTPUT_MEMO>-RECEIVED_AMT = <LS_OUTPUT_MEMO>-BAL_AMT +
                                            <LS_OUTPUT_MEMO>-EXPS_AMT +
                                            <LS_OUTPUT_MEMO>-FEE.

            <LS_OUTPUT_MEMO>-PAYIN_AMT    = <LS_OUTPUT_MEMO>-RECEIVED_AMT +
                                            <LS_OUTPUT_MEMO>-RETENTION +
                                            <LS_OUTPUT_MEMO>-INCOME_AMT +
                                            <LS_OUTPUT_MEMO>-CASH_CON.

            LV_REFRESH = ABAP_TRUE.

          WHEN 'BANK_DATE'.

            <LS_OUTPUT_MEMO>-FAEDT = <LS_OUTPUT_MEMO>-BANK_DATE.

            LV_REFRESH = ABAP_TRUE.
        ENDCASE.
    ENDCASE.
  ENDLOOP.


  IF LV_REFRESH = ABAP_TRUE.
*    ls_stable-row = gc_true.
    LS_STABLE-COL = GC_TRUE.

    PERFORM F_INIT_DOC_ALV_HEADER.

    GREF_GRID->REFRESH_TABLE_DISPLAY(
      IS_STABLE = LS_STABLE
    ).

  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_HANDLE_TOOLBAR
*----------------------------------------------------------------------*
*  Event ALV Toolbar
*----------------------------------------------------------------------*
FORM F_HANDLE_TOOLBAR USING UREF_OBJECT TYPE REF TO	CL_ALV_EVENT_TOOLBAR_SET ##CALLED
                              UF_INTERACTIVE TYPE	CHAR01 ##NEEDED.

  DATA: LS_TOOLBAR TYPE STB_BUTTON.

* Handle Toolbar as needed
  IF GV_EDIT EQ GC_TRUE.

    CASE GV_DATA_TYPE.
*----------------------------------------------------------------------*
* Collector log
*----------------------------------------------------------------------*
      WHEN GC_TAB_SEL-COLLECTION_LOG.

        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&CHECK'.
*    DELETE uref_object->mt_toolbar WHERE function EQ '&REFRESH'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP01'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&CUT'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&PASTE'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&UNDO'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP02'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&APPEND'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&INSERT_ROW'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&DELETE_ROW'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY_ROW'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP03'.

        "Create / Change
        IF RB_DIS IS INITIAL.
          CLEAR LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = '3'.   "separator
          APPEND LS_TOOLBAR TO UREF_OBJECT->MT_TOOLBAR.

          "Select All
          CLEAR LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = '0'.   "normal Button
          LS_TOOLBAR-FUNCTION = GC_FUNC-SEL_A.   "fcode
          LS_TOOLBAR-ICON = '@4B@'.
*    ls_toolbar-text = TEXT-b03.
          LS_TOOLBAR-QUICKINFO = 'Select All'(b03)  ##SHARE_OK.
          APPEND LS_TOOLBAR TO UREF_OBJECT->MT_TOOLBAR.

          "Select None
          CLEAR LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = '0'.   "normal Button
          LS_TOOLBAR-FUNCTION = GC_FUNC-SEL_N.   "fcode
          LS_TOOLBAR-ICON = '@4D@'.
*    ls_toolbar-text = TEXT-b04.
          LS_TOOLBAR-QUICKINFO = 'Select None'(b04)  ##SHARE_OK.
          APPEND LS_TOOLBAR TO UREF_OBJECT->MT_TOOLBAR.
        ENDIF.

        IF RB_CRE = GC_TRUE.
*          CLEAR ls_toolbar.
*          ls_toolbar-butn_type = '3'.   "separator
*          APPEND ls_toolbar TO uref_object->mt_toolbar.
*          CLEAR ls_toolbar.
*          ls_toolbar-butn_type = '0'.   "normal Button
*          ls_toolbar-function = gc_func-duplicate.   "duplicate
*          ls_toolbar-icon = '@14@'.
*          ls_toolbar-quickinfo = 'Duplicate selected item'(b02)   ##SHARE_OK.
*          APPEND ls_toolbar TO uref_object->mt_toolbar.
        ELSEIF RB_CHG = GC_TRUE.
*<-- Start of Deletion 02.12.2024 (Remove as functional requested)
*          CLEAR ls_toolbar.
*          ls_toolbar-butn_type = '3'.   "separator
*          APPEND ls_toolbar TO uref_object->mt_toolbar.
*          CLEAR ls_toolbar.
*          ls_toolbar-butn_type = '0'.   "normal Button
*          ls_toolbar-function = gc_func-del.   "mark flag delete
*          ls_toolbar-icon = '@11@'.
*          ls_toolbar-quickinfo = 'Delete selected item'(b01)   ##SHARE_OK.
*          APPEND ls_toolbar TO uref_object->mt_toolbar.
*--> End of Deletion 02.12.2024
        ENDIF.

*----------------------------------------------------------------------*
* New entry
*----------------------------------------------------------------------*
      WHEN GC_TAB_SEL-NEW_ENTRY.

        IF RB_DISN IS INITIAL.
          CLEAR LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = '3'.   "separator
          APPEND LS_TOOLBAR TO UREF_OBJECT->MT_TOOLBAR.

          "Select All
          CLEAR LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = '0'.   "normal Button
          LS_TOOLBAR-FUNCTION = GC_FUNC-SEL_A_NEW.   "fcode
          LS_TOOLBAR-ICON = '@4B@'.
*    ls_toolbar-text = TEXT-b03.
          LS_TOOLBAR-QUICKINFO = 'Select All'(b03)  ##SHARE_OK.
          APPEND LS_TOOLBAR TO UREF_OBJECT->MT_TOOLBAR.

          "Select None
          CLEAR LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = '0'.   "normal Button
          LS_TOOLBAR-FUNCTION = GC_FUNC-SEL_N_NEW.   "fcode
          LS_TOOLBAR-ICON = '@4D@'.
*    ls_toolbar-text = TEXT-b04.
          LS_TOOLBAR-QUICKINFO = 'Select None'(b04)  ##SHARE_OK.
          APPEND LS_TOOLBAR TO UREF_OBJECT->MT_TOOLBAR.
        ENDIF.

        IF RB_CHGN = GC_TRUE.

          DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&PASTE'.
          DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&APPEND'.
          DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&INSERT_ROW'.
          DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&DELETE_ROW'.
          DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY_ROW'.

*<<F36K914812 - 01 Begin of del
*          CLEAR ls_toolbar.
*          ls_toolbar-butn_type = '3'.   "separator
*          APPEND ls_toolbar TO uref_object->mt_toolbar.
*          CLEAR ls_toolbar.
*          ls_toolbar-butn_type = '0'.   "normal Button
*          ls_toolbar-function = gc_func-del_new.   "fcode
*          ls_toolbar-icon = '@11@'.
*          ls_toolbar-quickinfo = 'Delete selected item'(b01)   ##SHARE_OK.
*          APPEND ls_toolbar TO uref_object->mt_toolbar.
*<<F36K914812 - 01 End of del

        ENDIF.
*----------------------------------------------------------------------*
* Update for bank's vendor
*----------------------------------------------------------------------*
      WHEN GC_TAB_SEL-UPDATE_BANK.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&CHECK'.
*    DELETE uref_object->mt_toolbar WHERE function EQ '&REFRESH'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP01'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&CUT'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&PASTE'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&UNDO'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP02'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&APPEND'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&INSERT_ROW'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&DELETE_ROW'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY_ROW'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP03'.

        "Create / Change
        IF RB_DISU IS INITIAL.
          CLEAR LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = '3'.   "separator
          APPEND LS_TOOLBAR TO UREF_OBJECT->MT_TOOLBAR.

          "Select All
          CLEAR LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = '0'.   "normal Button
          LS_TOOLBAR-FUNCTION = GC_FUNC-SEL_A.   "fcode
          LS_TOOLBAR-ICON = '@4B@'.
          LS_TOOLBAR-QUICKINFO = 'Select All'(b03)  ##SHARE_OK.
          APPEND LS_TOOLBAR TO UREF_OBJECT->MT_TOOLBAR.

          "Select None
          CLEAR LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = '0'.   "normal Button
          LS_TOOLBAR-FUNCTION = GC_FUNC-SEL_N.   "fcode
          LS_TOOLBAR-ICON = '@4D@'.
          LS_TOOLBAR-QUICKINFO = 'Select None'(b04)  ##SHARE_OK.
          APPEND LS_TOOLBAR TO UREF_OBJECT->MT_TOOLBAR.
        ENDIF.

*        IF rb_creu = gc_true. "<<F36K914812 - 01 del
*          CLEAR ls_toolbar.
*          ls_toolbar-butn_type = '3'.   "separator
*          APPEND ls_toolbar TO uref_object->mt_toolbar.
*          CLEAR ls_toolbar.
*          ls_toolbar-butn_type = '0'.   "normal Button
*          ls_toolbar-function = gc_func-duplicate.   "duplicate
*          ls_toolbar-icon = '@14@'.
*          ls_toolbar-quickinfo = 'Duplicate selected item'(b02)   ##SHARE_OK.
*          APPEND ls_toolbar TO uref_object->mt_toolbar.
*<<F36K914812 - 01 Begin of del
*        ELSEIF rb_chgu = gc_true.
*          CLEAR ls_toolbar.
*          ls_toolbar-butn_type = '3'.   "separator
*          APPEND ls_toolbar TO uref_object->mt_toolbar.
*          CLEAR ls_toolbar.
*          ls_toolbar-butn_type = '0'.   "normal Button
*          ls_toolbar-function = gc_func-del.   "mark flag delete
*          ls_toolbar-icon = '@11@'.
*          ls_toolbar-quickinfo = 'Delete selected item'(b01)   ##SHARE_OK.
*          APPEND ls_toolbar TO uref_object->mt_toolbar.
*        ENDIF.
*<<F36K914812 - 01 End of del
*----------------------------------------------------------------------*
* Update memo
*----------------------------------------------------------------------*
      WHEN GC_TAB_SEL-MEMO.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&UNDO'.

*<<F36K914812 - 01 Begin of ins
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&DELETE_ROW'.
*<<F36K914812 - 01 End of ins

        IF RB_CHGM = GC_TRUE.
          CLEAR LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = '3'.   "separator
          APPEND LS_TOOLBAR TO UREF_OBJECT->MT_TOOLBAR.
          CLEAR LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = '0'.   "normal Button
          LS_TOOLBAR-FUNCTION = GC_FUNC-DEL.   "mark flag delete
          LS_TOOLBAR-ICON = '@11@'.
          LS_TOOLBAR-QUICKINFO = 'Delete selected item'(b01)   ##SHARE_OK.
          APPEND LS_TOOLBAR TO UREF_OBJECT->MT_TOOLBAR.
        ENDIF.

*----------------------------------------------------------------------*
* History log
*----------------------------------------------------------------------*
      WHEN GC_TAB_SEL-HISTORY_LOG.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&CHECK'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP01'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&CUT'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&PASTE'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&UNDO'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP02'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&APPEND'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&INSERT_ROW'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&DELETE_ROW'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY_ROW'.
        DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP03'.

        IF RB_HCHG = GC_TRUE.
          "Select All
          CLEAR LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = '0'.   "normal Button
          LS_TOOLBAR-FUNCTION = GC_FUNC-HIS_SEL_A.   "fcode
          LS_TOOLBAR-ICON = '@4B@'.
          LS_TOOLBAR-QUICKINFO = 'Select All'(b03)  ##SHARE_OK.
          APPEND LS_TOOLBAR TO UREF_OBJECT->MT_TOOLBAR.

          "Select None
          CLEAR LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = '0'.   "normal Button
          LS_TOOLBAR-FUNCTION = GC_FUNC-HIS_SEL_N.   "fcode
          LS_TOOLBAR-ICON = '@4D@'.
          LS_TOOLBAR-QUICKINFO = 'Select None'(b04)  ##SHARE_OK.
          APPEND LS_TOOLBAR TO UREF_OBJECT->MT_TOOLBAR.

          CLEAR LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = '3'.   "separator
          APPEND LS_TOOLBAR TO UREF_OBJECT->MT_TOOLBAR.
          CLEAR LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = '0'.   "normal Button
          LS_TOOLBAR-FUNCTION = GC_FUNC-DEL.   "mark flag delete
          LS_TOOLBAR-ICON = '@11@'.
          LS_TOOLBAR-QUICKINFO = 'Delete selected item'(b01)   ##SHARE_OK.
          APPEND LS_TOOLBAR TO UREF_OBJECT->MT_TOOLBAR.
        ENDIF.
    ENDCASE.

  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_USER_COMMAND
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM F_USER_COMMAND USING UF_UCOMM  TYPE  SY-UCOMM ##CALLED.

  DATA:
    LV_VALID   TYPE CHAR01,
    LV_REFRESH TYPE CHAR01,
    LV_SEL     TYPE CHAR01.

  DATA(LREF_MSG) = NEW CL_ALV_CHANGED_DATA_PROTOCOL( I_CALLING_ALV = GREF_GRID ).

  GREF_GRID->GET_FRONTEND_FIELDCATALOG(
    IMPORTING
      ET_FIELDCATALOG = LREF_MSG->MT_FIELDCATALOG
  ).

  LREF_MSG->REFRESH_PROTOCOL( ).

  CASE GV_DATA_TYPE.
*--------------------------------------------------------------------*
* Collector log & Update for bank's vendor
*--------------------------------------------------------------------*
    WHEN GC_TAB_SEL-COLLECTION_LOG OR
         GC_TAB_SEL-UPDATE_BANK.

      CASE UF_UCOMM.
        WHEN GC_FUNC-SEL_A.
          LV_SEL = GC_TRUE.

          PERFORM F_MARK_SEL_ALL USING    LV_SEL
                                 CHANGING GT_OUTPUT.

          PERFORM F_INIT_DOC_ALV_HEADER.

          GREF_GRID->REFRESH_TABLE_DISPLAY( ).

        WHEN GC_FUNC-SEL_N.
          LV_SEL = ''.

          PERFORM F_MARK_SEL_ALL USING    LV_SEL
                                 CHANGING GT_OUTPUT.

          PERFORM F_INIT_DOC_ALV_HEADER.

          GREF_GRID->REFRESH_TABLE_DISPLAY( ).

*        WHEN gc_func-duplicate.
*
*          PERFORM f_duplicate_sel_item CHANGING gt_output.
*
*          PERFORM f_init_doc_alv_header.
*
*          gref_grid->refresh_table_display( ).


        WHEN GC_FUNC-SAVE.
          IF GREF_GRID->IS_READY_FOR_INPUT( ) = 0.
            RETURN.
          ENDIF.

*     Validate Data
          GREF_GRID->CHECK_CHANGED_DATA( IMPORTING E_VALID   = LV_VALID
                                         CHANGING  C_REFRESH = LV_REFRESH ).
*     Continue processing only when valid
          IF LV_VALID IS INITIAL.
            RETURN.
          ENDIF.

          PERFORM F_VALIDATE_MANDATORY CHANGING LV_VALID LREF_MSG.
          PERFORM F_VALIDATE_INPUT     CHANGING LV_VALID LREF_MSG.

          IF GV_DATA_TYPE = GC_TAB_SEL-UPDATE_BANK.
            PERFORM F_VALIDATE_INPUT_UPDATE_BANK CHANGING LV_VALID LREF_MSG.
          ENDIF.

          IF LV_VALID <> GC_TRUE.
            LREF_MSG->DISPLAY_PROTOCOL(  ).
            RETURN.
          ENDIF.
          CASE GV_DATA_TYPE.
            WHEN GC_TAB_SEL-COLLECTION_LOG.
              CASE GC_TRUE.
                WHEN RB_CRE.
                  PERFORM F_CREATE_COLL_LOG USING GT_OUTPUT GT_STATUS_HIST.
                WHEN RB_CHG.
                  PERFORM F_UPDATE_COLL_LOG USING GT_OUTPUT GT_STATUS_HIST.
              ENDCASE.
            WHEN GC_TAB_SEL-UPDATE_BANK.
              CASE GC_TRUE.
                WHEN RB_CREU.
                  PERFORM F_CREATE_COLL_LOG USING GT_OUTPUT GT_STATUS_HIST.
                WHEN RB_CHGU.
                  PERFORM F_UPDATE_COLL_LOG USING GT_OUTPUT GT_STATUS_HIST.
              ENDCASE.
          ENDCASE.

          PERFORM F_UNLOCK_TRAN USING GT_OUTPUT.

          GREF_GRID->REFRESH_TABLE_DISPLAY( ).

        WHEN GC_FUNC-DEL.
          PERFORM F_DELETE_COLL_LOG.

        WHEN GC_FUNC-POST_INC.
          TRY.
              CALL TRANSACTION 'ZSDSFI020' WITH AUTHORITY-CHECK.
            CATCH CX_SY_AUTHORIZATION_ERROR .
              "Error You are not authorized to use transaction &
              MESSAGE S172(00) WITH 'Post incoming payment'(001).
          ENDTRY.

      ENDCASE.

*--------------------------------------------------------------------*
* New entry
*--------------------------------------------------------------------*
    WHEN GC_TAB_SEL-NEW_ENTRY.

      CASE UF_UCOMM.
        WHEN GC_FUNC-SEL_A_NEW.
          LV_SEL = GC_TRUE.

          PERFORM F_MARK_SEL_ALL_NEW USING LV_SEL
                                 CHANGING GT_OUTPUT_NEW.

          GREF_GRID->REFRESH_TABLE_DISPLAY( ).

        WHEN GC_FUNC-SEL_N_NEW.
          LV_SEL = ''.

          PERFORM F_MARK_SEL_ALL_NEW USING LV_SEL
                                 CHANGING GT_OUTPUT_NEW.

          GREF_GRID->REFRESH_TABLE_DISPLAY( ).

        WHEN GC_FUNC-SAVE.
          IF GREF_GRID->IS_READY_FOR_INPUT( ) = 0.
            RETURN.
          ENDIF.

*     Validate Data
          GREF_GRID->CHECK_CHANGED_DATA( IMPORTING E_VALID   = LV_VALID
                                         CHANGING  C_REFRESH = LV_REFRESH ).
*     Continue processing only when valid
          IF LV_VALID IS INITIAL.
            RETURN.
          ENDIF.

          PERFORM F_VALIDATE_MANDATORY_NEW CHANGING LV_VALID LREF_MSG.
          PERFORM F_VALIDATE_INPUT_NEW     CHANGING LV_VALID LREF_MSG.

          IF LV_VALID <> GC_TRUE.
            LREF_MSG->DISPLAY_PROTOCOL(  ).
            RETURN.
          ENDIF.
          CASE GC_TRUE.
            WHEN RB_CREN.
              PERFORM F_CREATE_NEW_ENTRY.
            WHEN RB_CHGN.
              PERFORM F_UPDATE_NEW_ENTRY.
          ENDCASE.

          PERFORM F_UNLOCK_TRAN_NEW USING GT_OUTPUT_NEW.

          GREF_GRID->REFRESH_TABLE_DISPLAY( ).

        WHEN GC_FUNC-DEL_NEW.
          PERFORM F_DELETE_NEW_ENTRY.


        WHEN GC_FUNC-POST_INC.
          TRY.
              CALL TRANSACTION 'ZSDSFI020' WITH AUTHORITY-CHECK.
            CATCH CX_SY_AUTHORIZATION_ERROR .
              "Error You are not authorized to use transaction &
              MESSAGE S172(00) WITH 'Post incoming payment'(001).
          ENDTRY.

      ENDCASE.

*--------------------------------------------------------------------*
* Update for memo
*--------------------------------------------------------------------*
    WHEN GC_TAB_SEL-MEMO.
      CASE UF_UCOMM.
        WHEN GC_FUNC-SAVE.
          IF GREF_GRID->IS_READY_FOR_INPUT( ) = 0.
            RETURN.
          ENDIF.

*     Validate Data
          GREF_GRID->CHECK_CHANGED_DATA( IMPORTING E_VALID   = LV_VALID
                                         CHANGING  C_REFRESH = LV_REFRESH ).
*     Continue processing only when valid
          IF LV_VALID IS INITIAL.
            RETURN.
          ENDIF.

          PERFORM F_VALIDATE_MANDATORY_MEMO CHANGING LV_VALID LREF_MSG.

          IF LV_VALID <> GC_TRUE.
            LREF_MSG->DISPLAY_PROTOCOL(  ).
            RETURN.
          ENDIF.
          CASE GC_TRUE.
            WHEN RB_CREM.
              PERFORM F_GET_MEMO_HIST USING GT_OUTPUT
                                      CHANGING GT_STATUS_HIST.
              PERFORM F_CREATE_MEMO USING GT_OUTPUT GT_STATUS_HIST.
            WHEN RB_CHGM.
              PERFORM F_UPDATE_MEMO USING GT_OUTPUT GT_STATUS_HIST.
          ENDCASE.

          PERFORM F_UNLOCK_TRAN USING GT_OUTPUT.

          GREF_GRID->REFRESH_TABLE_DISPLAY( ).

        WHEN GC_FUNC-DEL.
          PERFORM F_DELETE_COLL_LOG.

          GREF_GRID->REFRESH_TABLE_DISPLAY( ).

        WHEN GC_FUNC-POST_INC.
          TRY.
              CALL TRANSACTION 'ZSDSFI020' WITH AUTHORITY-CHECK.
            CATCH CX_SY_AUTHORIZATION_ERROR .
              "Error You are not authorized to use transaction &
              MESSAGE S172(00) WITH 'Post incoming payment'(001).
          ENDTRY.

      ENDCASE.

*--------------------------------------------------------------------*
* Collector log history
*--------------------------------------------------------------------*
    WHEN GC_TAB_SEL-HISTORY_LOG.
      CASE UF_UCOMM.
        WHEN GC_FUNC-HIS_SEL_A.
          LV_SEL = GC_TRUE.

          PERFORM F_MARK_SEL_ALL_HIST USING    LV_SEL
                                 CHANGING GT_HISTORY_LOG.

          GREF_GRID->REFRESH_TABLE_DISPLAY( ).

        WHEN GC_FUNC-HIS_SEL_N.
          LV_SEL = ''.

          PERFORM F_MARK_SEL_ALL_HIST USING    LV_SEL
                                 CHANGING GT_HISTORY_LOG.

          GREF_GRID->REFRESH_TABLE_DISPLAY( ).

        WHEN GC_FUNC-DEL.
          PERFORM F_DEL_HISTORY_LOG CHANGING GT_HISTORY_LOG.

          GREF_GRID->REFRESH_TABLE_DISPLAY( ).

        WHEN GC_FUNC-SAVE.
          IF GREF_GRID->IS_READY_FOR_INPUT( ) = 0.
            RETURN.
          ENDIF.

          PERFORM F_ADJUST_HISTORY_LOG USING GT_HISTORY_LOG.

          PERFORM F_UNLOCK_TRAN_LOG USING GT_BPLOG.

          GREF_GRID->REFRESH_TABLE_DISPLAY( ).
      ENDCASE.

  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_create_coll_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_CREATE_COLL_LOG USING UT_OUTPUT TYPE TT_OUTPUT
                             UT_CURR_STATUS_HIST TYPE TT_STATUS_HIST.

  DATA:
    LT_GRP            TYPE TT_GRP_DOC,
    LT_BPLOG          TYPE STANDARD TABLE OF ZSDSFIT029,
    LT_STATUS_HIST    TYPE TT_STATUS_HIST,
*    lt_doc_update     TYPE tt_doc_update,
    LT_BANK_STATEMENT TYPE TT_BANK_STATEMENT.

  LT_GRP = VALUE #( FOR LS_OUTPUT IN UT_OUTPUT WHERE ( SEL = 'X'  ) ( CORRESPONDING #( LS_OUTPUT  ) )  ).

  IF LT_GRP IS INITIAL.
    MESSAGE S000(38) WITH TEXT-E10.
    RETURN.
  ENDIF.

  LOOP AT LT_GRP ASSIGNING FIELD-SYMBOL(<LS_GRP>).
    IF <LS_GRP>-ZBANK_ITEM IS NOT INITIAL.
      <LS_GRP>-TRN_TYPE = 'TRN'.
      <LS_GRP>-SORT_FIELD =  |{ <LS_GRP>-TRN_TYPE }{ <LS_GRP>-ZBANK_ITEM }|.
    ELSEIF <LS_GRP>-PYMT_METHOD IN GR_BANK.
      <LS_GRP>-TRN_TYPE = 'TRN'.
      <LS_GRP>-SORT_FIELD =  |{ <LS_GRP>-TRN_TYPE }{ <LS_GRP>-KUNNR }{ <LS_GRP>-BILLPL_NO }{ <LS_GRP>-BANKK }{ <LS_GRP>-CHEQUE_NO }|.
    ELSEIF <LS_GRP>-PYMT_METHOD IN GR_PDC.
      <LS_GRP>-TRN_TYPE = 'PDC'.
*      <LS_GRP>-SORT_FIELD =  |{ <LS_GRP>-TRN_TYPE }{ <LS_GRP>-KUNNR }{ <LS_GRP>-BILLPL_NO }{ <LS_GRP>-HBKID }{ <LS_GRP>-HKTID }|.
*      <ls_grp>-sort_field =  |{ <ls_grp>-trn_type }{ <ls_grp>-kunnr }{ <ls_grp>-hbkid }{ <ls_grp>-hktid }|.
      <LS_GRP>-SORT_FIELD =  |{ <LS_GRP>-TRN_TYPE }{ <LS_GRP>-CHEQUE_NO }|.
    ENDIF.

    PERFORM F_IS_POPULATE_TRANF_NO USING <LS_GRP>-TRANF_NO
                                         <LS_GRP>-ACTION_TYPE
                                         <LS_GRP>-STATUS
                                         GT_ACTION_STATUS
                                   CHANGING <LS_GRP>-TRANF_NO_FLAG.

  ENDLOOP.

  PERFORM F_GEN_TRANF_NO CHANGING LT_GRP LT_BANK_STATEMENT.

*  PERFORM f_gen_update_fi_doc USING lt_grp
*                              CHANGING lt_doc_update.

  LT_BPLOG = CORRESPONDING #( LT_GRP ).

  SELECT
    COL~BUKRS,
    COL~BELNR,
    COL~GJAHR,
    COL~BUZEI,
    COL~SEQ
    FROM ZSDSFIT029 AS COL
    FOR ALL ENTRIES IN @LT_BPLOG
    WHERE DATA_TYPE = @GC_DATA_TYPE-INVOICE
    AND   BUKRS = @LT_BPLOG-BUKRS
    AND   BELNR = @LT_BPLOG-BELNR
    AND   GJAHR = @LT_BPLOG-GJAHR
    INTO TABLE @DATA(LT_MAX_SEQ).             "#EC CI_FAE_LINES_ENSURED

  SORT LT_MAX_SEQ BY BUKRS BELNR GJAHR SEQ DESCENDING.
  DELETE ADJACENT DUPLICATES FROM LT_MAX_SEQ COMPARING BUKRS BELNR GJAHR.

  LOOP AT LT_BPLOG ASSIGNING FIELD-SYMBOL(<LS_ZSDSFIT029>).
    READ TABLE GT_OUTPUT ASSIGNING FIELD-SYMBOL(<LS_OUTPUT>)
      WITH KEY SEL = 'X'
               BUKRS = <LS_ZSDSFIT029>-BUKRS
               BELNR = <LS_ZSDSFIT029>-BELNR
               GJAHR = <LS_ZSDSFIT029>-GJAHR
               BUZEI = <LS_ZSDSFIT029>-BUZEI.
    IF SY-SUBRC EQ 0.
      <LS_OUTPUT>-TRANF_NO = <LS_ZSDSFIT029>-TRANF_NO.
    ENDIF.

    "Get max sequence
    READ TABLE LT_MAX_SEQ INTO DATA(LS_MAX_SEQ)
      WITH KEY BUKRS = <LS_ZSDSFIT029>-BUKRS
               BELNR = <LS_ZSDSFIT029>-BELNR
               GJAHR = <LS_ZSDSFIT029>-GJAHR
               BUZEI = <LS_ZSDSFIT029>-BUZEI
      BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      <LS_ZSDSFIT029>-SEQ = LS_MAX_SEQ-SEQ + 1.
    ELSE.
      <LS_ZSDSFIT029>-SEQ = 1.
    ENDIF.

    <LS_ZSDSFIT029>-ERNAM = SY-UNAME. "<<F36K914812 - 04 ins
    <LS_ZSDSFIT029>-ERDAT = SY-DATUM.
    <LS_ZSDSFIT029>-ERZMT = SY-UZEIT.

    PERFORM F_CREATE_STATUS_HIST USING <LS_ZSDSFIT029>
                                       UT_CURR_STATUS_HIST
                                 CHANGING LT_STATUS_HIST.

  ENDLOOP.

  IF LT_BPLOG IS NOT INITIAL.
    MODIFY ZSDSFIT029 FROM TABLE LT_BPLOG.
  ENDIF.

  IF LT_STATUS_HIST IS NOT INITIAL.
    MODIFY ZSDSFIT038 FROM TABLE LT_STATUS_HIST.
  ENDIF.

  IF LT_BANK_STATEMENT IS NOT INITIAL.
    SORT LT_BANK_STATEMENT BY HBKID	ZBANK_ITEM TRNFER_NUMBER DESCENDING.
    DELETE ADJACENT DUPLICATES FROM LT_BANK_STATEMENT COMPARING HBKID	ZBANK_ITEM.

    UPDATE ZSDSFIT042
      FROM TABLE @LT_BANK_STATEMENT INDICATORS SET STRUCTURE COL_IND.
  ENDIF.

*  IF lt_doc_update IS NOT INITIAL.
*    PERFORM fi_doc_update USING lt_doc_update.
*  ENDIF.

  "Collection log saved
*  MESSAGE s000(38) WITH TEXT-r01.
  IF <LS_ZSDSFIT029>-TRANF_NO IS ASSIGNED.
    MESSAGE S000(38) WITH TEXT-R01 <LS_ZSDSFIT029>-TRANF_NO TEXT-R03.
  ELSE.
    MESSAGE S000(38) WITH TEXT-R04.
  ENDIF.
  COMMIT WORK AND WAIT. "Added 02.12.2024

  GREF_GRID->SET_READY_FOR_INPUT(
    EXPORTING
      I_READY_FOR_INPUT = 0 ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_update_coll_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_UPDATE_COLL_LOG USING UT_OUTPUT           TYPE TT_OUTPUT
                             UT_CURR_STATUS_HIST TYPE TT_STATUS_HIST.

  DATA:
    LT_GRP            TYPE TT_GRP_DOC,
    LT_BPLOG          TYPE STANDARD TABLE OF ZSDSFIT029,
    LT_STATUS_HIST    TYPE TT_STATUS_HIST,
*    lt_doc_update     TYPE tt_doc_update,
    LT_BANK_STATEMENT TYPE TT_BANK_STATEMENT.

  LT_GRP = VALUE #( FOR LS_OUTPUT IN UT_OUTPUT WHERE ( SEL = 'X'  ) ( CORRESPONDING #( LS_OUTPUT  ) )  ).

  IF LT_GRP IS INITIAL.
    MESSAGE S000(38) WITH TEXT-E10.
    RETURN.
  ENDIF.

  LOOP AT LT_GRP ASSIGNING FIELD-SYMBOL(<LS_GRP>).
    IF <LS_GRP>-ZBANK_ITEM IS NOT INITIAL.
      <LS_GRP>-TRN_TYPE = 'TRN'.
      <LS_GRP>-SORT_FIELD =  |{ <LS_GRP>-TRN_TYPE }{ <LS_GRP>-ZBANK_ITEM }|.
    ELSEIF <LS_GRP>-PYMT_METHOD IN GR_BANK.
      <LS_GRP>-TRN_TYPE = 'TRN'.
      <LS_GRP>-SORT_FIELD =  |{ <LS_GRP>-TRN_TYPE }{ <LS_GRP>-KUNNR }{ <LS_GRP>-BILLPL_NO }{ <LS_GRP>-BANKK }{ <LS_GRP>-CHEQUE_NO }|.
    ELSEIF <LS_GRP>-PYMT_METHOD IN GR_PDC.
      <LS_GRP>-TRN_TYPE = 'PDC'.
*      <LS_GRP>-SORT_FIELD =  |{ <LS_GRP>-TRN_TYPE }{ <LS_GRP>-KUNNR }{ <LS_GRP>-BILLPL_NO }{ <LS_GRP>-HBKID }{ <LS_GRP>-HKTID }|.
      <LS_GRP>-SORT_FIELD =  |{ <LS_GRP>-TRN_TYPE }{ <LS_GRP>-KUNNR }{ <LS_GRP>-HBKID }{ <LS_GRP>-HKTID }|.
    ENDIF.

    PERFORM F_IS_POPULATE_TRANF_NO USING <LS_GRP>-TRANF_NO
                                         <LS_GRP>-ACTION_TYPE
                                         <LS_GRP>-STATUS
                                         GT_ACTION_STATUS
                                   CHANGING <LS_GRP>-TRANF_NO_FLAG.

  ENDLOOP.

  PERFORM F_GEN_TRANF_NO CHANGING LT_GRP
                                  LT_BANK_STATEMENT.

  PERFORM F_UPDATE_DEL_ITEM CHANGING LT_BANK_STATEMENT.

*  PERFORM f_gen_update_fi_doc USING lt_grp
*                              CHANGING lt_doc_update.

  LT_BPLOG = CORRESPONDING #( LT_GRP ).

  LOOP AT LT_BPLOG ASSIGNING FIELD-SYMBOL(<LS_ZSDSFIT029>).

    READ TABLE GT_OUTPUT ASSIGNING FIELD-SYMBOL(<LS_OUTPUT>)
      WITH KEY SEL = 'X'
               BUKRS = <LS_ZSDSFIT029>-BUKRS
               BELNR = <LS_ZSDSFIT029>-BELNR
               GJAHR = <LS_ZSDSFIT029>-GJAHR
               BUZEI = <LS_ZSDSFIT029>-BUZEI.
    IF SY-SUBRC EQ 0.
      <LS_OUTPUT>-TRANF_NO = <LS_ZSDSFIT029>-TRANF_NO.
    ENDIF.

    "<<F36K912524 begin of ins
    IF <LS_ZSDSFIT029>-PRCTR IS NOT INITIAL.
      <LS_ZSDSFIT029>-PRCTR       = |{ <LS_ZSDSFIT029>-PRCTR ALPHA = IN }|.
    ENDIF.
    IF <LS_ZSDSFIT029>-KOSTL IS NOT INITIAL.
      <LS_ZSDSFIT029>-KOSTL       = |{ <LS_ZSDSFIT029>-KOSTL ALPHA = IN }|.
    ENDIF.
    "<<F36K912524 end of ins

    <LS_ZSDSFIT029>-UPDATE_BY   = SY-UNAME.
    <LS_ZSDSFIT029>-UPDATE_ON   = SY-DATUM.
    <LS_ZSDSFIT029>-UPDATE_TIME = SY-UZEIT.

    PERFORM F_CREATE_STATUS_HIST USING <LS_ZSDSFIT029>
                                       UT_CURR_STATUS_HIST
                                 CHANGING LT_STATUS_HIST.

  ENDLOOP.

  IF LT_STATUS_HIST IS NOT INITIAL.
    MODIFY ZSDSFIT038 FROM TABLE LT_STATUS_HIST.
  ENDIF.

  MODIFY ZSDSFIT029 FROM TABLE LT_BPLOG.

  IF LT_BANK_STATEMENT IS NOT INITIAL.
    SORT LT_BANK_STATEMENT BY HBKID	ZBANK_ITEM TRNFER_NUMBER DESCENDING.
    DELETE ADJACENT DUPLICATES FROM LT_BANK_STATEMENT COMPARING HBKID	ZBANK_ITEM.

    UPDATE ZSDSFIT042
      FROM TABLE @LT_BANK_STATEMENT INDICATORS SET STRUCTURE COL_IND.
  ENDIF.

*  IF lt_doc_update IS NOT INITIAL.
*    PERFORM fi_doc_update USING lt_doc_update.
*  ENDIF.

  IF <LS_ZSDSFIT029>-TRANF_NO IS ASSIGNED.
    IF <LS_ZSDSFIT029>-TRANF_NO IS NOT INITIAL.
      MESSAGE S000(38) WITH TEXT-R01 <LS_ZSDSFIT029>-TRANF_NO TEXT-R03.
    ELSE.
      MESSAGE S000(38) WITH TEXT-R04.
    ENDIF.
  ELSE.
    MESSAGE S000(38) WITH TEXT-R04.
  ENDIF.
  COMMIT WORK AND WAIT. "Added 02.12.2024

  IF GREF_GRID IS BOUND.
    GREF_GRID->SET_READY_FOR_INPUT(
      EXPORTING
        I_READY_FOR_INPUT = 0 ).
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_validate_mandatory
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_VALIDATE_MANDATORY CHANGING CF_VALID TYPE ABAP_BOOL
                                   CREF_MSG TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.


  CF_VALID = ABAP_TRUE.

  LOOP AT GT_OUTPUT INTO DATA(LS_OUTPUT)    ##INTO_OK
    WHERE SEL = GC_TRUE.

    "Skip check for delete data
    CHECK LS_OUTPUT-DELETE_FLAG = ''.

    DATA(LV_ROW_ID) = SY-TABIX.
    IF LS_OUTPUT-WORK_DATE IS INITIAL.
      CF_VALID = ABAP_FALSE.
      PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_REQUIRED LV_ROW_ID 'WORK_DATE' '' '' CHANGING CREF_MSG.
    ENDIF.

    "Skip check for update bank mode where action type <> 02 and status <> 02
    IF GV_DATA_TYPE = GC_TAB_SEL-UPDATE_BANK.
      READ TABLE GT_ACTION_STATUS
        WITH KEY ACTION_TYPE = LS_OUTPUT-ACTION_TYPE
                 STATUS      = LS_OUTPUT-STATUS
        TRANSPORTING NO FIELDS.
      IF SY-SUBRC NE 0.
        CONTINUE. "Skip check
      ENDIF.
    ENDIF.

    IF LS_OUTPUT-DEDUCT_AMT IS INITIAL.
      CF_VALID = ABAP_FALSE.
      PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_REQUIRED LV_ROW_ID 'DEDUCT_AMT' '' '' CHANGING CREF_MSG.
    ELSEIF LS_OUTPUT-DEDUCT_AMT > 0 AND LS_OUTPUT-DEDUCT_AMT > LS_OUTPUT-WRBTR.
      CF_VALID = ABAP_FALSE.
      PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_AMT_OVER LV_ROW_ID 'DEDUCT_AMT' TEXT-E05 '' CHANGING CREF_MSG.
    ELSEIF LS_OUTPUT-DEDUCT_AMT < 0 AND LS_OUTPUT-DEDUCT_AMT < LS_OUTPUT-WRBTR.
      PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_AMT_LOWER LV_ROW_ID 'DEDUCT_AMT' TEXT-E05 '' CHANGING CREF_MSG.
    ENDIF.

    IF LS_OUTPUT-PYMT_METHOD IS INITIAL.
*      cf_valid = abap_false.
*      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'PYMT_METHOD' '' CHANGING cref_msg.
    ELSEIF LS_OUTPUT-PYMT_METHOD IN GR_BANK.  "Bank transfer
      READ TABLE GT_ACTION_STATUS
        INTO DATA(LS_ACTION_STATUS)
        WITH KEY ACTION_TYPE = LS_OUTPUT-ACTION_TYPE
                 STATUS      = LS_OUTPUT-STATUS.
      IF LS_ACTION_STATUS IS NOT INITIAL AND LS_OUTPUT-HBKID IS INITIAL.
        CF_VALID = ABAP_FALSE.
        PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_REQUIRED LV_ROW_ID 'HBKID' '' '' CHANGING CREF_MSG.
      ENDIF.

      IF LS_ACTION_STATUS IS NOT INITIAL AND LS_OUTPUT-HKTID IS INITIAL.
        CF_VALID = ABAP_FALSE.
        PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_REQUIRED LV_ROW_ID 'HKTID' '' '' CHANGING CREF_MSG.
      ENDIF.

    ELSEIF LS_OUTPUT-PYMT_METHOD IN GR_PDC. "PDC

      READ TABLE GT_ACTION_STATUS
        WITH KEY ACTION_TYPE = LS_OUTPUT-ACTION_TYPE
                 STATUS      = LS_OUTPUT-STATUS
        TRANSPORTING NO FIELDS.
      IF SY-SUBRC EQ 0.

        IF LS_OUTPUT-BANK_DATE IS INITIAL.
          CF_VALID = ABAP_FALSE.
          PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_REQUIRED LV_ROW_ID 'BANK_DATE' '' '' CHANGING CREF_MSG.
        ENDIF.

        IF LS_OUTPUT-CHEQUE_NO IS INITIAL.
          CF_VALID = ABAP_FALSE.
          PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_REQUIRED LV_ROW_ID 'CHEQUE_NO' '' '' CHANGING CREF_MSG.
        ENDIF.

        IF LS_OUTPUT-BANKK IS INITIAL.
          CF_VALID = ABAP_FALSE.
          PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_REQUIRED LV_ROW_ID 'BANKK' '' '' CHANGING CREF_MSG.
        ENDIF.

        IF LS_OUTPUT-BANKN IS INITIAL.
          CF_VALID = ABAP_FALSE.
          PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_REQUIRED LV_ROW_ID 'BANKN' '' '' CHANGING CREF_MSG.
        ENDIF.

      ENDIF.

    ENDIF.

    IF LS_OUTPUT-ACTION_TYPE IS INITIAL.
      CF_VALID = ABAP_FALSE.
      PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_REQUIRED LV_ROW_ID 'ACTION_TYPE' '' '' CHANGING CREF_MSG.
    ENDIF.

    IF LS_OUTPUT-STATUS IS INITIAL.
      CF_VALID = ABAP_FALSE.
      PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_REQUIRED LV_ROW_ID 'STATUS' '' '' CHANGING CREF_MSG.
    ENDIF.

    IF LS_OUTPUT-EXPS_AMT IS NOT INITIAL.
      IF LS_OUTPUT-AUFNR_EXPS IS INITIAL.
        CF_VALID = ABAP_FALSE.
        PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_REQUIRED LV_ROW_ID 'AUFNR_EXPS' 'for expense'(m01) '' CHANGING CREF_MSG.
      ENDIF.
    ENDIF.

    IF LS_OUTPUT-FEE IS NOT INITIAL.
      IF LS_OUTPUT-AUFNR_FEE IS INITIAL.
        CF_VALID = ABAP_FALSE.
        PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_REQUIRED LV_ROW_ID 'AUFNR_FEE' 'for bank fee'(m02) '' CHANGING CREF_MSG.
      ENDIF.
    ENDIF.

*    IF ls_output-retention IS NOT INITIAL AND
*       ls_output-due_on IS INITIAL.
*      cf_valid = abap_false.
*      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'DUE_ON' 'for retention'(m03) '' CHANGING cref_msg  ##SHARE_OK.
*    ENDIF.

    CLEAR LS_ACTION_STATUS.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_validate_mandatory
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_VALIDATE_INPUT     CHANGING CF_VALID TYPE ABAP_BOOL
                                   CREF_MSG TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA:
    LV_TOTAL  TYPE ZSDSFIT029-PAYIN_AMT,
    LV_REMAIN TYPE ZSDSFIT029-PAYIN_AMT,
    LT_CHG    TYPE TT_OUTPUT.


  CHECK CF_VALID = GC_TRUE.

  LOOP AT GT_OUTPUT INTO DATA(LS_OUTPUT)
    WHERE SEL = GC_TRUE
    AND   DELETE_FLAG = ''.

    DATA(LV_ROW_ID) = SY-TABIX.

    IF LS_OUTPUT-REMAIN_AMT > 0 AND LS_OUTPUT-DEDUCT_AMT > LS_OUTPUT-REMAIN_AMT.
      CF_VALID = ABAP_FALSE.
      PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_AMT_OVER LV_ROW_ID 'DEDUCT_AMT' LS_OUTPUT-REMAIN_AMT '' CHANGING CREF_MSG.
    ENDIF.

*    IF ls_output-payin_amt > ls_output-wrbtr.
*      cf_valid = abap_false.
*      PERFORM f_add_msg USING gc_msg_ty-err_amt_balance lv_row_id 'PAYIN_AMT' ls_output-wrbtr '' CHANGING cref_msg.
*    ENDIF.

*    lv_total += ls_output-payin_amt.
    LV_TOTAL += ( LS_OUTPUT-RECEIVED_AMT + LS_OUTPUT-INCOME_AMT + LS_OUTPUT-CASH_CON ).

    IF P_RCVAMT IS INITIAL AND
       GV_DATA_TYPE NE GC_TAB_SEL-UPDATE_BANK. "Added 02.12.2024 (Remove check as func request)
      READ TABLE GT_ACTION_STATUS
        WITH KEY ACTION_TYPE = LS_OUTPUT-ACTION_TYPE
                 STATUS      = LS_OUTPUT-STATUS
        TRANSPORTING NO FIELDS.
      IF SY-SUBRC EQ 0 AND
        ( LS_OUTPUT-PYMT_METHOD IN GR_BANK OR
          LS_OUTPUT-PYMT_METHOD IN GR_PDC ).
        CF_VALID = ABAP_FALSE.
        PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_ACTION_STATUS LV_ROW_ID 'ACTION_TYPE'
                                LS_OUTPUT-ACTION_TYPE LS_OUTPUT-STATUS CHANGING CREF_MSG.
      ENDIF.
    ENDIF.

    IF LS_OUTPUT-HBKID IS NOT INITIAL AND
       LS_OUTPUT-HKTID IS NOT INITIAL.
      SELECT SINGLE HKONT
        INTO @DATA(LV_HKONT)    ##NEEDED
        FROM T012K
        WHERE BUKRS = @LS_OUTPUT-BUKRS
        AND   HBKID = @LS_OUTPUT-HBKID
        AND   HKTID = @LS_OUTPUT-HKTID.
      IF SY-SUBRC <> 0.
        PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_INVALID_ACC_ID LV_ROW_ID 'HKTID'
                                LS_OUTPUT-HKTID ''  CHANGING CREF_MSG.
      ENDIF.
    ENDIF.

    IF LS_OUTPUT-ZBANK_ITEM IS NOT INITIAL.
      SELECT SINGLE TRANF
        INTO @DATA(LV_TRANF)      ##NEEDED
        FROM ZSDSFIT042
        WHERE HBKID       = @LS_OUTPUT-HBKID
        AND   ZBANK_ITEM  = @LS_OUTPUT-ZBANK_ITEM.
      IF SY-SUBRC <> 0.
        PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_INVALID_INPUT LV_ROW_ID 'ZBANK_ITEM'
                                LS_OUTPUT-ZBANK_ITEM ''  CHANGING CREF_MSG.
      ENDIF.
    ENDIF.

*-Beg of INS by Jutamas Y.
*    READ TABLE gt_billpl INTO DATA(ls_billpl)
*      WITH KEY bukrs = ls_output-bukrs
*               belnr = ls_output-belnr
*               gjahr = ls_output-gjahr .
*    IF sy-subrc = 0 .
*      IF ls_billpl-billpl_no NE  ls_output-billpl_no.
*        cf_valid = abap_false.
*        PERFORM f_add_msg USING gc_msg_ty-err_invalid_billpl_no lv_row_id 'BILLPL_NO'
*                                ls_output-billpl_no ''  CHANGING cref_msg.
*      ENDIF.

*      IF ls_billpl-billpl_date NE  ls_output-billpl_date.
*        cf_valid = abap_false.
*        PERFORM f_add_msg USING gc_msg_ty-err_invalid_billpl_date lv_row_id 'BILLPL_DATE'
*                                ls_output-billpl_date ''  CHANGING cref_msg.
*      ENDIF.
*    ENDIF.
*-End of INS by Jutamas Y.

  ENDLOOP.


  IF P_RCVAMT IS NOT INITIAL AND
    GV_DATA_TYPE NE GC_TAB_SEL-UPDATE_BANK.
    LV_REMAIN = P_RCVAMT - LV_TOTAL.

    LT_CHG = VALUE #( FOR LS_CHG IN GT_OUTPUT WHERE ( SEL = 'X' AND DELETE_FLAG = ''  ) ( CORRESPONDING #( LS_CHG  ) )  ).

    IF LV_REMAIN <> 0 AND LT_CHG IS NOT INITIAL.
      CF_VALID = ABAP_FALSE.
      PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_AMT_BALANCE LV_ROW_ID 'PAYIN_AMT' '' '' CHANGING CREF_MSG.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_add_msg
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_ADD_MSG USING
                UF_ERR_TYPE   TYPE CHAR20
                UF_ROWID      TYPE INT4
                UF_FIELDNAME  TYPE LVC_FNAME
                UF_TEXT1                                ##PERF_NO_TYPE
                UF_TEXT2                                ##PERF_NO_TYPE
               CHANGING
                CREF_MSG      TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA:
    LV_MSGID     TYPE SYMSGID,
    LV_MSGTY     TYPE SYMSGTY,
    LV_MSGNO     TYPE SYMSGNO,
    LV_MSGV1     TYPE SYMSGV1,
    LV_MSGV2     TYPE SYMSGV2,
    LV_MSGV3     TYPE SYMSGV3,
    LV_MSGV4     TYPE SYMSGV4,
    LV_FIELDNAME TYPE LVC_FNAME,
    LV_ROWID     TYPE INT4.


  CASE UF_ERR_TYPE.
    WHEN GC_MSG_TY-ERR_REQUIRED.
      GREF_GRID->GET_FRONTEND_FIELDCATALOG(
        IMPORTING
          ET_FIELDCATALOG = DATA(LT_FCAT)        " Field Catalog
      ).

      "Error in item nnn, please input required field xxx
      LV_MSGID = '38'.
      LV_MSGTY = 'E'.
      LV_MSGNO = '000'.
      LV_MSGV1 = |{ TEXT-E04 } { UF_ROWID },|. "Error in item nn
      LV_MSGV2 = |{ TEXT-E03 } { VALUE #( LT_FCAT[ FIELDNAME = UF_FIELDNAME ]-SELTEXT OPTIONAL ) }| .    "please input required field
      LV_MSGV3 = UF_TEXT1.
      LV_FIELDNAME = UF_FIELDNAME.
      LV_ROWID     = UF_ROWID.
    WHEN GC_MSG_TY-ERR_AMT_OVER.
      "Error in item nnn, input amount is over xxx
      LV_MSGID = '38'.
      LV_MSGTY = 'E'.
      LV_MSGNO = '000'.
      LV_MSGV1 = |{ TEXT-E04 } { UF_ROWID }|. "Error in item nnn
      LV_MSGV2 = |{ TEXT-E06 } { UF_TEXT1 }|. "input amount is over xxx
      LV_FIELDNAME = UF_FIELDNAME.
      LV_ROWID     = UF_ROWID.

    WHEN GC_MSG_TY-ERR_AMT_LOWER.
      "Error in item nnn, input amount is over xxx
      LV_MSGID = '38'.
      LV_MSGTY = 'E'.
      LV_MSGNO = '000'.
      LV_MSGV1 = |{ TEXT-E04 } { UF_ROWID }|. "Error in item nnn
      LV_MSGV2 = |{ TEXT-E02 } { UF_TEXT1 }|. "input amount is lower xxx
      LV_FIELDNAME = UF_FIELDNAME.
      LV_ROWID     = UF_ROWID.

    WHEN GC_MSG_TY-ERR_AMT_BALANCE.
      "The difference is too large for clearing
      LV_MSGID = 'F5'.
      LV_MSGTY = 'E'.
      LV_MSGNO = '263'.
      LV_FIELDNAME = UF_FIELDNAME.
      LV_ROWID     = UF_ROWID.

    WHEN GC_MSG_TY-ERR_ACTION_STATUS.
      LV_MSGID = '38'.
      LV_MSGTY = 'E'.
      LV_MSGNO = '000'.
      LV_MSGV1 = |{ TEXT-E04 } { UF_ROWID }|. "Error in item nnn
      LV_MSGV2 = |{ TEXT-E07 } { UF_TEXT1 }|. "Can not input action type xxx
      LV_MSGV3 = |{ TEXT-E08 } { UF_TEXT2 } { TEXT-E09 }|. "Status xxx with empty input Amount Receive
      LV_FIELDNAME = UF_FIELDNAME.
      LV_ROWID     = UF_ROWID.

    WHEN GC_MSG_TY-ERR_ACTION_STATUS_BEFORE.
      LV_MSGID = '38'.
      LV_MSGTY = 'E'.
      LV_MSGNO = '000'.
      LV_MSGV1 = |{ TEXT-E04 } { UF_ROWID }|. "Error in item nnn
      LV_MSGV2 = |{ TEXT-E19 } ({ UF_TEXT1 }) { TEXT-E20 }|. "Input value (xxx) not allowed
      LV_FIELDNAME = UF_FIELDNAME.
      LV_ROWID     = UF_ROWID.

    WHEN GC_MSG_TY-ERR_INPUT_NOT_ALLOW.
      LV_MSGID = '38'.
      LV_MSGTY = 'E'.
      LV_MSGNO = '000'.
      LV_MSGV1 = |{ TEXT-E04 } { UF_ROWID }|. "Error in item nnn
      LV_MSGV2 = |{ TEXT-E16 } ({ UF_TEXT1 })|. "Can not input action type / status (xxx)
      LV_MSGV3 = |{ TEXT-E17 } { UF_TEXT2 } { TEXT-E18 }|. "payment method yyy for mode before receiving payment
      LV_FIELDNAME = UF_FIELDNAME.
      LV_ROWID     = UF_ROWID.

    WHEN GC_MSG_TY-ERR_INVALID_INPUT.
      LV_MSGID = '38'.
      LV_MSGTY = 'E'.
      LV_MSGNO = '000'.
      LV_MSGV1 = |{ TEXT-E04 } { UF_ROWID }|. "Error in item nnn
      LV_MSGV2 = |{ TEXT-E24 } ({ UF_TEXT1 })|. "Invalid input value (xxx)
      LV_FIELDNAME = UF_FIELDNAME.
      LV_ROWID     = UF_ROWID.

    WHEN GC_MSG_TY-ERR_INVALID_ACC_ID.
      LV_MSGID = '38'.
      LV_MSGTY = 'E'.
      LV_MSGNO = '000'.
      LV_MSGV1 = |{ TEXT-E04 } { UF_ROWID }|. "Error in item nnn
      LV_MSGV2 = |{ TEXT-E26 } ({ UF_TEXT1 }).|. "Incorrect account ID (xxx)
      LV_MSGV3 = |{ TEXT-E27 }|.  "Please select new Account ID
      LV_FIELDNAME = UF_FIELDNAME.
      LV_ROWID     = UF_ROWID.

*-Beg of INS by Jutamas Y.
    WHEN GC_MSG_TY-ERR_INVALID_BILLPL_NO.
      LV_MSGID = '38'.
      LV_MSGTY = 'E'.
      LV_MSGNO = '000'.
      LV_MSGV1 = |{ TEXT-E04 } { UF_ROWID }|. "Error in item nnn
      LV_MSGV2 = |{ TEXT-E29 } ({ UF_TEXT1 })|. "Can not input BillingPL No.
      LV_FIELDNAME = UF_FIELDNAME.
      LV_ROWID     = UF_ROWID.
    WHEN GC_MSG_TY-ERR_INVALID_BILLPL_DATE.
      LV_MSGID = '38'.
      LV_MSGTY = 'E'.
      LV_MSGNO = '000'.
      LV_MSGV1 = |{ TEXT-E04 } { UF_ROWID }|. "Error in item nnn
      LV_MSGV2 = |{ TEXT-E30 } ({ UF_TEXT1 })|. "Can not input BillingPL Date
      LV_FIELDNAME = UF_FIELDNAME.
      LV_ROWID     = UF_ROWID.
*-End of INS by Jutamas Y.

    WHEN OTHERS.

  ENDCASE.

  CREF_MSG->ADD_PROTOCOL_ENTRY(
    I_MSGID     = LV_MSGID            " Message ID
    I_MSGTY     = LV_MSGTY            " Message Type
    I_MSGNO     = LV_MSGNO            " Message No.
    I_MSGV1     = LV_MSGV1            " Message Variable1
    I_MSGV2     = LV_MSGV2            " Message Variable2
    I_MSGV3     = LV_MSGV3            " Message Variable3
    I_MSGV4     = LV_MSGV4            " Message Variable4
    I_FIELDNAME = LV_FIELDNAME        " Field Name
    I_ROW_ID    = LV_ROWID            " RowID
*   i_tabix     = lv_tabix            " Table Index
  ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_lock_tran
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_LOCK_TRAN USING  UF_DATA_TYPE TYPE SY-UCOMM
                        UT_OUTPUT TYPE TT_OUTPUT.

  CASE UF_DATA_TYPE.
    WHEN GC_TAB_SEL-COLLECTION_LOG.
      IF RB_DIS = GC_TRUE.
        RETURN.
      ENDIF.
    WHEN GC_TAB_SEL-UPDATE_BANK.
      IF RB_DISU = GC_TRUE.
        RETURN.
      ENDIF.
    WHEN GC_TAB_SEL-MEMO.
      IF RB_DISM = GC_TRUE OR
         RB_CREM = GC_TRUE.
        RETURN.
      ENDIF.
  ENDCASE.

  LOOP AT UT_OUTPUT INTO DATA(LS_OUTPUT)    ##INTO_OK
    GROUP BY ( KEY1 = LS_OUTPUT-KUNNR
               KEY2 = LS_OUTPUT-BUKRS
               KEY3 = LS_OUTPUT-BELNR
               KEY4 = LS_OUTPUT-GJAHR ).

    CALL FUNCTION 'ENQUEUE_EZSDSFIS090'
      EXPORTING
        MODE_ZSDSFIS090 = 'E'
        KUNNR           = LS_OUTPUT-KUNNR
        BUKRS           = LS_OUTPUT-BUKRS
        BELNR           = LS_OUTPUT-BELNR
        GJAHR           = LS_OUTPUT-GJAHR
        _COLLECT        = 'X'
      EXCEPTIONS
        FOREIGN_LOCK    = 1
        SYSTEM_FAILURE  = 2
        OTHERS          = 3.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDLOOP.
  CALL FUNCTION 'FLUSH_ENQUEUE'
    EXCEPTIONS
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2
      OTHERS         = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_unlock_tran
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_UNLOCK_TRAN USING UT_OUTPUT TYPE TT_OUTPUT.

  LOOP AT UT_OUTPUT INTO DATA(LS_OUTPUT)      ##INTO_OK
    GROUP BY ( KEY1 = LS_OUTPUT-KUNNR
               KEY2 = LS_OUTPUT-BUKRS
               KEY3 = LS_OUTPUT-BELNR
               KEY4 = LS_OUTPUT-GJAHR ).

    CALL FUNCTION 'DEQUEUE_EZSDSFIS090'
      EXPORTING
        MODE_ZSDSFIS090 = 'E'
        MANDT           = SY-MANDT
        KUNNR           = LS_OUTPUT-KUNNR
        BUKRS           = LS_OUTPUT-BUKRS
        BELNR           = LS_OUTPUT-BELNR
        GJAHR           = LS_OUTPUT-GJAHR
*       _SCOPE          = '3'
*       _SYNCHRON       = ' '
*       _COLLECT        = ' '
      .
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_delete_coll_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_DELETE_COLL_LOG .

  LOOP AT GT_OUTPUT ASSIGNING FIELD-SYMBOL(<LS_OUTPUT>)
    WHERE SEL = 'X'.
    IF <LS_OUTPUT>-DELETE_FLAG = GC_TRUE.
      <LS_OUTPUT>-DELETE_FLAG = ''.
      CLEAR <LS_OUTPUT>-DELETE_DATE.
    ELSE.
      <LS_OUTPUT>-DELETE_FLAG = GC_TRUE.
      <LS_OUTPUT>-DELETE_DATE = SY-DATUM.
    ENDIF.
  ENDLOOP.

  GREF_GRID->REFRESH_TABLE_DISPLAY(
    EXCEPTIONS
      FINISHED = 1                " Display was Ended (by Export)
      OTHERS   = 2
  ).

  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_hotspot_click
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> uf_row_id
*&      --> uf_column_id
*&---------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK USING
                        UF_ROW_ID     TYPE  LVC_S_ROW
                        UF_COLUMN_ID  TYPE  LVC_S_COL       ##CALLED.

  IF SY-TCODE <> GC_TCODE_HIS.

    READ TABLE GT_OUTPUT INTO DATA(LS_OUTPUT)
      INDEX UF_ROW_ID-INDEX.

    CASE UF_COLUMN_ID-FIELDNAME.
      WHEN 'BELNR'.
        IF LS_OUTPUT-BELNR IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD LS_OUTPUT-BELNR.
          SET PARAMETER ID 'BUK' FIELD LS_OUTPUT-BUKRS.
          SET PARAMETER ID 'GJR' FIELD LS_OUTPUT-GJAHR.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
        ENDIF.

      WHEN 'REMAIN_AMT'.
        CALL FUNCTION 'Z_SDSFI_POPUP'
          EXPORTING
*           I_START_COLUMN       = 25
*           I_START_LINE         = 6
*           I_END_COLUMN         = 100
*           I_END_LINE           = 16
*           I_TITLE = 'Popup'
            I_POPUP = GC_TRUE
          TABLES
            IT_DATA = LS_OUTPUT-BILLPL_LIST.
      WHEN 'VBELN'.
        IF LS_OUTPUT-VBELN IS NOT INITIAL.
*<-- Start of Insertion 20.06.2025 (Check Sales Order)
          SELECT SINGLE VBELN
            FROM VBAK
           WHERE VBELN EQ @LS_OUTPUT-VBELN
            INTO @DATA(LF_VBELN) ##NEEDED.
          IF SY-SUBRC NE 0.
*           Error: Cannot display document &1 due to it is not sales document.
            MESSAGE S021(ZSDSFI01) WITH LS_OUTPUT-VBELN.
          ELSE.
*--> End of Insertion 20.06.2025
            SET PARAMETER ID 'AUN' FIELD LS_OUTPUT-VBELN.
            CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          ENDIF.            "+20.06.2025
        ENDIF.

      WHEN 'VBELN_VF'.
        IF LS_OUTPUT-VBELN_VF IS NOT INITIAL.
          SET PARAMETER ID 'VF' FIELD LS_OUTPUT-VBELN_VF.
          CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
        ENDIF.

    ENDCASE.

  ELSE.

    READ TABLE GT_HISTORY_LOG INTO DATA(LS_HISTORY_LOG)
      INDEX UF_ROW_ID-INDEX.

    CASE UF_COLUMN_ID-FIELDNAME.
      WHEN 'BELNR'.
        IF LS_HISTORY_LOG-BELNR IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD LS_HISTORY_LOG-BELNR.
          SET PARAMETER ID 'BUK' FIELD LS_HISTORY_LOG-BUKRS.
          SET PARAMETER ID 'GJR' FIELD LS_HISTORY_LOG-GJAHR.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
        ENDIF.
      WHEN 'BELNR_CLR'.
        IF LS_HISTORY_LOG-BELNR_CLR IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD LS_HISTORY_LOG-BELNR_CLR.
          SET PARAMETER ID 'BUK' FIELD LS_HISTORY_LOG-BUKRS_CLR.
          SET PARAMETER ID 'GJR' FIELD LS_HISTORY_LOG-GJAHR_CLR.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
        ENDIF.
*<<F36K917931 - 03 Begin of ins
      WHEN 'STBLG'.
        IF LS_HISTORY_LOG-STBLG IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD LS_HISTORY_LOG-STBLG.
          SET PARAMETER ID 'BUK' FIELD LS_HISTORY_LOG-BUKRS_CLR.
          SET PARAMETER ID 'GJR' FIELD LS_HISTORY_LOG-STJAH.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
        ENDIF.
*<<F36K917931 - 03 End of ins
      WHEN 'VBELN'.
        IF LS_HISTORY_LOG-VBELN IS NOT INITIAL.
*<-- Start of Insertion 20.06.2025 (Check Sales Order)
          SELECT SINGLE VBELN
            FROM VBAK
           WHERE VBELN EQ @LS_HISTORY_LOG-VBELN
            INTO @LF_VBELN.
          IF SY-SUBRC NE 0.
*           Error: Cannot display document &1 due to it is not sales document.
            MESSAGE S021(ZSDSFI01) WITH LS_HISTORY_LOG-VBELN.
          ELSE.
*--> End of Insertion 20.06.2025
            SET PARAMETER ID 'AUN' FIELD LS_HISTORY_LOG-VBELN.
            CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          ENDIF.            "+20.06.2025
        ENDIF.
      WHEN 'VGBEL'.
        IF LS_HISTORY_LOG-VGBEL IS NOT INITIAL.
          SET PARAMETER ID 'VL' FIELD LS_HISTORY_LOG-VGBEL.
          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
        ENDIF.
      WHEN 'VBELN_VF'.
        IF LS_HISTORY_LOG-VBELN_VF IS NOT INITIAL.
          SET PARAMETER ID 'VF' FIELD LS_HISTORY_LOG-VBELN_VF.
          CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
        ENDIF.

    ENDCASE.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_mark_sel_all
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_SEL
*&      <-- GT_OUTPUT
*&---------------------------------------------------------------------*
FORM F_MARK_SEL_ALL  USING    UF_SEL    TYPE CHAR01
                     CHANGING CT_OUTPUT TYPE TT_OUTPUT.

  LOOP AT CT_OUTPUT ASSIGNING FIELD-SYMBOL(<LS_OUTPUT>).
    IF VALUE #( <LS_OUTPUT>-CELLTAB[ FIELDNAME = 'SEL' ]-STYLE OPTIONAL ) = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED .
      <LS_OUTPUT>-SEL = UF_SEL.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_create_status_hist
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LS_ZSDSFIT029>
*&      <-- LT_STATUS_HIST
*&---------------------------------------------------------------------*
FORM F_CREATE_STATUS_HIST  USING    UF_LOG TYPE ZSDSFIT029
                              VALUE(UT_CURR_STATUS_HIST) TYPE TT_STATUS_HIST
                           CHANGING CT_STATUS_HIST TYPE TT_STATUS_HIST.

  DATA: LS_STATUS_HIST     TYPE ZSDSFIT038,
        LS_STATUS_HIST_OLD TYPE ZSDSFIT038,
        LS_STATUS_HIST_NEW TYPE ZSDSFIT038.


  SORT UT_CURR_STATUS_HIST BY BUKRS BELNR GJAHR BUZEI SEQ SUB_SEQ DESCENDING.

  "Get lastest hist data
  LOOP AT UT_CURR_STATUS_HIST
    ASSIGNING FIELD-SYMBOL(<LS_STATUS_HIST>)
    WHERE BUKRS = UF_LOG-BUKRS
    AND   BELNR = UF_LOG-BELNR
    AND   GJAHR = UF_LOG-GJAHR
    AND   BUZEI = UF_LOG-BUZEI
    AND   SEQ   = UF_LOG-SEQ.
    EXIT.
  ENDLOOP.

  LS_STATUS_HIST = CORRESPONDING #( UF_LOG ).

  IF <LS_STATUS_HIST> IS ASSIGNED.
    IF <LS_STATUS_HIST>-PERNR       = UF_LOG-PERNR AND
       <LS_STATUS_HIST>-WORK_DATE   = UF_LOG-WORK_DATE AND
       <LS_STATUS_HIST>-ACTION_TYPE = UF_LOG-ACTION_TYPE AND
       <LS_STATUS_HIST>-STATUS      = UF_LOG-STATUS.

      LS_STATUS_HIST_OLD = CORRESPONDING #( <LS_STATUS_HIST> EXCEPT SUB_SEQ ERNAM_HIST ERDAT_HIST ERZMT_HIST ).
      LS_STATUS_HIST_NEW = CORRESPONDING #( UF_LOG ).

      IF LS_STATUS_HIST_OLD = LS_STATUS_HIST_NEW.
        "No history log add for no action type and status change
        RETURN.
      ELSE.
        LS_STATUS_HIST-SUB_SEQ = <LS_STATUS_HIST>-SUB_SEQ.  "Update existing record
      ENDIF.
    ELSE.
      LS_STATUS_HIST-SUB_SEQ = <LS_STATUS_HIST>-SUB_SEQ + 1.
    ENDIF.

  ELSE.
    LS_STATUS_HIST-SUB_SEQ = 1.
  ENDIF.

  LS_STATUS_HIST-TCODE      = SY-TCODE. "<<F36K910967 ins
  LS_STATUS_HIST-ERNAM_HIST = SY-UNAME.
  LS_STATUS_HIST-ERDAT_HIST = SY-DATUM.
  LS_STATUS_HIST-ERZMT_HIST = SY-UZEIT.

  APPEND LS_STATUS_HIST TO CT_STATUS_HIST.

ENDFORM.                                                 "#EC CI_VALPAR

*&---------------------------------------------------------------------*
*& Form f_prepare_f4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_PREPARE_F4   ##CALLED.

  DATA:
    LS_F4 TYPE LVC_S_F4,
    LT_F4 TYPE LVC_T_F4.


  CLEAR LS_F4.
  LS_F4-FIELDNAME = 'BANKK'.
  LS_F4-REGISTER  = GC_TRUE.
  LS_F4-GETBEFORE = GC_TRUE.
  LS_F4-CHNGEAFTER = SPACE.
  INSERT LS_F4 INTO TABLE LT_F4.

  CALL METHOD GREF_GRID->REGISTER_F4_FOR_FIELDS
    EXPORTING
      IT_F4 = LT_F4[].

* register events for abap objects (backend)
  CREATE OBJECT GREF_ONF4.
  SET HANDLER GREF_ONF4->ON_F4 FOR GREF_GRID.

ENDFORM.

**&---------------------------------------------------------------------*
**& Form f_prepare_drdn
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_prepare_drdn .
*
*  DATA: lt_domvalue TYPE STANDARD TABLE OF dd07v,
*        lt_dropdown TYPE lvc_t_dral,
*        ls_dropdown TYPE lvc_s_dral,
*        lv_domname  TYPE dd07l-domname.
*
*  DO 2 TIMES.
*    CASE sy-index.
*      WHEN 1.
*        lv_domname = gc_dom-action.
*      WHEN 2.
*        lv_domname = gc_dom-status.
*    ENDCASE.
*
*    CLEAR lt_domvalue.
*
*    CALL FUNCTION 'DD_DOMVALUES_GET'
*      EXPORTING
*        domname        = lv_domname
*        text           = gc_true
*        langu          = sy-langu
**   IMPORTING
**       RC             =
*      TABLES
*        dd07v_tab      = lt_domvalue
*      EXCEPTIONS
*        wrong_textflag = 1
*        OTHERS         = 2.
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*
*    LOOP AT lt_domvalue INTO DATA(ls_dom_value).
*      ls_dropdown-handle    = sy-index.
*      ls_dropdown-value     = ls_dom_value-ddtext.
*      ls_dropdown-int_value = ls_dom_value-domvalue_l.
*      APPEND ls_dropdown TO lt_dropdown.
*    ENDLOOP.
*
*  ENDDO.
*
*  IF lt_dropdown IS NOT INITIAL.
*    gref_grid->set_drop_down_table(
*      it_drop_down_alias = lt_dropdown        " ALV Control: Dropdown List Boxes
*    ).
*  ENDIF.
*
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_duplicate_sel_item
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      <-- GT_OUTPUT
**&---------------------------------------------------------------------*
*FORM f_duplicate_sel_item  CHANGING ct_output TYPE tt_output.
*
*  DATA lt_dup TYPE tt_output.
*
*  LOOP AT ct_output INTO DATA(ls_output)
*    WHERE sel = gc_true.
*    APPEND INITIAL LINE TO lt_dup ASSIGNING FIELD-SYMBOL(<ls_dup>).
*    <ls_dup> = ls_output.
*    <ls_dup>-seq += 1.
*
*    <ls_dup>-remain_amt = ls_output-remain_c_amt.
*
*    PERFORM f_default_value_new_item CHANGING <ls_dup>.
*
*  ENDLOOP.
*
*  APPEND LINES OF lt_dup TO ct_output.
*
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_pernr_name
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LS_OUTPUT>_PERNR
*&      <-- <LS_OUTPUT>_FULL_NAME
*&---------------------------------------------------------------------*
FORM F_GET_PERNR_NAME  USING    UF_PERNR      TYPE ZSDSFIT029-PERNR
                       CHANGING CF_FULL_NAME  TYPE ZSDSFIS085-FULL_NAME.

  DATA: LS_PERNR TYPE GTY_PERSONAL.

  CF_FULL_NAME = VALUE #( GT_FULLNAME[ PERNR = UF_PERNR ]-FULL_NAME OPTIONAL ).
  IF CF_FULL_NAME IS INITIAL.
    SELECT SINGLE
        PA0002~PERNR,
        CONCAT_WITH_SPACE( PA0002~VORNA, PA0002~NACHN, 1 ) AS FULL_NAME
      FROM PA0002
      WHERE PERNR = @UF_PERNR
      INTO @LS_PERNR    ##WARN_OK .                     "#EC CI_NOORDER
    IF SY-SUBRC = 0.
      INSERT LS_PERNR INTO TABLE GT_FULLNAME.
      CF_FULL_NAME = LS_PERNR-FULL_NAME.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_default_order_exp
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LS_OUTPUT>_PERNR
*&      <-- <LS_OUTPUT>_FULL_NAME
*&---------------------------------------------------------------------*
FORM F_SET_DEFAULT_ORDER_EXP USING
                               UF_EXPS_AMT TYPE ZSDSFIT029-EXPS_AMT
                             CHANGING
                               CF_AUFNR_EXPS TYPE ZSDSFIT029-AUFNR_EXPS.
  IF UF_EXPS_AMT IS NOT INITIAL.
    IF CF_AUFNR_EXPS IS INITIAL.
      CF_AUFNR_EXPS = GV_DF_ORDER_EXP.
    ENDIF.
  ELSE.
    CLEAR CF_AUFNR_EXPS.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_set_default_order_fee
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LS_OUTPUT>_PERNR
*&      <-- <LS_OUTPUT>_FULL_NAME
*&---------------------------------------------------------------------*
FORM F_SET_DEFAULT_ORDER_FEE USING
                               UF_FEE TYPE ZSDSFIT029-FEE
                             CHANGING
                               CF_AUFNR_FEE TYPE ZSDSFIT029-AUFNR_FEE.

  IF UF_FEE IS NOT INITIAL.
    IF CF_AUFNR_FEE IS INITIAL.
      CF_AUFNR_FEE = GV_DF_ORDER_FEE.
    ENDIF.
  ELSE.
    CLEAR CF_AUFNR_FEE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_is_populate_tranf_no
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LS_GRP>_TRANF_NO
*&      --> <LS_GRP>_ACTION_TYPE
*&      --> <LS_GRP>_STATUS
*&      --> GT_ACTION_STATUS
*&      <-- <LS_GRP>_TRANF_NO_FLAG
*&---------------------------------------------------------------------*
FORM F_IS_POPULATE_TRANF_NO  USING    UF_TRANF_NO      TYPE ZSDSFIT029-TRANF_NO
                                      UF_ACTION_TYPE   TYPE ZSDSFIT029-ACTION_TYPE
                                      UF_STATUS        TYPE ZSDSFIT029-STATUS
                                      UT_ACTION_STATUS TYPE TT_ACTION_STATUS
                             CHANGING CF_TRANF_NO_FLAG TYPE ABAP_BOOL.

  IF UF_TRANF_NO IS NOT INITIAL.
    CLEAR CF_TRANF_NO_FLAG.
    RETURN.
  ENDIF.

  LOOP AT UT_ACTION_STATUS
    TRANSPORTING NO FIELDS
    WHERE ACTION_TYPE = UF_ACTION_TYPE
    AND   STATUS      = UF_STATUS.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC = 0.
    CF_TRANF_NO_FLAG = GC_TRUE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_gen_tranf_no
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_GRP
*&      <-- LT_BANK_STATEMENT
*&---------------------------------------------------------------------*
FORM F_GEN_TRANF_NO  CHANGING CT_GRP  TYPE TT_GRP_DOC
                              CT_BANK_STATEMENT TYPE TT_BANK_STATEMENT.

  DATA: LV_CURRY TYPE BKPF-GJAHR. "<<F36K910991 ins

  "Update transfer number
  LOOP AT CT_GRP INTO DATA(LS_GRP) GROUP BY ( KEY1 = LS_GRP-TRN_TYPE
                                              KEY2 = LS_GRP-SORT_FIELD
                                              KEY3 = LS_GRP-TRANF_NO_FLAG ).
    CHECK LS_GRP-TRANF_NO_FLAG = GC_TRUE.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR             = GC_NO-NR
        OBJECT                  = GC_NO-OBJ
        TOYEAR                  = SY-DATUM(4)
*       IGNORE_BUFFER           = ' '
      IMPORTING
        NUMBER                  = LS_GRP-TRANF_NO
*       RETURNCODE              =
      EXCEPTIONS
        INTERVAL_NOT_FOUND      = 1
        NUMBER_RANGE_NOT_INTERN = 2
        OBJECT_NOT_FOUND        = 3
        QUANTITY_IS_0           = 4
        QUANTITY_IS_NOT_1       = 5
        INTERVAL_OVERFLOW       = 6
        BUFFER_OVERFLOW         = 7
        OTHERS                  = 8.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      RETURN.
*<<F36K917931 - CH01 Begin of ins
    ELSE.
      SET PARAMETER ID 'ZSDS_VAR_TRANF_NO' FIELD LS_GRP-TRANF_NO.
*<<F36K917931 - CH01 End of ins
    ENDIF.

    LOOP AT CT_GRP ASSIGNING FIELD-SYMBOL(<LS_GRP>)
      WHERE TRN_TYPE      = LS_GRP-TRN_TYPE
      AND   SORT_FIELD    = LS_GRP-SORT_FIELD
      AND   TRANF_NO_FLAG = LS_GRP-TRANF_NO_FLAG.

      <LS_GRP>-TRANF_NO = LS_GRP-TRANF_NO.

      IF  <LS_GRP>-ZBANK_ITEM IS NOT INITIAL.
        APPEND INITIAL LINE TO CT_BANK_STATEMENT ASSIGNING FIELD-SYMBOL(<LS_BANK>).

*<<F36K910991 start of ins
        CLEAR LV_CURRY.

        CALL FUNCTION 'GET_CURRENT_YEAR'
          EXPORTING
            BUKRS = <LS_GRP>-BUKRS
*           DATE  = SY-DATUM
          IMPORTING
*           CURRM =
            CURRY = LV_CURRY.

        <LS_BANK>-FYEAR_TRNFERNO = LV_CURRY.
*<<F36K910991 end of ins

        <LS_BANK>-HBKID           = <LS_GRP>-HBKID.
        <LS_BANK>-ZBANK_ITEM      = <LS_GRP>-ZBANK_ITEM.
        <LS_BANK>-MAP_STATUS      = GC_TRUE.
        <LS_BANK>-TRNFER_NUMBER   = <LS_GRP>-TRANF_NO.
*        <ls_bank>-fyear_trnferno  = sy-datum(4). <<F36K910991 del
        <LS_BANK>-COL_IND-TRNFER_NUMBER   = GC_TRUE.
        <LS_BANK>-COL_IND-FYEAR_TRNFERNO  = GC_TRUE.
        <LS_BANK>-COL_IND-MAP_STATUS      = GC_TRUE.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.

**&---------------------------------------------------------------------*
**& Form f_gen_update_fi_doc
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> LT_GRP
**&      <-- LT_DOC_UPDATE
**&---------------------------------------------------------------------*
*FORM f_gen_update_fi_doc  USING    ut_grp         TYPE tt_grp_doc
*                          CHANGING ct_doc_update  TYPE tt_doc_update.
*
*  "Update transfer number
*  LOOP AT ut_grp INTO DATA(ls_grp) GROUP BY ( key1 = ls_grp-bukrs
*                                              key2 = ls_grp-belnr
*                                              key3 = ls_grp-gjahr
*                                              key4 = ls_grp-buzei ).
*
*    CHECK ls_grp-tranf_no_flag = gc_true.
*
*    APPEND INITIAL LINE TO ct_doc_update ASSIGNING FIELD-SYMBOL(<ls_doc_update>).
*
*    <ls_doc_update> = CORRESPONDING #( ls_grp ).
*    <ls_doc_update>-samnr = ls_grp-billpl_no.
*
*  ENDLOOP.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form fi_doc_update
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> LT_DOC_UPDATE
**&---------------------------------------------------------------------*
*FORM fi_doc_update  USING ut_doc_update TYPE tt_doc_update.
*
*  DATA: lt_accchg TYPE STANDARD TABLE OF accchg,
*        lt_enq    TYPE STANDARD TABLE OF seqg3,
*
*        ls_accchg TYPE accchg,
*        lf_garg   TYPE eqegraarg.
*
*  LOOP AT ut_doc_update INTO DATA(ls_doc_update).
*
*    CLEAR:
*      lf_garg,
*      ls_accchg,
*      lt_accchg,
*      lt_enq.
*
*    DO 3 TIMES.
*      SELECT SINGLE bukrs ##WARN_OK,
*                    belnr,
*                    gjahr,
*                    buzei
*        FROM bsid_view
*        WHERE bukrs = @ls_doc_update-bukrs
*          AND belnr = @ls_doc_update-belnr
*          AND gjahr = @ls_doc_update-gjahr
*          AND buzei = @ls_doc_update-buzei
*        INTO @DATA(ls_bsid).
*      IF sy-subrc = 0.
*        ls_accchg-fdname = 'SAMNR'.
*        ls_accchg-newval = ls_doc_update-samnr.
*        APPEND ls_accchg TO lt_accchg.
*
**       Check if there is a lock on the document before proceeding
*        CALL FUNCTION 'ENQUEUE_READ'
*          EXPORTING
*            gclient               = sy-mandt
*            gname                 = 'BKPF'
*            garg                  = lf_garg
*          TABLES
*            enq                   = lt_enq
*          EXCEPTIONS
*            communication_failure = 1
*            system_failure        = 2
*            OTHERS                = 3.
*
*        IF sy-subrc = 0 AND lt_enq IS INITIAL.
** Call THE FUNCTION MODULE TO UPDATE THE FI DOCUMENT
*          CALL FUNCTION 'FI_DOCUMENT_CHANGE'
*            EXPORTING
*              i_bukrs      = ls_bsid-bukrs
*              i_belnr      = ls_bsid-belnr
*              i_gjahr      = ls_bsid-gjahr
*              i_buzei      = ls_bsid-buzei
*            TABLES
*              t_accchg     = lt_accchg
*            EXCEPTIONS
*              no_reference = 1
*              no_document  = 2
*              OTHERS       = 3.
*          IF sy-subrc = 0.
*            EXIT.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        WAIT UP TO 10 SECONDS.
*      ENDIF.
*    ENDDO.
*
*  ENDLOOP.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_chg_coll_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> UREF_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM F_VALIDATE_CHG_COLL_LOG  USING UREF_DATA_CHANGED TYPE REF TO  CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA:
    LS_MOD_CELL   TYPE LVC_S_MODI ##NEEDED.

  LOOP AT UREF_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELL ##INTO_OK.
    IF LS_MOD_CELL-FIELDNAME = 'ZBANK_ITEM'.
      READ TABLE GT_OUTPUT
        INTO DATA(LS_OUTPUT)
        INDEX LS_MOD_CELL-ROW_ID.

      SELECT SINGLE ZBANK_ITEM
        FROM ZSDSV_BANK_STMT
        WHERE ZBANK_ITEM = @LS_MOD_CELL-VALUE
        AND   HBKID      = @LS_OUTPUT-HBKID
        INTO @DATA(LV_BANK_ITEM)    ##NEEDED  ##WARN_OK.
      IF SY-SUBRC NE 0.
        PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_INVALID_INPUT LS_MOD_CELL-ROW_ID 'ZBANK_ITEM'
                                LS_MOD_CELL-VALUE '' CHANGING UREF_DATA_CHANGED.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_del_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_BANK_STATEMENT
*&---------------------------------------------------------------------*
FORM F_UPDATE_DEL_ITEM  CHANGING CT_BANK_STATEMENT TYPE TT_BANK_STATEMENT.

  DATA LV_CURRY TYPE BKPF-GJAHR.   "<<F36K910991 ins

  DATA(LT_DEL) = GT_OUTPUT.

  DELETE LT_DEL WHERE SEL <> GC_TRUE AND DELETE_FLAG <> GC_TRUE.
  DELETE LT_DEL WHERE HBKID IS INITIAL OR ZBANK_ITEM IS INITIAL.

  SELECT
    LOG~DATA_TYPE,
    LOG~BUKRS,
    LOG~BELNR,
    LOG~GJAHR,
    LOG~BUZEI,
    LOG~SEQ,
    LOG~TRANF_NO
    FROM ZSDSFIT029 AS LOG
    INNER JOIN @LT_DEL AS DEL
    ON LOG~TRANF_NO  = DEL~TRANF_NO
    WHERE LOG~DELETE_FLAG = ''
    INTO TABLE @DATA(LT_CHK).

  LOOP AT LT_DEL INTO DATA(LS_DEL)  ##INTO_OK.
    DELETE LT_CHK WHERE DATA_TYPE  = LS_DEL-DATA_TYPE
                  AND   BUKRS      = LS_DEL-BUKRS
                  AND   BELNR      = LS_DEL-BELNR
                  AND   GJAHR      = LS_DEL-GJAHR
                  AND   BUZEI      = LS_DEL-BUZEI
                  AND   SEQ        = LS_DEL-SEQ.
  ENDLOOP.

  LOOP AT LT_DEL ASSIGNING FIELD-SYMBOL(<LS_DEL>).
    READ TABLE LT_CHK
      WITH KEY TRANF_NO = <LS_DEL>-TRANF_NO
      TRANSPORTING NO FIELDS.
    IF SY-SUBRC <> 0.
      APPEND INITIAL LINE TO CT_BANK_STATEMENT ASSIGNING FIELD-SYMBOL(<LS_BANK>).

*<<F36K910991 start of ins
      CLEAR LV_CURRY.

      CALL FUNCTION 'GET_CURRENT_YEAR'
        EXPORTING
          BUKRS = <LS_DEL>-BUKRS
*         DATE  = SY-DATUM
        IMPORTING
*         CURRM =
          CURRY = LV_CURRY.

      <LS_BANK>-FYEAR_TRNFERNO = LV_CURRY.
*<<F36K910991 end of ins

      <LS_BANK>-HBKID           = <LS_DEL>-HBKID.
      <LS_BANK>-ZBANK_ITEM      = <LS_DEL>-ZBANK_ITEM.
      <LS_BANK>-MAP_STATUS      = GC_TRUE.
      <LS_BANK>-TRNFER_NUMBER   = <LS_DEL>-TRANF_NO.
*      <ls_bank>-fyear_trnferno  = sy-datum(4).     "<<F36K910991 del
      <LS_BANK>-COL_IND-TRNFER_NUMBER   = GC_TRUE.
      <LS_BANK>-COL_IND-FYEAR_TRNFERNO  = GC_TRUE.
      <LS_BANK>-COL_IND-MAP_STATUS      = GC_TRUE.
    ENDIF.
  ENDLOOP.


ENDFORM.

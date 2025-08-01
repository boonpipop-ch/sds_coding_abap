class ZCL_SDSSD_QUOTA_PLAN_UTIL definition
  public
  final
  create public .

public section.

  types TS_QUOTAGRP_MAPPING type ZSDSSDC013 .
  types:
    TT_QUOTAGRP_MAPPING TYPE STANDARD TABLE OF TS_QUOTAGRP_MAPPING
                              WITH DEFAULT KEY .
  types:
    BEGIN OF TS_KEYTAB,
        MATNR     TYPE MATNR,
        DAY_FIRST TYPE ZSDSDE_DAY_FIRST,
        DAY_LAST  TYPE ZSDSDE_DAY_LAST,
      END OF TS_KEYTAB .
  types:
    TT_KEYTAB TYPE STANDARD TABLE OF TS_KEYTAB
                                               WITH DEFAULT KEY .
  types:
    BEGIN OF TS_QUOTA_PLAN_ITM,
        QUOTAGRP     TYPE ZSDSDE_QUOTAGRP,
        PORTION_PERC TYPE ZSDSDE_PORTION,
        ZDEL_FLG     TYPE ZSDSSDT018-ZDEL_FLG,
        ZCRT_DATE    TYPE ZSDSSDT018-ZCRT_DATE,
        ZCRT_TIME    TYPE ZSDSSDT018-ZCRT_TIME,
        ZCRT_USER    TYPE ZSDSSDT018-ZCRT_USER,
        ZCRT_PGM     TYPE ZSDSSDT018-ZCRT_PGM,
        ZUPD_DATE    TYPE ZSDSSDT018-ZUPD_DATE,
        ZUPD_TIME    TYPE ZSDSSDT018-ZUPD_TIME,
        ZUPD_USER    TYPE ZSDSSDT018-ZUPD_USER,
        ZUPD_PGM     TYPE ZSDSSDT018-ZUPD_PGM,
      END OF TS_QUOTA_PLAN_ITM .
  types:
    TT_QUOTA_PLAN_ITM TYPE STANDARD TABLE OF TS_QUOTA_PLAN_ITM
                            WITH DEFAULT KEY .
  types:
    BEGIN OF TS_QUOTA_PLAN,
        MATNR     TYPE MATNR,
        DAY_FIRST TYPE ZSDSDE_DAY_FIRST,
        DAY_LAST  TYPE ZSDSDE_DAY_LAST,
        QUOTA_QTY TYPE ZSDSDE_QUOTA_QTY,
        ZDEL_FLG  TYPE ZSDSSDT016-ZDEL_FLG,
        ZCRT_DATE TYPE ZSDSSDT016-ZCRT_DATE,
        ZCRT_TIME TYPE ZSDSSDT016-ZCRT_TIME,
        ZCRT_USER TYPE ZSDSSDT016-ZCRT_USER,
        ZCRT_PGM  TYPE ZSDSSDT016-ZCRT_PGM,
        ZUPD_DATE TYPE ZSDSSDT016-ZUPD_DATE,
        ZUPD_TIME TYPE ZSDSSDT016-ZUPD_TIME,
        ZUPD_USER TYPE ZSDSSDT016-ZUPD_USER,
        ZUPD_PGM  TYPE ZSDSSDT016-ZUPD_PGM,
        ITEM      TYPE TT_QUOTA_PLAN_ITM,
      END OF TS_QUOTA_PLAN .
  types:
    TT_QUOTA_PLAN TYPE STANDARD TABLE OF TS_QUOTA_PLAN
                        WITH DEFAULT KEY .
  types:
    BEGIN OF TS_MATERIAL_DATA,
        MATNR   TYPE MARA-MATNR,
        MAKTX   TYPE MAKT-MAKTX,
        MEINS   TYPE MARA-MEINS,
        URSTOCK TYPE MARD-LABST,
      END OF TS_MATERIAL_DATA .
  types:
    TT_MATERIAL_DATA TYPE SORTED TABLE OF TS_MATERIAL_DATA
                           WITH UNIQUE KEY MATNR .
  types:
    BEGIN OF TS_PERIOD_SCOPE,
        REQDAT        TYPE SY-DATUM,
        CURRENT       TYPE SPMON,
        CURRENT_BEGIN TYPE SY-DATUM,
        CURRENT_END   TYPE SY-DATUM,
        NEXT1         TYPE SPMON,
        NEXT1_BEGIN   TYPE SY-DATUM,
        NEXT1_END     TYPE SY-DATUM,
      END OF TS_PERIOD_SCOPE .
  types:
    BEGIN OF TS_PO_DATA,
        MATNR  TYPE EKPO-MATNR,
        PERIOD TYPE SPMON,
        EBELN  TYPE EKPO-EBELN,
        EBELP  TYPE EKPO-EBELP,
        ETENR  TYPE EKET-ETENR,
        EINDT  TYPE EKET-EINDT,
        WERKS  TYPE EKPO-WERKS,
        LGORT  TYPE EKPO-LGORT,
        MENGE  TYPE EKET-MENGE,
        MEINS  TYPE EKPO-MEINS,
      END OF TS_PO_DATA .
  types:
    TT_PO_DATA TYPE STANDARD TABLE OF TS_PO_DATA
                     WITH DEFAULT KEY .
  types:
    BEGIN OF TS_QUOTA_DATA,
        MATNR         TYPE ZSDSSDT016-MATNR,
        PERIOD        TYPE SPMON,
        DAY_FIRST     TYPE ZSDSSDT016-DAY_FIRST,
        DAY_LAST      TYPE ZSDSSDT016-DAY_LAST,
        QUOTA_QTY     TYPE ZSDSSDT016-QUOTA_QTY,
        QUOTAGRP      TYPE ZSDSSDT018-QUOTAGRP,
        QUOTAGRP_DESC TYPE ZSDSSDC013-QUOTAGRP_DESC,
        PORTION_PERC  TYPE ZSDSSDT018-PORTION_PERC,
        ZCRT_DATE     TYPE ZSDSSDT016-ZCRT_DATE,
        ZCRT_TIME     TYPE ZSDSSDT016-ZCRT_TIME,
        ZCRT_USER     TYPE ZSDSSDT016-ZCRT_USER,
        ZUPD_DATE     TYPE ZSDSSDT016-ZUPD_DATE,
        ZUPD_TIME     TYPE ZSDSSDT016-ZUPD_TIME,
        ZUPD_USER     TYPE ZSDSSDT016-ZUPD_USER,
        ZDEL_FLG      TYPE ZSDSSDT018-ZDEL_FLG,
        PORTION_QTY   TYPE ZSDSSDT018-PORTION_QTY,
      END OF TS_QUOTA_DATA .
  types:
    TT_QUOTA_DATA TYPE SORTED TABLE OF TS_QUOTA_DATA
                        WITH UNIQUE KEY MATNR PERIOD QUOTAGRP .
  types:
    BEGIN OF TS_SALES_ORD_DATA,
        MATNR         TYPE VBAP-MATNR,
        PERIOD        TYPE SPMON,
        VKGRP         TYPE VBAK-VKGRP,
        ZZRUNNING     TYPE VBAK-ZZRUNNING,
        QUOTAGRP      TYPE ZSDSSDC013-QUOTAGRP,
        QUOTAGRP_DESC TYPE ZSDSSDC013-QUOTAGRP_DESC,
        VBELN         TYPE VBAK-VBELN,
        POSNR         TYPE VBAP-POSNR,
        ETENR         TYPE VBEP-ETENR,
        EDATU         TYPE VBEP-EDATU,
        PSTYV         TYPE VBAP-PSTYV,
        VBTYP         TYPE VBAK-VBTYP,
        KUNNR         TYPE VBAK-KUNNR,
        VKBUR         TYPE VBAK-VKBUR,
        WERKS         TYPE VBAP-WERKS,
        LGORT         TYPE VBAP-LGORT,
        WMENG         TYPE VBEP-WMENG,
        VRKME         TYPE VBEP-VRKME,
      END OF TS_SALES_ORD_DATA .
  types:
    TT_SALES_ORD_DATA TYPE STANDARD TABLE OF TS_SALES_ORD_DATA
                            WITH DEFAULT KEY .
  types:
    BEGIN OF TS_SUM_QTY,
        MATNR    TYPE VBAP-MATNR,
        PERIOD   TYPE SPMON,
        QUOTAGRP TYPE ZSDSDE_QUOTAGRP,
        TYPE     TYPE CHAR2,
        QTY      TYPE VBEP-WMENG,
        UNIT     TYPE VBEP-VRKME,
      END OF TS_SUM_QTY .
  types:
    TT_SUM_QTY TYPE STANDARD TABLE OF TS_SUM_QTY
                     WITH DEFAULT KEY .
  types:
    BEGIN OF TS_SALES_ORD_QTY,
        PERIOD TYPE SPMON,
        VBELN  TYPE VBAK-VBELN,
        WMENG  TYPE VBEP-WMENG,
        VRKME  TYPE VBEP-VRKME,
      END OF TS_SALES_ORD_QTY .
  types:
    TT_SALES_ORD_QTY TYPE STANDARD TABLE OF TS_SALES_ORD_QTY
                     WITH DEFAULT KEY .

  class-methods GET_BY_KEY_MULTI
    importing
      !IT_KEYTAB type TT_KEYTAB
      !IF_INCL_DEL_FLG type FLAG default SPACE
    exporting
      !ET_QUOTA_PLAN type TT_QUOTA_PLAN .
  class-methods GET_QUOTA_REPORT
    importing
      !IT_FILTER type /IWBEP/T_MGW_SELECT_OPTION
    exporting
      !ET_RESULT type ZSDSSDS053_TT
      !ET_PO_DATA type TT_PO_DATA
      !ET_QUOTATION_DATA type TT_SALES_ORD_DATA
      !ET_SALES_ORD_DATA type TT_SALES_ORD_DATA
      value(ET_DO_DATA) type TT_SALES_ORD_DATA .
  class-methods GET_SALES_ORDER_DATA
    importing
      !IS_PERIOD_SCOPE type TS_PERIOD_SCOPE
      !IT_MATNR type MMPUR_T_MATNR
      !IT_QTGRP type ZQUOTAGRP_R
    exporting
      !ET_SALES_ORD_DATA type TT_SALES_ORD_DATA
    changing
      !CT_SUM_QTY type TT_SUM_QTY optional .
  class-methods GET_QUOTATION_DATA
    importing
      !IS_PERIOD_SCOPE type TS_PERIOD_SCOPE
      !IT_MATNR type MMPUR_T_MATNR
      !IT_QTGRP type ZQUOTAGRP_R
    exporting
      !ET_QUOTATION_DATA type TT_SALES_ORD_DATA
    changing
      !CT_SUM_QTY type TT_SUM_QTY optional .
  class-methods GET_PO_DATA
    importing
      !IS_PERIOD_SCOPE type TS_PERIOD_SCOPE
      !IT_MATNR type MMPUR_T_MATNR
    exporting
      !ET_PO_DATA type TT_PO_DATA
    changing
      !CT_SUM_QTY type TT_SUM_QTY optional .
  class-methods SET_PERIOD_SCOPE
    importing
      !IS_REQDAT type SY-DATUM default SYST-DATUM
    returning
      value(RS_PERIOD_SCOPE) type TS_PERIOD_SCOPE .
  class-methods DETERMINE_QUOTA_GROUP
    importing
      !IF_VKGRP type VKGRP
      !IF_RUNNING type ZSDSDE_SD_RUNNING
    changing
      !CS_QUOTAGRP_INFO type ZSDSSDC013 optional
    returning
      value(RF_QUOTAGRP) type ZSDSDE_QUOTAGRP .
  class-methods GET_REMAIN_QUOTA
    importing
      !IF_EDATU type VBEP-EDATU
      !IF_VKGRP type VBAK-VKGRP
      !IF_ZZRUNNING type VBAK-ZZRUNNING
      !IF_MATNR type VBAP-MATNR
    exporting
      !EF_REMAIN_QUOTA type VBEP-WMENG
      !ES_RETURN type BAPIRET2
    changing
      !CT_SALES_ORD_QTY type TT_SALES_ORD_QTY optional .
  class-methods IS_QUOTA_ACTIVE
    importing
      !IF_MATNR type MATNR
      !IF_DATE type DATUM
      !IF_QTGRP type ZSDSDE_QUOTAGRP
    returning
      value(RF_RESULT) type ABAP_BOOLEAN .
  class-methods GET_DO_DATA
    importing
      !IS_PERIOD_SCOPE type TS_PERIOD_SCOPE
      !IT_MATNR type MMPUR_T_MATNR
      !IT_QTGRP type ZQUOTAGRP_R
    exporting
      !ET_DO_ORD_DATA type TT_SALES_ORD_DATA
    changing
      !CT_SUM_QTY type TT_SUM_QTY optional .
protected section.
private section.

  constants GC_REPID type ZSDSCAC001-REPID value 'ZCL_SDSSD_QUOTA_PLAN_UTIL' ##NO_TEXT.
  constants GC_MSGID type SYMSGID value 'ZSDSSD01' ##NO_TEXT.
  constants GC_WARNING type BAPI_MTYPE value 'W' ##NO_TEXT ##NEEDED.
  constants GC_ERROR type BAPI_MTYPE value 'E' ##NO_TEXT ##NEEDED.
  constants GC_SUCCESS type BAPI_MTYPE value 'S' ##NO_TEXT ##NEEDED.
  constants GC_INFO type BAPI_MTYPE value 'I' ##NO_TEXT ##NEEDED.
  class-data GRT_PLANT type MMPURUI_WERKS_RANGE_TTY .
  class-data GRT_SLOC type WVFI_LGORT_RTTY .
  class-data GRT_SD_ITEM_CAT type CKMCSO_PSTYV_T .

  class-methods GET_CONSTANTS .
  class-methods GET_MATERIAL_DATA
    importing
      !IT_QUOTA_DATA type TT_QUOTA_DATA
    exporting
      !ET_MATERIAL_DATA type TT_MATERIAL_DATA .
  class-methods GET_PLAN_QUOTA_DATA
    importing
      !IS_PERIOD_SCOPE type TS_PERIOD_SCOPE
      !IT_MATNR type MMPUR_T_MATNR
      !IT_QTGRP type ZQUOTAGRP_R
      !IF_INCL_DEL_FLG type FLAG
    exporting
      !ET_QUOTA_DATA type TT_QUOTA_DATA .
  class-methods COLLECT_REPORT_RESULT
    importing
      !IS_PERIOD_SCOPE type TS_PERIOD_SCOPE
      !IT_MATERIAL_DATA type TT_MATERIAL_DATA
      !IT_PO_DATA type TT_PO_DATA   ##NEEDED
      !IT_QUOTATION_DATA type TT_SALES_ORD_DATA   ##NEEDED
      !IT_SALES_ORD_DATA type TT_SALES_ORD_DATA   ##NEEDED
      !IT_QUOTA_DATA type TT_QUOTA_DATA
      !IT_SUM_QTY type TT_SUM_QTY
    exporting
      !ET_RESULT type ZSDSSDS053_TT .
  class-methods READ_SUM_QTY
    importing
      !IT_SUM_QTY type TT_SUM_QTY
      !IF_MATNR type MARA-MATNR
      !IF_PERIOD type SPMON
      !IF_TYPE type CHAR2
      !IF_QTGRP type ZSDSDE_QUOTAGRP
      !IF_MEINS type MARA-MEINS optional
    returning
      value(RF_QTY) type ZSDSDE_PO_QTY .
ENDCLASS.



CLASS ZCL_SDSSD_QUOTA_PLAN_UTIL IMPLEMENTATION.


METHOD COLLECT_REPORT_RESULT.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_QUOTA_PLAN_UTIL / COLLECT_REPORT_RESULT
*  Creation Date      : 16.09.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDR012
*  Description        : Collect result data for Quota Report
*  Purpose            : Collect result data for Quota Report to
*                       structure ZSDSSDS053
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  DATA: LF_TABIX_QUOTA    TYPE SY-TABIX,
        LF_APPEND_FLG     TYPE FLAG,
        LF_QUOTA_CURRENT  TYPE ZSDSDE_QUOTA_QTY,
        LF_QUOTA_NEXT     TYPE ZSDSDE_QUOTA_QTY,
        LF_SUM_SO_CURRENT TYPE ZSDSDE_SO_QTY,
        LF_SUM_SO_NEXT    TYPE ZSDSDE_SO_QTY.

  DATA: LS_RESULT          TYPE ZSDSSDS053,
        LS_RESULT_MATLEVEL TYPE ZSDSSDS053.

  DATA: LV_QTY TYPE P DECIMALS 0.

  FIELD-SYMBOLS: <L_RESULT_NEXT> TYPE ZSDSSDS053.

  CLEAR: ET_RESULT.

  LOOP AT IT_MATERIAL_DATA ASSIGNING FIELD-SYMBOL(<L_MATERIAL_DATA>).
    CLEAR: LS_RESULT, LS_RESULT_MATLEVEL.
    CLEAR: LF_SUM_SO_CURRENT, LF_SUM_SO_NEXT,
           LF_QUOTA_CURRENT, LF_QUOTA_NEXT.

*   Material data
    LS_RESULT-REQDAT = IS_PERIOD_SCOPE-REQDAT.
    LS_RESULT-MATNR = <L_MATERIAL_DATA>-MATNR.
    LS_RESULT-MAKTX = <L_MATERIAL_DATA>-MAKTX.
    LS_RESULT-MEINS = <L_MATERIAL_DATA>-MEINS.
    LS_RESULT-URSTOCK = <L_MATERIAL_DATA>-URSTOCK.

*--------------------------------
*   Assign period data
*--------------------------------
*   PO - Current
    LS_RESULT-00_PERIOD = IS_PERIOD_SCOPE-CURRENT.
    LS_RESULT-00_PO_QTY = READ_SUM_QTY( IT_SUM_QTY = IT_SUM_QTY
                                        IF_MATNR   = LS_RESULT-MATNR
                                        IF_PERIOD  = LS_RESULT-00_PERIOD
                                        IF_TYPE    = 'PO'
                                        IF_QTGRP   = SPACE
                                        IF_MEINS   = LS_RESULT-MEINS ).
*   Total Quantity = URSTOCK + PO Qty
    LS_RESULT-00_TOTAL_QTY = LS_RESULT-URSTOCK + LS_RESULT-00_PO_QTY.

*   PO - Next period
    LS_RESULT-01_PERIOD = IS_PERIOD_SCOPE-NEXT1.
    LS_RESULT-01_PO_QTY = READ_SUM_QTY( IT_SUM_QTY = IT_SUM_QTY
                                        IF_MATNR   = LS_RESULT-MATNR
                                        IF_PERIOD  = LS_RESULT-01_PERIOD
                                        IF_TYPE    = 'PO'
                                        IF_QTGRP   = SPACE
                                        IF_MEINS   = LS_RESULT-MEINS ).

    LS_RESULT_MATLEVEL = LS_RESULT.

*--------------------------------
*   Quota Plan - Current Month
*--------------------------------
    CLEAR: LF_TABIX_QUOTA.
    READ TABLE IT_QUOTA_DATA WITH KEY MATNR = LS_RESULT-MATNR
                                      PERIOD = LS_RESULT-00_PERIOD
                                      BINARY SEARCH
                                      TRANSPORTING NO FIELDS.
    IF SY-SUBRC EQ 0.
      LF_TABIX_QUOTA = SY-TABIX.
    ENDIF.

    LOOP AT IT_QUOTA_DATA ASSIGNING FIELD-SYMBOL(<L_QUOTA>) FROM LF_TABIX_QUOTA.
      IF <L_QUOTA>-MATNR NE LS_RESULT-MATNR OR
         <L_QUOTA>-PERIOD NE LS_RESULT-00_PERIOD.
        EXIT.
      ENDIF.

      LS_RESULT-00_DAY_FIRST = <L_QUOTA>-DAY_FIRST.
      LS_RESULT-00_DAY_LAST = <L_QUOTA>-DAY_LAST.
      LS_RESULT-00_QUOTA_QTY = <L_QUOTA>-QUOTA_QTY.
      LS_RESULT-00_QUOTAGRP = <L_QUOTA>-QUOTAGRP.
      LS_RESULT-00_QUOTAGRP_DESC = <L_QUOTA>-QUOTAGRP_DESC.
      LS_RESULT-00_PORTION = <L_QUOTA>-PORTION_PERC.
      LS_RESULT-00_ZCRT_USER = <L_QUOTA>-ZCRT_USER.
      LS_RESULT-00_ZCRT_DATE = <L_QUOTA>-ZCRT_DATE.
      LS_RESULT-00_ZCRT_TIME = <L_QUOTA>-ZCRT_TIME.
      LS_RESULT-00_ZUPD_USER = <L_QUOTA>-ZUPD_USER.
      LS_RESULT-00_ZUPD_DATE = <L_QUOTA>-ZUPD_DATE.
      LS_RESULT-00_ZUPD_TIME = <L_QUOTA>-ZUPD_TIME.
      LS_RESULT-00_ZDEL_FLG = <L_QUOTA>-ZDEL_FLG.

      LS_RESULT-00_QT_QTY = READ_SUM_QTY( IT_SUM_QTY = IT_SUM_QTY
                                          IF_MATNR   = <L_QUOTA>-MATNR
                                          IF_PERIOD  = <L_QUOTA>-PERIOD
                                          IF_TYPE    = 'QT'
                                          IF_QTGRP   = <L_QUOTA>-QUOTAGRP
                                          IF_MEINS   = LS_RESULT-MEINS ).

      LS_RESULT-00_SO_QTY = READ_SUM_QTY( IT_SUM_QTY = IT_SUM_QTY
                                          IF_MATNR   = <L_QUOTA>-MATNR
                                          IF_PERIOD  = <L_QUOTA>-PERIOD
                                          IF_TYPE    = 'SO'
                                          IF_QTGRP   = <L_QUOTA>-QUOTAGRP
                                          IF_MEINS   = LS_RESULT-MEINS ).

      LS_RESULT-00_DO_QTY = READ_SUM_QTY( IT_SUM_QTY = IT_SUM_QTY
                                          IF_MATNR   = <L_QUOTA>-MATNR
                                          IF_PERIOD  = <L_QUOTA>-PERIOD
                                          IF_TYPE    = 'DO'
                                          IF_QTGRP   = <L_QUOTA>-QUOTAGRP
                                          IF_MEINS   = LS_RESULT-MEINS ).

      LF_QUOTA_CURRENT = <L_QUOTA>-QUOTA_QTY.

*     Sum SO Quantity for all sales group - current period
      LF_SUM_SO_CURRENT = LF_SUM_SO_CURRENT + LS_RESULT-00_SO_QTY.

*     Plan allocated quota = Plan quota stock * Portion
      IF <L_QUOTA>-PORTION_QTY IS NOT INITIAL.
        LS_RESULT-00_ALLOC_QUOTA = <L_QUOTA>-PORTION_QTY.
      ELSE.
        LV_QTY                   = ( LS_RESULT-00_QUOTA_QTY * LS_RESULT-00_PORTION ) / 100.
        LS_RESULT-00_ALLOC_QUOTA = LV_QTY.
      ENDIF.

*     Remain Quota = Plan allocated quota - SO Qty
      LS_RESULT-00_REMAIN_QUOTA = LS_RESULT-00_ALLOC_QUOTA - LS_RESULT-00_SO_QTY.

      APPEND LS_RESULT TO ET_RESULT.
    ENDLOOP.

*--------------------------------
*   Quota Plan - Next Month
*--------------------------------
*   Quota Plan
    CLEAR: LF_TABIX_QUOTA, LF_APPEND_FLG.
    READ TABLE IT_QUOTA_DATA WITH KEY MATNR = LS_RESULT_MATLEVEL-MATNR
                                      PERIOD = LS_RESULT_MATLEVEL-01_PERIOD
                                      BINARY SEARCH
                                      TRANSPORTING NO FIELDS.
    IF SY-SUBRC EQ 0.
      LF_TABIX_QUOTA = SY-TABIX.
    ENDIF.

    LOOP AT IT_QUOTA_DATA ASSIGNING FIELD-SYMBOL(<L_QUOTA_NEXT>) FROM LF_TABIX_QUOTA.
      IF <L_QUOTA_NEXT>-MATNR NE LS_RESULT_MATLEVEL-MATNR OR
         <L_QUOTA_NEXT>-PERIOD NE LS_RESULT_MATLEVEL-01_PERIOD.
        EXIT.
      ENDIF.

      READ TABLE ET_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT_CURRENT>)
                           WITH KEY MATNR = LS_RESULT_MATLEVEL-MATNR
                                    00_QUOTAGRP = <L_QUOTA_NEXT>-QUOTAGRP.
      IF SY-SUBRC EQ 0.
        ASSIGN <L_RESULT_CURRENT> TO <L_RESULT_NEXT>.
      ELSE.
        CLEAR: LS_RESULT.
        LS_RESULT = LS_RESULT_MATLEVEL.
        ASSIGN LS_RESULT TO <L_RESULT_NEXT>.
        LF_APPEND_FLG = ABAP_TRUE.
      ENDIF.

      <L_RESULT_NEXT>-01_DAY_FIRST = <L_QUOTA_NEXT>-DAY_FIRST.
      <L_RESULT_NEXT>-01_DAY_LAST = <L_QUOTA_NEXT>-DAY_LAST.
      <L_RESULT_NEXT>-01_QUOTA_QTY = <L_QUOTA_NEXT>-QUOTA_QTY.
      <L_RESULT_NEXT>-01_QUOTAGRP = <L_QUOTA_NEXT>-QUOTAGRP.
      <L_RESULT_NEXT>-01_QUOTAGRP_DESC = <L_QUOTA_NEXT>-QUOTAGRP_DESC.
      <L_RESULT_NEXT>-01_PORTION = <L_QUOTA_NEXT>-PORTION_PERC.
      <L_RESULT_NEXT>-01_ZCRT_USER = <L_QUOTA_NEXT>-ZCRT_USER.
      <L_RESULT_NEXT>-01_ZCRT_DATE = <L_QUOTA_NEXT>-ZCRT_DATE.
      <L_RESULT_NEXT>-01_ZCRT_TIME = <L_QUOTA_NEXT>-ZCRT_TIME.
      <L_RESULT_NEXT>-01_ZUPD_USER = <L_QUOTA_NEXT>-ZUPD_USER.
      <L_RESULT_NEXT>-01_ZUPD_DATE = <L_QUOTA_NEXT>-ZUPD_DATE.
      <L_RESULT_NEXT>-01_ZUPD_TIME = <L_QUOTA_NEXT>-ZUPD_TIME.
      <L_RESULT_NEXT>-01_ZDEL_FLG = <L_QUOTA_NEXT>-ZDEL_FLG.

      <L_RESULT_NEXT>-01_QT_QTY = READ_SUM_QTY( IT_SUM_QTY = IT_SUM_QTY
                                           IF_MATNR   = <L_QUOTA_NEXT>-MATNR
                                           IF_PERIOD  = <L_QUOTA_NEXT>-PERIOD
                                           IF_TYPE    = 'QT'
                                           IF_QTGRP   = <L_QUOTA_NEXT>-QUOTAGRP
                                           IF_MEINS   = LS_RESULT-MEINS ).

      <L_RESULT_NEXT>-01_SO_QTY = READ_SUM_QTY( IT_SUM_QTY = IT_SUM_QTY
                                           IF_MATNR   = <L_QUOTA_NEXT>-MATNR
                                           IF_PERIOD  = <L_QUOTA_NEXT>-PERIOD
                                           IF_TYPE    = 'SO'
                                           IF_QTGRP   = <L_QUOTA_NEXT>-QUOTAGRP
                                           IF_MEINS   = LS_RESULT-MEINS ).

      <L_RESULT_NEXT>-01_DO_QTY = READ_SUM_QTY( IT_SUM_QTY = IT_SUM_QTY
                                           IF_MATNR   = <L_QUOTA_NEXT>-MATNR
                                           IF_PERIOD  = <L_QUOTA_NEXT>-PERIOD
                                           IF_TYPE    = 'DO'
                                           IF_QTGRP   = <L_QUOTA_NEXT>-QUOTAGRP
                                           IF_MEINS   = LS_RESULT-MEINS ).

      LF_QUOTA_NEXT = <L_QUOTA_NEXT>-QUOTA_QTY.

*     Sum SO Quantity for all sales group - next period
      LF_SUM_SO_NEXT = LF_SUM_SO_NEXT + <L_RESULT_NEXT>-01_SO_QTY. "LS_RESULT-01_SO_QTY.

*     Plan allocated quota = Plan quota stock * Portion
      IF <L_QUOTA_NEXT>-PORTION_QTY IS NOT INITIAL.
        <L_RESULT_NEXT>-01_ALLOC_QUOTA = <L_QUOTA_NEXT>-PORTION_QTY.
      ELSE.
        LV_QTY                         = ( <L_RESULT_NEXT>-01_QUOTA_QTY * <L_RESULT_NEXT>-01_PORTION ) / 100.
        <L_RESULT_NEXT>-01_ALLOC_QUOTA = LV_QTY.
      ENDIF.

*     Remain Quota = Plan allocated quota - SO Qty
      <L_RESULT_NEXT>-01_REMAIN_QUOTA = <L_RESULT_NEXT>-01_ALLOC_QUOTA - <L_RESULT_NEXT>-01_SO_QTY.

      IF LF_APPEND_FLG EQ ABAP_TRUE.
        APPEND <L_RESULT_NEXT> TO ET_RESULT.
      ENDIF.
    ENDLOOP.

*    Plan Free Stock = Total Stock - Plan Quota Stock – SO Qty.
*                    = 0x_TOTAL_QTY - 0x_QUOTA_QTY - 0x_SO_QTY
    LS_RESULT_MATLEVEL-00_FREE_QTY = LS_RESULT-00_TOTAL_QTY -
                                     LF_QUOTA_CURRENT.

*   Total Quantity = PO Qty Next Month + 00_FREE_QTY
    LS_RESULT_MATLEVEL-01_TOTAL_QTY = LS_RESULT-01_PO_QTY +
                                      LS_RESULT_MATLEVEL-00_FREE_QTY.

*   Plan Free Stock = Total Quantity - Plan Quota Stock – SO Qty.
    LS_RESULT_MATLEVEL-01_FREE_QTY = LS_RESULT_MATLEVEL-01_TOTAL_QTY -
                                     LF_QUOTA_NEXT.

    MODIFY ET_RESULT FROM LS_RESULT_MATLEVEL
                     TRANSPORTING 00_FREE_QTY
                                  01_FREE_QTY
                                  01_TOTAL_QTY
                     WHERE MATNR EQ <L_MATERIAL_DATA>-MATNR.

  ENDLOOP.

ENDMETHOD.


METHOD DETERMINE_QUOTA_GROUP.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_QUOTA_PLAN_UTIL / DETERMINE_QUOTA_GROUP
*  Creation Date      : 30.09.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDR012
*  Description        : Determine Quota Group from Sales Group and Running
*  Purpose            : Determine Quota Group from Sales Group and Running
*                       by reading mapping table ZSDSSDC013
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

* Initialize Output
  CLEAR: RF_QUOTAGRP, CS_QUOTAGRP_INFO.

  IF IF_VKGRP IS INITIAL OR
     IF_RUNNING IS INITIAL.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM ZSDSSDC013
    INTO @DATA(LS_ZSDSSDC013)
    WHERE VKGRP EQ @IF_VKGRP
      AND RUNNING EQ @IF_RUNNING
      AND ZDEL_FLG EQ @SPACE.
  IF SY-SUBRC EQ 0.
    RF_QUOTAGRP = LS_ZSDSSDC013-QUOTAGRP.
  ENDIF.

  IF CS_QUOTAGRP_INFO IS SUPPLIED.
    CS_QUOTAGRP_INFO = LS_ZSDSSDC013.
  ENDIF.

ENDMETHOD.


METHOD GET_BY_KEY_MULTI.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_QUOTA_PLAN_UTIL / GET_BY_KEY_MULTI
*  Creation Date      : 01.08.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDE029
*  Description        : Get Quota Plan List by KEYTAB
*  Purpose            : Get Quota Plan List from tables
*                        1. ZSDSSDT016: Quota Plan Header
*                        2. ZSDSSDT018: Quota Plan Item
*                       Using criteria in IT_KEYTAB
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  DATA: LT_DEL_FLG    TYPE RANGE OF ZSDSSDT018-ZDEL_FLG,
        LS_QUOTA_PLAN TYPE TS_QUOTA_PLAN,
        LS_ITEM       TYPE TS_QUOTA_PLAN_ITM.

  CLEAR: ET_QUOTA_PLAN.
  IF IT_KEYTAB IS INITIAL.
    RETURN.
  ENDIF.

* Include records with Delete Flag -> Get Delete Flag = SPACE or X
* Otherwise, Get only Delete Flag = SPACE
  IF IF_INCL_DEL_FLG EQ ABAP_FALSE.
    APPEND VALUE #( SIGN = 'I'
                    OPTION = 'EQ'
                    LOW = SPACE )
             TO LT_DEL_FLG .
  ENDIF.

* Quota Plan Header
  SELECT *
    FROM ZSDSSDT016
    INTO TABLE @DATA(LT_HDR)
    FOR ALL ENTRIES IN @IT_KEYTAB
    WHERE MATNR EQ @IT_KEYTAB-MATNR
      AND DAY_FIRST LE @IT_KEYTAB-DAY_LAST
      AND DAY_LAST GE @IT_KEYTAB-DAY_FIRST
      AND ZDEL_FLG IN @LT_DEL_FLG
    ORDER BY PRIMARY KEY.

* Quota Plan Item
  IF LT_HDR IS NOT INITIAL.
    SELECT *
    FROM ZSDSSDT018
    INTO TABLE @DATA(LT_ITM)
    FOR ALL ENTRIES IN @IT_KEYTAB
    WHERE MATNR EQ @IT_KEYTAB-MATNR
      AND DAY_FIRST EQ @IT_KEYTAB-DAY_FIRST
      AND DAY_LAST EQ @IT_KEYTAB-DAY_LAST
      AND ZDEL_FLG IN @LT_DEL_FLG
    ORDER BY PRIMARY KEY.
  ENDIF.

* Fill in result table
  LOOP AT LT_HDR ASSIGNING FIELD-SYMBOL(<L_HDR_DB>).
    CLEAR: LS_QUOTA_PLAN, LS_ITEM.

    MOVE-CORRESPONDING <L_HDR_DB> TO LS_QUOTA_PLAN.

    LOOP AT LT_ITM ASSIGNING FIELD-SYMBOL(<L_ITM_DB>)
                   WHERE MATNR EQ <L_HDR_DB>-MATNR
                     AND DAY_FIRST EQ <L_HDR_DB>-DAY_FIRST
                     AND DAY_LAST EQ <L_HDR_DB>-DAY_LAST.
      CLEAR: LS_ITEM.
      MOVE-CORRESPONDING <L_ITM_DB> TO LS_ITEM.
      INSERT LS_ITEM INTO TABLE LS_QUOTA_PLAN-ITEM.
    ENDLOOP.

    INSERT LS_QUOTA_PLAN INTO TABLE ET_QUOTA_PLAN.

  ENDLOOP.

ENDMETHOD.


METHOD GET_CONSTANTS.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_QUOTA_PLAN_UTIL / GET_CONSTANTS
*  Creation Date      : 16.09.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDR012
*  Description        : To get GENC constant from table ZSDSCAC001
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  CONSTANTS:
    LC_PLANT            TYPE ZSDSDE_PARAM_NAME VALUE 'PLANT',
    LC_STORAGE_LOCATION TYPE ZSDSDE_PARAM_NAME VALUE 'STORAGE_LOCATION',
    LC_SD_ITEM_CAT      TYPE ZSDSDE_PARAM_NAME VALUE 'SD_ITEM_CAT'.

  DATA: LT_GENC TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  CLEAR: GRT_PLANT,
         GRT_SLOC,
         GRT_SD_ITEM_CAT.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = GC_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).
    CASE <L_GENC>-PARAM.
*     PLANT
      WHEN LC_PLANT.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_PLANT.
*     STORAGE_LOCATION
      WHEN LC_STORAGE_LOCATION .
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_SLOC.
*     SD_ITEM_CAT
      WHEN LC_SD_ITEM_CAT.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_SD_ITEM_CAT.
    ENDCASE.
  ENDLOOP.

ENDMETHOD.


  METHOD GET_DO_DATA.
    DATA: LS_SUM_QTY TYPE TS_SUM_QTY.

    DATA: LS_DATA TYPE TS_SALES_ORD_DATA.

    CLEAR: ET_DO_ORD_DATA.

    SELECT VBAP~MATNR,
           VBEP~EDATU AS PERIOD,
           VBAK~VKGRP,
           VBAK~ZZRUNNING,
           QTGRP_MAP~QUOTAGRP,
           QTGRP_MAP~QUOTAGRP_DESC,
           LIPS~VBELN,
           LIPS~POSNR,
           VBEP~ETENR,
           VBEP~EDATU,
           VBAK~VBTYP,
           VBAK~KUNNR,
           VBAK~VKBUR,
           VBAP~PSTYV,
           VBAP~WERKS,
           VBAP~LGORT,
           VBEP~WMENG,
           VBEP~VRKME
       FROM VBAP
         INNER JOIN LIPS ON VBAP~VBELN EQ LIPS~VGBEL AND
                            VBAP~POSNR EQ LIPS~VGPOS
         INNER JOIN VBEP
         ON ( VBAP~VBELN EQ VBEP~VBELN AND
              VBAP~POSNR EQ VBEP~POSNR )
         INNER JOIN VBAK
         ON ( VBAP~VBELN EQ VBAK~VBELN )
         INNER JOIN ZSDSSDC013 AS QTGRP_MAP
         ON ( VBAK~VKGRP EQ QTGRP_MAP~VKGRP AND
              VBAK~ZZRUNNING EQ QTGRP_MAP~RUNNING )
       INTO TABLE @DATA(LT_SO)
       WHERE VBAK~VBTYP EQ 'C'                "Order
         AND VBAP~PSTYV IN @GRT_SD_ITEM_CAT
         AND VBAP~MATNR IN @IT_MATNR
         AND QTGRP_MAP~QUOTAGRP IN @IT_QTGRP
         AND VBEP~WMENG NE 0
         AND ( VBEP~EDATU BETWEEN @IS_PERIOD_SCOPE-CURRENT_BEGIN
                          AND @IS_PERIOD_SCOPE-NEXT1_END ).

    IF SY-SUBRC EQ 0.
      DATA: LT_SUM LIKE LT_SO.
      APPEND LINES OF LT_SO TO LT_SUM.
*    ET_SALES_ORD_DATA[] = CORRESPONDING #( LT_SO ).
*
*    SORT ET_SALES_ORD_DATA BY MATNR PERIOD VBELN POSNR ETENR.
    ENDIF.


    SELECT VBAP~MATNR,
         VBEP~EDATU AS PERIOD,
         VBAK~VKGRP,
         VBAK~ZZRUNNING,
         QTGRP_MAP~QUOTAGRP,
         QTGRP_MAP~QUOTAGRP_DESC,
         LIPS~VBELN,
         LIPS~POSNR,
         VBEP~ETENR,
         VBEP~EDATU,
         VBAK~VBTYP,
         VBAK~KUNNR,
         VBAK~VKBUR,
         VBAP~PSTYV,
         VBAP~WERKS,
         VBAP~LGORT,
         VBEP~WMENG,
         VBEP~VRKME
     FROM VBAP
       INNER JOIN LIPS ON VBAP~VBELN EQ LIPS~VGBEL AND
                          VBAP~POSNR EQ LIPS~VGPOS
       INNER JOIN VBEP
       ON ( VBAP~VBELN EQ VBEP~VBELN AND
            VBAP~POSNR EQ VBEP~POSNR )
       INNER JOIN VBAK
       ON ( VBAP~VBELN EQ VBAK~VBELN )
       INNER JOIN ZSDSSDC013 AS QTGRP_MAP
       ON ( VBAK~VKGRP        EQ QTGRP_MAP~VKGRP AND
            QTGRP_MAP~RUNNING EQ '0')
       INNER JOIN ZSDSSDC029
       ON VBAK~VBELN         EQ ZSDSSDC029~VBELN AND
          ZSDSSDC029~RUNNING EQ '1'
     INTO TABLE @DATA(LT_SO_TMP)
     WHERE VBAK~VBTYP EQ 'C'                "Order
       AND VBAP~PSTYV IN @GRT_SD_ITEM_CAT
       AND VBAP~MATNR IN @IT_MATNR
       AND QTGRP_MAP~QUOTAGRP IN @IT_QTGRP
       AND VBEP~WMENG NE 0
       AND ( VBEP~EDATU BETWEEN @IS_PERIOD_SCOPE-CURRENT_BEGIN
                        AND @IS_PERIOD_SCOPE-NEXT1_END ).
    IF LT_SO_TMP IS NOT INITIAL.
      APPEND LINES OF LT_SO_TMP TO LT_SUM.
    ENDIF.

    IF LT_SUM IS NOT INITIAL.
      ET_DO_ORD_DATA[] = CORRESPONDING #( LT_SUM ).
      SORT ET_DO_ORD_DATA BY MATNR PERIOD VBELN POSNR ETENR.
    ENDIF.

*  IF SY-SUBRC EQ 0.
*    ET_SALES_ORD_DATA[] = CORRESPONDING #( LT_SO ).
*
*    SORT ET_SALES_ORD_DATA BY MATNR PERIOD VBELN POSNR ETENR.
*  ENDIF.

    IF CT_SUM_QTY IS SUPPLIED.
*   Prepare summary quantity by MATNR, PERIOD, VKGRP
      LOOP AT LT_SO ASSIGNING FIELD-SYMBOL(<L_SO>).
        MOVE-CORRESPONDING <L_SO> TO LS_DATA.
        APPEND LS_DATA TO ET_DO_ORD_DATA.
        READ TABLE LT_SO_TMP
        WITH KEY VBELN = <L_SO>-VBELN TRANSPORTING NO FIELDS.
        IF SY-SUBRC EQ 0.
          DELETE LT_SO_TMP INDEX SY-TABIX.
        ENDIF.

        CLEAR: LS_SUM_QTY.
        LS_SUM_QTY-MATNR    = <L_SO>-MATNR.
        LS_SUM_QTY-PERIOD   = <L_SO>-PERIOD.
        LS_SUM_QTY-QUOTAGRP = <L_SO>-QUOTAGRP.
        LS_SUM_QTY-TYPE     = 'DO'.
        LS_SUM_QTY-QTY      = <L_SO>-WMENG.
        LS_SUM_QTY-UNIT     = <L_SO>-VRKME.

        COLLECT LS_SUM_QTY INTO CT_SUM_QTY.
      ENDLOOP.

      LOOP AT LT_SO_TMP ASSIGNING <L_SO>.
        MOVE-CORRESPONDING <L_SO> TO LS_DATA.
        APPEND LS_DATA TO ET_DO_ORD_DATA.
        CLEAR: LS_SUM_QTY.
        LS_SUM_QTY-MATNR     = <L_SO>-MATNR.
        LS_SUM_QTY-PERIOD    = <L_SO>-PERIOD.
        LS_SUM_QTY-QUOTAGRP  = <L_SO>-QUOTAGRP.
        LS_SUM_QTY-TYPE      = 'DO'.
        LS_SUM_QTY-QTY       = <L_SO>-WMENG.
        LS_SUM_QTY-UNIT      = <L_SO>-VRKME.
        COLLECT LS_SUM_QTY INTO CT_SUM_QTY.
      ENDLOOP.

      SORT CT_SUM_QTY BY MATNR PERIOD TYPE QUOTAGRP.
    ENDIF.

    IF ET_DO_ORD_DATA IS NOT INITIAL.
      SORT ET_DO_ORD_DATA BY MATNR PERIOD VBELN POSNR ETENR.
    ENDIF.
  ENDMETHOD.


METHOD GET_MATERIAL_DATA.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_QUOTA_PLAN_UTIL / GET_MATERIAL_INFO
*  Creation Date      : 16.09.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDR012
*  Description        : Get material master data and current stock quantity
*  Purpose            : Get material data from MARA and MAKT
*                       and get current stock quantity from MARD
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  DATA: LT_MATNR      TYPE MMPUR_T_MATNR,
        LT_QUOTA_DATA TYPE TT_QUOTA_DATA.

  CLEAR: ET_MATERIAL_DATA.

  IF IT_QUOTA_DATA IS INITIAL.
    RETURN.
  ENDIF.

  LT_QUOTA_DATA[] = IT_QUOTA_DATA[].
  DELETE ADJACENT DUPLICATES FROM LT_QUOTA_DATA COMPARING MATNR.

  CLEAR: LT_MATNR.
  LOOP AT LT_QUOTA_DATA ASSIGNING FIELD-SYMBOL(<L_QUOTA_DATA>).
    INSERT VALUE #( SIGN   = 'I'
                    OPTION = 'EQ'
                    LOW    = <L_QUOTA_DATA>-MATNR )
           INTO TABLE LT_MATNR.
  ENDLOOP.

  IF LT_MATNR IS NOT INITIAL.
* Get material master data and material description
    SELECT MARA~MATNR,
           MARA~MEINS,
           MAKT~MAKTX
      FROM MARA
      LEFT OUTER JOIN MAKT
        ON ( MARA~MATNR EQ MAKT~MATNR )
      WHERE MARA~MATNR IN @LT_MATNR
        AND MAKT~SPRAS EQ @SY-LANGU
      INTO TABLE @DATA(LT_MARA).

* Get current stock quantity
    SELECT MATNR,
           SUM( LABST ) AS URSTOCK
      FROM MARD
      INTO TABLE @DATA(LT_MARD)
      WHERE MATNR IN @LT_MATNR
        AND WERKS IN @GRT_PLANT
        AND LGORT IN @GRT_SLOC
      GROUP BY MATNR
    ORDER BY MATNR.

    ET_MATERIAL_DATA[] = CORRESPONDING #( LT_MARA ).

    LOOP AT ET_MATERIAL_DATA ASSIGNING FIELD-SYMBOL(<L_MATERIAL_DATA>).
      READ TABLE LT_MARD ASSIGNING FIELD-SYMBOL(<L_MARD>)
                         WITH KEY MATNR = <L_MATERIAL_DATA>-MATNR
                         BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        <L_MATERIAL_DATA>-URSTOCK = <L_MARD>-URSTOCK.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


METHOD GET_PLAN_QUOTA_DATA.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_QUOTA_PLAN_UTIL / GET_PLAN_QUOTA_DATA
*  Creation Date      : 16.09.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDR012
*  Description        : Get Quota Plan Table
*  Purpose            : Related tables for Quota Plan
*                       1. ZSDSSDT016: Quota Plan Header
*                       2. ZSDSSDT018: Quota Plan Item
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  TYPES: BEGIN OF LS_QUOTAGRP,
           QUOTAGRP      TYPE ZSDSSDC013-QUOTAGRP,
           QUOTAGRP_DESC TYPE ZSDSSDC013-QUOTAGRP_DESC,
         END OF LS_QUOTAGRP.
  DATA: LT_QUOTAGRP TYPE SORTED TABLE OF LS_QUOTAGRP
                    WITH NON-UNIQUE KEY QUOTAGRP.
  DATA: LT_RANGE_DELFLG TYPE RANGE OF FLAG.

  CLEAR: ET_QUOTA_DATA.

*-------------------------------
* Prepare value for ZDEL_FLG
*-------------------------------
* If Include records with delete flag = 'X', then get all records
* Otherwise, remove the records which delete flag on
  IF IF_INCL_DEL_FLG EQ 'X'.
    CLEAR: LT_RANGE_DELFLG.
  ELSE.
    INSERT VALUE #( SIGN   = 'I'
                    OPTION = 'EQ'
                    LOW    = SPACE
                    HIGH   = SPACE )
           INTO TABLE LT_RANGE_DELFLG.
  ENDIF.

*-----------------------
* Current month
*-----------------------
  SELECT HDR~DAY_FIRST AS PERIOD,
         HDR~MATNR,
         HDR~DAY_FIRST,
         HDR~DAY_LAST,
         HDR~QUOTA_QTY,
         ITM~QUOTAGRP,
         ITM~PORTION_PERC,
         HDR~ZCRT_DATE,
         HDR~ZCRT_TIME,
         HDR~ZCRT_USER,
         HDR~ZUPD_DATE,
         HDR~ZUPD_TIME,
         HDR~ZUPD_USER,
         ITM~ZDEL_FLG,
         ITM~PORTION_QTY
    FROM ZSDSSDT016 AS HDR
    INNER JOIN ZSDSSDT018 AS ITM
    ON ( HDR~MATNR EQ ITM~MATNR AND
         HDR~DAY_FIRST EQ ITM~DAY_FIRST AND
         HDR~DAY_LAST EQ ITM~DAY_LAST )
    INTO TABLE @DATA(LT_QUOTA)
    WHERE HDR~MATNR IN @IT_MATNR
      AND HDR~DAY_FIRST LE @IS_PERIOD_SCOPE-NEXT1_END
      AND HDR~DAY_LAST GE @IS_PERIOD_SCOPE-CURRENT_BEGIN
      AND ITM~QUOTAGRP IN @IT_QTGRP
      AND ITM~ZDEL_FLG IN @LT_RANGE_DELFLG.

*-----------------------
* Fill in result table
*-----------------------
  MOVE-CORRESPONDING LT_QUOTA TO ET_QUOTA_DATA.

* Get mapping table for quota group and sales group (ZSDSSDC013) to get
* to get quota group description
  IF ET_QUOTA_DATA IS NOT INITIAL.
    SELECT QUOTAGRP,
           QUOTAGRP_DESC
      FROM ZSDSSDC013
      INTO TABLE @LT_QUOTAGRP
      FOR ALL ENTRIES IN @ET_QUOTA_DATA
      WHERE QUOTAGRP EQ @ET_QUOTA_DATA-QUOTAGRP.

    IF SY-SUBRC EQ 0.
      LOOP AT ET_QUOTA_DATA ASSIGNING FIELD-SYMBOL(<L_QUOTA_DATA>).
        READ TABLE LT_QUOTAGRP ASSIGNING FIELD-SYMBOL(<L_QUOTAGRP>)
                               WITH KEY QUOTAGRP = <L_QUOTA_DATA>-QUOTAGRP
                               BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          <L_QUOTA_DATA>-QUOTAGRP_DESC = <L_QUOTAGRP>-QUOTAGRP_DESC.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD GET_PO_DATA.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_QUOTA_PLAN_UTIL / GET_PO_DATA
*  Creation Date      : 16.09.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDR012
*  Description        : Get purchase order data and schedule line data
*  Purpose            : Get purchase order data from EKPO and EKET
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  DATA: LS_SUM_QTY      TYPE TS_SUM_QTY,
        LS_PERIOD_SCOPE TYPE TS_PERIOD_SCOPE.

  CLEAR: ET_PO_DATA.

  LS_PERIOD_SCOPE = IS_PERIOD_SCOPE.

* For PO Data get from system date to end of next month
  LS_PERIOD_SCOPE-CURRENT_BEGIN = SY-DATUM.

  SELECT EKPO~MATNR,
         EKET~EINDT AS PERIOD,
         EKPO~EBELN,
         EKPO~EBELP,
         EKET~ETENR,
         EKET~EINDT,
         EKPO~WERKS,
         EKPO~LGORT,
         EKET~MENGE,
         EKPO~MEINS
    FROM EKPO
    INNER JOIN EKET
      ON ( EKPO~EBELN EQ EKET~EBELN AND
           EKPO~EBELP EQ EKET~EBELP )
    INTO TABLE @DATA(LT_PO)
    WHERE MATNR IN @IT_MATNR
      AND EINDT BETWEEN @LS_PERIOD_SCOPE-CURRENT_BEGIN
                AND @LS_PERIOD_SCOPE-NEXT1_END.

  IF SY-SUBRC EQ 0.
    ET_PO_DATA[] = CORRESPONDING #( LT_PO ).

    SORT ET_PO_DATA BY MATNR PERIOD EBELN EBELP ETENR.
  ENDIF.

  IF CT_SUM_QTY IS SUPPLIED.
*   Prepare summary quantity by MATNR, PERIOD
    LOOP AT LT_PO ASSIGNING FIELD-SYMBOL(<L_PO>).
      CLEAR: LS_SUM_QTY.
      LS_SUM_QTY-MATNR = <L_PO>-MATNR.
      LS_SUM_QTY-PERIOD = <L_PO>-PERIOD.
      LS_SUM_QTY-TYPE = 'PO'.
      LS_SUM_QTY-QTY = <L_PO>-MENGE.
      LS_SUM_QTY-UNIT = <L_PO>-MEINS.

      COLLECT LS_SUM_QTY INTO CT_SUM_QTY.
    ENDLOOP.

    SORT CT_SUM_QTY BY MATNR PERIOD TYPE QUOTAGRP.
  ENDIF.

ENDMETHOD.


METHOD GET_QUOTATION_DATA.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_QUOTA_PLAN_UTIL / GET_QUOTATION_DATA
*  Creation Date      : 16.09.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDR012
*  Description        : Get quotation data and schedule line data
*  Purpose            : Get quotation data and schedule line data
*                       for the specified material, sales group and period
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  DATA: LS_SUM_QTY TYPE TS_SUM_QTY.

  DATA: LS_DATA TYPE TS_SALES_ORD_DATA.

  CLEAR: ET_QUOTATION_DATA.

  SELECT VBAP~MATNR,
         VBEP~EDATU AS PERIOD,
         VBAK~VKGRP,
         VBAK~ZZRUNNING,
         QTGRP_MAP~QUOTAGRP,
         QTGRP_MAP~QUOTAGRP_DESC,
         VBAK~VBELN,
         VBAP~POSNR,
         VBEP~ETENR,
         VBEP~EDATU,
         VBAK~VBTYP,
         VBAK~KUNNR,
         VBAK~VKBUR,
         VBAP~PSTYV,
         VBAP~WERKS,
         VBAP~LGORT,
         VBEP~WMENG,
         VBEP~VRKME
     FROM VBAP
       INNER JOIN VBEP
       ON ( VBAP~VBELN EQ VBEP~VBELN AND
            VBAP~POSNR EQ VBEP~POSNR )
       INNER JOIN VBAK
       ON ( VBAP~VBELN EQ VBAK~VBELN )
       INNER JOIN ZSDSSDC013 AS QTGRP_MAP
       ON ( VBAK~VKGRP EQ QTGRP_MAP~VKGRP AND
            VBAK~ZZRUNNING EQ QTGRP_MAP~RUNNING )
     INTO TABLE @DATA(LT_QT)
     WHERE VBAK~VBTYP EQ 'B'     "Quotation
       AND VBAP~RFSTA EQ 'A'     "Document status = A (Not yet processed)
       AND VBAP~PSTYV IN @GRT_SD_ITEM_CAT
       AND VBAP~MATNR IN @IT_MATNR
       AND QTGRP_MAP~QUOTAGRP IN @IT_QTGRP
       AND VBEP~WMENG NE 0
       AND ( VBEP~EDATU BETWEEN @IS_PERIOD_SCOPE-CURRENT_BEGIN
                        AND @IS_PERIOD_SCOPE-NEXT1_END )
       AND NOT EXISTS ( SELECT VBELN
                          FROM ZSDSSDC029 AS ZZ
                         WHERE ZZ~VBELN   EQ VBAK~VBELN
                           AND ZZ~RUNNING EQ '1').

  IF LT_QT IS NOT INITIAL.
    DATA LT_SUM LIKE LT_QT.
    APPEND LINES OF LT_QT TO LT_SUM.
  ENDIF.

  SELECT VBAP~MATNR,
       VBEP~EDATU AS PERIOD,
       VBAK~VKGRP,
       VBAK~ZZRUNNING,
       QTGRP_MAP~QUOTAGRP,
       QTGRP_MAP~QUOTAGRP_DESC,
       VBAK~VBELN,
       VBAP~POSNR,
       VBEP~ETENR,
       VBEP~EDATU,
       VBAK~VBTYP,
       VBAK~KUNNR,
       VBAK~VKBUR,
       VBAP~PSTYV,
       VBAP~WERKS,
       VBAP~LGORT,
       VBEP~WMENG,
       VBEP~VRKME
   FROM VBAP
     INNER JOIN VBEP
     ON ( VBAP~VBELN EQ VBEP~VBELN AND
          VBAP~POSNR EQ VBEP~POSNR )
     INNER JOIN VBAK
     ON ( VBAP~VBELN EQ VBAK~VBELN )
     INNER JOIN ZSDSSDC013 AS QTGRP_MAP
     ON ( VBAK~VKGRP        EQ QTGRP_MAP~VKGRP AND
          QTGRP_MAP~RUNNING EQ '0' )
     INNER JOIN ZSDSSDC029
     ON VBAK~VBELN         EQ ZSDSSDC029~VBELN AND
        ZSDSSDC029~RUNNING EQ '1'
   INTO TABLE @DATA(LT_QT_TMP)
   WHERE VBAK~VBTYP EQ 'B'     "Quotation
     AND VBAP~RFSTA EQ 'A'     "Document status = A (Not yet processed)
     AND VBAP~PSTYV IN @GRT_SD_ITEM_CAT
     AND VBAP~MATNR IN @IT_MATNR
     AND QTGRP_MAP~QUOTAGRP IN @IT_QTGRP
     AND VBEP~WMENG NE 0
     AND ( VBEP~EDATU BETWEEN @IS_PERIOD_SCOPE-CURRENT_BEGIN
                      AND @IS_PERIOD_SCOPE-NEXT1_END ).

  IF LT_QT_TMP IS NOT INITIAL.
    LOOP AT LT_QT_TMP INTO DATA(LS_QT_TMP).
      LS_QT_TMP-ZZRUNNING = '0'.
      APPEND LS_QT_TMP TO LT_SUM.
    ENDLOOP.
  ENDIF.

  IF LT_SUM IS NOT INITIAL.
    ET_QUOTATION_DATA[] = CORRESPONDING #( LT_SUM ).
    SORT ET_QUOTATION_DATA BY MATNR PERIOD VBELN POSNR ETENR.
  ENDIF.

*  IF SY-SUBRC EQ 0.
*    ET_QUOTATION_DATA[] = CORRESPONDING #( LT_QT ).
*
*    SORT ET_QUOTATION_DATA BY MATNR PERIOD VBELN POSNR ETENR.
*  ENDIF.

  IF CT_SUM_QTY IS SUPPLIED.
*   Prepare summary quantity by MATNR, PERIOD, VKGRP
    LOOP AT LT_QT ASSIGNING FIELD-SYMBOL(<L_QT>).
      MOVE-CORRESPONDING <L_QT> TO LS_DATA.
      APPEND LS_DATA TO ET_QUOTATION_DATA.
      READ TABLE LT_QT_TMP
      WITH KEY VBELN = <L_QT>-VBELN TRANSPORTING NO FIELDS.
      IF SY-SUBRC EQ 0.
        DELETE LT_QT_TMP INDEX SY-TABIX.
      ENDIF.
      CLEAR: LS_SUM_QTY.
      LS_SUM_QTY-MATNR = <L_QT>-MATNR.
      LS_SUM_QTY-PERIOD = <L_QT>-PERIOD.
      LS_SUM_QTY-QUOTAGRP = <L_QT>-QUOTAGRP.
      LS_SUM_QTY-TYPE = 'QT'.
      LS_SUM_QTY-QTY = <L_QT>-WMENG.
      LS_SUM_QTY-UNIT = <L_QT>-VRKME.

      COLLECT LS_SUM_QTY INTO CT_SUM_QTY.
    ENDLOOP.

    LOOP AT LT_QT_TMP ASSIGNING <L_QT>.
      MOVE-CORRESPONDING <L_QT> TO LS_DATA.
      APPEND LS_DATA TO ET_QUOTATION_DATA.
      CLEAR: LS_SUM_QTY.
      LS_SUM_QTY-MATNR = <L_QT>-MATNR.
      LS_SUM_QTY-PERIOD = <L_QT>-PERIOD.
      LS_SUM_QTY-QUOTAGRP = <L_QT>-QUOTAGRP.
      LS_SUM_QTY-TYPE = 'QT'.
      LS_SUM_QTY-QTY = <L_QT>-WMENG.
      LS_SUM_QTY-UNIT = <L_QT>-VRKME.
      COLLECT LS_SUM_QTY INTO CT_SUM_QTY.
    ENDLOOP.

    IF ET_QUOTATION_DATA IS NOT INITIAL.
      SORT ET_QUOTATION_DATA BY MATNR PERIOD VBELN POSNR ETENR.
    ENDIF.

    SORT CT_SUM_QTY BY MATNR PERIOD TYPE QUOTAGRP.
  ENDIF.

ENDMETHOD.


METHOD GET_QUOTA_REPORT.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_QUOTA_PLAN_UTIL / GET_QUOTA_REPORT
*  Creation Date      : 16.09.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDR012
*  Description        : Get Quota Plan Detail for Reporting
*  Purpose            : Get Quota Plan Detail for Reporting
*                       This method will be called from ZSDSSDR0290
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  DATA:
    LT_REQDAT         TYPE RANGE OF SY-DATUM,
    LT_MATNR          TYPE RANGE OF MARA-MATNR,
    LT_QTGRP          TYPE ZQUOTAGRP_R,
    LT_INCDEL         TYPE RANGE OF ABAP_BOOL,
    LT_MATERIAL_DATA  TYPE TT_MATERIAL_DATA,
    LT_QUOTA_DATA     TYPE TT_QUOTA_DATA,
    LT_PO_DATA        TYPE TT_PO_DATA,
    LT_SALES_ORD_DATA TYPE TT_SALES_ORD_DATA,
    LT_QUOTATION_DATA TYPE TT_SALES_ORD_DATA,
    LT_DO_DATA        TYPE TT_SALES_ORD_DATA,
    LT_SUM_QTY        TYPE TT_SUM_QTY.

  DATA:
     LS_PERIOD_SCOPE  TYPE TS_PERIOD_SCOPE.

  DATA: LF_INCDEL_FLG TYPE FLAG.

* Initialize Output
  CLEAR: ET_RESULT,
         ET_PO_DATA,
         ET_QUOTATION_DATA,
         ET_SALES_ORD_DATA.

* Get Filter/Condition data
  LT_REQDAT = CORRESPONDING #( VALUE #( IT_FILTER[ PROPERTY = 'REQDAT' ]-SELECT_OPTIONS OPTIONAL ) ).
  LT_MATNR  = CORRESPONDING #( VALUE #( IT_FILTER[ PROPERTY = 'MATNR' ]-SELECT_OPTIONS OPTIONAL ) ).
  LT_QTGRP  = CORRESPONDING #( VALUE #( IT_FILTER[ PROPERTY = 'QTGRP' ]-SELECT_OPTIONS OPTIONAL ) ).
  LT_INCDEL = CORRESPONDING #( VALUE #( IT_FILTER[ PROPERTY = 'INCDEL' ]-SELECT_OPTIONS OPTIONAL ) ).
  READ TABLE LT_INCDEL INDEX 1 INTO DATA(LS_INCDEL).
  IF SY-SUBRC EQ 0.
    LF_INCDEL_FLG = LS_INCDEL-LOW.
  ELSE.
    LF_INCDEL_FLG = SPACE.
  ENDIF.

* Get GenC Setting for class
  GET_CONSTANTS( ).

* Set period scope of getting data
  READ TABLE LT_REQDAT INDEX 1 INTO DATA(LS_REQDAT).
  IF SY-SUBRC EQ 0.
    LS_PERIOD_SCOPE = SET_PERIOD_SCOPE( IS_REQDAT = LS_REQDAT-LOW ).
  ELSE.
    LS_PERIOD_SCOPE = SET_PERIOD_SCOPE( IS_REQDAT = SY-DATUM ).
  ENDIF.

* Get plan quota
  GET_PLAN_QUOTA_DATA( EXPORTING IS_PERIOD_SCOPE = LS_PERIOD_SCOPE
                                 IT_MATNR        = LT_MATNR
                                 IT_QTGRP        = LT_QTGRP
                                 IF_INCL_DEL_FLG = LF_INCDEL_FLG
                       IMPORTING ET_QUOTA_DATA   = LT_QUOTA_DATA ).

* Get material master data and current stock
  GET_MATERIAL_DATA( EXPORTING IT_QUOTA_DATA    = LT_QUOTA_DATA
                     IMPORTING ET_MATERIAL_DATA = LT_MATERIAL_DATA ).

* Get PO
  GET_PO_DATA( EXPORTING IS_PERIOD_SCOPE = LS_PERIOD_SCOPE
                         IT_MATNR        = LT_MATNR
               IMPORTING ET_PO_DATA      = LT_PO_DATA
                CHANGING CT_SUM_QTY      = LT_SUM_QTY ).

* Get sales quotation data
  GET_QUOTATION_DATA( EXPORTING IS_PERIOD_SCOPE   = LS_PERIOD_SCOPE
                                IT_MATNR          = LT_MATNR
                                IT_QTGRP          = LT_QTGRP
                      IMPORTING ET_QUOTATION_DATA = LT_QUOTATION_DATA
                       CHANGING CT_SUM_QTY        = LT_SUM_QTY ).

* Get sales order data
  GET_SALES_ORDER_DATA( EXPORTING IS_PERIOD_SCOPE   = LS_PERIOD_SCOPE
                                  IT_MATNR          = LT_MATNR
                                  IT_QTGRP          = LT_QTGRP
                        IMPORTING ET_SALES_ORD_DATA = LT_SALES_ORD_DATA
                        CHANGING  CT_SUM_QTY        = LT_SUM_QTY ).
* Get Delivery order data
  GET_DO_DATA( EXPORTING IS_PERIOD_SCOPE   = LS_PERIOD_SCOPE
                         IT_MATNR          = LT_MATNR
                         IT_QTGRP          = LT_QTGRP
               IMPORTING ET_DO_ORD_DATA    = LT_DO_DATA
               CHANGING  CT_SUM_QTY        = LT_SUM_QTY ).
* Collect report result
  COLLECT_REPORT_RESULT( EXPORTING IS_PERIOD_SCOPE   = LS_PERIOD_SCOPE
                                   IT_MATERIAL_DATA  = LT_MATERIAL_DATA
                                   IT_PO_DATA        = LT_PO_DATA
                                   IT_QUOTATION_DATA = LT_QUOTATION_DATA
                                   IT_SALES_ORD_DATA = LT_SALES_ORD_DATA
                                   IT_QUOTA_DATA     = LT_QUOTA_DATA
                                   IT_SUM_QTY        = LT_SUM_QTY
                         IMPORTING ET_RESULT         = ET_RESULT ).

  IF ET_PO_DATA IS SUPPLIED.
    ET_PO_DATA[] = LT_PO_DATA[].
  ENDIF.

  IF ET_QUOTATION_DATA IS SUPPLIED.
    ET_QUOTATION_DATA[] = LT_QUOTATION_DATA[].
  ENDIF.

  IF ET_SALES_ORD_DATA IS SUPPLIED.
    ET_SALES_ORD_DATA[] = LT_SALES_ORD_DATA[].
  ENDIF.

  IF ET_DO_DATA IS SUPPLIED.
    ET_DO_DATA = LT_DO_DATA.
  ENDIF.

ENDMETHOD.


METHOD GET_REMAIN_QUOTA.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_QUOTA_PLAN_UTIL / GET_REMAIN_QUOTA
*  Creation Date      : 30.09.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDE030
*  Description        : To get remain quota for material
*  Purpose            : To get remain quota for material for the
*                       specifed delivery date / sales group
*                       Program will return error message when
*                       Schedule line date (IF_EDATU) not between
*                       Current system date and Last date of next month
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  DATA: LS_PERIOD_SCOPE  TYPE TS_PERIOD_SCOPE,
        LS_SALES_ORD_QTY TYPE TS_SALES_ORD_QTY,
        LF_QTGRP         TYPE ZSDSDE_QUOTAGRP.
  DATA:
    LT_FILTER        TYPE /IWBEP/T_MGW_SELECT_OPTION,
    LT_RESULT        TYPE ZSDSSDS053_TT,
    LT_SO_DATA       TYPE TT_SALES_ORD_DATA,
    LT_SALES_ORD_QTY TYPE TT_SALES_ORD_QTY.

  CLEAR: EF_REMAIN_QUOTA, ES_RETURN.
  CLEAR: LF_QTGRP.

*-----------------------
* Validate input
*-----------------------
  IF IF_EDATU IS INITIAL OR
     IF_VKGRP IS INITIAL OR
     IF_ZZRUNNING IS INITIAL OR
     IF_MATNR IS INITIAL.
*   Required input data missing. Unable to determine available quota.
    ES_RETURN-TYPE = GC_INFO.
    ES_RETURN-ID = GC_MSGID.
    ES_RETURN-NUMBER = '000'.
    ES_RETURN-MESSAGE = TEXT-E01.
    RETURN.
  ENDIF.

  LF_QTGRP = DETERMINE_QUOTA_GROUP( EXPORTING IF_VKGRP   = IF_VKGRP
                                              IF_RUNNING = IF_ZZRUNNING ).
  IF LF_QTGRP IS INITIAL.
*   Not Control Quota
    ES_RETURN-TYPE = GC_INFO.
    ES_RETURN-ID = GC_MSGID.
    ES_RETURN-NUMBER = '025'.
    ES_RETURN-MESSAGE = TEXT-S01.

    RETURN.
  ENDIF.

  IF NOT IS_QUOTA_ACTIVE( IF_MATNR  = IF_MATNR
                          IF_DATE   = IF_EDATU
                          IF_QTGRP  = LF_QTGRP ).
*   Not Control Quota
    ES_RETURN-TYPE = GC_INFO.
    ES_RETURN-ID = GC_MSGID.
    ES_RETURN-NUMBER = '025'.
    ES_RETURN-MESSAGE = TEXT-S01.

    RETURN.
  ENDIF.

* Check period scope is between system date and last day of next month?
* If the request schedule line date (IF_EDATU) outside the range,
* then return warning message
  LS_PERIOD_SCOPE = SET_PERIOD_SCOPE( IS_REQDAT = SY-DATUM ).
  IF NOT ( IF_EDATU BETWEEN LS_PERIOD_SCOPE-CURRENT_BEGIN AND
                            LS_PERIOD_SCOPE-NEXT1_END ).
*   Out of allocate
    ES_RETURN-TYPE = GC_ERROR.
    ES_RETURN-ID = GC_MSGID.
    ES_RETURN-NUMBER = '025'.
    ES_RETURN-MESSAGE = TEXT-E02.

    MESSAGE ID ES_RETURN-ID TYPE ES_RETURN-TYPE NUMBER ES_RETURN-NUMBER
      WITH ES_RETURN-MESSAGE_V1
           ES_RETURN-MESSAGE_V2
           ES_RETURN-MESSAGE_V3
           ES_RETURN-MESSAGE_V4
      INTO ES_RETURN-MESSAGE.
    RETURN.
  ENDIF.

*-----------------------
* Retrieve Quota Report Information
*-----------------------
  APPEND VALUE #( PROPERTY = 'MATNR'
                  SELECT_OPTIONS = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = IF_MATNR ) ) )
            TO LT_FILTER.

  APPEND VALUE #( PROPERTY = 'QTGRP'
                  SELECT_OPTIONS = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = LF_QTGRP ) ) )
            TO LT_FILTER.

* Call Class method to retrieve quota report information
  CALL METHOD ZCL_SDSSD_QUOTA_PLAN_UTIL=>GET_QUOTA_REPORT
    EXPORTING
      IT_FILTER         = LT_FILTER
    IMPORTING
      ET_RESULT         = LT_RESULT
      ET_SALES_ORD_DATA = LT_SO_DATA.

*-----------------------
* Prepare result
*-----------------------
  IF LT_RESULT IS INITIAL.
*   Not Control Quota
    ES_RETURN-TYPE = GC_INFO.
    ES_RETURN-ID = GC_MSGID.
    ES_RETURN-NUMBER = '025'.
    ES_RETURN-MESSAGE = TEXT-S01.

    RETURN.
  ENDIF.

  READ TABLE LT_RESULT INTO DATA(LS_RESULT)
                       WITH KEY MATNR = IF_MATNR.
  IF SY-SUBRC EQ 0.

*   Current Month
    IF ( IF_EDATU BETWEEN LS_PERIOD_SCOPE-CURRENT_BEGIN AND
                          LS_PERIOD_SCOPE-CURRENT_END ).
      EF_REMAIN_QUOTA = LS_RESULT-00_REMAIN_QUOTA.

*   Next Month
    ELSEIF ( IF_EDATU BETWEEN LS_PERIOD_SCOPE-NEXT1_BEGIN AND
                              LS_PERIOD_SCOPE-NEXT1_END ).
      EF_REMAIN_QUOTA = LS_RESULT-01_REMAIN_QUOTA.
    ENDIF.

  ENDIF.

*-----------------------
* Prepare exising SO Qty
*-----------------------
  IF CT_SALES_ORD_QTY IS REQUESTED.
    CLEAR: LT_SALES_ORD_QTY.
    LOOP AT LT_SO_DATA ASSIGNING FIELD-SYMBOL(<L_SO_DATA>).
      MOVE-CORRESPONDING <L_SO_DATA> TO LS_SALES_ORD_QTY.
      COLLECT LS_SALES_ORD_QTY INTO LT_SALES_ORD_QTY.
    ENDLOOP.

    IF CT_SALES_ORD_QTY IS INITIAL.
      CT_SALES_ORD_QTY = LT_SALES_ORD_QTY[].
    ELSE.
      LOOP AT CT_SALES_ORD_QTY ASSIGNING FIELD-SYMBOL(<L_SO_QTY>).
        READ TABLE LT_SALES_ORD_QTY INTO DATA(LS_SO_QTY)
                                    WITH KEY PERIOD = <L_SO_QTY>-PERIOD
                                             VBELN = <L_SO_QTY>-VBELN.
        IF SY-SUBRC EQ 0.
          <L_SO_QTY>-WMENG = LS_SO_QTY-WMENG.
          <L_SO_QTY>-VRKME = LS_SO_QTY-VRKME.
        ENDIF.
      ENDLOOP.
    ENDIF.
    SORT CT_SALES_ORD_QTY BY PERIOD VBELN.
  ENDIF.

ENDMETHOD.


METHOD GET_SALES_ORDER_DATA.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_QUOTA_PLAN_UTIL / GET_SALES_ORD_DATA
*  Creation Date      : 16.09.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDR012
*  Description        : Get sales order data and schedule line data
*  Purpose            : Get sales order data and schedule line data
*                       for the specified material, sales group and period
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  DATA: LS_SUM_QTY TYPE TS_SUM_QTY.

  DATA: LS_DATA TYPE TS_SALES_ORD_DATA.

  CLEAR: ET_SALES_ORD_DATA.

  SELECT VBAP~MATNR,
         VBEP~EDATU AS PERIOD,
         VBAK~VKGRP,
         VBAK~ZZRUNNING,
         QTGRP_MAP~QUOTAGRP,
         QTGRP_MAP~QUOTAGRP_DESC,
         VBAK~VBELN,
         VBAP~POSNR,
         VBEP~ETENR,
         VBEP~EDATU,
         VBAK~VBTYP,
         VBAK~KUNNR,
         VBAK~VKBUR,
         VBAP~PSTYV,
         VBAP~WERKS,
         VBAP~LGORT,
         VBEP~WMENG,
         VBEP~VRKME
     FROM VBAP
       INNER JOIN VBEP
       ON ( VBAP~VBELN EQ VBEP~VBELN AND
            VBAP~POSNR EQ VBEP~POSNR )
       INNER JOIN VBAK
       ON ( VBAP~VBELN EQ VBAK~VBELN )
       INNER JOIN ZSDSSDC013 AS QTGRP_MAP
       ON ( VBAK~VKGRP EQ QTGRP_MAP~VKGRP AND
            VBAK~ZZRUNNING EQ QTGRP_MAP~RUNNING )
     INTO TABLE @DATA(LT_SO)
     WHERE VBAK~VBTYP EQ 'C'                "Order
       AND VBAP~PSTYV IN @GRT_SD_ITEM_CAT
       AND VBAP~MATNR IN @IT_MATNR
       AND QTGRP_MAP~QUOTAGRP IN @IT_QTGRP
       AND VBEP~WMENG NE 0
       AND ( VBEP~EDATU BETWEEN @IS_PERIOD_SCOPE-CURRENT_BEGIN
                        AND @IS_PERIOD_SCOPE-NEXT1_END )
       AND NOT EXISTS ( SELECT VBELN
                          FROM ZSDSSDC029 AS ZZ
                         WHERE ZZ~VBELN   EQ VBAK~VBELN
                           AND ZZ~RUNNING EQ '1').

  IF SY-SUBRC EQ 0.
    DATA: LT_SUM LIKE LT_SO.
    APPEND LINES OF LT_SO TO LT_SUM.
*    ET_SALES_ORD_DATA[] = CORRESPONDING #( LT_SO ).
*
*    SORT ET_SALES_ORD_DATA BY MATNR PERIOD VBELN POSNR ETENR.
  ENDIF.


  SELECT VBAP~MATNR,
       VBEP~EDATU AS PERIOD,
       VBAK~VKGRP,
       VBAK~ZZRUNNING,
       QTGRP_MAP~QUOTAGRP,
       QTGRP_MAP~QUOTAGRP_DESC,
       VBAK~VBELN,
       VBAP~POSNR,
       VBEP~ETENR,
       VBEP~EDATU,
       VBAK~VBTYP,
       VBAK~KUNNR,
       VBAK~VKBUR,
       VBAP~PSTYV,
       VBAP~WERKS,
       VBAP~LGORT,
       VBEP~WMENG,
       VBEP~VRKME
   FROM VBAP
     INNER JOIN VBEP
     ON ( VBAP~VBELN EQ VBEP~VBELN AND
          VBAP~POSNR EQ VBEP~POSNR )
     INNER JOIN VBAK
     ON ( VBAP~VBELN EQ VBAK~VBELN )
     INNER JOIN ZSDSSDC013 AS QTGRP_MAP
     ON ( VBAK~VKGRP        EQ QTGRP_MAP~VKGRP AND
          QTGRP_MAP~RUNNING EQ '0')
     INNER JOIN ZSDSSDC029
     ON VBAK~VBELN         EQ ZSDSSDC029~VBELN AND
        ZSDSSDC029~RUNNING EQ '1'
   INTO TABLE @DATA(LT_SO_TMP)
   WHERE VBAK~VBTYP EQ 'C'                "Order
     AND VBAP~PSTYV IN @GRT_SD_ITEM_CAT
     AND VBAP~MATNR IN @IT_MATNR
     AND QTGRP_MAP~QUOTAGRP IN @IT_QTGRP
     AND VBEP~WMENG NE 0
     AND ( VBEP~EDATU BETWEEN @IS_PERIOD_SCOPE-CURRENT_BEGIN
                      AND @IS_PERIOD_SCOPE-NEXT1_END ).
  IF LT_SO_TMP IS NOT INITIAL.
    LOOP AT LT_SO_TMP INTO DATA(LS_SO_TMP).
      LS_SO_TMP-ZZRUNNING = '0'.
      APPEND LS_SO_TMP TO LT_SUM.
    ENDLOOP.
  ENDIF.

  IF LT_SUM IS NOT INITIAL.
    ET_SALES_ORD_DATA[] = CORRESPONDING #( LT_SUM ).
    SORT ET_SALES_ORD_DATA BY MATNR PERIOD VBELN POSNR ETENR.
  ENDIF.

*  IF SY-SUBRC EQ 0.
*    ET_SALES_ORD_DATA[] = CORRESPONDING #( LT_SO ).
*
*    SORT ET_SALES_ORD_DATA BY MATNR PERIOD VBELN POSNR ETENR.
*  ENDIF.

  IF CT_SUM_QTY IS SUPPLIED.
*   Prepare summary quantity by MATNR, PERIOD, VKGRP
    LOOP AT LT_SO ASSIGNING FIELD-SYMBOL(<L_SO>).
      MOVE-CORRESPONDING <L_SO> TO LS_DATA.
      APPEND LS_DATA TO ET_SALES_ORD_DATA.
      READ TABLE LT_SO_TMP
      WITH KEY VBELN = <L_SO>-VBELN TRANSPORTING NO FIELDS.
      IF SY-SUBRC EQ 0.
        DELETE LT_SO_TMP INDEX SY-TABIX.
      ENDIF.

      CLEAR: LS_SUM_QTY.
      LS_SUM_QTY-MATNR = <L_SO>-MATNR.
      LS_SUM_QTY-PERIOD = <L_SO>-PERIOD.
      LS_SUM_QTY-QUOTAGRP = <L_SO>-QUOTAGRP.
      LS_SUM_QTY-TYPE = 'SO'.
      LS_SUM_QTY-QTY = <L_SO>-WMENG.
      LS_SUM_QTY-UNIT = <L_SO>-VRKME.

      COLLECT LS_SUM_QTY INTO CT_SUM_QTY.
    ENDLOOP.

    LOOP AT LT_SO_TMP ASSIGNING <L_SO>.
      MOVE-CORRESPONDING <L_SO> TO LS_DATA.
      APPEND LS_DATA TO ET_SALES_ORD_DATA.
      CLEAR: LS_SUM_QTY.
      LS_SUM_QTY-MATNR = <L_SO>-MATNR.
      LS_SUM_QTY-PERIOD = <L_SO>-PERIOD.
      LS_SUM_QTY-QUOTAGRP = <L_SO>-QUOTAGRP.
      LS_SUM_QTY-TYPE = 'SO'.
      LS_SUM_QTY-QTY = <L_SO>-WMENG.
      LS_SUM_QTY-UNIT = <L_SO>-VRKME.

      COLLECT LS_SUM_QTY INTO CT_SUM_QTY.
    ENDLOOP.

    SORT CT_SUM_QTY BY MATNR PERIOD TYPE QUOTAGRP.
  ENDIF.

  IF ET_SALES_ORD_DATA IS NOT INITIAL.
    SORT ET_SALES_ORD_DATA BY MATNR PERIOD VBELN POSNR ETENR.
  ENDIF.



ENDMETHOD.


METHOD IS_QUOTA_ACTIVE.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_QUOTA_PLAN_UTIL / IS_QUOTA_ACTIVE
*  Creation Date      : 02.10.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDE030
*  Description        : To check if quota plan is active for the material?
*  Purpose            : To check if quota plan is active for the material?
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  IF IF_MATNR IS INITIAL OR
     IF_DATE IS INITIAL OR
     IF_QTGRP IS INITIAL.
    RF_RESULT = ABAP_FALSE.
    RETURN.
  ENDIF.

  SELECT MATNR,
         DAY_FIRST,
         DAY_LAST,
         QUOTAGRP
    FROM ZSDSSDT018
    INTO TABLE @DATA(LT_ZSDSSDT018) ##NEEDED
    WHERE MATNR EQ @IF_MATNR
      AND DAY_FIRST LE @IF_DATE
      AND DAY_LAST GE @IF_DATE
      AND QUOTAGRP EQ @IF_QTGRP
      AND ZDEL_FLG EQ @SPACE.
  IF SY-SUBRC EQ 0.
    RF_RESULT = ABAP_TRUE.
  ELSE.
    RF_RESULT = ABAP_FALSE.
  ENDIF.

ENDMETHOD.


METHOD READ_SUM_QTY.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_QUOTA_PLAN_UTIL / READ_SUM_QTY
*  Creation Date      : 16.09.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDR012
*  Description        : Return summary quantity for MATNR/PERIOD
*  Purpose            : To return quantity for MATNR/PERIOD/TYPE
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  FIELD-SYMBOLS: <L_SUM_QTY> type ts_sum_qty.

  CLEAR: RF_QTY.

  CHECK IT_SUM_QTY IS NOT INITIAL.

  IF IF_MEINS IS NOT INITIAL.
    READ TABLE IT_SUM_QTY ASSIGNING <L_SUM_QTY>
                        WITH KEY MATNR = IF_MATNR
                                 PERIOD = IF_PERIOD
                                 TYPE = IF_TYPE
                                 QUOTAGRP = IF_QTGRP
                                 UNIT = IF_MEINS.
  ELSE.
    READ TABLE IT_SUM_QTY ASSIGNING <L_SUM_QTY>
                        WITH KEY MATNR = IF_MATNR
                                 PERIOD = IF_PERIOD
                                 TYPE = IF_TYPE
                                 QUOTAGRP = IF_QTGRP.
  ENDIF.

  IF SY-SUBRC EQ 0.
    RF_QTY = <L_SUM_QTY>-QTY.
  ENDIF.

ENDMETHOD.


METHOD SET_PERIOD_SCOPE.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_QUOTA_PLAN_UTIL / SET_PERIOD_SCOPE
*  Creation Date      : 16.09.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDR012
*  Description        : Set period in scope of getting data
*  Purpose            : Only for current system month and next month
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  CLEAR: RS_PERIOD_SCOPE.

  RS_PERIOD_SCOPE-REQDAT = IS_REQDAT.

*-----------------------
* Current month
* Starting from first day of current month to last day of month
*-----------------------
  RS_PERIOD_SCOPE-CURRENT = SY-DATUM(6).
  RS_PERIOD_SCOPE-CURRENT_BEGIN = SY-DATUM.
  RS_PERIOD_SCOPE-CURRENT_BEGIN+6(2) = '01'.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = SY-DATUM
    IMPORTING
      LAST_DAY_OF_MONTH = RS_PERIOD_SCOPE-CURRENT_END
    EXCEPTIONS
      DAY_IN_NO_DATE    = 1
      OTHERS            = 2.
  IF SY-SUBRC <> 0.
    CLEAR: RS_PERIOD_SCOPE-CURRENT_END.
  ENDIF.

*-----------------------
* Next month
*-----------------------
* Set last day of next month
  CALL FUNCTION 'PS_LAST_DAY_OF_NEXT_MONTH'
    EXPORTING
      DAY_IN               = SY-DATUM
    IMPORTING
      LAST_DAY_OF_NEXT_MON = RS_PERIOD_SCOPE-NEXT1_END
    EXCEPTIONS
      DAY_IN_NOT_VALID     = 1
      OTHERS               = 2.

  IF SY-SUBRC EQ 0.
    RS_PERIOD_SCOPE-NEXT1 = RS_PERIOD_SCOPE-NEXT1_END(6).
    RS_PERIOD_SCOPE-NEXT1_BEGIN = RS_PERIOD_SCOPE-NEXT1_END.
    RS_PERIOD_SCOPE-NEXT1_BEGIN+6(2) = '01'.
  ENDIF.

ENDMETHOD.
ENDCLASS.

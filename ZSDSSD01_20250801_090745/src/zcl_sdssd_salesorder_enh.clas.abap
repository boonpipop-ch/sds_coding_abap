class ZCL_SDSSD_SALESORDER_ENH definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF TS_DETER_PRCTR,
             VKORG TYPE VBAK-VKORG,
             AUART TYPE VBAK-AUART,
             VKBUR TYPE VBAK-VKBUR,
             VKGRP TYPE VBAK-VKGRP,
           END OF TS_DETER_PRCTR .

  class-methods USEREXIT_CHECK_VBAK
    importing
      !IS_YVBAK type VBAK
    changing
      !CS_XVBAK type VBAK
      !CT_XVBAP type TAB_XYVBAP optional .
  class-methods USEREXIT_CHECK_VBAP
    importing
      !IS_XVBAK type VBAK
      !IS_XVBAP type VBAPVB
      !IS_YVBAP type VBAPVB
    changing
      !CS_VBAP type VBAP
      !CS_OVBAP type VBAP
      !CT_XVBAP type TAB_XYVBAP
      !CT_YVBAP type TAB_XYVBAP optional .
  class-methods USEREXIT_CHECK_VBKD
    importing
      !IS_TRTYP type T180-TRTYP
      !IS_VBAK type VBAK
      !IS_VBKD type VBKD
      !IS_YVBKD type VBKDVB optional
      !IT_XVBAP type TAB_XYVBAP optional
    changing
      !CF_FCODE type FCODE
      !CF_ERROR type FLAG optional .
  class-methods USEREXIT_CHECK_VBEP
    importing
      !IS_TRTYP type T180-TRTYP
      !IS_VBAK type VBAK
      !IS_VBEP type VBEP
      !IT_XVBEP type TAB_XYVBEP
      !IT_YVBEP type TAB_XYVBEP
      !IT_XVBAP type TAB_XYVBAP
    changing
      !CF_FCODE type FCODE
      !CF_ERROR type FLAG .
  class-methods USEREXIT_REFRESH_DOCUMENT .
  class-methods USEREXIT_SAVE_DOCUMENT
    importing
      !IS_TRTYP type T180-TRTYP
      !IS_VBAK type VBAK
      !IT_XVBKD type TAB_XYVBKD
      !IT_YVBKD type TAB_XYVBKD
      !IT_XVBAP type TAB_XYVBAP
      !IT_YVBAP type TAB_XYVBAP .
  class-methods USEREXIT_SAVE_DOCUMENT_PREPARE
    importing
      !IS_TRTYP type T180-TRTYP
    changing
      !CS_VBAK type VBAK
      !CT_XVBKD type TAB_XYVBKD
      !CT_YVBKD type TAB_XYVBKD
      !CT_XVBAP type TAB_XYVBAP
      !CT_YVBAP type TAB_XYVBAP optional
      !CT_XVBEP type TAB_XYVBEP optional
      !CT_YVBEP type TAB_XYVBEP optional
      !CF_FCODE type FCODE
      !CF_ERROR type FLAG .
  class-methods DETERMINE_PROFIT_CENTER
    importing
      !IS_COND type TS_DETER_PRCTR
    returning
      value(RF_PRCTR) type VBAP-PRCTR .
  class-methods USEREXIT_FIELD_MODIFICATION
    importing
      !IS_T180 type T180
      !IS_VBAK type VBAK
    changing
      !CS_SCREEN type SCREEN .
protected section.
private section.

  types:
    BEGIN OF TS_VTWEG_MAP,
      VTWEG     TYPE VBAK-VTWEG,
      VTWEG_SEL TYPE VBAK-VTWEG,
    END OF TS_VTWEG_MAP .
  types:
    TT_VTWEG_MAP TYPE SORTED TABLE OF TS_VTWEG_MAP WITH UNIQUE KEY VTWEG .
  types:
    BEGIN OF TS_ZZRUNNING_RANGE,        "CH01+ 420000372
      SIGN   TYPE SIGN_RANGE,
      OPTION TYPE OPT_RANGE,
      LOW    TYPE VBAK-ZZRUNNING,
      HIGH   TYPE VBAK-ZZRUNNING,
    END OF TS_ZZRUNNING_RANGE .
  types:
    TT_ZZRUNNING_RANGE TYPE STANDARD TABLE OF TS_ZZRUNNING_RANGE .     "CH01+ 420000372

  class-data GF_PRCTR type VBAP-PRCTR .
  class-data GF_1ST type FLAG .
  class-data GF_GET_GENC type FLAG .
  constants GC_REPID type ZSDSCAC001-REPID value 'ZCL_SDSSD_SALESORDER_ENH' ##NO_TEXT.
  class-data GRT_SALES_ORG type CMM_T_VKORG_RANGE .
  class-data GRT_CHANGE_PRODH type RANGE_C10_T .
  class-data GRT_CHECK_PAYMENT_TERM_NORMAL type SD_AUART_RANGES .
  class-data GRT_CHECK_PAYMENT_TERM_PROJECT type SD_AUART_RANGES .
  class-data GT_VTWEG_MAP type TT_VTWEG_MAP .
  class-data GRT_SET_ZZRUNNING_SO_TYPE type FIP_T_AUART_RANGE .
  class-data GRT_PROJ_ZZRUNNING type TT_ZZRUNNING_RANGE .
  class-data GS_DETER_PRCTR type TS_DETER_PRCTR .

  class-methods GET_CONSTANTS .
  class-methods IS_ACTIVE
    importing
      !IF_VKORG type VBAK-VKORG
    returning
      value(RF_RESULT) type ABAP_BOOLEAN .
  class-methods ASSIGN_SVO_LOB
    importing
      !IS_XVBAK type VBAK
    changing
      !CS_VBAP type VBAP .
  class-methods ASSIGN_PROFIT_CENTER_VBAK_CHG
    importing
      !IS_YVBAK type VBAK
      !IS_XVBAK type VBAK
    changing
      !CT_XVBAP type TAB_XYVBAP optional .
  class-methods ASSIGN_PROFIT_CENTER_VBAP_CHG
    importing
      !IS_XVBAP type VBAPVB
      !IS_YVBAP type VBAPVB
    changing
      !CS_VBAP type VBAP
      !CS_OVBAP type VBAP .
  class-methods ASSIGN_MATGRP1_VBAP_CHG
    importing
      !IS_XVBAK type VBAK
      !IS_XVBAP type VBAPVB
      !IS_YVBAP type VBAPVB
    changing
      !CS_VBAP type VBAP
      !CS_OVBAP type VBAP
      !CT_XVBAP type TAB_XYVBAP
      !CT_YVBAP type TAB_XYVBAP optional .
  class-methods ASSIGN_PRODH_VBAP_CHG
    importing
      !IS_XVBAK type VBAK
      !IS_XVBAP type VBAPVB
      !IS_YVBAP type VBAPVB
    changing
      !CS_VBAP type VBAP
      !CS_OVBAP type VBAP
      !CT_XVBAP type TAB_XYVBAP
      !CT_YVBAP type TAB_XYVBAP .
  class-methods ASSIGN_UPMAT_VBAP_CHG
    importing
      !IS_XVBAK type VBAK
    changing
      !CS_VBAP type VBAP
      !CS_OVBAP type VBAP .
  class-methods CHECK_PAYMENT_TERM_VBKD
    importing
      !IS_VBAK type VBAK
      !IS_VBKD type VBKD
      !IS_YVBKD type VBKDVB   ##NEEDED
      !IT_XVBAP type TAB_XYVBAP
    changing
      !CF_FCODE type FCODE
      !CF_ERROR type FLAG .
  class-methods CREDIT_BLOCK_SEND_EMAIL
    importing
      !IF_TRTYP type TRTYP optional
      !IS_VBAK type VBAK   ##NEEDED
      !IT_XVBAP type TAB_XYVBAP  ##NEEDED.
  class-methods CHECK_QUOTA_VBEP
    importing
      !IS_VBAK type VBAK
      !IS_VBEP type VBEP optional
      !IT_XVBAP type TAB_XYVBAP
      !IT_XVBEP type TAB_XYVBEP
      !IT_YVBEP type TAB_XYVBEP   ##NEEDED
    changing
      !CF_FCODE type FCODE
      !CF_ERROR type FLAG .
  class-methods UPDATE_FIELD_VALUE
    changing
      !CS_VBAK type VBAK .
ENDCLASS.



CLASS ZCL_SDSSD_SALESORDER_ENH IMPLEMENTATION.


METHOD ASSIGN_MATGRP1_VBAP_CHG.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SALESORDER_ENH / ASSIGN_MATGRP1_VBAP_CHG
*  Creation Date      : 30.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDE016
*  Description        : To set material group 1 (VBAP-MVGR1) of sub item
*                       to be the same as material group 1 of main item
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

* Check Sales Org in scope of processing and Material group 1 was changed
  IF IS_XVBAP-MVGR1 NE IS_YVBAP-MVGR1.
*   Material Group 1 of main item was changed
*   Then update the same value of material group 1 to all sub items under it
    IF CS_VBAP-UEPOS IS INITIAL AND       "Main item change
       IS_XVBAP-UPDKZ NE SPACE.
      LOOP AT CT_XVBAP ASSIGNING FIELD-SYMBOL(<L_VBAP>)
                       WHERE UEPOS EQ CS_VBAP-POSNR.
        IF <L_VBAP>-MVGR1 NE IS_XVBAP-MVGR1.
          <L_VBAP>-MVGR1 = IS_XVBAP-MVGR1.

          IF <L_VBAP>-UPDKZ EQ SPACE.
            INSERT <L_VBAP> INTO TABLE CT_YVBAP.
            <L_VBAP>-UPDKZ = IS_XVBAP-UPDKZ.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF SY-SUBRC NE 0.   "Not found any subitem
*       Get Material group 1 from reference quotation
*       with same item number (Quotation item number = SO Item number)
        IF IS_XVBAK-VGBEL IS NOT INITIAL.
          SELECT SINGLE MVGR1
            FROM VBAP
            INTO @DATA(LF_MVGR1)
            WHERE VBELN EQ @IS_XVBAK-VGBEL
              AND POSNR EQ @CS_VBAP-POSNR.
          IF SY-SUBRC EQ 0.
*           Update Old value
            CS_OVBAP-MVGR1 = CS_VBAP-MVGR1.

*           Assign New value
            CS_VBAP-MVGR1 = LF_MVGR1.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSEIF CS_VBAP-UEPOS IS NOT INITIAL.   "Sub item
*   Material Group 1 of sub item was changed
*   Then read the material group 1 of its main item and copy the value to sub item
      READ TABLE CT_XVBAP INTO DATA(LS_VBAP)
                          WITH KEY POSNR = CS_VBAP-UEPOS.   "Read main item
      IF SY-SUBRC EQ 0.
*       Update Old value
        CS_OVBAP-MVGR1 = CS_VBAP-MVGR1.
*       Assign New value
        CS_VBAP-MVGR1 = LS_VBAP-MVGR1.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD ASSIGN_PRODH_VBAP_CHG.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SALESORDER_ENH / ASSIGN_PRODH_VBAP_CHG
*  Creation Date      : 30.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDE016
*  Description        : To set product hierarchy 1 and 3 (VBAP-PRODH) of
*                       sub item to be the same as main item
*                       Only when sub item has product hierarchy 2 = ACC
*                       (Maintain in GENC Param = CHANGE_PRODH)
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

* Check Sales Org in scope of processing and Product Hierarchy was changed
  IF GRT_CHANGE_PRODH IS NOT INITIAL AND
     IS_XVBAP-PRODH NE IS_YVBAP-PRODH.
*   Product Hierarchy of main item was changed
*   Then update the same value to sub items which PRODH2 = ACC
    IF CS_VBAP-UEPOS IS INITIAL AND            "Main item
       IS_XVBAP-UPDKZ NE SPACE.                "Main item change
      LOOP AT CT_XVBAP ASSIGNING FIELD-SYMBOL(<L_VBAP>)
                       WHERE UEPOS EQ CS_VBAP-POSNR
                         AND PRODH+5(5) IN GRT_CHANGE_PRODH.

        IF <L_VBAP>-PRODH(5) NE CS_VBAP-PRODH(5) OR
           <L_VBAP>-PRODH+10(8) NE CS_VBAP-PRODH+10(8).

          <L_VBAP>-PRODH(5) = CS_VBAP-PRODH(5).       "PRODH Level 1
          <L_VBAP>-PRODH+10(8) = CS_VBAP-PRODH+10(8). "PRODH Level 3

          IF <L_VBAP>-UPDKZ EQ SPACE.
            INSERT <L_VBAP> INTO TABLE CT_YVBAP.
            <L_VBAP>-UPDKZ = IS_XVBAP-UPDKZ.
          ENDIF.
        ENDIF.

      ENDLOOP.
      IF SY-SUBRC NE 0.   "Not found any subitem
*       Get product hierarchy from reference quotation
*       with same item number (Quotation item number = SO Item number)
        IF IS_XVBAK-VGBEL IS NOT INITIAL AND
           CS_VBAP-PRODH+5(5) IN GRT_CHANGE_PRODH.
          SELECT SINGLE PRODH
            FROM VBAP
            INTO @DATA(LF_PRODH_QT)
            WHERE VBELN EQ @IS_XVBAK-VGBEL
              AND POSNR EQ @CS_VBAP-POSNR.
          IF SY-SUBRC EQ 0.
*           Update Old value
            CS_OVBAP-PRODH = CS_VBAP-PRODH.

*           Assign New value
            CS_VBAP-PRODH(5) = LF_PRODH_QT(5).       "PRODH Level 1
            CS_VBAP-PRODH+10(8) = LF_PRODH_QT+10(8). "PRODH Level 3
          ENDIF.
        ENDIF.
      ENDIF.
    ELSEIF CS_VBAP-UEPOS IS NOT INITIAL.        "Sub item
*     Product Hierarchy of sub item was changed
*     Then read Product Hierarchy of main item and copy the value to this item
      IF CS_VBAP-PRODH+5(5) IN GRT_CHANGE_PRODH.
        READ TABLE CT_XVBAP INTO DATA(LS_VBAP)
                            WITH KEY POSNR = CS_VBAP-UEPOS.   "Read main item
        IF SY-SUBRC EQ 0.
*         Update Old value
          CS_OVBAP-PRODH = CS_VBAP-PRODH.

*         Assign New value
          CS_VBAP-PRODH(5) = LS_VBAP-PRODH(5).       "PRODH Level 1
          CS_VBAP-PRODH+10(8) = LS_VBAP-PRODH+10(8). "PRODH Level 3
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD ASSIGN_PROFIT_CENTER_VBAK_CHG.

  DATA:
    LS_DETER_PRCTR   TYPE  TS_DETER_PRCTR,
    LS_DETER_PRCTR_O TYPE  TS_DETER_PRCTR.


* -------------------------------------------------------
* On change of Condition of Profit Determination fields
* -------------------------------------------------------
  LS_DETER_PRCTR   = CORRESPONDING #( IS_XVBAK ).
  IF GS_DETER_PRCTR IS NOT INITIAL.                         "+420000536
    LS_DETER_PRCTR_O = GS_DETER_PRCTR.                      "+420000536
  ELSE.                                                     "+420000536
    LS_DETER_PRCTR_O = CORRESPONDING #( IS_YVBAK ).
  ENDIF.                                                    "+420000536

* For the 1st time or when Condition is changed
  IF GF_1ST IS INITIAL OR
     LS_DETER_PRCTR NE LS_DETER_PRCTR_O.

*   Determine Profit Center from Setting in table ZSDSSDC001
    GF_PRCTR = DETERMINE_PROFIT_CENTER( IS_COND = LS_DETER_PRCTR ).
    GS_DETER_PRCTR = LS_DETER_PRCTR.                        "+420000536

*   For not 1st time and setting found, update all items
    IF GF_1ST IS NOT INITIAL.
      IF GF_PRCTR IS NOT INITIAL.
        LOOP AT CT_XVBAP ASSIGNING FIELD-SYMBOL(<L_XVBAP>).
          <L_XVBAP>-PRCTR = GF_PRCTR.
        ENDLOOP.
      ELSE.
        LOOP AT CT_XVBAP ASSIGNING <L_XVBAP>.
*         Update From material master
          SELECT SINGLE PRCTR
            FROM MARC
           WHERE MATNR EQ @<L_XVBAP>-MATNR
             AND WERKS EQ @<L_XVBAP>-WERKS
            INTO @DATA(LF_PRCTR).
          IF SY-SUBRC NE 0.
            CLEAR LF_PRCTR.
          ENDIF.
          <L_XVBAP>-PRCTR = LF_PRCTR.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD ASSIGN_PROFIT_CENTER_VBAP_CHG.

* On Item Changed, assign Profit Center
  IF GF_PRCTR IS NOT INITIAL AND
     IS_XVBAP NE IS_YVBAP.
*   Update Old value
    CS_OVBAP-PRCTR = CS_VBAP-PRCTR.
*   Assign New value
    CS_VBAP-PRCTR = GF_PRCTR.
  ENDIF.

ENDMETHOD.


METHOD ASSIGN_SVO_LOB.

  DATA:
    LS_CUSTOMER_H  TYPE  CRMT_CUSTOMER_H_WRK.

  DATA:
    LF_GUID  TYPE  CRMT_OBJECT_GUID.


* Only When Reference Service Order
  IF IS_XVBAK-CRM_GUID IS INITIAL.
    RETURN.
  ENDIF.

* Read LOB from Service Order
  LF_GUID = IS_XVBAK-CRM_GUID.
  CALL FUNCTION 'CRM_CUSTOMER_H_READ_OW'
    EXPORTING
      IV_GUID           = LF_GUID
    IMPORTING
      ES_CUSTOMER_H_WRK = LS_CUSTOMER_H
    EXCEPTIONS
      HEADER_NOT_FOUND  = 1
      OTHERS            = 2.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

* Update LOB from service Order
  CS_VBAP-ZZ1_LOB_SO_SDI = LS_CUSTOMER_H-ZZ1_LOB_SRH.

ENDMETHOD.


METHOD ASSIGN_UPMAT_VBAP_CHG.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SALESORDER_ENH / ASSIGN_UPMAT_VBAP_CHG
*  Creation Date      : 29.10.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDE016
*  Description        : To copy field from Quotation to Sales Order
*                         - QT UPMAT to SO UPMAT
*                         - QT UEPOS to SO POSEX
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

* Get data from reference quotation
* with same item number (Quotation item number = SO Item number)
  IF IS_XVBAK-VBTYP EQ 'C' AND          "Sales Order
     IS_XVBAK-VGBEL IS NOT INITIAL.     "SO reference Quotation

    SELECT SINGLE UPMAT, UEPOS
      FROM VBAP
      INTO @DATA(LS_QUOTATION)
      WHERE VBELN EQ @IS_XVBAK-VGBEL
        AND POSNR EQ @CS_VBAP-POSNR.
    IF SY-SUBRC EQ 0.
*     Update Old value
      CS_OVBAP-UPMAT = CS_VBAP-UPMAT.
      CS_OVBAP-POSEX = CS_VBAP-POSEX.

*     Assign New value
      CS_VBAP-UPMAT = LS_QUOTATION-UPMAT.
      CS_VBAP-POSEX = LS_QUOTATION-UEPOS.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD CHECK_PAYMENT_TERM_VBKD.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SALESORDER_ENH / CHECK_PAYMENT_TERM_VBKD
*  Creation Date      : 31.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDE027
*  Description        : To check payment term (VBKD-ZTERM) on header only
*                       (VBKD-POSNR = 00000)
*                       - Normal Order Type: Check with table KNVV
*                       - Project Order Type: Check with ZSDSFIT036
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  21.02.2025  F36K912908  Waraporn S. CR#420000372, CH01
*                                      For order type project, ZZRUNNING
*                                      in GENC: Get payment term from
*                                      customer master (KNVV)
*-----------------------------------------------------------------------
  DATA: LF_SO_DAYS      TYPE T052-ZTAG1,
        LF_MASTER_DAYS  TYPE T052-ZTAG1,
        LF_MASTER_ZTERM TYPE DZTERM,
        LF_WBS          TYPE VBAP-PS_PSP_PNR,
        LF_PROJECT      TYPE PRPS-PSPHI,
        LF_VTWEG        TYPE VBAK-VTWEG.

  CLEAR: CF_ERROR.
* Pre-requisites check
* 1. Order type in scope of checking
* 2. Payment term in header level is specified
* 3. Customer number is specified
* 4. Sales order has at least 1 item
  IF ( GRT_CHECK_PAYMENT_TERM_NORMAL IS INITIAL AND
       GRT_CHECK_PAYMENT_TERM_PROJECT IS INITIAL ) OR
     ( IS_VBAK-AUART NOT IN GRT_CHECK_PAYMENT_TERM_NORMAL AND
       IS_VBAK-AUART NOT IN GRT_CHECK_PAYMENT_TERM_PROJECT ) OR
       IS_VBKD-ZTERM IS INITIAL OR
       IS_VBAK-KUNNR IS INITIAL OR
       IS_VBKD-POSNR IS NOT INITIAL OR
       IT_XVBAP IS INITIAL.
    RETURN.
  ENDIF.

* Get days from baseline date from sales order payment term
  SELECT ZTAG1
    FROM T052
    INTO @LF_SO_DAYS
    UP TO 1 ROWS
    WHERE ZTERM EQ @IS_VBKD-ZTERM
    ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    CLEAR: LF_SO_DAYS.
  ENDIF.

* -----------------------
* Normal Order Type
* -----------------------
  IF GRT_CHECK_PAYMENT_TERM_NORMAL IS NOT INITIAL AND
     IS_VBAK-AUART IN GRT_CHECK_PAYMENT_TERM_NORMAL.

    READ TABLE GT_VTWEG_MAP INTO DATA(LS_VTWEG_MAP) WITH KEY VTWEG = IS_VBAK-VTWEG.
    IF SY-SUBRC = 0.
      LF_VTWEG = LS_VTWEG_MAP-VTWEG_SEL.
    ELSE.
      LF_VTWEG = IS_VBAK-VTWEG.
    ENDIF.
*   Get payment term from customer master
    SELECT SINGLE ZTERM
      FROM KNVV
      INTO @LF_MASTER_ZTERM
      WHERE KUNNR EQ @IS_VBAK-KUNNR
        AND VKORG EQ @IS_VBAK-VKORG
        AND VTWEG EQ @LF_VTWEG
        AND SPART EQ @IS_VBAK-SPART.

* -----------------------
* Project Order Type
* -----------------------
  ELSEIF GRT_CHECK_PAYMENT_TERM_PROJECT IS NOT INITIAL AND
     IS_VBAK-AUART IN GRT_CHECK_PAYMENT_TERM_PROJECT AND
     IT_XVBAP IS NOT INITIAL.

    LOOP AT IT_XVBAP ASSIGNING FIELD-SYMBOL(<L_VBAP>)
                     WHERE PS_PSP_PNR IS NOT INITIAL.
      LF_WBS = <L_VBAP>-PS_PSP_PNR.
      EXIT.
    ENDLOOP.

*   Get WBS from sales order first item
    IF LF_WBS IS NOT INITIAL.
      SELECT SINGLE PSPHI
        FROM PRPS
        INTO @LF_PROJECT
        WHERE PSPNR EQ @LF_WBS.

      IF SY-SUBRC EQ 0.
* Begin of deletion - CH01 - 420000372 - 21.02.2025
**       Get payment term from ZSDSFIT036
*          SELECT ZTERM
*            FROM ZSDSFIT036
*            INTO @LF_MASTER_ZTERM
*            UP TO 1 ROWS
*            WHERE KUNNR EQ @IS_VBAK-KUNNR
*              AND PSPHI EQ @LF_PROJECT
*              AND ZDEL_FLG EQ @SPACE
*            ORDER BY SEQ DESCENDING.
*          ENDSELECT.
* End of deletion - CH01 - 420000372 - 21.02.2025

* Begin of insertion - CH01 - 420000372 - 21.02.2025
* If ZZRUNNING in GRT_PROJ_ZZRUNNING,
* Then get payment term from ZSDSFIT036
* Otherwise, get payment term from customer master KNVV
        IF GRT_PROJ_ZZRUNNING IS NOT INITIAL AND
           IS_VBAK-ZZRUNNING IN GRT_PROJ_ZZRUNNING.

*         Get payment term from ZSDSFIT036
          SELECT ZTERM
            FROM ZSDSFIT036
            INTO @LF_MASTER_ZTERM
            UP TO 1 ROWS
            WHERE KUNNR EQ @IS_VBAK-KUNNR
              AND PSPHI EQ @LF_PROJECT
              AND ZDEL_FLG EQ @SPACE
              AND (
                  ( STARTDATE LE @IS_VBAK-AUDAT AND
                    ENDDATE   GE @IS_VBAK-AUDAT ) OR
                    ENDDATE   EQ ( SELECT MAX( ENDDATE )
                                     FROM ZSDSFIT036 AS X
                                     WHERE X~KUNNR EQ @IS_VBAK-KUNNR
                                       AND X~PSPHI EQ @LF_PROJECT
                                       AND X~ENDDATE LT @IS_VBAK-AUDAT
                                       AND X~ZDEL_FLG EQ @SPACE )
                  )
            ORDER BY SEQ DESCENDING.
          ENDSELECT.

        ELSE.       "ZZRUNNING not in GRT_PROJ_ZZRUNNING

          READ TABLE GT_VTWEG_MAP INTO LS_VTWEG_MAP
                                  WITH KEY VTWEG = IS_VBAK-VTWEG.
          IF SY-SUBRC = 0.
            LF_VTWEG = LS_VTWEG_MAP-VTWEG_SEL.
          ELSE.
            LF_VTWEG = IS_VBAK-VTWEG.
          ENDIF.

*         Get payment term from customer master
          SELECT SINGLE ZTERM
            FROM KNVV
            INTO @LF_MASTER_ZTERM
            WHERE KUNNR EQ @IS_VBAK-KUNNR
              AND VKORG EQ @IS_VBAK-VKORG
              AND VTWEG EQ @LF_VTWEG
              AND SPART EQ @IS_VBAK-SPART.

        ENDIF.
* End of insertion - CH01 - 420000372 - 21.02.2025

      ENDIF.
    ELSE.
*     All SO Item has no WBS value, then no need to check payment term
      RETURN.
    ENDIF.
  ENDIF.

* Get days from baseline date from payment term key
  IF LF_MASTER_ZTERM IS NOT INITIAL.
    SELECT ZTAG1
      FROM T052
      INTO @LF_MASTER_DAYS
      UP TO 1 ROWS
      WHERE ZTERM EQ @LF_MASTER_ZTERM
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      CLEAR: LF_MASTER_DAYS.
    ENDIF.
  ENDIF.

* -----------------------
* Compare payment term in sales document with master data
* -----------------------
  IF IS_VBKD-ZTERM NE LF_MASTER_ZTERM.
    IF LF_SO_DAYS > LF_MASTER_DAYS.
*     The term of payment is more than term of payment in master data
      SY-MSGTY = 'E'.
      SY-MSGID = 'ZSDSSD01'.
      SY-MSGNO = '001'.

*     Error, do not allow to save sales document
      CASE CF_FCODE.
        WHEN 'SICH' OR 'SIBA'.
          CF_FCODE = 'ENT1'.
      ENDCASE.

      CF_ERROR = 'X'.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD CHECK_QUOTA_VBEP.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SALESORDER_ENH / CHECK_QUOTA_VBEP
*  Creation Date      : 01.10.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDE030
*  Description        : To check available quota quantity
*  Purpose            : To check available quota quantity for each
*                       schedule line item
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  TYPES: BEGIN OF LS_REQUEST_QTY,
           MATNR      TYPE VBAP-MATNR,
           PERIOD_END TYPE VBEP-EDATU,
           WMENG      TYPE VBEP-WMENG,
           VRKME      TYPE VBEP-VRKME,
         END OF LS_REQUEST_QTY.

  TYPES: BEGIN OF LS_VBEP_CHECK,
           VBELN TYPE VBAP-VBELN,
           POSNR TYPE VBAP-POSNR,
           ETENR TYPE VBEP-ETENR,
           MATNR TYPE VBAP-MATNR,
         END OF LS_VBEP_CHECK.

  DATA: LF_REMAIN_QUOTA TYPE VBEP-WMENG,
        LS_RETURN       TYPE BAPIRET2,
        LS_REQUEST_QTY  TYPE LS_REQUEST_QTY,
        LS_VBEP_CHECK   TYPE LS_VBEP_CHECK.

  DATA: LT_SO_QTY_DB   TYPE ZCL_SDSSD_QUOTA_PLAN_UTIL=>TT_SALES_ORD_QTY,
        LT_REQUEST_QTY TYPE STANDARD TABLE OF LS_REQUEST_QTY,
        LT_VBEP_CHECK  TYPE STANDARD TABLE OF LS_VBEP_CHECK.

  DATA: LF_NEW_QTY TYPE VBEP-WMENG,
        LF_QTGRP   TYPE ZSDSDE_QUOTAGRP.

  DATA: LS_RETURN_MSG TYPE ZSDSSDS110,
        LT_RETURN_MSG TYPE STANDARD TABLE OF ZSDSSDS110.

  DATA: LV_CHECK TYPE C.

  DATA: LV_ZZRUNNING LIKE IS_VBAK-ZZRUNNING.

  CLEAR: CF_ERROR, LT_RETURN_MSG, LF_QTGRP.

  IF IS_VBAK-VKGRP IS INITIAL OR
     IS_VBAK-ZZRUNNING IS INITIAL.
    RETURN.
  ENDIF.

* Determine quota group from Sales Group and ZZRUNNING from table ZSDSSDC013
  LF_QTGRP = ZCL_SDSSD_QUOTA_PLAN_UTIL=>DETERMINE_QUOTA_GROUP(
                           EXPORTING IF_VKGRP   = IS_VBAK-VKGRP
                                     IF_RUNNING = IS_VBAK-ZZRUNNING ).
  IF LF_QTGRP IS INITIAL.
*   Quota group cannot determine, no need to check material quota
    RETURN.
  ELSE.
*--------------------------------------------------------------------*
* Edit SDS
*--------------------------------------------------------------------*
    CONSTANTS : BEGIN OF LC_CON,
                  I_REPID             TYPE CHAR20 VALUE 'CHECK_QUOTA_VBEP',
                  I_SINGLE_VALUE_FLAG TYPE FLAG   VALUE ABAP_TRUE,
                  I_PARAM             TYPE CHAR20 VALUE 'USER',
                  I_PARAM_EXT         TYPE CHAR20 VALUE '',
                  I_SEQUENCE          TYPE CHAR20 VALUE '',
                END OF LC_CON.

    DATA : C_RETURN      TYPE C LENGTH 255.

    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID              = LC_CON-I_REPID
                                                  I_SINGLE_VALUE_FLAG  = LC_CON-I_SINGLE_VALUE_FLAG
                                                  I_PARAM              = LC_CON-I_PARAM
                                         CHANGING C_RETURN             = C_RETURN
                                       ).

    IF SY-UNAME      EQ C_RETURN OR
       IS_VBAK-ERNAM EQ C_RETURN.
      LV_CHECK = ABAP_TRUE.
      LF_QTGRP+1(3) = 'PRJ'.
*      COMMIT WORK AND WAIT.
    ELSE.
      CLEAR : LV_CHECK.
    ENDIF.
  ENDIF.
*--------------------------------------------------------------------*
* End Edit SDS
*--------------------------------------------------------------------*

* Set period scope
  DATA(LS_PERIOD_SCOPE) = ZCL_SDSSD_QUOTA_PLAN_UTIL=>SET_PERIOD_SCOPE(
                                         IS_REQDAT = SY-DATUM ).

*--------------------------------------------------------------------*
* Edit Jakarin
*--------------------------------------------------------------------*
  DATA : BEGIN OF LS_MAT,
           MATNR TYPE VBAP-MATNR,
         END OF LS_MAT.
  DATA : LT_MAT LIKE HASHED TABLE OF LS_MAT WITH UNIQUE KEY MATNR.

  LT_MAT =  CORRESPONDING #( IT_XVBAP DISCARDING DUPLICATES ).

  SELECT ZSDSSDC030~MATNR
    FROM @LT_MAT AS A
    INNER JOIN ZSDSSDC030 ON A~MATNR              EQ ZSDSSDC030~MATNR AND
                             ZSDSSDC030~DAY_FIRST LE @SY-DATUM        AND
                             ZSDSSDC030~DAY_LAST  GE @SY-DATUM        AND
                             ZSDSSDC030~CONTF     EQ @ABAP_TRUE       AND
                             ZSDSSDC030~FLAGD     EQ @SPACE
    INTO TABLE @DATA(LT_DATA).

*--------------------------------------------------------------------*
* End edit Jakarin
*--------------------------------------------------------------------*

* Read schedule line item which created or changed
  LOOP AT IT_XVBEP ASSIGNING FIELD-SYMBOL(<L_XVBEP>)
                   WHERE EDATU IS NOT INITIAL
                     AND UPDKZ NE 'D'.       "Delete

*   Get material number from VBAP
    READ TABLE IT_XVBAP INTO DATA(LS_VBAP)
                        WITH KEY VBELN = <L_XVBEP>-VBELN
                                 POSNR = <L_XVBEP>-POSNR.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE IT_YVBEP ASSIGNING FIELD-SYMBOL(<L_YVBEP>)
                        WITH KEY VBELN = <L_XVBEP>-VBELN
                                 POSNR = <L_XVBEP>-POSNR
                                 ETENR = <L_XVBEP>-ETENR.
    IF SY-SUBRC EQ 0.

*     If no changes in schedule line date and quantity
*     Then no need to check quota but need to summary the request quantity
      IF <L_XVBEP>-EDATU EQ <L_YVBEP>-EDATU AND
         <L_XVBEP>-WMENG EQ <L_YVBEP>-WMENG.

*       Prepare list of request qty for each material/period
        LS_REQUEST_QTY-MATNR = LS_VBAP-MATNR.
        IF <L_XVBEP>-EDATU BETWEEN LS_PERIOD_SCOPE-CURRENT_BEGIN AND
                                   LS_PERIOD_SCOPE-CURRENT_END.
*         Current period
          LS_REQUEST_QTY-PERIOD_END = LS_PERIOD_SCOPE-CURRENT_END.
        ELSEIF <L_XVBEP>-EDATU BETWEEN LS_PERIOD_SCOPE-NEXT1_BEGIN AND
                                       LS_PERIOD_SCOPE-NEXT1_END.
*         Next period
          LS_REQUEST_QTY-PERIOD_END = LS_PERIOD_SCOPE-NEXT1_END.
        ELSE.
          CONTINUE.
        ENDIF.

        LS_REQUEST_QTY-WMENG = <L_XVBEP>-WMENG.
        LS_REQUEST_QTY-VRKME = <L_XVBEP>-VRKME.
        COLLECT LS_REQUEST_QTY INTO LT_REQUEST_QTY.

*       If no changes in schedule line date and quantity
*       Then no need to check quota
        CONTINUE.
      ENDIF.
    ENDIF.

*   Check material control by quota or not?
    IF NOT ZCL_SDSSD_QUOTA_PLAN_UTIL=>IS_QUOTA_ACTIVE(
                            IF_MATNR  = LS_VBAP-MATNR
                            IF_DATE   = <L_XVBEP>-EDATU
                            IF_QTGRP  = LF_QTGRP ).
*     Material not control by quota table, skip checking
      CONTINUE.
    ENDIF.

    IF NOT ( <L_XVBEP>-EDATU BETWEEN LS_PERIOD_SCOPE-CURRENT_BEGIN AND
                                     LS_PERIOD_SCOPE-NEXT1_END ).

*     If schedule line date not in period and confirmed qty <> 0
*     and delivered quantity = 0
*     Error: Item &1: Out of Allocate
      IF <L_XVBEP>-DLVQTY_BU IS INITIAL AND  "Delivered Qty = 0
         <L_XVBEP>-BMENG IS NOT INITIAL.     "Confirmed Qty <> 0
*       Out of allocate

        READ TABLE LT_DATA
        WITH KEY MATNR = LS_VBAP-MATNR TRANSPORTING NO FIELDS.
        IF SY-SUBRC EQ 0.
          SY-MSGTY = 'S'.
          SY-MSGID = 'ZSDSSD01'.
          SY-MSGNO = '032'.
          SY-MSGV1 = LS_VBAP-POSNR.

*       Insert Return Message Table
          CLEAR: LS_RETURN_MSG.
          LS_RETURN_MSG-VBELN = LS_VBAP-VBELN.
          LS_RETURN_MSG-POSNR = LS_VBAP-POSNR.
          LS_RETURN_MSG-ETENR = <L_XVBEP>-ETENR.
          MOVE-CORRESPONDING SYST TO LS_RETURN_MSG.
          INSERT LS_RETURN_MSG INTO TABLE LT_RETURN_MSG.
        ELSE.
          CF_ERROR = 'X'.

          SY-MSGTY = 'E'.
          SY-MSGID = 'ZSDSSD01'.
          SY-MSGNO = '025'.
          SY-MSGV1 = LS_VBAP-POSNR.

*       Insert Return Message Table
          CLEAR: LS_RETURN_MSG.
          LS_RETURN_MSG-VBELN = LS_VBAP-VBELN.
          LS_RETURN_MSG-POSNR = LS_VBAP-POSNR.
          LS_RETURN_MSG-ETENR = <L_XVBEP>-ETENR.
          MOVE-CORRESPONDING SYST TO LS_RETURN_MSG.
          INSERT LS_RETURN_MSG INTO TABLE LT_RETURN_MSG.
        ENDIF.
      ENDIF.

    ELSE.

*     Prepare list of request qty for each material/period
      LS_REQUEST_QTY-MATNR = LS_VBAP-MATNR.
      IF <L_XVBEP>-EDATU BETWEEN LS_PERIOD_SCOPE-CURRENT_BEGIN AND
                                 LS_PERIOD_SCOPE-CURRENT_END.
*       Current period
        LS_REQUEST_QTY-PERIOD_END = LS_PERIOD_SCOPE-CURRENT_END.
      ELSEIF <L_XVBEP>-EDATU BETWEEN LS_PERIOD_SCOPE-NEXT1_BEGIN AND
                                     LS_PERIOD_SCOPE-NEXT1_END.
*       Next period
        LS_REQUEST_QTY-PERIOD_END = LS_PERIOD_SCOPE-NEXT1_END.
      ENDIF.

      LS_REQUEST_QTY-WMENG = <L_XVBEP>-WMENG.
      LS_REQUEST_QTY-VRKME = <L_XVBEP>-VRKME.
      COLLECT LS_REQUEST_QTY INTO LT_REQUEST_QTY.

*     Insert Return Message Table
      CLEAR: LS_RETURN_MSG.
      LS_VBEP_CHECK-VBELN = LS_VBAP-VBELN.
      LS_VBEP_CHECK-POSNR = LS_VBAP-POSNR.
      LS_VBEP_CHECK-MATNR = LS_VBAP-MATNR.
      LS_VBEP_CHECK-ETENR = <L_XVBEP>-ETENR.

      APPEND LS_VBEP_CHECK TO LT_VBEP_CHECK.
    ENDIF.

  ENDLOOP.

  DELETE LT_REQUEST_QTY WHERE WMENG IS INITIAL.

  LOOP AT LT_REQUEST_QTY ASSIGNING FIELD-SYMBOL(<L_REQUEST_QTY>).
    IF LV_CHECK EQ ABAP_TRUE.
      LV_ZZRUNNING = '0'.
    ELSE.
      LV_ZZRUNNING = IS_VBAK-ZZRUNNING.
    ENDIF.
    ZCL_SDSSD_QUOTA_PLAN_UTIL=>GET_REMAIN_QUOTA(
      EXPORTING
        IF_EDATU         = <L_REQUEST_QTY>-PERIOD_END
        IF_VKGRP         = IS_VBAK-VKGRP
        IF_ZZRUNNING     = LV_ZZRUNNING
        IF_MATNR         = <L_REQUEST_QTY>-MATNR
      IMPORTING
        EF_REMAIN_QUOTA  = LF_REMAIN_QUOTA
        ES_RETURN        = LS_RETURN
      CHANGING
        CT_SALES_ORD_QTY = LT_SO_QTY_DB ).

    IF LS_RETURN IS NOT INITIAL.
*     Cannot get remain quota
      IF LS_RETURN-TYPE EQ 'E'.   "Error
        CF_ERROR = 'X'.
      ELSEIF LS_RETURN-TYPE EQ 'W'.
        CF_ERROR = 'W'.
      ENDIF.

      IF LS_RETURN-TYPE NE 'I'.
        SY-MSGTY = LS_RETURN-TYPE.
        SY-MSGID = LS_RETURN-ID.
        SY-MSGNO = LS_RETURN-NUMBER.
        SY-MSGV1 = LS_VBAP-POSNR.

*       Insert Return Message Table
        CLEAR: LS_RETURN_MSG.
        LS_RETURN_MSG-VBELN = LS_VBAP-VBELN.
        LS_RETURN_MSG-POSNR = LS_VBAP-POSNR.

        MOVE-CORRESPONDING SYST TO LS_RETURN_MSG.
        INSERT LS_RETURN_MSG INTO TABLE LT_RETURN_MSG.
      ENDIF.
    ELSE.
*     Get existing qty of this sales order
      READ TABLE LT_SO_QTY_DB INTO DATA(LS_SO_QTY_DB)
                              WITH KEY PERIOD = <L_XVBEP>-EDATU(6)
                                       VBELN = IS_VBAK-VBELN
                                       BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LF_REMAIN_QUOTA = LF_REMAIN_QUOTA + LS_SO_QTY_DB-WMENG.
      ENDIF.

      LF_NEW_QTY = LF_REMAIN_QUOTA - <L_REQUEST_QTY>-WMENG.

      IF LF_NEW_QTY < 0.
        READ TABLE LT_DATA
        WITH KEY MATNR = <L_REQUEST_QTY>-MATNR TRANSPORTING NO FIELDS.
        IF SY-SUBRC EQ 0.

          READ TABLE IT_XVBAP INTO LS_VBAP
          WITH KEY MATNR = <L_REQUEST_QTY>-MATNR.

          SY-MSGTY = 'S'.
          SY-MSGID = 'ZSDSSD01'.
          SY-MSGNO = '032'.
          SY-MSGV1 = LS_VBAP-POSNR.

*       Insert Return Message Table
          CLEAR: LS_RETURN_MSG.
          LS_RETURN_MSG-VBELN = IS_VBAK-VBELN.
          LS_RETURN_MSG-POSNR = LS_VBAP-POSNR.
*          LS_RETURN_MSG-ETENR = <L_XVBEP>-ETENR.
          MOVE-CORRESPONDING SYST TO LS_RETURN_MSG.
          INSERT LS_RETURN_MSG INTO TABLE LT_RETURN_MSG.
          CLEAR : LS_VBAP.
        ELSE.
          CF_ERROR = 'X'.

          READ TABLE LT_VBEP_CHECK INTO LS_VBEP_CHECK
                                   WITH KEY MATNR = <L_REQUEST_QTY>-MATNR.
          IF SY-SUBRC NE 0.
            CLEAR: LS_VBEP_CHECK.
          ENDIF.

*       Item &1, Schedule Line &2: Material over quota &3 &4
          SY-MSGTY = 'E'.
          SY-MSGID = 'ZSDSSD01'.
          SY-MSGNO = '026'.
          SY-MSGV1 = LS_VBEP_CHECK-POSNR.
          SY-MSGV2 = ABS( LF_NEW_QTY ).
          SY-MSGV3 = <L_REQUEST_QTY>-VRKME.
          CONDENSE SY-MSGV2 NO-GAPS.

*       Insert Return Message Table
          CLEAR: LS_RETURN_MSG.
          LS_RETURN_MSG-VBELN = LS_VBEP_CHECK-VBELN.
          LS_RETURN_MSG-POSNR = LS_VBEP_CHECK-POSNR.

          MOVE-CORRESPONDING SYST TO LS_RETURN_MSG.
          INSERT LS_RETURN_MSG INTO TABLE LT_RETURN_MSG.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF CF_ERROR = 'X'.
*   Error, do not allow to save sales document
    CASE CF_FCODE.
      WHEN 'SICH' OR 'SIBA'.
        CF_FCODE = 'ENT1'.
    ENDCASE.
  ENDIF.

  IF LV_CHECK EQ ABAP_TRUE.
    DATA : LS_ZSDSSDC029 TYPE ZSDSSDC029.
    LS_ZSDSSDC029-VBELN   = IS_VBAK-VBELN.
    LS_ZSDSSDC029-RUNNING = IS_VBAK-ZZRUNNING.
    MODIFY ZSDSSDC029 FROM LS_ZSDSSDC029.
  ENDIF.

* Export all return message from quota check to
* Class ZCL_SDSSD_CREATE_CHANGE_SO_SRV , Method SALES_ORDER_CREATE, SALES_ORDER_CHANGE
  EXPORT LT_RETURN_MSG = LT_RETURN_MSG TO MEMORY ID 'CHECK_QUOTA_VBEP'.

ENDMETHOD.


METHOD CREDIT_BLOCK_SEND_EMAIL.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SALESORDER_ENH / CREDIT_BLOCK_SEND_EMAIL
*  Creation Date      : 23.08.2024
*  Author             : Boontip R. (Eviden)
*  Add-on ID          : FSCME035
*  Description        : check credit block, send email to notify
*                       approver
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
    LC_APPV_TYPE_OVCREDIT TYPE ZSDSFIC029-APPV_TYPE VALUE '1' ##NEEDED,
    LC_APPV_TYPE_OVDUE    TYPE ZSDSFIC029-APPV_TYPE VALUE '2' ##NEEDED,
    LC_TDNAME             TYPE RSTXT-TDNAME VALUE 'ZSDS_FICM_OV_CRDT_MAIL' ##NEEDED.
  DATA:
    LS_CREDIT_VALUE  TYPE ZSDSFIS133,
    LS_THEAD         TYPE THEAD,
    LT_TLINES        TYPE STANDARD TABLE OF TLINE,
    LT_SYMS          TYPE STANDARD TABLE OF ITCST,
    LO_SEND_REQUEST  TYPE REF TO CL_BCS,
    LO_DOCUMENT      TYPE REF TO CL_DOCUMENT_BCS,
    LO_SENDER        TYPE REF TO IF_SENDER_BCS,
    LO_RECIPIENT     TYPE REF TO IF_RECIPIENT_BCS,
    LX_BCS_EXCEPTION TYPE REF TO CX_BCS ##NEEDED.
  DATA:
    LF_SUBRC            TYPE SY-SUBRC,
    LF_APPROVER         TYPE ZSDSFIC029-APPROVER,
    LF_DES              TYPE SO_OBJ_DES,
    LF_TITLE            TYPE STRING,
    LF_RESULT           TYPE OS_BOOLEAN ##NEEDED,

    " --- must be found variable as in std text ZSDS_FICM_OV_CRDT_MAIL
    "IS_VBAK-VBELN
    LF_CREDIT_EXCEED_TX TYPE TEXT20 ##NEEDED,
    LF_OVDUE_AMOUNT_TX  TYPE TEXT20 ##NEEDED,
    LF_NETWR_TX         TYPE TEXT20 ##NEEDED,
    LF_KNKLI_NAME1      TYPE KNA1-NAME1 ##NEEDED.

* ---- get approver
  SELECT APPROVER, APPV_TYPE, SEQNO                     "#EC CI_NOORDER
  FROM ZSDSFIC029
  WHERE AUART = @IS_VBAK-AUART
  AND   APPV_TYPE =   @LC_APPV_TYPE_OVCREDIT
  INTO @DATA(LS_APPRV_OVCREDIT)
  UP TO 1 ROWS.
  ENDSELECT.

  SELECT APPROVER, APPV_TYPE, SEQNO                     "#EC CI_NOORDER
  FROM ZSDSFIC029
  WHERE AUART = @IS_VBAK-AUART
  AND   APPV_TYPE = @LC_APPV_TYPE_OVDUE
  AND   VKORG = @IS_VBAK-VKORG
  AND   VTWEG = @IS_VBAK-VTWEG
  AND   SPART = @IS_VBAK-SPART
  INTO @DATA(LS_APPRV_OVDUE)
  UP TO 1 ROWS.
  ENDSELECT.

  IF LS_APPRV_OVCREDIT IS INITIAL
  AND LS_APPRV_OVDUE IS INITIAL.
    RETURN.
  ENDIF.

* --- get amount of credit exceed , overdue amount
*  -WBS order type
  IF ZCL_SDSFICM_UKM_SD_FSCM=>CHECK_ACTIVE_WBS_ORDER_TYPE( IS_VBAK-AUART ) = ABAP_TRUE.
    CALL METHOD ZCL_SDSFICM_UKM_SD_FSCM=>CREDIT_CHECK_WBS_ORDER
      EXPORTING
        IF_TRTYP         = IF_TRTYP
        IS_XVBAK         = IS_VBAK
        IT_XVBAP         = IT_XVBAP
      CHANGING
        CF_FSCM_TE_CHECK = LF_SUBRC
        CF_DATA_VALUE    = LS_CREDIT_VALUE.
* -NORMAL order type
  ELSE.
    CALL METHOD ZCL_SDSFICM_UKM_SD_FSCM=>CREDIT_GET_VALUE_NORMAL_ORDER
      EXPORTING
        IS_XVBAK         = IS_VBAK
        IT_XVBAP         = IT_XVBAP
      CHANGING
        CF_FSCM_TE_CHECK = LF_SUBRC
        CF_DATA_VALUE    = LS_CREDIT_VALUE.

  ENDIF.
  IF LF_SUBRC = 0. "no found credit error
    RETURN.
  ENDIF.

  IF LS_CREDIT_VALUE-CREDIT_EXCEED > 0.
    LF_APPROVER = LS_APPRV_OVCREDIT-APPROVER.
  ELSEIF LS_CREDIT_VALUE-OVDUE_AMOUNT > 0.
    LF_APPROVER = LS_APPRV_OVDUE-APPROVER.
    IF LF_APPROVER IS INITIAL.
      LF_APPROVER = LS_APPRV_OVCREDIT-APPROVER.
    ENDIF.
  ENDIF.

  IF LF_APPROVER IS INITIAL.
    RETURN.
  ENDIF.
* --- get email's approver
  SELECT SMTP_ADDR                                      "#EC CI_NOORDER
  FROM USR21 AS A INNER JOIN ADR6 AS B
  ON A~ADDRNUMBER = B~ADDRNUMBER
  AND A~PERSNUMBER = B~PERSNUMBER
  WHERE BNAME = @LF_APPROVER
  INTO @DATA(LF_SMTP_ADDR)
  UP TO 1 ROWS.
  ENDSELECT.

  IF LF_SMTP_ADDR IS INITIAL.
    RETURN.
  ENDIF.

* --- prepare data variable as text
  IF LS_CREDIT_VALUE-CREDIT_EXCEED > 0.
    WRITE  LS_CREDIT_VALUE-CREDIT_EXCEED CURRENCY LS_CREDIT_VALUE-CREDIT_EXCEED_WAERS TO LF_CREDIT_EXCEED_TX .
    CONDENSE LF_CREDIT_EXCEED_TX.
  ELSE.
    LF_CREDIT_EXCEED_TX = '0.00'.
  ENDIF.
  IF LS_CREDIT_VALUE-OVDUE_AMOUNT > 0.
    WRITE  LS_CREDIT_VALUE-OVDUE_AMOUNT CURRENCY LS_CREDIT_VALUE-OVDUE_AMOUNT_WAERS TO LF_OVDUE_AMOUNT_TX .
    CONDENSE LF_OVDUE_AMOUNT_TX.
  ELSE.
    LF_OVDUE_AMOUNT_TX = '0.00' .
  ENDIF.
  IF IS_VBAK-KNKLI IS NOT INITIAL.
    SELECT SINGLE NAME1
    FROM KNA1
    WHERE KUNNR = @IS_VBAK-KNKLI
    INTO @LF_KNKLI_NAME1.
  ENDIF.

  WRITE IS_VBAK-NETWR CURRENCY IS_VBAK-WAERK TO LF_NETWR_TX.
  CONDENSE LF_NETWR_TX.

* --- read content of email from standard text
  LS_THEAD-TDOBJECT = 'TEXT'.
  LS_THEAD-TDNAME   = LC_TDNAME.
  LS_THEAD-TDID     = 'ST'.
  LS_THEAD-TDSPRAS  = SY-LANGU.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      CLIENT                  = SY-MANDT
      ID                      = LS_THEAD-TDID
      LANGUAGE                = LS_THEAD-TDSPRAS
      NAME                    = LS_THEAD-TDNAME
      OBJECT                  = LS_THEAD-TDOBJECT
    TABLES
      LINES                   = LT_TLINES
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
    RETURN.
  ENDIF.

  CALL FUNCTION 'INIT_TEXTSYMBOL'.

  CALL FUNCTION 'TEXT_SYMBOL_COLLECT'
    TABLES
      LINES   = LT_TLINES                 " Text lines
      SYMBOLS = LT_SYMS.                " Symbols found

  LOOP AT LT_SYMS INTO DATA(LS_SYMB) ##INTO_OK.
    ASSIGN (LS_SYMB-NAME) TO FIELD-SYMBOL(<F_TEXTSYM>).
    DATA(LF_TEXTSYM) = |&{ LS_SYMB-NAME }&|.

    IF <F_TEXTSYM> IS ASSIGNED.
      CALL FUNCTION 'TEXT_SYMBOL_SETVALUE'
        EXPORTING
          NAME  = LF_TEXTSYM                 " Symbol name
          VALUE = <F_TEXTSYM>.               " Symbol value
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'TEXT_SYMBOL_REPLACE'
    EXPORTING
      HEADER = LS_THEAD
    TABLES
      LINES  = LT_TLINES.

  CALL FUNCTION 'FORMAT_TEXTLINES' ##FM_SUBRC_OK
    EXPORTING
      FORMATWIDTH = 128
    TABLES
      LINES       = LT_TLINES
    EXCEPTIONS
      BOUND_ERROR = 1
      OTHERS      = 2.

* --- PREPARE DATA TEXT IN STD TEXT
  DATA(LT_TEXT_TABLE) = VALUE BCSY_TEXT( FOR LS_TLINES IN LT_TLINES ( LINE = LS_TLINES-TDLINE ) ).

* --- send email ---
  TRY.
      LF_TITLE = |Please release sales order { IS_VBAK-VBELN }| ##NO_TEXT.
      LF_DES = LF_TITLE.
      LO_SEND_REQUEST = CL_BCS=>CREATE_PERSISTENT( ).
      LO_DOCUMENT     = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
                          I_TYPE    = 'RAW'
                          I_TEXT    = LT_TEXT_TABLE
                          I_SUBJECT = LF_DES ).
      LO_SEND_REQUEST->SET_DOCUMENT( LO_DOCUMENT ).

      LF_TITLE = |Please release sales order { IS_VBAK-VBELN }| ##NO_TEXT.
      CALL METHOD LO_SEND_REQUEST->SET_MESSAGE_SUBJECT
        EXPORTING
          IP_SUBJECT = LF_TITLE.

      LO_SENDER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( 'k2servicedev@daikin.co.th' ) ##NO_TEXT.
      CALL METHOD LO_SEND_REQUEST->SET_SENDER
        EXPORTING
          I_SENDER = LO_SENDER.

      LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( LF_SMTP_ADDR ).
      CALL METHOD LO_SEND_REQUEST->ADD_RECIPIENT
        EXPORTING
          I_RECIPIENT  = LO_RECIPIENT
          I_EXPRESS    = 'X'
          I_BLIND_COPY = 'X'.

      CALL METHOD LO_SEND_REQUEST->SEND(
        EXPORTING
          I_WITH_ERROR_SCREEN = 'X'
        RECEIVING
          RESULT              = LF_RESULT ).
    CATCH CX_BCS INTO LX_BCS_EXCEPTION.
      RETURN.
  ENDTRY.
ENDMETHOD.


METHOD DETERMINE_PROFIT_CENTER.

* Initialize Output
  CLEAR: RF_PRCTR.

* Read Configuration
  SELECT SINGLE PRCTR
    FROM ZSDSSDC001
   WHERE VKORG EQ @IS_COND-VKORG
     AND AUART EQ @IS_COND-AUART
     AND VKBUR EQ @IS_COND-VKBUR
     AND VKGRP EQ @IS_COND-VKGRP
     AND ZDEL_FLG EQ @SPACE
    INTO @RF_PRCTR.
  IF SY-SUBRC EQ 0.
    RETURN.
  ENDIF.

* Read with SO Type wildcard
  SELECT AUART,
         PRCTR
    FROM ZSDSSDC001
   WHERE VKORG EQ @IS_COND-VKORG
     AND AUART LIKE '%*%'
     AND VKBUR EQ @IS_COND-VKBUR
     AND VKGRP EQ @IS_COND-VKGRP
     AND ZDEL_FLG EQ @SPACE
   ORDER BY AUART ASCENDING,
            PRCTR ASCENDING
    INTO TABLE @DATA(LT_DATA).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Check Document type which can wildcard
  LOOP AT LT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).
    CHECK IS_COND-AUART CP <L_DATA>-AUART.
    RF_PRCTR = <L_DATA>-PRCTR.
    EXIT.
  ENDLOOP.

ENDMETHOD.


METHOD GET_CONSTANTS.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SALESORDER_ENH / GET_CONSTANTS
*  Creation Date      : 30.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDE016
*  Description        : To get GENC constant from table ZSDSCAC001
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  21.02.2025  F36K912908  Waraporn S. CR#420000372, CH01
*                                      For order type project, ZZRUNNING
*                                      in GENC: Get payment term from
*                                      customer master (KNVV)
*-----------------------------------------------------------------------
  CONSTANTS:
    LC_SALES_ORG           TYPE ZSDSDE_PARAM_NAME VALUE 'SALES_ORG',
    LC_CHANGE_PRODH        TYPE ZSDSDE_PARAM_NAME VALUE 'CHANGE_PRODH',
    LC_CHECK_PAYMENT_TERM  TYPE ZSDSDE_PARAM_NAME VALUE 'CHECK_PAYMENT_TERM',
    LC_ORDER_TYPE_NORMAL   TYPE ZSDSDE_PARAM_EXT  VALUE 'ORDER_TYPE_NORMAL',
    LC_ORDER_TYPE_PROJECT  TYPE ZSDSDE_PARAM_EXT  VALUE 'ORDER_TYPE_PROJECT',
    LC_DIST_CHANNEL_MAP    TYPE ZSDSDE_PARAM_EXT  VALUE 'DIST_CHANNEL_MAP',
    LC_SET_ZZRUNNING       TYPE ZSDSDE_PARAM_NAME VALUE 'SET_ZZRUNNING',
    LC_SO_TYPE             TYPE ZSDSDE_PARAM_EXT  VALUE 'SO_TYPE',
    LC_PROJ_ZZRUNNING      TYPE ZSDSDE_PARAM_EXT  VALUE 'PROJ_ZZRUNNING'.   "CH01+

  DATA: LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

* Only for 1st Time
  IF GF_GET_GENC IS NOT INITIAL.
    RETURN.
  ENDIF.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = GC_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).
    CASE <L_GENC>-PARAM.
*     SALES_ORG
      WHEN LC_SALES_ORG.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_SALES_ORG.
*     CHANGE_PRODH
      WHEN LC_CHANGE_PRODH.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_CHANGE_PRODH.
*     CHECK_PAYMENT_TERM
      WHEN LC_CHECK_PAYMENT_TERM.
        IF <L_GENC>-PARAM_EXT EQ LC_ORDER_TYPE_NORMAL.
          INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                          OPTION = <L_GENC>-PARAM_OPTION
                          LOW    = <L_GENC>-VALUE_LOW
                          HIGH   = <L_GENC>-VALUE_HIGH )
                 INTO TABLE GRT_CHECK_PAYMENT_TERM_NORMAL.
        ELSEIF <L_GENC>-PARAM_EXT EQ LC_ORDER_TYPE_PROJECT.
          INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                          OPTION = <L_GENC>-PARAM_OPTION
                          LOW    = <L_GENC>-VALUE_LOW
                          HIGH   = <L_GENC>-VALUE_HIGH )
                 INTO TABLE GRT_CHECK_PAYMENT_TERM_PROJECT.
        ELSEIF <L_GENC>-PARAM_EXT EQ LC_DIST_CHANNEL_MAP.
          INSERT VALUE #( VTWEG = <L_GENC>-VALUE_LOW
                          VTWEG_SEL = <L_GENC>-VALUE_HIGH )
                 INTO TABLE GT_VTWEG_MAP.
* Begin of insertion CH01 - 420000372 - 21.02.2025
        ELSEIF <L_GENC>-PARAM_EXT EQ LC_PROJ_ZZRUNNING.
          INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                          OPTION = <L_GENC>-PARAM_OPTION
                          LOW    = <L_GENC>-VALUE_LOW
                          HIGH   = <L_GENC>-VALUE_HIGH )
                 INTO TABLE GRT_PROJ_ZZRUNNING.
* End of insertion CH01 - 420000372 - 21.02.2025
        ENDIF.
*     SET_ZZRUNNING
      WHEN LC_SET_ZZRUNNING.
        IF <L_GENC>-PARAM_EXT EQ LC_SO_TYPE.
          INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                          OPTION = <L_GENC>-PARAM_OPTION
                          LOW    = <L_GENC>-VALUE_LOW
                          HIGH   = <L_GENC>-VALUE_HIGH )
                 INTO TABLE GRT_SET_ZZRUNNING_SO_TYPE.
        ENDIF.
    ENDCASE.
  ENDLOOP.
  GF_GET_GENC = ABAP_TRUE.
ENDMETHOD.


METHOD IS_ACTIVE.
  IF GRT_SALES_ORG IS NOT INITIAL AND
     IF_VKORG IN GRT_SALES_ORG.
    RF_RESULT = ABAP_TRUE.
  ELSE.
    RF_RESULT = ABAP_FALSE.
  ENDIF.
ENDMETHOD.


  METHOD UPDATE_FIELD_VALUE.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SALESORDER_ENH /
*                       UPDATE_FIELD_VALUE
*  Creation Date      : 25.10.2024
*  Author             : Boontip (Eviden)
*  Add-on ID          : SDE036
*  Description        : Update field VBAK-ZZRUNNING
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    IF GRT_SET_ZZRUNNING_SO_TYPE IS NOT INITIAL
    AND CS_VBAK-AUART IN GRT_SET_ZZRUNNING_SO_TYPE
    AND CS_VBAK-ZZRUNNING IS INITIAL.
      CS_VBAK-ZZRUNNING = 1.
    ENDIF.
  ENDMETHOD.


METHOD USEREXIT_CHECK_VBAK.

* Get Constants setting
  GET_CONSTANTS( ).

* Only when Active
  IF NOT IS_ACTIVE( CS_XVBAK-VKORG ).
    RETURN.
  ENDIF.

* SDE020: Assign Profit Center
  ASSIGN_PROFIT_CENTER_VBAK_CHG(
    EXPORTING
      IS_YVBAK = IS_YVBAK
      IS_XVBAK = CS_XVBAK
    CHANGING
      CT_XVBAP = CT_XVBAP ).

* Mark 1st time flag
  GF_1ST = 'X'.

ENDMETHOD.


METHOD USEREXIT_CHECK_VBAP.

* Only when Active
  IF NOT IS_ACTIVE( IS_XVBAK-VKORG ).
    RETURN.
  ENDIF.

* SDE020: Assign Profit Center
  ASSIGN_PROFIT_CENTER_VBAP_CHG(
    EXPORTING
      IS_XVBAP = IS_XVBAP
      IS_YVBAP = IS_YVBAP
    CHANGING
      CS_VBAP  = CS_VBAP
      CS_OVBAP = CS_OVBAP ).

* SDE016: Assign Product Hierarchy Level 1 and 3
  ASSIGN_PRODH_VBAP_CHG(
    EXPORTING
      IS_XVBAK = IS_XVBAK
      IS_XVBAP = IS_XVBAP
      IS_YVBAP = IS_YVBAP
    CHANGING
      CS_VBAP  = CS_VBAP
      CS_OVBAP = CS_OVBAP
      CT_XVBAP = CT_XVBAP
      CT_YVBAP = CT_YVBAP ).

* SDE016: Assign Material Group 1
  ASSIGN_MATGRP1_VBAP_CHG(
    EXPORTING
      IS_XVBAK = IS_XVBAK
      IS_XVBAP = IS_XVBAP
      IS_YVBAP = IS_YVBAP
    CHANGING
      CS_VBAP  = CS_VBAP
      CS_OVBAP = CS_OVBAP
      CT_XVBAP = CT_XVBAP
      CT_YVBAP = CT_YVBAP ).

* SDE016: Assign UPMAT from Quotation to Sales Order
  ASSIGN_UPMAT_VBAP_CHG(
   EXPORTING
     IS_XVBAK = IS_XVBAK
   CHANGING
     CS_VBAP  = CS_VBAP
     CS_OVBAP = CS_OVBAP ).

* CME004: Assign Service Order LOB
  ASSIGN_SVO_LOB(
    EXPORTING
      IS_XVBAK = IS_XVBAK
    CHANGING
      CS_VBAP  = CS_VBAP ).

ENDMETHOD.


METHOD USEREXIT_CHECK_VBEP.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SALESORDER_ENH / USEREXIT_CHECK_VBEP
*  Creation Date      : 07.10.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDE027
*  Description        : Routine USEREXIT_CHECK_VBEP
*                       Call from MV45AFZB / USEREXIT_CHECK_VBEP
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  CLEAR: CF_ERROR.

* Only when Active
  IF NOT IS_ACTIVE( IS_VBAK-VKORG ).
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD USEREXIT_CHECK_VBKD.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SALESORDER_ENH / USEREXIT_CHECK_VBKD
*  Creation Date      : 04.06.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDE027
*  Description        : Routine USEREXIT_CHECK_VBKD
*                       Call from MV45AFZB / USEREXIT_CHECK_VBKD
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  CLEAR: CF_ERROR.

* Only when Active
  IF NOT IS_ACTIVE( IS_VBAK-VKORG ).
    RETURN.
  ENDIF.

* SDE027: Check payment term
* Only in Create/Change mode
  IF IS_TRTYP EQ 'V' OR
     IS_TRTYP EQ 'H'.
    CHECK_PAYMENT_TERM_VBKD(
      EXPORTING
        IS_VBAK  = IS_VBAK
        IS_VBKD  = IS_VBKD
        IS_YVBKD = IS_YVBKD
        IT_XVBAP = IT_XVBAP
      CHANGING
        CF_FCODE = CF_FCODE
        CF_ERROR = CF_ERROR ).
  ENDIF.

ENDMETHOD.


METHOD USEREXIT_FIELD_MODIFICATION.

* Get Constants setting
  GET_CONSTANTS( ).

* Only when Active
  IF NOT IS_ACTIVE( IS_VBAK-VKORG ).
    RETURN.
  ENDIF.

* Only in Create/Change mode
  IF IS_T180-TRTYP EQ 'V' OR
     IS_T180-TRTYP EQ 'H' .

*   Disable Delivery Block Field
    IF SCREEN-NAME EQ 'VBEP-LIFSP'.
      SCREEN-INPUT = 0.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD USEREXIT_REFRESH_DOCUMENT.
* Get Constants
  GET_CONSTANTS( ).
* Only SDS Related
  IF GRT_SALES_ORG IS NOT INITIAL.
*   Initialize Variables
    CLEAR: GF_1ST,
           GF_PRCTR,
           GS_DETER_PRCTR.
  ENDIF.
ENDMETHOD.


METHOD USEREXIT_SAVE_DOCUMENT.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SALESORDER_ENH /
*                       USEREXIT_SAVE_DOCUMENT
*  Creation Date      : 23.08.2024
*  Author             : Boontip (Eviden)
*  Add-on ID          :
*  Description        : Routine USEREXIT_SAVE_DOCUMENT
*                       Call from MV45AFZZ/USEREXIT_SAVE_DOCUMENT
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  23.08.2004  FSCME035    Boontip    send mail for credit limit block
*-----------------------------------------------------------------------
* Only when Active
  IF NOT IS_ACTIVE( IS_VBAK-VKORG ).
    RETURN.
  ENDIF.

*-- begin of FSCME025
* Only in Create/Change mode
  IF IS_VBAK-VBTYP = 'C' "order
  AND ( IS_TRTYP EQ 'V' OR IS_TRTYP EQ 'H' )
  AND IS_VBAK-CMGST = 'B' . "Block
    CREDIT_BLOCK_SEND_EMAIL(
      EXPORTING
        IF_TRTYP = IS_TRTYP
        IS_VBAK  = IS_VBAK
        IT_XVBAP = IT_XVBAP )  .
  ENDIF.
*-- end of FSCME025
*  IF SY-TCODE EQ 'VA01' OR
*     SY-TCODE EQ 'VA02'.
*    LCL_DATA=>UPDATE_CONFIRM_QTY( I_DATA = IT_XVBAP
*                                  I_HEAD = IS_VBAK ).
*  ENDIF.

ENDMETHOD.


METHOD USEREXIT_SAVE_DOCUMENT_PREPARE ##NEEDED.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SALESORDER_ENH /
*                       USEREXIT_SAVE_DOCUMENT_PREPARE
*  Creation Date      : 07.08.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDE027
*  Description        : Routine USEREXIT_SAVE_DOCUMENT_PREPARE
*                       Call from MV45AFZZ/USEREXIT_SAVE_DOCUMENT_PREPARE
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  CONSTANTS: LC_POSNR_HDR TYPE VBKD-POSNR VALUE '000000'.

  DATA: LS_VBKD TYPE VBKD.

  CONSTANTS : BEGIN OF LC_CON,
                I_REPID TYPE CHAR20 VALUE 'MV45AFZZ',
                I_PARAM TYPE CHAR20 VALUE 'CHECK_TAX_CLASS',
              END OF LC_CON.

  DATA : CR_RETURN TYPE RANGE OF VBAK-KVGR5.

  CLEAR: CF_ERROR.

* Only when Active
  IF NOT IS_ACTIVE( CS_VBAK-VKORG ).
    RETURN.
  ENDIF.

* SDE027: Check payment term
* Only in Create/Change mode
  IF IS_TRTYP EQ 'V' OR
     IS_TRTYP EQ 'H'.
    READ TABLE CT_XVBKD INTO DATA(LS_XVBKD)
                        WITH KEY POSNR = LC_POSNR_HDR.
    IF SY-SUBRC EQ 0.
      MOVE-CORRESPONDING LS_XVBKD TO LS_VBKD.
    ELSE.
      RETURN.
    ENDIF.

    READ TABLE CT_YVBKD INTO DATA(LS_YVBKD)
                        WITH KEY POSNR = LC_POSNR_HDR.
    IF SY-SUBRC NE 0.
      CLEAR: LS_YVBKD.
    ENDIF.

    CHECK_PAYMENT_TERM_VBKD(
      EXPORTING
        IS_VBAK  = CS_VBAK
        IS_VBKD  = LS_VBKD
        IS_YVBKD = LS_YVBKD
        IT_XVBAP = CT_XVBAP
      CHANGING
        CF_FCODE = CF_FCODE
        CF_ERROR = CF_ERROR ).

    IF CF_ERROR EQ 'X'.
      RETURN.
    ENDIF.
  ENDIF.

* Only Sales order Create/Change mode
  IF CS_VBAK-VBTYP EQ 'C' AND    "Sales Order
     ( IS_TRTYP EQ 'V' OR
       IS_TRTYP EQ 'H' ).

* SDE036: update ZZRUNNING
    UPDATE_FIELD_VALUE(
       CHANGING CS_VBAK = CS_VBAK ).

* SDE030: Check quota
    CHECK_QUOTA_VBEP(
      EXPORTING
        IS_VBAK  = CS_VBAK
        IT_XVBAP = CT_XVBAP
        IT_XVBEP = CT_XVBEP
        IT_YVBEP = CT_YVBEP
     CHANGING
        CF_FCODE = CF_FCODE
        CF_ERROR = CF_ERROR ).

    IF CF_ERROR EQ 'X'.
      RETURN.
    ENDIF.
  ENDIF.

  SELECT COUNT( * )
    FROM ZSDSCAC009
    WHERE PROCESS EQ 'TAX_CLASS'
      AND STATU   EQ ABAP_TRUE.
  IF SY-SUBRC EQ 0.
    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID              = LC_CON-I_REPID
                                                  I_PARAM              = LC_CON-I_PARAM
                                         CHANGING CR_RETURN            = CR_RETURN ).

    IF CR_RETURN[]   IS NOT INITIAL AND
       CS_VBAK-KVGR5 IN CR_RETURN[].
      IF CS_VBAK-TAXK1 NE '2'.
        CF_ERROR = 'X'.
        SY-MSGTY = 'E'.
        SY-MSGID = 'ZSDSSD01'.
        SY-MSGNO = '033'.
        SY-MSGV1 = CS_VBAK-KVGR5.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.
ENDCLASS.

*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0190
*  Creation Date      : 27.06.2024
*  Author             : Thanapong C. (Eviden)
*  Add-on ID          : ZSDR002
*  Description        : Sales Analysis Report
*  Purpose            : Report shows sales billing with linked quotes orders,
*                       deliveries, and shipments. Supports monthly summaries
*                       and drill-down to billing details
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSSDR0190.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  VBRK,
  VBRP,
  VBPA,
  VBKD,
  VBAK.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: TS_RESULT  TYPE  ZSDSSDS040.
TYPES: TT_RESULT  TYPE  STANDARD TABLE OF TS_RESULT.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_REPID TYPE SY-REPID  VALUE 'ZSDSSDR0190',
  GC_TRUE  TYPE CHAR1     VALUE 'X',
  GC_TCODE TYPE SY-TCODE  VALUE 'ZSDSSD015'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT        TYPE  TT_RESULT                           ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*
DATA:
  GF_PARAM           TYPE  CHAR10                              ##NEEDED.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSSDS040'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 10,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 90.

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
DEFINE MC_SHOW_PROGRESS.
  IF sy-batch IS INITIAL.
*   Show progress online
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = &1 ##NUMBER_OK
        text       = &2.
  ELSE.
*   Show message step in background
    MESSAGE i000(38) WITH &2 space space space.
  ENDIF.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s01: Selection Criteria
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  PARAMETERS:
   P_VKORG TYPE VBRK-VKORG DEFAULT '1000' OBLIGATORY.

  SELECT-OPTIONS:
    S_VTWEG FOR VBRK-VTWEG,
    S_SPART FOR VBRP-SPART,
    S_VKBUR FOR VBRP-VKBUR,
    S_VKGRP FOR VBRP-VKGRP,
    S_KUNAG FOR VBRK-KUNAG,
    S_FKART FOR VBRK-FKART,
    S_FKDAT FOR VBRK-FKDAT DEFAULT SY-DATUM,
    S_VBELN FOR VBRK-VBELN,
    S_MATNR FOR VBRP-MATNR,
    S_PRODH FOR VBRP-PRODH,
    S_PERNR FOR VBPA-PERNR,
    S_BSTKD FOR VBKD-BSTKD,
    S_KVGR2 FOR VBRP-KVGR2,
    S_BNAME FOR VBAK-BNAME.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
*   Get Data
  PERFORM F_GET_DATA CHANGING GT_RESULT.
  IF GT_RESULT IS INITIAL.
*     Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
*   Display Processing Result
  PERFORM F_DISPLAY_RESULT USING GT_RESULT.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE ZSDSCAI9990 ##INCL_OK.

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

*----------------------------------------------------------------------*
*  Form f_get_data
*----------------------------------------------------------------------*
*  Get Data
*----------------------------------------------------------------------*
FORM F_GET_DATA  CHANGING CT_RESULT TYPE TT_RESULT.

  CONSTANTS:
    LC_OPTION_CP TYPE DDOPTION VALUE 'CP',
    LC_OPTION_EQ TYPE DDOPTION VALUE 'EQ'.

  DATA:
    LS_PRODH       TYPE PRODH,
    LT_PRODH_RANGE TYPE RANGE OF VBRP-PRODH.

* Initialize Output
  CLEAR: CT_RESULT.

  IF S_PRODH[] IS NOT INITIAL.

    LOOP AT S_PRODH ASSIGNING FIELD-SYMBOL(<L_PRODH>).

      APPEND INITIAL LINE TO LT_PRODH_RANGE ASSIGNING FIELD-SYMBOL(<L_PRODH_RANGE>).
      <L_PRODH_RANGE>-SIGN   = <L_PRODH>-SIGN.
      <L_PRODH_RANGE>-OPTION = <L_PRODH>-OPTION.
      <L_PRODH_RANGE>-LOW    = <L_PRODH>-LOW.
      <L_PRODH_RANGE>-HIGH   = <L_PRODH>-HIGH.

      IF <L_PRODH>-OPTION = LC_OPTION_EQ.

        CLEAR LS_PRODH.
        LS_PRODH = <L_PRODH>-LOW.
        IF LS_PRODH-PRODH3 IS INITIAL.
          <L_PRODH_RANGE>-LOW    = |{ <L_PRODH_RANGE>-LOW }*|.
          <L_PRODH_RANGE>-OPTION = LC_OPTION_CP.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDIF.

  ZCL_SDSSD_SALES_ANALYSIS=>GET_DATA(
    EXPORTING
      IF_REPID       = GC_REPID
      IF_VKORG       = P_VKORG
      IT_VTWEG_RANGE = S_VTWEG[]
      IT_SPART_RANGE = S_SPART[]
      IT_VKBUR_RANGE = S_VKBUR[]
      IT_VKGRP_RANGE = S_VKGRP[]
      IT_KUNAG_RANGE = S_KUNAG[]
      IT_FKART_RANGE = S_FKART[]
      IT_FKDAT_RANGE = S_FKDAT[]
      IT_VBELN_RANGE = S_VBELN[]
      IT_MATNR_RANGE = S_MATNR[]
      IT_PRODH_RANGE = LT_PRODH_RANGE[]
      IT_PERNR_RANGE = S_PERNR[]
      IT_BSTKD_RANGE = S_BSTKD[]
      IT_KVGR2_RANGE = S_KVGR2[]
      IT_BNAME_RANGE = S_BNAME[]
    IMPORTING
      ET_DATA        = DATA(LT_DATA)
  ).

  CT_RESULT = CORRESPONDING #( LT_DATA ).

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_RESULT
*----------------------------------------------------------------------*
*  Display Processing Result
*----------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UT_RESULT TYPE TT_RESULT.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = GC_TRUE.

*   No auto refresh in edit mode
  GF_NO_AUTO_REFRESH_1 = GC_TRUE.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN UT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                           CS_VARIANT TYPE  DISVARIANT
                           CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-SEL_MODE   = 'B'. "Multiple Selection with Push Box
  CS_LAYOUT-CWIDTH_OPT = SPACE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.

  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

  CONSTANTS:
    LC_DECIMALS_O_2 TYPE I VALUE 2.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.

      WHEN 'HALF_PERIOD'.
        "Text-001 : 1st half / 2nd half
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-001
                                 CHANGING <L_FIELDCAT>.
      WHEN 'ZZMONTH'.
        "Text-002 : Month
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-002
                 CHANGING <L_FIELDCAT>.
      WHEN 'PERIOD'.
        "Text-003 : Period
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-003
                 CHANGING <L_FIELDCAT>.
      WHEN 'CALENDAR_YEAR'.
        "Text-004 : Calendar Year
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-004
                 CHANGING <L_FIELDCAT>.
      WHEN 'QUARTER'.
        "Text-005 : Quarter
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-005
                 CHANGING <L_FIELDCAT>.
      WHEN 'WEEK'.
        "Text-006 : Week
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-006
                 CHANGING <L_FIELDCAT>.
      WHEN 'FISCAL_YEAR'.
        "Text-007 : Fiscal Year
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-007
                 CHANGING <L_FIELDCAT>.
      WHEN 'SALE_ORG'.
        "Text-008 : Sales Organization
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-008
                 CHANGING <L_FIELDCAT>.
      WHEN 'DIST_CHANNEL'.
        "Text-009 : Distribution Channel
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-009
                 CHANGING <L_FIELDCAT>.
      WHEN 'DIVISION'.
        "Text-010 : Division
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-010
                  CHANGING <L_FIELDCAT>.
      WHEN 'SALE_AREA'.
        "Text-011 : Sales Area
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-011
                  CHANGING <L_FIELDCAT>.
      WHEN 'SALE_OFFICE'.
        "Text-012 : Sales Office
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-012
                  CHANGING <L_FIELDCAT>.
      WHEN 'SALE_GROUP'.
        "Text-013 : Sales Group
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-013
                  CHANGING <L_FIELDCAT>.
      WHEN 'CUST_CREATE_DATE'.
        "Text-014 : Cust. Create Date
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-014
                  CHANGING <L_FIELDCAT>.
      WHEN 'CUST_CODE'.
        "Text-015 : Customer Code
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-015
                  CHANGING <L_FIELDCAT>.
      WHEN 'CUST_NAME_EN'.
        "Text-016 : Customer Name (English)
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-016
                  CHANGING <L_FIELDCAT>.
      WHEN 'CUST_NAME_TH'.
        "Text-017 : Customer Name (Thai)
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-017
                  CHANGING <L_FIELDCAT>.
      WHEN 'APPLICATION'.
        "Text-018 : Application
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-018
                 CHANGING <L_FIELDCAT>.
      WHEN 'CUST_CLASS'.
        "Text-019 : Customer Classification
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-019
                 CHANGING <L_FIELDCAT>.
      WHEN 'ACC_CLERK'.
        "Text-020 : Account Clerk
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-020
                 CHANGING <L_FIELDCAT>.
      WHEN 'CUST_ADDR_1'.
        "Text-021 : Customer Address1
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-021
                 CHANGING <L_FIELDCAT>.
      WHEN 'CUST_ADDR_2'.
        "Text-022 : Customer Address2
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-022
                 CHANGING <L_FIELDCAT>.
      WHEN 'CUST_REGION'.
        "Text-023 : Customer Region
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-023
                 CHANGING <L_FIELDCAT>.
      WHEN 'CUST_POSTCODE'.
        "Text-024 : Customer Post Code
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-024
                 CHANGING <L_FIELDCAT>.
      WHEN 'MOBILE'.
        "Text-025 : Mobile phone
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-025
                 CHANGING <L_FIELDCAT>.
      WHEN 'TELEPHONE'.
        "Text-026 : Telephone
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-026
                 CHANGING <L_FIELDCAT>.
      WHEN 'ETAX_EMAIL'.
        "Text-027 : E-Tax Email
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-027
                 CHANGING <L_FIELDCAT>.
      WHEN 'CUST_CREDIT_GROUP'.
        "Text-028 : Customer credit group
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-028
                 CHANGING <L_FIELDCAT>.
      WHEN 'SHIPTO_CODE'.
        "Text-029 : ShipTo Code
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-029
                 CHANGING <L_FIELDCAT>.
      WHEN 'SHIPTO_NAME_EN'.
        "Text-030 : ShipTo Name (English)
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-030
                 CHANGING <L_FIELDCAT>.
      WHEN 'SHIPTO_NAME_TH'.
        "Text-031 : ShipTo Name (Thai)
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-031
                 CHANGING <L_FIELDCAT>.
      WHEN 'SHIPTO_ADDR'.
        "Text-032 : ShipTo Address
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-032
                 CHANGING <L_FIELDCAT>.
      WHEN 'SHIPTO_REGION'.
        "Text-033 : ShipTo Region
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-033
                 CHANGING <L_FIELDCAT>.
      WHEN 'SHIPTO_POSTCODE'.
        "Text-034 : ShipTo Post Code
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-034
                 CHANGING <L_FIELDCAT>.
      WHEN 'PERSON_NO'.
        "Text-035 : Personal Number
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-035
                 CHANGING <L_FIELDCAT>.
      WHEN 'PERSON_NAME'.
        "Text-036 : Personal Name
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-036
                 CHANGING <L_FIELDCAT>.
      WHEN 'CUST_GROUP_1'.
        "Text-037 : Cust. Grp1
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-037
                 CHANGING <L_FIELDCAT>.
      WHEN 'CUST_GROUP_1_TX'.
        "Text-038 : Cust. Grp1 Description
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-038
                 CHANGING <L_FIELDCAT>.
      WHEN 'CUST_GROUP_2'.
        "Text-039 : Cust. Grp2
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-039
                 CHANGING <L_FIELDCAT>.
      WHEN 'CUST_GROUP_2_TX'.
        "Text-040 : Cust. Grp2 Description
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-040
                 CHANGING <L_FIELDCAT>.
      WHEN 'QUOTE_TYPE'.
        "Text-041 : Q/T Type
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-041
                 CHANGING <L_FIELDCAT>.
      WHEN 'QUOTE_NO'.
        "Text-042 : Q/T No.
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-042
                 CHANGING <L_FIELDCAT>.
      WHEN 'SO_TYPE'.
        "Text-043 : S/O Type
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-043
                 CHANGING <L_FIELDCAT>.
      WHEN 'SO_NO'.
        "Text-044 : S/O No.
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-044
                 CHANGING <L_FIELDCAT>.
      WHEN 'SO_CREATE_DATE'.
        "Text-045 : S/O Create Date
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-045
                 CHANGING <L_FIELDCAT>.
      WHEN 'SO_CREATE_BY'.
        "Text-046 : S/O Create name
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-046
                 CHANGING <L_FIELDCAT>.
      WHEN 'DO_NO'.
        "Text-047 : D/O No.
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-047
                 CHANGING <L_FIELDCAT>.
      WHEN 'DO_CREATE_DATE'.
        "Text-048 : D/O Create Date
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-048
                 CHANGING <L_FIELDCAT>.
      WHEN 'DO_CREATE_TIME'.
        "Text-049 : D/O Create Time
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-049
                 CHANGING <L_FIELDCAT>.
      WHEN 'DO_DATE'.
        "Text-050 : D/O Date
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-050
                 CHANGING <L_FIELDCAT>.
      WHEN 'BILL_TYPE'.
        "Text-051 : Billing Type
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-051
                 CHANGING <L_FIELDCAT>.
      WHEN 'BILL_TYPE_TX'.
        "Text-052 : Billing Type Name
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-052
                 CHANGING <L_FIELDCAT>.
      WHEN 'BILL_NO'.
        "Text-053 : Invoice No.
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-053
                 CHANGING <L_FIELDCAT>.
      WHEN 'BILL_DATE'.
        "Text-054 : Invoice Date
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-054
                 CHANGING <L_FIELDCAT>.
      WHEN 'TAX_NO'.
        "Text-055 : Tax number
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-055
                 CHANGING <L_FIELDCAT>.
      WHEN 'REF_FIDOC'.
        "Text-056 : Ref. FI document no.
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-056
                 CHANGING <L_FIELDCAT>.
      WHEN 'EXT_SYSTEM_1'.
        "Text-057 : External systems1
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-057
                 CHANGING <L_FIELDCAT>.
      WHEN 'EXT_SYSTEM_2'.
        "Text-058 : External systems2
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-058
                 CHANGING <L_FIELDCAT>.
      WHEN 'PO_NO'.
        "Text-059 : Main P/O No.
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-059
                 CHANGING <L_FIELDCAT>.
      WHEN 'ORDER_REASON'.
        "Text-060 : Order Reason
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-060
                 CHANGING <L_FIELDCAT>.
      WHEN 'ZTERM'.
        "Text-061 : Payment term
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-061
                 CHANGING <L_FIELDCAT>.
      WHEN 'ZTERM_TX'.
        "Text-062 : Payment term desc.
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-062
                 CHANGING <L_FIELDCAT>.
      WHEN 'WAERK'.
        "Text-063 : Currency
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-063
                 CHANGING <L_FIELDCAT>.
      WHEN 'VSART'.
        "Text-064 : Shipping Type
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-064
                 CHANGING <L_FIELDCAT>.
      WHEN 'TKNUM'.
        "Text-065 : Shipment No.
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-065
                 CHANGING <L_FIELDCAT>.
      WHEN 'LSTEL'.
        "Text-066 : Loading Point
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-066
                 CHANGING <L_FIELDCAT>.
      WHEN 'LSTEL_TX'.
        "Text-067 : Loading Point Desc.
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-067
                 CHANGING <L_FIELDCAT>.
      WHEN 'DTDIS'.
        "Text-068 : Planning (Date for delivery)
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-068
                 CHANGING <L_FIELDCAT>.
      WHEN 'ROUTE'.
        "Text-069 : Route
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-069
                 CHANGING <L_FIELDCAT>.
      WHEN 'EXTI2'.
        "Text-070 : Truck driver
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-070
                 CHANGING <L_FIELDCAT>.
      WHEN 'EXTI1'.
        "Text-071 : External ID1
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-071
                 CHANGING <L_FIELDCAT>.
      WHEN 'UZDIS'.
        "Text-072 : Schedule(Time)
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-072
                 CHANGING <L_FIELDCAT>.
      WHEN 'DATEN'.
        "Text-073 : ActShipEnd(Date of Receive)
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-073
                 CHANGING <L_FIELDCAT>.
      WHEN 'UATEN'.
        "Text-074 : AcShET (Time Receive)
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-074
                 CHANGING <L_FIELDCAT>.
      WHEN 'REF_MEMO_HTX'.
        "Text-075 : Refer Memo
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-075
                 CHANGING <L_FIELDCAT>.
      WHEN 'REQ_REMARK_HTX'.
        "Text-076 : Req. Remark
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-076
                 CHANGING <L_FIELDCAT>.
      WHEN 'PROJ_HTX'.
        "Text-077 : Project Text
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-077
                 CHANGING <L_FIELDCAT>.
      WHEN 'REMARK_QT_HTX'.
        "Text-078 : Remarks QT
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-078
                 CHANGING <L_FIELDCAT>.
      WHEN 'REASON_HTX'.
        "Text-079 : Reason
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-079
                 CHANGING <L_FIELDCAT>.
      WHEN 'INV_REMARK_HTX'.
        "Text-080 : Inv. Remark
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-080
                 CHANGING <L_FIELDCAT>.
      WHEN 'LAND_NO_HTX'.
        "Text-081 : Land No.
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-081
                 CHANGING <L_FIELDCAT>.
      WHEN 'POSNR'.
        "Text-082 : Item
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-082
                 CHANGING <L_FIELDCAT>.
      WHEN 'UEPOS'.
        "Text-083 : H-Item
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-083
                 CHANGING <L_FIELDCAT>.
      WHEN 'MATNR_BOM'.
        "Text-084 : Header Bom Material refer.
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-084
                 CHANGING <L_FIELDCAT>.
      WHEN 'MATNR'.
        "Text-085 : Material No.
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-085
                 CHANGING <L_FIELDCAT>.
      WHEN 'ARKTX'.
        "Text-086 : Material Description
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-086
                 CHANGING <L_FIELDCAT>.
      WHEN 'WERKS_TX'.
        "Text-087 : Plant
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-087
                 CHANGING <L_FIELDCAT>.
      WHEN 'LGOBE'.
        "Text-088 : Storage Location
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-088
                 CHANGING <L_FIELDCAT>.
      WHEN 'H_BOM_QTY'.
        "Text-089 : Set Qty. Header BOM
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-089
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.

      WHEN 'SALE_QTY'.
        "Text-090 : Sale Qty.
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-090
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.

      WHEN 'BILL_QTY'.
        "Text-091 : Billing Qty.
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-091
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.

      WHEN 'ACT_QTY'.
        "Text-092 : Actual Qty.
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-092
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.

      WHEN 'VRKME'.
        "Text-093 : UOM
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-093
                 CHANGING <L_FIELDCAT>.
      WHEN 'NTGEW'.
        "Text-094 : Net Weight
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-094
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.

      WHEN 'BRGEW'.
        "Text-095 : Gross Weight
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-095
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.

      WHEN 'VOLUM'.
        "Text-096 : Volume
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-096
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.

      WHEN 'PRCTR'.
        "Text-097 : Profit Center
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-097
                 CHANGING <L_FIELDCAT>.
      WHEN 'ZZREFT'.
        "Text-098 : Refrigerant Type
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-098
                 CHANGING <L_FIELDCAT>.
      WHEN 'MFRNR'.
        "Text-099 : Manufact
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-099
                 CHANGING <L_FIELDCAT>.
      WHEN 'PS_PSP_PNR'.
        "Text-100 : WBS
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-100
                 CHANGING <L_FIELDCAT>.
      WHEN 'PRODH'.
        "Text-101 : Prod.Hierarchy
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-101
                 CHANGING <L_FIELDCAT>.
      WHEN 'PRODH_1'.
        "Text-102 : Prodh. Lv.1
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-102
                 CHANGING <L_FIELDCAT>.
      WHEN 'PRODH_2'.
        "Text-103 : Prodh. Lv.2
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-103
                 CHANGING <L_FIELDCAT>.
      WHEN 'PRODH_3'.
        "Text-104 : Prodh. Lv.3
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-104
                 CHANGING <L_FIELDCAT>.
      WHEN 'SUB_PO_NO'.
        "Text-105 : Sub P/O No.
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-105
                 CHANGING <L_FIELDCAT>.
      WHEN 'HEAD_DIS_KWERT'.
        "Text-106 : Header Discount %
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-106
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.
        <L_FIELDCAT>-DECIMALS_O = LC_DECIMALS_O_2.

      WHEN 'CASH_DIS_KWERT'.
        "Text-107 : Cash Discount %
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-107
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.
        <L_FIELDCAT>-DECIMALS_O = LC_DECIMALS_O_2.

      WHEN 'UNIT_PRICE'.
        "Text-108 : Price/Unit
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-108
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.
        <L_FIELDCAT>-DECIMALS_O = LC_DECIMALS_O_2.

      WHEN 'UNIT_NET_PRICE'.
        "Text-109 : Net Price/Unit
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-109
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.
        <L_FIELDCAT>-DECIMALS_O = LC_DECIMALS_O_2.

      WHEN 'BOM_AMOUNT'.
        "Text-110 : Amount Of BOM
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-110
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.
        <L_FIELDCAT>-DECIMALS_O = LC_DECIMALS_O_2.

      WHEN 'BOM_TOTAL_AMOUNT'.
        "Text-111 : Total Amount Of BOM
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-111
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.
        <L_FIELDCAT>-DECIMALS_O = LC_DECIMALS_O_2.

      WHEN 'NET_AMOUNT'.
        "Text-112 : Net Amount
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-112
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.
        <L_FIELDCAT>-DECIMALS_O = LC_DECIMALS_O_2.

      WHEN 'TAX_AMOUNT'.
        "Text-113 : Tax Amount
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-113
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.
        <L_FIELDCAT>-DECIMALS_O = LC_DECIMALS_O_2.

      WHEN 'TOTAL_AMOUNT'.
        "Text-114 : Total Amount
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-114
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.
        <L_FIELDCAT>-DECIMALS_O = LC_DECIMALS_O_2.

      WHEN 'UNIT_COST'.
        "Text-115 : Cost/Unit
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-115
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.
        <L_FIELDCAT>-DECIMALS_O = LC_DECIMALS_O_2.

      WHEN 'TOTAL_COST'.
        "Text-116 : Total Cost
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-116
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.
        <L_FIELDCAT>-DECIMALS_O = LC_DECIMALS_O_2.

      WHEN 'BOM_TOTAL_COST'.
        "Text-117 : Total Cost Of BOM
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-117
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.
        <L_FIELDCAT>-DECIMALS_O = LC_DECIMALS_O_2.

      WHEN 'SALE_AMOUNT_X1000'.
        "Text-118 : Sales Amount x 1000
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-118
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.
        <L_FIELDCAT>-DECIMALS_O = LC_DECIMALS_O_2.

      WHEN 'QUOTE_WORK_COND'.
        "Text-119 : Work Condition QT
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-119
                 CHANGING <L_FIELDCAT>.
      WHEN 'JOB_COMP_DATE'.
        "Text-120 : Job Completion Date
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-120
                 CHANGING <L_FIELDCAT>.
      WHEN 'SERV_ORDER'.
        "Text-121 : Service Order
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-121
                 CHANGING <L_FIELDCAT>.
      WHEN 'SERV_ORDER_WBS'.
        "Text-122 : Service Order WBS
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-122
                 CHANGING <L_FIELDCAT>.
      WHEN 'SERV_ORDER_STATUS'.
        "Text-123 : Service Order Status
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-123
                 CHANGING <L_FIELDCAT>.
      WHEN 'CAUSE_FAIL'.
        "Text-124 : Cause of Failure
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-124
                 CHANGING <L_FIELDCAT>.
      WHEN 'CLOSE_DATE'.
        "Text-125 : Closed Date of all Process
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-125
                 CHANGING <L_FIELDCAT>.
      WHEN 'PLACE_FAIL'.
        "Text-126 : Place of Failure"
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-126
                 CHANGING <L_FIELDCAT>.
      WHEN 'RECEPTION_DATE'.
        "Text-127 : Reception Date
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-127
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO = GC_TRUE.

      WHEN 'SERV_SUB_CONT'.
        "Text-128 : Service subcontract team
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-128
                 CHANGING <L_FIELDCAT>.
      WHEN 'SERV_CONT_STARTDATE'.
        "Text-129 : Service Contract Start Date
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-129
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO = GC_TRUE.

      WHEN 'SERV_CONT_ENDDATE'.
        "Text-130 : Service Contract End Date
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-130
                 CHANGING <L_FIELDCAT>.

        <L_FIELDCAT>-NO_ZERO = GC_TRUE.

      WHEN 'MGMT_NO'.
        "Text-131 : Management Number(After Service)
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-131
                 CHANGING <L_FIELDCAT>.
      WHEN 'PROJ_NAME'.
        "Text-132 : Project Name
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-132
                 CHANGING <L_FIELDCAT>.
      WHEN 'ADV_RECEIVE'.
        "Text-133 : Advance Receive
        PERFORM F_SET_TEXT_FIELDCAT USING TEXT-133
                 CHANGING <L_FIELDCAT>.
    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT  CHANGING CT_SORT TYPE LVC_T_SORT.

* Initialize Output
  CLEAR: CT_SORT.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_top_of_page_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING UREF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '18%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '82%'.

  DATA:
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

*-----------------------
* Add value in Line1
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h01 : Report:
  LF_TEXT = TEXT-H01.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  LF_TEXT = SY-TITLE.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_print_top_of_page_1
*----------------------------------------------------------------------*
*       Print Top of Page
*----------------------------------------------------------------------*
FORM F_PRINT_TOP_OF_PAGE_1.                                 "#EC CALLED

  DATA:
    LF_COL01 TYPE  I VALUE 18,
    LF_COL02 TYPE  I VALUE 35.


* Text-h01 : Report:
  WRITE AT: /1(LF_COL01)  TEXT-H01 INTENSIFIED ON NO-GAP,
            (LF_COL02)    SY-TITLE NO-GAP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_SET_TEXT_FIELDCAT
*&---------------------------------------------------------------------*
*& Set text for FieldCat
*&---------------------------------------------------------------------*
FORM F_SET_TEXT_FIELDCAT  USING    UF_TEXT TYPE CLIKE
                          CHANGING CS_FIELDCAT TYPE LVC_S_FCAT.

  DATA:
    LF_TEXT TYPE TEXT50.

  LF_TEXT = UF_TEXT.

  CS_FIELDCAT-COLTEXT   = LF_TEXT.
  CS_FIELDCAT-SCRTEXT_L = LF_TEXT.
  CS_FIELDCAT-SCRTEXT_M = LF_TEXT.
  CS_FIELDCAT-SCRTEXT_S = LF_TEXT.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_HOTSPOT_CLICK_1
*&---------------------------------------------------------------------*
*& Hotspot Click for ALV Report
*&---------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK_1 USING US_ROW TYPE LVC_S_ROW ##CALLED
                             US_COL TYPE LVC_S_COL ##NEEDED.

  IF US_ROW-INDEX IS INITIAL.
    RETURN.
  ENDIF.

  READ TABLE GT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>) INDEX US_ROW-INDEX.
  IF SY-SUBRC IS INITIAL.
    SET PARAMETER ID 'VF' FIELD <L_RESULT>-BILL_NO.

    TRY.
        CALL TRANSACTION 'VF03' WITH AUTHORITY-CHECK
                                AND SKIP FIRST SCREEN.
      CATCH CX_SY_AUTHORIZATION_ERROR.
        MESSAGE S172(00) WITH 'VF03'.
    ENDTRY.

  ENDIF.

ENDFORM.

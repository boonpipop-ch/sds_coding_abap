class ZCL_SDSSD_QUOTA_CHECK_SERV definition
  public
  inheriting from ZCL_SDSCA_REST_SERVICE
  final
  create public .

public section.

  constants GC_SUCCESS type ZSDSDE_INF_STATUS value 'S' ##NO_TEXT.
  constants GC_ERROR type ZSDSDE_INF_STATUS value 'E' ##NO_TEXT.

  methods PROCESS_DATA
    redefinition .
protected section.
private section.

  class-methods GET_QUOTA_DATA
    importing
      !IS_REQUEST type ZSDSSDS104
    exporting
      !ES_RESPONSE type ZSDSSDS104 .
  class-methods CONVERT_CUNIT_OUTPUT
    importing
      !IF_UNIT type MEINS
    exporting
      !EF_UNIT type MEINS .
ENDCLASS.



CLASS ZCL_SDSSD_QUOTA_CHECK_SERV IMPLEMENTATION.


METHOD CONVERT_CUNIT_OUTPUT.
*-----------------------------------------------------------------------
*  Class              : ZCL_SDSSD_QUOTA_CHECK_SERV / CONVERT_CUNIT_OUTPUT
*  Creation Date      : 25.09.2024
*  Author             : Waraporn S.(Eviden)
*  Add-on ID          : SDI037
*  Description        : Conversion unit of measure to output format
*  Purpose            : Conversion unit of measure to output format
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  CLEAR: EF_UNIT.

  IF IF_UNIT IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      INPUT          = IF_UNIT
    IMPORTING
      OUTPUT         = EF_UNIT
    EXCEPTIONS
      UNIT_NOT_FOUND = 1
      OTHERS         = 2.
  IF SY-SUBRC NE 0.
    EF_UNIT = IF_UNIT.
  ENDIF.

ENDMETHOD.


METHOD GET_QUOTA_DATA.
*-----------------------------------------------------------------------
*  Class              : ZCL_SDSSD_QUOTA_CHECK_SERV / GET_QUOTA_DATA
*  Creation Date      : 25.09.2024
*  Author             : Waraporn S.(Eviden)
*  Add-on ID          : SDI037
*  Description        : This is a Processing class of REST interface
*                       SDI037 to check quota of material
*  Purpose            : To check quota for each material
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
    LT_FILTER TYPE /IWBEP/T_MGW_SELECT_OPTION,
    LT_MATNR  TYPE RANGE OF MARA-MATNR,
    LT_RESULT TYPE ZSDSSDS053_TT.

  DATA: LS_QUOTAGROUP TYPE ZSDSSDS106.

  CLEAR: ES_RESPONSE.

  ES_RESPONSE = IS_REQUEST.

* Prepare material number list from request parameter
  CLEAR: LT_FILTER, LT_MATNR.
  LOOP AT IS_REQUEST-OBJECT ASSIGNING FIELD-SYMBOL(<L_REQUEST>).
    APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = <L_REQUEST>-MATNR )
    TO LT_MATNR.
  ENDLOOP.

  APPEND VALUE #( PROPERTY = 'MATNR'
                  SELECT_OPTIONS = CORRESPONDING #( LT_MATNR ) )
            TO LT_FILTER.
  IF LT_FILTER IS INITIAL.
    RETURN.
  ENDIF.

* Call Class method to retrieve quota report information
  CALL METHOD ZCL_SDSSD_QUOTA_PLAN_UTIL=>GET_QUOTA_REPORT
    EXPORTING
      IT_FILTER = LT_FILTER
    IMPORTING
      ET_RESULT = LT_RESULT.

* Prepare detail to response structure
  LOOP AT ES_RESPONSE-OBJECT ASSIGNING FIELD-SYMBOL(<L_RESPONSE>).

*   Group Data by Material number
    LOOP AT LT_RESULT INTO DATA(LS_RESULT)        ##INTO_OK
                      WHERE MATNR = <L_RESPONSE>-MATNR
                      GROUP BY ( MATNR = LS_RESULT-MATNR ) ASCENDING
                      ASSIGNING FIELD-SYMBOL(<L_GROUP>).

      <L_RESPONSE>-RESP_STATUS = GC_SUCCESS.
      <L_RESPONSE>-RESP_MESSAGE = TEXT-I01.    "Success
      CLEAR: <L_RESPONSE>-QUOTAGROUP.

      LOOP AT GROUP <L_GROUP> ASSIGNING FIELD-SYMBOL(<L_DATA>).
        CLEAR: LS_QUOTAGROUP.

        <L_RESPONSE>-MAKTX = <L_DATA>-MAKTX.
        CONVERT_CUNIT_OUTPUT( EXPORTING IF_UNIT = <L_DATA>-MEINS
                              IMPORTING EF_UNIT = <L_RESPONSE>-MEINS ).
        <L_RESPONSE>-TOTAL_QTY = <L_DATA>-00_TOTAL_QTY.
        <L_RESPONSE>-URSTOCK = <L_DATA>-URSTOCK.
        <L_RESPONSE>-PO_QTY = <L_DATA>-00_PO_QTY.
        <L_RESPONSE>-QUOTA_QTY = <L_DATA>-00_QUOTA_QTY.
        <L_RESPONSE>-FREE_QTY = <L_DATA>-00_FREE_QTY.

        LS_QUOTAGROUP-QUOTAGRP_DESC = <L_DATA>-00_QUOTAGRP_DESC.
        LS_QUOTAGROUP-00_ALLOC_QUOTA = <L_DATA>-00_ALLOC_QUOTA.
        LS_QUOTAGROUP-00_SO_QTY = <L_DATA>-00_SO_QTY.
        LS_QUOTAGROUP-00_REMAIN_QUOTA = <L_DATA>-00_REMAIN_QUOTA.

        ZCL_SDSCA_REST_INTF_UTILITY=>CONVERT_DATE_TIME_TO_ISO(
          EXPORTING
            IF_DATUM  = <L_DATA>-00_DAY_LAST
          IMPORTING
            EF_OUTPUT = LS_QUOTAGROUP-00_DAY_LAST ).

        LS_QUOTAGROUP-01_ALLOC_QUOTA = <L_DATA>-01_ALLOC_QUOTA.
        LS_QUOTAGROUP-01_SO_QTY = <L_DATA>-01_SO_QTY.
        LS_QUOTAGROUP-01_REMAIN_QUOTA = <L_DATA>-01_REMAIN_QUOTA.

        ZCL_SDSCA_REST_INTF_UTILITY=>CONVERT_DATE_TIME_TO_ISO(
          EXPORTING
            IF_DATUM  = <L_DATA>-01_DAY_LAST
          IMPORTING
            EF_OUTPUT = LS_QUOTAGROUP-01_DAY_LAST ).

        INSERT LS_QUOTAGROUP INTO TABLE <L_RESPONSE>-QUOTAGROUP.
      ENDLOOP.
    ENDLOOP.
    IF SY-SUBRC NE 0.
      <L_RESPONSE>-RESP_STATUS = GC_ERROR.
      <L_RESPONSE>-RESP_MESSAGE = TEXT-I02.    "No Data Found
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD PROCESS_DATA.
*-----------------------------------------------------------------------
*  Class              : ZCL_SDSSD_QUOTA_CHECK_SERV / PROCESS_DATA
*  Creation Date      : 25.09.2024
*  Author             : Waraporn S.(Eviden)
*  Add-on ID          : SDI037
*  Description        : This is a Processing class of REST interface
*                       SDI037 to check quota of material
*  Purpose            : To check quota for each material
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
    LS_REQUEST TYPE ZSDSSDS104.

  FIELD-SYMBOLS:
    <L_RESPONSE> TYPE ZSDSSDS104,
    <L_OBJECT>   TYPE ZSDSSDS105.

* Initialize Output
  CLEAR: EF_STATUS,
         EF_MESSAGE,
         EF_HTTP_ERROR.

  LS_REQUEST = IREF_REQUEST_DATA->*.

  ASSIGN EREF_RESPONSE_DATA->* TO <L_RESPONSE>.
  IF SY-SUBRC NE 0.
*   Critical error
    RETURN.
  ENDIF.

* Process the request and update response
  ZCL_SDSSD_QUOTA_CHECK_SERV=>GET_QUOTA_DATA(
    EXPORTING
      IS_REQUEST  = LS_REQUEST
    IMPORTING
      ES_RESPONSE = <L_RESPONSE> ).

* Set response status to EF_STATUS, EF_MESSAGE
* Capture the first success from response data
  READ TABLE <L_RESPONSE>-OBJECT ASSIGNING <L_OBJECT>
                                 WITH KEY RESP_STATUS = GC_SUCCESS.
  IF SY-SUBRC EQ 0.
    EF_STATUS = <L_OBJECT>-RESP_STATUS.
    EF_MESSAGE = <L_OBJECT>-RESP_MESSAGE.
  ELSE.
*   If no any record in sucess, then capture the first error message
    READ TABLE <L_RESPONSE>-OBJECT ASSIGNING <L_OBJECT>
                                   WITH KEY RESP_STATUS = GC_ERROR.
    IF SY-SUBRC EQ 0.
      EF_STATUS = <L_OBJECT>-RESP_STATUS.
      EF_MESSAGE = <L_OBJECT>-RESP_MESSAGE.
    ENDIF.
  ENDIF.

ENDMETHOD.
ENDCLASS.

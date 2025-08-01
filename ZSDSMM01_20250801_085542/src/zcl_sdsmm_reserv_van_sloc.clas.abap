class ZCL_SDSMM_RESERV_VAN_SLOC definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF TS_RESERVATION_DB,
        RSNUM TYPE RESB-RSNUM,
        RSPOS TYPE RESB-RSPOS,
        RSART TYPE RESB-RSART,
        XLOEK TYPE RESB-XLOEK,
        KZEAR TYPE RESB-KZEAR,
        MATNR TYPE RESB-MATNR,
        WERKS TYPE RESB-WERKS,
        LGORT TYPE RESB-LGORT,
        UMWRK TYPE RKPF-UMWRK,
        UMLGO TYPE RKPF-UMLGO,
        BDMNG TYPE RESB-BDMNG,
        MEINS TYPE RESB-MEINS,
      END OF TS_RESERVATION_DB .
  types TS_RESERVATIONITEM type ZCL_ZSDSMM_RESERV_VAN_MPC_EXT=>TS_RESERVATIONITEM .
  types TS_RESPONSE type ZCL_ZSDSMM_RESERV_VAN_MPC_EXT=>TS_RESPONSE .
  types:
    TT_RESERVATIONITEM TYPE STANDARD TABLE OF TS_RESERVATIONITEM WITH DEFAULT KEY .
  types:
    TT_RESPONSE TYPE STANDARD TABLE OF TS_RESPONSE WITH DEFAULT KEY .
  types TS_RESERVATION type ZCL_ZSDSMM_RESERV_VAN_MPC_EXT=>TS_RESERVATION .
  types TS_INTERFACE_LOG type ZSDSMMT014 .
  types:
    TT_INTERFACE_LOG TYPE STANDARD TABLE OF TS_INTERFACE_LOG .
  types:
    BEGIN OF TS_MATERIAL_PLANT,
        MATERIAL TYPE MARD-MATNR,
        PLANT    TYPE MARD-WERKS,
      END OF TS_MATERIAL_PLANT .
  types:
    TT_MATERIAL_PLANT TYPE STANDARD TABLE OF TS_MATERIAL_PLANT .

  constants GC_CREATE type ZSDSDE_MODE value 'C' ##NO_TEXT.
  constants GC_UPDATE type ZSDSDE_MODE value 'U' ##NO_TEXT.
  constants GC_SUCCESS type ZSDSDE_INF_STATUS value 'S' ##NO_TEXT.
  constants GC_ERROR type ZSDSDE_INF_STATUS value 'E' ##NO_TEXT.
  constants GC_WARNING type ZSDSDE_INF_STATUS value 'W' ##NO_TEXT.
  constants GC_COMMA type CHAR1 value ':' ##NO_TEXT.
  constants GC_MVT_TRF type BWART value '311' ##NO_TEXT.
  constants GC_RECTYPE_HDR type CHAR1 value 'H' ##NO_TEXT.
  constants GC_RECTYPE_ITM type CHAR1 value 'I' ##NO_TEXT.

  class-methods RESERVATION_PROCESS
    importing
      !IF_TEST type FLAG optional
    changing
      !CS_RESERVATION_HDR type TS_RESERVATION
      !CT_RESERVATION_ITM type TT_RESERVATIONITEM
      !CT_RESPONSE type TT_RESPONSE .
  class-methods CREATE_RESERVATION
    importing
      !IF_TEST type FLAG
      !IT_RESERVATION_ITM type TT_RESERVATIONITEM
    changing
      !CS_RESERVATION_HDR type TS_RESERVATION
      !CT_INTERFACE_LOG type TT_INTERFACE_LOG
      !CT_RESPONSE type TT_RESPONSE .
  class-methods UPDATE_RESERVATION
    importing
      !IF_TEST type FLAG
      !IT_RESERVATION_ITM type TT_RESERVATIONITEM
    changing
      !CS_RESERVATION_HDR type TS_RESERVATION
      !CT_INTERFACE_LOG type TT_INTERFACE_LOG
      !CT_RESPONSE type TT_RESPONSE .
  class-methods CHECK_MATERIAL_AVAILABILITY
    importing
      !IF_MATNR type MATNR
      !IF_PLANT type BAPIMATVP-WERKS
      !IF_STGE_LOC type BAPICM61V-LGORT
      !IF_UNIT type BAPIADMM-UNIT
      !IF_CHECK_RULE type BAPIT441V-PRREG default '03'
    exporting
      !EF_COM_QTY type MNG06 .
protected section.
private section.

  class-data GRT_PLANT type RANGE_WERKS .

  class-methods GET_CONSTANTS
    exporting
      !ET_PLANT type MMPURUI_WERKS_RANGE_TTY .
  class-methods GET_RESERVATION_DB
    importing
      !IF_RSNUM type TS_RESERVATIONITEM-RESERVATION_NO
      !IF_RSPOS type TS_RESERVATIONITEM-RESERVATION_ITEM optional
    exporting
      !ES_RESERVATION_DB type TS_RESERVATION_DB .
  class-methods GET_MATERIAL_DESCRIPTION
    importing
      !IF_MATNR type MATNR
    exporting
      !EF_MAKTX type MAKTX .
  class-methods VALIDATE_INPUT
    exporting
      !ET_INTERFACE_LOG type TT_INTERFACE_LOG
    changing
      !CS_RESERVATION_HDR type TS_RESERVATION
      !CT_RESERVATION_ITM type TT_RESERVATIONITEM
      !CT_RESPONSE type TT_RESPONSE .
  class-methods VALIDATE_INPUT_SINGLE
    importing
      !IS_RESERVATION_ITM type TS_RESERVATIONITEM
      !IF_UPDATEMODE type ZSDSDE_MODE
    exporting
      !EF_INVALID type FLAG
      !EF_MESSAGE type BAPI_MSG
      !ES_RESERVATION_DB type TS_RESERVATION_DB
    changing
      !CS_RESERVATION_HDR type TS_RESERVATION
      !CT_RESPONSE type TT_RESPONSE .
  class-methods VALIDATE_WITH_RESB
    importing
      !IS_RESERVATION_ITM type TS_RESERVATIONITEM
    exporting
      !EF_INVALID type FLAG
      !EF_MESSAGE type BAPI_MSG
      !ES_RESERVATION_DB type TS_RESERVATION_DB
    changing
      !CS_RESERVATION_HDR type TS_RESERVATION
      !CT_RESPONSE type TT_RESPONSE .
  class-methods REDETERMINE_QUANTITY
    importing
      !IF_UPDATEMODE type ZSDSDE_MODE
      !IF_QUANTITY_IN type TS_RESERVATIONITEM-QUANTITY
      !IF_AVAILABLE_QTY type BAPIWMDVE-COM_QTY
      !IS_RESERVATION_DB type TS_RESERVATION_DB optional
    changing
      !CF_QUANTITY_UPD type TS_RESERVATIONITEM-QUANTITY .
  class-methods APPEND_RESPONSE
    importing
      !IF_STATUS type CHAR1
      !IF_MESSAGE type BAPI_MSG
      !IF_ITEM_NO type TS_RESERVATIONITEM-SALESFORCE_ITEM_NO optional
    changing
      !CT_RESPONSE type TT_RESPONSE
      !CS_RESERVATION_HDR type TS_RESERVATION optional .
  class-methods CONVERT_MATN1_INPUT
    importing
      !IF_MATNR type MATNR
    exporting
      !EF_MATNR type MATNR .
  class-methods CONVERT_CUNIT_INPUT
    importing
      !IF_UNIT type MEINS
    exporting
      !EF_UNIT type MEINS .
  class-methods UPDATE_INTERFACE_LOG
    importing
      !IT_INTERFACE_LOG type TT_INTERFACE_LOG .
  class-methods EXTEND_MATERIAL_SLOC
    importing
      !IF_SLOC type UMLGO
      !IT_MATERIAL_PLANT type TT_MATERIAL_PLANT .
ENDCLASS.



CLASS ZCL_SDSMM_RESERV_VAN_SLOC IMPLEMENTATION.


METHOD APPEND_RESPONSE.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_RESERV_VAN_SLOC / APPEND_RESPONSE
*  Creation Date      : 14.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MMI029
*  Description        : Append response status and message to response table
*  Purpose            : To append response to CT_RESPONSE
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  DATA: LF_MESSAGE TYPE BAPI_MSG.
  DATA: LS_RESPONSE TYPE TS_RESPONSE.

* Item XXXX:
* If item no is specified, then concatenate text "Item xxxx:" before message
  IF IF_ITEM_NO IS NOT INITIAL.
    CONCATENATE TEXT-E08
                IF_ITEM_NO
                GC_COMMA
                IF_MESSAGE
                INTO LF_MESSAGE SEPARATED BY SPACE.
  ELSE.
    LF_MESSAGE = IF_MESSAGE.
  ENDIF.
  LS_RESPONSE-SALESFORCE_NO = CS_RESERVATION_HDR-SALESFORCE_NO.
  LS_RESPONSE-RESPONSE_STATUS = IF_STATUS.
  LS_RESPONSE-RESPONSE_MESSAGE = LF_MESSAGE.

  APPEND LS_RESPONSE TO CT_RESPONSE.

* Update the latest message to CS_RESERVATION_HDR
  IF IF_ITEM_NO IS INITIAL.
    CS_RESERVATION_HDR-RESPONSE_STATUS = IF_STATUS.
    CS_RESERVATION_HDR-RESPONSE_MESSAGE = LF_MESSAGE.
  ENDIF.

ENDMETHOD.


METHOD CHECK_MATERIAL_AVAILABILITY.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_RESERV_VAN_SLOC / CHECK_MATERIAL_AVAILABILITY
*  Creation Date      : 14.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MMI029
*  Description        : Check material availability
*  Purpose            : Perform material availablity check and return
*                       available quantity using BAPI_MATERIAL_AVAILABILITY
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  DATA: LF_MATNR  TYPE MATNR18,
        LT_WMDVSX TYPE TABLE OF BAPIWMDVS,
        LT_WMDVEX TYPE TABLE OF BAPIWMDVE.

  CLEAR: EF_COM_QTY.

  LF_MATNR = IF_MATNR.

  CALL FUNCTION 'BAPI_MATERIAL_AVAILABILITY'
    EXPORTING
      PLANT      = iF_PLANT
      MATERIAL   = lF_MATNR
      UNIT       = IF_UNIT
      CHECK_RULE = IF_CHECK_RULE
      STGE_LOC   = IF_STGE_LOC
    TABLES
      WMDVSX     = LT_WMDVSX
      WMDVEX     = LT_WMDVEX.

  READ TABLE LT_WMDVEX INTO DATA(LS_WMDVEX) INDEX 1.
  IF SY-SUBRC = 0.
*   Return available quantity
    EF_COM_QTY = LS_WMDVEX-COM_QTY.
  ENDIF.

ENDMETHOD.


METHOD CONVERT_CUNIT_INPUT.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_RESERV_VAN_SLOC / CONVERT_CUNIT_INPUT
*  Creation Date      : 14.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MMI029
*  Description        : Conversion unit of measure to intenal format
*  Purpose            : Conversion unit of measure to intenal format
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
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
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


METHOD CONVERT_MATN1_INPUT.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_RESERV_VAN_SLOC / CONVERT_MATN1_INPUT
*  Creation Date      : 14.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MMI029
*  Description        : Conversion material number to intenal format
*  Purpose            : Conversion material number to intenal format
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  CLEAR: EF_MATNR.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      INPUT        = IF_MATNR
    IMPORTING
      OUTPUT       = EF_MATNR
    EXCEPTIONS
      LENGTH_ERROR = 1
      OTHERS       = 2.

  IF SY-SUBRC NE 0.
    EF_MATNR = IF_MATNR.
  ENDIF.
ENDMETHOD.


METHOD CREATE_RESERVATION.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_RESERV_VAN_SLOC / CREATE_RESERVATION
*  Creation Date      : 14.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MMI029
*  Description        : Create reservation
*  Purpose            : To create reservation according to input data
*                       from Salesforce
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  05.11.2024  F36K908235  Waraporn S. To extend material storage location
*                                      before creating a reservation
*                                      (if receiving sloc not found)
*-----------------------------------------------------------------------

  DATA: LS_HEADER         TYPE BAPI2093_RES_HEAD,
        LS_ITEM           TYPE BAPI2093_RES_ITEM,
        LS_RETURN         TYPE BAPIRET2,
        LF_ATPCHECK       TYPE BAPI2093_ATPCHECK VALUE SPACE,
        LF_TEST           TYPE BAPI2093_TEST,
        LF_RESERVATION_NO TYPE BAPI2093_RES_KEY-RESERV_NO,
        LF_MESSAGE        TYPE BAPI_MSG,
        LF_ITEM_NO        TYPE RSNUM,
        LT_ITEM           TYPE STANDARD TABLE OF BAPI2093_RES_ITEM,
        LT_PROFIT_SEGMENT TYPE STANDARD TABLE OF BAPI_PROFITABILITY_SEGMENT,
        LT_RETURN         TYPE STANDARD TABLE OF BAPIRET2,
        LT_ITEM_SLOC      TYPE TT_MATERIAL_PLANT.

  CLEAR: LS_HEADER, LS_RETURN, LF_RESERVATION_NO,
         LT_ITEM, LT_PROFIT_SEGMENT, LT_RETURN.

  LF_TEST = IF_TEST.

  CHECK CT_INTERFACE_LOG IS NOT INITIAL.

* Prepare reservation header data
  LS_HEADER-RES_DATE = SY-DATUM.
  LS_HEADER-CREATED_BY = SY-UNAME.
  LS_HEADER-MOVE_TYPE = CT_INTERFACE_LOG[ 1 ]-MOVEMENT_TYPE.
  LS_HEADER-MOVE_PLANT = CT_INTERFACE_LOG[ 1 ]-RECEIVING_PLANT.
  LS_HEADER-MOVE_STLOC = CT_INTERFACE_LOG[ 1 ]-RECEIVING_SLOC.

* Prepare reservation item data
  CLEAR LF_ITEM_NO.
  LOOP AT CT_INTERFACE_LOG ASSIGNING FIELD-SYMBOL(<L_INTERFACE_LOG>)
                           WHERE SALESFORCE_ITEM_NO IS NOT INITIAL
                             AND RESPONSE_STATUS NE GC_ERROR.
    CLEAR: LS_ITEM.
    LF_ITEM_NO = LF_ITEM_NO + 1.
    <L_INTERFACE_LOG>-RESERVATION_ITEM = LF_ITEM_NO.

    LS_ITEM-MATERIAL = <L_INTERFACE_LOG>-MATERIAL_CODE.
    LS_ITEM-PLANT = <L_INTERFACE_LOG>-PLANT.
    LS_ITEM-STGE_LOC = <L_INTERFACE_LOG>-ISSUE_SLOC.
    LS_ITEM-ENTRY_QNT = <L_INTERFACE_LOG>-QUANTITY.
    LS_ITEM-ENTRY_UOM = <L_INTERFACE_LOG>-UNIT.
    LS_ITEM-MOVEMENT = 'X'.

    APPEND LS_ITEM TO LT_ITEM.
  ENDLOOP.

* To check and extend material master storage location data
  CLEAR: LT_ITEM_SLOC.
  LT_ITEM_SLOC[] = CORRESPONDING #( LT_ITEM ).

  SORT LT_ITEM_SLOC BY MATERIAL PLANT.
  DELETE ADJACENT DUPLICATES FROM LT_ITEM COMPARING MATERIAL PLANT.

  IF LT_ITEM_SLOC IS NOT INITIAL.
*   Check and extend material to receiving SLOC

  ENDIF.

  IF LS_HEADER IS NOT INITIAL AND
     LT_ITEM IS NOT INITIAL.

    CALL FUNCTION 'BAPI_RESERVATION_CREATE1'
      EXPORTING
        RESERVATIONHEADER    = LS_HEADER
        TESTRUN              = LF_TEST
        ATPCHECK             = LF_ATPCHECK
      IMPORTING
        RESERVATION          = LF_RESERVATION_NO
      TABLES
        RESERVATIONITEMS     = LT_ITEM
        PROFITABILITYSEGMENT = LT_PROFIT_SEGMENT
        RETURN               = LT_RETURN.

    IF LF_RESERVATION_NO IS NOT INITIAL.
*     Success
      IF IF_TEST EQ ABAP_FALSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = ABAP_TRUE.
      ENDIF.

      READ TABLE LT_RETURN INTO LS_RETURN
                           WITH KEY TYPE = GC_SUCCESS.
      IF SY-SUBRC EQ 0.
        LF_MESSAGE = LS_RETURN-MESSAGE.
*       Update the created reservation number to header response
        CS_RESERVATION_HDR-RESPONSE_RESERVATION_NO = LF_RESERVATION_NO.

*       Update the created reservation number to interface log
        MODIFY CT_INTERFACE_LOG FROM VALUE #( RESERVATION_NO = LF_RESERVATION_NO )
          TRANSPORTING RESERVATION_NO
          WHERE RECORDTYPE EQ GC_RECTYPE_ITM
            AND RESPONSE_STATUS <> GC_ERROR.

        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_SUCCESS
                                   IF_MESSAGE = LF_MESSAGE
                          CHANGING CT_RESPONSE = CT_RESPONSE
                                   CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
      ENDIF.

    ELSE.   "No reservation created. (either has error or in Test run mode)
*     Error or test run
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

*     Clear reservation item number in the interface log
      MODIFY CT_INTERFACE_LOG FROM VALUE #( RESERVATION_ITEM = '0000' )
        TRANSPORTING RESERVATION_ITEM WHERE RESERVATION_ITEM IS NOT INITIAL.

      LOOP AT LT_RETURN INTO LS_RETURN
                        WHERE TYPE = GC_ERROR.
        LF_MESSAGE = LS_RETURN-MESSAGE.

        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = LF_MESSAGE
                          CHANGING CT_RESPONSE = CT_RESPONSE
                                   CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
      ENDLOOP.
      IF SY-SUBRC NE 0.    "No error message return from BAPI
        IF IF_TEST EQ ABAP_TRUE.
*         Text E15: Function performed in Test run mode: Reservation can be created
          LF_MESSAGE = TEXT-E15.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_SUCCESS
                                     IF_MESSAGE = LF_MESSAGE
                            CHANGING CT_RESPONSE = CT_RESPONSE
                                     CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
*   Text E16: All items contained errors. Reservation could not be created
    LF_MESSAGE = TEXT-E16.
    APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                               IF_MESSAGE = LF_MESSAGE
                      CHANGING CT_RESPONSE = CT_RESPONSE
                               CS_RESERVATION_HDR = CS_RESERVATION_HDR ).

  ENDIF.

* Update interface log table for record type header
  CT_INTERFACE_LOG[ 1 ]-RESERVATION_NO = CS_RESERVATION_HDR-RESPONSE_RESERVATION_NO.
  CT_INTERFACE_LOG[ 1 ]-RESPONSE_STATUS = CS_RESERVATION_HDR-RESPONSE_STATUS.
  CT_INTERFACE_LOG[ 1 ]-RESPONSE_MESSAGE = CS_RESERVATION_HDR-RESPONSE_MESSAGE.

* Update interface log table for record type item
  MODIFY CT_INTERFACE_LOG FROM VALUE #(
      RESPONSE_STATUS = CS_RESERVATION_HDR-RESPONSE_STATUS
      RESPONSE_MESSAGE = CS_RESERVATION_HDR-RESPONSE_MESSAGE )
    TRANSPORTING RESPONSE_STATUS RESPONSE_MESSAGE
    WHERE RECORDTYPE EQ GC_RECTYPE_ITM
      AND RESPONSE_STATUS IS INITIAL.

ENDMETHOD.


METHOD EXTEND_MATERIAL_SLOC.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_RESERV_VAN_SLOC / EXTEND_MATERIAL_SLOC
*  Creation Date      : 05.11.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MMI029
*  Description        : Create reservation
*  Purpose            : To extend material storage location view
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*
*-----------------------------------------------------------------------
  DATA: LS_MATHEAD   TYPE  BAPIMATHEAD,
        LS_SLOCDATA  TYPE BAPI_MARD,
        LS_SLOCDATAX TYPE BAPI_MARDX,
        LS_RETURN    TYPE BAPIRET2.

  IF IF_SLOC IS INITIAL OR
     IT_MATERIAL_PLANT IS INITIAL.
    RETURN.
  ENDIF.

  IF IT_MATERIAL_PLANT IS NOT INITIAL.
    SELECT MATNR,
           WERKS,
           LGORT
      FROM MARD
      INTO TABLE @DATA(LT_MARD)
      FOR ALL ENTRIES IN @IT_MATERIAL_PLANT
      WHERE MATNR EQ @IT_MATERIAL_PLANT-MATERIAL
        AND WERKS EQ @IT_MATERIAL_PLANT-PLANT
        AND LGORT EQ @IF_SLOC
      ORDER BY PRIMARY KEY.

    LOOP AT IT_MATERIAL_PLANT ASSIGNING FIELD-SYMBOL(<L_MATERIAL_PLANT>).
      READ TABLE LT_MARD WITH KEY MATNR = <L_MATERIAL_PLANT>-MATERIAL
                                  WERKS = <L_MATERIAL_PLANT>-PLANT
                                  LGORT = IF_SLOC
                         BINARY SEARCH
                         TRANSPORTING NO FIELDS.
      IF SY-SUBRC NE 0.
*       Not found
        CLEAR: LS_MATHEAD, LS_SLOCDATA, LS_SLOCDATAX, LS_RETURN.

        LS_MATHEAD-MATERIAL = <L_MATERIAL_PLANT>-MATERIAL.
        LS_MATHEAD-STORAGE_VIEW = 'X'.

        LS_SLOCDATA-PLANT = <L_MATERIAL_PLANT>-PLANT.
        LS_SLOCDATA-STGE_LOC = IF_SLOC.

        LS_SLOCDATAX-PLANT = 'X'.
        LS_SLOCDATAX-STGE_LOC = 'X'.

        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
          EXPORTING
            HEADDATA         = LS_MATHEAD
            STORAGETYPEDATA  = LS_SLOCDATA
            STORAGETYPEDATAX = LS_SLOCDATAX
          IMPORTING
            RETURN           = LS_RETURN.

        IF LS_RETURN-TYPE EQ 'S'.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = ABAP_TRUE.

        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.

      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


METHOD GET_CONSTANTS.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_RESERV_VAN_SLOC / GET_CONSTANTS
*  Creation Date      : 14.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MMI029
*  Description        : Get general constants from table ZSDSCAC001
*  Purpose            : Get general constants from table ZSDSCAC001
*                       Maintenance view: ZSDSV_GEN_C
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  CLEAR: ET_PLANT.
  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZCL_SDSMM_RESERV_VAN_SLOC'
                                                  IF_PARAM = 'PLANT'
                                        IMPORTING ET_RANGE = ET_PLANT ).

ENDMETHOD.


METHOD GET_MATERIAL_DESCRIPTION.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_RESERV_VAN_SLOC / GET_MATERIAL_DESCRIPTION
*  Creation Date      : 14.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MMI029
*  Description        : Get material description
*  Purpose            : To return material description from MAKT
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  CLEAR: EF_MAKTX.

  IF IF_MATNR IS NOT INITIAL.
    SELECT SINGLE MAKTX
      FROM MAKT
      INTO @EF_MAKTX
      WHERE MATNR EQ @IF_matnr
        AND SPRAS EQ @SY-LANGU.
  ENDIF.

ENDMETHOD.


METHOD GET_RESERVATION_DB.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_RESERV_VAN_SLOC / GET_RESERVATION_DB
*  Creation Date      : 14.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MMI029
*  Description        : Get reservation data from RKPF, RESB
*  Purpose            : Retrive existing reservation detail from DB
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  DATA: LS_RESERVATION_DB TYPE TS_RESERVATION_DB.

  CLEAR: ES_RESERVATION_DB.

* Read existing reservation
  IF IF_RSNUM IS NOT INITIAL AND
     IF_RSPOS IS NOT INITIAL.
    SELECT I~RSNUM, I~RSPOS, I~RSART, I~XLOEK, I~KZEAR,
           I~MATNR, I~WERKS, I~LGORT, H~UMWRK, H~UMLGO,
           I~BDMNG, I~MEINS
      FROM RESB AS I
      INNER JOIN RKPF AS H
      ON ( I~RSNUM EQ H~RSNUM )
      WHERE I~RSNUM EQ @IF_RSNUM
        AND I~RSPOS EQ @IF_RSPOS
      ORDER BY I~RSNUM, I~RSPOS, I~RSART
      INTO @LS_RESERVATION_DB
      UP TO 1 ROWS.
    ENDSELECT.

  ELSEIF IF_RSNUM IS NOT INITIAL.
    SELECT SINGLE RSNUM, UMWRK, UMLGO
      FROM RKPF
      INTO CORRESPONDING FIELDS OF @LS_RESERVATION_DB
      WHERE RSNUM EQ @IF_RSNUM.

  ENDIF.

  IF SY-SUBRC EQ 0.
    ES_RESERVATION_DB = LS_RESERVATION_DB.
  ENDIF.

ENDMETHOD.


METHOD REDETERMINE_QUANTITY.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_RESERV_VAN_SLOC / REDETERMINE_QUANTITY
*  Creation Date      : 14.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MMI029
*  Description        : Redetermine reservation item quantity
*  Purpose            : Redetermine reservation item quantity
*                       based on available stock
*                       IF_QUANTITY_IN: Request quantity from Salesforce
*                       IF_AVAILABLE_QTY: Available quantity
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  DATA: LS_EXTRA_QTY TYPE TS_RESERVATIONITEM-QUANTITY.

  CF_QUANTITY_UPD = IF_QUANTITY_IN.

  CASE IF_UPDATEMODE.

    WHEN GC_CREATE.
* -------------------------------------
*   Record mode = C (Create reservation)
* -------------------------------------
      IF IF_AVAILABLE_QTY < IF_QUANTITY_IN.
*       Set reservation quantity = available quantity
        CF_QUANTITY_UPD = IF_AVAILABLE_QTY.
      ENDIF.

    WHEN GC_UPDATE.    "Update reservation
* -------------------------------------
*   Record mode = U (Update reservation)
* -------------------------------------
      IF IF_QUANTITY_IN > IS_RESERVATION_DB-BDMNG.
*       Request to update reservation with more quantity? -> Check with vailable stock
        LS_EXTRA_QTY = IF_QUANTITY_IN - IS_RESERVATION_DB-BDMNG.

        IF IF_AVAILABLE_QTY >= LS_EXTRA_QTY.
*         Available quantity > quantity to be added -> Ok to update
          CF_QUANTITY_UPD = IF_QUANTITY_IN.
        ELSE.
*         Available quantity < quantity to be added -> set quantity = existing + available
          CF_QUANTITY_UPD = IF_AVAILABLE_QTY.
        ENDIF.
      ELSE.
*       Request to update reservation with less quantity? -> Ok to update
        CF_QUANTITY_UPD = IF_QUANTITY_IN.
      ENDIF.
  ENDCASE.

ENDMETHOD.


METHOD RESERVATION_PROCESS.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_RESERV_VAN_SLOC / RESERVATION_PROCESS
*  Creation Date      : 14.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MMI029
*  Description        : Reservation create/update
*  Purpose            : Inbound interface from salesforce system to
*                       create or change reservation and update result to
*                       log table ZSDSMMT014
*                       Call from ODATA service project: ZSDSMM_RESERV_VAN
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  DATA: LT_INTERFACE_LOG TYPE TT_INTERFACE_LOG,
        LRT_PLANT        TYPE MMPURUI_WERKS_RANGE_TTY.

  CLEAR: CT_RESPONSE, LT_INTERFACE_LOG,
         CS_RESERVATION_HDR-RESPONSE_RESERVATION_NO,
         CS_RESERVATION_HDR-RESPONSE_STATUS,
         CS_RESERVATION_HDR-RESPONSE_MESSAGE.

* Get constant from table ZSDSCAC001
  GET_CONSTANTS( IMPORTING ET_PLANT = LRT_PLANT ).

* Check plant in scope
  IF LRT_PLANT IS NOT INITIAL AND
     CS_RESERVATION_HDR-PLANT NOT IN LRT_PLANT.

*   Text E19: The specified plant is not in scope
    APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                               IF_MESSAGE = TEXT-E19
                      CHANGING CT_RESPONSE = CT_RESPONSE
                               CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
    RETURN.
  ENDIF.

* Process input validation, check material availabilty and prepare interface log data
  VALIDATE_INPUT( IMPORTING ET_INTERFACE_LOG = LT_INTERFACE_LOG
                   CHANGING CS_RESERVATION_HDR = CS_RESERVATION_HDR
                            CT_RESERVATION_ITM = CT_RESERVATION_ITM
                            CT_RESPONSE = CT_RESPONSE ).

* Validation pass. Prepare to create or update reservation
  IF CS_RESERVATION_HDR-RESPONSE_STATUS NE GC_ERROR.
    CASE LT_INTERFACE_LOG[ 1 ]-UPDATEMODE.
      WHEN GC_CREATE.
        CREATE_RESERVATION( EXPORTING IF_TEST = IF_TEST
                                      IT_RESERVATION_ITM = CT_RESERVATION_ITM
                             CHANGING CS_RESERVATION_HDR = CS_RESERVATION_HDR
                                      CT_INTERFACE_LOG = LT_INTERFACE_LOG
                                      CT_RESPONSE = CT_RESPONSE ).
      WHEN GC_UPDATE.
        UPDATE_RESERVATION( EXPORTING IF_TEST = IF_TEST
                                      IT_RESERVATION_ITM = CT_RESERVATION_ITM
                             CHANGING CS_RESERVATION_HDR = CS_RESERVATION_HDR
                                      CT_INTERFACE_LOG = LT_INTERFACE_LOG
                                      CT_RESPONSE = CT_RESPONSE ).
    ENDCASE.
  ENDIF.

  IF IF_TEST EQ ABAP_FALSE.
    UPDATE_INTERFACE_LOG( EXPORTING IT_INTERFACE_LOG = LT_INTERFACE_LOG ).
  ENDIF.

ENDMETHOD.


METHOD UPDATE_INTERFACE_LOG.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_RESERV_VAN_SLOC / UPDATE_INTERFACE_LOG
*  Creation Date      : 14.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MMI029
*  Description        : Insert record to interface log table
*  Purpose            : Insert record to interface log table
*                       Table: ZSDSMMT014
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  IF IT_INTERFACE_LOG IS NOT INITIAL.
    MODIFY ZSDSMMT014 FROM TABLE IT_INTERFACE_LOG.
    IF SY-SUBRC EQ 0.
      COMMIT WORK.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD UPDATE_RESERVATION.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_RESERV_VAN_SLOC / UPDATE_RESERVATION
*  Creation Date      : 14.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MMI029
*  Description        : Update reservation
*  Purpose            : To change reservation according to input data
*                       from Salesforce
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  DATA: LF_RESERVATION_NO TYPE BAPI2093_RES_KEY-RESERV_NO,
        LF_TEST           TYPE BAPI2093_TEST,
        LF_ATPCHECK       TYPE BAPI2093_ATPCHECK VALUE SPACE,
        LF_MESSAGE        TYPE BAPI_MSG,
        LS_INTERFACE_LOG  TYPE TS_INTERFACE_LOG,
        LS_ITEM           TYPE BAPI2093_RES_ITEM_CHANGE,
        LS_ITEMX          TYPE BAPI2093_RES_ITEM_CHANGEX,
        LS_RETURN         TYPE BAPIRET2,
        LT_ITEM           TYPE STANDARD TABLE OF BAPI2093_RES_ITEM_CHANGE,
        LT_ITEMX          TYPE STANDARD TABLE OF BAPI2093_RES_ITEM_CHANGEX,
        LT_RETURN         TYPE STANDARD TABLE OF BAPIRET2.

  LF_TEST = IF_TEST.

  CHECK CT_INTERFACE_LOG IS NOT INITIAL.

* Fill in reservation number
  READ TABLE CT_INTERFACE_LOG WITH KEY RECORDTYPE = GC_RECTYPE_HDR
                              INTO LS_INTERFACE_LOG.
  IF SY-SUBRC EQ 0.
    LF_RESERVATION_NO = LS_INTERFACE_LOG-RESERVATION_NO.
    CS_RESERVATION_HDR-RESPONSE_RESERVATION_NO = LF_RESERVATION_NO.
  ENDIF.

* Fill in reservation item change
  LOOP AT CT_INTERFACE_LOG INTO LS_INTERFACE_LOG
                           WHERE SALESFORCE_ITEM_NO IS NOT INITIAL
                             AND RESERVATION_ITEM IS NOT INITIAL
                             AND RESPONSE_STATUS NE GC_ERROR.
    CLEAR: LS_ITEM, LS_ITEMX.
    LS_ITEM-RES_ITEM = LS_INTERFACE_LOG-RESERVATION_ITEM.
    LS_ITEMX-RES_ITEM = LS_ITEM-RES_ITEM.

    IF LS_INTERFACE_LOG-QUANTITY IS NOT INITIAL.
      LS_ITEM-ENTRY_QNT = LS_INTERFACE_LOG-QUANTITY.
      LS_ITEMX-ENTRY_QNT = 'X'.
    ENDIF.

    IF LS_INTERFACE_LOG-DELETED IS NOT INITIAL.
      LS_ITEM-DELETE_IND = LS_INTERFACE_LOG-DELETED.
      LS_ITEMX-DELETE_IND = 'X'.
    ENDIF.

    IF LS_INTERFACE_LOG-COMPLETED IS NOT INITIAL.
      LS_ITEM-WITHDRAWN = LS_INTERFACE_LOG-COMPLETED.
      LS_ITEMX-WITHDRAWN = 'X'.
    ENDIF.

    APPEND LS_ITEM TO LT_ITEM.

    APPEND LS_ITEMX TO LT_ITEMX.
  ENDLOOP.

  IF LF_RESERVATION_NO IS NOT INITIAL AND
     LT_ITEM IS NOT INITIAL.

    CALL FUNCTION 'BAPI_RESERVATION_CHANGE'
      EXPORTING
        RESERVATION               = LF_RESERVATION_NO
        TESTRUN                   = LF_TEST
        ATPCHECK                  = LF_ATPCHECK
      TABLES
        RESERVATIONITEMS_CHANGED  = LT_ITEM
        RESERVATIONITEMS_CHANGEDX = LT_ITEMX
        RETURN                    = LT_RETURN.

*   Read success message M7/070 Document & has been changed
    READ TABLE LT_RETURN INTO LS_RETURN
                         WITH KEY TYPE = GC_SUCCESS
                                  ID = 'M7'
                                  NUMBER = '070'.
    IF SY-SUBRC EQ 0.
*     Success
      IF IF_TEST EQ ABAP_FALSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = ABAP_TRUE.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

*     Update the result status and message to header response
      LF_MESSAGE = LS_RETURN-MESSAGE.

      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_SUCCESS
                                 IF_MESSAGE = LF_MESSAGE
                        CHANGING CT_RESPONSE = CT_RESPONSE
                                 CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
    ELSE.
*     Error
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      LOOP AT LT_RETURN INTO LS_RETURN
                        WHERE TYPE = GC_ERROR.
        LF_MESSAGE = LS_RETURN-MESSAGE.

        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = LF_MESSAGE
                          CHANGING CT_RESPONSE = CT_RESPONSE
                                   CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
      ENDLOOP.
    ENDIF.

  ELSE.
*   Text E18: All items contained errors. Reservation could not be updated
    LF_MESSAGE = TEXT-E18.
    APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                               IF_MESSAGE = LF_MESSAGE
                      CHANGING CT_RESPONSE = CT_RESPONSE
                               CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
  ENDIF.

* Update interface log table for record type header
  CT_INTERFACE_LOG[ 1 ]-RESERVATION_NO = CS_RESERVATION_HDR-RESPONSE_RESERVATION_NO.
  CT_INTERFACE_LOG[ 1 ]-RESPONSE_STATUS = CS_RESERVATION_HDR-RESPONSE_STATUS.
  CT_INTERFACE_LOG[ 1 ]-RESPONSE_MESSAGE = CS_RESERVATION_HDR-RESPONSE_MESSAGE.

* Update interface log table for record type item
  MODIFY CT_INTERFACE_LOG FROM VALUE #(
      RESPONSE_STATUS = CS_RESERVATION_HDR-RESPONSE_STATUS
      RESPONSE_MESSAGE = CS_RESERVATION_HDR-RESPONSE_MESSAGE )
    TRANSPORTING RESPONSE_STATUS RESPONSE_MESSAGE
    WHERE RECORDTYPE EQ GC_RECTYPE_ITM
      AND RESPONSE_STATUS IS INITIAL.

ENDMETHOD.


METHOD VALIDATE_INPUT.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_RESERV_VAN_SLOC / VALIDATE_INPUT
*  Creation Date      : 14.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MMI029
*  Description        : Input validation
*  Purpose            : 1. Validation request parameter received from
*                          Salesforce both header and item level
*                          and update the response parameters
*                       2. Check material availability of each item
*                          and update the available quantity in item
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  DATA: LS_INTERFACE_LOG   TYPE TS_INTERFACE_LOG,
        LS_RESERVATION_DB  TYPE TS_RESERVATION_DB,
        LF_UPDATEMODE      TYPE ZSDSDE_MODE,
        LF_INVALID         TYPE FLAG,
        LF_MESSAGE         TYPE BAPI_MSG,
        LF_QUANTITY_IN     TYPE TS_RESERVATIONITEM-QUANTITY,
        LF_AVAILABLE_QTY   TYPE BAPIWMDVE-COM_QTY,
        LF_ISSUE_SLOC      TYPE BAPICM61V-LGORT,
        LF_STOP_PROCESS    TYPE FLAG,
        LF_MATNR_INTERNAL  TYPE MATNR,
        LF_MEINS_INTERNAL  TYPE MEINS,
        LT_RESERVATION_ITM TYPE TT_RESERVATIONITEM.

  CLEAR: LS_INTERFACE_LOG, LF_STOP_PROCESS, LF_ISSUE_SLOC.

  LT_RESERVATION_ITM[] = CT_RESERVATION_ITM[].
  GET TIME.

* -------------------------------------
* Validation on HEADER level
* -------------------------------------
* Validate required fields in header
  IF CS_RESERVATION_HDR-SALESFORCE_NO IS INITIAL OR
     CS_RESERVATION_HDR-PLANT IS INITIAL OR
     CS_RESERVATION_HDR-MOVEMENT_TYPE IS INITIAL OR
     CS_RESERVATION_HDR-RECEIVING_SLOC IS INITIAL.

    LF_INVALID = ABAP_TRUE.
    LF_STOP_PROCESS = ABAP_TRUE.

*   Text E01: Required fields in reservation header could not be blank
    LF_MESSAGE = TEXT-E01.

    APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                               IF_MESSAGE = LF_MESSAGE
                      CHANGING CT_RESPONSE = CT_RESPONSE
                               CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
  ENDIF.

  IF CS_RESERVATION_HDR-MOVEMENT_TYPE NE GC_MVT_TRF.
    LF_INVALID = ABAP_TRUE.
    LF_STOP_PROCESS = ABAP_TRUE.

*   Text E17: The specified movement type is not supported
    LF_MESSAGE = TEXT-E17.

    APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                               IF_MESSAGE = LF_MESSAGE
                      CHANGING CT_RESPONSE = CT_RESPONSE
                               CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
  ENDIF.

* Valiate receiving sloc exist in T001L
  IF CS_RESERVATION_HDR-PLANT IS NOT INITIAL AND
     CS_RESERVATION_HDR-RECEIVING_SLOC IS NOT INITIAL.
    SELECT SINGLE LGORT
      INTO @DATA(LF_LGORT)
      FROM MARD
      WHERE WERKS EQ @CS_RESERVATION_HDR-PLANT
        AND LGORT EQ @CS_RESERVATION_HDR-RECEIVING_SLOC.

    IF SY-SUBRC NE 0.
      LF_INVALID = ABAP_TRUE.
      LF_STOP_PROCESS = ABAP_TRUE.

*     Text E20: Receiving Storage Location not found in T001L
      LF_MESSAGE = TEXT-E20.

      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = LF_MESSAGE
                        CHANGING CT_RESPONSE = CT_RESPONSE
                                 CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
    ENDIF.

  ENDIF.

* Update interface log header record
  LS_INTERFACE_LOG-MANDT = SY-MANDT.
  LS_INTERFACE_LOG-RECORDTYPE = GC_RECTYPE_HDR.
  LS_INTERFACE_LOG-RECEIVING_PLANT = CS_RESERVATION_HDR-PLANT.
  LS_INTERFACE_LOG-UPDATE_DATE = SY-DATUM.
  LS_INTERFACE_LOG-UPDATE_TIME = SY-UZEIT.
  MOVE-CORRESPONDING CS_RESERVATION_HDR TO LS_INTERFACE_LOG    ##ENH_OK.
  APPEND LS_INTERFACE_LOG TO ET_INTERFACE_LOG.

* -------------------------------------
* Validatation on ITEM level
* -------------------------------------
  IF LF_STOP_PROCESS EQ ABAP_FALSE.
    IF CT_RESERVATION_ITM IS NOT INITIAL.
*    To check that reservation number is the same for all items
*    Reservation number in first item = reservation number in the last item
      SORT LT_RESERVATION_ITM BY RESERVATION_NO.

      IF LT_RESERVATION_ITM[ 1 ]-RESERVATION_NO
         = LT_RESERVATION_ITM[ LINES( LT_RESERVATION_ITM ) ]-RESERVATION_NO.
        "Do nothing, reservation number is the same for all items
      ELSE.
        LF_INVALID = ABAP_TRUE.
        LF_STOP_PROCESS = ABAP_TRUE.
*       Text E09: Reservation number should be the same for all items
        LF_MESSAGE = TEXT-E09.

        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = LF_MESSAGE
                          CHANGING CT_RESPONSE = CT_RESPONSE
                                   CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
      ENDIF.

*     Loop to validate each reservation item and fill in interface log
      LOOP AT CT_RESERVATION_ITM ASSIGNING FIELD-SYMBOL(<L_RESERVATION_ITM>).
        CLEAR: LS_INTERFACE_LOG, LF_UPDATEMODE, LF_QUANTITY_IN, LF_ISSUE_SLOC,
               LF_AVAILABLE_QTY, LF_MATNR_INTERNAL, LF_MEINS_INTERNAL,
               LS_RESERVATION_DB, LF_INVALID, LF_MESSAGE.

        IF <L_RESERVATION_ITM>-RESERVATION_NO IS INITIAL.
          LF_UPDATEMODE = GC_CREATE.
          LF_ISSUE_SLOC = <L_RESERVATION_ITM>-ISSUE_SLOC.
        ELSE.
          LF_UPDATEMODE = GC_UPDATE.
        ENDIF.
**       Set update mode for interface log at header level
*        ET_INTERFACE_LOG[ 1 ]-UPDATEMODE = LF_UPDATEMODE.

        <L_RESERVATION_ITM>-SALESFORCE_NO = CS_RESERVATION_HDR-SALESFORCE_NO.
        LF_QUANTITY_IN = <L_RESERVATION_ITM>-QUANTITY.

*       Detail input validate for each item
        VALIDATE_INPUT_SINGLE( EXPORTING IS_RESERVATION_ITM = <L_RESERVATION_ITM>
                                         IF_UPDATEMODE = LF_UPDATEMODE
                               IMPORTING EF_INVALID = LF_INVALID
                                         EF_MESSAGE = LF_MESSAGE
                                         ES_RESERVATION_DB = LS_RESERVATION_DB
                               CHANGING  CS_RESERVATION_HDR = CS_RESERVATION_HDR
                                         CT_RESPONSE = CT_RESPONSE ).

*       Convert material number to SAP internal format
        IF <L_RESERVATION_ITM>-MATERIAL_CODE IS NOT INITIAL.
          CONVERT_MATN1_INPUT( EXPORTING IF_MATNR = <L_RESERVATION_ITM>-MATERIAL_CODE
                               IMPORTING EF_MATNR = LF_MATNR_INTERNAL ).
        ELSE.
          CONVERT_MATN1_INPUT( EXPORTING IF_MATNR = LS_RESERVATION_DB-MATNR
                               IMPORTING EF_MATNR = LF_MATNR_INTERNAL ).
        ENDIF.

*       Convert unit of measure to SAP internal format
        IF <L_RESERVATION_ITM>-UNIT IS NOT INITIAL.
          CONVERT_CUNIT_INPUT( EXPORTING IF_UNIT = <L_RESERVATION_ITM>-UNIT
                               IMPORTING EF_UNIT = LF_MEINS_INTERNAL ).
        ELSE.
          CONVERT_CUNIT_INPUT( EXPORTING IF_UNIT = LS_RESERVATION_DB-MEINS
                               IMPORTING EF_UNIT = LF_MEINS_INTERNAL ).
        ENDIF.

* -------------------------------------
*       Validation pass
* -------------------------------------
        IF LF_INVALID EQ ABAP_FALSE.

* -------------------------------------
*       Material availability check
* -------------------------------------
          IF LF_QUANTITY_IN IS NOT INITIAL.
*         Check material availability and get the available quantity
            IF LF_ISSUE_SLOC IS INITIAL.
              LF_ISSUE_SLOC = LS_RESERVATION_DB-LGORT.
            ENDIF.
            CHECK_MATERIAL_AVAILABILITY( EXPORTING IF_MATNR = LF_MATNR_INTERNAL
                                                   IF_PLANT = CS_RESERVATION_HDR-PLANT
                                                   IF_STGE_LOC = LF_ISSUE_SLOC
                                                   IF_UNIT = LF_MEINS_INTERNAL
                                         IMPORTING EF_COM_QTY = LF_AVAILABLE_QTY ).

*         Compare QUANTITY_IN with AVAILABLE_QTY and update the quantity
*         according to the availability
            REDETERMINE_QUANTITY( EXPORTING IF_UPDATEMODE = LF_UPDATEMODE
                                            IF_QUANTITY_IN = LF_QUANTITY_IN
                                            IF_AVAILABLE_QTY = LF_AVAILABLE_QTY
                                            IS_RESERVATION_DB = LS_RESERVATION_DB
                                   CHANGING CF_QUANTITY_UPD = <L_RESERVATION_ITM>-QUANTITY ).

            IF <L_RESERVATION_ITM>-QUANTITY IS INITIAL.
              LF_INVALID = ABAP_TRUE.
*           Text E10: Material does not exist or no available stock for this material
              LF_MESSAGE = TEXT-E10.

              APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                         IF_MESSAGE = LF_MESSAGE
                                         IF_ITEM_NO = <L_RESERVATION_ITM>-SALESFORCE_ITEM_NO
                                CHANGING CT_RESPONSE = CT_RESPONSE
                                         CS_RESERVATION_HDR = CS_RESERVATION_HDR ).

            ELSEIF LF_QUANTITY_IN NE <L_RESERVATION_ITM>-QUANTITY.
              LS_INTERFACE_LOG-RESPONSE_STATUS = GC_WARNING.
*           Text E11: Quantity redetermined according to available stock quantity
              LF_MESSAGE = TEXT-E11.
              APPEND_RESPONSE( EXPORTING IF_STATUS = GC_WARNING
                                         IF_MESSAGE = LF_MESSAGE
                                         IF_ITEM_NO = <L_RESERVATION_ITM>-SALESFORCE_ITEM_NO
                                CHANGING CT_RESPONSE = CT_RESPONSE
                                         CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
            ENDIF.

          ENDIF.
        ENDIF.
* -------------------------------------
*       Update inteface log
* -------------------------------------
*       Set update mode for interface log at header level
        ET_INTERFACE_LOG[ 1 ]-UPDATEMODE = LF_UPDATEMODE.
        IF LF_UPDATEMODE EQ GC_UPDATE.
          ET_INTERFACE_LOG[ 1 ]-RESERVATION_NO = <L_RESERVATION_ITM>-RESERVATION_NO.
        ENDIF.
        MOVE-CORRESPONDING <L_RESERVATION_ITM> TO LS_INTERFACE_LOG.

        IF LS_INTERFACE_LOG-MATERIAL_DESCRIPTION IS INITIAL.
          GET_MATERIAL_DESCRIPTION(
            EXPORTING IF_MATNR = LF_MATNR_INTERNAL
            IMPORTING EF_MAKTX = LS_INTERFACE_LOG-MATERIAL_DESCRIPTION ).
        ENDIF.
        IF LS_INTERFACE_LOG-ISSUE_SLOC IS INITIAL.
          LS_INTERFACE_LOG-ISSUE_SLOC = LS_RESERVATION_DB-LGORT.
        ENDIF.
        LS_INTERFACE_LOG-MANDT = SY-MANDT.
        LS_INTERFACE_LOG-MATERIAL_CODE = LF_MATNR_INTERNAL.
        LS_INTERFACE_LOG-UNIT = LF_MEINS_INTERNAL.
        LS_INTERFACE_LOG-RECORDTYPE = GC_RECTYPE_ITM.
        LS_INTERFACE_LOG-QUANTITY_IN = LF_QUANTITY_IN.
        LS_INTERFACE_LOG-PLANT = CS_RESERVATION_HDR-PLANT.
        LS_INTERFACE_LOG-MOVEMENT_TYPE = CS_RESERVATION_HDR-MOVEMENT_TYPE.
        LS_INTERFACE_LOG-RECEIVING_SLOC = CS_RESERVATION_HDR-RECEIVING_SLOC.
        LS_INTERFACE_LOG-RECEIVING_PLANT = CS_RESERVATION_HDR-PLANT.
        LS_INTERFACE_LOG-UPDATEMODE = LF_UPDATEMODE.
        LS_INTERFACE_LOG-UPDATE_DATE = SY-DATUM.
        LS_INTERFACE_LOG-UPDATE_TIME = SY-UZEIT.
        LS_INTERFACE_LOG-RESPONSE_MESSAGE = LF_MESSAGE.
        IF LF_INVALID EQ ABAP_TRUE.
          LS_INTERFACE_LOG-RESPONSE_STATUS = GC_ERROR.
        ENDIF.

        APPEND LS_INTERFACE_LOG TO ET_INTERFACE_LOG.

      ENDLOOP.
    ELSE.
      LF_INVALID = ABAP_TRUE.
      LF_STOP_PROCESS = ABAP_TRUE.

*     Text E14: No any reservation item specified. Reservation could not be created
      LF_MESSAGE = TEXT-E14.

      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = LF_MESSAGE
                        CHANGING CT_RESPONSE = CT_RESPONSE
                                 CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
    ENDIF.
  ENDIF.

  IF LF_STOP_PROCESS EQ ABAP_TRUE AND
     ET_INTERFACE_LOG[ 1 ]-RESPONSE_STATUS IS INITIAL.
    ET_INTERFACE_LOG[ 1 ]-RESPONSE_STATUS = GC_ERROR.
    ET_INTERFACE_LOG[ 1 ]-RESPONSE_MESSAGE = LF_MESSAGE.
  ENDIF.

ENDMETHOD.


METHOD VALIDATE_INPUT_SINGLE.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_RESERV_VAN_SLOC / VALIDATE_INPUT_SINGLE
*  Creation Date      : 14.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MMI029
*  Description        : Input validation - single line
*  Purpose            : Validate input item line by line
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  DATA: LF_INVALID        TYPE FLAG,
        LF_MESSAGE        TYPE BAPI_MSG,
        LS_RESERVATION_DB TYPE TS_RESERVATION_DB.

  CLEAR: EF_INVALID, EF_MESSAGE, ES_RESERVATION_DB.

* -------------------------------------
* Validate for mode create and update
* -------------------------------------
  CASE IF_UPDATEMODE.

    WHEN GC_CREATE.
* -------------------------------------
*   Record mode = C (Create reservation)
* -------------------------------------
* -------------------------------------
* Validate required fields in item level
* -------------------------------------
      IF IS_RESERVATION_ITM-SALESFORCE_ITEM_NO IS INITIAL OR
         IS_RESERVATION_ITM-MATERIAL_CODE IS INITIAL OR
         IS_RESERVATION_ITM-QUANTITY IS INITIAL OR
         IS_RESERVATION_ITM-UNIT IS INITIAL OR
         IS_RESERVATION_ITM-ISSUE_SLOC IS INITIAL.

        LF_INVALID = ABAP_TRUE.
        LF_MESSAGE = TEXT-E07.
*       Text E07: Item xxxx: Required fields in reservation item could not be blank

        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = LF_MESSAGE
                                   IF_ITEM_NO = IS_RESERVATION_ITM-SALESFORCE_ITEM_NO
                         CHANGING CT_RESPONSE = CT_RESPONSE
                                  CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
      ENDIF.

      IF IS_RESERVATION_ITM-COMPLETED IS NOT INITIAL.
        LF_INVALID = ABAP_TRUE.
*       Text E02: Item xxx: Create reservation cannot choose Final Issue
        LF_MESSAGE = TEXT-E02.

        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = LF_MESSAGE
                                   IF_ITEM_NO = IS_RESERVATION_ITM-SALESFORCE_ITEM_NO
                          CHANGING CT_RESPONSE = CT_RESPONSE
                                   CS_RESERVATION_HDR = CS_RESERVATION_HDR ).

      ENDIF.

      IF IS_RESERVATION_ITM-DELETED IS NOT INITIAL.
        LF_INVALID = ABAP_TRUE.
*       Text E03: Item xxx: Create reservation cannot choose Deleted
        LF_MESSAGE = TEXT-E03.

        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = LF_MESSAGE
                                   IF_ITEM_NO = IS_RESERVATION_ITM-SALESFORCE_ITEM_NO
                         CHANGING CT_RESPONSE = CT_RESPONSE
                                  CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
      ENDIF.

    WHEN GC_UPDATE.
* -------------------------------------
*   Record mode = U (Update reservation)
* -------------------------------------
*     Update reservation only possible for fields
*     Quantity, Completed, Deleted
      IF IS_RESERVATION_ITM-SALESFORCE_ITEM_NO IS INITIAL OR
         IS_RESERVATION_ITM-RESERVATION_NO IS INITIAL OR
         IS_RESERVATION_ITM-RESERVATION_ITEM IS INITIAL OR
         ( IS_RESERVATION_ITM-QUANTITY IS INITIAL AND
           IS_RESERVATION_ITM-COMPLETED IS INITIAL AND
           IS_RESERVATION_ITM-DELETED IS INITIAL ).

        LF_INVALID = ABAP_TRUE.
        LF_MESSAGE = TEXT-E07.
*       Text E07: Item xxxx: Required fields in reservation item could not be blank

        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = LF_MESSAGE
                                   IF_ITEM_NO = IS_RESERVATION_ITM-SALESFORCE_ITEM_NO
                         CHANGING CT_RESPONSE = CT_RESPONSE
                                  CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
      ENDIF.

      VALIDATE_WITH_RESB( EXPORTING IS_RESERVATION_ITM = IS_RESERVATION_ITM
                          IMPORTING EF_INVALID = LF_INVALID
                                    EF_MESSAGE = LF_MESSAGE
                                    ES_RESERVATION_DB = LS_RESERVATION_DB
                           CHANGING CS_RESERVATION_HDR = CS_RESERVATION_HDR
                                    CT_RESPONSE = CT_RESPONSE ).

  ENDCASE.

  EF_INVALID = LF_INVALID.
  EF_MESSAGE = LF_MESSAGE.
  ES_RESERVATION_DB = LS_RESERVATION_DB.

ENDMETHOD.


METHOD VALIDATE_WITH_RESB.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_RESERV_VAN_SLOC / VALIDATE_WITH_RESB
*  Creation Date      : 14.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MMI029
*  Description        : Check reservation with existing data
*  Purpose            : Validate reservation / item with RKPF, RESB
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  DATA: LS_RESERVATION_DB TYPE TS_RESERVATION_DB.

  CLEAR: EF_INVALID, EF_MESSAGE, ES_RESERVATION_DB.

  GET_RESERVATION_DB( EXPORTING IF_RSNUM = IS_RESERVATION_ITM-RESERVATION_NO
                                IF_RSPOS = IS_RESERVATION_ITM-RESERVATION_ITEM
                      IMPORTING ES_RESERVATION_DB = LS_RESERVATION_DB ).

  IF LS_RESERVATION_DB IS INITIAL.
    EF_INVALID = ABAP_TRUE.
*   Text E04: Reservation number or reservation item not found in SAP
    EF_MESSAGE = TEXT-E04.

    APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                               IF_MESSAGE = EF_MESSAGE
                               IF_ITEM_NO = IS_RESERVATION_ITM-SALESFORCE_ITEM_NO
                      CHANGING CT_RESPONSE = CT_RESPONSE
                               CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
  ELSE.
    IF LS_RESERVATION_DB-XLOEK IS NOT INITIAL.   "Item deleted
      EF_INVALID = ABAP_TRUE.
*     Text E06: Item is marked for deletion, change is not allowed
      EF_MESSAGE = TEXT-E06.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = EF_MESSAGE
                                 IF_ITEM_NO = IS_RESERVATION_ITM-SALESFORCE_ITEM_NO
                        CHANGING CT_RESPONSE = CT_RESPONSE
                                 CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
    ENDIF.

    IF LS_RESERVATION_DB-KZEAR IS NOT INITIAL.   "Completed (Final issue) is marked
      EF_INVALID = ABAP_TRUE.
*     Text E12: Cannot change quantity. Material document is created
      EF_MESSAGE = TEXT-E12.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = EF_MESSAGE
                                 IF_ITEM_NO = IS_RESERVATION_ITM-SALESFORCE_ITEM_NO
                        CHANGING CT_RESPONSE = CT_RESPONSE
                                 CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
    ENDIF.

*   Compare value of the following fields from request input and RESB
*   Fields: MATNR, WERKS, LGORT, UMLGO
    IF ( IS_RESERVATION_ITM-MATERIAL_CODE IS NOT INITIAL AND
         IS_RESERVATION_ITM-MATERIAL_CODE <> LS_RESERVATION_DB-MATNR ) OR
       ( CS_RESERVATION_HDR-PLANT IS NOT INITIAL AND
         CS_RESERVATION_HDR-PLANT <> LS_RESERVATION_DB-WERKS ) OR
       ( IS_RESERVATION_ITM-ISSUE_SLOC IS NOT INITIAL AND
         IS_RESERVATION_ITM-ISSUE_SLOC <> LS_RESERVATION_DB-LGORT ) OR
       ( CS_RESERVATION_HDR-RECEIVING_SLOC IS NOT INITIAL AND
         CS_RESERVATION_HDR-RECEIVING_SLOC <> LS_RESERVATION_DB-UMLGO ).

      EF_INVALID = ABAP_TRUE.
*     Text E05: Reservation detail not align with SAP
      EF_MESSAGE = TEXT-E05.

      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = EF_MESSAGE
                                 IF_ITEM_NO = IS_RESERVATION_ITM-SALESFORCE_ITEM_NO
                        CHANGING CT_RESPONSE = CT_RESPONSE
                                 CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
    ENDIF.

    IF IS_RESERVATION_ITM-QUANTITY = LS_RESERVATION_DB-BDMNG.
      EF_INVALID = ABAP_TRUE.
*     Text E13: Request quantity is the same as existing reservation quantity
      EF_MESSAGE = TEXT-E13.

      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = EF_MESSAGE
                                 IF_ITEM_NO = IS_RESERVATION_ITM-SALESFORCE_ITEM_NO
                        CHANGING CT_RESPONSE = CT_RESPONSE
                                 CS_RESERVATION_HDR = CS_RESERVATION_HDR ).
    ENDIF.
  ENDIF.

  ES_RESERVATION_DB = LS_RESERVATION_DB.

ENDMETHOD.
ENDCLASS.

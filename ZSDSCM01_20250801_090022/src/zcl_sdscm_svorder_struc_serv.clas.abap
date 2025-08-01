class ZCL_SDSCM_SVORDER_STRUC_SERV definition
  public
  inheriting from ZCL_SDSCA_REST_SERVICE
  final
  create public .

public section.

  methods PROCESS_DATA
    redefinition .
protected section.
private section.

  methods MAINTAIN_SVORDER_STRUCTURE
    importing
      !IS_REQUEST type ZSDSCMS001
    exporting
      !ES_RESPONSE type ZSDSCMS001 .
ENDCLASS.



CLASS ZCL_SDSCM_SVORDER_STRUC_SERV IMPLEMENTATION.


METHOD MAINTAIN_SVORDER_STRUCTURE.

  DATA:
    LS_REQUEST  TYPE  ZSDSCMS001,
    LS_RETURN   TYPE  BAPIRETURN1,
    LT_RETURN   TYPE  ZSDSCAS006_TT.

* Initialize Output
  CLEAR: ES_RESPONSE.

* -------------------
* Validate Service Order
* -------------------
  LS_REQUEST = IS_REQUEST.

  ZCL_SDSCM_SVORDER=>VALIDATE_SVORDER( CHANGING CS_REQUEST  = LS_REQUEST
                                                CS_RESPONSE = ES_RESPONSE ).
  IF ES_RESPONSE-RESP_STATUS IS NOT INITIAL.
    RETURN.
  ENDIF.

  ZCL_SDSCM_SVORDER=>DEFAULT_SVORDER( CHANGING CS_REQUEST  = LS_REQUEST ).

  ES_RESPONSE = LS_REQUEST.

  ZCL_SDSCM_SVORDER=>MAINTAIN_SVORDER( EXPORTING IS_REQUEST  = LS_REQUEST
                                       IMPORTING ES_RETURN   = LS_RETURN
                                                 ET_RETURN   = LT_RETURN ).

  ES_RESPONSE-RESP_STATUS = LS_RETURN-TYPE.
  ES_RESPONSE-RESP_MESSAGE = LS_RETURN-MESSAGE.
  IF LS_RETURN-TYPE EQ ZCL_SDSCA_REST_INTF_UTILITY=>GC_SUCCESS.
    ES_RESPONSE-SVO_NO = LS_RETURN-MESSAGE_V1.
  ENDIF.

  IF LT_RETURN IS NOT INITIAL.
    ES_RESPONSE-RESPONSE[] = LT_RETURN[].
  ELSE.
    ZCL_SDSCM_SVORDER=>READ_LOGCRM( EXPORTING IF_OBJECT_ID  = ES_RESPONSE-SVO_NO
                                    IMPORTING CT_MESSAGE    = ES_RESPONSE-RESPONSE[] ).
  ENDIF.

ENDMETHOD.


METHOD PROCESS_DATA.
*-----------------------------------------------------------------------
*  Class              : ZCL_SDSCM_SVORDER_STRUC_SERV
*  Creation Date      : 29.05.2024
*  Author             : Suthichai P.(Eviden)
*  Add-on ID          : CMI008 and CMI010
*  Description        : This is a Processing class of REST interface
*                       CMI008 to maintain Service Order structure
*  Purpose            : To maintain Service Order Structure
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
    LS_REQUEST  TYPE  ZSDSCMS001.

  FIELD-SYMBOLS:
    <L_RESPONSE>  TYPE  ZSDSCMS001.


*Initialize Output
  CLEAR: EF_STATUS,
         EF_MESSAGE,
         EF_HTTP_ERROR.

  LS_REQUEST = IREF_REQUEST_DATA->*.

  ASSIGN EREF_RESPONSE_DATA->* TO <L_RESPONSE>.
  IF SY-SUBRC NE 0.
* Critical error
    RETURN.
  ENDIF.

  LCL_DATA=>CONVERT_AND_SAVE_STATUS( CHANGING C_DATA = LS_REQUEST ).

* Maintain Service Order Structure
  MAINTAIN_SVORDER_STRUCTURE(
    EXPORTING
      IS_REQUEST  = LS_REQUEST
    IMPORTING
      ES_RESPONSE = <L_RESPONSE> ).

* Assign Log status from Response structure
  IF <L_RESPONSE>-RESP_STATUS EQ ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
*   Error at header level, Nothing success => HTTP error
    EF_STATUS  = <L_RESPONSE>-RESP_STATUS.
    EF_MESSAGE = <L_RESPONSE>-RESP_MESSAGE.

    "EF_HTTP_ERROR = 'X'.
  ELSE.
    LCL_DATA=>UPDATE_SVO_DATA( <L_RESPONSE> ).
    EF_STATUS  = <L_RESPONSE>-RESP_STATUS.
    EF_MESSAGE = <L_RESPONSE>-RESP_MESSAGE.
  ENDIF.

ENDMETHOD.
ENDCLASS.

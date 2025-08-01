class ZCL_SDSCM_SEARCH_AUTH definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_CRM_RF_Q1O_SEARCH .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSCM_SEARCH_AUTH IMPLEMENTATION.


METHOD IF_CRM_RF_Q1O_SEARCH~SEARCH.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSCM_SEARCH_AUTH->IF_CRM_RF_Q1O_SEARCH~SEARCH
*  Creation Date      : 06.11.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : N/A
*  Description        : This is implementation to avoid SAP authorization
*                       check on web client search
*  Purpose            : N/A
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
    LS_RETURN TYPE BAPIRET2.

  DATA:
    LF_AVOID_AUTHORIZATION TYPE CRMT_BOOLEAN,
    LF_SORT_LAST_N         TYPE NAME_KOMP.


  LF_AVOID_AUTHORIZATION = IV_AVOID_AUTHORIZATION.

* Mark Avoid Authorization flag
  IF ZCL_SDSCM_ENHANCEMENT=>IS_SDS( ) EQ 'X'.
    LF_AVOID_AUTHORIZATION = 'X'.
  ENDIF.

***************************
* Below code onward is copied from SAP Code
* CL_CRM_Q1O_SEARCH->GET_RESULT_GUIDS
***************************
  CALL FUNCTION 'CRM_BSP_OIC_1O_SEARCH_FROM_RF'
    EXPORTING
      IT_SEARCH_TAB             = IT_SEARCH_TAB
      IT_MULTIVALUES            = IT_MULTIVALUES
      IV_SORT_LAST_N            = LF_SORT_LAST_N
      IV_NUMBER                 = IV_NUMBER
      IV_ITEMS                  = IV_ITEM
      IV_ARCHIVE                = IV_ARCHIVE
      IV_EXTERN_CALL            = 'X'
      IV_SELECT_FOR_HEADERLEVEL = IV_SELECT_FOR_HEADERLEVEL
      IV_CALL_AUTHORITY_BADI    = IV_CALL_AUTHORITY_BADI
      IV_SELECT_FOR_ITEMLEVEL   = IV_SELECT_FOR_ITEMLEVEL
      IV_AVOID_AUTHORIZATION    = LF_AVOID_AUTHORIZATION
      IV_OBJ_IL                 = IV_OBJ_IL
      IV_REPORT_AREA            = IV_REPORT_AREA
      IV_ONLY_MAIN_ITEM         = IV_ONLY_MAIN_ITEM
      IV_ADMH_EXT_FOR_ITM       = IV_ADMH_EXT_FOR_ITM
    IMPORTING
      ET_GUIDLIST               = ET_GUIDLIST
      ET_RETURN                 = ET_RETURN
    EXCEPTIONS
      DATE_NOT_CORRECT          = 1
      NO_CARD_TYPE              = 2
      NO_CARD_NO                = 3
      NO_PROGRAM_ID             = 4
      NO_AUTHORITY              = 5.
  CASE SY-SUBRC.
    WHEN 1.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          TYPE   = 'E'
          CL     = 'CRMN_REPORT'
          NUMBER = 141
        IMPORTING
          RETURN = LS_RETURN.
      APPEND LS_RETURN TO ET_RETURN.
    WHEN 2.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          TYPE   = 'E'
          CL     = 'CRMN_REPORT'
          NUMBER = 128
        IMPORTING
          RETURN = LS_RETURN.
      APPEND LS_RETURN TO ET_RETURN.
    WHEN 3.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          TYPE   = 'E'
          CL     = 'CRMN_REPORT'
          NUMBER = 129
        IMPORTING
          RETURN = LS_RETURN.
      APPEND LS_RETURN TO ET_RETURN.
    WHEN 4.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          TYPE   = 'E'
          CL     = 'CRMN_REPORT'
          NUMBER = 142
        IMPORTING
          RETURN = LS_RETURN.
      APPEND LS_RETURN TO ET_RETURN.
    WHEN 5.
*      RAISE no_authority.
  ENDCASE.

ENDMETHOD.
ENDCLASS.

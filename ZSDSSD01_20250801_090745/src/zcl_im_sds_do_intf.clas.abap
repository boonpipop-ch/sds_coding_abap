class ZCL_IM_SDS_DO_INTF definition
  public
  final
  create public .

public section.

  interfaces IF_EX_DELIVERY_PUBLISH .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_SDS_DO_INTF IMPLEMENTATION.


  METHOD IF_EX_DELIVERY_PUBLISH~PUBLISH_AFTER_SAVE.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_IM_SDS_DO_INTF
*                       method IF_EX_DELIVERY_PUBLISH~PUBLISH_AFTER_SAVE
*  Creation Date      : 26.06.2024
*  Author             : Boontip
*  Add-on ID          : SDI036
*  Description        : after save DO method
*  Purpose            : submit program to send interface to salesforces
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    DATA: LF_DEL_FLAG TYPE FLAG.
    FIELD-SYMBOLS: <L_LIKP>   TYPE SHP_LIKP_T.
    ASSIGN ('(SAPMV50A)XLIKP[]') TO <L_LIKP>.
    IF <L_LIKP> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
    READ TABLE <L_LIKP> INTO DATA(LS_LIKP) INDEX 1.
    IF SY-SUBRC <> 0.
      RETURN.
    ENDIF.

    IF ZCL_SDSSD_SCHD_LN_STAT=>CHECK_ACTIVE_SALE_ORG( IF_SALES_ORG = LS_LIKP-VKORG ) = ABAP_TRUE
    AND ZCL_SDSSD_SCHD_LN_STAT=>CHECK_CRITERIA_WITH_GENC( IS_LIKP = LS_LIKP ) = ABAP_TRUE.
      IF LS_LIKP-UPDKZ = 'D'.
        LF_DEL_FLAG = ABAP_TRUE.
      ENDIF.
      CALL METHOD ZCL_SDSSD_SCHD_LN_STAT=>SUBMIT_PROG_TO_SEND_INTERFACE
        EXPORTING
          IF_DO_NUMBER = LS_LIKP-VBELN
          IF_DEL       = LF_DEL_FLAG
          IF_TEST      = ' '
          IF_WAIT      = ABAP_TRUE.
    ENDIF.

  ENDMETHOD.


  method IF_EX_DELIVERY_PUBLISH~PUBLISH_BEFORE_COMMIT ##NEEDED.
  endmethod.
ENDCLASS.

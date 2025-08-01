class ZCL_SDSSD_RV60B900 definition
  public
  final
  create public .

public section.

  class-methods VALIDATE_CM_CONFIRM
    importing
      value(IF_VBELN) type LIKP-VBELN
    exporting
      value(EF_ERROR_FLAG) type FLAG .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSSD_RV60B900 IMPLEMENTATION.


  METHOD VALIDATE_CM_CONFIRM.
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  31.07.2025  420000714   Wuthichai L. - Fix logic checking delete status
*-----------------------------------------------------------------------
    CLEAR EF_ERROR_FLAG.
    DATA: LRT_ZZPOB               TYPE RANGE OF VBAK-ZZPOB,
          LRT_STAT_LIFECYCLE_PASS TYPE RANGE OF CRMS4D_SERV_H-STAT_LIFECYCLE.


    CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
      EXPORTING
        IF_REPID = 'ZCL_SDSSD_RV60B900'
        IF_PARAM = 'ZZPOB'
      IMPORTING
        ET_RANGE = LRT_ZZPOB.
    IF LRT_ZZPOB[] IS INITIAL.
      RETURN.
    ENDIF.
    CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
      EXPORTING
        IF_REPID = 'ZCL_SDSSD_RV60B900'
        IF_PARAM = 'STAT_LIFECYCLE_PASS'
      IMPORTING
        ET_RANGE = LRT_STAT_LIFECYCLE_PASS.
    IF LRT_STAT_LIFECYCLE_PASS[] IS INITIAL.
      RETURN.
    ENDIF.
*     SELECT SO ORDER DATA
    SELECT VBAK~VBELN,                                  "#EC CI_NOORDER
           VBAK~ZZPOB
    FROM VBAK INNER JOIN VBFA
    ON   VBAK~VBELN = VBFA~VBELV
    WHERE VBFA~VBELN = @IF_VBELN
    AND   VBFA~VBTYP_V = 'C' "SO
    AND   VBFA~VBTYP_N = 'J' "DO
    INTO @DATA(LS_SO)
    UP TO 1 ROWS .
    ENDSELECT.

    IF  LS_SO-ZZPOB NOT IN LRT_ZZPOB.
      RETURN.
    ELSE.
      SELECT OBJTYPE_H,                                 "#EC CI_NOFIELD
             OBJECT_ID,
             STAT_LIFECYCLE
      FROM CRMS4D_SERV_H
      WHERE ZZ1_DELIVERY_ORD = @IF_VBELN
        AND STAT_CANCELLED = @SPACE                         "+420000714
      INTO @DATA(LS_SERV)
      UP TO 1 ROWS .                                    "#EC CI_NOORDER
      ENDSELECT.
      IF SY-SUBRC <> 0
      OR LS_SERV-STAT_LIFECYCLE NOT IN LRT_STAT_LIFECYCLE_PASS .
        EF_ERROR_FLAG = ABAP_TRUE.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

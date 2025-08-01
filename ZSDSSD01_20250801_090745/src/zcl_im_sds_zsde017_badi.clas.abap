class ZCL_IM_SDS_ZSDE017_BADI definition
  public
  final
  create public .

public section.

  interfaces IF_EX_SD_CIN_LV60AU02 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_SDS_ZSDE017_BADI IMPLEMENTATION.


  METHOD IF_EX_SD_CIN_LV60AU02~EXCISE_INVOICE_CREATE.
*-----------------------------------------------------------------------
*  Program ID         : ZSDS_ZSDE017_BADI
*  Creation Date      : 18.06.2024
*  Author             : Atitep B.
*  Add-on ID          : N/A
*  Description        : Post Advance Receive to special G/L - Billing
*  Purpose            : Post Advance Receive to special G/L - Billing
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

    DATA: LF_JOBNAME  TYPE TBTCJOB-JOBNAME,
          LF_JOBCOUNT TYPE TBTCJOB-JOBCOUNT.

    DATA: LF_POST(1),
          LF_RES(1),
          LF_CHECK(1).

    CONSTANTS : BEGIN OF LC_CON,
                  I_REPID             TYPE CHAR20 VALUE 'PROGRAM_BILLING',
                  I_SINGLE_VALUE_FLAG TYPE FLAG   VALUE 'X',
                  I_PARAM             TYPE CHAR20 VALUE 'BILIING',
                END OF LC_CON.
    DATA : CR_INV TYPE RANGE OF SY-TCODE.

*    CONSTANTS : BEGIN OF LC_CON_RE,
*                  I_REPID             TYPE CHAR20 VALUE 'PROGRAM_BILLING',
*                  I_SINGLE_VALUE_FLAG TYPE FLAG   VALUE 'X',
*                  I_PARAM             TYPE CHAR20 VALUE 'BILIING_CANCEL',
*                END OF LC_CON_RE.
*    DATA : CR_RETURN TYPE RANGE OF SY-TCODE.

    DATA: LV_TCODE LIKE SY-TCODE.

    LV_TCODE = SY-TCODE.

    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID   = LC_CON-I_REPID
                                                  I_PARAM   = LC_CON-I_PARAM
                                         CHANGING CR_RETURN = CR_INV
                                       ).
*
*    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID   = LC_CON_RE-I_REPID
*                                                  I_PARAM   = LC_CON_RE-I_PARAM
*                                         CHANGING CR_RETURN = CR_RETURN
*                                       ).

    IF SY-CPROG    IN CR_INV[] AND
       CR_INV[] IS NOT INITIAL.
      SELECT COUNT( * )
        FROM @XVBRK AS A
        WHERE VBTYP EQ 'N' or
              VBTYP EQ 'O' .
      IF SY-SUBRC EQ 0.
        LV_TCODE = 'VF11'.
      ELSE.
        LV_TCODE = 'VF01'.
      ENDIF.
    ENDIF.

    IF LV_TCODE = 'VF01' OR LV_TCODE = 'VF11'.
      READ TABLE XVBRK INTO DATA(LS_VBRK) INDEX 1.

      SELECT PARAM_SIGN   AS SIGN,
             PARAM_OPTION  AS OPTION,
             VALUE_LOW AS LOW,
             VALUE_HIGH AS HIGH
        FROM ZSDSCAC001
       WHERE REPID = 'ZSDSSDR0150'
         AND PARAM = 'CHECK_COMPANY'
       INTO TABLE @DATA(LRT_COMPANY).

      IF LS_VBRK-BUKRS IN LRT_COMPANY AND LRT_COMPANY[] IS NOT INITIAL.
        IF LV_TCODE = 'VF01'.
          LF_POST = 'X'.
          LF_RES  = ''.
          SELECT VBELN, POSNR
            FROM ZSDSSDT010
            INTO TABLE @DATA(LT_VBELN)
            FOR ALL ENTRIES IN @XVBRP
            WHERE VBELN = @XVBRP-AUBEL
              AND POSNR = @XVBRP-AUPOS.
          IF LT_VBELN[] IS NOT INITIAL.
            LF_CHECK = 'X'.
          ENDIF.
        ELSE.
          LF_POST = ''.
          LF_RES  = 'X'.
          SELECT SINGLE VBELN ##WARN_OK
            FROM ZSDSSDT015
            INTO @DATA(LF_VBELN)
            WHERE VBELN_B = @LS_VBRK-VBELN
              AND BELNR_C = ''.
          IF LF_VBELN IS NOT INITIAL.
            LF_CHECK = 'X'.
          ENDIF.
        ENDIF.

        IF LF_CHECK = 'X'.
          CONCATENATE 'ZSDE017'
                       SY-DATUM
                       SY-UZEIT
                 INTO  LF_JOBNAME.

*     open job
          CALL FUNCTION 'JOB_OPEN' ##FM_SUBRC_OK
            EXPORTING
              JOBNAME          = LF_JOBNAME
            IMPORTING
              JOBCOUNT         = LF_JOBCOUNT
            EXCEPTIONS
              CANT_CREATE_JOB  = 1
              INVALID_JOB_DATA = 2
              JOBNAME_MISSING  = 3
              OTHERS           = 4.

          SUBMIT ZSDSSDR0150                             "#EC CI_SUBMIT
*        WITH S_VBELN IN LRT_VBELN
            WITH P_VBELN = LS_VBRK-VBELN
            WITH R_POST = LF_POST
            WITH R_RES = LF_RES
            VIA JOB LF_JOBNAME NUMBER LF_JOBCOUNT
            USER SY-UNAME
            AND RETURN .

*   Schedule and close job.
          CALL FUNCTION 'JOB_CLOSE'
            EXPORTING
              JOBCOUNT  = LF_JOBCOUNT
              JOBNAME   = LF_JOBNAME
              STRTIMMED = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

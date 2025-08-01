*-----------------------------------------------------------------------
*  Program ID         : ZXVVFU10
*  Creation Date      : 19.02.2025
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : 420000412
*  Description        : This is an include program of EXIT_SAPLV60B_010.
*  Purpose            : - To combine customer lines which are splitted
*                         from WHT setting (condition type WTTX).
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

* Assign Default Output
  E_XACCIT_DEB = I_XACCIT_DEB.

* Get Company Code
  SELECT SINGLE BUKRS
    FROM TVKO
   WHERE VKORG EQ @I_XVBRP-VKORG_AUFT
    INTO @DATA(LF_BUKRS).
  IF SY-SUBRC NE 0.
    CLEAR LF_BUKRS.
  ENDIF.

* Only SDS Related and WTH Key Exists
  IF ZCL_SDSFI_ENHANCEMENT2=>IS_SDS( IF_BUKRS = LF_BUKRS
                                     IF_VKORG = I_XVBRP-VKORG_AUFT ) EQ 'X' AND
     E_XACCIT_DEB-WT_KEY IS NOT INITIAL.
*   Proceed Avoid AR Line Splitting from WT_KEY
    CALL METHOD ZCL_SDSFI_ENHANCEMENT2=>EXIT_SAPLV60B_010
      EXPORTING
        IS_XVBRP      = I_XVBRP
        IS_XACCIT_DEB = I_XACCIT_DEB
        IS_XKOMV      = I_XKOMV
      CHANGING
        CS_XACCIT_DEB = E_XACCIT_DEB.
  ENDIF.

*-----------------------------------------------------------------------
*  Program ID         : ZXVVFU04
*  Creation Date      : 31.07.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : N/A
*  Description        : Exit EXIT_SAPLV60B_004
*  Purpose            : To assign value into custom field
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

* >>>>> SDS ZPSE004 INSERT START >>>>> *
* Assign Item Product Hierarchy
  IF ZCL_SDSPS_WBS_SUBSTITUTION=>IS_SDS( VBRK-BUKRS ) EQ 'X'.
    XACCIT-ZZ1_PRODH = XVBRP-PRODH.
  ENDIF.
* <<<<< SDS ZPSE004 INSERT END   <<<<< *

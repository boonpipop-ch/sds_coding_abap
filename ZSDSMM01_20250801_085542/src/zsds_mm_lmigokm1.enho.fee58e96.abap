"Name: \PR:SAPLMIGO\EX:LMIGOKM1_01\EI
ENHANCEMENT 0 ZSDS_MM_LMIGOKM1.
*-----------------------------------------------------------------------
*  Program ID         : ZSDS_MM_LMIGOKM1
*  Creation Date      : 09.05.2024
*  Author             : Boontip R.(Eviden)
*  Add-on ID          : N/A
*  Description        : SDS_D04_FS_ZMMR004
*                       MIGO: Add Material Document to header text for Material Transfer
*  Purpose            : Add Material Document to header text for Material Transfer
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  IF S_ACTION-CREATE = ABAP_TRUE
  AND ( S_ACTION-PLACE = ABAP_TRUE OR S_ACTION-CANCEL = ABAP_TRUE ). "place in stock/cancel

    CALL METHOD ZCL_SDSMM_MIGO_MAT_TRNFR=>SET_BKTXT
      EXPORTING
        IF_REF_MBLNR  = PS_GOHEAD-MBLNR
        IF_REF_MJAHR  = PS_GOHEAD-MJAHR
        IF_CANCEL_FG  = S_ACTION-CANCEL
      IMPORTING
        EF_BKTXT      = PS_GOHEAD-BKTXT.

  ENDIF.
ENDENHANCEMENT.

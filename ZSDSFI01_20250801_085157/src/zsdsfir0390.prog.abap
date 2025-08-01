*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0390
*  Creation Date      : 15.07.2024
*  Author             : Boontip R.(Eviden)
*  Add-on ID          : FIARE030
*  Description        : The follow-up billing and DO program
*  Purpose            : The program purpose is to collect the updating
*                       follow up status into the Ztable
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
* 12.12.2024  F36K909945  Atitep B. - Fix select data
*-----------------------------------------------------------------------
* 08.01.2025  F36K910801  Boontip R. CH02 IMS 420000219
* 10.01.2025  F36K910961
* - fix missing document : remove reversal , change logic to exclude payment doc
* - add logic to get work date to replace bill placement date if blank
* - get ZSDSFIT0029 where BILLPL_NO <> ''
*-----------------------------------------------------------------------
* 14.01.2025 F36K911032  Boontip R. CH03 IMS 420000259
* 15.01.2025 F36K911082
* - get PO from service order ( case CRM )
*-----------------------------------------------------------------------
* 23.01.2025 F36K911524  Boontip R. CH04
*            F36K911821
*            F36K912255
* IMS 420000327
* - option billing, KUNNR = onetime , get name from BSEC -> ADRC version ''
* - option DO , change customer from sold to -> payer from VBPA/ADRC
* IMS 420000219 case data migration > clearing doc is stamped in XREF1
* ( but normal case it stamp in REBZG )
* IMS 420000332 change detail1, 2, 3 to be longtext
*-----------------------------------------------------------------------
* 24.03.2025 F36K914623  Boontip R. CH05
* IMS 420000332
* - change color of column 'TEXT1' , 'TEXT2', 'TEXT3'
*-----------------------------------------------------------------------
* 26.03.2025 F36K914744  Boontip R. CH06
* IMS 420000219
* -before change report does not show when augbl = belnr ( clearing in the
*  same documen) , change to include this case
* -if many original doc( xblnr) refer to the same number of migrate doc ( xref1)
*  then show separately without grouping to original doc
* - add loop invoice ref of doc migrate(REBZG) in loop of doc migrate
* IMS 420000332
* - change update date, time ,user when change long text
*-----------------------------------------------------------------------
* 09.04.2025 F36K915466  Boontip R. IMS 420000252
* fix function ZSDS_FI_MAINTAIN_LONG_TEXT
* issue : long text is shift everytime
*-----------------------------------------------------------------------
* 18.06.2025 F36K919561  Boontip R. IMS 420000671 (CH07)
*            F36K919596
* PIC field : fix knvv not join sale office , sale group to table ZSDSFIC027
*-----------------------------------------------------------------------
* 19.06.2025 F36K919678  Boontip R. IMS 420000671 (CH08)
*            F36K919596
* PIC field : fix select table ZSDSFIC027 by data from VBRP instead of KNVV
*-----------------------------------------------------------------------


REPORT ZSDSFIR0390 ##TEXT_USE.
INCLUDE ZSDSCAI9990 ##INCL_OK.
INCLUDE ZSDSFIR0390_TOP.
INCLUDE ZSDSFIR0390_F01.
INCLUDE ZSDSFIR0390_O01.
INCLUDE ZSDSFIR0390_I01.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_GET_GENC .
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM F_SEL_SCRN_SET_PROPERTIES.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN .
  IF  SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_VALIDATE_SEL_SCREEN.
  ENDIF.
*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_INIT_VALUE .
  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-FI.
      PERFORM F_SELECT_AND_PREPARE_BILL CHANGING GT_DATA1.
      IF GT_DATA1 IS INITIAL.
*     Message: No data found.
        MESSAGE S001(ZSDSCA01).
        RETURN.
      ENDIF.
      GT_DATA1_OLD = GT_DATA1.
*      PERFORM F_LOCK_ZSDSFIT040 USING GT_DATA1. "del - request multiple access
    WHEN GC_EXEC_TY-DO.
      PERFORM F_SELECT_AND_PREPARE_DO CHANGING GT_DATA2.
      IF GT_DATA2 IS INITIAL.
*     Message: No data found.
        MESSAGE S001(ZSDSCA01).
        RETURN.
      ENDIF.
      GT_DATA2_OLD = GT_DATA2.
*      PERFORM F_LOCK_ZSDSFIT041 USING GT_DATA2. "del- request multiple access
  ENDCASE.
*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-FI.
      PERFORM F_DISPLAY_RESULT USING GT_DATA1.
*      PERFORM F_UNLOCK_ZSDSFIT040 USING GT_DATA1.  "del - request multiple access
    WHEN GC_EXEC_TY-DO.
      PERFORM F_DISPLAY_RESULT USING GT_DATA2.
*      PERFORM F_UNLOCK_ZSDSFIT041 USING GT_DATA2.  "del - request multiple access
  ENDCASE.

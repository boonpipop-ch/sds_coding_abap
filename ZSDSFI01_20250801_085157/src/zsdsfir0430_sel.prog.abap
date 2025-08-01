*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0430_SEL
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0430_SEL
*  Creation Date      : 22.08.2024
*  Author             : Atitep B. (Eviden)
*  Add-on ID          : N/A
*  Description        : This is include program routine
*  Purpose            : Include program data type and global data object
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------


*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bn1 WITH FRAME TITLE TEXT-s01.
  PARAMETERS:
    p_bukrsn TYPE zsdsfit045-bukrs OBLIGATORY,
    p_pernrn TYPE p0002-pernr MATCHCODE OBJECT zsdsh_pernr,
    p_wrkdtn TYPE zsdsfit045-work_date OBLIGATORY MODIF ID cre.
*    p_rcvamn TYPE zsdsfit045-wrbtr MODIF ID df2.

SELECTION-SCREEN END OF BLOCK bn1.

*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bn2 WITH FRAME TITLE TEXT-s07.
* Text-s04: Create
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 1.
    PARAMETERS rb_cren RADIOBUTTON GROUP gn0 DEFAULT 'X' USER-COMMAND sel_moden.
    SELECTION-SCREEN COMMENT (30) TEXT-s21 FOR FIELD rb_cren.
  SELECTION-SCREEN END OF LINE.

* Text-s05: Change
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 1.
    PARAMETERS rb_chgn RADIOBUTTON GROUP gn0.
    SELECTION-SCREEN COMMENT (30) TEXT-s22 FOR FIELD rb_chgn.
  SELECTION-SCREEN END OF LINE.

* Text-s06: Display
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 1.
    PARAMETERS rb_disn RADIOBUTTON GROUP gn0.
    SELECTION-SCREEN COMMENT (30) TEXT-s23 FOR FIELD rb_disn.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK bn2.

*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bn5 WITH FRAME TITLE TEXT-s03.
  PARAMETERS: rb_tran RADIOBUTTON GROUP op DEFAULT 'X' USER-COMMAND op
               MODIF ID op,
              rb_pdc  RADIOBUTTON GROUP op MODIF ID op.
  PARAMETERS: rb_bank RADIOBUTTON GROUP op MODIF ID op,
              rb_ar   RADIOBUTTON GROUP op MODIF ID op.
SELECTION-SCREEN END OF BLOCK bn5.

*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bn3 WITH FRAME TITLE TEXT-s02.

  SELECT-OPTIONS:
    s_wrkdtn   FOR gs_zsdsfit045-work_date MODIF ID chg,
    s_trnfn    FOR  gs_zsdsfit045-tranf_no MODIF ID trn,
    s_kunnrn   FOR  gs_bsid-kunnr MODIF ID csn,
    s_xblnrn   FOR  gs_bsid-xblnr MODIF ID csn.

SELECTION-SCREEN END OF BLOCK bn3.


*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bn4 WITH FRAME TITLE TEXT-s08.
  PARAMETERS:
    p_dfpymn TYPE zsdsfit045-pymt_method MODIF ID dfn NO-DISPLAY,
    p_dfactn TYPE zsdsfit045-action_type MODIF ID dfn NO-DISPLAY,
    p_dfstan TYPE zsdsfit045-status      MODIF ID dfn NO-DISPLAY,
    p_dfhbkn TYPE zsdsfit045-hbkid       MODIF ID dfn NO-DISPLAY,
    p_dfhktn TYPE zsdsfit045-hktid       MODIF ID dfn NO-DISPLAY,
    p_dfbkdn TYPE zsdsfit045-bank_date   MODIF ID dfn NO-DISPLAY,
    p_dfchqn TYPE zsdsfit045-cheque_no   MODIF ID dfn NO-DISPLAY.

SELECTION-SCREEN END OF BLOCK bn4.

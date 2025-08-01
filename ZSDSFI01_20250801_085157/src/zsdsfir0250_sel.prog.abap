*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0250_SEL
*  Creation Date      : 21.05.2024
*  Author             : Apichat Ch. (Eviden)
*  Add-on ID          : N/A
*  Description        : This is include program for Global data
*                       and selection screen
*  Purpose            : Include program for selection screen
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

*----------------------------------------------------------------------*
* SELECTION-SCREEN 100
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 100 AS SUBSCREEN.
* Text-s01: Customer Selection
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
    PARAMETERS:
      p_bukrs  TYPE knb1-bukrs OBLIGATORY,
      p_pernr  TYPE char08 MATCHCODE OBJECT zsdsh_pernr,
      p_wrkdt  TYPE zsdsfit029-work_date,
      p_rcvamt TYPE zsdsfis143-received_amount MODIF ID amt.
  SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s07.
* Text-s04: Create
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 1.
      PARAMETERS rb_cre RADIOBUTTON GROUP g0 DEFAULT 'X' USER-COMMAND sel_mode.
      SELECTION-SCREEN COMMENT (50) TEXT-s04 FOR FIELD rb_cre.
    SELECTION-SCREEN END OF LINE.

* Text-s05: Change collection log for receiving payment
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 1.
      PARAMETERS rb_chg RADIOBUTTON GROUP g0.
      SELECTION-SCREEN COMMENT (50) TEXT-s05 FOR FIELD rb_chg.
    SELECTION-SCREEN END OF LINE.

* Text-s06: Display
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 1.
      PARAMETERS rb_dis RADIOBUTTON GROUP g0.
      SELECTION-SCREEN COMMENT (50) TEXT-s06 FOR FIELD rb_dis.
    SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-s08.
    PARAMETERS:
      p_dfpymt TYPE zsdsfit029-pymt_method MODIF ID def,
      p_dfact  TYPE zsdsfit029-action_type MODIF ID def,
      p_dfsta  TYPE zsdsfit029-status      MODIF ID def,
      p_dfhbk  TYPE zsdsfis143-hbkid       MODIF ID def,
      p_dfhkt  TYPE zsdsfis143-hktid       MODIF ID def,
      p_dfbkdt TYPE zsdsfis143-cheque_date MODIF ID def,
      p_dfchq  TYPE zsdsfit029-cheque_no   MODIF ID def,
      p_zbank  TYPE zsdsfis143-zbank_item  MODIF ID def.
  SELECTION-SCREEN END OF BLOCK b4.

*----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-s02.

*<<F36K917931 - 02 BOD
*    PARAMETERS:
*      cb_inck AS CHECKBOX USER-COMMAND include_inv_chk MODIF ID cre,
*      cb_inbl AS CHECKBOX USER-COMMAND exclude_inv MODIF ID cre.
*<<F36K917931 - 02 EOD

*<<F36K917931 - 02 BOI
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 1.
      PARAMETERS: cb_inck AS CHECKBOX USER-COMMAND include_inv_chk MODIF ID cre.
      SELECTION-SCREEN COMMENT (28) TEXT-s12 FOR FIELD cb_inck.

      SELECTION-SCREEN POSITION 33.
      PARAMETERS: cb_inbl AS CHECKBOX USER-COMMAND exclude_inv MODIF ID cre.
      SELECTION-SCREEN COMMENT (28) TEXT-s13 FOR FIELD cb_inbl.

    SELECTION-SCREEN END OF LINE.
*<<F36K917931 - 02 EOI


    SELECT-OPTIONS:
      s_trnf    FOR  gs_zsdsfit029-tranf_no MODIF ID trf,
      s_kunnr   FOR  gs_bsid-kunnr MODIF ID cus.

*<<F36K917931 - 02 BOD
*    SELECTION-SCREEN BEGIN OF LINE.
*      SELECTION-SCREEN POSITION 33.
*      PARAMETERS: cb_norm AS CHECKBOX DEFAULT gc_true MODIF ID cus.
*      SELECTION-SCREEN COMMENT (20) TEXT-s10 FOR FIELD cb_norm.
*    SELECTION-SCREEN END OF LINE.
*
*    SELECTION-SCREEN BEGIN OF LINE.
*      SELECTION-SCREEN POSITION 33.
*      PARAMETERS: cb_spgl AS CHECKBOX DEFAULT gc_true MODIF ID cus.
*      SELECTION-SCREEN COMMENT (20) TEXT-s11 FOR FIELD cb_spgl.
*    SELECTION-SCREEN END OF LINE.
*<<F36K917931 - 02 EOD

*<<F36K917931 - 02 BOI
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 1.
      PARAMETERS: cb_norm AS CHECKBOX DEFAULT gc_true MODIF ID cus.
      SELECTION-SCREEN COMMENT (20) TEXT-s10 FOR FIELD cb_norm.

      SELECTION-SCREEN POSITION 33.
      PARAMETERS: cb_spgl AS CHECKBOX DEFAULT gc_true MODIF ID cus.
      SELECTION-SCREEN COMMENT (20) TEXT-s11 FOR FIELD cb_spgl.

    SELECTION-SCREEN END OF LINE.
*<<F36K917931 - 02 EOI

    SELECT-OPTIONS:
       s_bill   FOR gs_zsdsfit029-billpl_no   MODIF ID bi1,
       s_pldate FOR gs_zsdsfit029-billpl_date MODIF ID bi1,
       s_pymn   FOR gs_zsdsfit029-pymt_method MODIF ID bl2, "<<F36K914812 - 03 insert
       s_acttyp FOR gs_zsdsfit029-action_type MODIF ID bl2,
       s_status FOR gs_zsdsfit029-status      MODIF ID bl2.

    SELECT-OPTIONS:
       s_belnr FOR gs_bsid-belnr MODIF ID inv,
       s_gjahr FOR gs_bsid-gjahr MODIF ID inv,
       s_xblnr FOR gs_bsid-xblnr MODIF ID inv.

  SELECTION-SCREEN END OF BLOCK b3.

*  SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-s43. *<<F36K917931 - 02 del
  PARAMETERS:
    p_var    TYPE disvariant-variant MEMORY ID zsds_var.
*  SELECTION-SCREEN END OF BLOCK b5. *<<F36K917931 - 02 del

SELECTION-SCREEN END OF SCREEN 100.

*----------------------------------------------------------------------*
* SELECTION-SCREEN 200
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 200 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK bn1 WITH FRAME TITLE TEXT-s01.
    PARAMETERS:
      p_bukrsn TYPE knb1-bukrs OBLIGATORY,
      p_pernrn TYPE char08 MATCHCODE OBJECT zsdsh_pernr,
      p_wrkdtn TYPE zsdsfit043-work_date,
      p_rcvamn TYPE zsdsfit043-wrbtr.

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
  SELECTION-SCREEN BEGIN OF BLOCK bn3 WITH FRAME TITLE TEXT-s02.

    SELECT-OPTIONS:
      s_trnfn    FOR  gs_zsdsfit029-tranf_no MODIF ID trn,
      s_kunnrn   FOR  gs_bsid-kunnr MODIF ID csn,
      s_xblnrn   FOR  gs_bsid-xblnr MODIF ID csn.

  SELECTION-SCREEN END OF BLOCK bn3.


*----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK bn4 WITH FRAME TITLE TEXT-s08.
    PARAMETERS:
      p_dfpymn TYPE zsdsfit043-pymt_method MODIF ID dfn,
      p_dfactn TYPE zsdsfit043-action_type MODIF ID dfn,
      p_dfstan TYPE zsdsfit043-status      MODIF ID dfn,
      p_dfhbkn TYPE zsdsfit043-hbkid       MODIF ID dfn,
      p_dfhktn TYPE zsdsfit043-hktid       MODIF ID dfn,
      p_dfbkdn TYPE zsdsfit043-bank_date   MODIF ID dfn,
      p_dfchqn TYPE zsdsfit043-cheque_no   MODIF ID dfn.

  SELECTION-SCREEN END OF BLOCK bn4.

  SELECTION-SCREEN BEGIN OF BLOCK bn5 WITH FRAME TITLE TEXT-s43.
    PARAMETERS:
      p_nvar    TYPE disvariant-variant.
  SELECTION-SCREEN END OF BLOCK bn5.

SELECTION-SCREEN END OF SCREEN 200.


*----------------------------------------------------------------------*
* SELECTION-SCREEN 300
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 300 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK bu1 WITH FRAME TITLE TEXT-s01.
    PARAMETERS:
      p_bukrsu TYPE knb1-bukrs OBLIGATORY,
      p_pernru TYPE char08 MATCHCODE OBJECT zsdsh_pernr,
      p_wrkdtu TYPE zsdsfit043-work_date.

  SELECTION-SCREEN END OF BLOCK bu1.


*----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK bu2 WITH FRAME TITLE TEXT-s07.

* Text-s04: Create
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 1.
      PARAMETERS rb_creu RADIOBUTTON GROUP bu0 DEFAULT 'X' USER-COMMAND sel_modeu.
      SELECTION-SCREEN COMMENT (50) TEXT-s04 FOR FIELD rb_creu.
    SELECTION-SCREEN END OF LINE.

* Text-s09: Change collection log before receiving payment
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 1.
      PARAMETERS rb_chgu RADIOBUTTON GROUP bu0.
      SELECTION-SCREEN COMMENT (50) TEXT-s09 FOR FIELD rb_chgu.
    SELECTION-SCREEN END OF LINE.

* Text-s06: Display
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 1.
      PARAMETERS rb_disu RADIOBUTTON GROUP bu0.
      SELECTION-SCREEN COMMENT (50) TEXT-s06 FOR FIELD rb_disu.
    SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN END OF BLOCK bu2.

*----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK bu4 WITH FRAME TITLE TEXT-s08.
    PARAMETERS:
      p_dfpymu TYPE zsdsfit029-pymt_method MODIF ID dfu,
      p_dfactu TYPE zsdsfit029-action_type MODIF ID dfu,
      p_dfstau TYPE zsdsfit029-status      MODIF ID dfu,
      p_dfhbku TYPE zsdsfit029-hbkid       MODIF ID dfu,
      p_dfhktu TYPE zsdsfit029-hktid       MODIF ID dfu,
      p_dfbkdu TYPE zsdsfit029-bank_date   MODIF ID dfu,
      p_dfchqu TYPE zsdsfit029-cheque_no   MODIF ID dfu.

  SELECTION-SCREEN END OF BLOCK bu4.

*----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK bu3 WITH FRAME TITLE TEXT-s02.

*<<F36K917931 - 02 BOD
*    PARAMETERS:
*      cb_incku AS CHECKBOX USER-COMMAND include_invu MODIF ID cru,
*      cb_inblu AS CHECKBOX USER-COMMAND include_blu  MODIF ID cru.
*<<F36K917931 - 02 EOD

*<<F36K917931 - 02 BOI
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 1.
      PARAMETERS: cb_incku AS CHECKBOX USER-COMMAND include_invu MODIF ID cru.
      SELECTION-SCREEN COMMENT (28) TEXT-s12 FOR FIELD cb_incku.

      SELECTION-SCREEN POSITION 33.
      PARAMETERS: cb_inblu AS CHECKBOX USER-COMMAND include_blu MODIF ID cru.
      SELECTION-SCREEN COMMENT (28) TEXT-s13 FOR FIELD cb_inblu.

    SELECTION-SCREEN END OF LINE.
*<<F36K917931 - 02 EOI

    SELECT-OPTIONS:
      s_trnfu    FOR  gs_zsdsfit029-tranf_no MODIF ID tru,
      s_kunnru   FOR  gs_bsid-kunnr MODIF ID cuu.

*<<F36K917931 - 02 BOD
*    SELECTION-SCREEN BEGIN OF LINE.
*      SELECTION-SCREEN POSITION 33.
*      PARAMETERS: cb_normu AS CHECKBOX DEFAULT gc_true MODIF ID cuu.
*      SELECTION-SCREEN COMMENT (20) TEXT-s10 FOR FIELD cb_normu.
*    SELECTION-SCREEN END OF LINE.
*
*    SELECTION-SCREEN BEGIN OF LINE.
*      SELECTION-SCREEN POSITION 33.
*      PARAMETERS: cb_spglu AS CHECKBOX DEFAULT gc_true MODIF ID cuu.
*      SELECTION-SCREEN COMMENT (20) TEXT-s11 FOR FIELD cb_spglu.
*    SELECTION-SCREEN END OF LINE.
*<<F36K917931 - 02 EOD

*<<F36K917931 - 02 BOI
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 1.
      PARAMETERS: cb_normu AS CHECKBOX DEFAULT gc_true MODIF ID cuu.
      SELECTION-SCREEN COMMENT (20) TEXT-s10 FOR FIELD cb_normu.

      SELECTION-SCREEN POSITION 33.
      PARAMETERS: cb_spglu AS CHECKBOX DEFAULT gc_true MODIF ID cuu.
      SELECTION-SCREEN COMMENT (20) TEXT-s11 FOR FIELD cb_spglu.

    SELECTION-SCREEN END OF LINE.
*<<F36K917931 - 02 EOI

    SELECT-OPTIONS:
       s_billu  FOR gs_zsdsfit029-billpl_no   MODIF ID bi3,
       s_pldatu FOR gs_zsdsfit029-billpl_date MODIF ID bi3,
       s_pymnu  FOR gs_zsdsfit029-pymt_method MODIF ID bi4, "<<F36K914812 - 03 insert
       s_acttyu FOR gs_zsdsfit029-action_type MODIF ID bi4,
       s_statuu FOR gs_zsdsfit029-status      MODIF ID bi4.

    SELECT-OPTIONS:
       s_belnru FOR gs_bsid-belnr ,
       s_gjahru FOR gs_bsid-gjahr ,
       s_xblnru FOR gs_bsid-xblnr .

  SELECTION-SCREEN END OF BLOCK bu3.

*  SELECTION-SCREEN BEGIN OF BLOCK bu5 WITH FRAME TITLE TEXT-s43. *<<F36K917931 - 02 del
    PARAMETERS:
      p_uvar    TYPE disvariant-variant MEMORY ID zsds_var_upd.
*  SELECTION-SCREEN END OF BLOCK bu5. *<<F36K917931 - 02 del

SELECTION-SCREEN END OF SCREEN 300.


*----------------------------------------------------------------------*
* SELECTION-SCREEN 350
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 350 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK bm1 WITH FRAME TITLE TEXT-s01.
    PARAMETERS:
      p_bukrsm TYPE knb1-bukrs OBLIGATORY,
      p_pernrm TYPE char08 MATCHCODE OBJECT zsdsh_pernr,
      p_wrkdtm TYPE zsdsfit043-work_date.

  SELECTION-SCREEN END OF BLOCK bm1.

*----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK bm2 WITH FRAME TITLE TEXT-s07.
* Text-s04: Create
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 1.
      PARAMETERS rb_crem RADIOBUTTON GROUP gm0 DEFAULT 'X' USER-COMMAND sel_modem.
      SELECTION-SCREEN COMMENT (30) TEXT-s31 FOR FIELD rb_crem.
    SELECTION-SCREEN END OF LINE.

* Text-s05: Change
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 1.
      PARAMETERS rb_chgm RADIOBUTTON GROUP gm0.
      SELECTION-SCREEN COMMENT (30) TEXT-s32 FOR FIELD rb_chgm.
    SELECTION-SCREEN END OF LINE.

* Text-s06: Display
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 1.
      PARAMETERS rb_dism RADIOBUTTON GROUP gm0.
      SELECTION-SCREEN COMMENT (30) TEXT-s33 FOR FIELD rb_dism.
    SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN END OF BLOCK bm2.

*----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK bm3 WITH FRAME TITLE TEXT-s02.

    SELECT-OPTIONS:
      s_trnfm    FOR gs_zsdsfit029-tranf_no    MODIF ID trm,
      s_kunnrm   FOR gs_bsid-kunnr             MODIF ID csm,
      s_acttym   FOR gs_zsdsfit029-action_type MODIF ID csm,
      s_statum   FOR gs_zsdsfit029-status      MODIF ID csm,
      s_belnrm   FOR gs_zsdsfit029-belnr       MODIF ID csm,
      s_gjahrm   FOR gs_bsid-gjahr             MODIF ID csm,
      s_xblnrm   FOR gs_bsid-xblnr             MODIF ID csm.

  SELECTION-SCREEN END OF BLOCK bm3.


*----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK bm4 WITH FRAME TITLE TEXT-s08.
    PARAMETERS:
      p_dfpymm TYPE zsdsfit043-pymt_method MODIF ID dfm,
      p_dfactm TYPE zsdsfit043-action_type MODIF ID dfm,
      p_dfstam TYPE zsdsfit043-status      MODIF ID dfm,
      p_dfhbkm TYPE zsdsfit043-hbkid       MODIF ID dfm,
      p_dfhktm TYPE zsdsfit043-hktid       MODIF ID dfm,
      p_dfbkdm TYPE zsdsfit043-bank_date   MODIF ID dfm,
      p_dfchqm TYPE zsdsfit043-cheque_no   MODIF ID dfm.

  SELECTION-SCREEN END OF BLOCK bm4.

  SELECTION-SCREEN BEGIN OF BLOCK bm5 WITH FRAME TITLE TEXT-s43.
    PARAMETERS:
      p_mvar    TYPE disvariant-variant MEMORY ID zsds_var_memo.
  SELECTION-SCREEN END OF BLOCK bm5.

SELECTION-SCREEN END OF SCREEN 350.

*----------------------------------------------------------------------*
* SELECTION-SCREEN 400
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 400 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK h41 WITH FRAME TITLE TEXT-s01.

* Text-s05: Change collection log for receiving payment
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 1.
      PARAMETERS rb_hchg RADIOBUTTON GROUP h40
                         USER-COMMAND dmy.  "Added 06.12.2024
      SELECTION-SCREEN COMMENT (50) TEXT-s05 FOR FIELD rb_chg.
    SELECTION-SCREEN END OF LINE.

* Text-s06: Display
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 1.
      PARAMETERS rb_hdis RADIOBUTTON GROUP h40.
      SELECTION-SCREEN COMMENT (50) TEXT-s06 FOR FIELD rb_dis.
    SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN END OF BLOCK h41.

  SELECTION-SCREEN BEGIN OF BLOCK h42 WITH FRAME TITLE TEXT-s02.

*<-- Start of Insertion 06.12.2024 (New Criteria)
    PARAMETERS:
      cb_wtdel TYPE char1 AS CHECKBOX DEFAULT ' ' MODIF ID dis.
*--> End of Insertion 06.12.2024
    SELECT-OPTIONS:
      s_htype   FOR gs_zsdsfit029-data_type,
      s_hwrkdt  FOR gs_zsdsfit029-work_date,
      s_hpernr  FOR gs_zsdsfit029-pernr MATCHCODE OBJECT zsdsh_pernr,
      s_htrnf   FOR gs_zsdsfit029-tranf_no ,
      s_hpymt   FOR gs_zsdsfit029-pymt_method,
      s_hactty  FOR gs_zsdsfit029-action_type ,
      s_hstat   FOR gs_zsdsfit029-status      ,
      s_hkunnr  FOR gs_zsdsfit029-kunnr ,
      s_hvbeln  FOR gs_zsdsfit029-vbeln_vf ,
      s_hxblnr  FOR gs_zsdsfit029-xblnr ,
      s_hbelnr  FOR gs_zsdsfit029-belnr ,
      s_humskz  FOR gs_zsdsfit029-umskz ,
      s_hgjahr  FOR gs_zsdsfit029-gjahr ,
      s_hbill   FOR gs_zsdsfit029-billpl_no ,
      s_hpldat  FOR gs_zsdsfit029-billpl_date ,
      s_hbankl  FOR gs_zsdsfit029-bankl       ,
      s_hbnkdt  FOR gs_zsdsfit029-bank_date   ,
      s_hfllw   FOR gs_zsdsfit029-follow_date ,
      s_hchq    FOR gs_zsdsfit029-cheque_no.

  SELECTION-SCREEN END OF BLOCK h42.

  SELECTION-SCREEN BEGIN OF BLOCK h43 WITH FRAME TITLE TEXT-s43.
    PARAMETERS:
      p_hvar    TYPE disvariant-variant MEMORY ID zsds_var_his.
  SELECTION-SCREEN END OF BLOCK h43.

SELECTION-SCREEN END OF SCREEN 400.

*----------------------------------------------------------------------*
* SELECTION-SCREEN TAB
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF TABBED BLOCK tab_blk FOR 30 LINES,
TAB (25) tab_bt1 USER-COMMAND col_log,
*TAB (25) tab_bt2 USER-COMMAND new_ent,
TAB (25) tab_bt3 USER-COMMAND upd_bnk,
TAB (25) tab_bt5 USER-COMMAND memo,
TAB (25) tab_bt4 USER-COMMAND hist_log,
END OF BLOCK tab_blk.

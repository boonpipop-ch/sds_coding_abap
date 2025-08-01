*&---------------------------------------------------------------------*
*& Report ZSDSFIR0170
*  Author             : Wantanee Prateep na thalang
*  Add-on ID          : ZFIAPR001
*  Description        : Report ap aging detail
*  Purpose            :
*  Copied from        :  ZR_AP_INV_AGING_REP_FYC_DETAIL
*  Restriction        :
*&---------------------------------------------------------------------*
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSFIR0170.

*&-----------------------------------------------------------------------------------*
* I N C L U D E S
*&-----------------------------------------------------------------------------------*

*&-----------------------------------------------------------------------------------*
*& T A B L E S
*&-----------------------------------------------------------------------------------*
TABLES : lfa1,
         lfb1,
         bsik,
         bseg,
         bsec,
         bkpf,
         bsega.
*&-----------------------------------------------------------------------------------*
* D A T A
*&-----------------------------------------------------------------------------------*
DATA : gv_chk   TYPE c,
       gv_gjahr TYPE gjahr,
       gv_periv TYPE periv,
       gv_hwaers TYPE waers.

TYPE-POOLS: slis.
DATA : gt_fieldcat TYPE slis_t_fieldcat_alv,
       gt_layout   TYPE slis_layout_alv,
       gt_events   TYPE slis_t_event,
       gt_heading  TYPE slis_t_listheader.

DATA : v_pos TYPE i .

*&-----------------------------------------------------------------------------------*
* I N I T I A L I Z A T I O N
*&-----------------------------------------------------------------------------------*
INITIALIZATION.
  CLEAR gv_chk.

*&-----------------------------------------------------------------------------------*
*& C O N S T A N T
*&-----------------------------------------------------------------------------------*
  CONSTANTS : gc_true  TYPE c VALUE 'X',
              gc_repid TYPE sy-repid VALUE 'ZSDSFIR0170',
              gc_partial TYPE char25 VALUE 'PARTIAL_BLART_*',
              gc_mask_date TYPE char10 VALUE '__.__.____',
              gc_mask_time TYPE char8  VALUE '__:__:__'.

*&-----------------------------------------------------------------------------------*
*& V A R I A B L E
*&-----------------------------------------------------------------------------------*
  DATA : gv_flag TYPE c.

*&-----------------------------------------------------------------------------------*
*& R A N G E S
*&-----------------------------------------------------------------------------------*
  RANGES : r_bstat FOR bsik-bstat,
           r_date1 FOR rfpdo1-allgrogr,
           r_date2 FOR rfpdo1-allgrogr,
           r_date3 FOR rfpdo1-allgrogr,
           r_date4 FOR rfpdo1-allgrogr,
           r_date5 FOR rfpdo1-allgrogr,
           r_mr8m  FOR bseg-belnr,
           r_partial FOR bsik-blart,
           r_gjahr FOR bsik-gjahr.

*&-----------------------------------------------------------------------------------*
*& T Y P E S
*&-----------------------------------------------------------------------------------*
  TYPES : BEGIN OF gy_output,
          breif(50) TYPE c,
          lifnr TYPE lifnr,
          name1 TYPE name1,
          belnr TYPE belnr_d,
          bukrs TYPE bukrs,
          gjahr TYPE gjahr,
          blart TYPE blart,
          xblnr TYPE xblnr,
          sgtxt TYPE sgtxt,
*          zbd1t TYPE dzbd1t,
          zbd1t TYPE char3,
          pstdt TYPE budat,
          invdt TYPE bldat,
          netdt TYPE bldat,
          waers TYPE waers,
          hwaers TYPE waers,
*          invdy TYPE p,
*          netdy TYPE p,
          invdy TYPE char8,
          netdy TYPE char8,
          wrbtr TYPE wrbtr,
*          kursf TYPE kursf,
          kursf TYPE char10,
          dmbtr TYPE dmbtr,
          hwbas TYPE hwbas,
          hwste TYPE hwste,
          remain_amt TYPE dmbtr,
          remain_amt_doc TYPE wrbtr,
          amt_1 TYPE wrbtr,
          amt_2 TYPE wrbtr,
          amt_3 TYPE wrbtr,
          amt_4 TYPE wrbtr,
          amt_5 TYPE wrbtr,
          aufnr TYPE aufk-aufnr,  "Add by Wantanee 20131207
          ktext TYPE aufk-ktext,  "Add by Wantanee 20131207
          usnam TYPE bkpf-usnam,   "Add by Wantanee 20190123
          hkont TYPE bseg-hkont,   "GL Account
          dmbtr_i TYPE bseg-dmbtr,   "Amount
          shkzg TYPE bseg-shkzg,
          prctr TYPE bseg-prctr,
          gl_text TYPE  skat-txt50,
          buzei TYPE bseg-buzei,
          ph(18)    TYPE c,
          wt_withcd TYPE with_item-wt_withcd, "WHT code Add by Wantanee 20200914
          wt_text40 TYPE t059zt-text40, "WHT text Add by Wantanee 20200914
          ebeln     TYPE bseg-ebeln,  "PO NO  "Add by Wantanee CH3
          ebelp     TYPE bseg-ebelp,  "PO Item "Add by Wantanee CH3
          KVERM     TYPE lfb1-KVERM,  "VAT Type  "Add by Wantanee CH4
          mwskz    TYPE bseg-mwskz,  "CH4 Add by wantanee 20210226
          prctr_ap(150)  TYPE c,
          hkont_ap(150) TYPE c,   "GL Account
          WT_QSSHH  TYPE  with_item-WT_QSSHH,
          END OF gy_output.

  TYPES : BEGIN OF gy_vendor,
          lifnr TYPE lifnr,
          bukrs TYPE bukrs,
          name1 TYPE name1,
          akont TYPE akont,
          ktokk TYPE ktokk,
          KVERM TYPE lfb1-KVERM, "Add by Wantanee CH4
          END OF gy_vendor.

  TYPES : BEGIN OF gy_bset,
          bukrs TYPE bukrs,
          belnr TYPE belnr_d,
          gjahr TYPE gjahr,
*          buzei TYPE buzei,   " comment by  IMS 300000167
          hwbas TYPE hwbas,
          hwste TYPE hwste,
          flag_read  TYPE c ,  "read b only one time - IMS 300000167
          END OF gy_bset.

  TYPES : BEGIN OF gy_bsik2,
          bukrs TYPE bukrs,
          lifnr TYPE bsik-lifnr,  "Add by wantanee 20140306
          gjahr TYPE gjahr,
          belnr TYPE belnr_d,
          buzei TYPE buzei,
          rebzg TYPE rebzg,
          rebzj TYPE rebzj,
* Start of modify IMS#300000167 to add sign for amount.
          shkzg TYPE shkzg,
* End of modify IMS#300000167 to add sign for amount.
          dmbtr TYPE dmbtr,
          wrbtr TYPE wrbtr,
          flag  TYPE c,
          END OF gy_bsik2.

"Add by Wantanee 20190123
TYPES : BEGIN OF gy_bkpf,
        belnr  TYPE bkpf-belnr,
        gjahr  TYPE bkpf-gjahr,
        usnam  TYPE bkpf-usnam,
END OF gy_bkpf.
"End Add by Wantanee 20190123

TYPES: BEGIN OF gy_bseg,
       belnr    TYPE bseg-belnr,
       gjahr    TYPE bseg-gjahr,
       buzei    TYPE bseg-buzei,
       dmbtr    TYPE bseg-dmbtr,
       shkzg    TYPE bseg-shkzg,
       aufnr    TYPE bseg-aufnr,
       vbel2    TYPE bseg-vbel2,
       posn2    TYPE bseg-posn2,
       hkont    TYPE bseg-hkont,
       lifnr    TYPE bseg-lifnr,
       prctr    TYPE bseg-prctr,
       sgtxt    TYPE bseg-sgtxt,
       ebeln    TYPE bseg-ebeln,
       ebelp    TYPE bseg-ebelp,
       mwskz    TYPE bseg-mwskz,  "CH4 Add by wantanee 20210226

END OF gy_bseg.
TYPES: BEGIN OF gy_vbap,
       vbeln TYPE vbeln_va,
       posnr TYPE posnr_va,
       prodh TYPE prodh_d,
END OF gy_vbap.
TYPES: BEGIN OF gy_ce11000,
       rbeln TYPE rkerfbelnr,
       rposn TYPE rkerfposnr,
       paph1 TYPE CE11000-paph1,
       wwphz TYPE char10,
       gjahr TYPE gjahr,
       kstar TYPE kstar,
       rkaufnr TYPE AUFNR,
       END OF gy_ce11000.
TYPES: BEGIN OF gy_bseg_sale,
       vbel2    TYPE bseg-vbel2,
       posn2    TYPE bseg-posn2,

END OF gy_bseg_sale.

TYPES: BEGIN OF gy_skat,
       saknr TYPE skat-saknr,
       txt50 TYPE skat-txt50,
END OF gy_skat.
TYPES: BEGIN OF gy_ce2,
       aufnr    TYPE aufnr,
       iloan    TYPE iloan,
END OF gy_ce2.

TYPES: BEGIN OF gy_ce2t,
       aufnr    TYPE aufnr,
       iloan    TYPE iloan,
       abckz    TYPE abckz,
END OF gy_ce2t.

TYPES: BEGIN OF gy_ekkn,
       ebeln    TYPE ekkn-ebeln,
       ebelp    TYPE ekkn-ebelp,
       sakto    TYPE ekkn-sakto,
END OF gy_ekkn.
"Add by Wantanee 20200914
TYPES: BEGIN OF gy_with_item,
       belnr   TYPE with_item-belnr,
       gjahr   TYPE with_item-gjahr,
       buzei   TYPE with_item-buzei,
       witht   TYPE with_item-witht,
       wt_withcd TYPE with_item-wt_withcd,
       WT_QSSHH TYPE with_item-WT_QSSHH,
END OF gy_with_item.


TYPES: BEGIN OF gy_t059zt,
       witht     TYPE t059zt-witht,
       wt_withcd TYPE t059zt-wt_withcd,
       text40    TYPE t059zt-text40,
END OF gy_t059zt.
"End Add by Wantanee 20200914

"CH5 Add by Wantanee 20210426
 TYPES: BEGIN OF gy_prctr,
          belnr TYPE belnr_d,
          gjahr TYPE gjahr,
          prctr TYPE bseg-prctr,
 END OF gy_prctr.
  TYPES: BEGIN OF gy_hkont,
          belnr TYPE belnr_d,
          gjahr TYPE gjahr,
          hkont TYPE bseg-hkont,
 END OF gy_hkont.
"CH5 End Add by Wantanee 20210426
*&-----------------------------------------------------------------------------------*
*& S T R U C T U R E
*&-----------------------------------------------------------------------------------*
  DATA: gw_bsik    TYPE bsik,
        gw_bsik_bk TYPE bsik,
        gw_output  TYPE gy_output,
        gw_vendor  TYPE gy_vendor,
        gw_bset    TYPE gy_bset,
        gw_bsik2   TYPE gy_bsik2.
*&-----------------------------------------------------------------------------------*
*& I N T E R N A L   T A B L E S
*&-----------------------------------------------------------------------------------*
  DATA: gt_bsik    TYPE SORTED TABLE OF bsik WITH
                     UNIQUE KEY bukrs lifnr gjahr belnr buzei,
        gt_bsik_bk TYPE STANDARD TABLE OF bsik,
        gt_output  TYPE STANDARD TABLE OF gy_output,
        gt_vendor  TYPE STANDARD TABLE OF gy_vendor,
        gt_bset    TYPE STANDARD TABLE OF gy_bset,
        gt_bset_tab    TYPE STANDARD TABLE OF gy_bset,  "IMS 300000167
        gt_bsik2   TYPE STANDARD TABLE OF gy_bsik2.
  DATA: gt_bseg   TYPE STANDARD TABLE OF gy_bseg,
        gs_bseg   TYPE gy_bseg,
        gt_vbap   TYPE STANDARD TABLE OF gy_vbap,
        gs_vbap   TYPE gy_vbap,
        gt_ce11000 TYPE STANDARD TABLE OF gy_ce11000,
        gs_ce11000 TYPE gy_ce11000,
        gt_bseg_sale TYPE STANDARD TABLE OF gy_bseg_sale,
        gs_bseg_sale TYPE gy_bseg_sale,
        gt_skat    TYPE STANDARD TABLE OF gy_skat,
        gs_skat    TYPE gy_skat,
        gt_ce2    TYPE STANDARD TABLE OF gy_ce2,
        gs_ce2      TYPE gy_ce2,
        gt_ce2t    TYPE STANDARD TABLE OF gy_ce2t,
        gs_ce2t     TYPE gy_ce2t,
        gt_ce2tt    TYPE STANDARD TABLE OF gy_ce2t,
        gs_ce2tt    TYPE gy_ce2t.
  DATA: gt_output_temp TYPE STANDARD TABLE OF gy_output.

  DATA: gt_ekkn   TYPE STANDARD TABLE OF gy_ekkn,
        gs_ekkn   TYPE gy_ekkn,
        gs_ekkn1   TYPE gy_ekkn.

  "Add by Wantanee 20190123
  DATA: gt_bkpf   TYPE STANDARD TABLE OF gy_bkpf,
        gs_bkpf   TYPE gy_bkpf.
  "End add by wantanee 20190123

  "CH5  add by Wantanee 20210426
  DATA: gt_prctr_temp   TYPE STANDARD TABLE OF gy_prctr,
        gs_prctr_temp   TYPE gy_prctr,
        gt_hkont_temp   TYPE STANDARD TABLE OF gy_hkont,
        gs_hkont_temp   TYPE gy_hkont.
  "CH5  end add by Wantanee 20210426

  "Add by Wantanee 20200914
  DATA: gt_with_item TYPE STANDARD TABLE OF gy_with_item,
        gs_with_item TYPE gy_with_item,
        gt_t059zt    TYPE STANDARD TABLE OF gy_t059zt,
        gs_t059zt    TYPE gy_t059zt.
  "End Add by Wantanee 20200914
*&-----------------------------------------------------------------------------------*
*& S E L E C T I O N - S C R E E N
*&-----------------------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK block0 WITH FRAME TITLE text-se0.
  PARAMETER : p_bukrs TYPE bukrs OBLIGATORY DEFAULT '1000'.
* begin of change : IMS 3000000043

*-->> Begin Remark Parinya24Jan2012
*-> Search key : 20120131
*              p_gjahr TYPE gjahr .
*-->> End Remark Parinya24Jan2012

*              p_gjahr TYPE gjahr OBLIGATORY DEFAULT sy-datum(4).
* end of change : IMS 3000000043
  SELECT-OPTIONS : s_ktokk FOR lfa1-ktokk,         "Vendor Group
                   s_akont FOR lfb1-akont,         "Reconcile Account
                   s_lifnr FOR lfa1-lifnr,         "Vendor
                   s_belnr FOR bsik-belnr.         "Document number
  PARAMETER : p_datum TYPE bldat OBLIGATORY DEFAULT sy-datum . "As of Date

* for option Due Date
  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETER : r_netdt RADIOBUTTON GROUP typ DEFAULT 'X'.
  SELECTION-SCREEN COMMENT 05(24) text-001.
  SELECT-OPTIONS : s_netdt FOR bsega-netdt.
  SELECTION-SCREEN END OF LINE.
* for option Invoice Date
  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETER : r_invdt RADIOBUTTON GROUP typ .
  SELECTION-SCREEN COMMENT 05(24) text-002 .
  SELECT-OPTIONS : s_invdt FOR bsega-netdt.
  SELECTION-SCREEN END OF LINE.

  SELECT-OPTIONS : s_budat FOR bsik-budat.               "Posting Date
  SELECT-OPTIONS : s_waers FOR bsik-waers NO INTERVALS.  "Currency

  SELECTION-SCREEN END OF BLOCK block0.

  SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-se1.
  PARAMETER : p_normal AS CHECKBOX DEFAULT 'X'.          "Normal Item
  SELECT-OPTIONS : s_umskz FOR bseg-umskz.                "Special GL Transaction
  SELECTION-SCREEN END OF BLOCK block1.

  SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-se2.
  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 01(31) text-003 FOR FIELD p_d1.
  PARAMETERS: p_d1 LIKE rfpdo1-allgrogr DEFAULT '000'.  PARAMETERS: p_d2 LIKE rfpdo1-allgrogr DEFAULT '030'.
  PARAMETERS: p_d3 LIKE rfpdo1-allgrogr DEFAULT '060'.
  PARAMETERS: p_d4 LIKE rfpdo1-allgrogr DEFAULT '090'.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK block2.

*&-----------------------------------------------------------------------------------*
*      A T     S E L E C T I O N     S C R E E N
*&-----------------------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF p_d4 IS INITIAL AND p_d3 IS INITIAL AND
     p_d2 IS INITIAL AND p_d1 IS INITIAL.
*    MESSAGE e999 WITH 'Please input Aging Days'.
  ENDIF.

  IF NOT p_d4 IS INITIAL.
    IF  p_d4 GT p_d3
    AND p_d3 GT p_d2
    AND p_d2 GT p_d1.
    ELSE.
*      MESSAGE e003.
    ENDIF.
  ELSE.
    IF NOT p_d3 IS INITIAL.
      IF  p_d3 GT p_d2
      AND p_d2 GT p_d1.
      ELSE.
*        MESSAGE e003.
      ENDIF.
    ELSE.
      IF NOT p_d2 IS INITIAL.
        IF  p_d2 GT p_d1.
        ELSE.
*          MESSAGE e003.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*&-----------------------------------------------------------------------------------*
*& * START-OF-SELECTION
*&-----------------------------------------------------------------------------------*
START-OF-SELECTION.
  CHECK gv_chk IS INITIAL.
  PERFORM get_genc.
  PERFORM check_flag.
  PERFORM check_day.
  PERFORM get_currency.
  PERFORM get_range.
  PERFORM get_year.
  PERFORM get_data.
  PERFORM map_data.
  PERFORM get_additional.

*&-----------------------------------------------------------------------------------*
*& * END-OF-SELECTION
*&-----------------------------------------------------------------------------------*
END-OF-SELECTION.
  IF NOT gt_output[] IS INITIAL.

    SORT gt_output BY lifnr belnr buzei.
    PERFORM display_reprot.
  ELSE.
*    MESSAGE i004.
    EXIT.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  REFRESH : gt_bsik ,gt_bset_tab , gt_bset .
  CLEAR gw_bsik.

* get data from BSIK
  SELECT *
  INTO TABLE gt_bsik
  FROM bsik
  WHERE bukrs EQ p_bukrs
    AND lifnr IN s_lifnr
    AND umskz IN s_umskz
    AND gjahr IN r_gjahr
    AND ( budat IN s_budat AND
          budat LE p_datum )
    AND belnr IN s_belnr
    AND waers IN s_waers.

* get data from BSAK
  SELECT *
  APPENDING TABLE gt_bsik
  FROM bsak
  WHERE bukrs EQ p_bukrs
    AND lifnr IN s_lifnr
    AND umskz IN s_umskz
    AND gjahr IN r_gjahr
    AND ( budat IN s_budat AND
          budat LE p_datum )
    AND augdt GT p_datum
*    AND ( budat IN s_budat and
*          budat gt p_datum )
    AND belnr IN s_belnr
    AND waers IN s_waers.

  IF NOT gt_bsik[] IS INITIAL.
    gt_bsik_bk[] = gt_bsik[].
    REFRESH r_mr8m.
    r_mr8m-sign   = 'I'.
    r_mr8m-option = 'EQ'.
    LOOP AT gt_bsik_bk INTO gw_bsik_bk.
      IF gw_bsik_bk-sgtxt = 'MR8M'.
        r_mr8m-low = gw_bsik_bk-belnr.
        APPEND r_mr8m.
        CLEAR r_mr8m-low.
        IF NOT gw_bsik_bk-rebzg IS INITIAL.
          r_mr8m-low = gw_bsik_bk-rebzg.
          APPEND r_mr8m.
          CLEAR r_mr8m-low.
        ELSE.
          READ TABLE gt_bsik INTO gw_bsik WITH KEY bukrs = gw_bsik_bk-bukrs
                                                   rebzg = gw_bsik_bk-belnr
                                                   gjahr = gw_bsik_bk-gjahr
                                                   buzei = gw_bsik_bk-buzei
                                                   sgtxt = 'MR8M'.
          IF sy-subrc = 0.
            r_mr8m-low = gw_bsik_bk-belnr.
            APPEND r_mr8m.
            CLEAR r_mr8m.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  REFRESH gt_bsik_bk.

* delete document from MR8M
  IF NOT r_mr8m[] IS INITIAL.
    DELETE gt_bsik WHERE belnr IN r_mr8m.
  ENDIF.

  IF NOT gt_bsik[] IS INITIAL.
* Start of modify IMS#300000167 to add sign for amount
* get partial doc from BSIK
*    SELECT bukrs gjahr belnr buzei
*           rebzg rebzj dmbtr wrbtr
    SELECT  bukrs
            lifnr
            gjahr
            belnr
            buzei
            rebzg
            rebzj
            shkzg
            dmbtr
            wrbtr
* End of modify IMS#300000167 to add sign for amount
    INTO TABLE gt_bsik2
    FROM bsik
    FOR ALL ENTRIES IN gt_bsik
    WHERE bukrs = gt_bsik-bukrs
      AND rebzg = gt_bsik-belnr
      AND rebzj = gt_bsik-gjahr
      AND ( budat IN s_budat AND
            budat LE p_datum )
      AND umskz   IN s_umskz.
* get partial doc from BSAK
* Start of modify IMS#300000167 to add sign for amount
*    SELECT bukrs gjahr belnr buzei
*           rebzg rebzj dmbtr wrbtr
    SELECT  bukrs
            lifnr
            gjahr
            belnr
            buzei
            rebzg
            rebzj
            shkzg
            dmbtr
            wrbtr
* End of modify IMS#300000167 to add sign for amount
    APPENDING TABLE gt_bsik2
    FROM bsak
    FOR ALL ENTRIES IN gt_bsik
    WHERE bukrs = gt_bsik-bukrs
      AND rebzg = gt_bsik-belnr
      AND rebzj = gt_bsik-gjahr
    AND ( budat IN s_budat AND
          budat LE p_datum )
    AND augdt GT p_datum
    AND umskz   IN s_umskz.

    SELECT a~lifnr bukrs name1 akont ktokk
           b~KVERM "CH4
    INTO TABLE gt_vendor
    FROM lfa1 AS a INNER JOIN lfb1 AS b
    ON  a~lifnr = b~lifnr
    AND b~bukrs = p_bukrs
    FOR ALL ENTRIES IN gt_bsik
    WHERE a~lifnr = gt_bsik-lifnr.

* Begin of change IMS 300000167
*    SELECT bukrs belnr gjahr buzei hwbas hwste
*    INTO TABLE gt_bset
    SELECT bukrs belnr gjahr hwbas hwste
     INTO TABLE gt_bset_tab
* Eng of change  IMS 300000167
     FROM bset
     FOR ALL ENTRIES IN gt_bsik
     WHERE bukrs = gt_bsik-bukrs
       AND belnr = gt_bsik-belnr
       AND gjahr = gt_bsik-gjahr .
* Begin of change IMS 300000167
*      AND buzei = gt_bsik-buzei.

    LOOP AT gt_bset_tab INTO gw_bset .
      COLLECT gw_bset INTO gt_bset.
    ENDLOOP.
* End of change IMS 300000167

    "Add by Wantanee 20200914
*           belnr   TYPE with_item-belnr,
*       gjahr   TYPE with_item-gjahr,
*       buzei   TYPE with_item-gjahr,
*       witht   TYPE with_item-gjahr,
*       wt_withcd TYPE with_item-wt_withcd,
     SELECT belnr gjahr buzei witht wt_withcd WT_QSSHH
       INTO TABLE gt_with_item
       FROM with_item
       FOR ALL ENTRIES IN gt_bsik
       WHERE bukrs = gt_bsik-bukrs
       AND belnr = gt_bsik-belnr
       AND   gjahr = gt_bsik-gjahr.


    "End Add by Wantanee 20200914
  ENDIF.
    "Add by Wantanee 20200914
    SELECT witht wt_withcd text40
    INTO TABLE gt_t059zt
    FROM t059zt
    WHERE SPRAS EQ 'EN'
    AND LAND1 EQ 'TH'
    AND WITHT EQ '11'.
    "End Add by Wantanee 20200914




ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_year .
  CLEAR : gv_gjahr,
          gv_periv.

  SELECT SINGLE periv
  INTO gv_periv
  FROM t001
  WHERE bukrs = p_bukrs.

*  IF sy-subrc = 0.
*    CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
*      EXPORTING
*        i_date         = p_datum
*        i_periv        = gv_periv
*      IMPORTING
*        e_gjahr        = gv_gjahr
*      EXCEPTIONS
*        input_false    = 1
*        t009_notfound  = 2
*        t009b_notfound = 3
*        OTHERS         = 4.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*    gv_gjahr = p_gjahr.

*  ENDIF.
  REFRESH r_gjahr.
  CLEAR r_gjahr.

*-->> Begin Remark Parinya25Jan2012
*-> Search key : 20120131
*  IF p_gjahr IS NOT INITIAL.
*    r_gjahr-sign = 'I'.
*    r_gjahr-option = 'EQ'.
*    r_gjahr-low = p_gjahr.
*    APPEND r_gjahr.
*    CLEAR r_gjahr.
*  ENDIF.
*-->> End Remark Parinya25Jan2012

ENDFORM.                    " GET_YEAR
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPROT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_reprot .
  PERFORM build_layout.
  PERFORM build_catalog.
  PERFORM build_event USING gt_events.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = gt_layout
      it_fieldcat        = gt_fieldcat
      it_events          = gt_events
      i_save             = 'A'
    TABLES
      t_outtab           = gt_output
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " DISPLAY_REPROT
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_layout .
  gt_layout-window_titlebar = sy-title.
  gt_layout-colwidth_optimize = 'X'.
ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_catalog .
  DATA : lv_text1(30) TYPE c,
         lv_text2(30) TYPE c,
         lv_text3(30) TYPE c,
         lv_text4(30) TYPE c,
         lv_text5(30) TYPE c,
         lv_d1(3)     TYPE n,
         lv_d2(3)     TYPE n,
         lv_d3(3)     TYPE n,
         lv_d4(3)     TYPE n,
         lv_temp(3)   TYPE n.

  CLEAR : v_pos,
          lv_text1,
          lv_text2,
          lv_text3,
          lv_text4,
          lv_text5.

* Concatenate vendor code and vendor name
  PERFORM append_fieldcat USING 'BREIF'
                                 space
                                 space
                                 text-t25
                                 space  space  space
                                 gt_fieldcat[].

* Vendor
  PERFORM append_fieldcat USING 'LIFNR'
                                'LFA1'
                                'LIFNR'
                                 text-t01
                                 space  space  space
                                 gt_fieldcat[].
* Vendor Name
  PERFORM append_fieldcat USING 'NAME1'
                                'LFA1'
                                'NAME1'
                                 text-t02
                                 space  space  space
                                 gt_fieldcat[].
* Document Number
  PERFORM append_fieldcat USING 'BELNR'
                                'BSIK'
                                'BELNR'
                                 text-t04
                                 space  space  space
                                 gt_fieldcat[].
* Document Type
  PERFORM append_fieldcat USING 'BLART'
                                'BSIK'
                                'BLART'
                                 text-t03
                                 space  space  space
                                 gt_fieldcat[].
* Invoice Number
  PERFORM append_fieldcat USING 'XBLNR'
                                'BSIK'
                                'XBLNR'
                                 text-t05
                                 space  space  space
                                 gt_fieldcat[].

* Document header text
  PERFORM append_fieldcat USING 'SGTXT'
                                'BSIK'
                                'SGTXT'
                                 text-t26
                                 space  space  space
                                 gt_fieldcat[].

* Payment Term
  PERFORM append_fieldcat USING 'ZBD1T'
                                'BSIK'
                                'ZBD1T'
                                 text-t06
                                 space  space  space
                                 gt_fieldcat[].

* Posting Date
  PERFORM append_fieldcat USING 'PSTDT'
                                 space
                                 space
                                 text-t00
                                 space  space  space
                                 gt_fieldcat[].

* Invoice Date
  PERFORM append_fieldcat USING 'INVDT'
                                 space
                                 space
                                 text-t07
                                 space  space  space
                                 gt_fieldcat[].
  IF r_netdt EQ gc_true.
* Due Date
    PERFORM append_fieldcat USING 'NETDT'
                                   space
                                   space
                                   text-t08
                                   space  space  space
                                   gt_fieldcat[].
* Due Days
    PERFORM append_fieldcat USING 'NETDY'
                                   space
                                   space
                                   text-t10
                                   space  space  space
                                   gt_fieldcat[].
  ELSE.
* Invoice Days
    PERFORM append_fieldcat USING 'INVDY'
                                   space
                                   space
                                   text-t09
                                   space  space  space
                                   gt_fieldcat[].
  ENDIF.

* Amount in Document Currency
  PERFORM append_fieldcat USING 'WRBTR'
                                'BSIK'
                                'WRBTR'
                                 text-t11
                                 space
                                 'WAERS'
                                 space
                                 gt_fieldcat[].
* Document Currency
  PERFORM append_fieldcat USING 'WAERS'
                                'BSIK'
                                'WAERS'
                                 text-t12
                                 space  space  space
                                 gt_fieldcat[].
* Exchange Rate
  PERFORM append_fieldcat USING 'KURSF'
                                'BKPF'
                                'KURSF'
                                 text-t18
                                 space  space  space
                                 gt_fieldcat[].
* Amount in Local Currency
  PERFORM append_fieldcat USING 'DMBTR'
                                'BSIK'
                                'DMBTR'
                                 text-t13
                                 space
                                 'HWAERS'
                                 space
                                 gt_fieldcat[].
* Local Currency
  PERFORM append_fieldcat USING 'HWAERS'
                                'BSIK'
                                'WAERS'
                                 text-t24
                                 space  space  space
                                 gt_fieldcat[].
* Original Amount
  PERFORM append_fieldcat USING 'HWBAS'
                                'BSET'
                                'HWBAS'
                                 text-t14
                                 space
                                 'HWAERS'
                                 space
                                 gt_fieldcat[].
* VAT
  PERFORM append_fieldcat USING 'HWSTE'
                                'BSET'
                                'HWSTE'
                                 text-t15
                                 space
                                 'HWAERS'
                                 space
                                 gt_fieldcat[].
* Remain Amount
  PERFORM append_fieldcat USING 'REMAIN_AMT'
                                'BSIK'
                                'DMBTR'
                                 text-t16
                                 'X'
                                 'HWAERS'
                                 space
                                 gt_fieldcat[].
*************************************************************************************
* 1 Aging
  IF p_d1 EQ 0.
    MOVE text-t23 TO lv_text1.
  ELSE.
    WRITE : p_d1 TO lv_d1 NO-ZERO.
    CONCATENATE text-t19 text-t22 lv_d1 text-t17 INTO lv_text1 SEPARATED BY space.
  ENDIF.
  PERFORM append_fieldcat USING 'AMT_1'
                                'BSIK'
                                'DMBTR'
                                 lv_text1
                                'X'
                                 'HWAERS'
                                 space
                                 gt_fieldcat[].
*************************************************************************************
* 2 Aging
  IF NOT p_d2 IS INITIAL.
    lv_temp = p_d1 + 1.
    WRITE : p_d2    TO lv_d2 NO-ZERO,
            lv_temp TO lv_d1 NO-ZERO.
    CONCATENATE text-t19 lv_d1 text-t20 lv_d2 text-t17 INTO lv_text2 SEPARATED BY space.
    PERFORM append_fieldcat USING 'AMT_2'
                                  'BSIK'
                                  'DMBTR'
                                   lv_text2
                                  'X'
                                   'HWAERS'
                                   space
                                   gt_fieldcat[].
  ELSE.
    lv_temp = p_d1 + 1.
    WRITE lv_temp TO lv_d1 NO-ZERO.
    CONCATENATE text-t19 text-t21 lv_d1 text-t17 INTO lv_text2 SEPARATED BY space.
    PERFORM append_fieldcat USING 'AMT_2'
                                  'BSIK'
                                  'DMBTR'
                                   lv_text2
                                  'X'
                                   'HWAERS'
                                   space
                                   gt_fieldcat[].
  ENDIF.
*************************************************************************************
* 3 Aging
  IF NOT p_d3 IS INITIAL.
    lv_temp = p_d2 + 1.
    WRITE : p_d3    TO lv_d3 NO-ZERO,
            lv_temp TO lv_d2 NO-ZERO.
    CONCATENATE text-t19 lv_d2 text-t20 lv_d3 text-t17 INTO lv_text3 SEPARATED BY space.
    PERFORM append_fieldcat USING 'AMT_3'
                                  'BSIK'
                                  'DMBTR'
                                   lv_text3
                                  'X'
                                   'HWAERS'
                                   space
                                   gt_fieldcat[].
  ELSE.
    lv_temp = p_d2 + 1.
    WRITE lv_temp TO lv_d2 NO-ZERO.
    CONCATENATE text-t19 text-t21 lv_d2 text-t17 INTO lv_text3 SEPARATED BY space.
    PERFORM append_fieldcat USING 'AMT_3'
                                  'BSIK'
                                  'DMBTR'
                                   lv_text3
                                  'X'
                                   'HWAERS'
                                   space
                                   gt_fieldcat[].
  ENDIF.
*************************************************************************************
* 4 Aging
  IF p_d4 IS INITIAL.
    IF NOT p_d3 IS INITIAL.
      lv_temp = p_d3 + 1.
      WRITE lv_temp TO lv_d3 NO-ZERO.
      CONCATENATE text-t19 text-t21 lv_d3 text-t17 INTO lv_text4 SEPARATED BY space.
      PERFORM append_fieldcat USING 'AMT_4'
                                  'BSIK'
                                  'DMBTR'
                                   lv_text4
                                  'X'
                                   'HWAERS'
                                   space
                                   gt_fieldcat[].
    ENDIF.
  ELSE.
    lv_temp = p_d3 + 1.
    WRITE : p_d4    TO lv_d4 NO-ZERO,
            lv_temp TO lv_d3 NO-ZERO.
    CONCATENATE text-t19 lv_d3 text-t20 lv_d4 text-t17 INTO lv_text4 SEPARATED BY space.
    PERFORM append_fieldcat USING 'AMT_4'
                                  'BSIK'
                                  'DMBTR'
                                   lv_text4
                                  'X'
                                   'HWAERS'
                                   space
                                   gt_fieldcat[].

*************************************************************************************
* 5 Aging
    lv_temp = p_d4 + 1.
    WRITE lv_temp TO lv_d4 NO-ZERO.
    CONCATENATE text-t19 text-t21 lv_d4 text-t17 INTO lv_text5 SEPARATED BY space.
    PERFORM append_fieldcat USING 'AMT_5'
                                  'BSIK'
                                  'DMBTR'
                                   lv_text5
                                  'X'
                                   'HWAERS'
                                   space
                                   gt_fieldcat[].
  ENDIF.
*************************************************************************************
*************************************************************************************
"Add by Wantanee 20131207
* IO number
  PERFORM append_fieldcat USING 'AUFNR'
                                'AUFK'
                                'AUFNR'
                                 'IO Number'
                                 space  space  space
                                 gt_fieldcat[].
* IO Description
  PERFORM append_fieldcat USING 'KTEXT'
                                'AUFK'
                                'KTEXT'
                                 'IO Description'
                                 space  space  space
                                 gt_fieldcat[].

"End Add by Wantanee 20131207
"Add by Wantanee 20190123
* User Name
  PERFORM append_fieldcat USING 'USNAM'
                                'BKPF'
                                'USNAM'
                                 'User Name'
                                 space  space  space
                                 gt_fieldcat[].
"End Add by Wantanee 20190123


* GL Account
  PERFORM append_fieldcat USING 'HKONT'
                                'BSEG'
                                'HKONT'
                                 'GL Account'
                                 space  space  space
                                 gt_fieldcat[].
* GL Account Desc
  PERFORM append_fieldcat USING 'GL_TEXT'
                                ''
                                ''
                                 'GL Account Desc'
                                 space  space  space
                                 gt_fieldcat[].
* Amount
  PERFORM append_fieldcat USING 'DMBTR_I'
                                'BSEG'
                                'DMBTR'
                                 'Amount'
                                 space  space  space
                                 gt_fieldcat[].
* Item
  PERFORM append_fieldcat USING 'BUZEI'
                                'BSEG'
                                'BUZEI'
                                 'Item'
                                 space  space  space
                                 gt_fieldcat[].
* Profit Center
  PERFORM append_fieldcat USING 'PRCTR'
                                'BSEG'
                                'PRCTR'
                                 'Profit Center'
                                 space  space  space
                                 gt_fieldcat[].

* PH
  PERFORM append_fieldcat USING 'PH'
                                ''
                                ''
                                'PH'
                                 space  space  space
                                 gt_fieldcat[].

* WHT Code
    PERFORM append_fieldcat USING 'WT_WITHCD'
                                ''
                                ''
                                'WHT Code'
                                 space  space  space
                                 gt_fieldcat[].
* WHT text
    PERFORM append_fieldcat USING 'WT_TEXT40'
                                ''
                                ''
                                'WHT Desc.'
                                 space  space  space
                                 gt_fieldcat[].
"CH3 Add by WAntanee
* WHT BASE
    PERFORM append_fieldcat USING 'WT_QSSHH'
                                ''
                                ''
                                'WHT Base'
                                 space  space  space
                                 gt_fieldcat[].

* PO no.
    PERFORM append_fieldcat USING 'EBELN'
                                ''
                                ''
                                'PO No.'
                                 space  space  space
                                 gt_fieldcat[].

* PO Item
    PERFORM append_fieldcat USING 'EBELP'
                                ''
                                ''
                                'PO Item.'
                                 space  space  space
                                 gt_fieldcat[].


"CH3 End Add by WAntanee
"CH4 Add by WAntanee
* PO Item
    PERFORM append_fieldcat USING 'KVERM'
                                ''
                                ''
                                'VAT TYPE'
                                 space  space  space
                                 gt_fieldcat[].
"CH4 End Add by WAntanee
"CH4 Add by WAntanee
* Tax Code
    PERFORM append_fieldcat USING 'MWSKZ'
                                ''
                                ''
                                'Tax Code'
                                 space  space  space
                                 gt_fieldcat[].
"CH4 End Add by WAntanee

"CH5 Add by WAntanee 20210426

* Profit AP
    PERFORM append_fieldcat USING 'PRCTR_AP'
                                ''
                                ''
                                'Profit center (list)'
                                 space  space  space
                                 gt_fieldcat[].
* hkont AP
    PERFORM append_fieldcat USING 'HKONT_AP'
                                ''
                                ''
                                'GL (list)'
                                 space  space  space
                                 gt_fieldcat[].
"CH5 End Add by WAntanee 20210426
ENDFORM.                    " BUILD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0630   text
*      -->P_0631   text
*      -->P_0632   text
*      -->P_TEXT_T13  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM append_fieldcat USING   p_field   "Field name
                             p_reftable"Reference Table name
                             p_reffield"Reference Field name
                             p_coltxt  "Col Text(for specify)
                             p_dosum   "Sum total
                             p_cfieldname  "  currency
                             p_no_zero     " no zero
                             p_it_fieldcat TYPE slis_t_fieldcat_alv.

  DATA: wa_infieldcat TYPE slis_fieldcat_alv,
        v_coltxt_length TYPE i.

  ADD 1 TO v_pos.

  wa_infieldcat-fieldname     = p_field.
  wa_infieldcat-ref_tabname   = p_reftable.
  wa_infieldcat-ref_fieldname = p_reffield.
  wa_infieldcat-col_pos       = v_pos .
  wa_infieldcat-do_sum        = p_dosum.

  IF NOT p_no_zero IS INITIAL .
    wa_infieldcat-no_zero = p_no_zero.
  ENDIF.
  IF NOT p_cfieldname IS INITIAL .
    wa_infieldcat-cfieldname = p_cfieldname .
  ENDIF.

*If we need to specify text ,don't need to derive from data dictionary
*program will check length and define width of the colum
  IF NOT p_coltxt IS INITIAL.
    v_coltxt_length = STRLEN( p_coltxt ).

    IF v_coltxt_length > 20.
      wa_infieldcat-ddictxt = 'L'."Long text
      wa_infieldcat-seltext_l = p_coltxt.
    ELSEIF v_coltxt_length > 10.
      wa_infieldcat-ddictxt = 'M'."Medium Text
      wa_infieldcat-seltext_m = p_coltxt.
    ELSE.
      wa_infieldcat-ddictxt = 'S'."Short Text
      wa_infieldcat-seltext_s = p_coltxt.
    ENDIF.
    wa_infieldcat-reptext_ddic = p_coltxt  .
  ENDIF.
  APPEND wa_infieldcat TO p_it_fieldcat.

ENDFORM.                    " APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  GET_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_range .
  IF NOT p_normal IS INITIAL.
    r_bstat-sign   = 'I'.
    r_bstat-option = 'NE'.
    r_bstat-low    = 'S'.
    APPEND r_bstat.
    r_bstat-low    = 'V'.
    APPEND r_bstat.
    r_bstat-low    = 'Z'.
    APPEND r_bstat.

* for special GL
    s_umskz-sign   =  'I'.
    s_umskz-option = 'EQ'.
    APPEND s_umskz.
    CLEAR  s_umskz.
  ENDIF.
ENDFORM.                    " GET_RANGE
*&---------------------------------------------------------------------*
*&      Form  MAP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM map_data .
  DATA : lv_zfbdt  TYPE dzfbdt,
         lv_zbd1t  TYPE dzbd1t,
         lv_remain TYPE dmbtr,
         lv_kursf  TYPE kursf,
         lv_netdy  TYPE p,
         lv_invdy  TYPE p,
         lv_earliest TYPE c,
         lv_remain_doc TYPE wrbtr.

  DATA : lv_vcod  TYPE lifnr,
         lv_vnam  TYPE name1.

  DATA: lv_usnam TYPE bkpf-usnam.

  CLEAR : lv_kursf,
          lv_netdy,
          lv_invdy.

  LOOP AT gt_bsik INTO gw_bsik.
    CLEAR: lv_usnam.
    READ TABLE gt_vendor INTO gw_vendor WITH KEY lifnr = gw_bsik-lifnr.
    IF sy-subrc = 0.
       gw_output-KVERM = gw_vendor-KVERM.  "CH4
* filter for reconcile account
      IF gw_vendor-akont IN s_akont.
        gw_output-name1 = gw_vendor-name1.
        CONCATENATE gw_vendor-lifnr '-' gw_vendor-name1 INTO gw_output-breif.
      ELSE.
        CONTINUE.
      ENDIF.
* filter for vendor group
      IF gw_vendor-ktokk IN s_ktokk.

      ELSE.
        CONTINUE.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.
* get due date
    CALL FUNCTION 'MRM_PAYMENT_TERMS_GET'
      EXPORTING
        if_zterm = gw_bsik-zterm
        if_bldat = gw_bsik-bldat
        if_budat = gw_bsik-budat
        if_zfbdt = gw_bsik-zfbdt
      IMPORTING
        ef_zfbdt = lv_zfbdt
        ef_zbd1t = lv_zbd1t.

    IF sy-subrc = 0.
      gw_output-netdt = lv_zfbdt + lv_zbd1t.
*       gw_output-netdy = p_datum - gw_output-netdt.

      CALL FUNCTION '/SDF/CMO_DATETIME_DIFFERENCE'
        EXPORTING
          date1            = p_datum
          date2            = gw_output-netdt
        IMPORTING
          datediff         = lv_netdy
          earliest         = lv_earliest
        EXCEPTIONS
          invalid_datetime = 1
          OTHERS           = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        IF lv_earliest = '1'.
*          gw_output-netdy = gw_output-netdy * -1.
          lv_netdy = lv_netdy * -1.
        ENDIF.
      ENDIF.


    ENDIF.

*    gw_output-invdy = p_datum - gw_bsik-bldat.
    CALL FUNCTION '/SDF/CMO_DATETIME_DIFFERENCE'
      EXPORTING
        date1            = p_datum
        date2            = gw_bsik-bldat
      IMPORTING
*        datediff         = gw_output-invdy
        datediff         = lv_invdy
        earliest         = lv_earliest
      EXCEPTIONS
        invalid_datetime = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      IF lv_earliest = '1'.
*        gw_output-invdy = gw_output-invdy * -1.
        lv_invdy = lv_invdy * -1.
      ENDIF.
    ENDIF.
* check for Dr/Cr Indicator
    IF gw_bsik-shkzg EQ 'S'.
      gw_bsik-wrbtr = gw_bsik-wrbtr * -1.
      gw_bsik-dmbtr = gw_bsik-dmbtr * -1.
    ENDIF.
    IF gw_bsik-lifnr(2) EQ 'OT'.
      SELECT name1
        INTO lv_vnam
        FROM bsec
       WHERE belnr EQ gw_bsik-belnr
         AND gjahr EQ gw_bsik-gjahr.
        IF sy-subrc EQ 0.
          CONCATENATE gw_bsik-lifnr '-' lv_vnam INTO gw_output-breif.
          gw_output-name1 = lv_vnam.
        ENDIF.
      ENDSELECT.
    ENDIF.


    MOVE : gw_bsik-lifnr TO gw_output-lifnr,
           gw_bsik-belnr TO gw_output-belnr,
           gw_bsik-buzei TO gw_output-buzei,
           gw_bsik-blart TO gw_output-blart,
           gw_bsik-xblnr TO gw_output-xblnr,
           gw_bsik-sgtxt TO gw_output-sgtxt,
           gw_bsik-budat TO gw_output-pstdt,
           gw_bsik-bldat TO gw_output-invdt,
           gw_bsik-wrbtr TO gw_output-wrbtr,
           gw_bsik-waers TO gw_output-waers,
           gw_bsik-dmbtr TO gw_output-dmbtr,
           gw_bsik-zbd1t TO gw_output-zbd1t,
           gv_hwaers     TO gw_output-hwaers,
           gw_bsik-mwskz TO gw_output-mwskz.  "CH4 Add by Wantanee 20210226

    "Add by Wantanee 20131207.

    IF gw_bsik-umskz EQ 'T'.
       PERFORM get_io USING gw_bsik-sgtxt CHANGING gw_output-aufnr gw_output-ktext.
    ELSE.
          gw_output-aufnr = ''.
          gw_output-ktext = ''.
    ENDIF.
    "End Add by Wantanee 20131207


    IF NOT r_netdt IS INITIAL.
      IF NOT s_netdt[] IS INITIAL.
        CHECK gw_output-netdt IN s_netdt.
      ENDIF.
    ELSE.
      IF NOT s_invdt[] IS INITIAL.
        CHECK gw_output-invdt IN s_invdt.
      ENDIF.
    ENDIF.

    IF gw_output-wrbtr NE 0.
      lv_kursf = gw_output-dmbtr / gw_output-wrbtr.
      MOVE lv_kursf TO gw_output-kursf.
*      gw_output-kursf = gw_output-dmbtr / gw_output-wrbtr.
    ENDIF.
* Begin of change by IMS 300000167
*    READ TABLE gt_bset INTO gw_bset WITH KEY bukrs = gw_bsik-bukrs
*                                             belnr = gw_bsik-belnr
*                                             gjahr = gw_bsik-gjahr .
*                                             buzei = gw_bsik-buzei.
*    IF sy-subrc = 0.
** 300000167: Begin of insertion
*      IF gw_bsik-shkzg EQ 'S'.
*        gw_output-hwbas = gw_bset-hwbas * -1.
*        gw_output-hwste = gw_bset-hwste * -1.
*      ELSE.
** 300000167: End of insertion
*        gw_output-hwbas = gw_bset-hwbas.
*        gw_output-hwste = gw_bset-hwste.
** 300000167: Begin of insertion
*      ENDIF.
*    ELSE.
** 300000167: Begin of comment -> gw_bsik-dmbtr is already check dr/ cr indicator
**      IF gw_bsik-shkzg EQ 'S'.
**        gw_output-hwbas = gw_bsik-dmbtr * -1.
**      ELSE.
*      gw_output-hwbas = gw_bsik-dmbtr.
**      ENDIF.
** 300000167: End of comment
*      gw_output-hwste = 0.
** 300000167: End of insertion
*    ENDIF.
    LOOP AT gt_bset INTO gw_bset WHERE  bukrs = gw_bsik-bukrs
                                  AND   belnr = gw_bsik-belnr
                                  AND   gjahr = gw_bsik-gjahr .
* read data from BSET only once per document
      IF gw_bset-flag_read IS INITIAL.
        IF gw_bsik-shkzg EQ 'S'.
          gw_output-hwbas = gw_bset-hwbas * -1.
          gw_output-hwste = gw_bset-hwste * -1.
        ELSE.
          gw_output-hwbas = gw_bset-hwbas.
          gw_output-hwste = gw_bset-hwste.
        ENDIF.
        gw_bset-flag_read = 'X' .
        MODIFY gt_bset FROM gw_bset TRANSPORTING flag_read .
      ELSE.
        CLEAR : gw_output-hwbas , gw_output-hwste.
      ENDIF.
    ENDLOOP.
    IF sy-subrc NE 0 .  " not found from BSET
      gw_output-hwbas = gw_bsik-dmbtr.
      gw_output-hwste = 0.
    ENDIF.
* End of change by IMS 300000167

* get remain amount for Partial
    CLEAR : lv_remain,
            lv_remain_doc.
    LOOP AT gt_bsik2 INTO gw_bsik2 WHERE bukrs = gw_bsik-bukrs
                                     AND rebzg = gw_bsik-belnr
                                     AND rebzj = gw_bsik-gjahr.
* Start of modify IMS#300000167 to add sign for amount.
      IF gw_bsik2-shkzg EQ 'H'.
        gw_bsik2-dmbtr = gw_bsik2-dmbtr * -1.
        gw_bsik2-wrbtr = gw_bsik2-wrbtr * -1.
      ENDIF.
* End of modify IMS#300000167 to add sign for amount.
      lv_remain     = lv_remain + gw_bsik2-dmbtr.
      lv_remain_doc = lv_remain_doc + gw_bsik2-wrbtr.
      gw_bsik2-flag = 'X'.
      MODIFY gt_bsik2 FROM gw_bsik2.
      CLEAR gw_bsik2.
    ENDLOOP.
    gw_output-remain_amt     = gw_output-dmbtr - lv_remain.
    gw_output-remain_amt_doc = gw_output-wrbtr - lv_remain_doc.
* Raking Aging Amount
    IF r_netdt EQ gc_true.        "due date
      IF gv_flag EQ gc_true.      "document currency
        PERFORM raking_amount USING    gw_output-remain_amt_doc
                                       lv_netdy
                              CHANGING gw_output-amt_1
                                       gw_output-amt_2
                                       gw_output-amt_3
                                       gw_output-amt_4
                                       gw_output-amt_5.
      ELSE.                       "local currency
        PERFORM raking_amount USING    gw_output-remain_amt
                                       lv_netdy
                              CHANGING gw_output-amt_1
                                       gw_output-amt_2
                                       gw_output-amt_3
                                       gw_output-amt_4
                                       gw_output-amt_5.

      ENDIF.
    ELSE.                           "invoice date
      IF gv_flag EQ gc_true.        "document currency
        PERFORM raking_amount USING    gw_output-remain_amt_doc
                                       lv_invdy
                              CHANGING gw_output-amt_1
                                       gw_output-amt_2
                                       gw_output-amt_3
                                       gw_output-amt_4
                                       gw_output-amt_5.
      ELSE.                         "local currency
        PERFORM raking_amount USING    gw_output-remain_amt
                                       lv_invdy
                              CHANGING gw_output-amt_1
                                       gw_output-amt_2
                                       gw_output-amt_3
                                       gw_output-amt_4
                                       gw_output-amt_5.
      ENDIF.

    ENDIF.
    gw_output-bukrs = gw_bsik-bukrs.
    gw_output-gjahr = gw_bsik-gjahr.

    MOVE : lv_invdy TO gw_output-invdy,
           lv_netdy TO gw_output-netdy.

    "Add by Wantanee 20190123
           SELECT SINGLE usnam
           INTO lv_usnam
           FROM bkpf
           WHERE bukrs EQ p_bukrs
             AND belnr EQ gw_output-belnr
             AND gjahr EQ gw_output-gjahr.
             gw_output-usnam = lv_usnam.
    "End Add by wantanee 20190123

    "Add by Wantanee 20200914

     READ TABLE gt_with_item INTO gs_with_item WITH KEY belnr = gw_output-belnr
                                                        gjahr = gw_output-gjahr.

         IF sy-subrc EQ 0.
              gw_output-wt_withcd = gs_with_item-wt_withcd.

              READ TABLE gt_t059zt INTO gs_t059zt WITH KEY wt_withcd = gw_output-wt_withcd.
                   IF sy-subrc EQ 0.
                       gw_output-wt_text40 = gs_t059zt-text40.
                   ENDIF.

              gw_output-WT_QSSHH = gs_with_item-WT_QSSHH.

         ENDIF.

    "End Add by Wantanee 20200914

    COLLECT gw_output INTO gt_output.
*    APPEND gw_output TO gt_output.
    CLEAR gw_output.
    CLEAR : lv_kursf,
            lv_zbd1t,
            lv_netdy,
            lv_invdy.
  ENDLOOP.

*  DELETE gt_output WHERE blart IN r_partial.

  CLEAR gw_bsik2.
  LOOP AT gt_bsik2 INTO gw_bsik2 WHERE flag = 'X'.
    READ TABLE gt_output INTO gw_output
                         WITH KEY bukrs = gw_bsik2-bukrs
                                  belnr = gw_bsik2-belnr
                                  gjahr = gw_bsik2-gjahr
                                  lifnr = gw_bsik2-lifnr.
    IF sy-subrc = 0.
      DELETE gt_output INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  DELETE gt_output WHERE remain_amt = 0.

ENDFORM.                    " MAP_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_FLAG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_flag .
  DATA : lv_line TYPE n.
  CLEAR gv_flag.

  DELETE ADJACENT DUPLICATES FROM s_waers
                  COMPARING  low.

* one currency == document currency == gv_flag = 'X'
  DESCRIBE TABLE s_waers LINES lv_line.
  IF lv_line EQ 1.
    gv_flag = gc_true.
  ENDIF.

ENDFORM.                    " CHECK_FLAG
*&---------------------------------------------------------------------*
*&      Form  RAKING_AMOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GW_BSIK_DMBTR  text
*      -->P_GW_NETDY       text
*      <--P_GW_OUTPUT_AMT_1  text
*      <--P_GW_OUTPUT_AMT_2  text
*      <--P_GW_OUTPUT_AMT_3  text
*      <--P_GW_OUTPUT_AMT_4  text
*----------------------------------------------------------------------*
FORM raking_amount  USING    p_amount
                             p_day
                    CHANGING p_amt_1
                             p_amt_2
                             p_amt_3
                             p_amt_4
                             p_amt_5.
  IF p_day IN r_date1.
    p_amt_1 = p_amount.
  ELSEIF p_day IN r_date2.
    p_amt_2 = p_amount.
  ELSEIF p_day IN r_date3.
    p_amt_3 = p_amount.
  ELSEIF p_day IN r_date4.
    p_amt_4 = p_amount.
  ELSEIF p_day IN r_date5.
    p_amt_5 = p_amount.
  ENDIF.

ENDFORM.                    " RAKING_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  CHECK_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_day .
  IF NOT p_d1 IS INITIAL.
    r_date1-sign   = 'I'.
    r_date1-option = 'LE'.
    r_date1-low    = p_d1.
    APPEND r_date1.
  ELSE.
    r_date1-sign   = 'I'.
    r_date1-option = 'LE'.
    r_date1-low    = 0.
    APPEND r_date1.
  ENDIF.

  IF NOT p_d2 IS INITIAL .
    r_date2-sign   = 'I'.
    r_date2-option = 'BT'.
    r_date2-low    = p_d1 + 1.
    r_date2-high   = p_d2.
    APPEND r_date2.
  ELSE.
    r_date2-sign   = 'I'.
    r_date2-option = 'GT'.
    r_date2-low    = p_d2.
    APPEND r_date2.
  ENDIF.

  IF NOT p_d3 IS INITIAL .
    r_date3-sign   = 'I'.
    r_date3-option = 'BT'.
    r_date3-low    = p_d2 + 1.
    r_date3-high   = p_d3.
    APPEND r_date3.
  ELSE.
    r_date3-sign   = 'I'.
    r_date3-option = 'GT'.
    r_date3-low    = p_d2.
    APPEND r_date3.
  ENDIF.

  IF NOT p_d4 IS INITIAL .
    r_date4-sign   = 'I'.
    r_date4-option = 'BT'.
    r_date4-low    = p_d3 + 1.
    r_date4-high   = p_d4.
    APPEND r_date4.
  ELSE.
    r_date4-sign   = 'I'.
    r_date4-option = 'GT'.
    r_date4-low    = p_d3.
    APPEND r_date4.
  ENDIF.

  IF NOT p_d4 IS INITIAL .
    r_date5-sign   = 'I'.
    r_date5-option = 'GT'.
    r_date5-low    = p_d4.
    APPEND r_date5.
  ENDIF.

ENDFORM.                    " CHECK_DAY
*&---------------------------------------------------------------------*
*&      Form  GET_CURRENCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_currency .

  CLEAR gv_hwaers.

  SELECT SINGLE waers
  INTO gv_hwaers
  FROM t001
  WHERE bukrs = p_bukrs.

ENDFORM.                    " GET_CURRENCY
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*----------------------------------------------------------------------*
FORM build_event  USING  p_gtyp_event TYPE slis_t_event.
  FIELD-SYMBOLS <fs_events> LIKE LINE OF p_gtyp_event.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    IMPORTING
      et_events       = p_gtyp_event
    EXCEPTIONS
      list_type_wrong = 0
      OTHERS          = 0.

  LOOP AT p_gtyp_event ASSIGNING <fs_events>
                    WHERE name = 'TOP_OF_PAGE'.
    <fs_events>-form = 'REPORT_HEADER'.
  ENDLOOP.

ENDFORM.                    " BUILD_EVENT

*&---------------------------------------------------------------------*
*&      Form  REPORT_HEADER
*&---------------------------------------------------------------------*

FORM report_header.
  DATA : lt_listhead TYPE slis_t_listheader,
         lw_listline TYPE slis_listheader.

  DATA : lv_text(256) TYPE c,
         lv_date(10)  TYPE c,
         lv_time(8)   TYPE c,
         lv_sign(7)   TYPE c,
         lv_option(12) TYPE c,
         lv_low(10)   TYPE c,
         lv_high(20)  TYPE c ,
         lv_count     TYPE i.

  DEFINE def_list_head.
    clear lw_listline.
    lw_listline-typ  = &1.
    lw_listline-key  = &2.
    lw_listline-info = &3.
    append lw_listline to lt_listhead.
  END-OF-DEFINITION.
* Company Name
  CLEAR lv_text.
  break attahornl.
  CONCATENATE text-h02 text-h01 INTO lv_text SEPARATED BY space.
  def_list_head 'H' '' lv_text .

* Report Title
  CLEAR lv_text.
  IF r_netdt EQ gc_true.
    CONCATENATE text-h03 text-h04 INTO lv_text SEPARATED BY space .
  ELSE.
    CONCATENATE text-h03 text-h05 INTO lv_text SEPARATED BY space .
  ENDIF.
  def_list_head 'S' '' lv_text  .

* Currency
  CLEAR lv_text.
  lv_text = text-h06.
  LOOP AT s_waers.
    CONCATENATE lv_text s_waers-low INTO lv_text SEPARATED BY space.
  ENDLOOP.
  def_list_head 'S' '' lv_text  .

* Date & Time
  CLEAR : lv_text,
          lv_date,
          lv_time.

  WRITE sy-datum TO lv_date USING EDIT MASK gc_mask_date.
  WRITE sy-uzeit TO lv_time USING EDIT MASK gc_mask_time.
  CONCATENATE text-h07 lv_date text-h08 lv_time INTO lv_text SEPARATED BY space.
  def_list_head 'S' '' lv_text  .

* As of Date
  CLEAR lv_text.
  CLEAR lv_date.
  WRITE p_datum TO lv_date USING EDIT MASK gc_mask_date.
  CONCATENATE text-h09 lv_date INTO lv_text SEPARATED BY space.
  def_list_head 'S' '' lv_text  .

* Fiscal Year
  CLEAR lv_text.

*-->> Begin Remark Parinya25Jan2012
*-> Search key : 20120131
*  CONCATENATE text-h10 p_gjahr INTO lv_text SEPARATED BY space.
*  def_list_head 'S' '' lv_text .
*-->> End Remark Parinya25Jan2012

* Account group
  lv_count = 0.
  LOOP AT s_ktokk.
    CLEAR: lv_text,
           lv_sign,
           lv_option,
           lv_low,
           lv_high.
    lv_count = lv_count + 1.
    IF s_ktokk-sign EQ 'I'.
      lv_sign = 'Include'.
    ELSEIF s_ktokk-sign EQ  'E' .
      lv_sign = 'Exclude'.
    ENDIF.
    lv_option = s_ktokk-option.
    lv_low  = s_ktokk-low.
    IF s_ktokk-high IS NOT INITIAL.
      CONCATENATE 'TO' s_ktokk-high INTO lv_high SEPARATED BY space.
    ENDIF.
    IF lv_count > 1.
      CONCATENATE '_____________' lv_sign lv_option  lv_low lv_high INTO lv_text SEPARATED BY space.
    ELSE.
      CONCATENATE text-h11 lv_sign lv_option  lv_low lv_high INTO lv_text SEPARATED BY space.
    ENDIF.
    def_list_head 'S' '' lv_text.
  ENDLOOP.
* Reconciliation acct
  lv_count = 0.
  LOOP AT s_akont.
    CLEAR: lv_text,
           lv_sign,
           lv_option,
           lv_low,
           lv_high.
    lv_count = lv_count + 1.
    IF s_akont-sign EQ 'I'.
      lv_sign = 'Include'.
    ELSEIF s_akont-sign EQ  'E' .
      lv_sign = 'Exclude'.
    ENDIF.
    lv_option = s_akont-option.
    lv_low  = s_akont-low.
    IF s_akont-high IS NOT INITIAL.
      CONCATENATE 'TO' s_akont-high INTO lv_high SEPARATED BY space.
    ENDIF.
    IF lv_count > 1.
      CONCATENATE '_____________' lv_sign lv_option  lv_low lv_high INTO lv_text SEPARATED BY space.
    ELSE.
      CONCATENATE text-h12 lv_sign lv_option  lv_low lv_high INTO lv_text SEPARATED BY space.
    ENDIF.
    def_list_head 'S' '' lv_text.
  ENDLOOP.
* Vendor
  lv_count = 0.
  LOOP AT s_lifnr.
    CLEAR: lv_text,
           lv_sign,
           lv_option,
           lv_low,
           lv_high.
    lv_count = lv_count + 1.
    IF s_lifnr-sign EQ 'I'.
      lv_sign = 'Include'.
    ELSEIF s_lifnr-sign EQ  'E' .
      lv_sign = 'Exclude'.
    ENDIF.
    lv_option = s_lifnr-option.
    lv_low  = s_lifnr-low.
    IF s_lifnr-high IS NOT INITIAL.
      CONCATENATE 'TO' s_lifnr-high INTO lv_high SEPARATED BY space.
    ENDIF.
    IF lv_count > 1.
      CONCATENATE '_______' lv_sign lv_option  lv_low lv_high INTO lv_text SEPARATED BY space.
    ELSE.
      CONCATENATE text-h13 lv_sign lv_option  lv_low lv_high INTO lv_text SEPARATED BY space.
    ENDIF.
    def_list_head 'S' '' lv_text.
  ENDLOOP.
* Document Number
  lv_count = 0.
  LOOP AT s_belnr.
    CLEAR: lv_text,
           lv_sign,
           lv_option,
           lv_low,
           lv_high.
    lv_count = lv_count + 1.
    IF s_belnr-sign EQ 'I'.
      lv_sign = 'Include'.
    ELSEIF s_belnr-sign EQ  'E' .
      lv_sign = 'Exclude'.
    ENDIF.
    lv_option = s_belnr-option.
    lv_low  = s_belnr-low.
    IF s_belnr-high IS NOT INITIAL.
      CONCATENATE 'TO' s_belnr-high INTO lv_high SEPARATED BY space.
    ENDIF.
    IF lv_count > 1.
      CONCATENATE '____________' lv_sign lv_option  lv_low lv_high INTO lv_text SEPARATED BY space.
    ELSE.
      CONCATENATE text-h14 lv_sign lv_option  lv_low lv_high INTO lv_text SEPARATED BY space.
    ENDIF.
    def_list_head 'S' '' lv_text.
  ENDLOOP.

* Normal Item
  CLEAR lv_text.
  IF p_normal IS NOT INITIAL.
    lv_text = 'Normal Item : Include'.
  ELSE.
    lv_text = 'Normal Item : Exclude'.
  ENDIF.
  def_list_head 'S' '' lv_text.

* Special G/L Indicator
  IF s_umskz IS NOT INITIAL.
    lv_count = 0.
    LOOP AT s_umskz.
      CLEAR: lv_text,
             lv_sign,
             lv_option,
             lv_low,
             lv_high.
      lv_count = lv_count + 1.
      IF s_umskz-sign EQ 'I'.
        lv_sign = 'Include'.
      ELSEIF s_umskz-sign EQ  'E' .
        lv_sign = 'Exclude'.
      ENDIF.
      lv_option = s_umskz-option.
      lv_low  = s_umskz-low.
      IF s_umskz-high IS NOT INITIAL.
        CONCATENATE 'TO' s_umskz-high INTO lv_high SEPARATED BY space.
      ENDIF.
      IF lv_count > 1.
        CONCATENATE '_______________________' lv_sign lv_option  lv_low lv_high INTO lv_text SEPARATED BY space.
      ELSE.
        CONCATENATE text-h15 lv_sign lv_option  lv_low lv_high INTO lv_text SEPARATED BY space.
      ENDIF.
      def_list_head 'S' '' lv_text.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_listhead
    EXCEPTIONS
      OTHERS             = 0.

ENDFORM.                    " REPORT_HEADER
*&---------------------------------------------------------------------*
*&      Form  GET_GENC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_genc .

  DATA : lw_genc TYPE ZSDSCAC001,
         lt_genc TYPE STANDARD TABLE OF ZSDSCAC001.

  REFRESH : lt_genc.
  CLEAR lw_genc.
  SELECT *
  INTO TABLE lt_genc
  FROM ZSDSCAC001
  WHERE repid = gc_repid.

  LOOP AT lt_genc INTO lw_genc.
    IF lw_genc-PARAM CP gc_partial.
      CLEAR r_partial.
      r_partial-sign   = 'I'.
      r_partial-option = 'EQ'.
      r_partial-low    = lw_genc-VALUE_LOW.
      APPEND r_partial.
      CLEAR r_partial.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_GENC
*&---------------------------------------------------------------------*
*&      Form  GET_IO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_io USING p_sgtxt
            CHANGING p_aufnr TYPE aufk-aufnr
                     p_ktext TYPE aufk-ktext.

  DATA : lv_aufnr(12) TYPE c,
         lv_ktext TYPE aufk-ktext,
        lv_POS TYPE i,
        lv_OFF TYPE i.
  DATA: result_tab TYPE match_result_tab.
  DATA: result_wa TYPE match_result.

  CLEAR : p_aufnr,p_ktext,lv_aufnr,lv_POS,lv_OFF.
  CONDENSE p_sgtxt NO-GAPS..


  FIND ALL OCCURRENCES OF REGEX '((COSTING)|(COSTING#))'  IN p_sgtxt
     RESULTS result_tab.

  LOOP AT result_tab INTO result_wa.

         lv_POS = result_wa-offset + result_wa-length.
         IF lv_pos NE 0.
           lv_pos = lv_pos.
           lv_aufnr = p_sgtxt+lv_pos(6).
           p_aufnr = lv_aufnr.
         ENDIF.
  ENDLOOP.

  CLEAR: lv_pos,result_tab,result_wa.
  IF p_aufnr EQ ''.

        FIND ALL OCCURRENCES OF REGEX 'IO' IN p_sgtxt
           RESULTS result_tab.

        LOOP AT result_tab INTO result_wa.

               lv_POS = result_wa-offset + result_wa-length.
               IF lv_pos NE 0.
                 lv_pos = lv_pos.
                 lv_aufnr = p_sgtxt+lv_pos(8).
                 CONCATENATE '0000' lv_aufnr INTO lv_aufnr.
                 SELECT SINGLE ktext
                 INTO (lv_ktext)
                 FROM aufk
                 WHERE aufnr EQ lv_aufnr.
                   IF lv_ktext NE ''.
                       p_aufnr = lv_aufnr.
                       p_ktext = lv_ktext.
                   ENDIF.

               ENDIF.
        ENDLOOP.

 ENDIF.



ENDFORM.                    " GET_GENC

*&---------------------------------------------------------------------*
*&      Form  GET_ADDITIONAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_additional.
  DATA: lv_idx TYPE sy-tabix.
  DATA: lv_iloan TYPE AFIH-iloan,
        lv_abckz TYPE iloa-abckz,
        lv_abctx TYPE t370c_t-abctx.
  DATA: lv_ktext TYPE aufk-ktext.

  "CH5 Add by Wantanee 20210426
  DATA: lv_hkont(150) TYPE c,
        lv_prctr(150) TYPE c.
  "CH5 End Add by Wantanee 20210426


     IF gt_output IS NOT INITIAL.

        SELECT  saknr txt50
        INTO TABLE gt_skat
        FROM skat
        WHERE ktopl EQ 'RCOA'.



        SELECT  belnr gjahr buzei dmbtr shkzg
                aufnr  vbel2 posn2 hkont lifnr prctr sgtxt
                ebeln  ebelp mwskz
          INTO TABLE gt_bseg
          FROM bseg
          FOR ALL ENTRIES IN gt_output
          WHERE belnr EQ gt_output-belnr
          AND   gjahr EQ gt_output-gjahr.

         SELECT vbel2 posn2
          INTO TABLE gt_bseg_sale
          FROM bseg
          FOR ALL ENTRIES IN gt_output
          WHERE belnr EQ gt_output-belnr
          AND   gjahr EQ gt_output-gjahr
          AND  vbel2 NE ''.

          IF gt_bseg IS NOT INITIAL.

              SELECT rbeln rposn paph1 PAPH3 gjahr kstar rkaufnr
              INTO TABLE gt_ce11000
              FROM ce11000
              FOR ALL ENTRIES IN gt_bseg
              WHERE rbeln = gt_bseg-belnr
                AND gjahr = gt_bseg-gjahr
                AND ( rbeln LIKE  '21%' or rbeln LIKE '22%' or rbeln LIKE '23%' or rbeln LIKE  '24%' or rbeln LIKE '25%' or rbeln LIKE '26%' )
                AND paledger = '02'.

              IF gt_bseg_sale IS NOT INITIAL.
                  SELECT vbeln posnr prodh
                   INTO TABLE gt_vbap
                   FROM vbap
                   FOR ALL ENTRIES IN gt_bseg_sale
                   WHERE vbeln = gt_bseg_sale-vbel2
                     AND posnr = gt_bseg_sale-posn2.
              ENDIF.
*                        hkont TYPE bseg-hkont,   "GL Account
*          dmbtr_i TYPE bseg-dmbtr,   "Amount
*          shkzg TYPE bseg-shkzg,
*          prctr TYPE bseg-prctr,
*          gl_text TYPE  skat-txt50,
*          buzei TYPE bseg-buzei,
* aufnr TYPE aufk-aufnr,  "Add by Wantanee 20131207
*          ktext TYPE aufk-ktext,  "Add by Wantanee 20131207
*                   ebeln    TYPE ekkn-ebeln,
*       ebelp    TYPE ekkn-ebelp,
*       sakto    TYPE ekkn-sakto,
           SELECT ebeln ebelp sakto
           INTO TABLE gt_ekkn
           FROM ekkn
           FOR ALL ENTRIES IN gt_bseg
           WHERE ebeln EQ gt_bseg-ebeln
             AND ebelp EQ gt_bseg-ebelp.




              gt_output_temp = gt_output.


              LOOP AT gt_output_temp INTO gw_output.
                      CLEAR: gs_ekkn1.
                      CLEAR: gt_prctr_temp,gs_prctr_temp,gt_hkont_temp,gs_hkont_temp. "CH5 Add by Wantanee 20210426

                      lv_idx = sy-tabix.

                      "CH5 Add by Wantanee 20210426
                      gs_prctr_temp-belnr = gw_output-belnr.
                      gs_prctr_temp-gjahr = gw_output-gjahr.
                      gs_hkont_temp-belnr = gw_output-belnr.
                      gs_hkont_temp-gjahr = gw_output-gjahr.
                      "CH5 End Add by Wantanee 20210426

                      READ TABLE gt_bseg  INTO gs_bseg WITH KEY  belnr = gw_output-belnr
                                                               gjahr = gw_output-gjahr
                                                               buzei = gw_output-buzei .
                            IF sy-subrc EQ 0.

                                 gw_output-ebeln = gs_bseg-ebeln. "CH3 Add by Wantanee
                                 gw_output-ebelp = gs_bseg-ebelp. "CH3 Add by Wantanee
                                 READ TABLE gt_ekkn INTO gs_ekkn1 WITH KEY ebeln = gs_bseg-ebeln
                                                                          ebelp = gs_bseg-ebelp.
                                     IF sy-subrc EQ 0.
                                         gw_output-hkont = gs_ekkn1-sakto.
                                     ELSE.
                                         gw_output-hkont = gs_bseg-hkont.
                                     ENDIF.

                                 READ TABLE gt_skat INTO gs_skat WITH KEY saknr = gw_output-hkont.
                                      IF sy-subrc EQ 0.
                                          gw_output-gl_text = gs_skat-txt50.
                                      ENDIF.

                                 gw_output-mwskz = gs_bseg-mwskz. "CH4 Add by Wantanee
                                 MODIFY gt_output INDEX lv_idx FROM gw_output
                                 TRANSPORTING hkont gl_text ebeln ebelp.
                            ENDIF.
                      LOOP AT gt_bseg  INTO gs_bseg WHERE belnr EQ gw_output-belnr
                                                      AND gjahr EQ gw_output-gjahr
                                                      AND buzei NE gw_output-buzei.

                                 CLEAR: lv_iloan,lv_abckz,lv_abctx,lv_ktext.
                                 CLEAR: gw_output-dmbtr, gw_output-hwbas, gw_output-hwste, gw_output-remain_amt.
                                 CLEAR: gw_output-remain_amt_doc, gw_output-amt_1, gw_output-amt_2, gw_output-amt_3.
                                 CLEAR: gw_output-amt_4, gw_output-amt_5,gw_output-hkont,gw_output-dmbtr_i,gw_output-WRBTR.
                                 CLEAR: gw_output-shkzg, gw_output-prctr,gw_output-gl_text,gw_output-aufnr,gw_output-ktext,gw_output-ph.
                                 CLEAR: gs_ekkn.
                                 CLEAR: gw_output-mwskz."CH4 Add by Wantanee

                                gw_output-buzei = gs_bseg-buzei.

                                gw_output-ebeln = gs_bseg-ebeln. "CH3 Add by Wantanee
                                gw_output-ebelp = gs_bseg-ebelp. "CH3 Add by Wantanee
                                gw_output-mwskz = gs_bseg-mwskz. "CH4 Add by Wantanee

                                READ TABLE gt_ekkn INTO gs_ekkn WITH KEY ebeln = gs_bseg-ebeln
                                                                          ebelp = gs_bseg-ebelp.
                                     IF sy-subrc EQ 0.
                                         gw_output-hkont = gs_ekkn-sakto.
                                     ELSE.
                                         gw_output-hkont = gs_bseg-hkont.
                                     ENDIF.

                                IF gs_bseg-shkzg EQ 'S'.
                                   gw_output-dmbtr_i = gs_bseg-dmbtr.
                                ELSE.
                                  gw_output-dmbtr_i = gs_bseg-dmbtr * -1.
                                ENDIF.
                                IF gs_bseg-aufnr IS NOT INITIAL.
                                     gw_output-aufnr = gs_bseg-aufnr.
                                     SELECT SINGLE ktext
                                       INTO lv_ktext
                                       FROM aufk
                                       WHERE aufnr = gw_output-aufnr.

                                     gw_output-ktext = lv_ktext.
                                ENDIF.
                                gw_output-sgtxt = gs_bseg-sgtxt.
                                gw_output-prctr = gs_bseg-prctr.



                                 READ TABLE gt_skat INTO gs_skat WITH KEY saknr = gw_output-hkont.
                                      IF sy-subrc EQ 0.
                                          gw_output-gl_text = gs_skat-txt50.
                                      ENDIF.

                                   READ TABLE gt_ce11000 INTO gs_ce11000 WITH KEY rbeln = gs_bseg-belnr
                                                                                  rposn = gs_bseg-buzei
                                                                                  gjahr = gs_bseg-gjahr.
                                    IF sy-subrc = 0.
                                      CONCATENATE gs_ce11000-paph1 gs_ce11000-wwphz INTO gw_output-ph SEPARATED BY space.
*                                  BOI: hard code for case SVO of MA which cost at contract ## janejira.
                                    ELSEIF gs_bseg-aufnr(5) EQ '00821'.
                                        SELECT SINGLE  iloan
                                           INTO  lv_iloan
                                           FROM AFIH
                                           WHERE aufnr = gs_bseg-aufnr.
                                        IF lv_iloan IS NOT INITIAL.
                                           SELECT SINGLE abckz
                                             INTO lv_abckz
                                             FROM iloa
                                             WHERE iloan = lv_iloan.
                                             IF lv_abckz IS NOT INITIAL.
                                                SELECT SINGLE abctx
                                                 INTO lv_abctx
                                                 FROM t370c_t
                                                WHERE abckz EQ lv_abckz
                                                  AND SPRAS = 'E'.
                                                  IF lv_abctx IS NOT INITIAL.
                                                     CONCATENATE lv_abctx+0(2) lv_abctx+10(8) INTO gw_output-ph SEPARATED BY space.
                                                  ENDIF.
                                             ENDIF.
                                        ENDIF.

                                     ELSEIF gs_bseg-aufnr(5) EQ '00810' or gs_bseg-aufnr(5) EQ '00812' or gs_bseg-aufnr(5) EQ '00813' or gs_bseg-aufnr(5) EQ '00816'.
                                       gw_output-ph = 'SV OUT'.
                                     ELSEIF gs_bseg-aufnr(5) EQ '00811' or gs_bseg-aufnr(5) EQ '00814' or gs_bseg-aufnr(5) EQ '00815' or gs_bseg-aufnr(5) EQ '00817' or gs_bseg-aufnr(5) EQ '00819'..
                                       gw_output-ph = 'SV IN'.
                                     ELSEIF gs_bseg-aufnr(5) EQ '00818'.
                                       gw_output-ph = 'MA MA'.
*                                  EOI: end of hard code
                                     ELSE.
                                       READ TABLE gt_vbap INTO gs_vbap WITH KEY vbeln = gs_bseg-vbel2
                                                                                posnr = gs_bseg-posn2.
                                       IF sy-subrc = 0.
                                         CONCATENATE gs_vbap-prodh+0(5)  gs_vbap-prodh+10(8)  INTO gw_output-ph SEPARATED BY space.
                                       endif.
                                    ENDIF.

                                    APPEND gw_output TO gt_output.

                                     "CH5 Add by Wantanee 20210426
                                     gs_prctr_temp-prctr = gw_output-prctr.
                                     APPEND gs_prctr_temp TO gt_prctr_temp.
                                     gs_hkont_temp-hkont = gw_output-hkont.
                                     APPEND gs_hkont_temp TO gt_hkont_temp.
                                     "CH5 End Add by Wantanee 20210426



                      ENDLOOP.

                      CLEAR: lv_hkont,lv_prctr.
                      SORT gt_prctr_temp.
                      DELETE ADJACENT DUPLICATES FROM gt_prctr_temp.
                      SORT gt_hkont_temp.
                      DELETE ADJACENT DUPLICATES FROM gt_hkont_temp.

                      LOOP AT gt_prctr_temp INTO gs_prctr_temp WHERE belnr EQ gw_output-belnr
                                                                 AND gjahr EQ gw_output-gjahr.

                             IF lv_prctr IS INITIAL.
                                lv_prctr = gs_prctr_temp-prctr.
                             ELSE.
                                CONCATENATE lv_prctr gs_prctr_temp-prctr INTO lv_prctr SEPARATED BY ','.
                             ENDIF.
                      ENDLOOP.

                      IF lv_prctr IS NOT INITIAL.
                          gw_output-prctr_ap = lv_prctr.
                      ENDIF.

                      LOOP AT gt_hkont_temp INTO gs_hkont_temp WHERE belnr EQ gw_output-belnr
                                                                 AND gjahr EQ gw_output-gjahr.

                              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                                   EXPORTING
                                     INPUT         = gs_hkont_temp-hkont
                                  IMPORTING
                                    OUTPUT        = gs_hkont_temp-hkont .

                             IF lv_hkont IS INITIAL.

                                lv_hkont = gs_hkont_temp-hkont.
                             ELSE.
                                CONCATENATE lv_hkont gs_hkont_temp-hkont INTO lv_hkont SEPARATED BY ','.
                             ENDIF.
                      ENDLOOP.

                      IF lv_hkont IS NOT INITIAL.
                          gw_output-hkont_ap = lv_hkont.
                      ENDIF.

                      MODIFY gt_output INDEX lv_idx FROM gw_output
                                 TRANSPORTING hkont_ap prctr_ap.



              ENDLOOP.


          ENDIF.

     ENDIF.



ENDFORM.                    " GET_DATA

*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0040
*  Creation Date      : 22.02.2023
*  Author             : B. Chiewsarikij
*  Add-on ID          : ZMMF002
*  Description        : Report front PO
*  Purpose            : N/A
*  Copied from        : ZR_MM_SV_FRONTPO_290113_V1
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Report ZSDSMMR0040
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT  zsdsmmr0040 MESSAGE-ID z_mm.
TYPE-POOLS : truxs,slis,icon.
*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : mseg,ekko,ekkn.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF gy_result,
          jobno  TYPE c LENGTH 12,
          aufnr  TYPE aufk-aufnr,
          name1  TYPE kna1-name1,
          prctr  TYPE prctr,
          subna  TYPE c LENGTH 255,
          mblnr  TYPE mseg-mblnr,
          budat  TYPE mkpf-budat,
          ebeln  TYPE ekko-ebeln,
          bedat  TYPE ekko-bedat,
          matnr  TYPE mara-matnr,
          sernr  TYPE equi-sernr,
          vkgrp  TYPE vbak-vkgrp,
          vkbur  TYPE vbak-vkbur,
          netwr  TYPE vbak-netwr,
          maktx  TYPE makt-maktx,
          menge  TYPE ekpo-menge,
          meins  TYPE ekpo-meins,
          namec  TYPE kna1-name1,
          matnr1 TYPE mara-matnr,
        END OF gy_result.

TYPES : BEGIN OF gy_mseg,
          mblnr TYPE mseg-mblnr,
          mjahr TYPE mseg-mjahr,
          zeile TYPE mseg-zeile,
          ebeln TYPE mseg-ebeln,
          ebelp TYPE mseg-ebelp,
          aufnr TYPE mseg-aufnr,
        END OF gy_mseg.

TYPES : BEGIN OF gy_aufk,
          aufnr TYPE aufk-aufnr,
        END OF gy_aufk.

TYPES : BEGIN OF gy_makt,
          matnr TYPE makt-matnr,
          maktx TYPE makt-maktx,
        END OF gy_makt.

TYPES : BEGIN OF gy_ekko,
          ebeln TYPE ekko-ebeln,
          bedat TYPE ekko-bedat,
          lifnr TYPE ekko-lifnr,
          name1 TYPE lfa1-name1,
        END OF gy_ekko.

TYPES : BEGIN OF gy_mkpf,
          mblnr TYPE mkpf-mblnr,
          budat TYPE mkpf-budat,
        END OF gy_mkpf.

TYPES : BEGIN OF gy_objk,
          obknr TYPE objk-obknr,
          matnr TYPE objk-matnr,
          sernr TYPE objk-sernr,
        END OF gy_objk.

TYPES : BEGIN OF gy_ser01,
          obknr   TYPE ser01-obknr,
          lief_nr TYPE ser01-lief_nr,
          posnr   TYPE ser01-posnr,
        END OF gy_ser01.

TYPES : BEGIN OF gy_lips,
          vbeln TYPE lips-vbeln,
          posnr TYPE lips-posnr,
          prctr TYPE lips-prctr,
          kunnr TYPE likp-kunnr,
          name1 TYPE kna1-name1,
        END OF gy_lips.

TYPES : BEGIN OF gy_likp,
          vbeln TYPE lips-vbeln,
          posnr TYPE lips-posnr,
          kunnr TYPE likp-kunnr,
          name1 TYPE kna1-name1,
        END OF gy_likp.

TYPES : BEGIN OF gy_vbfa,
          vbelv TYPE vbfa-vbelv,
          posnv TYPE vbfa-posnv,
          vbeln TYPE vbfa-vbeln,
          posnn TYPE vbfa-posnn,
        END OF gy_vbfa.

TYPES : BEGIN OF gy_vbap,
          vbeln TYPE vbak-vbeln,
          posnr TYPE vbap-posnr,
          netwr TYPE vbap-netwr,
          mwsbp TYPE vbap-mwsbp,
          vkgrp TYPE vbak-vkgrp,
          vkbur TYPE vbak-vkbur,
        END OF gy_vbap.

TYPES : BEGIN OF gy_ihpa,
          aufnr TYPE aufk-aufnr,
          adrnr TYPE ihpa-adrnr,
          name1 TYPE kna1-name1,
        END OF gy_ihpa.

TYPES : BEGIN OF gy_ekpo,
          ebeln TYPE ekpo-ebeln,
          ebelp TYPE ekpo-ebelp,
          txz01 TYPE ekpo-txz01,
          matnr TYPE ekpo-matnr,
          menge TYPE ekpo-menge,
          meins TYPE ekpo-meins,
          netwr TYPE ekpo-netwr,
        END OF gy_ekpo.

TYPES : BEGIN OF gy_ekkn,
          ebeln TYPE ekkn-ebeln,
          ebelp TYPE ekkn-ebelp,
          aufnr TYPE ekkn-aufnr,
          prctr TYPE ekkn-prctr,
        END OF gy_ekkn.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : gt_result TYPE TABLE OF gy_result,
       gs_result TYPE gy_result.

DATA : gt_mseg TYPE TABLE OF gy_mseg,
       gs_mseg TYPE gy_mseg.

DATA : gt_aufk TYPE TABLE OF gy_aufk,
       gs_aufk TYPE gy_aufk.

DATA : gt_makt TYPE TABLE OF gy_makt,
       gs_makt TYPE gy_makt.

DATA : gt_ekko TYPE TABLE OF gy_ekko,
       gs_ekko TYPE gy_ekko.

DATA : gt_mkpf TYPE TABLE OF gy_mkpf,
       gs_mkpf TYPE gy_mkpf.

DATA : gt_objk TYPE TABLE OF gy_objk,
       gs_objk TYPE gy_objk.

DATA : gt_ser01 TYPE TABLE OF gy_ser01,
       gs_ser01 TYPE gy_ser01.

DATA : gt_lips TYPE TABLE OF gy_lips,
       gs_lips TYPE gy_lips.

DATA : gt_likp TYPE TABLE OF gy_likp,
       gs_likp TYPE gy_likp.

DATA : gt_vbfa TYPE TABLE OF gy_vbfa,
       gs_vbfa TYPE gy_vbfa.

DATA : gt_vbap TYPE TABLE OF gy_vbap,
       gs_vbap TYPE gy_vbap.

DATA : gt_ihpa TYPE TABLE OF gy_ihpa,
       gs_ihpa TYPE gy_ihpa.

DATA : gt_ekpo TYPE TABLE OF gy_ekpo,
       gs_ekpo TYPE gy_ekpo.

DATA : gt_ekkn TYPE TABLE OF gy_ekkn,
       gs_ekkn TYPE gy_ekkn.

DATA : gt_fcat   TYPE slis_t_fieldcat_alv,
       gs_layout TYPE slis_layout_alv,
       gt_sort   TYPE slis_t_sortinfo_alv,
       gs_sort   TYPE slis_sortinfo_alv.

RANGES : gr_stat FOR jest-stat.
*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS : gc_true      TYPE c VALUE 'X'.

*-----------------------------------------------------------------------
* M A C R O S
*-----------------------------------------------------------------------
*DEFINE.
*END-OF-DEFINITION.
*
*-----------------------------------------------------------------------
* F I E L D – S Y M B O L S
*-----------------------------------------------------------------------
*FIELD-SYMBOLS:
*
*-----------------------------------------------------------------------
* F I E L D – G R O U P S
*-----------------------------------------------------------------------
*FIELD-GROUPS:
*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : s_ebeln FOR mseg-ebeln,
                   s_bedat FOR ekko-bedat MODIF ID po,
                   s_lifnr FOR ekko-lifnr MODIF ID po,
                   s_prctr FOR ekkn-prctr MODIF ID po,
                   s_ernam FOR ekko-ernam MODIF ID po,
                   s_aufnr FOR mseg-aufnr MODIF ID re.
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: r1 RADIOBUTTON GROUP log USER-COMMAND ucomm DEFAULT 'X',
              r4 RADIOBUTTON GROUP log,
              r2 RADIOBUTTON GROUP log,
              r3 RADIOBUTTON GROUP log.
SELECTION-SCREEN END OF BLOCK block2.
*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
*INITIALIZATION.
*
*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  PERFORM f_get_selection.
*AT SELECTION-SCREEN ON HELP-REQUEST FOR <field>
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR <field>
*AT SELECTION-SCREEN ON <field>
*AT SELECTION-SCREEN.
*
*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
  IF r1 EQ 'X' OR r4 EQ 'X'.
    PERFORM f_get_data.
  ELSE.
    PERFORM f_get_data_po.
  ENDIF.

*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
  IF gt_result[] IS NOT INITIAL.
    PERFORM f_show_report.
  ELSE.
    MESSAGE s000 DISPLAY LIKE 'E'.
  ENDIF.

*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
*TOP-OF-PAGE.
*
*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
*AT LINE-SELECTION.
*
*-----------------------------------------------------------------------
* F O R M S
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_get_data.
  IF s_ebeln IS NOT INITIAL.
    PERFORM f_get_matdoc.
    PERFORM f_get_aufnr_po.
    IF gt_aufk IS NOT INITIAL.
      PERFORM f_get_data_mat_doc_po.
    ENDIF.
  ELSE.
    PERFORM f_get_aufnr.
    IF gt_aufk IS NOT INITIAL.
      PERFORM f_get_data_mat_doc.
    ENDIF.
  ENDIF.

  IF gt_result[] IS NOT INITIAL.
    PERFORM f_get_mat_desc.
    PERFORM f_get_po_dat_sub_name.
    PERFORM f_get_mat_dat.
    PERFORM f_get_prctr.
    PERFORM f_get_so.
    PERFORM f_modif_result.
  ENDIF.

ENDFORM.                    "f_get_data
*&---------------------------------------------------------------------*
*&      Form  pf_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_pf_alv_grid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program          = sy-repid
      _callback_pf_status_set     = 'PF_STATUS_1'
      i_callback_user_command     = 'USER_COMMAND'
      i_callback_top_of_page      = ' '
      i_callback_html_top_of_page = ' '
      i_callback_html_end_of_list = ' '
      is_layout                   = gs_layout
      it_fieldcat                 = gt_fcat
      it_sort                     = gt_sort
      i_default                   = 'X'
      i_save                      = 'X'
    TABLES
      t_outtab                    = gt_result
    EXCEPTIONS
      program_error               = 1
      OTHERS                      = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "pf_alv_grid
*&---------------------------------------------------------------------*
*&      Form  set_layout_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PS_LAYOUT  text
*----------------------------------------------------------------------*
FORM f_set_layout_output." CHANGING ps_layout TYPE slis_layout_alv.
  gs_layout-box_fieldname     = 'SEL'.
  gs_layout-zebra             = 'X'.
  gs_layout-colwidth_optimize = 'X'.

ENDFORM.                    " set_layout_output
*&---------------------------------------------------------------------*
*&      Form  build_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_build_fcat.

  DATA:
   ls_fcat TYPE slis_fieldcat_alv.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'JOBNO'.
  ls_fcat-seltext_s   = 'Job No.'.
  ls_fcat-seltext_m   = 'Job No.'.
  ls_fcat-seltext_l   = 'Job No.'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'AUFNR'.
  ls_fcat-seltext_s   = 'Order No.'.
  ls_fcat-seltext_m   = 'Order No.'.
  ls_fcat-seltext_l   = 'Order No.'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'NAME1'.
  ls_fcat-seltext_s   = 'Customer Name'.
  ls_fcat-seltext_m   = 'Customer Name'.
  ls_fcat-seltext_l   = 'Customer Name'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'PRCTR'.
  ls_fcat-seltext_s   = 'Profit Center'.
  ls_fcat-seltext_m   = 'Profit Center'.
  ls_fcat-seltext_l   = 'Profit Center'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'SUBNA'.
  ls_fcat-seltext_s   = 'Sub. Name'.
  ls_fcat-seltext_m   = 'Sub. Name'.
  ls_fcat-seltext_l   = 'Sub. Name'.
  APPEND ls_fcat TO gt_fcat.

  IF r1 EQ 'X'.
    CLEAR ls_fcat.
    ls_fcat-fieldname   = 'MBLNR'.
    ls_fcat-seltext_s   = 'Mat. Doc.'.
    ls_fcat-seltext_m   = 'Mat. Doc.'.
    ls_fcat-seltext_l   = 'Mat. Doc.'.
    APPEND ls_fcat TO gt_fcat.

    CLEAR ls_fcat.
    ls_fcat-ref_tabname = 'MKPF'.
    ls_fcat-fieldname   = 'BUDAT'.
    APPEND ls_fcat TO gt_fcat.
  ENDIF.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'EBELN'.
  ls_fcat-seltext_s   = 'PO No.'.
  ls_fcat-seltext_m   = 'PO No.'.
  ls_fcat-seltext_l   = 'PO No.'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-ref_tabname = 'EKKO'.
  ls_fcat-fieldname   = 'BEDAT'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-ref_tabname = 'MARA'.
  ls_fcat-fieldname   = 'MATNR'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-ref_tabname = 'MAKT'.
  ls_fcat-fieldname   = 'MAKTX'.
  APPEND ls_fcat TO gt_fcat.

  IF r2 = 'X'.
    CLEAR ls_fcat.
    ls_fcat-fieldname   = 'MATNR1'.
    ls_fcat-seltext_s   = 'Material From SO'.
    ls_fcat-seltext_m   = 'Material From SO'.
    ls_fcat-seltext_l   = 'Material From SO'.
    APPEND ls_fcat TO gt_fcat.
  ENDIF.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'SERNR'.
  ls_fcat-seltext_s   = 'Serial'.
  ls_fcat-seltext_m   = 'Serial'.
  ls_fcat-seltext_l   = 'Serial'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-ref_tabname = 'VBAK'.
  ls_fcat-fieldname   = 'VKBUR'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-ref_tabname = 'VBAK'.
  ls_fcat-fieldname   = 'VKGRP'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-ref_tabname = 'VBAK'.
  ls_fcat-fieldname   = 'NETWR'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'NAMEC'.
  ls_fcat-seltext_s   = 'Customer In SO'.
  ls_fcat-seltext_m   = 'Customer In SO'.
  ls_fcat-seltext_l   = 'Customer In SO'.
  APPEND ls_fcat TO gt_fcat.

ENDFORM.                    "build_fcat_1
*&---------------------------------------------------------------------*
*&      Form  F_SHOW_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_show_report .
  PERFORM f_set_layout_output.
  PERFORM f_build_fcat.
  PERFORM f_sort.
  PERFORM f_pf_alv_grid.
ENDFORM.                    " F_SHOW_REPORT
*&---------------------------------------------------------------------*
*&      Form  F_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_sort .
  CLEAR gs_sort.
  gs_sort-fieldname = 'LIFNR'.
  gs_sort-spos = '1'.
  gs_sort-up = 'X'.
*  gs_sort-subtot = 'X'.
  APPEND gs_sort TO gt_sort.
ENDFORM.                    " F_SORT
*&---------------------------------------------------------------------*
*&      Form  F_GET_MATDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_matdoc .

  SELECT mblnr
         mjahr
         zeile
         ebeln
         ebelp
         aufnr
    FROM mseg
    INTO TABLE gt_mseg
    WHERE ebeln IN s_ebeln
      AND aufnr IN s_aufnr.

ENDFORM.                    " F_GET_MATDOC
*&---------------------------------------------------------------------*
*&      Form  F_GET_AUFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_aufnr .
  SELECT aufnr
    FROM aufk
    INTO TABLE gt_aufk
    WHERE aufnr IN s_aufnr.
ENDFORM.                    " F_GET_AUFNR
*&---------------------------------------------------------------------*
*&      Form  F_CALL_FM_PM_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_call_fm_pm_read USING lv_aufnr.


  DATA : order_number    TYPE aufk-aufnr,
         call_from_notif TYPE sy-datar.

  DATA : wcaufvd TYPE caufvd,
         wiloa   TYPE iloa,
         wriwo1  TYPE riwo1.

  DATA : iaffhd       TYPE TABLE OF affhd,
         iafvgd       TYPE TABLE OF afvgd,
         iresbd       TYPE TABLE OF resbd,
         iripw0       TYPE TABLE OF ripw0,
         op_print_tab TYPE TABLE OF riprt1,
         ihpad_tab    TYPE TABLE OF ihpad,
         ihsg_tab     TYPE TABLE OF ihsg,
         ihgns_tab    TYPE TABLE OF ihgns,
         kbedp_tab    TYPE TABLE OF kbedp.

  DATA : ls_iripw0 TYPE ripw0,
         ls_iresbd TYPE resbd.

  DATA : lv_price TYPE vbap-netwr.

  order_number    = lv_aufnr.
  call_from_notif = sy-datar.

  CALL FUNCTION 'CO_EXT_ORDER_RESET'.

  CALL FUNCTION 'PM_ORDER_DATA_READ'
    EXPORTING
      order_number    = order_number
      call_from_notif = call_from_notif
    IMPORTING
      wcaufvd         = wcaufvd
      wiloa           = wiloa
      wriwo1          = wriwo1
    TABLES
      iaffhd          = iaffhd
      iafvgd          = iafvgd
      iresbd          = iresbd
      iripw0          = iripw0
      op_print_tab    = op_print_tab
      ihpad_tab       = ihpad_tab
      ihsg_tab        = ihsg_tab
      ihgns_tab       = ihgns_tab
      kbedp_tab       = kbedp_tab
    EXCEPTIONS
      order_not_found = 1
      error_message   = 2
      itab_error      = 3
      OTHERS          = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  READ TABLE gt_mseg INTO gs_mseg
  WITH KEY aufnr = lv_aufnr.
  IF r1 EQ 'X'.
    LOOP AT iresbd INTO ls_iresbd.
      ADD ls_iresbd-gpreis_2 TO lv_price.
      lv_price = lv_price + ( ls_iresbd-gpreis_2 * ls_iresbd-bdmng ).
      gs_result-jobno = wcaufvd-lead_aufnr.
      gs_result-aufnr = lv_aufnr.
      gs_result-matnr = ls_iresbd-matnr.
      gs_result-prctr = wcaufvd-prctr.
      gs_result-ebeln = gs_mseg-ebeln.
      gs_result-netwr = ls_iresbd-gpreis_2.
      APPEND gs_result TO gt_result.
      CLEAR ls_iresbd.
    ENDLOOP.

    LOOP AT iripw0 INTO ls_iripw0.
      READ TABLE iripw0 INTO ls_iripw0 INDEX 1.
      IF sy-subrc = 0.
        gs_result-jobno = wcaufvd-lead_aufnr.
        gs_result-aufnr = lv_aufnr.
        gs_result-matnr = ls_iripw0-matnr.
        gs_result-sernr = ls_iripw0-sernr.
        gs_result-ebeln = gs_mseg-ebeln.
        gs_result-netwr = lv_price.
        APPEND gs_result TO gt_result.
        CLEAR ls_iripw0.
      ENDIF.
    ENDLOOP.
    SELECT SINGLE ihpa~adrnr
                  adrc~name1
    FROM ihpa
    INNER JOIN adrc ON ihpa~adrnr EQ adrc~addrnumber
    INTO CORRESPONDING FIELDS OF gs_ihpa
    WHERE objnr EQ wcaufvd-objnr
      AND parvw EQ 'AG'.

    gs_ihpa-aufnr = lv_aufnr.

    APPEND gs_ihpa TO gt_ihpa.
    CLEAR gs_ihpa.
  ELSEIF r4 EQ 'X'.

    LOOP AT iresbd INTO ls_iresbd.
      READ TABLE iripw0 INTO ls_iripw0 INDEX 1.
      IF sy-subrc = 0.
        gs_result-matnr1 = ls_iripw0-matnr.
        gs_result-sernr  = ls_iripw0-sernr.
      ENDIF.

      gs_result-jobno = wcaufvd-lead_aufnr.
      gs_result-aufnr = lv_aufnr.
      gs_result-matnr = ls_iresbd-matnr.
      gs_result-prctr = wcaufvd-prctr.
      gs_result-ebeln = gs_mseg-ebeln.
      gs_result-netwr = ( ls_iresbd-gpreis_2 * ls_iresbd-bdmng ).
      APPEND gs_result TO gt_result.
      CLEAR ls_iresbd.
    ENDLOOP.

    SELECT SINGLE ihpa~adrnr
                  adrc~name1
    FROM ihpa
    INNER JOIN adrc ON ihpa~adrnr EQ adrc~addrnumber
    INTO CORRESPONDING FIELDS OF gs_ihpa
    WHERE objnr EQ wcaufvd-objnr
      AND parvw EQ 'AG'.

    gs_ihpa-aufnr = lv_aufnr.

    APPEND gs_ihpa TO gt_ihpa.
    CLEAR gs_ihpa.

  ELSE.
    gs_result-jobno = wcaufvd-lead_aufnr.
    SELECT SINGLE ihpa~adrnr
                  adrc~name1
    FROM ihpa
    INNER JOIN adrc ON ihpa~adrnr EQ adrc~addrnumber
    INTO CORRESPONDING FIELDS OF gs_ihpa
    WHERE objnr EQ wcaufvd-objnr
      AND parvw EQ 'AG'.

    gs_result-name1 = gs_ihpa-name1.

    READ TABLE iripw0 INTO ls_iripw0 INDEX 1.
    IF sy-subrc = 0.
      gs_result-matnr  = ls_iripw0-matnr.
      gs_result-matnr1 = ls_iripw0-matnr.
      gs_result-sernr  = ls_iripw0-sernr.
      CLEAR ls_iripw0.
    ENDIF.

  ENDIF.

ENDFORM.                    " F_CALL_FM_PM_READ
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_MAT_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data_mat_doc .

  SELECT mblnr
         mjahr
         zeile
         ebeln
         ebelp
         aufnr
    FROM mseg
    INTO TABLE gt_mseg
    FOR ALL ENTRIES IN gt_aufk
    WHERE aufnr EQ gt_aufk-aufnr.

  LOOP AT gt_aufk INTO gs_aufk.
    PERFORM f_call_fm_pm_read USING gs_aufk-aufnr.
  ENDLOOP.

ENDFORM.                    " F_GET_DATA_MAT_DOC
*&---------------------------------------------------------------------*
*&      Form  F_GET_MAT_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_mat_desc .

  SELECT matnr
         maktx
    FROM makt
    INTO TABLE gt_makt
    FOR ALL ENTRIES IN gt_result
    WHERE matnr EQ gt_result-matnr.

ENDFORM.                    " F_GET_MAT_DESC
*&---------------------------------------------------------------------*
*&      Form  F_MOD_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_modif_result .
  DATA lv_tabix TYPE sy-tabix.

  LOOP AT gt_result INTO gs_result.
    lv_tabix = sy-tabix.

    PERFORM f_modif_matde USING lv_tabix.
    PERFORM f_modif_matdc USING lv_tabix.
    PERFORM f_modif_custo USING lv_tabix.
    PERFORM f_modif_po    USING lv_tabix.
    PERFORM f_modif_prctr USING lv_tabix.
    PERFORM f_modif_so    USING lv_tabix.

    CLEAR : gs_result,gs_lips,gs_ser01,gs_objk,gs_vbap,
            gs_vbfa,gs_ekko,gs_mseg,gs_mkpf,gs_makt,gs_ihpa.
  ENDLOOP.

ENDFORM.                    " F_MOD_RESULT
*&---------------------------------------------------------------------*
*&      Form  F_GET_PO_DAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_po_dat_sub_name .

  SELECT ekko~ebeln
         ekko~bedat
         ekko~lifnr
         lfa1~name1
    FROM ekko
    INNER JOIN lfa1 ON ekko~lifnr EQ lfa1~lifnr
    INTO TABLE gt_ekko
    FOR ALL ENTRIES IN gt_result
    WHERE ekko~ebeln EQ gt_result-ebeln.

ENDFORM.                    " F_GET_PO_DAT
*&---------------------------------------------------------------------*
*&      Form  F_GET_MAT_DAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_mat_dat .

  IF gt_mseg[] IS NOT INITIAL.
    SELECT mblnr
           budat
      FROM mkpf
      INTO TABLE gt_mkpf
      FOR ALL ENTRIES IN gt_mseg
      WHERE mblnr EQ gt_mseg-mblnr
        AND mjahr EQ gt_mseg-mjahr.
  ENDIF.

ENDFORM.                    " F_GET_MAT_DAT
*&---------------------------------------------------------------------*
*&      Form  F_GET_PRCTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_CHECK_PROFIT  text
*----------------------------------------------------------------------*
FORM f_get_prctr.

  SELECT obknr
         matnr
         sernr
    FROM objk
    INTO TABLE gt_objk
    FOR ALL ENTRIES IN gt_result
    WHERE matnr EQ gt_result-matnr
      AND sernr EQ gt_result-sernr
      AND taser EQ 'SER01'.

  IF gt_objk[] IS NOT INITIAL.
    SELECT obknr
           lief_nr
           posnr
      FROM ser01
      INTO TABLE gt_ser01
      FOR ALL ENTRIES IN gt_objk
      WHERE obknr EQ gt_objk-obknr.

    IF gt_ser01 IS NOT INITIAL.

      SELECT lips~vbeln
             lips~posnr
             lips~prctr
             likp~kunnr
             kna1~name1
        FROM lips
        INNER JOIN likp ON lips~vbeln EQ likp~vbeln
        INNER JOIN kna1 ON likp~kunnr EQ kna1~kunnr
        INTO TABLE gt_lips
        FOR ALL ENTRIES IN gt_ser01
        WHERE lips~vbeln EQ gt_ser01-lief_nr
          AND lips~posnr EQ gt_ser01-posnr
          AND lips~pstyv NE 'ELN'.

    ENDIF.
  ENDIF.

  SORT gt_lips BY vbeln DESCENDING posnr.

ENDFORM.                    " F_GET_PRCTR
*&---------------------------------------------------------------------*
*&      Form  F_GET_SO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_so.

  IF gt_lips[] IS NOT INITIAL.
    SELECT vbelv
           posnv
           vbeln
           posnn
      FROM vbfa
      INTO TABLE gt_vbfa
      FOR ALL ENTRIES IN gt_lips
      WHERE vbeln   EQ gt_lips-vbeln
        AND posnn   EQ gt_lips-posnr
        AND vbtyp_v EQ 'C'.

    IF gt_vbfa[] IS NOT INITIAL.
      SELECT vbap~vbeln
             vbap~posnr
             vbap~netwr
             vbap~mwsbp
             vbak~vkgrp
             vbak~vkbur
        FROM vbap
        INNER JOIN vbak ON vbap~vbeln EQ vbak~vbeln
        INTO TABLE gt_vbap
        FOR ALL ENTRIES IN gt_vbfa
        WHERE vbap~vbeln EQ gt_vbfa-vbelv
          AND vbap~posnv EQ gt_vbfa-posnv.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_GET_SO
*&---------------------------------------------------------------------*
*&      Form  F_MODIF_PRCTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_TABIX  text
*----------------------------------------------------------------------*
FORM f_modif_prctr  USING lv_tabix.
  LOOP AT gt_objk INTO gs_objk WHERE matnr = gs_result-matnr
                                 AND sernr = gs_result-sernr.
    READ TABLE gt_ser01 INTO gs_ser01
    WITH KEY obknr = gs_objk-obknr.
    IF sy-subrc = 0.
      READ TABLE gt_lips INTO gs_lips
      WITH KEY vbeln = gs_ser01-lief_nr
               posnr = gs_ser01-posnr.
      IF sy-subrc = 0.
        gs_result-prctr = gs_lips-prctr.
        gs_result-namec = gs_lips-name1.
        MODIFY gt_result FROM gs_result INDEX lv_tabix
                                 TRANSPORTING prctr namec .
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " F_MODIF_PRCTR
*&---------------------------------------------------------------------*
*&      Form  F_MODIF_SO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_TABIX  text
*----------------------------------------------------------------------*
FORM f_modif_so USING lv_tabix.

  READ TABLE gt_vbfa INTO gs_vbfa
  WITH KEY vbeln = gs_lips-vbeln
           posnn = gs_lips-posnr.
  IF sy-subrc = 0.
    READ TABLE gt_vbap INTO gs_vbap
    WITH KEY vbeln = gs_vbfa-vbelv
             posnr = gs_vbfa-posnv.
    IF sy-subrc = 0.
      gs_result-vkgrp = gs_vbap-vkgrp.
      gs_result-vkbur = gs_vbap-vkbur.
      gs_result-netwr = gs_vbap-mwsbp + gs_vbap-netwr.
      MODIFY gt_result FROM gs_result INDEX lv_tabix
                               TRANSPORTING vkgrp vkbur.
    ENDIF.

  ENDIF.

ENDFORM.                    " F_MODIF_SO
*&---------------------------------------------------------------------*
*&      Form  F_MODIF_MATDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_TABIX  text
*----------------------------------------------------------------------*
FORM f_modif_matdc USING lv_tabix.

  READ TABLE gt_mseg INTO gs_mseg
  WITH KEY aufnr = gs_result-aufnr.
  IF sy-subrc = 0.
    READ TABLE gt_mkpf INTO gs_mkpf
    WITH KEY mblnr = gs_mseg-mblnr.
    IF sy-subrc = 0.
      gs_result-mblnr = gs_mkpf-mblnr.
      gs_result-budat = gs_mkpf-budat.

      MODIFY gt_result FROM gs_result INDEX lv_tabix
                               TRANSPORTING mblnr budat.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_MODIF_MATDC
*&---------------------------------------------------------------------*
*&      Form  F_MODIF_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_TABIX  text
*----------------------------------------------------------------------*
FORM f_modif_po  USING lv_tabix.

  READ TABLE gt_ekko INTO gs_ekko
  WITH KEY ebeln = gs_result-ebeln.
  IF sy-subrc = 0.
    gs_result-subna = gs_ekko-name1.
    gs_result-ebeln = gs_ekko-ebeln.
    gs_result-bedat = gs_ekko-bedat.

    MODIFY gt_result FROM gs_result INDEX lv_tabix
                             TRANSPORTING subna ebeln bedat.

  ENDIF.

ENDFORM.                    " F_MODIF_PO
*&---------------------------------------------------------------------*
*&      Form  F_MODIF_MATDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_TABIX  text
*----------------------------------------------------------------------*
FORM f_modif_matde  USING lv_tabix.

  READ TABLE gt_makt INTO gs_makt
  WITH KEY matnr = gs_result-matnr.
  IF sy-subrc = 0.
    gs_result-maktx = gs_makt-maktx.
    MODIFY gt_result FROM gs_result INDEX lv_tabix
                             TRANSPORTING maktx .
  ENDIF.

ENDFORM.                    " F_MODIF_MATDE
*&---------------------------------------------------------------------*
*&      Form  F_MODIF_CUSTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_TABIX  text
*----------------------------------------------------------------------*
FORM f_modif_custo  USING lv_tabix.

  READ TABLE gt_ihpa INTO gs_ihpa
  WITH KEY aufnr = gs_result-aufnr.
  IF sy-subrc = 0.
    gs_result-name1 = gs_ihpa-name1.
    MODIFY gt_result FROM gs_result INDEX lv_tabix
                             TRANSPORTING name1 .
  ENDIF.

ENDFORM.                    " F_MODIF_CUSTO
*&---------------------------------------------------------------------*
*&      Form  F_GET_AUFNR_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_aufnr_po .
  IF gt_mseg IS NOT INITIAL.
    SELECT aufnr
      FROM aufk
      INTO TABLE gt_aufk
      FOR ALL ENTRIES IN gt_mseg
      WHERE aufnr EQ gt_mseg-aufnr.
  ENDIF.
ENDFORM.                    " F_GET_AUFNR_PO
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_MAT_DOC_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data_mat_doc_po .
  LOOP AT gt_aufk INTO gs_aufk.
    PERFORM f_call_fm_pm_read USING gs_aufk-aufnr.
  ENDLOOP.
ENDFORM.                    " F_GET_DATA_MAT_DOC_PO
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data_po.

  SELECT ekpo~ebeln
         ekpo~ebelp
         ekpo~txz01
         ekpo~matnr
         ekpo~menge
         ekpo~meins
         ekpo~netwr
    FROM ekko
    INNER JOIN ekpo ON ekko~ebeln EQ ekpo~ebeln
    INTO TABLE gt_ekpo
    WHERE ekpo~ebeln IN s_ebeln
      AND ekko~bedat IN s_bedat
      AND ekko~lifnr IN s_lifnr
      AND ekko~ernam IN s_ernam.

  IF gt_ekpo[] IS NOT INITIAL.
    PERFORM f_get_po_dat_sub_name_po.
    PERFORM f_get_detail_po.
    PERFORM f_get_aufk_report_po.

    PERFORM f_get_check_matdoc.
    IF     r2 EQ 'X'.
      PERFORM f_get_result.
    ELSEIF r3 EQ 'X'.
      PERFORM f_get_result_sum.
    ENDIF.

    IF gt_result[] IS NOT INITIAL.
      IF r3 EQ 'X'.
        PERFORM f_get_mat_desc.
      ENDIF.
      PERFORM f_get_cust_name.
      PERFORM f_get_so_po.
      PERFORM f_modif_result_po.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_GET_DATA_PO
*&---------------------------------------------------------------------*
*&      Form  F_GET_PO_DAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_po_dat_sub_name_po.

  SELECT ekko~ebeln
         ekko~bedat
         ekko~lifnr
         lfa1~name1
    FROM ekko
    INNER JOIN lfa1 ON ekko~lifnr EQ lfa1~lifnr
    INTO TABLE gt_ekko
    FOR ALL ENTRIES IN gt_ekpo
    WHERE ekko~ebeln EQ gt_ekpo-ebeln.

ENDFORM.                    " F_GET_PO_DAT
*&---------------------------------------------------------------------*
*&      Form  F_GET_AUFNR_REPORT_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_detail_po .
  SELECT ebeln
         ebelp
         aufnr
         prctr
    FROM ekkn
    INTO TABLE gt_ekkn
    FOR ALL ENTRIES IN gt_ekpo
    WHERE ebeln EQ gt_ekpo-ebeln
      AND ebelp EQ gt_ekpo-ebelp
      AND prctr IN s_prctr.

ENDFORM.                    " F_GET_AUFNR_REPORT_PO
*&---------------------------------------------------------------------*
*&      Form  F_GET_AUFK_REPORT_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_aufk_report_po.
  IF gt_ekkn IS NOT INITIAL.
    SELECT aufnr
      FROM aufk
      INTO TABLE gt_aufk
      FOR ALL ENTRIES IN gt_ekkn
      WHERE aufnr EQ gt_ekkn-aufnr.
  ENDIF.
ENDFORM.                    " F_GET_AUFK_REPORT_PO
*&---------------------------------------------------------------------*
*&      Form  F_GET_CHECK_MATDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_check_matdoc .
  DATA lv_tabix TYPE sy-tabix.

  SELECT mblnr
         mjahr
         zeile
         ebeln
         ebelp
         aufnr
    FROM mseg
    INTO TABLE gt_mseg
    FOR ALL ENTRIES IN gt_aufk
    WHERE aufnr EQ gt_aufk-aufnr.

  LOOP AT gt_ekpo INTO gs_ekpo.
    lv_tabix = sy-tabix.

    READ TABLE gt_mseg INTO gs_mseg
    WITH KEY ebeln = gs_ekpo-ebeln
             ebelp = gs_ekpo-ebelp.
    IF sy-subrc = 0.
      DELETE gt_ekpo INDEX lv_tabix.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " F_GET_CHECK_MATDOC
*&---------------------------------------------------------------------*
*&      Form  F_GET_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_result .

  LOOP AT gt_ekpo INTO gs_ekpo.

    READ TABLE gt_ekkn INTO gs_ekkn  "Check Profit
    WITH KEY ebeln = gs_ekpo-ebeln
             ebelp = gs_ekpo-ebelp.
    IF sy-subrc = 0.
      gs_result-prctr = gs_ekkn-prctr.
      gs_result-aufnr = gs_ekkn-aufnr.
    ELSE.
      CONTINUE.
    ENDIF.

    READ TABLE gt_ekko INTO gs_ekko
    WITH KEY ebeln = gs_ekpo-ebeln.
    IF sy-subrc = 0.
      gs_result-subna = gs_ekko-name1.
      gs_result-bedat = gs_ekko-bedat.
    ENDIF.

    PERFORM f_call_fm_pm_read USING gs_result-aufnr.

    gs_result-ebeln = gs_ekpo-ebeln.
    gs_result-matnr = gs_ekpo-matnr.
    gs_result-netwr = gs_ekpo-netwr.
    gs_result-maktx = gs_ekpo-txz01.
    gs_result-menge = gs_ekpo-menge.
    gs_result-meins = gs_ekpo-meins.

    APPEND gs_result TO gt_result.
    CLEAR : gs_ekpo,gs_ekkn,gs_ekko.
  ENDLOOP.

ENDFORM.                    " F_GET_RESULT
*&---------------------------------------------------------------------*
*&      Form  F_GET_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_selection.
  IF r1 = 'X' OR r4 EQ 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'PO'.
        screen-intensified = '1'.
        screen-active      = '0'.
        screen-display_3d  = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE."IF r2 = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'RE'.
        screen-intensified = '1'.
        screen-active      = '0'.
        screen-display_3d  = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " F_GET_SELECTION
*&---------------------------------------------------------------------*
*&      Form  F_GET_RESULT_SUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_result_sum .

  LOOP AT gt_ekpo INTO gs_ekpo.

    READ TABLE gt_ekkn INTO gs_ekkn  "Check Profit
    WITH KEY ebeln = gs_ekpo-ebeln
             ebelp = gs_ekpo-ebelp.
    IF sy-subrc = 0.
      gs_result-prctr = gs_ekkn-prctr.
      gs_result-aufnr = gs_ekkn-aufnr.
    ELSE.
      CONTINUE.
    ENDIF.

    READ TABLE gt_ekko INTO gs_ekko
    WITH KEY ebeln = gs_ekpo-ebeln.
    IF sy-subrc = 0.
      gs_result-subna = gs_ekko-name1.
      gs_result-bedat = gs_ekko-bedat.
    ENDIF.

    PERFORM f_call_fm_pm_read USING gs_result-aufnr.

    gs_result-ebeln = gs_ekpo-ebeln.
    gs_result-netwr = gs_ekpo-netwr.

    COLLECT gs_result INTO gt_result.
    CLEAR : gs_ekpo,gs_ekkn,gs_ekko.
  ENDLOOP.

ENDFORM.                    " F_GET_RESULT_SUM
*&---------------------------------------------------------------------*
*&      Form  F_GET_CUST_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_cust_name.
  IF r2 = 'X'.
    SELECT obknr
         matnr
         sernr
    FROM objk
    INTO TABLE gt_objk
    FOR ALL ENTRIES IN gt_result
    WHERE matnr EQ gt_result-matnr1
      AND sernr EQ gt_result-sernr
      AND taser EQ 'SER01'.
  ELSEIF r3 = 'X'.
    SELECT obknr
           matnr
           sernr
      FROM objk
      INTO TABLE gt_objk
      FOR ALL ENTRIES IN gt_result
      WHERE matnr EQ gt_result-matnr
        AND sernr EQ gt_result-sernr
        AND taser EQ 'SER01'.
  ENDIF.

  IF gt_objk[] IS NOT INITIAL.
    SELECT obknr
           lief_nr
           posnr
      FROM ser01
      INTO TABLE gt_ser01
      FOR ALL ENTRIES IN gt_objk
      WHERE obknr EQ gt_objk-obknr.

    IF gt_ser01 IS NOT INITIAL.
      SELECT likp~vbeln
             lips~posnr
             likp~kunnr
             kna1~name1
        FROM lips
        INNER JOIN likp ON lips~vbeln EQ likp~vbeln
        INNER JOIN kna1 ON likp~kunnr EQ kna1~kunnr
        INTO TABLE gt_likp
        FOR ALL ENTRIES IN gt_ser01
        WHERE lips~vbeln EQ gt_ser01-lief_nr
          AND lips~posnr EQ gt_ser01-posnr
          AND lips~pstyv NE 'ELN'.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_GET_CUST_NAME
*&---------------------------------------------------------------------*
*&      Form  F_GET_SALE_G_O
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_sale_g_o .
  READ TABLE gt_vbfa INTO gs_vbfa
  WITH KEY vbeln = gs_likp-vbeln
           posnn = gs_likp-posnr.
  IF sy-subrc = 0.
    READ TABLE gt_vbap INTO gs_vbap
    WITH KEY vbeln = gs_vbfa-vbelv
             posnr = gs_vbfa-posnv.
    IF sy-subrc = 0.
      gs_result-vkgrp = gs_vbap-vkgrp.
      gs_result-vkbur = gs_vbap-vkbur.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_GET_SALE_G_O
*&---------------------------------------------------------------------*
*&      Form  F_GET_SO_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_so_po.
  IF gt_likp[] IS NOT INITIAL.
    SELECT vbelv
           posnv
           vbeln
           posnn
      FROM vbfa
      INTO TABLE gt_vbfa
      FOR ALL ENTRIES IN gt_likp
      WHERE vbeln   EQ gt_likp-vbeln
        AND posnn   EQ gt_likp-posnr
        AND vbtyp_v EQ 'C'.

    IF gt_vbfa[] IS NOT INITIAL.
      SELECT vbap~vbeln
             vbap~posnr
             vbap~netwr
             vbap~mwsbp
             vbak~vkgrp
             vbak~vkbur
        FROM vbap
        INNER JOIN vbak ON vbap~vbeln EQ vbak~vbeln
        INTO TABLE gt_vbap
        FOR ALL ENTRIES IN gt_vbfa
        WHERE vbap~vbeln EQ gt_vbfa-vbelv
          AND vbap~posnr EQ gt_vbfa-posnv.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_GET_SO_PO
*&---------------------------------------------------------------------*
*&      Form  F_MODIF_RESULT_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_modif_result_po.

  DATA : lv_tabix TYPE sy-tabix.

  DATA : lv_matnr TYPE matnr.

  LOOP AT gt_result INTO gs_result.
    lv_tabix = sy-tabix.

    IF r2 = 'X'.
      lv_matnr = gs_result-matnr1.
    ELSEIF r3 = 'X'.
      lv_matnr = gs_result-matnr.
    ENDIF.

    LOOP AT gt_objk INTO gs_objk WHERE matnr = lv_matnr
                                   AND sernr = gs_result-sernr.
      READ TABLE gt_ser01 INTO gs_ser01
      WITH KEY obknr = gs_objk-obknr.
      IF sy-subrc = 0.
        READ TABLE gt_likp INTO gs_likp
        WITH KEY vbeln = gs_ser01-lief_nr
                 posnr = gs_ser01-posnr.
        IF sy-subrc = 0.
          gs_result-namec = gs_likp-name1.
          PERFORM f_get_sale_g_o.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF r3 EQ'X'.
      READ TABLE gt_makt INTO gs_makt
      WITH KEY matnr = gs_result-matnr.
      IF sy-subrc = 0.
        gs_result-maktx = gs_makt-maktx.
      ENDIF.
    ENDIF.

    MODIFY gt_result FROM gs_result INDEX lv_tabix
                             TRANSPORTING vkgrp vkbur namec maktx.

    CLEAR : lv_matnr,gs_result,gs_makt,gs_objk,gs_ser01,gs_likp.

  ENDLOOP.

ENDFORM.                    " F_MODIF_RESULT_PO

*&---------------------------------------------------------------------*
*& Report ZSDSCMR0190
*  Creation Date      : 06.07.2025
*  Author             : Wantanee Prateep na thalang
*  Add-on ID          :
*  Description        : Report and program interface
*  Purpose            :
*  Copied from        :
*  Restriction        :
*&---------------------------------------------------------------------*
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
REPORT zsdscmr0190.
*---------------------------------------------------------*
*                       CONSTANTS                         *
*---------------------------------------------------------*
CONSTANTS: gc_save(1) TYPE c      VALUE 'A',
           gc_eqtyp   TYPE eqtyp  VALUE 'M'.

************************************************************************
*      D E C L A R E  T A B L E & S T R U C T  U R E & V A R I A B L E *
************************************************************************
TYPE-POOLS: slis,vrm,truxs.
TABLES: crms4d_serv_h,crms4d_serv_i,TVKBT,TVGRT,eban,ekko,mkpf.
TYPES: BEGIN OF gy_out,
         BANFN      TYPE EBAN-BANFN,
         PR_ITEM    TYPE EBAN-BNFPO,
         PR_AMT     TYPE EBAN-PREIS,
         PR_DATE    TYPE EBAN-ERDAT,
         PO_NO      TYPE EKKO-EBELN,
         PO_ITEM    TYPE EKPO-EBELP,
         PO_AMT     TYPE EKPO-NETPR,
         PO_DATE    TYPE EKKO-AEDAT,
         GR_NO      TYPE MKPF-MBLNR,
         GR_DATE    TYPE MKPF-BUDAT,
         GR_ITEM    TYPE MSEG-ZEILE,
         GR_AMT     TYPE MSEG-DMBTR,
         MATNR      TYPE MSEG-MATNR,
         MENGE      TYPE MSEG-MENGE,
         SAP_SV_NO  TYPE crms4d_serv_h-object_id,
         po_number_sold      TYPE  crms4d_serv_h-po_number_sold,

       END OF gy_out.
TYPES: BEGIN OF gy_EBAN_EBKN,
         banfn TYPE  eban-banfn,
         ERDAT    TYPE EBAN-ERDAT,
         bnfpo TYPE  eban-bnfpo,
         matnr TYPE  eban-matnr,
         menge TYPE  eban-menge,
         PREIS TYPE  eban-PREIS,
         aufnr TYPE  ebkn-aufnr,
         prctr TYPE  ebkn-prctr,
         PS_PSP_PNR TYPE  ebkn-PS_PSP_PNR,
         objguid_a_sel TYPE  crmd_brelvonae-objguid_a_sel,
         ac_assignment TYPE crms4d_serv_i-ac_assignment,
         flag_pr_check TYPE c,
       END OF gy_EBAN_EBKN.
TYPES: BEGIN OF gy_EKKO_EKKN,

         ebeln TYPE  ekko-ebeln,
         AEDAT    TYPE EKKO-AEDAT,
         bsart TYPE  ekko-bsart,
         lifnr TYPE  ekko-lifnr,
         ebelp TYPE  ekpo-ebelp,
         matnr TYPE  ekpo-matnr,
         menge TYPE  ekpo-menge,
         netpr TYPE  ekpo-netpr,
         banfn TYPE  ekpo-banfn,
         bnfpo TYPE  ekpo-bnfpo,
         aufnr TYPE  ekkn-aufnr,
         prctr TYPE  ekkn-prctr,
         PS_PSP_PNR TYPE  ekkn-PS_PSP_PNR,

         objguid_a_sel TYPE  crmd_brelvonae-objguid_a_sel,
         ac_assignment TYPE crms4d_serv_i-ac_assignment,
         flag_po_check TYPE c,
       END OF gy_EKKO_EKKN.

TYPES: BEGIN OF gy_MKPF_MSEG,

         mblnr TYPE  mkpf-mblnr,
         mjahr TYPE  mkpf-mjahr,
         AEDAT TYPE  mkpf-AEDAT,
         budat TYPE  mkpf-budat,
         zeile TYPE  mseg-zeile,
         lifnr TYPE  mseg-lifnr,
         dmbtr TYPE  mseg-dmbtr,
         erfmg TYPE  mseg-erfmg,
         aufnr TYPE  mseg-aufnr,
         matnr TYPE  mseg-matnr,
         ebeln TYPE  mseg-ebeln,
         ebelp TYPE  mseg-ebelp,
         PS_PSP_PNR TYPE mseg-PS_PSP_PNR,
         objguid_a_sel TYPE  crmd_brelvonae-objguid_a_sel,
         ac_assignment TYPE crms4d_serv_i-ac_assignment,
       END OF gy_MKPF_MSEG.

TYPES: BEGIN OF gy_crms4d_serv_h_i,
         objtype_h           TYPE  crms4d_serv_h-objtype_h,
         object_id           TYPE  crms4d_serv_h-object_id,
         process_type        TYPE  crms4d_serv_h-process_type,
         po_number_sold      TYPE  crms4d_serv_h-po_number_sold,
         posting_date        TYPE  crms4d_serv_h-posting_date,
         header_guid         TYPE  crms4d_serv_h-header_guid,
         number_int           TYPE  crms4d_serv_i-number_int,
         po_number_sold_i       TYPE  crms4d_serv_i-po_number_sold,
         ordered_prod         TYPE  crms4d_serv_i-ordered_prod,
         description_i        TYPE  crms4d_serv_i-description_i,
         orig_order_qty       TYPE  crms4d_serv_i-orig_order_qty,
         order_qty            TYPE  crms4d_serv_i-order_qty,
         confirmed_qty        TYPE  crms4d_serv_i-confirmed_qty,
         net_price_i          TYPE  crms4d_serv_i-net_price_i,
         net_value_i          TYPE  crms4d_serv_i-net_value_i,
         net_value_man_i      TYPE  crms4d_serv_i-net_value_man_i,
         netpr_pric_unit      TYPE  crms4d_serv_i-netpr_pric_unit,
         stat_lifecycle_i       TYPE  crms4d_serv_i-stat_lifecycle,
       END OF gy_crms4d_serv_h_i.

TYPES: BEGIN OF gy_iaom_crm_aufk,
         ext_object_id    TYPE  iaom_crm_aufk-ext_object_id,
         aufnr            TYPE  iaom_crm_aufk-aufnr,
         auart            TYPE  iaom_crm_aufk-auart,
         object_id        TYPE  iaom_crm_aufk-object_id,
         process_descript TYPE  iaom_crm_aufk-process_descript,
         process_type     TYPE  iaom_crm_aufk-process_type,
         number_int       TYPE  iaom_crm_aufk-number_int,
         item_description TYPE  iaom_crm_aufk-item_description,
         itm_type         TYPE  iaom_crm_aufk-itm_type,
         category_id      TYPE  iaom_crm_aufk-category_id,
         kunnr            TYPE  iaom_crm_aufk-kunnr,
       END OF gy_iaom_crm_aufk.
TYPES: BEGIN OF gy_crmd_brelvonae,
         relationid    TYPE  crmd_brelvonae-relationid,
         posno         TYPE  crmd_brelvonae-posno,
         breltyp       TYPE  crmd_brelvonae-breltyp,
         attribut      TYPE  crmd_brelvonae-attribut,
         vona_kind     TYPE  crmd_brelvonae-vona_kind,
         invalid       TYPE  crmd_brelvonae-invalid,
         secondary     TYPE  crmd_brelvonae-secondary,
         seqno         TYPE  crmd_brelvonae-seqno,
         objguid_a_sel TYPE  crmd_brelvonae-objguid_a_sel,
         objkey_a_sel  TYPE  crmd_brelvonae-objkey_a_sel,
         objtype_a_sel TYPE  crmd_brelvonae-objtype_a_sel,
         logsys_a_sel  TYPE  crmd_brelvonae-logsys_a_sel,
         objguid_b_sel TYPE  crmd_brelvonae-objguid_b_sel,
         objkey_b_sel  TYPE  crmd_brelvonae-objkey_b_sel,
         objtype_b_sel TYPE  crmd_brelvonae-objtype_b_sel,
       END OF gy_crmd_brelvonae.

TYPES: BEGIN OF GY_objkey_b_sel,
        objkey_b_sel  TYPE  crmd_brelvonae-objkey_b_sel,
END OF GY_objkey_b_sel.



DATA: gt_output           TYPE TABLE OF gy_out,
      gs_output           TYPE gy_out,
      wa_output           TYPE gy_out,
      gw_output           TYPE gy_out,
      gt_crms4d_serv_h_i    TYPE STANDARD TABLE OF gy_crms4d_serv_h_i,
      gs_crms4d_serv_h_i    TYPE  gy_crms4d_serv_h_i,
      gt_MKPF_MSEG        TYPE STANDARD TABLE OF gy_MKPF_MSEG,
      gt_MKPF_MSEG_temp        TYPE STANDARD TABLE OF gy_MKPF_MSEG,
      gs_MKPF_MSEG        TYPE  gy_MKPF_MSEG,
      gt_EKKO_EKKN   TYPE STANDARD TABLE OF gy_EKKO_EKKN,
      gt_EKKO_EKKN_temp   TYPE STANDARD TABLE OF gy_EKKO_EKKN,
      gs_EKKO_EKKN   TYPE  gy_EKKO_EKKN,
      gt_EBAN_EBKN   TYPE STANDARD TABLE OF gy_EBAN_EBKN,
      gt_EBAN_EBKN_temp   TYPE STANDARD TABLE OF gy_EBAN_EBKN,
      gs_EBAN_EBKN   TYPE  gy_EBAN_EBKN,
      gt_crmd_brelvonae   TYPE STANDARD TABLE OF gy_crmd_brelvonae,
      gs_crmd_brelvonae   TYPE  gy_crmd_brelvonae,
      gt_objkey_b_sel TYPE STANDARD TABLE OF GY_objkey_b_sel,
      gs_objkey_b_sel TYPE GY_objkey_b_sel.



DATA: msg_show(200) TYPE c.



DATA: GRID_MAIN      TYPE REF TO CL_GUI_ALV_GRID,
      CONTAINER_MAIN TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_VARIANT     LIKE DISVARIANT.

DATA : V_POS TYPE I .
DATA : GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
       GT_LAYOUT   TYPE SLIS_LAYOUT_ALV,
       GT_EVENTS   TYPE SLIS_T_EVENT.
RANGES : GR_TCODE FOR CDHDR-TCODE.

DATA: bdcdata TYPE TABLE OF bdcdata WITH HEADER LINE.
DATA:     g_tc_100_lines  LIKE sy-loopc.
DATA: name  TYPE vrm_id,
      list  TYPE vrm_values,
      value LIKE LINE OF list.

DATA: lv_linecount TYPE i.
DATA: fill TYPE i .
CONTROLS: tc_100 TYPE TABLEVIEW USING SCREEN 0100.

RANGES : GAC_ASSIGNMENT FOR CRMS4D_SERV_I-AC_ASSIGNMENT.
RANGES : GAC_OBJECT_ID FOR CRMS4D_SERV_H-OBJECT_ID.

*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS : GC_REPID       TYPE REPID         VALUE 'ZSDSCMR0190',
            GC_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE'.
CONSTANTS: gc_mask_time   TYPE char8  VALUE '__:__',
           gc_mask_date   TYPE char10 VALUE '__/__/____'.
*&---------------------------------------------------------------------*
*&  M A C R O   C O M M A N D   D E F I N I T I O N                    *
*&---------------------------------------------------------------------*
DEFINE m_fill_cat.
  gs_fieldcat-tabname    = &1.
  gs_fieldcat-fieldname  = &2.
  gs_fieldcat-col_pos    = &3.
  gs_fieldcat-seltext_l  = &4.
  gs_fieldcat-no_out     = &5.
  gs_fieldcat-outputlen  = &6.
*  gs_fieldcat-input       = &7.
  gs_fieldcat-checkbox    = &7.
  gs_fieldcat-edit        = &8.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

END-OF-DEFINITION.

DEFINE def_list_head.
  clear: lw_listline.
  lw_listline-typ  = &1.
  lw_listline-key  = &2.
  lw_listline-info = &3.
  append lw_listline to lt_listhead.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&  M A C R O   C O M M A N D   D E F I N I T I O N  ( %_ )            *
*&---------------------------------------------------------------------*
DEFINE %show. " show input field
  IF screen-group1 = &1.
    screen-invisible = 0.
    screen-active = 1.
  ENDIF.
END-OF-DEFINITION.
DEFINE %hide. " hide input field
  IF screen-group1 = &1.
    screen-invisible = 1.
    screen-active = 0.
  ENDIF.
END-OF-DEFINITION.
************************************************************************
*      S E L E C T I O N  S C R E E N                                  *
************************************************************************



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.

  SELECT-OPTIONS : "s_OBJT_H FOR crms4d_serv_h-objtype_h,
                   S_PR_NO  FOR EBAN-BANFN,
                   S_CREDAT FOR MKPF-AEDAT,
                   S_PO_NO  FOR EKKO-EBELN,
                   S_GR_NO  FOR MKPF-MBLNR,
                   s_OBJ_ID FOR crms4d_serv_h-object_id,
                   s_PRO_TY FOR crms4d_serv_h-process_type.
SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK block3 WITH FRAME TITLE text-b02.
PARAMETERS: p_disp RADIOBUTTON GROUP gr1 ,                   " Download to WEB Server
*            p_local RADIOBUTTON GROUP gr1,
            p_idoc RADIOBUTTON GROUP gr1 DEFAULT 'X',
            p_path LIKE rlgrap-filename  LOWER CASE..
SELECTION-SCREEN END OF BLOCK block3.

************************************************************************
*      A T     S E L E C T I O N     S C R E E N                       *
************************************************************************
INITIALIZATION.




AT SELECTION-SCREEN OUTPUT.

************************************************************************
*      B E G I N      S E L E C T I O N                                *
************************************************************************
START-OF-SELECTION .
*  PERFORM F_CONVERT_DATA_IO.
  PERFORM f_get_data.
  PERFORM f_map_data.



************************************************************************
*      E N D      S E L E C T I O N                                    *
************************************************************************
END-OF-SELECTION .

  IF gt_output[] IS NOT INITIAL.
    PERFORM f_display_report.
  ELSE.
    MESSAGE i000(38) WITH TEXT-e04 DISPLAY LIKE 'E'.
  ENDIF.






*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_UNMAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data .
  DATA: lv_banfn TYPE eban-banfn,
        lv_ebeln TYPE ekko-ebeln,
        lv_vbeln TYPE vbrk-vbeln,
        lv_mblnr TYPE mkpf-mblnr,
        lv_mjahr TYPE mkpf-mjahr,
        lv_objguid_a_sel TYPE crmd_brelvonae-objguid_a_sel,
        lv_objkey_b_sel TYPE crmd_brelvonae-objkey_b_sel.

*    S_PR_NO  FOR EBAN-BANFN,
*                   S_CREDAT FOR MKPF-AEDAT,
*                   S_PO_NO  FOR EKKO-EBELN,
*                   S_GR_NO  FOR MKPF-MBLNR,
*                   s_OBJ_ID FOR crms4d_serv_h-object_id,
*  IF S_PR_NO IS INITIAL AND
*     S_PO_NO IS INITIAL AND
*     S_GR_NO IS INITIAL AND
*     s_OBJ_ID IS NOT INITIAL.
*
*
*  ELSE.


       SELECT a~banfn,a~CREATIONDATE, a~bnfpo, a~matnr, a~menge, a~PREIS, b~aufnr, b~prctr, b~PS_PSP_PNR
                      INTO TABLE @gt_EBAN_EBKN_temp
                      FROM EBAN AS a INNER JOIN EBKN AS b
                                           ON ( a~banfn EQ b~banfn )
                      WHERE a~banfn IN @S_PR_NO
                      AND   a~CREATIONDATE IN @S_CREDAT.
         LOOP AT gt_EBAN_EBKN_temp INTO gs_EBAN_EBKN.
                gs_objkey_b_sel-objkey_b_sel = gs_EBAN_EBKN-banfn.
                APPEND gs_objkey_b_sel TO gt_objkey_b_sel.
         ENDLOOP.



       SELECT b~ebeln, a~AEDAT, b~bsart, b~lifnr,
                             a~ebelp, a~matnr, a~menge, a~netpr, a~banfn, a~bnfpo,
                             d~aufnr, d~prctr,a~PS_PSP_PNR
                      INTO TABLE @gt_EKKO_EKKN_temp
                      FROM  ekpo AS a INNER JOIN EKKO AS b
                                           ON ( a~ebeln EQ b~ebeln )
                                     INNER JOIN ekkn AS d
                                           ON ( a~ebeln EQ d~ebeln
                                           AND  a~ebelp EQ d~ebelp )
                      WHERE a~ebeln IN @S_PO_NO
                        AND a~AEDAT IN @S_CREDAT.
         LOOP AT gt_EKKO_EKKN_temp INTO gs_EKKO_EKKN.
                gs_objkey_b_sel-objkey_b_sel = gs_EKKO_EKKN-ebeln.
                APPEND gs_objkey_b_sel TO gt_objkey_b_sel.
         ENDLOOP.

      SELECT a~mblnr, a~mjahr,a~AEDAT, a~budat,
                             b~zeile, b~lifnr, b~dmbtr, b~erfmg,
                             b~aufnr, b~matnr, b~ebeln, b~ebelp,
                             b~PS_PSP_PNR
                      INTO TABLE @gt_MKPF_MSEG_temp
                      FROM mkpf AS a INNER JOIN mseg AS b
                                           ON ( a~mblnr EQ b~mblnr
                                           AND a~mjahr EQ b~mjahr )
                      WHERE b~mblnr IN @S_GR_NO
                        AND a~AEDAT IN @S_CREDAT.
        LOOP AT gt_MKPF_MSEG_temp INTO gs_MKPF_MSEG.
                CONCATENATE gs_MKPF_MSEG-mblnr gs_MKPF_MSEG-mjahr INTO gs_objkey_b_sel-objkey_b_sel.
*                gs_objkey_b_sel-objkey_b_sel = gs_MKPF_MSEG-mblnr.
                APPEND gs_objkey_b_sel TO gt_objkey_b_sel.
         ENDLOOP.


*  ENDIF.

  IF gt_objkey_b_sel IS NOT INITIAL.
    SELECT relationid,posno,breltyp,attribut,vona_kind,invalid,secondary,
               seqno,objguid_a_sel,objkey_a_sel,objtype_a_sel,logsys_a_sel,
               objguid_b_sel,objkey_b_sel,objtype_b_sel
         INTO TABLE @gt_crmd_brelvonae
         FROM crmd_brelvonae
         FOR ALL ENTRIES IN @gt_objkey_b_sel
         WHERE objkey_b_sel = @gt_objkey_b_sel-objkey_b_sel.
  ENDIF.



   IF gt_crmd_brelvonae IS NOT INITIAL.
      SELECT  a~objtype_h,a~object_id,a~process_type,a~po_number_sold,a~posting_date,
               a~header_guid,
               b~number_int,
               b~po_number_sold,b~ordered_prod,b~description_i,b~orig_order_qty,b~order_qty,b~confirmed_qty,
              b~net_price_i,b~net_value_i,b~net_value_man_i,b~netpr_pric_unit,
              b~stat_lifecycle
      INTO TABLE @gt_crms4d_serv_h_i
        FROM crms4d_serv_h AS a INNER JOIN crms4d_serv_i AS b
                                  ON ( a~objtype_h EQ b~objtype_h
                                  AND  a~object_id EQ b~object_id )
*                                   ON ( a~object_id EQ b~object_id )
        FOR ALL ENTRIES IN @gt_crmd_brelvonae
        WHERE a~header_guid EQ @gt_crmd_brelvonae-objguid_a_sel.
   ENDIF.


*
*
*      IF gt_crms4d_serv_h_i IS NOT INITIAL.
*
*
*        SELECT relationid,posno,breltyp,attribut,vona_kind,invalid,secondary,
*               seqno,objguid_a_sel,objkey_a_sel,objtype_a_sel,logsys_a_sel,
*               objguid_b_sel,objkey_b_sel,objtype_b_sel
*         INTO TABLE @gt_crmd_brelvonae
*         FROM crmd_brelvonae
*         FOR ALL ENTRIES IN @gt_crms4d_serv_h_i
*         WHERE OBJGUID_A_SEL = @gt_crms4d_serv_h_i-header_guid.
*
*          SELECT objtype_h,object_id,bill_date,price_date,net_value
*            INTO TABLE @gt_CRMS4D_BILLREQ_I
*            FROM CRMS4D_BILLREQ_I
*            FOR ALL ENTRIES IN @gt_crms4d_serv_h_i
*            WHERE objtype_h = @gt_crms4d_serv_h_i-objtype_h
*            AND object_id = @gt_crms4d_serv_h_i-object_id.
*
*            LOOP AT gt_crmd_brelvonae INTO gs_crmd_brelvonae.
*                 CLEAR: gt_EKKO_EKKN_temp,gt_EBAN_EBKN_temp ,gt_MKPF_MSEG_temp,gt_VBRK_temp,
*                 lv_banfn, lv_ebeln,lv_vbeln,lv_mblnr,lv_mjahr,lv_objguid_a_sel,
*                 gs_EBAN_EBKN,gs_EKKO_EKKN,gs_VBRK.
*                 IF gs_crmd_brelvonae-objtype_b_sel EQ 'BUS2105'. "PR
*                    lv_banfn = gs_crmd_brelvonae-objkey_b_sel.
*
*                    SELECT a~banfn, a~bnfpo, a~matnr, a~menge, a~PREIS, b~aufnr, b~prctr, b~PS_PSP_PNR
*                      INTO TABLE @gt_EBAN_EBKN_temp
*                      FROM EBAN AS a INNER JOIN EBKN AS b
*                                           ON ( a~banfn EQ b~banfn )
*                      WHERE a~banfn EQ @lv_banfn.
*
*                      LOOP AT gt_EBAN_EBKN_temp INTO gs_EBAN_EBKN.
*                          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*                          EXPORTING
*                            INPUT  = gs_EBAN_EBKN-PS_PSP_PNR
*                          IMPORTING
*                            OUTPUT = gs_EBAN_EBKN-ac_assignment.
*
*                          gs_EBAN_EBKN-objguid_a_sel = gs_crmd_brelvonae-objguid_a_sel.
*                          APPEND gs_EBAN_EBKN TO gt_EBAN_EBKN.
*
*                      ENDLOOP.
*
*                 ELSEIF gs_crmd_brelvonae-objtype_b_sel EQ 'BUS2012'. "PO
*                       lv_ebeln = gs_crmd_brelvonae-objkey_b_sel.
*                      SELECT b~ebeln, b~bsart, b~lifnr,
*                             a~ebelp, a~matnr, a~menge, a~netpr,
*                             d~aufnr, d~prctr,a~PS_PSP_PNR
*                      INTO TABLE @gt_EKKO_EKKN_temp
*                      FROM  ekpo AS a INNER JOIN EKKO AS b
*                                           ON ( a~ebeln EQ b~ebeln )
*                                     INNER JOIN ekkn AS d
*                                           ON ( a~ebeln EQ d~ebeln
*                                           AND  a~ebelp EQ d~ebelp )
*                      WHERE a~ebeln EQ @lv_ebeln
*                        AND b~ebeln EQ @lv_ebeln
*                        AND d~ebeln EQ @lv_ebeln.
*
*
*                      LOOP AT gt_EKKO_EKKN_temp INTO gs_EKKO_EKKN.
*                          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*                          EXPORTING
*                            INPUT  = gs_EKKO_EKKN-PS_PSP_PNR
*                          IMPORTING
*                            OUTPUT = gs_EKKO_EKKN-ac_assignment.
*
*                          gs_EKKO_EKKN-objguid_a_sel = gs_crmd_brelvonae-objguid_a_sel.
*                          APPEND gs_EKKO_EKKN TO gt_EKKO_EKKN.
*
*                      ENDLOOP.
*
*                      SELECT a~mblnr, a~mjahr, a~budat,
*                             b~zeile, b~lifnr, b~dmbtr, b~erfmg,
*                             b~aufnr, b~matnr, b~ebeln, b~ebelp,
*                             b~PS_PSP_PNR
*                      INTO TABLE @gt_MKPF_MSEG_temp
*                      FROM mkpf AS a INNER JOIN mseg AS b
*                                           ON ( a~mblnr EQ b~mblnr
*                                           AND a~mjahr EQ b~mjahr )
*                      WHERE b~EBELN = @lv_ebeln.
*                        LOOP AT gt_MKPF_MSEG_temp INTO gs_MKPF_MSEG.
*                              CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*                                 EXPORTING
*                                   INPUT  = gs_MKPF_MSEG-PS_PSP_PNR
*                                 IMPORTING
*                                   OUTPUT = gs_MKPF_MSEG-ac_assignment.
*
**                               gs_MKPF_MSEG-objguid_a_sel = lv_objkey_b_sel.
*                                 gs_MKPF_MSEG-objguid_a_sel = gs_crmd_brelvonae-objguid_a_sel.
*                                 APPEND gs_MKPF_MSEG TO gt_MKPF_MSEG.
*
*                        ENDLOOP.
*
*                 ELSEIF gs_crmd_brelvonae-objtype_b_sel EQ 'VBRK'. "INV
*                      lv_vbeln = gs_crmd_brelvonae-objkey_b_sel.
*                      SELECT a~vbeln, a~fkart, a~ernam,
*                             a~erzet, a~erdat, a~belnr, a~bstnk_vf,
*                             b~posnr, b~matnr, b~netwr,b~aubel,b~aufnr, b~PS_PSP_PNR
*                      INTO TABLE @gt_VBRK_temp
*                      FROM vbrk AS a INNER JOIN vbrp AS b
*                                           ON ( a~vbeln EQ b~vbeln )
*                      WHERE a~vbeln EQ @lv_vbeln.
*
*                      LOOP AT gt_VBRK_temp INTO gs_VBRK.
*                         CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*                          EXPORTING
*                            INPUT  = gs_VBRK-PS_PSP_PNR
*                          IMPORTING
*                            OUTPUT = gs_VBRK-ac_assignment.
*
*                          gs_VBRK-objguid_a_sel = gs_crmd_brelvonae-objguid_a_sel.
*                          APPEND gs_VBRK TO gt_VBRK.
*
*                      ENDLOOP.
*
*                 ELSEIF gs_crmd_brelvonae-objtype_b_sel EQ 'BUS2017'. "MSEG
*                     lv_vbeln = gs_crmd_brelvonae-objkey_b_sel+0(10).
*                     lv_mjahr = gs_crmd_brelvonae-objkey_b_sel+10(4).
*
*                     SELECT a~mblnr, a~mjahr, a~budat,
*                             b~zeile, b~lifnr, b~dmbtr, b~erfmg,
*                             b~aufnr, b~matnr, b~ebeln, b~ebelp,
*                             b~PS_PSP_PNR
*                      INTO TABLE @gt_MKPF_MSEG_temp
*                      FROM mkpf AS a INNER JOIN mseg AS b
*                                           ON ( a~mblnr EQ b~mblnr
*                                           AND a~mjahr EQ b~mjahr )
*                      WHERE a~mblnr EQ @lv_vbeln
*                       AND  a~mjahr EQ @lv_mjahr.
*
*                      LOOP AT gt_MKPF_MSEG_temp INTO gs_MKPF_MSEG.
*                         CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*                          EXPORTING
*                            INPUT  = gs_MKPF_MSEG-PS_PSP_PNR
*                          IMPORTING
*                            OUTPUT = gs_MKPF_MSEG-ac_assignment.
*
*
*                          gs_MKPF_MSEG-objguid_a_sel = gs_crmd_brelvonae-objguid_a_sel.
*                          APPEND gs_MKPF_MSEG TO gt_MKPF_MSEG.
*
*                      ENDLOOP.
*
*                 ELSEIF gs_crmd_brelvonae-objtype_b_sel EQ 'BUS2000117'. "Confirmation
*                      CLEAR : lv_objkey_b_sel.
*                      lv_objkey_b_sel = gs_crmd_brelvonae-objkey_b_sel.
*                      lv_objguid_a_sel = gs_crmd_brelvonae-objkey_b_sel.
*
*                      SELECT GUID,OBJTYPE_H,OBJECT_ID,PROCESS_TYPE
*                      INTO TABLE @gt_CONFIRMATION_temp
*                      FROM CRMS4D_BTX_H
*                      WHERE GUID EQ @lv_objguid_a_sel.
*                      LOOP AT gt_CONFIRMATION_temp INTO gs_CONFIRMATION.
*
*                          gs_CONFIRMATION-objguid_a_sel = gs_crmd_brelvonae-objguid_a_sel.
*                          APPEND gs_CONFIRMATION TO gt_CONFIRMATION.
*
*                      ENDLOOP.
*
*                      SELECT SINGLE objkey_b_sel
*                        INTO lv_objkey_b_sel
*                        FROM crmd_brelvonae
*                        WHERE objguid_a_sel = lv_objkey_b_sel
*                        AND objtype_b_sel EQ 'BUS2017'.
*                     IF lv_objkey_b_sel IS NOT INITIAL.
*                          lv_vbeln = lv_objkey_b_sel+0(10).
*                          lv_mjahr = lv_objkey_b_sel+10(4).
*
*                          SELECT a~mblnr, a~mjahr, a~budat,
*                                  b~zeile, b~lifnr, b~dmbtr, b~menge,
*                                  b~aufnr, b~matnr, b~ebeln, b~ebelp,
*                                  b~PS_PSP_PNR
*                           INTO TABLE @gt_MKPF_MSEG_temp
*                           FROM mkpf AS a INNER JOIN mseg AS b
*                                                ON ( a~mblnr EQ b~mblnr
*                                                AND a~mjahr EQ b~mjahr )
*                           WHERE a~mblnr EQ @lv_vbeln
*                            AND  a~mjahr EQ @lv_mjahr.
*
*                           LOOP AT gt_MKPF_MSEG_temp INTO gs_MKPF_MSEG.
*                              CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*                                 EXPORTING
*                                   INPUT  = gs_MKPF_MSEG-PS_PSP_PNR
*                                 IMPORTING
*                                   OUTPUT = gs_MKPF_MSEG-ac_assignment.
*
**                               gs_MKPF_MSEG-objguid_a_sel = lv_objkey_b_sel.
*                                 gs_MKPF_MSEG-objguid_a_sel = gs_crmd_brelvonae-objguid_a_sel.
*                                 APPEND gs_MKPF_MSEG TO gt_MKPF_MSEG.
*
*                           ENDLOOP.
*                      ENDIF.
*
*                 ENDIF.
*
*            ENDLOOP.
*
*
*      ENDIF.





ENDFORM.                    " F_PROCESS_DATA


*&---------------------------------------------------------------------*
*&      Form  F_MAP_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_map_data .

  DATA: LV_PR_ITEM TYPE EBAN-BNFPO.
  DATA: LV_OBJECT_KEY_B_SALE TYPE  crmd_brelvonae-objkey_b_sel.

*          BANFN      TYPE EBAN-BANFN,
*         PR_AMT     TYPE EBAN-PREIS,
*         PR_DATE    TYPE EBAN-ERDAT,
*         PO_NO      TYPE EKKO-EBELN,
*         PO_AMT     TYPE EKPO-NETPR,
*         PO_DATE    TYPE EKKO-AEDAT,
*         GR_NO      TYPE MKPF-MBLNR,
*         GR_DATE    TYPE MKPF-BUDAT,
*         GR_AMT     TYPE MSEG-DMBTR,
*         MATNR      TYPE MSEG-MATNR,
*         MENGE      TYPE MSEG-MENGE,
*         SAP_SV_NO  TYPE crms4d_serv_h-object_id,
*         po_number_sold      TYPE  crms4d_serv_h-po_number_sold,



   CLEAR: gt_output.

   LOOP AT gt_MKPF_MSEG_temp INTO gs_MKPF_MSEG.
         CLEAR: gs_output,LV_PR_ITEM,LV_OBJECT_KEY_B_SALE.

          gs_output-GR_NO = gs_MKPF_MSEG-mblnr.

          gs_output-GR_DATE = gs_MKPF_MSEG-BUDAT.
          gs_output-GR_AMT = gs_MKPF_MSEG-DMBTR.
          gs_output-MATNR = gs_MKPF_MSEG-MATNR.
          gs_output-MENGE = gs_MKPF_MSEG-erfmg.
          gs_output-GR_ITEM = gs_MKPF_MSEG-zeile.
          CONCATENATE gs_MKPF_MSEG-mblnr gs_MKPF_MSEG-mjahr INTO LV_OBJECT_KEY_B_SALE.
          READ TABLE gt_crmd_brelvonae INTO gs_crmd_brelvonae WITH KEY objkey_b_sel = LV_OBJECT_KEY_B_SALE.
            IF sy-subrc EQ 0.
               READ TABLE gt_crms4d_serv_h_i INTO gs_crms4d_serv_h_i WITH KEY header_guid = gs_crmd_brelvonae-objguid_a_sel.
                  IF sy-subrc EQ 0.
                      gs_output-SAP_SV_NO  = gs_crms4d_serv_h_i-object_id.
                      gs_output-po_number_sold  = gs_crms4d_serv_h_i-po_number_sold.
                  ENDIF.
            ENDIF.

          gs_output-PO_NO = gs_MKPF_MSEG-EBELN.
          gs_output-PO_ITEM = gs_MKPF_MSEG-ebelp.
          SELECT SINGLE BANFN, NETPR, CREATIONDATE, BNFPO
            INTO ( @gs_output-BANFN, @gs_output-PO_AMT, @gs_output-PO_DATE ,@LV_PR_ITEM )
            FROM EKPO
            WHERE ebeln = @gs_MKPF_MSEG-EBELN
              AND ebelp = @gs_MKPF_MSEG-ebelp.



          gs_output-PR_ITEM = LV_PR_ITEM.
          SELECT SINGLE PREIS,CREATIONDATE
            INTO ( @gs_output-PR_AMT, @gs_output-PR_DATE )
            FROM EBAN
            WHERE BANFN = @gs_output-BANFN
              AND ebelp = @LV_PR_ITEM.


         APPEND gs_output to gt_output.

   ENDLOOP.

   LOOP AT gt_EKKO_EKKN_temp INTO gs_EKKO_EKKN.
        CLEAR: gs_output.

        READ TABLE gt_output INTO gw_output WITH KEY PO_NO = gs_EKKO_EKKN-EBELN.
            IF sy-subrc NE 0.
               gs_output-PO_NO = gs_EKKO_EKKN-EBELN.
               gs_output-PO_AMT = gs_EKKO_EKKN-NETPR.
               gs_output-PO_DATE = gs_EKKO_EKKN-AEDAT.
               gs_output-BANFN = gs_EKKO_EKKN-BANFN.
               gs_output-PR_ITEM = gs_EKKO_EKKN-BNFPO.
               gs_output-matnr = gs_EKKO_EKKN-matnr.
               gs_output-MENGE = gs_EKKO_EKKN-menge.
               gs_output-PO_ITEM = gs_EKKO_EKKN-EBELP.

               SELECT SINGLE PREIS,CREATIONDATE
               INTO ( @gs_output-PR_AMT, @gs_output-PR_DATE )
               FROM EBAN
               WHERE BANFN = @gs_EKKO_EKKN-BANFN
                 AND ebelp = @gs_EKKO_EKKN-BNFPO.
                  READ TABLE gt_crmd_brelvonae INTO gs_crmd_brelvonae WITH KEY objkey_b_sel = gs_EKKO_EKKN-EBELN.
                      IF sy-subrc EQ 0.
                         READ TABLE gt_crms4d_serv_h_i INTO gs_crms4d_serv_h_i WITH KEY header_guid = gs_crmd_brelvonae-objguid_a_sel.
                            IF sy-subrc EQ 0.
                                gs_output-SAP_SV_NO  = gs_crms4d_serv_h_i-object_id.
                                gs_output-po_number_sold  = gs_crms4d_serv_h_i-po_number_sold.
                            ENDIF.
                      ENDIF.

               APPEND gs_output to gt_output.
            ENDIF.

   ENDLOOP.

  LOOP AT gt_EBAN_EBKN_temp INTO gs_EBAN_EBKN.
        CLEAR: gs_output.

        READ TABLE gt_output INTO gw_output WITH KEY BANFN = gs_EBAN_EBKN-BANFN.
            IF sy-subrc NE 0.
               gs_output-BANFN = gs_EBAN_EBKN-BANFN.
               gs_output-PR_AMT = gs_EBAN_EBKN-PREIS.
               gs_output-PR_DATE = gs_EBAN_EBKN-ERDAT.
                gs_output-matnr = gs_EBAN_EBKN-matnr.
               gs_output-MENGE = gs_EBAN_EBKN-MENGE.
               gs_output-PR_ITEM = gs_EBAN_EBKN-bnfpo.
                READ TABLE gt_crmd_brelvonae INTO gs_crmd_brelvonae WITH KEY objkey_b_sel = gs_EBAN_EBKN-BANFN.
                     IF sy-subrc EQ 0.
                        READ TABLE gt_crms4d_serv_h_i INTO gs_crms4d_serv_h_i WITH KEY header_guid = gs_crmd_brelvonae-objguid_a_sel.
                           IF sy-subrc EQ 0.
                               gs_output-SAP_SV_NO  = gs_crms4d_serv_h_i-object_id.
                               gs_output-po_number_sold  = gs_crms4d_serv_h_i-po_number_sold.
                           ENDIF.
                     ENDIF.

               APPEND gs_output to gt_output.
            ENDIF.

   ENDLOOP.


IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.




ENDFORM.                    " F_PROCESS_DATA

*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_display_report .


  PERFORM BUILD_LAYOUT.
  PERFORM BUILD_CATALOG.
  PERFORM BUILD_EVENT USING GT_EVENTS[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = GC_REPID
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      I_SAVE                  = 'A'
      is_layout               = gt_layout
      IT_EVENTS               = GT_EVENTS[]
      IT_FIELDCAT             = GT_FIELDCAT
    TABLES
      T_OUTTAB                = gt_output
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " F_DISPLAY_REPORT
* ----------------------------------------------------
* Status
* ----------------------------------------------------
FORM status_set USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'ZSTANDARD' EXCLUDING rt_extab.

ENDFORM.                    "status_set
*&---------------------------------------------------------------------*
*&      Form  usercommand
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_UCOMM    text
*      -->I_SELFIELD text
*----------------------------------------------------------------------*
FORM usercommand USING i_ucomm i_selfield TYPE slis_selfield.

  DATA: lt_mess_tab TYPE tab_bdcmsgcoll,
        lw_mess_tab TYPE bdcmsgcoll.

  DATA: lv_mode   TYPE c VALUE 'N',
        lv_upd    TYPE c VALUE 'S',
        lv_msgtyp TYPE c.

*
**&---------------------------------------------------------------------*
**&for Check = 'X' when tick Check Box
**&---------------------------------------------------------------------*
*  DATA : ref_grid TYPE REF TO cl_gui_alv_grid.
*
*  IF ref_grid IS INITIAL.
*    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*      IMPORTING
*        e_grid = ref_grid.
*    CALL METHOD ref_grid->check_changed_data.
*  ENDIF.
**&---------------------------------------------------------------------*

*  BREAK wantanee.
*
*
*
*  CASE i_ucomm.
*    WHEN 'UPLOAD'.
*
*      LOOP AT gt_data INTO gs_data.
*        INSERT INTO zsdsfit042
*         VALUES gs_data.
*        COMMIT WORK.
*      ENDLOOP.
*      MESSAGE s000(38) WITH 'Upload Table ZTAP_STATE_BANK Complete'.
*      LEAVE TO SCREEN 0.
*
*    WHEN OTHERS.
*
*  ENDCASE.
*
*  i_selfield-refresh = 'X'.
*  i_selfield-col_stable = 'X'.
*  i_selfield-row_stable = 'X'.

ENDFORM.                    "USERCOMMAND


*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT .
  GT_LAYOUT-WINDOW_TITLEBAR = SY-TITLE.
  GT_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GT_LAYOUT-ZEBRA = 'X'.

ENDFORM.                    "build_layout

*&---------------------------------------------------------------------*
*&      Form  BUILD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_CATALOG .
  CLEAR : V_POS.


  PERFORM APPEND_FIELDCAT USING  'BANFN' 'EBAN' 'BANFN' 'PR_NO'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING  'PR_ITEM' 'EBAN' 'BNFPO' 'PR_ITEM'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING  'PR_AMT' 'EBAN' 'PREIS' 'PR_AMT'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING  'PR_DATE' 'EBAN' 'ERDAT' 'PR_DATE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING  'PO_NO' 'EKPO' 'EBELN' 'PO_NO'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING  'PO_ITEM' 'EKPO' 'EBELP' 'PO_ITEM'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING  'PO_DATE' 'EKPO' 'AEDAT' 'PO DATE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING  'PO_AMT' 'EKKO' 'PREIS' 'PR_AMT'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING  'GR_NO' 'MKPF' 'MBLNR' 'GR NO'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING  'GR_ITEM' 'MSEG' 'ZEILE' 'GR ITEM'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING  'GR_DATE' 'MKPF' 'BUDAT' 'GR_DATE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING  'GR_AMT' 'MSEG' 'DMBTR' 'GR AMT'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
*  PERFORM APPEND_FIELDCAT USING  'PO_DATE' 'EKKO' 'AEDAT' 'PO_DATE'
*                                 SPACE  SPACE  SPACE
*                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING  'MATNR' 'MSEG' 'MATNR' 'MATERIAL'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING  'MENGE' 'MSEG' 'MENGE' 'QTY'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING  'SAP_SV_NO' 'CRMS4D_SERV_H' 'OBJECT_ID' 'SAP SV NO'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING  'PO_NUMBER_SOLD' 'CRMS4D_SERV_H' 'PO_NUMBER_SOLD' 'SF WO NO'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].










ENDFORM.




*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*----------------------------------------------------------------------*
FORM BUILD_EVENT  USING E03_LT_EVENTS TYPE SLIS_T_EVENT.
  DATA: LW_EVENT TYPE SLIS_ALV_EVENT.
  FIELD-SYMBOLS <fs_events> LIKE LINE OF E03_LT_EVENTS.
* Read Event
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 1
    IMPORTING
      ET_EVENTS   = E03_LT_EVENTS.
  READ TABLE E03_LT_EVENTS WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
                           INTO LW_EVENT.
*   READ TABLE e03_lt_events WITH KEY name = slis_ev_top_of_list
*                             INTO lw_event.
*  IF SY-SUBRC = 0.
** register top of page event
*    MOVE GC_TOP_OF_PAGE TO LW_EVENT-FORM.
*    APPEND LW_EVENT TO E03_LT_EVENTS.
*  ENDIF.

  LOOP AT E03_LT_EVENTS ASSIGNING <fs_events>
                    WHERE name = 'TOP_OF_PAGE'.
    <fs_events>-form = 'REPORT_HEADER'.
  ENDLOOP.
ENDFORM.                    " BUILD_EVENT
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1453   text
*      -->P_1454   text
*      -->P_1455   text
*      -->P_TEXT_T01  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*

FORM APPEND_FIELDCAT USING   P_FIELD   "Field name
                             P_REFTABLE"Reference Table name
                             P_REFFIELD"Reference Field name
                             P_COLTXT  "Col Text(for specify)
                             P_DOSUM   "Sum total
                             P_CFIELDNAME  "  currency
                             P_NO_ZERO     " no zero
                             P_IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

  DATA: WA_INFIELDCAT   TYPE SLIS_FIELDCAT_ALV,
        V_COLTXT_LENGTH TYPE I.

  ADD 1 TO V_POS.

  WA_INFIELDCAT-FIELDNAME     = P_FIELD.
  WA_INFIELDCAT-REF_TABNAME   = P_REFTABLE.
  WA_INFIELDCAT-REF_FIELDNAME = P_REFFIELD.
  WA_INFIELDCAT-COL_POS       = V_POS .
  WA_INFIELDCAT-DO_SUM        = P_DOSUM.

  IF NOT P_NO_ZERO IS INITIAL .
    WA_INFIELDCAT-NO_ZERO = P_NO_ZERO.
  ENDIF.
  IF NOT P_CFIELDNAME IS INITIAL .
    WA_INFIELDCAT-CFIELDNAME = P_CFIELDNAME .
  ENDIF.


*If we need to specify text ,don't need to derive from data dictionary
*program will check length and define width of the colum
  IF NOT P_COLTXT IS INITIAL.
    V_COLTXT_LENGTH = STRLEN( P_COLTXT ).

    IF V_COLTXT_LENGTH > 20.
      WA_INFIELDCAT-DDICTXT = 'L'."Long text
      WA_INFIELDCAT-SELTEXT_L = P_COLTXT.
    ELSEIF V_COLTXT_LENGTH > 10.
      WA_INFIELDCAT-DDICTXT = 'M'."Medium Text
      WA_INFIELDCAT-SELTEXT_M = P_COLTXT.
    ELSE.
      WA_INFIELDCAT-DDICTXT = 'S'."Short Text
      WA_INFIELDCAT-SELTEXT_S = P_COLTXT.
    ENDIF.
    WA_INFIELDCAT-REPTEXT_DDIC = P_COLTXT  .
  ENDIF.
  APPEND WA_INFIELDCAT TO P_IT_FIELDCAT.
ENDFORM.                    " APPEND_FIELDCAT
*
***&---------------------------------------------------------------------*
***&      Form  CUSTOMER
***&---------------------------------------------------------------------*
*FORM get_customer USING    p_parnr TYPE  CRMS4D_PARTNER-PARTNER_ID
*                           p_adrnr TYPE kna1-adrnr
*                  CHANGING value(p_name_tha) TYPE c
*                           value(p_name_eng) TYPE c
*                           value(p_addr1) TYPE c
*                           value(p_addr2) TYPE c
*                           value(p_city2) TYPE adrc-city2
*                           value(p_city1) TYPE adrc-city1
*                           value(p_post_code1) TYPE adrc-post_code1
*                           value(p_tel_number) TYPE adrc-tel_number.
**                           value(p_brsch) TYPE kna1-brsch
**                           value(p_brtxt)  TYPE t016t-brtxt
**                           value(p_location) TYPE c.
*
*  DATA: lv_adrnr TYPE kna1-adrnr.
**  DATA: lv_brsch TYPE kna1-brsch,
**        lv_brtxt TYPE t016t-brtxt.
*
*
*  CLEAR:p_name_tha,p_name_eng,p_addr1,p_addr2,p_city2,
*        p_city1,p_post_code1,p_tel_number.
*
*
*  lv_adrnr =  p_adrnr.
*
*  IF p_adrnr EQ ''.
*    SELECT SINGLE adrnr
*      INTO (lv_adrnr)
*      FROM kna1
*    WHERE kunnr EQ p_parnr.
*
*  ENDIF.
*
*  SELECT addrnumber name1 name2 street str_suppl3 location
*         city2 city1 post_code1 tel_number nation
*  INTO TABLE gt_adrc
*  FROM adrc
*  WHERE addrnumber = lv_adrnr.
**            AND nation = 'I'.
*
*  IF p_parnr NE 'OT01'.
*    READ TABLE gt_adrc INTO gs_adrc WITH KEY addrnumber = lv_adrnr
*                                             nation = 'I'.
*    IF sy-subrc EQ 0.
*      CONCATENATE gs_adrc-name1 gs_adrc-name2 INTO p_name_eng.
*    ENDIF.
*    READ TABLE gt_adrc INTO gs_adrc WITH KEY addrnumber = lv_adrnr
*                                             nation = ''.
*    IF sy-subrc EQ 0.
*      CONCATENATE gs_adrc-name1 gs_adrc-name2 INTO p_name_tha.
*      p_addr1 = gs_adrc-street.
*      CONCATENATE gs_adrc-str_suppl3 gs_adrc-location INTO p_addr2.
**      p_addr2 = gs_adrc-str_suppl3.
*      p_city2 = gs_adrc-city2.
*      p_city1 = gs_adrc-city1.
*      p_post_code1 = gs_adrc-post_code1.
*      p_tel_number = gs_adrc-tel_number.
**      p_location = gs_adrc-location.
*    ENDIF.
*  ELSE.
*
*    READ TABLE gt_adrc INTO gs_adrc WITH KEY addrnumber = lv_adrnr
*                                             nation = ''.
*    IF sy-subrc EQ 0.
*      CONCATENATE gs_adrc-name1 gs_adrc-name2 INTO p_name_tha.
*      CONCATENATE gs_adrc-name1 gs_adrc-name2 INTO p_name_eng.
*      p_addr1 = gs_adrc-street.
*      CONCATENATE gs_adrc-str_suppl3 gs_adrc-location INTO p_addr2.
**      p_addr2 = gs_adrc-str_suppl3.
*      p_city2 = gs_adrc-city2.
*      p_city1 = gs_adrc-city1.
*      p_post_code1 = gs_adrc-post_code1.
*      p_tel_number = gs_adrc-tel_number.
**      p_location = gs_adrc-location.
*    ENDIF.
*  ENDIF.
*
**  CLEAR:lv_brsch ,lv_brtxt,p_brsch,p_brtxt.
**
**      SELECT SINGLE brsch
**      INTO lv_brsch
**      FROM kna1
**      WHERE kunnr EQ p_parnr.
**
**      IF NOT lv_brsch IS INITIAL.
**          p_brsch = lv_brsch.
**          SELECT SINGLE brtxt
**          INTO lv_brtxt
**          FROM t016t
**          WHERE brsch EQ lv_brsch
**            AND spras EQ 'E'.
**
**            p_brtxt = lv_brtxt.
**      ENDIF.
*
*
*
*
*ENDFORM. "

**&---------------------------------------------------------------------*
**&      Form  REPORT_HEADER
**&---------------------------------------------------------------------*
FORM report_header.
  DATA: lt_listhead  TYPE slis_t_listheader,
        lw_listline  TYPE slis_listheader.
  DATA: lv_text(256) TYPE c.
  DATA: lv_date(10)  TYPE c,
        lv_time(5)   TYPE c.

* execution date & time
  CLEAR: lv_date, lv_time.
  WRITE sy-datum TO lv_date USING EDIT MASK gc_mask_date.
  WRITE sy-uzeit TO lv_time USING EDIT MASK gc_mask_time.

  CLEAR lv_text.
  lv_text = 'Report PR PO GR for Service '.
  def_list_head 'H' '' lv_text.
  CLEAR lv_text.
  CONCATENATE 'DATE:' lv_date 'TIME:' lv_time INTO lv_text
    SEPARATED BY space.
  def_list_head 'S' '' lv_text.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_listhead
    EXCEPTIONS
      OTHERS             = 0.

ENDFORM.                    " REPORT_HEADER

*
**&---------------------------------------------------------------------*
**&      Form  F_CONVERT_DATA_IO
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
*FORM F_CONVERT_DATA_IO.
* DATA: LVAC_ASSIGNMENT TYPE CRMS4D_SERV_I-AC_ASSIGNMENT.
* DATA : r_AC_ASSIGNMENT_line LIKE LINE OF GAC_ASSIGNMENT.
* DATA : LV_AUFNR TYPE AUFK-AUFNR.
*
*  break 3sds006.
**  RANGES : GAC_ASSIGNMENT FOR CRMS4D_SERV_I-AC_ASSIGNMENT.
** s_IO FOR crms4d_serv_i-ac_assignment,
*  LOOP AT s_IO .
*    CLEAR: LV_AUFNR.
*    IF s_IO-option EQ 'BT'.
*
*       LV_AUFNR = s_IO-LOW.
*
*       CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*         EXPORTING
*           input         = LV_AUFNR
*        IMPORTING
*          OUTPUT        = LV_AUFNR
*                 .
*       r_AC_ASSIGNMENT_line-sign = s_IO-SIGN.
*       r_AC_ASSIGNMENT_line-option = s_IO-option.
*       r_AC_ASSIGNMENT_line-LOW = LV_AUFNR.
*
*       LV_AUFNR = S_IO-HIGH.
*
*       CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*         EXPORTING
*           input         = LV_AUFNR
*        IMPORTING
*          OUTPUT        = LV_AUFNR
*                 .
*       r_AC_ASSIGNMENT_line-HIGH = LV_AUFNR.
*
*
*       APPEND r_AC_ASSIGNMENT_line TO GAC_ASSIGNMENT.
*
*    ELSE.
*
*       LV_AUFNR = s_IO-LOW.
*       CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*         EXPORTING
*           input         = LV_AUFNR
*        IMPORTING
*          OUTPUT        = LV_AUFNR
*                 .
*
*       r_AC_ASSIGNMENT_line-sign = s_IO-SIGN.
*       r_AC_ASSIGNMENT_line-option = s_IO-option.
*       r_AC_ASSIGNMENT_line-LOW = LV_AUFNR.
*       APPEND r_AC_ASSIGNMENT_line TO GAC_ASSIGNMENT.
*
*    ENDIF.
*
*
*
*  ENDLOOP.
*
*  LOOP AT S_WBS.
*    r_AC_ASSIGNMENT_line = S_WBS.
**    r_AC_ASSIGNMENT_line-sign = 'I'.
**    r_AC_ASSIGNMENT_line-option = 'BT'.
**    r_AC_ASSIGNMENT_line-low = 'AA'.
**    r_AC_ASSIGNMENT_line-high = 'LH'.
*
*    APPEND r_AC_ASSIGNMENT_line TO GAC_ASSIGNMENT.
*  ENDLOOP.
*
*
*ENDFORM.                    " F_CHECK_ERROR

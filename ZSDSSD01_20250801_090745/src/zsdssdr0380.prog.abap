*&---------------------------------------------------------------------*
*& Report ZSDSSDR0380
*  Author             : Wantanee Prateep na thalang
*  Add-on ID          :
*  Description        : Report check status DO
*  Purpose            :
*  Copied from        : ZR_SD_STATUS_DO
*  Restriction        :
*&---------------------------------------------------------------------*
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
REPORT ZSDSSDR0380.
TYPE-POOLS : truxs,slis,icon.
*&---------------------------------------------------------------------*
*  DECLARATION
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*  TABLE
*&---------------------------------------------------------------------*
TABLES : ZSDSSDT001,kna1,likp,vbak.
*&---------------------------------------------------------------------*
*  TYPE
*&---------------------------------------------------------------------*
TYPES : BEGIN OF gy_result,
  lfdat   TYPE likp-lfdat,
  bezei_g TYPE tvgrt-bezei,
  bezei_o TYPE tvkbt-bezei,
  vtext   TYPE tvtwt-vtext,
  vtweg   TYPE vbak-vtweg,
  vkgrp   TYPE vbak-vkgrp,
  vkbur   TYPE vbak-vkbur,
  erzetd  TYPE likp-erzet,
  erdatd  TYPE likp-erdat,
  erzete  TYPE likp-erzet,
  erdate  TYPE likp-erdat,
  dosig_d TYPE sy-datum,
  podda_d TYPE sy-datum,
  ampmf_s TYPE ZSDSSDT001-ampmf,
  remap_s TYPE ZSDSSDT001-remap,
  reinv_s TYPE ZSDSSDT001-reinv,
  remar_r TYPE c LENGTH 255,
  remar_i TYPE c LENGTH 255,
  sds_shppv TYPE adrc-city1,
  sds_custname(100) TYPE c,
  sds_custcode TYPE vbpa-kunnr,
  sds_shipaddr(250) TYPE c,
  statu_u           TYPE c LENGTH 2.
INCLUDE TYPE ZSDSSDT001.
TYPES END OF gy_result.

TYPES : BEGIN OF gy_likp,
  vbeln   TYPE likp-vbeln,
  lfdat   TYPE likp-lfdat,
  bezei_g TYPE tvgrt-bezei,
  bezei_o TYPE tvkbt-bezei,
  vtext   TYPE tvtwt-vtext,
  vtweg   TYPE vbak-vtweg,
  vkgrp   TYPE vbak-vkgrp,
  vkbur   TYPE vbak-vkbur,
  erzet   TYPE likp-erzet,
  erdat   TYPE likp-erdat,
  vbeln_s TYPE vbap-vbeln,
  posnr   TYPE vbap-posnr,
  lstel   TYPE likp-lstel,
  vstel   TYPE likp-vstel,
END OF gy_likp.

TYPES : BEGIN OF gy_vbpa,
  vbeln      TYPE vbak-vbeln,
  kunnr      TYPE vbpa-kunnr,
  name1      TYPE adrc-name1,
  name2      TYPE adrc-name2,
  name3      TYPE adrc-name3,
  name4      TYPE adrc-name4,
  street     TYPE adrc-street,
  str_suppl1 TYPE adrc-str_suppl1,
  str_suppl2 TYPE adrc-str_suppl2,
  str_suppl3 TYPE adrc-str_suppl3,
  location   TYPE adrc-location,
  post_code1 TYPE adrc-post_code1,
  city1      TYPE adrc-city1,
END OF gy_vbpa.

TYPES : BEGIN OF gy_tvlat,
  vstel TYPE tvlat-vstel,
  lstel TYPE tvlat-lstel,
  vtext TYPE tvlat-vtext,
END OF gy_tvlat.

*&---------------------------------------------------------------------*
*  VARIABLE
*&---------------------------------------------------------------------*
DATA : gt_result TYPE TABLE OF gy_result,
       gs_result TYPE gy_result.

DATA : gt_ZSDSSDT002 TYPE TABLE OF ZSDSSDT002,
       gs_ZSDSSDT002 TYPE ZSDSSDT002.

DATA : gt_likp TYPE TABLE OF gy_likp,
       gs_likp TYPE gy_likp.

DATA : gt_vbpa TYPE TABLE OF gy_vbpa,
       gs_vbpa TYPE gy_vbpa.

DATA : gt_tvlat TYPE TABLE OF gy_tvlat,
       gs_tvlat TYPE gy_tvlat.

DATA : bdcdata LIKE bdcdata OCCURS 0 WITH HEADER LINE.

DATA : messtab     LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
       t_messtab   LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
       s_messtab   LIKE bdcmsgcoll.

DATA : dynpfields  LIKE dynpread OCCURS 5 WITH HEADER LINE.

DATA : gt_fcat   TYPE slis_t_fieldcat_alv,
       gs_layout TYPE slis_layout_alv,
       gt_sort TYPE slis_t_sortinfo_alv,
       gs_sort TYPE slis_sortinfo_alv.

*&---------------------------------------------------------------------*
*  RANGES
*&---------------------------------------------------------------------*
RANGES : gr_stat FOR jest-stat.
*&---------------------------------------------------------------------*
*  CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS : gc_true      TYPE c VALUE 'X'.
*&---------------------------------------------------------------------*
*  SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_lfart FOR likp-lfart DEFAULT 'ZD01',
                 s_donum FOR ZSDSSDT001-donum,
                 s_lfdat FOR likp-lfdat DEFAULT sy-datum,
                 s_erdat FOR likp-erdat DEFAULT sy-datum,   "CH3+
                 s_erzet FOR likp-erzet, "CH3+
                 s_vtweg FOR vbak-vtweg,
                 s_vkgrp FOR vbak-vkgrp,
                 s_vkbur FOR vbak-vkbur,
                 s_WMSDT FOR ZSDSSDT001-WMSDT,
                 s_stapc FOR ZSDSSDT001-stapc,
                 s_loadd FOR ZSDSSDT001-loadd,
                 s_contd FOR ZSDSSDT001-contd,
                 s_dealc FOR kna1-kunnr,
                 s_sernf FOR ZSDSSDT001-sernf NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK block1.

*&---------------------------------------------------------------------*
*  AT SELECTION-SCREEN OUTPUT
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.


*&---------------------------------------------------------------------*
*  AT SELECTION-SCREEN OUTPUT
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

*&---------------------------------------------------------------------*
*  START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM f_get_data.
*&---------------------------------------------------------------------*
*  END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  IF gt_result[] IS NOT INITIAL.
    PERFORM f_show_report.
  ELSE.
*    MESSAGE s003 DISPLAY LIKE 'E'.
  ENDIF.
*----------------------------------------------------------------------*
* CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS event_class DEFINITION.
*Handling double click
  PUBLIC SECTION.
    METHODS:
    handle_double_click
    FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.
ENDCLASS. "lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS event_class IMPLEMENTATION.
  METHOD handle_double_click.

  ENDMETHOD. "handle_double_click
ENDCLASS. "lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_get_data.

  IF s_WMSDT[] IS INITIAL AND
     s_stapc[] IS INITIAL AND
     s_loadd[] IS INITIAL AND
     s_contd[] IS INITIAL AND
     s_dealc[] IS INITIAL AND
     s_sernf[] IS INITIAL.
    PERFORM f_get_data_form_likp.
  ELSE.
    PERFORM f_get_data_form_status_do.
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
      i_callback_program = sy-repid
      i_callback_pf_status_set = 'PF_STATUS_1'
      i_callback_user_command  = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    =
      is_layout          = gs_layout
      it_fieldcat        = gt_fcat
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS  =
      it_sort            = gt_sort
*     IT_FILTER          =
*     IS_SEL_HIDE        =
      i_default          = 'X'
      i_save             = 'X'
*     IS_VARIANT         =
*     IT_EVENTS          =
*     IT_EVENT_EXIT      =
*     IS_PRINT           =
*     IS_REPREP_ID       =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  = 0
*     I_HTML_HEIGHT_END  = 0
*     IT_ALV_GRAPHICS    =
*     IT_HYPERLINK       =
*     IT_ADD_FIELDCAT    =
*     IT_EXCEPT_QINFO    =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab           = gt_result
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
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
  "gs_layout-box_fieldname     = 'SEL'.
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
  ls_fcat-fieldname   = 'DONUM'.
  ls_fcat-seltext_s   = 'Delivery Order No'.
  ls_fcat-seltext_m   = 'Delivery Order No'.
  ls_fcat-seltext_l   = 'Delivery Order No'.
  ls_fcat-fix_column  = 'X'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-ref_tabname = 'LIKP'.
  ls_fcat-fieldname   = 'LFDAT'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'DOWMS'.
  ls_fcat-seltext_s   = 'WMS Create Date'.
  ls_fcat-seltext_m   = 'WMS Create Date'.
  ls_fcat-seltext_l   = 'WMS Create Date'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'DOWMT'.
  ls_fcat-seltext_s   = 'WMS Create Time'.
  ls_fcat-seltext_m   = 'WMS Create Time'.
  ls_fcat-seltext_l   = 'WMS Create Time'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'STAPC'.
  ls_fcat-seltext_s   = 'Start Picking Date'.
  ls_fcat-seltext_m   = 'Start Picking Date'.
  ls_fcat-seltext_l   = 'Start Picking Date'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'STAPT'.
  ls_fcat-seltext_s   = 'Start Picking Time'.
  ls_fcat-seltext_m   = 'Start Picking Time'.
  ls_fcat-seltext_l   = 'Start Picking Time'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'LOADD'.
  ls_fcat-seltext_s   = 'ETD Warehouse'.
  ls_fcat-seltext_m   = 'ETD Warehouse'.
  ls_fcat-seltext_l   = 'ETD Warehouse'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'LOADT'.
  ls_fcat-seltext_s   = 'ETD Warehouse Time'.
  ls_fcat-seltext_m   = 'ETD Warehouse Time'.
  ls_fcat-seltext_l   = 'ETD Warehouse Time'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'CONTD'.
  ls_fcat-seltext_s   = 'Contract Date'.
  ls_fcat-seltext_m   = 'Contract Date'.
  ls_fcat-seltext_l   = 'Contract Date'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'CONTS'.
  ls_fcat-seltext_s   = 'Contract Time'.
  ls_fcat-seltext_m   = 'Contract Time'.
  ls_fcat-seltext_l   = 'Contract Time'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'CONTP'.
  ls_fcat-seltext_s   = 'Contract Person'.
  ls_fcat-seltext_m   = 'Contract Person'.
  ls_fcat-seltext_l   = 'Contract Person'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'CONTT'.
  ls_fcat-seltext_s   = 'Contract Details'.
  ls_fcat-seltext_m   = 'Contract Details'.
  ls_fcat-seltext_l   = 'Contract Details'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'DOSIG_D'.
  ls_fcat-seltext_s   = 'Dealer Received Date'.
  ls_fcat-seltext_m   = 'Dealer Received Date'.
  ls_fcat-seltext_l   = 'Dealer Received Date'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'PODDA_D'.
  ls_fcat-seltext_s   = 'WMS Received Document'.
  ls_fcat-seltext_m   = 'WMS Received Document'.
  ls_fcat-seltext_l   = 'WMS Received Document'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'PODRE'.
  ls_fcat-seltext_s   = 'Detail Document for send to SDS'.
  ls_fcat-seltext_m   = 'Detail Document for send to SDS'.
  ls_fcat-seltext_l   = 'Detail Document for send to SDS'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'DOSDS'.
  ls_fcat-seltext_s   = 'SDS Received Document Date'.
  ls_fcat-seltext_m   = 'SDS Received Document Date'.
  ls_fcat-seltext_l   = 'SDS Received Document Date'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'DOSDT'.
  ls_fcat-seltext_s   = 'SDS Received Document Time'.
  ls_fcat-seltext_m   = 'SDS Received Document Time'.
  ls_fcat-seltext_l   = 'SDS Received Document Time'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'DEALC'.
  ls_fcat-seltext_s   = 'Dealer code'.
  ls_fcat-seltext_m   = 'Dealer code'.
  ls_fcat-seltext_l   = 'Dealer code'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'DEALN'.
  ls_fcat-seltext_s   = 'Dealer name'.
  ls_fcat-seltext_m   = 'Dealer name'.
  ls_fcat-seltext_l   = 'Dealer name'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'REMAK'.
  ls_fcat-seltext_s   = 'Remark Sony'.
  ls_fcat-seltext_m   = 'Remark Sony'.
  ls_fcat-seltext_l   = 'Remark Sony'.
  APPEND ls_fcat TO gt_fcat.


  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'SHPAD'.
  ls_fcat-seltext_s   = 'Ship to address'.
  ls_fcat-seltext_m   = 'Ship to address'.
  ls_fcat-seltext_l   = 'Ship to address'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'SHPPV'.
  ls_fcat-seltext_s   = 'Ship to Province'.
  ls_fcat-seltext_m   = 'Ship to Province'.
  ls_fcat-seltext_l   = 'Ship to Province'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'AMPMF'.
  ls_fcat-seltext_s   = 'AM/PM'.
  ls_fcat-seltext_m   = 'AM/PM'.
  ls_fcat-seltext_l   = 'AM/PM'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'LOADP'.
  ls_fcat-seltext_s   = 'Loading point'.
  ls_fcat-seltext_m   = 'Loading point'.
  ls_fcat-seltext_l   = 'Loading point'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'REMAP'.
  ls_fcat-seltext_s   = 'Requier Map'.
  ls_fcat-seltext_m   = 'Requier Map'.
  ls_fcat-seltext_l   = 'Requier Map'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'REINV'.
  ls_fcat-seltext_s   = 'Require Invoice'.
  ls_fcat-seltext_m   = 'Require Invoice'.
  ls_fcat-seltext_l   = 'Require Invoice'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'AMPMF_S'.
  ls_fcat-seltext_s   = 'Text File SONY AM/PM'.
  ls_fcat-seltext_m   = 'Text File SONY AM/PM'.
  ls_fcat-seltext_l   = 'Text File SONY AM/PM'.
  ls_fcat-no_out      = 'X'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'REMAP_S'.
  ls_fcat-seltext_s   = 'Text File SONY Requier Map'.
  ls_fcat-seltext_m   = 'Text File SONY Requier Map'.
  ls_fcat-seltext_l   = 'Text File SONY Requier Map'.
  ls_fcat-no_out      = 'X'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'REINV_S'.
  ls_fcat-seltext_s   = 'Text File SONY Require Invoice'.
  ls_fcat-seltext_m   = 'Text File SONY Require Invoice'.
  ls_fcat-seltext_l   = 'Text File SONY Require Invoice'.
  ls_fcat-no_out      = 'X'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
*  ls_fcat-fieldname   = 'SERNF'.
  ls_fcat-fieldname   = 'STATU_U'.
  ls_fcat-seltext_s   = 'Upload Serial'.
  ls_fcat-seltext_m   = 'Upload Serial'.
  ls_fcat-seltext_l   = 'Upload Serial'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'BEZEI_G'.
  ls_fcat-seltext_s   = 'Sales Group'.
  ls_fcat-seltext_m   = 'Sales Group'.
  ls_fcat-seltext_l   = 'Sales Group'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'BEZEI_O'.
  ls_fcat-seltext_s   = 'Sales Office'.
  ls_fcat-seltext_m   = 'Sales Office'.
  ls_fcat-seltext_l   = 'Sales Office'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'VTEXT'.
  ls_fcat-seltext_s   = 'Dis.Channel'.
  ls_fcat-seltext_m   = 'Distribution Channel'.
  ls_fcat-seltext_l   = 'Distribution Channel'.
  APPEND ls_fcat TO gt_fcat.


  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'ERZETD'.
  ls_fcat-seltext_s   = 'DO Created Times'.
  ls_fcat-seltext_m   = 'DO Created Times'.
  ls_fcat-seltext_l   = 'DO Created Times'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'ERDATD'.
  ls_fcat-seltext_s   = 'DO Created Date'.
  ls_fcat-seltext_m   = 'DO Created Date'.
  ls_fcat-seltext_l   = 'DO Created Date'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'ERZETE'.
  ls_fcat-seltext_s   = 'Export Time'.
  ls_fcat-seltext_m   = 'Export Time'.
  ls_fcat-seltext_l   = 'Export Time'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'ERDATE'.
  ls_fcat-seltext_s   = 'Export Date'.
  ls_fcat-seltext_m   = 'Export Date'.
  ls_fcat-seltext_l   = 'Export Date'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'MAP'.
  ls_fcat-seltext_s   = 'Status Map'.
  ls_fcat-seltext_m   = 'Status Map'.
  ls_fcat-seltext_l   = 'Status Map'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'REMAR_R'.
  ls_fcat-seltext_s   = 'Request Remark'.
  ls_fcat-seltext_m   = 'Request Remark'.
  ls_fcat-seltext_l   = 'Request Remark'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'REMAR_I'.
  ls_fcat-seltext_s   = 'INV Remark'.
  ls_fcat-seltext_m   = 'INV Remark'.
  ls_fcat-seltext_l   = 'INV Remark'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'SDS_SHPPV'.
  ls_fcat-seltext_s   = 'SDS Ship to province'.
  ls_fcat-seltext_m   = 'SDS Ship to province'.
  ls_fcat-seltext_l   = 'SDS Ship to province'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'SDS_CUSTNAME'.
  ls_fcat-seltext_s   = 'SDS Dealer Name'.
  ls_fcat-seltext_m   = 'SDS Dealer Name'.
  ls_fcat-seltext_l   = 'SDS Dealer Name'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'SDS_CUSTCODE'.
  ls_fcat-seltext_s   = 'SDS Dealer Code'.
  ls_fcat-seltext_m   = 'SDS Dealer Code'.
  ls_fcat-seltext_l   = 'SDS Dealer Code'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'SDS_SHIPADDR'.
  ls_fcat-seltext_s   = 'SDS Ship to Address'.
  ls_fcat-seltext_m   = 'SDS Ship to Address'.
  ls_fcat-seltext_l   = 'SDS Ship to Address'.
  APPEND ls_fcat TO gt_fcat.

  " CH2 Add by WAntanee 20211201
  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'ZDO_TIME'.
  ls_fcat-seltext_s   = 'DO Sign Time'.
  ls_fcat-seltext_m   = 'DO Sign Time'.
  ls_fcat-seltext_l   = 'DO Sign Time'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'ZDIVER_NAME'.
  ls_fcat-seltext_s   = 'Diver Name'.
  ls_fcat-seltext_m   = 'Diver Name'.
  ls_fcat-seltext_l   = 'Diver Name'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'ZPLATE_NO'.
  ls_fcat-seltext_s   = 'Plate No'.
  ls_fcat-seltext_m   = 'Plate No'.
  ls_fcat-seltext_l   = 'Plate No'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'ZCARRIER_NAME'.
  ls_fcat-seltext_s   = 'Carrier Name'.
  ls_fcat-seltext_m   = 'Carrier Name'.
  ls_fcat-seltext_l   = 'Carrier Name'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'ZTRUCK_TYPE'.
  ls_fcat-seltext_s   = 'Truck Type'.
  ls_fcat-seltext_m   = 'Truck Type'.
  ls_fcat-seltext_l   = 'Truck Type'.
  APPEND ls_fcat TO gt_fcat.


  CLEAR ls_fcat.
  ls_fcat-fieldname   = 'ZSHIPMENT_NO'.
  ls_fcat-seltext_s   = 'Shipment No'.
  ls_fcat-seltext_m   = 'Shipment No'.
  ls_fcat-seltext_l   = 'Shipment No'.
  APPEND ls_fcat TO gt_fcat.
  " CH2 Add by WAntanee 20211201

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
*  CLEAR gs_sort.
*  gs_sort-fieldname = 'LIFNR'.
*  gs_sort-spos = '1'.
*  gs_sort-up = 'X'.
**  gs_sort-subtot = 'X'.
*  APPEND gs_sort TO gt_sort.
ENDFORM.                    " F_SORT
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->US_EXTAB   text
*----------------------------------------------------------------------*
FORM pf_status_1 USING us_extab TYPE slis_t_extab.

  SET PF-STATUS 'ZSTANDARD' EXCLUDING us_extab.

ENDFORM.                    "PF_STATUS_1
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM    text
*      -->P_SELFLD   text
*----------------------------------------------------------------------*
FORM user_command USING p_ucomm TYPE sy-ucomm
                        p_selfld TYPE slis_selfield.
*&---------------------------------------------------------------------*
*&for Check = 'X' when tick Check Box
*&---------------------------------------------------------------------*
  DATA : ref_grid TYPE REF TO cl_gui_alv_grid.

  IF ref_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = ref_grid.
    CALL METHOD ref_grid->check_changed_data.
  ENDIF.
*&---------------------------------------------------------------------*

  CASE p_ucomm.
    WHEN '&IC1'.
      PERFORM f_call_transection.
    WHEN 'REFRESH'.
      PERFORM f_clear_data.
      PERFORM f_get_data.
      CALL METHOD ref_grid->refresh_table_display.
      CLEAR : ref_grid.
  ENDCASE.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  F_CALL_TRANSECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_call_transection.
  DATA: le_row     TYPE i,
        le_value   TYPE c,
        le_col     TYPE i,
        les_row_id TYPE lvc_s_row,
        les_col_id TYPE lvc_s_col,
        les_row_no TYPE lvc_s_roid.

  DATA : ref_grid TYPE REF TO cl_gui_alv_grid.
  IF ref_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = ref_grid.
  ENDIF.

  CALL METHOD ref_grid->get_current_cell
    IMPORTING
      e_row     = le_row
      e_value   = le_value
      e_col     = le_col
      es_row_id = les_row_id
      es_col_id = les_col_id.

  CLEAR : bdcdata[],messtab[].

  READ TABLE gt_result INTO gs_result INDEX les_row_id-index.
  IF sy-subrc = 0.
    PERFORM bdc_dynpro   USING  'SAPMV50A'   '4004'.
    PERFORM bdc_field    USING  'BDC_CURSOR' 'LIKP-VBELN'.
    PERFORM bdc_field    USING  'BDC_OKCODE' '/00'.
    PERFORM bdc_field    USING  'LIKP-VBELN' gs_result-donum.

    PERFORM bdc_transaction USING 'VL03N'.
  ENDIF.

ENDFORM.                    " F_CALL_TRANSECTION
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "bdc_field
*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TCODE      text
*----------------------------------------------------------------------*
FORM bdc_transaction  USING    tcode.
  DATA: l_mstring(480).
  DATA: l_subrc LIKE sy-subrc.
  DATA: lv_opt  TYPE ctu_params.

  lv_opt-dismode  = 'E'."'A'
  lv_opt-updmode  = 'L'.
  lv_opt-nobinpt  = 'X'.
  "lv_opt-cattmode = 'A'.
  "lv_opt-defsize  = 'X'.

  CALL TRANSACTION tcode USING bdcdata
*                   MODE   'E'"N
*                   UPDATE 'L'
                   OPTIONS FROM lv_opt
                   MESSAGES INTO messtab.

ENDFORM.                    "bdc_transaction
*&---------------------------------------------------------------------*
*&      Form  F_INSERT_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_insert_result .

  DATA lv_tabix TYPE sy-tabix.

  DATA : BEGIN OF ls_vbuk,
    vbeln TYPE vbuk-vbeln,
  END OF ls_vbuk.
  DATA lt_vbuk LIKE TABLE OF ls_vbuk.

  IF gt_likp[] IS NOT INITIAL.
    PERFORM f_get_ship_to_addr.
    PERFORM f_get_status_send_data.
    PERFORM f_get_shiping_point.

    SELECT vbelv
      FROM vbfa
      INTO TABLE lt_vbuk
      FOR ALL ENTRIES IN gt_likp
      WHERE vbelv   EQ gt_likp-vbeln
        AND posnv   EQ space
        AND vbtyp_n EQ '8'.

  ENDIF.

  LOOP AT gt_likp INTO gs_likp.
*    READ TABLE gt_vbpa INTO gs_vbpa
*    WITH KEY vbeln = gs_likp-vbeln_s.
*    IF sy-subrc = 0.
*      gs_result-dealc = gs_vbpa-kunnr.
*      CONCATENATE gs_vbpa-name1 gs_vbpa-name2 gs_vbpa-name3 gs_vbpa-name4 INTO gs_result-dealn SEPARATED BY space.
*      CONCATENATE gs_vbpa-street gs_vbpa-str_suppl1 gs_vbpa-str_suppl2
*                  gs_vbpa-str_suppl3 gs_vbpa-location gs_vbpa-post_code1 INTO gs_result-shpad SEPARATED BY space.
*
*      gs_result-shppv = gs_vbpa-city1.
*    ENDIF.



    READ TABLE gt_result INTO gs_result
    WITH KEY donum = gs_likp-vbeln.
    IF sy-subrc = 0.
      lv_tabix = sy-tabix.

      READ TABLE gt_vbpa INTO gs_vbpa
      WITH KEY vbeln = gs_likp-vbeln_s.

      IF sy-subrc = 0.
        gs_result-sds_custcode = gs_vbpa-kunnr.
        CONCATENATE gs_vbpa-name1 gs_vbpa-name2 gs_vbpa-name3 gs_vbpa-name4 INTO gs_result-sds_custname SEPARATED BY space.
        CONCATENATE gs_vbpa-street gs_vbpa-str_suppl1 gs_vbpa-str_suppl2
                    gs_vbpa-str_suppl3 gs_vbpa-location gs_vbpa-post_code1 INTO gs_result-sds_shipaddr SEPARATED BY space.

        gs_result-sds_shppv = gs_vbpa-city1.
      ENDIF.


      PERFORM f_read_text.
      PERFORM f_read_text_inv.
      READ TABLE gt_ZSDSSDT002 INTO gs_ZSDSSDT002
      WITH KEY vbeln = gs_likp-vbeln.
      IF sy-subrc = 0.
        gs_result-erzete = gs_ZSDSSDT002-erzet.
        gs_result-erdate = gs_ZSDSSDT002-erdat.
        gs_result-ampmf  = gs_ZSDSSDT002-am_pm.
        gs_result-remap  = gs_ZSDSSDT002-req_map.
        gs_result-reinv  = gs_ZSDSSDT002-req_inv.
      ENDIF.
      gs_result-ampmf_s = gs_result-ampmf.
      gs_result-remap_s = gs_result-remap.
      gs_result-reinv_s = gs_result-reinv.
      gs_result-dosig_d = gs_result-dosig.
      gs_result-podda_d = gs_result-podda+0(8).
      gs_result-lfdat   = gs_likp-lfdat.
      gs_result-bezei_g = gs_likp-bezei_g.
      gs_result-bezei_o = gs_likp-bezei_o.
      gs_result-vtext   = gs_likp-vtext.
      gs_result-vtweg   = gs_likp-vtweg.
      gs_result-vkgrp   = gs_likp-vkgrp.
      gs_result-vkbur   = gs_likp-vkbur.
      gs_result-erzetd  = gs_likp-erzet.
      gs_result-erdatd  = gs_likp-erdat.

      READ TABLE gt_tvlat INTO gs_tvlat
      WITH KEY vstel = gs_likp-vstel
               lstel = gs_likp-lstel.
      IF sy-subrc = 0.
        gs_result-loadp = gs_tvlat-vtext.
      ENDIF.

      READ TABLE lt_vbuk INTO ls_vbuk
      WITH KEY vbeln = gs_likp-vbeln.
      IF sy-subrc = 0.
        CONCATENATE gs_result-sernf 'S' INTO gs_result-statu_u.
      ELSE.
        gs_result-statu_u = gs_result-sernf.
      ENDIF.

      MODIFY gt_result FROM gs_result INDEX lv_tabix.
    ELSE.
      READ TABLE gt_vbpa INTO gs_vbpa
      WITH KEY vbeln = gs_likp-vbeln_s.

      IF sy-subrc = 0.
        gs_result-sds_custcode = gs_vbpa-kunnr.
        CONCATENATE gs_vbpa-name1 gs_vbpa-name2 gs_vbpa-name3 gs_vbpa-name4 INTO gs_result-sds_custname SEPARATED BY space.
        CONCATENATE gs_vbpa-street gs_vbpa-str_suppl1 gs_vbpa-str_suppl2
                    gs_vbpa-str_suppl3 gs_vbpa-location gs_vbpa-post_code1 INTO gs_result-sds_shipaddr SEPARATED BY space.

        gs_result-sds_shppv = gs_vbpa-city1.
      ENDIF.
      PERFORM f_read_text.
      PERFORM f_read_text_inv.

      READ TABLE gt_tvlat INTO gs_tvlat
      WITH KEY vstel = gs_likp-vstel
               lstel = gs_likp-lstel.
      IF sy-subrc = 0.
        gs_result-loadp = gs_tvlat-vtext.
      ENDIF.

      READ TABLE gt_ZSDSSDT002 INTO gs_ZSDSSDT002
      WITH KEY vbeln = gs_likp-vbeln.
      IF sy-subrc = 0.
        gs_result-erzete = gs_ZSDSSDT002-erzet.
        gs_result-erdate = gs_ZSDSSDT002-erdat.
        gs_result-ampmf  = gs_ZSDSSDT002-am_pm.
        gs_result-remap  = gs_ZSDSSDT002-req_map.
        gs_result-reinv  = gs_ZSDSSDT002-req_inv.
      ENDIF.

      gs_result-donum   = gs_likp-vbeln.
      gs_result-lfdat   = gs_likp-lfdat.
      gs_result-bezei_g = gs_likp-bezei_g.
      gs_result-bezei_o = gs_likp-bezei_o.
      gs_result-vtext   = gs_likp-vtext.
      gs_result-vtweg   = gs_likp-vtweg.
      gs_result-vkgrp   = gs_likp-vkgrp.
      gs_result-vkbur   = gs_likp-vkbur.
      gs_result-erzetd  = gs_likp-erzet.
      gs_result-erdatd  = gs_likp-erdat.

      READ TABLE lt_vbuk INTO ls_vbuk
      WITH KEY vbeln = gs_likp-vbeln.
      IF sy-subrc = 0.
        CONCATENATE gs_result-sernf 'S' INTO gs_result-statu_u.
      ELSE.
        gs_result-statu_u = gs_result-sernf.
      ENDIF.

      APPEND gs_result TO gt_result.
    ENDIF.


    CLEAR : gs_likp,gs_result,gs_ZSDSSDT002,gs_tvlat.
  ENDLOOP.
ENDFORM.                    " F_INSERT_RESULT
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_FORM_LIKP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data_form_likp .
  SELECT likp~vbeln
         likp~lfdat
         tvgrt~bezei
         tvkbt~bezei
         tvtwt~vtext
         vbak~vtweg
         vbak~vkgrp
         vbak~vkbur
         likp~erzet
         likp~erdat
         vbap~vbeln
         vbap~posnr
         likp~lstel
         likp~vstel
    FROM likp
    INNER JOIN lips   ON likp~vbeln EQ lips~vbeln
    INNER JOIN vbap   ON lips~vgbel EQ vbap~vbeln AND
                         lips~vgpos EQ vbap~posnr
    INNER JOIN vbak   ON vbap~vbeln EQ vbak~vbeln
    INNER JOIN tvgrt  ON ( vbak~vkgrp  EQ tvgrt~vkgrp AND "get sale group description
                           tvgrt~spras EQ sy-langu )
    INNER JOIN tvkbt  ON ( vbak~vkbur  EQ tvkbt~vkbur AND "get sale office description
                           tvkbt~spras EQ sy-langu )
    INNER JOIN tvtwt  ON ( vbak~vtweg  EQ tvtwt~vtweg  AND "Distibution channel
                           tvtwt~spras EQ sy-langu )
    INTO TABLE gt_likp
    WHERE likp~vbeln IN s_donum
      AND likp~lfdat IN s_lfdat
      AND likp~erdat IN s_erdat                             "CH3+
      AND likp~erzet IN s_erzet                             "CH3+
      AND vbak~vtweg IN s_vtweg
      AND vbak~vkgrp IN s_vkgrp
      AND vbak~vkbur IN s_vkbur
      AND likp~lfart IN s_lfart.

  IF gt_likp[] IS NOT INITIAL.
    SELECT *
      FROM ZSDSSDT001
      INTO CORRESPONDING FIELDS OF TABLE gt_result
      FOR ALL ENTRIES IN gt_likp
      WHERE donum EQ gt_likp-vbeln.

*    PERFORM f_get_ship_to_addr.
    PERFORM f_insert_result.
  ENDIF.
ENDFORM.                    " F_GET_DATA_FORM_LIKP
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_FORM_STATUS_DO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data_form_status_do .
  SELECT *
      FROM ZSDSSDT001
      INTO CORRESPONDING FIELDS OF TABLE gt_result
      WHERE donum IN s_donum
        AND WMSDT IN s_WMSDT
        AND stapc IN s_stapc
        AND loadd IN s_loadd
        AND contd IN s_contd
        AND dealc IN s_dealc
        AND sernf IN s_sernf.

  IF gt_result[] IS NOT INITIAL.
    SELECT likp~vbeln
         likp~lfdat
         tvgrt~bezei
         tvkbt~bezei
         tvtwt~vtext
         vbak~vtweg
         vbak~vkgrp
         vbak~vkbur
         likp~erzet
         likp~erdat
         vbap~vbeln
         vbap~posnr
         likp~lstel
         likp~vstel
    FROM likp
    INNER JOIN lips   ON likp~vbeln EQ lips~vbeln
    INNER JOIN vbap   ON lips~vgbel EQ vbap~vbeln AND
                         lips~vgpos EQ vbap~posnr
    INNER JOIN vbak   ON vbap~vbeln EQ vbak~vbeln
    INNER JOIN tvgrt  ON ( vbak~vkgrp  EQ tvgrt~vkgrp AND "get sale group description
                           tvgrt~spras EQ sy-langu )
    INNER JOIN tvkbt  ON ( vbak~vkbur  EQ tvkbt~vkbur AND "get sale office description
                           tvkbt~spras EQ sy-langu )
    INNER JOIN tvtwt  ON ( vbak~vtweg  EQ tvtwt~vtweg  AND "Distibution channel
                           tvtwt~spras EQ sy-langu )
    INTO TABLE gt_likp
    FOR ALL ENTRIES IN gt_result
    WHERE likp~vbeln EQ gt_result-donum
      AND likp~lfdat IN s_lfdat
      AND likp~erdat IN s_erdat                             "CH3+
      AND likp~erzet IN s_erzet                             "CH3+
      AND vbak~vtweg IN s_vtweg
      AND vbak~vkgrp IN s_vkgrp
      AND vbak~vkbur IN s_vkbur
      AND likp~lfart IN s_lfart.

    PERFORM f_insert_result.
  ENDIF.

ENDFORM.                    " F_GET_DATA_FORM_STATUS_DO
*&---------------------------------------------------------------------*
*&      Form  F_GET_SHIP_TO_ADDR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_ship_to_addr.
  SELECT vbpa~vbeln
         vbpa~kunnr
         adrc~name1
         adrc~name2
         adrc~name3
         adrc~name4
         adrc~street
         adrc~str_suppl1
         adrc~str_suppl2
         adrc~str_suppl3
         adrc~location
         adrc~post_code1
         adrc~city1
    FROM vbpa
    INNER JOIN adrc ON vbpa~adrnr EQ adrc~addrnumber
    INTO TABLE gt_vbpa
    FOR ALL ENTRIES IN gt_likp
    WHERE vbeln EQ gt_likp-vbeln_s
      AND parvw EQ 'WE'
      AND posnr EQ space.

ENDFORM.                    " F_GET_SHIP_TO_ADDR
*&---------------------------------------------------------------------*
*&      Form  F_READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_read_text .
  DATA : lv_id       TYPE thead-tdid,
         lv_language TYPE thead-tdspras,
         lv_name     TYPE thead-tdname,
         lv_object   TYPE thead-tdobject,
         lv_text     TYPE string.

  DATA : lt_lines TYPE TABLE OF tline,
         ls_lines TYPE tline.

  CONSTANTS : BEGIN OF lc_read_text,
    id TYPE c LENGTH 4 VALUE 'Z012',
    ob TYPE c LENGTH 4 VALUE 'VBBK',
  END OF lc_read_text.
*--------------------------------------------------------------------*
  lv_id       = lc_read_text-id.
  lv_language = sy-langu.
  lv_name     = gs_likp-vbeln_s.
  lv_object   = lc_read_text-ob.
*--------------------------------------------------------------------*
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = lv_id
      language                = lv_language
      name                    = lv_name
      object                  = lv_object
    TABLES
      lines                   = lt_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR lv_text.
  IF lt_lines[] IS NOT INITIAL.
    LOOP AT lt_lines INTO ls_lines.
      IF lv_text IS NOT INITIAL.
        CONCATENATE lv_text ls_lines-tdline INTO lv_text.
      ELSE.
        lv_text = ls_lines-tdline.
      ENDIF.
    ENDLOOP.
    gs_result-remar_r = lv_text.
  ENDIF.

ENDFORM.                    " F_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  F_CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_clear_data .
  CLEAR : gt_result,gt_likp,gt_vbpa,gt_ZSDSSDT002,gt_tvlat.
ENDFORM.                    " F_CLEAR_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_STATUS_SEND_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_status_send_data .

  SELECT *
    FROM ZSDSSDT002
    INTO TABLE gt_ZSDSSDT002
    FOR ALL ENTRIES IN gt_likp
    WHERE vbeln EQ gt_likp-vbeln.

  SORT gt_ZSDSSDT002 BY run_id DESCENDING.

ENDFORM.                    " F_GET_STATUS_SEND_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_SHIPING_POINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_shiping_point.
  SELECT vstel
         lstel
         vtext
    FROM tvlat
    INTO TABLE gt_tvlat
    FOR ALL ENTRIES IN gt_likp
    WHERE spras EQ sy-langu
      AND vstel EQ gt_likp-vstel
      AND lstel EQ gt_likp-lstel.
ENDFORM.                    " F_GET_SHIPING_POINT
*&---------------------------------------------------------------------*
*&      Form  F_READ_TEXT_INV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_read_text_inv .
  DATA : lv_id       TYPE thead-tdid,
         lv_language TYPE thead-tdspras,
         lv_name     TYPE thead-tdname,
         lv_object   TYPE thead-tdobject,
         lv_text     TYPE string.

  DATA : lt_lines TYPE TABLE OF tline,
         ls_lines TYPE tline.

  CONSTANTS : BEGIN OF lc_read_text,
    id TYPE c LENGTH 4 VALUE 'Z011',
    ob TYPE c LENGTH 4 VALUE 'VBBK',
  END OF lc_read_text.
*--------------------------------------------------------------------*
  lv_id       = lc_read_text-id.
  lv_language = sy-langu.
  lv_name     = gs_likp-vbeln_s.
  lv_object   = lc_read_text-ob.
*--------------------------------------------------------------------*
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = lv_id
      language                = lv_language
      name                    = lv_name
      object                  = lv_object
    TABLES
      lines                   = lt_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR lv_text.
  IF lt_lines[] IS NOT INITIAL.
    LOOP AT lt_lines INTO ls_lines.
      IF lv_text IS NOT INITIAL.
        CONCATENATE lv_text ls_lines-tdline INTO lv_text.
      ELSE.
        lv_text = ls_lines-tdline.
      ENDIF.
    ENDLOOP.
    gs_result-remar_i = lv_text.
  ENDIF.

ENDFORM.                    " F_READ_TEXT_INV

*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0180_S01
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------*
*& S E L E C T I O N S C R E E N D A T A
*& - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*& select-options (s_)
*& parameters (p_)
*& radio buttons (rb_)
*& checkboxes (cb_)
*& pushbuttons (pb_)
*&--------------------------------------------------------------------*
*Module
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
PARAMETERS : cb_sd AS CHECKBOX DEFAULT 'X',
             cb_fi AS CHECKBOX DEFAULT 'X' MODIF ID m01.
SELECTION-SCREEN END OF BLOCK b01.
*Program Option
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rb_trn RADIOBUTTON GROUP r01 DEFAULT 'X' USER-COMMAND uc1.
SELECTION-SCREEN COMMENT 3(54) TEXT-s03.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rb_rej RADIOBUTTON GROUP r01.
SELECTION-SCREEN COMMENT 3(54) TEXT-s02.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b02.
*General Data Selection
SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-b03.
PARAMETERS:     p_bukrs TYPE t001-bukrs OBLIGATORY.
PARAMETERS:     p_gjahr TYPE bkpf-gjahr. "OBLIGATORY. CHG CH01
SELECT-OPTIONS: s_bldat FOR  bkpf-bldat.
SELECT-OPTIONS: s_kunnr FOR  bseg-kunnr.
SELECT-OPTIONS: s_bupla FOR  bseg-bupla.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : cb_cancl AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(54) TEXT-s01.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b03.
*Accounting Data Selection
SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE TEXT-b04.
SELECT-OPTIONS : s_blart FOR bkpf-blart,
                 s_belnr FOR bkpf-belnr.
SELECTION-SCREEN END OF BLOCK b04.
*Sales Data Selection
SELECTION-SCREEN BEGIN OF BLOCK b05 WITH FRAME TITLE TEXT-b05.
SELECT-OPTIONS : s_vkorg FOR vbrk-vkorg MODIF ID m01,
                 s_vtweg FOR vbrk-vtweg MODIF ID m01,
                 s_fkart FOR vbrk-fkart MODIF ID m01,
                 s_vbeln FOR vbrk-vbeln MODIF ID m01.
SELECTION-SCREEN END OF BLOCK b05.
*Creation Information Selection
SELECTION-SCREEN BEGIN OF BLOCK b06 WITH FRAME TITLE TEXT-b06.
SELECT-OPTIONS: s_usnam FOR bkpf-usnam,
                s_cpudt FOR bkpf-cpudt,
                s_cputm FOR bkpf-cputm.
SELECTION-SCREEN END OF BLOCK b06.
*Document Group
SELECTION-SCREEN BEGIN OF BLOCK b07 WITH FRAME TITLE TEXT-b07.
PARAMETERS: cb_inv AS CHECKBOX TYPE c DEFAULT 'X' MODIF ID m01,
            cb_rct AS CHECKBOX TYPE c DEFAULT 'X',
            cb_tiv AS CHECKBOX TYPE c DEFAULT 'X',
            cb_dcn AS CHECKBOX TYPE c DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b07.
*Document Status
SELECTION-SCREEN BEGIN OF BLOCK b08 WITH FRAME TITLE TEXT-b08.
PARAMETERS : p_new  TYPE char1 AS CHECKBOX MODIF ID m02 DEFAULT 'X',
             p_tf   TYPE char1 AS CHECKBOX MODIF ID m02 DEFAULT 'X'.
">>>>>> BEGIN OF INSERTION: <CH12> 30.11.2020 10:19:47 >>>>>>
SELECTION-SCREEN BEGIN OF LINE .
  POSITION 1.
  PARAMETERS  p_obslt TYPE char1 AS CHECKBOX MODIF ID m02 DEFAULT 'X'.
  POSITION 3.
  SELECTION-SCREEN COMMENT 3(60) text-obs.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b08.
"<<<<<< END OF INSERTION: <CH12> 30.11.2020 10:19:47  <<<<<<
*Layout Control
SELECTION-SCREEN BEGIN OF BLOCK b09 WITH FRAME TITLE TEXT-b09.
PARAMETERS: p_vari TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b09.

SELECTION-SCREEN BEGIN OF BLOCK b10 WITH FRAME TITLE TEXT-b10.
PARAMETERS: p_server TYPE ZSDSFIS028-sap_dir_in NO-DISPLAY. "zeetax_server_path NO-DISPLAY.
PARAMETERS: p_ziplin TYPE i DEFAULT '7' NO-DISPLAY.  "จำนวนไฟล์ที่อยู่ใน .ZIP
PARAMETERS: p_logd   AS CHECKBOX.
PARAMETERS: p_pc_dir TYPE string MODIF ID m03.
SELECTION-SCREEN END OF BLOCK b10.

*----------------------------------------------------------------------*
*   AT SELECTION SCREEN ON VALUE                                       *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pc_dir.
  PERFORM open_dir CHANGING p_pc_dir.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN OUTPUT
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM init_screen.
  PERFORM modify_screen .

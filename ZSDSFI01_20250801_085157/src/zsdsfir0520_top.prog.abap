*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0520_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0520_TOP
*  Creation Date      : 11.11.2024
*  Author             : Apichat Ch(Eviden)
*  Add-on ID          : N/A
*  Description        : Program Advance Received Report
*                       RICEP ZFIAR036
*  Purpose            :
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
* TABLES
*----------------------------------------------------------------------*
TABLES: zsdsfit045,
        bseg.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_t012,
    bukrs TYPE t012-bukrs,
    hbkid TYPE t012-hbkid,
    hktid TYPE t012t-hktid,
    bankl TYPE t012-bankl,
    text1 TYPE t012t-text1,
  END OF gty_t012,

  BEGIN OF gty_document,
    bukrs     TYPE bkpf-bukrs,
    belnr     TYPE bkpf-belnr,
    gjahr     TYPE bkpf-gjahr,
    buzei     type bseg-buzei,  "<<F36K914879 - 03 ins
    usnam     TYPE bkpf-usnam,
    cpudt     TYPE bkpf-cpudt,
    cputm     TYPE bkpf-cputm,
    bldat     TYPE bkpf-bldat,
    budat     TYPE bkpf-budat,
    bktxt     TYPE bkpf-bktxt,
    xblnr     TYPE bkpf-xblnr,
    waers     TYPE bkpf-waers,
    aedat     TYPE bkpf-aedat,
    zuonr     TYPE bseg-zuonr,
    umskz     TYPE bseg-umskz,
    sgtxt     TYPE bseg-sgtxt,
    kunnr     TYPE bseg-kunnr,
    cust_name TYPE zsdsde_cust_name,
*    wrbtr     TYPE bseg-wrbtr, "<<F36K914879 - 03 del
    wrbtr     TYPE ZSDSDE_AMOUNT, "<<F36K914879 - 03 ins
    tranf_no  TYPE zsdsde_transfer_no,
*    xref2     TYPE bseg-xref2,
    zfbdt     TYPE bseg-zfbdt,
    augdt     TYPE bseg-augdt,
    augbl     TYPE bseg-augbl,
    auggj     TYPE bseg-auggj,
*<<F36K914879 - 01 Begin of ins
    rebzg     TYPE bseg-rebzg,
    rebzj     TYPE bseg-rebzj,
    rebzz     TYPE bseg-rebzz,
*<<F36K914879 - 01 End of ins
  END OF gty_document,

  BEGIN OF gty_doc_item,
    bukrs TYPE bseg-bukrs,
    belnr TYPE bseg-belnr,
    gjahr TYPE bseg-gjahr,
    buzei TYPE bseg-buzei,
    hkont TYPE bseg-hkont,
    dmbtr TYPE bseg-dmbtr,
  END OF gty_doc_item,

  BEGIN OF gty_do,
    vbeln TYPE vbrp-vbeln,
    vgbel TYPE vbrp-vgbel,
  END OF gty_do,

  BEGIN OF gty_billing_data,
    vbeln TYPE vbrk-vbeln,
    fkdat TYPE vbrk-fkdat,
  END OF gty_billing_data,

  BEGIN OF gty_do_data,
    vbeln TYPE likp-vbeln,
    lfdat TYPE likp-lfdat,
  END OF gty_do_data,

* <<F36K910492 start ins
  BEGIN OF gty_cdhdr,
    objectclas TYPE cdhdr-objectclas,
    objectid   TYPE cdhdr-objectid,
    changenr   TYPE cdhdr-changenr,
    username   TYPE cdhdr-username,
    udate      TYPE cdhdr-udate,
    utime      TYPE cdhdr-utime,
    tcode      TYPE cdhdr-tcode,
  END OF gty_cdhdr,
  tt_cdhdr       TYPE STANDARD TABLE OF gty_cdhdr      WITH EMPTY KEY,
* <<F36K910492 end ins

  tt_t012        TYPE STANDARD TABLE OF gty_t012,
  tt_document    TYPE STANDARD TABLE OF gty_document      WITH EMPTY KEY,
  tt_doc_item    TYPE STANDARD TABLE OF gty_doc_item,
  tt_do          TYPE STANDARD TABLE OF gty_do            WITH EMPTY KEY,
  tt_bliing_data TYPE STANDARD TABLE OF gty_billing_data  WITH EMPTY KEY,
  tt_do_data     TYPE STANDARD TABLE OF gty_do_data       WITH EMPTY KEY,
  tt_order       TYPE STANDARD TABLE OF zsdssdt015        WITH EMPTY KEY,
  tt_advance     TYPE STANDARD TABLE OF zsdsfit045        WITH EMPTY KEY,
  tt_output      TYPE STANDARD TABLE OF zsdsfis179        WITH EMPTY KEY.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:

  BEGIN OF gc_genc_param,
    doc_type TYPE zsdsde_param_name VALUE 'BLART',
    sp_gl    TYPE zsdsde_param_name VALUE 'UMSKZ',
    gl_acct  TYPE zsdsde_param_name VALUE 'HKONT',
  END OF gc_genc_param,

  gc_structure     TYPE  tabname    VALUE 'ZSDSFIS179',
  gc_header_height TYPE  i          VALUE 16,
  gc_alv_height    TYPE  i          VALUE 90,

  gc_true          TYPE  char1      VALUE 'X'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  gt_genc_gl      TYPE zcl_sdsca_utilities=>tt_gen_c              ##NEEDED,

  gt_cdhdr        TYPE tt_cdhdr                               ##NEEDED, "<<F36K910492++
  gt_t012         TYPE tt_t012                                ##NEEDED,
  gt_document     TYPE tt_document                            ##NEEDED,
  gt_doc_item     TYPE tt_doc_item                            ##NEEDED,
  gt_do           TYPE tt_do                                  ##NEEDED,
  gt_billing_data TYPE tt_bliing_data                         ##NEEDED,
  gt_do_data      TYPE tt_do_data                             ##NEEDED,
  gt_order        TYPE tt_order                                   ##NEEDED,
  gt_advance      TYPE tt_advance                                 ##NEEDED,
  gt_output       TYPE tt_output                                  ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA:
      gf_path TYPE char255                 ##NEEDED.

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*
DATA:
  gr_blart TYPE RANGE OF bkpf-blart       ##NEEDED,
  gr_umskz TYPE RANGE OF bseg-umskz       ##NEEDED,
  gr_gl    TYPE RANGE OF bseg-hkont       ##NEEDED.


*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*
FIELD-SYMBOLS:
  <g_list> TYPE STANDARD TABLE ##FS_ASSIGN_OK.              "#EC NEEDED

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-s01.

*<<F36K914879 - 02 Begin of del
*    PARAMETERS:
*      p_keydt    TYPE bkpf-budat DEFAULT sy-datum. ##NEEDED

*<<F36K914879 - 02 Begin of ins
  SELECTION-SCREEN BEGIN OF BLOCK bl11 WITH FRAME TITLE TEXT-s11.
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: rb_open    RADIOBUTTON GROUP sel DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 2(30) TEXT-sl1 FOR FIELD rb_open.
    SELECTION-SCREEN END OF LINE.

    SELECT-OPTIONS:
      s_keydt    FOR zsdsfit045-budat DEFAULT sy-datum NO-EXTENSION NO INTERVALS.

    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: rb_all    RADIOBUTTON GROUP sel.
      SELECTION-SCREEN COMMENT 2(30) TEXT-sl2 FOR FIELD rb_all.
    SELECTION-SCREEN END OF LINE.

    SELECT-OPTIONS:
      s_budat  FOR zsdsfit045-budat.

  SELECTION-SCREEN END OF BLOCK bl11.
*<<F36K914879 - 02 End of ins

  SELECT-OPTIONS:
    s_kunnr    FOR  zsdsfit045-kunnr OBLIGATORY,
    s_zuonr    FOR  bseg-zuonr,           "<<F36K914879 - 05 ins
    s_trnfn    FOR  zsdsfit045-tranf_no,
    s_bldat    FOR  zsdsfit045-bldat,
    s_belnr    FOR  zsdsfit045-belnr,
    s_gjahr    FOR  zsdsfit045-gjahr,
    s_block    FOR  zsdsfit045-block_status.
SELECTION-SCREEN END OF BLOCK bl1.

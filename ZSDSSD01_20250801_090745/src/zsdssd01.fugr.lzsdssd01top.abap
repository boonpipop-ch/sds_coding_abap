FUNCTION-POOL ZSDSSD01.                     "MESSAGE-ID ..



TYPE-POOLS : truxs.
*-------------------------------*
* structure of Material and part Master"
*-------------------------------*
TYPES: BEGIN OF gy_out,
        MTART  TYPE MARA-MATNR,
        MATNR  TYPE MARA-MATNR,
        MATKL  TYPE MARA-MATKL,
        MFRPN  TYPE MARA-MFRPN,
        MAKTX  TYPE MAKT-MAKTX,
        EKGRP  TYPE MARC-EKGRP,
        PRDHA  TYPE MARA-PRDHA,
        LGPBE  TYPE MARD-LGPBE,
        VERPR(18)  TYPE c,
        KBETR(18)  TYPE c,
        "VERPR  TYPE MBEW-VERPR,
       " KBETR  TYPE KONP-KBETR,
END OF gy_out.

DATA: gt_out TYPE STANDARD TABLE OF gy_out,
      gs_out LIKE LINE OF gt_out.
*-------------------------------*
*-------------------------------*
* structure of Sales order invoice
*-------------------------------*
TYPES: BEGIN OF gy_inv_out_h,
      ID        TYPE VBRK-VBELN,
      IND       TYPE C,
      WEBNO     TYPE VBAK-BNAME,
      VBELN     TYPE VBAK-VBELN,
      VBELN_DO  TYPE LIKP-VBELN,
      BELNR     TYPE VBRK-VBELN,
      FKDAT     TYPE VBRK-FKDAT,
      ZFBDT     TYPE DZFBDT,
      KUNUM     TYPE VBPA-KUNNR,
      NETPR_TOT TYPE VBRP-NETWR,
      DISCOUNT  TYPE KBETR,
      NETPR     TYPE VBRP-NETWR,
      VAT_AMT   TYPE DMBTR,
      DMBTR     TYPE DMBTR,
END OF gy_inv_out_h.
TYPES: BEGIN OF gy_inv_out_i,
      ID        TYPE VBRK-VBELN,
      IND       TYPE C,
      BELNR     TYPE VBRK-VBELN,
      POSNR     TYPE VBRP-POSNR,
      MATNR     TYPE VBRP-MATNR,
      KWMENG    TYPE VBRP-FKIMG,
      KBETR     TYPE KONV-KBETR,
      KWERT     TYPE KONV-KWERT,
      DISCOUNT  TYPE KONV-KWERT,
      NETPR     TYPE VBRK-NETWR,
END OF gy_inv_out_i.
DATA: gt_inv_out_h type STANDARD TABLE OF gy_inv_out_h,
      gs_inv_out_h LIKE LINE OF gt_inv_out_h,
      gt_inv_out_i type STANDARD TABLE OF gy_inv_out_i,
      gs_inv_out_i LIKE LINE OF gt_inv_out_i.
*-------------------------------*
*-------------------------------*
* structure of Sales order delivery gi
*-------------------------------*
TYPES: BEGIN OF gy_gi_out_h,
        ID       TYPE VBAK-VBELN,
        IND      TYPE C,
        VBELN    TYPE VBAK-VBELN,
        WEBNO    TYPE VBAK-BNAME,
        VBELN_DO TYPE LIKP-VBELN,
        LFDAT    TYPE LIKP-LFDAT,
        MBLNR    TYPE MSEG-MBLNR,
        KUNUM    TYPE LIKP-KUNNR,
        ABGRU    TYPE C,
END OF gy_gi_out_h.
TYPES: BEGIN OF gy_gi_out_i,
        ID       TYPE VBAK-VBELN,
        IND      TYPE C,
        VBELN    TYPE VBAK-VBELN,
        MATNR    TYPE LIPS-MATNR,
        MBLNR    TYPE LIPS-VBELN,
        ZEILE    TYPE MBLPO,
        RFMNG    TYPE LIPS-LFIMG,
END OF gy_gi_out_i.
DATA: gt_gi_out_h type STANDARD TABLE OF gy_gi_out_h,
      gs_gi_out_h LIKE LINE OF gt_gi_out_h,
      gt_gi_out_i type STANDARD TABLE OF gy_gi_out_i,
      gs_gi_out_i LIKE LINE OF gt_gi_out_i.
*-------------------------------*
DATA: gv_skip TYPE flag.


*DATA:
*      gs_idoc_control type EDIDC,
*      gt_edidc TYPE STANDARD TABLE OF edidc,
*      gs_edidc like LINE OF gt_edidc,
*      gt_edidd TYPE STANDARD TABLE OF edidd,
*      gs_edidd like LINE OF gt_edidd,
*      gs_path  TYPE ZSDSSDS000,
*      gs_edp13 TYPE edp13.

CONSTANTS: gc_sep    type c VALUE '","',
           gc_quote  TYPE c VALUE '"'.

DEFINE append_range.
  &1-sign   = &2.
  &1-option = &3.
  &1-low    = &4.
  &1-high   = &5.
  append &1.
END-OF-DEFINITION.

* INCLUDE LZSDSSD01D...                      " Local class definition

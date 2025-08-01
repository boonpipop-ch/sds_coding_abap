*&---------------------------------------------------------------------*
*& Include          ZSDSMMI0050_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF typ_ekpo,
    ebeln    TYPE ekpo-ebeln,   "PO NO.
    loekz    TYPE ekpo-loekz,   "DELETION
    ebelp    TYPE ekpo-ebelp,   "PO Item
    banfn    TYPE ekpo-banfn,   "PR NO.
    menge    TYPE ekpo-menge,   "OTY. PO
    meins    TYPE ekpo-meins,   "UNT. PO
    netwr    TYPE ekpo-netwr,   "NET. PO/PER LINE
    werks    TYPE ekpo-werks,   "PLANT
    ko_prctr TYPE ekpo-ko_prctr, "Profit Center
  END OF typ_ekpo,

  BEGIN OF typ_ekkn,
    ebeln TYPE ekkn-ebeln,
    ebelp TYPE ekkn-ebelp,
    vbeln TYPE ekkn-vbeln,
    aufnr TYPE ekkn-aufnr,
    prctr TYPE ekkn-prctr,
  END OF typ_ekkn,

  BEGIN OF typ_ekko,
    ebeln TYPE ekko-ebeln,
    bedat TYPE ekko-bedat,
    lifnr TYPE ekko-lifnr,
    ernam TYPE ekko-ernam,
    waers TYPE ekko-waers,
  END OF typ_ekko,

  BEGIN OF typ_data,
      ebeln LIKE ekpo-ebeln, " PO NO.
      banfn LIKE ekpo-banfn, "PR NO.
      netwr(8) TYPE p DECIMALS 2, "NET. PO/PER LIN
      ebelp LIKE ekpo-ebelp, " PO Item
      loekz LIKE ekpo-loekz, " DELETION
      menge(5) TYPE p DECIMALS 0 , "OTY. PO
      meins(10) TYPE c, "UNT. PO
      werks LIKE ekpo-werks, "PLANT
      vbeln LIKE ekkn-vbeln,
      kunnr LIKE vbak-kunnr,
      vgbel LIKE vbak-vgbel,
      name1 LIKE kna1-name1,
      text(20) TYPE c," JOB NO. FROM TEXT VBAK
      bedat LIKE ekko-bedat, " PO DATE
      lifnr LIKE ekko-lifnr, " ACCOUNT VENDOR
      name2(100) TYPE c,  "Add by Wantanee C1-20110808
      p_sale(40) TYPE c,
      auart LIKE vbak-auart,
      bezei LIKE tvakt-bezei,
      prepare(20) TYPE c,
      vat(3) TYPE p DECIMALS 2,
      sumqty LIKE ekpo-menge,
      gno LIKE ekpo-ebelp,
      adrnr LIKE vbpa-adrnr,
      aufnr LIKE ekkn-aufnr, "SVO CS MODULE modify 211209
      objnr LIKE viaufks-objnr, "modify 211209
      kunum LIKE viaufks-kunum, "modify 211209
      cusname_svo LIKE kna1-name1, "modify 211209
      adrnr2 LIKE ihpa-adrnr, "modify 211209
      auart2 LIKE viaufks-auart, "modify 211209
      svotxt LIKE t003p-txt,"modify 211209
      vkbur LIKE pmsdo-vkbur,"modify 211209 add type job for svo
      vkgrp LIKE pmsdo-vkgrp,"modify 211209 add type job for svo
*      header_job LIKE ztcs_header_job-header_job,"modify 211209 add type job for svo
      maufnr LIKE afko-maufnr,"modify 211209 add Job no.(Super Order(SVO))
      aufnr2 LIKE viaufks-aufnr,"modify 211209 add Job no.(Super Order(SVO))
      kdauf LIKE viaufks-kdauf,"modify 70110 add S/O for SVO MA in Job no.
      jobno LIKE viaufks-aufnr,
      status(10) TYPE c,
      qmnum LIKE afih-qmnum,
      fecod LIKE viqmfe-fecod,
      aufpl LIKE viaufks-aufpl,
      usr05 LIKE afvu-usr05,
  END OF typ_data.

CONSTANTS:
  gc_true     TYPE  char1     VALUE 'X'.

DATA:

  ""--Internal Table
  gt_salegrp TYPE STANDARD TABLE OF zsdscac001,
  gt_ekpo    TYPE STANDARD TABLE OF typ_ekpo,
  gt_ekkn    TYPE STANDARD TABLE OF typ_ekkn,
  gt_ekko    TYPE STANDARD TABLE OF typ_ekko,
  gt_ekkov   TYPE STANDARD TABLE OF typ_ekko,

  ""--Work Area
  gs_data    TYPE typ_data.

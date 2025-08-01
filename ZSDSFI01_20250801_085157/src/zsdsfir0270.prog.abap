*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0270
*  Creation Date      : 23.05.2024
*  Author             : b.chiewsarikij
*  Add-on ID          : ZFIARF015
*  Description        : Customer Balance Confirmation
*  Purpose            : N/A
*  Copied from        : ZR_AR_STATEMENT_OF_ACCOUNT (ECC)
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

REPORT zsdsfir0270.

*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES: bkpf,
        bseg,
        bsid,
        bsad,
        kna1.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPE-POOLS: slis,vrm,truxs.
TYPES: BEGIN OF typ_bsid,
         kunnr TYPE bsid-kunnr,    "Customer No.
         belnr TYPE bsid-belnr,    "Doc.No.
         buzei TYPE bsid-buzei,    "Line item
         budat TYPE bsid-budat,    "Doc.Date
         zfbdt TYPE bsid-zfbdt,    "Baseline Payment Dte
         zbd1t TYPE bsid-zbd1t,    "Terms of Payment
         dmbtr TYPE bsid-dmbtr,    "amt
         umskz TYPE bsid-umskz,    "Spacail GL
         xblnr TYPE bsid-xblnr,    "Referance
         shkzg TYPE bsid-shkzg,    "Debit Credit
         rebzg TYPE bsid-rebzg,    "invoice referance
         rebzj TYPE bsid-rebzj,    "inv refer year
         kidno TYPE bsid-kidno,    "credit referance
         gjahr TYPE bsid-gjahr,    "Fiscal Year
         blart TYPE bsid-blart,    "Doc.Type
         zuonr TYPE bsid-zuonr,    "Assignment
         cputm TYPE bkpf-cputm,    "Time entry
       END OF typ_bsid.

TYPES: BEGIN OF typ_bsad,
         kunnr TYPE bsad-kunnr,    "Customer No.
         belnr TYPE bsad-belnr,    "Doc.No.
         buzei TYPE bsad-buzei,    "Line item
         budat TYPE bsad-budat,    "Doc.Date
         zfbdt TYPE bsad-zfbdt,    "Baseline Payment Dte
         zbd1t TYPE bsad-zbd1t,    "Terms of Payment
         dmbtr TYPE bsad-dmbtr,    "amt
         umskz TYPE bsad-umskz,    "Spacail GL
         xblnr TYPE bsad-xblnr,    "Referance
         shkzg TYPE bsad-shkzg,    "Debit Credit
         rebzg TYPE bsad-rebzg,    "invoice referance
         rebzj TYPE bsad-rebzj,    "inv refer year
         kidno TYPE bsad-kidno,    "credit referance
         gjahr TYPE bsad-gjahr,    "Fiscal Year
         blart TYPE bsad-blart,    "Doc Type
         zuonr TYPE bsad-zuonr,    "Assignment
         cputm TYPE bkpf-cputm,    "Time entry
       END OF typ_bsad.


TYPES: BEGIN OF typ_kna1,
         kunnr  TYPE kna1-kunnr,      "Customer Code
         name1  TYPE kna1-name1,      "Customer Name
         name2  TYPE kna1-name2,      "Customer Name
         stras  TYPE kna1-stras,
         ort02  TYPE kna1-ort02,
         ort01  TYPE kna1-ort01,
         pstlz  TYPE kna1-pstlz,
         telf1  TYPE kna1-telf1,
         adrnr  TYPE kna1-adrnr,
         e_mail TYPE zsdsfit030-e_mail,
         check  TYPE c,

       END OF typ_kna1.

TYPES: BEGIN OF typ_pc, "Check Post date cheques
         augbl TYPE bsad-augbl,    "Doc.Post date cheques
         belnr TYPE bsad-belnr,    "Doc original
       END OF typ_pc.

TYPES: BEGIN OF typ_pa0001,
         kunnr TYPE kna1-kunnr,
         pernr TYPE pa0182-pernr,
         alnam TYPE pa0182-alnam,

       END OF typ_pa0001.

TYPES: BEGIN OF typ_adrc,
         adrnr      TYPE kna1-adrnr,
         name1      TYPE adrc-name1,
         name2      TYPE adrc-name2,
         name3      TYPE adrc-name3,
         street     TYPE adrc-street,
         str_suppl3 TYPE adrc-str_suppl3,
         location   TYPE adrc-location,
         city1      TYPE adrc-city1,
         region     TYPE adrc-region,
         post_code1 TYPE adrc-post_code1,
         country    TYPE adrc-country,
         city2      TYPE adrc-city2,


       END OF typ_adrc.


TYPES: BEGIN OF typ_item,
         kunnr       TYPE bsid-kunnr,
         belnr       TYPE bsid-belnr,
         umskz       TYPE bsid-umskz,
         budat       TYPE bsid-budat,
         duedate     TYPE bsid-budat,
         chk_day     TYPE p,
         dmbtr       TYPE bsid-dmbtr,
         stus_cq(25) TYPE c,
         cq_no(25)   TYPE c,
         remark(255) TYPE c,
       END OF typ_item.

TYPES: BEGIN OF typ_ztar_mail_state,
         kunnr  TYPE zsdsfit030-kunnr,
         e_mail TYPE zsdsfit030-e_mail,
       END OF typ_ztar_mail_state.

TYPES: BEGIN OF typ_mail,
         kunnr  TYPE kna1-kunnr,
         e_mail TYPE zsdsfit030-e_mail,
       END OF typ_mail.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
*DATA:
*-> internal tables
DATA: gt_kna1      TYPE STANDARD TABLE OF typ_kna1,
      gt_kna1_1    TYPE STANDARD TABLE OF typ_kna1,
      gt_bsid      TYPE STANDARD TABLE OF typ_bsid,
      gt_bsid_diff TYPE STANDARD TABLE OF typ_bsid,
      gt_bsad      TYPE STANDARD TABLE OF typ_bsad,
      gt_post_chq  TYPE STANDARD TABLE OF typ_pc,
      gt_pa0001    TYPE STANDARD TABLE OF typ_pa0001,
      gt_adrc      TYPE STANDARD TABLE OF typ_adrc,
      gt_adrc_th   TYPE STANDARD TABLE OF typ_adrc,
      gt_bsid_ref  TYPE STANDARD TABLE OF typ_bsid,
      gt_bsad_ref  TYPE STANDARD TABLE OF typ_bsad,
      gt_bsid_adv  TYPE STANDARD TABLE OF typ_bsid,
      gt_bsad_adv  TYPE STANDARD TABLE OF typ_bsad,
      gt_item      TYPE STANDARD TABLE OF typ_item.

DATA: gw_kna1     TYPE typ_kna1,
      gw_kna1_1   TYPE typ_kna1,
      gw_bsid     TYPE typ_bsid,
      gw_bsad     TYPE typ_bsad,
      gw_post_chq TYPE typ_pc,
      gw_pa0001   TYPE typ_pa0001,
      gw_adrc     TYPE typ_adrc,
      gw_adrc_th  TYPE typ_adrc,
      gw_bsid_ref TYPE typ_bsid,
      gw_bsad_ref TYPE typ_bsad,
      gw_bsid_adv TYPE typ_bsid,
      gw_bsad_adv TYPE typ_bsad,
      gw_item     TYPE typ_item.

DATA: wa_kna1      TYPE typ_kna1,
      wa_bsid      TYPE typ_bsid,
      wa_bsad      TYPE typ_bsad,
      wa_post_chq  TYPE  typ_pc,
      wa_pa0001    TYPE  typ_pa0001,
      wa_adrc      TYPE  typ_adrc,
      wa_adrc_th   TYPE  typ_adrc,
      wa_bsid_ref  TYPE typ_bsid,
      wa_bsid_tmp  TYPE typ_bsid,
      wa_bsid_tmp1 TYPE typ_bsid,
      wa_bsad_ref  TYPE typ_bsad,
      wa_bsid_adv  TYPE typ_bsid,
      wa_bsad_adv  TYPE typ_bsad,
      wa_bsid_diff TYPE typ_bsid,
      wa_item      TYPE typ_item.

DATA: gt_ztar_mail_state TYPE STANDARD TABLE OF typ_ztar_mail_state,
      gw_ztar_mail_state TYPE typ_ztar_mail_state,
      wa_ztar_mail_state TYPE typ_ztar_mail_state,
      gt_send_mail       TYPE STANDARD TABLE OF typ_mail,
      gw_send_mail       TYPE typ_mail,
      wa_send_mail       TYPE typ_mail.

DATA: otfdata TYPE TABLE OF itcoo.
DATA: options TYPE itcpo.  "CH7 Add by Wantanee 20210726

DATA: lv_month_text(20) TYPE c,
      lv_day_text(2)    TYPE c.

DATA: gv_mail_sendor TYPE adr6-smtp_addr.



DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_fieldcat TYPE slis_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv.

DATA: it_fieldcat TYPE lvc_t_fcat,     "slis_t_fieldcat_alv WITH HEADER LINE,
      wa_fieldcat TYPE lvc_s_fcat.
*-> range
*-> work areas
DATA: gw_itcpp  LIKE itcpp.      "Print result
*-> variables
DATA: gv_formid(100)   TYPE c VALUE 'ZSDSFI001',
      gv_repid         LIKE sy-repid VALUE sy-repid,
      gv_kunnr         TYPE kna1-kunnr,     "Customer Code
      gv_name(100)     TYPE c,     "Customer Name
      gv_stras         TYPE adrc-street,     "Customer Address
      gv_ort02         TYPE kna1-ort02,
      gv_ort01         TYPE kna1-ort01,
      gv_pstlz         TYPE kna1-pstlz,
      gv_str_suppl3    TYPE adrc-str_suppl3, "Add by Wantanee 20190814
      gv_location      TYPE adrc-location,   "Add by Wantanee 20190814
      gv_telf1         TYPE kna1-telf1,
      gv_sale(100)     TYPE c,
      gv_belnr         TYPE bkpf-belnr,     "Document No.
      gv_budat         TYPE bkpf-budat,     "Posting Date
      gv_duedate       TYPE bkpf-budat,     "Due Date
      gv_day           TYPE p,              "Due
      gv_amt           TYPE dmbtr,          "Amount
      gv_tot_amt       TYPE dmbtr,          "Total Amt
      gv_stus_cq(25)   TYPE c,              "Status Cheques
      gv_cq_no(10)     TYPE c,              "Cheques
      gv_spell_word    TYPE char255,        "total amt word
      gv_remark(255)   TYPE c,              "Remark
      gv_asof          TYPE d,
      gv_forword       TYPE d,
      gv_date          TYPE sy-datum,
      gv_name_eng(100) TYPE c,
      gv_umskz         TYPE bsid-umskz,     "SP GL
      gv_stceg(20)     TYPE c,   "Tax of company
      gv_sign          TYPE c,  "+ and -
      gv_date_asof     LIKE sy-datum.
*-> reference

*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS : gc_repid   TYPE repid      VALUE 'ZR_AR_STATEMENT_OF_ACCOUNT',
            c_langu_th LIKE syst-langu VALUE '2',
            gc_save(1) TYPE c      VALUE 'A',
            gc_eqtyp   TYPE eqtyp  VALUE 'M'.

*-----------------------------------------------------------------------
* M A C R O S
*-----------------------------------------------------------------------
*DEFINE.
*END-OF-DEFINITION.

*-----------------------------------------------------------------------
* F I E L D – S Y M B O L S
*-----------------------------------------------------------------------
*FIELD-SYMBOLS:

*-----------------------------------------------------------------------
* F I E L D – G R O U P S
*-----------------------------------------------------------------------
*FIELD-GROUPS:

*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-h01.


  PARAMETERS: p_bukrs TYPE bukrs       OBLIGATORY DEFAULT '1000',  "Company Code
              p_budat TYPE sy-datum     OBLIGATORY .             "From date
  SELECT-OPTIONS: p_kunnr FOR kna1-kunnr.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-h03.
  PARAMETERS: p_mail AS CHECKBOX.
  PARAMETERS: p_redate TYPE sy-datum.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
  PARAMETERS: p_langu LIKE sy-langu DEFAULT sy-langu.   "Print Language
SELECTION-SCREEN END OF BLOCK b2.

*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.

*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
*AT SELECTION-SCREEN OUTPUT.
*AT SELECTION-SCREEN ON HELP-REQUEST FOR <field>
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR <field>
*AT SELECTION-SCREEN ON <field>
*AT SELECTION-SCREEN.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
*-> read data
  PERFORM f_prepare_data.
  PERFORM f_convert_year.
*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
*-> process data
  IF p_mail IS INITIAL.  "CH7 Add by Wantanee 20210726
    PERFORM f_open_form  USING gv_formid p_langu 'PRINTER'
                         CHANGING gw_itcpp.
    SORT gt_kna1.
    SORT gt_bsid.
    SORT gt_bsad.
    LOOP AT gt_kna1 INTO gw_kna1.
      CLEAR:  gv_kunnr,    "Customer Code
              gv_name,     "Customer Name
              gv_stras,    "Customer Address
              gv_ort02,
              gv_ort01,
              gv_pstlz,
              gv_telf1,
              gv_sale,
              gv_belnr,     "Document No.
              gv_budat,     "Posting Date
              gv_duedate,   "Due Date
              gv_day,       "Due
              gv_amt,       "Amount
              gv_stus_cq,   "Status Cheques
              gv_cq_no,     "Cheques
              gv_spell_word,"total amt word
              gv_remark,    "Remark
              gv_date ,
              gv_name_eng,
              gv_sign.

      PERFORM f_start_form USING gv_formid p_langu 'FIRST' gv_repid.

      PERFORM f_print_form.

      PERFORM f_end_form.

    ENDLOOP.

    PERFORM f_close_form.

  ELSE. "CH7 Add by Wantanee 20210726

    IF p_redate IS NOT INITIAL.

      PERFORM f_display_report.

    ELSE.
      MESSAGE e000(38) WITH 'กรุณาป้อนวันที่ ของ ตอบกลับเอกสารภายในวันที่'.
    ENDIF.
  ENDIF.
*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
*TOP-OF-PAGE.

*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
*AT LINE-SELECTION.

*-----------------------------------------------------------------------
* F O R M S
*-----------------------------------------------------------------------
**&---------------------------------------------------------------------*
**&      Form  F_GET_DATA
**&---------------------------------------------------------------------*
*
FORM f_prepare_data .

  DATA: idx TYPE sy-tabix.



  SELECT a~kunnr a~name1 a~name2 a~stras a~ort02 a~ort01 a~pstlz a~telf1 a~adrnr
  INTO TABLE gt_kna1
  FROM  kna1 AS a INNER JOIN bsid AS b
                  ON ( a~kunnr EQ b~kunnr
                  AND b~budat LE p_budat
                  AND b~umsks NE 'G' )
*                        LEFT OUTER JOIN knvp AS kn
*                        ON ( a~kunnr EQ kn~kunnr
*                        AND kn~parvw EQ 'VE' )
*                        INNER JOIN bsad AS k
*                        ON ( a~kunnr = k~kunnr
*                        AND k~augdt GT p_budat
*                        AND k~budat LT p_budat )
  WHERE a~kunnr IN p_kunnr
  GROUP BY a~kunnr a~name1 a~name2 a~stras a~ort02 a~ort01 a~pstlz a~telf1 a~adrnr.

  SELECT a~kunnr a~name1 a~name2 a~stras a~ort02 a~ort01 a~pstlz a~telf1 a~adrnr
  INTO TABLE gt_kna1_1
  FROM  kna1 AS a  INNER JOIN bsad AS k
                  ON ( a~kunnr = k~kunnr
                  AND k~augdt GT p_budat
                  AND k~budat LE p_budat )
  WHERE a~kunnr IN p_kunnr
  GROUP BY a~kunnr a~name1 a~name2 a~stras a~ort02 a~ort01 a~pstlz a~telf1 a~adrnr.

  LOOP AT gt_kna1_1 INTO gw_kna1_1.
    APPEND gw_kna1_1 TO gt_kna1.
  ENDLOOP.

  SORT gt_kna1.
  DELETE ADJACENT DUPLICATES FROM gt_kna1.


  SELECT a~kunnr a~belnr a~buzei a~budat a~zfbdt
         a~zbd1t a~dmbtr a~umskz a~xblnr a~shkzg
         a~rebzg a~rebzj a~kidno a~gjahr a~blart a~zuonr "Add by Wantanee 20130728
  INTO TABLE gt_bsid
  FROM  bsid AS a
  FOR ALL ENTRIES IN  gt_kna1
  WHERE a~kunnr = gt_kna1-kunnr
  AND a~umsks NE 'G'
  AND a~budat LE p_budat.

  "Add by Wantanee 20130728

  MOVE gt_bsid TO gt_bsid_ref.


  SELECT a~kunnr a~belnr a~buzei a~budat a~zfbdt
         a~zbd1t a~dmbtr a~umskz a~xblnr a~shkzg
         a~rebzg a~rebzj a~kidno a~gjahr a~blart a~zuonr "Add by Wantanee 20130728
  INTO TABLE gt_bsid_adv
  FROM bsid AS a
  FOR ALL ENTRIES IN  gt_kna1
  WHERE a~kunnr = gt_kna1-kunnr
  AND ( a~blart EQ 'DV'
   OR  a~blart EQ 'DJ' )
  AND a~budat LE p_budat.
  "End Add by Wantanee 20130728


  SELECT a~kunnr a~belnr a~buzei a~budat a~zfbdt
         a~zbd1t a~dmbtr a~umskz a~xblnr a~shkzg
         a~rebzg a~rebzj a~kidno a~gjahr a~blart a~zuonr  "Add by Wantanee 20130728
  INTO TABLE gt_bsad
  FROM bsad AS a
  FOR ALL ENTRIES IN  gt_kna1
  WHERE a~kunnr = gt_kna1-kunnr
  AND a~augdt GT p_budat
  AND a~budat LE p_budat.


  "Add by Wantanee 20130728
  MOVE gt_bsad TO gt_bsad_ref.

  SELECT  a~kunnr a~belnr a~buzei a~budat a~zfbdt
         a~zbd1t a~dmbtr a~umskz a~xblnr a~shkzg
         a~rebzg a~rebzj a~kidno a~gjahr a~blart a~zuonr  "Add by Wantanee 20130728
  INTO TABLE gt_bsad_adv
  FROM bsad AS a
  FOR ALL ENTRIES IN  gt_kna1
  WHERE a~kunnr = gt_kna1-kunnr
  AND a~augdt GT p_budat
  AND a~budat LE p_budat
  AND ( a~blart EQ 'DV'
  OR  a~blart EQ 'DJ' )  .
  "End Add by Wantanee 20130728

  SELECT k~kunnr k~pernr pa~alnam
  INTO TABLE gt_pa0001
  FROM knvp AS k INNER JOIN pa0182 AS pa
                 ON ( k~pernr EQ pa~pernr )
  FOR ALL ENTRIES IN gt_kna1
  WHERE k~kunnr EQ gt_kna1-kunnr
  AND k~parvw = 'VE' .


  SELECT addrnumber name1 name2 name3
  INTO TABLE gt_adrc
  FROM adrc
  FOR ALL ENTRIES IN gt_kna1
  WHERE addrnumber EQ gt_kna1-adrnr
    AND nation = 'I'.



  SELECT addrnumber name1 name2 name3
  street str_suppl3 location city1
  region post_code1 country city2
  INTO TABLE gt_adrc_th
  FROM adrc
  FOR ALL ENTRIES IN gt_kna1
  WHERE addrnumber = gt_kna1-adrnr
  AND nation = ' '.





  LOOP AT gt_bsad INTO wa_bsad.
    wa_bsid-kunnr = wa_bsad-kunnr.
    wa_bsid-belnr = wa_bsad-belnr.
    wa_bsid-buzei = wa_bsad-buzei.
    wa_bsid-budat = wa_bsad-budat.
    wa_bsid-zfbdt = wa_bsad-zfbdt.
    wa_bsid-zbd1t = wa_bsad-zbd1t.
    wa_bsid-dmbtr = wa_bsad-dmbtr.
    wa_bsid-umskz = wa_bsad-umskz.
    wa_bsid-xblnr = wa_bsad-xblnr.
    wa_bsid-shkzg = wa_bsad-shkzg.
    wa_bsid-rebzg = wa_bsad-rebzg.
    wa_bsid-rebzj = wa_bsad-rebzj.
    wa_bsid-kidno = wa_bsad-kidno.
    wa_bsid-gjahr = wa_bsad-gjahr.
    wa_bsid-blart = wa_bsad-blart.
    wa_bsid-zuonr = wa_bsad-zuonr.
    wa_bsid-cputm = wa_bsad-cputm.

    APPEND wa_bsid TO gt_bsid.
  ENDLOOP.
  LOOP AT gt_bsad_ref INTO wa_bsad_ref.
    wa_bsid_ref-kunnr = wa_bsad_ref-kunnr.
    wa_bsid_ref-belnr = wa_bsad_ref-belnr.
    wa_bsid_ref-buzei = wa_bsad_ref-buzei.
    wa_bsid_ref-budat = wa_bsad_ref-budat.
    wa_bsid_ref-zfbdt = wa_bsad_ref-zfbdt.
    wa_bsid_ref-zbd1t = wa_bsad_ref-zbd1t.
    wa_bsid_ref-dmbtr = wa_bsad_ref-dmbtr.
    wa_bsid_ref-umskz = wa_bsad_ref-umskz.
    wa_bsid_ref-xblnr = wa_bsad_ref-xblnr.
    wa_bsid_ref-shkzg = wa_bsad_ref-shkzg.
    wa_bsid_ref-rebzg = wa_bsad_ref-rebzg.
    wa_bsid_ref-rebzj = wa_bsad_ref-rebzj.
    wa_bsid_ref-kidno = wa_bsad_ref-kidno.
    wa_bsid_ref-gjahr = wa_bsad_ref-gjahr.
    wa_bsid_ref-blart = wa_bsad_ref-blart.
    wa_bsid_ref-zuonr = wa_bsad_ref-zuonr.
    wa_bsid_ref-cputm = wa_bsad_ref-cputm.

    APPEND wa_bsid_ref TO gt_bsid_ref.
  ENDLOOP.



  LOOP AT gt_bsid INTO wa_bsid WHERE blart EQ 'DV' AND umskz =  'V'.
    .
    READ TABLE gt_bsid INTO wa_bsid_tmp1 WITH KEY kunnr = wa_bsid-kunnr
                                                  belnr = wa_bsid-belnr
                                                  gjahr = wa_bsid-gjahr
                                                  umskz =  'S'.
    IF sy-subrc EQ 0.
      DELETE TABLE gt_bsid FROM wa_bsid_tmp1.
      DELETE TABLE gt_bsid_ref FROM wa_bsid_tmp1.
    ENDIF.

  ENDLOOP.


  IF gt_kna1 IS NOT INITIAL.

    SELECT kunnr e_mail
      INTO TABLE gt_ztar_mail_state
      FROM zsdsfit030
      FOR ALL ENTRIES IN gt_kna1
      WHERE kunnr = gt_kna1-kunnr.

    LOOP AT gt_kna1 INTO gw_kna1.
      idx = sy-tabix.
      READ TABLE gt_ztar_mail_state INTO gw_ztar_mail_state WITH KEY kunnr = gw_kna1-kunnr.
      IF sy-subrc EQ 0.
        gw_kna1-e_mail = gw_ztar_mail_state-e_mail.
        gw_kna1-check = 'X'.
        MODIFY gt_kna1 INDEX idx FROM gw_kna1
                  TRANSPORTING e_mail check.


      ENDIF.


    ENDLOOP.

  ENDIF.

ENDFORM.   "Get data.

**&---------------------------------------------------------------------*
**&      Form  MAP_DATA
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*
FORM f_print_form.
  DATA: p_total TYPE dmbtr.
  DATA: lv_count_line TYPE i, "Add by Wantanee 20130314
        lv_check_line TYPE i. "Add by Wantanee 20130314
  CLEAR: gw_bsid,
         gw_bsad,
         gv_remark,
         gv_stus_cq,
         gv_cq_no,
         lv_count_line.

  CLEAR: gv_belnr,gv_budat,gv_duedate,gv_day,gv_amt,gv_stus_cq,gv_cq_no,gv_remark,gv_umskz,gv_sign.
  CLEAR: gv_str_suppl3,gv_location.


  SELECT SINGLE stceg
  INTO (gv_stceg)
  FROM t001
  WHERE bukrs = '1000'.

  gv_stceg = gv_stceg+2(13).
  PERFORM f_write_form USING '' 'ADD_COM'.

  gv_date = sy-datum.
  gv_asof  = p_budat.
  gv_kunnr = gw_kna1-kunnr.
*  CONCATENATE gw_kna1-name1 gw_kna1-name2 INTO gv_name.

  gv_ort02 = gw_kna1-ort02.
  gv_ort01 = gw_kna1-ort01.
  gv_pstlz = gw_kna1-pstlz.

  READ TABLE gt_adrc_th INTO gw_adrc_th WITH KEY adrnr = gw_kna1-adrnr.
  IF sy-subrc EQ 0.
    CONCATENATE gw_adrc_th-name1 gw_adrc_th-name2 gw_adrc_th-name3 INTO gv_name.

    gv_str_suppl3 = gw_adrc_th-str_suppl3.
    gv_location  = gw_adrc_th-location.
    gv_stras = gw_adrc_th-street.
  ENDIF.


  IF NOT gw_kna1-telf1 IS INITIAL.
    gv_telf1 = gw_kna1-telf1.
  ELSE.
    gv_telf1 = '-'.
  ENDIF.



  READ TABLE gt_pa0001 INTO gw_pa0001 WITH KEY kunnr = gw_kna1-kunnr.
  IF  sy-subrc IS INITIAL.
    CONCATENATE gw_pa0001-pernr ' - ' gw_pa0001-alnam INTO gv_sale.
  ELSE.
    gv_sale = '-'.
  ENDIF.

  READ TABLE gt_adrc INTO gw_adrc WITH KEY adrnr = gw_kna1-adrnr.
  IF  sy-subrc IS INITIAL.
    CONCATENATE gw_adrc-name1 gw_adrc-name2 INTO gv_name_eng.
  ELSE.
    gv_name_eng = ' '.
  ENDIF.

  PERFORM f_write_form USING '' 'ADDR'.
  SORT gt_bsid BY kunnr budat .
  SORT gt_bsad BY kunnr budat .

  MOVE gt_bsid TO gt_bsid_diff.
  CLEAR: gt_item,wa_item,gw_item.


  LOOP AT gt_bsid INTO gw_bsid WHERE kunnr = gw_kna1-kunnr AND NOT ( blart EQ 'DJ'  AND  umskz EQ '' ) .
    CLEAR: gv_remark,
           gv_stus_cq,
           gv_cq_no.

    IF gw_bsid-umskz EQ 'B'.
      DELETE TABLE gt_bsid FROM gw_bsid.
      DELETE TABLE gt_bsid_diff FROM gw_bsid.

    ELSEIF gw_bsid-rebzg NE ''.
      READ TABLE gt_bsid INTO wa_bsid_tmp WITH KEY kunnr = gw_kna1-kunnr
                                                   belnr = gw_bsid-rebzg
                                                   gjahr = gw_bsid-rebzj.
      IF sy-subrc EQ 0.
        DELETE TABLE gt_bsid FROM gw_bsid.
        DELETE TABLE gt_bsid_diff FROM gw_bsid.
      ENDIF.
      CLEAR: wa_bsid_tmp.
    ELSE.


*              IF gw_bsid-blart EQ 'DV'.
*                 READ TABLE gt_bsid INTO wa_bsid_tmp WITH KEY kunnr = gw_kna1-kunnr
*                                                              belnr = gw_bsid-belnr
*                                                              umskz =  'V'.
*                      IF sy-subrc EQ 0.
*                         READ TABLE gt_bsid INTO wa_bsid_tmp1 WITH KEY kunnr = gw_kna1-kunnr
*                                                                       belnr = gw_bsid-belnr
*                                                                       umskz =  'S'.
*                            IF sy-subrc EQ 0.
*                                DELETE TABLE gt_bsid FROM wa_bsid_tmp1.
*                                DELETE TABLE gt_bsid_diff FROM wa_bsid_tmp1.
*                            ENDIF.
*                      ENDIF.
*
*              ENDIF.



      IF NOT ( ( gw_bsid-blart EQ 'DJ' ) AND ( gw_bsid-umskz EQ 'S' ) ).
        gv_belnr = gw_bsid-belnr.
        gv_budat  =  gw_bsid-budat.
        gv_umskz   = gw_bsid-umskz.

        IF gw_bsid-zbd1t NE 0.
          gv_duedate = gw_bsid-zfbdt + gw_bsid-zbd1t.
          gv_day = gv_asof - gv_duedate.
        ELSEIF gw_bsid-zfbdt NE ''.
          gv_duedate = gw_bsid-zfbdt.
          gv_day = gv_asof - gv_duedate.
        ENDIF.
        IF gw_bsid-umskz EQ 'S'  OR gw_bsid-umskz EQ 'R'  OR gw_bsid-umskz EQ 'Y' .
          CLEAR: gv_duedate,gv_day.
        ENDIF.
        IF gw_bsid-shkzg = 'S'.
          gv_amt = gw_bsid-dmbtr.
        ELSE.
          gv_amt = gw_bsid-dmbtr * -1.
        ENDIF.

        "Add by Wantanee 20130728
        LOOP AT gt_bsid_ref INTO wa_bsid_ref WHERE kunnr = gw_kna1-kunnr
                                               AND rebzg = gw_bsid-belnr
                                               AND rebzj = gw_bsid-gjahr.
          IF ( wa_bsid_ref-belnr NE wa_bsid_ref-rebzg ) .
            IF wa_bsid_ref-shkzg = 'S'.
              gv_amt = gv_amt + wa_bsid_ref-dmbtr.
            ELSE.
              gv_amt = gv_amt + ( wa_bsid_ref-dmbtr * -1 ).
            ENDIF.
            DELETE TABLE gt_bsid FROM wa_bsid_ref.
            DELETE TABLE gt_bsid_diff FROM wa_bsid_ref.
            DELETE TABLE gt_bsid_ref FROM wa_bsid_ref.
          ENDIF.

        ENDLOOP.
        IF ( gw_bsid-blart EQ 'DV' ) AND ( gw_bsid-umskz EQ 'S' ) AND ( gw_bsid-zuonr NE '' ).
          LOOP AT gt_bsid_ref INTO wa_bsid_ref WHERE kunnr = gw_kna1-kunnr
                                               AND zuonr = gw_bsid-zuonr
                                               AND blart = 'DJ'
                                               AND umskz = 'S'
                                               AND rebzg NE gw_bsid-belnr.

            IF ( wa_bsid_ref-belnr NE wa_bsid_ref-rebzg ) .
              IF wa_bsid_ref-shkzg = 'S'.
                gv_amt = gv_amt + wa_bsid_ref-dmbtr.
              ELSE.
                gv_amt = gv_amt + ( wa_bsid_ref-dmbtr * -1 ).
              ENDIF.
              DELETE TABLE gt_bsid FROM wa_bsid_ref.
              DELETE TABLE gt_bsid_diff FROM wa_bsid_ref.
              DELETE TABLE gt_bsid_ref FROM wa_bsid_ref.
            ENDIF.

          ENDLOOP.

        ENDIF.
        IF ( gw_bsid-blart EQ 'Y3' ) AND ( gw_bsid-umskz EQ 'S' ).
          LOOP AT gt_bsid_ref INTO wa_bsid_ref WHERE kunnr = gw_kna1-kunnr
                                               AND zuonr = gw_bsid-zuonr
*                                                              AND blart = 'DJ'
                                               AND umskz = 'S'.

            IF ( wa_bsid_ref-belnr NE gw_bsid-belnr ) .
              IF wa_bsid_ref-shkzg = 'S'.
                gv_amt = gv_amt + wa_bsid_ref-dmbtr.
              ELSE.
                gv_amt = gv_amt + ( wa_bsid_ref-dmbtr * -1 ).
              ENDIF.
              DELETE TABLE gt_bsid FROM wa_bsid_ref.
              DELETE TABLE gt_bsid_diff FROM wa_bsid_ref.
              DELETE TABLE gt_bsid_ref FROM wa_bsid_ref.
            ENDIF.

          ENDLOOP.

        ENDIF.

        "End Add by Wantanee 20130728
        p_total = p_total + gv_amt.

        IF gw_bsid-umskz = 'D'.
*                        gv_stus_cq = gw_bsid-umskz.
          gv_stus_cq = 'เช็ครับล่วงหน้า'.
          gv_cq_no = gw_bsid-xblnr.
          PERFORM post_cheq USING gw_bsid-belnr gw_bsid-budat CHANGING gv_remark.
        ENDIF.
        IF gw_bsid-umskz = 'S' OR gw_bsid-umskz = 'V'.
*                        gv_stus_cq = gw_bsid-umskz.
          gv_stus_cq = 'เงินมัดจำ'.
        ENDIF.
        IF gw_bsid-umskz = 'S'.
          gv_remark = 'ชำระเงินแล้ว'.
        ENDIF.
        IF gw_bsid-umskz = 'R'.
*                        gv_stus_cq = gw_bsid-umskz.
          gv_stus_cq = 'เช็คคืน'.
          gv_cq_no = gw_bsid-xblnr.
          PERFORM post_cheq USING gw_bsid-belnr gw_bsid-budat CHANGING gv_remark.
        ENDIF.
        IF gw_bsid-umskz = 'Y'.
*                       gv_stus_cq = gw_bsid-umskz.
          gv_stus_cq = 'เงินประกันผลงาน'.
        ENDIF.

        DELETE TABLE gt_bsid_diff FROM gw_bsid.

        IF gv_belnr NE ''.
          wa_item-kunnr =  gw_kna1-kunnr.
          wa_item-belnr = gv_belnr.
          wa_item-budat = gv_budat.
          wa_item-duedate = gv_duedate.
          wa_item-chk_day = gv_day.
          wa_item-dmbtr = gv_amt.
          wa_item-stus_cq = gv_stus_cq.
          wa_item-cq_no = gv_cq_no.
          wa_item-remark = gv_remark.
          wa_item-umskz = gv_umskz.

          APPEND wa_item TO gt_item.

        ENDIF.

*                      PERFORM f_write_form USING 'DETAIL' 'MAIN'.
*                      lv_count_line = lv_count_line + 1.

      ENDIF.
    ENDIF.





  ENDLOOP.

  CLEAR: gw_bsid.
  LOOP AT gt_bsid_diff INTO gw_bsid WHERE kunnr = gw_kna1-kunnr.
    CLEAR: gv_remark,
          gv_stus_cq,
          gv_cq_no.

    gv_belnr = gw_bsid-belnr.
    gv_budat  =  gw_bsid-budat.
    gv_umskz   = gw_bsid-umskz.

    IF gw_bsid-zbd1t NE 0.
      gv_duedate = gw_bsid-zfbdt + gw_bsid-zbd1t.
      gv_day = gv_asof - gv_duedate.

    ENDIF.
    IF gw_bsid-umskz EQ 'S'  OR gw_bsid-umskz EQ 'R'  OR gw_bsid-umskz EQ 'Y' .
      CLEAR: gv_duedate,gv_day.
    ENDIF.
    IF gw_bsid-shkzg = 'S'.
      gv_amt = gw_bsid-dmbtr.
    ELSE.
      gv_amt = gw_bsid-dmbtr * -1.
    ENDIF.


    p_total = p_total + gv_amt.

    IF gw_bsid-umskz = 'D'.
*                       gv_stus_cq = gw_bsid-umskz.
      gv_stus_cq = 'เช็ครับล่วงหน้า'.
      gv_cq_no = gw_bsid-xblnr.
      PERFORM post_cheq USING gw_bsid-belnr gw_bsid-budat CHANGING gv_remark.
    ENDIF.
    IF gw_bsid-umskz = 'S' OR gw_bsid-umskz = 'V'.
*                       gv_stus_cq = gw_bsid-umskz.
      gv_stus_cq = 'เงินมัดจำ'.
    ENDIF.
    IF gw_bsid-umskz = 'S'.
      gv_remark = 'ชำระเงินแล้ว'.
    ENDIF.
    IF gw_bsid-umskz = 'R'.
*                       gv_stus_cq = gw_bsid-umskz.
      gv_stus_cq = 'เช็คคืน'.
    ENDIF.
    IF gw_bsid-umskz = 'Y'.
*                       gv_stus_cq = gw_bsid-umskz.
      gv_stus_cq = 'เงินประกันผลงาน'.
    ENDIF.

    IF gv_belnr NE ''.
      wa_item-kunnr =  gw_kna1-kunnr.
      wa_item-belnr = gv_belnr.
      wa_item-budat = gv_budat.
      wa_item-duedate = gv_duedate.
      wa_item-chk_day = gv_day.
      wa_item-dmbtr = gv_amt.
      wa_item-stus_cq = gv_stus_cq.
      wa_item-cq_no = gv_cq_no.
      wa_item-remark = gv_remark.
      wa_item-umskz = gv_umskz.

      APPEND wa_item TO gt_item.

    ENDIF.

*                     PERFORM f_write_form USING 'DETAIL' 'MAIN'.
*                     lv_count_line = lv_count_line + 1.

  ENDLOOP.




*    LOOP AT gt_bsad INTO gw_bsad WHERE kunnr = gw_kna1-kunnr AND NOT ( blart EQ 'DJ'  AND  umskz EQ '' ) .
*      CLEAR: gv_remark,
*           gv_stus_cq,
*           gv_cq_no.
*      IF ( gw_bsad-blart NE 'DJ' ) AND ( gw_bsad-umskz NE 'S' ).
*               gv_belnr = gw_bsad-belnr.
*               gv_budat  =  gw_bsad-budat.
*
*               IF gw_bsad-zbd1t NE 0.
*                 gv_duedate = gw_bsad-zfbdt + gw_bsad-zbd1t.
*                 gv_day = gv_asof - gv_duedate.
*
*               ENDIF.
*
*               IF gw_bsad-shkzg = 'S'.
*                 gv_amt = gw_bsad-dmbtr.
*               ELSE.
*                 gv_amt = gw_bsad-dmbtr * -1.
*               ENDIF.
*               "Add by Wantanee 20130728
*               LOOP AT gt_bsad_ref INTO wa_bsad_ref WHERE kunnr = gw_kna1-kunnr
*                                                      AND rebzg = gw_bsid-belnr
*                                                      AND rebzj = gw_bsid-gjahr.
*                      IF ( wa_bsad_ref-belnr NE wa_bsad_ref-rebzg ).
*                                 IF wa_bsid_ref-shkzg = 'S'.
*                                   gv_amt = gv_amt + wa_bsad_ref-dmbtr.
*                                 ELSE.
*                                   gv_amt = gv_amt + ( wa_bsad_ref-dmbtr * -1 ).
*                                 ENDIF.
*                         DELETE TABLE gt_bsad FROM wa_bsad_ref.
*                      ENDIF.
*
*               ENDLOOP.
*               IF ( gw_bsad-blart EQ 'DV' ) AND ( gw_bsad-umskz EQ 'S' ) AND ( gw_bsad-zuonr NE '' ).
*                  LOOP AT gt_bsad_ref INTO wa_bsad_ref WHERE kunnr = gw_kna1-kunnr
*                                                       AND zuonr = gw_bsad-zuonr
*                                                       AND blart = 'DJ'
*                                                       AND umskz = 'S'.
*                      IF ( wa_bsad_ref-belnr NE wa_bsad_ref-rebzg ).
*                                 IF wa_bsid_ref-shkzg = 'S'.
*                                   gv_amt = gv_amt + wa_bsad_ref-dmbtr.
*                                 ELSE.
*                                   gv_amt = gv_amt + ( wa_bsad_ref-dmbtr * -1 ).
*                                 ENDIF.
*                         DELETE TABLE gt_bsad FROM wa_bsad_ref.
*                      ENDIF.
*
*                  ENDLOOP.
*
*               ENDIF.
*               "End Add by Wantanee 20130728
*               p_total = p_total + gv_amt.
*
*               IF gw_bsad-umskz = 'D'.
*                 gv_stus_cq = gw_bsad-umskz.
*                 gv_cq_no = gw_bsad-xblnr.
*                 PERFORM post_cheq USING gw_bsad-belnr gw_bsad-budat CHANGING gv_remark.
*               ENDIF.
*               PERFORM f_write_form USING 'DETAIL' 'MAIN'.
*
*               lv_count_line = lv_count_line + 1.
*
*    ENDIF.
*
*    ENDLOOP.

  SORT gt_item BY umskz budat belnr.

  CLEAR: wa_item.
  LOOP AT gt_item INTO wa_item WHERE kunnr = gw_kna1-kunnr.
    gv_belnr = wa_item-belnr.
    gv_budat = wa_item-budat.
    gv_duedate = wa_item-duedate.
    gv_day = wa_item-chk_day.
    IF wa_item-dmbtr LT 0.
      gv_sign = '-'.
    ELSE.
      gv_sign = ''.
    ENDIF.
    gv_amt = abs( wa_item-dmbtr ).
    gv_stus_cq = wa_item-stus_cq.
    gv_cq_no = wa_item-cq_no.
    gv_remark = wa_item-remark.

    PERFORM f_write_form USING 'DETAIL' 'MAIN'.
    lv_count_line = lv_count_line + 1.
  ENDLOOP.




  gv_tot_amt = p_total.
  gv_forword = p_budat + 1.
  lv_check_line = lv_count_line / 30.
  IF lv_check_line GT 0. "lv_count_line  GT '32' AND lv_count_line LT '36'.

*            PERFORM f_write_form USING 'TOT' 'MAIN'.
    CALL FUNCTION 'CONTROL_FORM'
      EXPORTING
        command   = 'NEW-PAGE SECOND'
      EXCEPTIONS
        unstarted = 1
        unopened  = 1.

    PERFORM f_write_form USING 'TOT' 'FOOTER'.
    PERFORM f_write_form USING 'SIGN' 'SIGN'.
  ELSE.

    PERFORM f_write_form USING 'TOT' 'FOOTER'.
    PERFORM f_write_form USING 'SIGN' 'SIGN'.
  ENDIF.





ENDFORM.                    " F_MAP_DATA

*&---------------------------------------------------------------------*
*&      Form  post_cheq
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BELNR    text
*      -->P_BUDAT    text
*      -->P_REF      text
*----------------------------------------------------------------------*
FORM post_cheq USING p_belnr TYPE bkpf-belnr
                     p_budat TYPE bkpf-budat
               CHANGING p_ref TYPE c.

  DATA: lv_chk_count TYPE i.
  CLEAR: p_ref,lv_chk_count.

  SELECT augbl belnr
  INTO TABLE gt_post_chq
  FROM bsad
  WHERE augbl EQ p_belnr
  AND   augdt EQ p_budat
  AND blart NE 'DP'.

  LOOP AT gt_post_chq INTO gw_post_chq.
    lv_chk_count = lv_chk_count + 1.

    IF gw_post_chq-belnr+0(2) NE '43' AND gw_post_chq-belnr NE p_belnr.
      IF lv_chk_count EQ 1.
        p_ref = gw_post_chq-belnr.
      ELSE.
        CONCATENATE gw_post_chq-belnr p_ref  INTO p_ref SEPARATED BY ','.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "post_cheq

*&---------------------------------------------------------------------*
*&      Form  f_open_form
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FORMNAME  text
*      -->P_LANGUAGE  text
*      -->P_DEVICE  text
*      <--P_ST_ITCPP  text
*----------------------------------------------------------------------*
FORM f_open_form USING  p_formname TYPE c
                        p_language LIKE thead-tdspras
                        p_device   TYPE c
               CHANGING p_gw_itcpp LIKE itcpp.

  CLEAR p_gw_itcpp.

  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      device                      = p_device
      form                        = p_formname
      language                    = p_language
    IMPORTING
      result                      = p_gw_itcpp
    EXCEPTIONS
      canceled                    = 1
      device                      = 2
      form                        = 3
      options                     = 4
      unclosed                    = 5
      mail_options                = 6
      archive_error               = 7
      invalid_fax_number          = 8
      more_params_needed_in_batch = 9
      spool_error                 = 10
      OTHERS                      = 11.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " f_open_form


*&---------------------------------------------------------------------*
*&      Form  f_open_form
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FORMNAME  text
*      -->P_LANGUAGE  text
*      -->P_DEVICE  text
*      <--P_ST_ITCPP  text
*----------------------------------------------------------------------*
FORM f_open_form_otf USING  p_formname TYPE c
                        p_language LIKE thead-tdspras
                        p_device   TYPE c
               CHANGING p_gw_itcpp LIKE itcpp.





  CLEAR p_gw_itcpp.
  options-tdgetotf = 'X'.
  options-tdnoprint = 'X'.
  options-tdnewid  = 'X'.      "New spool
  options-tdimmed  = 'X'.      "Act immediately
  options-tdfinal  = 'X'.      "Close spool
*  options-tddest   = 'ZPDU'.   "Use PDFU as default


*  options-TDDEST = 'TF01'.

*  options-tdnewid = 'X'.
*  options-tdimmed = 'X'.
*  options-tdtitle = 'TITLE'.

  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      device                      = p_device
      form                        = p_formname
      language                    = p_language
      options                     = options
      dialog                      = ' '
    IMPORTING
      result                      = p_gw_itcpp
    EXCEPTIONS
      canceled                    = 1
      device                      = 2
      form                        = 3
      options                     = 4
      unclosed                    = 5
      mail_options                = 6
      archive_error               = 7
      invalid_fax_number          = 8
      more_params_needed_in_batch = 9
      spool_error                 = 10
      OTHERS                      = 11.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " f_open_form

*&---------------------------------------------------------------------*
*&      Form f_start_form
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FORMNAME  text
*      -->P_LANGUAGE  text
*      -->P_STARTPAGE  text
*      -->P_PROGRAM  text
*----------------------------------------------------------------------*
FORM f_start_form USING  p_formname  TYPE c
                         p_language  LIKE thead-tdspras
                         p_startpage TYPE c
                         p_program   LIKE sy-repid.

  CALL FUNCTION 'START_FORM'
    EXPORTING
      form        = p_formname
      language    = p_language
      startpage   = p_startpage
      program     = p_program
    EXCEPTIONS
      form        = 1
      format      = 2
      unended     = 3
      unopened    = 4
      unused      = 5
      spool_error = 6
      OTHERS      = 7.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " f_start_form

*&---------------------------------------------------------------------*
*&      Form  f_write_form
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ELEMENT  text
*      -->P_WINDOW  text
*----------------------------------------------------------------------*
FORM f_write_form USING  p_element TYPE c
                         p_window  TYPE c.



*IF p_element = 'TOT'.
*            CALL FUNCTION  'CONTROL_FORM'
*                EXPORTING  COMMAND   = 'NEW-PAGE SECOND'
*                EXCEPTIONS UNSTARTED = 1
*                           UNOPENED  = 1.
*ENDIF.


  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element                  = p_element
      window                   = p_window
    EXCEPTIONS
      element                  = 1
      function                 = 2
      type                     = 3
      unopened                 = 4
      unstarted                = 5
      window                   = 6
      bad_pageformat_for_print = 7
      spool_error              = 8
      OTHERS                   = 9.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " f_write_form

*&---------------------------------------------------------------------*
*&      Form  f_end_form
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_end_form.

  CALL FUNCTION 'END_FORM'
    EXCEPTIONS
      unopened                 = 1
      bad_pageformat_for_print = 2
      spool_error              = 3
      OTHERS                   = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " f_end_form
*&---------------------------------------------------------------------*
*&      Form  close_form
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_close_form.



  CALL FUNCTION 'CLOSE_FORM'
    EXCEPTIONS
      unopened                 = 1
      bad_pageformat_for_print = 2
      send_error               = 3
      spool_error              = 4
      OTHERS                   = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " f_close_form

*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_close_form_otf.

  DATA: itcpp LIKE itcpp.

  MOVE-CORRESPONDING options TO itcpp.

  CALL FUNCTION 'CLOSE_FORM'
    IMPORTING
      result                   = itcpp
    TABLES
      otfdata                  = otfdata[]
    EXCEPTIONS
      unopened                 = 1
      bad_pageformat_for_print = 2
      send_error               = 3
      spool_error              = 4
      OTHERS                   = 5.




  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " f_close_form
*--------------------------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_display_report .

  PERFORM f_fill_fieldcat.

  PERFORM f_prepare_layout.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_save                   = gc_save
*     is_variant               = g_variant
      i_default                = 'X'
      it_fieldcat              = gt_fieldcat
      is_layout                = gs_layout
*     it_fieldcat_lvc          = gt_fieldcat
*     is_layout_lvc            = gs_layout
      i_callback_pf_status_set = 'STATUS_SET'
      i_callback_user_command  = 'USERCOMMAND'
    TABLES
      t_outtab                 = gt_kna1
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.


ENDFORM.                    " F_DISPLAY_REPORT

*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_prepare_layout .
*  gs_layout-box_fieldname      = 'SEL'.
  gs_layout-zebra              = 'X'.     " striped pattern
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-info_fieldname    = 'LINE_COLOR'.
*  gs_layout-info_fname    = 'LINE_COLOR'.
*  gs_layout-STYLEFNAME = 'STYLE'.
ENDFORM.                    " F_PREPARE_LAYOUT

* ----------------------------------------------------
* Status
* ----------------------------------------------------
FORM status_set USING rt_extab TYPE slis_t_extab.
*break wantanee.

  SET PF-STATUS 'STANDARD_MAIL' EXCLUDING rt_extab.


ENDFORM.                    "status_set

*&---------------------------------------------------------------------*
*&      Form  F_FILL_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_fill_fieldcat .
  DATA: lv_column TYPE i.

  CLEAR gs_fieldcat.

*  ls_fcat-ref_tabname = 'GT_RESULT'.
  gs_fieldcat-fieldname   = 'CHECK'.
  gs_fieldcat-seltext_s   = 'Check'.
  gs_fieldcat-seltext_m   = 'Check'.
  gs_fieldcat-seltext_l   = 'Check'.
  gs_fieldcat-checkbox    = 'X'.
  gs_fieldcat-input       = 'X'.
  gs_fieldcat-edit        = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
*  ls_fcat-ref_tabname = 'GT_RESULT'.
  gs_fieldcat-fieldname   = 'KUNNR'.
  gs_fieldcat-seltext_s   = 'Customer Number'.
  gs_fieldcat-seltext_m   = 'Customer Number'.
  gs_fieldcat-seltext_l   = 'Customer Number'.
  APPEND gs_fieldcat TO gt_fieldcat.


  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname   = 'NAME1'.
  gs_fieldcat-seltext_s   = 'Name1'.
  gs_fieldcat-seltext_m   = 'Name1'.
  gs_fieldcat-seltext_l   = 'Name1'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname   = 'NAME2'.
  gs_fieldcat-seltext_s   = 'Name2'.
  gs_fieldcat-seltext_m   = 'Name2'.
  gs_fieldcat-seltext_l   = 'Name2'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname   = 'E_MAIL'.
  gs_fieldcat-seltext_s   = 'E-mail'.
  gs_fieldcat-seltext_m   = 'E-mail'.
  gs_fieldcat-seltext_l   = 'E-mail'.
  APPEND gs_fieldcat TO gt_fieldcat.
ENDFORM.                    " F_FILL_FIELDCAT


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
  BREAK wantanee.
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




  CASE i_ucomm.
    WHEN 'SMAIL'.
      BREAK wantanee.

      IF p_mail IS NOT INITIAL.
        SORT gt_kna1.
        SORT gt_bsid.
        SORT gt_bsad.
        LOOP AT gt_kna1 INTO gw_kna1 WHERE check EQ 'X' AND e_mail IS NOT INITIAL .
          PERFORM f_open_form_otf  USING gv_formid p_langu 'PRINTER'
                                 CHANGING gw_itcpp.
          CLEAR:  gv_kunnr,    "Customer Code
                  gv_name,     "Customer Name
                  gv_stras,    "Customer Address
                  gv_ort02,
                  gv_ort01,
                  gv_pstlz,
                  gv_telf1,
                  gv_sale,
                  gv_belnr,     "Document No.
                  gv_budat,     "Posting Date
                  gv_duedate,   "Due Date
                  gv_day,       "Due
                  gv_amt,       "Amount
                  gv_stus_cq,   "Status Cheques
                  gv_cq_no,     "Cheques
                  gv_spell_word,"total amt word
                  gv_remark,    "Remark
                  gv_date ,
                  gv_name_eng,
                  gv_sign.

          PERFORM f_start_form USING gv_formid p_langu 'FIRST' gv_repid.

          PERFORM f_print_form.

          PERFORM f_end_form.
          PERFORM f_close_form_otf.
          PERFORM f_send_mail_pdf.
        ENDLOOP.


*                PERFORM refersh.
*                call method ref_grid->refresh_table_display.
*                clear : ref_grid.
        MESSAGE s000(38) WITH 'Already send'.
      ENDIF.

    WHEN OTHERS.

  ENDCASE.
*


ENDFORM.                    "USERCOMMAND
*&---------------------------------------------------------------------*
*&      Form  F_CONVERT_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_convert_year .
  CLEAR gv_date_asof.
  gv_date_asof = p_budat.
  gv_date_asof+0(4) = gv_date_asof+0(4) + 543.
ENDFORM.                    " F_CONVERT_YEAR

FORM f_send_mail_pdf.


  DATA : l_tab_docs    LIKE TABLE OF docs WITH HEADER LINE,
         it_pdf_output LIKE TABLE OF tline WITH HEADER LINE,
         l_filesize    TYPE tst01-dsize,
         l_tab_lines   TYPE i,
         l_docdata     LIKE sodocchgi1,
         l_objpack     LIKE TABLE OF sopcklsti1 WITH HEADER LINE,
         l_reclist     LIKE TABLE OF somlreci1 WITH HEADER LINE,
         att_type      LIKE sopcklsti1-doc_type,
         l_objbin      LIKE TABLE OF solisti1 WITH HEADER LINE,
         l_objhead     LIKE TABLE OF solisti1 WITH  HEADER LINE,
         l_objtxt      LIKE TABLE OF solisti1 WITH HEADER LINE,
         lt_mailtxt    TYPE STANDARD TABLE OF soli      WITH HEADER LINE,

         l_objhex      LIKE TABLE OF solix WITH HEADER LINE,
         it_mess_att   LIKE TABLE OF solisti1 WITH HEADER LINE,
         gd_buffer     TYPE string,
         l_sub(50).






  CALL FUNCTION 'CONVERT_OTF_2_PDF'
    IMPORTING
      bin_filesize           = l_filesize
    TABLES
      otf                    = otfdata
      doctab_archive         = l_tab_docs
      lines                  = it_pdf_output
    EXCEPTIONS
      err_conv_not_possible  = 1
      err_otf_mc_noendmarker = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Transfer the 132-long strings to 255-long strings
  LOOP AT it_pdf_output.
    TRANSLATE it_pdf_output USING ' ~'.
    CONCATENATE gd_buffer it_pdf_output INTO gd_buffer.
  ENDLOOP.
  TRANSLATE gd_buffer USING '~ '.
  DO.
    it_mess_att = gd_buffer.
    APPEND it_mess_att.
    SHIFT gd_buffer LEFT BY 255 PLACES.
    IF gd_buffer IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.
  l_objbin[] = it_mess_att[].



*Set contents
  CLEAR l_objtxt.
  l_objtxt-line = '<body lang=EN-US style=''tab-interval:36.0pt''>'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.
  l_objtxt-line = '<p class=MsoNormal><span style=''font-size:14.0pt;font-family:"Calibri","sans-serif"; color:#0F243E''>'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.
*  lf_mailtxt-line = '<b>Dear</b> Sir/Madam,<br><br>'.
  CONCATENATE '<b>เรียน ผู้จัดการแผนกบัญชี</b>' ',<br><br>' INTO l_objtxt-line SEPARATED BY space.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.

  PERFORM f_convert_date_text USING p_budat.

  CONCATENATE 'เพื่อประโยชน์ในการตรวจสอบบัญชี จึงขอความร่วมมือมายังท่านเพื่อตอบกลับยอดคงเหลือ ณ วันที่ '  lv_day_text lv_month_text gv_date_asof+0(4) INTO l_objtxt-line SEPARATED BY space.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.

  l_objtxt-line = '<br>'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.

  PERFORM f_convert_date_text USING p_redate.

  CONCATENATE 'ดังแสดงในเอกสารแนบ  ภายในวันที่ '  lv_day_text lv_month_text gv_date_asof+0(4) INTO l_objtxt-line SEPARATED BY space.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.

  l_objtxt-line = '<br>'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.

  l_objtxt-line = 'โดยขอให้ท่าน:'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.
  l_objtxt-line = '<br>'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.

  l_objtxt-line = 'ทำเครื่องหมาย   ในช่อง “ถูกต้อง” หรือ ช่อง “ไม่ถูกต้อง” สำหรับทุกรายการ ลงชื่อ และประทับตราสำคัญบริษัท ในส่วนล่างของเอกสารที่แนบมานี้'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.
  l_objtxt-line = '<br>'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.


  l_objtxt-line = 'หากยอดคงเหลือตามที่ปรากฏตามเอกสารแนบนี้ไม่ถูกต้อง โปรดระบุรายการที่ไม่ถูกต้อง หรือแนบรายละเอียดรายการของท่าน'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.

  l_objtxt-line = '<br>'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.

  l_objtxt-line = 'โดยแนบไฟล์ Scan หนังสือยืนยันยอด กลับมาทาง e-mail : credit@daikin.co.th'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.

  l_objtxt-line = '<br>'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.


  l_objtxt-line = 'หากมีข้อสงสัยในการตอบกลับหนังสือยืนยันยอด สามารถสอบถามได้ที่เจ้าหน้าที่แผนกสินเชื่อของบริษัทฯ  โทร. 028383200 ต่อ 3209 3210'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.
  l_objtxt-line = '<br>'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.


  l_objtxt-line = '<br>'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.

  l_objtxt-line = 'ขอขอบคุณที่ให้ความร่วมมือ'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.

  l_objtxt-line = '<br>'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.

  l_objtxt-line = 'แผนกสินเชื่อ'.
  APPEND l_objtxt TO l_objtxt.
  CLEAR l_objtxt.

  CLEAR l_objpack-transf_bin.

  l_objpack-head_start = 1.
  l_objpack-head_num = 0.
  l_objpack-body_start = 1.
  l_objpack-body_num = sy-tabix.
  l_objpack-doc_type = 'HTM'.
  APPEND l_objpack.

* Create Message Attachment
  att_type = 'PDF'.
  DESCRIBE TABLE l_objbin LINES l_tab_lines.
  READ     TABLE l_objbin INDEX l_tab_lines.
  l_objpack-doc_size = ( l_tab_lines - 1 ) * 255 + strlen( l_objbin ).
  l_objpack-transf_bin = 'X'.
  l_objpack-head_start = 1.
  l_objpack-head_num   = 0.
  l_objpack-body_start = 1.
  l_objpack-body_num   = l_tab_lines.
  l_objpack-doc_type   = att_type.
  l_objpack-obj_name   = 'objectname'.
  l_objpack-obj_descr  = 'หนังสือยืนยันยอดคงค้าง'.
  APPEND l_objpack.

* Create subject
  l_docdata-obj_langu = sy-langu.
  l_docdata-obj_name  = 'objname'.
  l_sub = 'หนังสือยืนยันยอดคงค้าง'.
  l_docdata-obj_descr = l_sub.

* Create receiver list
  l_reclist-receiver = gw_kna1-e_mail.
  l_reclist-rec_type = 'U'.
  APPEND l_reclist.





  BREAK wantanee.
*  CLEAR: l_objtxt.
*
*  l_objtxt-line = '<body lang=EN-US style=''tab-interval:36.0pt''>'.
*  append l_objtxt to lt_mailtxt.
*  clear l_objtxt.
*
*  l_objtxt-line = '<p class=MsoNormal><span style=''font-size:14.0pt;font-family:"Calibri","sans-serif"; color:#0F243E''>'.
*  append l_objtxt to lt_mailtxt.
*  clear l_objtxt.
**  lf_mailtxt-line = '<b>Dear</b> Sir/Madam,<br><br>'.
*  concatenate '<b> เรียน ผู้จัดการฝ่ายบัญชี </b>' ',<br><br>' into l_objtxt-line separated by space.
*  append l_objtxt to lt_mailtxt.
*  clear l_objtxt.
*
*  l_objtxt-line = '<br />'.
*  append l_objtxt to lt_mailtxt.
*   clear l_objtxt.


*

* Send Message
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = l_docdata
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      packing_list               = l_objpack
      object_header              = l_objhead
      contents_bin               = l_objbin
      contents_txt               = l_objtxt  "Text
*     contents_hex               = l_objhex
      receivers                  = l_reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.




  WAIT UP TO 2 SECONDS.
  SUBMIT rsconn01 WITH mode = 'INT'
                WITH output = ' '
                AND RETURN.

ENDFORM.

*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_convert_date_text USING p_conver_date.

  DATA : lv_day(2)   TYPE c,
         lv_month(2) TYPE c,
         lv_year(2)  TYPE c,
         lv_stext    TYPE t247-ktx,
         lv_ltext    TYPE t247-ltx.

  DATA: lv_day_i TYPE i.

  CALL FUNCTION 'HR_IN_GET_DATE_COMPONENTS'
    EXPORTING
      idate                         = p_conver_date
    IMPORTING
      day                           = lv_day
      month                         = lv_month
      year                          = lv_year
      stext                         = lv_stext
      ltext                         = lv_ltext
*     USERDATE                      = USERDATE
    EXCEPTIONS
      input_date_is_initial         = 1
      text_for_month_not_maintained = 2.
  lv_day_i = lv_day.
  lv_day_text = lv_day_i.

  IF lv_month EQ '01'.
    lv_month_text = 'มกราคม'.

  ELSEIF  lv_month EQ '02'.
    lv_month_text = 'กุมภาพันธ์'.
  ELSEIF  lv_month EQ '03'.
    lv_month_text = 'มีนาคม'.

  ELSEIF  lv_month EQ '04'.
    lv_month_text = 'เมษายน'.
  ELSEIF  lv_month EQ '05'.
    lv_month_text = 'พฤษภาคม'.
  ELSEIF  lv_month EQ '06'.
    lv_month_text = 'มิถุนายน'.
  ELSEIF  lv_month EQ '07'.
    lv_month_text = 'กรกฎาคม'.
  ELSEIF  lv_month EQ '08'.
    lv_month_text = 'สิงหาคม'.
  ELSEIF  lv_month EQ '09'.
    lv_month_text = 'กันยายน'.
  ELSEIF  lv_month EQ '10'.
    lv_month_text = 'ตุลาคม'.
  ELSEIF  lv_month EQ '11'.
    lv_month_text = 'พฤศจิกายน'.
  ELSEIF  lv_month EQ '12'.
    lv_month_text = 'ธันวาคม'.
  ENDIF.

  PERFORM f_convert_year.

ENDFORM.                    " f_close_form

*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_mail_sender.

  DATA: lv_persnumber TYPE usr21-persnumber,
        lv_addrnumber TYPE usr21-addrnumber,
        lv_smtp_addr  TYPE adr6-smtp_addr.

  SELECT SINGLE persnumber addrnumber
    INTO (lv_persnumber,lv_addrnumber)
    FROM usr21
    WHERE bname EQ sy-uname.

  IF lv_persnumber IS NOT INITIAL AND
     lv_addrnumber IS NOT INITIAL.

    SELECT SINGLE smtp_addr
      INTO lv_smtp_addr
      FROM adr6
      WHERE persnumber = lv_persnumber
      AND addrnumber = lv_addrnumber.

    gv_mail_sendor =  lv_smtp_addr.

  ENDIF.

ENDFORM.                    " f_close_form

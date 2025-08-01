*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0310
*  Creation Date      : 22.05.2024
*  Author             : b.chiewsarikij
*  Add-on ID          : ZMMF001
*  Description        :
*  Purpose            :
*  Copied from        : <<Reference Program>>
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT zsdsmmr0310 MESSAGE-ID zsdsmm02.
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES: vbak,
        vbrk,
        vbfa,
        cdhdr,
        cdpos,
        ser01,
        objk,
        equi_addr,
        bgmkobj,
        adrc,
        likp.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES: BEGIN OF typ_itab,

         equnr           TYPE objk-equnr,      "Equiment
         matnr           TYPE objk-matnr,      "material
         sernr           TYPE objk-sernr,      "Serial
         date_key        TYPE equz-erdat,     "Date Key warranty
         gwldt           TYPE bgmkobj-gwldt,   "Customer start warranty
         gwlen           TYPE bgmkobj-gwlen,   "Customer end warranty
         name1(200)      TYPE c,               "Customer name warranty
         name2(200)      TYPE c,               "Customer name warranty "Add by Wantanee 20120118
         building        TYPE adrc-building,    "Building Code
         floor           TYPE adrc-floor,       "floor
         roomnumber      TYPE adrc-roomnumber,  "room
         str_suppl1      TYPE adrc-str_suppl1,                 " Street 2
         str_suppl2      TYPE adrc-str_suppl2,                 " Street 3
         street          TYPE adrc-street,     " Street/House number
         str_suppl3      TYPE adrc-str_suppl3,                 " Street 4
         location        TYPE adrc-location,                   " Street 5
         city2           TYPE adrc-city2,       " District
         home_city       TYPE adrc-home_city,       " Other City
         city1           TYPE adrc-city1,      "City
         post_code1      TYPE adrc-post_code1, "Post code
         tel_number      TYPE adrc-tel_number, "Telephon
         ernam           TYPE equz-ernam,      "user name
         inv_no          TYPE vbrk-vbeln,      "invoice number
         fkdat           TYPE vbrk-fkdat,      "invoice date
         lief_nr         TYPE ser01-lief_nr,   "deliver number
         vbeln           TYPE vbak-vbeln,      "Sale order
         dea_code        TYPE kna1-kunnr,      "Dealer code
         dea_name_e(200) TYPE c,             "Dealer Name eng
         dea_name_t(200) TYPE c,             "Dealer Name tha
         sale_code       TYPE vbpa-parnr,      "sale code
         sale_name(200)  TYPE c,               "Sale name
         vkbur           TYPE vbak-vkbur,      "Sale Offcie
         vkbur_txt       TYPE tvkbt-bezei,     "Sale office detail
         vkgrp           TYPE vbak-vkgrp,      "Sale Group
         vkgrp_txt       TYPE tvgrt-bezei,     "Sale Group - Text
         lfdat           TYPE likp-lfdat,      "Delivery Date
         netpr           TYPE vbap-netpr,
         mwsbp           TYPE vbap-mwsbp,
         netwr           TYPE vbap-netwr,      " inc vat
         ltext(255)      TYPE c,
         prdha           TYPE mara-prdha,      "Product hierarchy
         prctr           TYPE lips-prctr,      "Profit Center
       END OF typ_itab.

TYPES: BEGIN OF typ_equi,
         equnr    TYPE objk-equnr,      "Equiment
         matnr    TYPE objk-matnr,      "material
         sernr    TYPE objk-sernr,      "Serial
         date_key TYPE equz-erdat,     "Date Key warranty
         ernam    TYPE equz-ernam,      "user name
         iloan    TYPE iloa-iloan,      "Loc/Acct Assignment
         adrnr    TYPE iloa-adrnr,      "Address Number
         lief_nr  TYPE ser01-lief_nr,   "Delivery
       END OF typ_equi.

TYPES: BEGIN OF typ_enduser,
         equnr      TYPE equz-equnr, "  equipment
         adrnr      TYPE adrc-addrnumber,      "Address Number
         name1      TYPE adrc-name1,      "Name
         name2      TYPE adrc-name2,                      "Name2
         city1      TYPE adrc-city1,                      "city1
         city2      TYPE adrc-city2,                      "city2
         post_code1 TYPE adrc-post_code1, "Post_code
         street     TYPE adrc-street,     "Street
         tel_number TYPE adrc-tel_number, "tel
         str_suppl3 TYPE adrc-str_suppl3, "Street4  "Add by Wantanee 20120118
         building   TYPE adrc-building,    "Building Code
         floor      TYPE adrc-floor,       "floor
         roomnumber TYPE adrc-roomnumber,  "room
         str_suppl1 TYPE adrc-str_suppl1,                 " Street 2
         str_suppl2 TYPE adrc-str_suppl2,                 " Street 3
         location   TYPE adrc-location,                   " Street 5
         home_city  TYPE adrc-home_city,       " Other City
       END OF typ_enduser.


TYPES: BEGIN OF typ_vbrk,
         vbelv      TYPE vbfa-vbelv,      "Deliver number
         posnv      TYPE vbfa-posnv,      "Delivery items
         vbeln      TYPE vbrk-vbeln,      "Invoice Number
         posnr      TYPE vbfa-posnn,      "Invoice Items
         aubel      TYPE vbrp-aubel,      "Sale Order
         aupos      TYPE vbrp-aupos,      "Item
         fkdat      TYPE vbrk-fkdat,      "invoice date
         vbtyp_n    TYPE vbfa-vbtyp_n,    "Type
         matnr      TYPE vbrp-matnr,
         netpr      TYPE vbap-netpr,
         kwmeng     TYPE vbap-kwmeng,    "Sale order quantity
         mwsbp      TYPE vbap-mwsbp,     "vat
         ltext(255) TYPE c,
       END OF typ_vbrk.
TYPES: BEGIN OF typ_likp,
         vbelv   TYPE vbfa-vbelv,      "Deliver number
         vbeln   TYPE likp-vbeln,      "Invoice Number
         vbtyp_n TYPE vbfa-vbtyp_n,    "Type

       END OF typ_likp.

TYPES: BEGIN OF typ_vbak,
         vbelv TYPE vbfa-vbelv,      "Deliver number
         vbeln TYPE vbak-vbeln,      "Invoice Number
         kunnr TYPE vbak-kunnr,      "Dealer Code
         pernr TYPE vbpa-pernr,      "Sale code
         adrnr TYPE vbpa-adrnr,      "Address number
         vkbur TYPE vbak-vkbur,      "Sale Office
         vkgrp TYPE vbak-vkgrp,      "Sale Group
       END OF typ_vbak.



TYPES: BEGIN OF typ_adrc,
         addrnumber TYPE adrc-addrnumber,      "Address Number
         name1      TYPE adrc-name1,      "Name
         name2      TYPE adrc-name2,                      "Name2
         nation     TYPE adrc-nation,                     "Name2
       END OF typ_adrc.

*
TYPES: BEGIN OF typ_pa0001,
         pernr LIKE pa0001-pernr, "Sale no
         ename LIKE pa0001-ename,  "sale name

       END OF typ_pa0001.

TYPES: BEGIN OF typ_tvkbt,
         vkbur TYPE vbak-vkbur,     "Sale office
         bezei TYPE tvkbt-bezei,    "Sale office detail
       END OF typ_tvkbt.

TYPES: BEGIN OF typ_tvgrt,
         vkgrp TYPE vbak-vkgrp,          "Sale Group
         bezei TYPE tvgrt-bezei,         "Sale Group - Text
       END OF typ_tvgrt.

TYPES : BEGIN OF typ_cn,
          vbelv TYPE vbfa-vbelv,
          posnv TYPE vbfa-posnv,
          matnr TYPE vbrp-matnr,
          sernr TYPE equi-sernr,
        END OF typ_cn.

TYPE-POOLS : vrm.
TYPE-POOLS: slis.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA: gv_formname  TYPE tdsfname,
      gv_fm_name   TYPE rs38l_fnam,
      gv_tabix     TYPE syst-tabix,
      gv_last_item TYPE syst-tabix.

DATA: v_pos TYPE i.
DATA:
  gt_fieldcat TYPE slis_t_fieldcat_alv,
  gt_events   TYPE slis_t_event,
  gt_heading  TYPE slis_t_listheader,
  gt_sort     TYPE slis_t_sortinfo_alv,
  gt_layout   TYPE slis_layout_alv,
  gt_exit(1)  TYPE c,
  gt_variant  TYPE disvariant,
  gx_variant  TYPE disvariant,
  ref_grid    TYPE REF TO cl_gui_alv_grid. "Refresh

DATA: xscreen(1)   TYPE c.                "Output on printer or screen
DATA: lv_fm        TYPE rs38l_fnam.

DEFINE def_list_head.
  CLEAR: lw_listline.
  lw_listline-typ  = &1.
  lw_listline-key  = &2.
  lw_listline-info = &3.
  APPEND lw_listline TO lt_listhead.
END-OF-DEFINITION.
*-> internal tables
DATA: gt_itab    TYPE STANDARD TABLE OF typ_itab,
      gt_equi    TYPE STANDARD TABLE OF typ_equi,
      gt_enduser TYPE STANDARD TABLE OF typ_enduser,
      gt_vbrk    TYPE STANDARD TABLE OF typ_vbrk,
      gt_vbak    TYPE STANDARD TABLE OF typ_vbak,
      gt_adrc    TYPE STANDARD TABLE OF typ_adrc,
      gt_pa0001  TYPE STANDARD TABLE OF typ_pa0001,
      gt_likp    TYPE STANDARD TABLE OF typ_likp,
      gt_tvkbt   TYPE STANDARD TABLE OF typ_tvkbt,
      gt_tvgrt   TYPE STANDARD TABLE OF typ_tvgrt.


DATA: wa_itab    TYPE typ_itab,
      wa_equi    TYPE typ_equi,
      wa_enduser TYPE typ_enduser,
      wa_vbrk    TYPE typ_vbrk,
      wa_vbak    TYPE typ_vbak,
      wa_adrc    TYPE typ_adrc,
      wa_pa0001  TYPE typ_pa0001,
      wa_likp    TYPE typ_likp,
      wa_tvkbt   TYPE typ_tvkbt,
      wa_tvgrt   TYPE typ_tvgrt.

DATA : i_cn TYPE STANDARD TABLE OF typ_cn,
       w_cn LIKE LINE OF i_cn.
*-> range
*-> work areas
*-> variables
*-> reference

*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS: gc_charx TYPE c VALUE 'X',
           gc_repid TYPE zsdscac001-repid VALUE 'ZSDSMMR0310'.

CONSTANTS: gc_lang_en TYPE c      VALUE 'E',
           gc_lang_th TYPE c      VALUE 'T',
           gc_mark_x  TYPE c      VALUE 'X'.
CONSTANTS: gc_mask_time TYPE char8  VALUE '__:__',
           gc_mask_date TYPE char10 VALUE '__.__.____'.
CONSTANTS: gc_tmty_header TYPE char1  VALUE 'S'.

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
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
  SELECT-OPTIONS: so_equnr FOR objk-equnr ,
                  so_matnr FOR objk-matnr ,
                  so_sernr FOR objk-sernr.

  PARAMETERS p_all AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.
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
  IF so_matnr[] IS INITIAL AND so_equnr[] IS INITIAL.
    MESSAGE s000(38) WITH 'Please enter material or equipment!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM get_data_v2.
  PERFORM f_read_text TABLES gt_vbrk.
  PERFORM map_data.

*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
*-> process data
  IF NOT gt_itab IS INITIAL.

    PERFORM sub_main.
  ELSE.
    MESSAGE s000.
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
**&      Form  SUB_MAIN
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
FORM sub_main .

  GET TIME.
  PERFORM: build_catalog CHANGING gt_fieldcat[],
           build_event   USING gt_events,
           build_layout  USING gt_layout,
           build_sort,
           display_output.

ENDFORM.                    "sub_main
**&---------------------------------------------------------------------*
**&      Form  GET_DATA
**&---------------------------------------------------------------------*
FORM get_data .
  SELECT  a~equnr
          a~matnr
          a~sernr
          b~erdat
          b~ernam
          d~iloan
          d~adrnr
         f~lief_nr
  INTO TABLE gt_equi
  FROM objk AS a INNER JOIN equz AS b
                 ON ( a~equnr = b~equnr )
                 INNER JOIN iloa AS d
                 ON ( b~iloan = d~iloan )
                 INNER JOIN ser01 AS f
                 ON ( a~obknr = f~obknr
                 AND  f~vbtyp = 'J' )
   WHERE a~matnr IN so_matnr
    AND a~sernr IN so_sernr
    AND a~taser EQ 'SER01'.
*    AND d~adrnr NE ''.
  SORT gt_equi.
  DELETE ADJACENT DUPLICATES FROM gt_equi.

  IF gt_equi[] IS NOT INITIAL.

    SELECT  addrnumber
            name1
            name2
*            city1
*            city2
*            post_code1
*            street
*            tel_number
*            str_suppl3
*            building
*            floor roomnumber
*            str_suppl1
*            str_suppl2
*            location
*            home_city
    INTO TABLE gt_enduser
    FROM adrc
    FOR ALL ENTRIES IN gt_equi
    WHERE addrnumber = gt_equi-adrnr
      AND nation = ''.

    SELECT a~vbelv
           a~vbeln
           b~fkdat
           a~vbtyp_n
     INTO CORRESPONDING FIELDS OF TABLE gt_vbrk
     FROM vbfa AS a INNER JOIN vbrk AS b ON ( a~vbeln = b~vbeln )
     FOR ALL ENTRIES IN gt_equi
     WHERE a~vbelv = gt_equi-lief_nr
     AND   a~vbtyp_n =  'M'.

    SORT gt_vbrk.
    DELETE ADJACENT DUPLICATES FROM gt_vbrk.

    SORT gt_likp.
    DELETE ADJACENT DUPLICATES FROM gt_likp.

    SELECT a~vbeln a~vbelv b~kunnr d~pernr f~adrnr
           b~vkbur b~vkgrp  "Add by Wantanee 20120921
     INTO TABLE gt_vbak
     FROM vbfa AS a INNER JOIN vbak AS b ON ( a~vbelv = b~vbeln )
                    INNER JOIN vbpa AS d ON ( a~vbelv = d~vbeln
                                         AND  d~parvw = 'VE' )
                    INNER JOIN vbpa AS f ON ( a~vbelv = f~vbeln
                                         AND  f~parvw = 'AG' )
     FOR ALL ENTRIES IN gt_equi
     WHERE a~vbeln = gt_equi-lief_nr
      AND  a~vbtyp_n = 'J'
      AND  a~vbtyp_v = 'C'.

    SORT gt_vbak.
    DELETE ADJACENT DUPLICATES FROM gt_vbak.

    SELECT pernr ename
    INTO TABLE gt_pa0001
    FROM pa0001
    FOR ALL ENTRIES IN gt_vbak
    WHERE pernr EQ gt_vbak-pernr.

    SELECT vkbur bezei
    INTO TABLE gt_tvkbt
    FROM tvkbt
    WHERE spras EQ 'E'.

    SELECT vkgrp bezei
    INTO TABLE gt_tvgrt
    FROM tvgrt
    WHERE spras EQ 'E'.

  ENDIF.


ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  get_data_v2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data_v2 .

  SELECT    a~equnr
            a~matnr
            a~sernr
           f~lief_nr
    INTO CORRESPONDING FIELDS OF TABLE gt_equi
    FROM objk AS a
                   INNER JOIN ser01 AS f
                   ON ( a~obknr = f~obknr
                   AND  f~vbtyp = 'J' )
     WHERE a~equnr IN so_equnr
      AND a~matnr IN so_matnr
      AND a~sernr IN so_sernr
      AND a~taser EQ 'SER01'.

  SORT gt_equi.
  DELETE ADJACENT DUPLICATES FROM gt_equi.

  IF gt_equi[] IS NOT INITIAL.

    SELECT
            equz~equnr
            adrc~addrnumber
            adrc~name1
            adrc~name2
            adrc~city1
            adrc~city2
            adrc~post_code1
            adrc~street
            adrc~tel_number
            adrc~str_suppl3
            adrc~building
            adrc~floor roomnumber
            adrc~str_suppl1
            adrc~str_suppl2
            adrc~location
            adrc~home_city
    INTO TABLE gt_enduser
    FROM equz
    INNER JOIN iloa ON equz~iloan EQ iloa~iloan
    INNER JOIN adrc ON adrc~addrnumber EQ iloa~adrnr
    FOR ALL ENTRIES IN gt_equi
    WHERE equz~equnr = gt_equi-equnr
    AND nation = ''.


* Get Invoice
    SELECT a~vbelv
           a~posnv
           a~vbeln
           a~posnn AS posnr
           c~aubel
           c~aupos
           b~fkdat
           a~vbtyp_n
           c~matnr
           p~netpr
           p~kwmeng
           p~mwsbp
     INTO TABLE gt_vbrk
     FROM vbfa AS a INNER JOIN vbrk AS b ON a~vbeln = b~vbeln
     INNER JOIN vbrp AS c ON a~vbeln EQ c~vbeln AND  a~posnn EQ c~posnr
     INNER JOIN vbap AS p ON c~aubel EQ p~vbeln AND c~aupos EQ p~posnr
     FOR ALL ENTRIES IN gt_equi
     WHERE a~vbelv = gt_equi-lief_nr
     AND   a~vbtyp_n =  'M'
     AND c~matnr EQ gt_equi-matnr.
    SORT gt_vbrk.

* Get serial CN
*    SELECT vbfa~vbelv
*           vbfa~posnv
*           vbrp~matnr
*           equi~sernr
*      INTO CORRESPONDING FIELDS OF TABLE i_cn
*      FROM vbrp
*      INNER JOIN vbfa ON vbfa~vbelv EQ vbrp~vbeln AND vbfa~posnv EQ vbrp~posnr AND vbfa~vbtyp_n EQ 'T'
*      INNER JOIN ser01 ON ser01~lief_nr EQ vbfa~vbeln AND ser01~posnr EQ vbfa~posnn
*      INNER JOIN objk ON ser01~obknr EQ objk~obknr
*      INNER JOIN equi ON objk~equnr EQ equi~equnr
*      FOR ALL entries IN gt_vbrk
*      WHERE vbrp~vbeln EQ gt_vbrk-vbeln
*      AND vbrp~posnr EQ gt_vbrk-posnr.

    SELECT vbfa~vbelv
           vbfa~posnv
           lips~matnr
           equi~sernr
      INTO CORRESPONDING FIELDS OF TABLE i_cn
      FROM lips
      INNER JOIN vbfa ON lips~vbeln EQ vbfa~vbelv AND lips~posnr EQ vbfa~posnv AND vbfa~vbtyp_n EQ 'T'
      INNER JOIN ser01 ON ser01~lief_nr EQ vbfa~vbeln AND ser01~posnr EQ vbfa~posnn
      INNER JOIN objk ON ser01~obknr EQ objk~obknr
      INNER JOIN equi ON objk~equnr EQ equi~equnr
      FOR ALL ENTRIES IN gt_equi
      WHERE lips~vbeln EQ gt_equi-lief_nr
      AND equi~sernr EQ gt_equi-sernr.

*    SORT gt_vbrk BY vbelv.
*    DELETE ADJACENT DUPLICATES FROM gt_vbrk COMPARING vbelv.

    SELECT a~vbeln a~vbelv
           b~kunnr d~pernr
           f~adrnr
           b~vkbur b~vkgrp  "Add by Wantanee 20120921
     INTO TABLE gt_vbak
     FROM vbfa AS a INNER JOIN vbak AS b ON ( a~vbelv = b~vbeln )
                    INNER JOIN vbpa AS d ON ( a~vbelv = d~vbeln
                                         AND  d~parvw = 'VE' )
                    INNER JOIN vbpa AS f ON ( a~vbelv = f~vbeln
                                         AND  f~parvw = 'AG' )
     FOR ALL ENTRIES IN gt_equi
     WHERE a~vbeln = gt_equi-lief_nr
      AND  a~vbtyp_n = 'J'
      AND  a~vbtyp_v = 'C'.

    SORT gt_vbak.
    DELETE ADJACENT DUPLICATES FROM gt_vbak.

    SELECT pernr ename
    INTO TABLE gt_pa0001
    FROM pa0001
    FOR ALL ENTRIES IN gt_vbak
    WHERE pernr EQ gt_vbak-pernr.

    SELECT vkbur bezei
    INTO TABLE gt_tvkbt
    FROM tvkbt
    WHERE spras EQ 'E'.

    SELECT vkgrp bezei
    INTO TABLE gt_tvgrt
    FROM tvgrt
    WHERE spras EQ 'E'.

  ENDIF.



ENDFORM.                    " GET_DATA
**&---------------------------------------------------------------------*
**&      Form  Map Data
**&---------------------------------------------------------------------*
FORM map_data .
  DATA: lv_salesername(100) TYPE c,
        lv_salename(30)     TYPE c.
  SORT gt_enduser BY equnr.

  DATA : BEGIN OF ls_mara,
           matnr TYPE mara-matnr,
           prdha TYPE mara-prdha,
         END OF ls_mara.
  DATA lt_mara LIKE TABLE OF ls_mara.

  DATA : BEGIN OF ls_lips,
           vbeln TYPE lips-vbeln,
           prctr TYPE lips-prctr,
         END OF ls_lips.
  DATA lt_lips LIKE TABLE OF ls_lips.

  CONSTANTS lc_true TYPE c VALUE 'X'.

  IF i_cn[] IS NOT INITIAL AND p_all NE lc_true.
    SORT gt_equi BY lief_nr matnr sernr.
    FIELD-SYMBOLS : <fscn> LIKE LINE OF i_cn[].
    LOOP AT i_cn ASSIGNING <fscn>.
      DELETE gt_equi WHERE lief_nr = <fscn>-vbelv AND matnr = <fscn>-matnr AND sernr = <fscn>-sernr.
    ENDLOOP.
  ENDIF.

*--------------------------------------------------------------------*
* Add by Jakarin 21.11.2016 T41K924893
*--------------------------------------------------------------------*
  IF gt_equi IS NOT INITIAL.
    PERFORM f_get_profit_prdha TABLES lt_lips
                                      lt_mara.
  ENDIF.
*--------------------------------------------------------------------*
* End Add by Jakarin 21.11.2016 T41K924893
*--------------------------------------------------------------------*

  LOOP AT gt_equi INTO wa_equi.
    CLEAR: wa_itab,ls_lips,ls_mara.
    wa_itab-equnr = wa_equi-equnr.
    wa_itab-matnr = wa_equi-matnr.
    wa_itab-sernr = wa_equi-sernr.
    wa_itab-date_key = wa_equi-date_key.
    wa_itab-ernam = wa_equi-ernam.
    wa_itab-lief_nr = wa_equi-lief_nr.
*--------------------------------------------------------------------*
* Add by Jakarin 21.11.2016 T41K924893
*--------------------------------------------------------------------*
    READ TABLE lt_lips INTO ls_lips
    WITH KEY vbeln = wa_equi-lief_nr.
    IF sy-subrc = 0.
      wa_itab-prctr = ls_lips-prctr.
    ENDIF.

    READ TABLE lt_mara INTO ls_mara
    WITH KEY matnr = wa_equi-matnr.
    IF sy-subrc = 0.
      wa_itab-prdha = ls_mara-prdha.
    ENDIF.
*--------------------------------------------------------------------*
* End Add by Jakarin 21.11.2016 T41K924893
*--------------------------------------------------------------------*
    READ TABLE gt_enduser INTO wa_enduser WITH KEY equnr = wa_equi-equnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_itab-name1 = wa_enduser-name1.
      wa_itab-name2 = wa_enduser-name2.
      wa_itab-street = wa_enduser-street.
      wa_itab-building = wa_enduser-building.
      wa_itab-floor = wa_enduser-floor.
      wa_itab-roomnumber = wa_enduser-roomnumber.
      wa_itab-str_suppl1 = wa_enduser-str_suppl1.
      wa_itab-str_suppl2 = wa_enduser-str_suppl2.
      wa_itab-location = wa_enduser-location.
      wa_itab-city2 = wa_enduser-city2.
      wa_itab-home_city = wa_enduser-home_city.
      wa_itab-city1 = wa_enduser-city1.
      wa_itab-post_code1 = wa_enduser-post_code1.
      wa_itab-tel_number = wa_enduser-tel_number.
    ENDIF.

    READ TABLE gt_vbrk INTO wa_vbrk WITH KEY vbelv = wa_equi-lief_nr matnr = wa_equi-matnr.
    IF sy-subrc EQ 0.
      wa_itab-inv_no = wa_vbrk-vbeln.
      wa_itab-fkdat = wa_vbrk-fkdat.
      wa_itab-netpr = wa_vbrk-netpr.
      wa_itab-mwsbp = wa_vbrk-mwsbp / wa_vbrk-kwmeng.
      wa_itab-netwr = wa_itab-netpr + wa_itab-mwsbp.
      wa_itab-ltext = wa_vbrk-ltext.
    ENDIF.

    READ TABLE gt_vbak INTO wa_vbak WITH KEY vbelv = wa_equi-lief_nr.
    IF sy-subrc EQ 0.
      wa_itab-vbeln = wa_vbak-vbeln.
      wa_itab-dea_code = wa_vbak-kunnr.
      PERFORM get_customer USING    wa_vbak-kunnr wa_vbak-adrnr  CHANGING wa_itab-dea_name_t wa_itab-dea_name_e.

      wa_itab-sale_code = wa_vbak-pernr.


      wa_itab-vkbur = wa_vbak-vkbur.
      wa_itab-vkgrp = wa_vbak-vkgrp.
      PERFORM get_salegroup_saleoffice USING wa_vbak-vkbur wa_vbak-vkgrp
                                       CHANGING wa_itab-vkbur_txt wa_itab-vkgrp_txt.

    ENDIF.

    READ TABLE gt_pa0001 INTO wa_pa0001 WITH KEY pernr = wa_itab-sale_code.
    IF sy-subrc EQ 0.

      SPLIT wa_pa0001-ename  AT space INTO: lv_salesername lv_salename.
      CONCATENATE lv_salename lv_salesername INTO wa_itab-sale_name SEPARATED BY space.
    ENDIF.

*      PERFORM get_datewarranty USING wa_itab-equnr CHANGING wa_itab-gwldt wa_itab-gwlen.

    SELECT SINGLE lfdat INTO (wa_itab-lfdat) FROM likp WHERE vbeln EQ wa_itab-lief_nr.

    APPEND wa_itab TO gt_itab.
  ENDLOOP.






ENDFORM.                    " MAP_DATA

*
**&---------------------------------------------------------------------*
**&      Form  CUSTOMER
**&---------------------------------------------------------------------*
FORM get_datewarranty USING    p_equnr TYPE equz-equnr
                  CHANGING VALUE(p_gwldt) TYPE bgmkobj-gwldt
                           VALUE(p_gwlen) TYPE bgmkobj-gwlen.
  DATA: lv_equnr(22) TYPE c.

  CLEAR: lv_equnr,p_gwldt,p_gwlen.

  CONCATENATE 'IE' p_equnr INTO lv_equnr.

  SELECT SINGLE gwldt gwlen
  INTO (p_gwldt,p_gwlen)
  FROM bgmkobj
  WHERE j_objnr EQ lv_equnr
    AND gaart EQ '1'.

ENDFORM. "

*
**&---------------------------------------------------------------------*
**&      Form  CUSTOMER
**&---------------------------------------------------------------------*
FORM get_customer USING    p_kunnr TYPE kna1-kunnr
                           p_adrnr TYPE kna1-adrnr
                  CHANGING VALUE(p_name_tha) TYPE c
                           VALUE(p_name_eng) TYPE c .

  CLEAR:p_name_tha,p_name_eng.

  SELECT addrnumber name1 name2 nation
  INTO TABLE gt_adrc
  FROM adrc
  WHERE addrnumber = p_adrnr.
*            AND nation = 'I'.
  IF p_kunnr NE 'OT01'.
    READ TABLE gt_adrc INTO wa_adrc WITH KEY addrnumber = p_adrnr
                                             nation = 'I'.
    IF sy-subrc EQ 0.
      CONCATENATE wa_adrc-name1 wa_adrc-name2 INTO p_name_eng.
    ENDIF.
  ENDIF.

  READ TABLE gt_adrc INTO wa_adrc WITH KEY addrnumber = p_adrnr
                                          nation = ''.
  IF sy-subrc EQ 0.
    CONCATENATE wa_adrc-name1 wa_adrc-name2 INTO p_name_tha.
  ENDIF.


ENDFORM. "


*&---------------------------------------------------------------------*
*&      Form  GET_SALEGROUP_SALEOFFICE  "Add by Wantanee 20120921
*&---------------------------------------------------------------------*
FORM get_salegroup_saleoffice USING p_vkbur TYPE vbak-vkbur
                                    p_vkgrp TYPE vbak-vkgrp
                              CHANGING VALUE(p_vkbur_txt) TYPE tvkbt-bezei
                                       VALUE(p_vkgrp_txt) TYPE tvgrt-bezei .

  CLEAR: p_vkbur_txt,p_vkgrp_txt.

  READ TABLE gt_tvkbt INTO wa_tvkbt WITH KEY vkbur = p_vkbur.
  IF sy-subrc EQ 0.
    p_vkbur_txt = wa_tvkbt-bezei.
  ENDIF.
  READ TABLE gt_tvgrt INTO wa_tvgrt WITH KEY vkgrp = p_vkgrp.
  IF sy-subrc EQ 0.
    p_vkgrp_txt = wa_tvgrt-bezei.
  ENDIF.

ENDFORM.                    "get_salegroup_saleoffice
*
*
**&---------------------------------------------------------------------*
**&      Form  BUILD_CATALOG
**&---------------------------------------------------------------------*
FORM build_catalog   USING rt_fieldcat TYPE slis_t_fieldcat_alv.
  DATA: wa_fieldcat TYPE slis_fieldcat_alv,
        pos         TYPE i VALUE 1.
  CLEAR wa_fieldcat.


* Equipment
  PERFORM append_fieldcat USING 'EQUNR' 'OBJK' 'EQUNR'  'Equipment.'
                                 space  space  space '' gt_fieldcat[].
* material
  PERFORM append_fieldcat USING 'MATNR' 'OBJK' 'MATNR'  'material.'
                                 space  space  space '' gt_fieldcat[].
* Serial
  PERFORM append_fieldcat USING 'SERNR' 'OBJK' 'SERNR'  'Serial'
                                 space  space  space '' gt_fieldcat[].
** Date Key warranty
*  PERFORM append_fieldcat USING 'DATE_KEY' 'EQUZ' 'ERDAT'  'Date Key warranty'
*                                 space  space  space '' gt_fieldcat[].
** Customer start warranty
*  PERFORM append_fieldcat USING 'GWLDT' 'BGMKOBJ' 'GWLDT'  'Customer start warranty'
*                                 space  space  space '' gt_fieldcat[].
** Customer start warranty
*  PERFORM append_fieldcat USING 'GWLEN' 'BGMKOBJ' 'GWLEN'  'Customer end warranty'
*                                 space  space  space '' gt_fieldcat[].
* Customer name warranty
  PERFORM append_fieldcat USING 'NAME1' '' ''  'Customer name'
                                 space  space  space '' gt_fieldcat[].
** Customer name warranty
*  PERFORM append_fieldcat USING 'NAME2' '' ''  'Customer name 2'
*                                 space  space  space '' gt_fieldcat[].
** Address 1
*  PERFORM append_fieldcat USING 'STREET' '' ''  'Street/House number'
*                                 space  space  space '' gt_fieldcat[].
** Address 2
*  PERFORM append_fieldcat USING 'ADDR2' '' ''  'Address 2'
*                                 space  space  space '' gt_fieldcat[].


**Add by Wantanee 20120921
** Building
*  PERFORM append_fieldcat USING 'BUILDING' '' ''  'Building'
*                                 space  space  space '' gt_fieldcat[].
** Floor
*  PERFORM append_fieldcat USING 'FLOOR' '' ''  'Floor'
*                                 space  space  space '' gt_fieldcat[].
** 'Room Number
*  PERFORM append_fieldcat USING 'ROOMNUMBER' '' ''  'Room Number'
*                                 space  space  space '' gt_fieldcat[].
** Street 2
*  PERFORM append_fieldcat USING 'STR_SUPPL1' '' ''  'Street 2'
*                                 space  space  space '' gt_fieldcat[].
** Street 3
*  PERFORM append_fieldcat USING 'STR_SUPPL2' '' ''  'Street 3'
*                                 space  space  space '' gt_fieldcat[].
** Street/House number
*  PERFORM append_fieldcat USING 'STREET' '' ''  'Street/House number'
*                                 space  space  space '' gt_fieldcat[].
** Street 4
*  PERFORM append_fieldcat USING 'STR_SUPPL3' '' ''  'Street 4'
*                                 space  space  space '' gt_fieldcat[].
** Street 5
*  PERFORM append_fieldcat USING 'LOCATION' '' ''  'Street 5'
*                                 space  space  space '' gt_fieldcat[].
** District
*  PERFORM append_fieldcat USING 'CITY2' '' ''  'District'
*                                 space  space  space '' gt_fieldcat[].
** Other City
*  PERFORM append_fieldcat USING 'HOME_CITY' '' ''  'Other City'
*                                 space  space  space '' gt_fieldcat[].

**Sale order
*  PERFORM append_fieldcat USING 'VBELN' 'VBAK' 'VBELN'  'Sale order'
*                                 space  space  space '' gt_fieldcat[].
*Dealer code
  PERFORM append_fieldcat USING 'DEA_CODE' 'KNA1' 'KUNNR'  'Dealer code'
                                 space  space  space '' gt_fieldcat[].
*Dealer Name
  PERFORM append_fieldcat USING 'DEA_NAME_T' '' ''  'Dealer Name TH'
                                 space  space  space '' gt_fieldcat[].
**Dealer Name
*  PERFORM append_fieldcat USING 'DEA_NAME_E' '' ''  'Dealer Name Eng'
*                                 space  space  space '' gt_fieldcat[].
*End Add by Wantanee 20120921
** City
*  PERFORM append_fieldcat USING 'CITY1' 'ADRC' 'CITY1'  'City'
*                                 space  space  space '' gt_fieldcat[].
** Post code
*  PERFORM append_fieldcat USING 'POST_CODE1' 'ADRC' 'POST_CODE1'  'Post code'
*                                 space  space  space '' gt_fieldcat[].
** Telephon
*  PERFORM append_fieldcat USING 'TEL_NUMBER' 'ADRC' 'TEL_NUMBER'  'Telephone'
*                                 space  space  space '' gt_fieldcat[].
* invoice number
  PERFORM append_fieldcat USING 'INV_NO' 'VBRK' 'VBELN'  'Invoice'
                                 space  space  space '' gt_fieldcat[].
* invoice date
  PERFORM append_fieldcat USING 'FKDAT' 'VBRK' 'FKDAT'  'invoice date'
                                 space  space  space '' gt_fieldcat[].

* Price per Unit
  PERFORM append_fieldcat USING 'NETPR' 'VBAP' 'NETPR'  'Unit Price'
                                 space  space  space '' gt_fieldcat[].

* Vat
  PERFORM append_fieldcat USING 'MWSBP' 'VBAP' 'MWSBP'  'Vat'
                                 space  space  space '' gt_fieldcat[].

* Net Amount
  PERFORM append_fieldcat USING 'NETWR' 'VBAP' 'NETWR'  'Net Price'
                                 space  space  space '' gt_fieldcat[].

* deliver number
  PERFORM append_fieldcat USING 'LIEF_NR' 'SER01' 'LIEF_NR'  'Delivery '
                                 space  space  space '' gt_fieldcat[].
* delivery Date
  PERFORM append_fieldcat USING 'LFDAT' '' ''  'Delivery Date '
                                 space  space  space '' gt_fieldcat[].

*sale code
  PERFORM append_fieldcat USING 'SALE_CODE' 'VBPA' 'PARNR'  'Sale code'
                                 space  space  space '' gt_fieldcat[].
*sale code
  PERFORM append_fieldcat USING 'SALE_NAME' '' ''  'Sale Name '
                                 space  space  space '' gt_fieldcat[].

*Add by Wantanee 20120921
*Sale Offcie
  PERFORM append_fieldcat USING 'VKBUR' '' ''  'Sale Office'
                                 space  space  space '' gt_fieldcat[].
*Sale Office Text
  PERFORM append_fieldcat USING 'VKBUR_TXT' '' ''  'Sale Office Text'
                                 space  space  space '' gt_fieldcat[].
*Sale Group
  PERFORM append_fieldcat USING 'VKGRP' '' ''  'Sale Group'
                                 space  space  space '' gt_fieldcat[].
*Sale Group Text
  PERFORM append_fieldcat USING 'VKGRP_TXT' '' ''  'Sale Group Text'
                                 space  space  space '' gt_fieldcat[].
** user name
*  PERFORM append_fieldcat USING 'ERNAM' 'EQUZ' 'ERNAM'  'User name'
*                                 space  space  space '' gt_fieldcat[].

* Text
  PERFORM append_fieldcat USING 'LTEXT' '' ''  'Text (Project)'
                                 space  space  space '' gt_fieldcat[].

*End Add by Wantanee 20120921

  "Add by Jakarin 20161121
  PERFORM append_fieldcat USING 'PRDHA' '' ''  'Product Hierarchy'
                                 space  space  space '' gt_fieldcat[].

  PERFORM append_fieldcat USING 'PRCTR' '' ''  'Profit Center'
                                 space  space  space '' gt_fieldcat[].
  "Add by Jakarin 20161121

ENDFORM.                    " BUILD_CATALOG
**&---------------------------------------------------------------------*
**&      Form  BUILD_EVENT
**&---------------------------------------------------------------------*
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
**&---------------------------------------------------------------------*
**&      Form  BUILD_LAYOUT
**&---------------------------------------------------------------------*
FORM build_layout  USING p_gtyp_layout TYPE slis_layout_alv.
  p_gtyp_layout-window_titlebar = sy-title.
  p_gtyp_layout-zebra = 'X'.
  p_gtyp_layout-colwidth_optimize = 'X'.
  p_gtyp_layout-info_fieldname =      'LINE_COLOR'. "Add by Wantanee 20110701



ENDFORM.                    " BUILD_LAYOUT
**&---------------------------------------------------------------------*
**&      Form  BUILD_SORT
**&---------------------------------------------------------------------*
FORM build_sort .
  DATA: wa_sort TYPE slis_sortinfo_alv.
  CLEAR gt_sort[].

ENDFORM.                    " BUILD_SORT
**&---------------------------------------------------------------------*
**&      Form  DISPLAY_OUTPUT
**&---------------------------------------------------------------------*
FORM display_output .
  DATA variant LIKE disvariant.
  variant-report = gc_repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = gc_repid
      i_callback_user_command = 'USER_COMMAND'
*     i_callback_pf_status_set = 'SUB_DISP_ALV_PFSTATUS'  "Refresh
      is_layout               = gt_layout
      it_fieldcat             = gt_fieldcat
      i_save                  = 'A'
      is_variant              = gt_variant
      it_sort                 = gt_sort
      it_events               = gt_events
    TABLES
      t_outtab                = gt_itab
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DISPLAY_OUTPUT
**&---------------------------------------------------------------------*
**&      Form  APPEND_FIELDCAT
**&---------------------------------------------------------------------*
FORM append_fieldcat  USING  p_field      "Field name
*                            p_table      "Table name
                             p_reftable   "Reference Table name
                             p_reffield   "Reference Field name
*                            p_colpos     "Col position
                             p_coltxt     "Col Text(for specify)
                             p_dosum      "Sum total
                             p_cfieldname "currency
                             p_no_zero    "no zero
                             p_color      "color
*                             p_checkbox
                             p_it_fieldcat TYPE slis_t_fieldcat_alv.

  DATA: wa_infieldcat   TYPE slis_fieldcat_alv,
        v_coltxt_length TYPE i.

  ADD 1 TO v_pos.

  wa_infieldcat-fieldname      = p_field.
* wa_infieldcat-tabname        = 'I_TAB'.
  wa_infieldcat-ref_tabname    = p_reftable.
  wa_infieldcat-ref_fieldname  = p_reffield.
  wa_infieldcat-col_pos        = v_pos.
  wa_infieldcat-do_sum         = p_dosum.
*  wa_infieldcat-checkbox       = '1' .
  " Add by wantanee 20110701
  IF NOT p_no_zero IS INITIAL.
    wa_infieldcat-no_zero = p_no_zero.
  ENDIF.
  IF NOT p_cfieldname IS INITIAL.
    wa_infieldcat-cfieldname = p_cfieldname.
  ENDIF.
*
  IF p_color = '1'.
    wa_infieldcat-emphasize = 'C110'.
  ELSEIF p_color = '2' .
    wa_infieldcat-emphasize = 'C310'.
  ELSEIF p_color = '3' .
    wa_infieldcat-emphasize = 'C510'.
  ELSEIF p_color = '4' .
    wa_infieldcat-emphasize = 'C710'.
  ELSEIF p_color = '5' .
    wa_infieldcat-emphasize = 'C200'.
  ELSEIF p_color = '6' .
    wa_infieldcat-emphasize = 'C400'.
  ELSEIF p_color = '7' .
    wa_infieldcat-emphasize = 'C500'.
  ENDIF.
  " End Add by wantanee 20110701
* If we need to specify text, don't need to derive from data dictionary
* program will check length and define width of the colum
  IF NOT p_coltxt IS INITIAL.
    v_coltxt_length = strlen( p_coltxt ).

    IF v_coltxt_length > 20.
      wa_infieldcat-ddictxt   = 'L'. "Long text
      wa_infieldcat-seltext_l = p_coltxt.
    ELSEIF v_coltxt_length > 10.
      wa_infieldcat-ddictxt   = 'M'. "Medium Text
      wa_infieldcat-seltext_m = p_coltxt.
    ELSE.
      wa_infieldcat-ddictxt   = 'S'. "Short Text
      wa_infieldcat-seltext_s = p_coltxt.
    ENDIF.
    wa_infieldcat-reptext_ddic = p_coltxt.
  ENDIF.
  APPEND wa_infieldcat TO p_it_fieldcat.

ENDFORM.                    " APPEND_FIELDCAT
**&---------------------------------------------------------------------*
**&      Form  REPORT_HEADER
**&---------------------------------------------------------------------*
FORM report_header.
  DATA: lt_listhead TYPE slis_t_listheader,
        lw_listline TYPE slis_listheader.
  DATA: lv_text(256) TYPE c.
  DATA: lv_date(10) TYPE c,
        lv_time(5)  TYPE c.

* execution date & time
  CLEAR: lv_date, lv_time.
  WRITE sy-datum TO lv_date USING EDIT MASK gc_mask_date.
  WRITE sy-uzeit TO lv_time USING EDIT MASK gc_mask_time.

  CLEAR lv_text.
  lv_text = 'Report Check Serial'.
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
**&---------------------------------------------------------------------*
**&      Form  SUB_DISP_ALV_PFSTATUS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM sub_disp_alv_pfstatus USING rt_extab TYPE slis_t_extab.
*
** Copy From FunctionGroup : SALV - GuiStatus : STANDARD
*  SET PF-STATUS 'STATUS001'.
*
*ENDFORM.                    " SUB_DISP_ALV_PFSTATUS

FORM user_command USING r_ucomm TYPE sy-ucomm
                        r_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN '&IC1'. " Double Click
      CASE r_selfield-fieldname.
        WHEN 'MATNR' OR 'SERNR'.
          READ TABLE gt_itab INDEX r_selfield-tabindex INTO wa_itab.
          IF sy-subrc EQ 0.
            SET PARAMETER ID: 'MAT'  FIELD wa_itab-matnr,
                              'SER'  FIELD wa_itab-sernr.
            CALL TRANSACTION 'IQ03' AND SKIP FIRST SCREEN.
          ENDIF.
      ENDCASE.

  ENDCASE.

  CLEAR wa_itab.

ENDFORM.                    "USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  f_read_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT         text
*----------------------------------------------------------------------*
FORM f_read_text TABLES it STRUCTURE wa_vbrk.

  CHECK it[] IS NOT INITIAL.
  DATA : zltext  TYPE TABLE OF tline WITH HEADER LINE,
         wa      LIKE LINE OF it,
         theader TYPE thead-tdname,
         itmp    TYPE STANDARD TABLE OF typ_vbrk.

  FIELD-SYMBOLS : <fs> LIKE LINE OF it.
  itmp[] = it[].
  SORT itmp BY aubel.
  DELETE ADJACENT DUPLICATES FROM itmp COMPARING aubel.

  LOOP AT itmp ASSIGNING <fs>.
    theader = <fs>-aubel.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'Z002'
        language                = 'E'
        name                    = theader
        object                  = 'VBBK'
      TABLES
        lines                   = zltext
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7.

    LOOP AT zltext.
      CONCATENATE <fs>-ltext zltext-tdline INTO <fs>-ltext SEPARATED BY space.
    ENDLOOP.
    CLEAR zltext[].
    MODIFY it FROM <fs> TRANSPORTING ltext WHERE aubel EQ <fs>-aubel.
  ENDLOOP.
  UNASSIGN <fs>.

ENDFORM.                    "f_read_text
*&---------------------------------------------------------------------*
*&      Form  F_GET_PROFIT_PRDHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_LIPS  text
*      -->P_LT_MARA  text
*----------------------------------------------------------------------*
FORM f_get_profit_prdha TABLES ft_lips
                               ft_mara.

  DATA : BEGIN OF ls_mara,
           matnr TYPE mara-matnr,
           prdha TYPE mara-prdha,
         END OF ls_mara.
  DATA lt_mara LIKE TABLE OF ls_mara.

  DATA : BEGIN OF ls_lips,
           vbeln TYPE lips-vbeln,
           prctr TYPE lips-prctr,
         END OF ls_lips.
  DATA lt_lips LIKE TABLE OF ls_lips.


  SELECT matnr
         prdha
    FROM mara
    INTO TABLE lt_mara
    FOR ALL ENTRIES IN gt_equi
    WHERE matnr EQ gt_equi-matnr.

  SELECT vbeln
         prctr
    FROM lips
    INTO TABLE lt_lips
    FOR ALL ENTRIES IN gt_equi
    WHERE vbeln EQ gt_equi-lief_nr
      AND matnr EQ gt_equi-matnr.

  ft_lips[] = lt_lips[].
  ft_mara[] = lt_mara[].

ENDFORM.                    " F_GET_PROFIT_PRDHA

*&---------------------------------------------------------------------*
*& Report ZSDSSDR0080
*  Author             : Wantanee Prateep na thalang
*  Add-on ID          : ZSDI020
*  Description        : Export interface DO detail to SF
*  Purpose            :
*  Copied from        :  ZPSV_SF_DO
*  Restriction        :
*&---------------------------------------------------------------------*
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSSDR0080.
TYPE-POOLS : truxs,slis,icon.
*&---------------------------------------------------------------------*
*  DECLARATION
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*  TABLE
*&---------------------------------------------------------------------*
TABLES : likp,somlreci1.
*&---------------------------------------------------------------------*
*  TYPE
*&---------------------------------------------------------------------*
TYPES : BEGIN OF gy_result,
  vbeln      TYPE lips-vbeln, "DO
  posnr      TYPE lips-posnr, "Item
  qty        TYPE lips-LFIMG,
  matnr      TYPE lips-matnr,
  sernr      TYPE equi-sernr,
  datum      TYPE c LENGTH 8 , " ser01-datum, "DATE Create date
  kunnr      TYPE likp-kunnr,
  name1      TYPE c LENGTH 255,
  street     TYPE adrc-street,
  location   TYPE adrc-location,
  city2      TYPE adrc-city2,
  city1      TYPE adrc-city1,
  post_code1 TYPE adrc-post_code1,
  tel_number TYPE adrc-tel_number,
  kunag      TYPE likp-kunag,
  solna      TYPE c LENGTH 255,
  wadat      TYPE  c LENGTH 8 , " likp-wadat, "DATE Planned Goods Movement Date
  wadat_ist  TYPE  c LENGTH 8 , " likp-wadat_ist, "DATE Actual Goods Movement Date
*  serwa      TYPE csku-ltext,
*  serpm      TYPE csku-ltext,
  sap_qt     TYPE vbak-vbeln,
  PS_PSP_PNR TYPE C LENGTH 50,
  sfdc_qt    TYPE vbak-bname,
  so_no      TYPE vbak-vbeln,
  so_item    TYPE vbap-posnr,
  dokey      TYPE c LENGTH 50,


END OF gy_result.

TYPES : BEGIN OF gy_invd,
  matnr TYPE vbrp-matnr,
  sernr TYPE equi-sernr,
  fkdat TYPE vbrk-fkdat,
END OF gy_invd.

*&---------------------------------------------------------------------*
*  VARIABLE
*&---------------------------------------------------------------------*
DATA : gt_result TYPE TABLE OF gy_result,
       gs_result TYPE gy_result.

DATA : gt_invd TYPE TABLE OF gy_invd,
       gs_invd TYPE gy_invd.

DATA : gt_file_list TYPE TABLE OF rsfillst,
       gs_file_list TYPE rsfillst.

DATA : gt_fcat   TYPE slis_t_fieldcat_alv,
       gs_layout TYPE slis_layout_alv,
       gt_sort TYPE slis_t_sortinfo_alv,
       gs_sort TYPE slis_sortinfo_alv.
*&---------------------------------------------------------------------*
*&      Conect to FTP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
TYPES: BEGIN OF gy_files,
        filename TYPE char128,
END OF gy_files.

TYPES: BEGIN OF gy_cmdout,
  line(100) TYPE c,
END OF gy_cmdout.

DATA : gt_files TYPE gy_files OCCURS 0 WITH HEADER LINE,
       gs_files TYPE gy_files.

DATA : gs_file_move TYPE gy_files,
       gt_file_move TYPE TABLE OF gy_files.

DATA: path TYPE string,
      ls_txt_data TYPE string,
      lv_rows TYPE i,
      zlen TYPE i,
      file_n TYPE string,
      gs_file_name TYPE c LENGTH 255.

DATA: w_cmd(255) TYPE c,
      w_hdl TYPE i,
      w_key TYPE i VALUE 26101957,
      w_slen TYPE i,
      it_cmdout TYPE STANDARD TABLE OF gy_cmdout,
      wa_cmdout TYPE gy_cmdout.

DATA: gs_ftp_path TYPE e_dexcommfilepath.

DATA : BEGIN OF LS_HEADER,
           FIELD1 TYPE STRING,
           FIELD2 TYPE STRING,
           FIELD3 TYPE STRING,
           FIELD4 TYPE STRING,
           FIELD5 TYPE STRING,
           FIELD6 TYPE STRING,
           FIELD7 TYPE STRING,
           FIELD8 TYPE STRING,
           FIELD9 TYPE STRING,
           FIELD10 TYPE STRING,
           FIELD11 TYPE STRING,
           FIELD12 TYPE STRING,
           FIELD13 TYPE STRING,
           FIELD14 TYPE STRING,
           FIELD15 TYPE STRING,
           FIELD16 TYPE STRING,
           FIELD17 TYPE STRING,
           FIELD18 TYPE STRING,
           FIELD19 TYPE STRING,
           FIELD20 TYPE STRING,
           FIELD21 TYPE STRING,
           FIELD22 TYPE STRING,
           FIELD23 TYPE STRING,
           FIELD24 TYPE STRING,
         END OF LS_HEADER.
DATA LT_HEADER LIKE TABLE OF LS_HEADER.
DATA: CT_RESULT TYPE TABLE OF STRING.
  CONSTANTS : BEGIN OF LC_CON,
                FIELD1    TYPE STRING VALUE 'DeliveryNumber',
                FIELD2    TYPE STRING VALUE 'Item',
                FIELD3    TYPE STRING VALUE 'Quantity',
                FIELD4    TYPE STRING VALUE 'MaterialNo',
                FIELD5    TYPE STRING VALUE 'SerialNo',
                FIELD6    TYPE STRING VALUE 'CreateDate',
                FIELD7    TYPE STRING VALUE 'ShiptoCode',
                FIELD8    TYPE STRING VALUE 'ShiptoName',
                FIELD9    TYPE STRING VALUE 'ShiptoAddress',
                FIELD10    TYPE STRING VALUE 'ShiptoAddress1',
                FIELD11   TYPE STRING VALUE 'ShiptoAddress2',
                FIELD12    TYPE STRING VALUE 'ShiptoProvince',
                FIELD13    TYPE STRING VALUE 'ShiptoPostCode',
                FIELD14    TYPE STRING VALUE 'ShiptoTell',
                FIELD15    TYPE STRING VALUE 'SoldtoCode',
                FIELD16    TYPE STRING VALUE 'SoldtoName',
                FIELD17    TYPE STRING VALUE 'GoodsIssueDate',
                FIELD18    TYPE STRING VALUE 'ActualGoodsMovementDate',
                FIELD19    TYPE STRING VALUE 'SAPQuotationNo',
                FIELD20    TYPE STRING VALUE 'WBS',
                FIELD21   TYPE STRING VALUE 'SFQuotationNo',
                FIELD22   TYPE STRING VALUE 'SAPSONo',
                FIELD23   TYPE STRING VALUE 'SAPSOItem',
                FIELD24   TYPE STRING VALUE 'DOKey',
              END OF LC_CON.


*&-----------------------------------------------------------------------------------*
*& V A R I A B L E
*&-----------------------------------------------------------------------------------*
DATA : v_pos TYPE i .

DATA : gt_fieldcat TYPE slis_t_fieldcat_alv,
       gt_layout   TYPE slis_layout_alv,
       gt_events   TYPE slis_t_event.
*        gt_heading  TYPE slis_t_listheader.
*&---------------------------------------------------------------------*
*  RANGES
*&---------------------------------------------------------------------*
RANGES : gr_stat FOR jest-stat.
*&---------------------------------------------------------------------*
*  CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS : gc_true      TYPE c VALUE 'X',
            gc_repid       TYPE repid         VALUE 'ZSDSSDR0080',
            gc_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE'.
*&---------------------------------------------------------------------*
*  SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK block3 WITH FRAME TITLE text-003.
SELECT-OPTIONS : s_vbeln FOR likp-vbeln,
                 s_erdat FOR likp-wadat_ist,
                 s_credt FOR likp-erdat.

PARAMETERS: p_disp RADIOBUTTON GROUP gr1 ,                   " Download to WEB Server
*            p_local RADIOBUTTON GROUP gr1,
            p_idoc RADIOBUTTON GROUP gr1 DEFAULT 'X',
            p_path LIKE rlgrap-filename  LOWER CASE..
SELECTION-SCREEN END OF BLOCK block3.


*SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
*PARAMETERS : p_file LIKE rlgrap-filename DEFAULT 'IN\PRD\DO',
*             p_achi LIKE rlgrap-filename OBLIGATORY DEFAULT 'Archive',
*             p_name LIKE rlgrap-filename OBLIGATORY .
*SELECTION-SCREEN END OF BLOCK block1.


*&---------------------------------------------------------------------*
*  INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*  AT SELECTION-SCREEN OUTPUT
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM f_set_selection.
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




     IF p_disp IS NOT INITIAL.
       IF gt_result[] IS NOT INITIAL.
          PERFORM display_reprot.
       ELSE.
*    MESSAGE s000 DISPLAY LIKE 'E'.
       ENDIF.
     ELSE.
       PERFORM F_GET_RESULT.
     ENDIF.



*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_get_data.

  DATA : BEGIN OF ls_ser01,
    vbeln     TYPE lips-vbeln, "DO number
    posnr     TYPE lips-posnr, "DO Item
    lfimg     TYPE lips-lfimg, "QTY
    vbelv     TYPE lips-vbelv, "SO
    posnv     TYPE lips-posnv, "SO Item
    matnr     TYPE lips-matnr, "Material
    aufnr     TYPE lips-aufnr, "IO
    sernr     TYPE equi-sernr, "Serial
    datum     TYPE ser01-datum, "Date
    kunnr     TYPE likp-kunnr,  "Customer Code
    kunag     TYPE likp-kunag,  "Customer
    wadat     TYPE likp-wadat,  "Ship date
    wadat_ist TYPE likp-wadat_ist,
    vgbel     TYPE vbap-vgbel,  "QT
    vgpos     TYPE vbap-vgpos,  "QT Item
    erdat     TYPE likp-erdat,
    PS_PSP_PNR  TYPE lips-PS_PSP_PNR,
    bname     TYPE vbak-bname,
  END OF ls_ser01.
  DATA lt_ser01 LIKE TABLE OF ls_ser01.

  DATA : BEGIN OF ls_lips,
    vbeln     TYPE lips-vbeln, "DO number
    posnr     TYPE lips-posnr, "DO Item
    lfimg     TYPE lips-lfimg, "QTY
    vbelv     TYPE lips-vbelv, "SO
    posnv     TYPE lips-posnv, "SO Item
    matnr     TYPE lips-matnr, "Material
    aufnr     TYPE lips-aufnr, "IO
    kunnr     TYPE likp-kunnr,  "Customer Code
    kunag     TYPE likp-kunag,  "Customer
    wadat     TYPE likp-wadat,  "Ship date
    wadat_ist TYPE likp-wadat_ist,
    vgbel     TYPE vbap-vgbel,  "QT
    vgpos     TYPE vbap-vgpos,  "QT Item
    erdat     TYPE likp-erdat,
    PS_PSP_PNR  TYPE lips-PS_PSP_PNR,
    bname     TYPE vbak-bname,
  END OF ls_lips.
  DATA lt_lips LIKE TABLE OF ls_lips.

  DATA : BEGIN OF ls_vbpa,
    vbeln      TYPE vbpa-vbeln,
    parvw      TYPE vbpa-parvw,
    name1      TYPE adrc-name1,
    name2      TYPE adrc-name2,
    street     TYPE adrc-street,
    location   TYPE adrc-location,
    city2      TYPE adrc-city2,
    city1      TYPE adrc-city1,
    post_code1 TYPE adrc-post_code1,
    tel_number TYPE adrc-tel_number,
  END OF ls_vbpa.
  DATA lt_vbpa LIKE TABLE OF ls_vbpa.
  DATA lt_vbpa_lips LIKE TABLE OF ls_vbpa.
  DATA lv_vbeln_qt TYPE vbak-vbeln.
  DATA: GV_WBS(50) TYPE c.

*  IF sy-batch EQ 'X'.
*    CONCATENATE 'DO_' sy-datum sy-uzeit '.CSV' INTO p_name.
*  ENDIF.

  SELECT lips~vbeln
         lips~posnr
         lips~lfimg
         lips~vgbel
         lips~vgpos
         lips~matnr
         lips~aufnr
         equi~sernr
         ser01~datum
         likp~kunnr
         likp~kunag
         likp~wadat
         likp~wadat_ist
         vbap~vgbel
         vbap~vgpos
         likp~erdat
         lips~PS_PSP_PNR
         vbak~bname
*         aufk~ktext
    FROM       likp
    INNER JOIN lips  ON likp~vbeln  EQ lips~vbeln
*    INNER JOIN aufk  ON lips~aufnr  EQ aufk~aufnr
    INNER JOIN vbap  ON lips~vgbel  EQ vbap~vbeln AND
                        lips~vgpos  EQ vbap~posnr
    INNER JOIN vbak  ON lips~vgbel  EQ vbak~vbeln
    INNER JOIN ser01 ON lips~vbeln  EQ ser01~lief_nr AND
                        lips~posnr  EQ ser01~posnr AND
                        ser01~vbtyp EQ 'J'
    INNER JOIN objk  ON ser01~obknr EQ objk~obknr AND
                        objk~taser  EQ 'SER01'
    INNER JOIN equi  ON objk~equnr  EQ equi~equnr
    INTO TABLE lt_ser01
    WHERE likp~vbeln     IN s_vbeln
      AND likp~wadat_ist IN s_erdat
      AND likp~erdat     IN s_credt.



    SELECT lips~vbeln
         lips~posnr
         lips~lfimg
         lips~vgbel
         lips~vgpos
         lips~matnr
         lips~aufnr
         likp~kunnr
         likp~kunag
         likp~wadat
         likp~wadat_ist
         vbap~vgbel
         vbap~vgpos
         likp~erdat
         lips~PS_PSP_PNR
         vbak~bname
*         aufk~ktext
    FROM       likp
    INNER JOIN lips  ON likp~vbeln  EQ lips~vbeln
*    INNER JOIN aufk  ON lips~aufnr  EQ aufk~aufnr
    INNER JOIN vbap  ON lips~vgbel  EQ vbap~vbeln AND
                        lips~vgpos  EQ vbap~posnr
    INNER JOIN vbak  ON lips~vgbel  EQ vbak~vbeln
    INTO TABLE lt_lips
    WHERE likp~vbeln     IN s_vbeln
      AND likp~wadat_ist IN s_erdat
      AND likp~erdat     IN s_credt
      AND lips~serail NE 'Z002'.


  IF lt_ser01[] IS NOT INITIAL.

    SELECT vbpa~vbeln
           vbpa~parvw
           adrc~name1
           adrc~name2
           adrc~street
           adrc~location
           adrc~city2
           adrc~city1
           adrc~post_code1
           adrc~tel_number
      FROM vbpa
      INNER JOIN adrc ON vbpa~adrnr EQ adrc~addrnumber
      INTO TABLE lt_vbpa
      FOR ALL ENTRIES IN lt_ser01
      WHERE vbpa~vbeln EQ lt_ser01-vbeln
        AND ( vbpa~parvw EQ 'AG' OR
              vbpa~parvw EQ 'WE' ).


  ENDIF.
  IF lt_lips[] IS NOT INITIAL.

    SELECT vbpa~vbeln
           vbpa~parvw
           adrc~name1
           adrc~name2
           adrc~street
           adrc~location
           adrc~city2
           adrc~city1
           adrc~post_code1
           adrc~tel_number
      FROM vbpa
      INNER JOIN adrc ON vbpa~adrnr EQ adrc~addrnumber
      INTO TABLE lt_vbpa_lips
      FOR ALL ENTRIES IN lt_lips
      WHERE vbpa~vbeln EQ lt_lips-vbeln
        AND ( vbpa~parvw EQ 'AG' OR
              vbpa~parvw EQ 'WE' ).


  ENDIF.

  LOOP AT lt_lips INTO ls_lips.
      CLEAR: lv_vbeln_qt.
    gs_result-vbeln     = ls_lips-vbeln.
    gs_result-posnr     = ls_lips-posnr.
    gs_result-matnr     = ls_lips-matnr.
    IF ls_lips-erdat <> '00000000'.
       gs_result-datum     = ls_lips-erdat.
    ELSE.
       gs_result-datum     = ''.
    ENDIF.
    gs_result-kunnr     = ls_lips-kunnr.
    gs_result-kunag     = ls_lips-kunag.
    IF ls_lips-wadat <> '00000000'.
       gs_result-wadat     = ls_lips-wadat.
    ELSE.
       gs_result-wadat     = ''.
    ENDIF.
    IF ls_lips-wadat_ist <> '00000000'.
       gs_result-wadat_ist = ls_lips-wadat_ist.
    ELSE.
       gs_result-wadat_ist     = ''.
    ENDIF.

*    gs_result-PS_PSP_PNR     = ls_lips-PS_PSP_PNR.
    gs_result-qty       = ls_lips-lfimg.

  CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
    EXPORTING
      INPUT         = ls_lips-PS_PSP_PNR
   IMPORTING
     OUTPUT        = gs_result-PS_PSP_PNR
            .

    SELECT SINGLE vbelv
      INTO lv_vbeln_qt
      FROM vbfa
      WHERE vbtyp_v = 'B'
      AND vbeln = ls_lips-vbeln
      AND vbtyp_n = 'J'.

    gs_result-sap_qt = lv_vbeln_qt.

     SELECT SINGLE bname
      INTO gs_result-sfdc_qt
      FROM vbak
      WHERE vbeln EQ lv_vbeln_qt.

*    gs_result-sfdc_qt = ls_ser01-bname.

    READ TABLE lt_vbpa_lips INTO ls_vbpa
    WITH KEY vbeln = ls_lips-vbeln
             parvw = 'AG'.
    IF sy-subrc EQ 0.
      CONCATENATE ls_vbpa-name1 ls_vbpa-name2 INTO gs_result-solna SEPARATED BY space.
    ENDIF.

    READ TABLE lt_vbpa_lips INTO ls_vbpa
    WITH KEY vbeln = ls_lips-vbeln
             parvw = 'WE'.
    IF sy-subrc EQ 0.
      CONCATENATE ls_vbpa-name1 ls_vbpa-name2 INTO gs_result-name1 SEPARATED BY space.
      gs_result-street     = ls_vbpa-street.
      gs_result-location   = ls_vbpa-location.
      gs_result-city2      = ls_vbpa-city2.
      gs_result-city1      = ls_vbpa-city1.
      gs_result-post_code1 = ls_vbpa-post_code1.
      gs_result-tel_number = ls_vbpa-tel_number.
    ENDIF.
    gs_result-so_no = ls_lips-vbelv.  "SAP SO number
    gs_result-so_item = ls_lips-posnv. "SAP SO item
*    CONCATENATE gs_result-vbeln gs_result-posnr gs_result-matnr gs_result-sernr INTO gs_result-dokey.
    CONCATENATE gs_result-vbeln gs_result-posnr INTO gs_result-dokey.

    PERFORM f_alpha_out USING gs_result-sernr
                     CHANGING gs_result-sernr.

    APPEND gs_result TO gt_result.

    CLEAR : gs_result,ls_ser01,ls_vbpa.
  ENDLOOP.

  LOOP AT lt_ser01 INTO ls_ser01.
    CLEAR: lv_vbeln_qt.
    gs_result-vbeln     = ls_ser01-vbeln.
    gs_result-posnr     = ls_ser01-posnr.
    gs_result-matnr     = ls_ser01-matnr.
    gs_result-sernr     = ls_ser01-sernr.
    IF ls_lips-erdat <> '00000000'.
       gs_result-datum     = ls_ser01-erdat.
    ELSE.
       gs_result-datum     = ''.
    ENDIF.

    gs_result-kunnr     = ls_ser01-kunnr.
    gs_result-kunag     = ls_ser01-kunag.
    IF ls_ser01-wadat <> '00000000'.
       gs_result-wadat     = ls_ser01-wadat..
    ELSE.
       gs_result-wadat     = ''.
    ENDIF.
    IF ls_ser01-wadat_ist <> '00000000'.
       gs_result-wadat_ist = ls_ser01-wadat_ist.
    ELSE.
       gs_result-wadat_ist     = ''.
    ENDIF.

*    gs_result-PS_PSP_PNR     = ls_ser01-PS_PSP_PNR.

   CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
    EXPORTING
      INPUT         = ls_ser01-PS_PSP_PNR
   IMPORTING
     OUTPUT        = gs_result-PS_PSP_PNR
            .

    IF ls_ser01-sernr IS NOT INITIAL.
        gs_result-qty = 1.
    ELSE.
        gs_result-qty = ls_ser01-lfimg.
    ENDIF.



    SELECT SINGLE vbelv
      INTO lv_vbeln_qt
      FROM vbfa
      WHERE vbtyp_v = 'B'
      AND vbeln = ls_ser01-vbeln
      AND vbtyp_n = 'J'.

    gs_result-sap_qt = lv_vbeln_qt.

     SELECT SINGLE bname
      INTO gs_result-sfdc_qt
      FROM vbak
      WHERE vbeln EQ lv_vbeln_qt.

*    gs_result-sfdc_qt = ls_ser01-bname.

    READ TABLE lt_vbpa INTO ls_vbpa
    WITH KEY vbeln = ls_ser01-vbeln
             parvw = 'AG'.
    IF sy-subrc EQ 0.
      CONCATENATE ls_vbpa-name1 ls_vbpa-name2 INTO gs_result-solna SEPARATED BY space.
    ENDIF.

    READ TABLE lt_vbpa INTO ls_vbpa
    WITH KEY vbeln = ls_ser01-vbeln
             parvw = 'WE'.
    IF sy-subrc EQ 0.
      CONCATENATE ls_vbpa-name1 ls_vbpa-name2 INTO gs_result-name1 SEPARATED BY space.
      gs_result-street     = ls_vbpa-street.
      gs_result-location   = ls_vbpa-location.
      gs_result-city2      = ls_vbpa-city2.
      gs_result-city1      = ls_vbpa-city1.
      gs_result-post_code1 = ls_vbpa-post_code1.
      gs_result-tel_number = ls_vbpa-tel_number.
    ENDIF.
    gs_result-so_no = ls_ser01-vbelv.  "SAP SO number
    gs_result-so_item = ls_ser01-posnv. "SAP SO item
*    CONCATENATE gs_result-vbeln gs_result-posnr gs_result-matnr  gs_result-sernr INTO gs_result-dokey.
    CONCATENATE gs_result-vbeln gs_result-posnr gs_result-sernr INTO gs_result-dokey.

    PERFORM f_alpha_out USING gs_result-sernr
                     CHANGING gs_result-sernr.

    APPEND gs_result TO gt_result.

    CLEAR : gs_result,ls_ser01,ls_vbpa.
  ENDLOOP.

  SORT gt_result BY vbeln posnr.



ENDFORM.                    "f_get_data
*&-----------------------------------------------------------------------------------*
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
  PERFORM build_event USING gt_events[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = gc_repid
      i_callback_user_command = 'USER_COMMAND'
      i_save             = 'A'
*      is_layout          = gt_layout
      it_events          = gt_events[]
      it_fieldcat        = gt_fieldcat

    TABLES
      t_outtab           = gt_result
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " DISPLAY_REPROT

* Add by K 20110215
*---------------------------------------------------------------------*
*       FORM User_command
*---------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  IF r_ucomm = '&IC1'.  "Double click event

*   goto XD03 display Customer Master

*     READ TABLE gt_itab INTO gs_itab INDEX rs_selfield-tabindex.
*
*              SET: PARAMETER ID 'KUN'  FIELD gs_itab-kunnr,
*                      PARAMETER ID 'BUK'  FIELD p_bukrs  .
*              CALL TRANSACTION 'XD03'.

  ENDIF.

  CLEAR r_ucomm.

ENDFORM.                    "user_comman

* End 20110215

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
  gt_layout-zebra = 'X'.

ENDFORM.                    "build_layout

*&---------------------------------------------------------------------*
*&      Form  BUILD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_catalog .

  CLEAR : v_pos.


* DeliveryNumber
  PERFORM append_fieldcat USING 'VBELN'
                                ''
                                ''
                                'DeliveryNumber'
                                 space  space  space
                                 gt_fieldcat[].

* Item
  PERFORM append_fieldcat USING 'POSNR'
                                ''
                                ''
                                'Item'
                                 space  space  space
                                 gt_fieldcat[].

* QTY
  PERFORM append_fieldcat USING 'QTY'
                                ''
                                ''
                                'QTY'
                                 space  space  space
                                 gt_fieldcat[].

* MaterialNo
  PERFORM append_fieldcat USING 'MATNR'
                                ''
                                ''
                                'MaterialNo'
                                 space  space  space
                                 gt_fieldcat[].
* SerialNo
  PERFORM append_fieldcat USING 'SERNR'
                                ''
                                ''
                                'SerialNo'
                                 space  space  space
                                 gt_fieldcat[].
* CreateDate
  PERFORM append_fieldcat USING 'DATUM'
                                ''
                                ''
                                'CreateDate'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoCode
  PERFORM append_fieldcat USING 'KUNNR'
                                ''
                                ''
                                'ShiptoCode'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoName
  PERFORM append_fieldcat USING 'NAME1'
                                ''
                                ''
                                'ShiptoName'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoAddress
  PERFORM append_fieldcat USING 'STREET'
                                ''
                                ''
                                'ShiptoAddress'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoAddress1
  PERFORM append_fieldcat USING 'LOCATION'
                                ''
                                ''
                                'ShiptoAddress1'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoAddress2
  PERFORM append_fieldcat USING 'CITY2'
                                ''
                                ''
                                'ShiptoAddress2'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoProvince
  PERFORM append_fieldcat USING 'CITY1'
                                ''
                                ''
                                'ShiptoProvince'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoPostCode
  PERFORM append_fieldcat USING 'POST_CODEL'
                                ''
                                ''
                                'ShiptoPostCode'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoTell
  PERFORM append_fieldcat USING 'TEL_NUMBER'
                                ''
                                ''
                                'ShiptoTell'
                                 space  space  space
                                 gt_fieldcat[].

* SoldtoCode
  PERFORM append_fieldcat USING 'KUNAG'
                                ''
                                ''
                                'SoldtoCode'
                                 space  space  space
                                 gt_fieldcat[].


* SoldtoName
  PERFORM append_fieldcat USING 'SOLNA'
                                ''
                                ''
                                'SoldtoName'
                                 space  space  space
                                 gt_fieldcat[].

* GoodsIssueDate
  PERFORM append_fieldcat USING 'WADAT'
                                ''
                                ''
                                'GoodsIssueDate'
                                 space  space  space
                                 gt_fieldcat[].
* ActualGoodsMovementD
  PERFORM append_fieldcat USING 'WADAT_IST'
                                ''
                                ''
                                'ActualGoodsMovementD'
                                 space  space  space
                                 gt_fieldcat[].
* SAPQuotationNo
  PERFORM append_fieldcat USING 'SAP_QT'
                                ''
                                ''
                                'SAPQuotationNo'
                                 space  space  space
                                 gt_fieldcat[].
* WBS
  PERFORM append_fieldcat USING 'PS_PSP_PNR'
                                ''
                                ''
                                'WBS'
                                 space  space  space
                                 gt_fieldcat[].
* SFQuotationNo
  PERFORM append_fieldcat USING 'SFDC_QT'
                                ''
                                ''
                                'SFQuotationNo'
                                 space  space  space
                                 gt_fieldcat[].
* SAPSONo
  PERFORM append_fieldcat USING 'SO_NO'
                                ''
                                ''
                                'SAPSONo'
                                 space  space  space
                                 gt_fieldcat[].
*SAPSOItem
  PERFORM append_fieldcat USING 'SO_ITEM'
                                ''
                                ''
                                'SAPSOItem'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'DOKEY'
                                ''
                                ''
                                'DOKey'
                                 space  space  space
                                 gt_fieldcat[].

ENDFORM.             "BUILD_ATALOG

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
*&      Form  BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*----------------------------------------------------------------------*
FORM build_event  USING e03_lt_events TYPE slis_t_event.
  DATA: lw_event TYPE slis_alv_event.
* Read Event
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 1
    IMPORTING
      et_events   = e03_lt_events.
  READ TABLE e03_lt_events WITH KEY name = slis_ev_top_of_page
                           INTO lw_event.
*   READ TABLE e03_lt_events WITH KEY name = slis_ev_top_of_list
*                             INTO lw_event.
  IF sy-subrc = 0.
* register top of page event
    MOVE gc_top_of_page TO lw_event-form.
    APPEND lw_event TO e03_lt_events.
  ENDIF.
ENDFORM.                    " BUILD_EVENT

*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_of_page.
  PERFORM write_heading.
ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  WRITE_HEADING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_heading .
  DATA: t_header      TYPE   slis_t_listheader,
        wa_header     TYPE   slis_listheader.

  wa_header-typ  = 'S'.
  wa_header-key = 'Report Name : '.
  wa_header-info = 'SFDC DO'.
  APPEND wa_header TO t_header.
  CLEAR wa_header.


  wa_header-typ  = 'S'.
  wa_header-key = 'Report Date : '.
  CONCATENATE  sy-datum+6(2) '.'
               sy-datum+4(2) '.'
               sy-datum(4) INTO wa_header-info.   "todays date
  APPEND wa_header TO t_header.
  CLEAR: wa_header.

  wa_header-typ  = 'S'.
  wa_header-key = 'Report Time : '.
  WRITE: sy-uzeit TO wa_header-info.    "todays date
  APPEND wa_header TO t_header.
  CLEAR: wa_header.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header.

ENDFORM.                    " WRITE_HEADING
*&---------------------------------------------------------------------*
*&      Form  F_EXIT_ALPHA_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_NETWR  text
*----------------------------------------------------------------------*
FORM f_exit_alpha_output  CHANGING lv_netwr.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = lv_netwr
    IMPORTING
      output = lv_netwr.

ENDFORM.                    " F_EXIT_ALPHA_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_INSERT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_insert_data.

ENDFORM.                    " F_INSERT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_REANG_PRODH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LR_PRODH  text
*----------------------------------------------------------------------*
FORM f_get_reang_prodh  TABLES fr_prodh.

  RANGES lr_prodh FOR vbrp-prodh.

  CONSTANTS : BEGIN OF lc_trans,
    fg TYPE c LENGTH 18 VALUE 'TRAN SV   FG      ',
    sp TYPE c LENGTH 18 VALUE 'TRAN SV   SP      ',
  END OF lc_trans.

  CLEAR lr_prodh.
  lr_prodh-sign   = 'I'.
  lr_prodh-option = 'EQ'.
  lr_prodh-low    = lc_trans-fg.
  APPEND lr_prodh.

  CLEAR lr_prodh.
  lr_prodh-sign   = 'I'.
  lr_prodh-option = 'EQ'.
  lr_prodh-low    = lc_trans-sp.
  APPEND lr_prodh.

  fr_prodh[] = lr_prodh[].

ENDFORM.                    " F_GET_REANG_PRODH
*&---------------------------------------------------------------------*
*&      Form  F_SET_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_set_selection .
  LOOP AT SCREEN.
    IF screen-name = 'P_PASS'.
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.


  ENDLOOP.

ENDFORM.                    " F_SET_SELECTION
*&---------------------------------------------------------------------*
*&      Form  F_APHA_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_SERIAL_ITEM  text
*----------------------------------------------------------------------*
FORM f_apha_input USING lv_input.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_input
    IMPORTING
      output = lv_input.

ENDFORM.                    " F_APHA_INPUT

*&---------------------------------------------------------------------*
*&      Form  F_ALPHA_IN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_A  text
*      <--P_B  text
*----------------------------------------------------------------------*
FORM f_alpha_out  USING    lv_in  "TYPE  clike
                 CHANGING lv_out. "TYPE	clike.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = lv_in
    IMPORTING
      output = lv_out.

ENDFORM.                    " F_ALPHA_IN

*&---------------------------------------------------------------------*
*&      Form  F_GET_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_RESULT.

  LS_HEADER-FIELD1 = LC_CON-FIELD1.
  LS_HEADER-FIELD2 = LC_CON-FIELD2.
  LS_HEADER-FIELD3 = LC_CON-FIELD3.
  LS_HEADER-FIELD4 = LC_CON-FIELD4.
  LS_HEADER-FIELD5 = LC_CON-FIELD5.
  LS_HEADER-FIELD6 = LC_CON-FIELD6.
  LS_HEADER-FIELD7 = LC_CON-FIELD7.
  LS_HEADER-FIELD8 = LC_CON-FIELD8.
  LS_HEADER-FIELD9 = LC_CON-FIELD9.
  LS_HEADER-FIELD10 = LC_CON-FIELD10.
  LS_HEADER-FIELD11 = LC_CON-FIELD11.
  LS_HEADER-FIELD12 = LC_CON-FIELD12.
  LS_HEADER-FIELD13 = LC_CON-FIELD13.
  LS_HEADER-FIELD14 = LC_CON-FIELD14.
  LS_HEADER-FIELD15 = LC_CON-FIELD15.
  LS_HEADER-FIELD16 = LC_CON-FIELD16.
  LS_HEADER-FIELD17 = LC_CON-FIELD17.
  LS_HEADER-FIELD18 = LC_CON-FIELD18.
  LS_HEADER-FIELD19 = LC_CON-FIELD19.
  LS_HEADER-FIELD20 = LC_CON-FIELD20.
  LS_HEADER-FIELD21 = LC_CON-FIELD21.
  LS_HEADER-FIELD22 = LC_CON-FIELD22.
  LS_HEADER-FIELD23 = LC_CON-FIELD23.
  LS_HEADER-FIELD24 = LC_CON-FIELD24.
  APPEND LS_HEADER TO LT_HEADER.

  "Z_DEMO_GEN_FILE
  CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING I_HEADER    = LT_HEADER
                                                                I_ITEM      = GT_RESULT
                                                                I_SEPARATOR = '","'
                                                                I_START_END_VALUE = '"').
  PERFORM F_EXPORT_TO_SERVER.
ENDFORM.                    " F_GET_RESULT
*&---------------------------------------------------------------------*
*&      Form  F_export_to_server
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_EXPORT_TO_SERVER.
  DATA : LCL_FTP TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.

  DATA : LV_STATUS TYPE C.

  DATA : LV_FILE TYPE EVE_TT_STRING.

  DATA : LT_FILE TYPE EREC_T_STRING,
         LS_FILE LIKE LINE OF LT_FILE.
  DATA: LV_PATH(100) TYPE C.
  DATA: LV_PATH_FILE TYPE string.



*ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             =  'ZSDSSDR0080'
*                                              I_SINGLE_VALUE_FLAG = ABAP_TRUE "--> need 1 record use ABAP_TRUE need many record Comment this field
*                                              I_PARAM             =  sy-sysid     "LC_CON-SEPARATOR
**                                              I_PARAM_EXT      =
*                                    CHANGING  C_RETURN            = LV_PATH_FILE ).


*  IF sy-sysid = 'F36'.
*     LV_PATH_FILE = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/MyDaikin_DaikinPro/ZMMI006'.
*  ELSEIF sy-sysid = 'F46'.
*     LV_PATH_FILE = '/interface/Z_DS/SDS/20_OUTBOUND/MyDaikin_DaikinPro/ZMMI006'.
*  ELSE.
**     LV_PATH_FILE = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/DSS/ZMMI008'.
*  ENDIF.

LV_PATH_FILE = p_path.

  IF LCL_FTP IS NOT BOUND.
    CREATE OBJECT LCL_FTP.
  ENDIF.



  CONCATENATE 'DO_' SY-DATUM  sy-timlo '.csv' INTO LV_PATH.
      LV_STATUS = LCL_FTP->FTP_FILE_PUT( "I_WINDOW_PATH = 'DEALER_CLAIM/DEV/OUT'
*                                       I_AL11_PATH   = '/tmp'
                                        I_AL11_PATH   = LV_PATH_FILE
                                        I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
                                        I_DATA_SPIDER = 'X'
                                        IT_DATA       = CT_RESULT ).

*  LV_STATUS = LCL_FTP->FTP_FILE_PUT( I_WINDOW_PATH = 'MY_DAIKIN/DEV/OUT'
*                                     I_AL11_PATH   = '/tmp'
*                                     I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
*                                     I_USER        = 'ds'
*                                     I_PASS        = 'ds=20240521'
*                                     I_IP          = '172.31.136.250'
*                                     I_PORT        = '21'
*                                     IT_DATA       = CT_RESULT ).
  IF LV_STATUS EQ 'S'.
    " SUCCESS FTP FILE
  ELSE.
    " CANNOT FTP FILE
  ENDIF.
ENDFORM.                 " F_GET_RESULT

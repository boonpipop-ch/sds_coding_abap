*&---------------------------------------------------------------------*
*& Report ZSDSMMR0080
*&---------------------------------------------------------------------*
*  Creation Date      : 11.03.2024
*  Author             : Wantanee Prateep na thalang
*  Add-on ID          :
*  Description        : Report and Export data of DO,Return-DO to WH SMP
*  Purpose            :
*  Copied from        :  ZR_MM_WMS_SMP_DORE
*  Restriction        :
*&---------------------------------------------------------------------*
REPORT ZSDSMMR0080.

*---------DATA DEFINATION-------------------------*
*&-----------------------------------------------------------------------------------*
*& T A B L E S
*&-----------------------------------------------------------------------------------*
TABLES: ser01,likp,lips.


*&-----------------------------------------------------------------------------------*
*& D A T A
*&-----------------------------------------------------------------------------------*

TYPES: BEGIN OF gy_itab,
       vbeln        TYPE lips-vbeln, "DO
       posnr        TYPE lips-posnr, "DO Item
       matnr        TYPE lips-matnr, "Material
       lfimg        TYPE lips-lfimg,
       lgort        TYPE lips-lgort,   "Storage Location
       sernr        TYPE objk-sernr,   "Serial No.
       erdat        TYPE likp-erdat, "DO date
 END OF gy_itab.

TYPES: BEGIN OF gy_out_deli_text,
        vbeln(10)     TYPE c,
        posnr(6)      TYPE c,
        matnr(35)     TYPE c,
        lfimg(13)     TYPE c,
        erdat(8)      TYPE c,
        lgort(4)      TYPE c,
        sernr(18)     TYPE c,


END OF gy_out_deli_text.
TYPES : BEGIN OF  gy_likp_lips,
        vbeln     TYPE likp-vbeln,
        erdat     TYPE likp-erdat,
        erzet     TYPE likp-erzet,
        posnr     TYPE lips-posnr,
        matnr     TYPE lips-matnr,
        werks     TYPE lips-werks,
        lgort     TYPE lips-lgort,
        lfimg     TYPE lips-lfimg,
        serail    TYPE lips-serail,
END OF gy_likp_lips.
TYPES : BEGIN OF  gy_do,
        lief_nr   TYPE ser01-lief_nr,
END OF gy_do.

TYPES: BEGIN OF gy_ser01_objk,
       obknr     TYPE ser01-obknr,
       lief_nr   TYPE ser01-lief_nr,
       posnr     TYPE ser01-posnr,
       matnr     TYPE objk-matnr,
       sernr     TYPE objk-sernr,
END OF gy_ser01_objk.

*TYPES: BEGIN OF gy_output.
*        INCLUDE  STRUCTURE ZSDSSDS006.
*TYPES: END OF gy_output.



*&-----------------------------------------------------------------------------------*
*& I N T E R N A L   T A B L E S
*&-----------------------------------------------------------------------------------*
* Data for ALV display

TYPE-POOLS: slis.
TYPE-POOLS: truxs.
* The inputs that need to be passed to the REUSE_ALV function module
DATA: gt_list_fieldcat  TYPE lvc_t_fcat,      "Field Catalog for List Viewer Control
      gt_exit(1)        TYPE c,
      gt_variant        TYPE disvariant,
      gx_variant        TYPE disvariant,
      gs_print           TYPE lvc_s_prnt.

*DATA : BEGIN OF tab_cat OCCURS 0.
*       INCLUDE STRUCTURE ztgain01.
*DATA : END   OF tab_cat.
DATA : BEGIN OF tab_err occurs 0.
       INCLUDE STRUCTURE  bapiret2.
DATA : END   OF tab_err.

DATA: gt_itab             TYPE STANDARD TABLE OF gy_itab,
      gt_deli_wms_txt     TYPE STANDARD TABLE OF gy_out_deli_text,
      gt_likp             TYPE STANDARD TABLE OF gy_likp_lips,
      gt_do               TYPE STANDARD TABLE OF gy_do,
      gt_ser01_objk       TYPE STANDARD TABLE OF gy_ser01_objk.

DATA: gs_itab             TYPE gy_itab,
      gs_deli_wms_txt     TYPE gy_out_deli_text,
      gs_likp             TYPE gy_likp_lips,
      gs_do               TYPE gy_do,
      gs_ser01_objk       TYPE gy_ser01_objk.

DATA: gs_itab1             TYPE gy_itab,
      gs_deli_wms_txt1     TYPE gy_out_deli_text,
      gs_likp1             TYPE gy_likp_lips,
      gs_odo1              TYPE gy_do,
      gs_ser01_objk1       TYPE gy_ser01_objk.

*DATA: gs_out           TYPE gy_output.


DATA: gcl_grid_main        TYPE REF TO cl_gui_alv_grid,
      gcl_container_main   TYPE REF TO cl_gui_custom_container,
      gs_variant       TYPE disvariant.

DATA: gt_export_txt TYPE truxs_t_text_data.

DATA: gs_export_txt   LIKE LINE OF gt_export_txt.

*DATA: gs_idoc_control  TYPE edidc,
*      gt_edidc          TYPE STANDARD TABLE OF edidc,
*      gs_edidc         LIKE LINE OF gt_edidc,
*      gt_edidd          TYPE STANDARD TABLE OF edidd,
*      gs_edidd         LIKE LINE OF gt_edidd,
**      gs_path          TYPE zsfs999,
*      gs_edp13         TYPE edp13.



DATA: lv_path_name TYPE string.
DATA: lv_text_export(506)    type c.

 DATA: it_lines TYPE STANDARD TABLE OF tline.
  DATA: wa_lines LIKE LINE OF it_lines.
  DATA: v_name TYPE thead-tdname.

DATA: lv_type_export TYPE c.
DATA: lv_space1 TYPE string.
DATA: CT_RESULT TYPE TABLE OF STRING.
ranges: r_blart         for bsid-blart.  "order type

*&-----------------------------------------------------------------------------------*
*& V A R I A B L E
*&-----------------------------------------------------------------------------------*
  DATA : v_pos TYPE i .

  DATA : gt_fieldcat TYPE slis_t_fieldcat_alv,
         gt_layout   TYPE slis_layout_alv,
         gt_events   TYPE slis_t_event.
*        gt_heading  TYPE slis_t_listheader.

*  DATA: mestyp1   like  edidc-mestyp.
*&-----------------------------------------------------------------------------------*
*& C O N S T A N T
*&-----------------------------------------------------------------------------------*
  CONSTANTS :  gc_repid       TYPE repid         VALUE 'ZSDSMMR0080',
               gc_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE'.
  CONSTANTS : gc_space1   TYPE syhex02 value '0x20'.
*  CONSTANTS : mestyp1   like  edidc-mestyp value 'ZEPPM001'.
*&-----------------------------------------------------------------------------------*
*& S E L E C T I O N   S C R E E N
*&-----------------------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-h01.

PARAMETERS:       p_bukrs       TYPE bukrs   OBLIGATORY DEFAULT '1000'.

  SELECT-OPTIONS: s_vbeln         FOR likp-vbeln,
                  s_erdat         FOR likp-erdat,
                  s_werks         FOR lips-werks,
                  s_lgort         FOR lips-lgort.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-h02.
   PARAMETERS:  p_disp radiobutton group gr1,
                p_local radiobutton group gr1,
                p_idoc radiobutton group gr1 default 'X',
                p_path TYPE string .
*                p_path like rlgrap-filename.
SELECTION-SCREEN END OF BLOCK b2.

*END SELECTION SCREEN
*&-----------------------------------------------------------------------------------*
* Event:Initialization
INITIALIZATION.
  p_path = '/usr/sap/tmp/'.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
*  PERFORM get_path_name CHANGING p_path.
  PERFORM pf_directory_browse USING p_path.
*&-----------------------------------------------------------------------------------*
*& * START-OF-SELECTION
*&-----------------------------------------------------------------------------------*
START-OF-SELECTION .



  PERFORM f_get_data.
  PERFORM f_map_data.


*&-----------------------------------------------------------------------------------*
*& * END-OF-SELECTION
*&-----------------------------------------------------------------------------------*
END-OF-SELECTION.

  IF ( NOT gt_itab[] IS INITIAL ) .
    IF NOT p_disp IS INITIAL.
       PERFORM display_reprot.
    ELSEIF NOT p_local IS INITIAL.
*      PERFORM cat_edit_rtn .


             CLEAR: lv_path_name.

*             CONCATENATE 'DI_'  sy-datum sy-timlo '.txt' INTO lv_path_name.
             lv_path_name = 'SDSRT_'.
             PERFORM f_text_export.
             CONCATENATE p_path lv_path_name INTO lv_path_name SEPARATED BY '\'.
             PERFORM export USING lv_path_name
                                  gt_export_txt.



    ELSE.
                 PERFORM f_text_export.
*                 mestyp1 = 'ZWMSMSMP005'.
*                 PERFORM export_idoc.
                 PERFORM display_reprot.

    ENDIF.
  ELSE.
*    MESSAGE i004.
    EXIT.
  ENDIF.
*--------------------------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*

FORM f_get_data .

        SELECT a~vbeln a~erdat a~erzet
               b~posnr b~matnr b~werks
               b~lgort b~lfimg b~serail
          INTO TABLE gt_likp
          FROM likp AS a INNER JOIN lips AS b
                         ON ( a~vbeln EQ b~vbeln )
          WHERE a~vbeln IN s_vbeln
            AND a~erdat IN s_erdat
            AND b~werks IN s_werks
            AND b~lgort IN s_lgort.

          LOOP AT gt_likp INTO gs_likp.
               gs_do-lief_nr = gs_likp-vbeln.
               APPEND gs_do TO gt_do.

          ENDLOOP.

          SORT gt_do.

       IF gt_do IS NOT INITIAL.
           SELECT a~obknr a~lief_nr a~posnr b~matnr b~sernr
           INTO TABLE gt_ser01_objk
           FROM ser01 AS a INNER JOIN objk AS b
                           ON ( a~obknr EQ b~obknr )
           FOR ALL ENTRIES IN gt_do
           WHERE a~lief_nr EQ gt_do-lief_nr.
       ENDIF.








ENDFORM.   "Get data.

*&---------------------------------------------------------------------*
*&      Form  MAP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM f_map_data.

 DATA: lv_qty TYPE i.

    LOOP AT gt_likp INTO gs_likp WHERE lgort NE ''.
           Clear: gs_itab1.




          gs_itab1-vbeln = gs_likp-vbeln.
          gs_itab1-posnr = gs_likp-posnr.
          gs_itab1-matnr = gs_likp-matnr.
          lv_qty = gs_likp-lfimg.
          gs_itab1-lfimg = lv_qty.
          gs_itab1-erdat = gs_likp-erdat.
          gs_itab1-lgort = gs_likp-lgort.
          gs_itab1-erdat = gs_likp-erdat.




          IF gs_likp-serail EQ 'ZSDS'.

               LOOP AT gt_ser01_objk INTO gs_ser01_objk WHERE lief_nr EQ gs_itab1-vbeln
                                                        AND posnr EQ gs_itab1-posnr.


                   gs_itab1-sernr = gs_ser01_objk-sernr.
                   gs_itab1-lfimg = 1 .


                   APPEND gs_itab1 TO gt_itab.
               ENDLOOP.
          ELSE.
              APPEND gs_itab1 TO gt_itab.
          ENDIF.



          gs_deli_wms_txt-vbeln = gs_itab1-vbeln.
          gs_deli_wms_txt-posnr = gs_itab1-posnr.
          gs_deli_wms_txt-matnr = gs_itab1-matnr.
          lv_qty = gs_likp-lfimg.
          gs_deli_wms_txt-lfimg = lv_qty.
          gs_deli_wms_txt-erdat = gs_itab1-erdat.
          gs_deli_wms_txt-lgort = gs_itab1-lgort.

          call function 'CLOI_PUT_SIGN_IN_FRONT'
                   changing
                     value         = gs_deli_wms_txt-lfimg.

          IF gs_likp-serail EQ 'ZSDS'.

               LOOP AT gt_ser01_objk INTO gs_ser01_objk1 WHERE lief_nr EQ gs_itab1-vbeln
                                                        AND posnr EQ gs_itab1-posnr.


                   gs_deli_wms_txt-sernr = gs_ser01_objk1-sernr.
                   gs_deli_wms_txt-lfimg = 1 .
                   call function 'CLOI_PUT_SIGN_IN_FRONT'
                   changing
                     value         = gs_deli_wms_txt-lfimg.


                   APPEND gs_deli_wms_txt TO gt_deli_wms_txt.
               ENDLOOP.
          ELSE.
              APPEND gs_deli_wms_txt TO gt_deli_wms_txt.
          ENDIF.




    ENDLOOP.











ENDFORM.  "map data

**&---------------------------------------------------------------------*
**&      F_TEXT_Export
**&---------------------------------------------------------------------*
FORM f_text_export.
DATA: g_space TYPE string,
      l_pos type i.
      g_space = cl_abap_conv_in_ce=>uccp( '00a0' ).
DATA: lv_scb_corporate_name(30) TYPE c,
      lv_scb_corporate_code(5)  TYPe c,
      lv_to_bank(10) TYPE c,
      lv_date_send(12) TYPE c.

     CLEAR: gs_itab1.",gt_edidd.

     lv_space1    = cl_abap_conv_in_ce=>uccp( gc_space1 ).



*      LOOP AT gt_deli_wms_txt INTO gs_deli_wms_txt1.
**           CLEAR: gs_edidd.
*           CONCATENATE gs_deli_wms_txt1-vbeln
*                        gs_deli_wms_txt1-posnr
*                        gs_deli_wms_txt1-matnr
*                        gs_deli_wms_txt1-lfimg
*                        gs_deli_wms_txt1-erdat
*                        gs_deli_wms_txt1-lgort
*                        gs_deli_wms_txt1-sernr
*
*                       INTO lv_text_export RESPECTING BLANKS.
*                        l_pos = strlen( lv_text_export ).
*                        WHILE l_pos < 80.
*
**                            lv_text_export+l_pos(1) = g_space.
*                            CONCATENATE lv_text_export  lv_space1 INTO lv_text_export.
*                            l_pos = l_pos + 1.
*                        ENDWHILE.
*                       APPEND lv_text_export TO gt_export_txt.
*
**                        gs_edidd-segnam = 'ZWMS009'.
**                        gs_edidd-sdata = lv_text_export.
**                        APPEND gs_edidd TO gt_edidd.
**                        clear gs_edidd.
*
*                CONCATENATE 'SDSRT_' sy-datum '_' sy-timlo '.txt' INTO lv_path_name.
*
*
*      ENDLOOP.
 CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING I_ITEM      = gt_deli_wms_txt "gt_export_txt
                                                                I_FIX_LEN   = 'X'
                                                                I_LEN       = 94 ).
*                                                             I_SEPARATOR = '","'
*                                                             I_START_END_VALUE = '"').
  PERFORM F_EXPORT_TO_SERVER.


ENDFORM.
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
*      i_callback_user_command = 'USER_COMMAND'
      i_save             = 'A'
*      is_layout          = gt_layout
      it_events          = gt_events[]
      it_fieldcat        = gt_fieldcat

    TABLES
      t_outtab           = gt_itab
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

*     READ TABLE gt_itab INTO gs_itab1 INDEX rs_selfield-tabindex.
*
*              SET: PARAMETER ID 'KUN'  FIELD gs_itab1-kunnr,
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

ENDFORM.

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

*IF NOT p_disp IS INITIAL.




*       DO No.
        PERFORM append_fieldcat USING 'VBELN'
                                      ''
                                      ''
                                      'DO number'
                                       space  space  space
                                       gt_fieldcat[].

*       Item
        PERFORM append_fieldcat USING 'POSNR'
                                      ''
                                      ''
                                      'Item'
                                       space  space  space
                                       gt_fieldcat[].

*       Material
        PERFORM append_fieldcat USING 'MATNR'
                                      ''
                                      ''
                                      'Material'
                                       space  space  space
                                       gt_fieldcat[].

*       Qty
        PERFORM append_fieldcat USING 'LFIMG'
                                      ''
                                      ''
                                      'Qty'
                                       space  space  space
                                       gt_fieldcat[].



        PERFORM append_fieldcat USING 'LGORT'
                                      ''
                                      ''
                                      'Storage Location'
                                       space  space  space
                                       gt_fieldcat[].



*       Serial
        PERFORM append_fieldcat USING 'SERNR'
                                      ''
                                      ''
                                      'Serial'
                                       space  space  space
                                       gt_fieldcat[].

*       Document Date
        PERFORM append_fieldcat USING 'ERDAT'
                                      ''
                                      ''
                                      'Document Date'
                                       space  space  space
                                       gt_fieldcat[].
*ENDIF.


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
  wa_header-info = 'DO Return to WMS'.
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

FORM export USING pa_path
                  lt_data_tab TYPE truxs_t_text_data.


 call function 'GUI_DOWNLOAD'
    exporting
      filename                = pa_path
      filetype                = 'ASC'
      write_field_separator   = 'X'
      trunc_trailing_blanks   = 'X'
      CODEPAGE                = '4110'
    tables
*      data_tab                = <fs_table>
      data_tab                = lt_data_tab
*      fieldnames              = lt_data_tab
    exceptions
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      others                  = 22.
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_PATH_NAME
*&---------------------------------------------------------------------*
FORM get_path_name  CHANGING path.
  DATA: l_length TYPE i.
  DATA: l_mask(20) TYPE c.

* S = Save, O = Open
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      mask             = ',*.txt,*.txt.'
      mode             = 'O'
    IMPORTING
      filename         = path
    EXCEPTIONS
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.

                .


ENDFORM.                    " GET_PATH_NAME

*
*FORM cat_edit_rtn .
*
*
**
*  PERFORM append_rtn USING 'BBLTEXT' 'X' '450' ' '. "
*
*
*ENDFORM.
*
*FORM append_rtn USING    p_fn
*                         p_type
*                         p_leng
*                         p_dec.
*
*  clear tab_cat.
*
*  tab_cat-fieldname = p_fn.             "
*  tab_cat-type      = p_type.           "
*  tab_cat-leng      = p_leng.           "
*  tab_cat-decimals  = p_dec.            "
*
*  append tab_cat.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Form  export_idoc
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_FN       text
**      -->P_TYPE     text
**      -->P_LENG     text
**      -->P_DEC      text
**----------------------------------------------------------------------*
*FORM export_idoc.
*
*  DATA: lv_path TYPE string.
*  DATA: lv_filename TYPE rlgrap-filename.
*  CLEAR: lv_filename.
*
** CONCATENATE 'DI_' sy-datum sy-uzeit '.txt' INTO lv_filename.
*  lv_filename = 'SDSRT.txt'.
*  CONCATENATE 'SDSRT_' sy-datum '_' sy-timlo '.txt' INTO lv_path_name.
*  lv_filename = lv_path_name.
*
*
*  lv_path = p_path.
*  TRANSLATE  lv_path TO LOWER CASE.
*
*  gs_path-pathname = lv_path.
*  gs_path-filename = lv_filename.
*  gs_edidd-segnam = 'ZSFS999'.
*  gs_edidd-sdata = gs_path.
*
*  insert gs_edidd into gt_edidd index 1.
*
*  CLEAR: gs_idoc_control.
*
*  select single * into gs_edp13
*         from edp13
*         where mestyp = mestyp1.
*
*  gs_idoc_control-mestyp = gs_edp13-mestyp.
*  gs_idoc_control-idoctp = gs_edp13-idoctyp.
*  gs_idoc_control-rcvpor = gs_edp13-rcvpor.
*  gs_idoc_control-rcvprn = gs_edp13-rcvprn.
*  gs_idoc_control-rcvprt = gs_edp13-rcvprt.
*
*
*
*  call function 'MASTER_IDOC_DISTRIBUTE'
*    exporting
*      master_idoc_control                  = gs_idoc_control
**     OBJ_TYPE                             = ''
**     CHNUM                                = ''
*    tables
*      communication_idoc_control           = gt_edidc
*      master_idoc_data                     = gt_edidd
*    exceptions
*      error_in_idoc_control                = 1
*      error_writing_idoc_status            = 2
*      error_in_idoc_data                   = 3
*      sending_logical_system_unknown       = 4
*      others                               = 5 .
*
*  commit work and wait.
*  READ TABLE gt_edidc into gs_edidc index 1.
*    CALL FUNCTION 'EDI_DOCUMENT_DEQUEUE_LATER'
*      EXPORTING
*        docnum                 = gs_edidc-docnum
*      EXCEPTIONS
*        idoc_is_not_to_dequeue = 1.
*
*ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PF_DIRECTORY_BROWSE
*&---------------------------------------------------------------------*
FORM pf_directory_browse  USING  lv_path.
*                                 lv_filename.

  DATA: lv_temp TYPE string.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
*    window_title         =
      initial_folder       = 'C:'
    CHANGING
      selected_folder      = lv_temp
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4
          .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
*    CONCATENATE  lv_temp
**                 lv_filename
*                INTO lv_path SEPARATED BY '\'.
    lv_path = lv_temp.

  ENDIF.

ENDFORM.                    " PF_DIRECTORY_BROWSE
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
FORM read_text  USING    p_id
                         p_object
                         p_vbeln
                CHANGING p_value.

  DATA: it_lines TYPE STANDARD TABLE OF tline.
  DATA: wa_lines LIKE LINE OF it_lines.
  DATA: v_name TYPE thead-tdname.

  CLEAR: p_value, it_lines[].

  v_name = p_vbeln.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = p_id
      language                = sy-langu
      name                    = v_name
      object                  = p_object
    TABLES
      lines                   = it_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF sy-subrc = 0.
    LOOP AT it_lines INTO wa_lines.
      CONCATENATE p_value  wa_lines-tdline INTO p_value.
    ENDLOOP.
  ENDIF.

ENDFORM.
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


  IF LCL_FTP IS NOT BOUND.
    CREATE OBJECT LCL_FTP.
  ENDIF.

*        LS_FILE = 'Hello Wold'.
*        APPEND LS_FILE TO LT_FILE.
*        LS_FILE = 'Hello Wold1'.
*        APPEND LS_FILE TO LT_FILE.

*  CONCATENATE 'mat' SY-DATUM '.txt' INTO LV_PATH.
  CONCATENATE 'SDSRT_' sy-datum '_' sy-timlo '.txt' INTO LV_PATH.
  LV_STATUS = LCL_FTP->FTP_FILE_PUT( I_WINDOW_PATH = 'MY_DAIKIN/DEV/OUT'
                                     I_AL11_PATH   = '/tmp'
                                     I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
                                     I_USER        = 'ds'
                                     I_PASS        = 'ds=20240521'
                                     I_IP          = '172.31.136.250'
                                     I_PORT        = '21'
                                     IT_DATA       = CT_RESULT ).
  IF LV_STATUS EQ 'S'.
    " SUCCESS FTP FILE
  ELSE.
    " CANNOT FTP FILE
  ENDIF.
ENDFORM.                 " F_GET_RESULT

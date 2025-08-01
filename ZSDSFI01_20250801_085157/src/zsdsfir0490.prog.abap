*&---------------------------------------------------------------------*
*& Report ZSDSFIR0490
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSFIR0490.
  DATA: lv_field(80),
        l_tabledescr_ref TYPE REF TO cl_abap_tabledescr,
        l_descr_ref      TYPE REF TO cl_abap_structdescr,
        idetails_account TYPE abap_compdescr_tab,
        idetails_contact TYPE abap_compdescr_tab,
        xdetails TYPE abap_compdescr.
*  DATA: lv_klimk TYPE ZTFIAR_CREDIT_LI-KLIMK.
  DATA: lv_arbalance TYPE bsid-dmbtr.
  DATA: lv_ytd_balnace TYPE bsid-dmbtr.
  DATA: lv_ar_balnace TYPE bsid-dmbtr.
  DATA: lv_lf.
  DATA: lv_mobile(15) TYPE c. "-->BOI Wantanee 20231011
  DATA:   lt_edidc TYPE STANDARD TABLE OF edidc,
          lt_edidd TYPE STANDARD TABLE OF edidd.
  FIELD-SYMBOLS: <fs_account>,<fs_contact>.
  RANGES: lr_ktokd FOR kna1-ktokd.

  TYPE-POOLS : truxs,slis,icon.
*&-----------------------------------------------------------------------------------*
*& T A B L E S
*&-----------------------------------------------------------------------------------*
TABLES: BUT000,BUT100,UKMBP_CMS_SGM.

*&---------------------------------------------------------------------*
*  TYPE
*&---------------------------------------------------------------------*
TYPES : BEGIN OF gy_result,

      FIRSTNAME(40)           TYPE C,
      LASTNAME(40)            TYPE C,
      TEL1_NUMBR(30)          TYPE C,
      E_MAIL(2541)            TYPE C,
      OLD_EPR_CONTRACT(10)    TYPE C,
      NEW_EPR_CONTRACT(10)    TYPE C,
      PARTNER1(10)            TYPE C,



END OF gy_result.
TYPES : BEGIN OF GY_BUT000,
      PARTNER     TYPE BUT051-PARTNER1,
      PARTNER2     TYPE BUT051-PARTNER2,
      TYPE         TYPE BUT000-TYPE,
      XBLCK        TYPE BUT000-XBLCK,
      NAME_FIRST   TYPE BUT000-NAME_FIRST,
      NAME_LAST    TYPE BUT000-NAME_LAST,
      PERSNUMBER   TYPE BUT000-PERSNUMBER,
      ADDRCOMM     TYPE BUT000-ADDRCOMM,
END OF GY_BUT000.

TYPES : BEGIN OF GY_ADR6,
     PERSNUMBER    TYPE ADR6-PERSNUMBER,
     SMTP_ADDR    TYPE ADR6-SMTP_ADDR,
END OF GY_ADR6.

TYPES : BEGIN OF GY_ADRC,
      ADDRNUMBER   TYPE ADRC-ADDRNUMBER,
      TEL_NUMBER   TYPE ADRC-TEL_NUMBER,
END OF GY_ADRC.


*&---------------------------------------------------------------------*
*  VARIABLE
*&---------------------------------------------------------------------*
DATA : gt_result TYPE TABLE OF gy_result,
       gs_result TYPE gy_result.

DATA : gt_but000 TYPE STANDARD TABLE OF GY_BUT000,
       gs_but000 TYPE GY_BUT000.

DATA : gt_ADR6 TYPE STANDARD TABLE OF GY_ADR6,
       gs_ADR6 TYPE GY_ADR6.
DATA : gt_ADRC TYPE STANDARD TABLE OF GY_ADRC,
       gs_ADRC TYPE GY_ADRC.

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


         END OF LS_HEADER.
DATA LT_HEADER LIKE TABLE OF LS_HEADER.
DATA: CT_RESULT TYPE TABLE OF STRING.
  CONSTANTS : BEGIN OF LC_CON,
                  FIELD1      TYPE STRING VALUE      'FirstName',
                  FIELD2      TYPE STRING VALUE      'LastName',
                  FIELD3      TYPE STRING VALUE      'MobilePhone',
                  FIELD4      TYPE STRING VALUE      'Email',
                  FIELD5      TYPE STRING VALUE      'Old_ERP_Contact_ID__c',
                  FIELD6      TYPE STRING VALUE      'New_ERP_Contact_ID__c',
                  FIELD7      TYPE STRING VALUE      'ERP_Customer_ID__c',

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
            gc_repid       TYPE repid         VALUE 'ZSDSFIR0490',
            gc_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE'.
*&---------------------------------------------------------------------*
*  SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK block3 WITH FRAME TITLE text-003.
SELECT-OPTIONS : S_PARTNE   FOR BUT000-PARTNER,
                 S_BU_GRP   FOR BUT000-BU_GROUP,
                 S_CRDAT    FOR BUT000-CRDAT.

PARAMETERS: p_disp RADIOBUTTON GROUP gr1 ,                   " Download to WEB Server
*            p_local RADIOBUTTON GROUP gr1,
            p_idoc RADIOBUTTON GROUP gr1 DEFAULT 'X',
            p_path LIKE rlgrap-filename  LOWER CASE..
SELECTION-SCREEN END OF BLOCK block3.



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
  PERFORM F_MAP_DATA.
*&---------------------------------------------------------------------*
*  END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.



  IF gt_result[] IS NOT INITIAL.
     IF p_disp IS NOT INITIAL.
        PERFORM display_reprot.
     ELSE.
       PERFORM F_GET_RESULT.
     ENDIF.


  ELSE.
*    MESSAGE s000 DISPLAY LIKE 'E'.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_get_data.


*  SELECT PARTNER TYPE XBLCK
*    INTO TABLE GT_BUT000
*    FROM BUT000
*    WHERE PARTNER IN S_PARTNE
*      AND BU_GROUP IN  S_BU_GRP
*      AND CRDAT IN S_CRDAT.

       SELECT BUT051~PARTNER1 BUT051~PARTNER2 BUT000~TYPE
              BUT000~XBLCK BUT000~NAME_FIRST BUT000~NAME_LAST
              BUT000~PERSNUMBER  BUT000~ADDRCOMM
       INTO TABLE GT_BUT000
       FROM BUT051 INNER JOIN BUT000 ON BUT051~PARTNER2 EQ BUT000~PARTNER.

       SORT GT_BUT000.
       DELETE ADJACENT DUPLICATES FROM GT_BUT000.

       IF GT_BUT000 IS NOT INITIAL.
          SELECT PERSNUMBER SMTP_ADDR
            INTO TABLE GT_ADR6
            FROM ADR6
            FOR ALL ENTRIES IN GT_BUT000
            WHERE  PERSNUMBER EQ GT_BUT000-PERSNUMBER.

          SELECT ADDRNUMBER TEL_NUMBER
            INTO TABLE GT_ADRC
            FROM ADRC
            FOR ALL ENTRIES IN GT_BUT000
            WHERE  ADDRNUMBER EQ GT_BUT000-ADDRCOMM.
       ENDIF.



ENDFORM.                    "f_get_data

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_MAP_DATA.

  DATA : LV_FLAG_CUST TYPE C,
         LV_FLAG_VENDOR TYPE C.
  DATA: GS_ADDRESS TYPE ZSDSFIS010.
  DATA: LV_KTGRD TYPE KNVV-KTGRD.
  DATA: LV_CREDIT_LIMIT TYPE UKMBP_CMS_SGM-CREDIT_LIMIT.
  DATA: LV_VSBED TYPE KNVV-VSBED.
  DATA: LV_TOTALAMT TYPE BSID-DMBTR.
  DATA: LV_YTDCREDIT_BAL TYPE BSID-DMBTR.
  DATA: LV_DATATYPE TYPE DD01V-DATATYPE.
  DATA: GV_PARTN_CAT  TYPE  BU_TYPE,
        GV_BU_GROUP TYPE  BU_GROUP,
        GV_K2_REFNO TYPE  BU_BPEXT,
        GV_SFDC_REFNO TYPE  BU_BPEXT,
        GS_CENTRAL  TYPE  ZSDSFIS003,
        GT_BP_ROLE  TYPE  ZSDSFIS089_TT,
        GT_ADDRESS  TYPE  ZSDSFIS010_TT,
        GS_CUSTOMER TYPE  ZSDSFIS078,
        GT_PARTNER  TYPE  ZSDSFIS083_TT,
        GT_CONTACT  TYPE  ZSDSFIS082_TT,
        GS_CONTACT  TYPE  ZSDSFIS082,
        GS_VENDOR TYPE  ZSDSFIS084,
        GT_RETURN TYPE  BAPIRET2_T,
        GS_BLOCK  TYPE  ZSDSFIS116.
  DATA:  TEXT1(1) TYPE C,
         VALUE(30) TYPE C.

    LOOP AT GT_BUT000 INTO GS_BUT000 WHERE TYPE NE '3'.
          CLEAR: GS_RESULT.

          GS_RESULT-PARTNER1  =  GS_BUT000-PARTNER.
          GS_RESULT-FIRSTNAME = GS_BUT000-NAME_FIRST.
          GS_RESULT-LASTNAME  = GS_BUT000-NAME_LAST.
          GS_RESULT-NEW_EPR_CONTRACT = GS_BUT000-PARTNER2.

          READ TABLE GT_ADR6 INTO GS_ADR6 WITH KEY PERSNUMBER = GS_BUT000-PERSNUMBER.
              IF SY-SUBRC EQ 0.
                 GS_RESULT-E_MAIL = GS_ADR6-SMTP_ADDR.
              ENDIF.
          READ TABLE GT_ADRC INTO GS_ADRC WITH KEY ADDRNUMBER = GS_BUT000-ADDRCOMM.
              IF SY-SUBRC EQ 0.
                 GS_RESULT-TEL1_NUMBR = GS_ADRC-TEL_NUMBER.
              ENDIF.
          GS_RESULT-OLD_EPR_CONTRACT = ''.

*          CALL FUNCTION 'Z_SDSFI_GET_BP_DETAIL'
*           EXPORTING
*             IV_PARTNER          = GS_BUT000-PARTNER
*             IV_MODE             = 'V'
*           IMPORTING
*             EV_PARTN_CAT        = GV_PARTN_CAT
*             EV_BU_GROUP         = GV_BU_GROUP
*             EV_K2_REFNO         = GV_K2_REFNO
*             EV_SFDC_REFNO       = GV_SFDC_REFNO
*             ES_CENTRAL          = GS_CENTRAL
*             ET_BP_ROLE          = GT_BP_ROLE
*             ET_ADDRESS          = GT_ADDRESS
*             ES_CUSTOMER         = GS_CUSTOMER
*             ET_PARTNER          = GT_PARTNER
*             ET_CONTACT          = GT_CONTACT
*             ES_VENDOR           = GS_VENDOR
*             ET_RETURN           = GT_RETURN
*             ES_BLOCK            = GS_BLOCK
*                    .
*      READ TABLE GT_CONTACT INTO GS_CONTACT WITH KEY PARTNER1 = GS_BUT000-PARTNER.
*
*                IF SY-SUBRC EQ 0.
*
*                   GS_RESULT-TEL1_NUMBR = GS_CONTACT-TEL1_NUMBR.
*                   GS_RESULT-E_MAIL = GS_CONTACT-E_MAIL.
*
*                   GS_RESULT-OLD_EPR_CONTRACT = ''.
*
*                   GS_RESULT-PARTNER1 = GS_CONTACT-PARTNER1.
*                ENDIF.



             APPEND GS_RESULT TO GT_RESULT.

    ENDLOOP.





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
      t_outtab           = GT_RESULT
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



* First Name
  PERFORM append_fieldcat USING 'FIRSTNAME'
                                ''
                                ''
                                'First Name'
                                 space  space  space
                                 gt_fieldcat[].

* Last Name
  PERFORM append_fieldcat USING 'LASTNAME'
                                ''
                                ''
                                'Last Name'
                                 space  space  space
                                 gt_fieldcat[].

* Tel
  PERFORM append_fieldcat USING 'TEL1_NUMBR'
                                ''
                                ''
                                'Tel'
                                 space  space  space
                                 gt_fieldcat[].

*E-mail
  PERFORM append_fieldcat USING 'E_MAIL'
                                ''
                                ''
                                'E-mail'
                                 space  space  space
                                 gt_fieldcat[].
* Old ERP Contract
  PERFORM append_fieldcat USING 'OLD_EPR_CONTRACT'
                                ''
                                ''
                                'Old ERP Contract'
                                 space  space  space
                                 gt_fieldcat[].
* New ERP Contract
  PERFORM append_fieldcat USING 'NEW_EPR_CONTRACT'
                                ''
                                ''
                                'New ERP Contract'
                                 space  space  space
                                 gt_fieldcat[].
*ERP BP No
  PERFORM append_fieldcat USING 'PARTNER1'
                                ''
                                ''
                                'ERP BP No'
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



  CONCATENATE 'CONTACT_' SY-DATUM  sy-timlo '.csv' INTO LV_PATH.
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

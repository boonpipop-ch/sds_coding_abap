*&---------------------------------------------------------------------*
*& Report ZSDSMMR0180
*  Author             : Wantanee Prateep na thalang
*  Add-on ID          : ZMMI006
*  Description        : Report and Export interface material master to MyDaikin and  Daikin Pro website
*  Purpose            :
*  Copied from        :  ZP_MM_IDOC_MATMASTER
*  Restriction        :
*&---------------------------------------------------------------------*
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSMMR0180.
TYPE-POOLS : TRUXS,SLIS,ICON.
*&---------------------------------------------------------------------*
*  DECLARATION
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*  TABLE
*&---------------------------------------------------------------------*
TABLES : ADRCITYT,MARA,VBRK.
*&---------------------------------------------------------------------*
*  TYPE
*&---------------------------------------------------------------------*
TYPES : BEGIN OF GY_RESULT,
          MATERIAL_TYPE        TYPE CHAR4,
          MATERIAL_NUMBER      TYPE CHAR18,
          MATERIAL_GROUP       TYPE CHAR9,
          MATERIAL_DESCRIPTION TYPE CHAR40,
          PH1                  TYPE CHAR5,
          PH2	                 TYPE CHAR255,
          CLASS	               TYPE CHAR255,
          OPTIONAL             TYPE CHAR50,
          SALES_PRICE	         TYPE CHAR13,
        END OF GY_RESULT.

TYPES : BEGIN OF GY_MARA,
          MATNR TYPE MARA-MATNR,
          MTART TYPE MARA-MTART,
          MATKL TYPE MARA-MATKL,
          MAKTX TYPE MAKT-MAKTX,
          PRDHA TYPE MARA-PRDHA,
        END OF GY_MARA.

TYPES : BEGIN OF GY_A304,
          MATNR TYPE A304-MATNR,
          KNUMH TYPE A304-KNUMH,
          KBETR TYPE KONP-KBETR,
          DATBI TYPE A304-DATBI, "CH1 Add by Wantanee 20230412
        END OF GY_A304.



*&---------------------------------------------------------------------*
*  VARIABLE
*&---------------------------------------------------------------------*
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_MARA TYPE TABLE OF GY_MARA,
       GS_MARA TYPE GY_MARA.

DATA : GT_A304 TYPE TABLE OF GY_A304,
       GS_A304 TYPE GY_A304.



DATA : GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_SORT   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT   TYPE SLIS_SORTINFO_ALV.

DATA: GS_PATH    TYPE STRING.
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
         END OF LS_HEADER.
DATA LT_HEADER LIKE TABLE OF LS_HEADER.

DATA : GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
       GT_LAYOUT   TYPE SLIS_LAYOUT_ALV,
       GT_EVENTS   TYPE SLIS_T_EVENT.

DATA: CT_RESULT TYPE TABLE OF STRING.
DATA : GS_POS TYPE I .
DATA: GT_EXPORT_TXT TYPE TRUXS_T_TEXT_DATA.
DATA: GS_EXPORT_TXT   LIKE LINE OF GT_EXPORT_TXT.


DATA: GS_PATH_NAME TYPE STRING.
DATA: GS_TEXT_EXPORT(506)    TYPE C.

DATA: GT_LINES TYPE STANDARD TABLE OF TLINE.
DATA: GS_LINES LIKE LINE OF GT_LINES.
DATA: GS_NAME TYPE THEAD-TDNAME.

DATA: GS_TYPE_EXPORT TYPE C.
DATA: GS_SPACE1 TYPE STRING.
*&---------------------------------------------------------------------*
*  RANGES
*&---------------------------------------------------------------------*
*RANGES : gr_blart FOR bkpf-blart



*&---------------------------------------------------------------------*
*  CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS : GC_REPID       TYPE REPID         VALUE 'ZSDSMMR0180',
            GC_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE'.
CONSTANTS : GC_SPACE1   TYPE SYHEX02 VALUE '0x20'.
CONSTANTS : GC_TRUE    TYPE C VALUE 'X',
            GC_STAT    TYPE C VALUE 'A',
            GC_A_RXZ   TYPE C LENGTH 5 VALUE 'A_RXZ',
            GC_PATH    TYPE STRING VALUE '/usr/sap/tmp/'.
*            GC_MESTYP1 TYPE EDIDC-MESTYP VALUE 'ZSFS059'.

  CONSTANTS : BEGIN OF LC_CON,
                FIELD1     TYPE STRING VALUE 'MATERIAL_TYPE',
                FIELD2 TYPE STRING VALUE 'MATERIAL_NUMBER',
                FIELD3 TYPE STRING VALUE 'MATERIAL_GROUP',
                FIELD4 TYPE STRING VALUE 'MATERIAL_DESCRIPTION',
                FIELD5    TYPE STRING VALUE 'PH1',
                FIELD6    TYPE STRING VALUE 'PH2',
                FIELD7    TYPE STRING VALUE 'CLASS',
                FIELD8    TYPE STRING VALUE 'OPTIONAL',
                FIELD9    TYPE STRING VALUE 'SALES_PRICE',
              END OF LC_CON.
*&---------------------------------------------------------------------*
*  SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_MATNR FOR MARA-MATNR,
                   S_PH1   FOR GS_RESULT-PH1,
                   S_PH2   FOR GS_RESULT-PH2,
                   S_MTART FOR MARA-MTART,
                   S_MATKL FOR MARA-MATKL.
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS : S_TSV FOR MARA-MATKL,
                   S_PRI FOR MARA-MTART.
SELECTION-SCREEN END OF BLOCK BLOCK2.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-H02.
  PARAMETERS: P_DISP  RADIOBUTTON GROUP GR1,
              P_LOCAL RADIOBUTTON GROUP GR1,
              P_SERV  RADIOBUTTON GROUP GR1 DEFAULT 'X',
              P_PATH  TYPE STRING  LOWER CASE.
SELECTION-SCREEN END OF BLOCK B2.
*&---------------------------------------------------------------------*
*  INITIALIZATION.
*&---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_GET_INITIAL.
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
  PERFORM F_GET_SELECTION .
  PERFORM F_GET_DATA.
*&---------------------------------------------------------------------*
*  END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.

  IF GT_RESULT[] IS NOT INITIAL.

    IF P_DISP IS NOT INITIAL.
         PERFORM DISPLAY_REPROT.
    ELSEIF P_LOCAL IS NOT INITIAL.
      CLEAR: GS_PATH_NAME.
      CONCATENATE 'mat_' SY-DATUM '_' sy-timlo '.csv' INTO GS_PATH_NAME.
*      GS_PATH_NAME = 'SDS_STK_BALANCE'.
      PERFORM F_TEXT_EXPORT_LOCAL.
      CONCATENATE P_PATH GS_PATH_NAME INTO GS_PATH_NAME SEPARATED BY '\'.
      PERFORM EXPORT USING GS_PATH_NAME
                           GT_EXPORT_TXT.
    ELSE.

    ENDIF..
  ELSE.
*    MESSAGE s000 DISPLAY LIKE 'E'.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_GET_DATA.

  SELECT MARA~MATNR
         MARA~MTART
         MARA~MATKL
         MAKT~MAKTX
         MARA~PRDHA
    FROM MARA
    INNER JOIN MAKT ON MARA~MATNR EQ MAKT~MATNR AND
                       MAKT~SPRAS EQ SY-LANGU
    INTO TABLE GT_MARA
    WHERE MARA~MATNR IN S_MATNR
      AND MARA~PRDHA IN S_PH1
      AND MARA~PRDHA IN S_PH2
      AND MARA~MATKL IN S_MATKL
      AND MARA~MTART IN S_MTART
      AND MARA~LVORM EQ SPACE.

  IF GT_MARA[] IS NOT INITIAL.
    SELECT A304~MATNR
           A304~KNUMH
           KONP~KBETR
           A304~DATBI "CH1 Add by Wantanee 20230412
      FROM A304
      INNER JOIN KONP ON A304~KNUMH EQ KONP~KNUMH
      INTO TABLE GT_A304
      FOR ALL ENTRIES IN GT_MARA
      WHERE MATNR EQ GT_MARA-MATNR.
*    SORT gt_a304 BY matnr knumh DESCENDING. "CH1 Remove by Wantanee 20230412
    SORT GT_A304 BY MATNR DATBI DESCENDING."CH1 Add by Wantanee 20230412



  ENDIF.


  PERFORM F_GET_RESULT.

ENDFORM.                    "f_get_data
*&---------------------------------------------------------------------*
*&      Form  pf_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_PF_ALV_GRID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
      "_CALLBACK_PF_STATUS_SET = 'PF_STATUS_1'
      "I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    =
      IS_LAYOUT          = GS_LAYOUT
      IT_FIELDCAT        = GT_FCAT
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS  =
      IT_SORT            = GT_SORT
*     IT_FILTER          =
*     IS_SEL_HIDE        =
      I_DEFAULT          = 'X'
      I_SAVE             = 'X'
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
      T_OUTTAB           = GT_RESULT
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
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
FORM F_SET_LAYOUT_OUTPUT." CHANGING ps_layout TYPE slis_layout_alv.
  "gs_layout-box_fieldname     = 'SEL'.
  GS_LAYOUT-ZEBRA             = 'X'.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

ENDFORM.                    " set_layout_output
*&---------------------------------------------------------------------*
*&      Form  build_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_BUILD_FCAT.

  DATA:
   LS_FCAT TYPE SLIS_FIELDCAT_ALV.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'I_DOC_NUM'.
  LS_FCAT-SELTEXT_S   = 'IDOC No.'.
  LS_FCAT-SELTEXT_M   = 'IDOC No.'.
  LS_FCAT-SELTEXT_L   = 'IDOC No.'.
  APPEND LS_FCAT TO GT_FCAT.

*  CLEAR ls_fcat.
*  ls_fcat-fieldname   = 'KUNAG'.
*  ls_fcat-seltext_s   = 'Customer Code'.
*  ls_fcat-seltext_m   = 'Customer Code'.
*  ls_fcat-seltext_l   = 'Customer Code'.
*  APPEND ls_fcat TO gt_fcat.
*
*  CLEAR ls_fcat.
*  ls_fcat-fieldname   = 'MATNR'.
*  ls_fcat-seltext_s   = 'Material'.
*  ls_fcat-seltext_m   = 'Material'.
*  ls_fcat-seltext_l   = 'Material'.
*  APPEND ls_fcat TO gt_fcat.
*
*  CLEAR ls_fcat.
*  ls_fcat-fieldname   = 'FKIMG'.
*  ls_fcat-seltext_s   = 'QTY'.
*  ls_fcat-seltext_m   = 'QTY'.
*  ls_fcat-seltext_l   = 'QTY'.
*  APPEND ls_fcat TO gt_fcat.

*  CLEAR ls_fcat.
*  ls_fcat-fieldname   = 'PH1'.
*  ls_fcat-seltext_s   = 'PH1'.
*  ls_fcat-seltext_m   = 'PH1'.
*  ls_fcat-seltext_l   = 'PH1'.
*  APPEND ls_fcat TO gt_fcat.
*
*  CLEAR ls_fcat.
*  ls_fcat-fieldname   = 'PH2'.
*  ls_fcat-seltext_s   = 'PH2'.
*  ls_fcat-seltext_m   = 'PH2'.
*  ls_fcat-seltext_l   = 'PH2'.
*  APPEND ls_fcat TO gt_fcat.

ENDFORM.                    "build_fcat_1
*&---------------------------------------------------------------------*
*&      Form  F_SHOW_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SHOW_REPORT .
  PERFORM F_SET_LAYOUT_OUTPUT.
  PERFORM F_BUILD_FCAT.
  PERFORM F_SORT.
  PERFORM F_PF_ALV_GRID.
ENDFORM.                    " F_SHOW_REPORT
*&---------------------------------------------------------------------*
*&      Form  F_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SORT .
*  CLEAR gs_sort.
*  gs_sort-fieldname = 'LIFNR'.
*  gs_sort-spos = '1'.
*  gs_sort-up = 'X'.
**  gs_sort-subtot = 'X'.
*  APPEND gs_sort TO gt_sort.
ENDFORM.                    " F_SORT

*&---------------------------------------------------------------------*
*&      Form  F_GET_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_SELECTION .

  RANGES : PH1 FOR GS_RESULT-PH1,
           PH2 FOR GS_RESULT-PH2.


  LOOP AT S_PH1.
    CLEAR PH1.
    PH1-SIGN   = S_PH1-SIGN.
    PH1-OPTION = 'CP'.
    CONCATENATE '*' S_PH1-LOW  '*' INTO PH1-LOW.
    IF S_PH1-HIGH IS NOT INITIAL.
      CONCATENATE '*' S_PH1-HIGH '*' INTO PH1-HIGH.
    ENDIF.
    APPEND PH1.
  ENDLOOP.


  LOOP AT S_PH2.
    CLEAR PH2.
    PH2-SIGN   = S_PH2-SIGN.
    PH2-OPTION = 'CP'.
    CONCATENATE '*' S_PH2-LOW  '*' INTO PH2-LOW.
    IF S_PH1-HIGH IS NOT INITIAL.
      CONCATENATE '*' S_PH2-HIGH '*' INTO PH2-HIGH.
    ENDIF.
    APPEND PH2.
  ENDLOOP.

  S_PH1[] = PH1[].
  S_PH2[] = PH2[].

ENDFORM.                    " F_GET_SELECTION
*&---------------------------------------------------------------------*
*&      Form  F_GET_INITIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_INITIAL .
*  CLEAR s_ph1.
*  s_ph1-sign   = 'I'.
*  s_ph1-option = 'EQ'.
*  s_ph1-low    = 'RA'.
*  APPEND s_ph1.
*
*  CLEAR s_ph1.
*  s_ph1-sign   = 'I'.
*  s_ph1-option = 'EQ'.
*  s_ph1-low    = 'SK'.
*  APPEND s_ph1.

ENDFORM.                    " F_GET_INITIAL
*&---------------------------------------------------------------------*
*&      Form  F_GET_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_RESULT.

  LOOP AT GT_MARA INTO GS_MARA.

    IF GS_MARA-MTART EQ 'UNBW'.
      IF GS_MARA-MATKL NOT IN S_TSV.
        CONTINUE.
      ENDIF.
    ENDIF.

    GS_RESULT-MATERIAL_TYPE        = GS_MARA-MTART.
    GS_RESULT-MATERIAL_NUMBER      = GS_MARA-MATNR.
    GS_RESULT-MATERIAL_GROUP       = GS_MARA-MATKL.
    GS_RESULT-MATERIAL_DESCRIPTION = GS_MARA-MAKTX.
    GS_RESULT-PH1                  = GS_MARA-PRDHA+0(5).
    GS_RESULT-PH2                  = GS_MARA-PRDHA+5(5).



    IF GS_RESULT-MATERIAL_TYPE IN S_PRI.
      READ TABLE GT_A304 INTO GS_A304
      WITH KEY MATNR = GS_MARA-MATNR.
      IF SY-SUBRC = 0.
        IF GS_A304-KBETR LE 99999999.
          GS_RESULT-SALES_PRICE = GS_A304-KBETR.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND GS_RESULT TO GT_RESULT.

    CLEAR : GS_MARA,GS_RESULT,GS_A304.


  ENDLOOP.
  LS_HEADER-FIELD1 = LC_CON-FIELD1.
  LS_HEADER-FIELD2 = LC_CON-FIELD2.
  LS_HEADER-FIELD3 = LC_CON-FIELD3.
  LS_HEADER-FIELD4 = LC_CON-FIELD4.
  LS_HEADER-FIELD5 = LC_CON-FIELD5.
  LS_HEADER-FIELD6 = LC_CON-FIELD6.
  LS_HEADER-FIELD7 = LC_CON-FIELD7.
  LS_HEADER-FIELD8 = LC_CON-FIELD8.
  LS_HEADER-FIELD9 = LC_CON-FIELD9.
  APPEND LS_HEADER TO LT_HEADER.

  "Z_DEMO_GEN_FILE
  CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING I_HEADER    = LT_HEADER
                                                                I_ITEM      = GT_RESULT
                                                                I_SEPARATOR = '"|"'
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



*ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             =  'ZSDSMMR0180'
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

  IF LCL_FTP IS NOT BOUND.
    CREATE OBJECT LCL_FTP.
  ENDIF.

*        LS_FILE = 'Hello Wold'.
*        APPEND LS_FILE TO LT_FILE.
*        LS_FILE = 'Hello Wold1'.
*        APPEND LS_FILE TO LT_FILE.

   LV_PATH_FILE  = P_PATH.

  CONCATENATE 'mat_' SY-DATUM '_' sy-timlo '.csv' INTO LV_PATH.
      LV_STATUS = LCL_FTP->FTP_FILE_PUT( "I_WINDOW_PATH = 'DEALER_CLAIM/DEV/OUT'
*                                     I_AL11_PATH   = '/tmp'
                                      I_AL11_PATH   = LV_PATH_FILE
                                     I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
                                    " I_USER        = 'ds'
                                     "I_PASS        = 'ds=20240521'
                                    " I_IP          = '172.31.136.250'
                                    " I_PORT        = '21'
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

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPROT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_REPROT .
  PERFORM BUILD_LAYOUT.
  PERFORM BUILD_CATALOG.
  PERFORM BUILD_EVENT USING GT_EVENTS[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = GC_REPID
*     i_callback_user_command = 'USER_COMMAND'
      I_SAVE             = 'A'
*     is_layout          = gt_layout
      IT_EVENTS          = GT_EVENTS[]
      IT_FIELDCAT        = GT_FIELDCAT
    TABLES
      T_OUTTAB           = GT_RESULT
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " DISPLAY_REPROT

*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT .
  GT_LAYOUT-WINDOW_TITLEBAR = SY-TITLE.
  GT_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GT_LAYOUT-ZEBRA = 'X'.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  BUILD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_CATALOG .

  CLEAR : GS_POS.


  IF NOT P_DISP IS INITIAL.


*       Material
    PERFORM APPEND_FIELDCAT USING 'MATERIAL_TYPE'
                                  ''
                                  ''
                                  'Material Type'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].
*       Material Description
    PERFORM APPEND_FIELDCAT USING 'MATERIAL_NUMBER'
                                  ''
                                  ''
                                  'Material Code'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].
*       Material Group
    PERFORM APPEND_FIELDCAT USING 'MATERIAL_GROUP'
                                  ''
                                  ''
                                  'Material Group'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].

*       Material Description
    PERFORM APPEND_FIELDCAT USING 'MATERIAL_DESCRIPTION'
                                  ''
                                  ''
                                  'Material Description'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].
*      PH1
    PERFORM APPEND_FIELDCAT USING 'PH1'
                                  ''
                                  ''
                                  'PH1'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].

*     PH2
    PERFORM APPEND_FIELDCAT USING 'PH2'
                                  ''
                                  ''
                                  'PH2'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].

*  CLASS
    PERFORM APPEND_FIELDCAT USING 'CLASS'
                                  ''
                                  ''
                                  'CLASS'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].

*  OPTIONAL
    PERFORM APPEND_FIELDCAT USING 'OPTIONAL'
                                  ''
                                  ''
                                  'OPTIONAL'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].

*  SALES_PRICE
    PERFORM APPEND_FIELDCAT USING 'SALES_PRICE'
                                  ''
                                  ''
                                  'SALES_PRICE'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].


  ENDIF.


ENDFORM.             "BUILD_ATALOG


*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*----------------------------------------------------------------------*
FORM BUILD_EVENT  USING E03_LT_EVENTS TYPE SLIS_T_EVENT.
  DATA: LW_EVENT TYPE SLIS_ALV_EVENT.
* Read Event
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 1
    IMPORTING
      ET_EVENTS   = E03_LT_EVENTS.
  READ TABLE E03_LT_EVENTS WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
                           INTO LW_EVENT.
*   READ TABLE e03_lt_events WITH KEY name = slis_ev_top_of_list
*                             INTO lw_event.
  IF SY-SUBRC = 0.
* register top of page event
    MOVE GC_TOP_OF_PAGE TO LW_EVENT-FORM.
    APPEND LW_EVENT TO E03_LT_EVENTS.
  ENDIF.
ENDFORM.                    " BUILD_EVENT

*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  PERFORM WRITE_HEADING.
ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  WRITE_HEADING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_HEADING .
  DATA: T_HEADER  TYPE   SLIS_T_LISTHEADER,
        WA_HEADER TYPE   SLIS_LISTHEADER.

  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Report Name : '.
  WA_HEADER-INFO = 'Daily to WMS'.
  APPEND WA_HEADER TO T_HEADER.
  CLEAR WA_HEADER.


  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Report Date : '.
  CONCATENATE  SY-DATUM+6(2) '.'
               SY-DATUM+4(2) '.'
               SY-DATUM(4) INTO WA_HEADER-INFO.   "todays date
  APPEND WA_HEADER TO T_HEADER.
  CLEAR: WA_HEADER.

  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Report Time : '.
  WRITE: SY-UZEIT TO WA_HEADER-INFO.    "todays date
  APPEND WA_HEADER TO T_HEADER.
  CLEAR: WA_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_HEADER.

ENDFORM.                    " WRITE_HEADING

FORM EXPORT USING PA_PATH
                  LT_DATA_TAB TYPE TRUXS_T_TEXT_DATA.


  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = PA_PATH
      FILETYPE                = 'ASC'
      WRITE_FIELD_SEPARATOR   = 'X'
      TRUNC_TRAILING_BLANKS   = 'X'
      CODEPAGE                = '4110'
    TABLES
*     data_tab                = <fs_table>
      DATA_TAB                = LT_DATA_TAB
*     fieldnames              = lt_data_tab
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      OTHERS                  = 22.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_PATH_NAME
*&---------------------------------------------------------------------*
FORM GET_PATH_NAME  CHANGING PATH.
  DATA: L_LENGTH TYPE I.
  DATA: L_MASK(20) TYPE C.

* S = Save, O = Open
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      MASK             = ',*.txt,*.txt.'
      MODE             = 'O'
    IMPORTING
      FILENAME         = PATH
    EXCEPTIONS
      INV_WINSYS       = 01
      NO_BATCH         = 02
      SELECTION_CANCEL = 03
      SELECTION_ERROR  = 04.

  .


ENDFORM.                    " GET_PATH_NAME

*&---------------------------------------------------------------------*
*&      Form  PF_DIRECTORY_BROWSE
*&---------------------------------------------------------------------*
FORM F_DIRECTORY_BROWSE  USING  LV_PATH.
*                                 lv_filename.

  DATA: LV_TEMP TYPE STRING.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE
    EXPORTING
*     window_title         =
      INITIAL_FOLDER       = 'C:'
    CHANGING
      SELECTED_FOLDER      = LV_TEMP
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
*    CONCATENATE  lv_temp
**                 lv_filename
*                INTO lv_path SEPARATED BY '\'.
    LV_PATH = LV_TEMP.

  ENDIF.

ENDFORM.                    " PF_DIRECTORY_BROWSE
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
FORM READ_TEXT  USING    P_ID
                         P_OBJECT
                         P_VBELN
                CHANGING P_VALUE.

  DATA: GT_LINES TYPE STANDARD TABLE OF TLINE.
  DATA: GS_LINES LIKE LINE OF GT_LINES.
  DATA: GS_NAME TYPE THEAD-TDNAME.

  CLEAR: P_VALUE, GT_LINES[].

  GS_NAME = P_VBELN.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = P_ID
      LANGUAGE                = SY-LANGU
      NAME                    = GS_NAME
      OBJECT                  = P_OBJECT
    TABLES
      LINES                   = GT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  IF SY-SUBRC = 0.
    LOOP AT GT_LINES INTO GS_LINES.
      CONCATENATE P_VALUE  GS_LINES-TDLINE INTO P_VALUE.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_TEXT

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

FORM APPEND_FIELDCAT USING   P_FIELD   "Field name
                             P_REFTABLE"Reference Table name
                             P_REFFIELD"Reference Field name
                             P_COLTXT  "Col Text(for specify)
                             P_DOSUM   "Sum total
                             P_CFIELDNAME  "  currency
                             P_NO_ZERO     " no zero
                             P_IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

  DATA: WA_INFIELDCAT   TYPE SLIS_FIELDCAT_ALV,
        V_COLTXT_LENGTH TYPE I.

  ADD 1 TO GS_POS.

  WA_INFIELDCAT-FIELDNAME     = P_FIELD.
  WA_INFIELDCAT-REF_TABNAME   = P_REFTABLE.
  WA_INFIELDCAT-REF_FIELDNAME = P_REFFIELD.
  WA_INFIELDCAT-COL_POS       = GS_POS .
  WA_INFIELDCAT-DO_SUM        = P_DOSUM.

  IF NOT P_NO_ZERO IS INITIAL .
    WA_INFIELDCAT-NO_ZERO = P_NO_ZERO.
  ENDIF.
  IF NOT P_CFIELDNAME IS INITIAL .
    WA_INFIELDCAT-CFIELDNAME = P_CFIELDNAME .
  ENDIF.


*If we need to specify text ,don't need to derive from data dictionary
*program will check length and define width of the colum
  IF NOT P_COLTXT IS INITIAL.
    V_COLTXT_LENGTH = STRLEN( P_COLTXT ).

    IF V_COLTXT_LENGTH > 20.
      WA_INFIELDCAT-DDICTXT = 'L'."Long text
      WA_INFIELDCAT-SELTEXT_L = P_COLTXT.
    ELSEIF V_COLTXT_LENGTH > 10.
      WA_INFIELDCAT-DDICTXT = 'M'."Medium Text
      WA_INFIELDCAT-SELTEXT_M = P_COLTXT.
    ELSE.
      WA_INFIELDCAT-DDICTXT = 'S'."Short Text
      WA_INFIELDCAT-SELTEXT_S = P_COLTXT.
    ENDIF.
    WA_INFIELDCAT-REPTEXT_DDIC = P_COLTXT  .
  ENDIF.
  APPEND WA_INFIELDCAT TO P_IT_FIELDCAT.
ENDFORM.                    " APPEND_FIELDCAT

**&---------------------------------------------------------------------*
**&      F_TEXT_Export
**&---------------------------------------------------------------------*
FORM F_TEXT_EXPORT_LOCAL.
  DATA: G_SPACE TYPE STRING,
        L_POS   TYPE I.
  G_SPACE = CL_ABAP_CONV_IN_CE=>UCCP( '00a0' ).
  DATA: LV_SCB_CORPORATE_NAME(30) TYPE C,
        LV_SCB_CORPORATE_CODE(5)  TYPE C,
        LV_TO_BANK(10)            TYPE C,
        LV_DATE_SEND(12)          TYPE C.
  DATA: CT_RESULT TYPE TABLE OF STRING.
  DATA: CS_STRING TYPE STRING.

  CLEAR: GS_RESULT.


  GS_SPACE1    = CL_ABAP_CONV_IN_CE=>UCCP( GC_SPACE1 ).

  LOOP AT GT_RESULT INTO GS_RESULT.
    CONCATENATE  GS_RESULT-MATERIAL_TYPE
                 GS_RESULT-MATERIAL_NUMBER
                 GS_RESULT-MATERIAL_GROUP
                 GS_RESULT-MATERIAL_DESCRIPTION
                 GS_RESULT-PH1
                 GS_RESULT-PH2
                 GS_RESULT-CLASS
                 GS_RESULT-OPTIONAL
                 GS_RESULT-SALES_PRICE

                INTO GS_TEXT_EXPORT RESPECTING BLANKS.

    L_POS = STRLEN( GS_TEXT_EXPORT ).
    WHILE L_POS < 649.

      GS_TEXT_EXPORT+L_POS(1) = G_SPACE.
      CONCATENATE GS_TEXT_EXPORT  GS_SPACE1 INTO GS_TEXT_EXPORT.
      L_POS = L_POS + 1.
    ENDWHILE.
    APPEND GS_TEXT_EXPORT TO GT_EXPORT_TXT.


    CONCATENATE 'mat_' SY-DATUM '_' sy-timlo '.csv' INTO GS_PATH_NAME.


  ENDLOOP.





ENDFORM.

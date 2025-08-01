*&---------------------------------------------------------------------*
*& Report ZSDSMMR0130
*  Creation Date      : 02.04.2024
*  Author             : Wantanee Prateep na thalang
*  Add-on ID          : ZMMI011,ZMMI014
*  Description        : Report and Export interface stock balance to SMP and SONY Spare part
*  Purpose            :
*  Copied from        :
*  Restriction        :
*&---------------------------------------------------------------------*
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSMMR0130.

*---------DATA DEFINATION-------------------------*
*&-----------------------------------------------------------------------------------*
*& T A B L E S
*&-----------------------------------------------------------------------------------*
TABLES: MARA,MARD.


*&-----------------------------------------------------------------------------------*
*& D A T A
*&-----------------------------------------------------------------------------------*

TYPES: BEGIN OF GY_ITAB,
         MATNR TYPE MSEG-MATNR,  "Material
         MAKTX TYPE MAKT-MAKTX, "Material Description
         MATKL TYPE MARA-MATKL, "Material Group
         LGPBE TYPE MARD-LGPBE, "Storage bin
         LGORT TYPE MARD-LGORT, "Storage Location
         LABST TYPE I,   "Qty
       END OF GY_ITAB.

TYPES: BEGIN OF GY_MARD,
         MATNR TYPE MARD-MATNR,
         LGPBE TYPE MARD-LGPBE,
         LGORT TYPE MARD-LGORT,
         LABST TYPE MARD-LABST,
         MATKL TYPE MARA-MATKL, "Material Group
         MAKTX TYPE MAKT-MAKTX, "Material Description

       END OF GY_MARD.

TYPES: BEGIN OF GY_OUT_STKBAL_TEXT,
         MATNR(35) TYPE C,  "Material
         MAKTX(40) TYPE C, "Material Description
         MATKL(9)  TYPE C, "Material Group
         LGPBE(10) TYPE C, "Storage bin
         LGORT(4)  TYPE C, "Storage Location
         LABST(13) TYPE C,   "Qty

       END OF GY_OUT_STKBAL_TEXT.

TYPES: BEGIN OF GY_OUTPUT,
         TEXT TYPE TEXT1000,
       END OF GY_OUTPUT.





*&-----------------------------------------------------------------------------------*
*& I N T E R N A L   T A B L E S
*&-----------------------------------------------------------------------------------*
* Data for ALV display

TYPE-POOLS: SLIS.
TYPE-POOLS: TRUXS.
* The inputs that need to be passed to the REUSE_ALV function module
DATA: GS_LIST_FIELDCAT TYPE LVC_T_FCAT,      "Field Catalog for List Viewer Control
      GS_EXIT(1)       TYPE C,
      GS_VARIANT       TYPE DISVARIANT,
      GS_PRINT         TYPE LVC_S_PRNT.

DATA: GT_ITAB           TYPE STANDARD TABLE OF GY_ITAB,
      GT_MARD           TYPE STANDARD TABLE OF GY_MARD,
      GT_STKBAL_WMS_TXT TYPE STANDARD TABLE OF GY_OUT_STKBAL_TEXT.



DATA: GS_ITAB            TYPE GY_ITAB,
      GS_MARD            TYPE GY_MARD,
      GS_STKBAL_WMS_TXT1 TYPE GY_OUT_STKBAL_TEXT,
      GS_STKBAL_WMS_TXT2 TYPE GY_OUT_STKBAL_TEXT.


DATA: GY_GRID_MAIN      TYPE REF TO CL_GUI_ALV_GRID,
      GY_CONTAINER_MAIN TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: GT_EXPORT_TXT TYPE TRUXS_T_TEXT_DATA.
*DATA: gt_export_txt TYPE TABLE OF string. "truxs_t_text_data.

DATA: GS_EXPORT_TXT   LIKE LINE OF GT_EXPORT_TXT.


DATA: GS_PATH_NAME TYPE STRING.
DATA: GS_TEXT_EXPORT(506)    TYPE C.

DATA: GT_LINES TYPE STANDARD TABLE OF TLINE.
DATA: GS_LINES LIKE LINE OF GT_LINES.
DATA: GS_NAME TYPE THEAD-TDNAME.

DATA: GS_TYPE_EXPORT TYPE C.
DATA: GS_SPACE1 TYPE STRING.


*&-----------------------------------------------------------------------------------*
*& V A R I A B L E
*&-----------------------------------------------------------------------------------*
DATA : GS_POS TYPE I .

DATA : GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
       GT_LAYOUT   TYPE SLIS_LAYOUT_ALV,
       GT_EVENTS   TYPE SLIS_T_EVENT.

*&-----------------------------------------------------------------------------------*
*& C O N S T A N T
*&-----------------------------------------------------------------------------------*
CONSTANTS : GC_REPID       TYPE REPID         VALUE 'ZSDSMMR0130',
            GC_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE'.
CONSTANTS : GC_SPACE1   TYPE SYHEX02 VALUE '0x20'.
*&-----------------------------------------------------------------------------------*
*& S E L E C T I O N   S C R E E N
*&-----------------------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-H01.

  PARAMETERS:       P_BUKRS       TYPE BUKRS   OBLIGATORY DEFAULT '1000'.

  SELECT-OPTIONS: S_WERKS        FOR MARD-WERKS,
                  S_LGORT        FOR MARD-LGORT.

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-H03.
  PARAMETERS: P_SONY RADIOBUTTON GROUP GR2 DEFAULT 'X',
              P_SMP  RADIOBUTTON GROUP GR2.

*                p_path like rlgrap-filename.
SELECTION-SCREEN END OF BLOCK B3.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-H02.
  PARAMETERS: P_DISP  RADIOBUTTON GROUP GR1,
              P_LOCAL RADIOBUTTON GROUP GR1,
              P_SERV  RADIOBUTTON GROUP GR1 DEFAULT 'X',
              P_PATH  TYPE STRING  LOWER CASE.
SELECTION-SCREEN END OF BLOCK B2.

*END SELECTION SCREEN
*&-----------------------------------------------------------------------------------*
* Event:Initialization
INITIALIZATION.
  P_PATH = '/usr/sap/tmp/'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PATH.
  PERFORM F_DIRECTORY_BROWSE USING P_PATH.
*&-----------------------------------------------------------------------------------*
*& * START-OF-SELECTION
*&-----------------------------------------------------------------------------------*
START-OF-SELECTION .



  PERFORM F_GET_DATA.
  PERFORM F_MAP_DATA.


*&-----------------------------------------------------------------------------------*
*& * END-OF-SELECTION
*&-----------------------------------------------------------------------------------*
END-OF-SELECTION.

  IF ( NOT GT_ITAB[] IS INITIAL ) .
    IF NOT P_DISP IS INITIAL.
      PERFORM DISPLAY_REPROT.
    ELSEIF NOT P_LOCAL IS INITIAL.


      CLEAR: GS_PATH_NAME.
      GS_PATH_NAME = 'SDS_STK_BALANCE'.
      PERFORM F_TEXT_EXPORT_LOCAL.
      CONCATENATE P_PATH GS_PATH_NAME INTO GS_PATH_NAME SEPARATED BY '\'.
      PERFORM EXPORT USING GS_PATH_NAME
                           GT_EXPORT_TXT.



    ELSE.

      PERFORM F_TEXT_EXPORT.



    ENDIF.
  ELSE.
    IF P_LOCAL IS INITIAL.
      PERFORM F_TEXT_EXPORT.
    ENDIF.
*    MESSAGE i004.
    EXIT.
  ENDIF.
*--------------------------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*

FORM F_GET_DATA .


  SELECT A~MATNR A~LGPBE A~LGORT A~LABST
         B~MATKL
         D~MAKTX
  INTO TABLE GT_MARD
  FROM MARD AS A INNER JOIN MARA AS B
                 ON ( A~MATNR EQ B~MATNR )
                 INNER JOIN MAKT AS D
                 ON ( B~MATNR EQ D~MATNR
                 AND  D~SPRAS EQ 'E' )
  WHERE  A~LGORT IN S_LGORT
    AND  A~WERKS IN S_WERKS
  AND  A~LABST NE 0.






ENDFORM.   "Get data.

*&---------------------------------------------------------------------*
*&      Form  MAP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM F_MAP_DATA.

  LOOP AT GT_MARD INTO GS_MARD.
    CLEAR: GS_ITAB.



    GS_ITAB-MATNR = GS_MARD-MATNR.
    GS_ITAB-MAKTX = GS_MARD-MAKTX.
    GS_ITAB-MATKL = GS_MARD-MATKL.
    GS_ITAB-LGPBE = GS_MARD-LGPBE.
    GS_ITAB-LGORT = GS_MARD-LGORT.
    GS_ITAB-LABST = GS_MARD-LABST.




    APPEND GS_ITAB TO GT_ITAB.


    GS_STKBAL_WMS_TXT2-MATNR = GS_ITAB-MATNR.
    GS_STKBAL_WMS_TXT2-MAKTX = GS_ITAB-MAKTX.
    GS_STKBAL_WMS_TXT2-MATKL = GS_ITAB-MATKL.
    GS_STKBAL_WMS_TXT2-LGPBE = GS_ITAB-LGPBE.
    GS_STKBAL_WMS_TXT2-LGORT = GS_ITAB-LGORT.
    GS_STKBAL_WMS_TXT2-LABST = GS_ITAB-LABST.

    APPEND GS_STKBAL_WMS_TXT2 TO GT_STKBAL_WMS_TXT.


  ENDLOOP.



ENDFORM.  "map data

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

  CLEAR: GS_ITAB.

  GS_SPACE1    = CL_ABAP_CONV_IN_CE=>UCCP( GC_SPACE1 ).

  LOOP AT GT_STKBAL_WMS_TXT INTO GS_STKBAL_WMS_TXT1.
    CONCATENATE  GS_STKBAL_WMS_TXT1-MATNR
                 GS_STKBAL_WMS_TXT1-MAKTX
                 GS_STKBAL_WMS_TXT1-MATKL
                 GS_STKBAL_WMS_TXT1-LGPBE
                 GS_STKBAL_WMS_TXT1-LGORT
                 GS_STKBAL_WMS_TXT1-LABST
                INTO GS_TEXT_EXPORT RESPECTING BLANKS.

    L_POS = STRLEN( GS_TEXT_EXPORT ).
    WHILE L_POS < 111.

      GS_TEXT_EXPORT+L_POS(1) = G_SPACE.
      CONCATENATE GS_TEXT_EXPORT  GS_SPACE1 INTO GS_TEXT_EXPORT.
      L_POS = L_POS + 1.
    ENDWHILE.
    APPEND GS_TEXT_EXPORT TO GT_EXPORT_TXT.


    CONCATENATE 'STKBL_' SY-DATUM '_' SY-TIMLO '.txt' INTO GS_PATH_NAME.


  ENDLOOP.





ENDFORM.
**&---------------------------------------------------------------------*
**&      F_TEXT_Export
**&---------------------------------------------------------------------*
FORM F_TEXT_EXPORT.
  DATA: G_SPACE TYPE STRING,
        L_POS   TYPE I.
  G_SPACE = CL_ABAP_CONV_IN_CE=>UCCP( '00a0' ).
  DATA: LV_SCB_CORPORATE_NAME(30) TYPE C,
        LV_SCB_CORPORATE_CODE(5)  TYPE C,
        LV_TO_BANK(10)            TYPE C,
        LV_DATE_SEND(12)          TYPE C.
  DATA: CT_RESULT TYPE TABLE OF STRING.
  DATA: CS_STRING TYPE STRING.

  CLEAR: GS_ITAB.

  GS_SPACE1    = CL_ABAP_CONV_IN_CE=>UCCP( GC_SPACE1 ).

*      LOOP AT gt_stkbal_wms_txt INTO gs_stkbal_wms_txt1.
*           CONCATENATE  gs_stkbal_wms_txt1-matnr
*                        gs_stkbal_wms_txt1-maktx
*                        gs_stkbal_wms_txt1-matkl
*                        gs_stkbal_wms_txt1-lgpbe
*                        gs_stkbal_wms_txt1-lgort
*                        gs_stkbal_wms_txt1-labst
*
*
*                       INTO gs_text_export RESPECTING BLANKS.
*                        l_pos = strlen( gs_text_export ).
*                        WHILE l_pos < 94.
*
**                            gs_text_export+l_pos(1) = g_space.
*                            CONCATENATE gs_text_export  gs_space1 INTO gs_text_export.
*                            l_pos = l_pos + 1.
*                        ENDWHILE.
*                       APPEND gs_text_export TO gt_export_txt.
**                       CS_STRING  = gs_text_export.
**                       APPEND CS_STRING TO CT_RESULT.
*
*
**                CONCATENATE 'STKBL_' sy-datum '_' sy-timlo '.txt' INTO gs_path_name.
*
*
*      ENDLOOP.

*  CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING I_ITEM      = GT_STKBAL_WMS_TXT "gt_export_txt
*                                                                I_FIX_LEN   = 'X'
*                                                                I_LEN       = 111 ).
**                                                             I_SEPARATOR = '","'
**                                                             I_START_END_VALUE = '"').

  CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING "I_HEADER    = LT_HEADER
                                                                I_ITEM      = GT_STKBAL_WMS_TXT
                                                                I_SEPARATOR = '|').
*                                                                I_START_END_VALUE = '|').

  DATA : LCL_FTP TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.

  DATA : LV_STATUS TYPE C.

  DATA : LV_FILE TYPE EVE_TT_STRING.

  DATA : LT_FILE TYPE EREC_T_STRING,
         LS_FILE LIKE LINE OF LT_FILE.
  DATA: LV_PATH(100) TYPE C.
  DATA: LV_PATH_FILE_SMP TYPE STRING.
  DATA: LV_PATH_FILE_SONY_SP TYPE STRING.

*  IF P_SMP IS NOT INITIAL.
*    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = 'ZSDSMMR0130'
*                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE "--> ต้องการค่าเดียว ใส่ ABAP_TRUE ต้องการหลายค่าให้ Comment
*                                                  I_PARAM             = SY-SYSID "LC_CON-SEPARATOR
*                                                  I_PARAM_EXT         = 'SMP'
*                                        CHANGING  C_RETURN            = LV_PATH_FILE_SMP ).
*  ELSE.
*    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = 'ZSDSMMR0130'
*                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE "--> ต้องการค่าเดียว ใส่ ABAP_TRUE ต้องการหลายค่าให้ Comment
*                                                  I_PARAM             = SY-SYSID"LC_CON-SEPARATOR
*                                                  I_PARAM_EXT         = 'SONY_SP'
*                                        CHANGING  C_RETURN            = LV_PATH_FILE_SONY_SP ).
*  ENDIF.
*  IF sy-sysid = 'F36'.
*     LV_PATH_FILE_SMP = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/WMS_SMP/ZMMI011'.
*     LV_PATH_FILE_SONY_SP = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/Sony_Logi/ZMMI014'.
*  ELSEIF sy-sysid = 'F46'.
*     LV_PATH_FILE_SMP = '/interface/Z_DS/SDS/20_OUTBOUND/WMS_SMP/ZMMI011'.
*     LV_PATH_FILE_SONY_SP = '/interface/Z_DS/SDS/20_OUTBOUND/Sony_Logi/ZMMI014'.
*  ELSE.
**     LV_PATH_FILE_SF = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/DSS/ZMMI008'.
**     LV_PATH_FILE_SONY_SP = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/Sony_Logi/ZMMI014'.
*  ENDIF.

  IF P_SMP IS NOT INITIAL.
      LV_PATH_FILE_SMP = P_PATH.

  ELSE.
      LV_PATH_FILE_SONY_SP = P_PATH.
  ENDIF.

*BREAK-POINT.

  IF LCL_FTP IS NOT BOUND.
    CREATE OBJECT LCL_FTP.
  ENDIF.
  CONCATENATE 'STKBL_' SY-DATUM '_' SY-TIMLO '.txt' INTO LV_PATH .
  IF P_SMP IS NOT INITIAL.
    LV_STATUS = LCL_FTP->FTP_FILE_PUT( "I_WINDOW_PATH = 'DEALER_CLAIM/DEV/OUT'
*                                     I_AL11_PATH   = '/tmp'
                                    I_AL11_PATH   = LV_PATH_FILE_SMP
                                   I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
                                  " I_USER        = 'ds'
                                   "I_PASS        = 'ds=20240521'
                                  " I_IP          = '172.31.136.250'
                                  " I_PORT        = '21'
                                    I_DATA_SPIDER = 'X'
                                   IT_DATA       = CT_RESULT ).
*       LV_STATUS = LCL_FTP->FTP_FILE_PUT( I_WINDOW_PATH = 'SMP/DEV/OUT/STOCK_BALANCE_INFORMATION'
*                                          I_AL11_PATH   = '/tmp'
*                                          I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
*                                          I_USER        = 'ds'
*                                          I_PASS        = 'ds=20240521'
*                                          I_IP          = '172.31.136.250'
*                                          I_PORT        = '21'
*                                          IT_DATA       = CT_RESULT ).
  ELSE."SONY
    LV_STATUS = LCL_FTP->FTP_FILE_PUT( "I_WINDOW_PATH = 'DEALER_CLAIM/DEV/OUT'
*                                     I_AL11_PATH   = '/tmp'
                                    I_AL11_PATH   = LV_PATH_FILE_SONY_SP
                                   I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
                                  " I_USER        = 'ds'
                                   "I_PASS        = 'ds=20240521'
                                  " I_IP          = '172.31.136.250'
                                  " I_PORT        = '21'
                                    I_DATA_SPIDER = 'X'
                                   IT_DATA       = CT_RESULT ).
*      LV_STATUS = LCL_FTP->FTP_FILE_PUT( I_WINDOW_PATH = 'SONY_SPARE_PART/DEV/OUT'
*                                          I_AL11_PATH   = LV_PATH_FILE_SONY_SP
*                                          I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
*                                          I_USER        = 'ds'
*                                          I_PASS        = 'ds=20240521'
*                                          I_IP          = '172.31.136.249'
*                                          I_PORT        = '21'
*                                          IT_DATA       = CT_RESULT ).
  ENDIF.

  IF LV_STATUS EQ 'S'.
    " SUCCESS FTP FILE
  ELSE.
    " CANNOT FTP FILE
  ENDIF.



ENDFORM.


***&---------------------------------------------------------------------*
***&      Form  CUSTOMER
***&---------------------------------------------------------------------*
*FORM get_customer USING    p_kunnr TYPE ihpa-parnr
*                           p_adrnr TYPE kna1-adrnr
*                  CHANGING VALUE(p_name_tha) TYPE c.
*
*
*   DATA: lv_adrnr TYPE kna1-adrnr.
*
*
*     CLEAR:p_name_tha.
*
*     lv_adrnr =  p_adrnr.
*
*     IF p_adrnr EQ ''.
*        SELECT SINGLE adrnr
*          INTO (lv_adrnr)
*          FROM kna1
*        WHERE kunnr EQ p_kunnr.
*
*     ENDIF.
*
*
*          SELECT addrnumber name1 name2 street str_suppl3
*                 location city2 city1 post_code1 tel_number
*                 fax_number nation
*          INTO TABLE gt_adrc
*          FROM adrc
*          WHERE addrnumber = lv_adrnr
*                AND nation = ''.
*
*
*
*           READ TABLE gt_adrc INTO wa_adrc WITH KEY addrnumber = lv_adrnr
*                                                    nation = ''.
*                 IF sy-subrc EQ 0.
*                    CONCATENATE wa_adrc-name1 wa_adrc-name2 INTO p_name_tha.
*                 ENDIF.
*
*
*
*
*ENDFORM. "
*&-----------------------------------------------------------------------------------*
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
      T_OUTTAB           = GT_ITAB
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " DISPLAY_REPROT



* Add by K 20110215
*---------------------------------------------------------------------*
*       FORM User_command
*---------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.

  IF R_UCOMM = '&IC1'.  "Double click event

*   goto XD03 display Customer Master

*     READ TABLE gt_itab INTO gs_itab INDEX rs_selfield-tabindex.
*
*              SET: PARAMETER ID 'KUN'  FIELD gs_itab-kunnr,
*                      PARAMETER ID 'BUK'  FIELD p_bukrs  .
*              CALL TRANSACTION 'XD03'.

  ENDIF.

  CLEAR R_UCOMM.

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
    PERFORM APPEND_FIELDCAT USING 'MATNR'
                                  ''
                                  ''
                                  'Material'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].
*       Material Description
    PERFORM APPEND_FIELDCAT USING 'MAKTX'
                                  ''
                                  ''
                                  'Material Description'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].
*       Material Group
    PERFORM APPEND_FIELDCAT USING 'MATKL'
                                  ''
                                  ''
                                  'Material Group'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].
*       Storage bin
    PERFORM APPEND_FIELDCAT USING 'LGPBE'
                                  ''
                                  ''
                                  'Storage bin'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].

*       Storage Location
    PERFORM APPEND_FIELDCAT USING 'LGORT'
                                  ''
                                  ''
                                  'Storage Location'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].

*       Qty
    PERFORM APPEND_FIELDCAT USING 'LABST'
                                  ''
                                  ''
                                  'Qty'
                                   SPACE  SPACE  SPACE
                                   GT_FIELDCAT[].





  ENDIF.


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

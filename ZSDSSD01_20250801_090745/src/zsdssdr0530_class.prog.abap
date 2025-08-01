**&---------------------------------------------------------------------*
**& Include          ZSDSSDR0530_CLASS
**&---------------------------------------------------------------------*
*CLASS LCL_UTIL DEFINITION.
*  PUBLIC SECTION.
*    METHODS :
*      CONSTRUCTOR.
*    CLASS-METHODS :
*      CONVERT_ALPHA_IN  IMPORTING I_DATA TYPE ANY
*                        EXPORTING E_DATA TYPE ANY,
*      CONVERT_ALPHA_OUT IMPORTING I_DATA TYPE ANY
*                        EXPORTING E_DATA TYPE ANY.
*
*ENDCLASS.
*CLASS LCL_UTIL IMPLEMENTATION.
*  METHOD CONSTRUCTOR.
*
*  ENDMETHOD.
*  METHOD CONVERT_ALPHA_IN.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        INPUT  = I_DATA
*      IMPORTING
*        OUTPUT = E_DATA.
*
*  ENDMETHOD.
*  METHOD CONVERT_ALPHA_OUT.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_outPUT'
*      EXPORTING
*        INPUT  = I_DATA
*      IMPORTING
*        OUTPUT = E_DATA.
*
*  ENDMETHOD.
*ENDCLASS.
*CLASS LCL_DATA DEFINITION.
*  PUBLIC SECTION.
*    METHODS :
*      CONSTRUCTOR,
*      START_PROCESS.
*    CLASS-METHODS :
*      GET_DATA,
*      GET_ADDTIONAL_DATA,
*      SHOW_REPORT,
*      SET_LAYOUT_OUTPUT,
*      BUILD_FCAT,
*      SET_SORT,
*      SET_ALV_GRID,
*      HTML_TOP_OF_PAGE.
*    CLASS-DATA :
*      LO TYPE REF TO LCL_DATA.
*ENDCLASS.
*CLASS LCL_DATA IMPLEMENTATION.
*  METHOD CONSTRUCTOR.
*
*  ENDMETHOD.
*  METHOD GET_DATA.
*    IF LO IS INITIAL.
*      CREATE OBJECT LO.
*    ENDIF.
*
*    LO->START_PROCESS( ).
*  ENDMETHOD.
*  METHOD START_PROCESS.
*
*    GS_RESULT-VBELN_QT             = ''.
*    GS_RESULT-VBELN_SO             = ''.
*    GS_RESULT-VBELN_DO             = ''.
*    GS_RESULT-VBELN_IV             = ''.
*    GS_RESULT-LIFSP                = ''.
*    GS_RESULT-AUART                = ''.
*    GS_RESULT-POSNR                = ''.
*    GS_RESULT-UEPOS                = ''.
*    GS_RESULT-ETENR                = ''.
*    GS_RESULT-EDATU                = ''.
*    GS_RESULT-ERDAT                = ''.
*    GS_RESULT-MATNR                = ''.
*    GS_RESULT-WMENG                = ''.
*    GS_RESULT-BMENG                = ''.
*    GS_RESULT-VRKME                = ''.
*    GS_RESULT-MEINS                = ''.
*    GS_RESULT-LGOBE                = ''.
*    GS_RESULT-PRODH_MAT            = ''.
*    GS_RESULT-PRODH                = ''.
*    GS_RESULT-PRODH1               = ''.
*    GS_RESULT-PRODH2               = ''.
*    GS_RESULT-PRODH3               = ''.
*    GS_RESULT-MVGR1                = ''.
*    GS_RESULT-AUDAT                = ''.
*    GS_RESULT-KVGR1                = ''.
*    GS_RESULT-KVGR1_TX             = ''.
*    GS_RESULT-KVGR2                = ''.
*    GS_RESULT-KVGR2_TX             = ''.
**    GS_RESULT-CREDIT_LIMIT         = ''.
**    GS_RESULT-CREDIT_CURRENCY      = ''.
*    GS_RESULT-CMGST_TX             = ''.
*    GS_RESULT-CMGST_USR            = ''.
*    GS_RESULT-CMGST_DATE           = ''.
*    GS_RESULT-CMGST_TIME           = ''.
*    GS_RESULT-BILLTO               = ''.
*    GS_RESULT-BILLTO_NAME_EN       = ''.
*    GS_RESULT-BILLTO_NAME_TH       = ''.
*    GS_RESULT-BILLTO_CITY1         = ''.
*    GS_RESULT-BILLTO_STREET        = ''.
**    GS_RESULT-WAERK                = ''.
**    GS_RESULT-AMT_BOM              = ''.
*    GS_RESULT-TOTAL_BOM            = ''.
*    GS_RESULT-PRICE_UNIT           = ''.
*    GS_RESULT-NETPRICE_UNIT        = ''.
*    GS_RESULT-NET_AMT              = ''.
*    GS_RESULT-TOTAL_AMT            = ''.
*    GS_RESULT-TAX_AMT              = ''.
*    GS_RESULT-HEAD_DISC            = ''.
*    GS_RESULT-TAX_AMT_ITEM         = ''.
*    GS_RESULT-CSH_DISC             = ''.
*    GS_RESULT-ERDAT_DO             = ''.
*    GS_RESULT-ERZET_DO             = ''.
*    GS_RESULT-BLDAT_DO             = ''.
*    GS_RESULT-CO_APPV_STAT         = ''.
*    GS_RESULT-CO_APPV_BY           = ''.
*    GS_RESULT-CO_APPV_DATE         = ''.
*    GS_RESULT-CO_APPV_TIME         = ''.
*    GS_RESULT-MGR_APPV_STAT        = ''.
*    GS_RESULT-WAITING_APPV         = ''.
*    GS_RESULT-MGR_APPV_BY          = ''.
*    GS_RESULT-MGR_APPV_DATE        = ''.
*    GS_RESULT-MGR_APPV_TIME        = ''.
*    GS_RESULT-AUGRU_TX             = ''.
*    GS_RESULT-BSTKD                = ''.
*    GS_RESULT-REASON               = ''.
*    GS_RESULT-BSTKD_E              = ''.
*    GS_RESULT-POSEX_E              = ''.
*    GS_RESULT-ERDAT_QT             = ''.
*    GS_RESULT-ABGRU                = ''.
*    GS_RESULT-ABGRU_TX             = ''.
*    GS_RESULT-REQUEST_REMARK       = ''.
*    GS_RESULT-VKGRP                = ''.
*    GS_RESULT-VKGRP_TX             = ''.
*    GS_RESULT-PERNR                = ''.
*    GS_RESULT-PERNR_NAME           = ''.
*    GS_RESULT-VKBUR                = ''.
*    GS_RESULT-VKBUR_TX             = ''.
*    GS_RESULT-SHIPTO_H_ADR         = ''.
*    GS_RESULT-SHIPTO_I_ADR         = ''.
*    GS_RESULT-SHIPTO               = ''.
*    GS_RESULT-SHIPTO_NAME_EN       = ''.
*    GS_RESULT-SHIPTO_NAME_TH       = ''.
*    GS_RESULT-ERNAM                = ''.
*    GS_RESULT-BNAME                = ''.
*    GS_RESULT-ZZ1_LOB_SO_SDI       = ''.
*    GS_RESULT-PS_PSP_PNR           = ''.
*    GS_RESULT-PSPHI                = ''.
*    GS_RESULT-POST1                = ''.
*    GS_RESULT-MONTH                = ''.
*    GS_RESULT-YEAR                 = ''.
*    GS_RESULT-UPMAT                = ''.
*    GS_RESULT-FKDAT                = ''.
*    GS_RESULT-XBLNR                = ''.
*    GS_RESULT-BELNR                = ''.
*    GS_RESULT-CM_NO                = ''.
*    GS_RESULT-ADV_DMBTR            = ''.
*    GS_RESULT-ADV_BELNR            = ''.
*    GS_RESULT-ZTERM                = ''.
*    GS_RESULT-ZTERM_TX             = ''.
*    GS_RESULT-FKSAK_TX             = ''.
*    GS_RESULT-LFSTK_TX             = ''.
*    GS_RESULT-SPSTG_TX             = ''.
*    GS_RESULT-GBSTK_TX             = ''.
*    GS_RESULT-INV_REMARK           = ''.
*    GS_RESULT-ITEM_REMARK          = ''.
*    GS_RESULT-LAND_NO              = ''.
*    GS_RESULT-ZZPOB                = ''.
*    GS_RESULT-INV_REF              = ''.
*    GS_RESULT-RUNNO                = ''.
*
*APPEND GS_RESULT TO GT_RESULT.
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*  ENDMETHOD.
*  METHOD GET_ADDTIONAL_DATA.
**    FIELD-SYMBOLS <LFS_RESULT> LIKE LINE OF GT_RESULT.
**    LOOP AT GT_RESULT ASSIGNING <LFS_RESULT>.
**
**    ENDLOOP.
*  ENDMETHOD.
*  METHOD SHOW_REPORT.
*    SET_LAYOUT_OUTPUT( ).
*    BUILD_FCAT( ).
*    SET_SORT( ).
*    SET_ALV_GRID( ).
*  ENDMETHOD.
*  METHOD SET_LAYOUT_OUTPUT.
**    CONSTANTS : BEGIN OF LC_CON,
**                  CHK_FILED TYPE C LENGTH 5 VALUE 'CHECK',
**                END OF LC_CON.
*    GS_LAYOUT-ZEBRA             = GC_X.
*    GS_LAYOUT-COLWIDTH_OPTIMIZE = GC_X.
**    GS_LAYOUT-BOX_FIELDNAME     = LC_CON-CHK_FILED.
*  ENDMETHOD.
*  METHOD BUILD_FCAT.
*    DATA:
*       LS_FCAT TYPE SLIS_FIELDCAT_ALV.
*
**    CONSTANTS : BEGIN OF LC_CON,
**                  CHK_FILED TYPE C LENGTH 5 VALUE 'CHECK',
**                  CHK_NAME  TYPE C LENGTH 3 VALUE 'CHK',
**                END OF LC_CON.
**
**    CLEAR LS_FCAT.
**    LS_FCAT-FIELDNAME   = LC_CON-CHK_FILED.
**    LS_FCAT-SELTEXT_S   = LC_CON-CHK_NAME.
**    LS_FCAT-SELTEXT_M   = LC_CON-CHK_NAME.
**    LS_FCAT-SELTEXT_L   = LC_CON-CHK_FILED.
**    LS_FCAT-CHECKBOX    = ABAP_TRUE.
**    LS_FCAT-INPUT       = ABAP_TRUE.
**    LS_FCAT-EDIT        = ABAP_TRUE.
**    APPEND LS_FCAT TO GT_FCAT.
*
*    DATA : LV_RUNNING  TYPE I,
*           LV_DATA     TYPE C LENGTH 6 VALUE 'TEXT-',
*           LV_RUN_TEXT TYPE C LENGTH 2.
*
*    CONSTANTS : LC_F TYPE C VALUE 'F',
*                LC_T TYPE C VALUE 'T',
*                LC_d TYPE C VALUE 'D'.
*
*    FIELD-SYMBOLS <LFS> TYPE ANY.
*
*    DATA : LV_TEXT TYPE C LENGTH 8.
**Field
*    CLEAR : LS_FCAT.
*    DO 99 TIMES.
*      ADD 1 TO LV_RUNNING.
*      LV_RUN_TEXT = LV_RUNNING.
*
*      LCL_UTIL=>CONVERT_ALPHA_IN( EXPORTING I_DATA = LV_RUN_TEXT
*                                  IMPORTING E_Data = LV_RUN_TEXT ).
*
*      IF <LFS> IS ASSIGNED.
*        UNASSIGN <LFS>.
*      ENDIF.
*      CONCATENATE LV_DATA LC_F LV_RUN_TEXT INTO LV_TEXT.
*      ASSIGN (LV_TEXT) TO <LFS>.
*      IF <LFS> IS NOT ASSIGNED.
*        EXIT.
*      ENDIF.
*      LS_FCAT-FIELDNAME = <LFS>.
**Teble Ref
*      IF <LFS> IS ASSIGNED.
*        UNASSIGN <LFS>.
*      ENDIF.
*      CONCATENATE LV_DATA LC_T LV_RUN_TEXT INTO LV_TEXT.
*      ASSIGN (LV_TEXT) TO <LFS>.
*      IF <LFS> IS ASSIGNED.
*        LS_FCAT-REF_TABNAME = <LFS>.
*      ENDIF.
**Description
*      IF <LFS> IS ASSIGNED.
*        UNASSIGN <LFS>.
*      ENDIF.
*      CONCATENATE LV_DATA LC_D LV_RUN_TEXT INTO LV_TEXT.
*      ASSIGN (LV_TEXT) TO <LFS>.
*      IF <LFS> IS ASSIGNED.
*        LS_FCAT-SELTEXT_S = <LFS>.
*        LS_FCAT-SELTEXT_M = <LFS>.
*        LS_FCAT-SELTEXT_L = <LFS>.
*      ENDIF.
*      APPEND LS_FCAT TO GT_FCAT.
*      CLEAR LS_FCAT.
*    ENDDO.
*
*  ENDMETHOD.
*  METHOD SET_SORT.
***  CLEAR gs_sort.
***  gs_sort-fieldname = 'LIFNR'.
***  gs_sort-spos = '1'.
***  gs_sort-up = 'X'.
****  gs_sort-subtot = 'X'.
***  APPEND gs_sort TO gt_sort.
*  ENDMETHOD.
*  METHOD SET_ALV_GRID.
**SAPLKKBL
*    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*      EXPORTING
*        I_CALLBACK_PROGRAM = SY-REPID
*        "I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_1'
*        "I_callback_user_command  = 'USER_COMMAND'
**       I_CALLBACK_TOP_OF_PAGE            = ' '
**       i_html_height_top  = 12
**       I_CALLBACK_HTML_TOP_OF_PAGE       = 'HTML_TOP_OF_PAGE'
**       I_CALLBACK_HTML_END_OF_LIST       = ' '
**       I_STRUCTURE_NAME   =
**       I_BACKGROUND_ID    = ' '
**       I_GRID_TITLE       =
**       I_GRID_SETTINGS    =
*        IS_LAYOUT          = GS_LAYOUT
*        IT_FIELDCAT        = GT_FCAT
**       IT_EXCLUDING       =
**       IT_SPECIAL_GROUPS  =
*        IT_SORT            = GT_SORT
**       IT_FILTER          =
**       IS_SEL_HIDE        =
*        I_DEFAULT          = GC_X
*        I_SAVE             = GC_A
**       IS_VARIANT         =
**       IT_EVENTS          =
**       IT_EVENT_EXIT      =
**       IS_PRINT           =
**       IS_REPREP_ID       =
**       I_SCREEN_START_COLUMN             = 0
**       I_SCREEN_START_LINE               = 0
**       I_SCREEN_END_COLUMN               = 0
**       I_SCREEN_END_LINE  = 0
**       I_HTML_HEIGHT_TOP  = 0
**       I_HTML_HEIGHT_END  = 0
**       IT_ALV_GRAPHICS    =
**       IT_HYPERLINK       =
**       IT_ADD_FIELDCAT    =
**       IT_EXCEPT_QINFO    =
**       IR_SALV_FULLSCREEN_ADAPTER        =
** IMPORTING
**       E_EXIT_CAUSED_BY_CALLER           =
**       ES_EXIT_CAUSED_BY_USER            =
*      TABLES
*        T_OUTTAB           = GT_RESULT
*      EXCEPTIONS
*        PROGRAM_ERROR      = 1
*        OTHERS             = 2.
*    IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*  ENDMETHOD.
*  METHOD HTML_TOP_OF_PAGE.
**  DATA: text TYPE sdydo_text_element.
**
**  CALL METHOD document->add_gap
**    EXPORTING
**      width = 100.
**  text =  'Company Code Data'.
**  CALL METHOD document->add_text
**    EXPORTING
**      text      = text
**      sap_style = 'HEADING'.
**
**  CALL METHOD document->new_line.
**  CALL METHOD document->new_line.
**  CALL METHOD document->new_line.
**
**  text = 'User Name : '.
**  CALL METHOD document->add_text
**    EXPORTING
**      text         = text
**      sap_emphasis = 'Strong'.
**
**  CALL METHOD document->add_gap
**    EXPORTING
**      width = 6.
**
**  text = sy-uname.
**  CALL METHOD document->add_text
**    EXPORTING
**      text      = text
**      sap_style = 'Key'.
**
**  CALL METHOD document->add_gap
**    EXPORTING
**      width = 50.
**
**
**  text = 'Date : '.
**  CALL METHOD document->add_text
**    EXPORTING
**      text         = text
**      sap_emphasis = 'Strong'.
**
**  CALL METHOD document->add_gap
**    EXPORTING
**      width = 6.
**
**  text = sy-datum.
**  CALL METHOD document->add_text
**    EXPORTING
**      text      = text
**      sap_style = 'Key'.
**
**  CALL METHOD document->add_gap
**    EXPORTING
**      width = 50.
**
**  text = 'Time : '.
**  CALL METHOD document->add_text
**    EXPORTING
**      text         = text
**      sap_emphasis = 'Strong'.
**
**  CALL METHOD document->add_gap
**    EXPORTING
**      width = 6.
**
**  text = sy-uzeit.
**  CALL METHOD document->add_text
**    EXPORTING
**      text      = text
**      sap_style = 'Key'.
**
**  CALL METHOD document->new_line.
**  CALL METHOD document->new_line.
*  ENDMETHOD.
*ENDCLASS.
**----------------------------------------------------------------------*
** CLASS lcl_event_receiver DEFINITION
**----------------------------------------------------------------------*
*CLASS EVENT_CLASS DEFINITION.
**Handling double click
*  PUBLIC SECTION.
*    METHODS:
*    HANDLE_DOUBLE_CLICK
*    FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
*ENDCLASS. "lcl_event_receiver DEFINITION
**----------------------------------------------------------------------*
**----------------------------------------------------------------------*
** CLASS lcl_event_receiver IMPLEMENTATION
**----------------------------------------------------------------------*
*CLASS EVENT_CLASS IMPLEMENTATION.
*  METHOD HANDLE_DOUBLE_CLICK.
*
*  ENDMETHOD. "handle_double_click
*ENDCLASS. "lcl_event_receiver IMPLEMENTATION

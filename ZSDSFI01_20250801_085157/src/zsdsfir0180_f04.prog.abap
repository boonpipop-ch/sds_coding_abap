*----------------------------------------------------------------------*
***INCLUDE ZETX001_F04.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DISPLAY_DATA USING VALUE(PI_SENT).

  DATA: LT_SLIS_FCAT  TYPE LVC_T_FCAT.

  DATA: LWA_LAYOUT  TYPE LVC_S_LAYO,
        LWA_VARIANT TYPE DISVARIANT.

  DATA: LV_GRID_TITLE TYPE LVC_TITLE.

  IF GT_HEAD IS INITIAL.
    MESSAGE S000(38) WITH TEXT-M01 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF RB_TRN EQ ABAP_TRUE.
    CONCATENATE SY-TITLE ':' TEXT-H01 INTO LV_GRID_TITLE SEPARATED BY SPACE.
  ELSEIF RB_REJ EQ ABAP_TRUE.
    CONCATENATE SY-TITLE ':' TEXT-H02 INTO LV_GRID_TITLE SEPARATED BY SPACE.
  ENDIF.

  PERFORM : SET_LAYOUT                 CHANGING LWA_LAYOUT,
            SET_VARIANT                CHANGING LWA_VARIANT,

            LVC_FIELDCATALOG_MERGE     USING    GC_DOC_HEADER
                                                'GT_HEAD'
                                       CHANGING LT_SLIS_FCAT,

            MANAGE_FCAT_HEAD           USING    'GT_HEAD'
                                                PI_SENT
                                       CHANGING LT_SLIS_FCAT,

            REUSE_ALV_GRID_DISPLAY_LVC TABLES   GT_HEAD
                                        USING   'PF_STATUS_HEAD'
                                                'USER_COMMAND'
                                                'HTML_TOP_OF_PAGE'
                                                LV_GRID_TITLE
                                                LWA_LAYOUT
                                                LWA_VARIANT
                                                LT_SLIS_FCAT.



ENDFORM.                    "display_data
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT
*&---------------------------------------------------------------------*
FORM SET_LAYOUT CHANGING PWA_LAYOUT TYPE LVC_S_LAYO.

  PWA_LAYOUT-ZEBRA      = ABAP_ON.
  PWA_LAYOUT-CWIDTH_OPT = ABAP_ON.
  PWA_LAYOUT-STYLEFNAME = 'STYL'.

ENDFORM.                    "set_layout

*&---------------------------------------------------------------------*
*&      FORM  PF_STATUS_HEAD
*&---------------------------------------------------------------------*
FORM PF_STATUS_HEAD USING PI_COMMAND.

  DATA: LT_EXCLUDE TYPE TABLE OF SY-UCOMM.


  APPEND: '&EB9' TO LT_EXCLUDE,
          '&REFRESH' TO LT_EXCLUDE,
          '&RNT_PREV' TO LT_EXCLUDE,
          '&AQW' TO LT_EXCLUDE,
          '%SL' TO LT_EXCLUDE,
          '&ABC' TO LT_EXCLUDE,
          '&GRAPH' TO LT_EXCLUDE,
          '&INFO' TO LT_EXCLUDE.

*  IF rb_cre EQ abap_true.
*    APPEND: '&ZCANC' TO lt_exclude.
*  ELSEIF rb_cnc EQ abap_true.
*    APPEND: '&ZSEND' TO lt_exclude.
*  ENDIF.

  SET PF-STATUS 'PF_STATUS_HEAD' EXCLUDING LT_EXCLUDE.

ENDFORM.                    " user_command_set

*&---------------------------------------------------------------------*
*& Form LVC_FIELDCATALOG_MERGE
*&---------------------------------------------------------------------*
FORM LVC_FIELDCATALOG_MERGE  USING PI_STRUCTURE_ETAX TYPE DD02L-TABNAME
                                   PI_TAB_NAME
                          CHANGING PT_SLIS_FCAT TYPE LVC_T_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     i_buffer_active        = 'X'
      I_STRUCTURE_NAME       = PI_STRUCTURE_ETAX
*     i_client_never_display = 'X'
*     i_bypassing_buffer     = 'X'
      I_INTERNAL_TABNAME     = PI_TAB_NAME
    CHANGING
      CT_FIELDCAT            = PT_SLIS_FCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
            DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF SY-BATCH IS NOT INITIAL.
*    PERFORM filter_fieldcatalog CHANGING pt_slis_fcat.
  ENDIF.

ENDFORM.                    "lvc_fieldcatalog_merge
*&---------------------------------------------------------------------*
*& Form MANAGE_FCAT_HEAD
*&---------------------------------------------------------------------*
FORM MANAGE_FCAT_HEAD     USING PI_TAB_NAME
                                VALUE(PI_SENT)
                       CHANGING CHT_SLIS_FCAT TYPE LVC_T_FCAT.

  DATA: LWA_FCAT           TYPE LVC_S_FCAT.
  DATA: LV_COL             TYPE I.
  FIELD-SYMBOLS <LFS_FCAT> TYPE LVC_S_FCAT.

  DESCRIBE TABLE CHT_SLIS_FCAT LINES LV_COL.

  IF SY-BATCH IS INITIAL.
    M_ADD_FIELDCAT LWA_FCAT CHT_SLIS_FCAT: 'SEL_CB' PI_TAB_NAME,
                                           'MAIL_CB' PI_TAB_NAME.
  ENDIF.

  M_ADD_FIELDCAT LWA_FCAT CHT_SLIS_FCAT: 'ICON' PI_TAB_NAME,
                                         'MESSG' PI_TAB_NAME,
                                         'E_MAIL' PI_TAB_NAME,
                                         'CANCEL' PI_TAB_NAME,
                                         'RD_DOC_TYPE_DESC' PI_TAB_NAME,
                                         'SAP_DOC_TYPE_DESC' PI_TAB_NAME,
                                         'LINK_PARTNER' PI_TAB_NAME,
                                         'LINK_REPLACE_DOC' PI_TAB_NAME,
*                                         'RD_DOC_RESN_DESC' pi_tab_name,
                                         'LINK_REF_DOC' PI_TAB_NAME,
*                                         'TAX_CODE' pi_tab_name,
*                                         'VAT_RATE' pi_tab_name,
                                         'GROSS_AMT' PI_TAB_NAME,
*                                         'RD_VAT_TYPE' pi_tab_name,
                                         'DISC_AMT' PI_TAB_NAME,
                                         'CHARGE_AMT' PI_TAB_NAME,
                                         'LINK_DISC_CHARGE' PI_TAB_NAME,
                                         'LINK_VAT' PI_TAB_NAME,
                                         'ERNAM' PI_TAB_NAME, "INS CH11
                                         'CPUDT' PI_TAB_NAME. "INS CH11


  LOOP AT CHT_SLIS_FCAT ASSIGNING <LFS_FCAT>.
*  &1-reptext   = &2.
*  &1-checkbox  = &3.
*  &1-edit      = &4.
*  &1-hotspot   = &5.
*  &1-tech      = &6.
*  &1-just      = &7.
*  &1-convexit  = &8.
*  &1-col_pos   = &9.
    CASE <LFS_FCAT>-FIELDNAME.
      WHEN 'SEL_CB'.
        IF PI_SENT EQ 'X'.
          %FCAT <LFS_FCAT> TEXT-A01 'X' 'X' '' 'X' '' '' 1.
        ELSE.
          %FCAT <LFS_FCAT> TEXT-A01 'X' 'X' '' '' '' '' 1.
        ENDIF.
      WHEN 'MAIL_CB'.
        IF RB_REJ EQ 'X' OR PI_SENT EQ 'X'.
          %FCAT <LFS_FCAT> TEXT-A05 'X' 'X' '' 'X' '' '' 2.
        ELSE.
          %FCAT <LFS_FCAT> TEXT-A05 'X' 'X' '' '' '' '' 2.
        ENDIF.
      WHEN 'ICON'.
        %FCAT <LFS_FCAT> TEXT-A03 '' '' '' '' '' '' 3.
      WHEN 'MESSG'.
        %FCAT <LFS_FCAT> TEXT-A04 '' '' '' '' '' '' 4.
      WHEN 'MOBILE_PHONE'.
        %FCAT <LFS_FCAT> TEXT-A83 '' '' '' '' '' '' 5.
      WHEN 'E_MAIL'.
        %FCAT <LFS_FCAT> TEXT-A02 '' '' '' '' '' '' 5.
      WHEN 'SAP_DOC_NO'.
        %FCAT <LFS_FCAT> TEXT-A06 '' '' 'X' '' '' '' 6.
      WHEN 'DOCUMENT_NO'.
        %FCAT <LFS_FCAT> TEXT-A07 '' '' '' '' '' '' 7.
      WHEN 'SAP_POSTING_DATE'.
        %FCAT <LFS_FCAT> TEXT-A08 '' '' '' '' '' '' 8.
      WHEN 'RD_DOC_TYPE'.
        %FCAT <LFS_FCAT> TEXT-A09 '' '' '' '' '' '' 9.
      WHEN 'RD_DOC_TYPE_DESC'.
        %FCAT <LFS_FCAT> TEXT-A10 '' '' '' '' '' '' 10.
      WHEN 'MODULE_ETX'.
        %FCAT <LFS_FCAT> TEXT-B01 '' '' '' '' '' '' 11.
      WHEN 'SAP_DOC_TYPE'.
        %FCAT <LFS_FCAT> TEXT-A11 '' '' '' '' '' '' 11.
      WHEN 'SAP_DOC_TYPE_DESC'.
        %FCAT <LFS_FCAT> TEXT-A12 '' '' '' '' '' '' 12.
      WHEN 'CANCEL'.
        %FCAT <LFS_FCAT> TEXT-A13 '' '' '' '' '' '' 13.
      WHEN 'BUPLA'.
        %FCAT <LFS_FCAT> TEXT-A14 '' '' '' '' '' '' 14.
      WHEN 'KUNNR'.
        %FCAT <LFS_FCAT> TEXT-A15 '' '' '' '' '' GC_ALPHA 15.
      WHEN 'KUNNR_NAME'.
        %FCAT <LFS_FCAT> TEXT-A16 '' '' '' '' '' '' 16.
      WHEN 'KUNNR_BRANCH'.
        %FCAT <LFS_FCAT> TEXT-A17 '' '' '' '' '' '' 17.
      WHEN 'KUNNR_TAX_ID'.
        %FCAT <LFS_FCAT> TEXT-A18 '' '' '' '' '' '' 18.
      WHEN 'LINK_PARTNER'.
        %FCAT <LFS_FCAT> TEXT-A19 '' '' 'X' '' '' '' 19.
      WHEN 'LINK_REPLACE_DOC'.
        %FCAT <LFS_FCAT> TEXT-A20 '' '' '' '' '' '' 20.
      WHEN 'SAP_DOC_RESN'.
        %FCAT <LFS_FCAT> TEXT-A21 '' '' '' '' '' '' 21.
      WHEN 'SAP_DOC_RESN_DESC'.
        %FCAT <LFS_FCAT> TEXT-A22 '' '' '' '' '' '' 22.
      WHEN 'RD_DOC_RESN'.
        %FCAT <LFS_FCAT> TEXT-A23 '' '' '' '' '' '' 23.
      WHEN 'RD_DOC_RESN_DESC'.
        %FCAT <LFS_FCAT> TEXT-A24 '' '' '' '' '' '' 24.
      WHEN 'LINK_REF_DOC'.
        %FCAT <LFS_FCAT> TEXT-A25 '' '' 'X' '' '' '' 25.
      WHEN 'REQ_DEV_DATE'.
        %FCAT <LFS_FCAT> TEXT-A26 '' '' '' '' '' '' 26.
      WHEN 'PAY_TERM'.
        %FCAT <LFS_FCAT> TEXT-A27 '' '' '' '' '' '' 27.
      WHEN 'PAY_TERM_DESC'.
        %FCAT <LFS_FCAT> TEXT-A28 '' '' '' '' '' '' 28.
      WHEN 'PAY_DUE_DATE'.
        %FCAT <LFS_FCAT> TEXT-A29 '' '' '' '' '' '' 29.
      WHEN 'INCO_TERM'.
        %FCAT <LFS_FCAT> TEXT-A30 '' '' '' '' '' '' 30.
      WHEN 'SAP_PO_NO'.
        %FCAT <LFS_FCAT> TEXT-A31 '' '' '' '' '' '' 31.
      WHEN 'SAP_PO_DATE'.
        %FCAT <LFS_FCAT> TEXT-A32 '' '' '' '' '' '' 32.
      WHEN 'SUBJECT'.
        %FCAT <LFS_FCAT> 'Subject' '' '' '' '' '' '' 33.
        <LFS_FCAT>-NO_OUT = 'X'.
      WHEN 'CONTENT'.
        %FCAT <LFS_FCAT> 'Content' '' '' '' '' '' '' 34.
        <LFS_FCAT>-NO_OUT = 'X'.
      WHEN 'GLOBAL_DOC_NO'.
        %FCAT <LFS_FCAT> TEXT-A33 '' '' '' '' '' '' 35.
      WHEN 'SAP_CURR'.
        %FCAT <LFS_FCAT> TEXT-A34 '' '' '' '' '' '' 36.
      WHEN 'RD_CURR_CODE'.
        %FCAT <LFS_FCAT> TEXT-A35 '' '' '' '' '' '' 37.
      WHEN 'RD_VAT_TYPE'.
        %FCAT <LFS_FCAT> TEXT-A36 '' '' '' '' '' '' 38.
      WHEN 'TAX_CODE'.
        %FCAT <LFS_FCAT> TEXT-A37 '' '' '' '' '' '' 39.
      WHEN 'VAT_RATE'.
        %FCAT <LFS_FCAT> TEXT-A38 '' '' '' '' '' '' 40.
      WHEN 'GROSS_AMT'.
        %FCAT <LFS_FCAT> TEXT-A39 '' '' '' '' '' '' 41.
      WHEN 'TOTAL_DISC_AMT'.
        %FCAT <LFS_FCAT> TEXT-A40 '' '' '' '' '' '' 42.
      WHEN 'TOTAL_CHARGE_AMT'.
        %FCAT <LFS_FCAT> TEXT-A41 '' '' '' '' '' '' 43.
      WHEN 'LINK_DISC_CHARGE'.
        %FCAT <LFS_FCAT> TEXT-A42 '' '' 'X' '' '' '' 44.
      WHEN 'VAT_BASE_AMT'.
        %FCAT <LFS_FCAT> TEXT-A43 '' '' '' '' '' '' 45.
      WHEN 'NET_AMT_BF_VAT'.
        %FCAT <LFS_FCAT> TEXT-A44 '' '' '' '' '' '' 46.
      WHEN 'VAT_AMT'.
        %FCAT <LFS_FCAT> TEXT-A45 '' '' '' '' '' '' 47.
      WHEN 'NET_AMT_AFT_VAT'.
        %FCAT <LFS_FCAT> TEXT-A46 '' '' '' '' '' '' 48.
      WHEN 'LINK_VAT'.
        %FCAT <LFS_FCAT> TEXT-A47 '' '' 'X' '' '' '' 49.
      WHEN 'REF_DOC_AMT'.
        %FCAT <LFS_FCAT> TEXT-A48 '' '' '' '' '' '' 50.
      WHEN 'CORRECT_AMT'.
        %FCAT <LFS_FCAT> TEXT-A49 '' '' '' '' '' '' 51.
      WHEN 'DIFF_AMT'.
        %FCAT <LFS_FCAT> TEXT-A50 '' '' '' '' '' '' 52.
*>>> BEGIN OF INSERTION: <ETAX001> on 06.09.2020 <<<
      WHEN 'BUPLA_INFO'.
        %FCAT <LFS_FCAT> '' '' '' '' '' '' '' 53.
      WHEN 'DEPOSIT_FLAG'.
        %FCAT <LFS_FCAT> '' '' '' '' '' '' '' 54.
*>>> END OF INSERTION: <ETAX001> on 06.09.2020 <<<
      WHEN 'ERNAM'.
        %FCAT <LFS_FCAT> 'Create User' '' '' '' '' '' '' 55.  "INS CH11
      WHEN 'CPUDT'.
        %FCAT <LFS_FCAT> 'Create Date' '' '' '' '' '' '' 56.  "INS CH11

*      WHEN 'XXX'.
*        %fcat <lfs_fcat> text-a4 '' '' '' '' '' 4.
      WHEN OTHERS.
        %FCAT <LFS_FCAT> '' '' '' '' 'X' '' '' LV_COL.
        ADD 1 TO LV_COL.

    ENDCASE.
  ENDLOOP.

ENDFORM.                    "manage_fcat_head
*&---------------------------------------------------------------------*
*& Form REUSE_ALV_GRID_DISPLAY_LVC
*&---------------------------------------------------------------------*
FORM REUSE_ALV_GRID_DISPLAY_LVC TABLES PT_OUTTAB TYPE TABLE
                                USING  P_STATUS_SET
                                       P_USER_COMMAND
                                       P_TOP_OF_PAGE
                                       P_GRID_TITLE
                                       PWA_LAYOUT   TYPE LVC_S_LAYO
                                       PWA_VARIANT  TYPE DISVARIANT
                                       PT_SLIS_FCAT TYPE LVC_T_FCAT.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM          = SY-REPID
      I_CALLBACK_PF_STATUS_SET    = P_STATUS_SET
      I_CALLBACK_USER_COMMAND     = P_USER_COMMAND
*     i_callback_top_of_page      = 'TOP_OF_PAGE'
      I_CALLBACK_HTML_TOP_OF_PAGE = P_TOP_OF_PAGE "'HTML_TOP_OF_PAGE'
      I_GRID_TITLE                = P_GRID_TITLE
      IS_LAYOUT_LVC               = PWA_LAYOUT
      IT_FIELDCAT_LVC             = PT_SLIS_FCAT
      I_SAVE                      = 'X'
      IS_VARIANT                  = PWA_VARIANT
    TABLES
      T_OUTTAB                    = PT_OUTTAB
    EXCEPTIONS
      PROGRAM_ERROR               = 1
      OTHERS                      = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
            DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.                    "reuse_alv_grid_display_lvc
*&---------------------------------------------------------------------*
*&      Form  HTML_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM HTML_TOP_OF_PAGE USING TOP TYPE REF TO CL_DD_DOCUMENT.

  DATA: LR_TABLE   TYPE REF TO CL_DD_TABLE_AREA.

  DATA: LV_TEXT1(255)    TYPE C,
        LV_TEXT2(255)    TYPE C,
        LV_COUNT_SUCCESS TYPE C LENGTH 10,
        LV_COUNT_ERROR   TYPE C LENGTH 10.

  LV_COUNT_SUCCESS = GV_COUNT_SUCCESS.
  LV_COUNT_ERROR   = GV_COUNT_ERROR.
  CONDENSE: LV_COUNT_SUCCESS, LV_COUNT_ERROR.

  CALL METHOD TOP->ADD_TABLE
    EXPORTING
      NO_OF_COLUMNS = 2
      BORDER        = '0'
      WIDTH         = '300'
    IMPORTING
      TABLEAREA     = LR_TABLE.

  LV_TEXT1 = TEXT-H09.
  LV_TEXT2 = P_BUKRS.
  PERFORM SET_TOP    USING LV_TEXT1 LV_TEXT2
                  CHANGING LR_TABLE.

  LV_TEXT1 = TEXT-H10.
  LV_TEXT2 = P_GJAHR.
  PERFORM SET_TOP    USING LV_TEXT1 LV_TEXT2
                  CHANGING LR_TABLE.

  LV_TEXT1 = TEXT-H08.
  IF CB_CANCL = 'X'.
    LV_TEXT2 = GC_YES.
  ELSE.
    LV_TEXT2 = GC_NO.
  ENDIF.
  PERFORM SET_TOP    USING LV_TEXT1 LV_TEXT2
                  CHANGING LR_TABLE.

  LV_TEXT1 = TEXT-H11.
  LV_TEXT2 = GV_COUNT.
  PERFORM SET_TOP    USING LV_TEXT1 LV_TEXT2
                  CHANGING LR_TABLE.

  LV_TEXT1 = TEXT-H12.
  CONCATENATE LV_COUNT_SUCCESS LV_COUNT_ERROR
  INTO LV_TEXT2 SEPARATED BY '/'.
  PERFORM SET_TOP    USING LV_TEXT1 LV_TEXT2
                  CHANGING LR_TABLE.

ENDFORM.                    "html_top_of_page
*&---------------------------------------------------------------------*
*& Form SET_TOP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_TEXT1
*&      --> LV_TEXT2
*&---------------------------------------------------------------------*
FORM SET_TOP  USING    PI_TEXT1
                       PI_TEXT2
              CHANGING PO_TABLE TYPE REF TO CL_DD_TABLE_AREA.

  "first column
  CALL METHOD PO_TABLE->ADD_TEXT
    EXPORTING
      TEXT = PI_TEXT1.

  "second column
  CALL METHOD PO_TABLE->ADD_TEXT
    EXPORTING
      TEXT = PI_TEXT2.

  "add this new filled row
  CALL METHOD PO_TABLE->NEW_ROW.

ENDFORM.                    "set_top
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM USER_COMMAND USING PI_UCOMM      TYPE SY-UCOMM
                        PWA_SELFIELD TYPE SLIS_SELFIELD.

  DATA: LREF_GRID TYPE REF TO CL_GUI_ALV_GRID,
        LWA_HEAD  TYPE GTY_HEAD,
        LV_ERROR.

  FIELD-SYMBOLS <LFS_HEAD> LIKE LINE OF GT_HEAD.

  IF LREF_GRID IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = LREF_GRID.
  ENDIF.

  IF LREF_GRID IS NOT INITIAL.
    CALL METHOD LREF_GRID->CHECK_CHANGED_DATA.
  ENDIF.

  CASE PI_UCOMM.

    WHEN '&IC1'.

      READ TABLE GT_HEAD INTO LWA_HEAD INDEX PWA_SELFIELD-TABINDEX.
      IF SY-SUBRC = 0.
        IF PWA_SELFIELD-SEL_TAB_FIELD EQ 'GT_HEAD-SAP_DOC_NO'.
          PERFORM DISPLAY_ITEM USING LWA_HEAD ''.
        ELSEIF PWA_SELFIELD-SEL_TAB_FIELD EQ 'GT_HEAD-LINK_PARTNER'.
          PERFORM DISPLAY_PARTNER USING LWA_HEAD ''.
        ELSEIF PWA_SELFIELD-SEL_TAB_FIELD EQ 'GT_HEAD-LINK_REF_DOC'.
          PERFORM DISPLAY_REF USING LWA_HEAD ''.
        ELSEIF PWA_SELFIELD-SEL_TAB_FIELD EQ 'GT_HEAD-LINK_VAT'.
          PERFORM DISPLAY_VAT USING LWA_HEAD ''.
        ELSEIF PWA_SELFIELD-SEL_TAB_FIELD EQ 'GT_HEAD-LINK_DISC_CHARGE'.
          PERFORM DISPLAY_CHARGE USING LWA_HEAD 'X'.
        ENDIF.
      ENDIF.

    WHEN '&ZEXE'.

      PERFORM VERIFY_ERROR_EXE CHANGING LV_ERROR.
      IF LV_ERROR IS INITIAL AND
         RB_TRN EQ 'X'.
        PERFORM SEND_DATA.
      ELSEIF LV_ERROR IS INITIAL AND
             RB_REJ EQ 'X'.
        PERFORM REJECT_DATA.
      ELSE.
        MESSAGE S000(38) WITH TEXT-M02  "Cannot process your request.
        DISPLAY LIKE 'E'.
      ENDIF.

    WHEN '&ZPAR'.
      PERFORM DISPLAY_PARTNER USING LWA_HEAD 'X'.
    WHEN '&ZITM'.
      PERFORM DISPLAY_ITEM USING LWA_HEAD 'X'.
    WHEN '&ZREF'.
      PERFORM DISPLAY_REF USING LWA_HEAD 'X'.
    WHEN '&ZVAT'.
      PERFORM DISPLAY_VAT USING LWA_HEAD 'X'.
    WHEN '&ZALL'.

      LOOP AT GT_HEAD ASSIGNING <LFS_HEAD>
        WHERE ICON NE ICON_RED_LIGHT.
        <LFS_HEAD>-SEL_CB  = ABAP_ON.
        <LFS_HEAD>-MAIL_CB = ABAP_ON.
      ENDLOOP.

    WHEN '&ZSAL'.

      LOOP AT GT_HEAD ASSIGNING <LFS_HEAD>
        WHERE ICON NE ICON_RED_LIGHT.
        <LFS_HEAD>-SEL_CB  = ''.
        <LFS_HEAD>-MAIL_CB = ''.
      ENDLOOP.

    WHEN '&F03' OR '&F15' OR '&F12'.
      LEAVE SCREEN.

  ENDCASE.

*--- Refresh result list
  PWA_SELFIELD-REFRESH    = ABAP_ON.
  PWA_SELFIELD-COL_STABLE = ABAP_ON.
  PWA_SELFIELD-ROW_STABLE = ABAP_ON.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*& Form DISPLAY_PARTNER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEAD
*&---------------------------------------------------------------------*
FORM DISPLAY_PARTNER  USING    PI_HEAD TYPE GTY_HEAD
                               PI_LIST TYPE ABAP_BOOL.

  DATA: LT_PARTNER    TYPE GTTY_DOC_PARTNER,
        LT_SLIS_FCAT  TYPE LVC_T_FCAT,
        LWA_LAYOUT    TYPE LVC_S_LAYO,
        LWA_VARIANT   TYPE DISVARIANT,
        LWA_HEAD      TYPE GTY_HEAD,
        LWA_PARTNER   TYPE GTY_DOC_PARTNER,
        LV_GRID_TITLE TYPE LVC_TITLE.

  CLEAR: GV_SAP_DOC_NO,
         GV_RD_DOC_TYPE,
         GV_KUNNR.

  IF PI_LIST IS INITIAL.
    LT_PARTNER[] = PI_HEAD-IT_PARTNER[].

    PERFORM CONVERSION_EXIT USING GC_ALPHA
                                  'OUTPUT'
                                  PI_HEAD-SAP_DOC_NO
                         CHANGING GV_SAP_DOC_NO.

    PERFORM CONVERSION_EXIT USING GC_ALPHA
                                  'OUTPUT'
                                  PI_HEAD-KUNNR
                         CHANGING GV_KUNNR.

    GV_RD_DOC_TYPE = PI_HEAD-RD_DOC_TYPE.

  ELSE.

    LOOP AT GT_HEAD INTO LWA_HEAD.
      CLEAR LWA_PARTNER.
      LOOP AT LWA_HEAD-IT_PARTNER INTO LWA_PARTNER.
        APPEND LWA_PARTNER TO LT_PARTNER.
      ENDLOOP.
    ENDLOOP.

    GV_SAP_DOC_NO  = '-'.
    GV_RD_DOC_TYPE = '-'.
    GV_KUNNR       = '-'.

  ENDIF.

  CHECK LT_PARTNER[] IS NOT INITIAL.

*  PERFORM set_grid_title CHANGING lv_grid_title.

  CONCATENATE SY-TITLE ':' TEXT-H03 INTO LV_GRID_TITLE SEPARATED BY SPACE.

  PERFORM : SET_LAYOUT               CHANGING LWA_LAYOUT,

            LVC_FIELDCATALOG_MERGE      USING GC_DOC_PARTNER
                                              'LT_PARTNER'
                                     CHANGING LT_SLIS_FCAT,

            MANAGE_FCAT_PARTNER         USING 'LT_PARTNER'
                                     CHANGING LT_SLIS_FCAT,

            REUSE_ALV_GRID_DISPLAY_LVC TABLES LT_PARTNER
                                        USING '' '' 'HTML_TOP_OF_PAGE_PARTNER'
                                              LV_GRID_TITLE
                                              LWA_LAYOUT
                                              LWA_VARIANT
                                              LT_SLIS_FCAT.

ENDFORM.                    "display_partner
*&---------------------------------------------------------------------*
*& Form MANAGE_FCAT_PARTNER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_SLIS_FCAT
*&---------------------------------------------------------------------*
FORM MANAGE_FCAT_PARTNER  USING PI_TAB_NAME
                       CHANGING CHT_SLIS_FCAT TYPE LVC_T_FCAT.

  DATA: LWA_FCAT           TYPE LVC_S_FCAT.
  DATA: LV_COL             TYPE I.
  FIELD-SYMBOLS <LFS_FCAT> TYPE LVC_S_FCAT.

  DESCRIBE TABLE CHT_SLIS_FCAT LINES LV_COL.

  M_ADD_FIELDCAT LWA_FCAT CHT_SLIS_FCAT: 'DOCUMENT_NO' PI_TAB_NAME,
*                                         'RD_DOC_TYPE' pi_tab_name,
                                         'RD_DOC_TYPE_DESC' PI_TAB_NAME,
                                         'RD_PARTNER_DESC' PI_TAB_NAME.

  LOOP AT CHT_SLIS_FCAT ASSIGNING <LFS_FCAT>.
*  &1-reptext   = &2.
*  &1-checkbox  = &3.
*  &1-edit      = &4.
*  &1-hotspot   = &5.
*  &1-tech      = &6.
*  &1-just      = &7.
*  &1-convexit  = &8.
*  &1-col_pos   = &9.
    <LFS_FCAT>-KEY = ''.
    CASE <LFS_FCAT>-FIELDNAME.
      WHEN 'SAP_DOC_NO'.
        %FCAT <LFS_FCAT> TEXT-A06 '' '' '' '' '' '' 1.
      WHEN 'DOCUMENT_NO'.
        %FCAT <LFS_FCAT> TEXT-A07 '' '' '' '' '' GC_ALPHA 2.
      WHEN 'RD_DOC_TYPE'.
        %FCAT <LFS_FCAT> TEXT-A09  '' '' '' '' '' '' 3.
      WHEN 'RD_DOC_TYPE_DESC'.
        %FCAT <LFS_FCAT> TEXT-A10  '' '' '' '' '' '' 4.
      WHEN 'RD_PARTNER'.
        %FCAT <LFS_FCAT> TEXT-A52  '' '' '' '' '' '' 5.
      WHEN 'RD_PARTNER_DESC'.
        %FCAT <LFS_FCAT> TEXT-A53  '' '' '' '' '' '' 6.
      WHEN 'KUNNR'.
        %FCAT <LFS_FCAT> TEXT-A51  '' '' '' '' '' GC_ALPHA 7.
      WHEN 'PARTNER_NAME'.
        %FCAT <LFS_FCAT> ''  '' '' '' '' '' '' 8.
      WHEN 'PARTNER_SCH_ID'.
        %FCAT <LFS_FCAT> TEXT-A54  '' '' '' '' '' '' 9.
      WHEN 'GLOBAL_ID'.
        %FCAT <LFS_FCAT> ''  '' '' '' '' '' '' 10.
      WHEN 'TAX_ID'.
        %FCAT <LFS_FCAT> ''  '' '' '' '' '' '' 11.
      WHEN 'CONTACT_NAME'.
        %FCAT <LFS_FCAT> TEXT-A55  '' '' '' '' '' '' 12.
      WHEN 'CONTACT_DEPT'.
        %FCAT <LFS_FCAT> ''  '' '' '' '' '' '' 13.
      WHEN 'POSTAL'.
        %FCAT <LFS_FCAT> TEXT-A56  '' '' '' '' '' '' 14.
      WHEN 'HOME_NO'.
        %FCAT <LFS_FCAT> ''  '' '' '' '' '' '' 15.
      WHEN 'ADDR_LINE1'.
        %FCAT <LFS_FCAT> ''  '' '' '' '' '' '' 16.
      WHEN 'ADDR_LINE2'.
        %FCAT <LFS_FCAT> ''  '' '' '' '' '' '' 17.
      WHEN 'SUB_DIST'.
        %FCAT <LFS_FCAT> TEXT-A57  '' '' '' '' '' '' 18.
      WHEN 'SUB_DIST_DESC'.
        %FCAT <LFS_FCAT> TEXT-A58  '' '' '' '' '' '' 19.
      WHEN 'DISTRICT'.
        %FCAT <LFS_FCAT> TEXT-A59  '' '' '' '' '' '' 20.
      WHEN 'DISTRICT_DESC'.
        %FCAT <LFS_FCAT> TEXT-A60  '' '' '' '' '' '' 21.
      WHEN 'PROVINCE_CODE'.
        %FCAT <LFS_FCAT> TEXT-A61  '' '' '' '' '' '' 22.
      WHEN 'PROVINCE_DESC'.
        %FCAT <LFS_FCAT> TEXT-A62  '' '' '' '' '' '' 23.
      WHEN 'COUNTRY'.
        %FCAT <LFS_FCAT> TEXT-A63  '' '' '' '' '' '' 24.
      WHEN 'COUNTRY_SCH_ID'.
        %FCAT <LFS_FCAT> TEXT-A64  '' '' '' '' '' '' 25.
      WHEN 'ADDR_NO'.
        %FCAT <LFS_FCAT> ''  '' '' '' '' '' '' 26.

      WHEN OTHERS.
        %FCAT <LFS_FCAT> '' '' '' '' 'X' '' '' LV_COL.
        ADD 1 TO LV_COL.

    ENDCASE.
  ENDLOOP.

ENDFORM.                    "manage_fcat_partner
*&---------------------------------------------------------------------*
*&      Form  HTML_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM HTML_TOP_OF_PAGE_PARTNER USING TOP TYPE REF TO CL_DD_DOCUMENT.

  DATA: LR_TABLE   TYPE REF TO CL_DD_TABLE_AREA.

  DATA: LV_TEXT1(255) TYPE C,
        LV_TEXT2(255) TYPE C.

  CALL METHOD TOP->ADD_TABLE
    EXPORTING
      NO_OF_COLUMNS = 2
      BORDER        = '0'
      WIDTH         = '300'
    IMPORTING
      TABLEAREA     = LR_TABLE.

  LV_TEXT1 = TEXT-H09.
  LV_TEXT2 = P_BUKRS.
  PERFORM SET_TOP    USING LV_TEXT1 LV_TEXT2
                  CHANGING LR_TABLE.

  LV_TEXT1 = TEXT-H10.
  LV_TEXT2 = P_GJAHR.
  PERFORM SET_TOP    USING LV_TEXT1 LV_TEXT2
                  CHANGING LR_TABLE.

  LV_TEXT1 = TEXT-A06.
  LV_TEXT2 = GV_SAP_DOC_NO.
  PERFORM SET_TOP    USING LV_TEXT1 LV_TEXT2
                  CHANGING LR_TABLE.

  LV_TEXT1 = TEXT-A09.
  LV_TEXT2 = GV_RD_DOC_TYPE.
  PERFORM SET_TOP    USING LV_TEXT1 LV_TEXT2
                  CHANGING LR_TABLE.

  LV_TEXT1 = TEXT-A15.
  LV_TEXT2 = GV_KUNNR .
  PERFORM SET_TOP    USING LV_TEXT1 LV_TEXT2
                  CHANGING LR_TABLE.

ENDFORM.                    "html_top_of_page_partner
*&---------------------------------------------------------------------*
*& Form DISPLAY_VAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEAD
*&      --> P_
*&---------------------------------------------------------------------*
FORM DISPLAY_VAT  USING        PI_HEAD TYPE GTY_HEAD
                               PI_LIST TYPE ABAP_BOOL.

  DATA: LT_VAT        TYPE GTTY_DOC_H_VAT,
        LT_SLIS_FCAT  TYPE LVC_T_FCAT,
        LWA_LAYOUT    TYPE LVC_S_LAYO,
        LWA_VARIANT   TYPE DISVARIANT,
        LWA_HEAD      TYPE GTY_HEAD,
        LWA_VAT       TYPE GTY_DOC_H_VAT,
        LV_GRID_TITLE TYPE LVC_TITLE.

  CLEAR: GV_SAP_DOC_NO,
         GV_RD_DOC_TYPE,
         GV_KUNNR.

  IF PI_LIST IS INITIAL.
    LT_VAT[] = PI_HEAD-IT_H_VAT[].


    PERFORM CONVERSION_EXIT USING GC_ALPHA
                                  'OUTPUT'
                                  PI_HEAD-SAP_DOC_NO
                         CHANGING GV_SAP_DOC_NO.

    PERFORM CONVERSION_EXIT USING GC_ALPHA
                                  'OUTPUT'
                                  PI_HEAD-KUNNR
                         CHANGING GV_KUNNR.

    GV_RD_DOC_TYPE = PI_HEAD-RD_DOC_TYPE.
  ELSE.

    LOOP AT GT_HEAD INTO LWA_HEAD.
      CLEAR LWA_VAT.
      LOOP AT LWA_HEAD-IT_H_VAT INTO LWA_VAT.
        APPEND LWA_VAT TO LT_VAT.
      ENDLOOP.
    ENDLOOP.

    GV_SAP_DOC_NO  = '-'.
    GV_RD_DOC_TYPE = '-'.
    GV_KUNNR       = '-'.

  ENDIF.

  CHECK LT_VAT[] IS NOT INITIAL.

  CONCATENATE SY-TITLE ':' TEXT-H04 INTO LV_GRID_TITLE SEPARATED BY SPACE.

  PERFORM : SET_LAYOUT               CHANGING LWA_LAYOUT,

            LVC_FIELDCATALOG_MERGE      USING GC_DOC_H_VAT
                                              'LT_VAT'
                                     CHANGING LT_SLIS_FCAT,

            MANAGE_FCAT_VAT             USING 'LT_VAT'
                                     CHANGING LT_SLIS_FCAT,

            REUSE_ALV_GRID_DISPLAY_LVC TABLES LT_VAT
                                        USING '' '' 'HTML_TOP_OF_PAGE_PARTNER'
                                              LV_GRID_TITLE
                                              LWA_LAYOUT
                                              LWA_VARIANT
                                              LT_SLIS_FCAT.
ENDFORM.                    "display_vat
*&---------------------------------------------------------------------*
*& Form MANAGE_FCAT_VAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LT_SLIS_FCAT
*&---------------------------------------------------------------------*
FORM MANAGE_FCAT_VAT      USING PI_TAB_NAME
                       CHANGING CHT_SLIS_FCAT TYPE LVC_T_FCAT.

  DATA: LWA_FCAT           TYPE LVC_S_FCAT.
  DATA: LV_COL             TYPE I.
  FIELD-SYMBOLS <LFS_FCAT> TYPE LVC_S_FCAT.

  DESCRIBE TABLE CHT_SLIS_FCAT LINES LV_COL.

  M_ADD_FIELDCAT LWA_FCAT CHT_SLIS_FCAT: 'DOCUMENT_NO' PI_TAB_NAME,
*                                         'RD_DOC_TYPE' pi_tab_name,
                                         'RD_DOC_TYPE_DESC' PI_TAB_NAME,
                                         'KUNNR' PI_TAB_NAME,
                                         'KUNNR_NAME' PI_TAB_NAME.

  LOOP AT CHT_SLIS_FCAT ASSIGNING <LFS_FCAT>.
*  &1-reptext   = &2.
*  &1-checkbox  = &3.
*  &1-edit      = &4.
*  &1-hotspot   = &5.
*  &1-tech      = &6.
*  &1-just      = &7.
*  &1-convexit  = &8.
*  &1-col_pos   = &9.
    <LFS_FCAT>-KEY = ''.
    CASE <LFS_FCAT>-FIELDNAME.
      WHEN 'SAP_DOC_NO'.
        %FCAT <LFS_FCAT> TEXT-A06 '' '' '' '' '' '' 1.
      WHEN 'DOCUMENT_NO'.
        %FCAT <LFS_FCAT> TEXT-A07 '' '' '' '' '' GC_ALPHA 2.
      WHEN 'RD_DOC_TYPE'.
        %FCAT <LFS_FCAT> TEXT-A09  '' '' '' '' '' '' 3.
      WHEN 'RD_DOC_TYPE_DESC'.
        %FCAT <LFS_FCAT> TEXT-A10  '' '' '' '' '' '' 4.
      WHEN 'KUNNR'.
        %FCAT <LFS_FCAT> TEXT-A15  '' '' '' '' '' GC_ALPHA 5.
      WHEN 'KUNNR_NAME'.
        %FCAT <LFS_FCAT> TEXT-A16  '' '' '' '' '' '' 6.
      WHEN 'RD_VAT_TYPE'.
        %FCAT <LFS_FCAT> TEXT-A36  '' '' '' '' '' '' 7.
      WHEN 'TAX_CODE'.
        %FCAT <LFS_FCAT> TEXT-A37  '' '' '' '' '' '' 8.
      WHEN 'VAT_RATE'.
        %FCAT <LFS_FCAT> ''  '' '' '' '' '' '' 9.
      WHEN 'NET_AMT_BF_VAT'.
        %FCAT <LFS_FCAT> ''  '' '' '' '' '' '' 10.
      WHEN 'VAT_AMT'.
        %FCAT <LFS_FCAT> ''  '' '' '' '' '' '' 11.

      WHEN OTHERS.
        %FCAT <LFS_FCAT> '' '' '' '' 'X' '' '' LV_COL.
        ADD 1 TO LV_COL.

    ENDCASE.
  ENDLOOP.

ENDFORM.                    "manage_fcat_vat
*&---------------------------------------------------------------------*
*& Form DISPLAY_REF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEAD
*&      --> P_
*&---------------------------------------------------------------------*
FORM DISPLAY_REF  USING        PI_HEAD TYPE GTY_HEAD
                               PI_LIST TYPE ABAP_BOOL.

  DATA: LT_REF        TYPE GTTY_DOC_H_REF,
        LT_SLIS_FCAT  TYPE LVC_T_FCAT,
        LWA_LAYOUT    TYPE LVC_S_LAYO,
        LWA_VARIANT   TYPE DISVARIANT,
        LWA_HEAD      TYPE GTY_HEAD,
        LWA_REF       TYPE GTY_DOC_H_REF,
        LV_GRID_TITLE TYPE LVC_TITLE.

  CLEAR: GV_SAP_DOC_NO,
         GV_RD_DOC_TYPE,
         GV_KUNNR.

  IF PI_LIST IS INITIAL.
    LT_REF[] = PI_HEAD-IT_H_REF[].

    PERFORM CONVERSION_EXIT USING GC_ALPHA
                                  'OUTPUT'
                                  PI_HEAD-SAP_DOC_NO
                         CHANGING GV_SAP_DOC_NO.

    PERFORM CONVERSION_EXIT USING GC_ALPHA
                                  'OUTPUT'
                                  PI_HEAD-KUNNR
                         CHANGING GV_KUNNR.

    GV_RD_DOC_TYPE = PI_HEAD-RD_DOC_TYPE.

  ELSE.

    LOOP AT GT_HEAD INTO LWA_HEAD.
      CLEAR LWA_REF.
      LOOP AT LWA_HEAD-IT_H_REF INTO LWA_REF.
        APPEND LWA_REF TO LT_REF.
      ENDLOOP.
    ENDLOOP.

    GV_SAP_DOC_NO  = '-'.
    GV_RD_DOC_TYPE = '-'.
    GV_KUNNR       = '-'.

  ENDIF.

  CHECK LT_REF[] IS NOT INITIAL.

  CONCATENATE SY-TITLE ':' TEXT-H05 INTO LV_GRID_TITLE SEPARATED BY SPACE.

  PERFORM : SET_LAYOUT               CHANGING LWA_LAYOUT,

            LVC_FIELDCATALOG_MERGE      USING GC_DOC_H_REF
                                              'LT_REF'
                                     CHANGING LT_SLIS_FCAT,

            MANAGE_FCAT_REF             USING 'LT_REF'
                                     CHANGING LT_SLIS_FCAT,

            REUSE_ALV_GRID_DISPLAY_LVC TABLES LT_REF
                                        USING '' '' 'HTML_TOP_OF_PAGE_PARTNER'
                                              LV_GRID_TITLE
                                              LWA_LAYOUT
                                              LWA_VARIANT
                                              LT_SLIS_FCAT.
ENDFORM.                    "display_ref
*&---------------------------------------------------------------------*
*& Form MANAGE_FCAT_REF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LT_SLIS_FCAT
*&---------------------------------------------------------------------*
FORM MANAGE_FCAT_REF      USING PI_TAB_NAME
                       CHANGING CHT_SLIS_FCAT TYPE LVC_T_FCAT.

  DATA: LWA_FCAT           TYPE LVC_S_FCAT.
  DATA: LV_COL             TYPE I.
  FIELD-SYMBOLS <LFS_FCAT> TYPE LVC_S_FCAT.

  DESCRIBE TABLE CHT_SLIS_FCAT LINES LV_COL.

  M_ADD_FIELDCAT LWA_FCAT CHT_SLIS_FCAT: 'DOCUMENT_NO' PI_TAB_NAME,
*                                         'RD_DOC_TYPE' pi_tab_name,
                                         'RD_DOC_TYPE_DESC' PI_TAB_NAME,
                                         'KUNNR' PI_TAB_NAME,
                                         'KUNNR_NAME' PI_TAB_NAME,
                                         'REF_RD_DOC_TYPE_DESC' PI_TAB_NAME.

  LOOP AT CHT_SLIS_FCAT ASSIGNING <LFS_FCAT>.
*  &1-reptext   = &2.
*  &1-checkbox  = &3.
*  &1-edit      = &4.
*  &1-hotspot   = &5.
*  &1-tech      = &6.
*  &1-just      = &7.
*  &1-convexit  = &8.
*  &1-col_pos   = &9.
    <LFS_FCAT>-KEY = ''.
    CASE <LFS_FCAT>-FIELDNAME.
      WHEN 'SAP_DOC_NO'.
        %FCAT <LFS_FCAT> TEXT-A06 '' '' '' '' '' '' 1.
      WHEN 'DOCUMENT_NO'.
        %FCAT <LFS_FCAT> TEXT-A07 '' '' '' '' '' GC_ALPHA 2.
      WHEN 'RD_DOC_TYPE'.
        %FCAT <LFS_FCAT> TEXT-A09 '' '' '' '' '' '' 3.
      WHEN 'RD_DOC_TYPE_DESC'.
        %FCAT <LFS_FCAT> TEXT-A10 '' '' '' '' '' '' 4.
      WHEN 'KUNNR'.
        %FCAT <LFS_FCAT> TEXT-A15 '' '' '' '' '' GC_ALPHA 5.
      WHEN 'KUNNR_NAME'.
        %FCAT <LFS_FCAT> TEXT-A16 '' '' '' '' '' '' 6.
      WHEN 'REF_DOC_NO'.
        %FCAT <LFS_FCAT> TEXT-A65 '' '' '' '' '' GC_ALPHA 7.
      WHEN 'REF_SAP_DOC_NO'.
        %FCAT <LFS_FCAT> TEXT-A84 '' '' '' '' '' GC_ALPHA 8.
      WHEN 'REF_SAP_POST_DATE'.
        %FCAT <LFS_FCAT> TEXT-A66 '' '' '' '' '' '' 9.
      WHEN 'REF_SAP_DOC_TYPE'.
        %FCAT <LFS_FCAT> '' '' '' '' '' '' '' 10.
      WHEN 'REF_BUKRS'.
        %FCAT <LFS_FCAT> '' '' '' '' '' '' '' 11.
      WHEN 'REF_GJAHR'.
        %FCAT <LFS_FCAT> '' '' '' '' '' '' GC_GJAHR 12.
      WHEN 'REF_RD_DOC_TYPE'.
        %FCAT <LFS_FCAT> TEXT-A67 '' '' '' '' '' '' 12.
      WHEN 'REF_RD_DOC_TYPE_DESC'.
        %FCAT <LFS_FCAT> TEXT-A68 '' '' '' '' '' '' 13.

      WHEN OTHERS.
        %FCAT <LFS_FCAT> '' '' '' '' 'X' '' '' LV_COL.
        ADD 1 TO LV_COL.

    ENDCASE.
  ENDLOOP.

ENDFORM.                    "manage_fcat_ref
*&---------------------------------------------------------------------*
*& Form DISPLAY_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEAD
*&      --> P_
*&---------------------------------------------------------------------*
FORM DISPLAY_ITEM  USING    PI_HEAD TYPE GTY_HEAD
                            PI_LIST TYPE ABAP_BOOL.

  DATA: LT_ITEM       TYPE GTTY_DOC_ITEM,
        LT_SLIS_FCAT  TYPE LVC_T_FCAT,
        LWA_LAYOUT    TYPE LVC_S_LAYO,
        LWA_VARIANT   TYPE DISVARIANT,
        LWA_HEAD      TYPE GTY_HEAD,
        LWA_ITEM      TYPE GTY_DOC_ITEM,
        LV_GRID_TITLE TYPE LVC_TITLE.

  REFRESH GT_CHARGE[].
  GT_CHARGE[] = PI_HEAD-IT_CHARGE[].

  CLEAR: GV_SAP_DOC_NO,
         GV_RD_DOC_TYPE,
         GV_KUNNR,
         GV_DIS_DETAIL.

  IF PI_LIST IS INITIAL.
    LT_ITEM[] = PI_HEAD-IT_ITEM[].

    PERFORM CONVERSION_EXIT USING GC_ALPHA
                                  'OUTPUT'
                                  PI_HEAD-SAP_DOC_NO
                         CHANGING GV_SAP_DOC_NO.

    PERFORM CONVERSION_EXIT USING GC_ALPHA
                                  'OUTPUT'
                                  PI_HEAD-KUNNR
                         CHANGING GV_KUNNR.

    GV_RD_DOC_TYPE = PI_HEAD-RD_DOC_TYPE.
    GV_DIS_DETAIL  = 'X'.

  ELSE.
    GV_DIS_DETAIL  = ''.

    LOOP AT GT_HEAD INTO LWA_HEAD.
      CLEAR LWA_ITEM.
      LOOP AT LWA_HEAD-IT_ITEM INTO LWA_ITEM.
        APPEND LWA_ITEM TO LT_ITEM.
      ENDLOOP.
    ENDLOOP.

    GV_SAP_DOC_NO  = '-'.
    GV_RD_DOC_TYPE = '-'.
    GV_KUNNR       = '-'.

  ENDIF.

  CHECK LT_ITEM[] IS NOT INITIAL.

  CONCATENATE SY-TITLE ':' TEXT-H06 INTO LV_GRID_TITLE SEPARATED BY SPACE.

  PERFORM : SET_LAYOUT               CHANGING LWA_LAYOUT,

            LVC_FIELDCATALOG_MERGE      USING GC_DOC_ITEM
                                              'LT_ITEM'
                                     CHANGING LT_SLIS_FCAT,

            MANAGE_FCAT_ITEM             USING 'LT_ITEM'
                                     CHANGING LT_SLIS_FCAT,

            REUSE_ALV_GRID_DISPLAY_LVC TABLES LT_ITEM
                                        USING '' 'USER_COMMAND_ITEM'
                                              'HTML_TOP_OF_PAGE_PARTNER'
                                              LV_GRID_TITLE
                                              LWA_LAYOUT
                                              LWA_VARIANT
                                              LT_SLIS_FCAT.

ENDFORM.                    "display_item
*&---------------------------------------------------------------------*
*& Form MANAGE_FCAT_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LT_SLIS_FCAT
*&---------------------------------------------------------------------*
FORM MANAGE_FCAT_ITEM  USING PI_TAB_NAME
                       CHANGING CHT_SLIS_FCAT TYPE LVC_T_FCAT.

  DATA: LWA_FCAT           TYPE LVC_S_FCAT.
  DATA: LV_COL  TYPE I,
        LV_TECH TYPE FLAG.
  FIELD-SYMBOLS <LFS_FCAT> TYPE LVC_S_FCAT.

  DESCRIBE TABLE CHT_SLIS_FCAT LINES LV_COL.

  M_ADD_FIELDCAT LWA_FCAT CHT_SLIS_FCAT: 'DOCUMENT_NO' PI_TAB_NAME,
*                                         'RD_DOC_TYPE' pi_tab_name,
                                         'RD_DOC_TYPE_DESC' PI_TAB_NAME,
                                         'KUNNR' PI_TAB_NAME,
                                         'KUNNR_NAME' PI_TAB_NAME,
*                                         'RD_UNIT' pi_tab_name,
                                         'LINK_DISC_CHARGE' PI_TAB_NAME.

  IF GV_DIS_DETAIL IS INITIAL.
    LV_TECH = 'X'.
  ENDIF.

  LOOP AT CHT_SLIS_FCAT ASSIGNING <LFS_FCAT>.
*  &1-reptext   = &2.
*  &1-checkbox  = &3.
*  &1-edit      = &4.
*  &1-hotspot   = &5.
*  &1-tech      = &6.
*  &1-just      = &7.
*  &1-convexit  = &8.
*  &1-col_pos   = &9.
    <LFS_FCAT>-KEY = ''.
    CASE <LFS_FCAT>-FIELDNAME.
      WHEN 'SAP_DOC_NO'.
        %FCAT <LFS_FCAT> TEXT-A06 '' '' '' '' '' '' 1.
      WHEN 'DOCUMENT_NO'.
        %FCAT <LFS_FCAT> TEXT-A07 '' '' '' '' '' GC_ALPHA 2.
      WHEN 'RD_DOC_TYPE'.
        %FCAT <LFS_FCAT> TEXT-A09 '' '' '' '' '' '' 3.
      WHEN 'RD_DOC_TYPE_DESC'.
        %FCAT <LFS_FCAT> TEXT-A10 '' '' '' '' '' '' 4.
      WHEN 'KUNNR'.
        %FCAT <LFS_FCAT> TEXT-A15 '' '' '' '' '' GC_ALPHA 5.
      WHEN 'KUNNR_NAME'.
        %FCAT <LFS_FCAT> TEXT-A16 '' '' '' '' '' '' 6.
      WHEN 'ITEM_NO'.
        %FCAT <LFS_FCAT> TEXT-A69 '' '' '' '' '' '' 7.
      WHEN 'ITEM_CODE'.
        %FCAT <LFS_FCAT> TEXT-A70 '' '' '' '' '' '' 8.
      WHEN 'ITEM_NAME'.
        %FCAT <LFS_FCAT> '' '' '' '' '' '' '' 9.
      WHEN 'UNIT_PRICE'.
        %FCAT <LFS_FCAT> TEXT-A72 '' '' '' '' '' '' 10.
      WHEN 'QTY'.
        %FCAT <LFS_FCAT> TEXT-A73 '' '' '' '' '' '' 11.
      WHEN 'UNIT'.
        %FCAT <LFS_FCAT> TEXT-A85 '' '' '' '' '' '' 12.
      WHEN 'RD_UNIT'.
        %FCAT <LFS_FCAT> TEXT-A74 '' '' '' '' '' '' 13.
      WHEN 'RD_VAT_TYPE'.
        %FCAT <LFS_FCAT> TEXT-A76 '' '' '' '' '' '' 14.
      WHEN 'TAX_CODE'.
        %FCAT <LFS_FCAT> TEXT-A75 '' '' '' '' '' '' 15.
      WHEN 'VAT_RATE'.
        %FCAT <LFS_FCAT> '' '' '' '' '' '' '' 16.
      WHEN 'GROSS_AMT'.
        %FCAT <LFS_FCAT> '' '' '' '' '' '' '' 17.
      WHEN 'RD_DISC_AMT'.
        %FCAT <LFS_FCAT> '' '' '' '' '' '' '' 18.
      WHEN 'CHARGE_AMT'.
        %FCAT <LFS_FCAT> '' '' '' '' '' '' '' 19.
      WHEN 'LINK_DISC_CHARGE'.
        %FCAT <LFS_FCAT> TEXT-A42 '' '' GV_DIS_DETAIL LV_TECH '' '' 20.
      WHEN 'VAT_BASE_AMT'.
        %FCAT <LFS_FCAT> '' '' '' '' '' '' '' 21.
      WHEN 'NET_AMT_BF_VAT'.
        %FCAT <LFS_FCAT> TEXT-A77 '' '' '' '' '' '' 22.
      WHEN 'VAT_AMT'.
        %FCAT <LFS_FCAT> TEXT-A78 '' '' '' '' '' '' 23.
      WHEN 'NET_AMT_AFT_VAT'.
        %FCAT <LFS_FCAT> TEXT-A79 '' '' '' '' '' '' 24.
      WHEN 'SAP_PO_NO' .
        %FCAT <LFS_FCAT> 'PO number' '' '' '' '' '' '' 25.
      WHEN 'SAP_PO_ITEM' .
        %FCAT <LFS_FCAT> 'PO item' '' '' '' '' '' '' 26.
      WHEN OTHERS.
        %FCAT <LFS_FCAT> '' '' '' '' 'X' '' '' LV_COL.
        ADD 1 TO LV_COL.

    ENDCASE.
  ENDLOOP.

ENDFORM.                    "manage_fcat_item
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM USER_COMMAND_ITEM USING PI_UCOMM      TYPE SY-UCOMM
                             PWA_SELFIELD TYPE SLIS_SELFIELD.

  DATA: LWA_HEAD TYPE GTY_HEAD,
        LV_ERROR.

  CASE PI_UCOMM.

    WHEN '&IC1'.
      IF PWA_SELFIELD-SEL_TAB_FIELD EQ 'LT_ITEM-LINK_DISC_CHARGE'.
        PERFORM DISPLAY_CHARGE USING LWA_HEAD 'X'.
      ENDIF.
  ENDCASE.

*--- Refresh result list
  PWA_SELFIELD-REFRESH    = ABAP_ON.
  PWA_SELFIELD-COL_STABLE = ABAP_ON.
  PWA_SELFIELD-ROW_STABLE = ABAP_ON.

ENDFORM.                    "user_command_item
*&---------------------------------------------------------------------*
*& Form DISPLAY_CHARGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEAD
*&      --> P_
*&---------------------------------------------------------------------*
FORM DISPLAY_CHARGE  USING  PI_HEAD TYPE GTY_HEAD
                            PI_ITEM_FLAG.

  DATA: LT_CHARGE     TYPE GTTY_DOC_DISCHG,
        LT_SLIS_FCAT  TYPE LVC_T_FCAT,
        LWA_LAYOUT    TYPE LVC_S_LAYO,
        LWA_VARIANT   TYPE DISVARIANT,
        LWA_CHARGE    TYPE GTY_DOC_DISCHG,
        LV_GRID_TITLE TYPE LVC_TITLE,
        LV_TAB_NAME   TYPE DD02L-TABNAME.

  CLEAR: GV_SAP_DOC_NO,
         GV_RD_DOC_TYPE,
         GV_KUNNR.

  IF PI_ITEM_FLAG = 'X'.
*    lt_charge[] = gt_charge[].
    LT_CHARGE[] = PI_HEAD-IT_CHARGE[].
    LV_TAB_NAME = 'LT_CHARGE'.

    READ TABLE LT_CHARGE INTO LWA_CHARGE INDEX 1.

    PERFORM CONVERSION_EXIT USING GC_ALPHA
                                  'OUTPUT'
                                  LWA_CHARGE-SAP_DOC_NO
                         CHANGING GV_SAP_DOC_NO.

    PERFORM CONVERSION_EXIT USING GC_ALPHA
                                  'OUTPUT'
                                  LWA_CHARGE-KUNNR
                         CHANGING GV_KUNNR.

    GV_RD_DOC_TYPE = LWA_CHARGE-RD_DOC_TYPE.

  ELSE.
    LT_CHARGE[] = PI_HEAD-IT_CHARGE[].
    LV_TAB_NAME = 'LT_CHARGE'.

    GV_SAP_DOC_NO  = '-'.
    GV_RD_DOC_TYPE = '-'.
    GV_KUNNR       = '-'.
  ENDIF.

  CHECK LT_CHARGE[] IS NOT INITIAL.

  CONCATENATE SY-TITLE ':' TEXT-H07 INTO LV_GRID_TITLE SEPARATED BY SPACE.

  PERFORM : SET_LAYOUT               CHANGING LWA_LAYOUT,

            LVC_FIELDCATALOG_MERGE      USING GC_DOC_DISCHG
                                              LV_TAB_NAME
                                     CHANGING LT_SLIS_FCAT,

            MANAGE_FCAT_CHARGE          USING LV_TAB_NAME
                                     CHANGING LT_SLIS_FCAT,

            REUSE_ALV_GRID_DISPLAY_LVC TABLES LT_CHARGE
                                        USING '' '' 'HTML_TOP_OF_PAGE_PARTNER'
                                              LV_GRID_TITLE
                                              LWA_LAYOUT
                                              LWA_VARIANT
                                              LT_SLIS_FCAT.
ENDFORM.                    "display_charge
*&---------------------------------------------------------------------*
*& Form MANAGE_FCAT_CHARGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_TAB_NAME
*&      <-- LT_SLIS_FCAT
*&---------------------------------------------------------------------*
FORM MANAGE_FCAT_CHARGE    USING PI_TAB_NAME
                        CHANGING CHT_SLIS_FCAT TYPE LVC_T_FCAT.

  DATA: LWA_FCAT           TYPE LVC_S_FCAT.
  DATA: LV_COL             TYPE I.
  FIELD-SYMBOLS <LFS_FCAT> TYPE LVC_S_FCAT.

  DESCRIBE TABLE CHT_SLIS_FCAT LINES LV_COL.

  M_ADD_FIELDCAT LWA_FCAT CHT_SLIS_FCAT: 'DOCUMENT_NO' PI_TAB_NAME,
*                                         'RD_DOC_TYPE' pi_tab_name,
                                         'RD_DOC_TYPE_DESC' PI_TAB_NAME,
                                         'KUNNR' PI_TAB_NAME,
                                         'KUNNR_NAME' PI_TAB_NAME,
                                         'SAP_CURR' PI_TAB_NAME,
                                         'RD_DISCHG' PI_TAB_NAME,
                                         'RD_DISCHG_DESC' PI_TAB_NAME,
                                         'CHARGE_AMT_ALV' PI_TAB_NAME.

  LOOP AT CHT_SLIS_FCAT ASSIGNING <LFS_FCAT>.
*  &1-reptext   = &2.
*  &1-checkbox  = &3.
*  &1-edit      = &4.
*  &1-hotspot   = &5.
*  &1-tech      = &6.
*  &1-just      = &7.
*  &1-convexit  = &8.
*  &1-col_pos   = &9.
    <LFS_FCAT>-KEY = ''.
    CASE <LFS_FCAT>-FIELDNAME.
      WHEN 'SAP_DOC_NO'.
        %FCAT <LFS_FCAT> TEXT-A06 '' '' '' '' '' '' 1.
      WHEN 'DOCUMENT_NO'.
        %FCAT <LFS_FCAT> TEXT-A07 '' '' '' '' '' GC_ALPHA 2.
      WHEN 'RD_DOC_TYPE'.
        %FCAT <LFS_FCAT> TEXT-A09 '' '' '' '' '' '' 3.
      WHEN 'RD_DOC_TYPE_DESC'.
        %FCAT <LFS_FCAT> TEXT-A10 '' '' '' '' '' '' 4.
      WHEN 'KUNNR'.
        %FCAT <LFS_FCAT> TEXT-A15 '' '' '' '' '' GC_ALPHA 5.
      WHEN 'KUNNR_NAME'.
        %FCAT <LFS_FCAT> TEXT-A16 '' '' '' '' '' '' 6.
      WHEN 'ITEM_NO'.
        %FCAT <LFS_FCAT> '' '' '' '' '' '' '' 7.
      WHEN 'RD_CHARGE_FLAG'.
        %FCAT <LFS_FCAT> '' '' '' '' '' '' '' 8.
      WHEN 'CHARGE_AMT'.
        LWA_FCAT = <LFS_FCAT>.
        %FCAT <LFS_FCAT> '' '' '' '' 'X' '' '' 9.
      WHEN 'CHARGE_AMT_ALV'.
        <LFS_FCAT> = LWA_FCAT.
        <LFS_FCAT>-FIELDNAME = 'CHARGE_AMT_ALV'.
        %FCAT <LFS_FCAT> '' '' '' '' '' '' '' 9.
      WHEN 'SAP_CURR'.
        %FCAT <LFS_FCAT> TEXT-A80 '' '' '' '' '' '' 10.
      WHEN 'RD_CHARGE_CODE'.
        %FCAT <LFS_FCAT> TEXT-A81 '' '' '' '' '' '' 11.
      WHEN 'CHARGE_RESN'.
        %FCAT <LFS_FCAT> TEXT-A82 '' '' '' '' '' '' 12.
      WHEN 'LINE_TYPE'.
        %FCAT <LFS_FCAT> '' '' '' '' '' '' '' 13.
      WHEN OTHERS.
        %FCAT <LFS_FCAT> '' '' '' '' 'X' '' '' LV_COL.
        ADD 1 TO LV_COL.

    ENDCASE.
  ENDLOOP.

ENDFORM.                    "manage_fcat_charge
*&---------------------------------------------------------------------*
*& Form VERIFY_ERROR_EXE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_ERROR
*&---------------------------------------------------------------------*
FORM VERIFY_ERROR_EXE  CHANGING CH_ERROR.

*>>> BOI CH08 >>>
  DATA: LS_BILL           TYPE GTY_BILL,
        LS_NEW_DOC_HEADER TYPE ZSDSFIT014,
        LV_VERIFIED       TYPE ABAP_BOOL,
        LS_MESSG          TYPE GTY_MESSG.
*<<< EOI CH08 <<<

  FIELD-SYMBOLS <LFS_HEAD> TYPE  GTY_HEAD.

  CLEAR CH_ERROR.

  "Filter error out before send data.
  LOOP AT GT_HEAD ASSIGNING <LFS_HEAD>
    WHERE SEL_CB = ABAP_ON.

*>>> BOI CH08 >>>
    "Update status
    SELECT SINGLE *
      INTO LS_NEW_DOC_HEADER
      FROM ZSDSFIT014
      WHERE BUKRS       = <LFS_HEAD>-BUKRS
        AND SAP_DOC_NO  = <LFS_HEAD>-SAP_DOC_NO
        AND GJAHR       = <LFS_HEAD>-GJAHR
        AND RD_DOC_TYPE = <LFS_HEAD>-RD_DOC_TYPE
        AND MODULE_ETX  = <LFS_HEAD>-MODULE_ETX.

    CLEAR LS_BILL.

    IF <LFS_HEAD>-MODULE_ETX = 'SD'.
      READ TABLE GT_SD INTO LS_BILL
        WITH KEY BUKRS = <LFS_HEAD>-BUKRS
                 VBELN = <LFS_HEAD>-SAP_DOC_NO
                 GJAHR = <LFS_HEAD>-GJAHR.

    ELSEIF <LFS_HEAD>-MODULE_ETX = 'FI'.
      READ TABLE GT_FI INTO LS_BILL
        WITH KEY BUKRS = <LFS_HEAD>-BUKRS
                 VBELN = <LFS_HEAD>-SAP_DOC_NO
                 GJAHR = <LFS_HEAD>-GJAHR.
    ENDIF.

    "Check Doc verification Status form cloud
    PERFORM CHECK_RD_SENT    USING LS_BILL
                                   <LFS_HEAD>-MODULE_ETX
                          CHANGING LV_VERIFIED.

    PERFORM CHECK_ERROR    USING LS_BILL
                                 LS_NEW_DOC_HEADER
                                 LV_VERIFIED
                        CHANGING <LFS_HEAD>-IT_MESSG.

    IF <LFS_HEAD>-IT_MESSG IS NOT INITIAL.
      READ TABLE <LFS_HEAD>-IT_MESSG INTO LS_MESSG INDEX 1.
      IF SY-SUBRC = 0.
        <LFS_HEAD>-ICON = LS_MESSG-ICON.
        <LFS_HEAD>-MESSG = LS_MESSG-MESSG.
      ENDIF.
    ENDIF.
*<<< EOI CH08 <<<

    IF <LFS_HEAD>-ICON = ICON_RED_LIGHT.
      <LFS_HEAD>-SEL_CB = SPACE.
    ENDIF.
    IF <LFS_HEAD>-DONE <> ABAP_FALSE.
      <LFS_HEAD>-SEL_CB = SPACE.
    ENDIF.
  ENDLOOP.

  READ TABLE GT_HEAD ASSIGNING <LFS_HEAD>
    WITH KEY SEL_CB = ABAP_ON.
  IF SY-SUBRC IS NOT INITIAL.
    CH_ERROR = ABAP_TRUE.
  ENDIF.

ENDFORM.                    "verify_error_exe
*&---------------------------------------------------------------------*
*& Form SEND_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SEND_DATA .

  DATA: LT_HEADCOUNT     TYPE TABLE OF GTY_HEAD,
        LT_HEAD          TYPE TABLE OF GTY_HEAD,
        LT_LOG           TYPE TABLE OF ZSDSFIT019,
        LT_BAPIRET2      TYPE BAPIRET2_T,
        LT_BAPIRET2_ALL  TYPE BAPIRET2_T,
        LT_ETAX_TEXTFILE TYPE ZSDSFIS033_TT.

  DATA: LWA_HEAD        TYPE GTY_HEAD,
        LWA_TEXT        LIKE LINE OF LT_ETAX_TEXTFILE,
        LWA_BAPIRET2    TYPE BAPIRET2,
        LWA_MSG_VX      TYPE GTY_MSG_VX,
        LWA_INF_PARAM01 TYPE ZSDSFIS040,
        LWA_TRANS       TYPE ZSDSFIS041.

  DATA: LV_TRANS_ITEM  TYPE ZSDSDE_TRANS_ITEM,
        LV_MESSAGE     TYPE STRING,
        LV_ERROR_TXT   TYPE C LENGTH 35,
        LV_SUCCESS_TXT TYPE C LENGTH 35,
        LV_ERROR       TYPE I,
        LV_SUCCESS     TYPE I,
        LV_SPLIT_AT    TYPE I VALUE 10,
        LV_EXE_TYPE    TYPE STRING,
        LV_LAST        TYPE FLAG,
        LV_LINES       TYPE I,
        LV_LOT         TYPE I,
        LV_TOT_LOT     TYPE I,
        LV_SAVE_PC     TYPE ABAP_BOOL.

  FIELD-SYMBOLS:
    <LFS_HEADCOUNT> LIKE LINE OF GT_HEAD,
    <LFS_HEAD>      LIKE LINE OF GT_HEAD,
    <LFS_TEXTFILE>  LIKE LINE OF LT_ETAX_TEXTFILE,
    <LFS_TAX_LOG>   LIKE LINE OF LT_LOG.

  PERFORM CHECK_SAVE_TO_PC CHANGING LV_SAVE_PC.

*  SORT gt_pdf_result BY wa_head-fyear
*                        wa_head-comp
*                        wa_head-sap_doc_no
*                        wa_head-doc_no
*                        wa_head-sap_doc_type
*                        wa_head-doc_type
*                        wa_head-ref_type.

* Initial transaction item No.
  LV_TRANS_ITEM = 1000000.
  LV_SUCCESS    = 0.
  LV_ERROR      = 0.

  PERFORM GET_EXE_TYPE      CHANGING LV_EXE_TYPE.
*  PERFORM separate_pdf_stat CHANGING lt_pdf_ok
*                                     lt_pdf_err.
  LV_SPLIT_AT = P_ZIPLIN.
  LT_HEADCOUNT[] = GT_HEAD[].

  PERFORM GET_TOT_LOT TABLES LT_HEADCOUNT
                    CHANGING LV_TOT_LOT.

  LOOP AT LT_HEADCOUNT ASSIGNING <LFS_HEADCOUNT>.
    AT LAST.
      LV_LAST = ABAP_TRUE.
    ENDAT.

    "---gt_head เก็บลง lt_head
    IF <LFS_HEADCOUNT>-SEL_CB EQ ABAP_ON AND
       <LFS_HEADCOUNT>-ICON   NE ICON_RED_LIGHT.
      APPEND <LFS_HEADCOUNT> TO LT_HEAD.
      ADD 1 TO LV_LINES.
    ENDIF.

    "---process transfer ทำเมื่อเป็น ณ เรคอร์ดที่ split หรือ ณ เรคอร์ดสุดท้าย และมี lt_head
    IF ( LV_LINES EQ LV_SPLIT_AT OR LV_LAST EQ ABAP_TRUE ) AND
       ( LV_LINES NE 0  ).

      LV_LINES      = 0.
      LV_TRANS_ITEM = 1000000.
      LV_LOT        = LV_LOT + 1.

      PERFORM MESSAGE_FOR_BATCH_LOT USING 'Start'
                                          LV_LOT
                                          LV_TOT_LOT.

      REFRESH: LT_ETAX_TEXTFILE,
               LT_LOG.
*               lt_head_fm_inf,
*               lt_trans_itm,
*               lt_log,
*               lt_bapiret2,
*               lt_log_result.

      "1.---PREPARE TEXTFILE จาก LT_HEAD
      LOOP AT LT_HEAD ASSIGNING <LFS_HEAD>.

        LWA_HEAD = <LFS_HEAD>.

        ADD 1 TO LV_TRANS_ITEM.

        APPEND INITIAL LINE TO LT_ETAX_TEXTFILE ASSIGNING <LFS_TEXTFILE>.
        <LFS_TEXTFILE>-TRANS_ITEM   = LV_TRANS_ITEM.

        PERFORM COLLECT_DOC_HEAD      USING LWA_HEAD
                                   CHANGING <LFS_TEXTFILE>.

        PERFORM COLLECT_DOC_H_REF     USING LWA_HEAD
                                   CHANGING <LFS_TEXTFILE>-DOC_H_REF[].

        PERFORM COLLECT_DOC_H_VAT     USING LWA_HEAD
                                   CHANGING <LFS_TEXTFILE>-DOC_H_VAT[].

        PERFORM COLLECT_DOC_DISCHG     USING LWA_HEAD
                                   CHANGING <LFS_TEXTFILE>-DOC_DISCHG[].

        PERFORM COLLECT_DOC_ITEM     USING LWA_HEAD
                                   CHANGING <LFS_TEXTFILE>-DOC_ITEM[].
*
        PERFORM COLLECT_DOC_PARTNER     USING LWA_HEAD
                                     CHANGING <LFS_TEXTFILE>-DOC_PARTNER[]
                                              <LFS_TEXTFILE>-KUNNR.

        PERFORM COLLECT_PDF USING LWA_HEAD
                         CHANGING <LFS_TEXTFILE>-ETAX_PDF[]
                                  <LFS_TEXTFILE>-X_PDF.

      ENDLOOP.

      "2.---TRANSFER ข้อมูล LOT นี้
      IF RB_TRN IS NOT INITIAL.

        IF LT_ETAX_TEXTFILE[] IS NOT INITIAL.
          CLEAR LWA_INF_PARAM01.
          LWA_INF_PARAM01-CREATE          = RB_TRN.
          LWA_INF_PARAM01-CANCEL          = RB_REJ.
*          lwa_inf_param01-sd              = space.
*          lwa_inf_param01-fi              = cb_fi.
          LWA_INF_PARAM01-EXE_TYPE        = LV_EXE_TYPE.
          LWA_INF_PARAM01-FULLPATH_SERVER = P_SERVER.

          "2.1. Transfer textfile, pdf to cloud
          CALL FUNCTION 'Z_SDSFI_PROCESS_TEXTFILE'
            EXPORTING
              IM_IT_TEXT     = LT_ETAX_TEXTFILE    "ข้อมูล text file
              IM_WA_INF      = LWA_INF_PARAM01
              IM_LOGD        = P_LOGD
              IM_PC_DIR      = P_PC_DIR
              IM_SAVE_PC     = LV_SAVE_PC
            IMPORTING
              EX_WA_TRANS    = LWA_TRANS
              EX_IT_TEXT     = LT_ETAX_TEXTFILE
              EX_IT_LOG      = LT_LOG
              EX_IT_BAPIRET2 = LT_BAPIRET2
            EXCEPTIONS
              ERROR          = 1
              OTHERS         = 2.


          APPEND LINES OF LT_BAPIRET2 TO LT_BAPIRET2_ALL.

          "2.2. Change result gt_head, Transfer data_h ==> used_h, move file to archive path
          PERFORM MESSAGE_FOR_BATCH USING 'Start: Archive File'.

          LOOP AT LT_ETAX_TEXTFILE INTO LWA_TEXT.

            PERFORM ARCHIVE_FILE USING LWA_TEXT-ARC_FULLPATH.

            "---Result แสดงที่ gt_head
            READ TABLE GT_HEAD ASSIGNING <LFS_HEAD>
            "Transfer DATA_H -> USED_H และ move file OUT -> Archived
              WITH KEY BUKRS       = LWA_TEXT-BUKRS
                       SAP_DOC_NO  = LWA_TEXT-SAP_DOC_NO
                       GJAHR       = LWA_TEXT-GJAHR
                       RD_DOC_TYPE = LWA_TEXT-RD_DOC_TYPE
                       MODULE_ETX  = LWA_TEXT-MODULE_ETX.
            IF SY-SUBRC IS INITIAL.

              IF LWA_TEXT-WA_MSG-MSGTY EQ 'S'.
                <LFS_HEAD>-ICON   = ICON_GREEN_LIGHT.
                <LFS_HEAD>-MESSG  = LWA_TEXT-WA_MSG-MESSAGE.
                <LFS_HEAD>-DONE   = ABAP_TRUE.
                ADD 1 TO LV_SUCCESS.
*                PERFORM transfer_data_h_to_used_h USING <lfs_head>.  "ZTETAX_DATA_H ==> ZTETAX_USED_H

*                PERFORM move_file_to_archive USING lwa_text-file_path    "call fn.'ZETAX_MOVE_FILE'
*                                                   <lfs_head>.
              ELSE.
                <LFS_HEAD>-ICON   = ICON_RED_LIGHT.
                <LFS_HEAD>-MESSG  = LWA_TEXT-WA_MSG-MESSAGE.
                <LFS_HEAD>-DONE   = ABAP_FALSE.
                ADD 1 TO LV_ERROR.
              ENDIF.
            ENDIF.

          ENDLOOP.

          PERFORM MESSAGE_FOR_BATCH USING 'End: Archive File'.

        ENDIF.

      ENDIF.

      REFRESH LT_HEAD.

      PERFORM MESSAGE_FOR_BATCH_LOT USING 'End'
                                          LV_LOT
                                          LV_TOT_LOT.

    ENDIF.

  ENDLOOP.

  LT_BAPIRET2[] = LT_BAPIRET2_ALL[].

*  IF p_memid IS NOT INITIAL.
*    EXIT.
*  ENDIF.

  IF SY-BATCH IS INITIAL.
    PERFORM REFRESH_ALV_GRID.
  ELSE.
    PERFORM DISPLAY_DATA USING ''.
  ENDIF.

*-----------------------------------------Alert Message & icon_light
  IF RB_TRN IS NOT INITIAL.

    READ TABLE LT_BAPIRET2 INTO LWA_BAPIRET2
      WITH KEY TYPE = 'E'.
    IF SY-SUBRC IS INITIAL.
      LWA_MSG_VX = LWA_BAPIRET2-MESSAGE.
      IF SY-BATCH IS INITIAL.
        MESSAGE E398(00) WITH LWA_MSG_VX-MESSAGE_V1
                              LWA_MSG_VX-MESSAGE_V2
                              LWA_MSG_VX-MESSAGE_V3
                              LWA_MSG_VX-MESSAGE_V4.
      ELSE.
        MESSAGE I398(00) WITH LWA_MSG_VX-MESSAGE_V1
                              LWA_MSG_VX-MESSAGE_V2
                              LWA_MSG_VX-MESSAGE_V3
                              LWA_MSG_VX-MESSAGE_V4.
      ENDIF.
    ELSE.
      READ TABLE LT_BAPIRET2 INTO LWA_BAPIRET2
        WITH KEY TYPE = 'W'.
      IF SY-SUBRC IS INITIAL.
        LWA_MSG_VX = LWA_BAPIRET2-MESSAGE.
        MESSAGE W398(00) WITH LWA_MSG_VX-MESSAGE_V1
                              LWA_MSG_VX-MESSAGE_V2
                              LWA_MSG_VX-MESSAGE_V3
                              LWA_MSG_VX-MESSAGE_V4.
        EXIT.
      ENDIF.
    ENDIF.

    IF SY-BATCH IS NOT INITIAL.
      LV_SUCCESS = 0.
      LV_ERROR   = 0.
      LOOP AT GT_HEAD ASSIGNING <LFS_HEAD>.
        CASE <LFS_HEAD>-ICON.
          WHEN ICON_GREEN_LIGHT.
            ADD 1 TO LV_SUCCESS.
          WHEN OTHERS.
            ADD 1 TO LV_ERROR.
        ENDCASE.
      ENDLOOP.  "gt_head
    ENDIF.

    WRITE: LV_SUCCESS TO LV_SUCCESS_TXT,
           LV_ERROR   TO LV_ERROR_TXT.

    CONDENSE: LV_SUCCESS_TXT NO-GAPS,
              LV_ERROR_TXT   NO-GAPS.

    LV_MESSAGE = 'Completed: &1 Error: &2 record(s)'.
    REPLACE ALL OCCURRENCES OF:
      '&1' IN LV_MESSAGE WITH LV_SUCCESS_TXT,
      '&2' IN LV_MESSAGE WITH LV_ERROR_TXT.

    MESSAGE S398(00) WITH LV_MESSAGE.

  ENDIF.

ENDFORM.                    "send_data
*&---------------------------------------------------------------------*
*& Form GET_EXE_TYPE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_EXE_TYPE
*&---------------------------------------------------------------------*
FORM GET_EXE_TYPE  CHANGING CH_EXE_TYPE.

  IF SY-BATCH IS INITIAL.
    CH_EXE_TYPE = 'O'.
  ELSE.
    CH_EXE_TYPE = 'B'.
  ENDIF.
ENDFORM.                    "get_exe_type
*&---------------------------------------------------------------------*
*& Form GET_TOT_LOT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_HEADCOUNT
*&      <-- LV_TOT_LOT
*&---------------------------------------------------------------------*
FORM GET_TOT_LOT  TABLES LT_HEADCOUNT TYPE GTTY_HEAD
                CHANGING CH_TOT_LOT   TYPE I.

  DATA: LV_LAST     TYPE FLAG,
        LV_LINES    TYPE I,
        LV_SPLIT_AT TYPE I VALUE 10.

  FIELD-SYMBOLS: <LFS_HEADCOUNT> LIKE LINE OF LT_HEADCOUNT.

  LV_SPLIT_AT = P_ZIPLIN.

  LOOP AT LT_HEADCOUNT ASSIGNING <LFS_HEADCOUNT>.
    AT LAST.
      LV_LAST = ABAP_TRUE.
    ENDAT.

    "---gt_head เก็บลง lt_head
    IF <LFS_HEADCOUNT>-SEL_CB EQ ABAP_ON. "AND
*       <lfs_headcount>-icon   NE icon_red_light.
      ADD 1 TO LV_LINES.
    ENDIF.

    "---process transfer ทำเมื่อเป็น ณ เรคอร์ดที่ split หรือ ณ เรคอร์ดสุดท้าย และมี lt_head
    IF ( LV_LINES EQ LV_SPLIT_AT OR LV_LAST EQ ABAP_TRUE ) AND
       ( LV_LINES NE 0  ).
      LV_LINES   = 0.
      CH_TOT_LOT = CH_TOT_LOT + 1.
    ENDIF.

  ENDLOOP.

ENDFORM.                    "get_tot_lot

*&---------------------------------------------------------------------*
*& Form COLLECT_DOC_HEAD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEAD
*&      <-- <LFS_TEXTFILE>
*&---------------------------------------------------------------------*
FORM COLLECT_DOC_HEAD  USING    PWA_HEAD    TYPE GTY_HEAD
                       CHANGING CH_TEXTFILE TYPE ZSDSFIS033.

  MOVE-CORRESPONDING PWA_HEAD TO CH_TEXTFILE.

  CH_TEXTFILE-SAP_DOC_RESN_DESC = PWA_HEAD-SAP_DOC_RESN_DES.

  IF PWA_HEAD-MAIL_CB EQ 'X'.
    CH_TEXTFILE-SEND_MAIL_REQ = 'Y'.
  ELSE.
    CH_TEXTFILE-SEND_MAIL_REQ = 'N'.
  ENDIF.

  CH_TEXTFILE-EMAIL         = PWA_HEAD-E_MAIL.

ENDFORM.                    "collect_doc_head
*&---------------------------------------------------------------------*
*& Form COLLECT_DOC_H_REF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEAD
*&      <-- <LFS_TEXTFILE>_DOC_H_REF[]
*&---------------------------------------------------------------------*
FORM COLLECT_DOC_H_REF  USING    PWA_HEAD TYPE GTY_HEAD
                        CHANGING CHT_H_REF TYPE ZSDSFIS035_TT.

  DATA : LWA_H_REF       TYPE GTY_DOC_H_REF.

  FIELD-SYMBOLS : <LFS_H_REF> LIKE LINE OF CHT_H_REF.

  REFRESH CHT_H_REF.

  LOOP AT PWA_HEAD-IT_H_REF[] INTO LWA_H_REF.
    APPEND INITIAL LINE TO CHT_H_REF ASSIGNING <LFS_H_REF>.
    MOVE-CORRESPONDING LWA_H_REF TO <LFS_H_REF>.

    <LFS_H_REF>-REF_SAP_POST_DATE = LWA_H_REF-REF_SAP_POST_DAT.
  ENDLOOP.

ENDFORM.                    "collect_doc_h_ref
*&---------------------------------------------------------------------*
*& Form COLLECT_DOC_H_VAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEAD
*&      <-- <LFS_TEXTFILE>_DOC_H_VAT[]
*&---------------------------------------------------------------------*
FORM COLLECT_DOC_H_VAT  USING    PWA_HEAD  TYPE GTY_HEAD
                        CHANGING CHT_H_VAT TYPE ZSDSFIS036_TT.

  DATA : LWA_H_VAT       TYPE GTY_DOC_H_VAT.

  FIELD-SYMBOLS : <LFS_H_VAT> LIKE LINE OF CHT_H_VAT.

  REFRESH CHT_H_VAT.

  LOOP AT PWA_HEAD-IT_H_VAT[] INTO LWA_H_VAT.
    APPEND INITIAL LINE TO CHT_H_VAT ASSIGNING <LFS_H_VAT>.
    MOVE-CORRESPONDING LWA_H_VAT TO <LFS_H_VAT>.
  ENDLOOP.

ENDFORM.                    "collect_doc_h_vat
*&---------------------------------------------------------------------*
*& Form COLLECT_DOC_DISCHG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEAD
*&      <-- <LFS_TEXTFILE>_DOC_DISCHG[]
*&---------------------------------------------------------------------*
FORM COLLECT_DOC_DISCHG  USING    PWA_HEAD   TYPE GTY_HEAD
                         CHANGING CHT_DISCHG TYPE ZSDSFIS037_TT.

  DATA : LWA_CHARGE       TYPE GTY_DOC_DISCHG.

  FIELD-SYMBOLS : <LFS_DISCHG> LIKE LINE OF CHT_DISCHG.

  REFRESH CHT_DISCHG.

  LOOP AT PWA_HEAD-IT_CHARGE[] INTO LWA_CHARGE.
    APPEND INITIAL LINE TO CHT_DISCHG ASSIGNING <LFS_DISCHG>.
    MOVE-CORRESPONDING LWA_CHARGE TO <LFS_DISCHG>.
  ENDLOOP.

ENDFORM.                    "collect_doc_dischg
*&---------------------------------------------------------------------*
*& Form COLLECT_DOC_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEAD
*&      <-- <LFS_TEXTFILE>_DOC_ITEM[]
*&---------------------------------------------------------------------*
FORM COLLECT_DOC_ITEM  USING    PWA_HEAD   TYPE GTY_HEAD
                       CHANGING CHT_ITEM   TYPE ZSDSFIS038_TT.

  DATA : LWA_ITEM       TYPE GTY_DOC_ITEM.

  FIELD-SYMBOLS : <LFS_ITEM> LIKE LINE OF CHT_ITEM.

  REFRESH CHT_ITEM.

  LOOP AT PWA_HEAD-IT_ITEM[] INTO LWA_ITEM.
    APPEND INITIAL LINE TO CHT_ITEM ASSIGNING <LFS_ITEM>.
    MOVE-CORRESPONDING LWA_ITEM TO <LFS_ITEM>.
  ENDLOOP.

ENDFORM.                    "collect_doc_item
*&---------------------------------------------------------------------*
*& Form COLLECT_DOC_PARTNER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEAD
*&      <-- <LFS_TEXTFILE>_DOC_PARTNER[]
*&---------------------------------------------------------------------*
FORM COLLECT_DOC_PARTNER  USING    PWA_HEAD    TYPE GTY_HEAD
                          CHANGING CHT_PARTNER TYPE ZSDSFIS039_TT
                                   CH_KUNNR    TYPE KUNNR.

  DATA : LWA_PARTNER       TYPE GTY_DOC_PARTNER.

  FIELD-SYMBOLS : <LFS_PARTNER> LIKE LINE OF CHT_PARTNER.

  REFRESH CHT_PARTNER.

  LOOP AT PWA_HEAD-IT_PARTNER[] INTO LWA_PARTNER.
    APPEND INITIAL LINE TO CHT_PARTNER ASSIGNING <LFS_PARTNER>.
    MOVE-CORRESPONDING LWA_PARTNER TO <LFS_PARTNER>.
*    IF lwa_partner-rd_partner EQ 'SELL'.
*      ch_kunnr = lwa_partner-kunnr.
*    ENDIF.
  ENDLOOP.

ENDFORM.                    "collect_doc_partner
*&---------------------------------------------------------------------*
*& Form COLLECT_PDF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEAD
*&      <-- <LFS_TEXTFILE>_ETAX_PDF[]
*&      <-- <LFS_TEXTFILE>_X_PDF
*&---------------------------------------------------------------------*
FORM COLLECT_PDF  USING    PI_WA_HEAD TYPE GTY_HEAD
                  CHANGING CHT_PDF    TYPE ZSDSFIS025_TT
                           CH_X_PDF   TYPE FLAG.

  CHT_PDF[] = PI_WA_HEAD-IT_PDF[].

  IF PI_WA_HEAD-REVERSE_FLAG = 'Y'.
    CH_X_PDF  = ''.  "กรณไม่ส่งกรม ไม่สร้าง PDF
  ELSE.
    CH_X_PDF  = PI_WA_HEAD-X_PDF.
  ENDIF.

ENDFORM.                    "collect_pdf
*&---------------------------------------------------------------------*
*& Form MESSAGE_FOR_BATCH_LOT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> LV_LOT
*&      --> LV_TOT_LOT
*&---------------------------------------------------------------------*
FORM MESSAGE_FOR_BATCH_LOT USING PI_TEXT1
                                 PI_LOT
                                 PI_TOT_LOT.

  DATA: LV_BATCH_MESSAGE TYPE STRING,
        LV_C_LOT         TYPE STRING,
        LV_C_TOT_LOT     TYPE STRING.

  IF SY-BATCH IS NOT INITIAL.

    LV_BATCH_MESSAGE = '============= $1: Lot $2/$3 ============='.
    LV_C_LOT         = PI_LOT.
    LV_C_TOT_LOT     = PI_TOT_LOT.

    CONDENSE: LV_C_LOT     NO-GAPS,
              LV_C_TOT_LOT NO-GAPS.

    REPLACE '$1' IN LV_BATCH_MESSAGE WITH PI_TEXT1.
    REPLACE '$2' IN LV_BATCH_MESSAGE WITH LV_C_LOT.
    REPLACE '$3' IN LV_BATCH_MESSAGE WITH LV_C_TOT_LOT.

    PERFORM MESSAGE_FOR_BATCH USING LV_BATCH_MESSAGE.

  ENDIF.


ENDFORM.                    "message_for_batch_lot
*&---------------------------------------------------------------------*
*& Form REFRESH_ALV_GRID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM REFRESH_ALV_GRID .

  DATA: OB_GRID    TYPE REF TO CL_GUI_ALV_GRID .

* Get grid reference
  IF OB_GRID IS INITIAL .
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = OB_GRID.
  ENDIF.

  IF OB_GRID IS NOT INITIAL. "Sanity Test
    CALL METHOD OB_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDFORM.                    "refresh_alv_grid
*&---------------------------------------------------------------------*
*& Form REJECT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM REJECT_DATA .

  DATA: LT_LOG     TYPE TABLE OF ZSDSFIT019.

  DATA: LWA_TRANS  TYPE ZSDSFIS041,
        LWA_HEADER LIKE LINE OF LWA_TRANS-IT_HEADER,
        LWA_LOG    LIKE LINE OF LT_LOG.

  DATA: LV_INIT_TRANS_NO   TYPE ZSDSFIT019-TRANS_NO,
        LV_INIT_TRANS_ITEM TYPE ZSDSFIT019-TRANS_ITEM,
        LV_DATE            TYPE SY-DATUM,
        LV_TIME            TYPE SY-UZEIT,
        LV_ICON            TYPE ICON-ID,
        LV_MESSG           TYPE STRING.

  FIELD-SYMBOLS: <LFS_HEAD> LIKE LINE OF GT_HEAD,
                 <LFS_LOG>  LIKE LINE OF LT_LOG.

  GET TIME.
  LV_DATE = SY-DATUM.
  LV_TIME = SY-UZEIT.

  LOOP AT GT_HEAD ASSIGNING <LFS_HEAD>
    WHERE SEL_CB = ABAP_ON.

    CLEAR LWA_HEADER.
    MOVE-CORRESPONDING <LFS_HEAD> TO LWA_HEADER.
    LWA_HEADER-MANDT        = SY-MANDT.
    LWA_HEADER-BUKRS        = <LFS_HEAD>-BUKRS.
    LWA_HEADER-SAP_DOC_NO   = <LFS_HEAD>-SAP_DOC_NO.
    LWA_HEADER-GJAHR        = <LFS_HEAD>-GJAHR.
    LWA_HEADER-RD_DOC_TYPE  = <LFS_HEAD>-RD_DOC_TYPE.
    LWA_HEADER-MODULE_ETX   = <LFS_HEAD>-MODULE_ETX.
    LWA_HEADER-ETAX_DATE    = LV_DATE.
    LWA_HEADER-ETAX_TIME    = LV_TIME.
    LWA_HEADER-ETAX_BY      = SY-UNAME.
    LWA_HEADER-STATUS       = '3'.        "Rejected Complete
*    Update ZSDSFIT014
    APPEND LWA_HEADER TO LWA_TRANS-IT_HEADER.

    CLEAR LWA_LOG.
    LWA_LOG-MANDT          = SY-MANDT.
    LWA_LOG-BUKRS          = <LFS_HEAD>-BUKRS.
    LWA_LOG-SAP_DOC_NO     = <LFS_HEAD>-SAP_DOC_NO.
    LWA_LOG-GJAHR          = <LFS_HEAD>-GJAHR.
    LWA_LOG-RD_DOC_TYPE    = <LFS_HEAD>-RD_DOC_TYPE.
    LWA_LOG-MODULE_ETX     = <LFS_HEAD>-MODULE_ETX.
    LWA_LOG-S_FLAG         = ABAP_OFF.
    LWA_LOG-R_FLAG         = ABAP_ON.
    LWA_LOG-XML_COMPLETED  = ABAP_OFF.
    LWA_LOG-PDF_COMPLETED  = ABAP_OFF.
    LWA_LOG-TRANS_NO       = LV_INIT_TRANS_NO.
    LWA_LOG-TRANS_ITEM     = LV_INIT_TRANS_ITEM.
    LWA_LOG-UPDATED_DATE   = LV_DATE.
    LWA_LOG-UPDATED_TIME   = LV_TIME.
    LWA_LOG-UPDATED_BY     = SY-UNAME.
    APPEND LWA_LOG TO LT_LOG.

  ENDLOOP.

*   Update log (ZTable)
  MODIFY ZSDSFIT014  FROM TABLE LWA_TRANS-IT_HEADER[].
  IF SY-SUBRC EQ 0.
    LV_ICON   = ICON_GREEN_LIGHT.
    LV_MESSG  = TEXT-E34.
    COMMIT WORK AND WAIT.
    MESSAGE S398(00) WITH TEXT-E34.
  ELSE.
    LV_ICON   = ICON_GREEN_LIGHT.
    LV_MESSG  = TEXT-E35.
    ROLLBACK WORK.
    MESSAGE S398(00) WITH TEXT-E35 DISPLAY LIKE 'E'.
  ENDIF.

  "Modify for Refresh ALV
  LOOP AT GT_HEAD ASSIGNING <LFS_HEAD>
    WHERE SEL_CB = ABAP_ON.
    <LFS_HEAD>-ICON   = LV_ICON.
    <LFS_HEAD>-MESSG  = LV_MESSG.
  ENDLOOP.

  LOOP AT LT_LOG ASSIGNING <LFS_LOG>.
    <LFS_HEAD>-ICON   = LV_ICON.
    <LFS_HEAD>-MESSG  = LV_MESSG.
  ENDLOOP.
*   Update log (ZTable)
  MODIFY ZSDSFIT019 FROM TABLE LT_LOG[].
  IF SY-SUBRC EQ 0.
    COMMIT WORK AND WAIT.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  IF SY-BATCH IS INITIAL.
    PERFORM REFRESH_ALV_GRID.
  ELSE.
    PERFORM DISPLAY_DATA USING ''.
  ENDIF.

ENDFORM.                    "reject_data
*&---------------------------------------------------------------------*
*& Form CHECK_SAVE_TO_PC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_SAVE_PC
*&---------------------------------------------------------------------*
FORM CHECK_SAVE_TO_PC  CHANGING CH_SAVE_PC.

  DATA: LV_NAME      TYPE ZSDSFIC015-NAME VALUE 'SAVE_FILES_TO_PC',
        LV_PARAM_EXT TYPE ZSDSFIC015-PARAM_EXT,
        LV_SEQUENCE  TYPE ZSDSFIC015-SEQUENCE VALUE '1',
        LV_VALUE     TYPE ZSDSFIC015-LOW_VALUE.

  CLEAR CH_SAVE_PC.
  PERFORM READ_PARAM    USING LV_NAME
                              LV_PARAM_EXT
                              LV_SEQUENCE
                     CHANGING LV_VALUE.

  CH_SAVE_PC = LV_VALUE.

ENDFORM.                    "check_save_to_pc
*&---------------------------------------------------------------------*
*& Form SET_VARIANT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LWA_VARIANT
*&---------------------------------------------------------------------*
FORM SET_VARIANT  CHANGING PWA_VARIANT TYPE DISVARIANT.

  IF P_VARI IS NOT INITIAL.
    PWA_VARIANT-REPORT  = SY-REPID.
    PWA_VARIANT-VARIANT = P_VARI.
  ENDIF.

ENDFORM.                    "set_variant
*&---------------------------------------------------------------------*
*& Form ARCHIVE_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_TEXT_ARC_FULLPATH
*&---------------------------------------------------------------------*
FORM ARCHIVE_FILE  USING PI_SOURCE.

  DATA: LV_SOURCE TYPE TEXT255,
        LV_TARGET TYPE TEXT255.

  CHECK PI_SOURCE IS NOT INITIAL.

  PERFORM GET_FILE_PATH CHANGING GWA_FILE_PATH.

  LV_SOURCE = PI_SOURCE.
  LV_TARGET = GWA_FILE_PATH-SAP_DIR_ARCH.

  PERFORM MOVE_FILE USING LV_SOURCE
                          LV_TARGET.

ENDFORM.                    "archive_file
*&---------------------------------------------------------------------*
*& Form MOVE_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_SOURCE
*&      --> LV_TARGET
*&---------------------------------------------------------------------*
FORM MOVE_FILE  USING    PI_SOURCE
                         PI_TARGET.

  DATA: LV_SUBRC   TYPE SY-SUBRC,
        LV_COMMAND TYPE STRING,
        LWA_RETURN TYPE BAPIRET2.

  CALL FUNCTION 'Z_SDSFI_MOVE_FILE'
    EXPORTING
      IM_SOURCE  = PI_SOURCE
      IM_TARGET  = PI_TARGET
    IMPORTING
      EX_SUBRC   = LV_SUBRC
      EX_COMMAND = LV_COMMAND
      EX_RETURN  = LWA_RETURN
    EXCEPTIONS
      ERROR      = 1
      OTHERS     = 2.

ENDFORM.                    "move_file

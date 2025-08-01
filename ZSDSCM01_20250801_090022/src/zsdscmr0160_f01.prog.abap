*-----------------------------------------------------------------------
*  Program ID         : ZSDSCMR0160_F01
*  Creation Date      : 23.05.2025
*  Author             : Boonpipop Ch.
*  Add-on ID          : N/A
*  Description        : This is include program routine
*  Purpose            : Include program logic
*  Copied from        : ZSDSFIR0480_F01
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Form F_AUTHORIZE_CHECK
*----------------------------------------------------------------------*
*  Check Authorization on t-code
*----------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK USING UF_TCODE  TYPE  SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
    ID 'TCD' FIELD UF_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH UF_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_set_selscr_default
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_SET_SELSCR_DEFAULT .
  DATA: LS_COMPDT LIKE LINE OF S_COMPDT.
  DATA: LF_DAY_IN            TYPE SY-DATUM,
        LF_LAST_DAY_OF_MONTH TYPE DATUM.

*  P_BUKRS = GC_CON-BUKRS.
*  P_BLDAT = SY-DATUM.
*  P_BUDAT = SY-DATUM.
*  P_BLART = GF_DOC_TYPE.
*  P_BUPLA = GC_CON-BUPLA.
*  P_SGTXT = TEXT-005. "Post Service to RAR

  LF_DAY_IN = |{ SY-DATUM(6) }| & |01|.

*  LAST DATE OF MONTH
  PERFORM F_GET_LAST_DAY_OF_MONTHS USING LF_DAY_IN
                                   CHANGING LF_LAST_DAY_OF_MONTH.
*  IF LF_LAST_DAY_OF_MONTH IS NOT INITIAL.
*    P_BUDT_W = LF_LAST_DAY_OF_MONTH.
*    P_BLDT_W = LF_LAST_DAY_OF_MONTH.
*    P_BUDT_D = LF_LAST_DAY_OF_MONTH. "For Display Report

*    GF_LAST_DAY_OF_MONTH  = LF_LAST_DAY_OF_MONTH.
*  ENDIF.

  CLEAR LS_COMPDT.
  LS_COMPDT-SIGN   = 'I'.
  LS_COMPDT-OPTION = 'BT'.
  LS_COMPDT-LOW    = LF_DAY_IN.
  LS_COMPDT-HIGH   = LF_LAST_DAY_OF_MONTH.
  APPEND LS_COMPDT TO S_COMPDT.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_DISPLAY_RESULT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_OUTPUT
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UT_OUTPUT TYPE TT_OUTPUT        ##NEEDED.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.


* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = SPACE.
* Enable Soft Refresh only in display mode
  GF_SOFT_REFRESH_1 = GC_CON-TRUE.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.

  ASSIGN UT_OUTPUT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT CHANGING GT_FIELDCAT_1.

* Exclude toolbar functions
  PERFORM F_ALV_EXCL_TOOLBAR CHANGING GT_TOOL_EXC_1.
** Sort data
*  PERFORM F_ALV_SORT_RESULT CHANGING GT_SORT_1.

* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                           CS_VARIANT TYPE  DISVARIANT
                           CS_PRINT   TYPE  LVC_S_PRNT          ##CALLED.

* Initialize Output
  CLEAR:  CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-CWIDTH_OPT = GC_CON-TRUE.
  CS_LAYOUT-ZEBRA      = GC_CON-TRUE.
  CS_LAYOUT-SEL_MODE   = ''.
  CS_LAYOUT-BOX_FNAME  = ''.
  CS_LAYOUT-NO_ROWMARK = GC_CON-TRUE.
  CS_LAYOUT-STYLEFNAME = 'FIELD_STYLE'.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.

  CS_PRINT-NO_COLWOPT = GC_CON-TRUE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.

* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'SEL'.
        <L_FIELDCAT>-CHECKBOX   = GC_CON-TRUE.
        <L_FIELDCAT>-EDIT       = GC_CON-TRUE.
        <L_FIELDCAT>-COLTEXT    = TEXT-CO1.
        <L_FIELDCAT>-SELTEXT    = TEXT-CO1.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-CO1.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-CO1.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-CO1.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'STATUS_ICON'.
        <L_FIELDCAT>-COLTEXT    = TEXT-CO2.
        <L_FIELDCAT>-SELTEXT    = TEXT-CO2.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-CO2.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-CO2.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-CO2.
        <L_FIELDCAT>-ICON       = GC_CON-TRUE.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'MSGTY'.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'MSGID'.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'MSGNO'.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'MESSAGE'.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'SERV_CON_ID'.
        <L_FIELDCAT>-COLTEXT    = TEXT-CO3.
        <L_FIELDCAT>-SELTEXT    = TEXT-CO3.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-CO3.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-CO3.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-CO3.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'SVC_ITEM'.
        <L_FIELDCAT>-COLTEXT    = TEXT-C12.
        <L_FIELDCAT>-SELTEXT    = TEXT-C12.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-C12.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-C12.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-C12.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'SVC_STARTDATE'.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'SVC_ENDDATE'.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'SERV_CON_VAL'.
        <L_FIELDCAT>-COLTEXT    = TEXT-CO4.
        <L_FIELDCAT>-SELTEXT    = TEXT-CO4.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-CO4.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-CO4.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-CO4.
      WHEN 'SERV_CON_STA'.
        <L_FIELDCAT>-COLTEXT    = TEXT-CO5.
        <L_FIELDCAT>-SELTEXT    = TEXT-CO5.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-CO5.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-CO5.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-CO5.
      WHEN 'AMTDAY'.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'POSDAYS'.
        <L_FIELDCAT>-COLTEXT    = TEXT-C14.
        <L_FIELDCAT>-SELTEXT    = TEXT-C14.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-C14.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-C14.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-C14.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'TOTDAYS'.
        <L_FIELDCAT>-COLTEXT    = TEXT-C15.
        <L_FIELDCAT>-SELTEXT    = TEXT-C15.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-C15.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-C15.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-C15.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'SVO_ID'.
        <L_FIELDCAT>-COLTEXT    = TEXT-CO6.
        <L_FIELDCAT>-SELTEXT    = TEXT-CO6.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-CO6.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-CO6.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-CO6.
      WHEN 'SVO_STARTDATE'.
      WHEN 'SVO_ENDDATE'.
      WHEN 'SVO_STATUS'.
      WHEN 'SVO_ITEM'.
        <L_FIELDCAT>-COLTEXT    = TEXT-CO7.
        <L_FIELDCAT>-SELTEXT    = TEXT-CO7.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-CO7.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-CO7.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-CO7.
      WHEN 'SEQNO'.
*        IF RB_POST EQ GC_CON-TRUE.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
*        ENDIF.
      WHEN 'RUNNO'.
        <L_FIELDCAT>-COLTEXT    = TEXT-C13.
        <L_FIELDCAT>-SELTEXT    = TEXT-C13.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-C13.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-C13.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-C13.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'EXT_POSID'.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'EXT_WRT_BEG'.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'EXT_WRT_END'.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'VKORG'.
        <L_FIELDCAT>-COLTEXT    = TEXT-CO8.
        <L_FIELDCAT>-SELTEXT    = TEXT-CO8.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-CO8.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-CO8.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-CO8.
      WHEN 'VKBUR'.
        <L_FIELDCAT>-COLTEXT    = TEXT-CO9.
        <L_FIELDCAT>-SELTEXT    = TEXT-CO9.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-CO9.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-CO9.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-CO9.
      WHEN 'VKGRP'.
        <L_FIELDCAT>-COLTEXT    = TEXT-C10.
        <L_FIELDCAT>-SELTEXT    = TEXT-C10.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-C10.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-C10.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-C10.
      WHEN 'MATNR'.
        <L_FIELDCAT>-COLTEXT    = TEXT-C11.
        <L_FIELDCAT>-SELTEXT    = TEXT-C11.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-C11.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-C11.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-C11.
      WHEN 'NET_VALUE'.
        <L_FIELDCAT>-COLTEXT    = TEXT-C17.
        <L_FIELDCAT>-SELTEXT    = TEXT-C17.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-C17.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-C17.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-C17.
      WHEN 'NETWR'.
        <L_FIELDCAT>-COLTEXT    = TEXT-C18.
        <L_FIELDCAT>-SELTEXT    = TEXT-C18.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-C18.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-C18.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-C18.
      WHEN 'SVO_ORDER'.
      WHEN 'SVO_PSPEL'.
      WHEN 'VBELN_VF'.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'VBELN_VL'.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'POSNR_VF'.
        <L_FIELDCAT>-COLTEXT    = TEXT-C16.
        <L_FIELDCAT>-SELTEXT    = TEXT-C16.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-C16.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-C16.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-C16.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'VBELN_VF_TXT'.
        <L_FIELDCAT>-COLTEXT    = TEXT-C20.
        <L_FIELDCAT>-SELTEXT    = TEXT-C20.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-C20.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-C20.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-C20.
      WHEN 'NETWR_VF'.
        <L_FIELDCAT>-COLTEXT    = TEXT-C21.
        <L_FIELDCAT>-SELTEXT    = TEXT-C21.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-C21.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-C21.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-C21.
      WHEN 'BALANCE'.
        <L_FIELDCAT>-COLTEXT    = TEXT-C22.
        <L_FIELDCAT>-SELTEXT    = TEXT-C22.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-C22.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-C22.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-C22.
      WHEN 'AUFNR'.  "Contract IO
        <L_FIELDCAT>-COLTEXT    = TEXT-C19.
        <L_FIELDCAT>-SELTEXT    = TEXT-C19.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-C19.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-C19.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-C19.
      WHEN 'POSID'. "WBS MA
        <L_FIELDCAT>-COLTEXT    = TEXT-C25.
        <L_FIELDCAT>-SELTEXT    = TEXT-C25.
        <L_FIELDCAT>-SCRTEXT_S  = TEXT-C25.
        <L_FIELDCAT>-SCRTEXT_M  = TEXT-C25.
        <L_FIELDCAT>-SCRTEXT_L  = TEXT-C25.
      WHEN 'BUPLA'.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'KOSTL'.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'PRCTR'.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
      WHEN 'XTRUEREV'.
        <L_FIELDCAT>-NO_OUT     = GC_CON-TRUE.
*      WHEN 'BUKRS'.
*      WHEN 'GJAHR'.

    ENDCASE.

  ENDLOOP.

ENDFORM.

**&---------------------------------------------------------------------*
**& Form f_post_document_soma
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> GT_OUTPUT
**&      --> GC_MODE_TEST_RUN
**&---------------------------------------------------------------------*
*FORM F_POST_DOCUMENT_SOMA USING    UF_MODE   TYPE CHAR1
*                          CHANGING CT_DATA_H TYPE TT_DATA_H
*                                   CT_DATA   TYPE TT_DATA
*                                   CT_OUTPUT TYPE TT_OUTPUT.
*  DATA:
*    LT_DATA_UPD TYPE TT_DATA,
*    LS_OUTPUT   TYPE ZSDSFIS139,
*    LS_MESSG    TYPE  TS_MESSG,
*    LS_FIDOC    TYPE  TS_FIDOC,
*    LS_STYLEROW TYPE LVC_S_STYL.
*
*  DATA:
*    LF_ERROR  TYPE  FLAG.
*
*  DELETE CT_DATA  WHERE BELNR IS NOT INITIAL AND XTRUEREV IS INITIAL.
*  DELETE CT_DATA  WHERE SVO_STATUS NE GF_STATUS_COMPLETE."C
*
*  SORT: CT_DATA_H BY SERV_CON_ID SVO_ID ,
*        CT_DATA BY SERV_CON_ID SVO_ID SVO_ITEM.
*
*  CLEAR: LT_DATA_UPD[].
*
*  LOOP AT CT_DATA_H INTO DATA(LS_DATA_H)
*                    WHERE BELNR IS INITIAL.
*
*    PERFORM F_POST_FI_DOC_SOMA  USING  LS_DATA_H
*                                       CT_DATA
*                                       UF_MODE
*                              CHANGING LS_FIDOC
*                                       LS_MESSG
*                                       LF_ERROR.
*    "Update Return Message
*    LOOP AT CT_DATA INTO DATA(LS_DATA)
*                    WHERE SERV_CON_ID = LS_DATA_H-SERV_CON_ID
*                      AND SVO_ID = LS_DATA_H-SVO_ID.
*      MOVE-CORRESPONDING LS_MESSG TO LS_DATA.
*      IF UF_MODE IS NOT INITIAL.
*        "Test Run
*        LS_DATA-STATUS_ICON = GC_STATUS-WARNING.
*        LS_DATA-MESSAGE     = LS_MESSG-MSGTX.
*      ELSE.
*        IF LF_ERROR IS INITIAL.
*          LS_DATA-STATUS_ICON = GC_STATUS-SUCCESS.
*          LS_DATA-MESSAGE     = LS_MESSG-MSGTX.
*          LS_DATA-BUKRS       = LS_FIDOC-BUKRS.
*          LS_DATA-BELNR       = LS_FIDOC-BELNR.
*          LS_DATA-GJAHR       = LS_FIDOC-GJAHR.
*          LS_DATA-BUDAT       = GF_BUDAT.
*
*          "Update FI doc in Header
*          LS_DATA_H-BELNR     = LS_FIDOC-BELNR.
*          MODIFY CT_DATA_H FROM LS_DATA_H.
*        ELSE.
*          LS_DATA-STATUS_ICON = GC_STATUS-ERROR.
*          LS_DATA-MESSAGE     = LS_MESSG-MSGTX.
*        ENDIF.
*
*        LS_DATA-USNAM = SY-UNAME.
*        LS_DATA-ERDAT = SY-DATUM.
*        LS_DATA-ERZMT = SY-UZEIT.
*
*        "For Update Ztable
*        APPEND LS_DATA TO LT_DATA_UPD.
*      ENDIF.
*
*      MODIFY CT_DATA FROM LS_DATA.
*    ENDLOOP.
*
*  ENDLOOP. "CT_DATA_H
*
*  "Production run
*  IF UF_MODE IS INITIAL AND LT_DATA_UPD[] IS NOT INITIAL.
*    PERFORM F_UPDATE_LOGS USING LT_DATA_UPD.
*  ENDIF.
*
*  SORT CT_DATA BY SERV_CON_ID SVO_ID SVO_ITEM.
*  IF CT_OUTPUT[] IS INITIAL.
*    CT_OUTPUT[] = CT_DATA[].
*  ELSE.
*    LOOP AT CT_OUTPUT INTO LS_OUTPUT.
*
*      READ TABLE CT_DATA INTO LS_DATA
*                         WITH KEY SERV_CON_ID = LS_OUTPUT-SERV_CON_ID
*                                  SVO_ID = LS_OUTPUT-SVO_ID
*                                  SVO_ITEM = LS_OUTPUT-SVO_ITEM
*                          BINARY SEARCH.
*      IF SY-SUBRC EQ 0.
*        MOVE-CORRESPONDING LS_DATA TO LS_OUTPUT.
*        MODIFY CT_OUTPUT FROM LS_OUTPUT.
*      ENDIF.
*
*    ENDLOOP.
*  ENDIF.
*
*  LOOP AT CT_OUTPUT ASSIGNING FIELD-SYMBOL(<LS_OUTPUT>).
*    IF <LS_OUTPUT>-RUNNO NE GC_CON-FIRST_ITEM_NO OR
*       <LS_OUTPUT>-MSGTY = GC_MSGTY-ERR OR
*       <LS_OUTPUT>-BELNR IS NOT INITIAL.
*      LS_STYLEROW-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*      LS_STYLEROW-FIELDNAME = 'SEL'.
*      INSERT LS_STYLEROW  INTO TABLE <LS_OUTPUT>-FIELD_STYLE.
*    ENDIF.
*  ENDLOOP.
*
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_post_document_sosla
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> GT_OUTPUT
**&      --> GC_MODE_TEST_RUN
**&---------------------------------------------------------------------*
*FORM F_POST_DOCUMENT_SOSLA USING    UF_MODE  TYPE CHAR1
*                                    UT_DATA_H TYPE TT_DATA_H
*                           CHANGING CT_DATA   TYPE TT_DATA
*                                   CT_OUTPUT TYPE TT_OUTPUT.
*  DATA:
*    LT_DATA_TMP TYPE TT_DATA,
*    LS_MESSG    TYPE  TS_MESSG,
*    LS_FIDOC    TYPE  TS_FIDOC.
*
*  DATA:
*    LF_ERROR  TYPE  FLAG.
*
*  DELETE CT_DATA  WHERE SVO_STATUS NE GF_STATUS_COMPLETE.
*
*  SORT: CT_DATA BY SERV_CON_ID SVC_ITEM.
*
*  LOOP AT UT_DATA_H INTO DATA(LS_DATA_H).
*
*    LT_DATA_TMP[] = CT_DATA[].
*    DELETE LT_DATA_TMP WHERE RUNNO <> LS_DATA_H-RUNNO.
*
*    PERFORM F_POST_FI_DOC_SOSLA  USING LT_DATA_TMP
*                                       UF_MODE
*                              CHANGING LS_FIDOC
*                                       LS_MESSG
*                                       LF_ERROR.
*    "Update Return Message
*    LOOP AT CT_DATA INTO DATA(LS_DATA)
*                    WHERE RUNNO  = LS_DATA_H-RUNNO.
*
*      MOVE-CORRESPONDING LS_MESSG TO LS_DATA.
*      IF UF_MODE IS NOT INITIAL.
*        "Test Run
*        LS_DATA-STATUS_ICON = GC_STATUS-WARNING.
*        LS_DATA-MESSAGE     = LS_MESSG-MSGTX.
*      ELSE.
*        IF LF_ERROR IS INITIAL.
*          LS_DATA-STATUS_ICON = GC_STATUS-SUCCESS.
*          LS_DATA-MESSAGE     = LS_MESSG-MSGTX.
*          LS_DATA-BUKRS       = LS_FIDOC-BUKRS.
*          LS_DATA-BELNR       = LS_FIDOC-BELNR.
*          LS_DATA-GJAHR       = LS_FIDOC-GJAHR.
*          LS_DATA-BUDAT       = GF_BUDAT.
*
*        ELSE.
*          LS_DATA-STATUS_ICON = GC_STATUS-ERROR.
*          LS_DATA-MESSAGE     = LS_MESSG-MSGTX.
*        ENDIF.
*
*        LS_DATA-USNAM = SY-UNAME.
*        LS_DATA-ERDAT = SY-DATUM.
*        LS_DATA-ERZMT = SY-UZEIT.
*
*      ENDIF.
*
*      MODIFY CT_DATA FROM LS_DATA.
*    ENDLOOP.
*
*  ENDLOOP. "CT_DATA_H
*
*  "Production run
*  IF UF_MODE IS INITIAL AND CT_DATA[] IS NOT INITIAL.
*    PERFORM F_UPDATE_LOGS_SOSLA USING CT_DATA.
*  ENDIF.
*
*  CT_OUTPUT[] = CT_DATA[].
*
**  IF CT_OUTPUT[] IS INITIAL.
**    CT_OUTPUT[] = CT_DATA[].
**  ELSE.
**    LOOP AT CT_OUTPUT INTO LS_OUTPUT WHERE SEL IS NOT INITIAL.
**
**      READ TABLE CT_DATA INTO LS_DATA
**                         WITH KEY  SERV_CON_ID = LS_OUTPUT-SERV_CON_ID
**                                   SVC_ITEM    = LS_OUTPUT-SVC_ITEM.
**      IF SY-SUBRC EQ 0.
**        MOVE-CORRESPONDING LS_DATA TO LS_OUTPUT.
**        MODIFY CT_OUTPUT FROM LS_OUTPUT.
**      ENDIF.
**
**    ENDLOOP.
**  ENDIF.
*
**  LOOP AT CT_OUTPUT ASSIGNING FIELD-SYMBOL(<LS_OUTPUT>).
**    IF <LS_OUTPUT>-RUNNO NE GC_CON-FIRST_ITEM_NO OR
**       <LS_OUTPUT>-MSGTY = GC_MSGTY-ERR OR
**       <LS_OUTPUT>-BELNR IS NOT INITIAL.
**      LS_STYLEROW-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
**      LS_STYLEROW-FIELDNAME = 'SEL'.
**      INSERT LS_STYLEROW  INTO TABLE <LS_OUTPUT>-FIELD_STYLE.
**    ENDIF.
**  ENDLOOP.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_user_command_1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> uf_ucomm
*&---------------------------------------------------------------------*
FORM F_USER_COMMAND_1 USING UF_UCOMM  TYPE  SY-UCOMM ##CALLED.
  DATA:
    LS_DATA_H TYPE TS_DATA_H,
    LT_DATA_H TYPE TT_DATA_H.


  DATA:
    LV_VALID   TYPE CHAR01,
    LV_REFRESH TYPE CHAR01,
    LV_SEL     TYPE CHAR01.

  DATA(LREF_MSG) = NEW CL_ALV_CHANGED_DATA_PROTOCOL( I_CALLING_ALV = GREF_GRID_1 ).

  GREF_GRID_1->GET_FRONTEND_FIELDCATALOG(
    IMPORTING
      ET_FIELDCATALOG   = LREF_MSG->MT_FIELDCATALOG
      ).

  LREF_MSG->REFRESH_PROTOCOL( ).

  CASE UF_UCOMM.

    WHEN GC_FUNC-SEL_A.
      LV_SEL = GC_CON-TRUE.

      PERFORM F_MARK_SEL_ALL USING    LV_SEL
                             CHANGING GT_OUTPUT.

      GREF_GRID_1->REFRESH_TABLE_DISPLAY( ).

    WHEN GC_FUNC-SEL_N.
      LV_SEL = ''.

      PERFORM F_MARK_SEL_ALL USING    LV_SEL
                             CHANGING GT_OUTPUT.


      GREF_GRID_1->REFRESH_TABLE_DISPLAY( ).

*    WHEN GC_FUNC-POST.
*
*      IF GREF_GRID_1->IS_READY_FOR_INPUT( ) = 0.
*        RETURN.
*      ENDIF.
*      "For Ext. Warranty (WBS)
*      CASE GC_CON-TRUE.
*        WHEN RB_SOMA.
**     Validate Data
*          GREF_GRID_1->CHECK_CHANGED_DATA( IMPORTING E_VALID = LV_VALID
*                                           CHANGING C_REFRESH = LV_REFRESH ).
**     Continue processing only when valid
*          IF LV_VALID IS INITIAL.
*            RETURN.
*          ENDIF.
*
*          LOOP AT GT_OUTPUT INTO DATA(LS_OUTPUT) WHERE SEL IS NOT INITIAL ##INTO_OK
*                                                   AND MSGTY EQ GC_MSGTY-SUC.
*            MOVE-CORRESPONDING LS_OUTPUT TO LS_DATA_H.
*            APPEND LS_DATA_H TO LT_DATA_H.
*          ENDLOOP.
*
*          IF LT_DATA_H IS NOT INITIAL.
*            PERFORM F_POST_DOCUMENT_SOMA USING     GC_MODE-PROD_RUN
*                                         CHANGING  LT_DATA_H
*                                                   GT_DATA
*                                                   GT_OUTPUT.
*
*
*            GREF_GRID_1->REFRESH_TABLE_DISPLAY( ).
*
*          ELSE.
*            MESSAGE S998(ZSDSFI01) WITH TEXT-E01.
*          ENDIF.
*        WHEN RB_WBS.
*          PERFORM F_POST_DOCUMENT_WBS_EXT USING  GC_MODE-PROD_RUN
*                                                 GT_DATA_H
*                                          CHANGING GT_DATA
*                                                 GT_OUTPUT.
*        WHEN RB_SOSLA.
*          PERFORM F_POST_DOCUMENT_SOSLA USING  GC_MODE-PROD_RUN
*                                               GT_DATA_H
*                                        CHANGING GT_DATA
*                                                 GT_OUTPUT.
*      ENDCASE.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_GET_DOCFLOW
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SERV_H
*&      <-- LT_DOC_FLOW
*&---------------------------------------------------------------------*
FORM F_GET_DOCFLOW CHANGING CT_SERV_H     TYPE TT_SERV_H
                             CT_DOC_FLOW   TYPE TT_DOC_FLOW
                             CT_SERV_C     TYPE TT_SERV_H
                             CT_DOC_FLOW_C TYPE TT_DOC_FLOW
                             CT_BILLING    TYPE TT_BILLING
                             CT_AUFK       TYPE TT_AUFK
                             CT_ACDOCA_MA  TYPE TT_ACDOCA.

  DATA: LT_CRMS4D_SERV_H_CON TYPE TABLE OF CRMS4D_SERV_H,
        LS_CRMS4D_SERV_H_CON TYPE CRMS4D_SERV_H.
  DATA: LR_VBELN TYPE RANGE OF VBELN_VF,
        LS_VBELN LIKE LINE OF LR_VBELN.
  DATA: LR_AUFNR TYPE RANGE OF AUFNR,
        LS_AUFNR LIKE LINE OF LR_AUFNR.

  "Service Order
  PERFORM F_CRM_ORDER_READ USING CT_SERV_H
                           CHANGING CT_DOC_FLOW.

  SORT CT_DOC_FLOW BY OBJKEY_B OBJTYPE_B.
  LOOP AT CT_DOC_FLOW INTO DATA(LS_DOC_FLOW) ##INTO_OK.
    CASE LS_DOC_FLOW-OBJTYPE_A.
      WHEN GC_CON-OBJTYPE_SVCONTRACT.
        LS_CRMS4D_SERV_H_CON-HEADER_GUID = LS_DOC_FLOW-OBJKEY_A.
        APPEND LS_CRMS4D_SERV_H_CON TO LT_CRMS4D_SERV_H_CON.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.
  IF LT_CRMS4D_SERV_H_CON[] IS NOT INITIAL.
* GET SERVICE CONTRACT LIST
    SELECT OBJTYPE_H,
         OBJECT_ID,
         HEADER_GUID,
         STAT_LIFECYCLE,
         SRV_CUST_BEG,
         SRV_CUST_END,
         CONTSTART,
         CONTEND,
         SOLD_TO_PARTY,
         NET_VALUE_H,
         CURRENCY,
         STAT_FOR_BILLING
      FROM CRMS4D_SERV_H
     FOR ALL ENTRIES IN @LT_CRMS4D_SERV_H_CON
     WHERE OBJTYPE_H EQ @GC_CON-OBJTYPE_SVCONTRACT
       AND HEADER_GUID EQ @LT_CRMS4D_SERV_H_CON-HEADER_GUID
      INTO TABLE @CT_SERV_C  ##TOO_MANY_ITAB_FIELDS.
    IF SY-SUBRC EQ 0.
      SORT CT_SERV_C BY OBJTYPE_H HEADER_GUID.
    ENDIF.

    "Read Service Contract
    PERFORM F_CRM_ORDER_READ  USING CT_SERV_C
                              CHANGING CT_DOC_FLOW_C.
    SORT CT_DOC_FLOW_C BY OBJKEY_A OBJTYPE_A OBJTYPE_B.
    IF CT_DOC_FLOW_C[] IS NOT INITIAL.
      LOOP AT CT_DOC_FLOW_C INTO DATA(LS_DOC_FLOW_C) ##INTO_OK
                            WHERE OBJTYPE_A = GC_CON-OBJTYPE_SVCONTRACT.
        CASE LS_DOC_FLOW_C-OBJTYPE_B.
          WHEN GC_CON-OBJTYPE_BILLING.
            LS_VBELN-SIGN = 'I'.
            LS_VBELN-OPTION = 'EQ'.
            LS_VBELN-LOW = LS_DOC_FLOW_C-OBJKEY_B.
            APPEND LS_VBELN TO LR_VBELN.
          WHEN GC_CON-OBJTYPE_ORDER.
            LS_AUFNR-SIGN = 'I'.
            LS_AUFNR-OPTION = 'EQ'.
            LS_AUFNR-LOW = LS_DOC_FLOW_C-OBJKEY_B.
            APPEND LS_AUFNR TO LR_AUFNR.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.

      IF LR_VBELN[] IS NOT INITIAL.
        DELETE ADJACENT DUPLICATES FROM LR_VBELN COMPARING ALL FIELDS.

* GET BILLING DATA
        SELECT A~VBELN,
               B~POSNR,
               A~BUPLA,
               B~KOSTL,
               B~PRCTR,
               A~VKORG,
               A~VTWEG,
               B~AUFNR,
               B~PS_PSP_PNR,
               A~WAERK,
               B~MATNR,
               B~NETWR
          FROM VBRK AS A
          INNER JOIN VBRP AS B
          ON A~VBELN EQ B~VBELN
         WHERE A~VBELN IN @LR_VBELN
           AND A~FKDAT <= @GF_BUDAT
           AND A~FKSTO EQ @SPACE
           AND A~SFAKN EQ @SPACE
*          AND B~MATNR IN @GRT_MATNR
          INTO TABLE @CT_BILLING.
        IF SY-SUBRC EQ 0.
          SORT CT_BILLING BY VBELN.
        ENDIF.
      ENDIF.

      IF LR_AUFNR[] IS NOT INITIAL.
        DELETE ADJACENT DUPLICATES FROM LR_AUFNR COMPARING ALL FIELDS.
* GET AUFK
        SELECT AUFNR,                                   "#EC CI_NOFIELD
               PRCTR
          FROM AUFK
          WHERE AUFNR IN @LR_AUFNR
          INTO TABLE @CT_AUFK.
        IF SY-SUBRC EQ 0.
          SORT CT_AUFK BY AUFNR.
        ENDIF.
      ENDIF.

      IF CT_AUFK[] IS NOT INITIAL.

* GET ACDOCA
        SELECT RLDNR,                                   "#EC CI_NOFIELD
             RBUKRS,
             GJAHR,
             BELNR,
             DOCLN,
             BUDAT,
             HSL,
             RHCUR,
             RACCT,
             AWTYP,
             AWREF,
             AWITEM,
             PS_POSID,
             MATNR,
             AUFNR
           FROM ACDOCA                             "#EC CI_NO_TRANSFORM
             FOR ALL ENTRIES IN @CT_AUFK
             WHERE RLDNR  = @GC_CON-RLDNR "0L
*               AND RBUKRS = @P_BUKRS
               AND BUDAT <= @GF_BUDAT
               AND RACCT  = @GF_GL_ACC_DR "'2495000040'
               AND XTRUEREV = @SPACE
               AND AUFNR  = @CT_AUFK-AUFNR
          INTO TABLE @CT_ACDOCA_MA                ##TOO_MANY_ITAB_FIELDS.
        IF SY-SUBRC EQ 0.
          SORT CT_ACDOCA_MA BY PS_POSID VBELV.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_convert_date
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> UF_CONTDATE
*&      <-- CF_DATE
*&---------------------------------------------------------------------*
FORM F_CONVERT_DATE  USING    UF_SVODATE TYPE CRMS4_SRV_CUST_BEG_TSTMP
                     CHANGING CF_DATE TYPE SY-DATUM.
  DATA: LF_TXT(30).

  CLEAR LF_TXT.
  LF_TXT = UF_SVODATE.
  CONDENSE LF_TXT.
  CF_DATE = LF_TXT+0(8).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CRM_ORDER_READ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_SERV_H
*&      <-- CT_DOC_FLOW
*&---------------------------------------------------------------------*
FORM F_CRM_ORDER_READ  USING UT_SERV_H     TYPE TT_SERV_H
                       CHANGING CT_DOC_FLOW TYPE TT_DOC_FLOW.
  DATA: LT_SERV_H_TEMP       TYPE TT_SERV_H.

  "For calling FM
  DATA:
    LT_HEADER_GUID     TYPE CRMT_OBJECT_GUID_TAB,
    LT_ITEM_GUID       TYPE CRMT_OBJECT_GUID_TAB,
    LV_ONLY_SPEC_ITEMS TYPE CRMT_BOOLEAN VALUE ABAP_FALSE,
    LT_DOC_FLOW        TYPE CRMT_DOC_FLOW_WRKT,
    LV_LOG_HANDLE      TYPE BALLOGHNDL.

  DATA: LS_HEADER_GUID     LIKE LINE OF LT_HEADER_GUID.

  LT_SERV_H_TEMP[] = UT_SERV_H[].
  SORT LT_SERV_H_TEMP BY HEADER_GUID.
  DELETE ADJACENT DUPLICATES FROM LT_SERV_H_TEMP COMPARING HEADER_GUID.

  LOOP AT LT_SERV_H_TEMP INTO DATA(LS_SERV_H_TMP) ##INTO_OK.
    CHECK LS_SERV_H_TMP-HEADER_GUID IS NOT INITIAL.
    LS_HEADER_GUID = LS_SERV_H_TMP-HEADER_GUID.
    APPEND LS_HEADER_GUID TO LT_HEADER_GUID.
  ENDLOOP.

  "Read Service Order
  CALL FUNCTION 'CRM_ORDER_READ'
    EXPORTING
      IT_HEADER_GUID       = LT_HEADER_GUID
      IT_ITEM_GUID         = LT_ITEM_GUID
      IV_ONLY_SPEC_ITEMS   = LV_ONLY_SPEC_ITEMS
    IMPORTING
      ET_DOC_FLOW          = LT_DOC_FLOW
    CHANGING
      CV_LOG_HANDLE        = LV_LOG_HANDLE
    EXCEPTIONS
      DOCUMENT_NOT_FOUND   = 1
      ERROR_OCCURRED       = 2
      DOCUMENT_LOCKED      = 3
      NO_CHANGE_AUTHORITY  = 4
      NO_DISPLAY_AUTHORITY = 5
      NO_CHANGE_ALLOWED    = 6
      OTHERS               = 7.
  IF SY-SUBRC EQ 0.
    CT_DOC_FLOW = LT_DOC_FLOW[].
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_INIT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_SET_INIT_DATA .

  DATA: LF_POPER    TYPE T009B-POPER,
        LF_DATE_NEX TYPE SY-DATUM,
        LF_TIME     TYPE TIMS,              " Time value
        LF_TSTAMP   TYPE TZNTSTMPSL      .  " Timestamp result
  DATA: LF_DAY_IN            TYPE SY-DATUM,
        LF_LAST_DAY_OF_MONTH TYPE DATUM.

  GF_BUDAT = P_BUDT_D.

  "First day of month
  GF_DATE_BEG =  |{ GF_BUDAT(6) }| & |01|.

  "Posting date
  GF_DATE_END = GF_BUDAT.

  PERFORM F_GET_CURRENT_YEAR USING GF_BUDAT
                             CHANGING GF_MONAT
                                      GF_GJAHR.

  "Period & Fiscal Year
  LF_POPER = GF_MONAT.
  GF_FISCYEARPER = |{ GF_GJAHR }| & |{ LF_POPER }|.

  LF_DAY_IN = |{ GF_BUDAT(6) }| & |01|.
  "Last date of Month in Posting date
  PERFORM F_GET_LAST_DAY_OF_MONTHS USING LF_DAY_IN
                                   CHANGING LF_LAST_DAY_OF_MONTH.
  IF LF_LAST_DAY_OF_MONTH IS NOT INITIAL.
    GF_LAST_DAY_OF_MONTH  = LF_LAST_DAY_OF_MONTH.
  ENDIF.

  LOOP AT S_COMPDT INTO DATA(LS_COMPDT).
    IF LS_COMPDT-SIGN = 'I' AND
       LS_COMPDT-OPTION = 'BT'.
      GF_COMPDT_BEG = LS_COMPDT-LOW.
      IF LS_COMPDT-HIGH IS NOT INITIAL.
        GF_COMPDT_END = LS_COMPDT-HIGH.
      ELSE.
        GF_COMPDT_END = LS_COMPDT-LOW.
      ENDIF.
    ENDIF.
    EXIT.
  ENDLOOP.

  IF GF_COMPDT_BEG IS NOT INITIAL.
    LF_TIME = GC_CON-MIN_TIME.
    PERFORM F_CONVERT_DATE_TIME_TO_TSTAMP USING GF_COMPDT_BEG
                                                LF_TIME
                                          CHANGING LF_TSTAMP.
    GF_TSTAMP_BEG =  LF_TSTAMP.
  ENDIF.

  IF GF_COMPDT_END IS NOT INITIAL.
    LF_TIME = GC_CON-MAX_TIME.
    LF_DATE_NEX = GF_COMPDT_END + 1.
    PERFORM F_CONVERT_DATE_TIME_TO_TSTAMP USING LF_DATE_NEX
                                                LF_TIME
                                          CHANGING LF_TSTAMP.
    GF_TSTAMP_END =  LF_TSTAMP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_LOGS_SOMA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_LOGS
*&---------------------------------------------------------------------*
FORM F_GET_LOGS_SOMA  CHANGING CT_LOGS TYPE TT_LOGS
                               CT_BKPF TYPE TT_BKPF.
  CLEAR: CT_LOGS[], CT_BKPF[].

* Get Log For Posting
  SELECT *
    FROM ZSDSFIT048
   WHERE FISCYEARPER <= @GF_FISCYEARPER
     AND SVO_ID IN @S_SVO_ID
     AND KUNNR IN @S_KUNNR
*    AND SVO_STARTDATE <= @GF_COMPDT_END
*    AND SVO_ENDDATE   >= @GF_COMPDT_BEG
     AND VKORG  EQ @P_VKORG
     AND RARTYP EQ @GC_RARTYP-MA
     AND MSGTY  EQ @GC_MSGTY-SUC
     AND XTRUEREV EQ @SPACE
    INTO TABLE @CT_LOGS.

  SORT CT_LOGS BY FISCYEARPER SVO_ID SVO_ITEM SERV_CON_ID SEQNO.

  PERFORM F_GET_ACCOUNTING_DOC USING CT_LOGS
                               CHANGING CT_BKPF.

  LOOP AT CT_LOGS INTO DATA(LS_LOGS).

    IF LS_LOGS-BELNR IS NOT INITIAL.
      READ TABLE CT_BKPF TRANSPORTING NO FIELDS
                         WITH KEY BUKRS = LS_LOGS-BUKRS
                                  BELNR = LS_LOGS-BELNR
                                  GJAHR = LS_LOGS-GJAHR
                         BINARY SEARCH.
      IF SY-SUBRC NE 0.
        IF LS_LOGS-XTRUEREV IS INITIAL.
          LS_LOGS-XTRUEREV = GC_CON-TRUE.
          LS_LOGS-USNAM = SY-UNAME.
          LS_LOGS-ERDAT = SY-DATUM.
          LS_LOGS-ERZMT = SY-UZEIT.
          MODIFY CT_LOGS FROM LS_LOGS
                         TRANSPORTING XTRUEREV USNAM ERDAT ERZMT.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PREPARE_REPORT_MA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_LOGS
*&      <-- GT_OUTPUT
*&---------------------------------------------------------------------*
FORM F_PREPARE_REPORT_SOMA  CHANGING CT_LOGS TYPE TT_LOGS
                                     CT_OUTPUT TYPE TT_OUTPUT  ##NEEDED.
  DATA: LS_OUTPUT TYPE ZSDSFIS139,
        LS_LOGS   TYPE ZSDSFIT048.

  CLEAR CT_OUTPUT[].

  SORT: GT_DATA BY SVO_ID SVO_ITEM SERV_CON_ID,
        CT_LOGS BY SVO_ID SVO_ITEM SERV_CON_ID
                   SEQNO DESCENDING ##DUPLICATE_OK.


  SORT CT_LOGS BY SVO_ID SVO_ITEM SERV_CON_ID SEQNO DESCENDING.
*-Display Data before post
  LOOP AT GT_DATA INTO DATA(LS_DATA)  ##INTO_OK.
    MOVE-CORRESPONDING LS_DATA TO LS_OUTPUT.

    LS_OUTPUT-MSGTY       = GC_MSGTY-WAR.
    LS_OUTPUT-STATUS_ICON = GC_STATUS-WARNING.

    CLEAR LS_LOGS.
    READ TABLE CT_LOGS INTO LS_LOGS WITH KEY SVO_ID    = LS_DATA-SVO_ID
                                             SVO_ITEM  = LS_DATA-SVO_ITEM
                                             SERV_CON_ID = LS_DATA-SERV_CON_ID
                                    BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IF LS_LOGS-XTRUEREV IS NOT INITIAL.
        LS_OUTPUT-SEQNO = LS_LOGS-SEQNO + 1.
      ENDIF.
    ELSE.
      LS_OUTPUT-SEQNO = 001.
    ENDIF.
    APPEND LS_OUTPUT TO CT_OUTPUT.
  ENDLOOP.
  SORT CT_OUTPUT BY SVO_ID SVO_ITEM SERV_CON_ID SEQNO.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_MARK_SEL_ALL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_SEL
*&      <-- GT_OUTPUT
*&---------------------------------------------------------------------*
FORM F_MARK_SEL_ALL  USING    UF_SEL    TYPE CHAR01
                     CHANGING CT_OUTPUT TYPE TT_OUTPUT.

  LOOP AT CT_OUTPUT ASSIGNING FIELD-SYMBOL(<LS_OUTPUT>)
                    WHERE MSGTY EQ GC_MSGTY-SUC
                      AND RUNNO EQ GC_CON-FIRST_ITEM_NO.
    <LS_OUTPUT>-SEL = UF_SEL.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_EXCL_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_TOOL_EXC_1
*&---------------------------------------------------------------------*
FORM F_ALV_EXCL_TOOLBAR CHANGING PT_EXCLUDE TYPE UI_FUNCTIONS.

  DATA:
  LS_EXCLUDE  TYPE  UI_FUNC.

* Initialize Output
  CLEAR: PT_EXCLUDE.

  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_CHECK.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_FIND_MORE.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_INFO.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_MB_SUM.
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_MB_SUBTOT.
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_CONSTANTS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_CONSTANTS .
  CONSTANTS:
    LC_GL_ACC_DR                TYPE  ZSDSDE_PARAM_NAME VALUE 'GL_ACC_DR',
    LC_GL_ACC_CR                TYPE  ZSDSDE_PARAM_NAME VALUE 'GL_ACC_CR',
    LC_GL_ACC_DR_EXT            TYPE  ZSDSDE_PARAM_NAME VALUE 'GL_ACC_DR_EXT',
    LC_GL_ACC_CR_EXT            TYPE  ZSDSDE_PARAM_NAME VALUE 'GL_ACC_CR_EXT',
    LC_GL_ACC_DR_SLA            TYPE  ZSDSDE_PARAM_NAME VALUE 'GL_ACC_DR_SLA',
    LC_GL_ACC_CR_SLA            TYPE  ZSDSDE_PARAM_NAME VALUE 'GL_ACC_CR_SLA',
    LC_DOC_TYPE                 TYPE  ZSDSDE_PARAM_NAME VALUE 'DOC_TYPE',
    LC_SYSTEM_STATUS            TYPE  ZSDSDE_PARAM_NAME VALUE 'SYSTEM_STATUS',
    LC_MAX_RECORDS              TYPE  ZSDSDE_PARAM_NAME VALUE 'MAX_RECORDS',
    LC_STATUS_COMPLETE          TYPE  ZSDSDE_PARAM_NAME VALUE 'STATUS_COMPLETE', "C
    LC_STATUS_OPEN              TYPE  ZSDSDE_PARAM_NAME VALUE 'STATUS_OPEN', "A
    LC_STATUS_INPROCESS         TYPE  ZSDSDE_PARAM_NAME VALUE 'STATUS_INPROCESS', "B
    LC_STATUS_RELEASED          TYPE  ZSDSDE_PARAM_NAME VALUE 'STATUS_RELEASED ', "D
    LC_WBS_SERV_EXTEND_WARRANTY TYPE  ZSDSDE_PARAM_NAME
                                VALUE 'WBS_SERV_EXTEND_WARRANTY'.
  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID   TYPE  PROGRAMM.


* Check Already Read?
  IF LF_READ EQ GC_CON-TRUE.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GF_GL_ACC_DR,
         GF_GL_ACC_CR,
         GF_GL_ACC_DR_EXT,
         GF_GL_ACC_CR_EXT,
         GF_GL_ACC_DR_SLA,
         GF_GL_ACC_CR_SLA.

* Assign REPID
  LF_REPID = SY-REPID.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = LF_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Mark Read Flag
  LF_READ = GC_CON-TRUE.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

    CASE <L_GENC>-PARAM.
*     ------------------------------------
*     GL Account for DR (MA)
*     ------------------------------------
      WHEN LC_GL_ACC_DR.
        GF_GL_ACC_DR = <L_GENC>-VALUE_LOW.

*     ------------------------------------
*     GL Account for CR (MA)
*     ------------------------------------
      WHEN LC_GL_ACC_CR.
        GF_GL_ACC_CR = <L_GENC>-VALUE_LOW.
*     ------------------------------------
*     GL Account for DR (Ext.Warranty)
*     ------------------------------------
      WHEN LC_GL_ACC_DR_EXT.
        GF_GL_ACC_DR_EXT = <L_GENC>-VALUE_LOW.
*     ------------------------------------
*     GL Account for CR(Ext.Warranty)
*     ------------------------------------
      WHEN LC_GL_ACC_CR_EXT.
        GF_GL_ACC_CR_EXT = <L_GENC>-VALUE_LOW.
*     ------------------------------------
*     GL Account for DR (SLA)
*     ------------------------------------
      WHEN LC_GL_ACC_DR_SLA.
        GF_GL_ACC_DR_SLA = <L_GENC>-VALUE_LOW.
*     ------------------------------------
*     GL Account for CR(SLA)
*     ------------------------------------
      WHEN LC_GL_ACC_CR_SLA.
        GF_GL_ACC_CR_SLA = <L_GENC>-VALUE_LOW.
*     ------------------------------------
*     Document type
*     ------------------------------------
      WHEN LC_DOC_TYPE.
        GF_DOC_TYPE = <L_GENC>-VALUE_LOW.

*     ------------------------------------
*     System Status = 'REL'
*     ------------------------------------
      WHEN LC_SYSTEM_STATUS.
        GF_SYSTEM_STATUS = <L_GENC>-VALUE_LOW.

*     ------------------------------------
*     WBS Type 4 extended warranty
*     ------------------------------------
      WHEN LC_WBS_SERV_EXTEND_WARRANTY.
        INSERT VALUE #( SIGN   =  <L_GENC>-PARAM_SIGN
                        OPTION =  <L_GENC>-PARAM_OPTION
                        LOW    =  <L_GENC>-VALUE_LOW
                        HIGH   =  <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_WBS_EXT.
*     ------------------------------------
*     Max Record
*     ------------------------------------
      WHEN LC_MAX_RECORDS.
        GF_MAX_RECORDS = <L_GENC>-VALUE_LOW.
*     ------------------------------------
*     SVO Status
*     ------------------------------------
      WHEN LC_STATUS_COMPLETE.
        GF_STATUS_COMPLETE = <L_GENC>-VALUE_LOW.
      WHEN LC_STATUS_OPEN.
        INSERT VALUE #( SIGN   =  <L_GENC>-PARAM_SIGN
                        OPTION =  <L_GENC>-PARAM_OPTION
                        LOW    =  <L_GENC>-VALUE_LOW
                        HIGH   =  <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_STATUS_OTH.
      WHEN LC_STATUS_INPROCESS.
        INSERT VALUE #( SIGN   =  <L_GENC>-PARAM_SIGN
                        OPTION =  <L_GENC>-PARAM_OPTION
                        LOW    =  <L_GENC>-VALUE_LOW
                        HIGH   =  <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_STATUS_OTH.
      WHEN LC_STATUS_RELEASED.
        INSERT VALUE #( SIGN   =  <L_GENC>-PARAM_SIGN
                        OPTION =  <L_GENC>-PARAM_OPTION
                        LOW    =  <L_GENC>-VALUE_LOW
                        HIGH   =  <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_STATUS_OTH.
    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_hotspot_click_1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> uf_row_id
*&      --> uf_column_id
*&---------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK_1 USING
                        UF_ROW_ID     TYPE  LVC_S_ROW
                        UF_COLUMN_ID  TYPE  LVC_S_COL       ##CALLED.

  READ TABLE GT_OUTPUT INTO DATA(LS_OUTPUT)
    INDEX UF_ROW_ID-INDEX.

  CASE UF_COLUMN_ID-FIELDNAME.
    WHEN 'BELNR'.
      IF LS_OUTPUT-BELNR IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD LS_OUTPUT-BELNR.
        SET PARAMETER ID 'BUK' FIELD LS_OUTPUT-BUKRS.
        SET PARAMETER ID 'GJR' FIELD LS_OUTPUT-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.   "#EC CI_CALLTA
      ENDIF.
    WHEN 'VBELN_VF'.
      IF LS_OUTPUT-VBELN_VF IS NOT INITIAL.
        SET PARAMETER ID 'VF' FIELD LS_OUTPUT-VBELN_VF.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN .  "#EC CI_CALLTA
      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_ALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_ALV OUTPUT.
* Check ALV 1 in Edit Mode?
  IF GS_LAYOUT_1-EDIT EQ SPACE.
    READ TABLE GT_FIELDCAT_1 TRANSPORTING NO FIELDS
                             WITH KEY EDIT = 'X'.
    IF SY-SUBRC NE 0.
      APPEND GC_SAVE_1 TO GT_EXCL.
    ENDIF.
  ENDIF.


  SET TITLEBAR '9000' ##TITL_UNDEF. "
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form F_SET_SEL_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_SET_SEL_SCREEN .
  LOOP AT SCREEN.

    IF SCREEN-NAME CS 'MSGTY'.
      SCREEN-ACTIVE = '1'.
      MODIFY SCREEN.
    ENDIF.
    IF SCREEN-GROUP1 EQ 'SO' OR
       SCREEN-GROUP1 EQ 'WBS' OR
       SCREEN-GROUP1 EQ 'FI'.
      SCREEN-ACTIVE = '0'.
      MODIFY SCREEN.
    ELSEIF SCREEN-GROUP1 EQ 'SVD'.
      SCREEN-ACTIVE = '1'.
      MODIFY SCREEN.
    ENDIF.

    IF SCREEN-GROUP1 EQ 'SV'.
      SCREEN-ACTIVE = '1'.
      MODIFY SCREEN.
    ENDIF.
    IF SCREEN-GROUP1 EQ 'REP'.
      SCREEN-ACTIVE = '0'.
      MODIFY SCREEN.
    ENDIF.

    IF SCREEN-GROUP1 EQ 'WBS'.
      SCREEN-ACTIVE = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CHK_SEL_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_CHK_SEL_SCREEN .

  IF S_COMPDT IS INITIAL.
*   Complete date is required.
    MESSAGE E000(38) WITH TEXT-E04.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_DATA_SOMA CHANGING CT_LOGS   TYPE TT_LOGS
                              CT_DATA_H TYPE TT_DATA_H
                              CT_DATA   TYPE TT_DATA.
  DATA:
    LT_SERV_C     TYPE TT_SERV_H,
    LT_DOC_FLOW   TYPE TT_DOC_FLOW,
    LT_DOC_FLOW_C TYPE TT_DOC_FLOW,
    LT_BILLING    TYPE TT_BILLING,
    LT_AUFK       TYPE TT_AUFK,
    LT_ACDOCA     TYPE TT_ACDOCA,
    LT_SERV_H     TYPE TT_SERV_H,
    LT_SERV_I     TYPE TT_SERV_I,
    LT_MATERIAL   TYPE TT_MATERIAL,
    LS_DATA_H     TYPE TS_DATA_H,
    LS_DATA       TYPE TS_DATA.

  DATA: LF_BILLING_NO TYPE VBRK-VBELN,
        LF_ORDER_NO   TYPE AUFK-AUFNR.

* Initialize DATA
  CLEAR: CT_DATA.
  SORT CT_LOGS BY SVO_ID SVO_ITEM SERV_CON_ID SEQNO DESCENDING.

* Get Service Order List
  SELECT OBJTYPE_H,
         OBJECT_ID,
         HEADER_GUID,
         STAT_LIFECYCLE,
         SRV_CUST_BEG,
         SRV_CUST_END,
         CONTSTART,
         CONTEND,
         SOLD_TO_PARTY,
         NET_VALUE_H,
         CURRENCY,
         STAT_FOR_BILLING
    FROM CRMS4D_SERV_H
   WHERE OBJTYPE_H EQ @GC_CON-OBJTYPE_SVORDER
     AND OBJECT_ID IN @S_SVO_ID
     AND SRV_CUST_BEG >= @GF_TSTAMP_BEG
     AND SRV_CUST_END <= @GF_TSTAMP_END
     AND ( PROCESS_TYPE = @GC_PROCESS_TYPE-ZRO1 OR
           PROCESS_TYPE = @GC_PROCESS_TYPE-ZRP1  )
     AND SOLD_TO_PARTY IN @S_KUNNR
    INTO TABLE @LT_SERV_H.
  IF SY-SUBRC NE 0.
  ELSE.
    SORT LT_SERV_H BY OBJECT_ID.

    SELECT OBJTYPE_H,
           OBJECT_ID,
           NUMBER_INT,
           SALES_ORG_SD,
           SALES_OFFICE_SD,
           SALES_GROUP_SD,
           DIS_CHANNEL,
           PRODUCT_ID,
           NET_VALUE_I,
           CURRENCY,
           AC_OBJECT_TYPE,
           AC_ASSIGNMENT,
           STAT_LIFECYCLE,
           ITEM_GUID
    FROM CRMS4D_SERV_I                             "#EC CI_NO_TRANSFORM
    FOR ALL ENTRIES IN @LT_SERV_H
    WHERE OBJECT_ID = @LT_SERV_H-OBJECT_ID
      AND SALES_ORG_SD = @P_VKORG
    INTO TABLE @LT_SERV_I.
    IF SY-SUBRC EQ 0.
      SORT LT_SERV_I BY OBJECT_ID NUMBER_INT.
    ENDIF.
  ENDIF.

  CLEAR GRT_ITEM_GUID.
  GRT_ITEM_GUID = VALUE #( FOR <LS_SERV_I> IN LT_SERV_I ( LOW  = <LS_SERV_I>-ITEM_GUID
                                                          OPTION  = 'EQ'
                                                          SIGN    = 'I' ) ).
  PERFORM F_FILTER_SVO_DEL_STATUS CHANGING LT_SERV_I.

  CLEAR: GRT_MATNR[], GRT_VTWEG[].
  GRT_MATNR  = VALUE #( FOR <LS_SERV_I> IN LT_SERV_I ( LOW  = <LS_SERV_I>-PRODUCT_ID
                                                          OPTION  = 'EQ'
                                                          SIGN    = 'I' ) ).
  GRT_VTWEG  = VALUE #( FOR <LS_SERV_I> IN LT_SERV_I ( LOW  = <LS_SERV_I>-DIS_CHANNEL
                                                          OPTION  = 'EQ'
                                                          SIGN    = 'I' ) ).

  PERFORM F_GET_MATERIAL_DATA USING GC_KTGRM-MA
                              CHANGING LT_MATERIAL.

  PERFORM F_GET_DOCFLOW CHANGING LT_SERV_H
                                 LT_DOC_FLOW
                                 LT_SERV_C
                                 LT_DOC_FLOW_C
                                 LT_BILLING
                                 LT_AUFK
                                 LT_ACDOCA.

  LOOP AT LT_SERV_H INTO DATA(LS_SERV_H).

    CLEAR LS_DATA.
    LS_DATA-FISCYEARPER   = GF_FISCYEARPER.
    LS_DATA-RARTYP        = GC_RARTYP-MA.
    LS_DATA-SVO_ID        = LS_SERV_H-OBJECT_ID.
    LS_DATA-SVO_STATUS    = LS_SERV_H-STAT_LIFECYCLE.

*   Start Date
    PERFORM F_CONVERT_DATE USING LS_SERV_H-SRV_CUST_BEG
                           CHANGING LS_DATA-SVO_STARTDATE.

*   End Date
    PERFORM F_CONVERT_DATE USING LS_SERV_H-SRV_CUST_END
                           CHANGING LS_DATA-SVO_ENDDATE.

    READ TABLE LT_DOC_FLOW INTO DATA(LS_DOC_FLOW)
                           WITH KEY OBJKEY_B = LS_SERV_H-HEADER_GUID
                                    OBJTYPE_B = GC_CON-OBJTYPE_SVORDER
                           BINARY SEARCH.
    IF SY-SUBRC EQ 0.

      "Check Service Contract
      READ TABLE LT_SERV_C INTO DATA(LS_SERV_C)
                           WITH KEY OBJTYPE_H   = LS_DOC_FLOW-OBJTYPE_A
                                    HEADER_GUID = LS_DOC_FLOW-OBJKEY_A  ##WARN_OK.
      IF SY-SUBRC EQ 0.
        CLEAR LS_SERV_C.
        LOOP AT LT_SERV_C INTO LS_SERV_C  ##WARN_OK ##INTO_OK
                          WHERE OBJTYPE_H   = LS_DOC_FLOW-OBJTYPE_A
                            AND HEADER_GUID = LS_DOC_FLOW-OBJKEY_A.
          IF LS_DATA-SERV_CON_ID IS INITIAL.
            LS_DATA-SERV_CON_ID   = LS_SERV_C-OBJECT_ID.
            LS_DATA-KUNNR         = LS_SERV_C-SOLD_TO_PARTY.
            LS_DATA-SERV_CON_VAL  = LS_SERV_C-NET_VALUE_H.
            LS_DATA-SERV_CON_STA  = LS_SERV_C-STAT_LIFECYCLE.
          ENDIF.

          IF LS_SERV_C-STAT_FOR_BILLING = GC_CON-STAT_FOR_BILLING_A.
            "'A' = No billing relevant items released for billing

            CLEAR: LF_BILLING_NO, LF_ORDER_NO.
            LOOP AT LT_DOC_FLOW_C INTO DATA(LS_DOC_FLOW_C)  ##INTO_OK
                                  WHERE OBJKEY_A = LS_SERV_C-HEADER_GUID
                                    AND OBJTYPE_A = GC_CON-OBJTYPE_SVCONTRACT.
              "Billing
              CASE LS_DOC_FLOW_C-OBJTYPE_B.
                WHEN GC_CON-OBJTYPE_BILLING.

                  LF_BILLING_NO = LS_DOC_FLOW_C-OBJKEY_B.

                  IF LS_DATA-VBELN_VF IS INITIAL.
                    LS_DATA-VBELN_VF     = LF_BILLING_NO.
                    LS_DATA-VBELN_VF_TXT = LF_BILLING_NO.
                  ELSE.
                    LS_DATA-VBELN_VF_TXT = |{ LS_DATA-VBELN_VF_TXT },{ LF_BILLING_NO } |.
                  ENDIF.

                  "Cal Derfer Rev(Billing)
                  LOOP AT LT_BILLING INTO DATA(LS_BILLING) ##INTO_OK
                                     WHERE VBELN = LF_BILLING_NO.
*                  IF LS_DATA-AUFNR IS INITIAL AND
*                     LS_DATA-POSID IS INITIAL.
*                    LS_DATA-AUFNR = LS_BILLING-AUFNR.
*                    LS_DATA-PRCTR = LS_BILLING-PRCTR.
*                   LS_DATA-BUPLA = LS_BILLING-BUPLA.
*                   LS_DATA-KOSTL = LS_BILLING-KOSTL.
*
*                    PERFORM F_CONVERSION_EXIT_ABPSP_OUTPUT USING LS_BILLING-PS_PSP_PNR
*                                                           CHANGING LS_DATA-POSID.
*                  ENDIF.

                    "Cal Billing Amount
                    LS_DATA-NETWR_VF = LS_DATA-NETWR_VF + LS_BILLING-NETWR.
                  ENDLOOP.

                WHEN GC_CON-OBJTYPE_ORDER.
                  LF_ORDER_NO = LS_DOC_FLOW_C-OBJKEY_B.

                  READ TABLE LT_AUFK INTO DATA(LS_AUFK)
                                     WITH KEY AUFNR = LF_ORDER_NO
                                     BINARY SEARCH.
                  IF SY-SUBRC EQ 0.
                    LS_DATA-AUFNR = LS_AUFK-AUFNR.
                    LS_DATA-PRCTR = LS_AUFK-PRCTR.

                    "Cal Balance Before Post
                    LOOP AT LT_ACDOCA INTO DATA(LS_ACDOCA)  ##INTO_OK
                                      WHERE AUFNR = LS_DATA-AUFNR.
                      LS_DATA-BALANCE = LS_DATA-BALANCE + ( LS_ACDOCA-HSL * -1 ).
                    ENDLOOP.
                  ENDIF.
              ENDCASE.
            ENDLOOP.
          ENDIF.
        ENDLOOP.

        LS_DATA-RUNNO  = GC_CON-FIRST_ITEM_NO.
        LOOP AT LT_SERV_I INTO DATA(LS_SERV_I)   ##INTO_OK
                          WHERE OBJECT_ID = LS_SERV_H-OBJECT_ID.

          LS_DATA-SVO_ITEM  = LS_SERV_I-NUMBER_INT.
          LS_DATA-VKORG     = LS_SERV_I-SALES_ORG_SD.
          LS_DATA-VKBUR     = LS_SERV_I-SALES_OFFICE_SD.
          LS_DATA-VKGRP     = LS_SERV_I-SALES_GROUP_SD.
          LS_DATA-MATNR     = LS_SERV_I-PRODUCT_ID.
          LS_DATA-NET_VALUE = LS_SERV_I-NET_VALUE_I.
          LS_DATA-WAERS     = LS_SERV_I-CURRENCY.

          IF LS_DATA-SVO_STATUS = GF_STATUS_COMPLETE.
            LS_DATA-NETWR     = LS_SERV_I-NET_VALUE_I.

          ENDIF.

          CASE LS_SERV_I-AC_OBJECT_TYPE.
            WHEN GC_AC_OBJECT_TYPE-ORDER. "ORDER
              LS_DATA-SVO_ORDER = LS_SERV_I-AC_ASSIGNMENT.
            WHEN GC_AC_OBJECT_TYPE-WBS. "PSPEL(WBS)
              LS_DATA-SVO_WBS = LS_SERV_I-AC_ASSIGNMENT.
            WHEN OTHERS.
          ENDCASE.

          READ TABLE LT_MATERIAL INTO DATA(LS_MATERIAL)
                                 WITH KEY MATNR = LS_DATA-MATNR
                                 BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            LS_DATA-KTGRM     = LS_MATERIAL-KTGRM.
            LS_DATA-VTEXT     = LS_MATERIAL-VTEXT.
          ENDIF.

          "Insert Header
          IF LS_DATA-SVO_STATUS EQ GF_STATUS_COMPLETE AND
             LS_DATA-MSGTY <> GC_MSGTY-ERR.

            CLEAR LS_DATA_H.
            LS_DATA_H-SVO_ID      = LS_DATA-SVO_ID.
            LS_DATA_H-SERV_CON_ID = LS_DATA-SERV_CON_ID.
            COLLECT LS_DATA_H INTO CT_DATA_H.
          ENDIF.

          READ TABLE CT_LOGS INTO DATA(LS_LOGS)
                             WITH KEY SVO_ID      = LS_DATA-SVO_ID
                                      SVO_ITEM    = LS_DATA-SVO_ITEM
                                      SERV_CON_ID = LS_DATA-SERV_CON_ID
                                      MSGTY       = GC_MSGTY-SUC
                                      XTRUEREV    = SPACE.
          IF SY-SUBRC EQ 0.
            IF LS_LOGS-BELNR IS NOT INITIAL.

              LS_DATA-SEQNO = LS_LOGS-SEQNO.
              LS_DATA-BUKRS = LS_LOGS-BUKRS.
              LS_DATA-BELNR = LS_LOGS-BELNR.
              LS_DATA-GJAHR = LS_LOGS-GJAHR.
              LS_DATA-BUDAT = LS_LOGS-BUDAT.
              LS_DATA-MSGTY = LS_LOGS-MSGTY.
              LS_DATA-MSGID = LS_LOGS-MSGID.
              LS_DATA-MSGNO = LS_LOGS-MSGNO.
              LS_DATA-MESSAGE = LS_LOGS-MESSAGE.
              LS_DATA-USNAM = LS_LOGS-USNAM.
              LS_DATA-ERDAT = LS_LOGS-ERDAT.
              LS_DATA-ERZMT = LS_LOGS-ERZMT.
            ENDIF.
          ENDIF.

          APPEND LS_DATA TO CT_DATA.
          CLEAR LS_DATA-RUNNO.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_MATERIAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_MATERIAL
*&---------------------------------------------------------------------*
FORM F_GET_MATERIAL_DATA USING UF_KTGRM TYPE KTGRM  ##NEEDED
                         CHANGING CT_MATERIAL TYPE TT_MATERIAL.
  CLEAR CT_MATERIAL[].

  SORT GRT_MATNR.
  DELETE ADJACENT DUPLICATES FROM GRT_MATNR COMPARING ALL FIELDS.
  SORT GRT_VTWEG.
  DELETE ADJACENT DUPLICATES FROM GRT_VTWEG COMPARING ALL FIELDS.

  IF GRT_MATNR[] IS NOT INITIAL.

    SELECT A~MATNR,
           A~VKORG,
           A~VTWEG,
           A~KTGRM,
           B~VTEXT
      INTO TABLE @CT_MATERIAL
      FROM MVKE AS A
       INNER JOIN TVKMT AS B                           "#EC CI_BUFFJOIN
      ON A~KTGRM = B~KTGRM
      WHERE A~MATNR IN @GRT_MATNR
        AND A~VKORG = @P_VKORG
        AND A~VTWEG IN @GRT_VTWEG
        AND B~SPRAS = @SY-LANGU.
    IF SY-SUBRC EQ 0.
      SORT CT_MATERIAL BY MATNR.
*      IF RB_SOMA IS NOT INITIAL
*      OR RB_SOSLA IS NOT INITIAL.
*        DELETE CT_MATERIAL WHERE KTGRM NE UF_KTGRM.
*      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_cal_amt_days
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DATA_SVC_STARTDATE
*&      --> LS_DATA_SVC_ENDDATE
*&      <-- LS_DATA_SVC_DAYS
*&---------------------------------------------------------------------*
FORM F_CAL_AMT_DAYS  USING    UF_STARTDATE TYPE SY-DATUM
                              UF_ENDDATE   TYPE SY-DATUM
                              UF_NETAMT    TYPE NETWR
                     CHANGING CF_DAYS      TYPE ZSDSDE_DAYS
                              CF_AMTDAY    TYPE ZSDSDE_AMTDAY.
  DATA: LV_DAYS    TYPE I.

  PERFORM F_CAL_TOTAL_DYAS USING UF_STARTDATE
                                 UF_ENDDATE
                           CHANGING LV_DAYS.
  IF SY-SUBRC EQ 0.
    CF_DAYS = LV_DAYS.
  ELSE.
    CF_DAYS = 0.
  ENDIF.
  IF UF_NETAMT > 0 AND CF_DAYS NE 0.
    CF_AMTDAY = UF_NETAMT / CF_DAYS.
  ENDIF.
ENDFORM.

**&---------------------------------------------------------------------*
**& Form F_GET_WBS_EXTEND_WARRANTY
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> GRT_WBS_EXT
**&      <-- GT_WBS
**&---------------------------------------------------------------------*
*FORM F_GET_WBS_EXTEND_WARRANTY  USING URT_WBS TYPE TT_RANGE_WBS
*                                CHANGING CT_DATA_H TYPE TT_DATA_H
*                                         CT_DATA   TYPE TT_DATA.
*
*  DATA: LT_PRPS     TYPE TT_PRPS,
*        LT_REVENUE  TYPE TT_REVENUE,
*        LT_ACDOCA   TYPE TT_ACDOCA,
*        LT_BILLING  TYPE TT_BILLING,
*        LT_WBS      TYPE TT_WBS,
*        LT_MATERIAL TYPE TT_MATERIAL.
*
*  DATA: LS_DATA   TYPE TS_DATA,
*        LS_DATA_H TYPE TS_DATA_H.
*
*  DATA: LF_BATCH_SIZE TYPE I, " Number of line items in each batch
*        LF_COUNT      TYPE I, " Line item counter
*        LF_RUNNO      TYPE BSEG-BUZEI,
*        LF_REMAINING  TYPE I. " Remaining line items to process
*
*  PERFORM F_SELECT_PRPS USING URT_WBS
*                        CHANGING LT_PRPS.
*  IF LT_PRPS IS INITIAL.
*    RETURN.
*  ENDIF.
*
*  SELECT EQUNR,                                         "#EC CI_NOFIELD
*         MATNR,
*         EXT_POSID,
*         EXT_WRT_BEG,
*         EXT_WRT_END,
*         VBELN_VL,
*         KUNNR,
*         VKBUR,
*         VKGRP,
*         B~POSID
*  FROM ZSDSCMT003 AS A INNER JOIN @LT_PRPS AS B
*  ON A~EXT_POSID = B~POSID
* WHERE  EXT_POSID IN  @GRT_WBS_EXT
*   AND  EXT_WRT_BEG <= @GF_DATE_END
*   AND  EXT_WRT_END >= @GF_DATE_BEG
*  INTO TABLE @DATA(LT_ZSDSCMT003).
*
*  DATA(LT_ZSDSCMT003_TMP) = LT_ZSDSCMT003[].
*  SORT LT_ZSDSCMT003_TMP BY POSID.
*  DELETE ADJACENT DUPLICATES FROM LT_ZSDSCMT003_TMP COMPARING POSID.
*  MOVE-CORRESPONDING LT_ZSDSCMT003_TMP TO LT_WBS KEEPING TARGET LINES.
*
*  "DO
*  CLEAR: GRT_VBELN[].
*  GRT_VBELN  = VALUE #( FOR <LS_ZSDSCMT003> IN LT_ZSDSCMT003
*                                       ( LOW  = <LS_ZSDSCMT003>-VBELN_VL
*                                         OPTION = 'EQ'
*                                         SIGN   = 'I' ) ).
*  SORT GRT_VBELN.
*  DELETE ADJACENT DUPLICATES FROM GRT_VBELN COMPARING ALL FIELDS.
*  IF GRT_VBELN[] IS NOT INITIAL.
*
*    SELECT RUUID,                                       "#EC CI_NOFIELD
*      VBELV,
*      POSNV,
*      VBTYP_V,
*      VBELN,
*      POSNN,
*      VBTYP_N
*    FROM VBFA AS B                                 "#EC CI_NO_TRANSFORM
*      WHERE VBELV  IN @GRT_VBELN      "DO
*        AND VBTYP_V = @GC_CON-VBTYP_V " 'J' "DO
*        AND VBTYP_N = @GC_CON-VBTYP_N " 'M' "Invoice
*      INTO TABLE @DATA(LT_VBFA).
*    IF SY-SUBRC EQ 0.
*      SORT LT_VBFA BY VBELN POSNN.
*      DELETE ADJACENT DUPLICATES FROM LT_VBFA COMPARING VBELN POSNN.
*
** GET BILLING DATA
*      SELECT A~VBELN,
*             B~POSNR,
*             A~BUPLA,
*             B~KOSTL,
*             B~PRCTR,
*             A~VKORG,
*             A~VTWEG,
*             B~AUFNR,
*             B~PS_PSP_PNR
*        FROM VBRK AS A
*        INNER JOIN VBRP AS B
*        ON A~VBELN EQ B~VBELN
*        FOR ALL ENTRIES IN @LT_VBFA
*       WHERE A~VBELN = @LT_VBFA-VBELN
*        AND  B~POSNR = @LT_VBFA-POSNN
*        INTO TABLE @LT_BILLING  ##TOO_MANY_ITAB_FIELDS.
*      IF SY-SUBRC EQ 0.
*        SORT LT_BILLING BY VBELN POSNR.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*
*  CLEAR: GRT_MATNR[], GRT_VTWEG[].
*  GRT_MATNR  = VALUE #( FOR <LS_ZSDSCMT003> IN LT_ZSDSCMT003
*                                           ( LOW  = <LS_ZSDSCMT003>-MATNR
*                                             OPTION = 'EQ'
*                                             SIGN   = 'I' ) ).
*  GRT_VTWEG  = VALUE #( FOR <LS_BILLING> IN LT_BILLING
*                                         ( LOW  = <LS_BILLING>-VTWEG
*                                           OPTION  = 'EQ'
*                                           SIGN    = 'I' ) ).
*
*  PERFORM F_GET_MATERIAL_DATA USING GC_KTGRM-EXT
*                              CHANGING LT_MATERIAL.
*
*  PERFORM F_SELECT_ACDOCA USING LT_WBS
*                                LT_BILLING
*                                LT_MATERIAL
*                               CHANGING LT_ACDOCA
*                                        LT_REVENUE.
*
*  LOOP AT LT_REVENUE INTO DATA(LS_REVENUE)  ##INTO_OK.
*
*    READ TABLE LT_ZSDSCMT003 INTO DATA(LS_ZSDSCMT003)
*                             WITH KEY POSID = LS_REVENUE-POSID
*                                      MATNR = LS_REVENUE-MATNR
*                                      VBELN_VL = LS_REVENUE-VBELN_VL.
*    IF SY-SUBRC EQ 0.
*
*      CLEAR LS_DATA.
*      LS_DATA-FISCYEARPER = GF_FISCYEARPER.
*      LS_DATA-RARTYP      = GC_RARTYP-EXT.
*      LS_DATA-EXT_POSID   = LS_ZSDSCMT003-EXT_POSID.
*      LS_DATA-EXT_WRT_BEG = LS_ZSDSCMT003-EXT_WRT_BEG.
*      LS_DATA-EXT_WRT_END = LS_ZSDSCMT003-EXT_WRT_END.
*      LS_DATA-MATNR     = LS_ZSDSCMT003-MATNR.
*
*      READ TABLE LT_MATERIAL INTO DATA(LS_MATERIAL)
*                             WITH KEY MATNR = LS_DATA-MATNR
*                             BINARY SEARCH.
*      IF SY-SUBRC EQ 0.
*        LS_DATA-KTGRM     = LS_MATERIAL-KTGRM.
*        LS_DATA-VTEXT     = LS_MATERIAL-VTEXT.
*      ENDIF.
*
*      LS_DATA-KUNNR     = LS_ZSDSCMT003-KUNNR.
*      LS_DATA-VKORG     = P_VKORG.
*      LS_DATA-VKBUR     = LS_ZSDSCMT003-VKBUR.
*      LS_DATA-VKGRP     = LS_ZSDSCMT003-VKGRP.
*
*      LS_DATA-VBELN_VL   = LS_REVENUE-VBELN_VL.
*      LS_DATA-VBELN_VF   = LS_REVENUE-VBELN_VF.
*      LS_DATA-POSNR_VF   = LS_REVENUE-POSNR_VF.
*      LS_DATA-NET_VALUE  = LS_REVENUE-AMOUNT. "Amount
*      LS_DATA-WAERS      = LS_REVENUE-RHCUR.
*
*      PERFORM F_CAL_AMT_DAYS USING LS_DATA-EXT_WRT_BEG
*                                   LS_DATA-EXT_WRT_END
*                                   LS_DATA-NET_VALUE  "Amount
*                        CHANGING LS_DATA-TOTDAYS "total Days
*                                 LS_DATA-AMTDAY.  "Amount per Day
*
*      PERFORM F_CAL_REVENUE_AMOUNT USING LS_DATA-EXT_WRT_BEG
*                                         LS_DATA-EXT_WRT_END
*                                         LS_DATA-AMTDAY
*                                  CHANGING LS_DATA-NETWR "Period Amount
*                                           LS_DATA-POSDAYS."Days of Posting
*
*      READ TABLE LT_BILLING INTO DATA(LS_BILLING)
*                            WITH KEY VBELN = LS_DATA-VBELN_VF
*                                     POSNR = LS_DATA-POSNR_VF
*                            BINARY SEARCH.
*      IF SY-SUBRC EQ 0.
*        LS_DATA-BUPLA = LS_BILLING-BUPLA.
*        LS_DATA-KOSTL = LS_BILLING-KOSTL.
*        LS_DATA-PRCTR = LS_BILLING-PRCTR.
*      ENDIF.
*
*      "Overwrite BUPLA From selection screen
*      IF P_BUPLA IS NOT INITIAL.
*        LS_DATA-BUPLA    = P_BUPLA.
*      ENDIF.
*
*      APPEND LS_DATA TO CT_DATA.
*
*    ENDIF.
*
*  ENDLOOP.
*
**-Split Doc
*  SORT CT_DATA BY FISCYEARPER EXT_POSID VBELN_VF POSNR_VF.
*
****** Insert Data For Test Split doc***********
**  DATA LT_DATA_TMP TYPE TT_DATA.
**  DATA(LV_INDEX) = 0.
**  DATA(LV_ITEM) = 10.
**  LOOP AT GT_DATA INTO LS_DATA.
**    LS_DATA-EXT_POSID = 'P-1-24000006-04-07-002'.
**    LS_DATA-VBELN_VF   = '5010000080'.
**    LS_DATA-POSNR_VF   = LV_ITEM.
**    LS_DATA-SEQNO = LV_INDEX + 1.
**    LS_DATA-NETWR = 10.
**    APPEND LS_DATA TO LT_DATA_TMP.
**    LV_ITEM = LV_ITEM + 1.
**  ENDLOOP.
**
**  LV_ITEM = 10.
**  LOOP AT GT_DATA INTO LS_DATA.
**    LS_DATA-EXT_POSID = 'P-1-24000006-04-07-002'.
**    LS_DATA-VBELN_VF   = '5010000081'.
**    LS_DATA-POSNR_VF   = LV_ITEM.
**    LS_DATA-SEQNO = LV_INDEX + 1.
**    LS_DATA-NETWR = 10.
**    APPEND LS_DATA TO LT_DATA_TMP.
**    LV_ITEM = LV_ITEM + 1.
**  ENDLOOP.
**
**  LV_ITEM = 10.
**  LOOP AT GT_DATA INTO LS_DATA.
**    LS_DATA-EXT_POSID = 'P-1-24000006-04-07-002'.
**    LS_DATA-VBELN_VF   = '5010000082'.
**    LS_DATA-POSNR_VF   = LV_ITEM.
**    LS_DATA-SEQNO = LV_INDEX + 1.
**    LS_DATA-NETWR = 10.
**    APPEND LS_DATA TO LT_DATA_TMP.
**    LV_ITEM = LV_ITEM + 1.
**  ENDLOOP.
**
**  LV_ITEM = 10.
**  LOOP AT GT_DATA INTO LS_DATA.
**    LS_DATA-EXT_POSID = 'P-1-24000006-04-07-002'.
**    LS_DATA-VBELN_VF   = '5010000083'.
**    LS_DATA-POSNR_VF   = LV_ITEM.
**    LS_DATA-SEQNO = LV_INDEX + 1.
**    LS_DATA-NETWR = 10.
**    APPEND LS_DATA TO LT_DATA_TMP.
**    LV_ITEM = LV_ITEM + 1.
**  ENDLOOP.
**
**  LV_ITEM = 10.
**  LOOP AT GT_DATA INTO LS_DATA.
**    LS_DATA-EXT_POSID = 'P-1-24000006-04-07-002'.
**    LS_DATA-VBELN_VF   = '5010000084'.
**    LS_DATA-POSNR_VF   = LV_ITEM.
**    LS_DATA-SEQNO = LV_INDEX + 1.
**    LS_DATA-NETWR = 10.
**    APPEND LS_DATA TO LT_DATA_TMP.
**    LV_ITEM = LV_ITEM + 1.
**  ENDLOOP.
**
**  LV_ITEM = 10.
**  LOOP AT GT_DATA INTO LS_DATA.
**    LS_DATA-EXT_POSID = 'P-1-24000006-04-07-002'.
**    LS_DATA-VBELN_VF   = '5010000085'.
**    LS_DATA-POSNR_VF   = LV_ITEM.
**    LS_DATA-SEQNO = LV_INDEX + 1.
**    LS_DATA-NETWR = 10.
**    APPEND LS_DATA TO LT_DATA_TMP.
**    LV_ITEM = LV_ITEM + 1.
**  ENDLOOP.
**
**  GT_DATA[] = LT_DATA_TMP[].
****** Insert Data For Test Split doc***********
*
*
** Initialize the line item counter and remaining line items
*  LF_COUNT = 1.
*  LF_REMAINING = LINES( CT_DATA ).
*
** Loop through the line items and split into batches
*  WHILE LF_REMAINING > 0.
*
*    LF_RUNNO = LF_RUNNO + 1.
*
** Determine the number of line items for this batch (either max or the remaining items)
*    LF_BATCH_SIZE = GF_MAX_RECORDS.
*    IF LF_REMAINING < GF_MAX_RECORDS.
*      LF_BATCH_SIZE = LF_REMAINING.
*    ENDIF.
*
** CLEAR THE SPLIT DOCUMENT TABLE FOR THIS BATCH
*    CLEAR LS_DATA_H.
*    LS_DATA_H-RUNNO = LF_RUNNO.
*    APPEND LS_DATA_H TO CT_DATA_H.
*
** SELECT THE LINE ITEMS FOR THE CURRENT BATCH
*    LOOP AT CT_DATA INTO LS_DATA
*                    FROM LF_COUNT TO LF_COUNT + LF_BATCH_SIZE.
*      LS_DATA-RUNNO = LF_RUNNO.
*      MODIFY CT_DATA FROM LS_DATA TRANSPORTING RUNNO.
*    ENDLOOP.
*
** Update counters for the next batch
*    LF_COUNT = LF_COUNT + LF_BATCH_SIZE.
*    LF_REMAINING = LF_REMAINING - LF_BATCH_SIZE.
*
*  ENDWHILE.
*
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_select_prps
**&---------------------------------------------------------------------*
*FORM F_SELECT_PRPS  USING    URT_WBS TYPE TT_RANGE_WBS
*                    CHANGING CT_PRPS TYPE TT_PRPS.
*
*  SELECT PSPNR,
*         POSID,
*         PRPS~OBJNR
*  FROM PRPS
*  WHERE POSID IN @URT_WBS
*  AND   POSID IN @S_WBS
*  AND   OBJNR IN ( SELECT OBJNR
*                   FROM JEST
*                   WHERE OBJNR = PRPS~OBJNR
*                   AND   STAT  = @GF_SYSTEM_STATUS
*                   AND   INACT = @SPACE )
*  INTO TABLE @CT_PRPS.
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CAL_TOTAL_DYAS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> UF_STARTDATE
*&      --> UF_ENDDATE
*&      <-- LV_DAYS
*&---------------------------------------------------------------------*
FORM F_CAL_TOTAL_DYAS  USING    UF_STARTDATE TYPE SY-DATUM
                                UF_ENDDATE   TYPE SY-DATUM
                       CHANGING CF_DAYS TYPE I.

  CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
    EXPORTING
      BEGDA = UF_STARTDATE
      ENDDA = UF_ENDDATE
    IMPORTING
      DAYS  = CF_DAYS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CAL_PERIOD_AMOUNT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DATA_SERV_CON_VAL
*&      --> LS_DATA_SVC_DAYS
*&      --> LS_DATA_AMTDAY
*&      <-- LS_DATA_NETWR
*&---------------------------------------------------------------------*
FORM F_CAL_PERIOD_AMOUNT  USING  UT_LOGS      TYPE TT_LOGS
                                 UF_SERV_CON_ID TYPE ZSDSFIS139-SERV_CON_ID
                                 UF_AMTDAY    TYPE ZSDSDE_AMTDAY
                          CHANGING CF_NETWR   TYPE NETWR
                                   CF_POSDAYS TYPE ZSDSDE_DAYS.
  DATA: LT_LOGS_TMP TYPE TT_LOGS.
  DATA: LV_DAYS      TYPE I,
        LV_STARTDATE TYPE SY-DATUM,
        LV_ENDDATE   TYPE SY-DATUM.

  LT_LOGS_TMP[] = UT_LOGS[].
  SORT LT_LOGS_TMP BY SERV_CON_ID SVC_ITEM SEQNO DESCENDING.

  READ TABLE LT_LOGS_TMP INTO DATA(LS_LOGS_TMP)
                     WITH KEY SERV_CON_ID = UF_SERV_CON_ID
                     BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    IF LS_LOGS_TMP-XTRUEREV IS INITIAL.
      IF LS_LOGS_TMP-BUDAT < GF_BUDAT.
        LV_STARTDATE = LS_LOGS_TMP-BUDAT + 1.
      ELSE.
        CLEAR LV_STARTDATE.
      ENDIF.
    ELSE.
      LV_STARTDATE = GF_DATE_BEG.
    ENDIF.
  ELSE.
    LV_STARTDATE = GF_DATE_BEG.
  ENDIF.

  IF LV_STARTDATE IS NOT INITIAL.
*   LV_STARTDATE = GF_DATE_BEG. "|{ GF_BUDAT(6) }| & |01|.  "UF_STARTDATE
    LV_ENDDATE   = GF_DATE_END. "GF_BUDAT.

    PERFORM F_CAL_TOTAL_DYAS USING LV_STARTDATE

                                   LV_ENDDATE
                             CHANGING LV_DAYS.
    CF_POSDAYS = LV_DAYS.
    CF_NETWR = UF_AMTDAY * LV_DAYS.
  ELSE.
    CLEAR:CF_POSDAYS, CF_NETWR.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CAL_REVENUE_AMOUNT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DATA_SERV_CON_VAL
*&      --> LS_DATA_SVC_DAYS
*&      --> LS_DATA_AMTDAY
*&      <-- LS_DATA_NETWR
*&---------------------------------------------------------------------*
FORM F_CAL_REVENUE_AMOUNT  USING   UF_EXT_WRT_BEG TYPE SY-DATUM
                                   UF_EXT_WRT_END TYPE SY-DATUM
                                   UF_AMTDAY    TYPE ZSDSDE_AMTDAY
                          CHANGING CF_NETWR TYPE NETWR
                                   CF_POSDAYS TYPE ZSDSDE_DAYS.
  DATA: LV_DAYS      TYPE I.

  IF UF_EXT_WRT_BEG = GF_DATE_BEG. "Date Beg(Warranty) = 01.xx.xxxx
    "Count from Beg of warranty to end of month
    PERFORM F_CAL_TOTAL_DYAS USING GF_DATE_BEG
                                   GF_DATE_END
                             CHANGING LV_DAYS.
  ELSEIF UF_EXT_WRT_BEG > GF_DATE_BEG."Date Beg(Warranty) > 01.xx.xxxx
    "Count from Beg of warranty to end of month
    PERFORM F_CAL_TOTAL_DYAS USING UF_EXT_WRT_BEG
                                   GF_DATE_END
                             CHANGING LV_DAYS.

  ELSEIF UF_EXT_WRT_BEG < GF_DATE_BEG."Date Beg(Warranty) < 01.xx.xxxx
    IF UF_EXT_WRT_END < GF_DATE_END. "Date End(Warranty) <  End of month
      "Count from Beg of month to end of Warranty
      PERFORM F_CAL_TOTAL_DYAS USING GF_DATE_BEG
                                     UF_EXT_WRT_END
                               CHANGING LV_DAYS.
    ELSE.                            "Date End(Warranty) >End of month
      "Count from Beg of month to end of period
      PERFORM F_CAL_TOTAL_DYAS USING GF_DATE_BEG
                                     GF_DATE_END
                               CHANGING LV_DAYS.
    ENDIF.
  ENDIF.

  IF LV_DAYS IS NOT INITIAL.
    CF_POSDAYS = LV_DAYS.
    CF_NETWR = UF_AMTDAY * LV_DAYS.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_FI_SD_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_WBS
*&      <-- LT_ACDOCA
*&      <-- LT_VBFA
*&---------------------------------------------------------------------*
FORM F_SELECT_ACDOCA  USING    UT_WBS TYPE TT_WBS
                               UT_BILLING TYPE TT_BILLING
                               UT_MATERIAL TYPE TT_MATERIAL
                           CHANGING CT_ACDOCA  TYPE TT_ACDOCA
                                    CT_REVENUE TYPE TT_REVENUE.
  DATA: LS_REVENUE TYPE TS_REVENUE.

  CLEAR: GRT_MATNR[].
  GRT_MATNR  = VALUE #( FOR <LS_MATERIAL> IN UT_MATERIAL
                                           ( LOW  = <LS_MATERIAL>-MATNR
                                             OPTION = 'EQ'
                                             SIGN   = 'I' ) ).

  GRT_VBELN  = VALUE #( FOR <LS_BILLING> IN UT_BILLING
                                       ( LOW  = <LS_BILLING>-VBELN
                                         OPTION = 'EQ'
                                         SIGN   = 'I' ) ).
  SORT GRT_VBELN.
  DELETE ADJACENT DUPLICATES FROM GRT_VBELN COMPARING ALL FIELDS.

  IF UT_WBS[] IS NOT INITIAL.

    SELECT A~RLDNR,                                     "#EC CI_NOFIELD
      A~RBUKRS,
      A~GJAHR,
      A~BELNR,
      A~DOCLN,
      A~BUDAT,
      A~HSL,
      A~RHCUR,
      A~RACCT,
      A~AWTYP,
      A~AWREF,
      A~AWITEM,
      A~PS_POSID,
      A~MATNR,
      A~AUFNR,
      B~VBELV,
      B~POSNV,
      B~VBELN,
      B~POSNN
    FROM ACDOCA AS A INNER JOIN VBFA AS B "#EC CI_NO_TRANSFORM                          "#EC CI_NO_TRANSFORM
      ON A~AWREF = B~VBELN
      AND A~AWITEM = B~POSNN
      FOR ALL ENTRIES IN @UT_WBS
      WHERE RLDNR  = @GC_CON-RLDNR "0L
*        AND RBUKRS = @P_BUKRS
        AND A~PS_POSID = @UT_WBS-POSID
        AND A~FISCYEARPER <= @GF_FISCYEARPER
        AND A~RACCT   = @GF_GL_ACC_DR_EXT "'2495000050'
        AND A~AWTYP   = @GC_CON-AWTYP "'VBRK'
        AND A~XTRUEREV = @SPACE
        AND A~AWREF IN @GRT_VBELN
        AND B~VBTYP_V = @GC_CON-VBTYP_V " 'J' "DO
        AND B~VBTYP_N = @GC_CON-VBTYP_N " 'M' "Invoice
      INTO TABLE @CT_ACDOCA.
    IF SY-SUBRC EQ 0.
      SORT CT_ACDOCA BY PS_POSID VBELV.
    ENDIF.
  ENDIF.
  LOOP AT CT_ACDOCA INTO DATA(LS_ACDOCA)  ##INTO_OK.
    CLEAR LS_REVENUE.
    LS_REVENUE-POSID  = LS_ACDOCA-PS_POSID.
    LS_REVENUE-MATNR  = LS_ACDOCA-MATNR.
    LS_REVENUE-VBELN_VL  = LS_ACDOCA-VBELV.
    LS_REVENUE-VBELN_VF  = LS_ACDOCA-VBELN.
    LS_REVENUE-POSNR_VF  = LS_ACDOCA-POSNN.
    LS_REVENUE-RHCUR  = LS_ACDOCA-RHCUR.
    LS_REVENUE-AMOUNT = LS_ACDOCA-HSL * -1.
    COLLECT LS_REVENUE INTO CT_REVENUE.

  ENDLOOP.

*  LT_ACDOCA_TMP = CT_ACDOCA[].
*  SORT LT_ACDOCA_TMP BY VBELN POSNN.
*  DELETE ADJACENT DUPLICATES FROM LT_ACDOCA_TMP COMPARING VBELN POSNN.
**  SORT LR_VBELN.
**  DELETE ADJACENT DUPLICATES FROM LR_VBELN COMPARING ALL FIELDS.
**  IF LR_VBELN[] IS NOT INITIAL.
*  IF LT_ACDOCA_TMP[] IS NOT INITIAL.
** GET BILLING DATA
*    SELECT A~VBELN,
*           B~POSNR,
*           A~BUPLA,
*           B~KOSTL,
*           B~PRCTR,
*           A~VKORG,
*           A~VTWEG
*      FROM VBRK AS A
*      INNER JOIN VBRP AS B
*      ON A~VBELN EQ B~VBELN
*      FOR ALL ENTRIES IN @LT_ACDOCA_TMP
*     WHERE A~VBELN = @LT_ACDOCA_TMP-VBELN
*      AND  B~POSNR = @LT_ACDOCA_TMP-POSNN
*      INTO TABLE @CT_BILLING.
*    IF SY-SUBRC EQ 0.
*      SORT CT_BILLING BY VBELN POSNR.
*    ENDIF.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PREPARE_REPORT_WBS_EXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_LOGS
*&      <-- GT_OUTPUT
*&---------------------------------------------------------------------*
FORM F_PREPARE_REPORT_WBS_EXT  USING    UT_LOGS TYPE TT_LOGS
                               CHANGING CT_OUTPUT TYPE TT_OUTPUT.
  DATA: LS_OUTPUT TYPE ZSDSFIS139.

  CLEAR CT_OUTPUT[].
  LOOP AT UT_LOGS INTO DATA(LS_LOGS) ##INTO_OK.
    MOVE-CORRESPONDING LS_LOGS TO LS_OUTPUT.

    IF LS_OUTPUT-MSGTY EQ GC_MSGTY-SUC.
      LS_OUTPUT-STATUS_ICON = GC_STATUS-SUCCESS.
    ELSE.
      LS_OUTPUT-STATUS_ICON = GC_STATUS-ERROR.
    ENDIF.

    APPEND LS_OUTPUT TO CT_OUTPUT.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_ACCOUNTING_DOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> T_LOGS_TMP
*&      <-- CT_BKPF
*&---------------------------------------------------------------------*
FORM F_GET_ACCOUNTING_DOC  USING    UT_LOGS   TYPE TT_LOGS
                           CHANGING CT_BKPF  TYPE TT_BKPF.
  DATA: LT_LOGS_TMP  TYPE STANDARD TABLE OF ZSDSFIT048.

  LT_LOGS_TMP[] = UT_LOGS[].
  DELETE LT_LOGS_TMP WHERE BELNR IS INITIAL.
  SORT LT_LOGS_TMP BY BUKRS BELNR GJAHR.
  DELETE ADJACENT DUPLICATES FROM LT_LOGS_TMP COMPARING BUKRS BELNR GJAHR.

  IF LT_LOGS_TMP[] IS NOT INITIAL.
    SELECT BUKRS,
           BELNR,
           GJAHR,
           BUDAT
    INTO TABLE @CT_BKPF
    FROM BKPF
    FOR ALL ENTRIES IN @LT_LOGS_TMP
    WHERE BUKRS EQ @LT_LOGS_TMP-BUKRS
      AND BELNR EQ @LT_LOGS_TMP-BELNR
      AND GJAHR EQ @LT_LOGS_TMP-GJAHR
      AND STBLG EQ @SPACE.
    IF SY-SUBRC EQ 0.
      SORT CT_BKPF BY BUKRS BELNR GJAHR.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_CONVERT_DATE_TIME_TO_TSTAMP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_DATE
*&      --> LV_TIME
*&      <-- LV_TSTAMP
*&---------------------------------------------------------------------*
FORM F_CONVERT_DATE_TIME_TO_TSTAMP  USING    UF_DATE TYPE DATS
                                             UF_TIME TYPE TIMS
                                    CHANGING CF_TSTAMP TYPE TZNTSTMPSL.
  CLEAR CF_TSTAMP.
  TRY.
      " Call the method to convert date and time to timestamp
      CL_CMS_COMMON=>CONVERT_DATE_TIME_TO_TSTAMP(
        EXPORTING
          IM_DATE   = UF_DATE
          IM_TIME   = UF_TIME
        IMPORTING
          EX_TIMESTAMP = CF_TSTAMP
      ).

    CATCH CX_ROOT INTO DATA(LX_ERROR) ##NO_HANDLER ##NEEDED ##CATCH_ALL.
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_LAST_DAY_OF_MONTHS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GF_PERIOD_BEG
*&      <-- GF_PERIOD_END
*&---------------------------------------------------------------------*
FORM F_GET_LAST_DAY_OF_MONTHS  USING    UF_DATE_BEG TYPE SY-DATUM
                               CHANGING CF_DATE_END TYPE SY-DATUM.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = UF_DATE_BEG
    IMPORTING
      LAST_DAY_OF_MONTH = CF_DATE_END
    EXCEPTIONS
      DAY_IN_NO_DATE    = 1
      OTHERS            = 2.
  IF SY-SUBRC NE 0.
    CLEAR CF_DATE_END.
  ENDIF.

ENDFORM.
**&---------------------------------------------------------------------*
**& Form F_GET_CURRENT_YEAR
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> GF_BUDAT
**&      <-- GF_MONAT
**&      <-- GF_GJAHR
**&---------------------------------------------------------------------*
*FORM F_GET_CURRENT_YEAR  USING    UF_BUDAT TYPE SY-DATUM
*                          CHANGING CF_MONAT TYPE BKPF-MONAT
*                                   CF_GJAHR TYPE BKPF-GJAHR.
*  "Get Period
*  CALL FUNCTION 'GET_CURRENT_YEAR'
*    EXPORTING
*      BUKRS = P_BUKRS
*      DATE  = UF_BUDAT
*    IMPORTING
*      CURRM = CF_MONAT.
*
*  "Get Fiscal Year
*  CALL FUNCTION 'GET_CURRENT_YEAR'
*    EXPORTING
*      BUKRS = P_BUKRS
*      DATE  = UF_BUDAT
*    IMPORTING
*      CURRY = CF_GJAHR.
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form F_CONVERSION_EXIT_ABPSP_OUTPUT
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> LS_BILLING_PS_PSP_PNR
**&      <-- LS_DATA_POSID
**&---------------------------------------------------------------------*
*FORM F_CONVERSION_EXIT_ABPSP_OUTPUT  USING    UF_PS_PSP_PNR TYPE PS_PSP_PNR
*                                     CHANGING CF_POSID TYPE PS_POSID.
*
*  CLEAR CF_POSID.
*  CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*    EXPORTING
*      INPUT  = UF_PS_PSP_PNR
*    IMPORTING
*      OUTPUT = CF_POSID.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILTER_SVO_DEL_STATUS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_CRM_JEST
*&---------------------------------------------------------------------*
FORM F_FILTER_SVO_DEL_STATUS  CHANGING CT_SERV_I   TYPE TT_SERV_I.
  DATA: LT_CRM_JEST   TYPE TABLE OF CRM_JEST.

  SORT GRT_ITEM_GUID.
  DELETE ADJACENT DUPLICATES FROM GRT_ITEM_GUID COMPARING ALL FIELDS.

  IF GRT_ITEM_GUID[] IS NOT INITIAL.

    SELECT MANDT,
           OBJNR,
           STAT,
           INACT,
           CHGNR
      INTO TABLE @LT_CRM_JEST
      FROM CRM_JEST
      WHERE OBJNR IN @GRT_ITEM_GUID
        AND STAT = @GC_CON-STAT_DEL
        AND INACT = @SPACE.
    IF SY-SUBRC EQ 0.
      SORT LT_CRM_JEST BY OBJNR.
      LOOP AT CT_SERV_I INTO DATA(LS_SERV_I)  ##INTO_OK.
        DATA(LV_INDEX) = SY-TABIX.
        READ TABLE LT_CRM_JEST TRANSPORTING NO FIELDS
                               WITH KEY OBJNR = LS_SERV_I-ITEM_GUID
                               BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          DELETE CT_SERV_I INDEX LV_INDEX.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_CURRENT_YEAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GF_BUDAT
*&      <-- GF_MONAT
*&      <-- GF_GJAHR
*&---------------------------------------------------------------------*
FORM F_GET_CURRENT_YEAR  USING    UF_BUDAT TYPE SY-DATUM
                          CHANGING CF_MONAT TYPE BKPF-MONAT
                                   CF_GJAHR TYPE BKPF-GJAHR.
  "Get Period
  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      BUKRS = '1000'
      DATE  = UF_BUDAT
    IMPORTING
      CURRM = CF_MONAT.

  "Get Fiscal Year
  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      BUKRS = '1000'
      DATE  = UF_BUDAT
    IMPORTING
      CURRY = CF_GJAHR.
ENDFORM.

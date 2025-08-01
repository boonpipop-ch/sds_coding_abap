*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0320_F01
*  Creation Date      : 05.06.2024
*  Author             : Thanapong C. (Eviden)
*  Add-on ID          : FSCMR013
*  Description        : This is include program routine
*  Purpose            : This is include program routine
*  Purpose            :
*  Copied from        : N/A
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

*----------------------------------------------------------------------*
*  Form F_INIT_VARIABLE
*----------------------------------------------------------------------*
*  Initialize Global Variables
*----------------------------------------------------------------------*
FORM F_INIT_VARIABLE.

  CLEAR: GT_EXCL,
         GT_FIELDCAT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_get_data
*----------------------------------------------------------------------*
*  Get Data
*----------------------------------------------------------------------*
FORM F_GET_DATA  CHANGING CT_RESULT TYPE TT_RESULT.

  DATA: LF_SEL_CREDIT TYPE CHAR01.
  DATA: LT_WBSST TYPE RANGE OF ZSDSDE_WBS_CRDT_STATUS.      "+420000174

* Initialize Output
  CLEAR: CT_RESULT.

  DATA(LS_SEL_CREDIT) = ZCL_SDSFI_CUSTOMER_CREDIT=>GC_SEL_CREDIT.

  CASE GC_TRUE.
    WHEN P_LYTCUS.
      LF_SEL_CREDIT =  LS_SEL_CREDIT-CUSTOMER.
    WHEN P_LYTWBS.
      LF_SEL_CREDIT = LS_SEL_CREDIT-PROJECT.
    WHEN P_LYTALL.
      LF_SEL_CREDIT = LS_SEL_CREDIT-ALL.
  ENDCASE.

*<-- Start of Insertion 420000174 21.01.2025 (Assign WBS status)
  CLEAR: LT_WBSST.
  CASE GC_TRUE.
    WHEN RB_OPEN.
      INSERT VALUE #( SIGN = 'I'
                      OPTION = 'EQ'
                      LOW = ZCL_SDSFI_CUSTOMER_CREDIT=>GC_STATUS_OPEN )
                     INTO TABLE LT_WBSST.
    WHEN RB_CLOSE.
      INSERT VALUE #( SIGN = 'I'
                      OPTION = 'EQ'
                      LOW = ZCL_SDSFI_CUSTOMER_CREDIT=>GC_STATUS_CLOSE )
                     INTO TABLE LT_WBSST.
    WHEN RB_ALL.
  ENDCASE.
*--> End of Insertion 420000174 21.01.2025

  ZCL_SDSFI_CUSTOMER_CREDIT=>GET_DATA(
    EXPORTING
      IT_PARTNER      = S_BP[]           " Table Type EDM_PARTN_RANGE
      IT_WBS          = S_WBS[]          " Selection Options for POSID (WBS Element)
      IF_CREDIT_SGMNT = P_CSGMT          " SAP Credit Management: Ranges Table for Credit Segments
      IT_CREDIT_GROUP = S_CGRP[]         " Customer Credit Group Table Type Range
      IT_RISK_CLASS   = S_RLCLS[]        " Range type for UKM_RISK_CLASS
      IT_CREDIT_LIMIT = S_CLT[]
      IT_BUSAB        = S_BUSAB[]
      IT_WBSSTAT      = LT_WBSST[]       "+420000174
      IF_ALL_CUSTOMER = P_ALL            " All Customer
      IF_SEL_CREDIT   = LF_SEL_CREDIT    " Selection (C: Customer Credit, P: Project Credit, *: All )
*     IF_LANGU        = '2'              " ABAP System Field: Language Key of Text Environment
    IMPORTING
      ET_RESULT       = DATA(LT_RESULT)
  ).

  CT_RESULT = CORRESPONDING #( LT_RESULT ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_DISPLAY_RESULT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_OUTPUT
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

* Build Field catalog from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE
                              CHANGING CT_FIELDCAT.
* Change Field catalog
  PERFORM F_CHANGE_FIELDCAT USING SPACE
                         CHANGING CT_FIELDCAT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CHANGE_FIELDCAT
*----------------------------------------------------------------------*
*  Change Field Catalog
*----------------------------------------------------------------------*
FORM F_CHANGE_FIELDCAT USING UF_XLS_X     TYPE FLAG
                    CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

  CONSTANTS:
    LC_ALIGN_LEFT   TYPE LVC_JUST VALUE 'L',
    LC_ALIGN_CENTER TYPE LVC_JUST VALUE 'C'.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.

  DATA: LF_TEXT TYPE TEXT30.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.
    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN GC_FNAME-PARTNER      OR
           GC_FNAME-PARTNER_NAME OR
           GC_FNAME-GJAHR        OR
           GC_FNAME-BUKRS        OR
           GC_FNAME-DOC_TYPE     OR
           GC_FNAME-DOC_NO       OR
           GC_FNAME-PSPID        OR
           GC_FNAME-PSPNR.
        <L_FIELDCAT>-TECH = GC_TRUE.
      WHEN GC_FNAME-UTILIZATION_PERCENT.
        <L_FIELDCAT>-NO_ZERO = GC_TRUE.
      WHEN GC_FNAME-XBLOCKED.
        <L_FIELDCAT>-JUST = LC_ALIGN_CENTER.
      WHEN GC_FNAME-DOC_DATE.
*Text-C01:  Document Date
        LF_TEXT                 = TEXT-C01.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
      WHEN GC_FNAME-REF_DOC.
*Text-C02:  Tax Invoice
        LF_TEXT                 = TEXT-C02.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
      WHEN GC_FNAME-PAYMENT_DATE.
*Text-C03:  Baseline Payment Date
        LF_TEXT                 = TEXT-C03.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
*Text-C09	Payment Date
        LF_TEXT                 = TEXT-C09.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
      WHEN GC_FNAME-DAY1.
*Text-C04:  Days
        LF_TEXT                 = TEXT-C04.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.
        <L_FIELDCAT>-JUST       = LC_ALIGN_LEFT.
      WHEN GC_FNAME-NET_DUEDATE.
*Text-C05:  Net Due Date
        LF_TEXT                 = TEXT-C05.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
      WHEN GC_FNAME-CREDIT_EXPOSURE.
*Text-C06:  Credit exposure
        LF_TEXT                 = TEXT-C06.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.
      WHEN GC_FNAME-AMOUNT.
*Text-C07:  Amount
        LF_TEXT                 = TEXT-C07.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.
      WHEN GC_FNAME-CREDIT_LIMIT.
*Text-C08:  Credit Limit
        LF_TEXT                 = TEXT-C08.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.

      WHEN GC_FNAME-TREE_DESC.

*Text-C10:  Partner/Customer&Project/DocType/DocNo
        LF_TEXT                 = TEXT-C10.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.

      WHEN GC_FNAME-REMAIN_CREDIT_LIMIT.

*Text-C11:  Remaining Credit Limit
        LF_TEXT                 = TEXT-C11.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
        <L_FIELDCAT>-NO_ZERO    = GC_TRUE.

      WHEN GC_FNAME-VKORG_TX.
*Text-C12:  Sales Org.
        LF_TEXT                 = TEXT-C12.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
      WHEN GC_FNAME-VTWEG_TX.
*Text-C13:  Distr. Channel
        LF_TEXT                 = TEXT-C13.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.

      WHEN GC_FNAME-SPART_TX.
*Text-C14:  Division
        LF_TEXT                 = TEXT-C14.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
      WHEN GC_FNAME-VKGRP_TX.
*Text-C15:  Sales Group
        LF_TEXT                 = TEXT-C15.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
      WHEN GC_FNAME-VKBUR_TX.
*Text-C16:  Sales Office
        LF_TEXT                 = TEXT-C16.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
      WHEN GC_FNAME-SALES_NAME.
*Text-C17:  Salesman
        LF_TEXT                 = TEXT-C17.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
      WHEN GC_FNAME-ACCT_CLERK_TX.
*Text-C18:  Accounting Clerk
        LF_TEXT                 = TEXT-C18.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
*<-- Start of Insertion 420000174 21.01.2025 (Add New Column)
      WHEN 'STATUS_TXT'.
*       Text-C19:  WBS Status
        LF_TEXT                 = TEXT-C19.
        <L_FIELDCAT>-COLTEXT    = LF_TEXT.
        <L_FIELDCAT>-SELTEXT    = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
      WHEN 'REBZG'.

      WHEN 'REBZJ'.
        <L_FIELDCAT>-NO_OUT = GC_TRUE.
*--> End of Insertion 420000174 21.01.2025
    ENDCASE.
    "Field Catalog for Excel file
    IF UF_XLS_X = GC_TRUE.
      CASE <L_FIELDCAT>-FIELDNAME.
        WHEN GC_FNAME_XLS-PROJECT.
*Text-T01: Permanent Credit / Project Code
          LF_TEXT                 = TEXT-T01.
          <L_FIELDCAT>-COLTEXT    = LF_TEXT.
          <L_FIELDCAT>-SELTEXT    = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
        WHEN GC_FNAME_XLS-PROJECT_TX.
*Text-T02: Description Credit/Project
          LF_TEXT                 = TEXT-T02.
          <L_FIELDCAT>-COLTEXT    = LF_TEXT.
          <L_FIELDCAT>-SELTEXT    = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
        WHEN GC_FNAME_XLS-DOC_TYPE_TX.
*Text-T03: Document Type
          LF_TEXT                 = TEXT-T03.
          <L_FIELDCAT>-COLTEXT    = LF_TEXT.
          <L_FIELDCAT>-SELTEXT    = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
        WHEN GC_FNAME_XLS-PARTNER.
          <L_FIELDCAT>-TECH = SPACE.
*Text-T04: Customer
          LF_TEXT                 = TEXT-T04.
          <L_FIELDCAT>-COLTEXT    = LF_TEXT.
          <L_FIELDCAT>-SELTEXT    = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
        WHEN GC_FNAME_XLS-PARTNER_NAME.
          <L_FIELDCAT>-TECH = SPACE.
*Text-T05: Customer Name
          LF_TEXT                 = TEXT-T05.
          <L_FIELDCAT>-COLTEXT    = LF_TEXT.
          <L_FIELDCAT>-SELTEXT    = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_S  = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_M  = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_L  = LF_TEXT.
        WHEN GC_FNAME_XLS-DOC_NO.
          <L_FIELDCAT>-TECH       = SPACE.
      ENDCASE.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CREATE_HIERARCHY
*&---------------------------------------------------------------------*
*& Create Hierarchy
*&---------------------------------------------------------------------*
FORM F_CREATE_HIERARCHY CHANGING CT_RESULT TYPE TT_RESULT.

  DATA:
    LF_DOC_TYPE        TYPE TS_DOC_DETAIL-DOC_TYPE,
    LF_TABIX           TYPE SY-TABIX,
    LF_CUST_KEY        TYPE LVC_NKEY,
    LF_PROJ_KEY        TYPE LVC_NKEY,
    LF_1ST_KEY         TYPE LVC_NKEY,
    LF_DOCTYPE_KEY     TYPE LVC_NKEY,
    LF_LAST_KEY        TYPE LVC_NKEY,

    LT_EXPAND_NODE_KEY TYPE LVC_T_NKEY.

  LOOP AT CT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>).

    CLEAR: LF_CUST_KEY,
           LF_PROJ_KEY,
           LF_DOCTYPE_KEY,
           LF_LAST_KEY.

    PERFORM F_ADD_CUSTOMER_LINE USING <L_RESULT>
                             CHANGING LF_CUST_KEY.

    IF LF_1ST_KEY IS INITIAL.
      LF_1ST_KEY = LF_CUST_KEY.
    ENDIF.

    APPEND LF_CUST_KEY TO LT_EXPAND_NODE_KEY.

    LOOP AT <L_RESULT>-SUMMARY_PROJ ASSIGNING FIELD-SYMBOL(<L_SUMMARY_PROJ>).

      PERFORM F_ADD_PROJ_LINE USING <L_SUMMARY_PROJ>
                                    LF_CUST_KEY
                           CHANGING LF_PROJ_KEY.

      READ TABLE <L_RESULT>-DOC_DETAIL TRANSPORTING NO FIELDS
      WITH KEY PSPID = <L_SUMMARY_PROJ>-PSPID
               BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.

        LF_TABIX = SY-TABIX.

        LF_DOC_TYPE = 'DUMMY'.

        LOOP AT <L_RESULT>-DOC_DETAIL ASSIGNING FIELD-SYMBOL(<L_DOC_DETAIL>) FROM LF_TABIX.

          IF <L_DOC_DETAIL>-PSPID <> <L_SUMMARY_PROJ>-PSPID.
            EXIT.
          ENDIF.

          IF <L_DOC_DETAIL>-DOC_TYPE <> LF_DOC_TYPE.

            LF_DOC_TYPE = <L_DOC_DETAIL>-DOC_TYPE.

            READ TABLE <L_RESULT>-SUMMARY_DOC_TYPE
            ASSIGNING FIELD-SYMBOL(<L_SUMMARY_DOC_TYPE>)
            WITH KEY PSPID    = <L_DOC_DETAIL>-PSPID
                     DOC_TYPE = <L_DOC_DETAIL>-DOC_TYPE
            BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              PERFORM F_ADD_DOC_TYPE_LINE USING <L_SUMMARY_DOC_TYPE>
                                                LF_PROJ_KEY
                                       CHANGING LF_DOCTYPE_KEY.

            ENDIF.

          ENDIF.

          PERFORM F_ADD_DOC_LINE USING <L_DOC_DETAIL>
                                       LF_DOCTYPE_KEY
                              CHANGING LF_LAST_KEY.
        ENDLOOP.

      ENDIF.

    ENDLOOP.

  ENDLOOP.

  IF LT_EXPAND_NODE_KEY IS NOT INITIAL.

    GREF_TREE->EXPAND_NODES(
      EXPORTING
        IT_NODE_KEY             = LT_EXPAND_NODE_KEY   " Node Key
      EXCEPTIONS
        FAILED                  = 1                " General Error
        CNTL_SYSTEM_ERROR       = 2                "
        ERROR_IN_NODE_KEY_TABLE = 3                " Node Table Contains Errors
        DP_ERROR                = 4                " Error in Data Provider
        NODE_NOT_FOUND          = 5                " node_not_found
        OTHERS                  = 6
    ).
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO DISPLAY LIKE 'E'
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      RETURN.
    ENDIF.

  ENDIF.

  IF LF_1ST_KEY IS NOT INITIAL.
    GREF_TREE->SET_TOP_NODE(
      EXPORTING
        I_NODE_KEY        = LF_1ST_KEY       " Node Key to be Selected
      EXCEPTIONS
        CNTL_SYSTEM_ERROR = 1                " "
        NODE_NOT_FOUND    = 2                " Error in Data Provider
        FAILED            = 3                " General Error
        OTHERS            = 4
    ).

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO DISPLAY LIKE 'E'
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      RETURN.
    ENDIF.
  ENDIF.

  CALL METHOD GREF_TREE->FRONTEND_UPDATE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ADD_CUSTOMER_LINE
*&---------------------------------------------------------------------*
*       Add hierarchy-level 1 to tree (Customer)
*----------------------------------------------------------------------*
FORM F_ADD_CUSTOMER_LINE USING US_RESULT   TYPE TS_RESULT
                     CHANGING  CF_NODE_KEY TYPE LVC_NKEY.

  DATA:
    LF_RELAT_KEY TYPE LVC_NKEY,
    LF_NODE_TEXT TYPE LVC_VALUE,
    LS_OUTPUT    TYPE TS_OUTPUT,
    LS_NODE      TYPE LVC_S_LAYN.

* set item-layout
  DATA:
    LT_ITEM_LAYOUT TYPE LVC_T_LAYI.

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GREF_TREE->C_HIERARCHY_COLUMN_NAME
                           STYLE     = CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL )
            TO LT_ITEM_LAYOUT.

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GC_FNAME-CREDIT_LIMIT
                           STYLE     = CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL )
            TO LT_ITEM_LAYOUT.

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GC_FNAME-CREDIT_EXPOSURE
                           STYLE     = CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL )
            TO LT_ITEM_LAYOUT.

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GC_FNAME-UTILIZATION_PERCENT
                           STYLE     = CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL )
            TO LT_ITEM_LAYOUT.

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GC_FNAME-REMAIN_CREDIT_LIMIT
                           STYLE     = CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL )
            TO LT_ITEM_LAYOUT.

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GC_FNAME-ACCT_CLERK_TX
                           STYLE     = CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL )
            TO LT_ITEM_LAYOUT.

  LS_OUTPUT = CORRESPONDING #( US_RESULT EXCEPT CREDIT_SGMNT
                                                XBLOCKED
                                                RISK_CLASS_TXT ).

* Add node text
  LF_NODE_TEXT =  |{ US_RESULT-PARTNER ALPHA = OUT } - { US_RESULT-PARTNER_NAME }|.
  CONDENSE LF_NODE_TEXT.

  LS_NODE-N_IMAGE   = SPACE.
  LS_NODE-EXP_IMAGE = SPACE.

  CALL METHOD GREF_TREE->ADD_NODE
    EXPORTING
      I_RELAT_NODE_KEY = LF_RELAT_KEY
      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      I_NODE_TEXT      = LF_NODE_TEXT
      IS_OUTTAB_LINE   = LS_OUTPUT
      IS_NODE_LAYOUT   = LS_NODE
      IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
    IMPORTING
      E_NEW_NODE_KEY   = CF_NODE_KEY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ADD_PROJ_LINE
*&---------------------------------------------------------------------*
*       Add hierarchy-level 2 to tree (Project)
*----------------------------------------------------------------------*
FORM F_ADD_PROJ_LINE USING US_SUMMARY_PROJ  TYPE TS_SUMMARY_PROJ
                           UF_RELAT_KEY     TYPE LVC_NKEY
                  CHANGING CF_NODE_KEY      TYPE LVC_NKEY.

  DATA:
    LF_NODE_TEXT   TYPE LVC_VALUE,
    LS_OUTPUT      TYPE TS_OUTPUT,
    LS_NODE        TYPE LVC_S_LAYN,
    LT_ITEM_LAYOUT TYPE LVC_T_LAYI.

  LS_OUTPUT = CORRESPONDING #( US_SUMMARY_PROJ ).

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GREF_TREE->C_HIERARCHY_COLUMN_NAME
                           STYLE     = CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL )
            TO LT_ITEM_LAYOUT.

* Add node text
  LF_NODE_TEXT =  US_SUMMARY_PROJ-DESCRIPTION.

  IF US_SUMMARY_PROJ-PSPID IS NOT INITIAL.
    LF_NODE_TEXT = |{ LF_NODE_TEXT } ({ US_SUMMARY_PROJ-PROJECT_TX })|.
  ENDIF.

  LS_NODE-N_IMAGE   = ICON_CLOSED_FOLDER.
  LS_NODE-EXP_IMAGE = ICON_OPEN_FOLDER.

  CALL METHOD GREF_TREE->ADD_NODE
    EXPORTING
      I_RELAT_NODE_KEY = UF_RELAT_KEY
      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      I_NODE_TEXT      = LF_NODE_TEXT
      IS_OUTTAB_LINE   = LS_OUTPUT
      IS_NODE_LAYOUT   = LS_NODE
      IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
    IMPORTING
      E_NEW_NODE_KEY   = CF_NODE_KEY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ADD_DOC_TYPE_LINE
*&---------------------------------------------------------------------*
*       Add hierarchy-level 3 to tree (Document Type)
*----------------------------------------------------------------------*
FORM F_ADD_DOC_TYPE_LINE USING US_SUMMARY_DOC_TYPE TYPE TS_SUMMARY_DOC_TYPE
                               UF_RELAT_KEY        TYPE LVC_NKEY
                      CHANGING CF_NODE_KEY         TYPE LVC_NKEY.

  DATA:
    LF_NODE_TEXT TYPE LVC_VALUE,
    LS_OUTPUT    TYPE TS_OUTPUT,
    LS_NODE      TYPE LVC_S_LAYN.

* Set item-layout
  DATA: LT_ITEM_LAYOUT TYPE LVC_T_LAYI.

  LS_OUTPUT = CORRESPONDING #( US_SUMMARY_DOC_TYPE ).

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GREF_TREE->C_HIERARCHY_COLUMN_NAME
                           STYLE     = CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL )
            TO LT_ITEM_LAYOUT.

* Add node text
  LF_NODE_TEXT =  US_SUMMARY_DOC_TYPE-DESCRIPTION.

  LS_NODE-N_IMAGE   = SPACE.
  LS_NODE-EXP_IMAGE = SPACE.

  CALL METHOD GREF_TREE->ADD_NODE
    EXPORTING
      I_RELAT_NODE_KEY = UF_RELAT_KEY
      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      I_NODE_TEXT      = LF_NODE_TEXT
      IS_OUTTAB_LINE   = LS_OUTPUT
      IS_NODE_LAYOUT   = LS_NODE
      IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
    IMPORTING
      E_NEW_NODE_KEY   = CF_NODE_KEY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ADD_DOC_LINE
*&---------------------------------------------------------------------*
*       Add hierarchy-level 4 to tree (Document Detail)
*----------------------------------------------------------------------*
FORM F_ADD_DOC_LINE USING  US_DOC_DETAIL  TYPE TS_DOC_DETAIL
                           UF_RELAT_KEY   TYPE LVC_NKEY
                  CHANGING CF_NODE_KEY    TYPE LVC_NKEY.

  STATICS:
   LF_TOGGLE TYPE ABAP_BOOL.

  DATA:
    LF_NODE_TEXT TYPE LVC_VALUE,
    LS_OUTPUT    TYPE TS_OUTPUT,
    LS_NODE      TYPE LVC_S_LAYN.

* Set item-layout
  DATA: LT_ITEM_LAYOUT TYPE LVC_T_LAYI.

  LS_OUTPUT = CORRESPONDING #( US_DOC_DETAIL ).

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GREF_TREE->C_HIERARCHY_COLUMN_NAME
                           STYLE     = CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL
                           CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK )
            TO LT_ITEM_LAYOUT.

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GC_FNAME-DOC_NO
                           CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK )
            TO LT_ITEM_LAYOUT.

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GC_FNAME-XBLOCKED
                           STYLE     = CL_GUI_COLUMN_TREE=>STYLE_INTENSIFIED
                           CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT )
            TO LT_ITEM_LAYOUT.

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GC_FNAME-AMOUNT
                           STYLE     = CL_GUI_COLUMN_TREE=>STYLE_EMPHASIZED_A
                           CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT )
            TO LT_ITEM_LAYOUT.

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GC_FNAME-VKORG_TX
                           STYLE     = CL_GUI_COLUMN_TREE=>STYLE_EMPHASIZED_A
                           CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT )
            TO LT_ITEM_LAYOUT.

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GC_FNAME-SPART_TX
                           STYLE     = CL_GUI_COLUMN_TREE=>STYLE_EMPHASIZED_A
                           CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT )
            TO LT_ITEM_LAYOUT.

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GC_FNAME-VKBUR_TX
                           STYLE     = CL_GUI_COLUMN_TREE=>STYLE_EMPHASIZED_A
                           CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT )
            TO LT_ITEM_LAYOUT.

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GC_FNAME-DOC_DATE
                           STYLE     = CL_GUI_COLUMN_TREE=>STYLE_EMPHASIZED_A
                           CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT )
            TO LT_ITEM_LAYOUT.

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GC_FNAME-PAYMENT_DATE
                           STYLE     = CL_GUI_COLUMN_TREE=>STYLE_EMPHASIZED_A
                           CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT )
            TO LT_ITEM_LAYOUT.

  APPEND VALUE LVC_S_LAYI( FIELDNAME = GC_FNAME-DAY1
                           STYLE     = CL_GUI_COLUMN_TREE=>STYLE_EMPHASIZED_A
                           CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT )
            TO LT_ITEM_LAYOUT.

* Add node text
  LF_NODE_TEXT =  |{ US_DOC_DETAIL-DOC_NO ALPHA = OUT }|.

  CONDENSE LF_NODE_TEXT.

  LS_NODE-N_IMAGE   = SPACE.
  LS_NODE-EXP_IMAGE = SPACE.

  IF LF_TOGGLE IS INITIAL.
    LF_TOGGLE = GC_TRUE.
  ELSE.
    CLEAR LF_TOGGLE.
  ENDIF.

  IF LF_TOGGLE EQ GC_TRUE.
    LS_NODE-STYLE     = CL_GUI_COLUMN_TREE=>STYLE_EMPHASIZED.
  ENDIF.

  CALL METHOD GREF_TREE->ADD_NODE
    EXPORTING
      I_RELAT_NODE_KEY = UF_RELAT_KEY
      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      I_NODE_TEXT      = LF_NODE_TEXT
      IS_OUTTAB_LINE   = LS_OUTPUT
      IS_NODE_LAYOUT   = LS_NODE
      IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
    IMPORTING
      E_NEW_NODE_KEY   = CF_NODE_KEY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_LINK_CLICK
*&---------------------------------------------------------------------*
FORM F_LINK_CLICK USING UF_FIELDNAME   TYPE LVC_FNAME
                        UF_NODE_KEY    TYPE LVC_NKEY ##CALLED.

  DATA LS_OUTPUT TYPE TS_OUTPUT.

  GREF_TREE->GET_OUTTAB_LINE(
    EXPORTING
      I_NODE_KEY     = UF_NODE_KEY                " Node Key
    IMPORTING
      E_OUTTAB_LINE  = LS_OUTPUT                   " Line of Outtab
*     e_node_text    =                  " node text
*     et_item_layout =                  " Layout structure for items of the ALV tree control
*     es_node_layout =                  " Node Layout of ALV Tree Control
    EXCEPTIONS
      NODE_NOT_FOUND = 1                " Node does not exist
      OTHERS         = 2
  ).
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  CASE UF_FIELDNAME.
    WHEN GREF_TREE->C_HIERARCHY_COLUMN_NAME.
      IF LS_OUTPUT-DOC_NO IS NOT INITIAL.

        IF LS_OUTPUT-DOC_TYPE = ZCL_SDSFI_CUSTOMER_CREDIT=>GC_DOC_TYPE-SALEORDER OR
           LS_OUTPUT-DOC_TYPE = ZCL_SDSFI_CUSTOMER_CREDIT=>GC_DOC_TYPE-SALEORDER_PROJ.
          SET PARAMETER ID 'AUN' FIELD LS_OUTPUT-DOC_NO.

          TRY.
              CALL TRANSACTION 'VA03' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
            CATCH CX_SY_AUTHORIZATION_ERROR.
              MESSAGE S172(00) WITH 'VA03'.
          ENDTRY.
        ELSE.
          SET PARAMETER ID: 'BLN' FIELD LS_OUTPUT-DOC_NO,
                            'BUK' FIELD LS_OUTPUT-BUKRS,
                            'GJR' FIELD LS_OUTPUT-GJAHR.
          TRY.
              CALL TRANSACTION 'FB03' WITH AUTHORITY-CHECK  AND SKIP FIRST SCREEN.
            CATCH CX_SY_AUTHORIZATION_ERROR.
              MESSAGE S172(00) WITH 'FB03'.
          ENDTRY.
        ENDIF.

      ENDIF.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  exit_program
*&---------------------------------------------------------------------*
*       free object and leave program
*----------------------------------------------------------------------*
FORM F_EXIT_PROGRAM.

  IF GREF_TREE IS BOUND.
    CALL METHOD GREF_TREE->FREE.
  ENDIF.

  LEAVE PROGRAM.

ENDFORM.                               " exit_program

*&---------------------------------------------------------------------*
*& Form f_VALIDATE_PROJECT
*&---------------------------------------------------------------------*
*       Validate Project
*----------------------------------------------------------------------*
FORM F_VALIDATE_PROJECT  USING UT_WBS_RANGE TYPE TT_PSPHI_RANGE.

  DATA: LF_INVALID TYPE FLAG,
        LF_MSGTX   TYPE STRING.

  LOOP AT UT_WBS_RANGE ASSIGNING FIELD-SYMBOL(<L_WBS>).

    IF <L_WBS>-LOW IS NOT INITIAL.
      ZCL_SDSFI_CUSTOMER_CREDIT=>VALIDATE_PROJECT(
        EXPORTING
          IF_PSPNR   = <L_WBS>-LOW                 " Project (internal)
        IMPORTING
          EF_INVALID = LF_INVALID                  " General Flag
          EF_MSGTX   = LF_MSGTX
      ).
      IF LF_INVALID EQ GC_TRUE.
        EXIT.
      ENDIF.
    ENDIF.

    IF <L_WBS>-HIGH IS NOT INITIAL.
      ZCL_SDSFI_CUSTOMER_CREDIT=>VALIDATE_PROJECT(
        EXPORTING
          IF_PSPNR   = <L_WBS>-HIGH                " Project (internal)
        IMPORTING
          EF_INVALID = LF_INVALID                  " General Flag
          EF_MSGTX   = LF_MSGTX
      ).

      IF LF_INVALID EQ GC_TRUE.
        EXIT.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF LF_INVALID EQ GC_TRUE.
    MESSAGE E000(ZSDSCA01) WITH LF_MSGTX.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_ALV_FUNCTION
*&---------------------------------------------------------------------*
*& ALV Function
*&---------------------------------------------------------------------*
FORM F_ALV_FUNCTION USING UF_FCODE TYPE UI_FUNC ##CALLED.

  CASE UF_FCODE.
    WHEN GC_FCODE-DOWNLOAD.
      PERFORM F_DOWNLOAD_XLSX USING GT_RESULT
                           CHANGING GT_XLS.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_download_xlsx
*&---------------------------------------------------------------------*
*& Download excel
*&---------------------------------------------------------------------*
FORM F_DOWNLOAD_XLSX USING CT_RESULT TYPE TT_RESULT
                  CHANGING CT_XLS    TYPE TT_XLS.

  IF CT_XLS IS INITIAL.
    PERFORM F_PREPARE_EXCEL USING CT_RESULT
                         CHANGING CT_XLS.
  ENDIF.

  PERFORM F_EXPORT_EXCEL USING CT_XLS.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_EXCEL
*&---------------------------------------------------------------------*
*  Prepare Excel File
*----------------------------------------------------------------------*
FORM F_PREPARE_EXCEL USING CT_RESULT TYPE TT_RESULT
                  CHANGING CT_XLS    TYPE TT_XLS.

  DATA:
    LF_DOC_TYPE TYPE TS_OUTPUT-DOC_TYPE,
    LF_TABIX    TYPE SY-TABIX,
    LS_XLS      TYPE TS_XLS,
    LS_XLS_PROJ TYPE TS_XLS.

  LOOP AT CT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>).

    CLEAR LS_XLS.

    LS_XLS-PARTNER          = <L_RESULT>-PARTNER.
    LS_XLS-PARTNER_NAME     = <L_RESULT>-PARTNER_NAME.
    LS_XLS-ACCT_CLERK_TX    = <L_RESULT>-ACCT_CLERK_TX.
    LS_XLS-RISK_CLASS_TXT   = <L_RESULT>-RISK_CLASS_TXT.
    LS_XLS-XBLOCKED         = <L_RESULT>-XBLOCKED.
    LS_XLS-CREDIT_SGMNT     = <L_RESULT>-CREDIT_SGMNT.

    "Customer
    APPEND INITIAL LINE TO CT_XLS ASSIGNING FIELD-SYMBOL(<L_XLS>).
    <L_XLS> = CORRESPONDING #( LS_XLS ).

    "Text-T06: Grand Total
    <L_XLS>-PROJECT             = TEXT-T06.
    <L_XLS>-CREDIT_LIMIT        = <L_RESULT>-CREDIT_LIMIT.
    <L_XLS>-CREDIT_EXPOSURE     = <L_RESULT>-CREDIT_EXPOSURE.
    <L_XLS>-UTILIZATION_PERCENT = <L_RESULT>-UTILIZATION_PERCENT.
    <L_XLS>-REMAIN_CREDIT_LIMIT = <L_RESULT>-REMAIN_CREDIT_LIMIT.

    LOOP AT <L_RESULT>-SUMMARY_PROJ ASSIGNING FIELD-SYMBOL(<L_SUMMARY_PROJ>).

      CLEAR: LS_XLS_PROJ.

      LS_XLS_PROJ            = CORRESPONDING #( LS_XLS ).
      LS_XLS_PROJ-PROJECT    = <L_SUMMARY_PROJ>-DESCRIPTION.
      LS_XLS_PROJ-PROJECT_TX = <L_SUMMARY_PROJ>-PROJECT_TX.

      "Project
      APPEND INITIAL LINE TO CT_XLS ASSIGNING <L_XLS>.
      <L_XLS>                     = CORRESPONDING #( LS_XLS_PROJ ).
      <L_XLS>-CREDIT_LIMIT        = <L_SUMMARY_PROJ>-CREDIT_LIMIT.
      <L_XLS>-CREDIT_EXPOSURE     = <L_SUMMARY_PROJ>-CREDIT_EXPOSURE.
      <L_XLS>-UTILIZATION_PERCENT = <L_SUMMARY_PROJ>-UTILIZATION_PERCENT.
      <L_XLS>-REMAIN_CREDIT_LIMIT = <L_SUMMARY_PROJ>-REMAIN_CREDIT_LIMIT.
      <L_XLS>-ZTERM               = <L_SUMMARY_PROJ>-ZTERM.
*<-- Start of Insertion 420000174 21.01.2025 (Add new Fields into Excel)
      <L_XLS>-STARTDATE           = <L_SUMMARY_PROJ>-STARTDATE.
      <L_XLS>-ENDDATE             = <L_SUMMARY_PROJ>-ENDDATE.
      <L_XLS>-STATUS_TXT          = <L_SUMMARY_PROJ>-STATUS_TXT.
*--> End of Insertion 420000174 21.01.2025

      "Document Detail
      DO 1 TIMES.

        READ TABLE <L_RESULT>-DOC_DETAIL TRANSPORTING NO FIELDS
        WITH KEY PSPID = <L_SUMMARY_PROJ>-PSPID
                 BINARY SEARCH.
        IF SY-SUBRC <> 0.
          EXIT.
        ENDIF.

        LF_TABIX = SY-TABIX.

        LF_DOC_TYPE = 'TEMP'.

        LOOP AT <L_RESULT>-DOC_DETAIL ASSIGNING FIELD-SYMBOL(<L_DOC_DETAIL>) FROM LF_TABIX.

          IF <L_DOC_DETAIL>-PSPID <> <L_SUMMARY_PROJ>-PSPID.
            EXIT.
          ENDIF.

          IF <L_DOC_DETAIL>-DOC_TYPE <> LF_DOC_TYPE.

            LF_DOC_TYPE = <L_DOC_DETAIL>-DOC_TYPE.

            READ TABLE <L_RESULT>-SUMMARY_DOC_TYPE
            ASSIGNING FIELD-SYMBOL(<L_SUMMARY_DOC_TYPE>)
            WITH KEY PSPID    = <L_DOC_DETAIL>-PSPID
                     DOC_TYPE = <L_DOC_DETAIL>-DOC_TYPE
                     BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              "Document Type
              APPEND INITIAL LINE TO CT_XLS ASSIGNING <L_XLS>.
              <L_XLS>                     = CORRESPONDING #( LS_XLS_PROJ ).
              <L_XLS>-DOC_TYPE_TX         = <L_SUMMARY_DOC_TYPE>-DESCRIPTION.
              <L_XLS>-AMOUNT              = <L_SUMMARY_DOC_TYPE>-AMOUNT.
            ENDIF.

          ENDIF.

          "Document Detail
          APPEND INITIAL LINE TO CT_XLS ASSIGNING <L_XLS>.
          <L_XLS>                = CORRESPONDING #( LS_XLS_PROJ ).
          <L_XLS>-DOC_TYPE_TX    = ZCL_SDSFI_CUSTOMER_CREDIT=>SET_DOC_TYPE_TEXT( <L_DOC_DETAIL>-DOC_TYPE ).
          <L_XLS>-DOC_NO         = <L_DOC_DETAIL>-DOC_NO.
          <L_XLS>-VKORG_TX       = <L_DOC_DETAIL>-VKORG_TX.
          <L_XLS>-VTWEG_TX       = <L_DOC_DETAIL>-VTWEG_TX.
          <L_XLS>-SPART_TX       = <L_DOC_DETAIL>-SPART_TX.
          <L_XLS>-VKGRP_TX       = <L_DOC_DETAIL>-VKGRP_TX.
          <L_XLS>-VKBUR_TX       = <L_DOC_DETAIL>-VKBUR_TX.
          <L_XLS>-SALES_NAME     = <L_DOC_DETAIL>-SALES_NAME.
          <L_XLS>-DOC_DATE       = <L_DOC_DETAIL>-DOC_DATE.
          <L_XLS>-REF_DOC        = <L_DOC_DETAIL>-REF_DOC.
          <L_XLS>-ZTERM          = <L_DOC_DETAIL>-ZTERM.
          <L_XLS>-DAY1           = <L_DOC_DETAIL>-DAY1.
          <L_XLS>-PAYMENT_DATE   = <L_DOC_DETAIL>-PAYMENT_DATE.
          <L_XLS>-NET_DUEDATE    = <L_DOC_DETAIL>-NET_DUEDATE.
          <L_XLS>-REBZG          = <L_DOC_DETAIL>-REBZG.    "+420000174
          <L_XLS>-AMOUNT         = <L_DOC_DETAIL>-AMOUNT.

        ENDLOOP.

      ENDDO.

    ENDLOOP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EXPORT_EXCEL
*&---------------------------------------------------------------------*
*& Export excel
*&---------------------------------------------------------------------*
FORM F_EXPORT_EXCEL  USING UT_XLS TYPE TT_XLS.

  DATA:
    LF_EXCEL_FILE   TYPE STRING,
    LF_FNAME        TYPE STRING,
    LF_WINDOW_TITLE TYPE STRING,
    LF_PATH         TYPE STRING,
    LF_FULLPATH     TYPE STRING,
    LREF_TABLE      TYPE REF TO CL_SALV_TABLE.

* Text-H01:  Export As
  LF_WINDOW_TITLE = TEXT-H01.

* Text-F02: .xlsx
  LF_EXCEL_FILE   = TEXT-F02.

  CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG(
    EXPORTING
      WINDOW_TITLE              = LF_WINDOW_TITLE
*     default_extension         =                  " Default Extension
*     default_file_name         =                  " Default File Name
*     with_encoding             =
      FILE_FILTER               = '*.xlsx'         " File Type Filter Table
*     initial_directory         =                  " Initial Directory
*     prompt_on_overwrite       = 'X'
    CHANGING
      FILENAME                  = LF_FNAME          " File Name to Save
      PATH                      = LF_PATH           " Path to File
      FULLPATH                  = LF_FULLPATH       " Path + File Name
*     user_action               =                  " User Action (C Class Const ACTION_OK, ACTION_OVERWRITE etc)
*     file_encoding             =
    EXCEPTIONS
      CNTL_ERROR                = 1                " Control error
      ERROR_NO_GUI              = 2                " No GUI available
      NOT_SUPPORTED_BY_GUI      = 3                " GUI does not support this
      INVALID_DEFAULT_FILE_NAME = 4                " Invalid default file name
      OTHERS                    = 5
  ).

  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  IF LF_FULLPATH IS INITIAL.
    RETURN.
  ENDIF.

  LF_FULLPATH = |{ LF_FULLPATH }{ LF_EXCEL_FILE }|.

  IF GT_FIELDCAT_XLS IS INITIAL.

    TRY.
        CL_SALV_TABLE=>FACTORY(
          IMPORTING
            R_SALV_TABLE = LREF_TABLE
          CHANGING
            T_TABLE      = UT_XLS ).

        GT_FIELDCAT_XLS = CL_SALV_CONTROLLER_METADATA=>GET_LVC_FIELDCATALOG(
          R_COLUMNS      = LREF_TABLE->GET_COLUMNS( )
          R_AGGREGATIONS = LREF_TABLE->GET_AGGREGATIONS( ) ).

        PERFORM F_CHANGE_FIELDCAT  USING GC_TRUE
                                CHANGING GT_FIELDCAT_XLS.

      CATCH CX_SALV_MSG.                                "#EC NO_HANDLER
*   Error occurred during generate Excel file.
        MESSAGE S004(ZSDSCA01) DISPLAY LIKE 'E'.
        RETURN.

    ENDTRY.

  ENDIF.

  PERFORM F_GENERATE_EXCEL USING UT_XLS
                                 GT_FIELDCAT_XLS
                                 LF_FULLPATH.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GENERATE_EXCEL
*----------------------------------------------------------------------*
*  Generate Excel File from internal table
*----------------------------------------------------------------------*
FORM F_GENERATE_EXCEL  USING  UT_XLS      TYPE TT_XLS
                              UT_FIELDCAT TYPE LVC_T_FCAT
                              UF_OFILE    TYPE STRING.

  DATA:
    LT_BIN      TYPE  SOLIX_TAB.

  DATA:
    LF_XSTRING TYPE  XSTRING,
    LF_SIZE    TYPE  I.


* Show progress
* Text-p98 : Generating Excel File . . .
  MC_SHOW_PROGRESS 99 TEXT-P98.

* Call Method Generate XLSX Xstring
  CLEAR LF_XSTRING.
  LF_XSTRING = ZCL_SDSCA_UTILITIES=>CREATE_XLSX_FROM_ITAB(
    EXPORTING
      IT_FIELDCAT = UT_FIELDCAT
      IT_DATA     = UT_XLS ).
  IF LF_XSTRING IS INITIAL.
*   Error occurred during generate Excel file.
    MESSAGE S004(ZSDSCA01) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

* Convert XString to Binary
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      BUFFER        = LF_XSTRING
    IMPORTING
      OUTPUT_LENGTH = LF_SIZE
    TABLES
      BINARY_TAB    = LT_BIN.

* Download to local file
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD
    EXPORTING
      BIN_FILESIZE            = LF_SIZE
      FILENAME                = UF_OFILE
      FILETYPE                = 'BIN'
    CHANGING
      DATA_TAB                = LT_BIN
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
      NOT_SUPPORTED_BY_GUI    = 22
      ERROR_NO_GUI            = 23
      OTHERS                  = 24.
  IF SY-SUBRC <> 0.
*   Error occurred during generate Excel file.
    MESSAGE S004(ZSDSCA01) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

* Show Success Message
* Export file generated successfully.
  MESSAGE S005(ZSDSCA01).

ENDFORM.

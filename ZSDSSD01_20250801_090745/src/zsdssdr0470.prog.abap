*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0470
*  Creation Date      : 23.01.2025
*  Author             : b.chiewsarikij
*  Add-on ID          : N/A
*  Description        : Return document report
*  Purpose            : N/A
*  Copied from        : ZR_SD_RETURN_REPORT (ECC)
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSSDR0470 NO STANDARD PAGE HEADING.
*&---------------------------------------------------------------------*
*& Report ZSDSSDR0470
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
* T A B L E S
*-----------------------------------------------------------------------
TABLES: VRPMA,
        VBRP,
        MARC,
        VBPA.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPE-POOLS: SLIS.

TYPES : BEGIN OF TYP_VRPMA ,
          VBELN LIKE VRPMA-VBELN,       "credit note no.
          POSNR LIKE VRPMA-POSNR,       "credit note item no.
          FKDAT LIKE VRPMA-FKDAT,        "credit note date
          VTWEG LIKE VRPMA-VTWEG ,     "distribution channel
          MATNR LIKE VRPMA-MATNR,       "material no.
          VTEXT TYPE TVTWT-VTEXT,
          KUNNR LIKE VRPMA-KUNNR,
        END OF TYP_VRPMA .

TYPES: BEGIN OF TYP_VBRP,
         VBELN      LIKE VBRP-VBELN,
         POSNR      LIKE VBRP-POSNR,
         MATNR      LIKE VBRP-MATNR,
         FKIMG      LIKE VBRP-FKIMG,
         NETWR      LIKE VBRP-NETWR,
         AUBEL      LIKE VBRP-AUBEL,
         AUPOS      LIKE VBRP-AUPOS,
         AUGRU_AUFT LIKE VBRP-AUGRU_AUFT,
         UEPOS      LIKE VBRP-UEPOS,
         LGORT      LIKE VBRP-LGORT,
       END OF TYP_VBRP.

TYPES: BEGIN OF TYP_OUT,
         VBELN1           LIKE VBRP-VBELN,
         POSNR1           LIKE VBRP-POSNR,
         FKDAT            LIKE VRPMA-FKDAT,     "billing date
         NAME(80),                             "customer name(name1+name2)
         NAME_ENG(100),                             "customer name eng (name1+name2)
         VTEXT            TYPE TVTWT-VTEXT,     "distribution channel name
         VORNA            TYPE PA0002-VORNA,  "sales person
         KUNNR            TYPE VBPA-KUNNR,      "customer no.
         TEXT(80),                              "text-type of CM
         RMA              TYPE VBFA-VBELN,          "no. RMA
         VBELN            TYPE VBFA-VBELN,       "goods receipt document no.
         MATNR            TYPE  VBRP-MATNR,     "material no.
         SERIAL(200),                           "serial no. 5 serials no. per 1 line
         LGORT            TYPE VBRP-LGORT,       "storage location
         NETPR            TYPE VBAP-NETPR,     "price per unit
         NETWR            TYPE VBRP-NETWR,      "credit not item amount
         QTY_SET          TYPE VBRP-FKIMG,       "quantity of parent BOM
         QTY_UNIT         TYPE VBRP-FKIMG,   "quantity of normal item or sub item of BOM
         BEZEI            TYPE TVAUT-BEZEI,      "credit note reason
         TEXT_MISTAKE(40),                     "mistake by
         TEXT_FIRST_ITEM  TYPE I,                   "every first item of CN, show 1
         UEPOS            TYPE VBRP-UEPOS,
         AUBEL            TYPE VBRP-AUBEL,
         AUPOS            TYPE VBRP-AUPOS,
         REASON(80),                  "Text Reason for Retrun
         CM_TYPE          TYPE CHAR20,         "CM Type
         CM_SHEET(100),               "Text CM Sheet for Retrun
         MISTAKE(100),                "Text Mistaken for Retrun
         BEZEIO           TYPE TVKBT-BEZEI,
         NETWRCN          TYPE VBAK-NETWR,
         INBCN            TYPE LIPS-VBELN,
         PRDHA            TYPE MARA-PRDHA,
         PH1              TYPE C LENGTH 5,
         PH3              TYPE C LENGTH 8,
         EXPTY            TYPE C LENGTH 255,
         EXPCG            TYPE C LENGTH 255,
*         EXPAT            TYPE ZTSD_EXP_TRANS-AMOUT,

       END OF TYP_OUT.

TYPES: BEGIN OF TYP_VBPA,
         VBELN LIKE  VBPA-VBELN,         "sales order no.
         POSNR LIKE  VBPA-POSNR,        "sales order item no.
         PARVW LIKE  VBPA-PARVW,        "partner function
         KUNNR LIKE  VBPA-KUNNR,         "customer no.
         PERNR LIKE VBPA-PERNR,          "personnel number
         ADRNR LIKE    VBPA-ADRNR,        "Address
         XCPDK LIKE VBPA-XCPDK,          "indicator
       END OF TYP_VBPA.
*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA: GT_VRPMA   TYPE STANDARD TABLE OF TYP_VRPMA,
      GT_VRPMA_1 TYPE STANDARD TABLE OF TYP_VRPMA,
      GT_VBRP    TYPE STANDARD TABLE OF TYP_VBRP,
      GT_OUT     TYPE STANDARD TABLE OF TYP_OUT,
      GT_DISPLAY TYPE STANDARD TABLE OF TYP_OUT,
      GT_VBPA    TYPE STANDARD TABLE OF TYP_VBPA.

DATA : WA_VRPMA      LIKE LINE OF GT_VRPMA,
       WA_VBRP       LIKE LINE OF GT_VBRP,
       WA_OUT        LIKE  LINE OF GT_OUT,
       WA_VBPA       LIKE  LINE OF GT_VBPA,
       WA_DISPLAY    LIKE LINE OF GT_DISPLAY,
       WA_KNA1_NAME1 TYPE KNA1-NAME1,
       WA_XCPDK      TYPE KNA1-XCPDK,
       WA_ADRNR      TYPE KNA1-ADRNR,
       WA_NAME1      TYPE  ADRC-NAME1,
       WA_NAME2      TYPE ADRC-NAME2,
       WA_NAME_ENG1  TYPE  ADRC-NAME1,
       WA_NAME_ENG2  TYPE ADRC-NAME2,
       WA_KUNNR      TYPE KNA1-KUNNR.

DATA : GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
       GT_EVENTS   TYPE SLIS_T_EVENT,
       GT_HEADING  TYPE SLIS_T_LISTHEADER,
       GT_SORT     TYPE SLIS_T_SORTINFO_ALV,
       GT_LAYOUT   TYPE SLIS_LAYOUT_ALV,
       GT_EXIT(1)  TYPE C,
       GT_VARIANT  TYPE DISVARIANT,
       GX_VARIANT  TYPE DISVARIANT.

DATA: V_POS TYPE I.
*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS : GC_BILLTO      TYPE VBPA-PARVW VALUE 'RE' ,  "Billto
            GC_SALE_PERSON TYPE VBPA-PARVW VALUE 'VE',      "Sale person
            GC_REPID       TYPE SY-REPID VALUE 'ZSDSSDR0470'.

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
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-T01 .

  PARAMETERS    : P_VKORG    LIKE VRPMA-VKORG  OBLIGATORY.  "sales organization
  SELECT-OPTIONS: S_VTWEG  FOR VRPMA-VTWEG ,                    "distribution channel
                            S_VKBUR   FOR VBRP-VKBUR ,                         "sales office
                            S_VKGRP   FOR VBRP-VKGRP ,                         "sales group
                            S_KUNNR   FOR VBPA-KUNNR,                        "customer no.
                            S_FKDAT   FOR VRPMA-FKDAT OBLIGATORY ,   "billing date
                            S_VBELN   FOR VRPMA-VBELN , "credit note no.
                            S_WERKS   FOR MARC-WERKS NO INTERVALS NO-EXTENSION OBLIGATORY DEFAULT '1000'.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-T02.
  PARAMETERS : R1 RADIOBUTTON GROUP LOG,
               R2 RADIOBUTTON GROUP LOG.
SELECTION-SCREEN END OF BLOCK B2.
*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.
  PERFORM BUILD_CATALOG CHANGING GT_FIELDCAT[].
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
  PERFORM AUTHORIZATION_CHECKING .
  PERFORM GET_DATA.
  PERFORM COLLECT_ALL_DATA.
  PERFORM F_GET_ADDITIONAL_DATA.


*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
  IF NOT GT_OUT[] IS INITIAL.
    PERFORM : BUILD_LAYOUT USING GT_LAYOUT,
                   BUILD_SORT     USING GT_SORT,
                   DISPLAY_OUTPUT.

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
*&---------------------------------------------------------------------*
*& Form BUILD_CATALOG
*&---------------------------------------------------------------------*
FORM BUILD_CATALOG  CHANGING RT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

  DATA: WA_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
        POS         TYPE I VALUE 1.

  CLEAR WA_FIELDCAT .

* column 1  Billing date
  PERFORM APPEND_FIELDCAT USING 'FKDAT' 'VRPMA' 'FKDAT'  TEXT-001
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* column 2 Customer name
  PERFORM APPEND_FIELDCAT USING 'NAME' SPACE SPACE  TEXT-002
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

* column 3 Customer name Eng
  PERFORM APPEND_FIELDCAT USING 'NAME_ENG' SPACE SPACE  TEXT-003
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

* column 4 Distribution channel name
  PERFORM APPEND_FIELDCAT USING 'VTEXT' 'TVTWT' 'VTEXT'  TEXT-004
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* column 5 Sale person
  PERFORM APPEND_FIELDCAT USING 'VORNA' 'PA0002' 'VORNA'  TEXT-005
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* column 6 Customer number
  PERFORM APPEND_FIELDCAT USING 'KUNNR' 'VBPA' 'KUNNR'  TEXT-006
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* column 7 CM type
  PERFORM APPEND_FIELDCAT USING 'TEXT' '' ''  TEXT-007
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* column 8 Goods receipt document number
  PERFORM APPEND_FIELDCAT USING 'RMA' 'VBFA' 'VBELN'  TEXT-008
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* column 9 Material number
  PERFORM APPEND_FIELDCAT USING 'MATNR' 'VBRP' 'MATNR'  TEXT-009
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* column 10 Serial
  PERFORM APPEND_FIELDCAT USING 'SERIAL' SPACE SPACE  TEXT-010
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* column 11 Storage location
  PERFORM APPEND_FIELDCAT USING 'LGORT' 'VBRP' 'LGORT'  TEXT-011
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* column 12 Billing number
  PERFORM APPEND_FIELDCAT USING 'VBELN' 'VRPMA' 'VBELN'  TEXT-012
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* column 13 Price per unit
  PERFORM APPEND_FIELDCAT USING 'NETPR' 'VBAP' 'NETPR'  TEXT-013
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* column 14 Credit note item amount
  PERFORM APPEND_FIELDCAT USING 'NETWR' 'VBRP' 'NETWR'  TEXT-014
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
* column 15 Quantity of Parent BOM
  PERFORM APPEND_FIELDCAT USING 'QTY_SET' 'VBRP' 'FKIMG'  TEXT-015
                                 SPACE  SPACE   'X'
                                 GT_FIELDCAT[].
* column 16 Quantity of normal item or sub item of BOM
  PERFORM APPEND_FIELDCAT USING 'QTY_UNIT' 'VBRP' 'FKIMG'  TEXT-016
                                 SPACE  SPACE   'X'
                                 GT_FIELDCAT[].
* column 17 Credit note reason
  PERFORM APPEND_FIELDCAT USING 'BEZEI' 'TVAUT' 'BEZEI'  TEXT-017
                                 SPACE  SPACE 'X'
                                 GT_FIELDCAT[].
* column 18 Mistake by
  PERFORM APPEND_FIELDCAT USING 'TEXT_MISTAKE' '' ''  TEXT-018
                                 SPACE  SPACE 'X'
                                 GT_FIELDCAT[].
*
* column 19 QTY invoice/month
  PERFORM APPEND_FIELDCAT USING 'TEXT_FIRST_ITEM' SPACE SPACE TEXT-019
                                  SPACE  SPACE 'X'
                                  GT_FIELDCAT[].

* column 20 Reason
  PERFORM APPEND_FIELDCAT USING 'REASON' SPACE SPACE TEXT-020
                                  SPACE  SPACE 'X'
                                  GT_FIELDCAT[].

* column 21 CM Sheet Number
  PERFORM APPEND_FIELDCAT USING 'CM_TYPE' SPACE SPACE TEXT-032
                                  SPACE  SPACE 'X'
                                  GT_FIELDCAT[].

* column 21 CM Sheet Number
  PERFORM APPEND_FIELDCAT USING 'CM_SHEET' SPACE SPACE TEXT-021
                                  SPACE  SPACE 'X'
                                  GT_FIELDCAT[].

* column 22 Return: Mistaken by
  PERFORM APPEND_FIELDCAT USING 'MISTAKE' SPACE SPACE TEXT-022
                                  SPACE  SPACE 'X'
                                  GT_FIELDCAT[].

* column 23 Sales Office
  PERFORM APPEND_FIELDCAT USING 'BEZEIO' SPACE SPACE TEXT-023
                                  SPACE  SPACE 'X'
                                  GT_FIELDCAT[].

* column 24 Return Amount
  PERFORM APPEND_FIELDCAT USING 'NETWRCN' SPACE SPACE TEXT-024
                                  SPACE  SPACE 'X'
                                  GT_FIELDCAT[].

* column 25 NO. RETURN
  PERFORM APPEND_FIELDCAT USING 'INBCN' SPACE SPACE TEXT-025
                                  SPACE  SPACE 'X'
                                  GT_FIELDCAT[].

* column 26 Product Hierarchy
  PERFORM APPEND_FIELDCAT USING 'PRDHA' SPACE SPACE TEXT-026
                                  SPACE  SPACE 'X'
                                  GT_FIELDCAT[].

* column 27 Category
  PERFORM APPEND_FIELDCAT USING 'PH1' SPACE SPACE TEXT-027
                                  SPACE  SPACE 'X'
                                  GT_FIELDCAT[].

* column 28 Class
  PERFORM APPEND_FIELDCAT USING 'PH3' SPACE SPACE TEXT-028
                                  SPACE  SPACE 'X'
                                  GT_FIELDCAT[].

* column 29 Expense Type
  PERFORM APPEND_FIELDCAT USING 'EXPTY' SPACE SPACE TEXT-029
                                  SPACE  SPACE 'X'
                                  GT_FIELDCAT[].

* column 30 Charge to
  PERFORM APPEND_FIELDCAT USING 'EXPCG' SPACE SPACE TEXT-030
                                  SPACE  SPACE 'X'
                                  GT_FIELDCAT[].

* column 31 Amount for expense
  PERFORM APPEND_FIELDCAT USING 'EXPAT' SPACE SPACE TEXT-031
                                  SPACE  SPACE 'X'
                                  GT_FIELDCAT[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM APPEND_FIELDCAT  USING   P_FIELD  "field name
*                           p_table   "Table name
                             P_REFTABLE"Reference Table name
                             P_REFFIELD"Reference Field name
*                            p_colpos  "Col position
                             P_COLTXT  "Col Text(for specify)
                             P_DOSUM   "Sum total
                             P_CFIELDNAME  "  currency
                             P_NO_ZERO     " no zero
                             P_IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

  DATA: WA_INFIELDCAT   TYPE SLIS_FIELDCAT_ALV,
        V_COLTXT_LENGTH TYPE I.

  ADD 1 TO V_POS.

  WA_INFIELDCAT-FIELDNAME     = P_FIELD.
  WA_INFIELDCAT-REF_TABNAME   = P_REFTABLE.
  WA_INFIELDCAT-REF_FIELDNAME = P_REFFIELD.
  WA_INFIELDCAT-COL_POS       = V_POS .
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
FORM READ_TXT_REASON USING  P_NAME
                     P_LANGUAGE
               CHANGING P_TEXT TYPE CHAR80.

  DATA: WA_LINE     TYPE TLINE,
        IT_LINE     TYPE STANDARD TABLE OF TLINE,
        LV_NAME     TYPE THEAD-TDNAME,
        P_TEXT_TEMP TYPE CHAR300.
  LV_NAME = P_NAME.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      CLIENT                  = SY-MANDT
      ID                      = 'ZH01'
      LANGUAGE                = P_LANGUAGE
      NAME                    = LV_NAME
      OBJECT                  = 'VBBK'
    TABLES
      LINES                   = IT_LINE
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC <> 0.
    "Don't do anything
  ELSE.
    LOOP AT IT_LINE INTO WA_LINE.
      MOVE WA_LINE-TDLINE TO P_TEXT_TEMP.
      CONCATENATE P_TEXT P_TEXT_TEMP INTO P_TEXT.
    ENDLOOP.

  ENDIF.

ENDFORM.   " read_standard_texts
*&---------------------------------------------------------------------*
*&      Form  read_txt_cm_sheet
*&---------------------------------------------------------------------*
FORM READ_TXT_CM_SHEET USING  P_NAME
                     P_LANGUAGE
               CHANGING P_TEXT TYPE CHAR100.

  DATA: WA_LINE     TYPE TLINE,
        IT_LINE     TYPE STANDARD TABLE OF TLINE,
        LV_NAME     TYPE THEAD-TDNAME,
        P_TEXT_TEMP TYPE CHAR300.
  LV_NAME = P_NAME.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      CLIENT                  = SY-MANDT
      ID                      = 'ZH04'
      LANGUAGE                = P_LANGUAGE
      NAME                    = LV_NAME
      OBJECT                  = 'VBBK'
    TABLES
      LINES                   = IT_LINE
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC <> 0.
    "don't do anything
  ELSE.
    LOOP AT IT_LINE INTO WA_LINE.
      MOVE WA_LINE-TDLINE TO P_TEXT_TEMP.
      CONCATENATE P_TEXT P_TEXT_TEMP INTO P_TEXT.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "read_txt_cm_sheet
*&---------------------------------------------------------------------*
*&      Form  read_txt_cm_type
*&---------------------------------------------------------------------*
FORM READ_TXT_CM_TYPE USING  P_NAME
                             P_LANGUAGE
                      CHANGING P_TEXT TYPE CHAR20.

  DATA: WA_LINE     TYPE TLINE,
        IT_LINE     TYPE STANDARD TABLE OF TLINE,
        LV_NAME     TYPE THEAD-TDNAME,
        P_TEXT_TEMP TYPE CHAR300.
  LV_NAME = P_NAME.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      CLIENT                  = SY-MANDT
      ID                      = 'ZH12'
      LANGUAGE                = P_LANGUAGE
      NAME                    = LV_NAME
      OBJECT                  = 'VBBK'
    TABLES
      LINES                   = IT_LINE
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC <> 0.
    "don't do anything
  ELSE.
    LOOP AT IT_LINE INTO WA_LINE.
      MOVE WA_LINE-TDLINE TO P_TEXT_TEMP.
      CONCATENATE P_TEXT P_TEXT_TEMP INTO P_TEXT.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "read_txt_cm_type
*&---------------------------------------------------------------------*
*&      Form  read_txt_remark
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NAME     text
*      -->P_LANGUAGE text
*      -->P_TEXT     text
*----------------------------------------------------------------------*
FORM READ_TXT_REMARK USING  P_NAME
                            P_LANGUAGE
                     CHANGING P_TEXT TYPE CHAR100.

  DATA: WA_LINE     TYPE TLINE,
        IT_LINE     TYPE STANDARD TABLE OF TLINE,
        LV_NAME     TYPE THEAD-TDNAME,
        P_TEXT_TEMP TYPE CHAR300.

  LV_NAME = P_NAME.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      CLIENT                  = SY-MANDT
      ID                      = 'ZH02'
      LANGUAGE                = P_LANGUAGE
      NAME                    = LV_NAME
      OBJECT                  = 'VBBK'
    TABLES
      LINES                   = IT_LINE
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC <> 0.
  ELSE.
    LOOP AT IT_LINE INTO WA_LINE.
      MOVE WA_LINE-TDLINE TO P_TEXT_TEMP.
      CONCATENATE P_TEXT P_TEXT_TEMP INTO P_TEXT.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "read_txt_remark
*&---------------------------------------------------------------------*
*&      Form  F_GET_ADDITIONAL_DATA
*&---------------------------------------------------------------------*
FORM F_GET_ADDITIONAL_DATA .

  DATA : BEGIN OF LS_VBAK,
           VBELN TYPE VBAK-VBELN,
           VKBUR TYPE VBAK-VKBUR,
           BEZEI TYPE TVKBT-BEZEI,
           NETWR TYPE VBAP-NETWR,
         END OF LS_VBAK.
  DATA LT_VBAK LIKE TABLE OF LS_VBAK.

  DATA : BEGIN OF LS_LIPS,
           VBELN TYPE LIPS-VBELN,
           VGBEL TYPE LIPS-VGBEL,
           VGPOS TYPE LIPS-VGPOS,
         END OF LS_LIPS.
  DATA LT_LIPS LIKE TABLE OF LS_LIPS.

  DATA : BEGIN OF LS_MARA,
           MATNR TYPE MARA-MATNR,
           PRDHA TYPE MARA-PRDHA,
         END OF LS_MARA.
  DATA LT_MARA LIKE TABLE OF LS_MARA.

  DATA LS_DISPLAY TYPE TYP_OUT.

  DATA : LT_DISPLAY_TMP LIKE GT_DISPLAY,
         LS_DISPLAY_TMP LIKE LINE OF LT_DISPLAY_TMP.

  DATA LV_TABIX TYPE SY-TABIX.

  IF GT_DISPLAY[] IS NOT INITIAL.

    SELECT VBAK~VBELN
           VBAK~VKBUR
           TVKBT~BEZEI
           VBAP~NETWR
      FROM VBAP
      INNER JOIN VBAK  ON VBAP~VBELN EQ VBAK~VBELN
      INNER JOIN TVKBT ON VBAK~VKBUR  EQ TVKBT~VKBUR AND
                          TVKBT~SPRAS EQ SY-LANGU
      INTO TABLE LT_VBAK
      FOR ALL ENTRIES IN GT_DISPLAY
      WHERE VBAP~VBELN EQ GT_DISPLAY-AUBEL
        AND VBAP~POSNR EQ GT_DISPLAY-AUPOS.

    SELECT VBELN
           VGBEL
           VGPOS
      FROM LIPS
      INTO TABLE LT_LIPS
      FOR ALL ENTRIES IN GT_DISPLAY
      WHERE VGBEL EQ GT_DISPLAY-AUBEL
        AND VGPOS EQ GT_DISPLAY-AUPOS.

    SELECT MATNR
           PRDHA
      FROM MARA
      INTO TABLE LT_MARA
      FOR ALL ENTRIES IN GT_DISPLAY
      WHERE MATNR EQ GT_DISPLAY-MATNR.


    LOOP AT GT_DISPLAY INTO LS_DISPLAY.
      LV_TABIX = SY-TABIX.

      READ TABLE LT_VBAK INTO LS_VBAK
      WITH KEY VBELN = LS_DISPLAY-AUBEL.
      IF SY-SUBRC = 0.
        LS_DISPLAY-BEZEIO  = LS_VBAK-BEZEI.
        LS_DISPLAY-NETWRCN = LS_VBAK-NETWR.
      ENDIF.

      READ TABLE LT_LIPS INTO LS_LIPS
      WITH KEY VGBEL = LS_DISPLAY-AUBEL
               VGPOS = LS_DISPLAY-AUPOS.
      IF SY-SUBRC = 0.
        LS_DISPLAY-INBCN = LS_LIPS-VBELN.
      ENDIF.

      READ TABLE LT_MARA INTO LS_MARA
      WITH KEY MATNR = LS_DISPLAY-MATNR.
      IF SY-SUBRC = 0.
        LS_DISPLAY-PRDHA = LS_MARA-PRDHA.
        LS_DISPLAY-PH1   = LS_MARA-PRDHA+0(5).
        LS_DISPLAY-PH3   = LS_MARA-PRDHA+5(5).
      ENDIF.

      IF R2 EQ 'X'.
        CLEAR LS_DISPLAY-SERIAL.
      ENDIF.

      MODIFY GT_DISPLAY FROM LS_DISPLAY INDEX LV_TABIX
                                 TRANSPORTING BEZEIO NETWRCN INBCN PRDHA PH1 PH3 SERIAL.

      CLEAR : LS_DISPLAY,LS_VBAK,LS_MARA,LS_LIPS.
    ENDLOOP.

  ENDIF.
ENDFORM.                    " F_GET_ADDITIONAL_DATA
*&---------------------------------------------------------------------*
*&      Form  AUTHORIZATION_CHECKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM AUTHORIZATION_CHECKING .

ENDFORM.                    " AUTHORIZATION_CHECKING
*&---------------------------------------------------------------------*
*&      Form  COLLECT_ALL_DATA
*&---------------------------------------------------------------------*
*    collect all data for display in ALV
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COLLECT_ALL_DATA .

  DATA: LV_VBELN       TYPE VBFA-VBELN,
        LV_POSNN       TYPE VBFA-POSNN,
        LV_OBKNR       TYPE SER01-OBKNR,
        LV_SERIAL(200),
        LV_CNT         TYPE I,
        LV_VBELN1      TYPE VBFA-VBELN,
        LV_VBELN2      TYPE VBFA-VBELN,
        LV_TEMP_VBELN  LIKE VBRP-VBELN.

  DATA: LV_TEMP_VBELN1 TYPE VBFA-VBELN,
        LV_TEMP_POSNN1 TYPE VBFA-POSNN.


  DATA : L_LINE LIKE TLINE OCCURS 0 WITH HEADER LINE,
         I_NAME TYPE THEAD-TDNAME.

  DATA: BEGIN OF ITAB3,
          VBELN LIKE VBRP-VBELN,
          POSNR LIKE VBRP-POSNR,
        END OF ITAB3.
  DATA: IT_ITAB_VBRP3 LIKE STANDARD TABLE OF ITAB3 WITH HEADER LINE.



  CONSTANTS: LC_SPRAS    TYPE TVAUT-SPRAS VALUE 'E',
             LC_TDOBJECT TYPE THEAD-TDOBJECT VALUE 'VBBK'.

  TYPES: BEGIN OF TYP_OBJK,
           SERNR LIKE OBJK-SERNR,
         END OF TYP_OBJK.
  DATA: GT_OBJK TYPE STANDARD TABLE OF TYP_OBJK.

  DATA: WA_OBJK LIKE LINE OF GT_OBJK.

  CLEAR GT_DISPLAY.
  REFRESH GT_DISPLAY.

  IF NOT GT_VRPMA_1[] IS INITIAL AND NOT GT_VBRP[] IS INITIAL.
    CLEAR WA_VRPMA.

    LOOP AT GT_VRPMA_1 INTO WA_VRPMA.
      CLEAR WA_OUT.
* Billing date and Distribution channel name
      MOVE: WA_VRPMA-FKDAT TO WA_OUT-FKDAT,
                WA_VRPMA-VTEXT TO WA_OUT-VTEXT,
                WA_VRPMA-VBELN TO WA_OUT-VBELN.
      CLEAR WA_VBRP.
      READ TABLE GT_VBRP INTO WA_VBRP WITH KEY VBELN = WA_VRPMA-VBELN
                                                                         POSNR = WA_VRPMA-POSNR.
      IF SY-SUBRC EQ 0.
        WA_OUT-UEPOS = WA_VBRP-UEPOS.
        WA_OUT-RMA = WA_VBRP-AUBEL.
        SELECT SINGLE BEZEI INTO WA_OUT-BEZEI
        FROM TVAUT
        WHERE SPRAS = LC_SPRAS
        AND     AUGRU = WA_VBRP-AUGRU_AUFT.
* Material Number, storage location
        WA_OUT-MATNR   = WA_VBRP-MATNR.
        WA_OUT-VBELN1  = WA_VBRP-VBELN.
        WA_OUT-POSNR1  = WA_VBRP-POSNR.
        WA_OUT-AUBEL    = WA_VBRP-AUBEL.
        WA_OUT-AUPOS   = WA_VBRP-AUPOS.
* Quantity of Parent BOM
        WA_OUT-QTY_UNIT = 0.
        WA_OUT-QTY_SET  = 0.
* Storage location
        WA_OUT-LGORT     = SPACE.
        SELECT SINGLE VBELN POSNN INTO (LV_TEMP_VBELN1, LV_TEMP_POSNN1)
        FROM VBFA
        WHERE VBELV = WA_VBRP-AUBEL
        AND     POSNV = WA_VBRP-AUPOS
        AND     VBTYP_N = 'T'.
        IF SY-SUBRC EQ 0.
          SELECT SINGLE LGORT INTO WA_OUT-LGORT
          FROM LIPS
          WHERE VBELN = LV_TEMP_VBELN1
          AND     POSNR = LV_TEMP_POSNN1
          AND     VGBEL = WA_VBRP-AUBEL
          AND     VGPOS = WA_VBRP-AUPOS.
        ENDIF.

        IF WA_VBRP-UEPOS = 000000.
* check this is Main item and no subitem
          CLEAR IT_ITAB_VBRP3.
          REFRESH IT_ITAB_VBRP3.
          SELECT VBELN POSNR INTO TABLE IT_ITAB_VBRP3
          FROM VBRP
          WHERE VBELN =  WA_VBRP-VBELN
          AND     UEPOS = WA_VBRP-POSNR.

          IF SY-SUBRC EQ 0.
* this is main item and has sub item
            WA_OUT-QTY_SET = WA_VBRP-FKIMG.
          ELSE.
            WA_OUT-QTY_UNIT  = WA_VBRP-FKIMG.
          ENDIF.
        ENDIF.

* Quantity of normal item or sub item of BOM
        IF WA_VBRP-UEPOS <> 000000.
          WA_OUT-QTY_UNIT  = WA_VBRP-FKIMG.
        ENDIF.

* Price per unit
        SELECT SINGLE NETPR INTO WA_OUT-NETPR
        FROM VBAP
        WHERE VBELN = WA_VBRP-AUBEL
        AND     POSNR = WA_VBRP-AUPOS.
* Credit note item amount
        WA_OUT-NETWR = WA_VBRP-NETWR.
* Sale person
        CLEAR WA_VBPA.

        DATA: WA_PERNR TYPE PA0002-PERNR.

        SELECT SINGLE PERNR INTO WA_PERNR
        FROM VBPA
        WHERE    VBELN = WA_VRPMA-VBELN
        AND        PARVW = GC_SALE_PERSON.

        IF SY-SUBRC EQ 0.
          SELECT SINGLE VORNA INTO WA_OUT-VORNA
          FROM PA0002
          WHERE PERNR = WA_PERNR.
        ENDIF.

* get Billto (Customer Name)
        CLEAR WA_VBPA.
        READ TABLE GT_VBPA INTO WA_VBPA WITH KEY VBELN = WA_VRPMA-VBELN
                                                                           PARVW = GC_BILLTO.
        IF SY-SUBRC EQ 0 .
* Customer Number
          WA_OUT-KUNNR = WA_VBPA-KUNNR.
* Customer Name
          CLEAR: WA_XCPDK, WA_ADRNR.
          SELECT SINGLE XCPDK ADRNR INTO (WA_XCPDK, WA_ADRNR)
          FROM KNA1
          WHERE KUNNR = WA_VBPA-KUNNR .
          IF SY-SUBRC EQ 0.
            IF WA_XCPDK = ' '.
              SELECT SINGLE NAME1 NAME2 INTO (WA_NAME1, WA_NAME2)
              FROM ADRC
              WHERE ADDRNUMBER = WA_ADRNR
              AND      NATION = ' '.

              SELECT SINGLE NAME1 NAME2 INTO (WA_NAME_ENG1, WA_NAME_ENG2)
              FROM ADRC
              WHERE ADDRNUMBER = WA_ADRNR
              AND      NATION = 'I'.
              CONCATENATE WA_NAME_ENG1 WA_NAME_ENG2 INTO WA_OUT-NAME_ENG.
            ELSE.
              SELECT SINGLE NAME1 NAME2 INTO (WA_NAME1, WA_NAME2)
              FROM ADRC
              WHERE ADDRNUMBER = WA_VBPA-ADRNR
              AND      NATION = ' '.
            ENDIF.
            CONCATENATE WA_NAME1 WA_NAME2 INTO WA_OUT-NAME.
          ENDIF.
        ELSE.
          CONTINUE.
        ENDIF .

* collect all data into internal table

* Read mistake by text vbbk, ZR01
        I_NAME =   WA_VBRP-AUBEL.
        CLEAR L_LINE.
        REFRESH  L_LINE[].
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            ID        = 'ZR01'
            LANGUAGE  = SY-LANGU
            NAME      = I_NAME
            OBJECT    = LC_TDOBJECT
          TABLES
            LINES     = L_LINE
          EXCEPTIONS
            ID        = 1
            LANGUAGE  = 2
            NAME      = 3
            NOT_FOUND = 4.

*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
        IF NOT L_LINE[] IS INITIAL.
          CLEAR  WA_DISPLAY-TEXT_MISTAKE.
          LOOP AT L_LINE.
            IF SYST-TABIX = 1 .
              WA_DISPLAY-TEXT_MISTAKE = L_LINE-TDLINE .
              WA_OUT-TEXT_MISTAKE = WA_DISPLAY-TEXT_MISTAKE.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.

* read CM Type vbbk, ZR02
        CLEAR  L_LINE.
        REFRESH  L_LINE.
        I_NAME =   WA_OUT-AUBEL.
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            ID        = 'ZR02'
            LANGUAGE  = SY-LANGU
            NAME      = I_NAME
            OBJECT    = LC_TDOBJECT
          TABLES
            LINES     = L_LINE
          EXCEPTIONS
            ID        = 1
            LANGUAGE  = 2
            NAME      = 3
            NOT_FOUND = 4.
*
        IF NOT L_LINE[] IS INITIAL.
          LOOP AT L_LINE.
            IF SYST-TABIX = 1 .
              WA_OUT-TEXT = L_LINE-TDLINE .
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.
        PERFORM READ_TXT_REASON USING WA_OUT-AUBEL SY-LANGU CHANGING WA_OUT-REASON.
        PERFORM READ_TXT_CM_TYPE  USING WA_OUT-AUBEL SY-LANGU CHANGING WA_OUT-CM_TYPE.
        PERFORM READ_TXT_CM_SHEET USING WA_OUT-AUBEL SY-LANGU CHANGING WA_OUT-CM_SHEET.
        PERFORM READ_TXT_REMARK USING WA_OUT-AUBEL SY-LANGU CHANGING WA_OUT-MISTAKE.


        APPEND WA_OUT TO GT_OUT.
      ENDIF.

    ENDLOOP.

  ENDIF.

* get Serial no. of each line item and collect all data for display

  IF NOT GT_OUT[] IS INITIAL.
    CLEAR: WA_OUT.      " wa_display.
    LOOP AT GT_OUT INTO WA_OUT.
* Serial No.
      CLEAR: GT_OBJK, LV_OBKNR, LV_VBELN, LV_POSNN.
      REFRESH GT_OBJK.
      SELECT SINGLE VBELN POSNN INTO (LV_VBELN, LV_POSNN)
      FROM VBFA
      WHERE VBELV     = WA_OUT-AUBEL
      AND     POSNV    = WA_OUT-AUPOS
      AND      VBTYP_N = 'T'.
      IF SY-SUBRC EQ 0.
        SELECT SINGLE  OBKNR INTO LV_OBKNR
        FROM SER01
        WHERE LIEF_NR = LV_VBELN
        AND     POSNR  = LV_POSNN.
        IF SY-SUBRC EQ 0 AND LV_OBKNR NE ' '.
          SELECT SERNR INTO TABLE GT_OBJK
          FROM OBJK
          WHERE OBKNR = LV_OBKNR.
        ENDIF.
      ENDIF.

      IF NOT GT_OBJK[] IS INITIAL.
        LV_CNT = 1.
        CLEAR LV_SERIAL.
        LV_TEMP_VBELN = WA_OUT-VBELN1.
        LOOP AT GT_OBJK INTO WA_OBJK.
          IF LV_CNT = 1.
            LV_SERIAL = WA_OBJK-SERNR+11(7).
          ELSE.
            IF LV_CNT = 10.
              MOVE-CORRESPONDING  WA_OUT TO WA_DISPLAY.
              WA_DISPLAY-VBELN = LV_TEMP_VBELN.
              WA_DISPLAY-SERIAL = LV_SERIAL.
              APPEND WA_DISPLAY TO GT_DISPLAY .
              CLEAR LV_SERIAL.
              CLEAR WA_OUT.
              LV_CNT   = 1.
              LV_SERIAL = WA_OBJK-SERNR+11(7).
            ELSE.
              CONCATENATE LV_SERIAL WA_OBJK-SERNR+11(7)
              INTO LV_SERIAL SEPARATED BY SPACE.
            ENDIF.
          ENDIF.
          LV_CNT = LV_CNT + 1.
        ENDLOOP.
        MOVE-CORRESPONDING  WA_OUT TO WA_DISPLAY.
        WA_DISPLAY-VBELN = LV_TEMP_VBELN.
        WA_DISPLAY-SERIAL = LV_SERIAL.
        APPEND WA_DISPLAY TO GT_DISPLAY .
      ELSE.
        WA_DISPLAY = WA_OUT.
        APPEND WA_DISPLAY TO GT_DISPLAY.
      ENDIF.
    ENDLOOP.

  ENDIF.

  CLEAR: WA_DISPLAY, LV_VBELN1, LV_VBELN2.
  LV_CNT = 1.
  LOOP AT GT_DISPLAY INTO WA_DISPLAY.
    IF LV_CNT = 1.
      LV_VBELN1 = WA_DISPLAY-VBELN.
      LV_VBELN2 = ' '.
      WA_DISPLAY-TEXT_FIRST_ITEM = 1.
      MODIFY GT_DISPLAY FROM WA_DISPLAY.
      WA_DISPLAY-TEXT_FIRST_ITEM = '' .
    ELSE.
      IF LV_CNT = 2.
        LV_VBELN2 = WA_DISPLAY-VBELN.
        IF LV_VBELN1 <> LV_VBELN2.
          LV_VBELN1 = LV_VBELN2.
          WA_DISPLAY-TEXT_FIRST_ITEM = 1.
          MODIFY GT_DISPLAY FROM WA_DISPLAY.
          WA_DISPLAY-TEXT_FIRST_ITEM = '' .
        ENDIF.
      ELSE.
        LV_VBELN2 = WA_DISPLAY-VBELN.
        IF LV_VBELN1 <> LV_VBELN2.
          LV_VBELN1 = LV_VBELN2.
          WA_DISPLAY-TEXT_FIRST_ITEM = 1.
          MODIFY GT_DISPLAY FROM WA_DISPLAY.
          WA_DISPLAY-TEXT_FIRST_ITEM = '' .
        ENDIF.
      ENDIF.
    ENDIF.
    LV_CNT = LV_CNT + 1.
  ENDLOOP.

ENDFORM.                    " COLLECT_ALL_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*    get credit note of return data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .
  RANGES : LR_SERIAL FOR MARC-SERNP.

  DATA: LV_VBTYP TYPE VBTYP,
        LV_AUBEL TYPE VBRP-AUBEL,
        LV_VGTYP TYPE VGTYP,
        LV_SFAKN TYPE SFAKN,
        LV_FKSTO TYPE FKSTO.

  SELECT VBELN POSNR FKDAT VRPMA~VTWEG MATNR
  TVTWT~VTEXT KUNNR
   INTO TABLE GT_VRPMA
   FROM VRPMA
     LEFT OUTER JOIN TVTWT
     ON (   TVTWT~VTWEG = VRPMA~VTWEG
      AND TVTWT~SPRAS = SY-LANGU )
   WHERE VKORG = P_VKORG
    AND  VRPMA~VTWEG IN S_VTWEG
    AND  FKDAT IN S_FKDAT
    AND  VBELN IN S_VBELN.
*    AND   vrpma~kunnr IN s_kunnr.

* get credit note item

  IF NOT GT_VRPMA[] IS INITIAL.

* check condition for billing category (VBRK-VBTYP = 'O' and
* Order category (VBAK-VGTYP = 'H')

    LOOP AT GT_VRPMA INTO WA_VRPMA.
      CLEAR LV_VBTYP.
      SELECT SINGLE VBTYP SFAKN FKSTO
      INTO (LV_VBTYP, LV_SFAKN, LV_FKSTO)
      FROM VBRK
      WHERE VBELN = WA_VRPMA-VBELN
      AND      SFAKN = ' '
      AND      FKSTO = ' '
      AND      VBTYP = 'O'.

      IF SY-SUBRC EQ 0.
* find sales order no.
        CLEAR LV_AUBEL.
        SELECT SINGLE AUBEL INTO LV_AUBEL
        FROM VBRP
        WHERE VBELN = WA_VRPMA-VBELN
        AND       POSNR = WA_VRPMA-POSNR.

        IF SY-SUBRC EQ 0 AND LV_AUBEL NE '  '.
          SELECT SINGLE VGTYP INTO LV_VGTYP
          FROM VBAK
          WHERE VBELN = LV_AUBEL
          AND      VBTYP = 'H'.
          IF SY-SUBRC EQ 0.
            APPEND WA_VRPMA TO GT_VRPMA_1.
          ENDIF.
        ENDIF.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDLOOP.

    SELECT VBRP~VBELN VBRP~POSNR VBRP~MATNR VBRP~FKIMG VBRP~NETWR
           VBRP~AUBEL VBRP~AUPOS VBRP~AUGRU_AUFT VBRP~UEPOS LGORT
    FROM VBRP
    INNER JOIN MARC ON VBRP~MATNR EQ MARC~MATNR
    INTO TABLE  GT_VBRP
    FOR ALL ENTRIES IN GT_VRPMA_1
    WHERE VBRP~VBELN = GT_VRPMA_1-VBELN
    AND   VBRP~POSNR = GT_VRPMA_1-POSNR
    AND   VBRP~VKBUR IN S_VKBUR
    AND   VBRP~VKGRP IN S_VKGRP
    AND   MARC~WERKS IN S_WERKS
    AND   MARC~SERNP IN LR_SERIAL.

    SELECT VBELN POSNR PARVW KUNNR PERNR ADRNR XCPDK
    INTO TABLE GT_VBPA
    FROM VBPA
    FOR ALL ENTRIES IN GT_VRPMA_1
    WHERE VBELN = GT_VRPMA_1-VBELN
    AND KUNNR IN S_KUNNR.

  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LAYOUT  text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT  USING    P_GTYP_LAYOUT TYPE SLIS_LAYOUT_ALV.

  P_GTYP_LAYOUT-WINDOW_TITLEBAR = SY-TITLE.
  P_GTYP_LAYOUT-ZEBRA = 'X'.
  P_GTYP_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT  text
*----------------------------------------------------------------------*
FORM BUILD_SORT  USING   P_GTYP_SORT TYPE SLIS_T_SORTINFO_ALV.


ENDFORM.                    " BUILD_SORT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_OUTPUT .

  DATA VARIANT LIKE DISVARIANT.
  VARIANT-REPORT = GC_REPID .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = GC_REPID
      IS_LAYOUT          = GT_LAYOUT
      IT_FIELDCAT        = GT_FIELDCAT
      I_SAVE             = 'A'
      IS_VARIANT         = GT_VARIANT
      IT_EVENTS          = GT_EVENTS
    TABLES
      T_OUTTAB           = GT_DISPLAY
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " DISPLAY_OUTPUT

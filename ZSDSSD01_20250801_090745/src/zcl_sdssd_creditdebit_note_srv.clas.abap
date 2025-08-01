class ZCL_SDSSD_CREDITDEBIT_NOTE_SRV definition
  public
  inheriting from ZCL_SDSCA_REST_SERVICE
  final
  create public .

public section.

  constants GC_CREATE type CHAR1 value 'C' ##NO_TEXT.
  constants GC_UPDATE type CHAR1 value 'U' ##NO_TEXT.
  constants GC_SUCCESS type CHAR1 value 'S' ##NO_TEXT.
  constants GC_ERROR type CHAR1 value 'E' ##NO_TEXT.
  constants GC_WARNING type CHAR1 value 'W' ##NO_TEXT.

  class-methods SALES_ORDER_PROCESS
    importing
      !IF_TESTRUN type FLAG optional
    exporting
      value(EF_SALES_DOC) type VBELN
      value(EF_RESPONSE) type ZSDSCAS006
    changing
      !CS_SALESORDER type ZSDSSDS025
      !CT_SALESORDERITEM type ZSDSSDS026_TT optional
      !CT_RESPONSE type ZSDSCAS006_TT .
  class-methods VALIDATE_INPUT
    exporting
      value(EF_INVALID) type FLAG
    changing
      !CS_SALESORDER type ZSDSSDS025
      !CT_SALESORDERITEM type ZSDSSDS026_TT
      !CT_RESPONSE type ZSDSCAS006_TT .
  class-methods VALIDATE_HEADER
    exporting
      value(EF_INVALID) type FLAG
    changing
      !CS_SALESORDER type ZSDSSDS025
      !CT_RESPONSE type ZSDSCAS006_TT .
  class-methods VALIDATE_ITEM
    exporting
      value(EF_INVALID) type FLAG
    changing
      !CS_SALESORDER type ZSDSSDS025
      !CT_SALESORDERITEM type ZSDSSDS026_TT
      !CT_RESPONSE type ZSDSCAS006_TT .

  methods PROCESS_DATA
    redefinition .
protected section.
private section.

  class-data GRT_SALESORG type CMM_T_VKORG_RANGE .
  class-data GRT_DIVISION type CMM_T_SPART_RANGE .
  class-data GRT_CREDITMEMO_DOCTYP type SD_AUART_RANGES .
  class-data GRT_DEBITMEMO_DOCTYP type SD_AUART_RANGES .
  class-data GRT_RETURN_DOCTYP type SD_AUART_RANGES .
  class-data GRT_CREDITDEBIT_COND_TYPE type KSCHL_RAN_TAB .
  class-data GRT_COND_TYPE_DELETE type KSCHL_RAN_TAB .

  class-methods VALIDATE_DATE
    importing
      !DATE_IN type CHAR10
    exporting
      value(DATE_OUT) type CHAR10
      !CS_RESPONSE type ZSDSCAS006 .
  class-methods GET_CONSTANTS
    changing
      !CT_RESPONSE type ZSDSCAS006_TT .
  class-methods APPEND_RESPONSE
    importing
      !IF_STATUS type CHAR1
      !IF_MESSAGE type ZSDSDE_REST_MESSAGE
    changing
      !CT_RESPONSE type ZSDSCAS006_TT .
  class-methods SALES_ORDER_CREATE
    importing
      !IF_TESTRUN type FLAG optional
    exporting
      value(EF_SALES_DOC) type VBELN
      value(EF_RESPONSE) type ZSDSCAS006
    changing
      !CS_SALESORDER type ZSDSSDS025
      !CT_SALESORDERITEM type ZSDSSDS026_TT optional
      !CT_RESPONSE type ZSDSCAS006_TT .
  class-methods VALIDATE_AMOUNT
    importing
      !AMOUNT_IN type CHAR20
    exporting
      value(AMOUNT_OUT) type CHAR20
      !CS_RESPONSE type ZSDSCAS006 .
  class-methods APPEND_TEXT
    importing
      !TEXT_ID type TDID
      !LANGU type SPRAS
      !TEXT_LINE type TEXT255
    changing
      !TEXT type BAPISDTEXT_T .
ENDCLASS.



CLASS ZCL_SDSSD_CREDITDEBIT_NOTE_SRV IMPLEMENTATION.


  method APPEND_RESPONSE.
    DATA: LS_RESPONSE TYPE ZSDSCAS006.

    LS_RESPONSE-RESP_STATUS = IF_STATUS.
    LS_RESPONSE-RESP_MESSAGE = IF_MESSAGE.

    APPEND LS_RESPONSE TO CT_RESPONSE.

*    IF CS_SALESORDER-RESPONSESTATUS IS INITIAL AND
*       CS_SALESORDER-RESPONSEMESSAGE IS INITIAL.
*      CS_SALESORDER-RESPONSESTATUS = IF_STATUS.
*      CS_SALESORDER-RESPONSEMESSAGE = IF_MESSAGE.
*    ENDIF.
  endmethod.


  METHOD APPEND_TEXT.

    DATA: LV_TDLINE1 TYPE TDLINE,
          LV_TDLINE2 TYPE TDLINE,
          LV_LEN     TYPE I.

    LV_LEN = STRLEN( TEXT_LINE ).

    IF LV_LEN > 132 ##NUMBER_OK.
      LV_TDLINE1 = TEXT_LINE+0(132).
      LV_TDLINE2 = TEXT_LINE+132(123).  " 255 char
      APPEND VALUE #( TEXT_ID    = TEXT_ID
                      LANGU      = LANGU
*                      FORMAT_COL = '='
                      TEXT_LINE  = LV_TDLINE1 ) TO TEXT.

      APPEND VALUE #( TEXT_ID    = TEXT_ID
                      LANGU      = LANGU
*                      FORMAT_COL = '='
                      TEXT_LINE  = LV_TDLINE2 ) TO TEXT.
    ELSE.
      APPEND VALUE #( TEXT_ID   = TEXT_ID
                      LANGU     = LANGU
                      TEXT_LINE = TEXT_LINE ) TO TEXT.
    ENDIF.



  ENDMETHOD.


  METHOD GET_CONSTANTS.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_CREATE_CHANGE_SO_SRV / GET_CONSTANTS
*  Creation Date      : 21.05.2024
*  Author             : Zulkiff B. (Eviden)
*  Add-on ID          : ZSDI023
*  Description        : Get general constants from table ZSDSCAC001
*  Purpose            : Get general constants from table ZSDSCAC001
*                       Maintenance view: ZSDSV_GEN_C
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

    DATA: LV_MSG TYPE ZSDSDE_REST_MESSAGE.

    ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZCL_SDSSD_CREDITDEBIT_NOTE_SRV'
                                                    IF_PARAM = 'SALESORG'
                                          IMPORTING ET_RANGE = GRT_SALESORG ).
    IF GRT_SALESORG IS INITIAL.
      LV_MSG = TEXT-E01.
      REPLACE '&' IN LV_MSG WITH TEXT-T01.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = LV_MSG
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ENDIF.

    ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZCL_SDSSD_CREDITDEBIT_NOTE_SRV'
                                                    IF_PARAM = 'DIVISION'
                                          IMPORTING ET_RANGE = GRT_DIVISION ).
    IF GRT_DIVISION IS INITIAL.
      LV_MSG = TEXT-E01.
      REPLACE '&' IN LV_MSG WITH TEXT-T02.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = LV_MSG
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ENDIF.

    ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZCL_SDSSD_CREDITDEBIT_NOTE_SRV'
                                                    IF_PARAM = 'CREDITMEMO'
                                          IMPORTING ET_RANGE = GRT_CREDITMEMO_DOCTYP ).
    IF GRT_CREDITMEMO_DOCTYP IS INITIAL.
      LV_MSG = TEXT-E01.
      REPLACE '&' IN LV_MSG WITH TEXT-T03.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = LV_MSG
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ENDIF.

    ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZCL_SDSSD_CREDITDEBIT_NOTE_SRV'
                                                    IF_PARAM = 'DEBITMEMO'
                                          IMPORTING ET_RANGE = GRT_DEBITMEMO_DOCTYP ).
    IF GRT_DEBITMEMO_DOCTYP IS INITIAL.
      LV_MSG = TEXT-E01.
      REPLACE '&' IN LV_MSG WITH TEXT-T04.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = LV_MSG
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ENDIF.

    ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZCL_SDSSD_CREDITDEBIT_NOTE_SRV'
                                                    IF_PARAM = 'RETURN'
                                          IMPORTING ET_RANGE = GRT_RETURN_DOCTYP ).
    IF GRT_RETURN_DOCTYP IS INITIAL.
      LV_MSG = TEXT-E01.
      REPLACE '&' IN LV_MSG WITH TEXT-T05.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = LV_MSG
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ENDIF.

    ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZCL_SDSSD_CREDITDEBIT_NOTE_SRV'
                                                    IF_PARAM = 'CREDITDEBIT_COND_TYPE'
                                          IMPORTING ET_RANGE = GRT_CREDITDEBIT_COND_TYPE ).

    ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZCL_SDSSD_CREDITDEBIT_NOTE_SRV'
                                                    IF_PARAM = 'COND_TYPE_DELETE'
                                          IMPORTING ET_RANGE = GRT_COND_TYPE_DELETE ).

  ENDMETHOD.


  method PROCESS_DATA.
*CALL METHOD SUPER->PROCESS_DATA
*  EXPORTING
*    IREF_REQUEST_DATA  =
**  IMPORTING
**    EREF_RESPONSE_DATA =
**    EF_STATUS          =
**    EF_MESSAGE         =
**    EF_HTTP_ERROR      =
*    .

    DATA: LS_REQUEST        TYPE ZSDSSDS027,
          LS_SALESORDER     TYPE ZSDSSDS025,
          LT_SALESORDERITEM TYPE ZSDSSDS026_TT,
          LS_RESPONSE	      TYPE ZSDSCAS006.

    FIELD-SYMBOLS: <L_RESPONSE> TYPE ZSDSSDS027.

* Initialize Output
    CLEAR: EF_STATUS,
           EF_MESSAGE,
           EF_HTTP_ERROR.

    LS_REQUEST = IREF_REQUEST_DATA->*.

    ASSIGN EREF_RESPONSE_DATA->* TO <L_RESPONSE>.
    IF SY-SUBRC NE 0.
*   Critical error
      RETURN.
    ENDIF.

    MOVE-CORRESPONDING LS_REQUEST TO LS_SALESORDER.
    MOVE-CORRESPONDING LS_REQUEST-SALESORDERITEM[] TO LT_SALESORDERITEM.

    SALES_ORDER_PROCESS(
      EXPORTING
        IF_TESTRUN        = ''
      IMPORTING
        EF_SALES_DOC      = LS_SALESORDER-SAPSALESORDERNO
        EF_RESPONSE       = LS_RESPONSE
      CHANGING
        CS_SALESORDER     = LS_SALESORDER
        CT_SALESORDERITEM = LT_SALESORDERITEM
        CT_RESPONSE       = <L_RESPONSE>-RESPONSE ).


    MOVE-CORRESPONDING LS_SALESORDER TO <L_RESPONSE>.
    MOVE-CORRESPONDING LT_SALESORDERITEM TO <L_RESPONSE>-SALESORDERITEM.

* Assign Log status from Response structure
    EF_STATUS  = LS_RESPONSE-RESP_STATUS .
    EF_MESSAGE = LS_RESPONSE-RESP_MESSAGE.


  endmethod.


  METHOD SALES_ORDER_CREATE.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_CREDITDEBIT_NOTE_SRV
*  Creation Date      : 08.06.2024
*  Author             : Zulkiff B.(Eviden)
*  Add-on ID          : N/A
*  Description        : CREATE Credit Note/Debit Note
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
*  31.03.2025  420000172   Zulkiff B.  Incorrect condition type when no ref billing
*-----------------------------------------------------------------------
    DATA:
      LT_ITEM                TYPE STANDARD TABLE OF BAPISDITM,
      LT_RETURN              TYPE STANDARD TABLE OF BAPIRET2,
      LT_PARTNERS            TYPE STANDARD TABLE OF BAPIPARNR,
      LT_TEXTS               TYPE STANDARD TABLE OF BAPISDTEXT ##NEEDED,
      LS_BAPE_VBAP           TYPE BAPE_VBAP,
      LS_EXTENSIONIN         TYPE BAPIPAREX,
      LT_EXTENSIONIN         TYPE BAPIPAREX_T,
      LS_HEADER              TYPE BAPISDHD1,
      LT_RETURN_SCHEDULES_IN TYPE STANDARD TABLE OF BAPISCHDL,
      LT_SALES_CONDITIONS_IN TYPE STANDARD TABLE OF BAPICOND,
      LV_SALES_UNIT_CONV     TYPE VRKME,
      LS_RESPONSE            TYPE ZSDSCAS006,
      LV_SALESORG            TYPE VKORG,
      LV_DIVISION            TYPE SPART,
      LT_CONDITION_ITEM      TYPE ZSDSSDS006_TT.

*for extension
    CONSTANTS:
      LC_PART1 TYPE I                    VALUE 240,
      LC_PART2 TYPE I                    VALUE 480,
      LC_PART3 TYPE I                    VALUE 720,
      LC_PART4 TYPE I                    VALUE 960.

    FIELD-SYMBOLS:
                   <LF_STRUCTURE> TYPE ANY.
    DATA:
      LF_OFF1        TYPE I,
      LF_OFF2        TYPE I,
      LF_OFF3        TYPE I,
      LF_OFF4        TYPE I,
      LF_LEN1        TYPE I,
      LF_LEN2        TYPE I,
      LF_LEN3        TYPE I,
      LF_LEN4        TYPE I,
      LF_LENGTH      TYPE I,
      LF_LENGTH1     TYPE I,
      LF_LENGTH2     TYPE I,
      LF_LENGTH3     TYPE I,
      LF_LENGTH4     TYPE I,
      LF_LENGTH_COMP TYPE I,
      LF_FLG_PART2   TYPE FLAG,
      LF_FLG_PART3   TYPE FLAG,
      LF_FLG_PART4   TYPE FLAG.

    DATA: LV_VBRK_I TYPE  VBRK,
          LT_XKOMV  TYPE TABLE OF  KOMV,
          LT_XVBPA  TYPE TABLE OF  VBPAVB,
          LT_XVBRK  TYPE TABLE OF  VBRKVB,
          LT_XVBRP  TYPE TABLE OF  VBRPVB.

    DATA: LOGIC_SWITCH    TYPE BAPISDLS,
          BUSINESS_OBJECT TYPE  BAPIUSW01-OBJTYPE.

    IF LS_HEADER-DOC_TYPE IN GRT_CREDITMEMO_DOCTYP.
      BUSINESS_OBJECT = 'BUS2094'.
    ELSEIF LS_HEADER-DOC_TYPE IN GRT_DEBITMEMO_DOCTYP.
      BUSINESS_OBJECT = 'BUS2096'.
    ELSEIF LS_HEADER-DOC_TYPE IN GRT_RETURN_DOCTYP.
      BUSINESS_OBJECT = 'BUS2102'.
    ENDIF.

*Constants
    READ TABLE GRT_SALESORG INTO DATA(LR_SALESORG) INDEX 1.
    IF SY-SUBRC = 0.
      LV_SALESORG = LR_SALESORG-LOW.
    ENDIF.

    READ TABLE GRT_DIVISION INTO DATA(LR_DIVISION) INDEX 1.
    IF SY-SUBRC = 0.
      LV_DIVISION = LR_DIVISION-LOW.
    ENDIF.
*Read Reference Condition

    IF CS_SALESORDER-REFERENCESDDOCUMENT IS NOT INITIAL.
      LV_VBRK_I-VBELN = CS_SALESORDER-REFERENCESDDOCUMENT.
      CALL FUNCTION 'RV_INVOICE_DOCUMENT_READ'
        EXPORTING
*         ACTIVITY     = '  '
          KONV_READ    = 'X'
*         NO_NAST      = ' '
          VBRK_I       = LV_VBRK_I
*         I_NO_AUTHORITY_CHECK           = ' '
*         OIA_UNZIP_FEES                 =
*         I_GV_ERDAT_SEL                 =
*         IV_NO_PRICING_BUFFER           = ABAP_FALSE
*     IMPORTING
*         VBRK_E       =
*         VBUK_E       =
        TABLES
          XKOMV        = LT_XKOMV
          XVBPA        = LT_XVBPA
          XVBRK        = LT_XVBRK
          XVBRP        = LT_XVBRP
*         XKOMFK       =
*         XVBFS        =
*         XTHEAD       =
*         XVBSS        =
*     CHANGING
*         I_DATA_READ_FROM_ARCHIVE       =
        EXCEPTIONS
          NO_AUTHORITY = 1
          OTHERS       = 2.
      IF SY-SUBRC = 0.
        IF GRT_CREDITDEBIT_COND_TYPE IS NOT INITIAL.
          SELECT KSCHL,KRECH,KMANU
            FROM T685A
           WHERE KAPPL = 'V'
             AND KSCHL IN @GRT_CREDITDEBIT_COND_TYPE
            INTO TABLE @DATA(LT_T685A).
          IF SY-SUBRC = 0.
            SORT LT_T685A BY KSCHL.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
* Header
    LS_HEADER = VALUE #( NAME       = CS_SALESORDER-K2ORDERNO
                         DOC_DATE   = CS_SALESORDER-CREATEDATE
                         REF_DOC_L  = CS_SALESORDER-REFERENCESDDOCUMENT
                         REF_DOC    = CS_SALESORDER-REFERENCESDDOCUMENT
                         REFDOC_CAT = 'M'
                         DOC_TYPE   = CS_SALESORDER-DOCUMENTTYPE
                         SALES_ORG  = LV_SALESORG "Default ‘1000’ SDS Siam Daikin Sale
                         DISTR_CHAN = CS_SALESORDER-DISTRIBUTIONCHANNEL
                         DIVISION   = LV_DIVISION "Default '00' Common
                         SALES_OFF  = CS_SALESORDER-SALESOFFICE
                         SALES_GRP  = CS_SALESORDER-SALESGROUP
                         ORD_REASON = CS_SALESORDER-ORDERREASON
                       ).


* Partner
    "Currency
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = CS_SALESORDER-BPSOLDTONUMBER
      IMPORTING
        OUTPUT = CS_SALESORDER-BPSOLDTONUMBER.
    SELECT SINGLE KUNNR,VKORG,VTWEG,SPART,WAERS
      FROM KNVV
     WHERE KUNNR = @CS_SALESORDER-BPSOLDTONUMBER
       AND VKORG = @LV_SALESORG
       AND VTWEG = '00' "Common
       AND SPART = '00' "Common
      INTO @DATA(LS_KNVV).

    IF CS_SALESORDER-BPSOLDTONUMBER IS NOT INITIAL.
      APPEND VALUE #( PARTN_ROLE = 'AG'
                      PARTN_NUMB = |{ CS_SALESORDER-BPSOLDTONUMBER ALPHA = IN }| ) TO LT_PARTNERS.
    ENDIF.

    IF CS_SALESORDER-BPBILLTONUMBER IS NOT INITIAL.
      APPEND VALUE #( PARTN_ROLE = 'RE'
                      PARTN_NUMB = |{ CS_SALESORDER-BPBILLTONUMBER ALPHA = IN }| ) TO LT_PARTNERS.
    ENDIF.

    IF CS_SALESORDER-BPPAYERNUMBER IS NOT INITIAL.
      APPEND VALUE #( PARTN_ROLE = 'RG'
                      PARTN_NUMB = |{ CS_SALESORDER-BPPAYERNUMBER ALPHA = IN }| ) TO LT_PARTNERS.
    ENDIF.


    IF CS_SALESORDER-BPSHIPTONUMBER IS NOT INITIAL.
      APPEND VALUE #( PARTN_ROLE = 'WE'
                      PARTN_NUMB = |{ CS_SALESORDER-BPSHIPTONUMBER ALPHA = IN }| ) TO LT_PARTNERS.
    ENDIF.


    IF CS_SALESORDER-BPSALESEMPLOYEE IS NOT INITIAL.
      APPEND VALUE #( PARTN_ROLE = 'VE'
                      PARTN_NUMB = |{ CS_SALESORDER-BPSALESEMPLOYEE ALPHA = IN }| ) TO LT_PARTNERS.
    ENDIF.

* HD Texts
    CLEAR: LT_TEXTS.

    IF CS_SALESORDER-HDTEXTINVREMARK IS NOT INITIAL.
      APPEND_TEXT( EXPORTING TEXT_ID   = 'ZH09'
                             LANGU     = 'E'
                             TEXT_LINE = CS_SALESORDER-HDTEXTINVREMARK
                   CHANGING  TEXT = LT_TEXTS ).
    ENDIF.

    IF CS_SALESORDER-HDTEXTREQREMARK IS NOT INITIAL.
      APPEND_TEXT( EXPORTING TEXT_ID   = 'ZH10'
                             LANGU     = 'E'
                             TEXT_LINE = CS_SALESORDER-HDTEXTREQREMARK
                   CHANGING  TEXT = LT_TEXTS ).
    ENDIF.

    IF CS_SALESORDER-HDTEXTCMSHEETNO IS NOT INITIAL.
      APPEND_TEXT( EXPORTING TEXT_ID   = 'ZH04'
                             LANGU     = 'E'
                             TEXT_LINE = CS_SALESORDER-HDTEXTCMSHEETNO
                   CHANGING  TEXT = LT_TEXTS ).
    ENDIF.

    IF CS_SALESORDER-HDTEXTLEGACYINVOICENO IS NOT INITIAL.
      APPEND_TEXT( EXPORTING TEXT_ID   = 'ZH26'
                             LANGU     = 'E'
                             TEXT_LINE = CS_SALESORDER-HDTEXTLEGACYINVOICENO
                   CHANGING  TEXT = LT_TEXTS ).
    ENDIF.

    IF CS_SALESORDER-HDTEXTREFLEGACYINVOICEVALUE IS NOT INITIAL.
      APPEND_TEXT( EXPORTING TEXT_ID   = 'ZH27'
                             LANGU     = 'E'
                             TEXT_LINE = CS_SALESORDER-HDTEXTREFLEGACYINVOICEVALUE
                   CHANGING  TEXT = LT_TEXTS ).
    ENDIF.

    "Add 21/11/2024
    IF CS_SALESORDER-HDTEXTLEGACYINVOICEDATE IS NOT INITIAL.
      APPEND_TEXT( EXPORTING TEXT_ID   = 'ZH28'
                             LANGU     = 'E'
                             TEXT_LINE = CS_SALESORDER-HDTEXTLEGACYINVOICEDATE
                   CHANGING  TEXT = LT_TEXTS ).
    ENDIF.
    CLEAR: LT_ITEM,LT_CONDITION_ITEM.
    LOOP AT CT_SALESORDERITEM ASSIGNING FIELD-SYMBOL(<LFS_ITEM>).

*Items
      "Convert Sales Unit to Internal Value
      IF <LFS_ITEM>-SALESUNIT IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            INPUT          = <LFS_ITEM>-SALESUNIT
          IMPORTING
            OUTPUT         = LV_SALES_UNIT_CONV
          EXCEPTIONS
            UNIT_NOT_FOUND = 1
            OTHERS         = 2.
        IF SY-SUBRC <> 0.
          CLEAR: LV_SALES_UNIT_CONV.
        ENDIF.
      ENDIF.


      APPEND VALUE #( ITM_NUMBER  = <LFS_ITEM>-ITEMNUMBER
                      HG_LV_ITEM  = <LFS_ITEM>-HIGHERLEVELITEM
                      REF_DOC     = CS_SALESORDER-REFERENCESDDOCUMENT
                      REF_DOC_IT  = <LFS_ITEM>-REFERENCESDDOCITEM
                      REF_DOC_CA  = 'M'
*                      MATERIAL    = <LFS_ITEM>-MATERIALNUMBER        "Del- 26.02.2025 IMS420000418
                      MATERIAL_LONG = <LFS_ITEM>-MATERIALNUMBER       "Add+ 26.02.2025 IMS420000418
                      TARGET_QTY  = <LFS_ITEM>-QUANTITY
                      TARGET_QU   = LV_SALES_UNIT_CONV
                      SALES_UNIT  = LV_SALES_UNIT_CONV
                      PLANT       = <LFS_ITEM>-PLANT
                      STORE_LOC   = <LFS_ITEM>-STORAGELOCATION
                      ITEM_CATEG  = <LFS_ITEM>-ITEMCATEGORY
                      WBS_ELEM    = <LFS_ITEM>-WBS
                      )
                      TO LT_ITEM.

      "In case of return -> Use REQ_QTY in structure return_schedules_in to update the quantity
      IF LS_HEADER-DOC_TYPE IN GRT_RETURN_DOCTYP.
        APPEND VALUE #( ITM_NUMBER  = <LFS_ITEM>-ITEMNUMBER
                        SCHED_LINE  = '0001'
                        REQ_QTY     = <LFS_ITEM>-QUANTITY
                        REQ_DATE    = SY-DATUM
                        )
                        TO LT_RETURN_SCHEDULES_IN.
      ENDIF.
*  Conditions

*Condition Type:'ZCD1' SDS Cr./Dr.Note Val
      IF <LFS_ITEM>-PRICEADJUST <> 0.
        APPEND VALUE #( KPOSN = <LFS_ITEM>-ITEMNUMBER
                        KSCHL = 'ZCD1'
                        KBETR = <LFS_ITEM>-PRICEADJUST
                        WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.
      ENDIF.

*Condition from reference billing
      IF GRT_CREDITDEBIT_COND_TYPE IS NOT INITIAL AND
        ( LS_HEADER-DOC_TYPE IN GRT_CREDITMEMO_DOCTYP OR LS_HEADER-DOC_TYPE IN GRT_DEBITMEMO_DOCTYP ).


        LOOP AT LT_XKOMV ASSIGNING FIELD-SYMBOL(<LFS_KOMV>) WHERE KPOSN = <LFS_ITEM>-REFERENCESDDOCITEM
                                                              AND KSCHL IN GRT_CREDITDEBIT_COND_TYPE.
          READ TABLE LT_T685A ASSIGNING FIELD-SYMBOL(<LFS_T685A>) WITH KEY KSCHL = <LFS_KOMV>-KSCHL BINARY SEARCH.
          IF SY-SUBRC = 0.
            IF <LFS_T685A>-KMANU = 'D'. "Manual only
              IF <LFS_T685A>-KRECH = 'A'. "Percentage
                APPEND VALUE #( KPOSN = <LFS_ITEM>-ITEMNUMBER
                                KSCHL = <LFS_KOMV>-KSCHL
                                KBETR = <LFS_KOMV>-KBETR * 10  ) TO LT_CONDITION_ITEM.
              ELSE."Quantity
                APPEND VALUE #( KPOSN = <LFS_ITEM>-ITEMNUMBER
                                KSCHL = <LFS_KOMV>-KSCHL
                                KBETR = <LFS_KOMV>-KBETR
                                WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.
              ENDIF.
            ELSE.
              APPEND VALUE #( ITM_NUMBER = <LFS_ITEM>-ITEMNUMBER
                              COND_TYPE  = <LFS_KOMV>-KSCHL
                              COND_VALUE = <LFS_KOMV>-KBETR
                              CURRENCY = LS_KNVV-WAERS ) TO LT_SALES_CONDITIONS_IN.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

*  Extensions
      CLEAR LS_EXTENSIONIN.
      LS_BAPE_VBAP-POSNR          = <LFS_ITEM>-ITEMNUMBER.
      LS_BAPE_VBAP-ZZ1_LOB_SO_SDI = <LFS_ITEM>-LOB.

      LS_EXTENSIONIN-STRUCTURE = 'BAPE_VBAP'.

      CLEAR: LF_LENGTH,LF_LENGTH1,LF_LENGTH2,LF_LENGTH3,LF_LENGTH4,
             LF_FLG_PART2,LF_FLG_PART3,LF_FLG_PART4.
      ASSIGN LS_BAPE_VBAP TO <LF_STRUCTURE>.
      IF <LF_STRUCTURE> IS ASSIGNED.
        DO.
          ASSIGN COMPONENT SY-INDEX OF STRUCTURE <LF_STRUCTURE> TO FIELD-SYMBOL(<LF_FIELD>).
          DESCRIBE FIELD <LF_FIELD> LENGTH LF_LENGTH_COMP IN CHARACTER MODE.

          IF SY-SUBRC <> 0.
            EXIT.
          ENDIF.
          LF_LENGTH = LF_LENGTH + LF_LENGTH_COMP.
          IF LF_LENGTH <= LC_PART1.
            LF_LENGTH1  = LF_LENGTH.
          ENDIF.
          IF LF_LENGTH >= LC_PART1 AND
             LF_LENGTH <= LC_PART2.
            LF_OFF2 = LF_LENGTH1.
            LF_LENGTH2  = LF_LENGTH.
            LF_FLG_PART2 = ABAP_ON.
          ENDIF.
          IF LF_LENGTH >= LC_PART2 AND
             LF_LENGTH <= LC_PART3.
            LF_OFF3 = LF_LENGTH2.
            LF_LENGTH3  = LF_LENGTH.
            LF_FLG_PART3 = ABAP_ON.
          ENDIF.
          IF LF_LENGTH >= LC_PART3 AND
             LF_LENGTH <= LC_PART4.
            LF_OFF4 = LF_LENGTH3.
            LF_LENGTH4  = LF_LENGTH.
            LF_FLG_PART4 = ABAP_ON.
          ENDIF.
        ENDDO.
        CLEAR: LF_LEN1,LF_LEN2,LF_LEN3,LF_LEN4.
        LF_LEN1 = LF_LENGTH1.
        LF_LEN2 = LF_LENGTH2.
        LF_LEN3 = LF_LENGTH3.
        LF_LEN4 = LF_LENGTH4.

        LS_EXTENSIONIN-VALUEPART1 = <LF_STRUCTURE>+LF_OFF1(LF_LEN1).
        IF LF_FLG_PART2 = ABAP_ON.
          LS_EXTENSIONIN-VALUEPART2 = <LF_STRUCTURE>+LF_OFF2(LF_LEN2).
        ENDIF.
        IF LF_FLG_PART3 = ABAP_ON.
          LS_EXTENSIONIN-VALUEPART3 = <LF_STRUCTURE>+LF_OFF3(LF_LEN3).
        ENDIF.
        IF LF_FLG_PART4 = ABAP_ON.
          LS_EXTENSIONIN-VALUEPART4 = <LF_STRUCTURE>+LF_OFF4(LF_LEN4).
        ENDIF.
      ENDIF.

      APPEND LS_EXTENSIONIN TO LT_EXTENSIONIN.

      CLEAR:
        LV_SALES_UNIT_CONV.

    ENDLOOP.


    "Export to routine 902 IN VOFM  -> FRM_KONDI_WERT_902
    EXPORT LT_CONDITION_ITEM FROM LT_CONDITION_ITEM TO MEMORY ID 'CONDTYP_SO_FROM_SF'.

*-------------------------------------
* Sales Order Create
*-------------------------------------

    LOGIC_SWITCH-NOSTRUCTURE = 'X'.

    IF LS_HEADER-DOC_TYPE IN GRT_CREDITMEMO_DOCTYP OR LS_HEADER-DOC_TYPE IN GRT_DEBITMEMO_DOCTYP.

      CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
        EXPORTING
          SALES_HEADER_IN     = LS_HEADER
          LOGIC_SWITCH        = LOGIC_SWITCH
          BUSINESS_OBJECT     = BUSINESS_OBJECT
          TESTRUN             = IF_TESTRUN
        IMPORTING
          SALESDOCUMENT_EX    = EF_SALES_DOC
        TABLES
          RETURN              = LT_RETURN
          SALES_ITEMS_IN      = LT_ITEM
          SALES_CONDITIONS_IN = LT_SALES_CONDITIONS_IN
          SALES_PARTNERS      = LT_PARTNERS
          SALES_TEXT          = LT_TEXTS
          EXTENSIONIN         = LT_EXTENSIONIN.

    ELSE. "Return

*Check condition if ZPR0 has been found.
*If found send to routine 902 IN VOFM  -> FRM_KONDI_WERT_902 to delete condition
      DATA: LT_CONDITION_ITEM_DEL TYPE ZSDSSDS006_TT.

      CLEAR: LT_CONDITION_ITEM_DEL.

      IF LT_XKOMV IS NOT INITIAL.
        LOOP AT LT_ITEM ASSIGNING FIELD-SYMBOL(<LFS_ITEM2>) ##LOOP_ASSIGN.

          READ TABLE LT_XKOMV ASSIGNING <LFS_KOMV> WITH KEY KPOSN = <LFS_ITEM2>-REF_DOC_IT
                                                                          KSCHL = 'ZPR0'.
          IF SY-SUBRC <> 0.
            APPEND VALUE #( KPOSN = <LFS_ITEM2>-ITM_NUMBER
                            KSCHL = 'ZPR0'
                             ) TO LT_CONDITION_ITEM_DEL.
          ENDIF.
          READ TABLE LT_XKOMV ASSIGNING <LFS_KOMV> WITH KEY KPOSN = <LFS_ITEM2>-REF_DOC_IT
                                                            KSCHL = 'ZCD1'.
          IF SY-SUBRC <> 0.
            READ TABLE LT_CONDITION_ITEM WITH KEY  KPOSN = <LFS_ITEM2>-ITM_NUMBER
                                                   KSCHL = 'ZCD1'
                                                   TRANSPORTING NO FIELDS.
            IF SY-SUBRC <> 0.
              APPEND VALUE #( KPOSN = <LFS_ITEM2>-ITM_NUMBER
                              KSCHL = 'ZCD1'
                               ) TO LT_CONDITION_ITEM_DEL.
            ENDIF.
          ENDIF.
*Del- 420000172 31.03.2025 >>>
*          "Special case : If condition type to delete is found in GENC, Delete it
*          LOOP AT LT_XKOMV ASSIGNING <LFS_KOMV> WHERE KPOSN = <LFS_ITEM2>-REF_DOC_IT
*                                                  AND KSCHL IN GRT_COND_TYPE_DELETE.
*            DATA(LV_FOUND) = 'X'.
*            APPEND VALUE #( KPOSN = <LFS_ITEM2>-ITM_NUMBER
*                            KSCHL = <LFS_KOMV>-KSCHL
*                             ) TO LT_CONDITION_ITEM_DEL.
*          ENDLOOP.
*          IF LV_FOUND = SPACE.
*            LOOP AT LT_XKOMV ASSIGNING <LFS_KOMV> WHERE KPOSN = <LFS_ITEM2>-ITM_NUMBER
*                                                    AND KSCHL IN GRT_COND_TYPE_DELETE.
*
*              APPEND VALUE #( KPOSN = <LFS_ITEM2>-ITM_NUMBER
*                              KSCHL = <LFS_KOMV>-KSCHL
*                               ) TO LT_CONDITION_ITEM_DEL.
*            ENDLOOP.
*          ENDIF.
*<<Del- 420000172 31.03.2025
*Add+ 420000172 31.03.2025 >>>
          LOOP AT GRT_COND_TYPE_DELETE ASSIGNING FIELD-SYMBOL(<LFS_COND_TYPE_DEL>).
            APPEND VALUE #( KPOSN = <LFS_ITEM2>-ITM_NUMBER
                            KSCHL = <LFS_COND_TYPE_DEL>-LOW
                             ) TO LT_CONDITION_ITEM_DEL.
          ENDLOOP.
*<<Add 420000172 31.03.2025
        ENDLOOP.

        "Export to routine 902 IN VOFM  -> FRM_KONDI_WERT_902
*        EXPORT LT_CONDITION_ITEM_DEL FROM LT_CONDITION_ITEM_DEL TO MEMORY ID 'CONDTYP_SO_FROM_SF_DEL'. "Del- 420000172  31.03.2025
*Add+ 420000172 31.03.2025 >>>
      ELSE.
        LOOP AT LT_ITEM ASSIGNING <LFS_ITEM2>.
          LOOP AT GRT_COND_TYPE_DELETE ASSIGNING <LFS_COND_TYPE_DEL>.
            APPEND VALUE #( KPOSN = <LFS_ITEM2>-ITM_NUMBER
                            KSCHL = <LFS_COND_TYPE_DEL>-LOW
                             ) TO LT_CONDITION_ITEM_DEL.
          ENDLOOP.
        ENDLOOP.
*<<Add 420000172 31.03.2025
      ENDIF.
      "Export to routine 902 IN VOFM  -> FRM_KONDI_WERT_902
      EXPORT LT_CONDITION_ITEM_DEL FROM LT_CONDITION_ITEM_DEL TO MEMORY ID 'CONDTYP_SO_FROM_SF_DEL'. "Add+ 420000172  31.03.2025

      CALL FUNCTION 'BAPI_CUSTOMERRETURN_CREATE'
        EXPORTING
          RETURN_HEADER_IN    = LS_HEADER
          LOGIC_SWITCH        = LOGIC_SWITCH
          TESTRUN             = IF_TESTRUN
*         CONVERT             = ' '
        IMPORTING
          SALESDOCUMENT       = EF_SALES_DOC
        TABLES
          RETURN              = LT_RETURN
          RETURN_ITEMS_IN     = LT_ITEM
          RETURN_SCHEDULES_IN = LT_RETURN_SCHEDULES_IN
          RETURN_PARTNERS     = LT_PARTNERS
          RETURN_TEXT         = LT_TEXTS
          EXTENSIONIN         = LT_EXTENSIONIN.

    ENDIF.

    IF LINE_EXISTS( LT_RETURN[ TYPE = 'A' ] ) OR
       LINE_EXISTS( LT_RETURN[ TYPE = 'E' ] ).

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.


    ELSE.

      READ TABLE LT_RETURN WITH KEY TYPE   = 'S'
                                    ID     = 'V1'
                                    NUMBER = '311'
                                    TRANSPORTING NO FIELDS.
      IF SY-SUBRC = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'
*         IMPORTING
*           RETURN        =
          .
      ENDIF.
    ENDIF.

    READ TABLE LT_RETURN INTO DATA(LS_RETURN) WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.
      EF_RESPONSE-RESP_STATUS  = LS_RETURN-TYPE.
      EF_RESPONSE-RESP_MESSAGE = LS_RETURN-MESSAGE.
    ELSE.
      READ TABLE LT_RETURN INTO LS_RETURN WITH KEY TYPE = 'S'
                                                   ID = 'V1'
                                                   NUMBER = '311'.
      IF SY-SUBRC = 0.
        EF_RESPONSE-RESP_STATUS  = LS_RETURN-TYPE.
        EF_RESPONSE-RESP_MESSAGE = LS_RETURN-MESSAGE.
      ELSE.
        READ TABLE LT_RETURN INTO LS_RETURN INDEX 1.
        IF SY-SUBRC = 0.
          EF_RESPONSE-RESP_STATUS  = LS_RETURN-TYPE.
          EF_RESPONSE-RESP_MESSAGE = LS_RETURN-MESSAGE.
        ENDIF.
      ENDIF.
    ENDIF.
    LOOP AT LT_RETURN ASSIGNING FIELD-SYMBOL(<LFS_RETURN>).
      LS_RESPONSE-RESP_STATUS  = <LFS_RETURN>-TYPE.
      LS_RESPONSE-RESP_MESSAGE = <LFS_RETURN>-MESSAGE.
      APPEND LS_RESPONSE TO CT_RESPONSE.
    ENDLOOP.

  ENDMETHOD. "#EC CI_VALPAR


  METHOD SALES_ORDER_PROCESS.

*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_CREDITDEBIT_NOTE_SRV
*  Creation Date      : 08.06.2024
*  Author             : Zulkiff B.(Eviden)
*  Add-on ID          : ZSDI012
*  Description        : Sales Order Create/Change via BAPI
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

    DATA: LV_INVALID TYPE FLAG.

* Get constant from table ZSDSCAC001
    GET_CONSTANTS( CHANGING CT_RESPONSE = CT_RESPONSE ).

    READ TABLE CT_RESPONSE INTO DATA(LS_RESPONSE) WITH KEY RESP_STATUS = GC_ERROR.
    IF SY-SUBRC = 0.
* Assign Log status from Response structure
      EF_RESPONSE-RESP_STATUS  = LS_RESPONSE-RESP_STATUS .
      EF_RESPONSE-RESP_MESSAGE = LS_RESPONSE-RESP_MESSAGE.
      RETURN.
    ENDIF.

* Process input validation
    VALIDATE_INPUT(
      IMPORTING
        EF_INVALID        = LV_INVALID
      CHANGING
        CS_SALESORDER     = CS_SALESORDER
        CT_SALESORDERITEM = CT_SALESORDERITEM
        CT_RESPONSE       = CT_RESPONSE
        ).

    IF LV_INVALID = SPACE. "No Error

      SALES_ORDER_CREATE(
        EXPORTING
          IF_TESTRUN        = IF_TESTRUN
        IMPORTING
          EF_SALES_DOC      = EF_SALES_DOC
          EF_RESPONSE       = EF_RESPONSE
        CHANGING
          CS_SALESORDER     = CS_SALESORDER
          CT_SALESORDERITEM = CT_SALESORDERITEM
          CT_RESPONSE       = CT_RESPONSE ).
    ELSE.
      READ TABLE CT_RESPONSE INTO LS_RESPONSE WITH KEY RESP_STATUS = GC_ERROR.
      IF SY-SUBRC = 0.
* Assign Log status from Response structure
        EF_RESPONSE-RESP_STATUS  = LS_RESPONSE-RESP_STATUS .
        EF_RESPONSE-RESP_MESSAGE = LS_RESPONSE-RESP_MESSAGE.
        RETURN.
      ENDIF.
    ENDIF.
  ENDMETHOD. "#EC CI_VALPAR


  method VALIDATE_AMOUNT.

    CLEAR: CS_RESPONSE.

* Only when value exist
    IF AMOUNT_IN IS NOT INITIAL.

      CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
        EXPORTING
          INPUT      = AMOUNT_IN
*         INTERNAL   = 'X'
*   IMPORTING
*         OUTPUT     =
        EXCEPTIONS
          NO_NUMERIC = 1
          OTHERS     = 2.
      IF SY-SUBRC <> 0.
        "Please input valid amount
        CS_RESPONSE-RESP_STATUS  = GC_ERROR.
        CS_RESPONSE-RESP_MESSAGE = TEXT-E34.
        RETURN.
      ELSE.
        AMOUNT_OUT = AMOUNT_IN.
      ENDIF.

    ENDIF.

  endmethod.


  method VALIDATE_DATE.
    DATA:
      LV_LENGTH  TYPE  I.

    CLEAR: CS_RESPONSE.

* Length?
    LV_LENGTH = STRLEN( DATE_IN ).
    IF LV_LENGTH NE 8.
*   Wrong Date format. Please use format YYYYMMDD
      CS_RESPONSE-RESP_STATUS  = GC_ERROR.
      CS_RESPONSE-RESP_MESSAGE = TEXT-E35.
      RETURN.
    ENDIF.

    IF NOT DATE_IN+6(2) BETWEEN 01 AND 31  ##NUMBER_OK.
*   Wrong Date format. Please use format YYYYMMDD
      CS_RESPONSE-RESP_STATUS  = GC_ERROR.
      CS_RESPONSE-RESP_MESSAGE = TEXT-E35.
      RETURN.
    ENDIF.

* Check month
    IF NOT DATE_IN+4(2) BETWEEN 01 AND 12  ##NUMBER_OK.
*   Wrong Date format. Please use format YYYYMMDD
      CS_RESPONSE-RESP_STATUS  = GC_ERROR.
      CS_RESPONSE-RESP_MESSAGE = TEXT-E35.
      RETURN.
    ENDIF.

* 1-4th digit is year
    IF NOT DATE_IN+0(4) BETWEEN 1900 AND 2200 AND  ##NUMBER_OK
       DATE_IN+0(4) NE '9999'.
*   Wrong Date format. Please use format YYYYMMDD
      CS_RESPONSE-RESP_STATUS  = GC_ERROR.
      CS_RESPONSE-RESP_MESSAGE = TEXT-E35.
      RETURN.
    ENDIF.

* Assign Output
    DATE_OUT = DATE_IN.
  endmethod.


  METHOD VALIDATE_HEADER.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_CREDITDEBIT_NOTE_SRV / VALIDATE_HEADER
*  Creation Date      : 06.06.2024
*  Author             : Zulkiff B. (Eviden)
*  Add-on ID          : ZSDI012
*  Description        : Validate data header data from Salesforce
*  Purpose            : Validate Header
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    DATA: LS_RESPONSE  TYPE	ZSDSCAS006.

    "Salesforce Order ID
    IF CS_SALESORDER-K2ORDERNO IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E02
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ENDIF.

    "Check if duplicate
    IF CS_SALESORDER-K2ORDERNO IS NOT INITIAL.
      SELECT SINGLE BNAME       ##WARN_OK
        FROM VBAK
       WHERE BNAME = @CS_SALESORDER-K2ORDERNO
        INTO @DATA(LV_BNAME)   ##NEEDED.
      IF SY-SUBRC = 0.
        "// Error duplicate
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E03
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ENDIF.
    ENDIF.

    IF CS_SALESORDER-CREATEDATE IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E04
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      "Check date
      VALIDATE_DATE( EXPORTING DATE_IN     = CS_SALESORDER-CREATEDATE
                     IMPORTING DATE_OUT    = CS_SALESORDER-CREATEDATE
                               CS_RESPONSE = LS_RESPONSE
      ).
      IF CS_SALESORDER-CREATEDATE IS INITIAL OR LS_RESPONSE-RESP_STATUS = GC_ERROR.
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = LS_RESPONSE-RESP_MESSAGE
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ENDIF.
    ENDIF.

    IF CS_SALESORDER-SAPSALESORDERNO IS NOT INITIAL.
      "Sales Order No. must be empty for create mode
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E05
                        CHANGING CT_RESPONSE = CT_RESPONSE  ).
    ENDIF.


    "Doc Type
    IF CS_SALESORDER-DOCUMENTTYPE IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E06
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      SELECT SINGLE AUART       ##WARN_OK
        FROM TVAK
       WHERE AUART = @CS_SALESORDER-DOCUMENTTYPE
        INTO @DATA(LV_AUART)    ##NEEDED.
      IF SY-SUBRC <> 0.
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E07
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ENDIF.
    ENDIF.


    "DistributionChannel
    IF CS_SALESORDER-DISTRIBUTIONCHANNEL IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E08
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      SELECT SINGLE VTWEG     ##WARN_OK
        FROM TVTW
       WHERE VTWEG = @CS_SALESORDER-DISTRIBUTIONCHANNEL
        INTO @DATA(LV_VTWEG)  ##NEEDED.
      IF SY-SUBRC <> 0.
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E09
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ENDIF.
    ENDIF.

    "Sales Office
    IF CS_SALESORDER-SALESOFFICE IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E10
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      SELECT SINGLE VKBUR     ##WARN_OK
        FROM TVBUR
       WHERE VKBUR = @CS_SALESORDER-SALESOFFICE
        INTO @DATA(LV_VKBUR)  ##NEEDED.
      IF SY-SUBRC <> 0.
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E11
                          CHANGING CT_RESPONSE = CT_RESPONSE  ).
      ENDIF.
    ENDIF.

    "Sales Group
    IF CS_SALESORDER-SALESGROUP IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E12
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      SELECT SINGLE VKGRP     ##WARN_OK
        FROM TVKGR
       WHERE VKGRP = @CS_SALESORDER-SALESGROUP
        INTO @DATA(LV_VKGRP)   ##NEEDED.
      IF SY-SUBRC <> 0.
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E13
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ENDIF.
    ENDIF.

    IF CS_SALESORDER-ORDERREASON IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E14
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      SELECT SINGLE AUGRU     ##WARN_OK
        FROM TVAU
       WHERE AUGRU = @CS_SALESORDER-ORDERREASON
        INTO @DATA(LV_AUGRU)   ##NEEDED.
      IF SY-SUBRC <> 0.
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E15
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ENDIF.
    ENDIF.


    "Customer Sold-to
    IF CS_SALESORDER-BPSOLDTONUMBER IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E16
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = CS_SALESORDER-BPSOLDTONUMBER
        IMPORTING
          OUTPUT = CS_SALESORDER-BPSOLDTONUMBER.
    ENDIF.

    IF CS_SALESORDER-BPSHIPTONUMBER IS INITIAL.
      "Missing Customer Ship-to
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E17
                        CHANGING CT_RESPONSE = CT_RESPONSE  ).
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = CS_SALESORDER-BPSHIPTONUMBER
        IMPORTING
          OUTPUT = CS_SALESORDER-BPSHIPTONUMBER.
    ENDIF.

    IF CS_SALESORDER-BPBILLTONUMBER IS INITIAL.
      "Missing Customer Bill-to
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E18
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = CS_SALESORDER-BPBILLTONUMBER
        IMPORTING
          OUTPUT = CS_SALESORDER-BPBILLTONUMBER.
    ENDIF.

    IF CS_SALESORDER-BPPAYERNUMBER IS INITIAL.
      "Missing Customer Payer
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E19
                        CHANGING CT_RESPONSE = CT_RESPONSE  ).
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = CS_SALESORDER-BPPAYERNUMBER
        IMPORTING
          OUTPUT = CS_SALESORDER-BPPAYERNUMBER.
    ENDIF.

    IF CS_SALESORDER-BPSALESEMPLOYEE IS INITIAL.
      "Missing Customer Sales Employee
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E20
                        CHANGING CT_RESPONSE = CT_RESPONSE  ).
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = CS_SALESORDER-BPSALESEMPLOYEE
        IMPORTING
          OUTPUT = CS_SALESORDER-BPSALESEMPLOYEE.
    ENDIF.


    READ TABLE CT_RESPONSE WITH KEY RESP_STATUS = GC_ERROR TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      EF_INVALID = 'X'.
    ENDIF.

  ENDMETHOD.


  method VALIDATE_INPUT.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_CREDITDEBIT_NOTE_SRV / VALIDATE_INPUT
*  Creation Date      : 08.06.2024
*  Author             : Zulkiff B. (Eviden)
*  Add-on ID          : ZSDI012
*  Description        : Validate data input from Salesforce
*  Purpose            : Validate Header / Item
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

* Initialize Output
    CLEAR: EF_INVALID.

    "Validate both Create and Change
    VALIDATE_HEADER(
           IMPORTING
             EF_INVALID        = EF_INVALID
           CHANGING
             CS_SALESORDER     = CS_SALESORDER
             CT_RESPONSE       = CT_RESPONSE
       ).

    VALIDATE_ITEM(
       IMPORTING
         EF_INVALID        = EF_INVALID
       CHANGING
         CS_SALESORDER     = CS_SALESORDER
         CT_SALESORDERITEM = CT_SALESORDERITEM
         CT_RESPONSE       = CT_RESPONSE
   ).

    READ TABLE CT_RESPONSE WITH KEY RESP_STATUS = GC_ERROR TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      EF_INVALID = 'X'.
    ENDIF.

  endmethod.


  METHOD VALIDATE_ITEM.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_CREDITDEBIT_NOTE_SRV / VALIDATE_ITEM
*  Creation Date      : 06.06.2024
*  Author             : Zulkiff B. (Eviden)
*  Add-on ID          : ZSDI012
*  Description        : Validate data Item data from Salesforce
*  Purpose            : Validate Header
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    DATA: LS_RESPONSE TYPE ZSDSCAS006,
          LV_MATNR    TYPE MATNR,
          LV_AMOUNT   TYPE CHAR20,
          LV_MEINS    TYPE MEINS,
          LV_MSG      TYPE ZSDSDE_REST_MESSAGE.


    LOOP AT CT_SALESORDERITEM ASSIGNING FIELD-SYMBOL(<LFS_SALESORDERITEM>).

      IF <LFS_SALESORDERITEM>-ITEMNUMBER IS INITIAL.
        "Missing Item Number
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E21
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ELSE.
        CLEAR: LV_AMOUNT .
        LV_AMOUNT = <LFS_SALESORDERITEM>-ITEMNUMBER.
        CONDENSE LV_AMOUNT.
        VALIDATE_AMOUNT( EXPORTING AMOUNT_IN     = LV_AMOUNT
                         IMPORTING AMOUNT_OUT    = LV_AMOUNT
                                   CS_RESPONSE = LS_RESPONSE
                        ).
        IF LS_RESPONSE-RESP_STATUS = GC_ERROR.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = TEXT-E22
                            CHANGING CT_RESPONSE = CT_RESPONSE ).
        ELSE.
          <LFS_SALESORDERITEM>-ITEMNUMBER = LV_AMOUNT.
        ENDIF.
      ENDIF.

      IF <LFS_SALESORDERITEM>-MATERIALNUMBER IS INITIAL.
        "Missing material number
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E23
                          CHANGING CT_RESPONSE = CT_RESPONSE  ).
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            INPUT        = <LFS_SALESORDERITEM>-MATERIALNUMBER
          IMPORTING
            OUTPUT       = LV_MATNR
          EXCEPTIONS
            LENGTH_ERROR = 1
            OTHERS       = 2.
        IF SY-SUBRC <> 0.
          CLEAR: LV_MATNR.
        ENDIF.

        SELECT SINGLE MATNR  ##WARN_OK
          FROM MVKE
         WHERE MATNR = @LV_MATNR
           AND VKORG IN @GRT_SALESORG
         INTO @DATA(LS_MVKE)  ##NEEDED.
        IF SY-SUBRC <> 0.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = TEXT-E24
                            CHANGING CT_RESPONSE = CT_RESPONSE  ).
        ENDIF.
      ENDIF.

      "Quantity
      IF <LFS_SALESORDERITEM>-QUANTITY IS INITIAL.
        "Missing target quantity
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E25
                          CHANGING CT_RESPONSE = CT_RESPONSE  ).
      ELSE.
        CLEAR: LV_AMOUNT .
        LV_AMOUNT = <LFS_SALESORDERITEM>-QUANTITY.
        CONDENSE LV_AMOUNT.
        VALIDATE_AMOUNT( EXPORTING AMOUNT_IN     = LV_AMOUNT
                         IMPORTING AMOUNT_OUT    = LV_AMOUNT
                                   CS_RESPONSE   = LS_RESPONSE
                        ).
        IF LS_RESPONSE-RESP_STATUS = GC_ERROR.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = LS_RESPONSE-RESP_MESSAGE
                            CHANGING CT_RESPONSE = CT_RESPONSE  ).
        ELSE.
          <LFS_SALESORDERITEM>-QUANTITY = LV_AMOUNT.
        ENDIF.
      ENDIF.

      "SalesUnit
      IF <LFS_SALESORDERITEM>-SALESUNIT IS INITIAL.
        "Missing Sales Unit
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E26
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ELSE.
        CLEAR: LV_MEINS.
        LV_MEINS = <LFS_SALESORDERITEM>-SALESUNIT.

* Validate in table
        SELECT SINGLE MSEHI
          INTO LV_MEINS
          FROM T006
         WHERE MSEHI EQ LV_MEINS.
        IF SY-SUBRC NE 0.

          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
            EXPORTING
              INPUT          = <LFS_SALESORDERITEM>-SALESUNIT
            IMPORTING
              OUTPUT         = LV_MEINS
            EXCEPTIONS
              UNIT_NOT_FOUND = 1
              OTHERS         = 2.
          IF SY-SUBRC <> 0.
            CLEAR LV_MEINS.
          ENDIF.
          SELECT SINGLE MSEHI
            INTO LV_MEINS
            FROM T006
           WHERE MSEHI EQ LV_MEINS.
          IF SY-SUBRC NE 0.
            CLEAR: LV_MSG.
*         Invalid unit:
            LV_MSG = |{ TEXT-E27 } { <LFS_SALESORDERITEM>-SALESUNIT }|.
            APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                       IF_MESSAGE = LV_MSG
                              CHANGING CT_RESPONSE = CT_RESPONSE ).
          ENDIF.
        ENDIF.
      ENDIF.

      "Plant
      IF <LFS_SALESORDERITEM>-PLANT IS INITIAL.
        "Missing Plant
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E28
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ELSE.
        SELECT SINGLE WERKS
          FROM T001W
         WHERE WERKS = @<LFS_SALESORDERITEM>-PLANT
          INTO @DATA(LV_WERKS) ##NEEDED.
        IF SY-SUBRC <> 0.
          "Plant does not exist
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = TEXT-E29
                            CHANGING CT_RESPONSE = CT_RESPONSE ).
        ENDIF.
      ENDIF.

      "StorageLocation
      IF <LFS_SALESORDERITEM>-STORAGELOCATION IS NOT INITIAL.
        SELECT SINGLE WERKS,LGORT
          FROM T001L
         WHERE WERKS = @<LFS_SALESORDERITEM>-PLANT
           AND LGORT = @<LFS_SALESORDERITEM>-STORAGELOCATION
          INTO @DATA(LS_T001L) ##NEEDED.
        IF SY-SUBRC <> 0.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = TEXT-E30
                            CHANGING CT_RESPONSE = CT_RESPONSE ).
        ENDIF.
      ENDIF.

      "ItemCategory
      IF <LFS_SALESORDERITEM>-ITEMCATEGORY IS INITIAL.
        "Missing Item Category
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E31
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ELSE.
        SELECT SINGLE PSTYV
          FROM TVAP
         WHERE PSTYV = @<LFS_SALESORDERITEM>-ITEMCATEGORY
          INTO @DATA(LV_PSTYV) ##NEEDED.
        IF SY-SUBRC <> 0.
          "Item Category does not exist
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = TEXT-E32
                            CHANGING CT_RESPONSE = CT_RESPONSE  ).
        ENDIF.
      ENDIF.

      "LOB
      IF <LFS_SALESORDERITEM>-LOB IS NOT INITIAL.
        SELECT SINGLE LOB       ##WARN_OK
          FROM ZDSMMC033
         WHERE LOB = @<LFS_SALESORDERITEM>-LOB
          INTO @DATA(LS_LOB) ##NEEDED.
        IF SY-SUBRC <> 0.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = TEXT-E33
                            CHANGING CT_RESPONSE = CT_RESPONSE ).
        ENDIF.
      ENDIF.


      "PriceAdjust":
      IF <LFS_SALESORDERITEM>-PRICEADJUST IS NOT INITIAL.
        CLEAR: LV_AMOUNT .
        LV_AMOUNT = <LFS_SALESORDERITEM>-PRICEADJUST.
        CONDENSE LV_AMOUNT.
        VALIDATE_AMOUNT( EXPORTING AMOUNT_IN     = LV_AMOUNT
                         IMPORTING AMOUNT_OUT    = LV_AMOUNT
                                   CS_RESPONSE = LS_RESPONSE
                        ).
        IF LS_RESPONSE-RESP_STATUS = GC_ERROR.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = LS_RESPONSE-RESP_MESSAGE
                            CHANGING CT_RESPONSE = CT_RESPONSE ).
        ELSE.
          <LFS_SALESORDERITEM>-PRICEADJUST = LV_AMOUNT.
        ENDIF.
      ENDIF.

    ENDLOOP.

    READ TABLE CT_RESPONSE WITH KEY RESP_STATUS = GC_ERROR TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      EF_INVALID = 'X'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

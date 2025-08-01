*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0070
*  Creation Date      : 02.05.2024
*  Author             : Atitep B.
*  Add-on ID          : N/A
*  Description        : Program Interface Price to Salesforce
*  Purpose            : Interface Price to Salesforce
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSSDR0070.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: A004.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_OUTPUT,
         MATNR TYPE A004-MATNR, "Material
         KSCHL TYPE A004-KSCHL, "Condition Type
         KBETR TYPE KONP-KBETR, "Unit Price
         KONWA TYPE KONP-KONWA, "Currency
         KMEIN TYPE KONP-KMEIN, "Unit
         DATBI TYPE A004-DATBI, "Valid To
         DATAB TYPE A004-DATAB, "Valid From
         MTART TYPE MARA-MTART,
         MATKL TYPE MARA-MATKL,
         VERPR TYPE MBEW-VERPR, "Moving price
         STPRS TYPE MBEW-STPRS, "Stndard price
         BWPRH TYPE MBEW-BWPRH,
         BWPH1 TYPE MBEW-BWPH1,
         VJBWH TYPE MBEW-VJBWH,
       END OF TS_OUTPUT,

       BEGIN OF TS_GENC,
         REPID        TYPE ZSDSCAC001-REPID,
         PARAM        TYPE ZSDSCAC001-PARAM,
         PARAM_EXT    TYPE ZSDSCAC001-PARAM_EXT,
         SEQUENCE     TYPE ZSDSCAC001-SEQUENCE,
         PARAM_SIGN   TYPE ZSDSCAC001-PARAM_SIGN,
         PARAM_OPTION TYPE ZSDSCAC001-PARAM_OPTION,
         VALUE_LOW    TYPE ZSDSCAC001-VALUE_LOW,
         VALUE_HIGH   TYPE ZSDSCAC001-VALUE_HIGH,
       END OF TS_GENC.


*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: GT_OUTPUT TYPE TABLE OF TS_OUTPUT ##NEEDED,
      GT_GENC   TYPE TABLE OF TS_GENC ##NEEDED.


*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA GS_OUTPUT TYPE TS_OUTPUT ##NEEDED.
*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS: GC_INTFNO   TYPE  ZSDSCAC004-INTFNO VALUE 'SDI018'.

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s01: Selection Criteria
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  PARAMETERS: p_DATAB TYPE SY-DATUM DEFAULT SY-DATUM OBLIGATORY.
  SELECT-OPTIONS:
    S_VKORG  FOR  A004-VKORG DEFAULT '1000' NO INTERVALS,
    S_VTWEG  FOR  A004-VTWEG DEFAULT '00' NO INTERVALS,
    S_MATNR  FOR  A004-MATNR.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM VALIDATE_DATA.
  PERFORM GET_DATA.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF GT_OUTPUT[] IS NOT INITIAL.
    PERFORM SEND_DATA.
  ELSE.
    MESSAGE TEXT-002 TYPE 'E'.
  ENDIF.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
*  INCLUDE ZSDSCAI9990 ##INCL_OK.

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form validate_data
*&---------------------------------------------------------------------*
FORM VALIDATE_DATA .
  IF S_MATNR[] IS NOT INITIAL.

    SELECT MATNR
      INTO TABLE @DATA(LT_MATNR)
      FROM MVKE
      WHERE MATNR IN @S_MATNR
      ORDER BY MATNR.
    IF LT_MATNR[] IS INITIAL.
      MESSAGE TEXT-002 TYPE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
FORM GET_DATA .

  DATA: LRT_KSCHL TYPE RANGE OF A004-KSCHL.

  SELECT REPID
         PARAM
         PARAM_EXT
         SEQUENCE
         PARAM_SIGN
         PARAM_OPTION
         VALUE_LOW
         VALUE_HIGH
    FROM ZSDSCAC001
    INTO TABLE GT_GENC
    WHERE REPID = 'ZSDSSDR0070'.

  LRT_KSCHL = VALUE #( FOR RECORD IN GT_GENC WHERE ( PARAM = 'A004_KSCHL' )
                        ( SIGN = RECORD-PARAM_SIGN
                          OPTION = RECORD-PARAM_OPTION
                          LOW = RECORD-VALUE_LOW
                          HIGH = RECORD-VALUE_HIGH )
                      ).

  SELECT A~MATNR
         A~KSCHL
         B~KBETR
         B~KONWA
         B~KMEIN
         A~DATBI
         A~DATAB
         C~MTART
         C~MATKL
         D~VERPR
         D~STPRS
         D~BWPRH
         D~BWPH1
         D~VJBWH
    FROM A004 AS A
    INNER JOIN KONP AS B
    ON A~KNUMH = B~KNUMH
    AND A~KSCHL = B~KSCHL
    INNER JOIN MARA AS C
    ON A~MATNR = C~MATNR
    LEFT OUTER JOIN MBEW AS D
    ON A~MATNR = D~MATNR
    INTO TABLE GT_OUTPUT
    BYPASSING BUFFER
    WHERE A~DATBI >= P_DATAB
      AND A~DATAB <= P_DATAB
      AND A~VKORG IN S_VKORG
      AND A~VTWEG IN S_VTWEG
      AND A~MATNR IN S_MATNR
      AND A~KSCHL IN LRT_KSCHL
      AND B~LOEVM_KO = ''
    ORDER BY A~MATNR.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form send_data
*&---------------------------------------------------------------------*
FORM SEND_DATA .
  DATA: LT_INTF_TXT      TYPE  ZCL_SDSCA_FILE_INTERFACE=>TT_DATATXT,
        LS_RETURN        TYPE  BAPIRET2,
        LF_FIRST(1),
        LF_DATBI(10),
        LF_DATAB(10),
        LF_KBETR(50),
        LF_KMEIN(10),
        LF_FILENAME_I    TYPE  STRING,
        LF_UID1(50),
        LF_UID2(50),
        LF_BOOK_NAME(50).

  DATA: LRT_MTART TYPE RANGE OF MARA-MTART,
        LRT_MATKL TYPE RANGE OF MARA-MATKL.

  LOOP AT GT_OUTPUT ASSIGNING FIELD-SYMBOL(<L_OUTPUT>).
    GS_OUTPUT = <L_OUTPUT>.
    IF GS_OUTPUT-DATBI = '99991231'.
      LF_DATBI = '4000-12-31'.
    ELSE.
      LF_DATBI = |{ GS_OUTPUT-DATBI(4) }-{ GS_OUTPUT-DATBI+4(2) }-{ GS_OUTPUT-DATBI+6(2) }|.
    ENDIF.

    LF_DATAB = |{ GS_OUTPUT-DATAB(4) }-{ GS_OUTPUT-DATAB+4(2) }-{ GS_OUTPUT-DATAB+6(2) }|.
*    WRITE GS_OUTPUT-KBETR to LF_KBETR CURRENCY GS_OUTPUT-KONWA.
    LF_KBETR = |{ GS_OUTPUT-KBETR }|.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        INPUT  = GS_OUTPUT-KMEIN
      IMPORTING
        OUTPUT = LF_KMEIN.
*      EXCEPTIONS
*        UNIT_NOT_FOUND = 1
*        OTHERS         = 2.

    IF LF_FIRST IS INITIAL.
      LOOP AT GT_GENC INTO DATA(LS_GENC) ##INTO_OK.
        CASE LS_GENC-PARAM.
          WHEN 'PRODUCT_UID'.
            CASE LS_GENC-SEQUENCE.
              WHEN '0001'.
                LF_UID1 = LS_GENC-VALUE_LOW.
              WHEN '0002'.
                LF_UID2 = LS_GENC-VALUE_LOW.
            ENDCASE.
          WHEN 'PRICE_BOOK'.
            LF_BOOK_NAME = LS_GENC-VALUE_LOW.
          WHEN 'CHECK_MATERIAL'.
            CASE LS_GENC-PARAM_EXT.
              WHEN 'MAT_TYPE'.
                APPEND VALUE #( SIGN = LS_GENC-PARAM_SIGN
                                OPTION = LS_GENC-PARAM_OPTION
                                LOW = LS_GENC-VALUE_LOW
                                HIGH = LS_GENC-VALUE_HIGH )
                          TO LRT_MTART.

              WHEN 'MAT_GROUP'.
                APPEND VALUE #( SIGN = LS_GENC-PARAM_SIGN
                                OPTION = LS_GENC-PARAM_OPTION
                                LOW = LS_GENC-VALUE_LOW
                                HIGH = LS_GENC-VALUE_HIGH )
                          TO LRT_MATKL.

            ENDCASE.
        ENDCASE.
      ENDLOOP.

      APPEND INITIAL LINE TO LT_INTF_TXT ASSIGNING FIELD-SYMBOL(<L_INTF_TXT>).
      <L_INTF_TXT> = '"ProductCode","ConditionType","UnitPrice","Currency","UnitOfMeasure"'.
      <L_INTF_TXT> = |{ <L_INTF_TXT> },"SDS_Validity_From__c","SDS_Validity_To__c","SDS_PricebookEntry_UID__c"|.
      <L_INTF_TXT> = |{ <L_INTF_TXT> },"Product2.Product_UID__c","Pricebook2.External_ID__c","SDS_Cost__c"|.
      <L_INTF_TXT> = |{ <L_INTF_TXT> },"IsActive","Commercial_Price1__c","Commercial_Price2__c"|.
      <L_INTF_TXT> = |{ <L_INTF_TXT> },"Commercial_Price3__c","Standard_Price__c"|.

      LF_FIRST = 'X'.
    ENDIF.

    APPEND INITIAL LINE TO LT_INTF_TXT ASSIGNING <L_INTF_TXT>.
    <L_INTF_TXT> = |"{ GS_OUTPUT-MATNR }","{ GS_OUTPUT-KSCHL }","{ LF_KBETR }","{ GS_OUTPUT-KONWA }","{ LF_KMEIN }"|.
    <L_INTF_TXT> = |{ <L_INTF_TXT> },"{ LF_DATAB }","{ LF_DATBI }"|.
    <L_INTF_TXT> = |{ <L_INTF_TXT> },"{ LF_UID1 } ({ GS_OUTPUT-KONWA })+{ GS_OUTPUT-MATNR }"|.
    <L_INTF_TXT> = |{ <L_INTF_TXT> },"{ LF_UID1 }+{ LF_UID2 }+{ GS_OUTPUT-MATNR }","{ LF_BOOK_NAME }"|.

    IF GS_OUTPUT-MTART IN LRT_MTART AND GS_OUTPUT-MATKL IN LRT_MATKL.
      <L_INTF_TXT> = |{ <L_INTF_TXT> },"{ GS_OUTPUT-STPRS }"|.
    ELSE.
      <L_INTF_TXT> = |{ <L_INTF_TXT> },"{ GS_OUTPUT-VERPR }"|.
    ENDIF.

    <L_INTF_TXT> = |{ <L_INTF_TXT> },"TRUE","{ GS_OUTPUT-BWPRH }","{ GS_OUTPUT-BWPH1 }","{ GS_OUTPUT-VJBWH }","{ GS_OUTPUT-STPRS }"|.

    CLEAR: LF_DATBI,
           LF_DATAB,
           LF_KBETR,
           LF_KMEIN.
  ENDLOOP.

* Create Interface file
  IF LT_INTF_TXT IS NOT INITIAL.
*   Assign Filename
    LF_FILENAME_I = 'PRICE_' && SY-DATUM  && SY-TIMLO && '.CSV' ##NO_TEXT.
    CALL METHOD ZCL_SDSCA_FILE_INTERFACE=>CREATE_INTERFACE_FILE
      EXPORTING
        IF_INTFNO   = GC_INTFNO
        IF_FILENAME = LF_FILENAME_I
        IT_DATATXT  = LT_INTF_TXT
      IMPORTING
        ES_RETURN   = LS_RETURN.
*        EF_FILEPATH = LF_FILEPATH_O
*        EF_FILENAME = LF_FILENAME_O.
    IF LS_RETURN-TYPE NE 'S'.
*     Error
      MESSAGE ID LS_RETURN-ID TYPE 'I'
              NUMBER LS_RETURN-NUMBER
              DISPLAY LIKE LS_RETURN-TYPE
              WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2
                   LS_RETURN-MESSAGE_V3 LS_RETURN-MESSAGE_V4.
      RETURN.
    ELSE.
      MESSAGE TEXT-001 TYPE 'S'.
    ENDIF.
  ENDIF.
ENDFORM.

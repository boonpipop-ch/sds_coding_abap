*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0150_SUBRUTINE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_HEADER  text
*----------------------------------------------------------------------*
FORM F_GET_DATA  TABLES IT STRUCTURE W_HEADER
                        IT_OUT STRUCTURE W_OUTPUT.

  DATA : TXT_NAME1(100) TYPE C.
  PERFORM F_REPLACE_STAR USING P_NAME1 CHANGING TXT_NAME1.
  FIELD-SYMBOLS : <FS> LIKE LINE OF IT.


  IF P_NAME1 IS NOT INITIAL.
    SELECT
          H~DOCNR
          H~VBELN
          KUNNR
          NAME1
          BSTKD
          AUFNR
          KTEXT
          DOCTYPE
          DDTEXT
          REFTYPE
          PMNTTYP
          SUM( D~NETWR ) AS NETWR
          PRCNT
          VAT
          WHTAX
          DOCDATE
          H~POSDESC
          TOPIC
          ATTEND
          H~ZTERM
          TEXT1
          H~ERNAM
          H~ERDAT
          H~ERTIM
          H~AENAM
          H~AEDAT
          H~AETIM
      INTO CORRESPONDING FIELDS OF TABLE IT
      FROM ZSDSFIT006 AS H
      INNER JOIN ZSDSFIT007 AS D ON H~DOCNR EQ D~DOCNR
      LEFT JOIN DD07T AS DT ON DT~DOMNAME EQ 'ZDOCTYPE_SB'
                            AND H~DOCTYPE EQ DT~DOMVALUE_L
      LEFT JOIN T052U AS U ON H~ZTERM EQ U~ZTERM AND U~SPRAS EQ 'E'
      WHERE H~DOCNR IN S_DOCNR
      AND DOCDATE IN S_DOCDAT
      AND KUNNR IN S_KUNNR
      AND H~VBELN IN S_VBELN
      AND DOCTYPE IN S_TYPE
      AND NAME1 LIKE P_NAME1
      AND BSTKD IN S_BSTKD
      AND AUFNR IN S_AUFNR
      AND H~DFLAG EQ SPACE
      AND D~DFLAG EQ SPACE

      GROUP BY
          H~DOCNR
          H~VBELN
          KUNNR
          NAME1
          BSTKD
          AUFNR
          KTEXT
          DOCTYPE
          DDTEXT
          REFTYPE
          PMNTTYP
          PRCNT
          VAT
          WHTAX
          DOCDATE
          H~POSDESC
          TOPIC
          ATTEND
          H~ZTERM
          TEXT1
          H~ERNAM
          H~ERDAT
          H~ERTIM
          H~AENAM
          H~AEDAT
          H~AETIM
      .
  ELSE.

    SELECT
      H~DOCNR
      H~VBELN
      KUNNR
      NAME1
      BSTKD
      AUFNR
      KTEXT
      DOCTYPE
      DDTEXT
      REFTYPE
      PMNTTYP
      SUM( D~NETWR ) AS NETWR
      PRCNT
      VAT
      WHTAX
      DOCDATE
      H~POSDESC
      TOPIC
      ATTEND
      H~ZTERM
      TEXT1
      H~ERNAM
      H~ERDAT
      H~ERTIM
      H~AENAM
      H~AEDAT
      H~AETIM
  INTO CORRESPONDING FIELDS OF TABLE IT
  FROM ZSDSFIT006 AS H
  INNER JOIN ZSDSFIT007 AS D ON H~DOCNR EQ D~DOCNR
  LEFT JOIN DD07T AS DT ON DT~DOMNAME EQ 'ZDOCTYPE_SB'
                        AND H~DOCTYPE EQ DT~DOMVALUE_L
  LEFT JOIN T052U AS U ON H~ZTERM EQ U~ZTERM AND U~SPRAS EQ 'E'
  WHERE H~DOCNR IN S_DOCNR
  AND DOCDATE IN S_DOCDAT
  AND KUNNR IN S_KUNNR
  AND H~VBELN IN S_VBELN
  AND DOCTYPE IN S_TYPE
  AND BSTKD IN S_BSTKD
  AND AUFNR IN S_AUFNR
  AND H~DFLAG EQ SPACE
  AND D~DFLAG EQ SPACE

  GROUP BY
      H~DOCNR
      H~VBELN
      KUNNR
      NAME1
      BSTKD
      AUFNR
      KTEXT
      DOCTYPE
      DDTEXT
      REFTYPE
      PMNTTYP
      PRCNT
      VAT
      WHTAX
      DOCDATE
      H~POSDESC
      TOPIC
      ATTEND
      H~ZTERM
      TEXT1
      H~ERNAM
      H~ERDAT
      H~ERTIM
      H~AENAM
      H~AEDAT
      H~AETIM
.
  ENDIF.


* Select Config
  IF IT[] IS NOT INITIAL.
    DATA : ITMP TYPE TABLE OF TYP_HEADER.
    ITMP[] = IT[].

    SORT ITMP BY ERNAM AENAM.
    DELETE ADJACENT DUPLICATES FROM ITMP COMPARING ERNAM AENAM.
    DELETE ITMP WHERE ERNAM IS INITIAL AND AENAM IS INITIAL.
    IF ITMP[] IS NOT INITIAL.
      SELECT
*        pernr
        BNAME
        VORNA
        NACHN
        INTO CORRESPONDING FIELDS OF TABLE IT_CFG
        FROM ZSDSFIT008
        FOR ALL ENTRIES IN ITMP[]
        WHERE ( BNAME EQ ITMP-ERNAM OR BNAME EQ ITMP-AENAM )
        AND SPRAS EQ '2'.
    ENDIF.
  ENDIF.

  SORT IT BY DOCNR.
  SORT IT_CFG BY BNAME.
  LOOP AT IT ASSIGNING <FS>.
    <FS>-VAT_AMT = ( <FS>-NETWR * <FS>-VAT ) / 100.
    <FS>-WHT_AMT = ( <FS>-NETWR * <FS>-WHTAX ) / 100.
    MOVE-CORRESPONDING <FS> TO W_OUTPUT.
    W_OUTPUT-G_NETWR = ( W_OUTPUT-NETWR + W_OUTPUT-VAT_AMT ) - W_OUTPUT-WHT_AMT.

*    Aenam
    READ TABLE IT_CFG WITH KEY BNAME = <FS>-AENAM INTO W_CFG.
    IF SY-SUBRC EQ 0.
      CONCATENATE W_CFG-VORNA W_CFG-NACHN INTO W_OUTPUT-AENAM_TXT SEPARATED BY SPACE.
    ENDIF.
    CLEAR W_CFG.

*    Ernam
    READ TABLE IT_CFG WITH KEY BNAME = <FS>-ERNAM INTO W_CFG.
    IF SY-SUBRC EQ 0.
      CONCATENATE W_CFG-VORNA W_CFG-NACHN INTO W_OUTPUT-ERNAM_TXT SEPARATED BY SPACE.
    ENDIF.
    CLEAR W_CFG.

    APPEND W_OUTPUT TO IT_OUT.
    CLEAR W_OUTPUT.
  ENDLOOP.

ENDFORM.                    " F_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  F_REPLACE_STAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_NAME1  text
*      <--P_TXT_NAME1  text
*----------------------------------------------------------------------*
FORM F_REPLACE_STAR  USING    L_NAME1
                     CHANGING V_NAME1.
  REPLACE ALL OCCURRENCES OF '*' IN L_NAME1 WITH '%'.
  CONCATENATE '''' L_NAME1 '''' INTO V_NAME1.

ENDFORM.                    " F_REPLACE_STAR
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_DISPLAY_ALV .

  DATA : TXT_DATE(10) TYPE C,
         TXT_TIME(8) TYPE C.
  TRY.
      CL_SALV_TABLE=>FACTORY(
        IMPORTING
          R_SALV_TABLE = GR_TABLE
        CHANGING
          T_TABLE      =  IT_OUTPUT ).
    CATCH CX_SALV_MSG.                                  "#EC NO_HANDLER
  ENDTRY.

  GR_FUNCTIONS = GR_TABLE->GET_FUNCTIONS( ).
  GR_FUNCTIONS->SET_ALL( 'X' ).

*
  GR_DISPLAY = GR_TABLE->GET_DISPLAY_SETTINGS( ).
  GR_DISPLAY->SET_STRIPED_PATTERN('X').


  GR_COLUMNS = GR_TABLE->GET_COLUMNS( ).
  GR_COLUMNS->SET_OPTIMIZE( 'X' ).

  GR_COLUMNS = GR_TABLE->GET_COLUMNS( ).


  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'DOCNR' ).
  GR_COLUMN->SET_LONG_TEXT('Letter Billing No.' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Letter No.' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Letter No.' ).
  GR_COLUMN->SET_OUTPUT_LENGTH( '12' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'DDTEXT' ).
  GR_COLUMN->SET_LONG_TEXT('Document Type' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Document Type' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Doc Type' ).
  GR_COLUMN->SET_OUTPUT_LENGTH( '10' ).


  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'KTEXT' ).
  GR_COLUMN->SET_LONG_TEXT('Project Name' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Project Name' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Project' ).
  GR_COLUMN->SET_OUTPUT_LENGTH( '40' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'TEXT1' ).
  GR_COLUMN->SET_LONG_TEXT('Terms of Payment' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Pmnt. Terms' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Terms' ).
  GR_COLUMN->SET_OUTPUT_LENGTH( '20' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'NETWR' ).
  GR_COLUMN->SET_LONG_TEXT('Net Amount' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Net Amount' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Net AMT.' ).
  GR_COLUMN->SET_OUTPUT_LENGTH( '17' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'VAT_AMT' ).
  GR_COLUMN->SET_LONG_TEXT('Vat Amount' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Vat Amount' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Vat AMT.' ).
  GR_COLUMN->SET_OUTPUT_LENGTH( '17' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'WHT_AMT' ).
  GR_COLUMN->SET_LONG_TEXT('With Holding Tax Amount' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'WHT Amount' ).
  GR_COLUMN->SET_SHORT_TEXT( 'WHT AMT.' ).
  GR_COLUMN->SET_OUTPUT_LENGTH( '17' ).


  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'G_NETWR' ).
  GR_COLUMN->SET_LONG_TEXT('Grand Total Amount' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Grand Total Amt' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Grand AMT.' ).
  GR_COLUMN->SET_OUTPUT_LENGTH( '17' ).


  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'VAT' ).
  GR_COLUMN->SET_LONG_TEXT('Vat (%)' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Vat (%)' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Vat (%)' ).
  GR_COLUMN->SET_OUTPUT_LENGTH( '10' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'WHTAX' ).
  GR_COLUMN->SET_LONG_TEXT('With Holding Tax (%)' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'WHT (%)' ).
  GR_COLUMN->SET_SHORT_TEXT( 'WHT (%)' ).
  GR_COLUMN->SET_OUTPUT_LENGTH( '10' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'TOPIC' ).
  GR_COLUMN->SET_LONG_TEXT('Topic' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Topic' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Topic' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'ATTEND' ).
  GR_COLUMN->SET_LONG_TEXT('Attention' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Attention' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Attend' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'ERNAM_TXT' ).
  GR_COLUMN->SET_LONG_TEXT('Created by' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Created by' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Created by' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'AENAM_TXT' ).
  GR_COLUMN->SET_LONG_TEXT('Changed by' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Changed by' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Changed by' ).

* HIDE COLUMNS
  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'DOCTYPE' ).
  GR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'ZTERM' ).
  GR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'REFTYPE' ).
  GR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'ERNAM' ).
  GR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'AENAM' ).
  GR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).

* SET COLUMN POSITION
  GR_COLUMNS->SET_COLUMN_POSITION(  COLUMNNAME = 'DDTEXT'
                                    POSITION   = 6 ).

  GR_COLUMNS->SET_COLUMN_POSITION(  COLUMNNAME = 'TEXT1'
                                  POSITION   = 12 ).



***-- Top Of Page
**
  DATA: LO_HEADER  TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
          LO_H_LABEL TYPE REF TO CL_SALV_FORM_LABEL,
          LO_H_FLOW  TYPE REF TO CL_SALV_FORM_LAYOUT_FLOW,
          LV_STR(50).

  CREATE OBJECT LO_HEADER.


*
  LO_H_LABEL = LO_HEADER->CREATE_LABEL( ROW = 1 COLUMN = 1 ).
  LO_H_LABEL->SET_TEXT( SY-TITLE ).


  CLEAR LV_STR.
  CONCATENATE SY-DATUM+6(2) '.' SY-DATUM+4(2) '.' SY-DATUM+0(4) INTO TXT_DATE.
  CONCATENATE SY-UZEIT+0(2) ':' SY-UZEIT+2(2) '.' SY-UZEIT+4(2) INTO TXT_TIME.
  CONCATENATE 'Date: ' TXT_DATE  '  Time: ' TXT_TIME INTO LV_STR RESPECTING BLANKS.
  LO_H_FLOW = LO_HEADER->CREATE_FLOW( ROW = 2  COLUMN = 1 ).
  LO_H_FLOW->CREATE_TEXT( TEXT = LV_STR ).

  LV_ROWS = LINES( IT_HEADER[] ).
  LV_SRWS = LV_ROWS.
  CLEAR LV_STR.
  CONCATENATE 'Number of document : ' LV_SRWS INTO LV_STR RESPECTING BLANKS.
  LO_H_FLOW = LO_HEADER->CREATE_FLOW( ROW = 3  COLUMN = 1 ).
  LO_H_FLOW->CREATE_TEXT( TEXT = LV_STR ).

*  lo_h_flow = lo_header->create_flow( row = 3  column = 1 ).
*  lo_h_flow->create_text( text = 'Number of Records in the output' ).
*  lo_h_flow = lo_header->create_flow( row = 3  column = 2 ).
*  lo_h_flow->create_text( text = 20 ).

  GR_TABLE->SET_TOP_OF_LIST( LO_HEADER ).
*   set the top of list using the header for Print.
  GR_TABLE->SET_TOP_OF_LIST_PRINT( LO_HEADER ).
**


***
* before display
  GR_LAYOUT = GR_TABLE->GET_LAYOUT( ).
  GR_LAYOUT->SET_DEFAULT( CL_SALV_LAYOUT=>TRUE ).  " allow save default variant
  GR_KEY-REPORT = SY-REPID.
  GR_LAYOUT->SET_KEY( GR_KEY ).
  GR_LAYOUT->SET_SAVE_RESTRICTION( CL_SALV_LAYOUT=>RESTRICT_NONE ).

  GR_TABLE->DISPLAY( ).
ENDFORM.                    " F_DISPLAY_ALV

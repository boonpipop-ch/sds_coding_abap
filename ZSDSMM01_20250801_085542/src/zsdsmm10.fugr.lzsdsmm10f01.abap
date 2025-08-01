*----------------------------------------------------------------------*
***INCLUDE LZSDSMM10F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_SUBMIT_PO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_SUBMIT_PO .
  DATA : LS_DATA LIKE LINE OF GT_DATA.

*  READ TABLE GT_DATA INTO LS_DATA INDEX 1.
*  IF ls_Data-EBELN IS NOT INITIAL.
  IF ZCL_IM_SDS_ME_GUI_PO_CUST=>PO_NO IS NOT INITIAL.
    SUBMIT ZSDSMMR0340 USING SELECTION-SCREEN 1000
                       WITH P_EBELN EQ ZCL_IM_SDS_ME_GUI_PO_CUST=>PO_NO
                       AND RETURN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CHECK_STATUS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_CHECK_STATUS.

  DATA : LV_EBELN TYPE EKKO-EBELN,
         LV_DATE  TYPE C LENGTH 10,
         LV_TIME  TYPE C LENGTH 8.

  DATA : LV_MESSAGE TYPE CHAR255.

  LV_EBELN = ZCL_IM_SDS_ME_GUI_PO_CUST=>PO_NO.

  SELECT SINGLE STATU,
                ERNAM,
                ERDAT,
                ERZET,
                FLAGD
    FROM ZSDSMMT002
    INTO @DATA(LS_DATA)
    WHERE EBELN EQ @LV_EBELN
      AND RUNNG EQ ( SELECT MAX( RUNNG )
                       FROM ZSDSMMT002
                       WHERE EBELN EQ @LV_EBELN ).

  IF LS_DATA-FLAGD EQ ABAP_TRUE.
    IF LV_EBELN IS NOT INITIAL.
      LV_MESSAGE = TEXT-105.
      MESSAGE LV_MESSAGE TYPE 'I'.
    ENDIF.
  ELSE.
    IF     LS_DATA-STATU EQ 'SUB'.
      LV_MESSAGE = TEXT-101.
      MESSAGE LV_MESSAGE TYPE 'I'.
    ELSEIF LS_DATA-STATU EQ 'WAI'.
      WRITE LS_DATA-ERDAT TO LV_DATE.
      WRITE LS_DATA-ERZET TO LV_TIME.
      CONCATENATE TEXT-102
                  LS_DATA-ERNAM
                  TEXT-103
                  LV_DATE
                  LV_TIME
             INTO LV_MESSAGE SEPARATED BY SPACE.
      MESSAGE LV_MESSAGE TYPE 'I'.
    ELSEIF LS_DATA-STATU EQ 'COM'.
      LV_MESSAGE = TEXT-104.
      MESSAGE LV_MESSAGE TYPE 'I'.
    ELSE.
      IF LV_EBELN IS NOT INITIAL.
        LV_MESSAGE = TEXT-105.
        MESSAGE LV_MESSAGE TYPE 'I'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_UNRELEASE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_UNRELEASE.
  DATA : LV_MESSAGE TYPE CHAR255.

  CONSTANTS : BEGIN OF LC_CON,
                COM TYPE C LENGTH 3 VALUE 'COM',
                I   TYPE C LENGTH 1 VALUE 'I',
                EQ  TYPE C LENGTH 2 VALUE 'EQ',
              END OF LC_CON.

  RANGES : LR_EBELN FOR EKKO-EBELN.

  IF ZCL_IM_SDS_ME_GUI_PO_CUST=>PO_NO IS NOT INITIAL.
    SELECT COUNT( * )
      FROM ZSDSMMT002
      WHERE STATU EQ @LC_CON-COM
        AND FLAGD EQ @SPACE.
    IF SY-SUBRC EQ 0.

      LR_EBELN[] =  VALUE #( ( SIGN  = LC_CON-I OPTION = LC_CON-EQ LOW = ZCL_IM_SDS_ME_GUI_PO_CUST=>PO_NO ) ).

      SUBMIT ZSDSMMR0020 USING SELECTION-SCREEN 1000
                         WITH S_EBELN IN LR_EBELN[]
                         AND RETURN.

*      SELECT COUNT( * )
*        FROM ZSDSMMT002
*        WHERE EBELN EQ ZCL_IM_SDS_ME_GUI_PO_CUST=>PO_NO
*          AND FLAGD EQ SPACE.
*      IF SY-SUBRC NE 0.
*        LV_MESSAGE = TEXT-106.
*        MESSAGE LV_MESSAGE TYPE 'I'.
*      ELSE.
*        LV_MESSAGE = TEXT-107.
*        MESSAGE LV_MESSAGE TYPE 'I'.
*      ENDIF.
    ELSE.
      LV_MESSAGE = TEXT-108.
    ENDIF.
  ENDIF.
ENDFORM.

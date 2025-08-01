*&---------------------------------------------------------------------*
*& Include          ZSDSFII0030_MODULE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA : PARA(15) TYPE C,
         LV_CONFIRMATION.
  DATA: BDCDATA_WA  TYPE BDCDATA,
        BDCDATA_TAB TYPE TABLE OF BDCDATA.
  DATA: MESSTAB TYPE TABLE OF BDCMSGCOLL.
  DATA: LV_CHECK_SAVE TYPE C.
  CONSTANTS LC_TCODE TYPE C LENGTH 5 VALUE 'ZVA22'.
  PERFORM F_UPDATE_AMOUNT_HEADER.
  CASE SY-UCOMM.
    WHEN 'SAVE'.
      IF GS_HEADER-BSTNK = '-'.
        MESSAGE I000 WITH 'Please check PO Number'.
      ENDIF.
      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          TEXTLINE1 = TEXT-103
          TITEL     = TEXT-104
        IMPORTING
          ANSWER    = LV_CONFIRMATION.
      IF LV_CONFIRMATION = 'J'.
        PERFORM F_CHECK_SAVE CHANGING LV_CHECK_SAVE.
        IF LV_CHECK_SAVE NE GC_ERROR.
          PERFORM F_SAVE_DATA.
          MESSAGE S000 WITH TEXT-101.
          IF GV_STATUS IS INITIAL. "Leave to selection screen only status Create.
            LEAVE TO SCREEN 0.
          ENDIF.
        ELSE.
          MESSAGE I000 WITH TEXT-100 DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.
    WHEN 'ALL'.
      PERFORM F_SELECT_ALL.
    WHEN 'DES'.
      PERFORM F_NON_SELECT.
    WHEN 'DEL'.
      PERFORM F_DELETE_DATA.
      PERFORM F_UPDATE_AMOUNT_HEADER.
    WHEN 'INS'.
      PERFORM F_INSERT_DATA.
    WHEN 'DELETE'.
      PERFORM F_DELETE_DOCUMENT.
      MESSAGE S000 WITH TEXT-101.
      LEAVE TO SCREEN 0.
  ENDCASE.

  GET CURSOR FIELD GV_CURSOR.
  CLEAR SY-UCOMM.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_UPDATE_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_UPDATE_DATA INPUT.
  MODIFY GT_DETAIL FROM GS_DETAIL INDEX COLTROL_QT-CURRENT_LINE.
ENDMODULE.                 " M_UPDATE_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CALC_AMOUNT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CALC_AMOUNT INPUT.
  DATA : LS_DETAIL LIKE GS_DETAIL.
  DATA : LV_TABIX  TYPE SY-TABIX,
         LV_PRBOM  LIKE GS_DETAIL-PRBOM.
  IF GS_DETAIL-KOWRR = GC_TRUE.
    LOOP AT GT_DETAIL INTO LS_DETAIL WHERE UEPOS EQ GS_DETAIL-POSNR.
      LV_TABIX = SY-TABIX.
      GS_HEADER-NETWR   = GS_HEADER-NETWR - LS_DETAIL-NETWR.
      GS_HEADER-VTAMT   = GS_HEADER-VTAMT - LS_DETAIL-MWSBP.
*--------------------------------------------------------------------*
      LS_DETAIL-KWMENG  = GS_DETAIL-KWMENG.
      LS_DETAIL-NETWR   = LS_DETAIL-NETPR * LS_DETAIL-KWMENG.
      LS_DETAIL-MWSBP   = ( ( LS_DETAIL-NETWR * GS_HEADER-VTPCT ) / 100 ).
      LS_DETAIL-NETWR_V = LS_DETAIL-NETWR + ( ( LS_DETAIL-NETWR * GS_HEADER-VTPCT ) / 100 ).
*--------------------------------------------------------------------*
      GS_HEADER-NETWR   = GS_HEADER-NETWR + LS_DETAIL-NETWR.
      GS_HEADER-VTAMT   = GS_HEADER-VTAMT + LS_DETAIL-MWSBP.
      GS_HEADER-TOTAL   = GS_HEADER-NETWR + GS_HEADER-VTAMT.

      MODIFY GT_DETAIL FROM LS_DETAIL INDEX LV_TABIX
                                      TRANSPORTING KWMENG NETWR MWSBP NETWR_V NETPR.
      CLEAR LS_DETAIL.
    ENDLOOP.
    MODIFY GT_DETAIL FROM GS_DETAIL INDEX COLTROL_QT-CURRENT_LINE.
  ELSE.
    GS_DETAIL-NETWR   = GS_DETAIL-NETPR * GS_DETAIL-KWMENG.
    GS_DETAIL-MWSBP   = ( ( GS_DETAIL-NETWR * GS_HEADER-VTPCT ) / 100 ).
    GS_DETAIL-NETWR_V = GS_DETAIL-NETWR + ( ( GS_DETAIL-NETWR * GS_HEADER-VTPCT ) / 100 ).
    MODIFY GT_DETAIL FROM GS_DETAIL INDEX COLTROL_QT-CURRENT_LINE
                                    TRANSPORTING KWMENG NETWR MWSBP NETWR_V NETPR.

    CLEAR GS_SUM_BOM.

    IF GS_DETAIL-UEPOS IS NOT INITIAL AND GS_DETAIL-UPMAT IS NOT INITIAL.
      LOOP AT GT_DETAIL INTO LS_DETAIL WHERE UEPOS EQ GS_DETAIL-UEPOS
                                         AND UPMAT EQ GS_DETAIL-UPMAT.
        ADD LS_DETAIL-NETPR TO LV_PRBOM.

        LS_DETAIL-PRBOM  = LV_PRBOM.
        GS_SUM_BOM-PRBOM = LV_PRBOM.
        GS_SUM_BOM-UEPOS = LS_DETAIL-UEPOS.
        GS_SUM_BOM-MATNR = GS_DETAIL-UPMAT.

        MODIFY GT_DETAIL FROM LS_DETAIL TRANSPORTING PRBOM
                                               WHERE POSNR = GS_DETAIL-UEPOS
                                                 AND MATNR = GS_DETAIL-UPMAT.

        MODIFY GT_SUM_BOM FROM GS_SUM_BOM TRANSPORTING PRBOM
                                                 WHERE UEPOS = GS_DETAIL-UEPOS
                                                   AND MATNR = GS_DETAIL-UPMAT.

      ENDLOOP.
    ENDIF.
    CLEAR LV_PRBOM.
  ENDIF.
ENDMODULE.                 " M_CALC_AMOUNT  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'Z_STATUS'.

  DATA: LT_FCODE TYPE TABLE OF SY-UCOMM,
        LS_FCODE TYPE SY-UCOMM.
*  SET TITLEBAR 'xxx'.
  CLEAR LT_FCODE[].

  IF GV_STATUS IS INITIAL.
    LS_FCODE = 'CHE'.
    APPEND LS_FCODE TO LT_FCODE.
    LS_FCODE = 'DIS'.
    APPEND LS_FCODE TO LT_FCODE.
    LS_FCODE = 'DELETE'.
    APPEND LS_FCODE TO LT_FCODE.
    SET PF-STATUS 'Z_STATAUS' EXCLUDING LT_FCODE.
  ELSEIF GV_STATUS = 'D'.
    LS_FCODE = 'SAVE'.
    APPEND LS_FCODE TO LT_FCODE.
    LS_FCODE = 'DELETE'.
    APPEND LS_FCODE TO LT_FCODE.
    SET PF-STATUS 'Z_STATAUS' EXCLUDING LT_FCODE.
  ELSE.
    SET PF-STATUS 'Z_STATAUS'.
  ENDIF.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'OUT'.
      LEAVE PROGRAM.
    WHEN 'CANC'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0101 OUTPUT.
  SET PF-STATUS 'Z_REASON'.
  SET TITLEBAR 'Z_REA'.

ENDMODULE.                 " STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_MODE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CHECK_MODE INPUT.
  DATA : P_STATUS TYPE C.

  CONSTANTS : BEGIN OF LC_CONFIRMATION,
    YES    TYPE C VALUE 'J',
    NO     TYPE C VALUE 'N',
    CANCEL TYPE C VALUE 'A',
  END OF LC_CONFIRMATION.

  CONSTANTS : LC_DISPLAY TYPE C VALUE 'D',
              LC_CHANGE  TYPE C VALUE 'C'.
  IF SY-UCOMM = 'CHE'.

    PERFORM F_GET_REASON CHANGING LV_CONFIRMATION.

    IF LV_CONFIRMATION = LC_CONFIRMATION-YES AND SY-UCOMM NE 'CA'.
      CALL TRANSACTION 'ZT_SD_CHA_PO_REF'.
      IMPORT P_STATUS FROM MEMORY ID 'STATUS_CHA_PO_REF'.
      FREE MEMORY ID 'STATUS_CHA_PO_REF'.
      IF P_STATUS IS NOT INITIAL.
        GV_STA_LOG = GV_STATUS.
        GV_STATUS  = P_STATUS.
      ENDIF.
    ELSEIF SY-UCOMM EQ 'CA'.
      GV_STA_LOG = LC_CHANGE.
      GV_STATUS  = LC_DISPLAY.
    ELSEIF LV_CONFIRMATION = LC_CONFIRMATION-NO.

    ELSEIF LV_CONFIRMATION = LC_CONFIRMATION-CANCEL.
      GV_STA_LOG = LC_CHANGE.
      GV_STATUS  = LC_DISPLAY.
    ENDIF.

  ELSEIF SY-UCOMM = 'DIS'.
    GV_STA_LOG = GV_STATUS.
    GV_STATUS  = LC_DISPLAY.
  ENDIF.

ENDMODULE.                 " M_CHECK_MODE  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.
  CASE SY-UCOMM.
    WHEN 'SA'.
      IF GV_ZCUSTOMER_REASON IS NOT INITIAL OR
         GV_ZUSER_REASON IS NOT INITIAL.
        GV_STATUS  = LC_CHANGE.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE S000 WITH 'Please select reason.' DISPLAY LIKE 'E'.
      ENDIF.
    WHEN 'CA'.
      GV_STATUS  = LC_DISPLAY.
      GV_STA_LOG = LC_CHANGE.
      LEAVE TO SCREEN 0.
    WHEN 'EX'.
      GV_STATUS  = LC_DISPLAY.
      GV_STA_LOG = LC_CHANGE.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR SY-UCOMM.
ENDMODULE.                 " USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_INPUT_MAT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_INPUT_MAT INPUT.

  DATA : LV_MATERIAL  TYPE RC29L-MATNR,
         LV_PLANT     TYPE RC29L-WERKS,
         LV_BOM_USAGE TYPE RC29L-STLAN.

  DATA : LS_FL_WARNING TYPE	CAPIFLAG-FLWARNING,
         LS_RETURN     TYPE BAPIRET2.

  DATA : LT_BOM_HEADER                  TYPE TABLE OF STKO_API02,
         LT_BOM_ITEM                    TYPE TABLE OF STPO_API02,
         LT_DOCUMENTDATA                TYPE TABLE OF DMU_DOCUMENT,
         LT_DOCUMENTFILES	              TYPE TABLE OF DMU_FILE,
         LT_DMUPOSITIONOBJECTS          TYPE TABLE OF DMU_POSOBJ,
         LT_DMUMATRICES	                TYPE TABLE OF DMU_MATRIX,
         LT_MULTIPLE_APPLICATION_FILTER	TYPE TABLE OF BAPI_DOC_APPLICATIONS.
  DATA : LV_LINE TYPE I.

  LV_MATERIAL  = GS_DETAIL-MATNR.
  LV_PLANT     = '1000'.
  LV_BOM_USAGE = '5'.
  CALL FUNCTION 'DMU_MAT_BOM_READ'
    EXPORTING
      MATERIAL                    = LV_MATERIAL
      PLANT                       = LV_PLANT
      BOM_USAGE                   = LV_BOM_USAGE
      GET_SINGLE_FILE             = GC_TRUE
    IMPORTING
      FL_WARNING                  = LS_FL_WARNING
      RETURN                      = LS_RETURN
    TABLES
      BOM_HEADER                  = LT_BOM_HEADER
      BOM_ITEM                    = LT_BOM_ITEM
      DOCUMENTDATA                = LT_DOCUMENTDATA
      DOCUMENTFILES               = LT_DOCUMENTFILES
      DMUPOSITIONOBJECTS          = LT_DMUPOSITIONOBJECTS
      DMUMATRICES                 = LT_DMUMATRICES
      MULTIPLE_APPLICATION_FILTER = LT_MULTIPLE_APPLICATION_FILTER.
*--------------------------------------------------------------------*
  IF COLTROL_QT-CURRENT_LINE GT GV_CHECK_CHANGE_ITEM.
    IF LS_RETURN-TYPE   = 'E'   AND
       LS_RETURN-ID     = 'M3'  AND
       LS_RETURN-NUMBER = '305'.
*--------------------------------------------------------------------*
      MESSAGE E000 WITH LS_RETURN-MESSAGE.
*--------------------------------------------------------------------*
    ELSEIF LS_RETURN IS INITIAL.
*--------------------------------------------------------------------*
      PERFORM F_GET_BOM USING LV_MATERIAL
                              LT_BOM_ITEM.
*--------------------------------------------------------------------*
    ELSE.
*--------------------------------------------------------------------*

      LV_LINE = COLTROL_QT-CURRENT_LINE.
      PERFORM F_CHECK_LINE CHANGING LV_LINE.
      MODIFY GT_DETAIL FROM GS_DETAIL INDEX LV_LINE TRANSPORTING MATNR.

      IF     GS_HEADER-VTWEG = '10'.
        PERFORM F_GET_PRICE_DEALER USING GS_DETAIL-MATNR.
        PERFORM F_GET_POSNR USING LV_LINE.
        PERFORM F_GET_MAT_DEST USING GS_DETAIL-MATNR.
        MODIFY GT_DETAIL FROM GS_DETAIL INDEX LV_LINE
                                        TRANSPORTING NETPR MEINS MWSBP NETWR_V AUFNR KTEXT POSNR.

      ELSEIF GS_HEADER-VTWEG = '20' OR GS_HEADER-VTWEG = '40'.
        PERFORM F_GET_PRICE_GEN_SER USING GS_DETAIL-MATNR.
        PERFORM F_GET_POSNR USING LV_LINE.
        PERFORM F_GET_MAT_DEST USING GS_DETAIL-MATNR.
        MODIFY GT_DETAIL FROM GS_DETAIL INDEX LV_LINE
                                        TRANSPORTING NETPR MEINS MWSBP NETWR_V AUFNR KTEXT POSNR.
      ENDIF.

      GET CURSOR LINE GV_LINE.
      CLEAR GV_FLAG.
*--------------------------------------------------------------------*
    ENDIF.
*--------------------------------------------------------------------*
  ELSE.
    PERFORM F_MODIFY_LINE TABLES LT_BOM_ITEM
                           USING LS_RETURN
                                 LV_MATERIAL.

  ENDIF.
ENDMODULE.                 " M_INPUT_MAT  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_LINE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CHECK_LINE INPUT.
  MODIFY GT_DETAIL FROM GS_DETAIL INDEX COLTROL_QT-CURRENT_LINE
                                  TRANSPORTING SEL.
ENDMODULE.                 " M_CHECK_LINE  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_UPDATE_DESC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_UPDATE_DESC INPUT.
  DATA : BEGIN OF LS_MAKT,
    MATNR TYPE MAKT-MATNR,
    MAKTX TYPE MAKT-MAKTX,
  END OF LS_MAKT.
  DATA LT_MAKT LIKE TABLE OF LS_MAKT.

  DATA : BEGIN OF LS_MARA,
    MATNR TYPE MARA-MATNR,
    MEINS TYPE MARA-MEINS,
  END OF LS_MARA.
  DATA LT_MARA LIKE TABLE OF LS_MARA.

  DATA LS_DETAIL_DEST LIKE GS_DETAIL.

  LS_DETAIL_DEST-VBELN = GS_HEADER-VBELN.

  MODIFY GT_DETAIL FROM LS_DETAIL_DEST TRANSPORTING VBELN
                                       WHERE VBELN IS INITIAL
                                         AND MATNR IS NOT INITIAL.

  CLEAR LS_DETAIL_DEST.
  IF GR_MATNR[] IS NOT INITIAL.
    SELECT MATNR
           MAKTX
      FROM MAKT
      INTO TABLE LT_MAKT
      WHERE MATNR IN GR_MATNR
        AND SPRAS EQ SY-LANGU.

    SELECT MATNR
           MEINS
      FROM MARA
      INTO TABLE LT_MARA
      WHERE MATNR IN GR_MATNR.

    LOOP AT GT_DETAIL INTO LS_DETAIL_DEST WHERE MATNR IS NOT INITIAL
                                            AND MAKTX IS INITIAL.
      LV_TABIX = SY-TABIX.

      READ TABLE LT_MAKT INTO LS_MAKT
      WITH KEY MATNR = LS_DETAIL_DEST-MATNR.
      IF SY-SUBRC = 0.
        LS_DETAIL_DEST-MAKTX = LS_MAKT-MAKTX.
      ENDIF.

      READ TABLE LT_MARA INTO LS_MARA
      WITH KEY MATNR = LS_DETAIL_DEST-MATNR.
      IF SY-SUBRC = 0.
        LS_DETAIL_DEST-MEINS = LS_MARA-MEINS.
      ENDIF.

      MODIFY GT_DETAIL FROM LS_DETAIL_DEST INDEX LV_TABIX
                                    TRANSPORTING MAKTX MEINS.
      CLEAR : LS_DETAIL_DEST,LS_MAKT.
    ENDLOOP.
  ENDIF.

  PERFORM F_GET_DESC_IO.

ENDMODULE.                 " M_UPDATE_DESC  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CLEAR_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CLEAR_DATA OUTPUT.
  CLEAR : GR_MATNR[],GV_CHECK_CHANGE_ITEM,GS_DETAIL-CM.
  GS_HEADER-AUDAT = SY-DATUM.
ENDMODULE.                 " M_CLEAR_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  M_CLOSE_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CLOSE_SCREEN OUTPUT.
  IF GV_STATUS = 'D'.
    LOOP AT SCREEN.
      SCREEN-INPUT    = 0.
      SCREEN-REQUIRED = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ELSEIF GV_STATUS = 'C'.
    LOOP AT SCREEN.
      IF SCREEN-NAME = 'GS_HEADER-KUNNR' OR
         "screen-name = 'GS_HEADER-BSTNK' OR
         SCREEN-NAME = 'GS_HEADER-VBELN' OR
         SCREEN-NAME = 'GS_HEADER-IHREZ'.

        SCREEN-INPUT    = 0.
        SCREEN-REQUIRED = 0.
        MODIFY SCREEN.
      ENDIF.

      IF GS_DETAIL-CM = 'X'.
        IF SCREEN-NAME = 'GS_DETAIL-KWMENG'
        OR SCREEN-NAME = 'GS_DETAIL-PRBOM'
        OR SCREEN-NAME = 'GS_DETAIL-MATNR'
        OR SCREEN-NAME = 'GS_DETAIL-NETPR'
        OR SCREEN-NAME = 'GS_DETAIL-MAKTX'
        OR SCREEN-NAME = 'GS_DETAIL-CM'
*        OR screen-name = 'GS_DETAIL-SEL'
        OR SCREEN-NAME = 'GS_DETAIL-LAND'.
          SCREEN-INPUT    = 0.
          SCREEN-REQUIRED = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.



  ENDIF.

ENDMODULE.                 " M_CLOSE_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  M_CLEAR_DATA_INPUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CLEAR_DATA_INPUT INPUT.
  CLEAR GS_DETAIL.
ENDMODULE.                 " M_CLEAR_DATA_INPUT  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CLOSE_FILED_COMPANANCE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CLOSE_FILED_COMPANANCE INPUT.
  IF GS_DETAIL-UEPOS IS NOT INITIAL.
    LOOP AT SCREEN.
      IF SCREEN-NAME = 'GS_DETAIL-KWMENG'.
        SCREEN-INPUT    = 0.
        SCREEN-REQUIRED = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " M_CLOSE_FILED_COMPANANCE  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CLOSE_FILED_COMPANANCE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CLOSE_FILED_COMPANANCE OUTPUT.
  CONSTANTS : LC_KOWRR TYPE C VALUE 'X'.
  IF GS_DETAIL-UEPOS IS NOT INITIAL.
    LOOP AT SCREEN.
      IF SCREEN-NAME = 'GS_DETAIL-KWMENG'
      OR SCREEN-NAME = 'GS_DETAIL-PRBOM'
      OR SCREEN-NAME = 'GS_DETAIL-MATNR'.
        SCREEN-INPUT    = 0.
        SCREEN-REQUIRED = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF GS_DETAIL-KOWRR EQ LC_KOWRR.
    LOOP AT SCREEN.
      IF SCREEN-NAME = 'GS_DETAIL-NETPR'.
        SCREEN-INPUT    = 0.
        SCREEN-REQUIRED = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF GS_DETAIL-KOWRR NE LC_KOWRR.
    LOOP AT SCREEN.
      IF SCREEN-NAME = 'GS_DETAIL-PRBOM'.
        SCREEN-INPUT    = 0.
        SCREEN-REQUIRED = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " M_CLOSE_FILED_COMPANANCE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.
  IF GV_STA_LOG IS NOT INITIAL.
    GV_STATUS = GV_STA_LOG.
    CLEAR GV_STA_LOG.
  ENDIF.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      CLEAR GV_REASON.
      LEAVE TO SCREEN 0.
    WHEN 'OUT'.
      CLEAR GV_REASON.
      LEAVE PROGRAM.
    WHEN 'CANC'.
      CLEAR GV_REASON.
      LEAVE PROGRAM.
  ENDCASE.

  CLEAR SY-UCOMM.
ENDMODULE.                 " USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_VAT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_VAT_DETAIL INPUT.
  GS_DETAIL-MWSBP = ( ( GS_DETAIL-NETPR * GS_HEADER-VTPCT ) / 100 ).
ENDMODULE.                 " CHECK_VAT  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_VAT_HEADER  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_VAT_HEADER INPUT.
  GS_HEADER-VTAMT = ( ( GS_HEADER-NETWR *  GS_HEADER-VTPCT ) / 100 ).
  GS_HEADER-TOTAL = GS_HEADER-VTAMT + GS_HEADER-NETWR.
ENDMODULE.                 " CHECK_VAT_HEADER  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  M_SET_CURSOR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_SET_CURSOR OUTPUT.
  DATA : LV_CURSOR LIKE GV_CURSOR.
  LV_CURSOR = GV_CURSOR.
  PERFORM GET_CURSOR USING GV_CURSOR.
  IF GV_CURSOR EQ 'MATNR'.
    SET CURSOR FIELD 'GS_DETAIL-MATNR' LINE 1.
  ELSEIF GV_CURSOR EQ 'DETAIL'.
    SET CURSOR FIELD LV_CURSOR LINE GV_LINE_DE.
  ELSE.
    SET CURSOR FIELD GV_CURSOR.
  ENDIF.
  GV_CURSOR = LV_CURSOR.
ENDMODULE.                 " M_SET_CURSOR  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  M_GET_CURSOR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_GET_CURSOR OUTPUT.
  IF GV_CURSOR EQ 'GS_DETAIL-MATNR' AND GV_FLAG IS INITIAL.
    SET CURSOR FIELD 'GS_DETAIL-KWMENG' LINE GV_LINE.
  ELSEIF GV_CURSOR EQ 'GS_DETAIL-MATNR' AND GV_FLAG IS NOT INITIAL.
    SET CURSOR FIELD 'GS_DETAIL-MATNR' LINE GV_LINE.
  ENDIF.
ENDMODULE.                 " M_GET_CURSOR  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  M_GET_CURSER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_GET_CURSER INPUT.
  DATA LV_TABIX_LINE TYPE SY-TABIX.
  GET CURSOR LINE LV_TABIX_LINE.
  GV_LINE = 1 + LV_TABIX_LINE.
  GV_FLAG = GC_TRUE.

  GET CURSOR LINE GV_LINE_DE.
ENDMODULE.                 " M_GET_CURSER  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_GET_IO_FROM_HEADER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_GET_IO_FROM_HEADER INPUT.
  GS_DETAIL-AUFNR = GS_HEADER-AUFNR.
  IF GS_DETAIL-MATNR IS NOT INITIAL.
    MODIFY GT_DETAIL FROM GS_DETAIL INDEX COLTROL_QT-CURRENT_LINE
                             TRANSPORTING AUFNR.
  ELSE.
    CLEAR GS_DETAIL-AUFNR.
  ENDIF.
ENDMODULE.                 " M_GET_IO_FROM_HEADER  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_ARV_PRICE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_ARV_PRICE INPUT.
  DATA : LS_DETAIL_AVR LIKE GS_DETAIL,
         LS_TABIX TYPE SY-TABIX.

  DATA : LT_SUM_BOM LIKE GT_SUM_BOM,
         LS_SUM_BOM LIKE GS_SUM_BOM.
  MODIFY GT_DETAIL FROM GS_DETAIL INDEX COLTROL_QT-CURRENT_LINE
                           TRANSPORTING PRBOM.

  LOOP AT GT_DETAIL INTO LS_DETAIL_AVR WHERE UEPOS = GS_DETAIL-POSNR
                                         AND UPMAT = GS_DETAIL-MATNR.
    LS_TABIX = SY-TABIX.
    READ TABLE GT_SUM_BOM INTO GS_SUM_BOM
    WITH KEY UEPOS = LS_DETAIL_AVR-UEPOS
             MATNR = LS_DETAIL_AVR-UPMAT.
    IF SY-SUBRC = 0 AND GS_SUM_BOM-PRBOM NE 0.
      LS_DETAIL_AVR-NETPR = ( GS_DETAIL-PRBOM * LS_DETAIL_AVR-NETPR ) / GS_SUM_BOM-PRBOM.
      LS_DETAIL_AVR-NETWR = LS_DETAIL_AVR-NETPR * LS_DETAIL_AVR-KWMENG.
    ENDIF.

    MOVE-CORRESPONDING GS_SUM_BOM TO LS_SUM_BOM.
    LS_SUM_BOM-PRBOM = LS_DETAIL_AVR-NETPR.


    COLLECT LS_SUM_BOM INTO LT_SUM_BOM.

    MODIFY GT_DETAIL FROM LS_DETAIL_AVR INDEX LS_TABIX
                                 TRANSPORTING NETPR NETWR.
    CLEAR : LS_DETAIL_AVR,GS_SUM_BOM.
  ENDLOOP.

  LOOP AT LT_SUM_BOM INTO LS_SUM_BOM.
    MODIFY GT_SUM_BOM FROM LS_SUM_BOM TRANSPORTING PRBOM
                                             WHERE UEPOS = LS_SUM_BOM-UEPOS
                                               AND MATNR = LS_SUM_BOM-MATNR.
  ENDLOOP.

  CLEAR LT_SUM_BOM[].

ENDMODULE.                 " M_ARV_PRICE  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_UPDATE_AMOUNT_HEADER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_UPDATE_AMOUNT_HEADER INPUT.

  PERFORM F_UPDATE_AMOUNT_HEADER.


ENDMODULE.                 " M_UPDATE_AMOUNT_HEADER  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_LINE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CHECK_LINE_INPUT INPUT.
  CLEAR GV_CHECK_CHANGE_ITEM.
  LOOP AT GT_DETAIL INTO GS_DETAIL WHERE POSNR IS NOT INITIAL.
    ADD 1 TO GV_CHECK_CHANGE_ITEM.
  ENDLOOP.
  CLEAR GS_DETAIL.
ENDMODULE.                 " M_CHECK_LINE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  M_SORT_DETAIL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_SORT_DETAIL INPUT.
  DATA LS_DETAIL_S LIKE GS_DETAIL.
  LS_DETAIL_S-POSNR = '999999'.
  MODIFY GT_DETAIL FROM LS_DETAIL_S TRANSPORTING POSNR
                                         WHERE POSNR IS INITIAL.
  SORT GT_DETAIL BY POSNR.
  CLEAR LS_DETAIL_S.
  MODIFY GT_DETAIL FROM LS_DETAIL_S TRANSPORTING POSNR
                                         WHERE POSNR = '999999'.
  PERFORM F_UPDATE_PRICE_BOM.
ENDMODULE.                 " M_SORT_DETAIL  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_LAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CHECK_LAND INPUT.

  IF GV_CHECK_CHANGE_LAND NE GS_HEADER-LAND.
    GV_CHECK_FLAG_LAND = 'X'.
  ELSE.
    CLEAR GV_CHECK_FLAG_LAND.
  ENDIF.

  GV_CHECK_CHANGE_LAND = GS_HEADER-LAND.

ENDMODULE.                 " M_CHECK_LAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CHANGE_LAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CHANGE_LAND INPUT.
  IF GV_CHECK_FLAG_LAND = 'X'.
    GS_DETAIL-LAND = GS_HEADER-LAND.
    MODIFY GT_DETAIL FROM GS_DETAIL INDEX COLTROL_QT-CURRENT_LINE
                                    TRANSPORTING LAND.
  ENDIF.
ENDMODULE.                 " M_CHANGE_LAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_UPDATE_CM_BOM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_UPDATE_CM_BOM INPUT.
  IF GS_DETAIL-POSNR IS INITIAL.
    CLEAR GS_DETAIL-CM.
  ELSE.
    PERFORM F_CHECK_CM.
  ENDIF.
ENDMODULE.                 " M_UPDATE_CM_BOM  INPUT

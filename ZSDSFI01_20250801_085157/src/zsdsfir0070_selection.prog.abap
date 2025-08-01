*&---------------------------------------------------------------------*
*& Include          ZSDSFII0070_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-B01.

PARAMETERS: P_BUKRS TYPE BKPF-BUKRS DEFAULT '1000' NO-DISPLAY.

SELECT-OPTIONS: S_GJAHR FOR WITH_ITEM-GJAHR OBLIGATORY,
                S_WITHT FOR WITH_ITEM-WITHT,
                S_WTHCD FOR WITH_ITEM-WT_WITHCD,
                S_QSREC FOR WITH_ITEM-QSREC,
                S_CTNUM FOR WITH_ITEM-CTNUMBER,    "WHT Cert. no.
                S_LIFNR FOR LFA1-LIFNR ,           "vendor no.
                S_KUNNR FOR KNA1-KUNNR,            "Customer no.
                S_BELNR FOR WITH_ITEM-BELNR,       "document no
                S_AUGBL FOR WITH_ITEM-AUGBL,       "clearing doc.
                S_AUGDT FOR WITH_ITEM-AUGDT,       "clearig date
                S_WHTDT FOR WITH_ITEM-CTISSUEDATE.       "Wht Create date  "Add by Wantaneee
SELECTION-SCREEN END OF BLOCK B1.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-B02.

PARAMETERS: R_RPT    RADIOBUTTON GROUP TYPS DEFAULT 'X' ,
            R_PND1   RADIOBUTTON GROUP TYPS ,
            R_PND2   RADIOBUTTON GROUP TYPS ,
            R_PND3   RADIOBUTTON GROUP TYPS ,
            R_PND53  RADIOBUTTON GROUP TYPS .
PARAMETERS: P_PATH LIKE RLGRAP-FILENAME DEFAULT 'C:\' MODIF ID SC5.

SELECTION-SCREEN END OF BLOCK B2.

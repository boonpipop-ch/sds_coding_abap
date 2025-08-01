*-----------------------------------------------------------------------
* Program     : ZSDSFIR0010
* Title       : Program for check expiration date BG
* Author      : Jakarin Sirilertlak.
* Date        : 19.01.2023
* Release     :
* Module      : FI
* Description :
* Authority   : No explicit Authority Check is performed in this
*               program.
*-----------------------------------------------------------------------
REPORT ZSDSFIR0010 MESSAGE-ID ZSDSFI01.
TYPE-POOLS : TRUXS,SLIS,ICON.
*&---------------------------------------------------------------------*
*  DECLARATION
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*  TABLE
*&---------------------------------------------------------------------*
TABLES: ZSDSFIT001,SOMLRECI1.
*&---------------------------------------------------------------------*
*  TYPE
*&---------------------------------------------------------------------*
TYPES : BEGIN OF GY_RESULT,
          KUNNR       TYPE ZSDSFIT001-KUNNR,
          VKBUR       TYPE ZSDSFIT001-VKBUR,
          VKGRP       TYPE ZSDSFIT001-VKGRP,
          BGTNO       TYPE ZSDSFIT001-BGTNO,
          BANKN       TYPE ZSDSFIT001-BANKN,
          SAKNR       TYPE ZSDSFIT001-SAKNR,
          DATE_FROM   TYPE ZSDSFIT001-DATE_FROM,
          DATE_TO     TYPE ZSDSFIT001-DATE_TO,
          NETWR       TYPE ZSDSFIT001-NETWR,
          STATUS1     TYPE ZSDSFIT001-STATUS1,
          PROJEC      TYPE ZSDSFIT001-PROJEC,
          CATAGORY    TYPE ZSDSFIT001-CATAGORY,
          BANK_BRANCH TYPE ZSDSFIT001-BANK_BRANCH,
          SALES       TYPE ZSDSFIT001-SALES,
          NAME1       TYPE KNA1-NAME1,
          NAMEE       TYPE ADRC-NAME1,
          SALES_O     TYPE C LENGTH 255,
          SALES_G     TYPE C LENGTH 255,
        END OF GY_RESULT.

TYPES : BEGIN OF GY_TVKBT,
          VKBUR TYPE TVKBT-VKBUR,
          BEZEI TYPE TVKBT-BEZEI,
        END OF GY_TVKBT.

TYPES : BEGIN OF GY_TVGRT,
          VKGRP TYPE TVGRT-VKGRP,
          BEZEI TYPE TVGRT-BEZEI,
        END OF GY_TVGRT.

TYPES : BEGIN OF GY_KNA1,
          KUNNR TYPE KNA1-KUNNR,
          NAME1 TYPE KNA1-NAME1,
        END OF GY_KNA1.

TYPES : BEGIN OF GY_ADRC,
          KUNNR TYPE KNA1-KUNNR,
          NAME1 TYPE KNA1-NAME1,
        END OF GY_ADRC.
*&---------------------------------------------------------------------*
*  VARIABLE
*&---------------------------------------------------------------------*
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_TVKBT TYPE TABLE OF GY_TVKBT,
       GS_TVKBT TYPE GY_TVKBT.

DATA : GT_TVGRT TYPE TABLE OF GY_TVGRT,
       GS_TVGRT TYPE GY_TVGRT.

DATA : GT_KNA1 TYPE TABLE OF GY_KNA1,
       GS_KNA1 TYPE GY_KNA1.

DATA : GT_ADRC TYPE TABLE OF GY_ADRC,
       GS_ADRC TYPE GY_ADRC.

DATA:
*- Structure to hold Output Details in delimited format
  GS_OUTPUT_SOLI TYPE  SOLI,
*- Structure to hold Imported Object Components
  GS_OBJPACK     TYPE SOPCKLSTI1,
*- Structure to hold data in single line format
  GS_OBJHEAD     TYPE SOLISTI1,
*- Structure to hold data in single line format
  GS_OBJTXT      TYPE SOLISTI1,
*- Structure to hold data for API Recipient List
  GS_RECLIST     TYPE SOMLRECI1,
*- Structure to hold  email data
  GS_DOC_CHNG    TYPE SODOCCHGI1,
*- Internal Table to hold Output Details in delimited format
  GT_OUTPUT_SOLI TYPE TABLE OF SOLI,
*- Internal Table to hold Imported Object Components
  GT_OBJPACK     TYPE TABLE OF SOPCKLSTI1,
*- Internal Table to hold data in single line format
  GT_OBJHEAD     TYPE TABLE OF SOLISTI1,
*- Internal Table to hold data in single line format
  GT_OBJTXT      TYPE TABLE OF SOLISTI1,
*- Internal Table to hold data for API Recipient List
  GT_RECLIST     TYPE TABLE OF SOMLRECI1.

DATA: L_BEGDA(10),
      L_ENDDA(10).
DATA: G_LINES       TYPE SY-TABIX,  "To hold number of records
      G_MSG_LINES   TYPE SY-TABIX,  "To hold number of records
      G_SENT_ALL(1) TYPE C.

DATA : GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_SORT   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT   TYPE SLIS_SORTINFO_ALV.

DATA : GV_ALL TYPE C.

*&---------------------------------------------------------------------*
*  RANGES
*&---------------------------------------------------------------------*
RANGES : GR_STAT FOR JEST-STAT.
*&---------------------------------------------------------------------*
*  CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS : GC_TRUE TYPE C VALUE 'X',
            GC_ONHD TYPE C LENGTH 6 VALUE 'Onhand'.

CONSTANTS : C_INT(3)          TYPE C VALUE 'INT',   "Internet mail address
            C_DOC_TYPE_HTM(3) TYPE C VALUE 'HTM',   "Code for document class
            C_REC_TYPE        TYPE C VALUE 'U',     "Recipient type
            C_EXPRESS         TYPE C VALUE 'X'.     "Send express
************************************************************************
*      S E L E C T I O N  S C R E E N                                  *
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-T01 .
  SELECT-OPTIONS: S_KUNNR  FOR ZSDSFIT001-KUNNR ,
                  S_VKBUR  FOR ZSDSFIT001-VKBUR ,
                  S_VKGRP  FOR ZSDSFIT001-VKGRP ,
                  S_DATF   FOR ZSDSFIT001-DATE_FROM.

  PARAMETERS      P_DATT   TYPE ZSDSFIT001-DATE_TO.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-T02 .
  SELECT-OPTIONS: S_RECEI FOR SOMLRECI1-RECEIVER NO INTERVALS.
SELECTION-SCREEN END OF BLOCK B2.

*&---------------------------------------------------------------------*
*  INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_GET_INITAIL.
*&---------------------------------------------------------------------*
*  AT SELECTION-SCREEN OUTPUT
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.


*&---------------------------------------------------------------------*
*  AT SELECTION-SCREEN OUTPUT
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.


*&---------------------------------------------------------------------*
*  START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM F_GET_DATA.

*&---------------------------------------------------------------------*
*  END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  IF GT_RESULT[] IS NOT INITIAL.
    PERFORM F_MODIF_RESULT.
    PERFORM F_SEND_EMAIL.
  ELSE.
    MESSAGE S004 DISPLAY LIKE 'E'.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_DATA .

  DATA LT_RESULT LIKE GT_RESULT.

  SELECT KUNNR
         VKBUR
         VKGRP
         BGTNO
         BANKN
         SAKNR
         DATE_FROM
         DATE_TO
         NETWR
         STATUS1
         PROJEC
         CATAGORY
         BANK_BRANCH
         SALES
    FROM ZSDSFIT001
    INTO TABLE GT_RESULT
    WHERE KUNNR     IN S_KUNNR
      AND VKBUR     IN S_VKBUR
      AND VKGRP     IN S_VKGRP
      AND DATE_FROM IN S_DATF
      AND DATE_TO   LE P_DATT
      AND STATUS1   EQ GC_ONHD.

  SORT GT_RESULT BY DATE_TO.
  DELETE GT_RESULT WHERE DATE_TO IS INITIAL.
  SORT GT_RESULT BY KUNNR VKBUR VKGRP BGTNO.
*  SELECT kunnr
*         vkbur
*         vkgrp
*         bgtno
*         bankn
*         saknr
*         date_from
*         date_to
*         netwr
*         status1
*         projec
*         catagory
*         bank_branch
*         sales
*    FROM ZSDSFIT001
*    INTO TABLE lt_result
*    WHERE kunnr     IN s_kunnr
*      AND vkbur     IN s_vkbur
*      AND vkgrp     IN s_vkgrp
*      AND date_from IN s_datf
*      AND date_to   EQ '00000000'
*      AND status1   EQ gc_onhd.
*
*  APPEND LINES OF lt_result TO gt_result.
*
*  SORT gt_result.
*  DELETE ADJACENT DUPLICATES FROM gt_result.

  IF GT_RESULT[] IS NOT INITIAL.
    PERFORM F_GET_SALES_OFFICE.
    PERFORM F_GET_SALES_GROUP.
    PERFORM F_GET_CUSTOMER_NAME.
  ENDIF.

ENDFORM.                    " F_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_SALES_OFFICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_SALES_OFFICE .
  SELECT VKBUR
         BEZEI
    FROM TVKBT
    INTO TABLE GT_TVKBT
    FOR ALL ENTRIES IN GT_RESULT
    WHERE VKBUR EQ GT_RESULT-VKBUR
      AND SPRAS EQ SY-LANGU.

ENDFORM.                    " F_GET_SALES_OFFICE
*&---------------------------------------------------------------------*
*&      Form  F_GET_SALES_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_SALES_GROUP .
  SELECT VKGRP
         BEZEI
    FROM TVGRT
    INTO TABLE GT_TVGRT
    FOR ALL ENTRIES IN GT_RESULT
    WHERE VKGRP EQ GT_RESULT-VKGRP
      AND SPRAS EQ SY-LANGU.
ENDFORM.                    " F_GET_SALES_GROUP
*&---------------------------------------------------------------------*
*&      Form  F_CUSTOMER_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_CUSTOMER_NAME.
  SELECT KUNNR
         NAME1
    FROM KNA1
    INTO TABLE GT_KNA1
    FOR ALL ENTRIES IN GT_RESULT
    WHERE KUNNR EQ GT_RESULT-KUNNR.

  SELECT KNA1~KUNNR
         ADRC~NAME1
    FROM KNA1
    INNER JOIN ADRC ON KNA1~ADRNR  EQ ADRC~ADDRNUMBER AND
                       ADRC~NATION EQ 'I'
    INTO TABLE GT_ADRC
    FOR ALL ENTRIES IN GT_RESULT
    WHERE KNA1~KUNNR EQ GT_RESULT-KUNNR.

ENDFORM.                    " F_GET_SALES_GROUP
*&---------------------------------------------------------------------*
*&      Form  F_MODIF_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MODIF_RESULT .
  DATA LV_TABIX TYPE SY-TABIX.

  LOOP AT GT_RESULT INTO GS_RESULT.
    LV_TABIX = SY-TABIX.

    READ TABLE GT_TVKBT INTO GS_TVKBT
    WITH KEY VKBUR = GS_RESULT-VKBUR.
    IF SY-SUBRC = 0.
      GS_RESULT-SALES_O = GS_TVKBT-BEZEI.
    ENDIF.

    READ TABLE GT_TVGRT INTO GS_TVGRT
    WITH KEY VKGRP = GS_RESULT-VKGRP.
    IF SY-SUBRC = 0.
      GS_RESULT-SALES_G = GS_TVGRT-BEZEI.
    ENDIF.

    READ TABLE GT_KNA1 INTO GS_KNA1
    WITH KEY KUNNR = GS_RESULT-KUNNR.
    IF SY-SUBRC = 0.
      GS_RESULT-NAME1 = GS_KNA1-NAME1.
    ENDIF.

    READ TABLE GT_ADRC INTO GS_ADRC
    WITH KEY KUNNR = GS_RESULT-KUNNR.
    IF SY-SUBRC = 0.
      GS_RESULT-NAMEE = GS_ADRC-NAME1.
    ENDIF.

    PERFORM F_EXIT_ALPHA_OUTPUT USING GS_RESULT-KUNNR
                             CHANGING GS_RESULT-KUNNR.

    MODIFY GT_RESULT FROM GS_RESULT INDEX LV_TABIX
                             TRANSPORTING SALES_O SALES_G NAME1 KUNNR NAMEE.
    CLEAR : GS_RESULT,GS_TVKBT,GS_TVGRT,GS_KNA1.
  ENDLOOP.

  SORT GT_RESULT BY DATE_TO.

ENDFORM.                    " F_MODIF_RESULT
*&---------------------------------------------------------------------*
*&      Form  F_SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SEND_EMAIL .

  DATA LV_AMOUNT TYPE C LENGTH 255.

  DATA LV_SENDER_ADDRESS TYPE SOEXTRECI1-RECEIVER.

  LV_SENDER_ADDRESS     = 'ORATHAI'."sy-uname.
  GS_DOC_CHNG-OBJ_NAME  = 'แจ้งเตือน BG ที่จะหมดอายุ : Remind BG Expiry Date'.
  GS_DOC_CHNG-OBJ_DESCR = 'แจ้งเตือน BG ที่จะหมดอายุ : Remind BG Expiry Date'.
*Set the Body background colour
  GS_OBJTXT-LINE = '<body bgcolor = "#E6E6FA">'.
*- Append
  APPEND GS_OBJTXT TO GT_OBJTXT.
*- Clear
  CLEAR GS_OBJTXT.
*Set font color and its type
  CONCATENATE '<FONT COLOR = "#191970" face="Garamond">' '<b>' INTO GS_OBJTXT-LINE.
*- Append
  APPEND GS_OBJTXT TO GT_OBJTXT.
*- Clear
  CLEAR GS_OBJTXT.
*Pripare mail body
  CONCATENATE '<p>' 'Dear Sir/Madam,' '</p>'
              INTO GS_OBJTXT-LINE.
*- Append
  APPEND GS_OBJTXT TO GT_OBJTXT.
*- Clear
  CLEAR GS_OBJTXT.
  GS_OBJTXT-LINE = SPACE.
*- Append
  APPEND GS_OBJTXT TO GT_OBJTXT.
*- Clear
  CLEAR GS_OBJTXT.
  CONCATENATE '<p>'
              'กรุณาตรวจสอบ BG หมดอายุ  และ ใกล้หมดอายุ ตามรายละเอียดด้านล่าง'
              '</p>'
                 INTO GS_OBJTXT-LINE SEPARATED BY SPACE.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR  GS_OBJTXT.
*--------------------------------------------------------------------*
  CONCATENATE '<p>' ' 1. ลูกค้ารายใดที่ยังมีความประสงค์จะดำเนินการซื้อสินค้าต่อเนื่องกับทาง SDS แต่ BG'
                       'หมดอายุหรือใกล้จะหมดอายุ ขอให้ฝ่ายขายแจ้งทางลูกค้าให้ต่ออายุ BG ให้แล้วเสร็จก่อนที่จะออก Sales Order โดยแบ่ง ดังนี้'
                 '</p>'
                 INTO GS_OBJTXT-LINE SEPARATED BY SPACE.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR  GS_OBJTXT.

  CONCATENATE '<p>' '&nbsp;' '&nbsp;' '1.1 BG ค้ำวงเงิน Permanent อายุของ BG ต้องไม่น้อยกว่า 1 ปี หรือไม่ต้องระบุวันหมดอายุ BG (เป็น BG ปลายเปิด)'
                 '</p>'
                 INTO GS_OBJTXT-LINE SEPARATED BY SPACE.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR GS_OBJTXT.

  CONCATENATE '<p>' '&nbsp;' '&nbsp;' '1.2 BG ค้ำวงเงิน Temporary(Project) อายุของ BG'
                      ' ต้องครอบคลุมถึงวันที่ชำระหนี้ของงวดสุดท้ายของ Project'
                 '</p>'
                 INTO GS_OBJTXT-LINE SEPARATED BY SPACE.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR GS_OBJTXT.

  CONCATENATE '<p>' '2. เมื่อโครงการสิ้นสุดให้ฝ่ายขายดำเนินการขอคืน BG โดยเร็ว'
                 '</p>'
                 INTO GS_OBJTXT-LINE SEPARATED BY SPACE.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR GS_OBJTXT.
*--------------------------------------------------------------------*
  GS_OBJTXT-LINE = '<center>'.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR  GS_OBJTXT.


*  Pripare Employee data in table format to dispay in Mail body
  GS_OBJTXT-LINE = '<TABLE  width= "100%" border="1">'.
  APPEND GS_OBJTXT TO GT_OBJTXT.
*--------------------------------------------------------------------*
* Header Table
*--------------------------------------------------------------------*
  CLEAR  GS_OBJTXT.
  CONCATENATE '<TR ><td align = "LEFT" BGCOLOR = "#708090">'
              '<FONT COLOR = "BLUE"><B>Customer Code</B> </FONT>'
                '</td>'  INTO GS_OBJTXT-LINE.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR  GS_OBJTXT.

  CONCATENATE '<td align = "LEFT" BGCOLOR = "#708090">'
              '<FONT COLOR = "BLUE"><B>Customer Name</B> </FONT>'
                '</td>'  INTO GS_OBJTXT-LINE.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR  GS_OBJTXT.

  CONCATENATE '<td align = "LEFT" BGCOLOR = "#708090">'
              '<FONT COLOR = "BLUE"><B>Sales Name</B> </FONT>'
                '</td>'  INTO GS_OBJTXT-LINE.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR  GS_OBJTXT.

  CONCATENATE '<td align = "LEFT"  BGCOLOR = "#708090">'
                '<FONT COLOR = "BLUE"> <B>Bankgaurantee</B> </FONT>'
                '</td>'  INTO GS_OBJTXT-LINE.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR  GS_OBJTXT.

  CONCATENATE '<td align = "LEFT"  BGCOLOR = "#708090">'
                   ' <FONT COLOR = "BLUE"><B>BG Start Date</B> </FONT>'
                   '</td>'  INTO GS_OBJTXT-LINE.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR  GS_OBJTXT.

  CONCATENATE '<td align = "LEFT"  BGCOLOR = "#708090">'
                   '<FONT COLOR = "BLUE"><B>BG Expiry Date</B> </FONT>'
                   '</td>'  INTO GS_OBJTXT-LINE.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR  GS_OBJTXT.

  CONCATENATE '<td align = "LEFT"  BGCOLOR = "#708090">'
                '<FONT COLOR = "BLUE"><B>Bank Name</B> </FONT>'
                '</td>'  INTO GS_OBJTXT-LINE.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR  GS_OBJTXT.

  CONCATENATE '<td align = "LEFT"  BGCOLOR = "#708090">'
                   ' <FONT COLOR = "BLUE"><B>Category</B> </FONT>'
                   '</td>'  INTO GS_OBJTXT-LINE.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR  GS_OBJTXT.

  CONCATENATE '<td align = "LEFT"  BGCOLOR = "#708090">'
                   ' <FONT COLOR = "BLUE"><B>Project</B> </FONT>'
                   '</td>'  INTO GS_OBJTXT-LINE.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR  GS_OBJTXT.

  CONCATENATE '<td align = "LEFT"  BGCOLOR = "#708090">'
                   '<FONT COLOR = "BLUE"><B>BG Amount</B> </FONT>'
                   '</td></tr>'  INTO GS_OBJTXT-LINE.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR  GS_OBJTXT.
*--------------------------------------------------------------------*
* Detail Table
*--------------------------------------------------------------------*
  LOOP AT GT_RESULT INTO GS_RESULT.

    WRITE: GS_RESULT-DATE_FROM TO  L_BEGDA.
    WRITE: GS_RESULT-DATE_TO   TO  L_ENDDA.

    IF L_BEGDA EQ '00.00.0000'.
      CLEAR L_BEGDA.
    ENDIF.

    IF L_ENDDA EQ '00.00.0000'.
      CLEAR L_ENDDA.
    ENDIF.

    CONCATENATE '<TR><td align = "LEFT">'
                '<FONT COLOR = "BLUE">' GS_RESULT-KUNNR '</FONT>'
                '</td>'  INTO GS_OBJTXT-LINE.
    APPEND GS_OBJTXT TO GT_OBJTXT.
    CLEAR  GS_OBJTXT.

    CONCATENATE '<td align = "LEFT">'
                '<FONT COLOR = "BLUE">' GS_RESULT-NAMEE '</FONT>'
                '</td>'  INTO GS_OBJTXT-LINE.
    APPEND GS_OBJTXT TO GT_OBJTXT.
    CLEAR  GS_OBJTXT.

    CONCATENATE '<td align = "LEFT">'
                '<FONT COLOR = "BLUE">' GS_RESULT-SALES '</FONT>'
                '</td>'  INTO GS_OBJTXT-LINE.
    APPEND GS_OBJTXT TO GT_OBJTXT.
    CLEAR  GS_OBJTXT.

    CONCATENATE '<td align = "LEFT">'
                '<FONT COLOR = "BLUE">' GS_RESULT-BGTNO '</FONT>'
                 '</td>'  INTO GS_OBJTXT-LINE.
    APPEND GS_OBJTXT TO GT_OBJTXT.
    CLEAR  GS_OBJTXT.

    CONCATENATE '<td align = "LEFT">'
                '<FONT COLOR = "BLUE">' L_BEGDA '</FONT>'
                 '</td>'  INTO GS_OBJTXT-LINE.
    APPEND GS_OBJTXT TO GT_OBJTXT.
    CLEAR  GS_OBJTXT.

    CONCATENATE '<td align = "LEFT">'
                '<FONT COLOR = "BLUE">' L_ENDDA '</FONT>'
                  '</td>'  INTO GS_OBJTXT-LINE.
    APPEND GS_OBJTXT TO GT_OBJTXT.

    CONCATENATE '<td align = "LEFT">'
                '<FONT COLOR = "BLUE">' GS_RESULT-BANKN '</FONT>'
                 '</td>'  INTO GS_OBJTXT-LINE.
    APPEND GS_OBJTXT TO GT_OBJTXT.
    CLEAR  GS_OBJTXT.

    CONCATENATE '<td align = "LEFT">'
                '<FONT COLOR = "BLUE">' GS_RESULT-CATAGORY '</FONT>'
                  '</td>'  INTO GS_OBJTXT-LINE.
    APPEND GS_OBJTXT TO GT_OBJTXT.
    CLEAR  GS_OBJTXT.

    CONCATENATE '<td align = "LEFT">'
                '<FONT COLOR = "BLUE">' GS_RESULT-PROJEC '</FONT>'
                  '</td>'  INTO GS_OBJTXT-LINE.
    APPEND GS_OBJTXT TO GT_OBJTXT.
    CLEAR  GS_OBJTXT.

    WRITE GS_RESULT-NETWR TO LV_AMOUNT.
    REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN LV_AMOUNT WITH ''.

    CONCATENATE '<td align = "LEFT">'
                '<FONT COLOR = "BLUE">' LV_AMOUNT '</FONT>'
                  '</td>'  INTO GS_OBJTXT-LINE.
    APPEND GS_OBJTXT TO GT_OBJTXT.
    CLEAR  GS_OBJTXT.
  ENDLOOP.
*--------------------------------------------------------------------*
* Footer
*--------------------------------------------------------------------*
  GS_OBJTXT-LINE = '</TABLE>'.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR  GS_OBJTXT.
  GS_OBJTXT-LINE = '</center>'.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR  GS_OBJTXT.

*  CONCATENATE '<p>' ' 1. ลูกค้ารายใดที่ยังมีความประสงค์จะดำเนินการซื้อสินค้าต่อเนื่องกับทาง SDS แต่ BG'
*                       'หมดอายุหรือใกล้จะหมดอายุ ขอให้ฝ่ายขายแจ้งทางลูกค้าให้ต่ออายุ BG ให้แล้วเสร็จก่อนที่จะออก Sales Order โดยแบ่ง ดังนี้'
*                 '</p>'
*                 INTO GS_objtxt-line SEPARATED BY space.
*  APPEND GS_objtxt TO gt_objtxt.
*  CLEAR  GS_objtxt.
*
*  CONCATENATE '<p>' '&nbsp;' '&nbsp;' '1.1 BG ค้ำวงเงิน Permanent อายุของ BG ต้องไม่น้อยกว่า 1 ปี หรือไม่ต้องระบุวันหมดอายุ BG (เป็น BG ปลายเปิด)'
*                 '</p>'
*                 INTO GS_objtxt-line SEPARATED BY space.
*  APPEND GS_objtxt TO gt_objtxt.
*  CLEAR GS_objtxt.
*
*  CONCATENATE '<p>' '&nbsp;' '&nbsp;' '1.2 BG ค้ำวงเงิน Temporary(Project) อายุของ BG'
*                      ' ต้องครอบคลุมถึงวันที่ชำระหนี้ของงวดสุดท้ายของ Project'
*                 '</p>'
*                 INTO GS_objtxt-line SEPARATED BY space.
*  APPEND GS_objtxt TO gt_objtxt.
*  CLEAR GS_objtxt.
*
*  CONCATENATE '<p>' '2. เมื่อโครงการสิ้นสุดให้ฝ่ายขายดำเนินการขอคืน BG โดยเร็ว'
*                 '</p>'
*                 INTO GS_objtxt-line SEPARATED BY space.
*  APPEND GS_objtxt TO gt_objtxt.
*  CLEAR GS_objtxt.


  GS_OBJTXT-LINE =  '<br>Regards,<br />'.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR GS_OBJTXT.
  CONCATENATE LV_SENDER_ADDRESS '<br />' INTO GS_OBJTXT-LINE.
*  GS_objtxt-line =   'Vamsi<br />'.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR GS_OBJTXT.
  GS_OBJTXT-LINE = '<br><br><b><center><i><font color = "RED">This is an auto generated Email.'.
  APPEND GS_OBJTXT TO GT_OBJTXT.
  CLEAR GS_OBJTXT.
  GS_OBJTXT-LINE = '</FONT></body>'.
*- Append
  APPEND GS_OBJTXT TO GT_OBJTXT.
*- Clear
  CLEAR GS_OBJTXT.
*--------------------------------------------------------------------*
*End Footer
*--------------------------------------------------------------------*
  DESCRIBE TABLE GT_OBJTXT LINES G_MSG_LINES.
  READ TABLE GT_OBJTXT INTO GS_OBJTXT INDEX G_MSG_LINES.
  GS_DOC_CHNG-DOC_SIZE = ( G_MSG_LINES - 1 ) * 255 + STRLEN( GS_OBJTXT ).
*- Creation of the entry for the compressed document
  GS_OBJPACK-TRANSF_BIN = ' '.
  GS_OBJPACK-HEAD_START = 1.
  GS_OBJPACK-HEAD_NUM   = 0.
  GS_OBJPACK-BODY_START = 1.
  GS_OBJPACK-BODY_NUM   = G_MSG_LINES.
  GS_OBJPACK-DOC_TYPE   = C_DOC_TYPE_HTM.
*- Append
  APPEND GS_OBJPACK TO GT_OBJPACK.
*- Clear
  CLEAR GS_OBJPACK.
*- Creation of the document attachment
*- Describe
  DESCRIBE TABLE GT_OUTPUT_SOLI LINES G_LINES.
*- Don't create attachment if no data is present
  IF G_LINES <> 0.
    LOOP AT GT_OUTPUT_SOLI INTO GS_OUTPUT_SOLI.
      GS_OBJTXT = GS_OUTPUT_SOLI.
*- Append
      APPEND GS_OBJTXT TO GT_OBJTXT.
*- Clear
      CLEAR GS_OBJTXT.
    ENDLOOP.
  ENDIF.

  LOOP AT S_RECEI.
*- Completing the recipient list
    GS_RECLIST-RECEIVER = S_RECEI-LOW.
    GS_RECLIST-REC_TYPE = C_REC_TYPE.
    GS_RECLIST-EXPRESS  = C_EXPRESS.
*- Append
    APPEND GS_RECLIST TO GT_RECLIST.
  ENDLOOP.

  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      DOCUMENT_DATA              = GS_DOC_CHNG
      PUT_IN_OUTBOX              = ''
      SENDER_ADDRESS             = LV_SENDER_ADDRESS
      SENDER_ADDRESS_TYPE        = 'B'
      COMMIT_WORK                = 'X'
    IMPORTING
      SENT_TO_ALL                = G_SENT_ALL
    TABLES
      PACKING_LIST               = GT_OBJPACK
      OBJECT_HEADER              = GT_OBJHEAD
      CONTENTS_TXT               = GT_OBJTXT
      RECEIVERS                  = GT_RECLIST
    EXCEPTIONS
      TOO_MANY_RECEIVERS         = 1
      DOCUMENT_NOT_SENT          = 2
      DOCUMENT_TYPE_NOT_EXIST    = 3
      OPERATION_NO_AUTHORIZATION = 4
      PARAMETER_ERROR            = 5
      X_ERROR                    = 6
      ENQUEUE_ERROR              = 7
      OTHERS                     = 8.
  IF SY-SUBRC = 0.
    CL_OS_TRANSACTION_END_NOTIFIER=>RAISE_COMMIT_REQUESTED( ).
    CALL FUNCTION 'DB_COMMIT'.
    CL_OS_TRANSACTION_END_NOTIFIER=>RAISE_COMMIT_FINISHED( ).
  ENDIF.

  SUBMIT RSCONN01 WITH MODE = 'INT' AND RETURN.

ENDFORM.                    " F_SEND_EMAIL
*&---------------------------------------------------------------------*
*&      Form  F_GET_INITAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_INITAIL .

*  DATA lv_data TYPE sy-datum.

  P_DATT = SY-DATUM + 30.

*  CLEAR s_datt.
*  s_datt-sign   = 'I'.
*  s_datt-option = 'BT'.
*  s_datt-high   = lv_data.
*  APPEND s_datt.

ENDFORM.                    " F_GET_INITAIL
*&---------------------------------------------------------------------*
*&      Form  F_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_ZARCM_ZCUSNO  text
*      <--P_LV_KUNNR  text
*----------------------------------------------------------------------*
FORM F_EXIT_ALPHA_OUTPUT USING    FT_ZARCM_ZCUSNO
                         CHANGING FV_KUNNR.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = FT_ZARCM_ZCUSNO
    IMPORTING
      OUTPUT = FV_KUNNR.

ENDFORM.                    " F_EXIT_ALPHA_INPUT

FUNCTION Z_SDSSD_UPDATE_SO_ADDR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_VBELN) TYPE  VBELN_VA
*"     VALUE(IS_ADDRESS) TYPE  ZSDSSDS120 OPTIONAL
*"  EXPORTING
*"     VALUE(ES_RETURN) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: LV_SALES_DOC     TYPE VBELN,
        LT_PARTNER       TYPE STANDARD TABLE OF BAPIPARNRC,
        LT_ADDRESS       TYPE STANDARD TABLE OF BAPIADDR1,
        LT_RETURN        TYPE STANDARD TABLE OF BAPIRET2,
        LS_SO_HEADER_INX TYPE BAPISDH1X.

  LV_SALES_DOC = IV_VBELN. "หมายเลขเอกสารการขาย

  SELECT
    VBPA~VBELN,
    VBPA~PARVW,
    VBPA~KUNNR,
    VBPA~ADRNR,
    ADRC~ADRC_UUID
    FROM VBPA
    INNER JOIN ADRC
    ON VBPA~ADRNR = ADRC~ADDRNUMBER
    INTO TABLE @DATA(LT_VBPA)
    WHERE VBELN = @LV_SALES_DOC
*      AND KUNNR LIKE 'OT%'
      AND PARVW <> 'VE'
      AND ADRC~NATION = @space.
  IF SY-SUBRC = 0.
    LOOP AT LT_VBPA INTO DATA(LS_VBPA).
      " เพิ่มข้อมูล Partner
      APPEND VALUE #( DOCUMENT    = LV_SALES_DOC
                      PARTN_ROLE  = LS_VBPA-PARVW "Role ของลูกค้า (เช่น AG: Sold-to Party)
                      UPDATEFLAG  = 'U'
                      ADDR_LINK   = '0000000001'
                      P_NUMB_OLD  = LS_VBPA-KUNNR
                      P_NUMB_NEW  = LS_VBPA-KUNNR ) "Customer Number
             TO LT_PARTNER.

      " เพิ่มข้อมูลที่อยู่ใหม่
      APPEND VALUE #( ADDR_NO     = '0000000001'
                      NAME        = IS_ADDRESS-NAME1
                      NAME_2      = IS_ADDRESS-NAME2
                      NAME_3      = IS_ADDRESS-NAME3
                      NAME_4      = IS_ADDRESS-NAME4
                      POSTL_COD1  = IS_ADDRESS-POSTL_COD1    "Post Code
                      STREET      = IS_ADDRESS-STREET1      "House no & Street
                      STR_SUPPL3  = IS_ADDRESS-STREET4      "Street4
                      LOCATION    = IS_ADDRESS-STREET5      "Street5
                      DISTRICT    = IS_ADDRESS-DISTRICT     "District
                      CITY        = IS_ADDRESS-CITY         "Province
                      TAXJURCODE  = IS_ADDRESS-TAXNUMBER    "Tax
                      COUNTRY     = IS_ADDRESS-COUNTRY )    "Country
             TO LT_ADDRESS.
    ENDLOOP.
  ENDIF.

  LS_SO_HEADER_INX-UPDATEFLAG = 'U'.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      SALESDOCUMENT    = LV_SALES_DOC
      ORDER_HEADER_INX = LS_SO_HEADER_INX
    TABLES
      RETURN           = LT_RETURN
      PARTNERCHANGES   = LT_PARTNER
      PARTNERADDRESSES = LT_ADDRESS.

  IF LINE_EXISTS( LT_RETURN[ TYPE = 'E' ] ) OR
     LINE_EXISTS( LT_RETURN[ TYPE = 'A' ] ).
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    READ TABLE LT_RETURN INTO ES_RETURN
                         INDEX 1.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = ABAP_TRUE.
    .
    READ TABLE LT_RETURN INTO DATA(LS_RETURN)
                         WITH KEY TYPE = 'S'
                                  ID   = 'V1'
                                  NUMBER = '311'.
    IF SY-SUBRC = 0.
      MOVE LS_RETURN TO ES_RETURN.
    ELSE.
      READ TABLE LT_RETURN INTO ES_RETURN INDEX 1.
    ENDIF.
  ENDIF.

ENDFUNCTION.

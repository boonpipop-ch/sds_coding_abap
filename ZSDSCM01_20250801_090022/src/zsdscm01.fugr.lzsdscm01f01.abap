*----------------------------------------------------------------------*
***INCLUDE LZSDSCM01F01.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Form F_GET_CONSTANTS
*----------------------------------------------------------------------*
*  Get GenC Constants
*----------------------------------------------------------------------*
FORM F_GET_CONSTANTS .

  CONSTANTS:
    LC_REPID        TYPE  SY-REPID  VALUE 'ZSDSCMR0010',
    LC_ACTIVE_VKORG TYPE  ZSDSDE_PARAM_NAME VALUE 'ACTIVE_SALES_ORG',
    LC_COMM         TYPE  ZSDSDE_PARAM_NAME VALUE 'COMMISSION_PROC_TYPE',
    LC_INST         TYPE  ZSDSDE_PARAM_NAME VALUE 'INSTALLATION_PROC_TYPE',
    LC_MAP_PRODH1   TYPE  ZSDSDE_PARAM_NAME VALUE 'PRODH_LV1_MAPPING'. "+420000429

  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_TEMP TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C,
    LT_GENC TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID   TYPE  PROGRAMM.


* Check Already Read?
  IF LF_READ EQ GC_TRUE.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GR_ACTIVE_VKORG,
         GR_COMM,
         GR_INST,
         GT_MAP_PRODH1.                                     "+420000429

* Assign REPID
  LF_REPID = SY-REPID.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = LF_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Assign REPID
  LF_REPID = LC_REPID.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = LF_REPID
    IMPORTING
      ET_GEN_C = LT_TEMP.
  APPEND LINES OF LT_TEMP TO LT_GENC.
  FREE LT_TEMP.

* Mark Read Flag
  LF_READ = GC_TRUE.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

    CASE <L_GENC>-PARAM.
*     ------------------------------------
*     Activated Sales Org
*     ------------------------------------
      WHEN LC_ACTIVE_VKORG.
        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO GR_ACTIVE_VKORG.

*     ------------------------------------
*     Process Type for Commission
*     ------------------------------------
      WHEN LC_COMM.
        INSERT VALUE #( SIGN = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GR_COMM.

*     ------------------------------------
*     Process Type for Installation
*     ------------------------------------
      WHEN LC_INST.
        INSERT VALUE #( SIGN = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GR_INST.

*<-- Start of Insertion 420000429 18.02.2025 (Assign New GenC)
*     ------------------------------------
*     Product hierarchy level 1 mapping
*     ------------------------------------
      WHEN LC_MAP_PRODH1.
        INSERT VALUE #( SALES_ORG_SD = <L_GENC>-PARAM_EXT
                        PROCESS_TYPE = <L_GENC>-VALUE_LOW
                        PRODH1       = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_MAP_PRODH1.
*--> End of Insertion 420000429 18.02.2025
    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_UPDATE_PRODH_FROM_PARENT
*----------------------------------------------------------------------*
*  Update Product Hierarchy from Parent
*----------------------------------------------------------------------*
FORM F_UPDATE_PRODH_FROM_PARENT  USING  UF_PARENT_GUID TYPE CRMT_OBJECT_GUID
                                        UF_OBJECT_GUID TYPE CRMT_OBJECT_GUID.

  DATA:
    LT_INPUT_FIELD_NAME   TYPE CRMT_INPUT_FIELD_NAMES_TAB.

  DATA:
    LS_PARENT_PRODUCT_I TYPE CRMT_PRODUCT_I_WRK,
    LS_PRODUCT_I_NEW    TYPE CRMT_PRODUCT_I_WRK,
    LS_PRODUCT_I_COM    TYPE CRMT_PRODUCT_I_COM.


  IF UF_PARENT_GUID IS INITIAL.
    RETURN.
  ENDIF.

* Read Product Object of Parent Data
  CALL FUNCTION 'CRM_PRODUCT_I_READ_OW'
    EXPORTING
      IV_GUID                   = UF_PARENT_GUID
    IMPORTING
      ES_PRODUCT_I              = LS_PARENT_PRODUCT_I
    EXCEPTIONS
      PRODUCT_I_NOT_IN_WORKAREA = 1
      OTHERS                    = 2.
  IF SY-SUBRC <> 0.
*   Cannot read Parent data
    RETURN.
  ENDIF.

* Read Product Object New
  CALL FUNCTION 'CRM_PRODUCT_I_READ_OW'
    EXPORTING
      IV_GUID                   = UF_OBJECT_GUID
    IMPORTING
      ES_PRODUCT_I              = LS_PRODUCT_I_NEW
    EXCEPTIONS
      PRODUCT_I_NOT_IN_WORKAREA = 1
      OTHERS                    = 2.
  IF SY-SUBRC <> 0.
    CLEAR LS_PRODUCT_I_NEW.
  ENDIF.

* Ignore If Same ProdH
  IF LS_PARENT_PRODUCT_I-PROD_HIERARCHY EQ LS_PRODUCT_I_NEW-PROD_HIERARCHY.
    RETURN.
  ENDIF.

* Update Product Hierarchy
  CLEAR LS_PRODUCT_I_COM.
  LS_PRODUCT_I_COM-REF_GUID       = UF_OBJECT_GUID.
  LS_PRODUCT_I_COM-PROD_HIERARCHY = LS_PARENT_PRODUCT_I-PROD_HIERARCHY.
  INSERT VALUE #( FIELDNAME = 'PROD_HIERARCHY' )
           INTO TABLE LT_INPUT_FIELD_NAME.

  CALL FUNCTION 'CRM_PRODUCT_I_MAINTAIN_OW'
    EXPORTING
      IS_PRODUCT_I_COM       = LS_PRODUCT_I_COM
    CHANGING
      CT_INPUT_FIELD_NAMES   = LT_INPUT_FIELD_NAME
    EXCEPTIONS
      PRODUCT_I_CHANGE_ERROR = 1
      PRODUCT_I_CREATE_ERROR = 2
      ERROR_OCCURRED         = 3
      OTHERS                 = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
           RAISING ABORT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_UPDATE_PRODH_FROM_PRODUCT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_ORDER_I_NEW_PRODUCT
*&      --> IV_OBJECT_GUID
*&---------------------------------------------------------------------*
FORM F_UPDATE_PRODH_FROM_PRODUCT  USING  UF_PRODUCT_GUID TYPE CRMT_OBJECT_GUID
                                         UF_OBJECT_GUID  TYPE CRMT_OBJECT_GUID.

  DATA:
    LT_INPUT_FIELD_NAME   TYPE CRMT_INPUT_FIELD_NAMES_TAB.

  DATA:
    LS_PRODUCT_SALES_DATA TYPE  CRMT_PRODUCT_SALES_API_EXP,
    LS_PRODUCT_I_NEW      TYPE CRMT_PRODUCT_I_WRK,
    LS_PRODUCT_I_COM      TYPE CRMT_PRODUCT_I_COM.


  IF UF_PRODUCT_GUID IS INITIAL.
    RETURN.
  ENDIF.

* Read From Reference Product
  CALL FUNCTION 'CRM_PRODUCT_I_READ_PRODUCT_OW'
    EXPORTING
      IV_ITEM_GUID          = UF_OBJECT_GUID
      IV_PRODUCT_GUID       = UF_PRODUCT_GUID
    IMPORTING
      ES_PRODUCT_SALES_DATA = LS_PRODUCT_SALES_DATA
    EXCEPTIONS
      PRODUCT_NOT_FOUND     = 1
      OTHERS                = 2.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

* Read Product Object New
  CALL FUNCTION 'CRM_PRODUCT_I_READ_OW'
    EXPORTING
      IV_GUID                   = UF_OBJECT_GUID
    IMPORTING
      ES_PRODUCT_I              = LS_PRODUCT_I_NEW
    EXCEPTIONS
      PRODUCT_I_NOT_IN_WORKAREA = 1
      OTHERS                    = 2.
  IF SY-SUBRC <> 0.
    CLEAR LS_PRODUCT_I_NEW.
  ENDIF.

* Ignore If Same ProdH
  IF LS_PRODUCT_SALES_DATA-PROD_HIERARCHY EQ LS_PRODUCT_I_NEW-PROD_HIERARCHY.
    RETURN.
  ENDIF.

* Update Product Hierarchy
  CLEAR LS_PRODUCT_I_COM.
  LS_PRODUCT_I_COM-REF_GUID       = UF_OBJECT_GUID.
  LS_PRODUCT_I_COM-PROD_HIERARCHY = LS_PRODUCT_SALES_DATA-PROD_HIERARCHY.
  INSERT VALUE #( FIELDNAME = 'PROD_HIERARCHY' )
           INTO TABLE LT_INPUT_FIELD_NAME.

  CALL FUNCTION 'CRM_PRODUCT_I_MAINTAIN_OW'
    EXPORTING
      IS_PRODUCT_I_COM       = LS_PRODUCT_I_COM
    CHANGING
      CT_INPUT_FIELD_NAMES   = LT_INPUT_FIELD_NAME
    EXCEPTIONS
      PRODUCT_I_CHANGE_ERROR = 1
      PRODUCT_I_CREATE_ERROR = 2
      ERROR_OCCURRED         = 3
      OTHERS                 = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
           RAISING ABORT.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_UPDATE_PRODH_IN_CHILDS
*----------------------------------------------------------------------*
*  Update Product Hierarchy in all childs
*----------------------------------------------------------------------*
FORM F_UPDATE_PRODH_IN_CHILDS  USING  UF_ITEM_GUID      TYPE CRMT_OBJECT_GUID
                                      UF_HEADER_GUID    TYPE CRMT_OBJECT_GUID
                                      US_ORGMAN_WRK    TYPE CRMT_ORGMAN_WRK
                                      UF_PROD_HIERARCHY TYPE CRMT_PRODUCT_I_WRK-PROD_HIERARCHY.

  DATA:
    LT_ORDERADM_I_WRK   TYPE  CRMT_ORDERADM_I_WRKT,
    LT_INPUT_FIELD_NAME TYPE CRMT_INPUT_FIELD_NAMES_TAB.

  DATA:
    LS_PRODUCT_I     TYPE CRMT_PRODUCT_I_WRK,
    LS_PRODUCT_I_COM TYPE CRMT_PRODUCT_I_COM.


* Get all items
  CALL FUNCTION 'CRM_ORDERADM_I_READ_OB'
    EXPORTING
      IV_HEADER                = UF_HEADER_GUID
      IV_INCLUDE_DELETED_ITEMS = ' '
    IMPORTING
      ET_ORDERADM_I_WRK        = LT_ORDERADM_I_WRK
    EXCEPTIONS
      ITEM_DOES_NOT_EXIST      = 1
      ERROR_OCCURRED           = 2
      OTHERS                   = 3.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

* Check Each item and update Product Hierachy
  LOOP AT LT_ORDERADM_I_WRK ASSIGNING FIELD-SYMBOL(<L_ORDERADM_I_WRK>)
                            WHERE PARENT EQ UF_ITEM_GUID. "#EC CI_SORTSEQ
*   Read Product Object Old
    CALL FUNCTION 'CRM_PRODUCT_I_READ_OW'
      EXPORTING
        IV_GUID                   = <L_ORDERADM_I_WRK>-GUID
      IMPORTING
        ES_PRODUCT_I              = LS_PRODUCT_I
      EXCEPTIONS
        PRODUCT_I_NOT_IN_WORKAREA = 1
        OTHERS                    = 2.
    IF SY-SUBRC <> 0.
      CLEAR LS_PRODUCT_I.
    ENDIF.

*   Ignore If Same ProdH
    IF LS_PRODUCT_I-PROD_HIERARCHY EQ UF_PROD_HIERARCHY.
      CONTINUE.
    ENDIF.

*   Update Product Hierarchy
    CLEAR LS_PRODUCT_I_COM.
    LS_PRODUCT_I_COM-REF_GUID       = <L_ORDERADM_I_WRK>-GUID.
    LS_PRODUCT_I_COM-PROD_HIERARCHY = UF_PROD_HIERARCHY.
    INSERT VALUE #( FIELDNAME = 'PROD_HIERARCHY' )
             INTO TABLE LT_INPUT_FIELD_NAME.

    CALL FUNCTION 'CRM_PRODUCT_I_MAINTAIN_OW'
      EXPORTING
        IS_PRODUCT_I_COM       = LS_PRODUCT_I_COM
      CHANGING
        CT_INPUT_FIELD_NAMES   = LT_INPUT_FIELD_NAME
      EXCEPTIONS
        PRODUCT_I_CHANGE_ERROR = 1
        PRODUCT_I_CREATE_ERROR = 2
        ERROR_OCCURRED         = 3
        OTHERS                 = 4.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
             RAISING ABORT.
    ENDIF.

  ENDLOOP.
*<-- Start of Insertion 420000429 18.02.2025 (New Product Hierarchy Determination)
* When Not Parent Item changed
  IF SY-SUBRC NE 0.
*   Check Only in No Parent assignment scenario
    LOOP AT LT_ORDERADM_I_WRK TRANSPORTING NO FIELDS
                              WHERE PARENT IS NOT INITIAL. "#EC CI_SORTSEQ
      EXIT.
    ENDLOOP.
    IF SY-SUBRC EQ 0.
      RETURN.
    ENDIF.

*   ------------------------------------------------------
*   Mode 5: Assign Product Hierarchy from New Logic
*           - When Service material is changed, reconsider
*             product hierarchy in all items
*   ------------------------------------------------------
    PERFORM F_UPDATE_PRODH_FROM_SERVMAT  USING  UF_HEADER_GUID
                                                US_ORGMAN_WRK
                                                GC_TRUE.
  ENDIF.
*--> End of Insertion 420000429 18.02.2025

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CREATE_BG_JOB
*----------------------------------------------------------------------*
*  Create Background job to execute the program
*----------------------------------------------------------------------*
FORM F_CREATE_BG_JOB.

  DATA:
    LR_OBJID  TYPE  TT_OBJID_RANGE.

  DATA:
    LF_JOBNAME  TYPE  TBTCJOB-JOBNAME,
    LF_JOBCOUNT TYPE  TBTCJOB-JOBCOUNT.


* Only when Criteria exist
  IF GR_OBJID IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT GR_OBJID ASSIGNING FIELD-SYMBOL(<L_OBJID>).

    CLEAR LR_OBJID.

*   Assign Job Name
    LF_JOBNAME = |ZSDSCM_UPDWRT_{ <L_OBJID>-LOW }|.

    INSERT <L_OBJID> INTO TABLE LR_OBJID.

*   Create Job
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        JOBNAME          = LF_JOBNAME
      IMPORTING
        JOBCOUNT         = LF_JOBCOUNT
      EXCEPTIONS
        CANT_CREATE_JOB  = 1
        INVALID_JOB_DATA = 2
        JOBNAME_MISSING  = 3
        OTHERS           = 4.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

*   Submit Program
    SUBMIT ZSDSCMR0010 AND RETURN                        "#EC CI_SUBMIT
                       WITH S_OBJID IN LR_OBJID
                       WITH CB_TEST EQ SPACE
                       WITH P_DATUM EQ SY-DATUM
                       WITH P_UZEIT EQ SY-UZEIT
                       VIA JOB LF_JOBNAME
                       NUMBER LF_JOBCOUNT.

*   Close Job
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        JOBCOUNT             = LF_JOBCOUNT
        JOBNAME              = LF_JOBNAME
        STRTIMMED            = 'X'
      EXCEPTIONS
        CANT_START_IMMEDIATE = 1
        INVALID_STARTDATE    = 2
        JOBNAME_MISSING      = 3
        JOB_CLOSE_FAILED     = 4
        JOB_NOSTEPS          = 5
        JOB_NOTEX            = 6
        LOCK_FAILED          = 7
        INVALID_TARGET       = 8
        INVALID_TIME_ZONE    = 9
        OTHERS               = 10.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

  ENDLOOP.

ENDFORM.

*<-- Start of Insertion 420000429 18.02.2025 (New Subroutines)
*----------------------------------------------------------------------*
*  Form F_UPDATE_PRODH_FROM_SERVMAT
*----------------------------------------------------------------------*
*  Update Product Hierarchy based on Service Material
*----------------------------------------------------------------------*
FORM F_UPDATE_PRODH_FROM_SERVMAT  USING  UF_HEADER_GUID    TYPE CRMT_OBJECT_GUID
                                         US_ORGMAN_WRK    TYPE CRMT_ORGMAN_WRK
                                         UF_CLEAR         TYPE FLAG.

  CONSTANTS:
    LC_SVMAT    TYPE  MARA-MATKL VALUE 'SV%'.

  DATA:
    LT_ORDERADM_I       TYPE  CRMT_ORDERADM_I_WRKT,
    LT_TEMP             TYPE  STANDARD TABLE OF CRMT_ORDERADM_I_WRK,
    LT_INPUT_FIELD_NAME TYPE  CRMT_INPUT_FIELD_NAMES_TAB.

  DATA:
    LS_PRODUCT_I          TYPE CRMT_PRODUCT_I_WRK,
    LS_PRODUCT_I_COM      TYPE CRMT_PRODUCT_I_COM,
    LS_PRODUCT_SALES_DATA TYPE CRMT_PRODUCT_SALES_API_EXP.

  DATA:
    LF_PROD_HIERARCHY     TYPE CRMT_PRODUCT_I_WRK-PROD_HIERARCHY,
    LF_CHK_PROD_HIERARCHY TYPE CRMT_PRODUCT_I_WRK-PROD_HIERARCHY.


* Get all items
  CALL FUNCTION 'CRM_ORDERADM_I_READ_OB'
    EXPORTING
      IV_HEADER                = UF_HEADER_GUID
      IV_INCLUDE_DELETED_ITEMS = ' '
    IMPORTING
      ET_ORDERADM_I_WRK        = LT_ORDERADM_I
    EXCEPTIONS
      ITEM_DOES_NOT_EXIST      = 1
      ERROR_OCCURRED           = 2
      OTHERS                   = 3.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  CLEAR: LF_PROD_HIERARCHY.
  LT_TEMP = LT_ORDERADM_I.
* Find 1st service material
  SELECT X~GUID,
         X~NUMBER_INT,
         X~ORDERED_PROD,
         A~MATKL
    FROM @LT_TEMP AS X ##ITAB_KEY_IN_SELECT
           INNER JOIN MARA AS A
             ON  A~MATNR = X~ORDERED_PROD
   WHERE A~MATKL  LIKE  @LC_SVMAT
   ORDER BY X~NUMBER_INT ASCENDING
    INTO @DATA(LS_SERVMAT_ITEM)
      UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC EQ 0.
*   Determine new Product hierarchy
    PERFORM F_GET_NEW_PROD_HIERARCHY  USING  UF_HEADER_GUID
                                             LS_SERVMAT_ITEM-GUID
                                             US_ORGMAN_WRK-SALES_ORG_SD
                                    CHANGING LF_PROD_HIERARCHY.
  ENDIF.
  FREE LT_TEMP.
  IF LF_PROD_HIERARCHY IS INITIAL AND
     UF_CLEAR IS INITIAL.
*   Cannot determine Product hierarchy
    RETURN.
  ENDIF.

* Update New product hierarchy into all items
  LOOP AT LT_ORDERADM_I ASSIGNING FIELD-SYMBOL(<L_ORDERADM_I_WRK>).

*   Read Product Object Old
    CALL FUNCTION 'CRM_PRODUCT_I_READ_OW'
      EXPORTING
        IV_GUID                   = <L_ORDERADM_I_WRK>-GUID
      IMPORTING
        ES_PRODUCT_I              = LS_PRODUCT_I
      EXCEPTIONS
        PRODUCT_I_NOT_IN_WORKAREA = 1
        OTHERS                    = 2.
    IF SY-SUBRC <> 0.
      CLEAR LS_PRODUCT_I.
    ENDIF.

*   Assign Checking Product Hierarchy
    IF LF_PROD_HIERARCHY IS NOT INITIAL.
      LF_CHK_PROD_HIERARCHY = LF_PROD_HIERARCHY.
    ELSEIF UF_CLEAR EQ GC_TRUE.
*     Refresh to determine from material master
      CALL FUNCTION 'CRM_PRODUCT_I_READ_PRODUCT_OW'
        EXPORTING
          IV_ITEM_GUID          = <L_ORDERADM_I_WRK>-GUID
          IV_PRODUCT_GUID       = <L_ORDERADM_I_WRK>-PRODUCT
        IMPORTING
          ES_PRODUCT_SALES_DATA = LS_PRODUCT_SALES_DATA
        EXCEPTIONS
          PRODUCT_NOT_FOUND     = 1
          OTHERS                = 2.
      IF SY-SUBRC <> 0.
        CONTINUE.
      ENDIF.
      LF_CHK_PROD_HIERARCHY = LS_PRODUCT_SALES_DATA-PROD_HIERARCHY.
    ELSE.
      CONTINUE.
    ENDIF.

*   Ignore If Same ProdH
    IF LS_PRODUCT_I-PROD_HIERARCHY EQ LF_CHK_PROD_HIERARCHY.
      CONTINUE.
    ENDIF.

*   Update Product Hierarchy
    CLEAR LS_PRODUCT_I_COM.
    LS_PRODUCT_I_COM-REF_GUID       = <L_ORDERADM_I_WRK>-GUID.
    LS_PRODUCT_I_COM-PROD_HIERARCHY = LF_CHK_PROD_HIERARCHY.
    INSERT VALUE #( FIELDNAME = 'PROD_HIERARCHY' )
             INTO TABLE LT_INPUT_FIELD_NAME.

    CALL FUNCTION 'CRM_PRODUCT_I_MAINTAIN_OW'
      EXPORTING
        IS_PRODUCT_I_COM       = LS_PRODUCT_I_COM
      CHANGING
        CT_INPUT_FIELD_NAMES   = LT_INPUT_FIELD_NAME
      EXCEPTIONS
        PRODUCT_I_CHANGE_ERROR = 1
        PRODUCT_I_CREATE_ERROR = 2
        ERROR_OCCURRED         = 3
        OTHERS                 = 4.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
             RAISING ABORT.
    ENDIF.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_NEW_PROD_HIERARCHY
*----------------------------------------------------------------------*
*  Get New Product Hierarchy from Service material item
*----------------------------------------------------------------------*
FORM F_GET_NEW_PROD_HIERARCHY  USING  UF_HEADER_GUID TYPE CRMT_OBJECT_GUID
                                      UF_ITEM_GUID TYPE CRMT_OBJECT_GUID
                                      UF_SALES_ORG_SD TYPE CRMT_ORGMAN_WRK-SALES_ORG_SD
                             CHANGING CF_PROD_HIERARCHY TYPE CRMT_PRODUCT_I_WRK-PROD_HIERARCHY.

  DATA:
    LS_ORDERADM_H_WRK TYPE  CRMT_ORDERADM_H_WRK,
    LS_PRODUCT_I      TYPE  CRMT_PRODUCT_I_WRK,
    LS_PRODH_SERVMAT  TYPE  PRODH,
    LS_PRODH          TYPE  PRODH.


* Initialize Output
  CLEAR: CF_PROD_HIERARCHY.

* Read Header Data
  CALL FUNCTION 'CRM_ORDERADM_H_READ_OB'
    EXPORTING
      IV_GUID                       = UF_HEADER_GUID
    IMPORTING
      ES_ORDERADM_H_WRK             = LS_ORDERADM_H_WRK
    EXCEPTIONS
      PARAMETER_ERROR               = 1
      RECORD_NOT_FOUND              = 2
      AT_LEAST_ONE_RECORD_NOT_FOUND = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

* Read LV1 value from GenC mapping using Order type
  READ TABLE GT_MAP_PRODH1 ASSIGNING FIELD-SYMBOL(<L_MAP_PRODH1>)
                           WITH KEY SALES_ORG_SD = UF_SALES_ORG_SD
                                    PROCESS_TYPE = LS_ORDERADM_H_WRK-PROCESS_TYPE
                           BINARY SEARCH.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Read Product Object of Service Mat Item
  CALL FUNCTION 'CRM_PRODUCT_I_READ_OW'
    EXPORTING
      IV_GUID                   = UF_ITEM_GUID
    IMPORTING
      ES_PRODUCT_I              = LS_PRODUCT_I
    EXCEPTIONS
      PRODUCT_I_NOT_IN_WORKAREA = 1
      OTHERS                    = 2.
  IF SY-SUBRC <> 0.
    CLEAR LS_PRODUCT_I.
  ENDIF.

  LS_PRODH_SERVMAT = LS_PRODUCT_I-PROD_HIERARCHY.

* Assign New Product hierachy
  LS_PRODH-PRODH1 = <L_MAP_PRODH1>-PRODH1.
  LS_PRODH-PRODH2 = LS_PRODH_SERVMAT-PRODH2.
  LS_PRODH-PRODH3 = LS_PRODH_SERVMAT-PRODH3.

* Assign Output
  CF_PROD_HIERARCHY = LS_PRODH.

ENDFORM.
*--> End of Insertion 420000429 18.02.2025

*-----------------------------------------------------------------------
*  Program ID         : Z_SDSCM_PROD_HIER_DETERMINE_EC
*  Creation Date      : 16.05.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : ZCME006
*  Description        : Product Hierarchy Determination
*  Purpose            : Product Hierarchy Determination
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer    Description
*-----------------------------------------------------------------------
*  18.02.2025  420000429   Wuthichai L.  - CME027 Add new logic to
*                                          determine Product hierarchy
*                                          from service material
*-----------------------------------------------------------------------
FUNCTION Z_SDSCM_PROD_HIER_DETERMINE_EC .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_HEADER_GUID) TYPE  CRMT_OBJECT_GUID
*"     REFERENCE(IV_OBJECT_GUID) TYPE  CRMT_OBJECT_GUID
*"     REFERENCE(IV_OBJECT_NAME) TYPE  CRMT_OBJECT_NAME
*"     REFERENCE(IV_STRVAL_NEW) TYPE  ANY OPTIONAL
*"     REFERENCE(IV_STRVAL_OLD) TYPE  ANY OPTIONAL
*"     REFERENCE(IV_EVENT) TYPE  CRMT_EVENT OPTIONAL
*"     REFERENCE(IV_ATTRIBUT) TYPE  CRMT_EVENT_ATTRIBUT OPTIONAL
*"  EXCEPTIONS
*"      ABORT
*"----------------------------------------------------------------------

  DATA:
    LS_ORGMAN_WRK    TYPE CRMT_ORGMAN_WRK,
    LS_ORDER_I_NEW   TYPE CRMT_ORDERADM_I_WRK,
    LS_ORDER_I_OLD   TYPE CRMT_ORDERADM_I_WRK,
    LS_PRODUCT_I_NEW TYPE CRMT_PRODUCT_I_WRK,
    LS_PRODUCT_I_OLD TYPE CRMT_PRODUCT_I_WRK.

  DATA:
    LF_CLEAR  TYPE  FLAG.                                   "+420000429


* Read Constants
  PERFORM F_GET_CONSTANTS.

* Read Organization data
  CALL FUNCTION 'CRM_ORGMAN_READ_OW'
    EXPORTING
      IV_REF_GUID          = IV_HEADER_GUID
      IV_REF_KIND          = 'A'
    IMPORTING
      ES_ORGMAN_WRK        = LS_ORGMAN_WRK
    EXCEPTIONS
      ENTRY_DOES_NOT_EXIST = 1
      OTHERS               = 2.
  IF SY-SUBRC <> 0.
*   Cannot read organization data
    RETURN.
  ENDIF.

* Check Sales Org activated for processing?
  IF NOT ( GR_ACTIVE_VKORG IS NOT INITIAL AND
           LS_ORGMAN_WRK-SALES_ORG_SD IN GR_ACTIVE_VKORG ).
    RETURN.
  ENDIF.

  CASE IV_OBJECT_NAME.
    WHEN 'ORDERADM_I'.
      LS_ORDER_I_NEW = CORRESPONDING #( IV_STRVAL_NEW ).
      LS_ORDER_I_OLD = CORRESPONDING #( IV_STRVAL_OLD ).

*     ------------------------------------------------------
*     Mode 1: Assign Product Hierarchy from Parent
*              - After create with Parenet assigned
*              - After change Parent is changed
*     ------------------------------------------------------
      IF LS_ORDER_I_NEW-NUMBER_PARENT IS NOT INITIAL  AND
         ( IV_EVENT EQ GC_AFTER_CREATE OR
           ( IV_EVENT EQ GC_AFTER_CHANGE AND
              LS_ORDER_I_OLD-NUMBER_PARENT NE LS_ORDER_I_NEW-NUMBER_PARENT )
          ).
        PERFORM F_UPDATE_PRODH_FROM_PARENT  USING  LS_ORDER_I_NEW-PARENT
                                                   IV_OBJECT_GUID.

*     ------------------------------------------------------
*     Mode 2: Assign Product Hierarchy from Product
*              - After change Parent from non-blank to blank
*     ------------------------------------------------------
      ELSEIF LS_ORDER_I_NEW-NUMBER_PARENT IS INITIAL  AND
             IV_EVENT EQ GC_AFTER_CHANGE AND
             LS_ORDER_I_OLD-NUMBER_PARENT IS NOT INITIAL.

        PERFORM F_UPDATE_PRODH_FROM_PRODUCT  USING  LS_ORDER_I_NEW-PRODUCT
                                                    IV_OBJECT_GUID.

*<-- Start of Insertion 420000429 18.02.2025 (New Product Hierarchy Determination)
      ELSE.
*     ------------------------------------------------------
*     Mode 4: Assign Product Hierarchy from New Logic
*             - When Service material is found, change all
*               product hierarchy in all items with:-
*                   - Lv1: from GenC mapping by document type
*                   - Lv2 and 3: from Product hierarchy of
*                     service material found
*     ------------------------------------------------------
*       Determine clear (Reset) flag
        CLEAR LF_CLEAR.
        IF IV_EVENT EQ GC_AFTER_DELETE.
          LF_CLEAR = GC_TRUE.
        ENDIF.
        PERFORM F_UPDATE_PRODH_FROM_SERVMAT  USING  IV_HEADER_GUID
                                                    LS_ORGMAN_WRK
                                                    LF_CLEAR.
*--> End of Insertion 420000429 18.02.2025
      ENDIF.

    WHEN 'PRODUCT_I'.
      LS_PRODUCT_I_NEW = IV_STRVAL_NEW.
      LS_PRODUCT_I_OLD = IV_STRVAL_OLD.

*     ------------------------------------------------------
*     Mode 3: Assign Product Hierarchy to all childs
*              - After change Product Hierarchy
*     ------------------------------------------------------
      IF IV_EVENT EQ GC_AFTER_CHANGE AND
         LS_PRODUCT_I_NEW-PROD_HIERARCHY NE LS_PRODUCT_I_OLD-PROD_HIERARCHY.

        PERFORM F_UPDATE_PRODH_IN_CHILDS  USING  IV_OBJECT_GUID
                                                 IV_HEADER_GUID
                                                 LS_ORGMAN_WRK "+420000429
                                                 LS_PRODUCT_I_NEW-PROD_HIERARCHY.

      ENDIF.

    WHEN OTHERS.
      RETURN.

  ENDCASE.

ENDFUNCTION.

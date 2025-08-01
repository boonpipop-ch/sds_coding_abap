FUNCTION-POOL ZSDSCM01.                     "MESSAGE-ID ..
*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: TT_OBJID_RANGE TYPE  RANGE OF CRMS4D_SERV_H-OBJECT_ID.
TYPES: TT_PROC_TYPE_RANGE  TYPE  RANGE OF CRMS4D_SERV_H-PROCESS_TYPE.

*<-- Start of Insertion 420000429 18.02.2025 (New Data Types)
TYPES: BEGIN OF TS_MAP_PRODH1,
         SALES_ORG_SD TYPE  CRMS4D_SERV_H-SALES_ORG_SD,
         PROCESS_TYPE TYPE  CRMS4D_SERV_H-PROCESS_TYPE,
         PRODH1       TYPE  PRODH-PRODH1,
       END OF TS_MAP_PRODH1.
TYPES: TT_MAP_PRODH1 TYPE SORTED TABLE OF TS_MAP_PRODH1
                          WITH UNIQUE KEY SALES_ORG_SD
                                          PROCESS_TYPE.
*--> End of Insertion 420000429 18.02.2025

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE         TYPE  CHAR1      VALUE 'X',
  GC_AFTER_CREATE TYPE  CRMT_EVENT VALUE 'AFTER_CREATE',
  GC_AFTER_CHANGE TYPE  CRMT_EVENT VALUE 'AFTER_CHANGE',
  GC_AFTER_DELETE TYPE  CRMT_EVENT VALUE 'AFTER_DELETE'.    "+420000429

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*
DATA:
  GR_OBJID  TYPE  TT_OBJID_RANGE                              ##NEEDED.

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*
DATA:
  GR_ACTIVE_VKORG TYPE  RANGE OF CRMT_ORGMAN_WRK-SALES_ORG_SD ##NEEDED,
  GR_COMM         TYPE  TT_PROC_TYPE_RANGE                    ##NEEDED,
  GR_INST         TYPE  TT_PROC_TYPE_RANGE                    ##NEEDED.

*<-- Start of Insertion 420000429 18.02.2025 (New GenC Variables)
DATA:
  GT_MAP_PRODH1   TYPE  TT_MAP_PRODH1                         ##NEEDED.
*--> End of Insertion 420000429 18.02.2025

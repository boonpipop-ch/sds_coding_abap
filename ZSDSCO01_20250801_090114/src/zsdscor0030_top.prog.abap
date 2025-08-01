*&---------------------------------------------------------------------*
*& Include          ZSDSCOR0010_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   T A B L E S                                                        *
*----------------------------------------------------------------------*
TABLES: BKPF, PRPS, ACDOCA.

*----------------------------------------------------------------------*
*  Include                                                             *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*  Constant                                                            *
*----------------------------------------------------------------------*
CONSTANTS: "GC_KOKRS_1000 TYPE CODIA-KOKRS VALUE '1000',
           GC_TCODE      TYPE SY-TCODE VALUE 'ZSDSCO030'.

TYPES: BEGIN OF TY_HEADER,
         REF_BUKRS TYPE BKPF-BUKRS,
         REF_BELNR TYPE BKPF-BELNR,
         REF_GJAHR TYPE BKPF-GJAHR,
         REF_AWKEY TYPE BKPF-AWKEY,
         WAERS     TYPE WAERS,
       END OF TY_HEADER.

TYPES: BEGIN OF TY_ITEM,
*         REF_BUZEI TYPE BUZEI,
         BUZEI     TYPE BUZEI,
         BSCHL     TYPE BSCHL,
         RACCT     TYPE RACCT,
         WRBTR     TYPE WRBTR,
         PS_POSID  TYPE ACDOCA-PS_POSID,
         PAOBJNR   TYPE ACDOCA-PAOBJNR,
         SGTXT     TYPE SGTXT,
       END OF TY_ITEM.

TYPES: BEGIN OF G_TYPE_S_MASTER.
TYPES:   EXCEPTION  TYPE CHAR01.
         INCLUDE TYPE TY_HEADER.
TYPES: T_CELLTYPE TYPE SALV_T_INT4_COLUMN,
         EXPAND     TYPE CHAR01,
         BUKRS      TYPE BKPF-BUKRS,
         BELNR      TYPE BKPF-GJAHR,
         GJAHR      TYPE BKPF-GJAHR,
         MSGTX      TYPE NATXT,
         END OF G_TYPE_S_MASTER,

         BEGIN OF G_TYPE_S_SLAVE.
           INCLUDE TYPE TY_HEADER.
           INCLUDE TYPE TY_ITEM.
           INCLUDE TYPE ACDOC_SI_GL_ACCAS.
           INCLUDE TYPE ACDOC_SI_COPA.
TYPES END OF G_TYPE_S_SLAVE.
*TYPES: TT_POST TYPE STANDARD TABLE OF TS_POST.
*------ Document header and items for internal posting interface
TYPES: TT_FTPOST TYPE STANDARD TABLE OF FTPOST,
*------ Tax Data
       TT_FTTAX  TYPE STANDARD TABLE OF FTTAX,
*------ Financial Accounting document number table
       TT_BLNTAB TYPE STANDARD TABLE OF BLNTAB.

CLASS LCL_HANDLE_EVENTS DEFINITION DEFERRED.

TYPES: GTY_MASTER TYPE STANDARD TABLE OF G_TYPE_S_MASTER.
TYPES: GTY_SLAVE TYPE STANDARD TABLE OF G_TYPE_S_SLAVE.

DATA: GT_MASTER TYPE GTY_MASTER ##NEEDED,
      GT_SLAVE  TYPE GTY_SLAVE  ##NEEDED.
##NEEDED DATA: GR_HIERSEQ TYPE REF TO CL_SALV_HIERSEQ_TABLE.
*..Interface posting
DATA: GT_FTPOST TYPE TT_FTPOST ##NEEDED,
      GT_FTTAX  TYPE TT_FTTAX ##NEEDED,
      GT_BLNTAB TYPE TT_BLNTAB ##NEEDED,
      GS_FTPOST TYPE FTPOST ##NEEDED,
      GF_COUNT  TYPE I ##NEEDED.

##NEEDED DATA: GR_EVENTS TYPE REF TO LCL_HANDLE_EVENTS.

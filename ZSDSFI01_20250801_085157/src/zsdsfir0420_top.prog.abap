*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0420_TOP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   T A B L E S                                                        *
*----------------------------------------------------------------------*
TABLES: MARA, VBRK, VBAK, ZSDSCMT003 .

*----------------------------------------------------------------------*
*  Constant                                                            *
*----------------------------------------------------------------------*
CONSTANTS: GC_TCODE       TYPE SY-TCODE VALUE 'ZSDSFI039',
           GC_DEBIT       TYPE BSCHL VALUE '40',
           GC_CREDIT      TYPE BSCHL VALUE '50',
           GC_DEBIT_ACC   TYPE RACCT VALUE '2211000010',
           GC_CREDIT_ACC  TYPE RACCT VALUE '5411000010',
           GC_BUPLA_0000  TYPE BUPLA VALUE '0000',
           GC_KOSTL_DUMMY TYPE KOSTL VALUE '0010999009'.
* Service Support >> Dummy Cost center

TYPES: BEGIN OF TY_ACDOCA.
TYPES: BUKRS   TYPE BKPF-BUKRS,
       BELNR   TYPE BKPF-BELNR,
       GJAHR   TYPE BKPF-GJAHR,
       BUZEI   TYPE BSEG-BUZEI,
       AWKEY   TYPE BSEG-AWKEY,
       MATNR   TYPE BSEG-MATNR,
       HKONT   TYPE BSEG-HKONT,
       XREF3   TYPE CHAR18,      "BSEG-HKONT,
       PAOBJNR TYPE BSEG-PAOBJNR,

       AUFNR   TYPE BSEG-AUFNR,  "CH01
       PROJK   TYPE BSEG-PROJK,  "CH01
       KOSTL   TYPE BSEG-KOSTL,  "CH01


       XBLNR   TYPE BKPF-XBLNR,
       BUDAT   TYPE BKPF-BUDAT,
       BLART   TYPE BKPF-BLART,
       BKTXT   TYPE BKPF-BKTXT,
       TXT50   TYPE SKAT-TXT50,
       RACCT   TYPE ACDOCA-RACCT,
       WSL     TYPE ACDOCA-WSL,
       RWCUR   TYPE ACDOCA-RWCUR,
       HSL     TYPE ACDOCA-HSL,
       RHCUR   TYPE ACDOCA-RHCUR.
       INCLUDE TYPE ACDOC_SI_GL_ACCAS.
       INCLUDE TYPE ACDOC_SI_COPA.
TYPES: END OF TY_ACDOCA.
TYPES: TTY_ACDOCA TYPE STANDARD TABLE OF TY_ACDOCA.

TYPES: BEGIN OF TY_HEADER.
         INCLUDE TYPE ZSDSFIS125.
TYPES: END OF TY_HEADER.

TYPES: BEGIN OF TY_ITEM,
         BUZEI    TYPE BUZEI,
         BSCHL    TYPE BSCHL,
         RACCT    TYPE RACCT,
         SGTXT    TYPE SGTXT,
         WRBTR    TYPE WRBTR,
         MWSKZ    TYPE MWSKZ,
         XREF3    TYPE XREF3,
         BUPLA    TYPE BUPLA,
         KOSTL    TYPE KOSTL,
         MATNR    TYPE MARA-MATNR,
*         PS_POSID TYPE ACDOCA-PS_POSID, "CH01-
         PAOBJNR  TYPE RKEOBJNR,
         AUFNR    TYPE BSEG-AUFNR, "CH01+
         PROJK    TYPE BSEG-PROJK, "CH01+
*         PRCTR    TYPE BSEG-PRCTR,
       END OF TY_ITEM.

TYPES: BEGIN OF G_TYPE_S_MASTER.
TYPES:   EXCEPTION  TYPE CHAR01.
         INCLUDE TYPE TY_HEADER.
TYPES: T_CELLTYPE TYPE SALV_T_INT4_COLUMN,
         EXPAND     TYPE CHAR01,
         MSGTX      TYPE NATXT,
         BUKRS      TYPE BKPF-BUKRS,
         BELNR      TYPE BKPF-BELNR,
         GJAHR      TYPE BKPF-GJAHR,
         BLART      TYPE BKPF-BLART,
         WAERS      TYPE BKPF-WAERS,
         BUDAT      TYPE BKPF-BUDAT,
         XBLNR      TYPE BKPF-XBLNR,
         BKTXT      TYPE BKPF-BKTXT,
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

*CLASS LCL_HANDLE_EVENTS DEFINITION DEFERRED.

TYPES: GTY_MASTER TYPE STANDARD TABLE OF G_TYPE_S_MASTER.
TYPES: GTY_SLAVE TYPE STANDARD TABLE OF G_TYPE_S_SLAVE.
TYPES: GTY_DATA  TYPE STANDARD TABLE OF ZSDSFIS128.
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
DATA: GR_TABLE   TYPE REF TO CL_SALV_TABLE.                 "#EC NEEDED
DATA: GT_OUTTAB  TYPE STANDARD TABLE OF ZSDSFIS128.         "#EC NEEDED

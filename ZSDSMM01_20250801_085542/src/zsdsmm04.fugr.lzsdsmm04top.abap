FUNCTION-POOL ZSDSMM04.                     "MESSAGE-ID ..

TYPES : BEGIN OF GY_REF_EXP,
          ASSET_LINE   TYPE I,
          EXPENSE_LINE TYPE I,
        END OF GY_REF_EXP.

TYPES : BEGIN OF GY_BOM,
          BOM TYPE MATNR,
          COM TYPE MATNR,
          DES TYPE MAKT-MAKTX,
        END OF GY_BOM.

CONSTANTS : BEGIN OF GC_CONSTANT,
              CO        TYPE C LENGTH 3 VALUE 'COS',
              IO        TYPE C LENGTH 2 VALUE 'IO',
              WBS       TYPE C LENGTH 3 VALUE 'WBS',
              R         TYPE C LENGTH 1 VALUE 'R',
              A         TYPE C LENGTH 1 VALUE 'A',
              GET       TYPE C LENGTH 3 VALUE 'GET',
              S         TYPE C LENGTH 1 VALUE 'S',
              E         TYPE C LENGTH 1 VALUE 'E',
              G         TYPE C LENGTH 1 VALUE 'G',
              P         TYPE C LENGTH 1 VALUE 'P',
              B         TYPE C LENGTH 1 VALUE 'B',
              C         TYPE C LENGTH 1 VALUE 'C',
              D         TYPE C LENGTH 1 VALUE 'D',
              K         TYPE C LENGTH 1 VALUE 'K',
              H         TYPE C LENGTH 1 VALUE 'H',
              A_L       TYPE C LENGTH 1 VALUE 'a',
              B_L       TYPE C LENGTH 1 VALUE 'b',
              C_L       TYPE C LENGTH 1 VALUE 'c',
              D_L       TYPE C LENGTH 1 VALUE 'd',
              H_L       TYPE C LENGTH 1 VALUE 'h',
              E_L       TYPE C LENGTH 1 VALUE 'e',
              ZPR       TYPE C LENGTH 3 VALUE 'ZPR',
              COMC      TYPE C LENGTH 4 VALUE '1000',
              COAR      TYPE C LENGTH 4 VALUE '1000',
              AUC       TYPE C LENGTH 6 VALUE 'FA-AUC',
              ITEM_TEXT TYPE C LENGTH 3 VALUE 'B01',
              MEMO      TYPE C LENGTH 3 VALUE 'B72',
            END OF GC_CONSTANT.

DATA : BEGIN OF GS_DATA,
         GT_ITEM      TYPE zSDSMMS003_TT,
         GS_HEADER    TYPE ZSDSMMS002,
         GT_ASSET     TYPE ZSDSFIS012_TT,
         GT_ASSET_TMP TYPE ZSDSFIS012_TT,
         GV_PR_SAP    TYPE BANFN,
         GV_MESSAGE   TYPE CHAR255,
         GV_EMAIL     TYPE FLAG,
         GV_GL_AUC    TYPE ZSDSMMT019-SAKNR,
         GV_LOCK      TYPE FLAG,
         GT_REF_EXP   TYPE TABLE OF GY_REF_EXP,
         GS_REF_EXP   TYPE GY_REF_EXP,
         GT_BOM       TYPE TABLE OF GY_BOM,
         GR_UOM       TYPE RANGE OF ZSDSCAC001-VALUE_LOW,
         GV_COATING   TYPE C,
         GV_EKGRP     TYPE EKGRP,
       END OF GS_DATA.

DATA : BEGIN OF GS_MAP_GL,
         MATKL TYPE ZSDSMMT019-MATKL,
         SAKNR TYPE ZSDSMMT019-SAKNR,
       END OF GS_MAP_GL.
DATA GT_MAP_GL LIKE TABLE OF GS_MAP_GL.




* INCLUDE LZSDSMM04D...                      " Local class definition

*&---------------------------------------------------------------------*
*& Include          ZCOR0100T01
*&---------------------------------------------------------------------*

TABLES: SSCRFIELDS,
        CEPC,
        PROJ,
        COBRB.
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
*CONSTANTS: GC_KOKRS TYPE KOKRS VALUE '1000'.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. TY_  : Global, Local Types

*----------------------------------------------------------------------*
* VARIABLE
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. GV_  : Global Variable
*   2. LV_  : Local Variable

*EX) DATA: GV_EXIT   TYPE C,
*          GV_ANSWER TYPE C,
*          OK_CODE   TYPE SY-UCOMM,   "예외
*          SAVE_OK   TYPE SY-UCOMM.   "예외

DATA: GV_REPID TYPE SYREPID.

DATA: GO_ALV        TYPE REF TO CL_SALV_TABLE,
      GO_FUNCTIONS  TYPE REF TO CL_SALV_FUNCTIONS_LIST,
      GO_LAYOUT     TYPE REF TO CL_SALV_LAYOUT,
      GO_DSPSET     TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
      GS_KEY        TYPE SALV_S_LAYOUT_KEY,
      GT_COLUMN_REF TYPE SALV_T_COLUMN_REF.

TYPES: BEGIN OF TY_LAYOUT,
         REPID    TYPE SYREPID,
         RESTRICT TYPE SALV_DE_LAYOUT_RESTRICTION,
         DEFAULT  TYPE SAP_BOOL,
         LAYOUT   TYPE DISVARIANT-VARIANT,
       END OF TY_LAYOUT.

DATA: GS_SALV_LAYOUT TYPE TY_LAYOUT.

DATA: GV_COUNT TYPE I,
      GV_FAIL  TYPE I.

DATA: GR_COLUMN TYPE REF TO CL_SALV_COLUMN_TABLE.

DATA: GV_COLUMN_TEXT TYPE STRING,
      GV_SCRTEXT_S   TYPE SCRTEXT_S,
      GV_SCRTEXT_M   TYPE SCRTEXT_M,
      GV_SCRTEXT_L   TYPE SCRTEXT_L,
      GV_TOOLTIP     TYPE LVC_TIP.

DATA: GV_MODE.
*----------------------------------------------------------------------*
* STRUCTURE
*----------------------------------------------------------------------*
DATA: GS_OUTTAB TYPE ZCOS0100.
*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
DATA: GT_ITAB   TYPE TABLE OF ZCOS0100 WITH HEADER LINE,
      GT_OUTTAB LIKE TABLE OF ZCOS0100.

*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*
RANGES: R_PBUKR FOR PRPS-PBUKR,
        R_PRCTR FOR CEPC-PRCTR.

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* MACRO (Define)
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. _ : '_' 1개로 시작; Global, Local 구분 없음.
DEFINE _CONVERSION_IN.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
  input = &1
  IMPORTING
  output = &1.
END-OF-DEFINITION.

DEFINE _CONVERSION_OUT.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
  EXPORTING
  input = &1
  IMPORTING
  output = &1.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* Table Controls
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. TC_ : Table Controls

*----------------------------------------------------------------------*
* Custom Controls
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. CC_ : Custom Controls

*----------------------------------------------------------------------*
* Tabstrip Controls
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. TS_ : Tabstrip Controls

*&---------------------------------------------------------------------*
*& Include          ZCOR0140T01
*&---------------------------------------------------------------------*
TABLES: SSCRFIELDS.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS GC_KTOPL TYPE KTOPL VALUE '1000'.

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

DATA: GV_EXIT   TYPE C,
      GV_ANSWER TYPE C,
      OK_CODE   TYPE SY-UCOMM,   "예외
      SAVE_OK   TYPE SY-UCOMM.   "예외

DATA: GV_ZBSUP LIKE ZCOT0140-ZBSUP,
      GV_WAERS TYPE ZCOT0140-WAERS VALUE 'KRW'.

*DATA: GV_REPID TYPE SYREPID.

*DATA: GO_ALV        TYPE REF TO CL_SALV_TABLE,
*      GO_FUNCTIONS  TYPE REF TO CL_SALV_FUNCTIONS_LIST,
*      GO_LAYOUT     TYPE REF TO CL_SALV_LAYOUT,
*      GO_DSPSET     TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
*      GS_KEY        TYPE SALV_S_LAYOUT_KEY,
*      GT_COLUMN_REF TYPE SALV_T_COLUMN_REF.
*
*TYPES: BEGIN OF TY_LAYOUT,
*         REPID    TYPE SYREPID,
*         RESTRICT TYPE SALV_DE_LAYOUT_RESTRICTION,
*         DEFAULT  TYPE SAP_BOOL,
*         LAYOUT   TYPE DISVARIANT-VARIANT,
*       END OF TY_LAYOUT.
*
*DATA: GS_SALV_LAYOUT TYPE TY_LAYOUT.
*
*DATA: GV_COUNT TYPE I,
*      GV_FAIL  TYPE I.
*
*DATA: GR_COLUMN TYPE REF TO CL_SALV_COLUMN_TABLE.
*
*DATA: GV_COLUMN_TEXT TYPE STRING,
*      GV_SCRTEXT_S   TYPE SCRTEXT_S,
*      GV_SCRTEXT_M   TYPE SCRTEXT_M,
*      GV_SCRTEXT_L   TYPE SCRTEXT_L.
*
*DATA: GV_MODE.
*----------------------------------------------------------------------*
* STRUCTURE
*----------------------------------------------------------------------*
*DATA: GS_ITAB TYPE ZCOS0140.

DATA: BEGIN OF GS_ITAB.
        INCLUDE TYPE ZCOS0140.
        DATA:   STYLE TYPE LVC_T_STYL,
      END OF GS_ITAB.

DATA: GS_OUTTAB LIKE GS_ITAB,
      GS_MODTAB LIKE GS_ITAB.

DATA: GS_ZCOT0140 TYPE ZCOT0140.

*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
*DATA: GT_DISPLAY  TYPE TABLE OF ZCOS0140 WITH HEADER LINE.
*
*DATA: GT_OUTTAB TYPE TABLE OF ZCOS0140,
*      GS_OUTTAB TYPE ZCOS0140,
*      GS_MODTAB TYPE ZCOS0140.

DATA: GT_DISPLAY LIKE TABLE OF GS_ITAB WITH HEADER LINE,
      GT_OUTTAB  LIKE TABLE OF GS_ITAB.

DATA: GT_ZCOT0140 TYPE TABLE OF ZCOT0140,
      GT_TRANSFER LIKE TABLE OF GS_ITAB WITH HEADER LINE.

DATA: GT_RETURN TYPE TABLE OF BAPIRET2 WITH HEADER LINE.

*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*
*RANGES: R_VBUKR FOR PROJ-VBUKR,
*        R_PRCTR FOR CEPC-PRCTR.

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
      INPUT  = &1
    IMPORTING
      OUTPUT = &1.
END-OF-DEFINITION.

DEFINE _CONVERSION_OUT.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
  EXPORTING
  input = &1
  IMPORTING
  output = &1.
END-OF-DEFINITION.

DEFINE _CURR_INPUT.
  WRITE &1 CURRENCY 'KRW' TO &2.
  CONDENSE &2.
END-OF-DEFINITION.

DEFINE _CONV_KRW.
  IF &1 IS NOT INITIAL .
    &1 = &1 / 100.
  ENDIF.
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

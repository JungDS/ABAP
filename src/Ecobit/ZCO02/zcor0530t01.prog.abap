*&---------------------------------------------------------------------*
*& Include          ZCOR0530T01
*&---------------------------------------------------------------------*
TYPE-POOLS : ICON,ABAP.

TABLES : SSCRFIELDS. "선택화면의 필드(Function Key)


CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

*---------------------------------------------------------------------*
* CONSTANTS
*---------------------------------------------------------------------*
CONSTANTS : GC_A     TYPE CHAR01   VALUE 'A',
            GC_S     TYPE CHAR01   VALUE 'S',
            GC_W     TYPE CHAR01   VALUE 'W',
            GC_E     TYPE CHAR01   VALUE 'E',
            GC_X     TYPE CHAR01   VALUE 'X',
            GC_N     TYPE CHAR01   VALUE 'N',
            GC_TCODE TYPE SY-TCODE VALUE ''.

*---------------------------------------------------------------------*
* TYPES
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. TY_  : Global, Local Types
TYPES: BEGIN OF TY_LAYOUT,
         KEY      TYPE SALV_S_LAYOUT_KEY,
         RESTRICT TYPE SALV_DE_LAYOUT_RESTRICTION,
         DEFAULT  TYPE SAP_BOOL,
         LAYOUT   TYPE DISVARIANT-VARIANT,
       END OF TY_LAYOUT.

TYPES: BEGIN OF TY_EXCEL,
         ZZCD1        TYPE ALSMEX_TABLINE-VALUE,
         ZZCD1TX      TYPE ALSMEX_TABLINE-VALUE,
         ZZCD2        TYPE ALSMEX_TABLINE-VALUE,
         ZZCD2TX      TYPE ALSMEX_TABLINE-VALUE,
         ZZCD3        TYPE ALSMEX_TABLINE-VALUE,
         ZZCD3TX      TYPE ALSMEX_TABLINE-VALUE,
       END OF TY_EXCEL.


*---------------------------------------------------------------------*
* VARIABLE
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. GV_  : Global Variable
*   2. LV_  : Local Variable

*EX) DATA: GV_EXIT   TYPE C,
*          GV_ANSWER TYPE C,
*          OK_CODE   TYPE SY-UCOMM,   "예외
*          SAVE_OK   TYPE SY-UCOMM.   "예외

DATA: GV_EXIT     TYPE C,
      GV_ANSWER   TYPE C,
      OK_CODE     TYPE SY-UCOMM,   "예외
      SAVE_OK     TYPE SY-UCOMM.   "예외

DATA: GV_TOTAL    TYPE C LENGTH 30,
      GV_SUCESS   TYPE C LENGTH 30,
      GV_ERROR    TYPE C LENGTH 30.
DATA: GV_MESSAGE  TYPE C LENGTH 100.

DATA: A           TYPE BAPICURR_D.

*---------------------------------------------------------------------*
* STRUCTURE
*---------------------------------------------------------------------*
DATA GS_FUNTXT        TYPE SMP_DYNTXT. "Excel 양식 Download(펑션키)
DATA GS_EXCEL         TYPE TY_EXCEL.


DATA: BEGIN OF GS_OUTTAB ,
         STATUS(1),
         ICON         LIKE ICON-ID,
         ZZCD1        LIKE ZCOT1280-ZZCD1,
         ZZCD1TX      LIKE ZCOT1280T-ZZCD1TX,
         ZZCD2        LIKE ZCOT1290-ZZCD2,
         ZZCD2TX      LIKE ZCOT1290T-ZZCD2TX,
         ZZCD3        LIKE ZCOT1300-ZZCD3,
         ZZCD3TX      LIKE ZCOT1300T-ZZCD3TX,
         MESSAGE      LIKE BAPIRET2-MESSAGE,
       END OF GS_OUTTAB.

*---------------------------------------------------------------------*
* INTERNAL TABLE
*---------------------------------------------------------------------*
DATA GT_INTERN        TYPE TABLE OF ALSMEX_TABLINE WITH HEADER LINE.
DATA GT_RETURN        TYPE TABLE OF BAPIRET2       WITH HEADER LINE.
DATA GT_OUTTAB        LIKE TABLE OF GS_OUTTAB.
DATA GT_OUTTAB_TREE   LIKE TABLE OF GS_OUTTAB.
DATA GT_EXCEL         LIKE TABLE OF GS_EXCEL.

DATA GT_1280          TYPE TABLE OF ZCOT1280  WITH HEADER LINE.
DATA GT_1280C         TYPE TABLE OF ZCOT1280  WITH HEADER LINE.
DATA GT_1280T         TYPE TABLE OF ZCOT1280T WITH HEADER LINE.
DATA GT_1290          TYPE TABLE OF ZCOT1290 	WITH HEADER LINE.
DATA GT_1290C         TYPE TABLE OF ZCOT1290  WITH HEADER LINE.
DATA GT_1290T         TYPE TABLE OF ZCOT1290T WITH HEADER LINE.
DATA GT_1300          TYPE TABLE OF ZCOT1300  WITH HEADER LINE.
DATA GT_1300C         TYPE TABLE OF ZCOT1300  WITH HEADER LINE.
DATA GT_1300T         TYPE TABLE OF ZCOT1300T WITH HEADER LINE.

*---------------------------------------------------------------------*
* ALV
*---------------------------------------------------------------------*
DATA GR_CON_SPLIT   TYPE REF TO CL_GUI_SPLITTER_CONTAINER.
DATA GR_CON_SPLIT2  TYPE REF TO CL_GUI_SPLITTER_CONTAINER.
DATA GR_CON_TOP     TYPE REF TO CL_GUI_CONTAINER.
DATA GR_CON_MAIN    TYPE REF TO CL_GUI_CONTAINER.
DATA GR_CON_MAIN2   TYPE REF TO CL_GUI_CONTAINER.

DATA GR_DDOC        TYPE REF TO CL_DD_DOCUMENT.
DATA GR_ALV         TYPE REF TO ZCL_CO_ALV.
DATA GR_EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

DATA: GT_FIELDCAT     TYPE LVC_T_FCAT,
      GS_FIELDCAT     TYPE LVC_S_FCAT,
      GS_LAYOUT       TYPE LVC_S_LAYO,
      GT_SORT         TYPE LVC_T_SORT,
      GS_SORT         TYPE LVC_S_SORT,
      GT_ROW          TYPE LVC_T_ROID,
      GS_ROW          TYPE LVC_S_ROID.

DATA  GV_REPID        TYPE SYREPID.
DATA  GV_COUNT        TYPE I.
DATA  GV_TREE         TYPE C.

DATA GR_TREE    TYPE REF TO CL_SALV_TREE.
DATA GT_NODE    TYPE TABLE OF MTREESNODE.
DATA GS_NODE    TYPE MTREESNODE.

*---------------------------------------------------------------------*
* RANGES
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. R_  : Global, Local Ranges

* EX) RANGES: R_FKART  FOR VBRK-FKART,
*             GS_FKDAT LIKE LINE OF R_FKART.
*---------------------------------------------------------------------*
* FIELD-SYMBOLS
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. <FS_  >  : Global, Local Field-symbols

* EX) FIELD-SYMBOLS <FS_SFLIGHT> TYPE SFLIGHT.


*---------------------------------------------------------------------*
* MACRO (Define)
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. _ : '_' 1개로 시작; Global, Local 구분 없음.
DEFINE _INITIAL_CHK.
  IF &1 IS INITIAL.
  GS_OUTTAB-STATUS    = 'E'.
  GS_OUTTAB-MESSAGE   = &2.
  ENDIF.
END-OF-DEFINITION.

DEFINE _CONVERSION_IN.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING input  = &1
    IMPORTING output = &1.
END-OF-DEFINITION.

DEFINE _CONVERSION_WBS_IN.
  CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
    EXPORTING input  = &1
    IMPORTING output = &1.
END-OF-DEFINITION.

DEFINE _CONVERSION_WBS_OUT.
  CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
    EXPORTING input  = &1
    IMPORTING output = &1.
END-OF-DEFINITION.

DEFINE _CONVERSION_OUT.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
  EXPORTING input  = &1
  IMPORTING output = &1.
END-OF-DEFINITION.

DEFINE _CLEAR_ITAB.
  CLEAR &1. REFRESH &1.
END-OF-DEFINITION.

DEFINE _MAKE_ICON.
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      NAME                  = &1
      INFO                  = &2
    IMPORTING
      RESULT                = &3
    EXCEPTIONS
      ICON_NOT_FOUND        = 1
      OUTPUTFIELD_TOO_SHORT = 2
      OTHERS                = 3.
END-OF-DEFINITION.


DEFINE _STRING_REPLACE.
  IF &1 IS NOT INITIAL.
    CALL FUNCTION 'STRING_REPLACE'
      EXPORTING
        PATTERN    = ','
        SUBSTITUTE = ''
      CHANGING
        TEXT       = &1.

    CONDENSE  &1.

    TRY.
      MOVE &1 TO A.
    CATCH CX_SY_CONVERSION_NO_NUMBER.
      GS_OUTTAB-STATUS =  'E'.
      GS_OUTTAB-MESSAGE = TEXT-E01.
      CLEAR  &1.
    ENDTRY.
  ENDIF.
END-OF-DEFINITION.


*---------------------------------------------------------------------*
* Table Controls
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. TC_ : Table Controls

*---------------------------------------------------------------------*
* Custom Controls
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. CC_ : Custom Controls

*---------------------------------------------------------------------*
* Tabstrip Controls
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. TS_ : Tabstrip Controls

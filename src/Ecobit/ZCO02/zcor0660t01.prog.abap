*&---------------------------------------------------------------------*
*& Include          ZCOR0660T01
*&---------------------------------------------------------------------*
TYPE-POOLS : ICON,ABAP.

TABLES: SSCRFIELDS. "선택화면의 필드(Function Key)
TABLES: T001.


CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

*---------------------------------------------------------------------*
* CONSTANTS
*---------------------------------------------------------------------*
CONSTANTS : GC_A        TYPE CHAR01   VALUE 'A',
            GC_C        TYPE CHAR01   VALUE 'C',
            GC_D        TYPE CHAR01   VALUE 'D',
            GC_E        TYPE CHAR01   VALUE 'E',
            GC_I        TYPE CHAR01   VALUE 'I',
            GC_N        TYPE CHAR01   VALUE 'N',
            GC_S        TYPE CHAR01   VALUE 'S',
            GC_P        TYPE CHAR01   VALUE 'P',
            GC_W        TYPE CHAR01   VALUE 'W',
            GC_X        TYPE CHAR01   VALUE 'X'.

CONSTANTS : GC_0        TYPE N VALUE '0',
            GC_1        TYPE N VALUE '1',
            GC_2        TYPE N VALUE '2',
            GC_3        TYPE N VALUE '3',
            GC_4        TYPE N VALUE '4'.

*---------------------------------------------------------------------*
* TYPES
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. TY_  : Global, Local Types


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

DATA: GV_EXIT           TYPE C,
      GV_ANSWER         TYPE C,
      OK_CODE           TYPE SY-UCOMM,   "예외
      SAVE_OK           TYPE SY-UCOMM.   "예외

DATA: GV_MENU_TEXT      LIKE SETHEADERT-DESCRIPT.

DATA: GV_KOKRS          LIKE TKA01-KOKRS,
      GV_ERKRS          LIKE TKA01-ERKRS,
      GV_KTOPL          LIKE TKA01-KTOPL,
      GV_CLOSE          TYPE TEXT255.


*---------------------------------------------------------------------*
* STRUCTURE
*---------------------------------------------------------------------*
DATA GS_FUNTXT          TYPE SMP_DYNTXT.

DATA: BEGIN OF GS_DISPLAY,
*        MENU            LIKE SETHEADERT-DESCRIPT, " 메뉴이름
        G1              LIKE SETLEAF-VALFROM,     " G1
        G1TXT           LIKE SETLINET-DESCRIPT,   " G1 그룹명
        G2              LIKE SETLEAF-VALFROM,     " G2
        G2TXT           LIKE SETLINET-DESCRIPT,   " G2 그룹명
        DESCR           LIKE SETLINET-DESCRIPT,   " 작업설명(그룹반영)
        TCODE           LIKE TSTC-TCODE,          " T-Code
        TTEXT           LIKE TSTCT-TTEXT,         " T-Code명
        HELP            LIKE ICON-ID,             " 도움말 등록여부
        CPROG           LIKE SY-CPROG,
        DYNNR           LIKE SY-DYNNR,
        TDIDF           TYPE ZETDIDF,
        SEQNR           LIKE SETLEAF-SEQNR,       " 라인
*        BUKRS           LIKE CE11000-BUKRS,       " 회사
*        GJAHR           LIKE CE11000-GJAHR,       " 연도
*        PERDE           LIKE CE11000-PERDE,       " 월
        STYLE           TYPE LVC_T_STYL,
        COLOR           TYPE LVC_T_SCOL,
      END OF GS_DISPLAY.

DATA: BEGIN OF GS_SET,
        LINEID          LIKE SETLEAF-LINEID,
        VALFROM         LIKE SETLEAF-VALFROM,
        SEQNR           LIKE SETLEAF-SEQNR,
        DESCRIPT        LIKE SETLINET-DESCRIPT,
      END OF GS_SET.

DATA: BEGIN OF GS_TSTCT,
        TCODE           LIKE TSTCT-TCODE,
        TTEXT           LIKE TSTCT-TTEXT,
      END OF GS_TSTCT.

DATA: BEGIN OF GS_HELP,
        CPROG           LIKE ZCAT0010-CPROG,
        DYNNR           LIKE ZCAT0010-DYNNR,
        TDIDF           LIKE ZCAT0010-TDIDF,
      END OF GS_HELP.


*---------------------------------------------------------------------*
* INTERNAL TABLE
*---------------------------------------------------------------------*

DATA GT_DISPLAY       LIKE TABLE OF GS_DISPLAY.
DATA GT_SET           LIKE TABLE OF GS_SET.
DATA GT_TSTCT         LIKE TABLE OF GS_TSTCT.
DATA GT_HELP          LIKE TABLE OF GS_HELP.


*---------------------------------------------------------------------*
* ALV
*---------------------------------------------------------------------*
DATA GR_SPLIT           TYPE REF TO CL_GUI_SPLITTER_CONTAINER.
DATA GR_CON_TOP         TYPE REF TO CL_GUI_CONTAINER.
DATA GR_CON_ALV         TYPE REF TO CL_GUI_CONTAINER.
DATA GR_DDOC            TYPE REF TO CL_DD_DOCUMENT.
DATA GR_ALV             TYPE REF TO ZCL_CO_ALV.
DATA GR_GRID            TYPE REF TO CL_GUI_ALV_GRID.
DATA GR_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER.

*---------------------------------------------------------------------*
* BDC
*---------------------------------------------------------------------*
*DATA GT_BDC_DATA TYPE TABLE OF BDCDATA.
*DATA GS_BDC_DATA TYPE BDCDATA.
*DATA GT_BDC_MSG  TYPE TABLE OF BDCMSGCOLL.
*DATA GS_BDC_MSG  TYPE BDCMSGCOLL.
*DATA GS_BDC_OPT  TYPE CTU_PARAMS.
*DATA GV_BDC_CHK  TYPE C.


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

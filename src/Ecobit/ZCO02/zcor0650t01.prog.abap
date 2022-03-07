*&---------------------------------------------------------------------*
*& Include          ZCOR0650T01
*&---------------------------------------------------------------------*
TYPE-POOLS : ICON,ABAP.

TABLES: SSCRFIELDS. "선택화면의 필드(Function Key)
TABLES: T001.


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
            GC_X        TYPE CHAR01   VALUE 'X',
            GC_TCODE    TYPE SY-TCODE VALUE '',
            GC_TABNAME  TYPE TABNAME  VALUE 'CE11000'.


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

DATA: GV_REPID          TYPE SY-REPID.

*---------------------------------------------------------------------*
* STRUCTURE
*---------------------------------------------------------------------*
DATA: BEGIN OF GS_T001,
        BUKRS           LIKE T001-BUKRS,
        BUTXT           LIKE T001-BUTXT,
      END OF GS_T001.

*-- 레코드유형명
DATA: BEGIN OF GS_TVGAT,
        VRGAR           LIKE TVGAT-VRGAR,
        VRGARX          LIKE TVGAT-VRGARX,
      END OF GS_TVGAT.

*---------------------------------------------------------------------*
* INTERNAL TABLE
*---------------------------------------------------------------------*
DATA GT_T001            LIKE TABLE OF GS_T001.
DATA GT_TVGAT           LIKE TABLE OF GS_TVGAT.

*---------------------------------------------------------------------*
* ALV
*---------------------------------------------------------------------*
*DATA GR_SPLIT           TYPE REF TO CL_GUI_SPLITTER_CONTAINER.
*DATA GR_CON_TOP         TYPE REF TO CL_GUI_CONTAINER.
*DATA GR_CON_ALV         TYPE REF TO CL_GUI_CONTAINER.
*DATA GR_DDOC            TYPE REF TO CL_DD_DOCUMENT.
*DATA GR_ALV             TYPE REF TO ZCL_CO_ALV.
*DATA GR_GRID            TYPE REF TO CL_GUI_ALV_GRID.
*DATA GR_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER.

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

DEFINE __ADD_SELOPT_TEXT.
  CLEAR: &1, GV_TEMP1, GV_TEMP2.

  LOOP AT &2.
    IF &1 IS NOT INITIAL.
      &1 = &1 && ','.
    ENDIF.

    WRITE &2-LOW  TO GV_TEMP1.

    IF &2-OPTION EQ 'BT'.
      WRITE &2-HIGH TO GV_TEMP2.
      &1 = |{ &1 } { GV_TEMP1 } ~ { GV_TEMP2 }|.
    ELSE.
      CASE &2-OPTION.
        WHEN 'GT'.    GV_TEMP2 = '>'.
        WHEN 'GE'.    GV_TEMP2 = '>='.
        WHEN 'LT'.    GV_TEMP2 = '<'.
        WHEN 'LE'.    GV_TEMP2 = '<='.
        WHEN 'NE'.    GV_TEMP2 = 'Not'.
        WHEN OTHERS.  CLEAR GV_TEMP2.
      ENDCASE.

      CONCATENATE &1 GV_TEMP2 GV_TEMP1 INTO &1 SEPARATED BY SPACE.
    ENDIF.


    IF &2-SIGN EQ GC_E.
      CONCATENATE &1 '제외' INTO &1 SEPARATED BY SPACE.
    ENDIF.
  ENDLOOP.

  CONDENSE &1.
END-OF-DEFINITION.

DEFINE __WRITE_DATE.
  IF &1 IS INITIAL.
    CLEAR &2.
  ELSE.
    CONCATENATE &1(4) &1+4(2) &1+6(2) INTO &2 SEPARATED BY '-'.
  ENDIF.
END-OF-DEFINITION.

DEFINE __SET_DATE.
  IF &1 IS INITIAL.
    CLEAR &2.
  ELSE.
    CASE STRLEN( &1 ).
      WHEN  8.  " YYYYMMDD
        &2 = &1.
      WHEN 10.  " YYYY-MM-DD / YYYY.MM.DD
        &2 = &1+0(4) && &1+5(2) && &1+8(2).
      WHEN OTHERS.
        __ERROR '날짜 양식이 올바르지 않습니다.'.
    ENDCASE.
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

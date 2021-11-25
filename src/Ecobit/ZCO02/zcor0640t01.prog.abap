*&---------------------------------------------------------------------*
*& Include          ZCOR0640T01
*&---------------------------------------------------------------------*
TYPE-POOLS : ICON,ABAP.

TABLES: SSCRFIELDS. "선택화면의 필드(Function Key)
TABLES: T001, PROJ, PRPS.


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
DATA: GV_REFRESH        TYPE C.

DATA: GV_TEMP1          TYPE TEXT255,
      GV_TEMP2          TYPE TEXT255.

DATA: GV_WAERS          TYPE TKA01-WAERS.
DATA: GV_ERKRS          TYPE TKA01-ERKRS.

*---------------------------------------------------------------------*
* STRUCTURE
*---------------------------------------------------------------------*
DATA GS_FUNTXT          TYPE SMP_DYNTXT. "Excel 양식 Download(펑션키)


DATA GS_EXCEL           TYPE ZCOS0640.

DATA: BEGIN OF GS_DISPLAY,
        STATUS          LIKE ICON-ID,               " 상태
*--------------------------------------------------------------------*
        BUKRS           LIKE CE11000-BUKRS,         " 회사코드
        BUTXT           LIKE T001-BUTXT,            " 회사코드명
        WW120           LIKE CE11000-WW120,         " BU
        BEZEK           LIKE T25A1-BEZEK,           " BU명
        PSPNR           LIKE CE11000-PSPNR,         " WBS 요소
        POSID           LIKE PRPS-POSID,            " 작업 분석 구조 요소(WBS 요소)
        POST1           LIKE PRPS-POST1,            " WBS명
        VRGAR           LIKE CE11000-VRGAR,         " 레코드유형
        VRGARX          LIKE TVGAT-VRGARX,          " 레코드유형명
        VALFD           LIKE RKEA2-FIELDNAME,       " 값필드
        REPTX           LIKE DD04T-REPTEXT,         " 값필드명
        WKGXX           LIKE COSP-WKG001,           " 업로드값
*--------------------------------------------------------------------*
        BELNR           LIKE CE11000-BELNR,
        MESSAGE         LIKE BAPIRET2-MESSAGE,      " 결과메시지
        MESSAGE_TAB     LIKE TABLE OF BAPIRET2-MESSAGE,      " 결과메시지
        STYLE           TYPE LVC_T_STYL,
        COLOR           TYPE LVC_T_SCOL,
      END OF GS_DISPLAY.

*-- 회사코드
DATA: BEGIN OF GS_T001,
        BUKRS           LIKE T001-BUKRS,
        BUTXT           LIKE T001-BUTXT,
      END OF GS_T001.

*-- BU명
DATA: BEGIN OF GS_T25A1,
        WW120           LIKE T25A1-WW120,
        BEZEK           LIKE T25A1-BEZEK,
      END OF GS_T25A1.

*-- WBS
DATA: BEGIN OF GS_PRPS,
        PSPNR           LIKE PRPS-PSPNR,
        POSID           LIKE PRPS-POSID,
        POST1           LIKE PRPS-POST1,
        POSID_EDIT      LIKE PRPS-POSID_EDIT,
      END OF GS_PRPS.

*-- 레코드유형명
DATA: BEGIN OF GS_TVGAT,
        VRGAR           LIKE TVGAT-VRGAR,
        VRGARX          LIKE TVGAT-VRGARX,
      END OF GS_TVGAT.

*-- 값필드
DATA: BEGIN OF GS_DD03L,
        FIELDNAME       LIKE DD03L-FIELDNAME,
        REPTEXT         LIKE DD04T-REPTEXT,
      END OF GS_DD03L.

DATA: BEGIN OF GS_VALUE,
        FIENM           LIKE TKEF-FIENM,
        DATATYPE        LIKE DD04L-DATATYPE,
        REPTEXT         LIKE DD04T-REPTEXT,
      END OF GS_VALUE.

*---------------------------------------------------------------------*
* INTERNAL TABLE
*---------------------------------------------------------------------*

DATA GT_EXCEL           LIKE TABLE OF GS_EXCEL.
DATA GT_DISPLAY         LIKE TABLE OF GS_DISPLAY.
DATA GT_T001            LIKE TABLE OF GS_T001.
DATA GT_T25A1           LIKE TABLE OF GS_T25A1.
DATA GT_PRPS            LIKE TABLE OF GS_PRPS.
DATA GT_TVGAT           LIKE TABLE OF GS_TVGAT.
DATA GT_DD03L           LIKE TABLE OF GS_DD03L.
DATA GT_VALUE           LIKE TABLE OF GS_VALUE.

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

DATA GR_CON_DIALOG      TYPE REF TO CL_GUI_DIALOGBOX_CONTAINER.
DATA GR_ALV_DIALOG      TYPE REF TO ZCL_CO_ALV.

DATA GR_CON_DIALOG_2    TYPE REF TO CL_GUI_DIALOGBOX_CONTAINER.
DATA GR_ALV_DIALOG_2    TYPE REF TO ZCL_CO_ALV.

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

DEFINE __ERROR.
  GS_DISPLAY-STATUS  = ICON_RED_LIGHT.
  GS_DISPLAY-MESSAGE = &1.
  APPEND GS_DISPLAY-MESSAGE TO GS_DISPLAY-MESSAGE_TAB.
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

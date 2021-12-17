*&---------------------------------------------------------------------*
*& Include          ZCOR0630T01
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
            GC_TCODE    TYPE SY-TCODE VALUE ''.


CONSTANTS : GC_0        TYPE N VALUE '0',
            GC_1        TYPE N VALUE '1',
            GC_2        TYPE N VALUE '2',
            GC_3        TYPE N VALUE '3',
            GC_4        TYPE N VALUE '4'.

CONSTANTS : GC_NEW_LINE TYPE C VALUE CL_ABAP_CHAR_UTILITIES=>NEWLINE.


CONSTANTS : GC_PROFILE  TYPE PROJ-PROFL VALUE 'Z000003'.

CONSTANTS : GC_STRUCTURE TYPE   BAPIPAREX-STRUCTURE
                         VALUE  'BAPI_TE_WBS_ELEMENT'.
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

*---------------------------------------------------------------------*
* STRUCTURE
*---------------------------------------------------------------------*
DATA GS_FUNTXT          TYPE SMP_DYNTXT. "Excel 양식 Download(펑션키)


DATA GS_EXCEL           TYPE ZCOS0630.

DATA: BEGIN OF GS_DISPLAY,
        STATUS          LIKE ICON-ID,               " 상태
*--------------------------------------------------------------------*
        POSID           LIKE PRPS-POSID,            " WBS
        POST1           LIKE PRPS-POST1,            " WBS명
        PSPID           LIKE PROJ-PSPID_EDIT,       " 프로젝트
        ZZSCT           LIKE PRPS-ZZSCT,            " 매출유형
        ZZSCTTX         LIKE ZCOT1010T-ZZSCTTX,     " 매출유형 내역
        ZZPHA           LIKE PRPS-ZZPHA,            " 프로젝트 단계
        ZZPHATX         LIKE ZCOT1020T-ZZPHATX,     " 프로젝트 단계 내역
        ZZWBT           LIKE PRPS-ZZWBT,            " WBS 유형
        ZZWBTTX         LIKE ZCOT1030T-ZZWBTTX,     " WBS 유형 내역
        ZZBGU           LIKE PRPS-ZZBGU,            " 사업구분
        ZZBGUTX         LIKE ZCOT1040T-ZZBGUTX,     " 사업구분 내역
        ZZBGD           LIKE PRPS-ZZBGD,            " 사업구분상세
        ZZBGDTX         LIKE ZCOT1050T-ZZBGDTX,     " 사업구분상세 내역
        ZZIVC           LIKE PRPS-ZZIVC,            " 투자여부
        ZZIVCTX         LIKE ZCOT0020T-ZZIVCTX,     " 투자여부 내역
        ZZBAG           LIKE PRPS-ZZBAG,            " 사업소유무
        ZZBAGTX         LIKE ZCOT1070T-ZZBAGTX,     " 사업소유무 내역
        ZZADT           LIKE PRPS-ZZADT,            " 행정구역
        ZZADTTX         LIKE ZCOT1090T-ZZADTTX,     " 행정구역 내역
        ZZPRG           LIKE PRPS-ZZPRG,            " 발주처 유형
        ZZPRGTX         LIKE ZCOT1100T-ZZPRGTX,     " 발주처 유형 내역
        ZZHWB           LIKE PRPS-ZZHWB,            " ENG 하위본부
        ZZHWBTX         LIKE ZCOT1110T-ZZHWBTX,     " ENG 하위본부 내역
        ZZCOP           LIKE PRPS-ZZCOP,            " 수주유형
        ZZCOPTX         LIKE ZCOT1120T-COTXT,       " 수주유형 내역
        ZZCYP           LIKE PRPS-ZZCYP,            " 통제유형
        ZZCYPTX         LIKE ZCOT1130T-CTEXT,       " 통제유형명
*--------------------------------------------------------------------*
        OBJNR           LIKE PRPS-OBJNR,            " 오브젝트
        MESSAGE         LIKE BAPIRET2-MESSAGE,      " 결과메시지
        MESSAGE_TAB     LIKE TABLE OF BAPIRET2-MESSAGE,      " 결과메시지
        STYLE           TYPE LVC_T_STYL,
        COLOR           TYPE LVC_T_SCOL,
      END OF GS_DISPLAY.


*-- WBS
DATA: BEGIN OF GS_PRPS,
        POSID   LIKE PRPS-POSID_EDIT,
        OBJNR   LIKE PRPS-OBJNR,
        PSPID   LIKE PROJ-PSPID,
      END OF GS_PRPS.


*-- 매출유형
DATA: BEGIN OF GS_1010,
        ZZSCT   LIKE ZCOT1010T-ZZSCT,
        ZZSCTTX LIKE ZCOT1010T-ZZSCTTX,
      END OF GS_1010.


*-- 프로젝트 단계
DATA: BEGIN OF GS_1020,
        ZZPHA   LIKE ZCOT1020T-ZZPHA,
        ZZPHATX LIKE ZCOT1020T-ZZPHATX,
      END OF GS_1020.

*-- WBS유형
DATA: BEGIN OF GS_1030,
        ZZWBT   LIKE ZCOT1030T-ZZWBT,
        ZZWBTTX LIKE ZCOT1030T-ZZWBTTX,
      END OF GS_1030.

*-- 사업구분
DATA: BEGIN OF GS_1040,
        ZZBGU   LIKE ZCOT1040T-ZZBGU,
        ZZBGUTX LIKE ZCOT1040T-ZZBGUTX,
      END OF GS_1040.

*-- 사업구분상세
DATA: BEGIN OF GS_1050,
        ZZBGU   LIKE ZCOT1050T-ZZBGU,
        ZZBGD   LIKE ZCOT1050T-ZZBGD,
        ZZBGDTX LIKE ZCOT1050T-ZZBGDTX,
      END OF GS_1050.

*-- 투자여부
DATA: BEGIN OF GS_0020,
        ZZIVC   LIKE ZCOT0020T-ZZIVC,
        ZZIVCTX LIKE ZCOT0020T-ZZIVCTX,
      END OF GS_0020.

*-- 사업소유무
DATA: BEGIN OF GS_1070,
        ZZBAG   LIKE ZCOT1070T-ZZBAG,
        ZZBAGTX LIKE ZCOT1070T-ZZBAGTX,
      END OF GS_1070.

*-- 행정구역
DATA: BEGIN OF GS_1090,
        ZZADT   LIKE ZCOT1090T-ZZADT,
        ZZADTTX LIKE ZCOT1090T-ZZADTTX,
      END OF GS_1090.

*-- 발주처 유형
DATA: BEGIN OF GS_1100,
        ZZPRG   LIKE ZCOT1100T-ZZPRG,
        ZZPRGTX LIKE ZCOT1100T-ZZPRGTX,
      END OF GS_1100.

*-- ENG 하위본부
DATA: BEGIN OF GS_1110,
        ZZHWB   LIKE ZCOT1110T-ZZHWB,
        ZZHWBTX LIKE ZCOT1110T-ZZHWBTX,
      END OF GS_1110.

*-- 수주유형
DATA: BEGIN OF GS_1120,
        COTYP   LIKE ZCOT1120T-COTYP,
        COTXT   LIKE ZCOT1120T-COTXT,
      END OF GS_1120.

*-- 통제유형
DATA: BEGIN OF GS_1130,
        CTYPE   LIKE ZCOT1130T-CTYPE,
        CTEXT   LIKE ZCOT1130T-CTEXT,
      END OF GS_1130.

*---------------------------------------------------------------------*
* INTERNAL TABLE
*---------------------------------------------------------------------*

DATA GT_EXCEL           LIKE TABLE OF GS_EXCEL.
DATA GT_DISPLAY         LIKE TABLE OF GS_DISPLAY.
DATA GT_PRPS            LIKE TABLE OF GS_PRPS.
DATA GT_1010            LIKE TABLE OF GS_1010.    " 매출유형
DATA GT_1020            LIKE TABLE OF GS_1020.    " 프로젝트 단계
DATA GT_1030            LIKE TABLE OF GS_1030.    " WBS 유형
DATA GT_1040            LIKE TABLE OF GS_1040.    " 사업구분
DATA GT_1050            LIKE TABLE OF GS_1050.    " 사업구분상세
DATA GT_0020            LIKE TABLE OF GS_0020.    " 투자여부
DATA GT_1070            LIKE TABLE OF GS_1070.    " 사업소유무
DATA GT_1090            LIKE TABLE OF GS_1090.    " 행정구역
DATA GT_1100            LIKE TABLE OF GS_1100.    " 발주처 유형
DATA GT_1110            LIKE TABLE OF GS_1110.    " ENG 하위본부
DATA GT_1120            LIKE TABLE OF GS_1120.    " 수주유형
DATA GT_1130            LIKE TABLE OF GS_1130.    " 통제유형

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

DEFINE __SET_VALUE.
  ASSIGN COMPONENT &2 OF STRUCTURE &1 TO <FS_VALUE>.
  IF SY-SUBRC EQ 0.
    <FS_VALUE> = &3.
    UNASSIGN <FS_VALUE>.
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

*&---------------------------------------------------------------------*
*& Include          ZCOR0620T01
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


DATA GS_EXCEL           TYPE ZCOS0620.

DATA: BEGIN OF GS_DISPLAY,
        STATUS          LIKE ICON-ID,               " 상태
*--------------------------------------------------------------------*
        VBUKR           LIKE PROJ-VBUKR,            " 회사코드
        VGSBR           LIKE PROJ-VGSBR,            " 사업영역
        PRCTR           LIKE PROJ-PRCTR,            " 손익센터
        PSPID           LIKE PROJ-PSPID,            " 프로젝트
        PSPIDTX         LIKE PROJ-POST1,            " 프로젝트명
        POSID           LIKE PRPS-POSID,            " WBS
        POSIDTX         LIKE PRPS-POST1,            " WBS명
*        PSTRT           LIKE PRTE-PSTRT,            " 시작일
*        PENDE           LIKE PRTE-PENDE,            " 종료일
        ZZIZW           LIKE PRPS-ZZIZW,            " 투자사유
        ZZIZWTX         LIKE ZCOT1270T-ZZIZWTX,     " 투자사유명
        ZZCD1           LIKE PRPS-ZZCD1,            " 대분류
        ZZCD1TX         LIKE ZCOT1280T-ZZCD1TX,     " 대분류명
        ZZCD2           LIKE PRPS-ZZCD2,            " 중분류
        ZZCD2TX         LIKE ZCOT1290T-ZZCD2TX,     " 중분류명
        ZZCD3           LIKE PRPS-ZZCD3,            " 소분류
        ZZCD3TX         LIKE ZCOT1300T-ZZCD3TX,     " 소분류명
*        ZZTRD           LIKE PRPS-ZZTRD,            " 거래처명
        ZZTCV           LIKE ZCOS0021-ZZTCV,        " 계약금액
        ZZWAE           LIKE ZCOS0021-ZZWAE,        " 통화
*        ZZRT1           LIKE ZCOS0021-ZZRT1,        " 계약금%
*        ZZRT2           LIKE ZCOS0021-ZZRT2,        " 중도금%
*        ZZRT3           LIKE ZCOS0021-ZZRT3,        " 잔금%
*        ZZDT1           LIKE ZCOS0021-ZZDT1,        " 계약금예정일
*        ZZDT2           LIKE ZCOS0021-ZZDT2,        " 중도금예정일
*        ZZDT3           LIKE ZCOS0021-ZZDT3,        " 잔금예정일
        ZZUNT           LIKE ZCOS0021-ZZUNT,        " 호기
        ZZCMD           LIKE ZCOS0021-ZZCMD,        " 공사착공일
        ZZCPD           LIKE ZCOS0021-ZZCPD,        " 공사준공일
*        USR00           LIKE PRPS-USR00,            " 발주자
*        USR01           LIKE PRPS-USR01,            " 부서
*        USR02           LIKE PRPS-USR02,            " 점검유형
*        USR03           LIKE PRPS-USR03,            " 고장유형
*        USR08           LIKE PRPS-USR08,            " 계약발주일
*        USR09           LIKE PRPS-USR09,            " 점검일
*        USR10           LIKE PRPS-USR10,            " 사전계획
        ZZWBT           LIKE PRPS-ZZWBT,            " WBS유형
        ZZWBTTX         LIKE ZCOT1030T-ZZWBTTX,     " WBS유형명
        ZZCYP           LIKE PRPS-ZZCYP,            " 통제유형
        ZZCYPTX         LIKE ZCOT1130T-CTEXT,       " 통제유형명
*        TDLINE          LIKE BAPITLINE-TDLINE,      " 비고
*        TDLINE_TAB      TYPE LDPS_TXT_TAB,          " 비고(MultiLine)
*--------------------------------------------------------------------*
        OBJNR           LIKE PRPS-OBJNR,            " 오브젝트
        MESSAGE         LIKE BAPIRET2-MESSAGE,      " 결과메시지
        MESSAGE_TAB     LIKE TABLE OF BAPIRET2-MESSAGE,      " 결과메시지
        PSPID_NEW       TYPE C,
        STYLE           TYPE LVC_T_STYL,
        COLOR           TYPE LVC_T_SCOL,
      END OF GS_DISPLAY.

*-- 회사코드
DATA: BEGIN OF GS_T001,
        BUKRS   LIKE T001-BUKRS,
        LAND1   LIKE T001-LAND1,
        WAERS   LIKE T001-WAERS,
      END OF GS_T001.

DATA: BEGIN OF GS_TKA02,
        BUKRS   LIKE TKA02-BUKRS,
        GSBER   LIKE TKA02-GSBER,
      END OF GS_TKA02.

*-- 사업영역
DATA: BEGIN OF GS_TGSB,
        GSBER   LIKE TGSB-GSBER,
      END OF GS_TGSB.

*-- 손익센터
DATA: BEGIN OF GS_CEPC,
        PRCTR   LIKE CEPC-PRCTR,
      END OF GS_CEPC.

*-- 프로젝트
DATA: BEGIN OF GS_PROJ,
        PSPID  LIKE PROJ-PSPID,
        PROFL  LIKE PROJ-PROFL,
      END OF GS_PROJ.

*-- WBS
DATA: BEGIN OF GS_PRPS,
        POSID  LIKE PRPS-POSID,
      END OF GS_PRPS.

*-- WBS유형
DATA: BEGIN OF GS_1030,
        ZZWBT   LIKE ZCOT1030T-ZZWBT,
        ZZWBTTX LIKE ZCOT1030T-ZZWBTTX,
      END OF GS_1030.

*-- 통제유형
DATA: BEGIN OF GS_1130,
        CTYPE   LIKE ZCOT1130T-CTYPE,
        CTEXT   LIKE ZCOT1130T-CTEXT,
      END OF GS_1130.

*-- 투자사유
DATA: BEGIN OF GS_1270,
        ZZIZW   LIKE ZCOT1270T-ZZIZW,
        ZZIZWTX LIKE ZCOT1270T-ZZIZWTX,
      END OF GS_1270.

*-- 설비대분류코드
DATA: BEGIN OF GS_1280,
        ZZCD1   LIKE ZCOT1280T-ZZCD1,
        ZZCD1TX LIKE ZCOT1280T-ZZCD1TX,
      END OF GS_1280.

*-- 설비중분류코드
DATA: BEGIN OF GS_1290,
        ZZCD1   LIKE ZCOT1280T-ZZCD1,
        ZZCD2   LIKE ZCOT1290T-ZZCD2,
        ZZCD2TX LIKE ZCOT1290T-ZZCD2TX,
      END OF GS_1290.

*-- 설비소분류코드
DATA: BEGIN OF GS_1300,
        ZZCD1   LIKE ZCOT1280T-ZZCD1,
        ZZCD2   LIKE ZCOT1290T-ZZCD2,
        ZZCD3   LIKE ZCOT1300T-ZZCD3,
        ZZCD3TX LIKE ZCOT1300T-ZZCD3TX,
      END OF GS_1300.

*-- 통화코드
DATA: BEGIN OF GS_TCURC,
        WAERS   LIKE TCURC-WAERS,
      END OF GS_TCURC.


DATA: BEGIN OF GS_2001_NEW,
        PSPID LIKE PROJ-PSPID_EDIT,
        POST1 LIKE PROJ-POST1,
        BUKRS LIKE PROJ-VBUKR,
        GSBER LIKE PROJ-VGSBR,
        PRCTR LIKE PROJ-PRCTR,
      END OF GS_2001_NEW.

DATA: BEGIN OF GS_2054_NEW,
        PSPID LIKE PROJ-PSPID_EDIT,
        POSID LIKE PRPS-POSID_EDIT,
        TABIX LIKE SY-TABIX,
      END OF GS_2054_NEW.

DATA: BEGIN OF GS_WBS,
        POSID LIKE PRPS-POSID_EDIT,
      END OF GS_WBS.

*---------------------------------------------------------------------*
* INTERNAL TABLE
*---------------------------------------------------------------------*

DATA GT_EXCEL           LIKE TABLE OF GS_EXCEL.
DATA GT_DISPLAY         LIKE TABLE OF GS_DISPLAY.
DATA GT_T001            LIKE TABLE OF GS_T001.
DATA GT_TKA02           LIKE TABLE OF GS_TKA02.
DATA GT_TGSB            LIKE TABLE OF GS_TGSB.
DATA GT_CEPC            LIKE TABLE OF GS_CEPC.
DATA GT_PROJ            LIKE TABLE OF GS_PROJ.
DATA GT_PRPS            LIKE TABLE OF GS_PRPS.
DATA GT_1030            LIKE TABLE OF GS_1030.
DATA GT_1130            LIKE TABLE OF GS_1130.
DATA GT_1270            LIKE TABLE OF GS_1270.
DATA GT_1280            LIKE TABLE OF GS_1280.
DATA GT_1290            LIKE TABLE OF GS_1290.
DATA GT_1300            LIKE TABLE OF GS_1300.
DATA GT_TCURC           LIKE TABLE OF GS_TCURC.
DATA GT_2001_NEW        LIKE TABLE OF GS_2001_NEW.
DATA GT_2054_NEW        LIKE TABLE OF GS_2054_NEW.

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
  IF &2 IS INITIAL.
    CLEAR &3.
  ELSE.
    CASE STRLEN( &2 ).
      WHEN  8.  " YYYYMMDD
        &3 = &2.
      WHEN 10.  " YYYY-MM-DD or YYYY.MM.DD or YYYY/MM/DD
        &3 = &2+0(4) && &2+5(2) && &2+8(2).
      WHEN OTHERS.
        " 날짜 양식이 올바르지 않습니다.
        GS_DISPLAY-MESSAGE = |[| && &1 && |] { TEXT-E33 }|.
        __ERROR GS_DISPLAY-MESSAGE.
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

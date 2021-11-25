*&---------------------------------------------------------------------*
*& Include          ZCOR0590T01
*&---------------------------------------------------------------------*
TYPE-POOLS : ICON,ABAP.

TABLES: SSCRFIELDS. "선택화면의 필드(Function Key)
TABLES: T811C.



CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

*---------------------------------------------------------------------*
* CONSTANTS
*---------------------------------------------------------------------*
CONSTANTS : GC_A     TYPE CHAR01   VALUE 'A',
            GC_C     TYPE CHAR01   VALUE 'C',
            GC_D     TYPE CHAR01   VALUE 'D',
            GC_E     TYPE CHAR01   VALUE 'E',
            GC_I     TYPE CHAR01   VALUE 'I',
            GC_N     TYPE CHAR01   VALUE 'N',
            GC_S     TYPE CHAR01   VALUE 'S',
            GC_P     TYPE CHAR01   VALUE 'P',
            GC_W     TYPE CHAR01   VALUE 'W',
            GC_X     TYPE CHAR01   VALUE 'X',
            GC_TCODE TYPE SY-TCODE VALUE '',
            GC_TAB_CE71000 TYPE T811C-TAB VALUE 'CE71000'.

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

DATA: GV_EXIT     TYPE C,
      GV_ANSWER   TYPE C,
      OK_CODE     TYPE SY-UCOMM,   "예외
      SAVE_OK     TYPE SY-UCOMM.   "예외

DATA: GV_REPID    TYPE SY-REPID.
DATA: GV_REFRESH  TYPE C.
DATA: GV_CYCLE    TYPE CHAR6.

DATA GV_TEMP1       TYPE TEXT255.
DATA GV_TEMP2       TYPE TEXT255.


*---------------------------------------------------------------------*
* STRUCTURE
*---------------------------------------------------------------------*
DATA GS_FUNTXT        TYPE SMP_DYNTXT. "Excel 양식 Download(펑션키)

DATA: BEGIN OF GS_T811C,
        CYCLE         LIKE T811C-CYCLE,         " 사이클
        SDATE         LIKE T811C-SDATE,         " 시작일
        EDATE         LIKE T811C-EDATE,         " 종료일
        CRDATE        LIKE T811C-CRDATE,        " 생성일
        MODDATE       LIKE T811C-MODDATE,       " 생성일최종변경일
        MODUSER       LIKE T811C-MODUSER,       " 최종변경자
        LASTEXEC      LIKE T811C-LASTEXEC,      " 최종수행일
        PROC_GROUP    LIKE T811C-PROC_GROUP,    " 실행그룹
      END OF GS_T811C.

DATA: BEGIN OF GS_T811S,
        CYCLE         LIKE T811S-CYCLE,         " 사이클
        SDATE         LIKE T811S-SDATE,         " 시작일
        SEQNR         LIKE T811S-SEQNR,         " Seg.
        NAME          LIKE T811S-NAME,          " 세그먼트
        SPERCENT      LIKE T811S-SPERCENT,      " 센더%
        RCDATA        LIKE T811S-RCDATA,        " 배부기준1
        ABSCH         LIKE T811S-ABSCH,         " 배부구조
        ERSCH         LIKE T811S-ERSCH,         " PA전송
      END OF GS_T811S.

DATA: BEGIN OF GS_T811K,
        CYCLE         LIKE T811K-CYCLE,         " 사이클
        SDATE         LIKE T811K-SDATE,         " 시작일
        SEQNR         LIKE T811K-SEQNR,         " Seg.
        SETKIND       LIKE T811K-SETKIND,       " 세트유형
        POS           LIKE T811K-POS,           " 배부전표의 일련번호
        FIELD         LIKE T811K-FIELD,         " DB 필드 이름
        VALMIN        LIKE T811K-VALMIN,        " 시작값
        VALMAX        LIKE T811K-VALMAX,        " 종료 값
        SETID         LIKE T811K-SETID,         " 세트 ID
      END OF GS_T811K.

DATA: BEGIN OF GS_T811L,
        CYCLE         LIKE T811L-CYCLE,         " 사이클
        SDATE         LIKE T811L-SDATE,         " 시작일
        SEQNR         LIKE T811L-SEQNR,         " Seg.
        TXT           LIKE T811L-TXT,           " 텍스트(30자)
      END OF GS_T811L.

DATA: BEGIN OF GS_DISPLAY,
*--------------------------------------------------------------------*
        CATEG         LIKE T811C-CYCLE,         " 구분(사이클(2))
        CYCLE         LIKE T811C-CYCLE,         " 사이클
        CYCLE_OUT     LIKE T811C-CYCLE,         " 사이클
        CYCLE_TXT     LIKE T811L-TXT,           " 사이클명
        SEQNR         LIKE T811S-SEQNR,         " Seg.
        SEGMENT       LIKE T811S-NAME,          " 세그먼트
        SEGMENT_TXT   LIKE T811L-TXT,           " 세그먼트명

        S_CC_F        LIKE T811K-VALMIN,        " S.CC_F
        S_CC_T        LIKE T811K-VALMAX,        " S.CC_T
        S_CC_G        LIKE T811K-SETID,         " S.CC_G
        S_CC_GRP      LIKE SETHEADERT-SETNAME,  " S.CC_G
        S_CC_TXT      LIKE SETHEADERT-DESCRIPT, " S.CC명

        S_CE_F        LIKE T811K-VALMIN,        " S.CE_F
        S_CE_T        LIKE T811K-VALMAX,        " S.CE_T
        S_CE_G        LIKE T811K-SETID,         " S.CE_G
        S_CE_GRP      LIKE SETHEADERT-SETNAME,  " S.CE_G
        S_CE_TXT      LIKE SETHEADERT-DESCRIPT, " S.CE명

        R_BU_F        LIKE T811K-VALMIN,        " R.BU_F
        R_BU_T        LIKE T811K-VALMAX,        " R.BU_T
        R_BU_G        LIKE T811K-SETID,         " R.BU_G
        R_BU_GRP      LIKE SETHEADERT-SETNAME,  " R.BU_G
        R_BU_TXT      LIKE SETHEADERT-DESCRIPT, " R.BU명

        R_CD_F        LIKE T811K-VALMIN,        " R.CD_F
        R_CD_T        LIKE T811K-VALMAX,        " R.CD_T
        R_CD_G        LIKE T811K-SETID,         " R.CD_G
        R_CD_GRP      LIKE SETHEADERT-SETNAME,  " R.CD_G
        R_CD_TXT      LIKE SETHEADERT-DESCRIPT, " R.CD명

        R_BA_F        LIKE T811K-VALMIN,        " R.BA_F
        R_BA_T        LIKE T811K-VALMAX,        " R.BA_T
        R_BA_G        LIKE T811K-SETID,         " R.BA_G
        R_BA_GRP      LIKE SETHEADERT-SETNAME,  " R.BA_G
        R_BA_TXT      LIKE SETHEADERT-DESCRIPT, " R.BA명

        R_RG_F        LIKE T811K-VALMIN,        " R.BG_F
        R_RG_T        LIKE T811K-VALMAX,        " R.BG_T
        R_RG_G        LIKE T811K-SETID,         " R.BG_G
        R_RG_GRP      LIKE SETHEADERT-SETNAME,  " R.BG_G
        R_RG_TXT      LIKE SETHEADERT-DESCRIPT, " R.BG명

        R_WB_F        LIKE T811K-VALMIN,        " R.WBS_F
        R_WB_T        LIKE T811K-VALMAX,        " R.WBS_T
        R_WB_G        LIKE T811K-SETID,         " R.WBS_G
        R_WB_GRP      LIKE SETHEADERT-SETNAME,  " R.WBS_G
        R_WB_TXT      LIKE SETHEADERT-DESCRIPT, " R.WBS명

        RCDATA        LIKE T811S-RCDATA,        " 배부기준1
        RCDATA_TXT    LIKE DD04T-REPTEXT,       " 배부기준명
        VERSN         LIKE T811K-VALMIN,        " 버전
        SPERCENT      LIKE T811S-SPERCENT,      " 센더%
        ABSCH         LIKE T811S-ABSCH,         " 배부구조
        ERSCH         LIKE T811S-ERSCH,         " PA전송
        ERSCH_TXT     LIKE TKB9B-STEXT,         " PA전송구조명
        SDATE         LIKE T811C-SDATE,         " 시작일
        EDATE         LIKE T811C-EDATE,         " 종료일
        CRDATE        LIKE T811C-CRDATE,        " 생성일
        MODDATE       LIKE T811C-MODDATE,       " 생성일최종변경일
        MODUSER       LIKE T811C-MODUSER,       " 최종변경자
        LASTEXEC      LIKE T811C-LASTEXEC,      " 최종수행일
        PROC_GROUP    LIKE T811C-PROC_GROUP,    " 실행그룹
*--------------------------------------------------------------------*
        STYLE         TYPE LVC_T_STYL,
        COLOR         TYPE LVC_T_SCOL,
      END OF GS_DISPLAY.

DATA: BEGIN OF GS_SETHDRT,
        SETCLASS      LIKE SETHEADERT-SETCLASS,
        SUBCLASS      LIKE SETHEADERT-SUBCLASS,
        SETNAME       LIKE SETHEADERT-SETNAME,
        DESCRIPT      LIKE SETHEADERT-DESCRIPT,
      END OF GS_SETHDRT.

DATA: BEGIN OF GS_CSKT,
        KOSTL         LIKE CSKT-KOSTL,
        DATBI         LIKE CSKT-DATBI,
        KTEXT         LIKE CSKT-KTEXT,
      END OF GS_CSKT.

DATA: BEGIN OF GS_CSKU,
        KSTAR         LIKE CSKU-KSTAR,
        KTEXT         LIKE CSKU-KTEXT,
      END OF GS_CSKU.

DATA: BEGIN OF GS_T001,
        BUKRS         LIKE T001-BUKRS,
        BUTXT         LIKE T001-BUTXT,
      END OF GS_T001.

DATA: BEGIN OF GS_T25A1,
        WW120         LIKE T25A1-WW120,
        BEZEK         LIKE T25A1-BEZEK,
      END OF GS_T25A1.

DATA: BEGIN OF GS_TGSBT,
        GSBER         LIKE TGSBT-GSBER,
        GTEXT         LIKE TGSBT-GTEXT,
      END OF GS_TGSBT.

DATA: BEGIN OF GS_1090T,
        ZZADT         LIKE ZCOT1090T-ZZADT,
        ZZADTTX       LIKE ZCOT1090T-ZZADTTX,
      END OF GS_1090T.

DATA: BEGIN OF GS_DD04T,
        ROLLNAME    LIKE DD04T-ROLLNAME,
        REPTEXT     LIKE DD04T-REPTEXT,
      END OF GS_DD04T.

DATA: BEGIN OF GS_TKCTK,
        KEYRA       LIKE TKCTK-KEYRA,
        TXT_M       LIKE TKCTK-TXT_M,
      END OF GS_TKCTK.

DATA: BEGIN OF GS_TKB9B,
        ERSCH LIKE TKB9B-ERSCH,
        STEXT LIKE TKB9B-STEXT,
      END OF GS_TKB9B.

DATA: BEGIN OF GS_PRPS,
        POSID     LIKE PRPS-POSID,
        POST1     LIKE PRPS-POST1,
      END OF GS_PRPS.

DATA: BEGIN OF GS_PROJ,
        PSPID     LIKE PROJ-PSPID,
        POST1     LIKE PROJ-POST1,
      END OF GS_PROJ.


*---------------------------------------------------------------------*
* INTERNAL TABLE
*---------------------------------------------------------------------*

DATA GT_T811C         LIKE TABLE OF GS_T811C.
DATA GT_T811S         LIKE TABLE OF GS_T811S.
DATA GT_T811K         LIKE TABLE OF GS_T811K.
DATA GT_T811L         LIKE TABLE OF GS_T811L.
DATA GT_DISPLAY       LIKE TABLE OF GS_DISPLAY.
DATA GT_SETHDRT       LIKE TABLE OF GS_SETHDRT.
DATA GT_CSKT          LIKE TABLE OF GS_CSKT.
DATA GT_CSKU          LIKE TABLE OF GS_CSKU.
DATA GT_T001          LIKE TABLE OF GS_T001.
DATA GT_T25A1         LIKE TABLE OF GS_T25A1.
DATA GT_TGSBT         LIKE TABLE OF GS_TGSBT.
DATA GT_1090T         LIKE TABLE OF GS_1090T.
DATA GT_DD04T         LIKE TABLE OF GS_DD04T.
DATA GT_TKCTK         LIKE TABLE OF GS_TKCTK.
DATA GT_TKB9B         LIKE TABLE OF GS_TKB9B.
DATA GT_PRPS          LIKE TABLE OF GS_PRPS.
DATA GT_PROJ          LIKE TABLE OF GS_PROJ.

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

RANGES: R_CYCLE FOR T811C-CYCLE.

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
    IF SY-TABIX > 5.
      &1 = &1 && '...'.
      EXIT.
    ENDIF.

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

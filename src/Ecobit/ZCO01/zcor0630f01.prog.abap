*&---------------------------------------------------------------------*
*& Include          ZCOR0630F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION .

  GV_REPID = SY-REPID.

  " 기본값 : 관리회계영역
  P_KOKRS  = ZCL_CO_COMMON=>GET_DEFAULT_KOKRS( ).
  IF P_KOKRS IS INITIAL.
    P_KOKRS = '1000'.
  ENDIF.

  ZCL_CO_COMMON=>SET_KOKRS( P_KOKRS ).

  P_FILE = 'C:\'.


  " Selection Screen 텍스트
  TEXT_S01 = '실행조건'(S01).
  TEXT_S02 = '선택조건'(S02).
  SY-TITLE = '[CO] 사업 WBS 일괄 변경'(T01).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM SCRFIELDS_FUNCTXT .

  SSCRFIELDS = VALUE #(
    FUNCTXT_01 = VALUE SMP_DYNTXT( ICON_ID   = ICON_INFORMATION
                                   QUICKINFO = TEXT-S03 )
    FUNCTXT_02 = VALUE SMP_DYNTXT( ICON_ID   = ICON_XLS
                                   ICON_TEXT = TEXT-S04 )
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCR_USER_COMMAND
*&---------------------------------------------------------------------*
FORM SCR_USER_COMMAND .

  CASE SSCRFIELDS-UCOMM.
    WHEN 'FC01'.
      PERFORM CALL_POPUP_HELP(ZCAR9000) USING SY-REPID
                                              SY-DYNNR
                                              SY-LANGU ''.

    WHEN 'FC02'.
      PERFORM FILE_DOWNLOAD.

    WHEN OTHERS.


  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILE_DOWNLOAD
*&---------------------------------------------------------------------*
FORM FILE_DOWNLOAD .
*
*  DATA LT_DOWN  TYPE ZCL_CO_COMMON=>TT_EXCEL_TABLINE.
*  DATA LS_DOWN  TYPE ZCL_CO_COMMON=>TS_EXCEL_TABLINE.
*
*  DATA LS_EXCEL TYPE ZCOS0620.
*  DATA LS_0021  TYPE ZCOS0021.
*
*  SELECT B~VBUKR,                 " 회사코드
*         B~VGSBR,                 " 사업영역
*         B~PRCTR,                 " 손익센터
*         B~PSPID_EDIT AS PSPID,   " 프로젝트
*         B~POST1      AS PSPIDTX, " 프로젝트명
*         A~POSID_EDIT AS POSID,   " WBS
*         A~POST1      AS POSIDTX, " WBS명
*         C~PSTRT,                 " 시작일
*         C~PENDE,                 " 종료일
*         A~ZZIZW,                 " 투자사유
*         A~ZZCD1,                 " 대분류
*         A~ZZCD2,                 " 중분류
*         A~ZZCD3,                 " 소분류
*         A~ZZTRD,                 " 거래처명
*         A~ZZTCV,                 " 계약금액
*         A~ZZWAE,                 " 통화
*         A~ZZRT1,                 " 계약금%
*         A~ZZRT2,                 " 중도금%
*         A~ZZRT3,                 " 잔금%
*         A~ZZDT1,                 " 계약금예정일
*         A~ZZDT2,                 " 중도금예정일
*         A~ZZDT3,                 " 잔금예정일
*         A~USR00,                 " 발주자
*         A~USR01,                 " 부서
*         A~USR02,                 " 점검유형
*         A~USR03,                 " 고장유형
*         A~USR08,                 " 계약발주일
*         A~USR09,                 " 점검일
*         A~USR10,                 " 사전계획
*         A~ZZWBT,                 " WBS유형
*         A~ZZCYP                  " 통제유형
*    FROM PRPS AS A
*    JOIN PROJ AS B ON B~PSPNR EQ A~PSPHI
*    JOIN PRTE AS C ON C~POSNR EQ A~PSPNR
*   WHERE B~PROFL EQ @GC_PROFILE " 설비 WBS
*    INTO TABLE @DATA(LT_DATA).
*
*  SORT LT_DATA BY PSPID POSID.
*
*
*  LOOP AT LT_DATA INTO DATA(LS_DATA).
*
*    LS_DOWN-ROW = SY-TABIX.
*
*    LS_0021  = CORRESPONDING #( LS_DATA ).
*    LS_EXCEL = CORRESPONDING #( LS_DATA ).
*    LS_EXCEL = CORRESPONDING #( BASE ( LS_EXCEL ) LS_0021 ).
*
*    IF LS_0021-ZZTCV EQ 0.
*      CLEAR LS_EXCEL-ZZTCV.
*    ELSE.
*      WRITE LS_0021-ZZTCV TO LS_EXCEL-ZZTCV CURRENCY LS_DATA-ZZWAE.
*      REPLACE ALL OCCURRENCES OF ',' IN LS_EXCEL-ZZTCV WITH SPACE.
*      CONDENSE LS_EXCEL-ZZTCV NO-GAPS.
*    ENDIF.
*
*    IF LS_0021-ZZRT1 EQ 0.
*      CLEAR LS_EXCEL-ZZRT1.
*    ENDIF.
*
*    IF LS_0021-ZZRT2 EQ 0.
*      CLEAR LS_EXCEL-ZZRT2.
*    ENDIF.
*
*    IF LS_0021-ZZRT3 EQ 0.
*      CLEAR LS_EXCEL-ZZRT3.
*    ENDIF.
*
*
*    __WRITE_DATE: LS_DATA-PSTRT LS_EXCEL-PSTRT, " 시작일
*                  LS_DATA-PENDE LS_EXCEL-PENDE, " 종료일
*                  LS_DATA-ZZDT1 LS_EXCEL-ZZDT1, " 계약금예정일
*                  LS_DATA-ZZDT2 LS_EXCEL-ZZDT2, " 중도금예정일
*                  LS_DATA-ZZDT3 LS_EXCEL-ZZDT3, " 잔금예정일
*                  LS_DATA-USR08 LS_EXCEL-USR08, " 계약발주일
*                  LS_DATA-USR09 LS_EXCEL-USR09. " 점검일
*
*    DO.
*      ASSIGN COMPONENT SY-INDEX
*          OF STRUCTURE LS_EXCEL TO FIELD-SYMBOL(<FS>).
*      IF SY-SUBRC NE 0.
*        EXIT.
*      ENDIF.
*
*      LS_DOWN-COL   = SY-INDEX.
*      LS_DOWN-VALUE = <FS>.
*      APPEND LS_DOWN TO LT_DOWN.
*    ENDDO.
*
*  ENDLOOP.
*


  ZCL_CO_COMMON=>FILE_DOWNLOAD(
    EXCEPTIONS
      NOT_EXIST_OBJECT_ID = 1
      NO_DATA_LENGTH      = 2
      FILE_DOWNLOAD_ERROR = 3
      OTHERS              = 4
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_FILE
*&---------------------------------------------------------------------*
FORM F4_FILE  CHANGING PV_FILE.

  PV_FILE = ZCL_CO_COMMON=>GET_DYNP_VALUE( I_FIELD = 'P_FILE' ).
  PV_FILE = ZCL_CO_COMMON=>F4_FILE( I_PATH      = PV_FILE
                                    I_FILE_TYPE = GC_E ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  PERFORM CLEAR_ITAB.
  PERFORM SELECT_OTHERS.
  PERFORM IMPORT_EXCEL_FILE.
  PERFORM MAKE_DISPLAY_DATA.
  PERFORM VALID_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_TOP_OF_PAGE_0100
*&---------------------------------------------------------------------*
FORM CREATE_TOP_OF_PAGE_0100 .

  IF GR_DDOC IS INITIAL.
    CREATE OBJECT GR_DDOC
      EXPORTING
        STYLE = 'ALV_GRID'.
  ENDIF.

  GR_DDOC->INITIALIZE_DOCUMENT( ).

  PERFORM WRITE_DOCUMENT.

  GR_DDOC->MERGE_DOCUMENT( ).
  GR_DDOC->DISPLAY_DOCUMENT(
    EXPORTING
      REUSE_CONTROL      = GC_X
      PARENT             = GR_CON_TOP  " Contain Object Already Exists
    EXCEPTIONS
      " Error Displaying the Document in the HTML Control
      HTML_DISPLAY_ERROR = 1
      OTHERS             = 2
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form WRITE_DOCUMENT
*&---------------------------------------------------------------------*
FORM WRITE_DOCUMENT .

*--------------------------------------------------------------------*
* Top of Page Layout 설정
*--------------------------------------------------------------------*
  DATA LR_TABLE     TYPE REF TO CL_DD_TABLE_ELEMENT.
  DATA LR_COL_L     TYPE REF TO CL_DD_AREA. " Left
  DATA LR_COL_R     TYPE REF TO CL_DD_AREA. " Right

  DATA LR_TABLE_B01 TYPE REF TO CL_DD_TABLE_ELEMENT.
  DATA LR_COL_B01_I TYPE REF TO CL_DD_AREA. " Icon
  DATA LR_COL_B01_L TYPE REF TO CL_DD_AREA. " Label
  DATA LR_COL_B01_C TYPE REF TO CL_DD_AREA. " Conditions

  PERFORM ADD_TABLE  USING GR_DDOC  LR_TABLE '100%'  2.
  PERFORM ADD_COLUMN USING LR_TABLE LR_COL_L '400px'.
  PERFORM ADD_COLUMN USING LR_TABLE LR_COL_R '*'.

  PERFORM ADD_TABLE  USING LR_COL_L     LR_TABLE_B01 '100%'  3.
  PERFORM ADD_COLUMN USING LR_TABLE_B01 LR_COL_B01_I '30px'.
  PERFORM ADD_COLUMN USING LR_TABLE_B01 LR_COL_B01_L '100px'.
  PERFORM ADD_COLUMN USING LR_TABLE_B01 LR_COL_B01_C '*'.
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Selection Screen Block [B01] : Parameter & Select Options
*--------------------------------------------------------------------*
  PERFORM WRITE_DOCUMENT_B01 USING LR_TABLE_B01
                                   LR_COL_B01_I
                                   LR_COL_B01_L
                                   LR_COL_B01_C.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_TABLE
*&---------------------------------------------------------------------*
FORM ADD_TABLE  USING PR_DDOC
                      PR_TABLE          TYPE REF TO CL_DD_TABLE_ELEMENT
                      VALUE(PV_WIDTH)   TYPE SDYDO_VALUE
                      VALUE(PV_NUMBER)  TYPE I.

  CHECK PR_DDOC IS BOUND AND PR_DDOC IS INSTANCE OF CL_DD_AREA.

  DATA LR_DDOC TYPE REF TO CL_DD_AREA.

  LR_DDOC = PR_DDOC.

  LR_DDOC->ADD_TABLE(
    EXPORTING
      " Number of Table Columns
      NO_OF_COLUMNS               = PV_NUMBER
      " Width of Table Frame; '0' = No Frame
      BORDER                      = '0'
      " Width of Table; '100%' = Entire Width of Control
      WIDTH                       = PV_WIDTH
    IMPORTING
      TABLE                       = PR_TABLE  " Table Element
*      TABLEAREA                   =          " Table Area
    EXCEPTIONS
      " Reference Variable for TABLE Already Used
      TABLE_ALREADY_USED          = 1
      OTHERS                      = 2
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_COLUMN
*&---------------------------------------------------------------------*
FORM ADD_COLUMN  USING PR_TABLE         TYPE REF TO CL_DD_TABLE_ELEMENT
                       PR_COLUMN        TYPE REF TO CL_DD_AREA
                       VALUE(PV_WIDTH)  TYPE SDYDO_VALUE.

  CHECK PR_TABLE IS BOUND.

  PR_TABLE->ADD_COLUMN(
    EXPORTING
      " Width of Column (Example '20%')
      WIDTH               = PV_WIDTH
    IMPORTING
      COLUMN              = PR_COLUMN  " Column Area
    EXCEPTIONS
      " Reference Variable for COLUMN has Already Been Used
      COLUMN_ALREADY_USED = 1
      OTHERS              = 2
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form WRITE_DOCUMENT_B01
*&---------------------------------------------------------------------*
FORM WRITE_DOCUMENT_B01 USING PR_TABLE TYPE REF TO CL_DD_TABLE_ELEMENT
                              PR_COL_I TYPE REF TO CL_DD_AREA
                              PR_COL_L TYPE REF TO CL_DD_AREA
                              PR_COL_C TYPE REF TO CL_DD_AREA.

  DATA LV_LABEL   TYPE TEXT255.
  DATA LV_CONDI   TYPE TEXT255.

*--------------------------------------------------------------------*
  LV_LABEL = '관리회계영역'(L01).
  LV_CONDI = P_KOKRS.
  PR_COL_I->ADD_ICON( 'ICON_PARAMETER' ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*
  LV_LABEL = '파일경로'(L02).
  LV_CONDI = P_FILE.
  PR_TABLE->NEW_ROW( ).
  PR_COL_I->ADD_ICON( 'ICON_PARAMETER' ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_MAIN_GRID_0100
*&---------------------------------------------------------------------*
FORM CREATE_MAIN_GRID_0100 .

  IF GR_ALV IS NOT BOUND.
    GR_ALV = NEW #( GR_CON_ALV ).
  ENDIF.

  PERFORM MAKE_FIELDCATALOG_0100.
  PERFORM REGISTER_EVENT_0100.

  GR_ALV->SET_LAYOUT(
    I_TYPE       = 'A'
    I_STYLEFNAME = 'STYLE'
    I_CTAB_FNAME = 'COLOR'
  ).

*  GR_ALV->SET_SORT( IT_FIELD = VALUE #( ( 'PBUKR' )
*                                        ( 'KTEXT' )
*                                        ) ).

  GR_ALV->MS_VARIANT-REPORT = SY-REPID.
  GR_ALV->MV_SAVE = 'A'.
  GR_ALV->DISPLAY( CHANGING T_OUTTAB = GT_DISPLAY ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_FIELDCATALOG_0100
*&---------------------------------------------------------------------*
FORM MAKE_FIELDCATALOG_0100 .

  GR_ALV->SET_FIELD_CATALOG(
    EXPORTING
      I_TABNAME               = 'GS_DISPLAY'
    EXCEPTIONS
      INVALID_INPUT_PARAMETER = 1
      EMPTY_FIELD_CATALOG     = 2
      OTHERS                  = 3
  ).

  IF SY-SUBRC <> 0.
    FREE GR_ALV.
    MESSAGE '필드카탈로그가 비어있습니다.' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 0.
  ENDIF.


  DATA LV_TEXT        TYPE TEXT100.
  DATA LV_TOOLTIP     TYPE TEXT100.
  DATA LV_KEY_FIX     TYPE C.

*  LV_KEY_FIX = GC_X.

  LOOP AT GR_ALV->MT_FIELDCAT INTO DATA(LS_FIELDCAT).

    CLEAR LV_TEXT.
    CLEAR LV_TOOLTIP.
    CLEAR LS_FIELDCAT-KEY.

*-- 열최적화
*    LS_FIELDCAT-COL_OPT = GC_X.

*-- 열고정
*    LS_FIELDCAT-FIX_COLUMN = LV_KEY_FIX.
*    LS_FIELDCAT-KEY =


*-- Field 속성
    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'STATUS'.
        LV_TEXT               = '상태'.
        LS_FIELDCAT-JUST      = GC_C.
        LS_FIELDCAT-ICON      = GC_X.

      WHEN 'POSID'.
        LV_TEXT               = 'WBS'.
        LS_FIELDCAT-KEY       = GC_X.
        LS_FIELDCAT-HOTSPOT   = GC_X.

      WHEN 'POST1'.
        LV_TEXT               = 'WBS명'.

      WHEN 'ZZWBT'.
        LV_TEXT               = 'WBS유형'.
        LS_FIELDCAT-JUST      = GC_C.
      WHEN 'ZZCYP'.
        LV_TEXT               = '통제유형'.
        LS_FIELDCAT-JUST      = GC_C.
      WHEN 'STYLE'.
        LS_FIELDCAT-TECH      = GC_X.
      WHEN 'COLOR'.
        LS_FIELDCAT-TECH      = GC_X.
      WHEN 'MESSAGE'.
        LV_TEXT               = '오류 점검 및 결과 메시지'.
        LS_FIELDCAT-HOTSPOT   = GC_X.

      WHEN OTHERS.
    ENDCASE.


    IF LV_TEXT IS NOT INITIAL.
      LS_FIELDCAT-REPTEXT   = LV_TEXT.
      LS_FIELDCAT-COLTEXT   = LV_TEXT.
      LS_FIELDCAT-SCRTEXT_L = LV_TEXT.
      LS_FIELDCAT-SCRTEXT_M = LV_TEXT.
      LS_FIELDCAT-SCRTEXT_S = LV_TEXT.

      IF LV_TOOLTIP IS NOT INITIAL.
        LS_FIELDCAT-TOOLTIP = LV_TOOLTIP.
      ENDIF.
    ENDIF.

    MODIFY GR_ALV->MT_FIELDCAT FROM LS_FIELDCAT.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form REGISTER_EVENT_0100
*&---------------------------------------------------------------------*
FORM REGISTER_EVENT_0100 .

*  GR_ALV->MR_ALV_GRID->REGISTER_EDIT_EVENT(
*    EXPORTING
*      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED " Event ID
*    EXCEPTIONS
*      ERROR      = 1                " Error
*      OTHERS     = 2
*  ).

  IF GR_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_EVENT_RECEIVER.
  ENDIF.

  SET HANDLER:
    GR_EVENT_RECEIVER->ON_HOTSPOT_CLICK FOR GR_ALV->MR_ALV_GRID.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form IMPORT_EXCEL_FILE
*&---------------------------------------------------------------------*
FORM IMPORT_EXCEL_FILE .

  DATA LT_INTERN TYPE ZCL_CO_COMMON=>TT_EXCEL_TABLINE.

  FIELD-SYMBOLS <FS>.


*-- 입력받은 경로로부터 엑셀 데이터 조회
  ZCL_CO_COMMON=>GET_EXCEL_CONTENTS(
    EXPORTING
      I_FILENAME              = P_FILE     " Excel 파일명(경로포함)
      I_BEGIN_ROW             = 2          " 조회시작행
      I_BEGIN_COL             = 1          " 조회시작열
*      I_END_ROW               =           " 조회종료행
      I_END_COL               = 32         " 조회종료열
    IMPORTING
      ET_INTERN               = LT_INTERN  " Excel 데이타
    EXCEPTIONS
      NO_INPUT_EXCEL_FILE     = 1          " Excel 파일명 공백
      INCONSISTENT_PARAMETERS = 2
      OTHERS                  = 3
  ).


*-- 테이블형태로 전환
  LOOP AT LT_INTERN INTO DATA(LS_INTERN).
    AT NEW ROW.
      CLEAR GS_EXCEL.
    ENDAT.

    ASSIGN COMPONENT LS_INTERN-COL OF STRUCTURE GS_EXCEL TO <FS>.

    IF SY-SUBRC EQ 0.
      <FS> = LS_INTERN-VALUE.
      UNASSIGN <FS>.
    ENDIF.

    AT END OF ROW.
      CHECK  GS_EXCEL IS NOT INITIAL.
      APPEND GS_EXCEL TO GT_EXCEL.
    ENDAT.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM MAKE_DISPLAY_DATA .

  RANGES LR_PRPS FOR GS_PRPS-POSID.


  CHECK GT_EXCEL[] IS NOT INITIAL.


  LR_PRPS[] = CORRESPONDING #( GT_EXCEL[] MAPPING LOW = POSID ).
  LR_PRPS = VALUE #( SIGN = 'I' OPTION = 'EQ' ).
  MODIFY LR_PRPS TRANSPORTING SIGN OPTION WHERE SIGN IS INITIAL.


  SELECT B~POSID_EDIT, B~OBJNR, A~PSPID_EDIT
    FROM PROJ AS A
    JOIN PRPS AS B    ON A~PSPNR EQ B~PSPHI
   WHERE A~PROFL      NE @GC_PROFILE     " 설비WBS 제외
     AND B~POSID_EDIT IN @LR_PRPS
    INTO TABLE @GT_PRPS.

  SORT GT_PRPS BY POSID.


  LOOP AT GT_EXCEL INTO GS_EXCEL.

    TRY.
      CLEAR GS_DISPLAY.
      GS_DISPLAY = CORRESPONDING #( GS_EXCEL ).
      GS_DISPLAY-STATUS = ICON_YELLOW_LIGHT.
    CATCH CX_ROOT.
      GS_DISPLAY-STATUS = ICON_RED_LIGHT.
    ENDTRY.


    PERFORM CHECK_POSID.    " WBS
    PERFORM CHECK_POST1.    " WBS명
    PERFORM CHECK_ZZSCT.    " 매출유형
    PERFORM CHECK_ZZPHA.    " 프로젝트 단계
    PERFORM CHECK_ZZWBT.    " WBS 유형
    PERFORM CHECK_ZZBGU.    " 사업구분
    PERFORM CHECK_ZZBGD.    " 사업구분상세
    PERFORM CHECK_ZZIVC.    " 투자여부
    PERFORM CHECK_ZZBAG.    " 사업소유무
    PERFORM CHECK_ZZADT.    " 행정구역
    PERFORM CHECK_ZZPRG.    " 발주처 유형
    PERFORM CHECK_ZZHWB.    " ENG 하위본부
    PERFORM CHECK_ZZCOP.    " 수주유형
    PERFORM CHECK_ZZCYP.    " 통제유형

    APPEND GS_DISPLAY TO GT_DISPLAY.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form VALID_DATA
*&---------------------------------------------------------------------*
FORM VALID_DATA .


  " WBS ID 중복점검
  SELECT POSID, COUNT(*) AS COUNT
    FROM @GT_DISPLAY AS A
   GROUP BY POSID HAVING COUNT(*) > 1
    INTO TABLE @DATA(LT_CHECK).

  IF SY-SUBRC EQ 0.
    LOOP AT LT_CHECK INTO DATA(LS_CHECK).

      LOOP AT GT_DISPLAY INTO GS_DISPLAY WHERE POSID EQ LS_CHECK-POSID.
        __ERROR '동일한 WBS ID가 엑셀파일 내에 존재합니다.'.
        MODIFY GT_DISPLAY FROM GS_DISPLAY.
      ENDLOOP.

    ENDLOOP.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form AMOUNT_TO_SAP
*&---------------------------------------------------------------------*
FORM AMOUNT_TO_SAP  USING PV_WAERS
                          PV_AMOUNT.

  DATA LV_CURRENCY  LIKE TCURC-WAERS.
  DATA LV_AMOUNT    LIKE BAPICURR-BAPICURR.


  LV_CURRENCY = PV_WAERS.
  LV_AMOUNT   = PV_AMOUNT.

  CHECK LV_CURRENCY IS NOT INITIAL
    AND LV_AMOUNT   NE 0.

  CALL FUNCTION 'CURRENCY_AMOUNT_BAPI_TO_SAP'
    EXPORTING
      CURRENCY              = LV_CURRENCY
      BAPI_AMOUNT           = LV_AMOUNT
    IMPORTING
      SAP_AMOUNT            = LV_AMOUNT
    EXCEPTIONS
      BAPI_AMOUNT_INCORRECT = 1
      OTHERS                = 2.

  PV_AMOUNT = LV_AMOUNT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR_ITAB
*&---------------------------------------------------------------------*
FORM CLEAR_ITAB .

  REFRESH: GT_EXCEL,
           GT_DISPLAY,
           GT_1010,
           GT_1020,
           GT_1030,
           GT_1040,
           GT_1050,
           GT_0020,
           GT_1070,
           GT_1090,
           GT_1100,
           GT_1110,
           GT_1120,
           GT_1130.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_OTHERS
*&---------------------------------------------------------------------*
FORM SELECT_OTHERS .


  SELECT FROM ZCOT1010T FIELDS ZZSCT, ZZSCTTX         WHERE SPRAS EQ @SY-LANGU INTO TABLE @GT_1010.   " 매출유형
  SELECT FROM ZCOT1020T FIELDS ZZPHA, ZZPHATX         WHERE SPRAS EQ @SY-LANGU INTO TABLE @GT_1020.   " 프로젝트 단계
  SELECT FROM ZCOT1030T FIELDS ZZWBT, ZZWBTTX         WHERE SPRAS EQ @SY-LANGU INTO TABLE @GT_1030.   " WBS 유형
  SELECT FROM ZCOT1040T FIELDS ZZBGU, ZZBGUTX         WHERE SPRAS EQ @SY-LANGU INTO TABLE @GT_1040.   " 사업구분
  SELECT FROM ZCOT1050T FIELDS ZZBGU, ZZBGD,  ZZBGDTX WHERE SPRAS EQ @SY-LANGU INTO TABLE @GT_1050.   " 사업구분상세
  SELECT FROM ZCOT0020T FIELDS ZZIVC, ZZIVCTX         WHERE SPRAS EQ @SY-LANGU INTO TABLE @GT_0020.   " 투자여부
  SELECT FROM ZCOT1070T FIELDS ZZBAG, ZZBAGTX         WHERE SPRAS EQ @SY-LANGU INTO TABLE @GT_1070.   " 사업소유무
  SELECT FROM ZCOT1090T FIELDS ZZADT, ZZADTTX         WHERE SPRAS EQ @SY-LANGU INTO TABLE @GT_1090.   " 행정구역
  SELECT FROM ZCOT1100T FIELDS ZZPRG, ZZPRGTX         WHERE SPRAS EQ @SY-LANGU INTO TABLE @GT_1100.   " 발주처 유형
  SELECT FROM ZCOT1110T FIELDS ZZHWB, ZZHWBTX         WHERE SPRAS EQ @SY-LANGU INTO TABLE @GT_1110.   " ENG 하위본부
  SELECT FROM ZCOT1120T FIELDS COTYP, COTXT           WHERE SPRAS EQ @SY-LANGU INTO TABLE @GT_1120.   " 수주유형
  SELECT FROM ZCOT1130T FIELDS CTYPE, CTEXT           WHERE SPRAS EQ @SY-LANGU INTO TABLE @GT_1130.   " 통제유형


  SORT: GT_1010 BY ZZSCT,
        GT_1020 BY ZZPHA,
        GT_1030 BY ZZWBT,
        GT_1040 BY ZZBGU,
        GT_1050 BY ZZBGU ZZBGD,
        GT_0020 BY ZZIVC,
        GT_1070 BY ZZBAG,
        GT_1090 BY ZZADT,
        GT_1100 BY ZZPRG,
        GT_1110 BY ZZHWB,
        GT_1120 BY COTYP,
        GT_1130 BY CTYPE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_POSID
*&---------------------------------------------------------------------*
FORM CHECK_POSID .

  IF GS_DISPLAY-POSID(1)   EQ 'E'.
    __ERROR '[WBS]의 첫문자는 ''E'' 로 시작할 수 없습니다.'.
  ELSE.
    READ TABLE GT_PRPS INTO GS_PRPS
                       WITH KEY POSID = GS_DISPLAY-POSID
                                BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DISPLAY-OBJNR = GS_PRPS-OBJNR.
      GS_DISPLAY-PSPID = GS_PRPS-PSPID.
    ELSE.
      __ERROR '[WBS]가 존재하지 않습니다.'.

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_POST1
*&---------------------------------------------------------------------*
FORM CHECK_POST1 .

*  CHECK GS_DISPLAY-POST1 IS INITIAL.

*  __ERROR '[WBS명]이 존재하지 않습니다.'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_DATA
*&---------------------------------------------------------------------*
FORM SAVE_DATA .

  " 오류라인 존재여부 점검
  READ TABLE GT_DISPLAY TRANSPORTING NO FIELDS
                        WITH KEY STATUS = ICON_RED_LIGHT.
  IF SY-SUBRC EQ 0.
    " 오류라인이 존재합니다. 메시지를 확인하세요.
    MESSAGE I000 DISPLAY LIKE 'E' WITH TEXT-E01.
    EXIT.
  ENDIF.


  " 사업 WBS 일괄 변경 작업을 수행하시겠습니까?
  CHECK GC_X EQ ZCL_CO_COMMON=>POPUP_CONFIRM( CONV #( TEXT-M01 ) ).

  CLEAR GV_EXIT.

  PERFORM EXECUTE_BAPI USING GC_X.
  PERFORM EXECUTE_BAPI USING SPACE.

  IF GV_EXIT IS INITIAL.
    " 성공적으로 저장하였습니다.
    MESSAGE S007.
    PERFORM UPDATE_DISPLAY_DATA.
  ELSE.
    " 저장에 실패하였습니다.
    MESSAGE S008 DISPLAY LIKE GC_E.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BAPI_BUS2054_GETDATA
*&---------------------------------------------------------------------*
FORM BAPI_BUS2054_GETDATA
  TABLES PT_LIST          STRUCTURE BAPI_WBS_LIST
         PT_WBS_ELEMENT   STRUCTURE BAPI_BUS2054_DETAIL
         PT_EXTENS_IN     STRUCTURE BAPIPAREX
         PT_EXTENS_OUT    STRUCTURE BAPIPAREX.


  DATA: LT_RETURN     TYPE TABLE OF BAPIRET2.

  CALL FUNCTION 'BAPI_BUS2054_GETDATA'
    TABLES
      IT_WBS_ELEMENT = PT_LIST        " Search WBS Element List
      ET_WBS_ELEMENT = PT_WBS_ELEMENT " WBS Element GetDetail
      ET_RETURN      = LT_RETURN      " Return Parameter
      EXTENSIONIN    = PT_EXTENS_IN
      EXTENSIONOUT   = PT_EXTENS_OUT.

  SORT PT_WBS_ELEMENT BY WBS_ELEMENT.

  READ TABLE LT_RETURN TRANSPORTING NO FIELDS
                       WITH KEY TYPE = GC_E.

  CHECK SY-SUBRC EQ 0.

  GV_EXIT = GC_X.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BAPI_BUS2054_CHANGE
*&---------------------------------------------------------------------*
FORM BAPI_BUS2054_CHANGE USING LS_WBS_ELEMENT STRUCTURE BAPI_BUS2054_DETAIL
                               LS_EXTENS      STRUCTURE BAPIPAREX.

  DATA: LV_PROJECT  TYPE BAPI_BUS2001_NEW-PROJECT_DEFINITION,
        LT_ELEMENT  TYPE TABLE OF BAPI_BUS2054_CHG,
        LT_UPDATE   TYPE TABLE OF BAPI_BUS2054_UPD,
        LT_EXTENS   TYPE TABLE OF BAPIPAREX,
        LT_RETURN   TYPE TABLE OF BAPIRET2 WITH HEADER LINE.


  LV_PROJECT = GS_DISPLAY-PSPID.

  LT_ELEMENT = VALUE #( ( WBS_ELEMENT = LS_WBS_ELEMENT-WBS_ELEMENT
                          DESCRIPTION = LS_WBS_ELEMENT-DESCRIPTION ) ).

  LT_UPDATE  = VALUE #( ( WBS_ELEMENT = LS_WBS_ELEMENT-WBS_ELEMENT
                          DESCRIPTION =
               COND  #( WHEN LS_WBS_ELEMENT-DESCRIPTION IS INITIAL
                        THEN SPACE ELSE GC_X ) ) ).

  LT_EXTENS  = VALUE #( ( LS_EXTENS ) ).


  " BAPI Initialize
  CALL FUNCTION 'BAPI_PS_INITIALIZATION'.

  CALL FUNCTION 'BAPI_BUS2054_CHANGE_MULTI'
    EXPORTING
      I_PROJECT_DEFINITION  = LV_PROJECT
    TABLES
      IT_WBS_ELEMENT        = LT_ELEMENT
      IT_UPDATE_WBS_ELEMENT = LT_UPDATE
      ET_RETURN             = LT_RETURN
      EXTENSIONIN           = LT_EXTENS.


  READ TABLE LT_RETURN WITH KEY TYPE = GC_E.

  IF SY-SUBRC EQ 0.

    " 에러 내용 기록
    GV_EXIT = GC_X.
    GS_DISPLAY-STATUS  = ICON_RED_LIGHT.
    GS_DISPLAY-MESSAGE = LT_RETURN-MESSAGE.

    LOOP AT LT_RETURN.
      APPEND LT_RETURN-MESSAGE TO GS_DISPLAY-MESSAGE_TAB.
    ENDLOOP.

  ELSE.

    "-- 프로젝트 및 WBS 정상처리 사전점검
    PERFORM BAPI_PS_PRECOMMIT.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK
  USING PS_ROW_ID     TYPE LVC_S_ROW
        PS_COLUMN_ID  TYPE LVC_S_COL
        PS_ROW_NO     TYPE LVC_S_ROID
        PR_SENDER     TYPE REF TO CL_GUI_ALV_GRID.

  CASE PR_SENDER.
    WHEN GR_ALV->MR_ALV_GRID.

      READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX PS_ROW_ID-INDEX.
      CHECK SY-SUBRC EQ 0.

      ASSIGN COMPONENT PS_COLUMN_ID-FIELDNAME
          OF STRUCTURE GS_DISPLAY
          TO FIELD-SYMBOL(<FS_VALUE>).

      CHECK SY-SUBRC EQ 0 AND <FS_VALUE> IS NOT INITIAL.

      CASE PS_COLUMN_ID-FIELDNAME.
        WHEN 'POSID'.

          SET PARAMETER ID 'PSP' FIELD SPACE.
          SET PARAMETER ID 'PRO' FIELD <FS_VALUE>.

          CALL TRANSACTION 'CJ03' AND SKIP FIRST SCREEN.

        WHEN 'MESSAGE'.

          PERFORM SHOW_MESSAGE TABLES GS_DISPLAY-MESSAGE_TAB.

      ENDCASE.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_CLOSE
*&---------------------------------------------------------------------*
FORM HANDLE_CLOSE
  USING PR_SENDER TYPE REF TO CL_GUI_DIALOGBOX_CONTAINER.

  PR_SENDER->FREE(
    EXCEPTIONS
      CNTL_ERROR        = 1                " CNTL_ERROR
      CNTL_SYSTEM_ERROR = 2                " CNTL_SYSTEM_ERROR
      OTHERS            = 3
  ).

  FREE PR_SENDER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_MESSAGE
*&---------------------------------------------------------------------*
FORM SHOW_MESSAGE TABLES PT_MESSAGE LIKE GS_DISPLAY-MESSAGE_TAB.

  PERFORM FREE_DIALOGBOX.
  PERFORM CREATE_DIALOGBOX.
  PERFORM CREATE_DIALOGALV.
  PERFORM SHOW_DIALOGALV.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FREE_DIALOGBOX
*&---------------------------------------------------------------------*
FORM FREE_DIALOGBOX .

  IF GR_ALV_DIALOG IS NOT INITIAL.
    IF GR_ALV_DIALOG->MR_ALV_GRID IS BOUND.
      GR_ALV_DIALOG->MR_ALV_GRID->FREE(
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          OTHERS            = 3 ).
    ENDIF.

    IF GR_ALV_DIALOG->MR_CONTAINER IS BOUND.
      GR_ALV_DIALOG->MR_CONTAINER->FREE(
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          OTHERS            = 3 ).
    ENDIF.

    FREE GR_ALV_DIALOG.
  ENDIF.

  IF GR_CON_DIALOG IS NOT INITIAL.
    IF GR_CON_DIALOG IS BOUND.
      GR_CON_DIALOG->FREE(
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          OTHERS            = 3
      ).
    ENDIF.

    FREE GR_CON_DIALOG.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_DIALOGBOX
*&---------------------------------------------------------------------*
FORM CREATE_DIALOGBOX .

  CREATE OBJECT GR_CON_DIALOG
    EXPORTING
      WIDTH   = 500              " Width of This Container
      HEIGHT  = 200              " Height of This Container
      TOP     = 100              " Top Position of Dialog Box
      LEFT    = 300              " Left Position of Dialog Box
      CAPTION = '메시지 기록'    " Dialog Box Caption
    EXCEPTIONS
      CNTL_ERROR                  = 1 " CNTL_ERROR
      CNTL_SYSTEM_ERROR           = 2 " CNTL_SYSTEM_ERROR
      CREATE_ERROR                = 3 " CREATE_ERROR
      LIFETIME_ERROR              = 4 " LIFETIME_ERROR
      LIFETIME_DYNPRO_DYNPRO_LINK = 5 " LIFETIME_DYNPRO_DYNPRO_LINK
      EVENT_ALREADY_REGISTERED    = 6 " Event Already Registered
      ERROR_REGIST_EVENT          = 7 " Error While Registering Event
      OTHERS                      = 8.

  CHECK SY-SUBRC EQ 0.

  SET HANDLER GR_EVENT_RECEIVER->ON_CLOSE FOR GR_CON_DIALOG.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_DIALOGALV
*&---------------------------------------------------------------------*
FORM CREATE_DIALOGALV .

  CREATE OBJECT GR_ALV_DIALOG
    EXPORTING
      I_CONTAINER = GR_CON_DIALOG.

  GR_ALV_DIALOG->SET_FIELD_CATALOG(
    EXPORTING
      " ABAP Dictionary 의 Table/View/Structure
      I_STRUCTURE             = 'BAPIRET2'
    EXCEPTIONS
      INVALID_INPUT_PARAMETER = 1
      EMPTY_FIELD_CATALOG     = 2
      OTHERS                  = 3
  ).

  LOOP AT GR_ALV_DIALOG->MT_FIELDCAT INTO DATA(LS_FIELDCAT).
    CHECK LS_FIELDCAT-FIELDNAME NE 'MESSAGE'.
    LS_FIELDCAT-NO_OUT = GC_X.
    MODIFY GR_ALV_DIALOG->MT_FIELDCAT FROM LS_FIELDCAT.
  ENDLOOP.

  GR_ALV_DIALOG->SET_LAYOUT(
    EXPORTING
      I_LAYOUT = VALUE #( NO_TOOLBAR = GC_X )
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_DIALOGALV
*&---------------------------------------------------------------------*
FORM SHOW_DIALOGALV .

  DATA LT_MESSAGE TYPE TABLE OF BAPIRET2 WITH HEADER LINE.

  LOOP AT GS_DISPLAY-MESSAGE_TAB INTO DATA(LV_MESSAGE).
    LT_MESSAGE = VALUE #( MESSAGE = LV_MESSAGE ).
    APPEND LT_MESSAGE.
  ENDLOOP.

  GR_ALV_DIALOG->DISPLAY(
    CHANGING
      T_OUTTAB = LT_MESSAGE[]
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM UPDATE_DISPLAY_DATA .

  CHECK GV_EXIT IS INITIAL.

  LOOP AT GT_DISPLAY INTO GS_DISPLAY.

    "-- 갱신
    IF GS_DISPLAY-STATUS EQ ICON_YELLOW_LIGHT.
      GS_DISPLAY-STATUS   = ICON_GREEN_LIGHT.
    ENDIF.

    GS_DISPLAY-MESSAGE      = '변경완료'.
    APPEND GS_DISPLAY-MESSAGE TO GS_DISPLAY-MESSAGE_TAB.

    MODIFY GT_DISPLAY FROM GS_DISPLAY.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BAPI_PS_PRECOMMIT
*&---------------------------------------------------------------------*
FORM BAPI_PS_PRECOMMIT.

  DATA LT_RETURN_PRE  TYPE TABLE OF BAPIRET2 WITH HEADER LINE.
  DATA LV_MESSAGE     LIKE GS_DISPLAY-MESSAGE.

  CHECK GV_EXIT IS INITIAL.

  " Precommit 으로 점검
  REFRESH LT_RETURN_PRE[].
  CALL FUNCTION 'BAPI_PS_PRECOMMIT'
    TABLES
      ET_RETURN = LT_RETURN_PRE.

  LOOP AT LT_RETURN_PRE.

    LV_MESSAGE = LT_RETURN_PRE-MESSAGE.

    IF LT_RETURN_PRE-TYPE EQ GC_E.

      GV_EXIT = GC_X.
      __ERROR LV_MESSAGE.

    ELSE.

      IF GS_DISPLAY-STATUS NE ICON_RED_LIGHT.
        GS_DISPLAY-MESSAGE = LV_MESSAGE.
      ENDIF.

      APPEND LV_MESSAGE TO GS_DISPLAY-MESSAGE_TAB.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXECUTE_BAPI
*&---------------------------------------------------------------------*
FORM EXECUTE_BAPI  USING PV_TEST.

  CHECK GV_EXIT IS INITIAL.

  DATA: LT_LIST         TYPE TABLE OF BAPI_WBS_LIST,
        LT_WBS_ELEMENT  TYPE TABLE OF BAPI_BUS2054_DETAIL,
        LT_EXTENS_IN    TYPE TABLE OF BAPIPAREX,
        LT_EXTENS_OUT   TYPE TABLE OF BAPIPAREX,
        LS_WBS_ELEMENT  TYPE BAPI_BUS2054_DETAIL,
        LS_EXTENS       TYPE BAPIPAREX.


  LT_LIST[] = CORRESPONDING #( GT_DISPLAY[] MAPPING WBS_ELEMENT = POSID ).

  " WBS 데이터 조회
  PERFORM BAPI_BUS2054_GETDATA TABLES LT_LIST
                                      LT_WBS_ELEMENT
                                      LT_EXTENS_IN
                                      LT_EXTENS_OUT.

  LOOP AT GT_DISPLAY INTO GS_DISPLAY.

    CLEAR: GS_DISPLAY-MESSAGE,
           GS_DISPLAY-MESSAGE_TAB.

    READ TABLE LT_WBS_ELEMENT INTO LS_WBS_ELEMENT
                              WITH KEY WBS_ELEMENT = GS_DISPLAY-POSID
                                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      " 고객필드 정보 조회
      CLEAR LS_EXTENS.
      READ TABLE LT_EXTENS_OUT INTO LS_EXTENS INDEX SY-TABIX.

      IF LS_EXTENS-STRUCTURE IS INITIAL.
        LS_EXTENS-STRUCTURE = GC_STRUCTURE.
      ENDIF.

      " WBS설명 & 고객필드 갱신
      LS_WBS_ELEMENT-DESCRIPTION = GS_DISPLAY-POST1.
      PERFORM UPDATE_EXTENS USING LS_EXTENS.

      " WBS 에 반영
      PERFORM BAPI_BUS2054_CHANGE USING LS_WBS_ELEMENT
                                        LS_EXTENS.


      " 수행결과 처리
      IF PV_TEST EQ GC_X OR GV_EXIT EQ GC_X.
        " Test / Error 일 경우 Rollback
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING WAIT = GC_X.
      ENDIF.

    ELSE.
      __ERROR 'WBS를 찾을 수가 없습니다.'.
    ENDIF.

    MODIFY GT_DISPLAY FROM GS_DISPLAY.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZSCT
*&---------------------------------------------------------------------*
FORM CHECK_ZZSCT .

  CHECK GS_DISPLAY-ZZSCT IS NOT INITIAL.

  READ TABLE GT_1010 INTO GS_1010
                     WITH KEY ZZSCT = GS_DISPLAY-ZZSCT
                              BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_DISPLAY-ZZSCTTX = GS_1010-ZZSCTTX.
  ELSE.
    __ERROR '[매출유형]의 값이 유효하지 않습니다.'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZPHA
*&---------------------------------------------------------------------*
FORM CHECK_ZZPHA .

  CHECK GS_DISPLAY-ZZPHA IS NOT INITIAL.

  READ TABLE GT_1020 INTO GS_1020
                     WITH KEY ZZPHA = GS_DISPLAY-ZZPHA
                              BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_DISPLAY-ZZPHATX = GS_1020-ZZPHATX.
  ELSE.
    __ERROR '[프로젝트 단계]의 값이 유효하지 않습니다.'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZWBT
*&---------------------------------------------------------------------*
FORM CHECK_ZZWBT .

  CHECK GS_DISPLAY-ZZWBT IS NOT INITIAL.

  READ TABLE GT_1030 INTO GS_1030
                     WITH KEY ZZWBT = GS_DISPLAY-ZZWBT
                              BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_DISPLAY-ZZWBTTX = GS_1030-ZZWBTTX.
  ELSE.
    __ERROR '[WBS 유형]의 값이 유효하지 않습니다.'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZBGU
*&---------------------------------------------------------------------*
FORM CHECK_ZZBGU .

  CHECK GS_DISPLAY-ZZBGU IS NOT INITIAL.

  READ TABLE GT_1040 INTO GS_1040
                     WITH KEY ZZBGU = GS_DISPLAY-ZZBGU
                              BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_DISPLAY-ZZBGUTX = GS_1040-ZZBGUTX.
  ELSE.
    __ERROR '[사업구분]의 값이 유효하지 않습니다.'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZBGD
*&---------------------------------------------------------------------*
FORM CHECK_ZZBGD .

  CHECK GS_DISPLAY-ZZBGD IS NOT INITIAL.

  READ TABLE GT_1050 INTO GS_1050
                     WITH KEY ZZBGU = GS_DISPLAY-ZZBGU
                              ZZBGD = GS_DISPLAY-ZZBGD
                              BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_DISPLAY-ZZBGDTX = GS_1050-ZZBGDTX.
  ELSE.
    __ERROR '[사업구분상세]의 값이 유효하지 않습니다.'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZIVC
*&---------------------------------------------------------------------*
FORM CHECK_ZZIVC .

  CHECK GS_DISPLAY-ZZIVC IS NOT INITIAL.

  READ TABLE GT_0020 INTO GS_0020
                     WITH KEY ZZIVC = GS_DISPLAY-ZZIVC
                              BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_DISPLAY-ZZIVCTX = GS_0020-ZZIVCTX.
  ELSE.
    __ERROR '[투자여부]의 값이 유효하지 않습니다.'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZBAG
*&---------------------------------------------------------------------*
FORM CHECK_ZZBAG .

  CHECK GS_DISPLAY-ZZBAG IS NOT INITIAL.

  READ TABLE GT_1070 INTO GS_1070
                     WITH KEY ZZBAG = GS_DISPLAY-ZZBAG
                              BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_DISPLAY-ZZBAGTX = GS_1070-ZZBAGTX.
  ELSE.
    __ERROR '[사업소유무]의 값이 유효하지 않습니다.'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZADT
*&---------------------------------------------------------------------*
FORM CHECK_ZZADT .

  CHECK GS_DISPLAY-ZZADT IS NOT INITIAL.

  READ TABLE GT_1090 INTO GS_1090
                     WITH KEY ZZADT = GS_DISPLAY-ZZADT
                              BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_DISPLAY-ZZADTTX = GS_1090-ZZADTTX.
  ELSE.
    __ERROR '[행정구역]의 값이 유효하지 않습니다.'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZPRG
*&---------------------------------------------------------------------*
FORM CHECK_ZZPRG .

  CHECK GS_DISPLAY-ZZPRG IS NOT INITIAL.

  READ TABLE GT_1100 INTO GS_1100
                     WITH KEY ZZPRG = GS_DISPLAY-ZZPRG
                              BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_DISPLAY-ZZPRGTX = GS_1100-ZZPRGTX.
  ELSE.
    __ERROR '[발주처 유형]의 값이 유효하지 않습니다.'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZHWB
*&---------------------------------------------------------------------*
FORM CHECK_ZZHWB .

  CHECK GS_DISPLAY-ZZHWB IS NOT INITIAL.

  READ TABLE GT_1110 INTO GS_1110
                     WITH KEY ZZHWB = GS_DISPLAY-ZZHWB
                              BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_DISPLAY-ZZHWBTX = GS_1110-ZZHWBTX.
  ELSE.
    __ERROR '[ENG 하위본부]의 값이 유효하지 않습니다.'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZCOP
*&---------------------------------------------------------------------*
FORM CHECK_ZZCOP .

  CHECK GS_DISPLAY-ZZCOP IS NOT INITIAL.

  READ TABLE GT_1120 INTO GS_1120
                     WITH KEY COTYP = GS_DISPLAY-ZZCOP
                              BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_DISPLAY-ZZCOPTX = GS_1120-COTXT.
  ELSE.
    __ERROR '[수주유형]의 값이 유효하지 않습니다.'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZCYP
*&---------------------------------------------------------------------*
FORM CHECK_ZZCYP .

  CHECK GS_DISPLAY-ZZCYP IS NOT INITIAL.

  READ TABLE GT_1130 INTO GS_1130
                     WITH KEY CTYPE = GS_DISPLAY-ZZCYP
                              BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_DISPLAY-ZZCYPTX = GS_1130-CTEXT.
  ELSE.
    __ERROR '[통제유형]의 값이 유효하지 않습니다.'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_EXTENS
*&---------------------------------------------------------------------*
FORM UPDATE_EXTENS  USING PS_EXTENS STRUCTURE BAPIPAREX .

  DATA DREF TYPE REF TO DATA.
  FIELD-SYMBOLS <FS_WA>.
  FIELD-SYMBOLS <FS_VALUE>.

  CREATE DATA DREF TYPE (PS_EXTENS-STRUCTURE).
  ASSIGN DREF->* TO <FS_WA>.

  CHECK <FS_WA> IS ASSIGNED.
  CLEAR <FS_WA>.

  __SET_VALUE <FS_WA>: 'WBS_ELEMENT'  GS_DISPLAY-POSID,
                       'ZZSCT'        GS_DISPLAY-ZZSCT,
                       'ZZPHA'        GS_DISPLAY-ZZPHA,
                       'ZZWBT'        GS_DISPLAY-ZZWBT,
                       'ZZBGU'        GS_DISPLAY-ZZBGU,
                       'ZZBGD'        GS_DISPLAY-ZZBGD,
                       'ZZIVC'        GS_DISPLAY-ZZIVC,
                       'ZZBAG'        GS_DISPLAY-ZZBAG,
                       'ZZADT'        GS_DISPLAY-ZZADT,
                       'ZZPRG'        GS_DISPLAY-ZZPRG,
                       'ZZHWB'        GS_DISPLAY-ZZHWB,
                       'ZZCOP'        GS_DISPLAY-ZZCOP,
                       'ZZCYP'        GS_DISPLAY-ZZCYP.

  PS_EXTENS-VALUEPART1 = <FS_WA>.
  UNASSIGN <FS_WA>.

ENDFORM.

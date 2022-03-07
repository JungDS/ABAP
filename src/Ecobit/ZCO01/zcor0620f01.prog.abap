*&---------------------------------------------------------------------*
*& Include          ZCOR0620F01
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
  SY-TITLE = '[CO] 설비 WBS 엑셀 업로드'(T01).

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
  PERFORM IMPORT_EXCEL_FILE.

  CHECK GT_EXCEL[] IS NOT INITIAL.

  PERFORM SELECT_OTHERS.
  PERFORM MAKE_DISPLAY_DATA.
  PERFORM CHECK_DUPLECATED_DATA.

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
    MESSAGE '필드카탈로그가 비어있습니다.' TYPE 'S' DISPLAY LIKE GC_E.
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
        LV_TEXT               = TEXT-F01. " 상태
        LS_FIELDCAT-JUST      = GC_C.
        LS_FIELDCAT-ICON      = GC_X.

      WHEN 'VBUKR'.
        LV_TEXT               = TEXT-F02. " 회사코드
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'VGSBR'.
        LV_TEXT               = TEXT-F03. " 사업영역
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'PRCTR'.
        LV_TEXT               = TEXT-F04. " 손익센터
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'PSPID'.
        LV_TEXT               = TEXT-F05. " 프로젝트
        LS_FIELDCAT-KEY       = GC_X.

      WHEN 'PSPIDTX'.
        LV_TEXT               = TEXT-F06. " 프로젝트명

      WHEN 'POSID'.
        LV_TEXT               = TEXT-F07. " WBS
        LS_FIELDCAT-KEY       = GC_X.
        LS_FIELDCAT-HOTSPOT   = GC_X.

      WHEN 'POSIDTX'.
        LV_TEXT               = TEXT-F08. " WBS명

      WHEN 'PSTRT'.
        LV_TEXT               = TEXT-F09. " 시작일
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'PENDE'.
        LV_TEXT               = TEXT-F10. " 종료일
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'ZZIZW'.
        LV_TEXT               = TEXT-F11. " 투자사유
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'ZZCD1'.
        LV_TEXT               = TEXT-F12. " 대분류
        LS_FIELDCAT-JUST      = GC_C.
      WHEN 'ZZCD1TX'.
        LV_TEXT               = TEXT-F13. " 대분류명

      WHEN 'ZZCD2'.
        LV_TEXT               = TEXT-F14. " 중분류
        LS_FIELDCAT-JUST      = GC_C.
      WHEN 'ZZCD2TX'.
        LV_TEXT               = TEXT-F15. " 중분류명

      WHEN 'ZZCD3'.
        LV_TEXT               = TEXT-F16. " 소분류
        LS_FIELDCAT-JUST      = GC_C.
      WHEN 'ZZCD3TX'.
        LV_TEXT               = TEXT-F17. " 소분류명

      WHEN 'ZZTRD'.
        LV_TEXT               = TEXT-F18. " 거래처명

      WHEN 'ZZTCV'.
        LV_TEXT               = TEXT-F19. " 거래금액
        LS_FIELDCAT-NO_SIGN   = GC_X.

      WHEN 'ZZWAE'.
        LV_TEXT               = TEXT-F20. " 통화
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'ZZRT1'.
        LV_TEXT               = TEXT-F21. " 계약금%
        LS_FIELDCAT-NO_SIGN   = GC_X.

      WHEN 'ZZRT2'.
        LV_TEXT               = TEXT-F22. " 중도금%
        LS_FIELDCAT-NO_SIGN   = GC_X.

      WHEN 'ZZRT3'.
        LV_TEXT               = TEXT-F23. " 잔금%
        LS_FIELDCAT-NO_SIGN   = GC_X.

      WHEN 'ZZDT1'.
        LV_TEXT               = TEXT-F24. " 계약금예정일
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'ZZDT2'.
        LV_TEXT               = TEXT-F25. " 중도금예정일
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'ZZDT3'.
        LV_TEXT               = TEXT-F26. " 잔금예정일
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'ZZUNT'.
        LV_TEXT               = TEXT-F27. " 호기
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'ZZCMD'.
        LV_TEXT               = TEXT-F28. " 공사착공일
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'ZZCPD'.
        LV_TEXT               = TEXT-F29. " 공사준공일
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'USR00'.
        LV_TEXT               = TEXT-F30. " 발주자

      WHEN 'USR01'.
        LV_TEXT               = TEXT-F31. " 부서

      WHEN 'USR02'.
        LV_TEXT               = TEXT-F32. " 점검유형

      WHEN 'USR03'.
        LV_TEXT               = TEXT-F33. " 고장유형

      WHEN 'USR08'.
        LV_TEXT               = TEXT-F34. " 계약발주일
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'USR09'.
        LV_TEXT               = TEXT-F35. " 점검일
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'USR10'.
        LV_TEXT               = TEXT-F36. " 사전계획
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'ZZWBT'.
        LV_TEXT               = TEXT-F37. " WBS유형
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'ZZCYP'.
        LV_TEXT               = TEXT-F38. " 통제유형
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'TDLINE'.
        LV_TEXT               = TEXT-F39. " 비고

      WHEN 'TDLINE_TAB'
        OR 'PSPID_NEW'
        OR 'STYLE'
        OR 'COLOR'.
        LS_FIELDCAT-TECH      = GC_X.

      WHEN 'MESSAGE'.
        LV_TEXT               = TEXT-F40. " 오류 점검 및 결과 메시지
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
      I_END_COL               = 35         " 조회종료열
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
      CONDENSE LS_INTERN-VALUE.
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


  LOOP AT GT_EXCEL INTO GS_EXCEL.

    TRY.
      CLEAR GS_DISPLAY.

      " 일부 양식일치화 - 계약금액 콤마(,) 삭제
      REPLACE ALL OCCURRENCES OF ',' IN GS_EXCEL-ZZTCV WITH SPACE.

      _CONVERSION_WBS_IN: GS_EXCEL-PSPID,
                          GS_EXCEL-POSID.

      GS_DISPLAY = CORRESPONDING #( GS_EXCEL ).
      GS_DISPLAY-STATUS = ICON_YELLOW_LIGHT.
    CATCH CX_ROOT.
      GS_DISPLAY-STATUS = ICON_RED_LIGHT.
    ENDTRY.

    PERFORM CHECK_VBUKR.    " 회사코드
    PERFORM CHECK_VGSBR.    " 사업영역
    PERFORM CHECK_PRCTR.    " 손익센터
    PERFORM CHECK_PSPID.    " 프로젝트
    PERFORM CHECK_PSPIDTX.  " 프로젝트명
    PERFORM CHECK_POSID.    " WBS
    PERFORM CHECK_POSIDTX.  " WBS명
    PERFORM CHECK_PSTRT.    " 시작일
    PERFORM CHECK_PENDE.    " 종료일
    PERFORM CHECK_ZZIZW.    " 투자사유
    PERFORM CHECK_ZZCD.     " 대분류/중분류/소분류
    PERFORM CHECK_ZZWAE.    " 통화/계약금액
    PERFORM CHECK_RATE.     " 계약금% + 중도금% + 잔금% = 100% ?
    PERFORM CHECK_ZZUNT.    " 호기
    PERFORM CHECK_ZZCPD.    " 공사준공일
    PERFORM CHECK_USR00.    " 발주자
    PERFORM CHECK_USR01.    " 부서
    PERFORM CHECK_USR10.    " 사전계획
    PERFORM CHECK_ZZWBT.    " WBS유형
    PERFORM CHECK_ZZCYP.    " 통제유형

    __SET_DATE:
*      TEXT-F09 GS_EXCEL-PSTRT GS_DISPLAY-PSTRT,   " 시작일
*      TEXT-F10 GS_EXCEL-PENDE GS_DISPLAY-PENDE,   " 종료일
*      TEXT-F24 GS_EXCEL-ZZDT1 GS_DISPLAY-ZZDT1,   " 계약금예정일
*      TEXT-F25 GS_EXCEL-ZZDT2 GS_DISPLAY-ZZDT2,   " 중도금예정일
*      TEXT-F26 GS_EXCEL-ZZDT3 GS_DISPLAY-ZZDT3,   " 잔금예정일
      TEXT-F28 GS_EXCEL-ZZCMD GS_DISPLAY-ZZCMD,   " 공사착공일
      TEXT-F29 GS_EXCEL-ZZCPD GS_DISPLAY-ZZCPD.   " 공사준공일
*      TEXT-F34 GS_EXCEL-USR08 GS_DISPLAY-USR08,   " 계약발주일
*      TEXT-F35 GS_EXCEL-USR09 GS_DISPLAY-USR09.   " 점검일

*    " 비고(멀티라인처리)
*    PERFORM MAKE_TDLINE_TAB USING    GS_DISPLAY-TDLINE
*                            CHANGING GS_DISPLAY-TDLINE_TAB.

    APPEND GS_DISPLAY TO GT_DISPLAY.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DUPLECATED_DATA
*&---------------------------------------------------------------------*
FORM CHECK_DUPLECATED_DATA .

  CHECK GT_DISPLAY[] IS NOT INITIAL.


  " WBS ID 중복점검
  SELECT POSID, COUNT(*) AS COUNT
    FROM @GT_DISPLAY AS A
   GROUP BY POSID HAVING COUNT(*) > 1
    INTO TABLE @DATA(LT_CHECK).

  CHECK SY-SUBRC EQ 0.

  LOOP AT LT_CHECK INTO DATA(LS_CHECK).

    LOOP AT GT_DISPLAY INTO GS_DISPLAY WHERE POSID EQ LS_CHECK-POSID.
      " 동일한 WBS ID가 엑셀파일 내에 존재합니다.
      __ERROR TEXT-E34.
      MODIFY GT_DISPLAY FROM GS_DISPLAY.
    ENDLOOP.

  ENDLOOP.

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

  REFRESH:
    GT_EXCEL    ,
    GT_DISPLAY  ,
    GT_T001     ,
    GT_TKA02    ,
    GT_TGSB     ,
    GT_CEPC     ,
    GT_PROJ     ,
    GT_PRPS     ,
    GT_1030     ,
    GT_1130     ,
    GT_1270     ,
    GT_1280     ,
    GT_1290     ,
    GT_1300     ,
    GT_TCURC    ,
    GT_2001_NEW ,
    GT_2054_NEW .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_OTHERS
*&---------------------------------------------------------------------*
FORM SELECT_OTHERS .


  " 회사코드
  SELECT BUKRS, LAND1, WAERS
    FROM T001
    INTO TABLE @GT_T001.

  SORT GT_T001 BY BUKRS.


  " 회사코드 종속관계
  SELECT BUKRS, GSBER
    FROM TKA02
   WHERE KOKRS EQ @P_KOKRS
    INTO TABLE @GT_TKA02.

  SORT GT_TKA02 BY BUKRS GSBER.


  " 사업영역
  SELECT GSBER
    FROM TGSB
    INTO TABLE @GT_TGSB.

  SORT GT_TGSB BY GSBER.


  " 손익센터
  SELECT PRCTR
    FROM CEPC
   WHERE KOKRS EQ @P_KOKRS
     AND DATBI GE @SY-DATUM
    INTO TABLE @GT_CEPC.

  SORT GT_CEPC BY PRCTR.


  " 프로젝트
  SELECT PSPID, PROFL
    FROM PROJ
    INTO TABLE @GT_PROJ.

  SORT GT_PROJ BY PSPID PROFL.


  " WBS
  SELECT POSID
    FROM PRPS
    INTO TABLE @GT_PRPS.

  SORT GT_PRPS BY POSID.


  " 투자사유
  SELECT ZZIZW, ZZIZWTX
    FROM ZCOT1270T
   WHERE SPRAS EQ @SY-LANGU
    INTO TABLE @GT_1270.

  SORT GT_1270 BY ZZIZW.


  " 설비분류
  SELECT ZZCD1,
         ZZCD1TX
    FROM ZCOT1280T
   WHERE SPRAS EQ @SY-LANGU
    INTO TABLE @GT_1280.

  SELECT ZZCD1,
         ZZCD2,
         ZZCD2TX
    FROM ZCOT1290T
   WHERE SPRAS EQ @SY-LANGU
    INTO TABLE @GT_1290.

  SELECT ZZCD1,
         ZZCD2,
         ZZCD3,
         ZZCD3TX
    FROM ZCOT1300T
   WHERE SPRAS EQ @SY-LANGU
    INTO TABLE @GT_1300.

  SORT GT_1280 BY ZZCD1.
  SORT GT_1290 BY ZZCD1 ZZCD2.
  SORT GT_1300 BY ZZCD1 ZZCD2 ZZCD3.


  " 통화코드
  SELECT WAERS
    FROM TCURC
    INTO TABLE @GT_TCURC.

  SORT GT_TCURC BY WAERS.


  " WBS유형
  SELECT ZZWBT, ZZWBTTX
    FROM ZCOT1030T
   WHERE SPRAS EQ @SY-LANGU
    INTO TABLE @GT_1030.

  SORT GT_1030 BY ZZWBT.


  " 통제유형
  SELECT CTYPE, CTEXT
    FROM ZCOT1130T
   WHERE SPRAS EQ @SY-LANGU
    INTO TABLE @GT_1130.

  SORT GT_1130 BY CTYPE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_VBUKR
*&---------------------------------------------------------------------*
FORM CHECK_VBUKR .

  DATA LV_CHK TYPE C.

  READ TABLE GT_TKA02 INTO GS_TKA02
                      WITH KEY BUKRS = GS_EXCEL-VBUKR
                               BINARY SEARCH.

  IF SY-SUBRC EQ 0.

    IF GS_TKA02-GSBER IS NOT INITIAL.
      CLEAR LV_CHK.

      LOOP AT GT_TKA02 INTO GS_TKA02 FROM SY-TABIX.
        IF GS_TKA02-BUKRS NE GS_EXCEL-VBUKR.
          EXIT.
        ENDIF.

        CHECK GS_TKA02-GSBER EQ GS_EXCEL-VGSBR.
        LV_CHK = GC_X.
        EXIT.
      ENDLOOP.

      IF LV_CHK IS INITIAL.
        " [사업영역]은 [회사코드]와 같이 사용할 수 없습니다.
        __ERROR TEXT-E03.
      ENDIF.
    ENDIF.

  ELSE.

    " [회사코드]가 존재하지 않습니다.
    __ERROR TEXT-E04.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_VGSBR
*&---------------------------------------------------------------------*
FORM CHECK_VGSBR .

  READ TABLE GT_TGSB TRANSPORTING NO FIELDS
                     WITH KEY GSBER = GS_EXCEL-VGSBR
                              BINARY SEARCH.
  CHECK SY-SUBRC NE 0.

  " [사업영역]가 존재하지 않습니다.
  __ERROR TEXT-E05.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_PRCTR
*&---------------------------------------------------------------------*
FORM CHECK_PRCTR .

  READ TABLE GT_CEPC TRANSPORTING NO FIELDS
                     WITH KEY PRCTR = GS_EXCEL-PRCTR
                              BINARY SEARCH.
  CHECK SY-SUBRC NE 0.

  " [손익센터]가 존재하지 않습니다.
  __ERROR TEXT-E06.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_PSPID
*&---------------------------------------------------------------------*
FORM CHECK_PSPID .

  IF GS_EXCEL-PSPID IS INITIAL.

    " [프로젝트]가 존재하지 않습니다.
    __ERROR TEXT-E07.

  ELSEIF GS_EXCEL-PSPID(1)   NE GC_E.

    " [프로젝트]의 첫문자는 GC_E 로 시작해야 합니다.
    __ERROR TEXT-E08.

  ELSEIF GS_EXCEL-PSPID+1(2) NE GS_EXCEL-VBUKR(2).

    " [프로젝트] E.##의 ##은 회사코드 앞 2자리와 일치해야 합니다.
    __ERROR TEXT-E09.

  ELSE.

    READ TABLE GT_PROJ INTO GS_PROJ
                       WITH KEY PSPID = GS_EXCEL-PSPID
                                BINARY SEARCH.

    IF SY-SUBRC EQ 0.
      IF GS_PROJ-PROFL NE GC_PROFILE.

        " [프로젝트]가 설비프로파일이 아닌 프로젝트입니다.
        __ERROR TEXT-E10.

      ENDIF.
    ELSE.

      " 신규
      GS_DISPLAY-PSPID_NEW = GC_X.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_PSPIDTX
*&---------------------------------------------------------------------*
FORM CHECK_PSPIDTX .

  CHECK GS_EXCEL-PSPIDTX IS INITIAL.

  " [프로젝트명]이 존재하지 않습니다.
  __ERROR TEXT-E11.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_POSID
*&---------------------------------------------------------------------*
FORM CHECK_POSID .

  IF GS_EXCEL-POSID(1)   NE GC_E.

    " [WBS]의 첫문자는 GC_E 로 시작해야 합니다.
    __ERROR TEXT-E12.

  ELSEIF GS_EXCEL-POSID+1(2) NE GS_EXCEL-VBUKR(2).

    " [WBS] E.##의 ##은 회사코드 앞 2자리와 일치해야 합니다.
    __ERROR TEXT-E13.

  ELSEIF GS_EXCEL-POSID EQ GS_EXCEL-PSPID.

    " [프로젝트] 와 [WBS] 가 동일합니다.
    __ERROR TEXT-E14.

  ELSE.

    DATA(LV_PSPID) = CONV PS_PSPID_EDIT( GS_EXCEL-PSPID ).
    DATA(LV_POSID) = CONV PS_POSID_EDIT( GS_EXCEL-POSID ).

      _CONVERSION_WBS_OUT: LV_PSPID,
                           LV_POSID.

    IF LV_POSID NP LV_PSPID && '*'.

    " [WBS] 가 [프로젝트] 로 시작되지 않습니다.
    __ERROR TEXT-E15.

    ELSE.

      READ TABLE GT_PRPS TRANSPORTING NO FIELDS
                         WITH KEY POSID = GS_EXCEL-POSID
                                  BINARY SEARCH.
      IF SY-SUBRC EQ 0.

        " 동일한 [WBS]가 존재합니다.
        __ERROR TEXT-E16.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_POSIDTX
*&---------------------------------------------------------------------*
FORM CHECK_POSIDTX .

  CHECK GS_EXCEL-POSIDTX IS INITIAL.

  " [WBS명]이 존재하지 않습니다.
  __ERROR TEXT-E17.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_PSTRT
*&---------------------------------------------------------------------*
FORM CHECK_PSTRT .

*  CHECK GS_EXCEL-PSTRT IS INITIAL.
*
*  " [시작일]이 존재하지 않습니다.
*  __ERROR TEXT-E18.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_PENDE
*&---------------------------------------------------------------------*
FORM CHECK_PENDE .

*  IF GS_EXCEL-PENDE IS INITIAL.
*
*    " [종료일]이 존재하지 않습니다.
*    __ERROR TEXT-E19.
*
*  ELSEIF GS_EXCEL-PSTRT GT GS_EXCEL-PENDE.
*
*    " [시작일]이 [종료일]보다 이후입니다.
*    __ERROR TEXT-E20.
*
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZIZW
*&---------------------------------------------------------------------*
FORM CHECK_ZZIZW .

  READ TABLE GT_1270 INTO GS_1270
                     WITH KEY ZZIZW = GS_EXCEL-ZZIZW
                              BINARY SEARCH.
  IF SY-SUBRC EQ 0.

    GS_DISPLAY-ZZIZWTX = GS_1270-ZZIZWTX.

  ELSE.

    " [투자사유]가 존재하지 않습니다.
    __ERROR TEXT-E21.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZCD
*&---------------------------------------------------------------------*
FORM CHECK_ZZCD .

  READ TABLE GT_1280 INTO GS_1280
                     WITH KEY ZZCD1 = GS_EXCEL-ZZCD1
                              BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_DISPLAY-ZZCD1TX = GS_1280-ZZCD1TX.

    IF GS_EXCEL-ZZCD2 IS INITIAL.

      IF GS_EXCEL-ZZCD3 IS NOT INITIAL.

        " [중분류]가 존재하지 않습니다.
        __ERROR TEXT-E22.

      ENDIF.

    ELSE.

      READ TABLE GT_1290 INTO GS_1290
                           WITH KEY ZZCD1 = GS_EXCEL-ZZCD1
                                    ZZCD2 = GS_EXCEL-ZZCD2
                                    BINARY SEARCH.

      IF SY-SUBRC EQ 0.

        GS_DISPLAY-ZZCD2TX = GS_1290-ZZCD2TX.

        IF GS_EXCEL-ZZCD3 IS NOT INITIAL.
          READ TABLE GT_1300 INTO GS_1300
                             WITH KEY ZZCD1 = GS_EXCEL-ZZCD1
                                      ZZCD2 = GS_EXCEL-ZZCD2
                                      ZZCD3 = GS_EXCEL-ZZCD3
                                      BINARY SEARCH.
          IF SY-SUBRC EQ 0.

            GS_DISPLAY-ZZCD3TX = GS_1300-ZZCD3TX.

          ELSE.

            " [소분류]가 존재하지 않습니다.
            __ERROR TEXT-E23.

          ENDIF.

        ENDIF.

      ELSE.

        " [중분류]가 존재하지 않습니다.
        __ERROR TEXT-E22.

      ENDIF.

    ENDIF.

  ELSE.

    " [대분류]가 존재하지 않습니다.
    __ERROR TEXT-E24.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZWAE
*&---------------------------------------------------------------------*
FORM CHECK_ZZWAE .


  IF GS_EXCEL-ZZWAE IS INITIAL.

    IF GS_EXCEL-ZZTCV IS NOT INITIAL.
      " [통화코드]가 존재하지 않습니다.
      __ERROR TEXT-E25.
    ENDIF.

  ELSE.

    READ TABLE GT_TCURC TRANSPORTING NO FIELDS
                        WITH KEY WAERS = GS_EXCEL-ZZWAE
                                 BINARY SEARCH.

    IF SY-SUBRC EQ 0.
      " 통화코드 점검 이후 금액 소수점 단위 조정
      PERFORM AMOUNT_TO_SAP USING GS_DISPLAY-ZZWAE
                                  GS_DISPLAY-ZZTCV.

    ELSE.

      " [통화코드]가 존재하지 않습니다.
      __ERROR TEXT-E25.

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_RATE
*&---------------------------------------------------------------------*
FORM CHECK_RATE .

*  CHECK NOT ( GS_DISPLAY-ZZRT1 EQ 0
*          AND GS_DISPLAY-ZZRT2 EQ 0
*          AND GS_DISPLAY-ZZRT3 EQ 0 ).
*
*  IF GS_DISPLAY-ZZRT1 +
*     GS_DISPLAY-ZZRT2 +
*     GS_DISPLAY-ZZRT3 NE 100.
*
*    " [계약금%]/[중도금%]/[잔금%]은 합계는 100%가 되어야 합니다.
*    __ERROR TEXT-E26.
*
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZUNT
*&---------------------------------------------------------------------*
FORM CHECK_ZZUNT .

*  CHECK GS_EXCEL-ZZUNT IS NOT INITIAL
*    AND GS_EXCEL-ZZUNT NOT BETWEEN '0' AND '9'.
*
*  " [호기]는 숫자 1자리만 허용합니다.
*  __ERROR TEXT-E27.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZCPD
*&---------------------------------------------------------------------*
FORM CHECK_ZZCPD .

  IF GS_EXCEL-ZZCPD IS INITIAL.
   " [공사준공일]이 존재하지 않습니다.
   __ERROR TEXT-E37.
  ELSE.
    CHECK GS_EXCEL-ZZCMD IS NOT INITIAL
      AND GS_EXCEL-ZZCMD GT GS_EXCEL-ZZCPD.

    " [공사착공일]이 [공사준공일]보다 이후입니다.
    __ERROR TEXT-E35.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_USR00
*&---------------------------------------------------------------------*
FORM CHECK_USR00 .

*  CHECK GS_EXCEL-USR00 IS INITIAL.
*
*  " [발주자]이 존재하지 않습니다.
*  __ERROR TEXT-E28.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_USR01
*&---------------------------------------------------------------------*
FORM CHECK_USR01 .

*  CHECK GS_EXCEL-USR01 IS INITIAL.
*
*  " [부서]가 존재하지 않습니다.
*  __ERROR TEXT-E29.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_USR10
*&---------------------------------------------------------------------*
FORM CHECK_USR10 .

*  CHECK NOT ( GS_EXCEL-USR10 IS INITIAL
*           OR GS_EXCEL-USR10 EQ GC_X ).
*
*  " [사전계획]은 공백 또는 'X'만 허용합니다.
*  __ERROR TEXT-E30.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZWBT
*&---------------------------------------------------------------------*
FORM CHECK_ZZWBT .

  READ TABLE GT_1030 INTO GS_1030
                     WITH KEY ZZWBT = GS_EXCEL-ZZWBT
                              BINARY SEARCH.

  IF SY-SUBRC EQ 0.

    GS_DISPLAY-ZZWBTTX = GS_1030-ZZWBTTX.

  ELSE.

    " [WBS유형]이 존재하지 않습니다.
    __ERROR TEXT-E31.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ZZCYP
*&---------------------------------------------------------------------*
FORM CHECK_ZZCYP .

  READ TABLE GT_1130 INTO GS_1130
                     WITH KEY CTYPE = GS_EXCEL-ZZCYP
                              BINARY SEARCH.

  IF SY-SUBRC EQ 0.

    GS_DISPLAY-ZZCYPTX = GS_1130-CTEXT.

  ELSE.

    " [통제유형]이 존재하지 않습니다.
    __ERROR TEXT-E32.

  ENDIF.

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
    MESSAGE I000 DISPLAY LIKE GC_E WITH TEXT-E01.
    EXIT.
  ENDIF.


  " 생성대상 데이터
  PERFORM MAKE_NEW_DATA.

  IF GV_EXIT EQ GC_X.
    " 프로젝트 생성 중단
    EXIT.
  ENDIF.


  " 설비 WBS 업로드 작업을 수행하시겠습니까?
  CHECK GC_X EQ ZCL_CO_COMMON=>POPUP_CONFIRM( CONV #( TEXT-M01 ) ).


  " 프로젝트 기준 정렬
  SORT GT_2001_NEW BY PSPID.
  SORT GT_2054_NEW BY PSPID.

  PERFORM EXECUTE_BAPI USING GC_X.
  PERFORM EXECUTE_BAPI USING SPACE.

  CHECK GV_EXIT IS INITIAL.

  "Success
  PERFORM SET_PRJ_STATUS_RELEASE.
  PERFORM SET_WBS_STATUS_RELEASE.
*  PERFORM SET_WBS_SHORT_ID.      " Short ID 의 마지막 숫자 짤림 방지
  PERFORM UPDATE_DISPLAY_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BAPI_BUS2001_CREATE
*&---------------------------------------------------------------------*
FORM BAPI_BUS2001_CREATE  USING PV_PSPID
                                PV_POST1
                                PV_BUKRS
                                PV_GSBER
                                PV_PRCTR.

  DATA LS_PROJECT TYPE BAPI_BUS2001_NEW.
  DATA LT_RETURN  TYPE TABLE OF BAPIRET2 WITH HEADER LINE.
  DATA LV_MESSAGE TYPE BAPIRET2-MESSAGE.

  IF GS_T001-BUKRS NE PV_BUKRS.
    CLEAR GS_T001.
    READ TABLE GT_T001 INTO GS_T001
                       WITH KEY BUKRS = PV_BUKRS
                                BINARY SEARCH.
  ENDIF.


  LS_PROJECT = VALUE #(
    PROJECT_DEFINITION    = PV_PSPID        " 프로젝트 정의
    DESCRIPTION           = PV_POST1        " PS: 간단한 설명(첫 번째 텍스트 라인)
    PROJECT_PROFILE       = GC_PROFILE      " 프로젝트 프로파일
    CONTROLLING_AREA      = P_KOKRS         " 프로젝트 관리회계영역
    COMPANY_CODE          = PV_BUKRS        " 프로젝트 회사 코드
    BUSINESS_AREA         = PV_GSBER        " 프로젝트 사업영역
    PROFIT_CTR            = PV_PRCTR        " 손익 센터
    PROJECT_CURRENCY      = GS_T001-WAERS   " WBS 통화(프로젝트 정의)
    CALENDAR              = GS_T001-LAND1   " 공장 달력 키
    TIME_UNIT             = 'STD'           " 시간 일정계획내 시간단위
  ).

  CALL FUNCTION 'BAPI_BUS2001_CREATE'
    EXPORTING
      " Data Structure: Create Project Definition
      I_PROJECT_DEFINITION = LS_PROJECT
    TABLES
      " Return Parameter
      ET_RETURN            = LT_RETURN.

  READ TABLE LT_RETURN WITH KEY TYPE = GC_E.

  IF SY-SUBRC EQ 0.
    LV_MESSAGE = LT_RETURN-MESSAGE.

    " 프로젝트 생성 중 ERROR
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    GV_EXIT = GC_X.

    LOOP AT GT_DISPLAY INTO GS_DISPLAY WHERE PSPID EQ PV_PSPID.

      " 에러 내용 기록
      GS_DISPLAY-STATUS  = ICON_RED_LIGHT.
      GS_DISPLAY-MESSAGE = LV_MESSAGE.

      LOOP AT LT_RETURN.
        APPEND LT_RETURN-MESSAGE TO GS_DISPLAY-MESSAGE_TAB.
      ENDLOOP.

      MODIFY GT_DISPLAY FROM GS_DISPLAY.
    ENDLOOP.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form BAPI_BUS2054_CREATE
*&---------------------------------------------------------------------*
FORM BAPI_BUS2054_CREATE TABLES PT_WBS TYPE STANDARD TABLE
                          USING PV_PSPID.


  DATA LT_WBS_ELEMENT TYPE TABLE OF BAPI_BUS2054_NEW WITH HEADER LINE.
  DATA LT_EXTENSIONIN TYPE TABLE OF BAPIPAREX        WITH HEADER LINE.
  DATA LT_RETURN      TYPE TABLE OF BAPIRET2         WITH HEADER LINE.


  DATA: BEGIN OF LS_BAPI_TE_WBS.
          INCLUDE STRUCTURE BAPI_TE_WBS_ELEMENT.
  DATA:   PROJECT_DEFINITION TYPE PS_PSPID,
       END OF LS_BAPI_TE_WBS.

  DATA LV_WBS_ELEMENT TYPE BAPI_BUS2054_NEW-WBS_ELEMENT.
  DATA LV_MESSAGE     TYPE BAPIRET2-MESSAGE.
  DATA LV_TABIX       TYPE SY-TABIX.


  LOOP AT PT_WBS INTO GS_WBS.

    READ TABLE GT_DISPLAY INTO GS_DISPLAY WITH KEY POSID = GS_WBS-POSID.
    CHECK SY-SUBRC EQ 0.


    " 회사코드 정보 조회
    PERFORM GET_T001 USING GS_DISPLAY-VBUKR.


    LT_WBS_ELEMENT = VALUE #(
      WBS_ELEMENT                     = GS_DISPLAY-POSID    " 작업 분석 구조 요소(WBS 요소)
      DESCRIPTION                     = GS_DISPLAY-POSIDTX  " PS: 간단한 설명(첫 번째 텍스트 라인)
      COMPANY_CODE                    = GS_DISPLAY-VBUKR    " WBS 요소의 회사 코드
      BUSINESS_AREA                   = GS_DISPLAY-VGSBR    " WBS 요소에 대한 사업영역
      CONTROLLING_AREA                = P_KOKRS             " WBS 요소의 관리회계 영역
      PROFIT_CTR                      = GS_DISPLAY-PRCTR    " 손익 센터
      PROJ_TYPE                       = '01'                " 프로젝트유형
      WBS_PLANNING_ELEMENT            = GC_X                " 지시자: 계획 요소
      WBS_ACCOUNT_ASSIGNMENT_ELEMENT  = GC_X                " 지시자: 계정 지정 요소
      CALENDAR                        = GS_T001-LAND1       " 공장 달력 키
      CURRENCY                        = GS_T001-WAERS       " WBS 요소 통화
      USER_FIELD_KEY                  = '0000001'	          " 사용자 정의 필드에 대한 키워드 ID
*      USER_FIELD_KEY                  = 'Z000001'            " 사용자 정의 필드에 대한 키워드 ID
*      USER_FIELD_CHAR20_1             = GS_DISPLAY-USR00    " 발주자
*      USER_FIELD_CHAR20_2             = GS_DISPLAY-USR01    " 부서
*      USER_FIELD_CHAR10_1             = GS_DISPLAY-USR02    " 점검유형
*      USER_FIELD_CHAR10_2             = GS_DISPLAY-USR03    " 고장유형
*      USER_FIELD_DATE1                = GS_DISPLAY-USR08    " 계약발주일
*      USER_FIELD_DATE2                = GS_DISPLAY-USR09    " 점검일
*      USER_FIELD_FLAG1                = GS_DISPLAY-USR10    " 사전계획
      WBS_SUMMARIZATION               = GC_X                " 프로젝트 집계
      WBS_BASIC_START_DATE            = '20010101'          " WBS 요소: 기본 시작일
      WBS_BASIC_FINISH_DATE           = '20471231'          " GS_DISPLAY-PENDE    " WBS 요소: 기본종료일
*      WBS_BASIC_START_DATE            = GS_DISPLAY-PSTRT    " WBS 요소: 기본 시작일
*      WBS_BASIC_FINISH_DATE           = GS_DISPLAY-PENDE    " WBS 요소: 기본종료일
      WBS_LEFT                        = LV_WBS_ELEMENT      " 작업 분석 구조 요소(WBS 요소)
    ).
    APPEND LT_WBS_ELEMENT.

    LS_BAPI_TE_WBS = VALUE #(
      WBS_ELEMENT         = GS_DISPLAY-POSID   " 작업 분석 구조 요소(WBS 요소)
      ZZWBT               = GS_DISPLAY-ZZWBT   " WBS 유형
      ZZCYP               = GS_DISPLAY-ZZCYP   " 통제유형
      ZZIZW               = GS_DISPLAY-ZZIZW   " 투자사유
      ZZCD1               = GS_DISPLAY-ZZCD1   " 설비분류코드(대분류)
      ZZCD2               = GS_DISPLAY-ZZCD2   " 설비분류코드(중분류)
      ZZCD3               = GS_DISPLAY-ZZCD3   " 설비분류코드(소분류)
*      ZZTRD               = GS_DISPLAY-ZZTRD   " 거래처
      ZZTCV               = GS_DISPLAY-ZZTCV   " 계약금액
      ZZWAE               = GS_DISPLAY-ZZWAE   " 통화
*      ZZRT1               = GS_DISPLAY-ZZRT1   " 계약금 비율(%)
*      ZZRT2               = GS_DISPLAY-ZZRT2   " 중도금 비율(%)
*      ZZRT3               = GS_DISPLAY-ZZRT3   " 잔금   비율(%)
*      ZZDT1               = GS_DISPLAY-ZZDT1   " 계약금 지급예정일
*      ZZDT2               = GS_DISPLAY-ZZDT2   " 중도금 지급예정일
*      ZZDT3               = GS_DISPLAY-ZZDT3   " 잔금   지급예정일
      ZZUNT               = GS_DISPLAY-ZZUNT   " 호기
      ZZCMD               = GS_DISPLAY-ZZCMD   " 공사착공일
      ZZCPD               = GS_DISPLAY-ZZCPD   " 공사준공일
      PROJECT_DEFINITION  = GS_DISPLAY-PSPID
    ).

    LT_EXTENSIONIN = VALUE #(
      STRUCTURE  = 'BAPI_TE_WBS_ELEMENT'
      VALUEPART1 = LS_BAPI_TE_WBS
    ).
    APPEND LT_EXTENSIONIN.


    LV_WBS_ELEMENT = GS_DISPLAY-POSID.
  ENDLOOP.


  CALL FUNCTION 'BAPI_BUS2054_CREATE_MULTI'
    EXPORTING
      I_PROJECT_DEFINITION = GS_DISPLAY-PSPID " Project Definition
    TABLES
      " Data Structure: Create WBS Element
      IT_WBS_ELEMENT       = LT_WBS_ELEMENT[]
      " Return Parameter
      ET_RETURN            = LT_RETURN[]
      " Reference Structure for BAPI Parameters EXTENSIONIN/EXTENSIONOUT
      EXTENSIONIN          = LT_EXTENSIONIN[]
      " Reference Structure for BAPI Parameters EXTENSIONIN/EXTENSIONOUT
*      EXTENSIONOUT         =
    .

  READ TABLE LT_RETURN WITH KEY TYPE = GC_E.


  IF SY-SUBRC EQ 0.
    LV_MESSAGE = LT_RETURN-MESSAGE.

    " WBS 생성 중 ERROR
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    GV_EXIT = GC_X.

    LOOP AT PT_WBS INTO GS_WBS.

      READ TABLE GT_DISPLAY INTO GS_DISPLAY WITH KEY POSID = GS_WBS-POSID.
      CHECK SY-SUBRC EQ 0. LV_TABIX = SY-TABIX.

      " 에러 내용 기록
      GS_DISPLAY-STATUS  = ICON_RED_LIGHT.
      GS_DISPLAY-MESSAGE = LV_MESSAGE.

      LOOP AT LT_RETURN.
        APPEND LT_RETURN-MESSAGE TO GS_DISPLAY-MESSAGE_TAB.
      ENDLOOP.

      MODIFY GT_DISPLAY FROM GS_DISPLAY INDEX LV_TABIX.
    ENDLOOP.

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

          SELECT COUNT(*)
            FROM PRPS
           WHERE POSID = <FS_VALUE>.

          IF SY-SUBRC EQ 0.

            SET PARAMETER ID 'PSP'      FIELD SPACE.
            SET PARAMETER ID 'PRO'      FIELD <FS_VALUE>.
            SET PARAMETER ID 'ANR'      FIELD SPACE.

            ZCL_CO_COMMON=>CALL_TRANSACTION(
              EXPORTING
                I_TCODE       = 'CJ20N'  " Transaction Code
                I_SKIP_SCREEN = GC_X     " Skip First Screen
                I_NEW_SESSION = SPACE    " Call from New Session
                IT_SPAGPA     = VALUE #( ( PARID = 'PSP' PARVAL = SPACE )
                                         ( PARID = 'PRO' PARVAL = <FS_VALUE> )
                                         ( PARID = 'ANR' PARVAL = SPACE ) ) " Transaction Parameters
            ).
          ENDIF.

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
*& Form SET_PRJ_STATUS_RELEASE
*&---------------------------------------------------------------------*
FORM SET_PRJ_STATUS_RELEASE .

  DATA LS_RETURN  TYPE BAPIRETURN1.
  DATA LT_RESULT  TYPE TABLE OF BAPI_STATUS_RESULT WITH HEADER LINE.
  DATA LV_MESSAGE TYPE BAPI_STATUS_RESULT-MESSAGE_TEXT.


  LOOP AT GT_2001_NEW INTO GS_2001_NEW.
    CLEAR GV_EXIT.

    " BAPI Initialize
    CALL FUNCTION 'BAPI_PS_INITIALIZATION'.
    CALL FUNCTION 'BAPI_BUS2001_SET_STATUS'
      EXPORTING
        PROJECT_DEFINITION = GS_2001_NEW-PSPID  " Project Definition
        SET_SYSTEM_STATUS  = 'REL'              " Set System Status
      IMPORTING
        RETURN             = LS_RETURN          " Return Parameter
      TABLES
        " Error Messages for Setting/Resetting Status
        E_RESULT           = LT_RESULT.

    READ TABLE LT_RESULT WITH KEY MESSAGE_TYPE = GC_E.

    IF SY-SUBRC EQ 0.

      GV_EXIT = GC_X.
      LV_MESSAGE = LT_RESULT-MESSAGE_TEXT.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      LOOP AT GT_DISPLAY  INTO GS_DISPLAY
                         WHERE PSPID EQ GS_2001_NEW-PSPID.
        " 에러 내용 기록
        GS_DISPLAY-STATUS  = ICON_RED_LIGHT.
        GS_DISPLAY-MESSAGE = LV_MESSAGE.

        LOOP AT LT_RESULT.
          APPEND LT_RESULT-MESSAGE_TEXT TO GS_DISPLAY-MESSAGE_TAB.
        ENDLOOP.

        MODIFY GT_DISPLAY FROM GS_DISPLAY.
      ENDLOOP.

    ELSE.

      PERFORM BAPI_PS_PRECOMMIT.

      IF GV_EXIT IS INITIAL.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = GC_X.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        LOOP AT GT_DISPLAY  INTO GS_DISPLAY
                           WHERE PSPID EQ GS_2001_NEW-PSPID.

          " 에러 내용 기록
          __ERROR TEXT-E36. " 프로젝트 상태변환 실패

          MODIFY GT_DISPLAY FROM GS_DISPLAY.
        ENDLOOP.

      ENDIF.
    ENDIF.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_WBS_STATUS_RELEASE
*&---------------------------------------------------------------------*
FORM SET_WBS_STATUS_RELEASE.

  DATA LT_STATUS  TYPE TABLE OF BAPI_WBS_MNT_SYSTEM_STATUS.
  DATA LT_RESULT  TYPE TABLE OF BAPI_STATUS_RESULT WITH HEADER LINE.
  DATA LV_MESSAGE TYPE BAPI_STATUS_RESULT-MESSAGE_TEXT.


  LOOP AT GT_DISPLAY INTO GS_DISPLAY.
    CLEAR GV_EXIT.

    LT_STATUS = VALUE #( (
      WBS_ELEMENT       = GS_DISPLAY-POSID
      SET_SYSTEM_STATUS = 'REL'
    ) ).

    " BAPI Initialize
    CALL FUNCTION 'BAPI_PS_INITIALIZATION'.
    CALL FUNCTION 'BAPI_BUS2054_SET_STATUS'
      TABLES
        " Set/Reset WBS Element System Status
        I_WBS_SYSTEM_STATUS = LT_STATUS
        " Set/Reset WBS Element User Status
*        I_WBS_USER_STATUS   =
        " Error Messages for Setting/Resetting Status
        E_RESULT            = LT_RESULT.

    READ TABLE LT_RESULT WITH KEY MESSAGE_TYPE = GC_E.

    IF SY-SUBRC EQ 0.

      GV_EXIT = GC_X.
      LV_MESSAGE = LT_RESULT-MESSAGE_TEXT.

      " Rollback
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      " 에러 내용 기록
      GS_DISPLAY-STATUS  = ICON_RED_LIGHT.
      GS_DISPLAY-MESSAGE = LV_MESSAGE.

      LOOP AT LT_RESULT.
        APPEND LT_RESULT-MESSAGE_TEXT TO GS_DISPLAY-MESSAGE_TAB.
      ENDLOOP.

      MODIFY GT_DISPLAY FROM GS_DISPLAY.

    ELSE.

      PERFORM BAPI_PS_PRECOMMIT.

      IF GV_EXIT IS INITIAL.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = GC_X.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        " 에러 내용 기록
        __ERROR TEXT-E36. " 프로젝트 상태변환 실패

        MODIFY GT_DISPLAY FROM GS_DISPLAY.

      ENDIF.

    ENDIF.

  ENDLOOP.



ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPDATE_DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM UPDATE_DISPLAY_DATA .

  CHECK GV_EXIT IS INITIAL.

  SELECT POSID, PSPNR, OBJNR
    FROM PRPS
     FOR ALL ENTRIES IN @GT_DISPLAY
   WHERE POSID = @GT_DISPLAY-POSID
    INTO TABLE @DATA(LT_PRPS).

  SORT LT_PRPS BY POSID.


  LOOP AT GT_DISPLAY INTO GS_DISPLAY.

    READ TABLE LT_PRPS INTO DATA(LS_PRPS)
                       WITH KEY POSID = GS_DISPLAY-POSID
                                BINARY SEARCH.
    IF SY-SUBRC EQ 0.

      "-- 갱신
      IF GS_DISPLAY-STATUS EQ ICON_YELLOW_LIGHT.
        GS_DISPLAY-STATUS   = ICON_GREEN_LIGHT.
      ENDIF.

      GS_DISPLAY-OBJNR    = LS_PRPS-OBJNR.
      GS_DISPLAY-MESSAGE  = TEXT-M03. " 생성완료

    ELSE.

      GS_DISPLAY-STATUS   = ICON_RED_LIGHT.
      GS_DISPLAY-MESSAGE  = TEXT-M04. " 생성실패

    ENDIF.

    GS_DISPLAY-PSPID_NEW  = SPACE.
    MODIFY GT_DISPLAY FROM GS_DISPLAY.

    "-- Long Save 기록
    PERFORM SAVE_TEXT_EXECUTE USING LS_PRPS-PSPNR.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_TDLINE_TAB
*&---------------------------------------------------------------------*
FORM MAKE_TDLINE_TAB USING PV_TDLINE LIKE BAPITLINE-TDLINE
                           PT_TDLINE TYPE LDPS_TXT_TAB.

  " 엑셀의 비고 데이터를 라인 단위로 잘라 Internal table 에 보관
  SPLIT PV_TDLINE AT GC_NEW_LINE INTO TABLE PT_TDLINE.

  DATA(LV_LINES) = LINES( PT_TDLINE ).

  " 엑셀은 2줄 이상인 경우 셀 복사시 앞뒤로 쌍따음표(") 가 붙는다.
  CHECK LV_LINES > 1.

  " 앞뒤의 쌍따음표(") 제거를 위한 조회
  READ TABLE PT_TDLINE INTO DATA(LV_TDLINE_FIRST) INDEX 1.
  READ TABLE PT_TDLINE INTO DATA(LV_TDLINE_LAST)  INDEX LV_LINES.

  DATA(LV_STRLEN) = STRLEN( LV_TDLINE_LAST ) - 1.

  IF LV_TDLINE_FIRST(1) EQ '"' AND LV_TDLINE_LAST+LV_STRLEN(1) EQ '"'.
    LV_TDLINE_FIRST = LV_TDLINE_FIRST+1.
    LV_TDLINE_LAST  = COND #( WHEN LV_STRLEN > 0
                              THEN LV_TDLINE_LAST(LV_STRLEN) ).

    MODIFY PT_TDLINE FROM LV_TDLINE_FIRST INDEX 1.
    MODIFY PT_TDLINE FROM LV_TDLINE_LAST  INDEX LV_LINES.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_TEXT_EXECUTE
*&---------------------------------------------------------------------*
FORM SAVE_TEXT_EXECUTE  USING PV_PSPNR.

*  DATA LS_HEADER  TYPE THEAD.
*  DATA LT_LINES   TYPE TABLE OF TLINE WITH HEADER LINE.
*
*
*  CHECK GS_DISPLAY-TDLINE_TAB[] IS NOT INITIAL.
*
*  LOOP AT GS_DISPLAY-TDLINE_TAB INTO DATA(LV_TDLINE).
*    LT_LINES-TDLINE = CONV #( LV_TDLINE ).
*    APPEND LT_LINES.
*  ENDLOOP.
*
*  "-- Long Text
*  LS_HEADER = VALUE #(
*    TDOBJECT  = 'PMS'
*    TDNAME    = GC_E && PV_PSPNR
*    TDID      = 'LTXT'
*    TDSPRAS   = SY-LANGU
*    TDFORM    = 'SYSTEM'
*    TDSTYLE   = 'S_OFFICE'
*  ).
*
*  "__ SAVE TEXT
*  PERFORM SAVE_TEXT(ZCAR9000) TABLES LT_LINES
*                               USING LS_HEADER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BAPI_PS_PRECOMMIT
*&---------------------------------------------------------------------*
FORM BAPI_PS_PRECOMMIT.

  DATA LT_RETURN_PRE  TYPE TABLE OF BAPIRET2 WITH HEADER LINE.

  " Precommit 으로 점검
  REFRESH LT_RETURN_PRE[].
  CALL FUNCTION 'BAPI_PS_PRECOMMIT'
    TABLES
      ET_RETURN = LT_RETURN_PRE.

  READ TABLE LT_RETURN_PRE WITH KEY TYPE = GC_E.

  CHECK SY-SUBRC EQ 0.

  " Rollback
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  GV_EXIT = GC_X.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXECUTE_BAPI
*&---------------------------------------------------------------------*
FORM EXECUTE_BAPI  USING PV_TEST.

  DATA LT_WBS   LIKE TABLE OF GS_WBS.


  CHECK GV_EXIT IS INITIAL.

  LOOP AT GT_2054_NEW INTO GS_2054_NEW.

    AT NEW PSPID.
      REFRESH LT_WBS.

      " BAPI Initialize
      CALL FUNCTION 'BAPI_PS_INITIALIZATION'.

      " 신규 프로젝트 여부 확인 및 생성
      READ TABLE GT_2001_NEW INTO GS_2001_NEW
                             WITH KEY PSPID = GS_2054_NEW-PSPID
                                      BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        PERFORM BAPI_BUS2001_CREATE USING GS_2001_NEW-PSPID
                                          GS_2001_NEW-POST1
                                          GS_2001_NEW-BUKRS
                                          GS_2001_NEW-GSBER
                                          GS_2001_NEW-PRCTR.
        IF GV_EXIT EQ GC_X.
          " 저장에 실패하였습니다.
          MESSAGE S008 DISPLAY LIKE GC_E.
          EXIT.
        ENDIF.
      ENDIF.
    ENDAT.


    GS_WBS-POSID = GS_2054_NEW-POSID.
    APPEND GS_WBS TO LT_WBS.


    AT END OF PSPID.
      PERFORM BAPI_BUS2054_CREATE TABLES LT_WBS
                                   USING GS_2054_NEW-PSPID.

      IF GV_EXIT EQ GC_X.
        " 저장에 실패하였습니다.
        MESSAGE S008 DISPLAY LIKE GC_E.
        EXIT.
      ENDIF.

      "-- 프로젝트 및 WBS 정상처리 사전점검
      PERFORM BAPI_PS_PRECOMMIT.

      IF GV_EXIT EQ GC_X.
        " 저장에 실패하였습니다.
        MESSAGE S008 DISPLAY LIKE GC_E.
        EXIT.
      ENDIF.

      IF PV_TEST EQ GC_X.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = GC_X.
      ENDIF.
    ENDAT.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_NEW_DATA
*&---------------------------------------------------------------------*
FORM MAKE_NEW_DATA .

  DATA LV_TABIX       LIKE SY-TABIX.


  REFRESH: GT_2001_NEW,
           GT_2054_NEW.

  " Error Check 초기화
  CLEAR GV_EXIT.


  LOOP AT GT_DISPLAY INTO GS_DISPLAY.

    LV_TABIX = SY-TABIX.

    IF GS_DISPLAY-STATUS NE ICON_YELLOW_LIGHT.
      " 이미 수행된 이력이 존재합니다.
      MESSAGE I000 DISPLAY LIKE 'W' WITH TEXT-E02.
      GV_EXIT = GC_X.
      EXIT.
    ENDIF.


    " 프로젝트 생성대상 추출( 중복제거 )
    IF GS_DISPLAY-PSPID_NEW EQ GC_X.
      GS_2001_NEW = VALUE #(
        PSPID  = GS_DISPLAY-PSPID
        POST1  = GS_DISPLAY-PSPIDTX
        BUKRS  = GS_DISPLAY-VBUKR
        GSBER  = GS_DISPLAY-VGSBR
        PRCTR  = GS_DISPLAY-PRCTR
      ).
      COLLECT GS_2001_NEW INTO GT_2001_NEW.
    ENDIF.


    " WBS 생성대상 추출
    GS_2054_NEW = VALUE #(
      PSPID  = GS_DISPLAY-PSPID
      POSID  = GS_DISPLAY-POSID
      TABIX  = LV_TABIX
    ).

    APPEND GS_2054_NEW TO GT_2054_NEW.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_T001
*&---------------------------------------------------------------------*
FORM GET_T001  USING PV_BUKRS.

  CHECK GS_T001-BUKRS NE GS_DISPLAY-VBUKR.

  CLEAR GS_T001.
  READ TABLE GT_T001 INTO GS_T001 WITH KEY BUKRS = PV_BUKRS
                                           BINARY SEARCH.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_WBS_SHORT_ID
*&---------------------------------------------------------------------*
FORM SET_WBS_SHORT_ID .

  DATA LS_PROJ    TYPE BAPI_PROJECT_DEFINITION.
  DATA LS_PROJ_UP TYPE BAPI_PROJECT_DEFINITION_UP.
  DATA LS_RETURN  TYPE BAPIRETURN1.
  DATA LT_METHOD  TYPE TABLE OF BAPI_METHOD_PROJECT     WITH HEADER LINE.
  DATA LT_WBS_UP  TYPE TABLE OF BAPI_WBS_ELEMENT_UPDATE WITH HEADER LINE.
  DATA LT_WBS     TYPE TABLE OF BAPI_WBS_ELEMENT        WITH HEADER LINE.
  DATA LT_MESSAGE TYPE TABLE OF BAPI_METH_MESSAGE.
  DATA LV_POSKI   TYPE PRPS-POSKI.
  DATA LV_LAST    TYPE C.
  DATA LV_SEQ     TYPE BAPI_METHOD_PROJECT-REFNUMBER.

  DATA: BEGIN OF LS_DATA,
          PSPID LIKE GS_DISPLAY-PSPID,
          POSID LIKE GS_DISPLAY-POSID,
        END OF LS_DATA,
        LT_DATA LIKE TABLE OF LS_DATA.


  LT_DATA = CORRESPONDING #( GT_DISPLAY ).
  SORT LT_DATA BY PSPID POSID.


  LOOP AT LT_DATA INTO LS_DATA.
    AT NEW PSPID.
      REFRESH : LT_METHOD,
                LT_WBS_UP,
                LT_WBS,
                LT_MESSAGE.

      CLEAR: LS_PROJ,
             LS_PROJ_UP,
             LS_RETURN,
             LV_POSKI,
             LV_LAST,
             LV_SEQ.
    ENDAT.

    LV_SEQ    = LV_SEQ + 1.
    LT_METHOD = VALUE #( REFNUMBER          = LV_SEQ
                         OBJECTTYPE         = CO1_TYPE-WBS_ELEMENT
                         METHOD             = CO1_METH-UPDATE
                         OBJECTKEY          = LS_DATA-POSID ).
    LT_WBS_UP = VALUE #( SHORT_ID           = GC_X ).
    LT_WBS    = VALUE #( WBS_ELEMENT        = LS_DATA-POSID
                         PROJECT_DEFINITION = LS_DATA-PSPID ).

    _CONVERSION_WBS_OUT LS_DATA-POSID.

    SPLIT LS_DATA-POSID AT '/' INTO TABLE DATA(LT_SPLIT).

    LOOP AT LT_SPLIT INTO DATA(LS_SPLIT).
      AT FIRST. LV_LAST = SPACE. ENDAT.
      AT LAST.  LV_LAST = GC_X.  ENDAT.
      IF LV_LAST EQ GC_X.
        REPLACE ALL OCCURRENCES OF '-' IN LS_SPLIT WITH SPACE.
      ENDIF.
      LV_POSKI = CONDENSE( COND #( WHEN LV_POSKI IS INITIAL
                                   THEN LS_SPLIT
                                   ELSE LV_POSKI && '/' && LS_SPLIT ) ).
    ENDLOOP.

    LT_WBS-SHORT_ID = LV_POSKI.

    APPEND LT_METHOD.
    APPEND LT_WBS_UP.
    APPEND LT_WBS.

    AT END OF PSPID.

      LT_METHOD = VALUE #( METHOD = 'SAVE' ).
      APPEND LT_METHOD.

      CALL FUNCTION 'BAPI_PS_INITIALIZATION'.
      CALL FUNCTION 'BAPI_PROJECT_MAINTAIN'
        EXPORTING
          I_PROJECT_DEFINITION         = LS_PROJ
          I_PROJECT_DEFINITION_UPD     = LS_PROJ_UP
        IMPORTING
          RETURN                       = LS_RETURN
        TABLES
          I_METHOD_PROJECT             = LT_METHOD
          I_WBS_ELEMENT_TABLE_UPDATE   = LT_WBS_UP
          I_WBS_ELEMENT_TABLE          = LT_WBS
*          I_WBS_MILESTONE_TABLE        =
*          I_WBS_MILESTONE_TABLE_UPDATE =
*          I_WBS_HIERARCHIE_TABLE       =
*          I_NETWORK                    =
*          I_NETWORK_UPDATE             =
*          I_ACTIVITY                   =
*          I_ACTIVITY_UPDATE            =
*          I_RELATION                   =
*          I_RELATION_UPDATE            =
          E_MESSAGE_TABLE              = LT_MESSAGE
*          I_ACTIVITY_ELEMENT           =
*          I_ACTIVITY_ELEMENT_UPDATE    =
*          I_ACTIVITY_MILESTONE         =
*          I_ACTIVITY_MILESTONE_UPDATE  =
        .

    ENDAT.
  ENDLOOP.



ENDFORM.

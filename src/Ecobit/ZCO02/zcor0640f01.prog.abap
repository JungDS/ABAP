*&---------------------------------------------------------------------*
*& Include          ZCOR0640F01
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

  DATA LV_DATUM TYPE SY-DATUM.

  LV_DATUM = SY-DATUM.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = LV_DATUM
      DAYS      = '00'
      MONTHS    = '01'
      SIGNUM    = '-'
      YEARS     = '00'
    IMPORTING
      CALC_DATE = LV_DATUM.

  P_GJAHR = LV_DATUM+0(4).
  P_PERDE = LV_DATUM+4(2).


  P_FILE = 'C:\'.


  " Selection Screen 텍스트
  TEXT_S01 = '실행조건'(S01).
  TEXT_S02 = '선택조건'(S02).
  SY-TITLE = '[CO] CO-PA 실적값 업로드'(T01).

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
    FUNCTXT_03 = VALUE SMP_DYNTXT( ICON_ID   = ICON_DETAIL
                                   ICON_TEXT = TEXT-S05 )
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

    WHEN 'FC03'.
      PERFORM SHOW_VALUE_FIELD.

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
  PERFORM SELECT_TKA01.
  PERFORM IMPORT_EXCEL_FILE.
  PERFORM MAKE_DISPLAY_DATA.
  PERFORM SELECT_OTHERS.
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
  LV_LABEL = '기간'(L03).
  LV_CONDI = |{ P_GJAHR } . { P_PERDE }|.
  PR_TABLE->NEW_ROW( ).
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

*  GR_ALV->SET_SORT( IT_FIELD = VALUE #( ( '' )
*                                        ( '' )
*                                        ) ).

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
    LS_FIELDCAT-COL_OPT = GC_X.

*-- 열고정
*    LS_FIELDCAT-FIX_COLUMN = LV_KEY_FIX.
*    LS_FIELDCAT-KEY =


*-- Field 속성
    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'STATUS'.
        LV_TEXT               = '상태'.
        LS_FIELDCAT-JUST      = GC_C.
        LS_FIELDCAT-ICON      = GC_X.

      WHEN 'BUKRS'.
        LV_TEXT               = '회사코드'.
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'BUTXT'.
        LV_TEXT               = '회사이름'.

      WHEN 'WW120'.
        LV_TEXT               = 'BU'.
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'BEZEK'.
        LV_TEXT               = 'BU명'.

      WHEN 'PSPNR'.
        LS_FIELDCAT-NO_OUT    = GC_X.

      WHEN 'POSID'.
        LV_TEXT               = 'WBS'.
        LS_FIELDCAT-KEY       = GC_X.
        LS_FIELDCAT-HOTSPOT   = GC_X.

      WHEN 'POST1'.
        LV_TEXT               = 'WBS명'.

      WHEN 'VRGAR'.
        LV_TEXT               = '레코드유형'.
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'VRGARX'.
        LV_TEXT               = '레코드유형명'.

      WHEN 'VALFD'.
        LV_TEXT               = '값필드'.
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'REPTX'.
        LV_TEXT               = '값필드명'.

      WHEN 'WKGXX'.
        LV_TEXT               = '업로드값'.
        LS_FIELDCAT-CURRENCY  = GV_WAERS.

      WHEN 'BELNR'.
        LV_TEXT               = '전표번호'.
        LS_FIELDCAT-HOTSPOT   = GC_X.
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
      I_BEGIN_ROW             = 3          " 조회시작행
      I_BEGIN_COL             = 1          " 조회시작열
*      I_END_ROW               =           " 조회종료행
      I_END_COL               = 6          " 조회종료열
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

  DATA LV_AMOUNT TYPE BAPICURR-BAPICURR.

  LOOP AT GT_EXCEL INTO GS_EXCEL.

    REPLACE ALL OCCURRENCES OF ',' IN GS_EXCEL-WKGXX WITH SPACE.

    TRY.
      CLEAR GS_DISPLAY.
      GS_DISPLAY = CORRESPONDING #( GS_EXCEL ).
      GS_DISPLAY-STATUS = ICON_YELLOW_LIGHT.
    CATCH CX_ROOT.
      GS_DISPLAY-STATUS = ICON_RED_LIGHT.
    ENDTRY.


    LV_AMOUNT = GS_DISPLAY-WKGXX.

    CALL FUNCTION 'CURRENCY_AMOUNT_BAPI_TO_SAP'
      EXPORTING
        CURRENCY              = GV_WAERS
        BAPI_AMOUNT           = LV_AMOUNT
      IMPORTING
        SAP_AMOUNT            = LV_AMOUNT
      EXCEPTIONS
        BAPI_AMOUNT_INCORRECT = 1
        OTHERS                = 2.

    IF SY-SUBRC EQ 0.
      GS_DISPLAY-WKGXX = LV_AMOUNT.
    ENDIF.

    APPEND GS_DISPLAY TO GT_DISPLAY.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form VALID_DATA
*&---------------------------------------------------------------------*
FORM VALID_DATA .

  CHECK GT_DISPLAY[] IS NOT INITIAL.


  LOOP AT GT_DISPLAY INTO GS_DISPLAY.

    PERFORM CHECK_BUKRS.        " 회사코드
    PERFORM CHECK_WW120.        " BU
    PERFORM CHECK_POSID.        " WBS
    PERFORM CHECK_WW120_POSID.  " BU/WBS 점검
    PERFORM CHECK_VRGAR.        " 레코드 유형
    PERFORM CHECK_VALFD.        " 값필드

    MODIFY GT_DISPLAY FROM GS_DISPLAY.
  ENDLOOP.


  " 중복점검
  SELECT BUKRS, WW120, POSID, VRGAR, VALFD, COUNT(*) AS COUNT
    FROM @GT_DISPLAY AS A
   GROUP BY BUKRS, WW120, POSID, VRGAR, VALFD HAVING COUNT(*) > 1
    INTO TABLE @DATA(LT_CHECK).

  IF SY-SUBRC EQ 0.
    LOOP AT LT_CHECK INTO DATA(LS_CHECK).
      LOOP AT GT_DISPLAY INTO GS_DISPLAY WHERE BUKRS EQ LS_CHECK-BUKRS
                                           AND WW120 EQ LS_CHECK-WW120
                                           AND POSID EQ LS_CHECK-POSID
                                           AND VRGAR EQ LS_CHECK-VRGAR
                                           AND VALFD EQ LS_CHECK-VALFD.
        __ERROR '동일한 라인이 엑셀파일 내에 존재합니다.'.
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

  REFRESH:
    GT_EXCEL   ,
    GT_DISPLAY ,
    GT_T001    ,
    GT_PRPS    .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_OTHERS
*&---------------------------------------------------------------------*
FORM SELECT_OTHERS .

  " 회사코드
  SELECT DISTINCT A~BUKRS, A~BUTXT
    FROM T001     AS A
    JOIN TKA02    AS B ON A~BUKRS EQ B~BUKRS
   WHERE B~KOKRS  EQ @P_KOKRS
    INTO TABLE @GT_T001.

  SORT GT_T001 BY BUKRS.


  " BU
  SELECT WW120, BEZEK
    FROM T25A1
   WHERE SPRAS EQ @SY-LANGU
    INTO TABLE @GT_T25A1.

  SORT GT_T25A1 BY WW120.


  " WBS
  IF GT_DISPLAY[] IS NOT INITIAL.
    SELECT PSPNR,
           POSID,
           POST1,
           POSID_EDIT
      FROM PRPS
       FOR ALL ENTRIES IN @GT_DISPLAY
     WHERE POSID_EDIT EQ @GT_DISPLAY-POSID
      INTO TABLE @GT_PRPS.
  ENDIF.

  SORT GT_PRPS BY POSID_EDIT.


  " 레코드 유형
  SELECT VRGAR, VRGARX
    FROM TVGAT
   WHERE SPRAS EQ @SY-LANGU
    INTO TABLE @GT_TVGAT.

  SORT GT_TVGAT BY VRGAR.


  " 값필드

  SELECT A~FIELDNAME,
         B~REPTEXT
    FROM DD03L        AS A
    JOIN DD04T        AS B ON B~ROLLNAME EQ A~ROLLNAME
   WHERE A~TABNAME    EQ @GC_TABNAME   " CE11000
     AND A~AS4LOCAL   EQ @GC_A
     AND B~DDLANGUAGE EQ @SY-LANGU
     AND B~AS4LOCAL   EQ @GC_A
    INTO TABLE @GT_DD03L.

  SORT GT_DD03L BY FIELDNAME.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_BUKRS
*&---------------------------------------------------------------------*
FORM CHECK_BUKRS .


  IF GS_DISPLAY-BUKRS IS INITIAL.

    __ERROR '[회사코드]는 필수입력입니다.'.

  ELSEIF GS_DISPLAY-BUKRS NE GS_T001-BUKRS.

    READ TABLE GT_T001 INTO GS_T001
                       WITH KEY BUKRS = GS_DISPLAY-BUKRS
                                BINARY SEARCH.

    IF SY-SUBRC EQ 0.
      GS_DISPLAY-BUTXT = GS_T001-BUTXT.
    ELSE.
      CLEAR GS_T001.
      __ERROR '[회사코드] 가 존재하지 않습니다.'.
    ENDIF.

  ELSE.

    GS_DISPLAY-BUTXT = GS_T001-BUTXT.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_WW120
*&---------------------------------------------------------------------*
FORM CHECK_WW120 .


  CHECK GS_DISPLAY-WW120 IS NOT INITIAL.

  IF GS_DISPLAY-WW120 NE GS_T25A1-WW120.
    READ TABLE GT_T25A1 INTO GS_T25A1
                        WITH KEY WW120 = GS_DISPLAY-WW120
                                 BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DISPLAY-BEZEK = GS_T25A1-BEZEK.
    ELSE.
      CLEAR GS_T25A1.
      __ERROR '[BU] 가 존재하지 않습니다.'.
    ENDIF.
  ELSE.
    GS_DISPLAY-BEZEK = GS_T25A1-BEZEK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_POSID
*&---------------------------------------------------------------------*
FORM CHECK_POSID .

  CHECK GS_DISPLAY-POSID IS NOT INITIAL.

  IF GS_DISPLAY-POSID NE GS_PRPS-POSID_EDIT.

    READ TABLE GT_PRPS INTO GS_PRPS
                       WITH KEY POSID_EDIT = GS_DISPLAY-POSID
                                BINARY SEARCH.
    IF SY-SUBRC EQ 0.

      GS_DISPLAY-PSPNR = GS_PRPS-PSPNR.
      GS_DISPLAY-POST1 = GS_PRPS-POST1.

    ELSE.

      CLEAR GS_PRPS.
      __ERROR '[WBS] 가 존재하지 않습니다.'.

    ENDIF.
  ELSE.

    GS_DISPLAY-PSPNR = GS_PRPS-PSPNR.
    GS_DISPLAY-POST1 = GS_PRPS-POST1.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_WW120_POSID
*&---------------------------------------------------------------------*
FORM CHECK_WW120_POSID .

  IF ( GS_DISPLAY-WW120 EQ SPACE AND GS_DISPLAY-POSID EQ SPACE ) OR
     ( GS_DISPLAY-WW120 NE SPACE AND GS_DISPLAY-POSID NE SPACE ).

    __ERROR '[BU]와 [WBS] 중 하나의 필드만 필수입력입니다.'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_VRGAR
*&---------------------------------------------------------------------*
FORM CHECK_VRGAR .

  CASE GS_DISPLAY-VRGAR.

    WHEN SPACE.

      __ERROR '[레코드유형]은 필수입력입니다.'.

    WHEN GS_TVGAT-VRGAR.

      GS_DISPLAY-VRGARX = GS_TVGAT-VRGARX.

    WHEN OTHERS.

      READ TABLE GT_TVGAT INTO GS_TVGAT
                          WITH KEY VRGAR = GS_DISPLAY-VRGAR
                                   BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        GS_DISPLAY-VRGARX = GS_TVGAT-VRGARX.

      ELSE.
        __ERROR '[레코드유형] 이 존재하지 않습니다.'.

      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_VALFD
*&---------------------------------------------------------------------*
FORM CHECK_VALFD .

  IF GS_DISPLAY-VALFD IS INITIAL.

    __ERROR '[값필드]는 필수입력입니다.'.

  ELSE.

    READ TABLE GT_DD03L INTO GS_DD03L
                        WITH KEY FIELDNAME = GS_DISPLAY-VALFD
                                 BINARY SEARCH.
    IF SY-SUBRC EQ 0.

      GS_DISPLAY-REPTX = GS_DD03L-REPTEXT.

    ELSE.

      __ERROR '[값필드] 가 존재하지 않습니다.'.

    ENDIF.

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
    MESSAGE I000 DISPLAY LIKE 'E' WITH TEXT-E01.
    EXIT.
  ENDIF.


  " CO-PA실적값 업로드 작업을 수행하시겠습니까?
  CHECK GC_X EQ ZCL_CO_COMMON=>POPUP_CONFIRM( CONV #( TEXT-M01 ) ).


  PERFORM EXECUTE_BAPI USING GC_X.
  IF GV_EXIT EQ GC_X.
    MESSAGE I000 DISPLAY LIKE 'E' WITH TEXT-E01.
  ENDIF.
  PERFORM EXECUTE_BAPI USING SPACE.

  CHECK GV_EXIT IS INITIAL.

  "Success
*  PERFORM UPDATE_DISPLAY_DATA.

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

        WHEN 'BELNR'.

          SET PARAMETER ID 'ERB' FIELD GV_ERKRS.
          SET PARAMETER ID 'BCO' FIELD <FS_VALUE>.
          SET PARAMETER ID 'GFP' FIELD P_PERDE.
          SET PARAMETER ID 'GJR' FIELD P_GJAHR.
          SET PARAMETER ID 'VGA' FIELD GS_DISPLAY-VRGAR.

          TRY.
            CALL TRANSACTION 'KE23N' AND SKIP FIRST SCREEN.

          CATCH CX_ROOT.

          ENDTRY.

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

  IF GR_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_EVENT_RECEIVER.
  ENDIF.

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
      GS_DISPLAY-STATUS     = ICON_GREEN_LIGHT.
    ENDIF.
    GS_DISPLAY-MESSAGE      = '등록완료'.
    GS_DISPLAY-MESSAGE_TAB  = VALUE #( ( '등록완료' ) ).

    MODIFY GT_DISPLAY FROM GS_DISPLAY.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXECUTE_BAPI
*&---------------------------------------------------------------------*
FORM EXECUTE_BAPI  USING PV_TEST.

  CHECK GV_EXIT IS INITIAL.


  DATA: LT_INPUT        TYPE TABLE OF BAPI_COPA_DATA,
        LT_FIELD        TYPE TABLE OF BAPI_COPA_FIELD,
        LT_RETURN       TYPE TABLE OF BAPIRET2 WITH HEADER LINE,
        LV_RECORD_ID    TYPE BAPI_COPA_DATA-RECORD_ID,
        LV_AMOUNT       TYPE BAPICURR-BAPICURR.

  DATA: LV_PERIO        TYPE CE11000-PERIO,
        LV_VALUE_WHERE  TYPE STRING.

  FIELD-SYMBOLS <FS_INPUT> TYPE BAPI_COPA_DATA.
  FIELD-SYMBOLS <FS_FIELD> TYPE BAPI_COPA_FIELD.

DEFINE __SET_INPUT_FIELD.

  APPEND INITIAL LINE TO LT_INPUT ASSIGNING <FS_INPUT>.
  APPEND INITIAL LINE TO LT_FIELD ASSIGNING <FS_FIELD>.

  <FS_INPUT>-RECORD_ID = LV_RECORD_ID.  " 데이타레코드번호
  <FS_INPUT>-FIELDNAME = &1.            " CO-PA: 필드명
  <FS_INPUT>-VALUE     = &2.            " 매개변수 값
  <FS_FIELD>-FIELDNAME = &1.

END-OF-DEFINITION.


  LV_PERIO = P_GJAHR && P_PERDE.


  LOOP AT GT_DISPLAY INTO GS_DISPLAY.

    REFRESH: LT_INPUT,
             LT_FIELD,
             LT_RETURN.

    LV_RECORD_ID = 1.

    __SET_INPUT_FIELD: 'KOKRS' P_KOKRS,
                       'PERIO' LV_PERIO,
                       'GJAHR' P_GJAHR,
                       'PERDE' P_PERDE,
                       'BUDAT' SY-DATUM,
                       'BUKRS' GS_DISPLAY-BUKRS,
                       'VRGAR' GS_DISPLAY-VRGAR.


    IF GS_DISPLAY-WW120 IS NOT INITIAL.
      __SET_INPUT_FIELD 'WW120' GS_DISPLAY-WW120.
    ELSE.
      __SET_INPUT_FIELD 'PSPNR' GS_DISPLAY-PSPNR.
    ENDIF.

    LV_AMOUNT = GS_DISPLAY-WKGXX.

    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        CURRENCY    = GV_WAERS
        SAP_AMOUNT  = LV_AMOUNT
      IMPORTING
        BAPI_AMOUNT = LV_AMOUNT.

    __SET_INPUT_FIELD: GS_DISPLAY-VALFD LV_AMOUNT.
    <FS_INPUT>-CURRENCY = GV_WAERS. " 데이터 레코드 통화

    CALL FUNCTION 'BAPI_COPAACTUALS_POSTCOSTDATA'
      EXPORTING
        OPERATINGCONCERN = GV_ERKRS  " Operating Concern
        TESTRUN          = PV_TEST   " Switch to Simulation Session
      TABLES
        INPUTDATA        = LT_INPUT  " Input: Data to be Posted
        FIELDLIST        = LT_FIELD  " Input: Field List for Data Param
        RETURN           = LT_RETURN " Output: Messages
      .


    READ TABLE LT_RETURN WITH KEY TYPE = GC_E.

    IF SY-SUBRC EQ 0.

      GV_EXIT = GC_X.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      GS_DISPLAY-STATUS  = ICON_RED_LIGHT.
      GS_DISPLAY-MESSAGE = LT_RETURN-MESSAGE.

      LOOP AT LT_RETURN.
        APPEND LT_RETURN-MESSAGE TO GS_DISPLAY-MESSAGE_TAB.
      ENDLOOP.

      MODIFY GT_DISPLAY FROM GS_DISPLAY.

    ELSE.

      IF PV_TEST EQ GC_X.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        GS_DISPLAY-MESSAGE = '기표준비 완료'.
        MODIFY GT_DISPLAY FROM GS_DISPLAY.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = GC_X.

        LV_VALUE_WHERE = |{ GS_DISPLAY-VALFD } EQ '{ GS_DISPLAY-WKGXX }'|.

        SELECT SINGLE BELNR
          FROM CE11000
         WHERE VRGAR EQ @GS_DISPLAY-VRGAR
           AND VERSI EQ @SPACE
           AND PERIO EQ @LV_PERIO
           AND GJAHR EQ @P_GJAHR
           AND PERDE EQ @P_PERDE
           AND HZDAT EQ @SY-DATUM
           AND USNAM EQ @SY-UNAME
           AND (LV_VALUE_WHERE)
          INTO @GS_DISPLAY-BELNR.

        GS_DISPLAY-STATUS  = ICON_GREEN_LIGHT.
        GS_DISPLAY-MESSAGE = '개별항목생성완료'.
        MODIFY GT_DISPLAY FROM GS_DISPLAY.

      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_TKA01
*&---------------------------------------------------------------------*
FORM SELECT_TKA01 .

  " 관리회계영역 통화
  SELECT SINGLE WAERS, ERKRS
    FROM TKA01
   WHERE KOKRS EQ @P_KOKRS
    INTO ( @GV_WAERS, @GV_ERKRS ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_VALUE_FIELD
*&---------------------------------------------------------------------*
FORM SHOW_VALUE_FIELD .

  PERFORM SELECT_TKEF.

  PERFORM FREE_DIALOGBOX_2.
  PERFORM CREATE_DIALOGBOX_2.
  PERFORM CREATE_DIALOGALV_2.
  PERFORM SHOW_DIALOGALV_2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_TKEF
*&---------------------------------------------------------------------*
FORM SELECT_TKEF .

  REFRESH GT_VALUE.

  SELECT A~FIENM,
         B~DATATYPE,
         C~REPTEXT
    FROM TKEF   AS A
    JOIN DD04L  AS B ON B~ROLLNAME    EQ A~ROLNM
                    AND B~AS4LOCAL    EQ @GC_A
                    AND B~DATATYPE    IN ('CURR','QUAN')
    LEFT
    JOIN DD04T  AS C ON C~ROLLNAME    EQ B~ROLLNAME
                    AND C~DDLANGUAGE  EQ @SY-LANGU
                    AND C~AS4LOCAL    EQ @GC_A
   WHERE A~FIENM LIKE 'VV%'
    INTO TABLE @GT_VALUE.

  SORT GT_VALUE BY DATATYPE FIENM.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FREE_DIALOGBOX_2
*&---------------------------------------------------------------------*
FORM FREE_DIALOGBOX_2 .

  IF GR_ALV_DIALOG_2 IS NOT INITIAL.
    IF GR_ALV_DIALOG_2->MR_ALV_GRID IS BOUND.
      GR_ALV_DIALOG_2->MR_ALV_GRID->FREE(
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          OTHERS            = 3 ).
    ENDIF.

    IF GR_ALV_DIALOG_2->MR_CONTAINER IS BOUND.
      GR_ALV_DIALOG_2->MR_CONTAINER->FREE(
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          OTHERS            = 3 ).
    ENDIF.

    FREE GR_ALV_DIALOG_2.
  ENDIF.

  IF GR_CON_DIALOG_2 IS NOT INITIAL.
    IF GR_CON_DIALOG_2 IS BOUND.
      GR_CON_DIALOG_2->FREE(
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          OTHERS            = 3
      ).
    ENDIF.

    FREE GR_CON_DIALOG_2.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_DIALOGBOX_2
*&---------------------------------------------------------------------*
FORM CREATE_DIALOGBOX_2 .

  CREATE OBJECT GR_CON_DIALOG_2
    EXPORTING
      WIDTH   = 600              " Width of This Container
      HEIGHT  = 300              " Height of This Container
      TOP     = 100              " Top Position of Dialog Box
      LEFT    = 300              " Left Position of Dialog Box
      CAPTION = '값필드 리스트'  " Dialog Box Caption
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

  IF GR_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_EVENT_RECEIVER.
  ENDIF.

  SET HANDLER GR_EVENT_RECEIVER->ON_CLOSE FOR GR_CON_DIALOG_2.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_DIALOGALV_2
*&---------------------------------------------------------------------*
FORM CREATE_DIALOGALV_2 .

  CREATE OBJECT GR_ALV_DIALOG_2
    EXPORTING
      I_CONTAINER = GR_CON_DIALOG_2.

  GR_ALV_DIALOG_2->SET_FIELD_CATALOG(
    EXPORTING
      " ABAP Dictionary 의 Table/View/Structure
      I_TABNAME               = 'GS_VALUE'
    EXCEPTIONS
      INVALID_INPUT_PARAMETER = 1
      EMPTY_FIELD_CATALOG     = 2
      OTHERS                  = 3
  ).


  GR_ALV_DIALOG_2->SET_LAYOUT( I_TYPE = GC_A ).

*  GR_ALV_DIALOG_2->SET_LAYOUT(
*    EXPORTING
*      I_LAYOUT = VALUE #( CWIDTH_OPT  = GC_X
*                          NO_TOOLBAR  = GC_X )
*  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_DIALOGALV_2
*&---------------------------------------------------------------------*
FORM SHOW_DIALOGALV_2 .

  GR_ALV_DIALOG_2->DISPLAY(
    CHANGING
      T_OUTTAB = GT_VALUE[]
  ).

ENDFORM.

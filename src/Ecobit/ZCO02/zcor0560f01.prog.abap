*&---------------------------------------------------------------------*
*& Include          ZCOR0560F01
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

  " 기본값 : 년도/월
  P_GJAHR     = LV_DATUM(4).
  S_POPER-LOW = LV_DATUM+4(2).
  APPEND S_POPER.

  " 기본값 : 회사코드
*  S_BUKRS      = 'IEQ'.
  S_BUKRS-LOW  = ZCL_CO_COMMON=>GET_DEFAULT_BUKRS( ).
  IF S_BUKRS-LOW IS INITIAL.
    S_BUKRS-LOW = '1100'.
  ENDIF.
  APPEND S_BUKRS.

  " Selection Screen 텍스트
  TEXT_S01 = '실행조건'(S01).
  TEXT_S02 = 'WBS 선택'(S02).
  TEXT_S03 = '실적/계획 선택'(S03).
  TEXT_S04 = '조회조건'(S04).
  TEXT_S06 = '계획'(S06).
  TEXT_S07 = '계획버젼'(S07).


  " 기본값 : 조회월 범위
  GV_MONTH_MIN = '999'.
  GV_MONTH_MAX = '000'.

  P_NOZERO = GC_X.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM SCRFIELDS_FUNCTXT .

  CLEAR GS_FUNTXT.
  GS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  GS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = GS_FUNTXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_SCR
*&---------------------------------------------------------------------*
FORM MODIFY_SCR .

  LOOP AT SCREEN.
    CASE SCREEN-NAME.
      WHEN 'P_KOKRS'.
        SCREEN-INPUT = 0.
      WHEN 'P_VERSN'.
        IF P_PLN IS INITIAL.
          SCREEN-INPUT = '0'.
        ENDIF.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

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
    WHEN 'UC_VT'.

      IF P_ACT EQ GC_X.
        CLEAR P_VERSN.
      ENDIF.


    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_BLOCK_B03
*&---------------------------------------------------------------------*
FORM CHECK_BLOCK_B03.

  CHECK SSCRFIELDS-UCOMM EQ 'ONLI'.
  CHECK P_PLN EQ GC_X.


  IF P_VERSN IS INITIAL.
    " & 필드는 필수입니다.
    MESSAGE E026 WITH '계획버젼'(S07).
  ENDIF.


  SELECT COUNT(*)
    FROM TKA09
   WHERE KOKRS    EQ @P_KOKRS
     AND VERSN    EQ @P_VERSN
     AND PLANNING EQ @GC_X.

  IF SY-SUBRC NE 0.
    " & 필드값이 유효하지 않습니다.
    MESSAGE E027 WITH '계획버젼'(S07).
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  SET PARAMETER ID 'BUK' FIELD S_BUKRS-LOW.

  PERFORM CLEAR_ITAB.
  PERFORM SELECT_TKA01.
  PERFORM SELECT_WBS.
  PERFORM SELECT_COSX USING 'COSP'.
  PERFORM SELECT_COSX USING 'COSS'.

  PERFORM MAKE_DISPLAY_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR_ITAB
*&---------------------------------------------------------------------*
FORM CLEAR_ITAB .

  REFRESH: GT_DATA,
           GT_DISPLAY,
           GT_COSP,
           GT_COSS.

  CLEAR GS_TKA01.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_TKA01
*&---------------------------------------------------------------------*
FORM SELECT_TKA01 .

  CLEAR GS_TKA01.

  SELECT SINGLE
         KOKRS,
         BEZEI,
         WAERS
    FROM TKA01
   WHERE KOKRS EQ @P_KOKRS
    INTO CORRESPONDING FIELDS OF @GS_TKA01.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_WBS
*&---------------------------------------------------------------------*
FORM SELECT_WBS .

  DATA LR_PROFL TYPE RANGE OF PROJ-PROFL.

  CASE GC_X.
    WHEN P_WBS_P.
      LR_PROFL = VALUE #( ( SIGN = 'E' OPTION = 'EQ' LOW = 'Z000003' ) ).
    WHEN P_WBS_E.
      LR_PROFL = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = 'Z000003' ) ).
    WHEN P_WBS_A.
  ENDCASE.


  " WBS 정보 추출

  SELECT B~PBUKR,
         C~BUTXT,
         A~PROFL,
         B~PSPNR,
         B~POSID,
         B~POST1,
         B~OBJNR
    FROM PROJ AS A
    JOIN PRPS AS B ON B~PSPHI EQ A~PSPNR
    JOIN T001 AS C ON C~BUKRS EQ B~PBUKR
   WHERE A~PROFL IN @LR_PROFL
     AND A~LOEVM IS INITIAL
     AND B~PKOKR EQ @P_KOKRS
     AND B~PBUKR IN @S_BUKRS
     AND B~LOEVM IS INITIAL
    INTO TABLE @GT_DATA.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_COSP
*&---------------------------------------------------------------------*
FORM SELECT_COSX USING VALUE(PV_TABNAME) TYPE TABNAME.

  DATA: LT_SELECT_FIELDS TYPE TABLE OF FIELDNAME,
        LR_OBJNR         TYPE RANGE OF COSP-OBJNR WITH HEADER LINE,
        LR_WRTTP         TYPE RANGE OF COSP-WRTTP,
        LR_VERSN         TYPE RANGE OF COSP-VERSN,
        LR_KSTAR         TYPE RANGE OF COSP-KSTAR,
        LV_TABNAME       TYPE TABNAME.

  FIELD-SYMBOLS <FS_COSX> TYPE STANDARD TABLE.
  FIELD-SYMBOLS <FS_LINE> TYPE FIELDNAME.

  CASE PV_TABNAME.
    WHEN 'COSP'.  ASSIGN GT_COSP TO <FS_COSX>.  CHECK SY-SUBRC EQ 0.
    WHEN 'COSS'.  ASSIGN GT_COSS TO <FS_COSX>.  CHECK SY-SUBRC EQ 0.
    WHEN OTHERS. EXIT.
  ENDCASE.

  CHECK GT_DATA[] IS NOT INITIAL.
  LOOP AT GT_DATA INTO GS_DATA.
    LR_OBJNR  = VALUE #(
      SIGN    = 'I'
      OPTION  = 'EQ'
      LOW     = GS_DATA-OBJNR
    ).

    APPEND LR_OBJNR.
  ENDLOOP.


  CASE GC_X.
    WHEN P_ACT.   LR_WRTTP = VALUE #( ( CONV #( 'IEQ04' ) ) ).
    WHEN P_PLN.   LR_WRTTP = VALUE #( ( CONV #( 'IEQ01' ) ) ).
                  LR_VERSN = VALUE #( ( CONV #( 'IEQ' && P_VERSN ) ) ).
  ENDCASE.


  SELECT RIGHT( FIELDNAME, 3 ) AS MONTH,
         FIELDNAME
    FROM DD03L
   WHERE TABNAME   EQ @PV_TABNAME
     AND FIELDNAME LIKE 'WKG0__'
   ORDER BY MONTH
    INTO TABLE @DATA(LT_DD03L).

  CHECK SY-SUBRC EQ 0.

  APPEND ' OBJNR' TO LT_SELECT_FIELDS.
  APPEND ',VRGNG' TO LT_SELECT_FIELDS.
  APPEND ',BEKNZ' TO LT_SELECT_FIELDS.

  " 특정월만 조회대상 필드로 선정
  APPEND ', SUM( CAST( ( ' TO LT_SELECT_FIELDS.
  LOOP AT LT_DD03L INTO DATA(LS_DD03L) WHERE MONTH IN S_POPER.
    CONCATENATE LS_DD03L-FIELDNAME '+'
           INTO LS_DD03L-FIELDNAME SEPARATED BY SPACE.

    APPEND LS_DD03L-FIELDNAME TO LT_SELECT_FIELDS ASSIGNING <FS_LINE>.

    IF GV_MONTH_MIN > LS_DD03L-MONTH.
      GV_MONTH_MIN = LS_DD03L-MONTH.
    ENDIF.

    IF GV_MONTH_MAX < LS_DD03L-MONTH.
      GV_MONTH_MAX = LS_DD03L-MONTH.
    ENDIF.
  ENDLOOP.

  CHECK <FS_LINE> IS ASSIGNED.
  REPLACE ' +' IN <FS_LINE> WITH SPACE.
  APPEND ' ) AS DEC( 31, 2 ) ) )' TO LT_SELECT_FIELDS.
  APPEND ' AS WKGSUM' TO LT_SELECT_FIELDS.




  IF P_EXCLDM EQ GC_X.
    LR_KSTAR = VALUE #(
      ( SIGN = 'E' OPTION = 'EQ' LOW = '0504101003' ) " 구매가격차이-원재료
      ( SIGN = 'E' OPTION = 'EQ' LOW = '0504101013' ) " 가격변경차이-원재료
      ( SIGN = 'E' OPTION = 'BT' LOW = '0505000000' HIGH = '0505999999' ) " 제조 관련 계정
    ).
  ENDIF.


  SELECT (LT_SELECT_FIELDS)
    FROM (PV_TABNAME)
   WHERE LEDNR EQ '00'
     AND OBJNR IN @LR_OBJNR
     AND GJAHR EQ @P_GJAHR
     AND WRTTP IN @LR_WRTTP
     AND VERSN IN @LR_VERSN
     AND KSTAR IN @LR_KSTAR
     AND VRGNG NE 'SDOR'
   GROUP BY OBJNR, VRGNG, BEKNZ
    INTO CORRESPONDING FIELDS OF TABLE @<FS_COSX>.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM MAKE_DISPLAY_DATA .

  SORT GT_DATA BY BUKRS POSID.
  SORT GT_COSP BY OBJNR VRGNG.
  SORT GT_COSS BY OBJNR VRGNG.

  LOOP AT GT_DATA INTO GS_DATA.

    GS_DISPLAY = CORRESPONDING #( GS_DATA ).
    GS_DISPLAY-STATUS = ICON_LED_GREEN.

    PERFORM SET_AMOUNT TABLES GT_COSP.
    PERFORM SET_AMOUNT TABLES GT_COSS.


    IF GS_DISPLAY-AMT_P     EQ 0 AND  " 기표금액 = 0
       GS_DISPLAY-AMT_D     EQ 0 AND  " 배부금액 = 0
       GS_DISPLAY-AMT_S     EQ 0 AND  " 정산금액 = 0
       GS_DISPLAY-AMT_B     EQ 0 AND  " 잔고금액 = 0
       GS_DISPLAY-AMT_CHECK EQ 0.     " 차변금액 = 0

      CONTINUE. " 제외

    ENDIF.


    IF GS_DISPLAY-AMT_B NE 0.

      GS_DISPLAY-STATUS   = ICON_LED_RED.
      GS_DISPLAY-MESSAGE  = '남은 잔고가 존재합니다.'(E01).

    ELSEIF P_NOZERO EQ 'X'. " 잔고 '0' 이 아닌 것만 조회 조건인 경우

      CONTINUE. " 제외

    ENDIF.

    APPEND GS_DISPLAY TO GT_DISPLAY.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_AMOUNT
*&---------------------------------------------------------------------*
FORM SET_AMOUNT  TABLES PT_COSX .

  FIELD-SYMBOLS: <FS_WA>,
                 <FS_OBJNR>,
                 <FS_VRGNG>,
                 <FS_BEKNZ>,
                 <FS_WKGSUM>.


  READ TABLE PT_COSX ASSIGNING <FS_WA>
                     WITH KEY ('OBJNR') = GS_DATA-OBJNR
                              BINARY SEARCH.
  CHECK SY-SUBRC EQ 0.


  " 필드 점검
  ASSIGN COMPONENT 'VRGNG'  OF STRUCTURE <FS_WA> TO <FS_VRGNG>.
  CHECK SY-SUBRC EQ 0.
  UNASSIGN <FS_VRGNG>.

  ASSIGN COMPONENT 'BEKNZ'  OF STRUCTURE <FS_WA> TO <FS_BEKNZ>.
  CHECK SY-SUBRC EQ 0.
  UNASSIGN <FS_BEKNZ>.

  ASSIGN COMPONENT 'WKGSUM' OF STRUCTURE <FS_WA> TO <FS_WKGSUM>.
  CHECK SY-SUBRC EQ 0.
  UNASSIGN <FS_WKGSUM>.



  LOOP AT PT_COSX ASSIGNING <FS_WA> FROM SY-TABIX.

    ASSIGN COMPONENT 'OBJNR' OF STRUCTURE <FS_WA> TO <FS_OBJNR>.
    IF <FS_OBJNR> EQ GS_DATA-OBJNR.
      UNASSIGN <FS_OBJNR>.
    ELSE.
      EXIT.
    ENDIF.

    ASSIGN COMPONENT 'VRGNG'  OF STRUCTURE <FS_WA> TO <FS_VRGNG>.
    ASSIGN COMPONENT 'BEKNZ'  OF STRUCTURE <FS_WA> TO <FS_BEKNZ>.
    ASSIGN COMPONENT 'WKGSUM' OF STRUCTURE <FS_WA> TO <FS_WKGSUM>.


    " 차변금액( BEKNZ(차/대) = 'S(D)' )
    IF <FS_BEKNZ> EQ GC_S.
      ADD <FS_WKGSUM> TO GS_DISPLAY-AMT_CHECK.
    ENDIF.

    ADD <FS_WKGSUM> TO GS_DISPLAY-AMT_B.
    ADD <FS_WKGSUM> TO GS_DISPLAY-AMT_P.

    CASE <FS_VRGNG>.
      WHEN 'KOAO'
        OR 'KOAP'. " 정산
        ADD       <FS_WKGSUM> TO   GS_DISPLAY-AMT_S.
        SUBTRACT  <FS_WKGSUM> FROM GS_DISPLAY-AMT_P.

      WHEN 'RKIV'
        OR 'RKPV'
        OR 'KSPA'
        OR 'KSPB'. " 배부

        ADD       <FS_WKGSUM> TO   GS_DISPLAY-AMT_D.
        SUBTRACT  <FS_WKGSUM> FROM GS_DISPLAY-AMT_P.
      WHEN OTHERS.
    ENDCASE.

    UNASSIGN <FS_VRGNG>.
    UNASSIGN <FS_WKGSUM>.
  ENDLOOP.
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
      PARENT             = GR_CON_TOP          " Contain Object Already Exists
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1                " Error Displaying the Document in the HTML Control
      OTHERS             = 2
  ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form WRITE_DOCUMENT
*&---------------------------------------------------------------------*
FORM WRITE_DOCUMENT .

  DATA LR_TABLE   TYPE REF TO CL_DD_TABLE_ELEMENT.
  DATA LR_COLUMN1 TYPE REF TO CL_DD_AREA.
  DATA LR_COLUMN2 TYPE REF TO CL_DD_AREA.

  PERFORM ADD_TABLE  USING LR_TABLE 2 '100%'.
  PERFORM ADD_COLUMN USING LR_TABLE LR_COLUMN1 '150px'.
  PERFORM ADD_COLUMN USING LR_TABLE LR_COLUMN2 '*'.


  DATA LV_TEXT1   TYPE TEXT255.
  DATA LV_TEXT2   TYPE TEXT255.

*--------------------------------------------------------------------*
  LV_TEXT1 = '관리회계영역 : '(L01).
  LV_TEXT2 = P_KOKRS.

  LR_TABLE->NEW_ROW( ).
  LR_COLUMN1->ADD_TEXT( TEXT = LV_TEXT1 SAP_EMPHASIS = CL_DD_AREA=>STRONG ).
  LR_COLUMN2->ADD_TEXT( TEXT = LV_TEXT2 ).
*--------------------------------------------------------------------*
  LV_TEXT1 = '기간 : '(L02).
  LV_TEXT2 = SPACE.

  LOOP AT S_POPER.
    IF SY-TABIX NE 1.
      CONCATENATE LV_TEXT2 ',' INTO LV_TEXT2 SEPARATED BY SPACE.
    ENDIF.

    IF S_POPER-HIGH IS INITIAL.
      LV_TEXT2 = |{ LV_TEXT2 } { P_GJAHR }.{ S_POPER-LOW+1(2) }|.
    ELSE.
      LV_TEXT2 = |{ LV_TEXT2 } { P_GJAHR }.{ S_POPER-LOW+1(2) } ~ { P_GJAHR }.{ S_POPER-HIGH+1(2) }|.
    ENDIF.

  ENDLOOP.

  CONDENSE LV_TEXT2.

  LR_TABLE->NEW_ROW( ).
  LR_COLUMN1->ADD_TEXT( TEXT = LV_TEXT1 SAP_EMPHASIS = CL_DD_AREA=>STRONG ).
  LR_COLUMN2->ADD_TEXT( TEXT = LV_TEXT2 ).
*--------------------------------------------------------------------*
  LV_TEXT1 = '옵션 : '.
  LV_TEXT2 = SPACE.

  CASE GC_X.
    WHEN P_WBS_P. LV_TEXT2 = '사업 WBS'(L04).
    WHEN P_WBS_E. LV_TEXT2 = '설비 WBS'(L05).
    WHEN P_WBS_A. LV_TEXT2 = '전체'(L06).
  ENDCASE.

  CASE GC_X.
    WHEN P_ACT. LV_TEXT2 = |{ LV_TEXT2 } / 실적|.
    WHEN P_PLN. LV_TEXT2 = |{ LV_TEXT2 } / 계획 / 계획버젼: { P_VERSN }|.
  ENDCASE.

  IF P_NOZERO EQ GC_X.
    LV_TEXT2 = |{ LV_TEXT2 } / 잔고 '0'인 WBS 제외|.
  ENDIF.

  IF P_EXCLDM EQ GC_X.
    LV_TEXT2 = |{ LV_TEXT2 } / 제조 제외|.
  ENDIF.

  LR_TABLE->NEW_ROW( ).
  LR_COLUMN1->ADD_TEXT( TEXT = LV_TEXT1 SAP_EMPHASIS = CL_DD_AREA=>STRONG ).
  LR_COLUMN2->ADD_TEXT( TEXT = LV_TEXT2 ).

*--------------------------------------------------------------------*
*  LV_TEXT1 = 'WBS 선택 : '(L03).
*  CASE GC_X.
*    WHEN P_WBS_P. LV_TEXT2 = '사업  WBS'(L04).
*    WHEN P_WBS_E. LV_TEXT2 = '설비  WBS'(L05).
*  ENDCASE.
*
*  LR_TABLE->NEW_ROW( ).
*  LR_COLUMN1->ADD_TEXT( TEXT = LV_TEXT1 SAP_EMPHASIS = CL_DD_AREA=>STRONG ).
*  LR_COLUMN2->ADD_TEXT( TEXT = LV_TEXT2 ).
**--------------------------------------------------------------------*
*  LV_TEXT1 = '실적/계획 선택 : '(L06).
*  CASE GC_X.
*    WHEN P_ACT. LV_TEXT2 = '실적'(L07).
*    WHEN P_PLN. LV_TEXT2 = |계획 / 계획버젼: { P_VERSN }|.
*  ENDCASE.
*
*  LR_TABLE->NEW_ROW( ).
*  LR_COLUMN1->ADD_TEXT( TEXT = LV_TEXT1 SAP_EMPHASIS = CL_DD_AREA=>STRONG ).
*  LR_COLUMN2->ADD_TEXT( TEXT = LV_TEXT2 ).
**--------------------------------------------------------------------*

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_TABLE
*&---------------------------------------------------------------------*
FORM ADD_TABLE  USING PR_TABLE          TYPE REF TO CL_DD_TABLE_ELEMENT
                      VALUE(PV_NUMBER)  TYPE I
                      VALUE(PV_WIDTH)   TYPE SDYDO_VALUE.

  CHECK GR_DDOC IS BOUND.

  GR_DDOC->ADD_TABLE(
    EXPORTING
      NO_OF_COLUMNS               = PV_NUMBER " Number of Table Columns
      BORDER                      = '0'       " Width of Table Frame; '0' = No Frame
      WIDTH                       = PV_WIDTH  " Width of Table; '100%' = Entire Width of Control
    IMPORTING
      TABLE                       = PR_TABLE  " Table Element
*      TABLEAREA                   =          " Table Area
    EXCEPTIONS
      TABLE_ALREADY_USED          = 1         " Reference Variable for TABLE Already Used
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
      WIDTH               = PV_WIDTH   " Width of Column (Example '20%')
    IMPORTING
      COLUMN              = PR_COLUMN  " Column Area
    EXCEPTIONS
      COLUMN_ALREADY_USED = 1          " Reference Variable for COLUMN has Already Been Used
      OTHERS              = 2
  ).

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
    I_TYPE       = 'B'
    I_STYLEFNAME = 'STYLE'
    I_CTAB_FNAME = 'COLOR'
  ).

  GR_ALV->SET_SORT( IT_FIELD = VALUE #( ( 'BUKRS' )
                                        ( 'BUTXT' )
                                        ( 'PROFL' )
                                        ( 'POSID' ) ) ).

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


  DATA LV_TEXT TYPE TEXT100.

  CLEAR GS_DISPLAY.

  LOOP AT GR_ALV->MT_FIELDCAT INTO DATA(LS_FIELDCAT).

    CLEAR LV_TEXT.
    CLEAR LS_FIELDCAT-KEY.
    CLEAR LS_FIELDCAT-COL_OPT.

*-- Field 속성
    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'STATUS'.
        LS_FIELDCAT-OUTPUTLEN = 4.
        LS_FIELDCAT-ICON      = GC_X.
*        LS_FIELDCAT-EMPHASIZE = 'C100'.
        LS_FIELDCAT-JUST      = GC_C.
        LS_FIELDCAT-FIX_COLUMN = GC_X.

      WHEN 'BUKRS'.
        LS_FIELDCAT-OUTPUTLEN = 5.
*        LS_FIELDCAT-KEY       = GC_X.
        LS_FIELDCAT-JUST      = GC_C.
        LS_FIELDCAT-FIX_COLUMN = GC_X.

      WHEN 'BUTXT'.
        LS_FIELDCAT-OUTPUTLEN = 15.
*        LS_FIELDCAT-EMPHASIZE = 'C500'.
        LS_FIELDCAT-FIX_COLUMN = GC_X.

      WHEN 'PSPNR'.
        LS_FIELDCAT-TECH      = GC_X.

      WHEN 'PROFL'.
        LS_FIELDCAT-OUTPUTLEN = 9.
        LS_FIELDCAT-JUST      = GC_C.
        LS_FIELDCAT-FIX_COLUMN = GC_X.

      WHEN 'POSID'.
        IF P_WBS_P EQ GC_X.
          LS_FIELDCAT-OUTPUTLEN = 10.
        ELSE.
          LS_FIELDCAT-OUTPUTLEN = 16.
        ENDIF.

*        LS_FIELDCAT-KEY       = GC_X.
        LS_FIELDCAT-HOTSPOT   = GC_X.
        LS_FIELDCAT-FIX_COLUMN = GC_X.

      WHEN 'POST1'.
        LS_FIELDCAT-OUTPUTLEN = 20.
*        LS_FIELDCAT-EMPHASIZE = 'C500'.
        LS_FIELDCAT-FIX_COLUMN = GC_X.

      WHEN 'AMT_CHECK'.
        LS_FIELDCAT-OUTPUTLEN = 12.
        LS_FIELDCAT-CURRENCY  = GS_TKA01-WAERS.
        LS_FIELDCAT-NO_ZERO   = GC_X.

      WHEN 'AMT_P'.
        LS_FIELDCAT-OUTPUTLEN = 12.
        LS_FIELDCAT-CURRENCY  = GS_TKA01-WAERS.
        LS_FIELDCAT-NO_ZERO   = GC_X.

      WHEN 'AMT_D'.
        LS_FIELDCAT-OUTPUTLEN = 12.
        LS_FIELDCAT-CURRENCY  = GS_TKA01-WAERS.
        LS_FIELDCAT-NO_ZERO   = GC_X.

      WHEN 'AMT_S'.
        LS_FIELDCAT-OUTPUTLEN = 12.
        LS_FIELDCAT-CURRENCY  = GS_TKA01-WAERS.
        LS_FIELDCAT-NO_ZERO   = GC_X.

      WHEN 'AMT_B'.
        LS_FIELDCAT-OUTPUTLEN = 12.
        LS_FIELDCAT-EMPHASIZE = 'C300'.
        LS_FIELDCAT-HOTSPOT   = GC_X.
        LS_FIELDCAT-CURRENCY  = GS_TKA01-WAERS.
        LS_FIELDCAT-NO_ZERO   = GC_X.

      WHEN 'MESSAGE'.
        LS_FIELDCAT-OUTPUTLEN = 15.

      WHEN 'STYLE'
        OR 'COLOR'.
        LS_FIELDCAT-TECH      = GC_X.

      WHEN OTHERS.
*        LS_FIELDCAT-TECH = GC_X.

    ENDCASE.

*-- Field 텍스트
    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'STATUS'.       LV_TEXT = '구분'.
      WHEN 'BUKRS'.        LV_TEXT = '회사'.
      WHEN 'BUTXT'.        LV_TEXT = '회사명'.
      WHEN 'PROFL'.        LV_TEXT = '프로파일'.
      WHEN 'POSID'.        LV_TEXT = 'WBS'.
      WHEN 'POST1'.        LV_TEXT = 'WBS이름'.
      WHEN 'AMT_P'.        LV_TEXT = '기표금액'.
      WHEN 'AMT_D'.        LV_TEXT = '배부금액'.
      WHEN 'AMT_S'.        LV_TEXT = '정산금액'.
      WHEN 'AMT_B'.        LV_TEXT = '잔고금액'.
      WHEN 'AMT_CHECK'.    LV_TEXT = '차변금액'.
      WHEN 'MESSAGE'.      LV_TEXT = '메세지'.
    ENDCASE.

    IF LV_TEXT IS NOT INITIAL.
      LS_FIELDCAT-REPTEXT   = LV_TEXT.
      LS_FIELDCAT-COLTEXT   = LV_TEXT.
      LS_FIELDCAT-SCRTEXT_L = LV_TEXT.
      LS_FIELDCAT-SCRTEXT_M = LV_TEXT.
      LS_FIELDCAT-SCRTEXT_S = LV_TEXT.
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

  SET HANDLER GR_EVENT_RECEIVER->ON_HOTSPOT_CLICK FOR GR_ALV->MR_ALV_GRID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK  USING PS_ROW_ID     TYPE LVC_S_ROW
                                 PS_COLUMN_ID  TYPE LVC_S_COL
                                 PS_ROW_NO     TYPE LVC_S_ROID
                                 PR_SENDER     TYPE REF TO CL_GUI_ALV_GRID.

  CASE PR_SENDER.
    WHEN GR_ALV->MR_ALV_GRID.

      READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX PS_ROW_ID-INDEX.
      CHECK SY-SUBRC EQ 0.

      CASE PS_COLUMN_ID-FIELDNAME.
        WHEN 'POSID'.
          SET PARAMETER ID 'PSP' FIELD SPACE.
          SET PARAMETER ID 'PRO' FIELD GS_DISPLAY-POSID.
          CALL TRANSACTION 'CJ03' AND SKIP FIRST SCREEN.
        WHEN 'AMT_B'.
          CASE GC_X.
            WHEN P_ACT.

              DATA LV_DATE1 TYPE SY-DATUM.
              DATA LV_DATE2 TYPE SY-DATUM.

              LV_DATE1 = P_GJAHR && GV_MONTH_MIN+1(2) && '01'.
              LV_DATE2 = P_GJAHR && GV_MONTH_MAX+1(2) && '01'.

              CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
                EXPORTING
                  DAY_IN            = LV_DATE2         " Key date
                IMPORTING
                  LAST_DAY_OF_MONTH = LV_DATE2         " Date of last day of the month from key  date
                EXCEPTIONS
                  DAY_IN_NO_DATE    = 1                " Key date is no date
                  OTHERS            = 2.


              SET PARAMETER ID 'PDB' FIELD '000000000001'.
              SET PARAMETER ID 'PSP' FIELD SPACE.
              SET PARAMETER ID 'PRO' FIELD GS_DISPLAY-POSID.
              SET PARAMETER ID 'ANR' FIELD SPACE.
              SET PARAMETER ID 'VGN' FIELD SPACE.
              SET PARAMETER ID 'MAT' FIELD SPACE.
              SET PARAMETER ID 'KAT' FIELD SPACE.
              SET PARAMETER ID 'KAG' FIELD SPACE.
              SET PARAMETER ID 'KS7' FIELD LV_DATE1.
              SET PARAMETER ID 'KS8' FIELD LV_DATE2.

              CALL TRANSACTION 'CJI3' AND SKIP FIRST SCREEN.

            WHEN P_PLN.

              SET PARAMETER ID 'PDB' FIELD '000000000001'.
              SET PARAMETER ID 'PSP' FIELD SPACE.
              SET PARAMETER ID 'PRO' FIELD GS_DISPLAY-POSID.
              SET PARAMETER ID 'MAT' FIELD SPACE.
              SET PARAMETER ID 'KAT' FIELD SPACE.
              SET PARAMETER ID 'KAG' FIELD SPACE.
              SET PARAMETER ID 'KVS' FIELD P_VERSN.
              SET PARAMETER ID 'VPE' FIELD GV_MONTH_MIN.
              SET PARAMETER ID 'BPE' FIELD GV_MONTH_MAX.
              SET PARAMETER ID 'GJR' FIELD P_GJAHR.

              CALL TRANSACTION 'CJI4' AND SKIP FIRST SCREEN.

          ENDCASE.
        WHEN OTHERS.
      ENDCASE.


  ENDCASE.

ENDFORM.

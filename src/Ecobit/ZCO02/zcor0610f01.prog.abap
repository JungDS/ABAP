*&---------------------------------------------------------------------*
*& Include          ZCOR0610F01
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


  " 기본값 : 회사코드
  S_BUKRS-LOW = ZCL_CO_COMMON=>GET_DEFAULT_BUKRS( ).

  IF S_BUKRS-LOW IS NOT INITIAL.
    APPEND S_BUKRS.
  ENDIF.


  S_GJAHR[] = VALUE #( ( LOW  = SY-DATUM(4)
                         HIGH = SY-DATUM(4) + 1 ) ).

  P_VERSN = 'E1'.


  " Selection Screen 텍스트
  TEXT_S01 = '실행조건'(S01).
  TEXT_S02 = '선택조건'(S02).
  SY-TITLE = '[CO] 설비 WBS 계획 대비 실적 비교'(T01).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM SCRFIELDS_FUNCTXT .

  CLEAR GS_FUNTXT.
  GS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  GS_FUNTXT-QUICKINFO = 'Program Help'(S03).

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

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  ZCL_CO_COMMON=>SET_KOKRS( P_KOKRS ).


  PERFORM CLEAR_ITAB.
  PERFORM SELECT_WBS.
  PERFORM SELECT_COSP.
  PERFORM SELECT_TEXT.

  PERFORM MAKE_DISPLAY_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR_ITAB
*&---------------------------------------------------------------------*
FORM CLEAR_ITAB .

  REFRESH: GT_WBS    ,
           GT_COSP   ,
           GT_DISPLAY,
           GT_1270T  ,
           GT_1280T  ,
           GT_1290T  ,
           GT_1300T  ,
           R_OBJNR   .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_WBS
*&---------------------------------------------------------------------*
FORM SELECT_WBS .

  SELECT A~PSPID,
         A~POST1     AS POST0,
         B~*,
         C~PSTRT,
         C~PENDE

    FROM PROJ AS A
    JOIN PRPS AS B ON B~PSPHI EQ A~PSPNR
    LEFT
    JOIN PRTE AS C ON C~POSNR EQ B~PSPNR
   WHERE A~PROFL EQ 'Z000003'
     AND B~PKOKR EQ @P_KOKRS
     AND B~PBUKR IN @S_BUKRS
     AND B~ZZIZW IN @S_ZZIZW
     AND B~ZZCD1 IN @S_ZZCD1
     AND B~POSID IN @S_POSID
     AND B~ERDAT IN @S_ERDAT
     AND A~LOEVM EQ @SPACE
     AND B~LOEVM EQ @SPACE
    INTO CORRESPONDING FIELDS OF TABLE @GT_WBS.

  CHECK SY-SUBRC EQ 0.

  R_OBJNR = VALUE #( SIGN = 'I' OPTION = 'EQ' ).

  LOOP AT GT_WBS INTO GS_WBS.
    R_OBJNR-LOW = GS_WBS-OBJNR.
    APPEND R_OBJNR.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_COSP
*&---------------------------------------------------------------------*
FORM SELECT_COSP .

  DATA LV_SELECTED_FIELDS TYPE TABLE OF FIELDNAME.
  DATA LV_GROUP_BY_FIELDS TYPE TABLE OF FIELDNAME.

  CHECK R_OBJNR[] IS NOT INITIAL.





  IF P_INCLD EQ GC_X.

    IF P_DTL_CE EQ GC_X.

      SELECT OBJNR,
             GJAHR,
             KSTAR,
             CASE WRTTP WHEN '01' THEN '01' ELSE '04' END AS WRTTP,
             SUM( WKG001 ) AS WKG001,
             SUM( WKG002 ) AS WKG002,
             SUM( WKG003 ) AS WKG003,
             SUM( WKG004 ) AS WKG004,
             SUM( WKG005 ) AS WKG005,
             SUM( WKG006 ) AS WKG006,
             SUM( WKG007 ) AS WKG007,
             SUM( WKG008 ) AS WKG008,
             SUM( WKG009 ) AS WKG009,
             SUM( WKG010 ) AS WKG010,
             SUM( WKG011 ) AS WKG011,
             SUM( WKG012 ) AS WKG012
        FROM COSP
       WHERE LEDNR EQ '00'
         AND OBJNR IN @R_OBJNR
         AND GJAHR IN @S_GJAHR
         AND ( ( VRGNG IN ('COIN','RKP1')
                       " 01: 계획 / 04: 실적 / 60: 임시전표
                 AND ( ( WRTTP EQ '01'        AND VERSN EQ @P_VERSN )
                    OR ( WRTTP IN ('04','60') AND VERSN EQ '000' ) ) )
            OR ( VRGNG IN ('RMBE','RMBA')
                       "" 구매약정
                     AND WRTTP IN ('21','22') AND VERSN EQ '000' ) )
         AND BEKNZ EQ 'S'
       GROUP BY OBJNR,
                GJAHR,
                KSTAR,
                CASE WRTTP
                  WHEN '01' THEN '01' ELSE '04'
                END
        INTO CORRESPONDING FIELDS OF TABLE @GT_COSP.

    ELSE.

      SELECT OBJNR,
             GJAHR,
*             KSTAR,
             CASE WRTTP WHEN '01' THEN '01' ELSE '04' END AS WRTTP,
             SUM( WKG001 ) AS WKG001,
             SUM( WKG002 ) AS WKG002,
             SUM( WKG003 ) AS WKG003,
             SUM( WKG004 ) AS WKG004,
             SUM( WKG005 ) AS WKG005,
             SUM( WKG006 ) AS WKG006,
             SUM( WKG007 ) AS WKG007,
             SUM( WKG008 ) AS WKG008,
             SUM( WKG009 ) AS WKG009,
             SUM( WKG010 ) AS WKG010,
             SUM( WKG011 ) AS WKG011,
             SUM( WKG012 ) AS WKG012
        FROM COSP
       WHERE LEDNR EQ '00'
         AND OBJNR IN @R_OBJNR
         AND GJAHR IN @S_GJAHR
         AND ( ( VRGNG IN ('COIN','RKP1')
                 AND ( ( WRTTP EQ '01'        AND VERSN EQ @P_VERSN )       " 01: 계획
                    OR ( WRTTP IN ('04','60') AND VERSN EQ '000' ) ) )      " 04: 실적 / 60: 임시전표
            OR ( VRGNG IN ('RMBE','RMBA')
                     AND WRTTP IN ('21','22') AND VERSN EQ '000' ) )        " 구매약정
         AND BEKNZ EQ 'S'
       GROUP BY OBJNR,
                GJAHR,
*                KSTAR,
                CASE WRTTP WHEN '01' THEN '01' ELSE '04' END
        INTO CORRESPONDING FIELDS OF TABLE @GT_COSP.

    ENDIF.


  ELSE.

    IF P_DTL_CE EQ GC_X.

      SELECT OBJNR,
             GJAHR,
             KSTAR,
             WRTTP,
             SUM( WKG001 ) AS WKG001,
             SUM( WKG002 ) AS WKG002,
             SUM( WKG003 ) AS WKG003,
             SUM( WKG004 ) AS WKG004,
             SUM( WKG005 ) AS WKG005,
             SUM( WKG006 ) AS WKG006,
             SUM( WKG007 ) AS WKG007,
             SUM( WKG008 ) AS WKG008,
             SUM( WKG009 ) AS WKG009,
             SUM( WKG010 ) AS WKG010,
             SUM( WKG011 ) AS WKG011,
             SUM( WKG012 ) AS WKG012
        FROM COSP
       WHERE LEDNR EQ '00'
         AND OBJNR IN @R_OBJNR
         AND GJAHR IN @S_GJAHR
         AND ( ( WRTTP EQ '01' AND VERSN EQ @P_VERSN )  " 01: 계획
            OR ( WRTTP EQ '04' AND VERSN EQ '000' )  )  " 04: 실적
         AND VRGNG IN ( 'COIN', 'RKP1' )
         AND BEKNZ EQ 'S'
       GROUP BY OBJNR, GJAHR, KSTAR, WRTTP
        INTO CORRESPONDING FIELDS OF TABLE @GT_COSP.

    ELSE.

      SELECT OBJNR,
             GJAHR,
*             KSTAR,
             WRTTP,
             SUM( WKG001 ) AS WKG001,
             SUM( WKG002 ) AS WKG002,
             SUM( WKG003 ) AS WKG003,
             SUM( WKG004 ) AS WKG004,
             SUM( WKG005 ) AS WKG005,
             SUM( WKG006 ) AS WKG006,
             SUM( WKG007 ) AS WKG007,
             SUM( WKG008 ) AS WKG008,
             SUM( WKG009 ) AS WKG009,
             SUM( WKG010 ) AS WKG010,
             SUM( WKG011 ) AS WKG011,
             SUM( WKG012 ) AS WKG012
        FROM COSP
       WHERE LEDNR EQ '00'
         AND OBJNR IN @R_OBJNR
         AND GJAHR IN @S_GJAHR
         AND ( ( WRTTP EQ '01' AND VERSN EQ @P_VERSN )  " 01: 계획
            OR ( WRTTP EQ '04' AND VERSN EQ '000' )  )  " 04: 실적
         AND VRGNG IN ( 'COIN', 'RKP1' )
         AND BEKNZ EQ 'S'
       GROUP BY OBJNR,
                GJAHR,
*                KSTAR,
                WRTTP
        INTO CORRESPONDING FIELDS OF TABLE @GT_COSP.

    ENDIF.

  ENDIF.




  CHECK SY-SUBRC EQ 0.




  SORT GT_COSP BY OBJNR
                  GJAHR
                  KSTAR
                  WRTTP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM MAKE_DISPLAY_DATA .

  DATA LT_DISPLAY LIKE TABLE OF GS_DISPLAY.


  SORT GT_WBS BY PSPID POSID.

  LOOP AT GT_WBS INTO GS_WBS.

    GS_DISPLAY = CORRESPONDING #( GS_WBS ).
    PERFORM SET_TEXT.

    PERFORM APPEND_DISPLAY.
  ENDLOOP.


  IF P_DTL_CE IS INITIAL.
    DELETE GT_DISPLAY WHERE KSTAR IS NOT INITIAL.
  ELSE.
    DELETE GT_DISPLAY WHERE KSTAR IS INITIAL.
  ENDIF.

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

  DATA LR_TABLE_B02 TYPE REF TO CL_DD_TABLE_ELEMENT.
  DATA LR_COL_B02_I TYPE REF TO CL_DD_AREA. " Icon
  DATA LR_COL_B02_L TYPE REF TO CL_DD_AREA. " Label

  PERFORM ADD_TABLE  USING GR_DDOC  LR_TABLE '100%'  2.
  PERFORM ADD_COLUMN USING LR_TABLE LR_COL_L '400px'.
  PERFORM ADD_COLUMN USING LR_TABLE LR_COL_R '*'.

  PERFORM ADD_TABLE  USING LR_COL_L     LR_TABLE_B01 '100%'  3.
  PERFORM ADD_COLUMN USING LR_TABLE_B01 LR_COL_B01_I '30px'.
  PERFORM ADD_COLUMN USING LR_TABLE_B01 LR_COL_B01_L '100px'.
  PERFORM ADD_COLUMN USING LR_TABLE_B01 LR_COL_B01_C '*'.

  PERFORM ADD_TABLE  USING LR_COL_R     LR_TABLE_B02 '100%'  2.
  PERFORM ADD_COLUMN USING LR_TABLE_B02 LR_COL_B02_I '30px'.
  PERFORM ADD_COLUMN USING LR_TABLE_B02 LR_COL_B02_L '*'.
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Selection Screen Block [B01] : Parameter & Select Options
*--------------------------------------------------------------------*
  PERFORM WRITE_DOCUMENT_B01 USING LR_TABLE_B01
                                   LR_COL_B01_I
                                   LR_COL_B01_L
                                   LR_COL_B01_C.

*--------------------------------------------------------------------*
* Selection Screen Block [B02] : Checkbox
*--------------------------------------------------------------------*
  PERFORM WRITE_DOCUMENT_B02 USING LR_TABLE_B02
                                   LR_COL_B02_I
                                   LR_COL_B02_L.

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

  GR_ALV->SET_SORT( IT_FIELD = VALUE #( ( 'PBUKR' )
                                        ( 'ZZIZW' )
                                        ( 'PSPID' )
                                        ( 'POST0' )
                                        ( 'POSID' )
                                        ( 'GJAHR' )
                                        ( 'KSTAR' )
                                        ) ).

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

  LV_KEY_FIX = GC_X.

  LOOP AT GR_ALV->MT_FIELDCAT INTO DATA(LS_FIELDCAT).

    CLEAR LV_TEXT.
    CLEAR LV_TOOLTIP.
    CLEAR LS_FIELDCAT-KEY.

*-- 열최적화
*    LS_FIELDCAT-COL_OPT = GC_X.

*-- 열고정
    LS_FIELDCAT-KEY = LS_FIELDCAT-FIX_COLUMN = LV_KEY_FIX.

*-- Field 속성
    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'ID'.
        LS_FIELDCAT-TECH      = GC_X.

      WHEN 'PBUKR'.
        LS_FIELDCAT-JUST      = GC_C.
*        LS_FIELDCAT-COL_OPT   = SPACE.
*        LS_FIELDCAT-OUTPUTLEN = 5.

      WHEN 'ZZIZW'.
        LS_FIELDCAT-JUST      = GC_C.
*        LS_FIELDCAT-COL_OPT   = SPACE.
*        LS_FIELDCAT-OUTPUTLEN = 8.

      WHEN 'ZZIZWTX'.
        LS_FIELDCAT-NO_OUT = GC_X.
*        LS_FIELDCAT-COL_OPT   = SPACE.
*        LS_FIELDCAT-OUTPUTLEN = 15.

      WHEN 'PSPID'.
*        LS_FIELDCAT-COL_OPT   = SPACE.
*        LS_FIELDCAT-OUTPUTLEN = 11.

      WHEN 'POST0'.
*        LS_FIELDCAT-COL_OPT   = SPACE.
*        LS_FIELDCAT-OUTPUTLEN = 10.

      WHEN 'STUFE'.
        LS_FIELDCAT-NO_OUT    = GC_X.
        LS_FIELDCAT-JUST      = GC_C.
*        LS_FIELDCAT-COL_OPT   = SPACE.
*        LS_FIELDCAT-OUTPUTLEN = 3.

      WHEN 'POSID'.
*        LS_FIELDCAT-COL_OPT   = SPACE.
*        LS_FIELDCAT-OUTPUTLEN = 16.
        LS_FIELDCAT-HOTSPOT   = GC_X.

      WHEN 'POST1'.
        CLEAR LV_KEY_FIX.
*        LS_FIELDCAT-COL_OPT   = SPACE.
*        LS_FIELDCAT-OUTPUTLEN = 40.


      WHEN 'ZZCD1'
        OR 'ZZCD2'
        OR 'ZZCD3'.
        LS_FIELDCAT-JUST      = GC_C.
*        LS_FIELDCAT-COL_OPT   = SPACE.
*        LS_FIELDCAT-OUTPUTLEN = 2.

      WHEN 'ZZCD1TX'
        OR 'ZZCD2TX'
        OR 'ZZCD3TX'.
*        LS_FIELDCAT-COL_OPT   = SPACE.
*        LS_FIELDCAT-OUTPUTLEN = 10.

      WHEN 'ZZUNT'.
        LS_FIELDCAT-JUST      = GC_C.

      WHEN 'ZZCMD'
        OR 'ZZCPD'.
        LS_FIELDCAT-JUST      = GC_C.
        LS_FIELDCAT-NO_OUT    = GC_X.

      WHEN 'KSTAR'.
        LS_FIELDCAT-JUST      = GC_C.
*        LS_FIELDCAT-COL_OPT   = SPACE.
*        LS_FIELDCAT-OUTPUTLEN = 10.
        LS_FIELDCAT-EMPHASIZE = 'C700'.
        IF P_DTL_CE IS INITIAL.
          LS_FIELDCAT-TECH = GC_X.
        ENDIF.

      WHEN 'KTEXT'.
*        LS_FIELDCAT-COL_OPT   = SPACE.
*        LS_FIELDCAT-OUTPUTLEN = 15.
        LS_FIELDCAT-EMPHASIZE = 'C700'.
        IF P_DTL_CE IS INITIAL.
          LS_FIELDCAT-TECH = GC_X.
        ENDIF.

      WHEN 'PSTRT'
        OR 'PENDE'.
*        LS_FIELDCAT-COL_OPT   = SPACE.
*        LS_FIELDCAT-OUTPUTLEN = 10.

      WHEN 'OBJNR'.
*        LS_FIELDCAT-COL_OPT   = SPACE.
*        LS_FIELDCAT-OUTPUTLEN = 10.
        LS_FIELDCAT-NO_OUT = GC_X.

      WHEN 'TDLINE'.
*        LS_FIELDCAT-COL_OPT   = SPACE.
*        LS_FIELDCAT-OUTPUTLEN = 10.
        LS_FIELDCAT-NO_OUT = GC_X.

      WHEN 'STYLE'
        OR 'COLOR'.
        LS_FIELDCAT-TECH = GC_X.

      WHEN OTHERS.

        IF LS_FIELDCAT-FIELDNAME(3) EQ 'WKG'.
*          LS_FIELDCAT-COL_OPT   = SPACE.
*          LS_FIELDCAT-OUTPUTLEN = 13.
          LS_FIELDCAT-CURRENCY = TKA01-WAERS.
          IF LS_FIELDCAT-FIELDNAME+3(3) EQ 'SUM'.
            LS_FIELDCAT-EMPHASIZE = 'C300'.
            LS_FIELDCAT-HOTSPOT   = GC_X.
          ELSEIF P_DTL_MN IS INITIAL.
            LS_FIELDCAT-TECH = GC_X.
          ENDIF.
        ENDIF.

    ENDCASE.


*-- Field 텍스트
    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'PBUKR'.     LV_TEXT     = TEXT-F01. " 회사
      WHEN 'ZZIZW'.     LV_TEXT     = TEXT-F02. " 투자사유
      WHEN 'ZZIZWTX'.   LV_TEXT     = TEXT-F03. " 투자사유명
      WHEN 'PSPID'.     LV_TEXT     = TEXT-F04. " 프로젝트
      WHEN 'POST0'.     LV_TEXT     = TEXT-F05. " 프로젝트명
      WHEN 'STUFE'.     LV_TEXT     = TEXT-F06. " 계층구조 레벨
                        LV_TOOLTIP  = TEXT-F07. " 프로젝트 계층구조내 레벨
      WHEN 'POSID'.     LV_TEXT     = TEXT-F08. " WBS
      WHEN 'POST1'.     LV_TEXT     = TEXT-F09. " WBS명

      WHEN 'ZZCD1'.     LV_TEXT     = TEXT-F10. " 대분류
                        LV_TOOLTIP  = TEXT-F11. " 설비대분류
      WHEN 'ZZCD1TX'.   LV_TEXT     = TEXT-F12. " 대분류명
                        LV_TOOLTIP  = TEXT-F13. " 설비대분류명
      WHEN 'ZZCD2'.     LV_TEXT     = TEXT-F14. " 중분류
                        LV_TOOLTIP  = TEXT-F15. " 설비중분류
      WHEN 'ZZCD2TX'.   LV_TEXT     = TEXT-F16. " 중분류명
                        LV_TOOLTIP  = TEXT-F17. " 설비중분류명
      WHEN 'ZZCD3'.     LV_TEXT     = TEXT-F18. " 소분류
                        LV_TOOLTIP  = TEXT-F19. " 설비소분류
      WHEN 'ZZCD3TX'.   LV_TEXT     = TEXT-F20. " 소분류명
                        LV_TOOLTIP  = TEXT-F21. " 설비소분류명

      WHEN 'KSTAR'.     LV_TEXT     = TEXT-F22. " 원가요소
      WHEN 'KTEXT'.     LV_TEXT     = TEXT-F23. " 원가요소명

      WHEN 'PSTRT'.     LV_TEXT     = TEXT-F24. " 시작일
      WHEN 'PENDE'.     LV_TEXT     = TEXT-F25. " 종료일
      WHEN 'OBJNR'.     LV_TEXT     = TEXT-F26. " 오브젝트
      WHEN 'TDLINE'.    LV_TEXT     = TEXT-F27. " 비고

      WHEN OTHERS.

        IF LS_FIELDCAT-FIELDNAME(3) EQ 'WKG'.

          IF LS_FIELDCAT-FIELDNAME+3(3) EQ 'SUM'.
            LV_TEXT = '연간'.
          ELSE.
            LV_TEXT = CONV INT1( LS_FIELDCAT-FIELDNAME+4(2) ) && '월'.
          ENDIF.

          LV_TEXT = LV_TEXT
                 && COND #( LET TYPE = LS_FIELDCAT-FIELDNAME+6(1) IN
                            WHEN TYPE EQ 'P' THEN '계획'
                            WHEN TYPE EQ 'A' THEN '실적' ).
        ENDIF.

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
*& Form HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK
  USING PS_ROW_ID     TYPE LVC_S_ROW
        PS_COLUMN_ID  TYPE LVC_S_COL
        PS_ROW_NO     TYPE LVC_S_ROID
        PR_SENDER     TYPE REF TO CL_GUI_ALV_GRID.


  DATA LV_DATE1 TYPE SY-DATUM.
  DATA LV_DATE2 TYPE SY-DATUM.


  CASE PR_SENDER.
    WHEN GR_ALV->MR_ALV_GRID.

      READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX PS_ROW_ID-INDEX.
      CHECK SY-SUBRC EQ 0.

      ASSIGN COMPONENT PS_COLUMN_ID-FIELDNAME
          OF STRUCTURE GS_DISPLAY
          TO FIELD-SYMBOL(<FS_VALUE>).

      CHECK SY-SUBRC EQ 0.


      CASE PS_COLUMN_ID-FIELDNAME.
        WHEN 'POSID'.

          SET PARAMETER ID 'PSP' FIELD SPACE.
          SET PARAMETER ID 'PRO' FIELD <FS_VALUE>.
          CALL TRANSACTION 'CJ03' AND SKIP FIRST SCREEN.

        WHEN 'WKGSUMA'.

          LV_DATE1 = GS_DISPLAY-GJAHR && '0101'.
          LV_DATE2 = GS_DISPLAY-GJAHR && '1231'.

          SET PARAMETER ID 'PDB' FIELD '000000000001'.
          SET PARAMETER ID 'PSP' FIELD SPACE.
          SET PARAMETER ID 'PRO' FIELD GS_DISPLAY-POSID.
          SET PARAMETER ID 'ANR' FIELD SPACE.
          SET PARAMETER ID 'VGN' FIELD SPACE.
          SET PARAMETER ID 'MAT' FIELD SPACE.
          SET PARAMETER ID 'KAT' FIELD GS_DISPLAY-KSTAR.
          SET PARAMETER ID 'KAG' FIELD SPACE.
          SET PARAMETER ID 'KS7' FIELD LV_DATE1.
          SET PARAMETER ID 'KS8' FIELD LV_DATE2.

          CALL TRANSACTION 'CJI3' AND SKIP FIRST SCREEN.

        WHEN 'WKGSUMP'.

          SET PARAMETER ID 'PDB' FIELD '000000000001'.
          SET PARAMETER ID 'PSP' FIELD SPACE.
          SET PARAMETER ID 'PRO' FIELD GS_DISPLAY-POSID.
          SET PARAMETER ID 'MAT' FIELD SPACE.
          SET PARAMETER ID 'KAT' FIELD GS_DISPLAY-KSTAR.
          SET PARAMETER ID 'KAG' FIELD SPACE.
          SET PARAMETER ID 'KVS' FIELD P_VERSN.
          SET PARAMETER ID 'VPE' FIELD '001'.
          SET PARAMETER ID 'BPE' FIELD '012'.
          SET PARAMETER ID 'GJR' FIELD GS_DISPLAY-GJAHR.

          CALL TRANSACTION 'CJI4' AND SKIP FIRST SCREEN.

      ENDCASE.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_TEXT
*&---------------------------------------------------------------------*
FORM SELECT_TEXT.

  SELECT SINGLE *
    FROM TKA01
   WHERE KOKRS EQ P_KOKRS.

*-- 투자사유 내역
  SELECT *
    FROM ZCOT1270T
   WHERE SPRAS EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @GT_1270T.

*-- 설비대분류명
  SELECT *
    FROM ZCOT1280T
   WHERE SPRAS EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @GT_1280T.

*-- 설비중분류명
  SELECT *
    FROM ZCOT1290T
   WHERE SPRAS EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @GT_1290T.

*-- 설비소분류명
  SELECT *
    FROM ZCOT1300T
   WHERE SPRAS EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @GT_1300T.

*-- 원가요소명
  SELECT *
    FROM CSKU
   WHERE SPRAS EQ @SY-LANGU
     AND KTOPL EQ @TKA01-KTOPL
    INTO CORRESPONDING FIELDS OF TABLE @GT_CSKU.


  SORT GT_1270T       BY ZZIZW.
  SORT GT_1280T       BY ZZCD1.
  SORT GT_1290T       BY ZZCD1 ZZCD2.
  SORT GT_1300T       BY ZZCD1 ZZCD2 ZZCD3.
  SORT GT_CSKU        BY KSTAR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TEXT
*&---------------------------------------------------------------------*
FORM SET_TEXT .

  " 투자사유명
  IF GS_DISPLAY-ZZIZW IS NOT INITIAL.
    READ TABLE GT_1270T INTO GS_1270T
                        WITH KEY ZZIZW = GS_DISPLAY-ZZIZW
                                 BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DISPLAY-ZZIZWTX = GS_1270T-ZZIZWTX.
    ENDIF.
  ENDIF.


  " 설비대분류명
  IF GS_DISPLAY-ZZCD1 IS NOT INITIAL.
    READ TABLE GT_1280T INTO GS_1280T
                        WITH KEY ZZCD1 = GS_DISPLAY-ZZCD1
                                 BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DISPLAY-ZZCD1TX = GS_1280T-ZZCD1TX.
    ENDIF.

    " 설비중분류명
    IF GS_DISPLAY-ZZCD2 IS NOT INITIAL.
      READ TABLE GT_1290T INTO GS_1290T
                          WITH KEY ZZCD1 = GS_DISPLAY-ZZCD1
                                   ZZCD2 = GS_DISPLAY-ZZCD2
                                   BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        GS_DISPLAY-ZZCD2TX = GS_1290T-ZZCD2TX.
      ENDIF.

      " 설비소분류명
      IF GS_DISPLAY-ZZCD3 IS NOT INITIAL.
        READ TABLE GT_1300T INTO GS_1300T
                            WITH KEY ZZCD1 = GS_DISPLAY-ZZCD1
                                     ZZCD2 = GS_DISPLAY-ZZCD2
                                     ZZCD3 = GS_DISPLAY-ZZCD3
                                     BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          GS_DISPLAY-ZZCD3TX = GS_1300T-ZZCD3TX.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


  IF GS_DISPLAY-KSTAR IS NOT INITIAL.
    READ TABLE GT_CSKU INTO GS_CSKU
                       WITH KEY KSTAR = GS_DISPLAY-KSTAR
                                BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DISPLAY-KTEXT = GS_CSKU-KTEXT.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_DISPLAY
*&---------------------------------------------------------------------*
FORM APPEND_DISPLAY.

  DATA LS_DISPLAY     LIKE GS_DISPLAY.

  DATA LV_FIELD       TYPE FIELDNAME.
  DATA LV_POPER       TYPE POPER.
  DATA LV_TYPE        TYPE C.

  FIELD-SYMBOLS: <FS_WKGXXX>     TYPE COSP-WKG001," 월별금액
                 <FS_WKGSUM>     TYPE COSP-WKG001," 연간금액
                 <FS_WKGXXX_CO>  TYPE COSP-WKG001." 계획/실적 금액

  FIELD-SYMBOLS: <FS_TOTAL>      LIKE GS_DISPLAY, " 합계라인
                 <FS_WKGXXX_TOT> TYPE COSP-WKG001," 합계라인의 월별금액
                 <FS_WKGSUM_TOT> TYPE COSP-WKG001." 합계라인의 연간금액

*-- 해당 오브젝트에 대한 계획/실적 정보 점검
  READ TABLE GT_COSP TRANSPORTING NO FIELDS
                      WITH KEY OBJNR = GS_DISPLAY-OBJNR
                               BINARY SEARCH.
  CHECK SY-SUBRC EQ 0.


  LOOP AT GT_COSP INTO GS_COSP FROM SY-TABIX.

    " Binary Search 이후 순차조회로 Exit 지점 체크
    IF GS_COSP-OBJNR NE GS_DISPLAY-OBJNR.
      EXIT.
    ENDIF.

    IF P_DTL_CE EQ GC_X.
      AT NEW GJAHR.
        APPEND GS_DISPLAY TO GT_DISPLAY ASSIGNING <FS_TOTAL>.
        <FS_TOTAL>-KTEXT = '합계'.
        <FS_TOTAL>-GJAHR = GS_COSP-GJAHR.
        IF P_DTL_CE EQ GC_X.
          <FS_TOTAL>-COLOR = VALUE #( ( COLOR-COL = 3 COLOR-INT = 1 ) ).
        ENDIF.
      ENDAT.
    ENDIF.

    AT NEW KSTAR.
      LS_DISPLAY = GS_DISPLAY.
      LS_DISPLAY-GJAHR = GS_COSP-GJAHR.
      LS_DISPLAY-KSTAR = GS_COSP-KSTAR.

      READ TABLE GT_CSKU INTO GS_CSKU
                         WITH KEY KSTAR = GS_COSP-KSTAR
                                  BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LS_DISPLAY-KTEXT = GS_CSKU-KTEXT.
      ENDIF.
    ENDAT.


    CASE GS_COSP-WRTTP.
      WHEN '01'. LV_TYPE = 'P'. " 계획
      WHEN '04'. LV_TYPE = 'A'. " 실적
      WHEN OTHERS.
        " & 필드값이 유효하지 않습니다. = 값유형
        MESSAGE E027 WITH TEXT-E01.
    ENDCASE.


    " 연간총금액
    LV_FIELD = 'WKGSUM' && LV_TYPE.
    ASSIGN COMPONENT LV_FIELD
        OF STRUCTURE LS_DISPLAY TO <FS_WKGSUM>.

    IF SY-SUBRC NE 0.
      " 필드 참조에 실패했습니다.
      MESSAGE E000 WITH TEXT-E02.
    ENDIF.

    IF <FS_TOTAL> IS ASSIGNED.
      ASSIGN COMPONENT LV_FIELD
          OF STRUCTURE <FS_TOTAL> TO <FS_WKGSUM_TOT>.
    ENDIF.


    DO 12 TIMES.

      " 월별 계획/실적 금액
      LV_POPER  = SY-INDEX.
      LV_FIELD = 'WKG' && LV_POPER.

      ASSIGN COMPONENT LV_FIELD
          OF STRUCTURE GS_COSP TO <FS_WKGXXX_CO>. " 계획/실적금액

      IF SY-SUBRC NE 0.
        " 필드 참조에 실패했습니다.
        MESSAGE E000 WITH TEXT-E02.
      ENDIF.


      " 출력화면의 계획/실적 금액
      LV_FIELD = LV_FIELD && LV_TYPE.

      ASSIGN COMPONENT LV_FIELD
          OF STRUCTURE LS_DISPLAY TO <FS_WKGXXX>. " 월별금액

      IF SY-SUBRC NE 0.
        " 필드 참조에 실패했습니다.
        MESSAGE E000 WITH TEXT-E02.
      ENDIF.

      " 원가요소별 라인
      <FS_WKGXXX> = <FS_WKGXXX_CO>.            " 월별금액
      <FS_WKGSUM> = <FS_WKGSUM> + <FS_WKGXXX>. " 연간합계

      " 합계라인
      IF <FS_TOTAL> IS ASSIGNED.
        ASSIGN COMPONENT LV_FIELD
            OF STRUCTURE <FS_TOTAL> TO <FS_WKGXXX_TOT>.
        IF SY-SUBRC NE 0.
          " 필드 참조에 실패했습니다.
          MESSAGE E000 WITH TEXT-E02.
        ENDIF.
        <FS_WKGXXX_TOT> = <FS_WKGXXX_TOT> + <FS_WKGXXX>. " 월별총금액
        <FS_WKGSUM_TOT> = <FS_WKGSUM_TOT> + <FS_WKGXXX>. " 연간총합계
      ENDIF.

    ENDDO.

    AT END OF KSTAR.
      APPEND LS_DISPLAY TO GT_DISPLAY.
    ENDAT.

  ENDLOOP.

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
*  LV_LABEL = '관리회계영역'(L01).
*  LV_CONDI = P_KOKRS.
*  PR_COL_I->ADD_ICON( 'ICON_PARAMETER' ).
*  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
*  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*
  LV_LABEL = '회사코드'(L02).
  __ADD_SELOPT_TEXT LV_CONDI S_BUKRS.

  PR_COL_I->ADD_ICON( COND #( LET  LINES  = LINES( S_BUKRS ) IN
                              WHEN LINES LE 1
                              THEN 'ICON_ENTER_MORE'
                              ELSE 'ICON_DISPLAY_MORE' ) ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*
  LV_LABEL = '투자사유'(L03).
  __ADD_SELOPT_TEXT LV_CONDI S_ZZIZW.

  PR_TABLE->NEW_ROW( ).
  PR_COL_I->ADD_ICON( COND #( LET  LINES  = LINES( S_ZZIZW ) IN
                              WHEN LINES LE 1
                              THEN 'ICON_ENTER_MORE'
                              ELSE 'ICON_DISPLAY_MORE' ) ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*
  LV_LABEL = '설비대분류'(L04).
  __ADD_SELOPT_TEXT LV_CONDI S_ZZCD1.

  PR_TABLE->NEW_ROW( ).
  PR_COL_I->ADD_ICON( COND #( LET  LINES  = LINES( S_ZZIZW ) IN
                              WHEN LINES LE 1
                              THEN 'ICON_ENTER_MORE'
                              ELSE 'ICON_DISPLAY_MORE' ) ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*
  LV_LABEL = '설비WBS'(L05).
  __ADD_SELOPT_TEXT LV_CONDI S_POSID.

  PR_TABLE->NEW_ROW( ).
  PR_COL_I->ADD_ICON( COND #( LET  LINES  = LINES( S_POSID ) IN
                              WHEN LINES LE 1
                              THEN 'ICON_ENTER_MORE'
                              ELSE 'ICON_DISPLAY_MORE' ) ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*
  LV_LABEL = 'WBS생성일'(L06).
  __ADD_SELOPT_TEXT LV_CONDI S_ERDAT.

  PR_TABLE->NEW_ROW( ).
  PR_COL_I->ADD_ICON( COND #( LET  LINES  = LINES( S_ERDAT ) IN
                              WHEN LINES LE 1
                              THEN 'ICON_ENTER_MORE'
                              ELSE 'ICON_DISPLAY_MORE' ) ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*
  LV_LABEL = '계획 버전'(L07).

  SELECT SINGLE TXT
    FROM TKT09
   WHERE LANGU EQ @SY-LANGU
     AND KOKRS EQ @P_KOKRS
     AND VERSN EQ @P_VERSN
    INTO @LV_CONDI.

  IF SY-SUBRC EQ 0.
    LV_CONDI = |{ P_VERSN } ({ LV_CONDI })|.
  ELSE.
    LV_CONDI = P_VERSN.
  ENDIF.

  PR_TABLE->NEW_ROW( ).
  PR_COL_I->ADD_ICON( 'ICON_PARAMETER' ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*

ENDFORM.
*&---------------------------------------------------------------------*
*& Form WRITE_DOCUMENT_B02
*&---------------------------------------------------------------------*
FORM WRITE_DOCUMENT_B02 USING PR_TABLE TYPE REF TO CL_DD_TABLE_ELEMENT
                              PR_COL_I TYPE REF TO CL_DD_AREA
                              PR_COL_L TYPE REF TO CL_DD_AREA.

  DATA LV_LABEL   TYPE TEXT255.

*--------------------------------------------------------------------*
  LV_LABEL = '월별 상세조회'(L08).

  PR_COL_I->ADD_ICON( COND #( WHEN P_DTL_MN EQ GC_X
                              THEN 'ICON_WD_RADIO_BUTTON'
                              ELSE 'ICON_WD_RADIO_BUTTON_EMPTY' ) ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
*--------------------------------------------------------------------*
  LV_LABEL = '원가요소별 상세조회'(L09).

  PR_TABLE->NEW_ROW( ).
  PR_COL_I->ADD_ICON( COND #( WHEN P_DTL_CE EQ GC_X
                              THEN 'ICON_WD_RADIO_BUTTON'
                              ELSE 'ICON_WD_RADIO_BUTTON_EMPTY' ) ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
*--------------------------------------------------------------------*
  LV_LABEL = '구매약정 및 임시전표 포함'(L10).

  PR_TABLE->NEW_ROW( ).
  PR_COL_I->ADD_ICON( COND #( WHEN P_INCLD EQ GC_X
                              THEN 'ICON_WD_RADIO_BUTTON'
                              ELSE 'ICON_WD_RADIO_BUTTON_EMPTY' ) ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
*--------------------------------------------------------------------*

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_VERSN
*&---------------------------------------------------------------------*
FORM CHECK_VERSN USING PV_VERSN.

  CHECK PV_VERSN IS NOT INITIAL.

  IF PV_VERSN(1) NE 'E'.
    " 버전의 첫문자는 영문 'E'를 사용해야 합니다.
    MESSAGE E000 WITH TEXT-E03.
  ENDIF.

  SELECT COUNT(*)
    FROM TKA09
   WHERE KOKRS EQ @P_KOKRS
     AND VERSN EQ @PV_VERSN.

  IF SY-SUBRC NE 0.
    " & 필드값이 유효하지 않습니다. with 계획 버전
    MESSAGE E027 WITH TEXT-L07.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_VERSN
*&---------------------------------------------------------------------*
FORM F4_VERSN  USING PV_VERSN.

  SELECT A~VERSN,
         A~ACTUAL,
         A~PLANNING,
         B~TXT
    FROM TKA09 AS A LEFT JOIN TKT09 AS B  ON B~LANGU EQ @SY-LANGU
                                         AND B~KOKRS EQ A~KOKRS
                                         AND B~VERSN EQ A~VERSN
   WHERE A~KOKRS    EQ @P_KOKRS
     AND A~PLANNING EQ @GC_X
     AND A~VERSN    LIKE 'E%'
   ORDER BY A~VERSN
    INTO TABLE @DATA(LT_TKA09).

  PV_VERSN = ZCL_CO_COMMON=>POPUP_VALUE_REQUEST(
               I_RETFIELD = 'VERSN'
               IT_VALUE   = LT_TKA09 ).

ENDFORM.

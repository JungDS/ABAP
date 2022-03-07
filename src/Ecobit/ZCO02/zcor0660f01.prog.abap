*&---------------------------------------------------------------------*
*& Include          ZCOR0640F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION .

  DATA LV_DATUM TYPE SY-DATUM.

  " 현재일로부터 30일 전의 연월을 사용
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = SY-DATUM
      DAYS      = '30'
      MONTHS    = '00'
      SIGNUM    = '-'
      YEARS     = '00'
    IMPORTING
      CALC_DATE = LV_DATUM.


  P_MENU  = 'ZCO_C1'.
  P_BUKRS = ZCL_CO_COMMON=>GET_DEFAULT_BUKRS( ).
  P_PERIO = LV_DATUM(6).


  " Selection Screen 텍스트
  TEXT_S01 = '실행조건'(S01).
  SY-TITLE = '[CO] 관리결산 수행메뉴'(T01).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM SCRFIELDS_FUNCTXT .

  SSCRFIELDS = VALUE #(
    FUNCTXT_01 = VALUE SMP_DYNTXT( ICON_ID   = ICON_INFORMATION
                                   QUICKINFO = TEXT-S02 ) " Program Help
    FUNCTXT_02 = VALUE SMP_DYNTXT( TEXT      = '관리결산 마감 절차등록'(S03) )
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

      SET PARAMETER ID 'GSE' FIELD P_MENU.

      ZCL_CO_COMMON=>CALL_TRANSACTION(
        EXPORTING
          I_TCODE                 = 'GS02' " Transaction Code
          I_SKIP_SCREEN           = GC_X   " Skip First Screen
        EXCEPTIONS
          CALL_TRANSACTION_DENIED = 1      " No Authorization
          TCODE_INVALID           = 2
          UNKNOWN_EXCEPTION       = 3
          OTHERS                  = 4 ).

      IF SY-SUBRC NE 0.
        CALL TRANSACTION 'GS03' WITHOUT AUTHORITY-CHECK
                                AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  PERFORM CLEAR_ITAB.
  PERFORM GET_MENU_TEXT.
  PERFORM SELECT_SETLEAF.
  PERFORM SELECT_TSTC.
  PERFORM SELECT_ZCAT0010.
  PERFORM SELECT_OTHERS.
  PERFORM MAKE_DISPLAY_DATA.

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

  GR_ALV->SET_SORT( IT_FIELD = VALUE #( ( 'SEQNR' )
*                                        ( 'MENU' )
                                        ( 'G1' )
                                        ( 'G1TXT' )
                                        ( 'G2' )
                                        ( 'G2TXT' )
                                        ( 'DESCR' )
                                        ) ).

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
    LS_FIELDCAT-COL_OPT = ABAP_ON.

*-- 열고정
*    LS_FIELDCAT-FIX_COLUMN = LV_KEY_FIX.
*    LS_FIELDCAT-KEY =


*-- Field 속성
    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'G1'.
        LV_TEXT = '그룹1'(F01).
        LS_FIELDCAT-KEY = ABAP_ON.

      WHEN 'G1TXT'.
        LV_TEXT = '그룹1명'(F02).
        LS_FIELDCAT-KEY = ABAP_ON.

      WHEN 'G2'.
        LV_TEXT = '그룹2'(F03).
        LS_FIELDCAT-KEY = ABAP_ON.

      WHEN 'G2TXT'.
        LV_TEXT = '그룹2명'(F04).
        LS_FIELDCAT-KEY = ABAP_ON.

      WHEN 'DESCR'.
        LV_TEXT = '작업설명'(F05).
        LS_FIELDCAT-EMPHASIZE = 'C500'.

      WHEN 'TCODE'.
        LV_TEXT = 'T-Code'(F06).
        LS_FIELDCAT-EMPHASIZE = 'C500'.

      WHEN 'TTEXT'.
        LV_TEXT = 'T-Code명'(F07).
        LS_FIELDCAT-EMPHASIZE = 'C500'.

      WHEN 'HELP'.
        LV_TEXT = '도움말'(F08).
        LS_FIELDCAT-EMPHASIZE = 'C500'.
        LS_FIELDCAT-ICON      = ABAP_ON.
        LS_FIELDCAT-HOTSPOT   = ABAP_ON.

      WHEN 'SEQNR'.
        LV_TEXT = '라인'(F09).
        LS_FIELDCAT-EMPHASIZE = 'C500'.

      WHEN 'STYLE'
        OR 'COLOR'.
        LS_FIELDCAT-TECH      = ABAP_ON.
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
    GR_EVENT_RECEIVER->ON_DOUBLE_CLICK  FOR GR_ALV->MR_ALV_GRID,
    GR_EVENT_RECEIVER->ON_HOTSPOT_CLICK FOR GR_ALV->MR_ALV_GRID.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM MAKE_DISPLAY_DATA .

  DATA: LV_G1 LIKE GS_DISPLAY-G1,
        LV_G2 LIKE GS_DISPLAY-G2.

  LOOP AT GT_SET INTO GS_SET.

    LV_G1 = GS_SET-VALFROM+0(1).
    LV_G2 = GS_SET-VALFROM+1(1).

    CLEAR GS_TSTCT.
    READ TABLE GT_TSTCT INTO GS_TSTCT
                        WITH KEY TCODE = GS_SET-VALFROM+2
                                 BINARY SEARCH.
    IF SY-SUBRC NE 0.
      READ TABLE GT_TSTCT INTO GS_TSTCT
                          WITH KEY TCODE = GS_SET-VALFROM+3
                                   BINARY SEARCH.
      LV_G1 = GS_SET-VALFROM+0(1).
      LV_G2 = GS_SET-VALFROM+1(2).
    ENDIF.

    GS_DISPLAY = VALUE #(
*      MENU  = GV_MENU_TEXT    " 메뉴이름
      G1    = LV_G1           " G1
*      G1TXT =                " G1 그룹명
      G2    = LV_G2           " G2
*      G2TXT =                " G2 그룹명
*      DESCR =                " 작업설명(그룹반영)
      TCODE = GS_TSTCT-TCODE  " T-Code
      TTEXT = GS_TSTCT-TTEXT  " T-Code명
*      HELP  =                " 도움말 등록여부 표시
      SEQNR = GS_SET-SEQNR    " 라인
*      BUKRS = P_BUKRS         " 회사
*      GJAHR = P_PERIO+0(4)    " 연도
*      PERDE = P_PERIO+4(2)    " 월
    ).

    IF GS_DISPLAY-TCODE IS NOT INITIAL.

      GS_DISPLAY-DESCR = GS_SET-DESCRIPT.

    ELSEIF GS_DISPLAY-G2 IS INITIAL
        OR GS_DISPLAY-G2 EQ '0'.

      GS_DISPLAY-G1TXT = GS_SET-DESCRIPT.

    ELSE.

      GS_DISPLAY-G2TXT = GS_SET-DESCRIPT.

    ENDIF.


    IF GS_DISPLAY-TCODE IS NOT INITIAL.

      " 도움말 가져올 때 화면 '1000' 먼저 조회하고,
      " 없으면 정렬된 첫 번째 도움말을 조회한다.
      CLEAR GS_HELP.
      READ TABLE GT_HELP INTO GS_HELP
                         WITH KEY CPROG = GS_DISPLAY-TCODE
                                  DYNNR = '1000'
                                  BINARY SEARCH.
      IF SY-SUBRC NE 0.
        READ TABLE GT_HELP INTO GS_HELP
                           WITH KEY CPROG = GS_DISPLAY-TCODE
                                    BINARY SEARCH.
      ENDIF.

      IF GS_HELP IS NOT INITIAL.
        GS_DISPLAY-HELP  = ICON_DISPLAY_TEXT.
        GS_DISPLAY-CPROG = GS_HELP-CPROG.
        GS_DISPLAY-DYNNR = GS_HELP-DYNNR.
        GS_DISPLAY-TDIDF = GS_HELP-TDIDF.
      ENDIF.
    ENDIF.

    APPEND GS_DISPLAY TO GT_DISPLAY.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR_ITAB
*&---------------------------------------------------------------------*
FORM CLEAR_ITAB .

  REFRESH: GT_DISPLAY,
           GT_SET,
           GT_TSTCT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MENU_TEXT
*&---------------------------------------------------------------------*
FORM GET_MENU_TEXT .

  SELECT SINGLE DESCRIPT
    FROM SETHEADERT
   WHERE SETCLASS EQ '0000'
     AND SETNAME  EQ @P_MENU
     AND LANGU    EQ @SY-LANGU
    INTO @GV_MENU_TEXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_SETLEAF
*&---------------------------------------------------------------------*
FORM SELECT_SETLEAF .

  SELECT A~LINEID,
         A~VALFROM,
         A~SEQNR,
         B~DESCRIPT
    FROM SETLEAF AS A LEFT JOIN SETLINET AS B
                             ON B~SETCLASS EQ A~SETCLASS
                            AND B~SUBCLASS EQ A~SUBCLASS
                            AND B~SETNAME  EQ A~SETNAME
                            AND B~LANGU    EQ @SY-LANGU
                            AND B~LINEID   EQ A~LINEID
   WHERE A~SETCLASS EQ '0000'
     AND A~SETNAME  EQ @P_MENU
    INTO TABLE @GT_SET.

  SORT GT_SET BY SEQNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_TSTC
*&---------------------------------------------------------------------*
FORM SELECT_TSTC .

  SELECT A~TCODE,
         B~TTEXT
    FROM TSTC  AS A LEFT JOIN TSTCT AS B ON B~SPRSL EQ @SY-LANGU
                                        AND B~TCODE EQ A~TCODE
    INTO TABLE @GT_TSTCT.

  SORT GT_TSTCT BY TCODE.

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

      TRY.
        CASE PS_COLUMN_ID-FIELDNAME.
          WHEN 'HELP'.
            CALL FUNCTION 'ZCA_F1_HELP'
              EXPORTING
                IV_CPROG = GS_DISPLAY-CPROG
                IV_DYNNR = GS_DISPLAY-DYNNR
                IV_SPRAS = SY-LANGU
                IV_TDIDF = GS_DISPLAY-TDIDF.
        ENDCASE.
      CATCH CX_ROOT.
      ENDTRY.

  ENDCASE.

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
*      REUSE_CONTROL      = GC_X
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
  LV_LABEL = '마감메뉴'(L01).
  LV_CONDI = P_MENU && COND #( WHEN GV_MENU_TEXT IS NOT INITIAL
                               THEN | ( { GV_MENU_TEXT } )|
                               ELSE SPACE ).
  PR_COL_I->ADD_ICON( 'ICON_PARAMETER' ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*
  PR_TABLE->NEW_ROW( ).
*--------------------------------------------------------------------*
  LV_LABEL = '회사코드'(L02).
  LV_CONDI = COND #( WHEN P_BUKRS EQ '*'
                     THEN P_BUKRS
                     ELSE |{ P_BUKRS } ( { GV_CLOSE } )| ).
  PR_COL_I->ADD_ICON( 'ICON_PARAMETER' ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*
  PR_TABLE->NEW_ROW( ).
*--------------------------------------------------------------------*
  LV_LABEL = '기준년월'(L03).
  WRITE P_PERIO TO LV_CONDI.
  PR_COL_I->ADD_ICON( 'ICON_PARAMETER' ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_ZCAT0010
*&---------------------------------------------------------------------*
FORM SELECT_ZCAT0010 .

  SELECT CPROG, DYNNR, TDIDF
    FROM ZCAT0010
   WHERE TDSPRAS EQ @SY-LANGU
     AND ( TDTITLE  IS NOT INITIAL OR COMMTENT IS NOT INITIAL )
    INTO TABLE @GT_HELP.

  SORT GT_HELP BY CPROG DYNNR TDIDF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK
  USING PS_ROW     TYPE LVC_S_ROW
        PS_COLUMN  TYPE LVC_S_COL
        PS_ROW_NO  TYPE LVC_S_ROID
        PR_SENDER  TYPE REF TO CL_GUI_ALV_GRID.

  CASE PR_SENDER.
    WHEN GR_ALV->MR_ALV_GRID.

      READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX PS_ROW-INDEX.
      CHECK SY-SUBRC EQ 0.

      CASE PS_COLUMN-FIELDNAME.
        WHEN 'TCODE'.
          PERFORM EXECUTE_TRANSACTION USING GS_DISPLAY-TCODE.
        WHEN OTHERS.
      ENDCASE.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXECUTE_TRANSACTION
*&---------------------------------------------------------------------*
FORM EXECUTE_TRANSACTION USING PV_TCODE LIKE TSTC-TCODE.

  DATA : LV_GJAHR TYPE GJAHR.
  DATA : LV_PERBL TYPE PERBL.

  LV_GJAHR = P_PERIO+0(4).
  LV_PERBL = P_PERIO+4(2).


*.. PV_TCODE 점검
  READ TABLE GT_TSTCT TRANSPORTING NO FIELDS
                      WITH KEY TCODE = PV_TCODE
                               BINARY SEARCH.
  CHECK SY-SUBRC EQ 0.



  SET PARAMETER ID 'BUK' FIELD P_BUKRS.
  SET PARAMETER ID 'CAC' FIELD GV_KOKRS.
  SET PARAMETER ID 'ERB' FIELD GV_ERKRS.
  SET PARAMETER ID 'KPL' FIELD GV_KTOPL.

  SET PARAMETER ID 'GJR'   FIELD LV_GJAHR.
  SET PARAMETER ID 'BDTJ'  FIELD LV_GJAHR.
  SET PARAMETER ID 'MLB'   FIELD LV_GJAHR.
  SET PARAMETER ID 'ACJ'   FIELD LV_GJAHR.
*
  SET PARAMETER ID 'ACM'   FIELD LV_PERBL.
  SET PARAMETER ID 'VPE'   FIELD LV_PERBL.
  SET PARAMETER ID 'MLP'   FIELD LV_PERBL.
  SET PARAMETER ID 'BPE'   FIELD LV_PERBL.
  SET PARAMETER ID 'POPR'  FIELD LV_PERBL.
  SET PARAMETER ID 'ACPR'  FIELD LV_PERBL+1(2).
  SET PARAMETER ID 'ACM'   FIELD LV_PERBL.
  SET PARAMETER ID '74P'   FIELD LV_PERBL.



  TRY.
    CALL TRANSACTION PV_TCODE WITH AUTHORITY-CHECK.
  CATCH CX_SY_AUTHORIZATION_ERROR INTO DATA(LX_AUTH).
    " 실행권한 없음
    MESSAGE LX_AUTH->GET_TEXT( ) TYPE GC_I DISPLAY LIKE GC_E.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_OTHERS
*&---------------------------------------------------------------------*
FORM SELECT_OTHERS .

  IF P_BUKRS EQ '*'.

    GV_KOKRS = GV_ERKRS = GV_KTOPL = '1000'.

  ELSE.
    SELECT SINGLE KOKRS
      FROM TKA02
     WHERE BUKRS EQ @P_BUKRS
      INTO @GV_KOKRS.

    SELECT SINGLE ERKRS
      FROM TKA01
     WHERE KOKRS EQ @GV_KOKRS
      INTO @GV_ERKRS.

    SELECT SINGLE KTOPL
      FROM T001
     WHERE BUKRS EQ @P_BUKRS
      INTO @GV_KTOPL.
  ENDIF.


  PERFORM GET_CLOSE_STATUS.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CLOSE_STATUS
*&---------------------------------------------------------------------*
FORM GET_CLOSE_STATUS .

  DATA LV_FIELD TYPE FIELDNAME.


  GV_CLOSE = '사별결산 확정전'(M02).


  SELECT VERSN,
         MON01,
         MON02,
         MON03,
         MON04,
         MON05,
         MON06,
         MON07,
         MON08,
         MON09,
         MON10,
         MON11,
         MON12
    FROM ZCOT0190
   WHERE KOKRS EQ @GV_KOKRS
     AND BUKRS EQ @P_BUKRS
     AND GJAHR EQ @P_PERIO(4)
    INTO TABLE @DATA(LT_0190).

  CHECK SY-SUBRC EQ 0.

  LOOP AT LT_0190 INTO DATA(LS_0190).

    LS_0190-VERSN = |{ LS_0190-VERSN ALPHA = OUT }|.

    CHECK LS_0190-VERSN EQ '0'. " 실제만 취급


    LV_FIELD = 'MON' && P_PERIO+4(2).

    ASSIGN COMPONENT LV_FIELD
        OF STRUCTURE LS_0190
        TO FIELD-SYMBOL(<FS_CLOSE>).

    CHECK SY-SUBRC EQ 0.

    IF <FS_CLOSE> EQ ABAP_ON.
      GV_CLOSE = '사별결산 확정'(M03).
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.

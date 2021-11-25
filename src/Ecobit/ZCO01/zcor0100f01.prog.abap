*&---------------------------------------------------------------------*
*& Include          ZCOR0100F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITAIL
*&---------------------------------------------------------------------*
FORM INITAIL .
  GV_REPID = SY-REPID.

*  GET PARAMETER ID 'CAC' FIELD PA_KOKRS.

  SELECT SINGLE BEZEI INTO @PA_KTXT
    FROM TKA01
   WHERE KOKRS = @PA_KOKRS.

*  CONCATENATE SY-DATUM(4) '0101' INTO PA_PSTRT.
  PA_PSTRT = '19000101'.
  PA_PENDE = '99991231'.

  CASE SY-TCODE.

    WHEN 'ZCOR0100'.
      GV_MODE = 'M'.
      SY-TITLE = '[CO] Proj, WBS 마스터 일괄 조회_관리자용'.

    WHEN  OTHERS.
      GV_MODE = 'U'.
      SY-TITLE = '[CO] Proj, WBS 마스터 일괄 조회_사업소용'.

      SELECT SINGLE PARVA
        INTO PA_PBUKR
        FROM USR05
       WHERE BNAME EQ SY-UNAME
         AND PARID EQ 'BUK'.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM SET_SCREEN .

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'PA_KOKRS'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.

    IF GV_MODE = 'U'.
      IF SCREEN-NAME = 'PA_VBUKR'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-GROUP1 = 'PRC'.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INTIAL_VALUE_CHECK
*&---------------------------------------------------------------------*
FORM INTIAL_VALUE_CHECK .

  IF GV_MODE = 'U' AND PA_PBUKR IS INITIAL.
    SET CURSOR FIELD 'PA_PBUKR'.
    MESSAGE S026  WITH TEXT-C05 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  PERFORM MAKE_RANGE_VARIABLE.
  PERFORM SELECTED_MAIN_DATA.
  PERFORM ADDITIONAL_DATA.

*--------------------------------------------------------------------*
* [CO] ESG Pjt. 기존PGM 고도화 - 2021.11.10 14:25:25, MDP_06
*--------------------------------------------------------------------*
  PERFORM ADDITIONAL_DATA_2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_RANGE_VARIABLE
*&---------------------------------------------------------------------*
FORM MAKE_RANGE_VARIABLE .
  CLEAR: R_PBUKR, R_PBUKR[], R_PRCTR, R_PRCTR[].

  IF PA_PBUKR IS NOT INITIAL.
    MOVE: 'I'      TO R_PBUKR-SIGN,
          'EQ'     TO R_PBUKR-OPTION,
          PA_PBUKR TO R_PBUKR-LOW.
    APPEND R_PBUKR.
  ENDIF.

  IF PA_PRCTR IS NOT INITIAL.
    MOVE: 'I'      TO R_PRCTR-SIGN,
          'EQ'     TO R_PRCTR-OPTION,
          PA_PRCTR TO R_PRCTR-LOW.
    APPEND R_PRCTR.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_MAIN_DATA
*&---------------------------------------------------------------------*
FORM SELECTED_MAIN_DATA .
  CLEAR: GT_ITAB, GT_ITAB[], GV_COUNT.

*  SELECT A~PSPID A~OBJNR A~PROFL A~KZBWS   A~PLINT A~VBUKR A~VGSBR
*         A~PRCTR B~POSID B~POST1 B~OBJNR AS OBJNR2 B~PLINT AS PLINT2
*         B~PBUKR B~PGSBR B~PRCTR AS PRCTR2 B~WERKS B~STUFE C~PSTRT
*         C~PENDE B~PLAKZ B~BELKZ B~FAKKZ   B~CLASF B~ABGSL B~ZZSCT
*         B~ZZPHA B~ZZWBT B~ZZBGU B~ZZBGD   B~ZZPRG B~ZZADT B~ZZHWB
*         B~ZZBAG B~ZZIVC B~ZZCYP B~ZZCOP
*    INTO CORRESPONDING FIELDS OF TABLE GT_ITAB
*    FROM PROJ AS A JOIN PRPS AS B
*                     ON A~PSPNR = B~PSPHI
*                   JOIN PRTE AS C
*                     ON B~PSPNR = C~POSNR
*   WHERE B~PBUKR IN R_PBUKR
*     AND B~PRCTR IN R_PRCTR
*     AND C~PSTRT >= PA_PSTRT
*     AND C~PENDE <= PA_PENDE.

  SELECT A~PSPID A~POST1 AS PRPOST1
         A~OBJNR A~PROFL A~KZBWS A~PLINT A~VBUKR A~VGSBR
         A~PRCTR B~POSID B~POST1 B~OBJNR AS OBJNR2 B~PLINT AS PLINT2
         B~PBUKR B~PGSBR E~GTEXT B~PRCTR AS PRCTR2 D~WERKS
         B~STUFE C~PSTRT C~PENDE B~PLAKZ B~BELKZ B~FAKKZ   B~CLASF
         B~ABGSL B~ZZSCT B~ZZPHA B~ZZWBT B~ZZBGU B~ZZBGD   B~ZZPRG
         B~ZZADT B~ZZHWB B~ZZBAG B~ZZIVC B~ZZCYP B~ZZCOP
    INTO CORRESPONDING FIELDS OF TABLE GT_ITAB
    FROM PROJ AS A JOIN PRPS AS B
                     ON A~PSPNR = B~PSPHI
                   JOIN PRTE AS C
                     ON B~PSPNR = C~POSNR
        LEFT OUTER JOIN ZSDT0120 AS D
                     ON B~PSPNR = D~WBSNR
                    AND D~MODUL = 'M'
*                    AND D~LVORM = ''    "mm 삭제지시자
                    AND D~HR_LVORM = ''  "CO-HR 플랜트 제외 2019.12.16 추가
        LEFT OUTER JOIN TGSBT AS E
                     ON E~SPRAS = SY-LANGU
                    AND B~PGSBR = E~GSBER

*--------------------------------------------------------------------*
* [CO] ESG Pjt. 기존PGM 고도화 - 2021.11.10 14:25:25, MDP_06
*--------------------------------------------------------------------*
   WHERE A~PROFL NE 'Z000003'   " 설비WBS를 위한 Profile 제외
     AND B~PBUKR IN R_PBUKR
     AND B~PRCTR IN R_PRCTR
     AND C~PSTRT >= PA_PSTRT
     AND C~PENDE <= PA_PENDE.

*-- 기존 조건절
*   WHERE B~PBUKR IN R_PBUKR
*     AND B~PRCTR IN R_PRCTR
*     AND C~PSTRT >= PA_PSTRT
*     AND C~PENDE <= PA_PENDE.


  GV_COUNT = LINES( GT_ITAB ).

  IF GV_COUNT > 0.
    SORT GT_ITAB BY PSPID.
    MESSAGE S039 WITH GV_COUNT.
  ELSE.
    MESSAGE S004 DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADDITIONAL_DATA
*&---------------------------------------------------------------------*
FORM ADDITIONAL_DATA .
  CLEAR: GV_FAIL, GT_OUTTAB.

  DATA: LT_TJ02T TYPE TABLE OF TJ02T WITH HEADER LINE.
  DATA: BEGIN OF LT_COBRB OCCURS 0,
          OBJNR LIKE COBRB-OBJNR,
          BUREG LIKE COBRB-BUREG,
          LFDNR LIKE COBRB-LFDNR,
          VERSN LIKE COBRB-VERSN,
        END OF LT_COBRB.

  DATA: BEGIN OF LT_T001W OCCURS 0,
          WERKS LIKE T001W-WERKS,
          NAME1 LIKE T001W-NAME1,
        END OF LT_T001W.

  SELECT WERKS NAME1 INTO TABLE LT_T001W
    FROM T001W.
  SORT LT_T001W BY WERKS.

  LOOP AT GT_ITAB.

    READ TABLE LT_T001W WITH KEY WERKS = GT_ITAB-WERKS
                        BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_ITAB-NAME1 = LT_T001W-NAME1.
      CLEAR LT_T001W.
    ENDIF.

    CLEAR: LT_TJ02T, LT_TJ02T[].
    SELECT B~ISTAT B~SPRAS B~TXT04 B~TXT30
      INTO CORRESPONDING FIELDS OF TABLE LT_TJ02T
      FROM JEST AS A JOIN TJ02T AS B
                       ON A~STAT = B~ISTAT
     WHERE A~OBJNR = GT_ITAB-OBJNR
       AND A~INACT = ''
       AND B~SPRAS = SY-LANGU.

    LOOP AT LT_TJ02T.
      IF SY-TABIX EQ 1.
        GT_ITAB-TXT30 = LT_TJ02T-TXT30.
      ELSE.
        CONCATENATE GT_ITAB-TXT30 ', ' LT_TJ02T-TXT30 INTO GT_ITAB-TXT30.
      ENDIF.
    ENDLOOP.

    CLEAR: LT_TJ02T, LT_TJ02T[].
    SELECT B~ISTAT B~SPRAS B~TXT04 B~TXT30
      INTO CORRESPONDING FIELDS OF TABLE LT_TJ02T
      FROM JEST AS A JOIN TJ02T AS B
                       ON A~STAT = B~ISTAT
     WHERE A~OBJNR = GT_ITAB-OBJNR2
       AND A~INACT = ''
       AND B~SPRAS = SY-LANGU.

    LOOP AT LT_TJ02T.
      IF SY-TABIX EQ 1.
        GT_ITAB-TXT302 = LT_TJ02T-TXT30.
      ELSE.
        CONCATENATE GT_ITAB-TXT302 `, ` LT_TJ02T-TXT30 INTO GT_ITAB-TXT302.
      ENDIF.
    ENDLOOP.

    CLEAR: LT_COBRB, LT_COBRB[].
    SELECT OBJNR BUREG LFDNR VERSN
      INTO TABLE LT_COBRB
      FROM COBRB
     WHERE OBJNR = GT_ITAB-OBJNR2
       AND ( VERSN = '' OR VERSN = '000' ).

    DATA(LV_LINES) = LINES( LT_COBRB ).
    IF LV_LINES = 2.
      GT_ITAB-VERSN = '있음'.
    ENDIF.

    PERFORM CHECK_DATA.

    APPEND GT_ITAB TO GT_OUTTAB.
    CLEAR GT_ITAB.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATA
*&---------------------------------------------------------------------*
FORM CHECK_DATA .

  IF GT_ITAB-VBUKR = '1700' AND GT_ITAB-KZBWS <> 'M'.
    GT_ITAB-MESSAGE = TEXT-M01.
    GV_FAIL = GV_FAIL + 1.
    EXIT.
  ENDIF.

  IF GT_ITAB-PLINT <> 'X'.
    GT_ITAB-MESSAGE = TEXT-M02.
    GV_FAIL = GV_FAIL + 1.
    EXIT.
  ENDIF.

  IF GT_ITAB-VGSBR IS NOT INITIAL OR GT_ITAB-PRCTR IS NOT INITIAL.
    GT_ITAB-MESSAGE = TEXT-M03.
    GV_FAIL = GV_FAIL + 1.
    EXIT.
  ENDIF.

  IF GT_ITAB-PLINT2 <> 'X'.
    GT_ITAB-MESSAGE = TEXT-M04.
    GV_FAIL = GV_FAIL + 1.
    EXIT.
  ENDIF.

  IF GT_ITAB-ABGSL IS NOT INITIAL.
    GT_ITAB-MESSAGE = TEXT-M05.
    GV_FAIL = GV_FAIL + 1.
    EXIT.
  ENDIF.

  IF GT_ITAB-STUFE <> '1'.
    GT_ITAB-MESSAGE = TEXT-M06.
    GV_FAIL = GV_FAIL + 1.
    EXIT.
  ENDIF.

  IF GT_ITAB-PBUKR IS INITIAL OR GT_ITAB-PGSBR IS INITIAL OR GT_ITAB-PRCTR2 IS INITIAL.
    GT_ITAB-MESSAGE = TEXT-M07.
    GV_FAIL = GV_FAIL + 1.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SALV_CALL
*&---------------------------------------------------------------------*
FORM SALV_CALL .

  TRY.
      CL_SALV_TABLE=>FACTORY(
                IMPORTING
                  R_SALV_TABLE = GO_ALV
                CHANGING
                  T_TABLE      = GT_OUTTAB ).
    CATCH CX_SALV_MSG.
  ENDTRY.

  PERFORM SET_PF_STATUS.
  PERFORM SET_LAYOUT.

  PERFORM SET_TOP_OF_PAGE.

  PERFORM SET_EVENT.
  PERFORM SET_TABLE_SETTINGS.

  GO_ALV->DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_PF_STATUS
*&---------------------------------------------------------------------*
FORM SET_PF_STATUS .

  DATA: LT_FUNC_LIST TYPE SALV_T_UI_FUNC,
        LS_FUNC_LIST TYPE SALV_S_UI_FUNC.

  DATA: LV_TITLE TYPE LVC_TITLE.
  DATA: LV_TEXT  TYPE CHAR100.

  GO_ALV->SET_SCREEN_STATUS(
    PFSTATUS      =  'STANDARD'
    REPORT        =  GV_REPID
    SET_FUNCTIONS = GO_ALV->C_FUNCTIONS_ALL ).

  LV_TEXT = TEXT-T00.
  CONCATENATE LV_TEXT '_관리자용' INTO LV_TITLE.

* set output control : ZEBRA
  GO_DSPSET = GO_ALV->GET_DISPLAY_SETTINGS( ).
  GO_DSPSET->SET_STRIPED_PATTERN( ABAP_TRUE ).
  GO_DSPSET->SET_LIST_HEADER( LV_TITLE ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT
*&---------------------------------------------------------------------*
FORM SET_LAYOUT .

  GS_SALV_LAYOUT-REPID = SY-REPID.
  GS_SALV_LAYOUT-DEFAULT = 'X'.
  GS_SALV_LAYOUT-LAYOUT = '/STANDARD'.
  GS_KEY-REPORT = GV_REPID.

  GO_LAYOUT = GO_ALV->GET_LAYOUT( ).
  GO_LAYOUT->SET_KEY( GS_KEY ).

  GS_SALV_LAYOUT-RESTRICT = IF_SALV_C_LAYOUT=>RESTRICT_NONE.
  GO_LAYOUT->SET_DEFAULT( GS_SALV_LAYOUT-DEFAULT ).
  GO_LAYOUT->SET_SAVE_RESTRICTION( GS_SALV_LAYOUT-RESTRICT ).
  GO_LAYOUT->SET_INITIAL_LAYOUT( GS_SALV_LAYOUT-LAYOUT ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM SET_TOP_OF_PAGE .

  DATA: LR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.
  PERFORM BUILT_HEADER CHANGING LR_CONTENT.
  GO_ALV->SET_TOP_OF_LIST( LR_CONTENT ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILT_HEADER
*&---------------------------------------------------------------------*
FORM BUILT_HEADER  CHANGING CR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.

  DATA: LR_GRID   TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_GRID_1 TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_LABEL  TYPE REF TO CL_SALV_FORM_LABEL,
        LR_TEXT   TYPE REF TO CL_SALV_FORM_TEXT.

  CREATE OBJECT LR_GRID.

  LR_GRID_1 = LR_GRID->CREATE_GRID(
                ROW    = 2
                COLUMN = 1 ).

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW     = 1
    COLUMN  = 1
    TEXT    = TEXT-T03
    TOOLTIP = TEXT-T03 ).

  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW     = 1
    COLUMN  = 2
    TEXT    = GV_COUNT
    TOOLTIP = GV_COUNT ).

  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW    = 2
    COLUMN = 1
    TEXT    = TEXT-T04
    TOOLTIP = TEXT-T04 ).

  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW    = 2
    COLUMN = 2
    TEXT    = GV_FAIL
    TOOLTIP = GV_FAIL ).

  CR_CONTENT = LR_GRID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TABLE_SETTINGS
*&---------------------------------------------------------------------*
FORM SET_TABLE_SETTINGS .

  DATA: LR_COLUMNS  TYPE REF TO CL_SALV_COLUMNS_TABLE.
  DATA: LO_AGGRS    TYPE REF TO CL_SALV_AGGREGATIONS.

*-- set column
  TRY.
      LR_COLUMNS = GO_ALV->GET_COLUMNS( ).
      LO_AGGRS   = GO_ALV->GET_AGGREGATIONS( ).
      LR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).
      LR_COLUMNS->SET_CELL_TYPE_COLUMN( 'CELLTYPE' ).
    CATCH CX_SALV_DATA_ERROR.
  ENDTRY.

  PERFORM SET_COLUMNS_TECHNICAL USING LR_COLUMNS
                                      LO_AGGRS.

  TRY.
      LR_COLUMNS->SET_KEY_FIXATION( ABAP_TRUE ).
    CATCH CX_SALV_DATA_ERROR.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_COLUMNS_TECHNICAL
*&---------------------------------------------------------------------*
FORM SET_COLUMNS_TECHNICAL  USING IR_COLUMNS TYPE REF TO
                                            CL_SALV_COLUMNS_TABLE
                                  IR_AGG TYPE REF TO
                                            CL_SALV_AGGREGATIONS.

  DATA: LR_COLUMN     TYPE REF TO CL_SALV_COLUMN_TABLE.
  DATA: LR_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS,
        LT_COLUMN     TYPE SALV_T_COLUMN.

  DATA LV_LTEXT TYPE SCRTEXT_L.
  DATA LV_MTEXT TYPE SCRTEXT_M.
  DATA LV_STEXT TYPE SCRTEXT_S.

  FIELD-SYMBOLS:
    <COLUMN_REF> LIKE LINE OF GT_COLUMN_REF.

  GT_COLUMN_REF = IR_COLUMNS->GET( ).

  TRY.
      LOOP AT GT_COLUMN_REF
        ASSIGNING <COLUMN_REF>.
        GR_COLUMN ?= IR_COLUMNS->GET_COLUMN( <COLUMN_REF>-COLUMNNAME ).

        PERFORM SET_COLUMN_TEXT USING    <COLUMN_REF>-COLUMNNAME
                                CHANGING GV_COLUMN_TEXT.

        IF GV_COLUMN_TEXT IS NOT INITIAL.

          GV_SCRTEXT_S = GV_COLUMN_TEXT.
          GV_SCRTEXT_M = GV_COLUMN_TEXT.
          GV_SCRTEXT_L = GV_COLUMN_TEXT.

          GR_COLUMN->SET_SHORT_TEXT( GV_SCRTEXT_S ).
          GR_COLUMN->SET_MEDIUM_TEXT( GV_SCRTEXT_M ).
          GR_COLUMN->SET_LONG_TEXT( GV_SCRTEXT_L ).

        ENDIF.

*--------------------------------------------------------------------*
* [CO] ESG Pjt. 기존PGM 고도화 - 2021.11.10 14:25:25, MDP_06
*--------------------------------------------------------------------*
        PERFORM SET_TOOLTIP_TEXT USING    <COLUMN_REF>-COLUMNNAME
                                 CHANGING GV_TOOLTIP.
        IF GV_TOOLTIP IS NOT INITIAL.
          GR_COLUMN->SET_TOOLTIP( GV_TOOLTIP ).
        ENDIF.


      ENDLOOP.

    CATCH CX_SALV_NOT_FOUND.
    CATCH CX_SALV_DATA_ERROR.
    CATCH CX_SALV_EXISTING.

  ENDTRY.

  TRY.
      LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'OBJNR' ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'OBJNR2' ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

*-- SELECT FIELD 추가
  LR_SELECTIONS = GO_ALV->GET_SELECTIONS( ).
  LR_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>CELL ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_COLUMN_TEXT
*&---------------------------------------------------------------------*
FORM SET_COLUMN_TEXT  USING    PV_COLUMNNAME
                      CHANGING PV_COLUMN_TEXT.

  CLEAR PV_COLUMN_TEXT.

  CASE PV_COLUMNNAME.

    WHEN 'PSPID'.
      PV_COLUMN_TEXT = TEXT-C01.
      GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).
      GR_COLUMN->SET_CELL_TYPE( IF_SALV_C_CELL_TYPE=>HOTSPOT ).

    WHEN 'PRPOST1'.
      PV_COLUMN_TEXT = TEXT-C41.
      GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

    WHEN 'PROFL'.
      PV_COLUMN_TEXT = TEXT-C02.
      GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

    WHEN 'KZBWS'.
      PV_COLUMN_TEXT = TEXT-C03.
      GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

    WHEN 'PLINT'.
      PV_COLUMN_TEXT = TEXT-C04.
      GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

    WHEN 'VBUKR'.
      PV_COLUMN_TEXT = TEXT-C05.
      GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

    WHEN 'VGSBR'.
      PV_COLUMN_TEXT = TEXT-C06.
      GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

    WHEN 'PRCTR'.
      PV_COLUMN_TEXT = TEXT-C07.
      GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

    WHEN 'TXT30'.
      PV_COLUMN_TEXT = TEXT-C08.
      GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

    WHEN 'POSID'.
      PV_COLUMN_TEXT = TEXT-C09.
      GR_COLUMN->SET_CELL_TYPE( IF_SALV_C_CELL_TYPE=>HOTSPOT ).

    WHEN 'POST1'.
      PV_COLUMN_TEXT = TEXT-C10.

    WHEN 'PLINT2'.
      PV_COLUMN_TEXT = TEXT-C11.

    WHEN 'PBUKR'.
      PV_COLUMN_TEXT = TEXT-C12.

    WHEN 'PRCTR2'.
      PV_COLUMN_TEXT = TEXT-C14.

    WHEN 'PGSBR'.
      PV_COLUMN_TEXT = TEXT-C13.

    WHEN 'GTEXT'.
      PV_COLUMN_TEXT = TEXT-C39.

    WHEN 'WERKS'.
      PV_COLUMN_TEXT = TEXT-C15.

    WHEN 'NAME1'.
      PV_COLUMN_TEXT = TEXT-C40.

    WHEN 'STUFE'.
      PV_COLUMN_TEXT = TEXT-C16.

    WHEN 'PSTRT'.
      PV_COLUMN_TEXT = TEXT-C17.

    WHEN 'PENDE'.
      PV_COLUMN_TEXT = TEXT-C18.

    WHEN 'PLAKZ'.
      PV_COLUMN_TEXT = TEXT-C19.

    WHEN 'BELKZ'.
      PV_COLUMN_TEXT = TEXT-C20.

    WHEN 'FAKKZ'.
      PV_COLUMN_TEXT = TEXT-C21.

    WHEN 'CLASF'.
      PV_COLUMN_TEXT = TEXT-C22.

    WHEN 'ABGSL'.
      PV_COLUMN_TEXT = TEXT-C23.

    WHEN 'ZZSCT'.
      PV_COLUMN_TEXT = TEXT-C24.

    WHEN 'ZZPHA'.
      PV_COLUMN_TEXT = TEXT-C25.

    WHEN 'ZZWBT'.
      PV_COLUMN_TEXT = TEXT-C26.

    WHEN 'ZZBGU'.
      PV_COLUMN_TEXT = TEXT-C27.

    WHEN 'ZZBGD'.
      PV_COLUMN_TEXT = TEXT-C28.

    WHEN 'ZZPRG'.
      PV_COLUMN_TEXT = TEXT-C29.

    WHEN 'ZZADT'.
      PV_COLUMN_TEXT = TEXT-C30.

    WHEN 'ZZHWB'.
      PV_COLUMN_TEXT = TEXT-C31.

    WHEN 'ZZBAG'.
      PV_COLUMN_TEXT = TEXT-C32.

    WHEN 'ZZIVC'.
      PV_COLUMN_TEXT = TEXT-C33.

    WHEN 'ZZCYP'.
      PV_COLUMN_TEXT = TEXT-C37.

    WHEN 'ZZCOP'.
      PV_COLUMN_TEXT = TEXT-C38.

*--------------------------------------------------------------------*
* [CO] ESG Pjt. 기존PGM 고도화 - 2021.11.10 14:25:25, MDP_06
*--------------------------------------------------------------------*
    WHEN 'WW120'.
      PV_COLUMN_TEXT = TEXT-C42.

    WHEN 'BEZEK'.
      PV_COLUMN_TEXT = TEXT-C43.


    WHEN 'TXT302'.
      PV_COLUMN_TEXT = TEXT-C34.

    WHEN 'VERSN'.
      PV_COLUMN_TEXT = TEXT-C35.

    WHEN 'MESSAGE'.
      PV_COLUMN_TEXT = TEXT-C36.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EVENT
*&---------------------------------------------------------------------*
FORM SET_EVENT .

*-- EVENT
  DATA: LR_EVENTS TYPE REF TO CL_SALV_EVENTS_TABLE.

  GO_FUNCTIONS = GO_ALV->GET_FUNCTIONS( ).
  GO_FUNCTIONS->SET_ALL( ).

  LR_EVENTS = GO_ALV->GET_EVENT( ).

  CREATE OBJECT G_EVENT_RECEIVER.
  SET HANDLER G_EVENT_RECEIVER->ON_USER_COMMAND FOR LR_EVENTS.
  SET HANDLER G_EVENT_RECEIVER->TOP_OF_PAGE     FOR LR_EVENTS.
  SET HANDLER G_EVENT_RECEIVER->ON_LINK_CLICK   FOR LR_EVENTS.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND  USING P_UCOMM TYPE SALV_DE_FUNCTION.

  DATA L_DUMMY TYPE C LENGTH 100.

  CASE P_UCOMM.

    WHEN '&REF'.
      PERFORM SELECTED_DATA_RTN.
      GO_ALV->REFRESH( ).

    WHEN '&HELP'.
      PERFORM CALL_POPUP_HELP(ZCAR9000)
               USING SY-REPID SY-DYNNR SY-LANGU ''.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_LINK_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_LINK_CLICK  USING    PV_ROW    TYPE SALV_DE_ROW
                                 PV_COLUMN TYPE SALV_DE_COLUMN.

  DATA: LT_SPAGPA TYPE TABLE OF RFC_SPAGPA WITH HEADER LINE.

  READ TABLE GT_OUTTAB INTO GS_OUTTAB INDEX PV_ROW.
  CHECK SY-SUBRC = 0.

  CASE PV_COLUMN.

    WHEN 'PSPID' OR 'POSID'.
*      SET PARAMETER ID 'PSP' FIELD GS_OUTTAB-PSPID.
*      CALL TRANSACTION 'CJ20N' AND SKIP FIRST SCREEN.

      CLEAR: LT_SPAGPA, LT_SPAGPA[].
      LT_SPAGPA-PARID  = 'PSP'.
      LT_SPAGPA-PARVAL = GS_OUTTAB-PSPID.
      APPEND LT_SPAGPA. CLEAR LT_SPAGPA.

      CALL FUNCTION 'ABAP4_CALL_TRANSACTION' STARTING NEW TASK ''
        EXPORTING
          TCODE       = 'CJ20N'
          SKIP_SCREEN = 'X'
        TABLES
          SPAGPA_TAB  = LT_SPAGPA.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_INIT_HELP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_INIT_HELP .

  "__ Function Key
  DATA: LS_FUNTXT TYPE SMP_DYNTXT.

  LS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  LS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = LS_FUNTXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCR_USER_COMMAND_HELP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SCR_USER_COMMAND_HELP .

  CASE SSCRFIELDS-UCOMM.
    WHEN 'FC01'.
      PERFORM CALL_POPUP_HELP(ZCAR9000) USING SY-REPID SY-DYNNR SY-LANGU ''.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADDITIONAL_DATA_2
*&---------------------------------------------------------------------*
* [CO] ESG Pjt. 기존PGM 고도화 - 2021.11.10 14:25:25, MDP_06
* - BU 정보 추가
*&---------------------------------------------------------------------*
FORM ADDITIONAL_DATA_2 .

  DATA LR_POSID   TYPE RANGE OF PRPS-POSID WITH HEADER LINE.
  DATA LS_COPA    TYPE CE11000.


  CHECK GT_OUTTAB[] IS NOT INITIAL.


  " WBS 내부번호 조회
  LR_POSID[] = CORRESPONDING #( GT_OUTTAB MAPPING LOW = POSID ).
  LR_POSID   = VALUE #( SIGN   = 'I'
                        OPTION = 'EQ' ).
  MODIFY LR_POSID TRANSPORTING SIGN OPTION WHERE SIGN IS INITIAL.

  SORT LR_POSID BY LOW.
  DELETE ADJACENT DUPLICATES FROM LR_POSID COMPARING LOW.

  SELECT POSID, PSPNR
    FROM PRPS
   WHERE POSID IN @LR_POSID
    INTO TABLE @DATA(LT_PRPS).

  SORT LT_PRPS BY POSID PSPNR.


  " BU구분명 조회
  SELECT WW120, BEZEK
    FROM T25A1
   WHERE SPRAS EQ @SY-LANGU
    INTO TABLE @DATA(LT_T25A1).

  SORT LT_T25A1 BY WW120.



  " BU구분 및 BU구분명 출력데이터에 반영

  LOOP AT GT_OUTTAB INTO GS_OUTTAB.

    " WBS 정보 조회
    READ TABLE LT_PRPS INTO DATA(LS_PRPS)
                       WITH KEY POSID = GS_OUTTAB-POSID
                                BINARY SEARCH.
    CHECK SY-SUBRC EQ 0.

    LS_COPA = VALUE #( BUKRS = GS_OUTTAB-PBUKR
                       WW040 = GS_OUTTAB-ZZBGU
                       WW050 = GS_OUTTAB-ZZBGD
                       WW100 = GS_OUTTAB-ZZPRG ).

    " BU구분 조회
    CALL FUNCTION 'ZCO_GET_BU_TYPE_BY_MAPPING'
      EXPORTING
        I_PSPNR        = LS_PRPS-PSPNR      " WBS 요소
        I_COPA         = LS_COPA            " BU구분을 위한 추가정보
      IMPORTING
        E_WW120        = GS_OUTTAB-WW120.   " BU구분

    " BU구분 조회결과 점검
    CHECK GS_OUTTAB-WW120 IS NOT INITIAL.

    " BU구분명 조회
    READ TABLE LT_T25A1 INTO DATA(LS_T25A1)
                        WITH KEY WW120 = GS_OUTTAB-WW120
                                 BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_OUTTAB-BEZEK = LS_T25A1-BEZEK.
    ENDIF.

    MODIFY GT_OUTTAB FROM GS_OUTTAB TRANSPORTING WW120 BEZEK.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TOOLTIP_TEXT
*&---------------------------------------------------------------------*
* [CO] ESG Pjt. 기존PGM 고도화 - 2021.11.10 14:25:25, MDP_06
*&---------------------------------------------------------------------*
FORM SET_TOOLTIP_TEXT USING    PV_COLUMNNAME
                      CHANGING PV_TOOLTIP.

  CLEAR PV_TOOLTIP.

  CASE PV_COLUMNNAME.

    WHEN 'WW120'.
      PV_TOOLTIP = TEXT-C44.

    WHEN 'BEZEK'.
      PV_TOOLTIP = TEXT-C45.

  ENDCASE.


ENDFORM.

*&---------------------------------------------------------------------*
*& Include          ZCOR0300F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITAIL
*&---------------------------------------------------------------------*
FORM INITAIL .

  GV_REPID = SY-REPID.

  SELECT SINGLE BEZEI, WAERS INTO (@PA_KTXT, @GV_WAERS)
    FROM TKA01
   WHERE KOKRS = @PA_KOKRS.

  SELECT SINGLE VTEXT INTO @PA_VTXT
    FROM TKVS AS A
    LEFT JOIN TKVST AS B
      ON A~VERSI = B~VERSI
     AND B~SPRAS = @SY-LANGU
   WHERE A~VERSI = '000'.

  "__ 20191223 BSGSM_FCM ADD default cac
  SET PARAMETER ID 'CAC' FIELD PA_KOKRS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM SET_SCREEN .

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'MG1'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATA_GET
*&---------------------------------------------------------------------*
FORM DATA_GET .

  DATA LV_FIELDNAME TYPE FIELDNAME.
  DATA LV_FYEAR     TYPE JAHRPER.

  CLEAR: GT_DATA, GT_DATA[].
  CLEAR: R_OBJNR, R_OBJNR[].
  CLEAR: R_KSTAR, R_KSTAR[].

  R_OBJNR-LOW    = 'KS' && PA_KOKRS && '*'.
  R_OBJNR-SIGN   = 'I'.
  R_OBJNR-OPTION = 'CP'.
  APPEND R_OBJNR.

  R_OBJNR-LOW    = 'PR' && '*'.
  R_OBJNR-SIGN   = 'I'.
  R_OBJNR-OPTION = 'CP'.
  APPEND R_OBJNR.

  R_KSTAR-LOW    = '0100000000'.
  R_KSTAR-HIGH   = '0399999999'.
  R_KSTAR-SIGN   = 'E'.
  R_KSTAR-OPTION = 'BT'.
  APPEND R_KSTAR.

  R_KSTAR-LOW    = '0990000000'.
  R_KSTAR-SIGN   = 'E'.
  R_KSTAR-OPTION = 'EQ'.
  APPEND R_KSTAR.

  SELECT A~KOSTL, B~KTEXT
    INTO TABLE @GT_KOSTL
    FROM CSKS AS A
    LEFT JOIN CSKT AS B
      ON A~KOKRS = B~KOKRS
     AND A~KOSTL = B~KOSTL
     AND A~DATBI = B~DATBI
     AND B~SPRAS = @SY-LANGU
   WHERE A~KOKRS = @PA_KOKRS.

  SELECT OBJNR, PSPNR, POSID, POST1, ZZCYP
    INTO TABLE @GT_PRPS
    FROM PRPS
   WHERE PKOKR = @PA_KOKRS.

  SELECT OBJNR, KSTAR,
         SUM( WKG001 ) AS WKG001, SUM( WKG002 ) AS WKG002,
         SUM( WKG003 ) AS WKG003, SUM( WKG004 ) AS WKG004,
         SUM( WKG005 ) AS WKG005, SUM( WKG006 ) AS WKG006,
         SUM( WKG007 ) AS WKG007, SUM( WKG008 ) AS WKG008,
         SUM( WKG009 ) AS WKG009, SUM( WKG010 ) AS WKG010,
         SUM( WKG011 ) AS WKG011, SUM( WKG012 ) AS WKG012
    FROM COSP
   WHERE LEDNR = '00'
     AND VERSN = @PA_VERSN
     AND WRTTP = '04'
     AND GJAHR = @PA_GJAHR
     AND KSTAR IN @R_KSTAR
     AND OBJNR IN @R_OBJNR
     AND VRGNG <> 'SDOR'
     AND BUKRS IN @SO_BUKRS
   GROUP BY OBJNR, KSTAR
   UNION ALL
  SELECT OBJNR, KSTAR,
         SUM( WKG001 ) AS WKG001, SUM( WKG002 ) AS WKG002,
         SUM( WKG003 ) AS WKG003, SUM( WKG004 ) AS WKG004,
         SUM( WKG005 ) AS WKG005, SUM( WKG006 ) AS WKG006,
         SUM( WKG007 ) AS WKG007, SUM( WKG008 ) AS WKG008,
         SUM( WKG009 ) AS WKG009, SUM( WKG010 ) AS WKG010,
         SUM( WKG011 ) AS WKG011, SUM( WKG012 ) AS WKG012
    FROM COSS
   WHERE LEDNR = '00'
     AND VERSN = @PA_VERSN
     AND WRTTP = '04'
     AND GJAHR = @PA_GJAHR
     AND KSTAR IN @R_KSTAR
     AND OBJNR IN @R_OBJNR
     AND VRGNG <> 'SDOR'
     AND BUKRS IN @SO_BUKRS
   GROUP BY OBJNR, KSTAR
    INTO TABLE @GT_DATA.

  DELETE GT_DATA WHERE WKG001 = 0
                   AND WKG002 = 0
                   AND WKG003 = 0
                   AND WKG004 = 0
                   AND WKG005 = 0
                   AND WKG006 = 0
                   AND WKG007 = 0
                   AND WKG008 = 0
                   AND WKG009 = 0
                   AND WKG010 = 0
                   AND WKG011 = 0
                   AND WKG012 = 0.

  LV_FYEAR = PA_GJAHR && PA_SPERL.

  SELECT OBJNR,
         SUM( HSL ) AS HSL
    FROM ACDOCA
   WHERE RLDNR  = '0L'
     AND GJAHR  = @PA_GJAHR
     AND KOKRS  = @PA_KOKRS
     AND RACCT  IN @R_KSTAR
     AND OBJNR  IN @R_OBJNR
     AND RBUKRS IN @SO_BUKRS
     AND FISCYEARPER = @LV_FYEAR
   GROUP BY OBJNR
    INTO TABLE @GT_ACDOCA.

  DELETE GT_ACDOCA WHERE HSL = 0.

  LOOP AT GT_DATA.

    PERFORM READ_TEXT USING    GT_DATA-OBJNR
                      CHANGING GS_OUTTAB.

    MOVE: GV_WAERS        TO GS_OUTTAB-WAERS,
          GT_DATA-OBJNR   TO GS_OUTTAB-OBJNR.

    LV_FIELDNAME = 'GT_DATA-WKG' && PA_SPERL.
    ASSIGN (LV_FIELDNAME) TO FIELD-SYMBOL(<FS_WKG>).

    GS_OUTTAB-CODMBTR = <FS_WKG>.

    COLLECT GS_OUTTAB INTO GT_OUTTAB.
    CLEAR   GS_OUTTAB.

  ENDLOOP.

  LOOP AT GT_ACDOCA.

    PERFORM READ_TEXT USING    GT_ACDOCA-OBJNR
                      CHANGING GS_OUTTAB.

    MOVE: GV_WAERS          TO GS_OUTTAB-WAERS,
          GT_ACDOCA-OBJNR   TO GS_OUTTAB-OBJNR.

    GS_OUTTAB-FIDMBTR = GT_ACDOCA-HSL.

    COLLECT GS_OUTTAB INTO GT_OUTTAB.
    CLEAR   GS_OUTTAB.

  ENDLOOP.

  LOOP AT GT_OUTTAB INTO GS_OUTTAB.

    MOVE-CORRESPONDING GS_OUTTAB TO GS_DISPLAY.

    GS_DISPLAY-DIDMBTR =  GS_DISPLAY-FIDMBTR -
                          GS_DISPLAY-CODMBTR.

    IF GS_DISPLAY-DIDMBTR <> 0.
      ADD_COLOR : 'DIDMBTR' 6.
      GS_DISPLAY-COLOR = GT_CELL_COLOR.
    ENDIF.

    APPEND GS_DISPLAY TO GT_DISPLAY.

    CLEAR: GT_CELL_COLOR[].
    CLEAR  GS_DISPLAY.

  ENDLOOP.

  DELETE GT_DISPLAY WHERE FIDMBTR = 0
                      AND CODMBTR = 0.

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
                  T_TABLE      = GT_DISPLAY ).
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
*& Form READ_TEXT
*&---------------------------------------------------------------------*
FORM READ_TEXT USING     P_OBJNR TYPE J_OBJNR
                CHANGING PS_OUTTAB STRUCTURE ZCOS0300.

  CASE P_OBJNR(2).

    WHEN 'KS'.
      MOVE P_OBJNR+6 TO PS_OUTTAB-KOSTL.

      READ TABLE GT_KOSTL ASSIGNING FIELD-SYMBOL(<LS_KOSTL>)
                   WITH KEY KOSTL = PS_OUTTAB-KOSTL.

      IF SY-SUBRC = 0.
        MOVE <LS_KOSTL>-KTEXT TO PS_OUTTAB-KTEXT.
      ENDIF.

    WHEN 'PR'.
      READ TABLE GT_PRPS ASSIGNING FIELD-SYMBOL(<LS_PRPS>)
         WITH KEY OBJNR = P_OBJNR.

      IF SY-SUBRC = 0.

        MOVE:  <LS_PRPS>-POSID TO PS_OUTTAB-POSID,
               <LS_PRPS>-POST1 TO PS_OUTTAB-POST1.

      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_PF_STATUS
*&---------------------------------------------------------------------*
FORM SET_PF_STATUS .

  DATA: LT_FUNC_LIST TYPE SALV_T_UI_FUNC,
        LS_FUNC_LIST TYPE SALV_S_UI_FUNC.

  DATA: L_TITLE TYPE LVC_TITLE,
        L_LINES TYPE C LENGTH 100,
        L_COUNT TYPE I.

  DATA LV_TEXT TYPE CHAR100.
  DATA LV_GUBUN  TYPE CHAR40.


  GO_ALV->SET_SCREEN_STATUS(
    PFSTATUS      =  'STANDARD'
    REPORT        =  GV_REPID
    SET_FUNCTIONS = GO_ALV->C_FUNCTIONS_ALL ).

  L_COUNT = LINES( GT_DISPLAY ).

  WRITE L_COUNT TO L_LINES.
  CONDENSE L_LINES.

  CONCATENATE TEXT-T00 '(' 'Selected entries :' L_LINES ')'
        INTO L_TITLE SEPARATED BY SPACE.

* set output control : ZEBRA
  GO_DSPSET = GO_ALV->GET_DISPLAY_SETTINGS( ).
  GO_DSPSET->SET_STRIPED_PATTERN( ABAP_TRUE ).
  GO_DSPSET->SET_LIST_HEADER( L_TITLE ).

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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TABLE_SETTINGS
*&---------------------------------------------------------------------*
FORM SET_TABLE_SETTINGS .

  DATA: LR_COLUMNS  TYPE REF TO CL_SALV_COLUMNS_TABLE.
  DATA: LO_AGGRS    TYPE REF TO CL_SALV_AGGREGATIONS.
  DATA: LO_SORT     TYPE REF TO CL_SALV_SORTS.

*-- set column
  TRY.
      LR_COLUMNS = GO_ALV->GET_COLUMNS( ).
      LO_AGGRS   = GO_ALV->GET_AGGREGATIONS( ).
      LO_SORT = GO_ALV->GET_SORTS( ).

*      LR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).
      LR_COLUMNS->SET_COLOR_COLUMN( 'COLOR' ).

    CATCH CX_SALV_DATA_ERROR.
  ENDTRY.

  PERFORM SET_COLUMNS_TECHNICAL USING LR_COLUMNS
                                      LO_AGGRS
                                      LO_SORT.

  TRY.
      LR_COLUMNS->SET_KEY_FIXATION( ABAP_TRUE ).
    CATCH CX_SALV_DATA_ERROR.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_COLUMNS_TECHNICAL
*&---------------------------------------------------------------------*
FORM SET_COLUMNS_TECHNICAL USING IR_COLUMNS TYPE REF TO
                                            CL_SALV_COLUMNS_TABLE
                                  IR_AGG TYPE REF TO
                                            CL_SALV_AGGREGATIONS
                                  IR_SORT TYPE REF TO
                                            CL_SALV_SORTS.

  DATA LR_COLUMN  TYPE REF TO CL_SALV_COLUMN_TABLE.

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

        IF <COLUMN_REF>-COLUMNNAME CP '*DMBTR'.

          CALL METHOD IR_AGG->ADD_AGGREGATION
            EXPORTING
              COLUMNNAME  = <COLUMN_REF>-COLUMNNAME
              AGGREGATION = IF_SALV_C_AGGREGATION=>TOTAL.

        ELSE.

          CALL METHOD IR_SORT->ADD_SORT
            EXPORTING
              COLUMNNAME = <COLUMN_REF>-COLUMNNAME
              SUBTOTAL   = IF_SALV_C_BOOL_SAP=>FALSE.

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

*-- SELECT FIELD 추가
  LR_SELECTIONS = GO_ALV->GET_SELECTIONS( ).
  LR_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>CELL ).

  IR_AGG->SET_AGGREGATION_BEFORE_ITEMS( ).

ENDFORM. " SET_COLUMNS_TECHNICAL
*&---------------------------------------------------------------------*
*& Form HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND USING P_UCOMM TYPE SALV_DE_FUNCTION.

  DATA L_DUMMY TYPE C LENGTH 100.

  CASE P_UCOMM.

    WHEN '&HELP'.
      PERFORM CALL_POPUP_HELP(ZCAR9000)
               USING SY-REPID SY-DYNNR SY-LANGU ''.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM POPUP_TO_CONFIRM USING PV_TITLE
                            PV_QUEST.
  "-- call popup
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR       = PV_TITLE
      TEXT_QUESTION  = PV_QUEST
    IMPORTING
      ANSWER         = GV_ANSWER
    EXCEPTIONS
      TEXT_NOT_FOUND = 1
      OTHERS         = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_COLUMN_TEXT
*&---------------------------------------------------------------------*
FORM SET_COLUMN_TEXT USING P_COLUMNNAME
                      CHANGING P_COLUMN_TEXT.

  DATA L_FIELD TYPE LVC_CFNAME VALUE 'WAERS'.

  TRY.

      CLEAR P_COLUMN_TEXT.

      CASE P_COLUMNNAME.

        WHEN 'KOSTL'.
          P_COLUMN_TEXT = TEXT-C01.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 10 ).

        WHEN 'KTEXT'.
          P_COLUMN_TEXT = TEXT-C02.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'POSID'.
          P_COLUMN_TEXT = TEXT-C03.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 10 ).

        WHEN 'POST1'.
          P_COLUMN_TEXT = TEXT-C04.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 20 ).

        WHEN 'WAERS'.
          P_COLUMN_TEXT = TEXT-C07.

        WHEN 'FIDMBTR'.
          P_COLUMN_TEXT = TEXT-C08.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '7' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'CODMBTR'.
          P_COLUMN_TEXT = TEXT-C09.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '5' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'DIDMBTR'.
          P_COLUMN_TEXT = TEXT-C10.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '3' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

      ENDCASE.

    CATCH CX_SALV_NOT_FOUND.
    CATCH CX_SALV_DATA_ERROR.
  ENDTRY.

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
FORM BUILT_HEADER CHANGING CR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.

  DATA: LR_GRID   TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_GRID_1 TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_LABEL  TYPE REF TO CL_SALV_FORM_LABEL,
        LR_TEXT   TYPE REF TO CL_SALV_FORM_TEXT,
        LV_TEXT   TYPE STRING,
        LV_M1     TYPE C LENGTH 10,
        LV_M2     TYPE C LENGTH 10.

  CREATE OBJECT LR_GRID.

  LR_GRID_1 = LR_GRID->CREATE_GRID(
                ROW    = 2
                COLUMN = 1 ).

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW    = 1
    COLUMN = 1
    TEXT    = TEXT-E03
    TOOLTIP = TEXT-E03 ).

  LV_M1 = PA_GJAHR && '.' && PA_SPERL+1(2).
  LV_TEXT = LV_M1.

  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW    = 1
    COLUMN = 2
    TEXT    = LV_TEXT
    TOOLTIP = LV_TEXT ).

  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW    = 2
    COLUMN = 1
    TEXT    = TEXT-T03
    TOOLTIP = TEXT-T03 ).

  CONCATENATE PA_VERSN '(' PA_VTXT ')' INTO LV_TEXT
  SEPARATED BY SPACE.

  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW    = 2
    COLUMN = 2
    TEXT    = LV_TEXT
    TOOLTIP = LV_TEXT ).

  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).

  CR_CONTENT = LR_GRID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM AUTHORITY_CHECK .

  DATA LV_TYPE TYPE C.
  DATA LV_MESSAGE TYPE BAPI_MSG.

  DATA: LT_0070  LIKE TABLE OF ZCAS0070,
        LS_0070  LIKE ZCAS0070,
        LV_CLASS TYPE ZCAT0031-CD_CLASS,
        LV_CODE  TYPE ZCAT0031-CD_CODE.

  LV_CLASS = 'CASUSR'.
  LV_CODE  = SY-UNAME.

  "__ SUPER USER ID 체크
  PERFORM CALL_F4_VALUES(ZCAR9000) TABLES LT_0070
                                    USING LV_CLASS LV_CODE LS_0070.
  IF LT_0070[] IS NOT INITIAL.
    EXIT.
  ELSE.
    LV_CLASS = 'CASUCO'.
    LV_CODE  = SY-UNAME.

    "__ SUPER USER ID 체크
    PERFORM CALL_F4_VALUES(ZCAR9000) TABLES LT_0070
                                      USING LV_CLASS LV_CODE LS_0070.
    IF LT_0070[] IS NOT INITIAL.
      EXIT.
    ENDIF.
  ENDIF.

  IF SO_BUKRS[] IS INITIAL.
    MESSAGE S000 WITH TEXT-E12 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
    EXPORTING
      I_MODULE    = 'CO'
    IMPORTING
      E_TYPE      = LV_TYPE
      E_MESSAGE   = LV_MESSAGE
    TABLES
      IT_BUKRS_CO = SO_BUKRS[].

  IF LV_TYPE = 'E'.
    MESSAGE S000 WITH LV_MESSAGE DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM SCRFIELDS_FUNCTXT .

  GS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  GS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = GS_FUNTXT.

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

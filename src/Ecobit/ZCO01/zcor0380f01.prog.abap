*&---------------------------------------------------------------------*
*& Include          ZCOR0380F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITAIL
*&---------------------------------------------------------------------*
FORM INITAIL .

  GV_REPID = SY-REPID.

  SELECT SINGLE BEZEI, WAERS INTO (@PA_KTXT, @GV_WAERS)
    FROM TKA01
   WHERE KOKRS = '1000'.

  "__ 20191223 BSGSM_FCM ADD default cac
  SET PARAMETER ID 'CAC' FIELD PA_KOKRS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_B1
*&---------------------------------------------------------------------*
FORM CHECK_B1 .

  SELECT SINGLE BUTXT INTO @PA_BUTXT
    FROM T001
   WHERE BUKRS = @PA_BUKRS.

  IF SY-SUBRC <> 0.
    SET CURSOR FIELD 'PA_BUKRS'.
    MESSAGE E027  WITH TEXT-002.
  ENDIF.

  SELECT SINGLE NAME1 INTO @PA_NAME1
    FROM T001W
   WHERE WERKS = @PA_WERKS.

  IF SY-SUBRC <> 0.
    SET CURSOR FIELD 'PA_WERKS'.
    MESSAGE E027  WITH TEXT-002.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATA_GET
*&---------------------------------------------------------------------*
FORM DATA_GET .

  CLEAR: GT_OUTTAB[], GS_OUTTAB.
  CLEAR: GT_ZCOT1240, GT_ZCOT1240[].

  DATA: BEGIN OF LT_ZMMT0320 OCCURS 0,
          BWART     TYPE BWART,
          ZSAKNR    TYPE ZESAKNR,
          ZSAKNRTXT TYPE CHAR20,
          ZKSTAR    TYPE ZEKSTAR,
          ZKSTARTXT TYPE CHAR20,
        END OF LT_ZMMT0320.

  DATA: BEGIN OF LT_ZCOT1230 OCCURS 0,
          POSID TYPE PS_POSID,
          SAKNR TYPE SAKNR,
          TXT20 TYPE CHAR20,
        END OF LT_ZCOT1230.

  SELECT A~BUKRS, C~BUTXT, A~WERKS, D~NAME1, H~MTART,
         A~MATNR, E~MAKTX, A~BWART, G~POSID,
         G~POST1, A~SHKZG, A~MEINS,
         SUM( A~MENGE ) AS MENGE, B~NETPR, B~WAERS
    FROM MATDOC AS A
   INNER JOIN MARA AS H
      ON A~MATNR = H~MATNR
   LEFT JOIN ZCOT1220 AS B
      ON A~WERKS = B~WERKS
     AND A~MATNR = B~MATNR
    LEFT JOIN T001 AS C
      ON A~BUKRS = C~BUKRS
    LEFT JOIN T001W AS D
      ON A~WERKS = D~WERKS
    LEFT JOIN MAKT AS E
      ON A~MATNR = E~MATNR
     AND E~SPRAS = @SY-LANGU
    LEFT JOIN PRPS AS G
      ON A~PS_PSP_PNR = G~PSPNR
   WHERE A~BUKRS           = @PA_BUKRS
     AND A~WERKS           = @PA_WERKS
     AND A~YEARMONTH_BUDAT = @PA_MONTH
     AND H~MTART           = @PA_MTART
     AND A~BWART           IN ( SELECT DISTINCT
                                        BWART FROM ZMMT0320
                                 WHERE ZKSTAR IS NOT INITIAL )
   GROUP BY A~BUKRS, C~BUTXT, A~WERKS, D~NAME1, H~MTART,
            A~MATNR, E~MAKTX, A~BWART, G~POSID,
            G~POST1, A~SHKZG, A~MEINS, B~NETPR, B~WAERS
    INTO CORRESPONDING FIELDS OF TABLE @GT_OUTTAB.

  IF SY-SUBRC <> 0.
    MESSAGE S004.
    STOP.
  ENDIF.

  SELECT DISTINCT
         A~BWART, A~ZSAKNR, B~TXT20 AS ZSAKNRTXT,
         A~ZKSTAR, C~TXT20 AS ZKSTARTXT
    FROM ZMMT0320 AS A
    LEFT JOIN SKAT AS B
      ON A~ZSAKNR = B~SAKNR
     AND B~SPRAS  = @SY-LANGU
     AND B~KTOPL  = @GC_KTOPL
    LEFT JOIN SKAT AS C
      ON A~ZKSTAR = C~SAKNR
     AND C~SPRAS  = @SY-LANGU
     AND C~KTOPL  = @GC_KTOPL
    INTO TABLE @LT_ZMMT0320
   WHERE A~ZKSTAR <> @SPACE.

  SELECT A~POSID, A~SAKNR, B~TXT20
    FROM ZCOT1230 AS A
   INNER JOIN SKAT AS B
      ON A~SAKNR = B~SAKNR
     AND B~SPRAS = @SY-LANGU
     AND B~KTOPL = @GC_KTOPL
    INTO TABLE @LT_ZCOT1230
   WHERE A~BUKRS = @PA_BUKRS.

  SELECT * FROM ZCOT1240
    INTO TABLE @GT_ZCOT1240
   WHERE BUKRS = @PA_BUKRS
     AND SPMON = @PA_MONTH
     AND WERKS = @PA_WERKS.

  SELECT * FROM T156T
    INTO TABLE @DATA(LT_T156T)
   WHERE SPRAS = @SY-LANGU
     AND SOBKZ = @SPACE.

  LOOP AT GT_OUTTAB INTO GS_OUTTAB.

    GS_OUTTAB-DMBTR = GS_OUTTAB-MENGE * GS_OUTTAB-NETPR.

*-- G/L 계정지정
    READ TABLE LT_ZMMT0320  WITH KEY BWART = GS_OUTTAB-BWART.

    IF SY-SUBRC = 0.
      MOVE: LT_ZMMT0320-ZKSTAR    TO GS_OUTTAB-SAKNR,
            LT_ZMMT0320-ZKSTARTXT TO GS_OUTTAB-TXT20.

    ELSE.
      READ TABLE LT_ZCOT1230
          WITH KEY POSID = GS_OUTTAB-POSID.
      IF SY-SUBRC = 0.
        MOVE: LT_ZCOT1230-SAKNR TO GS_OUTTAB-SAKNR,
              LT_ZCOT1230-TXT20 TO GS_OUTTAB-TXT20.
      ENDIF.

    ENDIF.

    READ TABLE GT_ZCOT1240 WITH KEY BUKRS = GS_OUTTAB-BUKRS
                                    WERKS = GS_OUTTAB-WERKS
                                    MATNR = GS_OUTTAB-MATNR
                                    SHKZG = GS_OUTTAB-SHKZG
                                    BWART = GS_OUTTAB-BWART
                                    POSID = GS_OUTTAB-POSID.

    IF SY-SUBRC = 0.
      MOVE: GT_ZCOT1240-GJAHR TO GS_OUTTAB-GJAHR,
            GT_ZCOT1240-BELNR TO GS_OUTTAB-BELNR.
    ENDIF.

    IF GS_OUTTAB-SAKNR IS INITIAL.
      GS_OUTTAB-ICON    = ICON_LED_RED.
      GS_OUTTAB-MESSAGE = TEXT-E07.
    ENDIF.

    READ TABLE LT_T156T INTO DATA(LS_T156T)
            WITH KEY BWART = GS_OUTTAB-BWART.

    IF SY-SUBRC = 0.
      MOVE LS_T156T-BTEXT TO GS_OUTTAB-BTEXT.
    ENDIF.

    MODIFY GT_OUTTAB FROM GS_OUTTAB.

  ENDLOOP.

  MESSAGE S005.

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
*& Form HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND  USING P_UCOMM TYPE SALV_DE_FUNCTION.

  DATA L_DUMMY TYPE C LENGTH 100.

  CASE P_UCOMM.

    WHEN '&HELP'.
      PERFORM CALL_POPUP_HELP(ZCAR9000)
               USING SY-REPID SY-DYNNR SY-LANGU ''.

    WHEN '&POST'.
      PERFORM ERROR_CHECK.
      CHECK GV_EXIT IS INITIAL.
      PERFORM POPUP_TO_CONFIRM USING TEXT-PT1
                                     TEXT-QT1.
      CHECK GV_ANSWER EQ '1'.

      PERFORM DOC_POST.

      MESSAGE S000 WITH TEXT-S01.

      CALL METHOD GO_ALV->REFRESH
        EXPORTING
          REFRESH_MODE = IF_SALV_C_REFRESH=>FULL.

    WHEN '&REVERSE'.
      PERFORM ERROR_CHECK_REVERSE.
      CHECK GV_EXIT IS INITIAL.
      PERFORM POPUP_TO_CONFIRM USING TEXT-PT1
                                     TEXT-QT2.
      CHECK GV_ANSWER EQ '1'.

      PERFORM REVERSE_POST.

      CALL METHOD GO_ALV->REFRESH
        EXPORTING
          REFRESH_MODE = IF_SALV_C_REFRESH=>FULL.

      MESSAGE S000 WITH TEXT-S05.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM SET_SCREEN .

  LOOP AT SCREEN.

    IF SCREEN-NAME = 'PA_KOKRS' OR
       SCREEN-NAME = 'PA_MTART'.

      SCREEN-INPUT = 0.
      MODIFY SCREEN.

    ENDIF.

  ENDLOOP.

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

  GO_ALV->SET_SCREEN_STATUS(
    PFSTATUS      =  'STANDARD'
    REPORT        =  GV_REPID
    SET_FUNCTIONS = GO_ALV->C_FUNCTIONS_ALL ).

  L_COUNT = LINES( GT_OUTTAB ).
  LV_TEXT = TEXT-T00.

  WRITE L_COUNT TO L_LINES.
  CONDENSE L_LINES.

  CONCATENATE LV_TEXT '(' 'Selected entries :' L_LINES ')'
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
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_COLUMNS_TECHNICAL
*&---------------------------------------------------------------------*
FORM SET_COLUMNS_TECHNICAL USING IR_COLUMNS TYPE REF TO
                                            CL_SALV_COLUMNS_TABLE
                                  IR_AGG TYPE REF TO
                                            CL_SALV_AGGREGATIONS.

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

      ENDLOOP.

    CATCH CX_SALV_NOT_FOUND.
    CATCH CX_SALV_DATA_ERROR.
    CATCH CX_SALV_EXISTING.

  ENDTRY.

  TRY.
      LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'STATUS' ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

*-- SELECT FIELD 추가
  LR_SELECTIONS = GO_ALV->GET_SELECTIONS( ).
  LR_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>CELL ).

  IR_AGG->SET_AGGREGATION_BEFORE_ITEMS( ).

ENDFORM. " SET_COLUMNS_TECHNICAL
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

  DATA: LV_SMONTH TYPE N LENGTH 2,
        LV_EMONTH TYPE N LENGTH 2,
        LV_MONTH  TYPE N LENGTH 2.

  DATA L_FIELD TYPE LVC_CFNAME VALUE 'WAERS'.

  TRY.

      CLEAR P_COLUMN_TEXT.

      CASE P_COLUMNNAME.

        WHEN 'ICON'.
          P_COLUMN_TEXT = TEXT-C01.

          GR_COLUMN->SET_ICON( IF_SALV_C_BOOL_SAP=>TRUE ).
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'NAME1'.
          P_COLUMN_TEXT = TEXT-C02.

        WHEN 'DMBTR'.
          P_COLUMN_TEXT = TEXT-C03.

        WHEN 'POSID'.
          P_COLUMN_TEXT = TEXT-C04.

        WHEN 'POST1'.
          P_COLUMN_TEXT = TEXT-C05.
*
*        WHEN 'KSTAR'.
*          P_COLUMN_TEXT = TEXT-C04.
*
*        WHEN 'KTEXT2'.
*          P_COLUMN_TEXT = TEXT-C05.
*
**        WHEN 'WAERS'.
**          P_COLUMN_TEXT = TEXT-C10.
*
*        WHEN 'SUM'.
*          P_COLUMN_TEXT = TEXT-C06.
*          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).
*
*          _SET_COLOR '3' '0' '0'.
*          GR_COLUMN->SET_COLOR( GS_COLOR ).
*
        WHEN 'MESSAGE'.
          P_COLUMN_TEXT = TEXT-C07.

      ENDCASE.

*      IF P_COLUMNNAME CP 'M*' AND P_COLUMNNAME <> 'MESSAGE'.
*
*        P_COLUMN_TEXT = P_COLUMNNAME+1(2).
*
*        SHIFT P_COLUMN_TEXT LEFT DELETING LEADING '0'.
*        CONCATENATE P_COLUMN_TEXT TEXT-M01 INTO P_COLUMN_TEXT.
*
*        LV_MONTH = P_COLUMNNAME+1(2).
*
*        IF LV_SMONTH > LV_MONTH OR
*           LV_EMONTH < LV_MONTH.
*
*          GR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
*        ENDIF.
*
*        GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).
*
*      ENDIF.

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
                ROW    = 3
                COLUMN = 1 ).

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW    = 1
    COLUMN = 1
    TEXT    = TEXT-I01
    TOOLTIP = TEXT-I01 ).

  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW    = 1
    COLUMN = 2
    TEXT    = PA_MONTH
    TOOLTIP = PA_MONTH ).

  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW    = 2
    COLUMN = 1
    TEXT    = TEXT-I02
    TOOLTIP = TEXT-I02 ).

  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW    = 2
    COLUMN = 2
    TEXT    = PA_BUTXT
    TOOLTIP = PA_BUTXT ).

  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW    = 3
    COLUMN = 1
    TEXT    = TEXT-I03
    TOOLTIP = TEXT-I03 ).

  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW    = 3
    COLUMN = 2
    TEXT    = PA_NAME1
    TOOLTIP = PA_NAME1 ).

  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).

  CR_CONTENT = LR_GRID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_P_MONTH
*&---------------------------------------------------------------------*
FORM F4_P_MONTH .

  DATA: BEGIN OF MF_DYNPFIELDS OCCURS 1.
          INCLUDE STRUCTURE DYNPREAD.
        DATA: END   OF MF_DYNPFIELDS.
  DATA: MF_RETURNCODE LIKE SY-SUBRC,
        MF_MONAT      LIKE ISELLIST-MONTH,
        MF_HLP_REPID  LIKE SY-REPID.
  FIELD-SYMBOLS: <MF_FELD>.

  GET CURSOR FIELD MF_DYNPFIELDS-FIELDNAME.
  APPEND MF_DYNPFIELDS.
  MF_HLP_REPID = SY-REPID.
  DO 2 TIMES.
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        DYNAME               = MF_HLP_REPID
        DYNUMB               = SY-DYNNR
      TABLES
        DYNPFIELDS           = MF_DYNPFIELDS
      EXCEPTIONS
        INVALID_ABAPWORKAREA = 01
        INVALID_DYNPROFIELD  = 02
        INVALID_DYNPRONAME   = 03
        INVALID_DYNPRONUMMER = 04
        INVALID_REQUEST      = 05
        NO_FIELDDESCRIPTION  = 06
        UNDEFIND_ERROR       = 07.
    IF SY-SUBRC = 3.
      MF_HLP_REPID = 'SAPLALDB'.
    ELSE.
      READ TABLE MF_DYNPFIELDS INDEX 1.
      TRANSLATE MF_DYNPFIELDS-FIELDVALUE USING '_ '.
      EXIT.
    ENDIF.
  ENDDO.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'CONVERSION_EXIT_PERI_INPUT'
      EXPORTING
        INPUT  = MF_DYNPFIELDS-FIELDVALUE
      IMPORTING
        OUTPUT = MF_MONAT.
    IF MF_MONAT IS INITIAL.
      MF_MONAT = SY-DATLO(6).
    ENDIF.
    CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
      EXPORTING
        ACTUAL_MONTH               = MF_MONAT
      IMPORTING
        SELECTED_MONTH             = MF_MONAT
        RETURN_CODE                = MF_RETURNCODE
      EXCEPTIONS
        FACTORY_CALENDAR_NOT_FOUND = 01
        HOLIDAY_CALENDAR_NOT_FOUND = 02
        MONTH_NOT_FOUND            = 03.
    IF SY-SUBRC = 0 AND MF_RETURNCODE = 0.
      CALL FUNCTION 'CONVERSION_EXIT_PERI_OUTPUT'
        EXPORTING
          INPUT  = MF_MONAT
        IMPORTING
          OUTPUT = MF_DYNPFIELDS-FIELDVALUE.
**** 날짜 . 없애기 추가
*      REPLACE '.' IN MF_DYNPFIELDS-FIELDVALUE  WITH ''.

      CONDENSE MF_DYNPFIELDS-FIELDVALUE.
******

      COLLECT MF_DYNPFIELDS.
      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          DYNAME               = MF_HLP_REPID
          DYNUMB               = SY-DYNNR
        TABLES
          DYNPFIELDS           = MF_DYNPFIELDS
        EXCEPTIONS
          INVALID_ABAPWORKAREA = 01
          INVALID_DYNPROFIELD  = 02
          INVALID_DYNPRONAME   = 03
          INVALID_DYNPRONUMMER = 04
          INVALID_REQUEST      = 05
          NO_FIELDDESCRIPTION  = 06
          UNDEFIND_ERROR       = 07.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ERROR_CHECK
*&---------------------------------------------------------------------*
FORM ERROR_CHECK .

  DATA LR_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS.

  CLEAR: GT_ROWS, GV_EXIT.

  LR_SELECTIONS = GO_ALV->GET_SELECTIONS( ).
  GT_ROWS = LR_SELECTIONS->GET_SELECTED_ROWS( ).

  IF GT_ROWS IS INITIAL.
    MESSAGE S015 DISPLAY LIKE 'E'.
    GV_EXIT = ABAP_TRUE.
    EXIT.
  ENDIF.

  LOOP AT GT_ROWS INTO GS_ROW.

    READ TABLE GT_OUTTAB INTO DATA(LS_OUTTAB) INDEX GS_ROW.

    IF SY-SUBRC = 0.

      IF LS_OUTTAB-BELNR IS NOT INITIAL.
        MESSAGE S000 WITH TEXT-E05 DISPLAY LIKE 'E'.
        GV_EXIT = ABAP_TRUE.
        EXIT.
      ENDIF.

      IF LS_OUTTAB-SAKNR IS INITIAL.
        MESSAGE S000 WITH TEXT-E07 DISPLAY LIKE 'E'.
        GV_EXIT = ABAP_TRUE.
        EXIT.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOC_POST
*&---------------------------------------------------------------------*
FORM DOC_POST .

  DATA LV_ERROR.

  DATA: LV_MESSAGE TYPE STRING.

  DATA LS_ZCOT1240 TYPE ZCOT1240.

  DATA : LV_MSG(100),  LV_CHK(1).
  DATA : I_BELNR TYPE  BELNR_D.
  DATA : ITMNO   TYPE  N LENGTH 4.

  DATA : IBKPF   LIKE  ZFIS0050 OCCURS 0 WITH HEADER LINE,
         IBSEG   LIKE  ZFIS0120 OCCURS 0 WITH HEADER LINE,
         IWITH   LIKE  ZFIS0070 OCCURS 0 WITH HEADER LINE,
         T_FIMSG LIKE  ZFIS0080 OCCURS 0 WITH HEADER LINE,
         ITTAX   LIKE  ZFIS0090 OCCURS 0 WITH HEADER LINE.

  DATA LV_DATE TYPE DATS.

  LV_DATE = PA_MONTH && '01'.

  CALL FUNCTION 'SG_PS_GET_LAST_DAY_OF_MONTH'
    EXPORTING
      DAY_IN            = LV_DATE
    IMPORTING
      LAST_DAY_OF_MONTH = LV_DATE
    EXCEPTIONS
      DAY_IN_NOT_VALID  = 1
      OTHERS            = 2.

  LOOP AT GT_ROWS INTO GS_ROW.

    CLEAR :   IBKPF,IBSEG,IWITH,T_FIMSG,ITTAX,ITMNO, I_BELNR,
              LS_ZCOT1240.

    REFRESH : IBKPF,IBSEG,IWITH,T_FIMSG,ITTAX.

    READ TABLE GT_OUTTAB INTO GS_OUTTAB INDEX GS_ROW.

    IF SY-SUBRC = 0.

      IBKPF-SERIAL = '0001'.    "순번
      IBKPF-TCODE  = 'FB01'.    "트랜잭션 코드
      IBKPF-BLART  = 'WZ'.      "전표유형
      IBKPF-BUKRS  = PA_BUKRS.  "회사코드
      IBKPF-BLDAT  = LV_DATE.   "증빙일자
      IBKPF-BUDAT  = LV_DATE.   "전기일자
      IBKPF-WAERS  = 'KRW'.     "거래통화
      IBKPF-BKTXT  = TEXT-H01.  "전표 헤더텍스트
      IBKPF-XBLNR  = SPACE.     "참조전표번호
      APPEND IBKPF. CLEAR IBKPF.

      DO 2 TIMES.
        ITMNO = ITMNO + 1.
        IBSEG-SERIAL = '0001'. "일련번호
        IBSEG-ITMNO  = ITMNO.  "라인 일련번호
        IBSEG-SGTXT  = TEXT-T00.

        CASE ITMNO.

          WHEN 1. " 제품

            CASE GS_OUTTAB-SHKZG.
              WHEN 'S'.
                IBSEG-NEWBS = '40'.         "전기키
              WHEN 'H'.
                IBSEG-NEWBS = '50'.         "전기키
            ENDCASE.

            IBSEG-HKONT = '0101302001'.    "GL 계정
            WRITE GS_OUTTAB-DMBTR TO IBSEG-WRBTR
                   CURRENCY GS_OUTTAB-WAERS LEFT-JUSTIFIED.

          WHEN 2.  "상대계정

            CASE GS_OUTTAB-SHKZG.
              WHEN 'S'.
                IBSEG-NEWBS = '50'.         "전기키
              WHEN 'H'.
                IBSEG-NEWBS = '40'.         "전기키
            ENDCASE.

            WRITE GS_OUTTAB-DMBTR TO IBSEG-WRBTR
                   CURRENCY GS_OUTTAB-WAERS LEFT-JUSTIFIED.

            IBSEG-HKONT = GS_OUTTAB-SAKNR.    "GL 계정

            CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
              EXPORTING
                INPUT  = GS_OUTTAB-POSID
              IMPORTING
                OUTPUT = IBSEG-PROJK.

        ENDCASE.

        APPEND IBSEG.
        CLEAR: IBSEG.

      ENDDO.

      CALL FUNCTION 'Z_FI_CREATE_DOC'
        IMPORTING
          E_BELNR         = I_BELNR
        TABLES
          IBKPF           = IBKPF
          IBSEG           = IBSEG
          IWITH           = IWITH
          T_FIMSG         = T_FIMSG
          ITTAX           = ITTAX
        EXCEPTIONS
          NO_POSTING_DATA = 1
          ERROR_MODE      = 2
          POSTING_ERROR   = 3
          OTHERS          = 4.

      IF I_BELNR IS NOT INITIAL.

        GS_OUTTAB-ICON    = ICON_LED_GREEN.
        GS_OUTTAB-BELNR   = I_BELNR.
        GS_OUTTAB-GJAHR   = PA_MONTH(4).
        GS_OUTTAB-MESSAGE = 'Success!'.

        MODIFY GT_OUTTAB FROM GS_OUTTAB INDEX GS_ROW.

        MOVE-CORRESPONDING GS_OUTTAB TO LS_ZCOT1240.

        MOVE: PA_MONTH    TO LS_ZCOT1240-SPMON,
              I_BELNR     TO LS_ZCOT1240-BELNR,
              PA_MONTH(4) TO LS_ZCOT1240-GJAHR.

        TRY .

            INSERT  ZCOT1240 FROM LS_ZCOT1240.
            COMMIT WORK.

          CATCH CX_SY_SQL_ERROR INTO DATA(LR_ERROR).

            ROLLBACK WORK.

            LV_MESSAGE = LR_ERROR->GET_TEXT( ).
            MESSAGE S001 WITH LV_MESSAGE DISPLAY LIKE 'E'.
            EXIT.

        ENDTRY.

      ELSE.

        DATA(LV_LINE) = LINES( T_FIMSG ).

        READ TABLE T_FIMSG INDEX LV_LINE.

        PERFORM GET_TEXT  USING   T_FIMSG-MSGID
                                  T_FIMSG-MSGNO
                                  T_FIMSG-MSGV1
                                  T_FIMSG-MSGV2
                                  T_FIMSG-MSGV3
                                  T_FIMSG-MSGV4
                         CHANGING LV_MSG.

        GS_OUTTAB-ICON    = ICON_LED_RED.
        GS_OUTTAB-MESSAGE = LV_MSG.

        MODIFY GT_OUTTAB FROM GS_OUTTAB INDEX GS_ROW.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_TEXT
*&---------------------------------------------------------------------*
FORM GET_TEXT  USING   P_MSGID P_MSGNO
                       P_MSGV1 P_MSGV2
                       P_MSGV3 P_MSGV4
                CHANGING  P_MSG.

  DATA: LD_MESSAGE LIKE SY-LISEL.
  DATA: LV_MSGID LIKE SY-MSGID,
        LV_MSGNO LIKE SY-MSGNO,
        LV_MSGV1 LIKE SY-MSGV1,
        LV_MSGV2 LIKE SY-MSGV2,
        LV_MSGV3 LIKE SY-MSGV3,
        LV_MSGV4 LIKE SY-MSGV4.

  CLEAR P_MSG.

  LV_MSGID = P_MSGID.
  LV_MSGNO = P_MSGNO.
  LV_MSGV1 = P_MSGV1.
  LV_MSGV2 = P_MSGV2.
  LV_MSGV3 = P_MSGV3.
  LV_MSGV4 = P_MSGV4.

  CALL FUNCTION 'RPY_MESSAGE_COMPOSE'
    EXPORTING
      MESSAGE_ID        = LV_MSGID
      MESSAGE_NUMBER    = LV_MSGNO
      MESSAGE_VAR1      = LV_MSGV1
      MESSAGE_VAR2      = LV_MSGV2
      MESSAGE_VAR3      = LV_MSGV3
      MESSAGE_VAR4      = LV_MSGV4
    IMPORTING
      MESSAGE_TEXT      = LD_MESSAGE
    EXCEPTIONS
      MESSAGE_NOT_FOUND = 1.

  P_MSG = LD_MESSAGE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ERROR_CHECK_REVERSE
*&---------------------------------------------------------------------*
FORM ERROR_CHECK_REVERSE .

  DATA LR_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS.

  CLEAR: GT_ROWS, GV_EXIT.

  LR_SELECTIONS = GO_ALV->GET_SELECTIONS( ).
  GT_ROWS = LR_SELECTIONS->GET_SELECTED_ROWS( ).

  IF GT_ROWS IS INITIAL.
    MESSAGE S015 DISPLAY LIKE 'E'.
    GV_EXIT = ABAP_TRUE.
    EXIT.
  ENDIF.

  LOOP AT GT_ROWS INTO GS_ROW.

    READ TABLE GT_OUTTAB INTO DATA(LS_OUTTAB) INDEX GS_ROW.

    IF SY-SUBRC = 0.

      IF LS_OUTTAB-BELNR IS INITIAL.
        MESSAGE S000 WITH TEXT-E06 DISPLAY LIKE 'E'.
        GV_EXIT = ABAP_TRUE.
        EXIT.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REVERSE_POST
*&---------------------------------------------------------------------*
FORM REVERSE_POST .

  DATA : LV_GJAHR   TYPE GJAHR,
         LV_BELNR   TYPE BELNR_D,
         LV_ERROR   TYPE C,
         LV_ERR_TXT TYPE NATXT.

  DATA LV_MESSAGE TYPE STRING.

  DATA LV_DATE TYPE DATS.
  DATA LS_ZCOT1240 TYPE ZCOT1240.

  DATA : I_BELNR TYPE  BELNR_D.

  LV_DATE = PA_MONTH && '01'.

  CALL FUNCTION 'SG_PS_GET_LAST_DAY_OF_MONTH'
    EXPORTING
      DAY_IN            = LV_DATE
    IMPORTING
      LAST_DAY_OF_MONTH = LV_DATE
    EXCEPTIONS
      DAY_IN_NOT_VALID  = 1
      OTHERS            = 2.

  LOOP AT GT_ROWS INTO GS_ROW.

    READ TABLE GT_OUTTAB INTO GS_OUTTAB INDEX GS_ROW.

    IF SY-SUBRC = 0.

      CLEAR: LV_BELNR, LV_ERROR, LV_ERR_TXT.

      CALL FUNCTION 'Z_FI_CANCEL_DOC'
        EXPORTING
          I_BUKRS   = GS_OUTTAB-BUKRS
          I_BELNR   = GS_OUTTAB-BELNR
          I_GJAHR   = GS_OUTTAB-GJAHR
          I_BUDAT   = LV_DATE
          I_STGRD   = '03'
        IMPORTING
          E_RESULT  = LV_ERROR
          E_RBELNR  = LV_BELNR
          E_ERR_TXT = LV_ERR_TXT.

      IF LV_BELNR IS NOT INITIAL.

        TRY .

            DELETE FROM ZCOT1240 WHERE BUKRS = GS_OUTTAB-BUKRS
                                   AND BELNR = GS_OUTTAB-BELNR
                                   AND GJAHR = GS_OUTTAB-GJAHR.

            COMMIT WORK.

            GS_OUTTAB-ICON    = ICON_LED_GREEN.
            GS_OUTTAB-MESSAGE = TEXT-S03.
            CLEAR: GS_OUTTAB-GJAHR, GS_OUTTAB-BELNR.

            MODIFY GT_OUTTAB FROM GS_OUTTAB INDEX GS_ROW.

          CATCH CX_SY_SQL_ERROR INTO DATA(LR_ERROR).

            ROLLBACK WORK.

            LV_MESSAGE = LR_ERROR->GET_TEXT( ).
            MESSAGE S001 WITH LV_MESSAGE DISPLAY LIKE 'E'.
            EXIT.

        ENDTRY.

      ELSE.

        GS_OUTTAB-ICON    = ICON_LED_RED.
        GS_OUTTAB-MESSAGE = LV_ERR_TXT.
        MODIFY GT_OUTTAB FROM GS_OUTTAB INDEX GS_ROW.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM SCRFIELDS_FUNCTXT .

  GS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  GS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = GS_FUNTXT.

  GS_FUNTXT-ICON_ID   = ICON_CREATE.
  GS_FUNTXT-QUICKINFO = '매출원가 계정지정'.
  GS_FUNTXT-ICON_TEXT = '매출원가 계정지정'.

  SSCRFIELDS-FUNCTXT_02 = GS_FUNTXT.

  GS_FUNTXT-QUICKINFO = '매출원가 제품 단가'.
  GS_FUNTXT-ICON_TEXT = '매출원가 제품 단가'.

  SSCRFIELDS-FUNCTXT_03 = GS_FUNTXT.

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
      CALL TRANSACTION 'ZCOV1230' AND SKIP FIRST SCREEN.

    WHEN 'FC03'.
      CALL TRANSACTION 'ZCOV1220' AND SKIP FIRST SCREEN.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.

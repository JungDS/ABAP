*&---------------------------------------------------------------------*
*& Include          ZCOR0090F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITAIL
*&---------------------------------------------------------------------*
FORM INITAIL .

  GV_REPID = SY-REPID.
  GET PARAMETER ID 'CAC' FIELD PA_KOKRS.

  IF PA_KOKRS IS INITIAL.
    PA_KOKRS = '1000'.
  ENDIF.

  SELECT SINGLE BEZEI INTO @PA_KTXT
    FROM TKA01
   WHERE KOKRS = @PA_KOKRS.

  CASE SY-TCODE.

    WHEN 'ZCOR0091B'.
      GV_MODE = 'S'.
      SY-TITLE = TEXT-T05.
      PA_VERSN = 'P0'.
      PV_TVERS = 'PZ'.

    WHEN 'ZCOR0092B'.
      GV_MODE = 'C'.
      SY-TITLE = TEXT-T06.
      PA_VERSN = '000'.
      PV_TVERS = 'R1'.

    WHEN  OTHERS.
      GV_MODE = 'H'.
      PA_VERSN = 'PZ'.
      PV_TVERS = 'B0'.
      PV_TVERE = 'B1'.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM SET_SCREEN .

  LOOP AT SCREEN.

    IF SCREEN-NAME = 'PA_KOKRS' OR
       SCREEN-NAME = 'P_TGJAHR' OR
       SCREEN-NAME = 'P_HGJAHR' OR
       SCREEN-NAME = 'P_HSPERL' OR
       SCREEN-NAME = 'P_HEPERL' OR
       SCREEN-NAME = 'PA_CHK'   OR
       SCREEN-NAME = 'P_HVERSN' OR
       SCREEN-NAME = 'P_TSPERL' OR
       SCREEN-NAME = 'P_TEPERL'.
      SCREEN-INPUT = 0.
    ENDIF.

    CASE GV_MODE.

      WHEN 'S'.
        IF SCREEN-GROUP1 = 'MG1'.
          SCREEN-ACTIVE = 0.
        ENDIF.

      WHEN 'C'.
        IF SCREEN-NAME = 'PA_VERSN'.
          SCREEN-INPUT = 0.
        ENDIF.

        IF SCREEN-GROUP1 = 'MG1'.
          SCREEN-ACTIVE = 0.
        ENDIF.

      WHEN 'H'.
        IF SCREEN-NAME = 'PA_SPERL' OR
           SCREEN-NAME = 'PA_EPERL'.
          SCREEN-INPUT = 0.
        ENDIF.

    ENDCASE.

    MODIFY SCREEN.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_B1
*&---------------------------------------------------------------------*
FORM CHECK_B1.

  IF PA_SPERL > PA_EPERL.
    SET CURSOR FIELD 'PA_SPERL'.
    MESSAGE E018  WITH TEXT-E03.
  ENDIF.

  IF PA_SPERL > 12 OR
     PA_EPERL > 12.
    SET CURSOR FIELD 'PA_SPERL'.
    MESSAGE E023  WITH TEXT-E03.
  ENDIF.

  SELECT SINGLE VTEXT INTO @PA_VTXT
    FROM TKVS AS A
    LEFT JOIN TKVST AS B
      ON A~VERSI = B~VERSI
     AND B~SPRAS = @SY-LANGU
   WHERE A~VERSI = @PA_VERSN.

  IF SY-SUBRC <> 0.
    SET CURSOR FIELD 'PA_VERSN'.
    MESSAGE E027  WITH TEXT-005.
  ENDIF.

  P_HGJAHR = P_TGJAHR = PA_GJAHR.
  P_TSPERL = PA_SPERL.
  P_TEPERL = PA_EPERL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_B3
*&---------------------------------------------------------------------*
FORM CHECK_B3 .

  SELECT SINGLE VTEXT INTO @P_HVTXT
  FROM TKVS AS A
  LEFT JOIN TKVST AS B
    ON A~VERSI = B~VERSI
   AND B~SPRAS = @SY-LANGU
 WHERE A~VERSI = @P_HVERSN.

  IF SY-SUBRC <> 0.
    SET CURSOR FIELD 'P_HVERSN'.
    MESSAGE E027  WITH TEXT-005.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_B2
*&---------------------------------------------------------------------*
FORM CHECK_B2 .


  CLEAR: R_VERNS, R_VERNS[].

  IF PV_TVERS IS INITIAL AND
     PV_TVERE IS INITIAL.
    MESSAGE E026 WITH TEXT-E01.
  ENDIF.

  IF PV_TVERE < PV_TVERS AND
     PV_TVERE IS NOT INITIAL.
    SET CURSOR FIELD 'PV_TVERS'.
    MESSAGE E018  WITH TEXT-E01.
  ENDIF.

  SELECT SINGLE VTEXT INTO @DATA(LV_T1)
    FROM TKVS AS A
    LEFT JOIN TKVST AS B
      ON A~VERSI = B~VERSI
     AND B~SPRAS = @SY-LANGU
   WHERE A~VERSI = @PV_TVERS.

  IF SY-SUBRC <> 0.
    SET CURSOR FIELD 'PV_TVERS'.
    MESSAGE E027  WITH TEXT-005.
  ENDIF.

  IF GV_MODE = 'H' AND PV_TVERS NP 'B*'.
    SET CURSOR FIELD 'PV_TVERS'.
    MESSAGE E000  WITH TEXT-E06.

  ELSEIF ( GV_MODE = 'S' OR GV_MODE = 'C' ) AND
            PV_TVERS CP 'B*'.

    SET CURSOR FIELD 'PV_TVERS'.
    MESSAGE E000  WITH TEXT-E07.

  ENDIF.

  IF PV_TVERE IS NOT INITIAL.

    SELECT SINGLE VTEXT INTO @DATA(LV_T2)
      FROM TKVS AS A
      LEFT JOIN TKVST AS B
        ON A~VERSI = B~VERSI
       AND B~SPRAS = @SY-LANGU
     WHERE A~VERSI = @PV_TVERE.

    IF SY-SUBRC <> 0.
      SET CURSOR FIELD 'PV_TVERE'.
      MESSAGE E027  WITH TEXT-005.
    ENDIF.

    IF GV_MODE = 'H' AND PV_TVERE NP 'B*'.
      SET CURSOR FIELD 'PV_TVERE'.
      MESSAGE E000  WITH TEXT-E06.

    ELSEIF ( GV_MODE = 'S' OR GV_MODE = 'C' ) AND
              PV_TVERE CP 'B*'.

      SET CURSOR FIELD 'PV_TVERE'.
      MESSAGE E000  WITH TEXT-E07.

    ENDIF.

  ENDIF.

  IF PV_TVERE IS INITIAL.

    MOVE: 'I'      TO R_VERNS-SIGN,
          'EQ'     TO R_VERNS-OPTION,
          PV_TVERS TO R_VERNS-LOW.

    APPEND R_VERNS.

  ELSE.

    MOVE: 'I'      TO R_VERNS-SIGN,
          'BT'     TO R_VERNS-OPTION,
          PV_TVERS TO R_VERNS-LOW,
          PV_TVERE TO R_VERNS-HIGH.

    APPEND R_VERNS.

  ENDIF.

  SELECT * FROM TKVS
    INTO TABLE @GT_TKVS
   WHERE VERSI IN @R_VERNS.

  CASE GV_MODE.

    WHEN 'S' OR 'C'.

      SELECT SINGLE VERSI INTO @DATA(LV_VERSN)
        FROM TKVS
        WHERE VERSI IN @R_VERNS
          AND VERSI LIKE 'B%'.

      IF SY-SUBRC = 0.
        SET CURSOR FIELD 'PV_TVERES'.
        MESSAGE E000  WITH TEXT-E07.
      ENDIF.

    WHEN 'H'.

      SELECT SINGLE VERSI INTO @LV_VERSN
        FROM TKVS
        WHERE VERSI IN @R_VERNS
          AND VERSI NOT LIKE 'B%'.

      IF SY-SUBRC = 0.
        SET CURSOR FIELD 'PV_TVERES'.
        MESSAGE E000  WITH TEXT-E06.
      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATA_GET
*&---------------------------------------------------------------------*
FORM DATA_GET .

  DATA LV_FIELDNAME TYPE FIELDNAME.
  DATA LV_FYEAR     TYPE N LENGTH 7.

  DATA LV_RFIELD    TYPE FIELDNAME.
  DATA LV_DIFF.

  DATA OTAB TYPE ABAP_SORTORDER_TAB .

  FIELD-SYMBOLS: <FS_S>     TYPE ANY,
                 <FS_R>     TYPE ANY,
                 <FS_D>     TYPE ANY,
                 <FS_D2>    TYPE ANY,
                 <FS_OBJNR> TYPE J_OBJNR,
                 <FS_KSTAR> TYPE KSTAR,
                 <FS_HSL>   TYPE ANY,
                 <FS_HIS>   TYPE ANY.

  CLEAR: <GT_TABLE> .
  CLEAR: GT_COSP, GT_COSP[].

  SELECT SINGLE WAERS INTO @DATA(LV_WAERS)
    FROM TKA01
   WHERE KOKRS = @PA_KOKRS.

  CASE GV_MODE.

    WHEN 'C'.

      SELECT A~VERSN, A~OBJNR, A~KSTAR, B~KTEXT AS KSTXT,
             SUM( A~WKG001 ) AS WKG001,
             SUM( A~WKG002 ) AS WKG002,
             SUM( A~WKG003 ) AS WKG003,
             SUM( A~WKG004 ) AS WKG004,
             SUM( A~WKG005 ) AS WKG005,
             SUM( A~WKG006 ) AS WKG006,
             SUM( A~WKG007 ) AS WKG007,
             SUM( A~WKG008 ) AS WKG008,
             SUM( A~WKG009 ) AS WKG009,
             SUM( A~WKG010 ) AS WKG010,
             SUM( A~WKG011 ) AS WKG011,
             SUM( A~WKG012 ) AS WKG012

        FROM COSP AS A
        LEFT JOIN CSKU AS B
          ON A~KSTAR = B~KSTAR
         AND B~KTOPL = @GC_KTOPL
         AND B~SPRAS = @SY-LANGU
       WHERE A~LEDNR = '00'
         AND A~GJAHR = @PA_GJAHR
         AND ( ( A~WRTTP = '04' AND A~VERSN = @PA_VERSN ) OR "실적
               ( A~WRTTP = '01' AND A~VERSN IN @R_VERNS ) )  "계획
         AND A~KSTAR IN ( SELECT DISTINCT SAKNR
                            FROM SKA1
                           WHERE KTOPL = @GC_KTOPL
                             AND ( KTOKS = 'PL' OR
                                   KTOKS = 'SECC' ) )

        GROUP BY A~VERSN, A~OBJNR, A~KSTAR, B~KTEXT

        UNION ALL

      SELECT A~VERSN, A~OBJNR, A~KSTAR, B~KTEXT AS KSTXT,
             SUM( A~WKG001 ) AS WKG001,
             SUM( A~WKG002 ) AS WKG002,
             SUM( A~WKG003 ) AS WKG003,
             SUM( A~WKG004 ) AS WKG004,
             SUM( A~WKG005 ) AS WKG005,
             SUM( A~WKG006 ) AS WKG006,
             SUM( A~WKG007 ) AS WKG007,
             SUM( A~WKG008 ) AS WKG008,
             SUM( A~WKG009 ) AS WKG009,
             SUM( A~WKG010 ) AS WKG010,
             SUM( A~WKG011 ) AS WKG011,
             SUM( A~WKG012 ) AS WKG012

        FROM COSS AS A
        LEFT JOIN CSKU AS B
          ON A~KSTAR = B~KSTAR
         AND B~KTOPL = @GC_KTOPL
         AND B~SPRAS = @SY-LANGU
       WHERE A~LEDNR = '00'
         AND A~GJAHR = @PA_GJAHR
         AND A~WRTTP = '04'             "실적
         AND A~VERSN = @PA_VERSN
         AND A~KSTAR IN ( SELECT DISTINCT SAKNR
                            FROM SKA1
                           WHERE KTOPL = @GC_KTOPL
                             AND ( KTOKS = 'PL' OR
                                   KTOKS = 'SECC' ) )

        GROUP BY A~VERSN, A~OBJNR, A~KSTAR, B~KTEXT

         INTO TABLE @GT_COSP.

    WHEN OTHERS.

      SELECT A~VERSN, A~OBJNR, A~KSTAR, B~KTEXT AS KSTXT,
             SUM( A~WKG001 ) AS WKG001,
             SUM( A~WKG002 ) AS WKG002,
             SUM( A~WKG003 ) AS WKG003,
             SUM( A~WKG004 ) AS WKG004,
             SUM( A~WKG005 ) AS WKG005,
             SUM( A~WKG006 ) AS WKG006,
             SUM( A~WKG007 ) AS WKG007,
             SUM( A~WKG008 ) AS WKG008,
             SUM( A~WKG009 ) AS WKG009,
             SUM( A~WKG010 ) AS WKG010,
             SUM( A~WKG011 ) AS WKG011,
             SUM( A~WKG012 ) AS WKG012
        INTO TABLE @GT_COSP
        FROM COSP AS A
        LEFT JOIN CSKU AS B
          ON A~KSTAR = B~KSTAR
         AND B~KTOPL = @GC_KTOPL
         AND B~SPRAS = @SY-LANGU
       WHERE A~LEDNR = '00'
         AND A~GJAHR = @PA_GJAHR
         AND A~WRTTP = '01'                 "계획
         AND ( A~VERSN IN @R_VERNS OR
               A~VERSN = @PA_VERSN )
         AND A~KSTAR IN ( SELECT DISTINCT SAKNR
                            FROM SKA1
                           WHERE KTOPL = @GC_KTOPL
                             AND ( KTOKS = 'PL' OR
                                   KTOKS = 'SECC' ) )

        GROUP BY A~VERSN, A~OBJNR, A~KSTAR, B~KTEXT.

  ENDCASE.

  SELECT A~KOSTL, B~KTEXT
    INTO TABLE @DATA(LT_KOSTL)
    FROM CSKS AS A
    LEFT JOIN CSKT AS B
      ON A~KOKRS = B~KOKRS
     AND A~KOSTL = B~KOSTL
     AND A~DATBI = B~DATBI
     AND B~SPRAS = @SY-LANGU
   WHERE A~KOKRS = @PA_KOKRS.

  SELECT PSPNR, POSID, POST1 INTO TABLE @DATA(LT_PRPS)
    FROM PRPS.

  IF GV_MODE = 'H'.

    SELECT RVERS, ROBJNR, RKSTAR,
           SUM( HSL01 ) AS HSL01, SUM( HSL02 ) AS HSL02,
           SUM( HSL03 ) AS HSL03, SUM( HSL04 ) AS HSL04,
           SUM( HSL05 ) AS HSL05, SUM( HSL06 ) AS HSL06,
           SUM( HSL07 ) AS HSL07, SUM( HSL08 ) AS HSL08,
           SUM( HSL09 ) AS HSL09, SUM( HSL10 ) AS HSL10,
           SUM( HSL11 ) AS HSL11, SUM( HSL12 ) AS HSL12
      INTO TABLE @DATA(LT_ZCOT0040)
      FROM ZCOT0040
     WHERE RLDNR = '00'
       AND RRCTY = '1'
       AND RVERS = @P_HVERSN
       AND RYEAR = @P_HGJAHR
     GROUP BY RVERS, ROBJNR, RKSTAR.

  ENDIF.

  LOOP AT GT_COSP ASSIGNING FIELD-SYMBOL(<LS_COSP>).

    MOVE-CORRESPONDING <LS_COSP> TO <GS_LINE>.

    CASE <LS_COSP>-OBJNR(2).

      WHEN 'KS'.

        ASSIGN COMPONENT 'KOSTL' OF STRUCTURE <GS_LINE> TO <FS_D>.
        MOVE <LS_COSP>-OBJNR+6 TO <FS_D>.

        READ TABLE LT_KOSTL ASSIGNING FIELD-SYMBOL(<LS_KOSTL>)
                            WITH KEY KOSTL = <FS_D>.

        IF SY-SUBRC = 0.

          ASSIGN COMPONENT 'KTEXT'
               OF STRUCTURE <GS_LINE> TO <FS_D2>.

          MOVE <LS_KOSTL>-KTEXT TO <FS_D2>.

        ENDIF.

      WHEN 'PR'.
        ASSIGN COMPONENT 'POSID' OF STRUCTURE <GS_LINE> TO <FS_D>.
        ASSIGN COMPONENT 'POST1' OF STRUCTURE <GS_LINE> TO <FS_D2>.

        READ TABLE LT_PRPS ASSIGNING FIELD-SYMBOL(<LS_PRPS>)
             WITH KEY PSPNR = <LS_COSP>-OBJNR+2.

        MOVE: <LS_PRPS>-POSID TO <FS_D>,
              <LS_PRPS>-POST1 TO <FS_D2>.

    ENDCASE.

    CASE <LS_COSP>-VERSN.

      WHEN PA_VERSN.
        LV_RFIELD = 'SDATA'.

      WHEN OTHERS.
        LV_RFIELD = 'T_' && <LS_COSP>-VERSN.

    ENDCASE.

    ASSIGN COMPONENT LV_RFIELD OF STRUCTURE <GS_LINE>
          TO <FS_R>.

    LOOP AT GT_MONTH.

*-- COSP
      LV_FIELDNAME = 'WKG0' && GT_MONTH-V1.

      ASSIGN COMPONENT LV_FIELDNAME OF STRUCTURE <LS_COSP>
            TO <FS_S>.

      <FS_R> = <FS_R> + <FS_S>.

    ENDLOOP.

    ASSIGN COMPONENT 'WAERS' OF STRUCTURE <GS_LINE> TO <FS_D>.
    MOVE LV_WAERS TO <FS_D>.

    COLLECT <GS_LINE> INTO <GT_TABLE>.
    CLEAR   <GS_LINE>.

  ENDLOOP.

  LOOP AT <GT_TABLE> ASSIGNING <GS_LINE>.

    CLEAR LV_DIFF.

    ASSIGN COMPONENT 'SDATA' OF STRUCTURE <GS_LINE>
       TO <FS_S>.

    ASSIGN COMPONENT 'HDATA' OF STRUCTURE <GS_LINE>
          TO <FS_HIS>.

    ASSIGN COMPONENT 'OBJNR' OF STRUCTURE <GS_LINE>
          TO <FS_OBJNR>.

    ASSIGN COMPONENT 'KSTAR' OF STRUCTURE <GS_LINE>
          TO <FS_KSTAR>.

    IF <FS_S> IS INITIAL.
      DELETE <GT_TABLE> INDEX SY-TABIX.
      CONTINUE.
    ENDIF.

*-- History
    READ TABLE LT_ZCOT0040 ASSIGNING FIELD-SYMBOL(<LS_ZCOT0040>)
                          WITH KEY ROBJNR = <FS_OBJNR>
                                   RKSTAR = <FS_KSTAR>.

    IF SY-SUBRC = 0.

      LOOP AT GT_MONTH.

        LV_FIELDNAME = 'HSL' && GT_MONTH-V1.

        ASSIGN COMPONENT LV_FIELDNAME OF STRUCTURE <LS_ZCOT0040>
              TO <FS_HSL>.

        <FS_HIS> = <FS_HIS> + <FS_HSL>.

      ENDLOOP.

    ENDIF.

*-- Value check
    LOOP AT GT_TKVS.

      LV_FIELDNAME = 'T_' && GT_TKVS-VERSI.

      ASSIGN COMPONENT LV_FIELDNAME OF STRUCTURE <GS_LINE>
         TO <FS_R>.

      IF GV_MODE = 'H'.

        IF NOT ( <FS_S> = <FS_R> AND
                 <FS_S> = <FS_HIS> ).
          LV_DIFF = ABAP_TRUE.
        ENDIF.

      ELSE.

        IF NOT (  <FS_S> = <FS_R> ).
          LV_DIFF = ABAP_TRUE.
        ENDIF.

      ENDIF.

    ENDLOOP.

    ASSIGN COMPONENT 'ICON' OF STRUCTURE <GS_LINE>
      TO <FS_D2>.

    IF LV_DIFF = ABAP_TRUE.
      <FS_D2> = '@4A@'.  "Copy

    ELSE.
      <FS_D2> = '@00@'.
    ENDIF.

  ENDLOOP.

  OTAB = VALUE #( ( NAME = 'KOSTL'  )
                  ( NAME = 'POSID'  )
                  ( NAME = 'KSTAR'  ) ).

  SORT <GT_TABLE> BY (OTAB).

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
                  T_TABLE      = <GT_TABLE> ).
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

  DATA: L_TITLE TYPE LVC_TITLE,
        L_LINES TYPE C LENGTH 100,
        L_COUNT TYPE I.

  DATA LV_TEXT TYPE CHAR100.

  GO_ALV->SET_SCREEN_STATUS(
    PFSTATUS      =  'STANDARD'
    REPORT        =  GV_REPID
    SET_FUNCTIONS = GO_ALV->C_FUNCTIONS_ALL ).

  L_COUNT = LINES( <GT_TABLE> ).

  CASE GV_MODE.
    WHEN 'S'.
      LV_TEXT = TEXT-T05.
    WHEN 'C'.
      LV_TEXT = TEXT-T06.
    WHEN OTHERS.
      LV_TEXT = TEXT-T00.
  ENDCASE.

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
*      LR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).
      LR_COLUMNS->SET_CELL_TYPE_COLUMN( 'CELLTYPE' ).
*      LR_COLUMNS->SET_KEY_FIXATION( ABAP_TRUE ).
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

        IF ( <COLUMN_REF>-COLUMNNAME CP 'T_*' OR
            <COLUMN_REF>-COLUMNNAME = 'HDATA'  OR
            <COLUMN_REF>-COLUMNNAME = 'SDATA' ).

          CALL METHOD IR_AGG->ADD_AGGREGATION
            EXPORTING
              COLUMNNAME  = <COLUMN_REF>-COLUMNNAME
              AGGREGATION = IF_SALV_C_AGGREGATION=>TOTAL.

        ENDIF.

      ENDLOOP.

    CATCH CX_SALV_NOT_FOUND.
    CATCH CX_SALV_DATA_ERROR.
    CATCH CX_SALV_EXISTING.

  ENDTRY.

  CASE GV_MODE.

    WHEN 'S' OR 'C'.

      TRY.
          LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'HDATA' ).
          LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
        CATCH CX_SALV_NOT_FOUND.
      ENDTRY.

  ENDCASE.

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

    WHEN 'CREATE'.

      PERFORM ERROR_CHECK.
      CHECK GV_EXIT IS INITIAL.
      PERFORM POPUP_TO_CONFIRM USING TEXT-PT1
                                     TEXT-QT1.
      CHECK GV_ANSWER EQ '1'.
      PERFORM CREATE_EXECUTE.

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

        WHEN 'ICON'.
          P_COLUMN_TEXT = TEXT-C01.

          GR_COLUMN->SET_ICON( IF_SALV_C_BOOL_SAP=>TRUE ).
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 4 ).
*          GR_COLUMN->SET_KEY_FIXATION( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'KOSTL'.
          P_COLUMN_TEXT = TEXT-C02.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 10 ).

        WHEN 'KTEXT'.
          P_COLUMN_TEXT = TEXT-C03.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'POSID'.
          P_COLUMN_TEXT = TEXT-C04.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 10 ).

        WHEN 'POST1'.
          P_COLUMN_TEXT = TEXT-C05.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 20 ).

        WHEN 'KSTAR'.
          P_COLUMN_TEXT = TEXT-C06.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'KSTXT'.
          P_COLUMN_TEXT = TEXT-C07.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'WAERS'.
          P_COLUMN_TEXT = TEXT-C09.

        WHEN 'SDATA'.
          P_COLUMN_TEXT = TEXT-T01.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '3' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'HDATA'.
          P_COLUMN_TEXT = TEXT-T03.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '6' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'LOG'.
          P_COLUMN_TEXT = TEXT-C08.
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 50 ).

      ENDCASE.

      IF P_COLUMNNAME CP 'T_*'.

        P_COLUMN_TEXT = TEXT-T02 && '(' && P_COLUMNNAME+2(3) &&
                        ')'.

        GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

        _SET_COLOR '5' '0' '0'.
        GR_COLUMN->SET_COLOR( GS_COLOR ).
        GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

      ENDIF.

    CATCH CX_SALV_NOT_FOUND.
    CATCH CX_SALV_DATA_ERROR.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_EXECUTE
*&---------------------------------------------------------------------*
FORM CREATE_EXECUTE .

  DATA LV_RFIELD    TYPE FIELDNAME.
  DATA LV_LAST.

  FIELD-SYMBOLS: <FS_MESSAGE> TYPE ANY,
                 <FS_ICON>    TYPE ANY,
                 <FS_SOUR>    TYPE ANY,
                 <FS_RECV>    TYPE ANY,
                 <FS_HIST>    TYPE ANY.

  DATA LV_ERROR.

  LOOP AT GT_ROWS INTO GS_ROW.

    READ TABLE <GT_TABLE> ASSIGNING <GS_LINE> INDEX GS_ROW.

    IF SY-SUBRC = 0.

      ASSIGN COMPONENT 'LOG' OF STRUCTURE <GS_LINE>
         TO <FS_MESSAGE>.

      ASSIGN COMPONENT 'ICON' OF STRUCTURE <GS_LINE>
         TO <FS_ICON>.

      IF <FS_ICON> = '@4A@'.      "복사가능 한것만

        ASSIGN COMPONENT 'SDATA' OF STRUCTURE <GS_LINE>
           TO <FS_SOUR>.

        LOOP AT GT_TKVS.

          CLEAR LV_LAST.

          AT LAST.
            LV_LAST = ABAP_TRUE.
          ENDAT.

          PERFORM COPY_VERSION USING    GT_TKVS-VERSI
                                        LV_LAST
                               CHANGING <FS_MESSAGE>
                                        LV_ERROR.
          IF LV_ERROR IS NOT INITIAL.
            EXIT.
          ENDIF.

          LV_RFIELD = 'T_' && GT_TKVS-VERSI.
          ASSIGN COMPONENT LV_RFIELD OF STRUCTURE <GS_LINE>
             TO <FS_RECV>.

          MOVE <FS_SOUR> TO <FS_RECV>.

          IF LV_LAST = ABAP_TRUE.

            MOVE: '@00@'   TO <FS_ICON>,
                  TEXT-S01 TO <FS_MESSAGE>.

            IF GV_MODE = 'H'.

              ASSIGN COMPONENT 'HDATA' OF STRUCTURE <GS_LINE>
                 TO <FS_HIST>.

              MOVE <FS_SOUR> TO <FS_HIST>.

            ENDIF.

          ENDIF.

        ENDLOOP.

      ELSE.
        <FS_MESSAGE> = TEXT-S03.

      ENDIF.

    ENDIF.

  ENDLOOP.

  MESSAGE S000 WITH TEXT-S02 .

  CALL METHOD GO_ALV->REFRESH
    EXPORTING
      REFRESH_MODE = IF_SALV_C_REFRESH=>FULL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ERROR_CHECK
*&---------------------------------------------------------------------*
FORM ERROR_CHECK .

  DATA LR_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS.

  FIELD-SYMBOLS <FS_ICON> TYPE ANY.

  CLEAR: GT_ROWS, GV_EXIT.

  LR_SELECTIONS = GO_ALV->GET_SELECTIONS( ).
  GT_ROWS = LR_SELECTIONS->GET_SELECTED_ROWS( ).

  IF GT_ROWS IS INITIAL.
    MESSAGE S015 DISPLAY LIKE 'E'.
    GV_EXIT = ABAP_TRUE.
    EXIT.
  ENDIF.

*  LOOP AT GT_ROWS INTO GS_ROW.
*
*    READ TABLE <GT_TABLE> ASSIGNING <GS_LINE> INDEX GS_ROW.
*
*    IF SY-SUBRC = 0.
*
*      ASSIGN COMPONENT 'ICON' OF STRUCTURE <GS_LINE>
*         TO  <FS_ICON>.
*
*      IF <FS_ICON> = '@00@'.
*        MESSAGE S000   WITH TEXT-E05 DISPLAY LIKE 'E'.
*        GV_EXIT = ABAP_TRUE.
*        EXIT.
*      ENDIF.
*    ENDIF.
*
*  ENDLOOP.

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
*& Form MAKE_TABLE
*&---------------------------------------------------------------------*
FORM MAKE_TABLE .

  DATA LV_FIELDNAME TYPE FIELDNAME.
  DATA LV_TITLE     TYPE LVC_TXTCOL.
  DATA LV_MONTH     TYPE N LENGTH 2.

  CLEAR GT_FIELDCAT[].

  CLEAR: GT_MONTH, GT_MONTH[].

  LV_MONTH = PA_SPERL+1(2).

  DO .
    MOVE LV_MONTH TO GT_MONTH-V1.
    APPEND GT_MONTH.

    ADD 1 TO LV_MONTH.
    IF LV_MONTH > PA_EPERL+1(2).
      EXIT.
    ENDIF.
  ENDDO.

  SELECT * FROM TKVS
    INTO TABLE @GT_TKVS
   WHERE VERSI IN @R_VERNS.

  IF <GT_TABLE> IS ASSIGNED.
    UNASSIGN <GT_TABLE>.
    UNASSIGN <GS_LINE>.
    CLEAR GR_DATA.
  ENDIF.

  PERFORM FILL_FIELD_CATEGORY USING :

        'S' 'FIELDNAME'   'ICON',
        ' ' 'OUTPUTLEN'   '8',
        ' ' 'ICON'        'X',
        ' ' 'REF_TABLE'   'ICON',
        ' ' 'REF_FIELD'   'ID',
        'E' 'COLTEXT'     TEXT-C01,

        'S' 'FIELDNAME'   'OBJNR',
        ' ' 'OUTPUTLEN'   '22',
        ' ' 'REF_TABLE'   'COSP',
        ' ' 'REF_FIELD'   'OBJNR',
        'E' 'COLTEXT'     TEXT-C10,

        'S' 'FIELDNAME'   'KOSTL',
        ' ' 'OUTPUTLEN'   '10',
        ' ' 'REF_TABLE'   'CSKS',
        ' ' 'REF_FIELD'   'KOSTL',
        'E' 'COLTEXT'     TEXT-C02,

        'S' 'FIELDNAME'   'KTEXT',
        ' ' 'OUTPUTLEN'   '20',
        ' ' 'REF_TABLE'   'CSKT',
        ' ' 'REF_FIELD'   'KTEXT',
        'E' 'COLTEXT'     TEXT-C03,

        'S' 'FIELDNAME'   'POSID',
        ' ' 'OUTPUTLEN'   '25',
        ' ' 'REF_TABLE'   'PRPS',
        ' ' 'REF_FIELD'   'POSID',
        'E' 'COLTEXT'     TEXT-C04,

        'S' 'FIELDNAME'   'POST1',
        ' ' 'OUTPUTLEN'   '30',
        ' ' 'REF_TABLE'   'PRPS',
        ' ' 'REF_FIELD'   'POST1',
        'E' 'COLTEXT'     TEXT-C05,

        'S' 'FIELDNAME'   'KSTAR',
        ' ' 'OUTPUTLEN'   '10',
        ' ' 'REF_TABLE'   'CSKA',
        ' ' 'REF_FIELD'   'KSTAR',
        'E' 'COLTEXT'     TEXT-C06,

        'S' 'FIELDNAME'   'KSTXT',
        ' ' 'OUTPUTLEN'   '20',
        ' ' 'REF_TABLE'   'CSKU',
        ' ' 'REF_FIELD'   'KTEXT',
        'E' 'COLTEXT'     TEXT-C07,

        'S' 'FIELDNAME'   'WAERS',
        ' ' 'OUTPUTLEN'   '4',
        ' ' 'REF_TABLE'   'TKA01',
        ' ' 'REF_FIELD'   'WAERS',
        'E' 'COLTEXT'     TEXT-C09,

         'S' 'FIELDNAME'   'SDATA',
         ''  'OUTPUTLEN'    '23',
         ' ' 'CFIELDNAME'  'WAERS',
         ' ' 'REF_TABLE'   'ZCOT0040',
         ' ' 'REF_FIELD'   'HSLVT',
         'E' 'COLTEXT'     TEXT-T01.

  LOOP AT GT_TKVS.

    LV_FIELDNAME = 'T_' && GT_TKVS-VERSI.
    LV_TITLE     = TEXT-T02 && '(' && GT_TKVS-VERSI && ')'.

    PERFORM FILL_FIELD_CATEGORY USING :
          'S' 'FIELDNAME'   LV_FIELDNAME,
          ''  'OUTPUTLEN'    '23',
          ' ' 'CFIELDNAME'  'WAERS',
          ' ' 'REF_TABLE'   'ZCOT0040',
          ' ' 'REF_FIELD'   'HSLVT',
          'E' 'COLTEXT'     LV_TITLE.

  ENDLOOP.

  PERFORM FILL_FIELD_CATEGORY USING :

         'S' 'FIELDNAME'   'HDATA',
         ''  'OUTPUTLEN'    '23',
         ' ' 'CFIELDNAME'  'WAERS',
         ' ' 'REF_TABLE'   'ZCOT0040',
         ' ' 'REF_FIELD'   'HSLVT',
         'E' 'COLTEXT'     TEXT-T03,

        'S' 'FIELDNAME'   'LOG',
        ''  'OUTPUTLEN'   '80',
        'E' 'COLTEXT'     TEXT-C08.

  CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
    EXPORTING
      IT_FIELDCATALOG = GT_FIELDCAT
    IMPORTING
      EP_TABLE        = GR_DATA.

  ASSIGN  GR_DATA->* TO <GT_TABLE>.
  CREATE DATA GR_DATA LIKE LINE OF <GT_TABLE>.
  ASSIGN  GR_DATA->* TO <GS_LINE>.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------*
FORM FILL_FIELD_CATEGORY  USING PV_GUB PV_FNAME PV_CON.

  IF PV_GUB = 'S'.
    CLEAR GS_FIELDCAT.
  ENDIF.

* 속성 MOVE
  DATA LV_COL(40).
  FIELD-SYMBOLS <FS>.
  CONCATENATE 'GS_FIELDCAT-' PV_FNAME  INTO LV_COL.
  ASSIGN      (LV_COL)       TO        <FS>.
  MOVE         PV_CON        TO        <FS>.

  IF PV_GUB = 'E'.
    APPEND GS_FIELDCAT TO GT_FIELDCAT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILT_HEADER
*&---------------------------------------------------------------------*
FORM BUILT_HEADER  CHANGING CR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.

  DATA: LR_GRID   TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_GRID_1 TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_LABEL  TYPE REF TO CL_SALV_FORM_LABEL,
        LR_TEXT   TYPE REF TO CL_SALV_FORM_TEXT,
        LV_TEXT   TYPE STRING,
        L_TOTAL   TYPE STRING,
        L_SUCESS  TYPE STRING,
        L_ERROR   TYPE STRING.

  CREATE OBJECT LR_GRID.

  LR_GRID_1 = LR_GRID->CREATE_GRID(
                ROW    = 4
                COLUMN = 1 ).

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW     = 1
    COLUMN  = 1
    TEXT    = TEXT-T04
    TOOLTIP = TEXT-T04 ).

  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW     = 1
    COLUMN  = 2
    TEXT    = PA_GJAHR
    TOOLTIP = PA_GJAHR ).

  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW    = 2
    COLUMN = 1
    TEXT    = TEXT-T01
    TOOLTIP = TEXT-T01 ).

  SELECT SINGLE VTEXT INTO @DATA(LV_T1)
    FROM TKVS AS A
    LEFT JOIN TKVST AS B
      ON A~VERSI = B~VERSI
     AND B~SPRAS = @SY-LANGU
   WHERE A~VERSI = @PA_VERSN.

  CONCATENATE PA_VERSN '(' LV_T1 ')'
        INTO LV_TEXT SEPARATED BY SPACE.

  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW    = 2
    COLUMN = 2
    TEXT    = LV_TEXT
    TOOLTIP = LV_TEXT ).

  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW    = 3
    COLUMN = 1
    TEXT    = TEXT-T02
    TOOLTIP = TEXT-T02 ).

  IF PV_TVERE IS NOT INITIAL.

    SELECT SINGLE VTEXT INTO @DATA(LV_T2)
      FROM TKVS AS A
      LEFT JOIN TKVST AS B
        ON A~VERSI = B~VERSI
       AND B~SPRAS = @SY-LANGU
     WHERE A~VERSI = @PV_TVERS.

    SELECT SINGLE VTEXT INTO @DATA(LV_T3)
      FROM TKVS AS A
      LEFT JOIN TKVST AS B
        ON A~VERSI = B~VERSI
       AND B~SPRAS = @SY-LANGU
     WHERE A~VERSI = @PV_TVERE.

    CONCATENATE PV_TVERS '(' LV_T2 ')'
            '~' PV_TVERE '(' LV_T3 ')'
           INTO LV_TEXT SEPARATED BY SPACE.

  ELSE.

    SELECT SINGLE VTEXT INTO @LV_T2
      FROM TKVS AS A
      LEFT JOIN TKVST AS B
        ON A~VERSI = B~VERSI
       AND B~SPRAS = @SY-LANGU
     WHERE A~VERSI = @PV_TVERS.

    CONCATENATE PV_TVERS '(' LV_T2 ')'
          INTO LV_TEXT SEPARATED BY SPACE.

  ENDIF.

  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW    = 3
    COLUMN = 2
    TEXT    = LV_TEXT
    TOOLTIP = LV_TEXT ).

  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).

  IF GV_MODE = 'H'.

    LR_LABEL = LR_GRID_1->CREATE_LABEL(
      ROW    = 4
      COLUMN = 1
      TEXT    = TEXT-T03
      TOOLTIP = TEXT-T03 ).

    SELECT SINGLE VTEXT INTO @DATA(LV_T4)
      FROM TKVS AS A
      LEFT JOIN TKVST AS B
        ON A~VERSI = B~VERSI
       AND B~SPRAS = @SY-LANGU
     WHERE A~VERSI = @P_HVERSN.

    CONCATENATE P_HVERSN '(' LV_T4 ')'
          INTO LV_TEXT SEPARATED BY SPACE.

    LR_TEXT = LR_GRID_1->CREATE_TEXT(
      ROW    = 4
      COLUMN = 2
      TEXT    = LV_TEXT
      TOOLTIP = LV_TEXT ).

  ENDIF.

  CR_CONTENT = LR_GRID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form COPY_VERSION
*&---------------------------------------------------------------------*
FORM COPY_VERSION  USING    P_VERSN
                            P_LAST
                   CHANGING P_MESSAGE
                            P_ERROR.

  DATA LV_MESSAGE TYPE STRING.

  DATA LS_HEADERINFO LIKE BAPIPLNHDR.
  DATA LS_RETURN     TYPE BAPIRET2.

  DATA LV_FNAME TYPE FIELDNAME.

  DATA LV_FIELDNAME TYPE FIELDNAME.

  DATA : LT_INDEXSTRUCTURE LIKE BAPIACPSTRU OCCURS 0 WITH HEADER LINE,
         LT_COOBJECT       LIKE BAPIPCPOBJ  OCCURS 0 WITH HEADER LINE,
         LT_PERVALUE       LIKE BAPIPCPVAL  OCCURS 0 WITH HEADER LINE.

  DATA LT_ZCOT0040 TYPE TABLE OF ZCOT0040 WITH HEADER LINE.

  FIELD-SYMBOLS: <FS_KOSTL> TYPE KOSTL,
                 <FS_POSID> TYPE PS_POSID,
                 <FS_OBJNR> TYPE J_OBJNR,
                 <FS_KSTAR> TYPE KSTAR,
                 <FS_WAERS> TYPE WAERS,
                 <FS_SOUR>  TYPE ANY,
                 <FS_RECV>  TYPE ANY,
                 <FS_HIST>  TYPE ANY.

  DATA LV_BELNR TYPE BELNR_D.
  DATA LV_SEQ   TYPE ZERSEQ.

  CLEAR: GT_RETURN, GT_RETURN[].
  CLEAR P_MESSAGE.
  CLEAR P_ERROR.

  IF P_LAST = ABAP_TRUE.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR             = '01'
        OBJECT                  = 'ZCO_DOCNR'
        TOYEAR                  = PA_GJAHR
      IMPORTING
        NUMBER                  = LV_BELNR
      EXCEPTIONS
        INTERVAL_NOT_FOUND      = 1
        NUMBER_RANGE_NOT_INTERN = 2
        OBJECT_NOT_FOUND        = 3
        QUANTITY_IS_0           = 4
        QUANTITY_IS_NOT_1       = 5
        INTERVAL_OVERFLOW       = 6
        BUFFER_OVERFLOW         = 7
        OTHERS                  = 8.

    IF SY-SUBRC <> 0.
      MESSAGE S001 WITH TEXT-E04 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDIF.

*-- Header Data
  LS_HEADERINFO-CO_AREA     = PA_KOKRS.     "관리 회계영역
  LS_HEADERINFO-FISC_YEAR   = PA_GJAHR.     "회계연도
  LS_HEADERINFO-PERIOD_FROM = PA_SPERL.      "기간 시작
  LS_HEADERINFO-PERIOD_TO   = PA_EPERL.      "기간 종료
  LS_HEADERINFO-VERSION     = P_VERSN.       "버전

*-- 전표 헤더 텍스트
  LS_HEADERINFO-PLAN_CURRTYPE = 'C'. "통화

*-- CO-계획: 액티비티투입 & 주요지표 계획 BAPIs
  LT_INDEXSTRUCTURE-OBJECT_INDEX = 1.
  LT_INDEXSTRUCTURE-VALUE_INDEX  = 1.
  APPEND LT_INDEXSTRUCTURE.

*-- CO 계획: 1차 원가 BAPI에 대한 오브젝트
  LT_COOBJECT-OBJECT_INDEX  = 1.

  ASSIGN COMPONENT 'KOSTL' OF STRUCTURE <GS_LINE> TO <FS_KOSTL>.
  ASSIGN COMPONENT 'POSID' OF STRUCTURE <GS_LINE> TO <FS_POSID>.
  ASSIGN COMPONENT 'OBJNR' OF STRUCTURE <GS_LINE> TO <FS_OBJNR>.
  ASSIGN COMPONENT 'KSTAR' OF STRUCTURE <GS_LINE> TO <FS_KSTAR>.
  ASSIGN COMPONENT 'WAERS' OF STRUCTURE <GS_LINE> TO <FS_WAERS>.

  IF <FS_KOSTL> IS NOT INITIAL.
    LT_COOBJECT-COSTCENTER    = <FS_KOSTL>.
  ELSE.
    LT_COOBJECT-WBS_ELEMENT   = <FS_POSID>.
  ENDIF.

  APPEND LT_COOBJECT.

  LT_PERVALUE-VALUE_INDEX  = 1.
  LT_PERVALUE-COST_ELEM    = <FS_KSTAR>.
  LT_PERVALUE-TRANS_CURR   = <FS_WAERS>.

  READ TABLE GT_COSP ASSIGNING FIELD-SYMBOL(<LS_COSP>)
                 WITH KEY VERSN = PA_VERSN
                          OBJNR = <FS_OBJNR>
                          KSTAR = <FS_KSTAR>.

  LOOP AT GT_MONTH.

    LV_FNAME = 'LT_PERVALUE-FIX_VAL_PER' && GT_MONTH-V1.

    ASSIGN (LV_FNAME) TO <FS_RECV>.

    LV_FNAME = 'WKG0' && GT_MONTH-V1.

    IF <LS_COSP> IS ASSIGNED.

      ASSIGN COMPONENT LV_FNAME OF STRUCTURE <LS_COSP>
            TO <FS_SOUR>.

      IF <FS_WAERS> = 'KRW'.
        <FS_RECV> = <FS_SOUR>  * 100.
      ELSE.
        <FS_RECV> = <FS_SOUR>.
      ENDIF.

    ENDIF.

    IF GV_MODE = 'H' AND P_LAST = ABAP_TRUE.

*-- CBO 저장 데이터
      MOVE: '00'           TO LT_ZCOT0040-RLDNR,
            '1'            TO LT_ZCOT0040-RRCTY,
            '1'            TO LT_ZCOT0040-RSEQ,
            P_HVERSN       TO LT_ZCOT0040-RVERS,
            PA_GJAHR       TO LT_ZCOT0040-RYEAR,
            <FS_OBJNR>     TO LT_ZCOT0040-ROBJNR,
            LV_BELNR       TO LT_ZCOT0040-RDOCNR,
            'BT01'         TO LT_ZCOT0040-BUDTYPE_9,
            PA_KOKRS       TO LT_ZCOT0040-RKOKRS,
            SY-UZEIT       TO LT_ZCOT0040-CPUTM,
            SY-DATUM       TO LT_ZCOT0040-CPUDT,
            SY-UNAME       TO LT_ZCOT0040-USNAM,
            <FS_WAERS>     TO LT_ZCOT0040-RTCUR,
            <FS_KSTAR>     TO LT_ZCOT0040-RKSTAR,
            'ENTR'         TO LT_ZCOT0040-PROCESS_9.

      LV_FNAME = 'HSL' && GT_MONTH-V1.

      ASSIGN COMPONENT LV_FNAME OF STRUCTURE LT_ZCOT0040
         TO <FS_HIST>.

      MOVE <FS_SOUR> TO <FS_HIST>.

      COLLECT LT_ZCOT0040.
      CLEAR   LT_ZCOT0040.

    ENDIF.

  ENDLOOP.

  APPEND LT_PERVALUE.

  CALL FUNCTION 'BAPI_COSTACTPLN_POSTPRIMCOST'
    EXPORTING
      HEADERINFO     = LS_HEADERINFO
      DELTA          = ABAP_FALSE
    TABLES
      INDEXSTRUCTURE = LT_INDEXSTRUCTURE
      COOBJECT       = LT_COOBJECT
      PERVALUE       = LT_PERVALUE
      RETURN         = GT_RETURN.

  READ TABLE GT_RETURN WITH KEY TYPE = 'E'.

  IF SY-SUBRC EQ 0 .

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    PERFORM BUILD_MESSAGE USING    GT_RETURN
                          CHANGING P_MESSAGE.

    P_ERROR = ABAP_TRUE.

  ELSE.

    TRY .

        IF GV_MODE = 'H' AND P_LAST = ABAP_TRUE.

          DELETE FROM ZCOT0040 WHERE RVERS  = P_HVERSN
                                 AND RYEAR  = PA_GJAHR
                                 AND ROBJNR = <FS_OBJNR>
                                 AND RKSTAR = <FS_KSTAR>.

          INSERT  ZCOT0040 FROM TABLE LT_ZCOT0040.

        ENDIF.

        COMMIT WORK.

      CATCH CX_SY_SQL_ERROR INTO DATA(LR_ERROR).

        ROLLBACK WORK.

        LV_MESSAGE = LR_ERROR->GET_TEXT( ).
        MESSAGE S001 WITH LV_MESSAGE INTO P_MESSAGE.

        P_ERROR = ABAP_TRUE.

    ENDTRY.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_MESSAGE
*&---------------------------------------------------------------------*
FORM BUILD_MESSAGE USING    PS_MESSAGE STRUCTURE BAPIRET2
                     CHANGING PV_TEXT.

  CLEAR PV_TEXT.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      MSGID               = PS_MESSAGE-ID
      MSGNR               = PS_MESSAGE-NUMBER
      MSGV1               = PS_MESSAGE-MESSAGE_V1
      MSGV2               = PS_MESSAGE-MESSAGE_V2
      MSGV3               = PS_MESSAGE-MESSAGE_V3
      MSGV4               = PS_MESSAGE-MESSAGE_V4
    IMPORTING
      MESSAGE_TEXT_OUTPUT = PV_TEXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ANSWER
*&---------------------------------------------------------------------*
FORM CHECK_ANSWER .

  CHECK GV_MODE = 'H'.

  SELECT SINGLE * FROM ZCOT0040 INTO @DATA(LS_ZCOT0040)
   WHERE RVERS     = @P_HVERSN
     AND RYEAR     = @P_HGJAHR
     AND PROCESS_9 = 'ENTR'.

  IF SY-SUBRC = 0.

    CLEAR GV_ANSWER.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TITLEBAR       = TEXT-PT1
        ICON_BUTTON_1  = 'ICON_OKAY'
        TEXT_QUESTION  = TEXT-I01
        POPUP_TYPE     = 'ICON_MESSAGE_WARNING'
      IMPORTING
        ANSWER         = GV_ANSWER
      EXCEPTIONS
        TEXT_NOT_FOUND = 1
        OTHERS         = 2.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    IF GV_ANSWER <> '1'.
      MESSAGE S052 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

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

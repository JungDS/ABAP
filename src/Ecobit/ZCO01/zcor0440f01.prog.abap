*&---------------------------------------------------------------------*
*& Include          ZCOR0440F01
*&---------------------------------------------------------------------*
*& Form INITIAL_SET
*&---------------------------------------------------------------------*
FORM INITIAL_SET .


  GV_REPID = SY-REPID.

  SELECT SINGLE VTEXT INTO @PA_VTXT
    FROM TKVS AS A
    LEFT JOIN TKVST AS B
      ON A~VERSI = B~VERSI
     AND B~SPRAS = @SY-LANGU
   WHERE A~VERSI = 'B1'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SELSCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_SELSCREEN .

* INPUT 제한 취소. 버전 제한 없이 하고싶다고 하셔서...
  LOOP AT SCREEN.

    IF SCREEN-GROUP1 = 'MG1'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ENQUE_LOCK
*&---------------------------------------------------------------------*
FORM ENQUE_LOCK .

  DATA LS_RSTABLE TYPE RSTABLE.

  LS_RSTABLE-TABNAME = 'ZCOT0040'.
  LS_RSTABLE-VARKEY  = SY-MANDT && PA_VERSN && PA_GJAHR.

  CALL FUNCTION 'ENQUEUE_E_TABLE'
    EXPORTING
      MODE_RSTABLE   = 'E'
      TABNAME        = LS_RSTABLE-TABNAME
      VARKEY         = LS_RSTABLE-VARKEY
    EXCEPTIONS
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2
      OTHERS         = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
        DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DEQUE_LOCK
*&---------------------------------------------------------------------*
FORM DEQUE_LOCK .

  DATA LS_RSTABLE TYPE RSTABLE.

  LS_RSTABLE-TABNAME = 'ZCOT0040'.
  LS_RSTABLE-VARKEY  = SY-MANDT && PA_VERSN && PA_GJAHR.

  CALL FUNCTION 'DEQUEUE_E_TABLE'
    EXPORTING
      MODE_RSTABLE   = 'E'
      TABNAME        = LS_RSTABLE-TABNAME
      VARKEY         = LS_RSTABLE-VARKEY
    EXCEPTIONS
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2
      OTHERS         = 3.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM POPUP_TO_CONFIRM  USING PV_TITLE
                            PV_QUEST.

  "-- call popup
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR       = PV_TITLE                "TEXT-PT1
      TEXT_QUESTION  = PV_QUEST                "TEXT-QT1
    IMPORTING
      ANSWER         = GV_ANSWER
*   TABLES
*     PARAMETER      =
    EXCEPTIONS
      TEXT_NOT_FOUND = 1
      OTHERS         = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_GJAHR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_GJAHR .
  CLEAR : GV_CHECK_GJAHR.
  DATA : LV_GJAHR TYPE GJAHR.

  LV_GJAHR = SY-DATUM+0(4).

  IF PA_GJAHR < LV_GJAHR.
    GV_CHECK_GJAHR = 'X'.
    MESSAGE E027  WITH TEXT-001.
    STOP.
  ELSE.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form DELETE_DATA_RTN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DELETE_DATA_RTN .
* 조회조건의 조건(버전, 회계년도)에 따라 데이터를 DELETE한다.

  DELETE FROM ZCOT0040 WHERE RVERS = PA_VERSN
    AND  RYEAR = PA_GJAHR.

  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .
* ZCOR0090, GV_MODE = 'H' 참조

  DATA LV_FIELDNAME TYPE FIELDNAME.
  DATA LV_FYEAR     TYPE N LENGTH 7.

  DATA LV_RFIELD    TYPE FIELDNAME.
  DATA LV_DIFF.

  DATA OTAB TYPE ABAP_SORTORDER_TAB .

  DATA LV_VALUE TYPE C LENGTH 6.

  RANGES R_OBJNR1 FOR ZCOT0040-ROBJNR.
  RANGES R_OBJNR2 FOR ZCOT0040-ROBJNR.

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
  CLEAR : GT_KOSTL, GT_KOSTL[].
  CLEAR : GT_PRPS, GT_PRPS[].

* WBS 그룹 Get
    RANGES R_POSID FOR PRPS-POSID.
    PERFORM READ_HIERARCHY_TABLES TABLES GT_VALUES
                                  USING '0110'
                                        PA_PDGR.   "WBS 요소 그룹
 IF PA_PDGR IS NOT INITIAL.

    PERFORM READ_HIERARCHY_TABLES TABLES GT_VALUES
                                  USING '0110'
                                        PA_PDGR.   "WBS 요소 그룹

    LOOP AT GT_VALUES.

      MOVE: 'I'   TO R_POSID-SIGN,
            'BT'  TO R_POSID-OPTION.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          INPUT     = GT_VALUES-VFROM
        IMPORTING
          OUTPUT    = R_POSID-LOW
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          INPUT     = GT_VALUES-VTO
        IMPORTING
          OUTPUT    = R_POSID-HIGH
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.

      COLLECT R_POSID.
      CLEAR   R_POSID.

    ENDLOOP.

  ENDIF.

* 회사코드 필터(CCTR, WBS)
  LV_VALUE = 'KS' && GV_KOKRS.
  SELECT A~KOSTL, B~KTEXT,
         CONCAT( @LV_VALUE , A~KOSTL ) AS OBJNR
    FROM CSKS AS A
    LEFT JOIN CSKT AS B
      ON A~KOSTL = B~KOSTL
     AND A~KOKRS = B~KOKRS
     AND A~DATBI = B~DATBI
   WHERE A~KOKRS = @GV_KOKRS
     AND A~DATAB <= @SY-DATUM
     AND A~DATBI >= @SY-DATUM
     AND A~BUKRS EQ @PA_BUKRS
    INTO TABLE @GT_KOSTL
    .

    LOOP AT GT_KOSTL.
      MOVE: 'I'            TO R_OBJNR1-SIGN,
            'EQ'           TO R_OBJNR1-OPTION,
            GT_KOSTL-OBJNR TO R_OBJNR1-LOW.

      COLLECT R_OBJNR1.
      CLEAR   R_OBJNR1.
    ENDLOOP.

  IF PA_PDGR IS NOT INITIAL.
    SELECT POSID, POST1, OBJNR
          FROM PRPS
          INTO TABLE @GT_PRPS
         WHERE PBUKR EQ @PA_BUKRS
           AND LOEVM = @SPACE
           AND PSPNR IN @R_POSID
          .
*      WBS 그룹에 값이 있으면 WBS만 조회하게 한다.
      CLEAR : R_OBJNR1[], R_OBJNR1.
  ELSE.
    SELECT POSID, POST1, OBJNR
      FROM PRPS
      INTO TABLE @GT_PRPS
     WHERE PBUKR EQ @PA_BUKRS
       AND LOEVM = @SPACE
      .
  ENDIF.


    LOOP AT GT_PRPS.
      MOVE: 'I'           TO R_OBJNR1-SIGN,
            'EQ'          TO R_OBJNR1-OPTION,
            GT_PRPS-OBJNR TO R_OBJNR1-LOW.

      COLLECT R_OBJNR1.
      CLEAR   R_OBJNR1.
    ENDLOOP.

  CHECK R_OBJNR1[] IS NOT INITIAL.


  SELECT SINGLE WAERS INTO @DATA(LV_WAERS)
    FROM TKA01
   WHERE KOKRS = @GV_KOKRS.

* Standard Version(B1) Data
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
     AND B~KTOPL = @GV_KTOPL
     AND B~SPRAS = @SY-LANGU
   WHERE A~LEDNR = '00'
     AND A~GJAHR = @PA_GJAHR
     AND A~WRTTP = '01'                 "계획
     AND (
           A~VERSN = @PA_VERSN )
     AND A~KSTAR IN ( SELECT DISTINCT SAKNR
                        FROM SKA1
                       WHERE KTOPL = @GV_KTOPL
                         AND ( KTOKS = 'PL' OR
                               KTOKS = 'SECC' ) )
    AND A~OBJNR IN @R_OBJNR1
    GROUP BY A~VERSN, A~OBJNR, A~KSTAR, B~KTEXT
    ORDER BY A~OBJNR.

* Historical Data
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
     AND RVERS = @PA_VERSN
     AND RYEAR = @PA_GJAHR
   GROUP BY RVERS, ROBJNR, RKSTAR.

* Text 명칭 Get
  LOOP AT GT_COSP ASSIGNING FIELD-SYMBOL(<LS_COSP>).
* ALV에 먼저 양식을 담아둠.
    MOVE-CORRESPONDING <LS_COSP> TO <GS_LINE>.

    CASE <LS_COSP>-OBJNR(2).
      WHEN 'KS'.
* ALV 내 KOSTL 필드 할당 및 설정
        ASSIGN COMPONENT 'KOSTL' OF STRUCTURE <GS_LINE> TO <FS_D>.
        MOVE <LS_COSP>-OBJNR+6 TO <FS_D>.

        READ TABLE GT_KOSTL ASSIGNING FIELD-SYMBOL(<LS_KOSTL>)
                            WITH KEY KOSTL = <FS_D>.

        IF SY-SUBRC = 0.

          ASSIGN COMPONENT 'KTEXT'
               OF STRUCTURE <GS_LINE> TO <FS_D2>.

          MOVE <LS_KOSTL>-KTEXT TO <FS_D2>.

        ENDIF.
      WHEN 'PR'.
        ASSIGN COMPONENT 'POSID' OF STRUCTURE <GS_LINE> TO <FS_D>.
        ASSIGN COMPONENT 'POST1' OF STRUCTURE <GS_LINE> TO <FS_D2>.

        READ TABLE GT_PRPS ASSIGNING FIELD-SYMBOL(<LS_PRPS>)
             WITH KEY OBJNR = <LS_COSP>-OBJNR.

        MOVE: <LS_PRPS>-POSID TO <FS_D>,
              <LS_PRPS>-POST1 TO <FS_D2>.

    ENDCASE.

    CASE <LS_COSP>-VERSN.

      WHEN PA_VERSN.
        LV_RFIELD = 'SDATA'. "SOURCE DATA

      WHEN OTHERS.
        LV_RFIELD = 'T_' && <LS_COSP>-VERSN."이 프로그램에서는 딱히 의미 없음.

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

*    IF <FS_S> IS INITIAL.
*      DELETE <GT_TABLE> INDEX SY-TABIX.
*      CONTINUE.
*    ENDIF.

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

*    Value Check
    ASSIGN COMPONENT 'ICON' OF STRUCTURE <GS_LINE>
      TO <FS_D2>.
    IF <FS_S> = 0 AND <FS_HIS> = 0.
      <FS_D2> = '@00@'. "이미 초기화되어있음. X
    ELSE.
      <FS_D2> = '@4A@'.  "초기화 가능
    ENDIF.

  ENDLOOP.

  OTAB = VALUE #( ( NAME = 'KOSTL'  )
                  ( NAME = 'POSID'  )
                  ( NAME = 'KSTAR'  ) ).

  SORT <GT_TABLE> BY (OTAB).


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
*& Form MAKE_TABLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM MAKE_TABLE .


  DATA LV_FIELDNAME TYPE FIELDNAME.
  DATA LV_TITLE     TYPE LVC_TXTCOL.
  DATA LV_MONTH     TYPE N LENGTH 2.

  CLEAR GT_FIELDCAT[].

  CLEAR: GT_MONTH, GT_MONTH[].

  LV_MONTH = '01'.

  DO .
    MOVE LV_MONTH TO GT_MONTH-V1.
    APPEND GT_MONTH.

    ADD 1 TO LV_MONTH.
    IF LV_MONTH > '12'.
      EXIT.
    ENDIF.
  ENDDO.

  SELECT * FROM TKVS
    INTO TABLE @GT_TKVS
   WHERE VERSI EQ @PA_VERSN.

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

*  LOOP AT GT_TKVS.
*
*    LV_FIELDNAME = 'T_' && GT_TKVS-VERSI.
*    LV_TITLE     = TEXT-T02 && '(' && GT_TKVS-VERSI && ')'.
*
*    PERFORM FILL_FIELD_CATEGORY USING :
*          'S' 'FIELDNAME'   LV_FIELDNAME,
*          ''  'OUTPUTLEN'    '23',
*          ' ' 'CFIELDNAME'  'WAERS',
*          ' ' 'REF_TABLE'   'ZCOT0040',
*          ' ' 'REF_FIELD'   'HSLVT',
*          'E' 'COLTEXT'     LV_TITLE.
*
*  ENDLOOP.

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
*& Form SALV_CALL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SALV_CALL .

* http://blog.daum.net/rightvoice/788
*https://imny.tistory.com/entry/SALV-Simple-Abap-List-View
* SALV -> 따로 스크린 페인터로 안그리고 할 수 있음. 수정불가. 기능 여러개 필요하면
*              못씀.
*          -> CLASS ALV와 다른점은 FACTORY 메소드로 인스턴스 얻어오고 여기에
*              디스플레이(필드카탈로그) 등 하위 객체를 사용함.

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
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_PF_STATUS .

  DATA: LT_FUNC_LIST TYPE SALV_T_UI_FUNC,
        LS_FUNC_LIST TYPE SALV_S_UI_FUNC.

  DATA: L_TITLE TYPE LVC_TITLE,
        L_LINES TYPE C LENGTH 100,
        L_COUNT TYPE I.

  DATA LV_TEXT TYPE CHAR100.
*
  GO_ALV->SET_SCREEN_STATUS(
    PFSTATUS      =  'STANDARD'
    REPORT        =  GV_REPID
    SET_FUNCTIONS = GO_ALV->C_FUNCTIONS_ALL ).

  L_COUNT = LINES( <GT_TABLE> ).

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
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
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
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_TOP_OF_PAGE .
  DATA: LR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.
  PERFORM BUILT_HEADER CHANGING LR_CONTENT.
  GO_ALV->SET_TOP_OF_LIST( LR_CONTENT ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILT_HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_CONTENT
*&---------------------------------------------------------------------*
FORM BUILT_HEADER CHANGING CR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.

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
    TEXT    = TEXT-T02
    TOOLTIP = TEXT-T02 ).

    SELECT SINGLE VTEXT INTO @DATA(LV_T2)
      FROM TKVS AS A
      LEFT JOIN TKVST AS B
        ON A~VERSI = B~VERSI
       AND B~SPRAS = @SY-LANGU
     WHERE A~VERSI = @PA_VERSN.

  CONCATENATE PA_VERSN '(' LV_T2 ')' INTO LV_TEXT SEPARATED BY SPACE.

  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW    = 2
    COLUMN = 2
    TEXT    = LV_TEXT
    TOOLTIP = LV_TEXT ).

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW    = 3
    COLUMN = 1
    TEXT    = TEXT-T03
    TOOLTIP = TEXT-T03 ).

  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW    = 3
    COLUMN = 2
    TEXT    = TEXT-T05
    TOOLTIP = TEXT-T05 ).

  CR_CONTENT = LR_GRID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EVENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
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
*& Form HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_SALV_FUNCTION
*&---------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND  USING P_UCOMM TYPE SALV_DE_FUNCTION.

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
*& Form SET_TABLE_SETTINGS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
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
*& Form SET_COLUMNS_TECHNICAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LR_COLUMNS
*&      --> LO_AGGRS
*&---------------------------------------------------------------------*
FORM SET_COLUMNS_TECHNICAL  USING IR_COLUMNS TYPE REF TO
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


  TRY.
      LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'OBJNR' ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

*-- SELECT FIELD 추가
  LR_SELECTIONS = GO_ALV->GET_SELECTIONS( ).
  LR_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>CELL ).

  IR_AGG->SET_AGGREGATION_BEFORE_ITEMS( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_COLUMN_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <COLUMN_REF>_COLUMNNAME
*&      <-- GV_COLUMN_TEXT
*&---------------------------------------------------------------------*
FORM SET_COLUMN_TEXT  USING P_COLUMNNAME
                      CHANGING P_COLUMN_TEXT.

  DATA L_FIELD TYPE LVC_CFNAME VALUE 'WAERS'.

  TRY.

      CLEAR P_COLUMN_TEXT.

      CASE P_COLUMNNAME.

        WHEN 'ICON'.
          P_COLUMN_TEXT = TEXT-C01.

          GR_COLUMN->SET_ICON( IF_SALV_C_BOOL_SAP=>TRUE ).
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 8 ).
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
          P_COLUMN_TEXT = TEXT-T02.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '3' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'HDATA'.
          P_COLUMN_TEXT = TEXT-T05.

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
*& Form ERROR_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
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


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_EXECUTE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_EXECUTE .

  DATA LV_RFIELD1    TYPE FIELDNAME.
  DATA LV_RFIELD2    TYPE FIELDNAME.

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
        PERFORM INITIAL_VERSION CHANGING <FS_MESSAGE> LV_ERROR.
        IF LV_ERROR IS NOT INITIAL.
          EXIT.
        ENDIF.
* 0으로 세팅
        ASSIGN COMPONENT 'SDATA' OF STRUCTURE <GS_LINE>
          TO <FS_SOUR>.
        ASSIGN COMPONENT 'HDATA' OF STRUCTURE <GS_LINE>
          TO <FS_HIST>.
        MOVE '0' TO <FS_SOUR>.
        MOVE '0' TO <FS_HIST>.

        MOVE: '@00@'   TO <FS_ICON>,
        TEXT-S01 TO <FS_MESSAGE>.

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
*& Form INITIAL_VERSION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <FS_MESSAGE>
*&      <-- LV_ERROR
*&---------------------------------------------------------------------*
FORM INITIAL_VERSION  CHANGING P_MESSAGE
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

*-- Header Data
  LS_HEADERINFO-CO_AREA     = GV_KOKRS.     "관리 회계영역
  LS_HEADERINFO-FISC_YEAR   = PA_GJAHR.     "회계연도
  LS_HEADERINFO-PERIOD_FROM = '001'.      "기간 시작
  LS_HEADERINFO-PERIOD_TO   = '012'.      "기간 종료
  LS_HEADERINFO-VERSION     = PA_VERSN.       "버전

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

*    0값 세팅
    <FS_RECV> = 0.

  ENDLOOP.
*
  APPEND LT_PERVALUE.
*
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
        DELETE FROM ZCOT0040 WHERE RVERS  = PA_VERSN
                               AND RYEAR  = PA_GJAHR
                               AND ROBJNR = <FS_OBJNR>
                               AND RKSTAR = <FS_KSTAR>.

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
*& text
*&---------------------------------------------------------------------*
*&      --> GT_RETURN
*&      <-- P_MESSAGE
*&---------------------------------------------------------------------*
FORM BUILD_MESSAGE  USING    PS_MESSAGE STRUCTURE BAPIRET2
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
*& Form READ_HIERARCHY_TABLES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_VALUES
*&      --> P_
*&      --> PA_PDGR
*&---------------------------------------------------------------------*
FORM READ_HIERARCHY_TABLES  TABLES PT_VALUES STRUCTURE GRPVALUES
                            USING PV_CLASS TYPE SETCLASS
                                  PV_SETID.

  DATA: LV_SETID     LIKE SETHIER-SETID,
        LV_OVERWRITE LIKE SY-DATAR,
        LT_INFO      LIKE GRPHINFO OCCURS 0 WITH HEADER LINE,
        LT_NODES     LIKE GRPOBJECTS OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'G_SET_ENCRYPT_SETID'
    EXPORTING
      SETCLASS  = PV_CLASS
      SHORTNAME = PV_SETID
    IMPORTING
      SETID     = LV_SETID.

  IF SY-SUBRC = 0.

    CALL FUNCTION 'K_HIERARCHY_TABLES_READ'
      EXPORTING
        E_CLASS                     = PV_CLASS
        E_SETID                     = LV_SETID
        E_KOKRS                     = '1000'
      TABLES
        T_NODES                     = LT_NODES
        T_VALUES                    = PT_VALUES
      CHANGING
        C_INFO                      = LT_INFO
        C_OVERWRITE                 = LV_OVERWRITE
      EXCEPTIONS
        NO_CONTROLLING_AREA         = 1
        NO_CHART_OF_ACCOUNT         = 2
        DIFFERENT_CONTROLLING_AREAS = 3
        DIFFERENT_CHART_OF_ACCOUNTS = 4
        SET_NOT_FOUND               = 5
        ILLEGAL_FIELD_REPLACEMENT   = 6
        ILLEGAL_TABLE_REPLACEMENT   = 7
        FM_RAISE                    = 8
        CONVERT_ERROR               = 9
        NO_OVERWRITE_STANDARD_HIER  = 10
        NO_BUKRS_FOR_KOKRS          = 11
        OTHERS                      = 12.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_PDGR
*&---------------------------------------------------------------------*
FORM F4_PDGR  CHANGING P_PDGR.

  DATA: HELP_SETNR     LIKE RGSMH-SETNR,
        HELP_SEARCHFLD LIKE RGSMH-SEARCHFLD,
        HELP_SET       LIKE RGSBS-SETNR,
        HELP_SETCLASS  LIKE RGSMH-CLASS.

  MOVE '1000' TO HELP_SEARCHFLD.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
      CLASS           = '0110'
      FIELD_NAME      = 'POSID'
      SEARCHFLD       = HELP_SEARCHFLD
      SEARCHFLD_INPUT = ' '
      SET             = HELP_SET
    IMPORTING
      SET_NAME        = HELP_SETNR
    EXCEPTIONS
      NO_SET_PICKED   = 1.

  IF SY-SUBRC = 0.
    P_PDGR = HELP_SETNR.
  ENDIF.

ENDFORM.

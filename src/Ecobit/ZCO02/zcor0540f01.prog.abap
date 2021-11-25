*&---------------------------------------------------------------------*
*& Include          ZCOR0540F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION .

  GV_MODE = GC_D. " 조회모드 기본값

  TEXT_S01 = '실행기준'(S01).
  TEXT_S02 = 'WBS별 매핑'(S02).
  TEXT_S03 = 'WBS속성기준 매핑'(S03).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  CASE GC_X.
    WHEN P_R01.
      CALL TRANSACTION 'ZCOV1320' .
    WHEN P_R02.
      PERFORM SELECTED_DATA_R02.
      CALL SCREEN 0100.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_R02
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_R02 .

  PERFORM CLEAR_ITAB.
  PERFORM SELECT_DB.
  PERFORM SELECT_OTHERS.

  PERFORM MAKE_DISPLAY_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR_ITAB
*&---------------------------------------------------------------------*
FORM CLEAR_ITAB .

  _CLEAR_ITAB : GT_DATA,
                GT_DISPLAY,
                GT_1310,
                GT_1040,
                GT_1100,
                GT_T2501.

  CLEAR GV_DRDN_HANDLE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_DB
*&---------------------------------------------------------------------*
FORM SELECT_DB .

  SELECT A~BUKRS,
         B~BUTXT,
         A~ZZBGU,
         A~ZZBGD,
         A~ZZPRG,
         A~WW120,
         A~ERDAT,
         A~ERZET,
         A~ERNAM,
         A~AEDAT,
         A~AEZET,
         A~AENAM
    FROM ZCOT1310 AS A LEFT JOIN T001 AS B ON B~BUKRS EQ A~BUKRS
    INTO CORRESPONDING FIELDS OF TABLE @GT_DATA.


  SORT GT_DATA BY BUKRS
                  ZZBGU
                  ZZBGD
                  ZZPRG
                  WW120.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECT_DB
*&---------------------------------------------------------------------*
FORM SELECT_OTHERS.

  SELECT FROM ZCOT1040  AS A
    LEFT JOIN ZCOT1040T AS B  ON  B~SPRAS EQ @SY-LANGU
                              AND B~ZZBGU EQ A~ZZBGU
         FIELDS   A~ZZBGU, B~ZZBGUTX
         ORDER BY A~ZZBGU
         INTO TABLE @GT_1040 .


  SELECT FROM ZCOT1050  AS A
    LEFT JOIN ZCOT1050T AS B  ON  B~SPRAS EQ @SY-LANGU
                              AND B~ZZBGU EQ A~ZZBGU
                              AND B~ZZBGD EQ A~ZZBGD
         FIELDS   A~ZZBGU, A~ZZBGD, B~ZZBGDTX
         ORDER BY A~ZZBGU, A~ZZBGD
         INTO TABLE @GT_1050 .

  SELECT FROM ZCOT1100  AS A
    LEFT JOIN ZCOT1100T AS B  ON  B~SPRAS EQ @SY-LANGU
                              AND B~ZZPRG EQ A~ZZPRG
         FIELDS   A~ZZPRG, B~ZZPRGTX
         ORDER BY A~ZZPRG
         INTO TABLE @GT_1100.

  SELECT FROM T2501  AS A
    LEFT JOIN T25A1  AS B  ON  B~SPRAS EQ @SY-LANGU
                           AND B~WW120 EQ A~WW120
         FIELDS   A~WW120, B~BEZEK
         ORDER BY A~WW120
         INTO TABLE @GT_T2501.

  PERFORM MAKE_DRDN_VALUE TABLES : GT_1040,
                                   GT_1100,
                                   GT_T2501.

  LOOP AT GT_1040 INTO GS_1040.

    READ TABLE GT_1050 TRANSPORTING NO FIELDS
                       WITH KEY ZZBGU = GS_1040-ZZBGU
                                BINARY SEARCH.

    CHECK SY-SUBRC EQ 0.

    LOOP AT GT_1050 INTO GS_1050 FROM SY-TABIX.
      IF GS_1040-ZZBGU NE GS_1050-ZZBGU.
        EXIT.
      ENDIF.

      GS_1050-HANDLE = GS_1040-HANDLE2.
      GS_1050-VALUE  = |{ GS_1050-ZZBGD } { GS_1050-ZZBGDTX }|.
      MODIFY GT_1050 FROM GS_1050.
    ENDLOOP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DRDN_VALUE
*&---------------------------------------------------------------------*
FORM MAKE_DRDN_VALUE  TABLES PT_DATA TYPE TABLE.

  FIELD-SYMBOLS: <FS>,
                 <FS_HANDLE>,
                 <FS_VALUE>.

  ADD 1 TO GV_DRDN_HANDLE.

  DATA LV_DRDN_HANDLE LIKE GV_DRDN_HANDLE.
  LV_DRDN_HANDLE = GV_DRDN_HANDLE.

  LOOP AT PT_DATA ASSIGNING FIELD-SYMBOL(<FS_WA>).

    ASSIGN COMPONENT 'HANDLE' OF STRUCTURE <FS_WA> TO <FS_HANDLE>.
    IF SY-SUBRC EQ 0.
      <FS_HANDLE> = LV_DRDN_HANDLE.
    ENDIF.

    ASSIGN COMPONENT 'HANDLE2' OF STRUCTURE <FS_WA> TO <FS_HANDLE>.
    IF SY-SUBRC EQ 0.
      ADD 1 TO GV_DRDN_HANDLE.
      <FS_HANDLE> = GV_DRDN_HANDLE.
    ENDIF.



    ASSIGN COMPONENT 'VALUE'  OF STRUCTURE <FS_WA> TO <FS_VALUE>.
    IF SY-SUBRC EQ 0.
      DO 2 TIMES.
        ASSIGN COMPONENT SY-INDEX OF STRUCTURE <FS_WA> TO <FS>.
        IF SY-SUBRC NE 0.
          EXIT.
        ENDIF.

        IF <FS_VALUE> IS INITIAL.
          <FS_VALUE> = <FS>.
        ELSE.
          CONCATENATE <FS_VALUE> <FS>
                 INTO <FS_VALUE> SEPARATED BY SPACE.
        ENDIF.

        UNASSIGN <FS>.
      ENDDO.
      UNASSIGN <FS_VALUE>.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM MAKE_DISPLAY_DATA .

  LOOP AT GT_DATA INTO GS_DATA.

    GS_DISPLAY = CORRESPONDING #( GS_DATA ).

    IF GS_DATA-ZZBGU IS NOT INITIAL.
      READ TABLE GT_1040 INTO GS_1040
                         WITH KEY ZZBGU = GS_DATA-ZZBGU
                                  BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        GS_DISPLAY-ZZBGU_DRDN   = GS_1040-VALUE.
        " 사업구분이 지정되면, 세부사업 Dropdown Handle 도 기록한다.
        GS_DISPLAY-ZZBGD_HANDLE = GS_1040-HANDLE2.
      ELSE.
        GS_DISPLAY-ZZBGU_DRDN   = GS_DATA-ZZBGU && '(제거된 속성)'.
      ENDIF.
    ENDIF.


    IF GS_DATA-ZZBGD IS NOT INITIAL.
      READ TABLE GT_1050 INTO GS_1050
                         WITH KEY ZZBGU = GS_DATA-ZZBGU
                                  ZZBGD = GS_DATA-ZZBGD
                                  BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        GS_DISPLAY-ZZBGD_DRDN = GS_1050-VALUE.
      ELSE.
        GS_DISPLAY-ZZBGD_DRDN = GS_DATA-ZZBGD && '(제거된 속성)'.
      ENDIF.
    ENDIF.

    IF GS_DATA-ZZPRG IS NOT INITIAL.
      READ TABLE GT_1100 INTO GS_1100
                         WITH KEY ZZPRG = GS_DATA-ZZPRG
                                  BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        GS_DISPLAY-ZZPRG_DRDN = GS_1100-VALUE.
      ELSE.
        GS_DISPLAY-ZZPRG_DRDN = GS_DATA-ZZPRG && '(제거된 속성)'.
      ENDIF.
    ENDIF.


    IF GS_DATA-WW120 IS NOT INITIAL.
      READ TABLE GT_T2501 INTO GS_T2501
                         WITH KEY WW120 = GS_DATA-WW120
                                  BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        GS_DISPLAY-WW120_DRDN = GS_T2501-VALUE.
      ELSE.
        GS_DISPLAY-WW120_DRDN = GS_DATA-WW120 && '(제거된 속성)'.
      ENDIF.
    ENDIF.

    APPEND GS_DISPLAY TO GT_DISPLAY.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_MAIN_GRID_0100
*&---------------------------------------------------------------------*
FORM CREATE_MAIN_GRID_0100 .

  IF GR_ALV IS NOT BOUND.
    GR_ALV = NEW #( GR_CON ).
  ENDIF.

  PERFORM MAKE_FIELDCATALOG_0100.
  PERFORM REGISTER_EVENT_0100.

  GR_ALV->SET_LAYOUT(
    I_TYPE       = 'C'
    I_BOX_FNAME  = 'MARK'
    I_STYLEFNAME = 'STYLE'
    I_CTAB_FNAME = 'COLOR'
  ).
  GR_ALV->SET_SORT( IT_FIELD = VALUE #( ( 'BUKRS' )
                                        ( 'BUTXT' )
                                        ( 'ZZBGU_DRDN' )
                                        ( 'ZZBGD_DRDN' ) ) ).
  GR_ALV->DISPLAY( CHANGING T_OUTTAB = GT_DISPLAY ).

*  CREATE OBJECT GR_GRID
*    EXPORTING
**      I_SHELLSTYLE            = 0                " Control Style
**      I_LIFETIME              =                  " Lifetime
*      I_PARENT                = GR_CON                 " Parent Container
**      I_APPL_EVENTS           = SPACE            " Register Events as Application Events
**      I_PARENTDBG             =                  " Internal, Do not Use
**      I_APPLOGPARENT          =                  " Container for Application Log
**      I_GRAPHICSPARENT        =                  " Container for Graphics
**      I_NAME                  =                  " Name
**      I_FCAT_COMPLETE         = SPACE            " Boolean Variable (X=True, Space=False)
**      O_PREVIOUS_SRAL_HANDLER =
*    EXCEPTIONS
*      ERROR_CNTL_CREATE       = 1                " Error when creating the control
*      ERROR_CNTL_INIT         = 2                " Error While Initializing Control
*      ERROR_CNTL_LINK         = 3                " Error While Linking Control
*      ERROR_DP_CREATE         = 4                " Error While Creating DataProvider Control
*      OTHERS                  = 5
*    .
*  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
*
*  DATA LT_FIELDCAT_KKB TYPE KKBLO_T_FIELDCAT.
*  DATA LT_FIELDCAT_LVC TYPE LVC_T_FCAT.
*
*  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
*    EXPORTING
*      I_CALLBACK_PROGRAM     = SY-REPID          " Internal table declaration program
*      I_INCLNAME             = SY-REPID
*      I_TABNAME              = 'GS_DISPLAY'        " Name of table to be displayed
*      I_BYPASSING_BUFFER     = GC_X              " Ignore buffer while reading
**      I_STRUCNAME            =
**      I_BUFFER_ACTIVE        =
*    CHANGING
*      CT_FIELDCAT            = LT_FIELDCAT_KKB     " Field Catalog with Field Descriptions
*    EXCEPTIONS
*      INCONSISTENT_INTERFACE = 1
*      OTHERS                 = 2.
*
*  CALL FUNCTION 'LVC_TRANSFER_FROM_KKBLO'
*    EXPORTING
*      IT_FIELDCAT_KKBLO         = LT_FIELDCAT_KKB
*    IMPORTING
*      ET_FIELDCAT_LVC           = LT_FIELDCAT_LVC
*    EXCEPTIONS
*      IT_DATA_MISSING           = 1
*      OTHERS                    = 2.
*
*
*
*  CALL METHOD GR_GRID->SET_TABLE_FOR_FIRST_DISPLAY
**    EXPORTING
**      I_BUFFER_ACTIVE               =                  " Buffering Active
**      I_BYPASSING_BUFFER            =                  " Switch Off Buffer
**      I_CONSISTENCY_CHECK           =                  " Starting Consistency Check for Interface Error Recognition
**      I_STRUCTURE_NAME              =                  " Internal Output Table Structure Name
**      IS_VARIANT                    =                  " Layout
**      I_SAVE                        =                  " Save Layout
**      I_DEFAULT                     = GC_X              " Default Display Variant
**      IS_LAYOUT                     =                  " Layout
**      IS_PRINT                      =                  " Print Control
**      IT_SPECIAL_GROUPS             =                  " Field Groups
**      IT_TOOLBAR_EXCLUDING          =                  " Excluded Toolbar Standard Functions
**      IT_HYPERLINK                  =                  " Hyperlinks
**      IT_ALV_GRAPHICS               =                  " Table of Structure DTC_S_TC
**      IT_EXCEPT_QINFO               =                  " Table for Exception Tooltip
**      IR_SALV_ADAPTER               =                  " Interface ALV Adapter
*    CHANGING
*      IT_OUTTAB                     = GT_DISPLAY        " Output Table
*      IT_FIELDCATALOG               = LT_FIELDCAT_LVC   " Field Catalog
**      IT_SORT                       =                  " Sort Criteria
**      IT_FILTER                     =                  " Filter Criteria
*    EXCEPTIONS
*      INVALID_PARAMETER_COMBINATION = 1                " Wrong Parameter
*      PROGRAM_ERROR                 = 2                " Program Errors
*      TOO_MANY_LINES                = 3                " Too many Rows in Ready for Input Grid
*      OTHERS                        = 4
*    .
*  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

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


  LOOP AT GR_ALV->MT_FIELDCAT INTO DATA(LS_FIELDCAT).

    CLEAR LV_TEXT.
    CLEAR LS_FIELDCAT-KEY.
    CLEAR LS_FIELDCAT-COL_OPT.

    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'BUKRS'.
        LS_FIELDCAT-OUTPUTLEN = 10.
        LS_FIELDCAT-EDIT = GC_X.
        LS_FIELDCAT-KEY  = GC_X.

      WHEN 'BUTXT'.
        LS_FIELDCAT-OUTPUTLEN = 25.
        LS_FIELDCAT-EDIT = SPACE.
        LS_FIELDCAT-EMPHASIZE = 'C500'.

      WHEN 'ZZBGU_DRDN'.
        LS_FIELDCAT-OUTPUTLEN = 15.
        LS_FIELDCAT-EDIT = GC_X.
        PERFORM MAKE_DROPDOWN CHANGING LS_FIELDCAT.

      WHEN 'ZZBGD_DRDN'.
        LS_FIELDCAT-OUTPUTLEN = 25.
        LS_FIELDCAT-EDIT = GC_X.
        PERFORM MAKE_DROPDOWN CHANGING LS_FIELDCAT.

      WHEN 'ZZPRG_DRDN'.
        LS_FIELDCAT-OUTPUTLEN = 15.
        LS_FIELDCAT-EDIT = GC_X.
        PERFORM MAKE_DROPDOWN CHANGING LS_FIELDCAT.

      WHEN 'WW120_DRDN'.
        LS_FIELDCAT-OUTPUTLEN = 15.
        LS_FIELDCAT-EDIT = GC_X.
        PERFORM MAKE_DROPDOWN CHANGING LS_FIELDCAT.
        LS_FIELDCAT-EMPHASIZE = 'C300'.

      WHEN OTHERS.
        LS_FIELDCAT-TECH = GC_X.

    ENDCASE.


    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'BUKRS'.        LV_TEXT = '회사코드'(F01).
      WHEN 'ZZBGU_DRDN'.   LV_TEXT = '사업구분'(F02).
      WHEN 'ZZBGD_DRDN'.   LV_TEXT = '세부사업'(F03).
      WHEN 'ZZPRG_DRDN'.   LV_TEXT = '발주처유형'(F04).
      WHEN 'WW120_DRDN'.   LV_TEXT = 'BU구분'(F05).
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
*& Form MAKE_DROPDOWN
*&---------------------------------------------------------------------*
FORM MAKE_DROPDOWN CHANGING PS_FIELDCAT TYPE LVC_S_FCAT.

  DATA LT_DROP TYPE LVC_T_DROP.


  PS_FIELDCAT-CHECKTABLE = '!'.

  "-- Dropdown List 구성
  CASE PS_FIELDCAT-FIELDNAME.

    WHEN 'ZZBGU_DRDN'.

      READ TABLE GT_1040 INTO GS_1040 INDEX 1.
      IF SY-SUBRC EQ 0.
        PS_FIELDCAT-DRDN_HNDL = GS_1040-HANDLE.
      ENDIF.

      LT_DROP[] = CORRESPONDING #( GT_1040 ).

    WHEN 'ZZBGD_DRDN'.

      CLEAR PS_FIELDCAT-DRDN_HNDL.
      PS_FIELDCAT-DRDN_FIELD = 'ZZBGD_HANDLE'.

      LT_DROP[] = CORRESPONDING #( GT_1050 ).

    WHEN 'ZZPRG_DRDN'.

      READ TABLE GT_1100 INTO GS_1100 INDEX 1.
      IF SY-SUBRC EQ 0.
        PS_FIELDCAT-DRDN_HNDL = GS_1100-HANDLE.
      ENDIF.

      LT_DROP[] = CORRESPONDING #( GT_1100 ).

    WHEN 'WW120_DRDN'.

      READ TABLE GT_T2501 INTO GS_T2501 INDEX 1.
      IF SY-SUBRC EQ 0.
        PS_FIELDCAT-DRDN_HNDL = GS_T2501-HANDLE.
      ENDIF.

      LT_DROP[] = CORRESPONDING #( GT_T2501 ).

    WHEN OTHERS.

      EXIT.

  ENDCASE.


  GR_ALV->MR_ALV_GRID->SET_DROP_DOWN_TABLE(
    IT_DROP_DOWN = LT_DROP
  ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_DATA
*&---------------------------------------------------------------------*
FORM SAVE_DATA .

  IF GC_X NE ZCL_CO_COMMON=>POPUP_CONFIRM(
      I_TITLEBAR = '확인'(PT1)
      I_QUESTION = CONV #( '저장하시겠습니까?'(QT1) )
  ).
    MESSAGE '취소되었습니다.' TYPE GC_S DISPLAY LIKE GC_W.
    EXIT.
  ENDIF.


  PERFORM CHECK_DATA.
  CHECK GV_EXIT IS INITIAL.

  DELETE FROM ZCOT1310.
  MODIFY      ZCOT1310 FROM TABLE @GT_1310.

  IF SY-SUBRC EQ 0.
    COMMIT WORK.
    MESSAGE '저장이 완료되었습니다.' TYPE GC_S.

    PERFORM REFRESH_DATA.
  ELSE.
    ROLLBACK WORK.
    MESSAGE '저장이 실패되었습니다.' TYPE GC_S DISPLAY LIKE GC_E.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATA
*&---------------------------------------------------------------------*
FORM CHECK_DATA.

  DATA LS_1310 LIKE LINE OF GT_1310.
  DATA LS_TEMP LIKE LINE OF GT_1310.

  DATA LT_COLOR TYPE LVC_T_SCOL.
  DATA LS_COLOR TYPE LVC_S_SCOL.

  DATA LT_MESSAGE TYPE TABLE OF STRING WITH HEADER LINE.

  LS_COLOR = VALUE #( COLOR = VALUE #( COL = 6 ) ) .


  CLEAR GV_EXIT.
  REFRESH GT_1310.


  SELECT BUKRS,
         ZZBGU,
         ZZBGD,
         ZZPRG,
         WW120
    FROM ZCOT1310
    INTO TABLE @DATA(LT_1310).

  SORT LT_1310 BY BUKRS ZZBGU ZZBGD WW120.



  LOOP AT GT_DISPLAY INTO GS_DISPLAY.

    REFRESH: GS_DISPLAY-COLOR.

    IF GS_DISPLAY-BUKRS IS INITIAL.
      LS_COLOR-FNAME = 'BUKRS'.
      APPEND LS_COLOR TO GS_DISPLAY-COLOR.

      LT_MESSAGE = '회사코드는 필수값 입니다.'.
      APPEND LT_MESSAGE.
      GV_EXIT = GC_X.
    ENDIF.

    CLEAR GS_1040.
    IF GS_DISPLAY-ZZBGU_DRDN IS NOT INITIAL.
      READ TABLE GT_1040 INTO GS_1040 WITH KEY VALUE = GS_DISPLAY-ZZBGU_DRDN.
      IF SY-SUBRC NE 0.
        LS_COLOR-FNAME = 'ZZBGU_DRDN'.
        APPEND LS_COLOR TO GS_DISPLAY-COLOR.

        LT_MESSAGE = '알 수 없는 사업구분 입니다.'.
        APPEND LT_MESSAGE.
        GV_EXIT = GC_X.
      ENDIF.
    ENDIF.

    CLEAR GS_1050.
    IF GS_DISPLAY-ZZBGD_DRDN IS NOT INITIAL.
      READ TABLE GT_1050 INTO GS_1050 WITH KEY VALUE = GS_DISPLAY-ZZBGD_DRDN.
      IF SY-SUBRC NE 0.
        LS_COLOR-FNAME = 'ZZBGD_DRDN'.
        APPEND LS_COLOR TO GS_DISPLAY-COLOR.

        LT_MESSAGE = '알 수 없는 세부사업 입니다.'.
        APPEND LT_MESSAGE.
        GV_EXIT = GC_X.
      ENDIF.
    ENDIF.

    CLEAR GS_1100.
    IF GS_DISPLAY-ZZPRG_DRDN IS NOT INITIAL.
      READ TABLE GT_1100 INTO GS_1100 WITH KEY VALUE = GS_DISPLAY-ZZPRG_DRDN.
      IF SY-SUBRC NE 0.
        LS_COLOR-FNAME = 'ZZPRG_DRDN'.
        APPEND LS_COLOR TO GS_DISPLAY-COLOR.

        LT_MESSAGE = '알 수 없는 발주처유형 입니다.'.
        APPEND LT_MESSAGE.
        GV_EXIT = GC_X.
      ENDIF.
    ENDIF.

    CLEAR GS_T2501.

    IF GS_DISPLAY-WW120_DRDN IS INITIAL.
      LS_COLOR-FNAME = 'WW120_DRDN'.
      APPEND LS_COLOR TO GS_DISPLAY-COLOR.

      LT_MESSAGE = 'BU구분은 필수값 입니다.'.
      APPEND LT_MESSAGE.
      GV_EXIT = GC_X.
    ELSE.
      READ TABLE GT_T2501 INTO GS_T2501 WITH KEY VALUE = GS_DISPLAY-WW120_DRDN.
      IF SY-SUBRC NE 0.
        LS_COLOR-FNAME = 'WW120_DRDN'.
        APPEND LS_COLOR TO GS_DISPLAY-COLOR.

        LT_MESSAGE = '알 수 없는 BU구분 입니다.'.
        APPEND LT_MESSAGE.
        GV_EXIT = GC_X.
      ENDIF.
    ENDIF.

    MODIFY GT_DISPLAY FROM GS_DISPLAY TRANSPORTING COLOR.

    IF GS_DISPLAY-ERDAT IS INITIAL.
      GS_DISPLAY-ERDAT = SY-DATUM.
      GS_DISPLAY-ERZET = SY-UZEIT.
      GS_DISPLAY-ERNAM = SY-UNAME.
      GS_DISPLAY-AEDAT = SY-DATUM.
      GS_DISPLAY-AEZET = SY-UZEIT.
      GS_DISPLAY-AENAM = SY-UNAME.
    ELSE.
      READ TABLE LT_1310 TRANSPORTING NO FIELDS WITH KEY BUKRS = GS_DISPLAY-BUKRS
                                                         ZZBGU = GS_1040-ZZBGU
                                                         ZZBGD = GS_1050-ZZBGD
                                                         ZZPRG = GS_1100-ZZPRG
                                                         WW120 = GS_T2501-WW120
                                                         BINARY SEARCH.
      IF SY-SUBRC NE 0.
        GS_DISPLAY-AEDAT = SY-DATUM.
        GS_DISPLAY-AEZET = SY-UZEIT.
        GS_DISPLAY-AENAM = SY-UNAME.
      ENDIF.
    ENDIF.

    CLEAR LS_1310.
    LS_1310 = VALUE #(
      BUKRS = GS_DISPLAY-BUKRS
      ZZBGU = GS_1040-ZZBGU
      ZZBGD = GS_1050-ZZBGD
      ZZPRG = GS_1100-ZZPRG
      WW120 = GS_T2501-WW120
      ERDAT = GS_DISPLAY-ERDAT
      ERZET = GS_DISPLAY-ERZET
      ERNAM = GS_DISPLAY-ERNAM
      AEDAT = GS_DISPLAY-AEDAT
      AEZET = GS_DISPLAY-AEZET
      AENAM = GS_DISPLAY-AENAM
    ).

    APPEND LS_1310 TO GT_1310.

  ENDLOOP.

  " 중복여부 체크
  SORT GT_1310 BY BUKRS ZZBGU ZZBGD ZZPRG.

  LOOP AT GT_1310 INTO LS_1310.
    IF LS_TEMP-BUKRS EQ LS_1310-BUKRS AND
       LS_TEMP-ZZBGU EQ LS_1310-ZZBGU AND
       LS_TEMP-ZZBGD EQ LS_1310-ZZBGD AND
       LS_TEMP-ZZPRG EQ LS_1310-ZZPRG .

      GV_EXIT = GC_X.

      LOOP AT GT_DISPLAY INTO GS_DISPLAY WHERE BUKRS         EQ LS_1310-BUKRS
                                           AND ZZBGU_DRDN(1) EQ LS_1310-ZZBGU
                                           AND ZZBGD_DRDN(2) EQ LS_1310-ZZBGD
                                           AND ZZPRG_DRDN(3) EQ LS_1310-ZZPRG.

        LS_COLOR-FNAME    = SPACE.
        LS_COLOR-NOKEYCOL = GC_X.
        APPEND LS_COLOR TO GS_DISPLAY-COLOR.

*        LS_COLOR-FNAME = 'ZZBGU_DRDN'.
*        APPEND LS_COLOR TO GS_DISPLAY-COLOR.
*
*        LS_COLOR-FNAME = 'ZZBGD_DRDN'.
*        APPEND LS_COLOR TO GS_DISPLAY-COLOR.
*
*        LS_COLOR-FNAME = 'ZZPRG_DRDN'.
*        APPEND LS_COLOR TO GS_DISPLAY-COLOR.

        MODIFY GT_DISPLAY FROM GS_DISPLAY TRANSPORTING COLOR.
      ENDLOOP.

      LT_MESSAGE = '중복된 BU 매핑정보 입니다.'.
      APPEND LT_MESSAGE.

    ENDIF.

    LS_TEMP = LS_1310.
  ENDLOOP.


  IF LT_MESSAGE[] IS NOT INITIAL.
    READ TABLE LT_MESSAGE INDEX 1.

    MESSAGE LT_MESSAGE TYPE 'I' DISPLAY LIKE GC_E.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED  USING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                                PV_ONF4
                                PV_ONF4_BEFORE
                                PV_ONF4_AFTER
                                PV_UCOMM
                                PR_SENDER TYPE REF TO CL_GUI_ALV_GRID.

  CHECK PR_SENDER EQ GR_ALV->MR_ALV_GRID.

  DATA(LT_MOD) = PR_DATA_CHANGED->MT_MOD_CELLS.

  LOOP AT LT_MOD INTO DATA(LS_MOD).

    CLEAR GS_DISPLAY.
    READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX LS_MOD-ROW_ID.
    CHECK SY-SUBRC EQ 0.

    CASE LS_MOD-FIELDNAME.
      WHEN 'BUKRS'.

        IF GS_DISPLAY-BUKRS NE LS_MOD-VALUE.

          IF GT_T001 IS INITIAL.
            PERFORM SELECT_T001.
          ENDIF.

          CLEAR GS_T001.
          READ TABLE GT_T001 INTO GS_T001 WITH KEY BUKRS = LS_MOD-VALUE BINARY SEARCH.

          PR_DATA_CHANGED->MODIFY_CELL(
            I_ROW_ID    = LS_MOD-ROW_ID " Row ID
            I_TABIX     = LS_MOD-TABIX  " Row Index
            I_FIELDNAME = 'BUTXT'       " Field Name
            I_VALUE     = GS_T001-BUTXT " Value
          ).
        ENDIF.

      WHEN 'ZZBGU_DRDN'.
        IF GS_DISPLAY-ZZBGU_DRDN NE LS_MOD-VALUE.
          PR_DATA_CHANGED->MODIFY_CELL(
            I_ROW_ID    = LS_MOD-ROW_ID " Row ID
            I_TABIX     = LS_MOD-TABIX  " Row Index
            I_FIELDNAME = 'ZZBGD_DRDN'  " Field Name
            I_VALUE     = SPACE         " Value
          ).
          GV_REFRESH = GC_X.
        ENDIF.
    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_FINISHED
*&---------------------------------------------------------------------*
FORM HANDLE_FINISHED  USING PV_MODIFIED
                            PT_GOOD_CELLS TYPE LVC_T_MODI
                            PR_SENDER TYPE REF TO CL_GUI_ALV_GRID.

  CHECK GV_REFRESH EQ GC_X.
  CLEAR GV_REFRESH.

  DATA(LT_GOOD_CELLS) = PT_GOOD_CELLS.


  SORT LT_GOOD_CELLS BY FIELDNAME ROW_ID.

  READ TABLE LT_GOOD_CELLS TRANSPORTING NO FIELDS
                           WITH KEY FIELDNAME = 'ZZBGU_DRDN'
                                    BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    LOOP AT LT_GOOD_CELLS INTO DATA(LS_GOOD_CELLS) FROM SY-TABIX.
      IF LS_GOOD_CELLS-FIELDNAME NE 'ZZBGU_DRDN'.
        EXIT.
      ENDIF.

      READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX LS_GOOD_CELLS-ROW_ID.
      CHECK SY-SUBRC EQ 0.

      CLEAR GS_1040.
*      READ TABLE GT_1040 INTO GS_1040 WITH KEY VALUE = LS_GOOD_CELLS-VALUE.
      READ TABLE GT_1040 INTO GS_1040 WITH KEY VALUE = GS_DISPLAY-ZZBGU_DRDN.
      GS_DISPLAY-ZZBGD_HANDLE = GS_1040-HANDLE2.

      MODIFY GT_DISPLAY FROM GS_DISPLAY INDEX LS_GOOD_CELLS-ROW_ID TRANSPORTING ZZBGD_HANDLE .

    ENDLOOP.
  ENDIF.


  PR_SENDER->REFRESH_TABLE_DISPLAY(
    EXPORTING
      IS_STABLE      = VALUE #( ROW = GC_X
                                COL = GC_X )  " With Stable Rows/Columns
      I_SOFT_REFRESH = GC_X                   " Without Sort, Filter, etc.
    EXCEPTIONS
      FINISHED       = 1                " Display was Ended (by Export)
      OTHERS         = 2
  ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form REGISTER_EVENT_0100
*&---------------------------------------------------------------------*
FORM REGISTER_EVENT_0100 .

  GR_ALV->MR_ALV_GRID->REGISTER_EDIT_EVENT(
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED " Event ID
    EXCEPTIONS
      ERROR      = 1                " Error
      OTHERS     = 2
  ).

  IF GR_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_EVENT_RECEIVER.
  ENDIF.

  SET HANDLER GR_EVENT_RECEIVER->ON_DATA_CHANGED FOR GR_ALV->MR_ALV_GRID.
  SET HANDLER GR_EVENT_RECEIVER->ON_FINISHED     FOR GR_ALV->MR_ALV_GRID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_T001
*&---------------------------------------------------------------------*
FORM SELECT_T001 .

  REFRESH GT_T001.

  SELECT BUKRS,
         BUTXT
    FROM T001
    INTO TABLE @GT_T001.

  SORT GT_T001 BY BUKRS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_DATA
*&---------------------------------------------------------------------*
FORM REFRESH_DATA .

  PERFORM SELECTED_DATA_R02.

  LOOP AT GR_ALV->MT_FIELDCAT INTO DATA(LS_FIELDCAT).
    PERFORM MAKE_DROPDOWN USING LS_FIELDCAT.
  ENDLOOP.

  GR_ALV->REFRESH( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_UPDATE_INFO
*&---------------------------------------------------------------------*
FORM SET_UPDATE_INFO .

  DATA: LV_DATE TYPE CHAR12,
        LV_TIME TYPE CHAR10,
        LV_NAME TYPE CHAR40.


  SELECT USR21~BNAME,
         ADRP~DATE_FROM,
         ADRP~NATION,
         ADRP~NAME_TEXT
    FROM USR21
    JOIN ADRP   ON ADRP~PERSNUMBER EQ USR21~PERSNUMBER
   WHERE ADRP~DATE_FROM LE @SY-DATUM
    INTO TABLE @DATA(LT_USR21).

  SORT LT_USR21 BY BNAME
                   DATE_FROM.

  DELETE ADJACENT DUPLICATES FROM LT_USR21 COMPARING BNAME.




  SELECT AEDAT,
         AEZET,
         AENAM
    FROM ZCOT1320
    INTO TABLE @DATA(LT_1320).

  IF SY-SUBRC EQ 0.

    SORT LT_1320 BY AEDAT DESCENDING
                    AEZET DESCENDING.

    READ TABLE LT_1320 INTO DATA(LS_1320) INDEX 1.
    IF SY-SUBRC EQ 0.
      WRITE LS_1320-AEDAT TO LV_DATE.
      WRITE LS_1320-AEZET TO LV_TIME.

      READ TABLE LT_USR21 INTO DATA(LS_USR21)
                          WITH KEY BNAME = LS_1320-AENAM
                                   BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LV_NAME = LS_1320-AENAM && '(' && LS_USR21-NAME_TEXT && ')'.
      ELSE.
        LV_NAME = LS_1320-AENAM.
      ENDIF.


      CONCATENATE LV_DATE LV_TIME LV_NAME
             INTO TEXT_S04 SEPARATED BY ' / '.
    ENDIF.

  ENDIF.


  SELECT AEDAT,
         AEZET,
         AENAM
    FROM ZCOT1310
    INTO TABLE @DATA(LT_1310).

  IF SY-SUBRC EQ 0.

    SORT LT_1310 BY AEDAT DESCENDING
                    AEZET DESCENDING.

    READ TABLE LT_1310 INTO DATA(LS_1310) INDEX 1.
    IF SY-SUBRC EQ 0.
      WRITE LS_1310-AEDAT TO LV_DATE.
      WRITE LS_1310-AEZET TO LV_TIME.

      READ TABLE LT_USR21 INTO LS_USR21
                          WITH KEY BNAME = LS_1310-AENAM
                                   BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LV_NAME = LS_1310-AENAM && '(' && LS_USR21-NAME_TEXT && ')'.
      ELSE.
        LV_NAME = LS_1310-AENAM.
      ENDIF.

      CONCATENATE LV_DATE LV_TIME LV_NAME
             INTO TEXT_S05 SEPARATED BY ' / '.
    ENDIF.

  ENDIF.

ENDFORM.

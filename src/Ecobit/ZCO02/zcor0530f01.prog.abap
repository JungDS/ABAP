*&---------------------------------------------------------------------*
*& Include          ZCOR0530F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION .

  GV_REPID = SY-REPID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM SCRFIELDS_FUNCTXT .

  CLEAR GS_FUNTXT.
  GS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  GS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = GS_FUNTXT.

  CLEAR GS_FUNTXT.
  GS_FUNTXT-ICON_ID = ICON_IMPORT.
  GS_FUNTXT-QUICKINFO = '양식 Download'.
  GS_FUNTXT-ICON_TEXT = '양식 Download'.

  SSCRFIELDS-FUNCTXT_02 = GS_FUNTXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_FILE
*&---------------------------------------------------------------------*
FORM F4_FILE  CHANGING  PV_FILE.

  DATA: LT_FILE_TABLE TYPE FILETABLE,
        LS_FIEL_TABLE TYPE FILE_TABLE.

  DATA: LV_SUBRC  TYPE I,
        LV_ACTION TYPE I.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = 'File Path'
      DEFAULT_FILENAME        = ''
      FILE_FILTER             = 'Excel files (*.XLSX)|*.XLSX|Excel files (*.XLS)|*.XLS|'
      INITIAL_DIRECTORY       = 'C:\'
      MULTISELECTION          = ' '
    CHANGING
      FILE_TABLE              = LT_FILE_TABLE
      RC                      = LV_SUBRC
      USER_ACTION             = LV_ACTION
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  CASE LV_ACTION.

    WHEN  CL_GUI_FRONTEND_SERVICES=>ACTION_OK.

      READ TABLE LT_FILE_TABLE INDEX 1 INTO LS_FIEL_TABLE.
      PV_FILE = LS_FIEL_TABLE.

    WHEN CL_GUI_FRONTEND_SERVICES=>ACTION_CANCEL.
      EXIT.
  ENDCASE.

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
      ZCL_CO_COMMON=>FILE_DOWNLOAD(
        EXCEPTIONS
          NOT_EXIST_OBJECT_ID = 1
          NO_DATA_LENGTH      = 2
          FILE_DOWNLOAD_ERROR = 3
          OTHERS              = 4
      ).

    WHEN OTHERS.


  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXCEL_UPLOAD_EXEC
*&---------------------------------------------------------------------*
FORM EXCEL_UPLOAD_EXEC .

  TYPE-POOLS TRUXS.

  FIELD-SYMBOLS: <FS_COL>.

  DATA LV_FILE TYPE STRING.
  DATA LV_RESULT.

  LV_FILE = PA_FILE.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_EXIST
    EXPORTING
      FILE                 = LV_FILE
    RECEIVING
      RESULT               = LV_RESULT
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      WRONG_PARAMETER      = 3
      NOT_SUPPORTED_BY_GUI = 4
      OTHERS               = 5.

  IF  LV_RESULT IS INITIAL.
    " 파일이 존재 하지 않습니다.
    MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M03.
    LEAVE LIST-PROCESSING.
  ENDIF.

  _CLEAR_ITAB : GT_INTERN.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = PA_FILE
      I_BEGIN_COL             = 1         "Excel file start col
      I_BEGIN_ROW             = 2         "Excel file start row
      I_END_COL               = 6
      I_END_ROW               = 10000
    TABLES
      INTERN                  = GT_INTERN
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  DESCRIBE TABLE GT_INTERN LINES SY-TFILL.

  IF SY-TFILL EQ 0.
    " 업로드 데이터가 존재 하지 않습니다.
    MESSAGE S001 WITH TEXT-E16.
    LEAVE LIST-PROCESSING.
  ENDIF.


  CLEAR GS_EXCEL.
  REFRESH GT_EXCEL.

  LOOP AT GT_INTERN.

    ASSIGN COMPONENT GT_INTERN-COL OF STRUCTURE GS_EXCEL TO <FS_COL>.
    <FS_COL> = GT_INTERN-VALUE.

    AT END OF ROW.
      APPEND GS_EXCEL TO GT_EXCEL.
      CLEAR  GS_EXCEL.
    ENDAT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATA_CONVERT
*&---------------------------------------------------------------------*
FORM DATA_CONVERT .

  LOOP AT GT_EXCEL INTO GS_EXCEL.
    MOVE-CORRESPONDING GS_EXCEL TO: GS_OUTTAB,
                                    GT_1280,
                                    GT_1280T,
                                    GT_1290 ,
                                    GT_1290T,
                                    GT_1300 ,
                                    GT_1300T.

    GS_OUTTAB-STATUS  = GC_W.
    GS_OUTTAB-ICON    = ICON_YELLOW_LIGHT.
    GT_1280T-SPRAS    =
    GT_1290T-SPRAS    =
    GT_1300T-SPRAS    = SY-LANGU.

    APPEND: GS_OUTTAB TO GT_OUTTAB,
            GT_1280 ,
            GT_1280T,
            GT_1290 ,
            GT_1290T,
            GT_1300 ,
            GT_1300T.
  ENDLOOP.


  SORT: GT_1280  BY ZZCD1,
        GT_1280T BY ZZCD1 ZZCD1TX,
        GT_1290  BY ZZCD1 ZZCD2,
        GT_1290T BY ZZCD1 ZZCD2 ZZCD2TX,
        GT_1300  BY ZZCD1 ZZCD2 ZZCD3,
        GT_1300T BY ZZCD1 ZZCD2 ZZCD3 ZZCD3TX.

  DELETE ADJACENT DUPLICATES FROM:
        GT_1280   COMPARING ZZCD1,
        GT_1280T  COMPARING ZZCD1 ZZCD1TX,
        GT_1290   COMPARING ZZCD1 ZZCD2,
        GT_1290T  COMPARING ZZCD1 ZZCD2 ZZCD2TX.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_TOP_OF_PAGE_0100
*&---------------------------------------------------------------------*
FORM CREATE_TOP_OF_PAGE_0100.

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
  DATA LV_TEXT    TYPE TEXT255.


  GR_DDOC->ADD_TABLE(
    EXPORTING
      NO_OF_COLUMNS               = 2                " Number of Table Columns
      BORDER                      = '0'              " Width of Table Frame; '0' = No Frame
      WIDTH                       = '100%'           " Width of Table; '100%' = Entire Width of Control
    IMPORTING
      TABLE                       = LR_TABLE         " Table Element
*      TABLEAREA                   =                  " Table Area
    EXCEPTIONS
      TABLE_ALREADY_USED          = 1                " Reference Variable for TABLE Already Used
      OTHERS                      = 2
  ).


  LR_TABLE->ADD_COLUMN(
    EXPORTING
      WIDTH               = '150px'          " Width of Column (Example '20%')
    IMPORTING
      COLUMN              = LR_COLUMN1       " Column Area
    EXCEPTIONS
      COLUMN_ALREADY_USED = 1                " Reference Variable for COLUMN has Already Been Used
      OTHERS              = 2
  ).
  LR_TABLE->ADD_COLUMN(
    EXPORTING
      WIDTH               = '*'              " Width of Column (Example '20%')
    IMPORTING
      COLUMN              = LR_COLUMN2       " Column Area
    EXCEPTIONS
      COLUMN_ALREADY_USED = 1                " Reference Variable for COLUMN has Already Been Used
      OTHERS              = 2
  ).


*-- 업로드 파일 경로 출력
  IF PA_UPLD EQ GC_X.
    LV_TEXT = 'Excel File'.
    LR_COLUMN1->ADD_TEXT( TEXT = LV_TEXT SAP_EMPHASIS = CL_DD_AREA=>STRONG ).

    LV_TEXT = PA_FILE.
    LR_COLUMN2->ADD_TEXT( TEXT = LV_TEXT ).
  ENDIF.





  DATA: LV_SUCCESS TYPE I,
        LV_ERROR   TYPE I.

  GV_TOTAL  = '0'.

  LOOP AT GT_OUTTAB INTO DATA(LS_OUTTAB).
    CASE LS_OUTTAB-STATUS.
      WHEN GC_S. ADD 1 TO LV_SUCCESS.
      WHEN GC_W.
      WHEN GC_E. ADD 1 TO LV_ERROR.
    ENDCASE.
  ENDLOOP.

  IF SY-SUBRC EQ 0.
    GV_TOTAL = SY-TFILL.
  ENDIF.

  WRITE LV_SUCCESS TO GV_SUCESS NO-GAP.
  WRITE LV_ERROR   TO GV_ERROR  NO-GAP.



*-- 현재 총 라인 수 출력
  LR_TABLE->NEW_ROW( ).
  LV_TEXT = 'Total Count'(T03).
  LR_COLUMN1->ADD_ICON( 'ICON_SUM' ).
  LR_COLUMN1->ADD_TEXT( TEXT = LV_TEXT SAP_EMPHASIS = CL_DD_AREA=>STRONG ).
  LR_COLUMN2->ADD_TEXT( TEXT = CONV #( CONDENSE( GV_TOTAL ) ) ).


*-- 저장성공 수 출력 ( 업로드 모드만 출력 )
  IF PA_UPLD EQ GC_X.
    LR_TABLE->NEW_ROW( ).
    LR_COLUMN1->ADD_ICON( 'ICON_LED_GREEN' ).
    LR_COLUMN1->ADD_TEXT( TEXT = 'Success Count'(T04) SAP_EMPHASIS = CL_DD_AREA=>STRONG ).
    LR_COLUMN2->ADD_TEXT( TEXT = CONV #( CONDENSE( GV_SUCESS ) ) ).
  ENDIF.


*-- 현재 오류 or 저장실패 수 출력
  LR_TABLE->NEW_ROW( ).
  LR_COLUMN1->ADD_ICON( 'ICON_LED_RED' ).
  LR_COLUMN1->ADD_TEXT( TEXT = 'Error Count'(T05) SAP_EMPHASIS = CL_DD_AREA=>STRONG ).
  LR_COLUMN2->ADD_TEXT( TEXT = CONV #( CONDENSE( GV_ERROR ) ) ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_MAIN_GRID_0100
*&---------------------------------------------------------------------*
FORM CREATE_MAIN_GRID_0100 .

  IF GR_ALV IS NOT BOUND.
    GR_ALV = NEW #( I_CONTAINER = GR_CON_MAIN2 ).
  ENDIF.

  GR_ALV->SET_LAYOUT( I_TYPE = 'B' ).
  GR_ALV->SET_FIELD_CATALOG(
    EXPORTING
      I_TABNAME               = 'GS_OUTTAB'
    EXCEPTIONS
      INVALID_INPUT_PARAMETER = 1
      EMPTY_FIELD_CATALOG     = 2
      OTHERS                  = 3
  ).
  IF SY-SUBRC <> 0.
    MESSAGE '필드카탈로그가 비어있습니다.' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 0.
  ENDIF.

  GR_ALV->SET_SORT( IT_FIELD = VALUE #( ( 'STATUS' )
                                        ( 'ICON' )
                                        ( 'ZZCD1' )
                                        ( 'ZZCD1TX' )
                                        ( 'ZZCD2' )
                                        ( 'ZZCD2TX' )
                                        ( 'ZZCD3' )
                                        ( 'ZZCD3TX' ) ) ).

  GR_ALV->SET_EXCLUDE_TOOLBAR(
    IT_TOOLBAR = VALUE #( ( '&SUBTOT'     )
                          ( '&SUMC'       )
                          ( '&MB_SUBTOT'  )
                          ( '&MB_SUM'     ) ) ).


  PERFORM MAKE_FIELDCATALOG_0100.
  PERFORM SET_ALV_EVENT_0100.

  GR_ALV->MS_VARIANT-REPORT = SY-REPID.
  GR_ALV->MV_SAVE = 'A'.
  GR_ALV->DISPLAY( CHANGING T_OUTTAB = GT_OUTTAB ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_FIELDCATALOG_0100
*&---------------------------------------------------------------------*
FORM MAKE_FIELDCATALOG_0100 .

  DATA LV_TEXT TYPE TEXT100.

  CLEAR GS_OUTTAB.

  LOOP AT GR_ALV->MT_FIELDCAT INTO DATA(LS_FIELDCAT).

    CLEAR LV_TEXT.
    CLEAR LS_FIELDCAT-KEY.

    LS_FIELDCAT-COL_OPT = GC_X.

    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'STATUS'.
        LS_FIELDCAT-TECH = GC_X.

      WHEN 'ICON'.
        LV_TEXT = 'Status'(C01).
        LS_FIELDCAT-ICON = GC_X.
        LS_FIELDCAT-KEY  = GC_X.
        LS_FIELDCAT-NO_MERGING = GC_X.

      WHEN 'ZZCD1'.
        LV_TEXT = '대분류'(C02).

      WHEN 'ZZCD1TX'.
        LV_TEXT = '대분류명'(C03).
        LS_FIELDCAT-EMPHASIZE = 'C500'.

      WHEN 'ZZCD2'.
        LV_TEXT = '중분류'(C04).

      WHEN 'ZZCD2TX'.
        LV_TEXT = '중분류명'(C05).
        LS_FIELDCAT-EMPHASIZE = 'C500'.

      WHEN 'ZZCD3'.
        LV_TEXT = '소분류'(C06).

      WHEN 'ZZCD3TX'.
        LV_TEXT = '소분류명'(C07).
        LS_FIELDCAT-EMPHASIZE = 'C500'.

      WHEN 'MESSAGE'.
        IF PA_UPLD EQ GC_X.
          LV_TEXT = '처리결과'(C08).
          LS_FIELDCAT-COL_OPT   = ' '.
          LS_FIELDCAT-OUTPUTLEN = 40.
        ELSE.
          LS_FIELDCAT-TECH = GC_X.
        ENDIF.

      WHEN 'CELLTYPE'.
        LS_FIELDCAT-TECH = GC_X.

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
*& Form SAVE_DATA
*&---------------------------------------------------------------------*
FORM SAVE_DATA .


  READ TABLE GT_OUTTAB TRANSPORTING NO FIELDS WITH KEY STATUS = GC_E.
  IF SY-SUBRC EQ 0.
    " 선택하신 파일에 오류가 존재합니다 확인하십시오.
    MESSAGE S000 DISPLAY LIKE GC_E WITH TEXT-E05.
    EXIT.
  ENDIF.


  IF PA_OVER EQ GC_X.
    CHECK GC_X EQ ZCL_CO_COMMON=>POPUP_CONFIRM(
        I_TITLEBAR = '확인'(PT1)
        I_QUESTION = CONV #( '[주의] 현재 등록된 분류코드를 전부 삭제한 후   첨부파일 내용으로 업로드 하시겠습니까?'(QT2) )
        I_POPUP_TYPE = 'ICON_MESSAGE_ERROR'
    ).
  ELSE.
    CHECK GC_X EQ ZCL_CO_COMMON=>POPUP_CONFIRM(
        I_TITLEBAR = '확인'(PT1)
        I_QUESTION = CONV #( '설비WBS 속성을 업로드 하시겠습니까?'(QT1) )
    ).
  ENDIF.



  DATA(LT_OUTTAB) = GT_OUTTAB.

  SORT LT_OUTTAB BY ZZCD1
                    ZZCD2
                    ZZCD3.

  IF PA_OVER EQ GC_X.
    DELETE FROM: ZCOT1280,
                 ZCOT1280T,
                 ZCOT1290,
                 ZCOT1290T,
                 ZCOT1300,
                 ZCOT1300T.
  ENDIF.

  DATA LV_SUBRC TYPE SY-SUBRC.

  INSERT ZCOT1280 FROM TABLE @GT_1280 ACCEPTING DUPLICATE KEYS. ADD SY-SUBRC TO LV_SUBRC.
  INSERT ZCOT1290 FROM TABLE @GT_1290 ACCEPTING DUPLICATE KEYS. ADD SY-SUBRC TO LV_SUBRC.
  INSERT ZCOT1300 FROM TABLE @GT_1300 ACCEPTING DUPLICATE KEYS. ADD SY-SUBRC TO LV_SUBRC.

  MODIFY ZCOT1280T FROM TABLE @GT_1280T.  ADD SY-SUBRC TO LV_SUBRC.
  MODIFY ZCOT1290T FROM TABLE @GT_1290T.  ADD SY-SUBRC TO LV_SUBRC.
  MODIFY ZCOT1300T FROM TABLE @GT_1300T.  ADD SY-SUBRC TO LV_SUBRC.


  IF LV_SUBRC EQ 0.
    COMMIT WORK.
    MESSAGE '저장이 완료되었습니다.' TYPE GC_S.

    GS_OUTTAB-STATUS  = GC_S.
    GS_OUTTAB-ICON    = ICON_GREEN_LIGHT.
    GS_OUTTAB-MESSAGE = '저장완료'(M01).
  ELSE.
    ROLLBACK WORK.
    MESSAGE '저장이 실패되었습니다.' TYPE GC_S DISPLAY LIKE GC_E.

    GS_OUTTAB-STATUS  = GC_E.
    GS_OUTTAB-ICON    = ICON_RED_LIGHT.
    GS_OUTTAB-MESSAGE = '저장실패'(M02).
  ENDIF.


  LOOP AT GT_OUTTAB TRANSPORTING NO FIELDS
                    WHERE ICON EQ ICON_YELLOW_LIGHT.
    MODIFY GT_OUTTAB FROM GS_OUTTAB TRANSPORTING STATUS
                                                 ICON
                                                 MESSAGE.
  ENDLOOP.

  PERFORM CREATE_TOP_OF_PAGE_0100.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_SSCR
*&---------------------------------------------------------------------*
FORM MODIFY_SSCR .

  IF PA_DISP EQ GC_X.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'M01'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  PERFORM CLEAR_ITAB.


  CASE GC_X.
    WHEN PA_UPLD.
      PERFORM EXCEL_UPLOAD_EXEC.
      PERFORM DATA_CONVERT.
    WHEN PA_DISP.
      PERFORM SELECT_DB.
      PERFORM MAKE_OUTTAB.
  ENDCASE.

  PERFORM CHECK_VALID_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_DB
*&---------------------------------------------------------------------*
FORM SELECT_DB .

  SELECT ZZCD1
    FROM ZCOT1280
    INTO CORRESPONDING FIELDS OF TABLE @GT_1280.

  SELECT @SY-LANGU AS SPRAS,
         ZZCD1,
         ZZCD1TX
    FROM ZCOT1280T
   WHERE SPRAS EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @GT_1280T.

  SELECT ZZCD1,
         ZZCD2
    FROM ZCOT1290
    INTO CORRESPONDING FIELDS OF TABLE @GT_1290.

  SELECT @SY-LANGU AS SPRAS,
         ZZCD1,
         ZZCD2,
         ZZCD2TX
    FROM ZCOT1290T
   WHERE SPRAS EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @GT_1290T.

  SELECT ZZCD1, ZZCD2, ZZCD3
    FROM ZCOT1300
    INTO CORRESPONDING FIELDS OF TABLE @GT_1300.

  SELECT @SY-LANGU AS SPRAS,
         ZZCD1,
         ZZCD2,
         ZZCD3,
         ZZCD3TX
    FROM ZCOT1300T
   WHERE SPRAS EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @GT_1300T.

  SORT GT_1280  BY ZZCD1.
  SORT GT_1280T BY ZZCD1.
  SORT GT_1290  BY ZZCD1 ZZCD2.
  SORT GT_1290T BY ZZCD1 ZZCD2.
  SORT GT_1300  BY ZZCD1 ZZCD2 ZZCD3.
  SORT GT_1300T BY ZZCD1 ZZCD2 ZZCD3.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_VALID_DATA
*&---------------------------------------------------------------------*
FORM CHECK_VALID_DATA .

  DATA LV_ICON_RED TYPE ICON-ID.

  CASE ABAP_ON.
    WHEN PA_UPLD.
      LV_ICON_RED = ICON_RED_LIGHT.
    WHEN PA_DISP.
      LV_ICON_RED = ICON_LED_RED.
  ENDCASE.



  LOOP AT GT_1290C.
    GS_OUTTAB = VALUE #(
      STATUS  = GC_E
      ICON    = LV_ICON_RED
      ZZCD1   = GT_1290C-ZZCD1
      ZZCD2   = GT_1290C-ZZCD2
    ).

    IF GS_OUTTAB-ZZCD1 IS INITIAL.
      GS_OUTTAB-MESSAGE = '대분류 코드가 공란입니다.'(E01).
    ELSE.
      GS_OUTTAB-MESSAGE = '대분류 코드가 존재하지 않습니다.'(E02).
    ENDIF.

    APPEND GS_OUTTAB TO GT_OUTTAB.
  ENDLOOP.


  LOOP AT GT_1300C.
    GS_OUTTAB = VALUE #(
      STATUS  = GC_E
      ICON    = LV_ICON_RED
      ZZCD1   = GT_1300C-ZZCD1
      ZZCD2   = GT_1300C-ZZCD2
      ZZCD3   = GT_1300C-ZZCD3
    ).

    IF GS_OUTTAB-ZZCD1 IS INITIAL.
      GS_OUTTAB-MESSAGE = '대분류 코드가 공란입니다.'(E01).
    ELSE.
      READ TABLE GT_1280C TRANSPORTING NO FIELDS
                         WITH KEY ZZCD1 = GT_1300C-ZZCD1 BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        IF GS_OUTTAB-ZZCD2 IS INITIAL.
          GS_OUTTAB-MESSAGE = '중분류 코드가 공란입니다.'(E03).
        ELSE.
          GS_OUTTAB-MESSAGE = '중분류 코드가 존재하지 않습니다.'(E04).
        ENDIF.
      ELSE.
        GS_OUTTAB-MESSAGE = '대분류 코드가 존재하지 않습니다.'(E02).
      ENDIF.
    ENDIF.

    APPEND GS_OUTTAB TO GT_OUTTAB.
  ENDLOOP.



  SORT GT_OUTTAB BY ZZCD1 ZZCD2 ZZCD3.

  DATA LV_TABIX  TYPE SY-TABIX.
  DATA LS_OUTTAB LIKE GS_OUTTAB.

  LOOP AT GT_OUTTAB INTO GS_OUTTAB.
    LV_TABIX = SY-TABIX.
    PERFORM SET_CODE_TEXT.

    IF GS_OUTTAB-ZZCD1 IS INITIAL.
      GS_OUTTAB-STATUS  = GC_E.
      GS_OUTTAB-ICON    = LV_ICON_RED.
      GS_OUTTAB-MESSAGE = '대분류 코드가 공란입니다.'(E01).

    ELSEIF GS_OUTTAB-ZZCD2 IS INITIAL.

      GS_OUTTAB-STATUS  = GC_E.
      GS_OUTTAB-ICON    = LV_ICON_RED.
      GS_OUTTAB-MESSAGE = '중분류 코드가 공란입니다.'(E03).

    ELSEIF GS_OUTTAB-ZZCD3 IS INITIAL.

      GS_OUTTAB-STATUS  = GC_E.
      GS_OUTTAB-ICON    = LV_ICON_RED.
      GS_OUTTAB-MESSAGE = '소분류 코드가 공란입니다.'(E06).

    ELSEIF LV_TABIX > 1
       AND GS_OUTTAB-ZZCD1 EQ LS_OUTTAB-ZZCD1
       AND GS_OUTTAB-ZZCD2 EQ LS_OUTTAB-ZZCD2
       AND GS_OUTTAB-ZZCD3 EQ LS_OUTTAB-ZZCD3.

      GS_OUTTAB-STATUS  = GC_E.
      GS_OUTTAB-ICON    = LV_ICON_RED.
      GS_OUTTAB-MESSAGE = '중복된 코드가 존재합니다.'(E07).

    ENDIF.

    MODIFY GT_OUTTAB FROM GS_OUTTAB.
    LS_OUTTAB = GS_OUTTAB.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_CODE_TEXT
*&---------------------------------------------------------------------*
FORM SET_CODE_TEXT .

  READ TABLE GT_1280T WITH KEY ZZCD1 = GS_OUTTAB-ZZCD1 BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_OUTTAB-ZZCD1TX = GT_1280T-ZZCD1TX.
  ENDIF.


  READ TABLE GT_1290T WITH KEY ZZCD1 = GS_OUTTAB-ZZCD1
                               ZZCD2 = GS_OUTTAB-ZZCD2 BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_OUTTAB-ZZCD2TX = GT_1290T-ZZCD2TX.
  ENDIF.


  READ TABLE GT_1300T WITH KEY ZZCD1 = GS_OUTTAB-ZZCD1
                               ZZCD2 = GS_OUTTAB-ZZCD2
                               ZZCD3 = GS_OUTTAB-ZZCD3 BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_OUTTAB-ZZCD3TX = GT_1300T-ZZCD3TX.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR_ITAB
*&---------------------------------------------------------------------*
FORM CLEAR_ITAB .

  _CLEAR_ITAB : GT_INTERN,
                GT_RETURN,
                GT_OUTTAB,
                GT_EXCEL,

                GT_1280,
                GT_1280T,
                GT_1280C,
                GT_1290,
                GT_1290T,
                GT_1290C,
                GT_1300,
                GT_1300T,
                GT_1300C.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_OUTTAB
*&---------------------------------------------------------------------*
FORM MAKE_OUTTAB .


  GT_1280C[] = GT_1280[].
  GT_1290C[] = GT_1290[].
  GT_1300C[] = GT_1300[].


  LOOP AT GT_1280C.

    CLEAR GS_OUTTAB.

    GS_OUTTAB-STATUS  = GC_S.
    GS_OUTTAB-ICON    = ICON_LED_GREEN.
    GS_OUTTAB-ZZCD1   = GT_1280C-ZZCD1.


    READ TABLE GT_1290C TRANSPORTING NO FIELDS
                       WITH KEY ZZCD1 = GT_1280C-ZZCD1
                                BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LOOP AT GT_1290C FROM SY-TABIX.
        IF GT_1290C-ZZCD1 NE GT_1280C-ZZCD1.
          EXIT.
        ENDIF.

        GS_OUTTAB-ZZCD2 = GT_1290C-ZZCD2.

        READ TABLE GT_1300C TRANSPORTING NO FIELDS
                           WITH KEY ZZCD1 = GT_1290C-ZZCD1
                                    ZZCD2 = GT_1290C-ZZCD2
                                    BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          LOOP AT GT_1300C FROM SY-TABIX.
            IF NOT ( GT_1300C-ZZCD1 EQ GT_1290C-ZZCD1 AND
                     GT_1300C-ZZCD2 EQ GT_1290C-ZZCD2 ).
              EXIT.
            ENDIF.
            GS_OUTTAB-ZZCD3 = GT_1300C-ZZCD3.
            APPEND GS_OUTTAB TO GT_OUTTAB.
            DELETE GT_1300C.
          ENDLOOP.
        ELSE.
          GS_OUTTAB-STATUS  = GC_W.
          GS_OUTTAB-ICON    = ICON_LED_YELLOW.
          APPEND GS_OUTTAB TO GT_OUTTAB.
        ENDIF.

        DELETE GT_1290C.
      ENDLOOP.
    ELSE.
      GS_OUTTAB-STATUS  = GC_W.
      GS_OUTTAB-ICON    = ICON_LED_YELLOW.
      APPEND GS_OUTTAB TO GT_OUTTAB.
    ENDIF.

  ENDLOOP.


  LOOP AT GT_OUTTAB INTO GS_OUTTAB.
    PERFORM SET_CODE_TEXT.
    MODIFY GT_OUTTAB FROM GS_OUTTAB.
  ENDLOOP.



*  LOOP AT GT_1300C INTO DATA(LS_1300).
*
*    AT NEW ZZCD1.
*      READ TABLE GT_1280C INTO DATA(LS_1280)
*                         WITH KEY ZZCD1 = LS_1300-ZZCD1.
*      IF SY-SUBRC EQ 0.
*        DELETE GT_1280C FROM 1 TO SY-TABIX.
*      ELSE.
*        CLEAR LS_1280.
*      ENDIF.
*    ENDAT.
*
*    AT NEW ZZCD2.
*      READ TABLE GT_1290C INTO DATA(LS_1290)
*                         WITH KEY ZZCD1 = LS_1300-ZZCD1
*                                  ZZCD2 = LS_1300-ZZCD2.
*      IF SY-SUBRC EQ 0.
*        DELETE GT_1290C FROM 1 TO SY-TABIX.
*      ELSE.
*        CLEAR LS_1290.
*      ENDIF.
*    ENDAT.
*
*    GS_OUTTAB = VALUE #(
*      ZZCD1   = LS_1300-ZZCD1
*      ZZCD1TX = LS_1280-ZZCD1TX
*      ZZCD2   = LS_1300-ZZCD2
*      ZZCD2TX = LS_1290-ZZCD2TX
*      ZZCD3   = LS_1300-ZZCD3
*      ZZCD3TX = LS_1300-ZZCD3TX
*    ).
*
*    APPEND GS_OUTTAB TO GT_OUTTAB.
*
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_ALV_EVENT_0100
*&---------------------------------------------------------------------*
FORM SET_ALV_EVENT_0100 .

  IF GR_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_EVENT_RECEIVER.
  ENDIF.

  SET HANDLER:
    GR_EVENT_RECEIVER->ON_TOOLBAR       FOR GR_ALV->MR_ALV_GRID,
    GR_EVENT_RECEIVER->ON_USER_COMMAND  FOR GR_ALV->MR_ALV_GRID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
FORM HANDLE_TOOLBAR
  USING PR_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
        SENDER    TYPE REF TO CL_GUI_ALV_GRID.

  DATA LS_TOOLBAR LIKE LINE OF PR_OBJECT->MT_TOOLBAR.

  CASE SENDER.
    WHEN GR_ALV->MR_ALV_GRID.

      " 조회전용일 때 트리전환 버튼 추가
      CHECK PA_DISP EQ GC_X.
      LS_TOOLBAR-BUTN_TYPE = 3.
      APPEND LS_TOOLBAR TO PR_OBJECT->MT_TOOLBAR.
      CLEAR  LS_TOOLBAR.

      LS_TOOLBAR-BUTN_TYPE = 0.
      LS_TOOLBAR-FUNCTION  = 'TOGGLE_TREE'.
      LS_TOOLBAR-ICON      = ICON_VIEW_HIER_LIST. "ICON_TREE.
      LS_TOOLBAR-TEXT      = 'Tree 전환'.
      APPEND LS_TOOLBAR TO PR_OBJECT->MT_TOOLBAR.
      CLEAR  LS_TOOLBAR.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND
  USING PV_UCOMM  TYPE SY-UCOMM
        SENDER    TYPE REF TO CL_GUI_ALV_GRID.

  CASE SENDER.
    WHEN GR_ALV->MR_ALV_GRID.
      CASE PV_UCOMM.
        WHEN 'TOGGLE_TREE'.
          GV_TREE = GC_X.
          GR_ALV->MR_ALV_GRID->FREE( ). FREE GR_ALV.
          PERFORM CREATE_SALV_TREE_0100 .
        WHEN OTHERS.
      ENDCASE.
    WHEN OTHERS.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_SALV_TREE_0100
*&---------------------------------------------------------------------*
FORM CREATE_SALV_TREE_0100 .


  TRY.
    FREE    GR_TREE.
    REFRESH GT_OUTTAB_TREE.

    CL_SALV_TREE=>FACTORY(
      EXPORTING
        R_CONTAINER = GR_CON_MAIN2
*        HIDE_HEADER =                  " Do Not Show Header
      IMPORTING
        R_SALV_TREE = GR_TREE
      CHANGING
        T_TABLE     = GT_OUTTAB_TREE
    ).

    PERFORM BUILD_NODE.
    PERFORM SET_TREE_TOOLBAR.
    PERFORM SET_TREE_HIERARCHY.
    PERFORM SET_TREE_COLUMNS.
    PERFORM SET_TREE_EVENT.

    GR_TREE->DISPLAY( ).

  CATCH CX_SALV_ERROR INTO DATA(LX_ERROR).
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_NODE
*&---------------------------------------------------------------------*
FORM BUILD_NODE .

  DATA LS_OUTTAB LIKE GS_OUTTAB.

  DATA(LR_NODES) = GR_TREE->GET_NODES( ).

  DATA LV_ZZCD1 LIKE GS_OUTTAB-ZZCD1.
  DATA LV_ZZCD2 LIKE GS_OUTTAB-ZZCD2.
  DATA LV_ZZCD3 LIKE GS_OUTTAB-ZZCD3.

  LOOP AT GT_OUTTAB INTO GS_OUTTAB.

    IF LV_ZZCD1 NE GS_OUTTAB-ZZCD1.
      LV_ZZCD1 = GS_OUTTAB-ZZCD1.
      LV_ZZCD2 = SPACE.

      LS_OUTTAB = VALUE #(

      ).

      DATA(LR_NODE_1) = LR_NODES->ADD_NODE(
        EXPORTING
          RELATED_NODE   = ' '
          RELATIONSHIP   = CL_SALV_NODE=>IF_SALV_C_NODE_RELATION~PARENT
          DATA_ROW       = LS_OUTTAB         " Data Row
*          COLLAPSED_ICON =                  " Image for Tree Hierarchy
*          EXPANDED_ICON  =                  " Image for Tree Hierarchy
*          ROW_STYLE      =                  " ALV: Data Element for Constants
          TEXT           = |{ GS_OUTTAB-ZZCD1 } { GS_OUTTAB-ZZCD1TX }|                 " ALV Control: Cell Content
*          VISIBLE        = ABAP_TRUE        " Boolean Variable (X=True, Space=False)
*          EXPANDER       =                  " Boolean Variable (X=True, Space=False)
*          ENABLED        = ABAP_TRUE        " Boolean Variable (X=True, Space=False)
*          FOLDER         =                  " Boolean Variable (X=True, Space=False)
      ).
    ENDIF.

    IF LV_ZZCD2 NE GS_OUTTAB-ZZCD2.
      LV_ZZCD2 = GS_OUTTAB-ZZCD2.

      LS_OUTTAB = VALUE #(
        ZZCD1   = GS_OUTTAB-ZZCD1
        ZZCD1TX = GS_OUTTAB-ZZCD1TX
      ).

      DATA(LR_NODE_2) = LR_NODES->ADD_NODE(
        EXPORTING
          RELATED_NODE   = LR_NODE_1->GET_KEY( )
          RELATIONSHIP   = CL_SALV_NODE=>IF_SALV_C_NODE_RELATION~LAST_CHILD
          DATA_ROW       = LS_OUTTAB         " Data Row
*          COLLAPSED_ICON =                  " Image for Tree Hierarchy
*          EXPANDED_ICON  =                  " Image for Tree Hierarchy
*          ROW_STYLE      =                  " ALV: Data Element for Constants
          TEXT           = |{ GS_OUTTAB-ZZCD2 } { GS_OUTTAB-ZZCD2TX }|                 " ALV Control: Cell Content
*          VISIBLE        = ABAP_TRUE        " Boolean Variable (X=True, Space=False)
*          EXPANDER       =                  " Boolean Variable (X=True, Space=False)
*          ENABLED        = ABAP_TRUE        " Boolean Variable (X=True, Space=False)
*          FOLDER         =                  " Boolean Variable (X=True, Space=False)
      ).
    ENDIF.


    LS_OUTTAB = VALUE #(
      ZZCD1   = GS_OUTTAB-ZZCD1
      ZZCD1TX = GS_OUTTAB-ZZCD1TX
      ZZCD2   = GS_OUTTAB-ZZCD2
      ZZCD2TX = GS_OUTTAB-ZZCD2TX
    ).


    LR_NODES->ADD_NODE(
      EXPORTING
        RELATED_NODE   = LR_NODE_2->GET_KEY( )
        RELATIONSHIP   = CL_SALV_NODE=>IF_SALV_C_NODE_RELATION~LAST_CHILD
        DATA_ROW       = LS_OUTTAB         " Data Row
        COLLAPSED_ICON = CONV #( GS_OUTTAB-ICON )  " Image for Tree Hierarchy
        EXPANDED_ICON  = CONV #( GS_OUTTAB-ICON )  " Image for Tree Hierarchy
*        ROW_STYLE      =                  " ALV: Data Element for Constants
        TEXT           = |{ GS_OUTTAB-ZZCD3 } { GS_OUTTAB-ZZCD3TX }|                 " ALV Control: Cell Content
*        VISIBLE        = ABAP_TRUE        " Boolean Variable (X=True, Space=False)
*        EXPANDER       =                  " Boolean Variable (X=True, Space=False)
*        ENABLED        = ABAP_TRUE        " Boolean Variable (X=True, Space=False)
*        FOLDER         =                  " Boolean Variable (X=True, Space=False)
*      RECEIVING
*        NODE           = DATA(LV_NKEY_3)   " Node Key
    ).
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TREE_TOOLBAR
*&---------------------------------------------------------------------*
FORM SET_TREE_TOOLBAR .
    DATA(LR_FUNCTIONS) = GR_TREE->GET_FUNCTIONS( ).
    LR_FUNCTIONS->SET_ALL( ).
    LR_FUNCTIONS->SET_GROUP_AGGREGATION( SPACE ).


    " CL_SALV_FUNCTIONS=>IF_SALV_C_FUNCTION_POSITION~RIGHT_OF_SALV_FUNCTIONS
    CONSTANTS C_POSITION_RIGHT TYPE SALV_DE_FUNCTION_POS VALUE 2.


    LR_FUNCTIONS->ADD_FUNCTION(
      EXPORTING
        NAME     = 'TOGGLE_ALV'               " ALV Function
        ICON     = CONV #( ICON_VIEW_TABLE )
        TEXT     = 'ALV 전환'
        TOOLTIP  = 'ALV 전환'
        POSITION = C_POSITION_RIGHT  " Positioning Function
    ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TREE_HIERARCHY
*&---------------------------------------------------------------------*
FORM SET_TREE_HIERARCHY .

  DATA(LR_HIERARCHY) = GR_TREE->GET_TREE_SETTINGS( ).

  LR_HIERARCHY->SET_HIERARCHY_SIZE( 40 ).
  LR_HIERARCHY->SET_HIERARCHY_HEADER( '설비분류코드 구조' ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TREE_COLUMNS
*&---------------------------------------------------------------------*
FORM SET_TREE_COLUMNS .

  DATA(LR_COLUMNS) = GR_TREE->GET_COLUMNS( ).
  DATA(LT_COLUMNS) = LR_COLUMNS->GET( ).


  DATA LV_TEXT TYPE STRING.

  LOOP AT LT_COLUMNS ASSIGNING FIELD-SYMBOL(<FS_COLUMN>).

    CLEAR LV_TEXT.

    CASE <FS_COLUMN>-COLUMNNAME.
      WHEN 'STATUS'.
        <FS_COLUMN>-R_COLUMN->SET_TECHNICAL( ).
      WHEN 'ICON'.
        <FS_COLUMN>-R_COLUMN->SET_TECHNICAL( ).
      WHEN 'ZZCD1'.
        <FS_COLUMN>-R_COLUMN->SET_OUTPUT_LENGTH( 13 ).
        LV_TEXT = '대분류코드'.
      WHEN 'ZZCD1TX'.
        <FS_COLUMN>-R_COLUMN->SET_OUTPUT_LENGTH( 20 ).
        LV_TEXT = '대분류코드명'.
      WHEN 'ZZCD2'.
        <FS_COLUMN>-R_COLUMN->SET_OUTPUT_LENGTH( 13 ).
        LV_TEXT = '중분류코드'.
      WHEN 'ZZCD2TX'.
        <FS_COLUMN>-R_COLUMN->SET_OUTPUT_LENGTH( 20 ).
        LV_TEXT = '중분류코드명'.
      WHEN 'ZZCD3'.
        <FS_COLUMN>-R_COLUMN->SET_TECHNICAL( ).
      WHEN 'ZZCD3TX'.
        <FS_COLUMN>-R_COLUMN->SET_TECHNICAL( ).
      WHEN OTHERS.
        <FS_COLUMN>-R_COLUMN->SET_OPTIMIZED( ).
    ENDCASE.

    IF LV_TEXT IS NOT INITIAL.
      <FS_COLUMN>-R_COLUMN->SET_SHORT_TEXT( CONV #( LV_TEXT ) ).
      <FS_COLUMN>-R_COLUMN->SET_MEDIUM_TEXT( CONV #( LV_TEXT ) ).
      <FS_COLUMN>-R_COLUMN->SET_LONG_TEXT( CONV #( LV_TEXT ) ).
      <FS_COLUMN>-R_COLUMN->SET_TOOLTIP( CONV #( LV_TEXT ) ).
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_ADDED_FUNCTION
*&---------------------------------------------------------------------*
FORM HANDLE_ADDED_FUNCTION  USING PV_FUNCTION TYPE SALV_DE_FUNCTION
                                  PR_SENDER.

  CASE PV_FUNCTION.
    WHEN 'TOGGLE_ALV'.
      CLEAR GV_TREE.
      PERFORM CREATE_CONTAINER_MAIN.
      PERFORM CREATE_MAIN_GRID_0100.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TREE_EVENT
*&---------------------------------------------------------------------*
FORM SET_TREE_EVENT .

  DATA(LR_EVENT) = GR_TREE->GET_EVENT( ).

  IF GR_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_EVENT_RECEIVER.
  ENDIF.

  SET HANDLER GR_EVENT_RECEIVER->ON_ADDED_FUNCTION FOR LR_EVENT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_CONTAINER_MAIN
*&---------------------------------------------------------------------*
FORM CREATE_CONTAINER_MAIN .

  IF GR_CON_SPLIT2 IS NOT INITIAL.
    GR_CON_SPLIT2->FREE( ).
    FREE GR_CON_SPLIT2.
  ENDIF.


  CREATE OBJECT GR_CON_SPLIT2
    EXPORTING
      PARENT                  = GR_CON_MAIN                   " Parent Container
      ROWS                    = 1                   " Number of Rows to be displayed
      COLUMNS                 = 1                   " Number of Columns to be Displayed
    EXCEPTIONS
      CNTL_ERROR              = 1                  " See Superclass
      CNTL_SYSTEM_ERROR       = 2                  " See Superclass
      OTHERS                  = 3.


  CALL METHOD GR_CON_SPLIT2->SET_BORDER
    EXPORTING
      BORDER            = SPACE            " Draw Frame (gfw_true); Do Not Draw Frame (gfw_false)
    EXCEPTIONS
      CNTL_ERROR        = 1                " See CL_GUI_CONTROL
      CNTL_SYSTEM_ERROR = 2                " See CL_GUI_CONTROL
      OTHERS            = 3.

  CALL METHOD GR_CON_SPLIT2->GET_CONTAINER
    EXPORTING
      ROW       = 1                " Row
      COLUMN    = 1                " Column
    RECEIVING
      CONTAINER = GR_CON_MAIN2.    " Container

ENDFORM.

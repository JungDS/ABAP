*&---------------------------------------------------------------------*
*& Include          ZCOR101_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INIT_VALUE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM INIT_VALUE .
* 관리회계 영역
  P_VKOKR = '1000'.

* 시작년도
  P_GJAHR = SY-DATUM(4).

* 생성일자
  DATA: L_GJAHR TYPE GJAHR.
  L_GJAHR = SY-DATUM(4) - 1.

  S_ERDAT(3)   = 'IBT'.
*  S_ERDAT-LOW  = '20190101'.
  CONCATENATE L_GJAHR '0101' INTO S_ERDAT-LOW.
  S_ERDAT-HIGH = SY-DATUM.
  APPEND S_ERDAT.

* 조회화면 Function Key
  G_FUNCTION_KEY-ICON_ID   = ICON_EXPORT.
  G_FUNCTION_KEY-ICON_TEXT = '양식 download'.
  SSCRFIELDS-FUNCTXT_01    = G_FUNCTION_KEY.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM MODIFY_SCREEN .
  LOOP AT SCREEN.
    CASE 'X'.
      WHEN RB_CRT.  "생성
        CASE SCREEN-GROUP1.
          WHEN 'A'.
            SCREEN-ACTIVE = 1.
          WHEN 'B'.
            SCREEN-ACTIVE = 0.
        ENDCASE.

      WHEN RB_DISP.  "조회/변경
        CASE SCREEN-GROUP1.
          WHEN 'A'.
            SCREEN-ACTIVE = 0.
          WHEN 'B'.
            SCREEN-ACTIVE = 1.
        ENDCASE.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_FORM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DOWNLOAD_FORM .
  DATA: LV_OBJECTNAME(40) VALUE 'ZCOR101'.

  DATA: LV_FILENM LIKE RLGRAP-FILENAME.
  DATA: LV_UACT  TYPE I,
        LV_NAME  TYPE STRING,
        LV_PATH  TYPE STRING,
        LV_FNAME TYPE STRING.

  SELECT SINGLE F~RELID
                F~OBJID
                F~CHECKOUT
                F~CHECKNEW
                F~CHNAME
                F~TDATE
                F~TTIME
                F~TEXT
                P~DEVCLASS
    INTO CORRESPONDING FIELDS OF GS_KEY
    FROM WWWDATA  AS F
    JOIN TADIR    AS P
      ON F~OBJID    = P~OBJ_NAME
   WHERE F~SRTF2    = 0
     AND F~RELID    = 'MI'
     AND P~PGMID    = 'R3TR'
     AND P~OBJECT   = 'W3MI'
     AND P~OBJ_NAME = LV_OBJECTNAME.

  LV_NAME = LV_OBJECTNAME.

* 사용자 지정 경로 팝업
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
      DEFAULT_EXTENSION = 'xls'
      DEFAULT_FILE_NAME = LV_NAME
    CHANGING
      FILENAME          = LV_NAME
      PATH              = LV_PATH
      FULLPATH          = LV_FNAME
      USER_ACTION       = LV_UACT.

  IF LV_UACT = CL_GUI_FRONTEND_SERVICES=>ACTION_CANCEL.
    EXIT.
  ENDIF.

* 사용자가 원하는 경로명.
  LV_FILENM = LV_NAME.

  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      KEY         = GS_KEY
      DESTINATION = LV_FILENM.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_FILENM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- P_FILENM
*&---------------------------------------------------------------------*
FORM GET_FILENM  CHANGING P_FILENM.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      DEF_PATH         = ' '
      MASK             = ',*.xls,*.xls,*.xlsx,*.xlsx.'
      MODE             = 'O'
    IMPORTING
      FILENAME         = P_FILENM
    EXCEPTIONS
      INV_WINSYS       = 1
      NO_BATCH         = 2
      SELECTION_CANCEL = 3
      SELECTION_ERROR  = 4
      OTHERS           = 5.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_EXCEL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_EXCEL_DATA .
  DATA: LO_CX  TYPE REF TO CX_ROOT,
        LV_MSG TYPE STRING.

  DATA: BEGIN OF LS_TGSBT,
          GSBER TYPE TGSBT-GSBER,
          GTEXT TYPE TGSBT-GTEXT,
        END OF LS_TGSBT,
        LT_TGSBT LIKE TABLE OF LS_TGSBT.
  DATA: BEGIN OF LS_T001W,
          WERKS TYPE T001W-WERKS,
          NAME1 TYPE T001W-NAME1,
        END OF LS_T001W,
        LT_T001W LIKE TABLE OF LS_T001W.

  CLEAR: GT_UPLOAD, GT_UPLOAD[],
         LS_TGSBT, LT_TGSBT, LS_T001W, LT_T001W,
         LO_CX, LV_MSG.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FILENM
      I_BEGIN_COL             = '1'
      I_BEGIN_ROW             = '2'
      I_END_COL               = '40'
      I_END_ROW               = 10000
    TABLES
      INTERN                  = GT_UPLOAD
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  IF SY-SUBRC NE 0.
    "엑셀파일 처리중에 에러가 발생하였습니다.
    MESSAGE S001 WITH TEXT-M01 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  LOOP AT GT_UPLOAD.
    AT NEW ROW.
      CLEAR GS_OUTTAB.
    ENDAT.

    CONDENSE GT_UPLOAD-VALUE.

    TRY .
        CASE GT_UPLOAD-COL.
          WHEN '0001'. "프로젝트
            GS_OUTTAB-PSPID = GT_UPLOAD-VALUE.
          WHEN '0002'. "프로젝트 내역
            GS_OUTTAB-PRPOST1 = GT_UPLOAD-VALUE.
          WHEN '0003'. "회사코드
            GS_OUTTAB-VBUKR = GT_UPLOAD-VALUE.
          WHEN '0004'. "사업영역
            GS_OUTTAB-VGSBR = GT_UPLOAD-VALUE.
          WHEN '0005'. "손익센터
            GS_OUTTAB-VPRCTR = GT_UPLOAD-VALUE.
          WHEN '0006'. "프로젝트 재고
            GS_OUTTAB-BESTA = GT_UPLOAD-VALUE.
          WHEN '0007'. "WBS요소
            GS_OUTTAB-POSID = GT_UPLOAD-VALUE.
          WHEN '0008'. "WBS내역
            GS_OUTTAB-WBSPOST1 = GT_UPLOAD-VALUE.
          WHEN '0009'. "회사코드
            GS_OUTTAB-PBUKR = GT_UPLOAD-VALUE.
          WHEN '0010'. "손익센터
            GS_OUTTAB-PPRCTR = GT_UPLOAD-VALUE.
          WHEN '0011'. "사업영역
            GS_OUTTAB-PGSBR = GT_UPLOAD-VALUE.
          WHEN '0012'. "플랜트
            GS_OUTTAB-WERKS = GT_UPLOAD-VALUE.
          WHEN '0013'. "시작일자
            REPLACE ALL OCCURRENCES OF '.' IN GT_UPLOAD-VALUE WITH SPACE.
            CONDENSE GT_UPLOAD-VALUE.
            GS_OUTTAB-PSTRT = GT_UPLOAD-VALUE.
          WHEN '0014'. "종료일자
            REPLACE ALL OCCURRENCES OF '.' IN GT_UPLOAD-VALUE WITH SPACE.
            CONDENSE GT_UPLOAD-VALUE.
            GS_OUTTAB-PENDE = GT_UPLOAD-VALUE.
          WHEN '0015'. "매출유형
            GS_OUTTAB-ZZSCT = GT_UPLOAD-VALUE.
          WHEN '0016'. "프로젝트단계
            GS_OUTTAB-ZZPHA = GT_UPLOAD-VALUE.
          WHEN '0017'. "WBS유형
            GS_OUTTAB-ZZWBT = GT_UPLOAD-VALUE.
          WHEN '0018'. "사업구분
            GS_OUTTAB-ZZBGU = GT_UPLOAD-VALUE.
          WHEN '0019'. "사업구분상세
            GS_OUTTAB-ZZBGD = GT_UPLOAD-VALUE.
          WHEN '0020'. "발주처유형
            GS_OUTTAB-ZZPRG = GT_UPLOAD-VALUE.
          WHEN '0021'. "행정구역
            GS_OUTTAB-ZZADT = GT_UPLOAD-VALUE.
          WHEN '0022'. "Huvis.W 하위
            GS_OUTTAB-ZZHWB = GT_UPLOAD-VALUE.
          WHEN '0023'. "사업소유무
            GS_OUTTAB-ZZBAG = GT_UPLOAD-VALUE.
          WHEN '0024'. "투자유무
            GS_OUTTAB-ZZIVC = GT_UPLOAD-VALUE.
          WHEN '0025'. "수주유형
            GS_OUTTAB-ZZCOP = GT_UPLOAD-VALUE.
          WHEN '0026'. "통제유형
            GS_OUTTAB-ZZCYP = GT_UPLOAD-VALUE.
        ENDCASE.

      CATCH CX_ROOT INTO LO_CX.
        LV_MSG = LO_CX->GET_TEXT( ).
        CONCATENATE LV_MSG '(Row:' GT_UPLOAD-ROW ',Col:' GT_UPLOAD-COL ')'
        INTO LV_MSG.

        MESSAGE S002 WITH '엑셀데이터 오류:' LV_MSG DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
    ENDTRY.

    AT END OF ROW.
      APPEND GS_OUTTAB TO GT_OUTTAB.
    ENDAT.
  ENDLOOP.

  SELECT GSBER GTEXT
    INTO TABLE LT_TGSBT
    FROM TGSBT
     FOR ALL ENTRIES IN GT_OUTTAB
   WHERE GSBER = GT_OUTTAB-PGSBR
     AND SPRAS = SY-LANGU.
  SORT LT_TGSBT BY GSBER.

  SELECT WERKS NAME1
    INTO TABLE LT_T001W
    FROM T001W
     FOR ALL ENTRIES IN GT_OUTTAB
   WHERE WERKS = GT_OUTTAB-WERKS
     AND SPRAS = SY-LANGU.
  SORT LT_T001W BY WERKS.

  LOOP AT GT_OUTTAB INTO GS_OUTTAB.
    READ TABLE LT_TGSBT INTO LS_TGSBT WITH KEY GSBER = GS_OUTTAB-PGSBR
                                      BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_OUTTAB-GTEXT = LS_TGSBT-GTEXT.
    ENDIF.

    READ TABLE LT_T001W INTO LS_T001W WITH KEY WERKS = GS_OUTTAB-WERKS
                                      BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_OUTTAB-NAME1 = LS_T001W-NAME1.
    ENDIF.

    MODIFY GT_OUTTAB FROM GS_OUTTAB.
    CLEAR  GS_OUTTAB.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM MODIFY_DATA .
  DATA LT_CELLTAB TYPE LVC_T_STYL.
  REFRESH LT_CELLTAB.

* Disable edit field
  LOOP AT GT_OUTTAB INTO GS_OUTTAB.
    PERFORM DISABLE_FIELD USING 'PRPOST1'   CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'VBUKR'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'VGSBR'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'VPRCTR'    CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'BESTA'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'WBSPOST1'  CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'PBUKR'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'PPRCTR'    CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'PGSBR'     CHANGING LT_CELLTAB.
*    PERFORM DISABLE_FIELD USING 'GTEXT'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'WERKS'     CHANGING LT_CELLTAB.
*    PERFORM DISABLE_FIELD USING 'NAME1'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'PSTRT'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'PENDE'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'STUFE'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'PLINT'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'PLAKZ'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'BELKZ'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'FAKKZ'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'CLASF'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'ABGSL'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'ZZSCT'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'ZZPHA'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'ZZWBT'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'ZZBGU'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'ZZBGD'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'ZZPRG'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'ZZADT'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'ZZHWB'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'ZZBAG'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'ZZIVC'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'ZZCOP'     CHANGING LT_CELLTAB.
    PERFORM DISABLE_FIELD USING 'ZZCYP'     CHANGING LT_CELLTAB.

    GS_OUTTAB-CELLTAB = LT_CELLTAB.

    GS_OUTTAB-ICON = ICON_YELLOW_LIGHT.

    "프로파일
    IF RB_CRT = 'X'.
      IF GS_OUTTAB-VBUKR = '1700'.
        GS_OUTTAB-PROFL  = 'Z000001'.
      ELSE.
        GS_OUTTAB-PROFL  = 'Z000002'.
      ENDIF.
    ENDIF.

    MODIFY GT_OUTTAB FROM GS_OUTTAB TRANSPORTING CELLTAB ICON PROFL.
    CLEAR  GS_OUTTAB.
  ENDLOOP.

  GT_TEMP[] = GT_OUTTAB[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISABLE_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0124   text
*      <--P_LT_CELLTAB  text
*----------------------------------------------------------------------*
FORM DISABLE_FIELD  USING    P_FIELD
                    CHANGING PT_CELLTAB TYPE LVC_T_STYL.

  DATA LS_CELLTAB TYPE LVC_S_STYL.

  LS_CELLTAB-FIELDNAME = P_FIELD.
  LS_CELLTAB-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.

  INSERT LS_CELLTAB INTO TABLE PT_CELLTAB.
ENDFORM.                    " DISABLE_FIELD
*&---------------------------------------------------------------------*
*&      Form  ENABLE_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1490   text
*      <--P_LT_CELLTAB  text
*----------------------------------------------------------------------*
FORM ENABLE_FIELD  USING    P_FIELD
                   CHANGING PT_CELLTAB TYPE LVC_T_STYL.

  DATA LS_CELLTAB TYPE LVC_S_STYL.

  LS_CELLTAB-FIELDNAME = P_FIELD.
  LS_CELLTAB-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.

  INSERT LS_CELLTAB INTO TABLE PT_CELLTAB.
ENDFORM.                    " ENABLE_FIELD
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA .
  DATA: LS_PROJ TYPE PROJ,
        LT_PROJ TYPE TABLE OF PROJ,
        LS_PRPS TYPE PRPS,
        LT_PRPS TYPE TABLE OF PRPS.

  DATA: BEGIN OF LS_PRTE,
          POSNR TYPE PRTE-POSNR,
          PSTRT TYPE PRTE-PSTRT,
          PENDE TYPE PRTE-PENDE,
        END OF LS_PRTE,
        LT_PRTE LIKE TABLE OF LS_PRTE.
  DATA: BEGIN OF LS_TGSBT,
          GSBER TYPE TGSBT-GSBER,
          GTEXT TYPE TGSBT-GTEXT,
        END OF LS_TGSBT,
        LT_TGSBT LIKE TABLE OF LS_TGSBT.
  DATA: BEGIN OF LS_T001W,
          WERKS TYPE T001W-WERKS,
          NAME1 TYPE T001W-NAME1,
        END OF LS_T001W,
        LT_T001W LIKE TABLE OF LS_T001W.

  CLEAR: LS_PROJ, LT_PROJ, LS_PRPS, LT_PRPS,
         LS_TGSBT, LT_TGSBT, LS_T001W, LT_T001W,
         GS_OUTTAB, GT_OUTTAB.

  SELECT PSPID POST1 PROFL VBUKR VGSBR PRCTR PSPNR BESTA
    INTO CORRESPONDING FIELDS OF TABLE LT_PROJ
    FROM PROJ
   WHERE VBUKR IN S_VBUKR
     AND PSPID IN S_PSPID
     AND ERDAT IN S_ERDAT.

  SELECT PSPNR POSID POST1 PBUKR PGSBR PRCTR WERKS STUFE PLINT
         PLAKZ BELKZ FAKKZ CLASF ABGSL ZZSCT ZZPHA ZZWBT
         ZZBGU ZZBGD ZZPRG ZZADT ZZHWB ZZBAG ZZIVC ZZCOP ZZCYP
    INTO CORRESPONDING FIELDS OF TABLE LT_PRPS
    FROM PRPS
   WHERE ERDAT IN S_ERDAT
     AND PRCTR IN S_PRCTR
     AND POSID IN S_POSID.
  SORT LT_PRPS BY POSID.

  SELECT POSNR PSTRT PENDE
    INTO TABLE LT_PRTE
    FROM PRTE
     FOR ALL ENTRIES IN LT_PRPS
   WHERE POSNR = LT_PRPS-PSPNR.
  SORT LT_PRTE BY POSNR.

  SELECT GSBER GTEXT
    INTO TABLE LT_TGSBT
    FROM TGSBT
     FOR ALL ENTRIES IN LT_PRPS
   WHERE GSBER = LT_PRPS-PGSBR
     AND SPRAS = SY-LANGU.
  SORT LT_TGSBT BY GSBER.

  SELECT WERKS NAME1
    INTO TABLE LT_T001W
    FROM T001W
     FOR ALL ENTRIES IN LT_PRPS
   WHERE WERKS = LT_PRPS-WERKS
     AND SPRAS = SY-LANGU.
  SORT LT_T001W BY WERKS.


  LOOP AT LT_PROJ INTO LS_PROJ.
    LOOP AT LT_PRPS INTO LS_PRPS WHERE POSID(5) = LS_PROJ-PSPID.
      MOVE-CORRESPONDING LS_PROJ TO GS_OUTTAB.
      GS_OUTTAB-PRPOST1 = LS_PROJ-POST1.
      GS_OUTTAB-VPRCTR  = LS_PROJ-PRCTR.

      GS_OUTTAB-POSID    = LS_PRPS-POSID.
      GS_OUTTAB-WBSPOST1 = LS_PRPS-POST1.
      GS_OUTTAB-PBUKR    = LS_PRPS-PBUKR.
      GS_OUTTAB-PGSBR    = LS_PRPS-PGSBR.
      GS_OUTTAB-PPRCTR   = LS_PRPS-PRCTR.
      GS_OUTTAB-WERKS    = LS_PRPS-WERKS.
      GS_OUTTAB-STUFE    = LS_PRPS-STUFE.
      GS_OUTTAB-PLINT    = LS_PRPS-PLINT.
      GS_OUTTAB-PLAKZ    = LS_PRPS-PLAKZ.
      GS_OUTTAB-BELKZ    = LS_PRPS-BELKZ.
      GS_OUTTAB-FAKKZ    = LS_PRPS-FAKKZ.
      GS_OUTTAB-CLASF    = LS_PRPS-CLASF.
      GS_OUTTAB-ABGSL    = LS_PRPS-ABGSL.
      GS_OUTTAB-ZZSCT    = LS_PRPS-ZZSCT.
      GS_OUTTAB-ZZPHA    = LS_PRPS-ZZPHA.
      GS_OUTTAB-ZZWBT    = LS_PRPS-ZZWBT.
      GS_OUTTAB-ZZBGU    = LS_PRPS-ZZBGU.
      GS_OUTTAB-ZZBGD    = LS_PRPS-ZZBGD.
      GS_OUTTAB-ZZPRG    = LS_PRPS-ZZPRG.
      GS_OUTTAB-ZZADT    = LS_PRPS-ZZADT.
      GS_OUTTAB-ZZHWB    = LS_PRPS-ZZHWB.
      GS_OUTTAB-ZZBAG    = LS_PRPS-ZZBAG.
      GS_OUTTAB-ZZIVC    = LS_PRPS-ZZIVC.
      GS_OUTTAB-ZZCOP    = LS_PRPS-ZZCOP.
      GS_OUTTAB-ZZCYP    = LS_PRPS-ZZCYP.

      READ TABLE LT_PRTE INTO LS_PRTE WITH KEY POSNR = LS_PRPS-PSPNR
                                      BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        GS_OUTTAB-PSTRT = LS_PRTE-PSTRT.
        GS_OUTTAB-PENDE = LS_PRTE-PENDE.
      ENDIF.

      READ TABLE LT_TGSBT INTO LS_TGSBT WITH KEY GSBER = GS_OUTTAB-PGSBR
                                        BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        GS_OUTTAB-GTEXT = LS_TGSBT-GTEXT.
      ENDIF.

      READ TABLE LT_T001W INTO LS_T001W WITH KEY WERKS = GS_OUTTAB-WERKS
                                        BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        GS_OUTTAB-NAME1 = LS_T001W-NAME1.
      ENDIF.

      APPEND GS_OUTTAB TO GT_OUTTAB.
      CLEAR  GS_OUTTAB.
    ENDLOOP.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATA_CHANGED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ER_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM DATA_CHANGED  USING    PR_DATA_CHANGED
                            TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA: LS_MOD_CELLS TYPE LVC_S_MODI,
        LT_CELLTAB   TYPE LVC_T_STYL.

  LOOP AT PR_DATA_CHANGED->MT_GOOD_CELLS INTO LS_MOD_CELLS.

    READ TABLE GT_OUTTAB INTO GS_OUTTAB INDEX LS_MOD_CELLS-ROW_ID.

    CASE LS_MOD_CELLS-FIELDNAME.
      WHEN 'WERKS'.
        CALL METHOD PR_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_MOD_CELLS-ROW_ID
            I_FIELDNAME = 'WERKS'
          IMPORTING
            E_VALUE     = GS_OUTTAB-WERKS.

* modify field
        SELECT SINGLE NAME1
          FROM T001W
          INTO GS_OUTTAB-NAME1
         WHERE WERKS = GS_OUTTAB-WERKS
           AND SPRAS = SY-LANGU.

        CALL METHOD PR_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_MOD_CELLS-ROW_ID
            I_FIELDNAME = 'NAME1'
            I_VALUE     = GS_OUTTAB-NAME1.

        MODIFY GT_OUTTAB FROM GS_OUTTAB INDEX LS_MOD_CELLS-ROW_ID
                                        TRANSPORTING WERKS NAME1.
        CLEAR  GS_OUTTAB.

      WHEN 'PGSBR'.
        CALL METHOD PR_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_MOD_CELLS-ROW_ID
            I_FIELDNAME = 'PGSBR'
          IMPORTING
            E_VALUE     = GS_OUTTAB-PGSBR.

* modify field
        SELECT SINGLE GTEXT
          FROM TGSBT
          INTO GS_OUTTAB-GTEXT
         WHERE GSBER = GS_OUTTAB-PGSBR
           AND SPRAS = SY-LANGU.

        CALL METHOD PR_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_MOD_CELLS-ROW_ID
            I_FIELDNAME = 'GTEXT'
            I_VALUE     = GS_OUTTAB-GTEXT.

        MODIFY GT_OUTTAB FROM GS_OUTTAB INDEX LS_MOD_CELLS-ROW_ID
                                        TRANSPORTING PGSBR GTEXT.
        CLEAR  GS_OUTTAB.

    ENDCASE.
  ENDLOOP.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_OBJ .
  CREATE OBJECT GO_CONTAINER
    EXPORTING
      REPID     = SY-REPID
      DYNNR     = SY-DYNNR
      SIDE      = GO_CONTAINER->DOCK_AT_LEFT
      EXTENSION = 2500
    EXCEPTIONS
      OTHERS    = 1.

  CREATE OBJECT GO_GRID
    EXPORTING
      I_PARENT = GO_CONTAINER.
ENDFORM.                    " CREATE_OBJ
*&---------------------------------------------------------------------*
*&      Form  SETTING_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SETTING_LAYOUT .
  CLEAR GS_LAYOUT.

  GS_LAYOUT-CWIDTH_OPT = 'X'.
  GS_LAYOUT-ZEBRA      = 'X'.
  GS_LAYOUT-SEL_MODE   = 'D'.
  GS_LAYOUT-STYLEFNAME = 'CELLTAB'.
ENDFORM.                    " SETTING_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SETTING_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SETTING_FCAT .
  IF GT_FCAT[] IS INITIAL.
    PERFORM MAKE_FIELDCAT.
    PERFORM MODIFY_FIELDCAT.
  ENDIF.
ENDFORM.                    " SETTING_FCAT
*&---------------------------------------------------------------------*
*&      Form  MAKE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_FIELDCAT .
  DATA : LT_FCAT TYPE SLIS_T_FIELDCAT_ALV.
  CLEAR: LT_FCAT[], GT_FCAT[].

  SET PARAMETER ID 'ALVBUFFER' FIELD SY-UZEIT.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = SY-REPID
      I_INTERNAL_TABNAME     = 'GT_FCTAB'
      I_INCLNAME             = SY-REPID
      I_BYPASSING_BUFFER     = 'X'
    CHANGING
      CT_FIELDCAT            = LT_FCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      IT_FIELDCAT_ALV = LT_FCAT
    IMPORTING
      ET_FIELDCAT_LVC = GT_FCAT
    TABLES
      IT_DATA         = GT_FCTAB
    EXCEPTIONS
      IT_DATA_MISSING = 1
      OTHERS          = 2.
ENDFORM.                    " MAKE_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  MODIFY_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_FIELDCAT .
  DEFINE __FIELDCAT.
    if &1 is not initial.
      gs_fcat-col_pos = &1.
    endif.

    if &2 is not initial.
      gs_fcat-coltext
      = gs_fcat-tooltip
      = gs_fcat-seltext
      = gs_fcat-scrtext_m
      = &2.
    endif.

    if &3 is not initial.
      gs_fcat-edit = &3.
    endif.

    if &4 is not initial.
      gs_fcat-emphasize = &4.
    endif.
  END-OF-DEFINITION.

  LOOP AT GT_FCAT INTO GS_FCAT.
    CASE GS_FCAT-FIELDNAME.
      WHEN 'ICON'.
        __FIELDCAT : '1'    TEXT-F37  ' '  'C300'.
      WHEN 'PSPID'.
        __FIELDCAT : '2'    TEXT-F01  ' '  'C300'.
      WHEN 'PRPOST1'.
        __FIELDCAT : '3'    TEXT-F02  'X'  'C300'.
      WHEN 'PROFL'.
        __FIELDCAT : '4'    TEXT-F03  ' '  'C300'.
      WHEN 'VBUKR'.
        __FIELDCAT : '5'    TEXT-F04  'X'  'C300'.
      WHEN 'VGSBR'.
        __FIELDCAT : '6'    TEXT-F05  'X'  'C300'.
      WHEN 'VPRCTR'.
        __FIELDCAT : '7'    TEXT-F06  'X'  'C300'.
      WHEN 'BESTA'.
        __FIELDCAT : '8'    TEXT-F38  'X'  'C300'.
      WHEN 'POSID'.
        __FIELDCAT : '9'    TEXT-F08  ' '  ''.
      WHEN 'WBSPOST1'.
        __FIELDCAT : '10'   TEXT-F09  'X'  ''.
      WHEN 'PBUKR'.
        __FIELDCAT : '11'   TEXT-F04  'X'  ''.
      WHEN 'PPRCTR'.  .
        __FIELDCAT : '12'   TEXT-F06  'X'  ''.
      WHEN 'PGSBR'.
        __FIELDCAT : '13'   TEXT-F05  'X'  ''.
      WHEN 'GTEXT'.
        __FIELDCAT : '14'   TEXT-F10  ' '  ''.
      WHEN 'WERKS'.
        __FIELDCAT : '15'   TEXT-F11  'X'  ''.
      WHEN 'NAME1'.
        __FIELDCAT : '16'   TEXT-F12  ' '  ''.
      WHEN 'PSTRT'.
        __FIELDCAT : '17'   TEXT-F13  'X'  ''.
      WHEN 'PENDE'.
        __FIELDCAT : '18'   TEXT-F14  'X'  ''.
      WHEN 'STUFE'.
        __FIELDCAT : '19'   TEXT-F15  'X'  ''.
      WHEN 'PLINT'.
        __FIELDCAT : '20'   TEXT-F16  'X'  ''.
      WHEN 'PLAKZ'.
        __FIELDCAT : '21'   TEXT-F17  'X'  ''.
      WHEN 'BELKZ'.
        __FIELDCAT : '22'   TEXT-F18  'X'  ''.
      WHEN 'FAKKZ'.
        __FIELDCAT : '23'   TEXT-F19  'X'  ''.
      WHEN 'CLASF'.
        __FIELDCAT : '24'   TEXT-F20  'X'  ''.
      WHEN 'ABGSL'.
        __FIELDCAT : '25'   TEXT-F21  'X'  ''.
      WHEN 'ZZSCT'.
        __FIELDCAT : '26'   TEXT-F22  'X'  ''.
      WHEN 'ZZPHA'.
        __FIELDCAT : '27'   TEXT-F23  'X'  ''.
      WHEN 'ZZWBT'.
        __FIELDCAT : '28'   TEXT-F24  'X'  ''.
      WHEN 'ZZBGU'.
        __FIELDCAT : '29'   TEXT-F25  'X'  ''.
      WHEN 'ZZBGD'.
        __FIELDCAT : '30'   TEXT-F26  'X'  ''.
      WHEN 'ZZPRG'.
        __FIELDCAT : '31'   TEXT-F27  'X'  ''.
      WHEN 'ZZADT'.
        __FIELDCAT : '32'   TEXT-F28  'X'  ''.
      WHEN 'ZZHWB'.
        __FIELDCAT : '33'   TEXT-F29  'X'  ''.
      WHEN 'ZZBAG'.
        __FIELDCAT : '34'   TEXT-F30  'X'  ''.
      WHEN 'ZZIVC'.
        __FIELDCAT : '35'   TEXT-F31  'X'  ''.
      WHEN 'ZZCOP'.
        __FIELDCAT : '36'   TEXT-F32  'X'  ''.
      WHEN 'ZZCYP'.
        __FIELDCAT : '37'   TEXT-F33  'X'  ''.
      WHEN 'MESSAGE'.
        __FIELDCAT : '38'   TEXT-F36  ' '  ''.
      WHEN OTHERS.
        GS_FCAT-NO_OUT = 'X'.
    ENDCASE.

    MODIFY GT_FCAT FROM GS_FCAT.
    CLEAR  GS_FCAT.
  ENDLOOP.
ENDFORM.                    " MODIFY_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SETTING_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SETTING_EVENT .
  CALL METHOD GO_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  CALL METHOD GO_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CREATE OBJECT GO_EVENT.
  SET HANDLER GO_EVENT->HANDLE_DATA_CHANGED FOR GO_GRID.
ENDFORM.                    " SETTING_EVENT
*&---------------------------------------------------------------------*
*&      Form  SETTING_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SETTING_EXCLUDE  CHANGING PT_EXCLUDE TYPE UI_FUNCTIONS.
  PERFORM APPEND_EXCLUDE_FUNCTIONS
         TABLES PT_EXCLUDE
         USING:
                CL_GUI_ALV_GRID=>MC_FC_FIND,
*                CL_GUI_ALV_GRID=>MC_FC_SORT_ASC,
*                CL_GUI_ALV_GRID=>MC_FC_SORT_DSC,
                CL_GUI_ALV_GRID=>MC_MB_SUBTOT,
                CL_GUI_ALV_GRID=>MC_MB_SUM,

                CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
                CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
                CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
                CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
                CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,
                CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
                CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
                CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
                CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW,
                CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
ENDFORM.                    " SETTING_EXCLUDE
*&---------------------------------------------------------------------*
*&      Form  APPEND_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_EXCLUDE  text
*      -->P_CL_GUI_ALV_GRID=>MC_FC_LOC_COP  text
*----------------------------------------------------------------------*
FORM APPEND_EXCLUDE_FUNCTIONS  TABLES   P_TABLE USING P_VALUE.
  DATA: LS_EXCLUDE TYPE UI_FUNC.

  LS_EXCLUDE = P_VALUE.
  APPEND LS_EXCLUDE TO P_TABLE.
ENDFORM.                    " APPEND_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV .
  GS_VARIANT-REPORT   = SY-REPID.
  GS_VARIANT-USERNAME = SY-UNAME.

  CALL METHOD GO_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_BYPASSING_BUFFER   = 'X'
      I_SAVE               = 'A'
      I_DEFAULT            = 'X'
      IS_LAYOUT            = GS_LAYOUT
      IS_VARIANT           = GS_VARIANT
      IT_TOOLBAR_EXCLUDING = GT_EXCLUDE
    CHANGING
      IT_OUTTAB            = GT_OUTTAB[]
      IT_FIELDCATALOG      = GT_FCAT.
ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH_ALV .
  GS_STBL-COL = 'X'.
  GS_STBL-ROW = 'X'.

  CALL METHOD GO_GRID->SET_FRONTEND_LAYOUT
    EXPORTING
      IS_LAYOUT = GS_LAYOUT.

  CALL METHOD GO_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = GS_STBL.
ENDFORM.                    " REFRESH_ALV
*&---------------------------------------------------------------------*
*& Form DISP_EDIT_MODE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DISP_EDIT_MODE USING P_EDIT.
  DATA LT_CELLTAB  TYPE LVC_T_STYL.
  REFRESH LT_CELLTAB.

  IF P_EDIT IS INITIAL.
    LOOP AT GT_OUTTAB INTO GS_OUTTAB.
      PERFORM ENABLE_FIELD USING 'PRPOST1'   CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'VBUKR'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'VGSBR'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'VPRCTR'    CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'BESTA'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'WBSPOST1'  CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'PBUKR'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'PPRCTR'    CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'PGSBR'     CHANGING LT_CELLTAB.
*      PERFORM ENABLE_FIELD USING 'GTEXT'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'WERKS'     CHANGING LT_CELLTAB.
*      PERFORM ENABLE_FIELD USING 'NAME1'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'PSTRT'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'PENDE'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'STUFE'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'PLINT'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'PLAKZ'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'BELKZ'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'FAKKZ'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'CLASF'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'ABGSL'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'ZZSCT'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'ZZPHA'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'ZZWBT'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'ZZBGU'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'ZZBGD'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'ZZPRG'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'ZZADT'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'ZZHWB'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'ZZBAG'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'ZZIVC'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'ZZCOP'     CHANGING LT_CELLTAB.
      PERFORM ENABLE_FIELD USING 'ZZCYP'     CHANGING LT_CELLTAB.

      GS_OUTTAB-CELLTAB = LT_CELLTAB.

      MODIFY GT_OUTTAB FROM GS_OUTTAB TRANSPORTING CELLTAB.
      CLEAR  GS_OUTTAB.
    ENDLOOP.

  ELSE.
    LOOP AT GT_OUTTAB INTO GS_OUTTAB.
      PERFORM DISABLE_FIELD USING 'PRPOST1'   CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'VBUKR'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'VGSBR'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'VPRCTR'    CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'BESTA'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'WBSPOST1'  CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'PBUKR'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'PPRCTR'    CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'PGSBR'     CHANGING LT_CELLTAB.
*      PERFORM DISABLE_FIELD USING 'GTEXT'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'WERKS'     CHANGING LT_CELLTAB.
*      PERFORM DISABLE_FIELD USING 'NAME1'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'PSTRT'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'PENDE'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'STUFE'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'PLINT'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'PLAKZ'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'BELKZ'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'FAKKZ'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'CLASF'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'ABGSL'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'ZZSCT'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'ZZPHA'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'ZZWBT'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'ZZBGU'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'ZZBGD'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'ZZPRG'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'ZZADT'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'ZZHWB'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'ZZBAG'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'ZZIVC'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'ZZCOP'     CHANGING LT_CELLTAB.
      PERFORM DISABLE_FIELD USING 'ZZCYP'     CHANGING LT_CELLTAB.

      GS_OUTTAB-CELLTAB = LT_CELLTAB.

      MODIFY GT_OUTTAB FROM GS_OUTTAB TRANSPORTING CELLTAB.
      CLEAR  GS_OUTTAB.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_SELECTED_ROW
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_SELECTED_ROW .
  CALL METHOD GO_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROW.

  IF GT_ROW[] IS INITIAL.
    MESSAGE S015 DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_PRJ_WBS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM RUN_BAPI_PROJECT_MAINTAIN .
  DATA: LS_DATA LIKE GS_OUTTAB,
        LT_DATA LIKE TABLE OF GS_OUTTAB.

  CHECK GT_ROW[] IS NOT INITIAL.

  LOOP AT GT_ROW INTO GS_ROW.
    READ TABLE GT_OUTTAB INTO GS_OUTTAB INDEX GS_ROW-INDEX.

    GS_OUTTAB-MARK = 'X'.
    MODIFY GT_OUTTAB FROM GS_OUTTAB INDEX GS_ROW-INDEX.

    MOVE-CORRESPONDING GS_OUTTAB TO LS_DATA.
    APPEND LS_DATA TO LT_DATA.

    IF RB_DISP = 'X'.
      READ TABLE GT_TEMP INTO GS_TEMP INDEX GS_ROW-INDEX.

*PROJECT
      IF LS_DATA-PRPOST1 NE GS_TEMP-PRPOST1.
        GS_FLAG-PRPOST1 = 'X'.
        GV_PROJ = 'X'.
      ENDIF.
      IF LS_DATA-VBUKR NE GS_TEMP-VBUKR.
        GS_FLAG-VBUKR = 'X'.
        GV_PROJ = 'X'.
      ENDIF.
      IF LS_DATA-VGSBR NE GS_TEMP-VGSBR.
        GS_FLAG-VGSBR = 'X'.
        GV_PROJ = 'X'.
      ENDIF.
      IF LS_DATA-VPRCTR NE GS_TEMP-VPRCTR.
        GS_FLAG-VPRCTR = 'X'.
        GV_PROJ = 'X'.
      ENDIF.
      IF LS_DATA-BESTA NE GS_TEMP-BESTA.
        GS_FLAG-BESTA = 'X'.
        GV_PROJ = 'X'.
      ENDIF.

*WBS
      IF LS_DATA-WBSPOST1 NE GS_TEMP-WBSPOST1.
        GS_FLAG-WBSPOST1 = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-PBUKR NE GS_TEMP-PBUKR.
        GS_FLAG-PBUKR = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-PPRCTR NE GS_TEMP-PPRCTR.
        GS_FLAG-PPRCTR = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-PGSBR NE GS_TEMP-PGSBR.
        GS_FLAG-PGSBR = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-WERKS NE GS_TEMP-WERKS.
        GS_FLAG-WERKS = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-PSTRT NE GS_TEMP-PSTRT.
        GS_FLAG-PSTRT = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-PENDE NE GS_TEMP-PENDE.
        GS_FLAG-PENDE = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-STUFE NE GS_TEMP-STUFE.
        GS_FLAG-STUFE = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-PLINT NE GS_TEMP-PLINT.
        GS_FLAG-PLINT = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-PLAKZ NE GS_TEMP-PLAKZ.
        GS_FLAG-PLAKZ = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-BELKZ NE GS_TEMP-BELKZ.
        GS_FLAG-BELKZ = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-FAKKZ NE GS_TEMP-FAKKZ.
        GS_FLAG-FAKKZ = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-CLASF NE GS_TEMP-CLASF.
        GS_FLAG-CLASF = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-ABGSL NE GS_TEMP-ABGSL.
        GS_FLAG-ABGSL = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-ZZSCT NE GS_TEMP-ZZSCT.
        GS_FLAG-ZZSCT = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-ZZPHA NE GS_TEMP-ZZPHA.
        GS_FLAG-ZZPHA = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-ZZWBT NE GS_TEMP-ZZWBT.
        GS_FLAG-ZZWBT = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-ZZBGU NE GS_TEMP-ZZBGU.
        GS_FLAG-ZZBGU = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-ZZBGD NE GS_TEMP-ZZBGD.
        GS_FLAG-ZZBGD = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-ZZPRG NE GS_TEMP-ZZPRG.
        GS_FLAG-ZZPRG = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-ZZADT NE GS_TEMP-ZZADT.
        GS_FLAG-ZZADT = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-ZZHWB NE GS_TEMP-ZZHWB.
        GS_FLAG-ZZHWB = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-ZZBAG NE GS_TEMP-ZZBAG.
        GS_FLAG-ZZBAG = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-ZZIVC NE GS_TEMP-ZZIVC.
        GS_FLAG-ZZIVC = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-ZZCOP NE GS_TEMP-ZZCOP.
        GS_FLAG-ZZCOP = 'X'.
        GV_WBS = 'X'.
      ENDIF.
      IF LS_DATA-ZZCYP NE GS_TEMP-ZZCYP.
        GS_FLAG-ZZCYP = 'X'.
        GV_WBS = 'X'.
      ENDIF.

      APPEND GS_FLAG TO GT_FLAG.
      CLEAR  GS_FLAG.
    ENDIF.
  ENDLOOP.

  IF RB_CRT = 'X'. "생성
    PERFORM CREATE_PRJ_WBS TABLES LT_DATA.

  ELSEIF RB_DISP = 'X'. "조회/변경
    PERFORM UPDATE_PRJ_WBS TABLES LT_DATA.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_PRJ_WBS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_DATA
*&---------------------------------------------------------------------*
FORM CREATE_PRJ_WBS  TABLES    PT_DATA STRUCTURE GS_OUTTAB.
  DATA: LS_DATA   LIKE GS_OUTTAB.

  DATA: LV_REFNO  TYPE BAPI_METHOD_PROJECT-REFNUMBER,
        LV_TABIX  TYPE SY-TABIX,
        LV_ERR(1).

  CLEAR: LV_REFNO, LV_TABIX, LV_ERR.

  LOOP AT PT_DATA INTO LS_DATA.
    LV_TABIX = SY-TABIX.
    LV_REFNO = LV_REFNO + 1.

    AT NEW PSPID.
      "Default Return Value
      GS_RETURN-TYPE = GC_S.

      CLEAR: GS_PROJ, GS_RETURN.

      READ TABLE PT_DATA INTO LS_DATA INDEX LV_TABIX.
      GS_PROJ-PROJECT_DEFINITION = LS_DATA-PSPID.   "외부번호
      GS_PROJ-DESCRIPTION        = LS_DATA-PRPOST1. "내역
      GS_PROJ-COMP_CODE          = LS_DATA-VBUKR.   "회사코드
      GS_PROJ-BUS_AREA           = LS_DATA-VGSBR.   "사업영역
      GS_PROJ-PROFIT_CTR         = LS_DATA-VPRCTR.  "손익센터
*    CONCATENATE P_GJAHR '0101' INTO LS_PROJ-START.  "시작일
      GS_PROJ-START              = LS_DATA-PSTRT.   "시작일

      IF GS_OUTTAB-VBUKR = '1700'.
        GS_PROJ-PROJECT_PROFILE  = 'Z000001'.         "프로파일
      ELSE.
        GS_PROJ-PROJECT_PROFILE  = 'Z000002'.         "프로파일
      ENDIF.

* Input Check
      PERFORM CHECK_INPUT_DATA_PROJ USING GS_PROJ
                                          SPACE
                                 CHANGING GS_RETURN.

      IF GS_RETURN-TYPE = GC_E. "실패
        LV_ERR = 'X'.
        EXIT.

      ELSE. "성공 시 프로젝트 생성 및 릴리즈
        PERFORM IMPORTING_PROJ_CRE_DATA_PROCC USING 'CREATE'
                                                    LS_DATA.

        PERFORM EXECUTE_BAPI_TRANSACTION TABLES GT_METHPROJ
                                                GT_WBS_TAB_UP
                                                GT_WBS_TAB
                                                GT_MSGTAB
                                          USING GS_PROJ
                                                GS_PROJDEF_UP
                                       CHANGING GS_RETURN.
        IF GS_RETURN-TYPE EQ GC_S.
          CLEAR: GS_RETURN, GT_METHPROJ, GT_METHPROJ[].
          PERFORM IMPORTING_PROJ_CRE_DATA_PROCC USING 'RELEASE'
                                                      LS_DATA.

          PERFORM EXECUTE_BAPI_TRANSACTION TABLES GT_METHPROJ
                                                  GT_WBS_TAB_UP
                                                  GT_WBS_TAB
                                                  GT_MSGTAB
                                            USING GS_PROJ
                                                  GS_PROJDEF_UP
                                         CHANGING GS_RETURN.

          CLEAR: GT_METHPROJ, GT_METHPROJ[].
          IF GS_RETURN-TYPE EQ GC_E.
            LV_ERR = 'X'.
            EXIT.
          ENDIF.

        ELSE.
          LV_ERR = 'X'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDAT.

* BAPI 펑션 파라미터 구조에 따라 데이터 가공
    PERFORM IMPORTING_WBS_CRE_DATA_PROCC USING 'CREATE'
                                               LS_DATA
                                               GS_FLAG
                                               LV_REFNO.

    AT END OF PSPID.
* I_PROJECT_DEFINITION
      CLEAR GS_PROJ.
      GS_PROJ-PROJECT_DEFINITION = LS_DATA-PSPID.   "외부번호

* 마지막 라인에 METHOD'SAVE' 추가해야 반영됨.
      CLEAR GT_METHPROJ.
      GT_METHPROJ-METHOD = CO1_METH-SAVE.           "TEXT: 'SAVE'
      APPEND GT_METHPROJ.

* 프로젝트 생성 위한 BAPI 펑션 호출
      PERFORM EXECUTE_BAPI_TRANSACTION TABLES GT_METHPROJ
                                              GT_WBS_TAB_UP
                                              GT_WBS_TAB
                                              GT_MSGTAB
                                        USING GS_PROJ
                                              GS_PROJDEF_UP
                                     CHANGING GS_RETURN.

      IF GS_RETURN-TYPE = GC_S.
        LOOP AT GT_OUTTAB INTO GS_OUTTAB WHERE PSPID = LS_DATA-PSPID.
          UPDATE PRPS
             SET ZZSCT = GS_OUTTAB-ZZSCT
                 ZZPHA = GS_OUTTAB-ZZPHA
                 ZZWBT = GS_OUTTAB-ZZWBT
                 ZZBGU = GS_OUTTAB-ZZBGU
                 ZZBGD = GS_OUTTAB-ZZBGD
                 ZZPRG = GS_OUTTAB-ZZPRG
                 ZZADT = GS_OUTTAB-ZZADT
                 ZZHWB = GS_OUTTAB-ZZHWB
                 ZZBAG = GS_OUTTAB-ZZBAG
                 ZZIVC = GS_OUTTAB-ZZIVC
                 ZZCOP = GS_OUTTAB-ZZCOP
                 ZZCYP = GS_OUTTAB-ZZCYP
           WHERE POSID = GS_OUTTAB-POSID.

          IF SY-SUBRC EQ 0.
            GS_OUTTAB-ICON    = ICON_GREEN_LIGHT.
            GS_OUTTAB-MESSAGE = 'PROJECT-WBS 생성 성공'.

            MODIFY GT_OUTTAB FROM GS_OUTTAB.
            CLEAR  GS_OUTTAB.

          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

            GS_OUTTAB-ICON    = ICON_RED_LIGHT.
            GS_OUTTAB-MESSAGE = 'PROJECT-WBS 생성 오류: WBS관련 데이터 확인요망!'.

            MODIFY GT_OUTTAB FROM GS_OUTTAB.
            CLEAR  GS_OUTTAB.
          ENDIF.
        ENDLOOP.

      ELSE.
        READ TABLE GT_MSGTAB WITH KEY MESSAGE_TYPE = GC_E.

        LOOP AT GT_OUTTAB INTO GS_OUTTAB WHERE PSPID = LS_DATA-PSPID.
          GS_OUTTAB-ICON    = ICON_RED_LIGHT.
          GS_OUTTAB-MESSAGE = GT_MSGTAB-MESSAGE_TEXT.

          MODIFY GT_OUTTAB FROM GS_OUTTAB.
          CLEAR  GS_OUTTAB.
        ENDLOOP.
      ENDIF.
    ENDAT.
  ENDLOOP.

  IF LV_ERR = 'X'.
    READ TABLE GT_MSGTAB WITH KEY MESSAGE_TYPE = GC_E.

    LOOP AT GT_OUTTAB INTO GS_OUTTAB WHERE PSPID = LS_DATA-PSPID.
      GS_OUTTAB-ICON = ICON_RED_LIGHT.

      IF GT_MSGTAB-MESSAGE_TEXT IS NOT INITIAL.
        GS_OUTTAB-MESSAGE = GT_MSGTAB-MESSAGE_TEXT.
      ELSE.
        GS_OUTTAB-MESSAGE = GS_RETURN-MESSAGE.
      ENDIF.

      MODIFY GT_OUTTAB FROM GS_OUTTAB.
      CLEAR  GS_OUTTAB.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_PRJ_WBS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_DATA
*&---------------------------------------------------------------------*
FORM UPDATE_PRJ_WBS  TABLES    PT_DATA STRUCTURE GS_OUTTAB.
  DATA: LS_DATA   LIKE GS_OUTTAB.

  DATA: LV_REFNO  TYPE BAPI_METHOD_PROJECT-REFNUMBER,
        LV_TABIX  TYPE SY-TABIX,
        LV_MSG    TYPE STRING,
        LV_ERR(1).

  CLEAR: GT_METHPROJ, GT_METHPROJ[], GT_WBS_TAB, GT_WBS_TAB[],
         GT_MSGTAB, GT_MSGTAB[],
         LV_REFNO, LV_TABIX, LV_MSG, LV_ERR.

  LOOP AT PT_DATA INTO LS_DATA.
    LV_TABIX = SY-TABIX.
    LV_REFNO = LV_REFNO + 1.

    READ TABLE GT_FLAG INTO GS_FLAG INDEX LV_TABIX.

    AT NEW PSPID.
      "Default Return Value
      GS_RETURN-TYPE = GC_S.

      IF GV_PROJ = 'X'.
*I_PROJECT_DEFINITION
        CLEAR: GS_PROJ, GS_PROJDEF_UP, GS_RETURN.

        READ TABLE PT_DATA INTO LS_DATA INDEX LV_TABIX.
        GS_PROJ-PROJECT_DEFINITION = LS_DATA-PSPID.   "외부번호
        GS_PROJ-DESCRIPTION        = LS_DATA-PRPOST1. "내역
        GS_PROJ-COMP_CODE          = LS_DATA-VBUKR.   "회사코드
        GS_PROJ-BUS_AREA           = LS_DATA-VGSBR.   "사업영역
        GS_PROJ-PROFIT_CTR         = LS_DATA-VPRCTR.  "손익센터
*    CONCATENATE P_GJAHR '0101' INTO LS_PROJ-START.  "시작일
        GS_PROJ-START              = LS_DATA-PSTRT.   "시작일자

        IF GS_OUTTAB-VBUKR = '1700'.
          GS_PROJ-PROJECT_PROFILE  = 'Z000001'.         "프로파일
        ELSE.
          GS_PROJ-PROJECT_PROFILE  = 'Z000002'.         "프로파일
        ENDIF.

*I_PROJECT_DEFINITION_UPD
        IF GS_FLAG-PRPOST1 = 'X'.
          GS_PROJDEF_UP-DESCRIPTION = 'X'.
        ENDIF.
        IF GS_FLAG-VBUKR = 'X'.
          GS_PROJDEF_UP-COMP_CODE = 'X'.
        ENDIF.
        IF GS_FLAG-VGSBR = 'X'.
          GS_PROJDEF_UP-BUS_AREA = 'X'.
        ENDIF.
        IF GS_FLAG-VPRCTR = 'X'.
          GS_PROJDEF_UP-PROFIT_CTR = 'X'.
        ENDIF.
        IF GS_FLAG-BESTA = 'X'.
          GS_PROJDEF_UP-PROJECT_STOCK = 'X'.
        ENDIF.

* Input Check
        PERFORM CHECK_INPUT_DATA_PROJ USING GS_PROJ
                                            SPACE
                                   CHANGING GS_RETURN.

        IF GS_RETURN-TYPE = GC_E. "실패
          LV_ERR = 'X'.
          EXIT.

        ELSE. "성공 시 프로젝트 생성 및 릴리즈
          PERFORM IMPORTING_PROJ_CRE_DATA_PROCC USING 'UPDATE'
                                                      LS_DATA.

          PERFORM EXECUTE_BAPI_TRANSACTION TABLES GT_METHPROJ
                                                  GT_WBS_TAB_UP
                                                  GT_WBS_TAB
                                                  GT_MSGTAB
                                            USING GS_PROJ
                                                  GS_PROJDEF_UP
                                         CHANGING GS_RETURN.

          CLEAR: GS_PROJ, GS_PROJDEF_UP, GT_METHPROJ, GT_METHPROJ[].

          IF GS_RETURN-TYPE EQ GC_E.
            LV_ERR = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDAT.

* BAPI 펑션 파라미터 구조에 따라 데이터 가공
    IF GV_WBS = 'X'.
      READ TABLE PT_DATA INTO LS_DATA INDEX LV_TABIX.
      PERFORM IMPORTING_WBS_CRE_DATA_PROCC USING 'UPDATE'
                                                 LS_DATA
                                                 GS_FLAG
                                                 LV_REFNO.
    ENDIF.

    AT END OF PSPID.
      IF GV_WBS = 'X'.
* I_PROJECT_DEFINITION
        CLEAR: GS_PROJ, GS_PROJDEF_UP.
        GS_PROJ-PROJECT_DEFINITION = LS_DATA-PSPID.   "외부번호

* 마지막 라인에 METHOD'SAVE' 추가해야 반영됨.
        CLEAR GT_METHPROJ.
        GT_METHPROJ-METHOD = CO1_METH-SAVE.           "TEXT: 'SAVE'
        APPEND GT_METHPROJ.
* 프로젝트 생성 위한 BAPI 펑션 호출
        PERFORM EXECUTE_BAPI_TRANSACTION TABLES GT_METHPROJ
                                                GT_WBS_TAB_UP
                                                GT_WBS_TAB
                                                GT_MSGTAB
                                          USING GS_PROJ
                                                GS_PROJDEF_UP
                                       CHANGING GS_RETURN.
      ENDIF.

      IF GS_RETURN-TYPE = GC_S.
        LOOP AT GT_OUTTAB INTO GS_OUTTAB WHERE PSPID = LS_DATA-PSPID
                                           AND MARK  = 'X'.
          CLEAR LV_MSG.

          IF GV_WBS = 'X'.
            UPDATE PRPS
               SET STUFE = GS_OUTTAB-STUFE
                   PLINT = GS_OUTTAB-PLINT
                   PLAKZ = GS_OUTTAB-PLAKZ
                   BELKZ = GS_OUTTAB-BELKZ
                   FAKKZ = GS_OUTTAB-FAKKZ
                   CLASF = GS_OUTTAB-CLASF
                   ABGSL = GS_OUTTAB-ABGSL
                   ZZSCT = GS_OUTTAB-ZZSCT
                   ZZPHA = GS_OUTTAB-ZZPHA
                   ZZWBT = GS_OUTTAB-ZZWBT
                   ZZBGU = GS_OUTTAB-ZZBGU
                   ZZBGD = GS_OUTTAB-ZZBGD
                   ZZPRG = GS_OUTTAB-ZZPRG
                   ZZADT = GS_OUTTAB-ZZADT
                   ZZHWB = GS_OUTTAB-ZZHWB
                   ZZBAG = GS_OUTTAB-ZZBAG
                   ZZIVC = GS_OUTTAB-ZZIVC
                   ZZCOP = GS_OUTTAB-ZZCOP
                   ZZCYP = GS_OUTTAB-ZZCYP
             WHERE POSID = GS_OUTTAB-POSID.
          ENDIF.

          IF SY-SUBRC EQ 0.
            GS_OUTTAB-ICON = ICON_GREEN_LIGHT.
*            GS_OUTTAB-MESSAGE = 'PROJECT-WBS 업데이트 성공'.

            IF GV_PROJ = 'X'.
              LV_MSG = 'PROJECT-'.
            ENDIF.
            IF GV_WBS = 'X'.
              CONCATENATE LV_MSG 'WBS' INTO LV_MSG.
            ENDIF.
            CONCATENATE LV_MSG '업데이트 성공' INTO GS_OUTTAB-MESSAGE.

            MODIFY GT_OUTTAB FROM GS_OUTTAB.
            CLEAR  GS_OUTTAB.

          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

            GS_OUTTAB-ICON = ICON_RED_LIGHT.
*            GS_OUTTAB-MESSAGE = 'PROJECT-WBS 업데이트 오류'.

            IF GV_PROJ = 'X'.
              LV_MSG = 'PROJECT-'.
            ENDIF.
            IF GV_WBS = 'X'.
              CONCATENATE LV_MSG 'WBS' INTO LV_MSG.
            ENDIF.
            CONCATENATE LV_MSG '업데이트 오류' INTO GS_OUTTAB-MESSAGE.

            MODIFY GT_OUTTAB FROM GS_OUTTAB.
            CLEAR  GS_OUTTAB.
          ENDIF.
        ENDLOOP.

      ELSE.
        READ TABLE GT_MSGTAB WITH KEY MESSAGE_TYPE = GC_E.

        LOOP AT GT_OUTTAB INTO GS_OUTTAB WHERE PSPID = LS_DATA-PSPID.
          GS_OUTTAB-ICON    = ICON_RED_LIGHT.
          GS_OUTTAB-MESSAGE = GT_MSGTAB-MESSAGE_TEXT.

          MODIFY GT_OUTTAB FROM GS_OUTTAB.
          CLEAR  GS_OUTTAB.
        ENDLOOP.
      ENDIF.
    ENDAT.
  ENDLOOP.

  IF LV_ERR = 'X'.
    READ TABLE GT_MSGTAB WITH KEY MESSAGE_TYPE = GC_E.

    LOOP AT GT_OUTTAB INTO GS_OUTTAB WHERE PSPID = LS_DATA-PSPID.
      GS_OUTTAB-ICON    = ICON_RED_LIGHT.

      IF GT_MSGTAB-MESSAGE_TEXT IS NOT INITIAL.
        GS_OUTTAB-MESSAGE = GT_MSGTAB-MESSAGE_TEXT.
      ELSE.
        GS_OUTTAB-MESSAGE = GS_RETURN-MESSAGE.
      ENDIF.

      MODIFY GT_OUTTAB FROM GS_OUTTAB.
      CLEAR  GS_OUTTAB.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_DATA_PROJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CHECK_INPUT_DATA_PROJ USING PS_PROJ STRUCTURE BAPI_PROJECT_DEFINITION
                                 PV_CHG
                        CHANGING PS_RET STRUCTURE BAPIRETURN1.

* 처리 할 데이터가 없음.
  IF PS_PROJ IS INITIAL.
    PS_RET-TYPE    = GC_E.
    PS_RET-MESSAGE = TEXT-M02. "데이터를 입력하십시오.
    RETURN.
  ELSE.
    CHECK PV_CHG IS INITIAL.
    "필수값 체크(디스크립션은 BAPI에서 필수 체크 안함).
    IF PS_PROJ-DESCRIPTION IS INITIAL.
      PS_RET-TYPE    = GC_E.
      PS_RET-MESSAGE = TEXT-M03. "Description을 입력하십시오.
      RETURN.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_INPUT_DATA_PROJ
*&---------------------------------------------------------------------*
*&      Form  IMPORTING_PROJ_CRE_DATA_PROCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM IMPORTING_PROJ_CRE_DATA_PROCC USING P_METHOD
                                         PS_DATA STRUCTURE GS_OUTTAB.
* PROJECT
* I_METHOD_PROJECT
  CLEAR GT_METHPROJ.
  GT_METHPROJ-REFNUMBER  = GC_REFNO.                    "순번: '000001'
  GT_METHPROJ-OBJECTTYPE = CO1_TYPE-PROJECT_DEFINITION. "TEXT: 'PROJECTDEFINITION'
  GT_METHPROJ-METHOD     = P_METHOD.                    "TEXT: 'CREATE'
  GT_METHPROJ-OBJECTKEY  = PS_DATA-PSPID.               "외부 번호

  APPEND GT_METHPROJ.

* 마지막 라인에 METHOD'SAVE' 추가해야 반영됨.
  CLEAR GT_METHPROJ.
  GT_METHPROJ-METHOD     = CO1_METH-SAVE.               "TEXT: 'SAVE'

  APPEND GT_METHPROJ.
ENDFORM.                    " IMPORTING_PROJ_CRE_DATA_PROCC
*&---------------------------------------------------------------------*
*& Form IMPORTING_WBS_CRE_DATA_PROCC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_GUBUN
*&      --> GS_PROJ
*&      --> LS_DATA
*&      --> LV_REFNO
*&---------------------------------------------------------------------*
FORM IMPORTING_WBS_CRE_DATA_PROCC  USING P_METHOD
                                         PS_DATA STRUCTURE GS_OUTTAB
                                         PS_FLAG STRUCTURE GS_FLAG
                                         P_REFNO.
* PROJECT
* I_METHOD_PROJECT
  CLEAR GT_METHPROJ.
  GT_METHPROJ-REFNUMBER  = P_REFNO.                     "순번: '000001'
  GT_METHPROJ-OBJECTTYPE = CO1_TYPE-WBS_ELEMENT.        "TEXT: 'WBS_ELEMENT'
  GT_METHPROJ-METHOD     = P_METHOD.                    "TEXT: 'CREATE' / 'UPDATE'
  GT_METHPROJ-OBJECTKEY  = PS_DATA-POSID.               "외부 번호

  APPEND GT_METHPROJ.

* WBS
* I_WBS_ELEMENT_TABLE
  CLEAR GT_WBS_TAB.
  GT_WBS_TAB-WBS_ELEMENT                    = PS_DATA-POSID.
  GT_WBS_TAB-PROJECT_DEFINITION             = PS_DATA-PSPID.
  GT_WBS_TAB-DESCRIPTION                    = PS_DATA-WBSPOST1.
  GT_WBS_TAB-SHORT_ID                       = PS_DATA-POSID.
  GT_WBS_TAB-COMP_CODE                      = PS_DATA-PBUKR.
  GT_WBS_TAB-BUS_AREA                       = PS_DATA-PGSBR.
  GT_WBS_TAB-PROFIT_CTR                     = PS_DATA-PPRCTR.
  GT_WBS_TAB-PLANT                          = PS_DATA-WERKS.
  GT_WBS_TAB-WBS_BASIC_START_DATE           = PS_DATA-PSTRT.
  GT_WBS_TAB-WBS_BASIC_FINISH_DATE          = PS_DATA-PENDE.
*  GT_WBS_TAB-USER_FIELD_CHAR20_1            = PS_DATA-ZZSCT.
*  GT_WBS_TAB-USER_FIELD_CHAR20_2            = PS_DATA-ZZPHA.
*  GT_WBS_TAB-USER_FIELD_CHAR10_1            = PS_DATA-ZZWBT.
*  GT_WBS_TAB-USER_FIELD_CHAR10_2            = PS_DATA-ZZBGU.
*  GT_WBS_TAB-WBS_PLANNING_ELEMENT           = PS_DATA-PLAKZ.
*  GT_WBS_TAB-WBS_ACCOUNT_ASSIGNMENT_ELEMENT = PS_DATA-BELKZ.
*  GT_WBS_TAB-WBS_BILLING_ELEMENT            = PS_DATA-FAKKZ.

  APPEND GT_WBS_TAB.

  IF P_METHOD = 'UPDATE'.
    CLEAR  GT_WBS_TAB_UP.
    IF PS_FLAG-WBSPOST1 = 'X'.
      GT_WBS_TAB_UP-DESCRIPTION = 'X'.
    ENDIF.
    IF PS_FLAG-PBUKR = 'X'.
      GT_WBS_TAB_UP-COMP_CODE = 'X'.
    ENDIF.
    IF PS_FLAG-PPRCTR = 'X'.
      GT_WBS_TAB_UP-PROFIT_CTR = 'X'.
    ENDIF.
    IF PS_FLAG-PGSBR = 'X'.
      GT_WBS_TAB_UP-BUS_AREA = 'X'.
    ENDIF.
    IF PS_FLAG-WERKS = 'X'.
      GT_WBS_TAB_UP-PLANT = 'X'.
    ENDIF.
    IF PS_FLAG-PSTRT = 'X'.
      GT_WBS_TAB_UP-WBS_BASIC_START_DATE = 'X'.
    ENDIF.
    IF PS_FLAG-PENDE = 'X'.
      GT_WBS_TAB_UP-WBS_BASIC_FINISH_DATE = 'X'.
    ENDIF.

    APPEND GT_WBS_TAB_UP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXECUTE_BAPI_TRANSACTION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM EXECUTE_BAPI_TRANSACTION  TABLES PT_METHPROJ
                                      PT_WBS_TAB_UP
                                      PT_WBS_TAB
                                      PT_MSGTAB
                                USING PS_PROJ
                                      PS_PROJDEF_UP
                             CHANGING PS_RETURN.

* 프로젝트 생성 위한 BAPI 펑션 호출
  CALL FUNCTION 'BAPI_PS_INITIALIZATION'.

  CLEAR PT_MSGTAB[].
  CALL FUNCTION 'BAPI_PROJECT_MAINTAIN'
    EXPORTING
      I_PROJECT_DEFINITION       = PS_PROJ
      I_PROJECT_DEFINITION_UPD   = PS_PROJDEF_UP
    IMPORTING
      RETURN                     = PS_RETURN
    TABLES
      I_METHOD_PROJECT           = PT_METHPROJ[]
      I_WBS_ELEMENT_TABLE_UPDATE = PT_WBS_TAB_UP[]
      I_WBS_ELEMENT_TABLE        = PT_WBS_TAB[]
      E_MESSAGE_TABLE            = PT_MSGTAB[].

* 결과처리
  PERFORM BAPI_TRANSACTION_COMMIT CHANGING PS_RETURN.

  WAIT UP TO '0.5' SECONDS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BAPI_TRANSACTION_COMMIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BAPI_TRANSACTION_COMMIT CHANGING PS_RETURN STRUCTURE BAPIRETURN1.
  DATA: LT_RET TYPE TABLE OF BAPIRET2 WITH HEADER LINE.

  IF PS_RETURN-TYPE = GC_E.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  ELSE.
    REFRESH LT_RET.
    CALL FUNCTION 'BAPI_PS_PRECOMMIT'
      TABLES
        ET_RETURN = LT_RET.

    SORT LT_RET BY TYPE.
    CLEAR LT_RET.
    READ TABLE LT_RET WITH KEY TYPE = GC_E
                      BINARY SEARCH.
    IF SY-SUBRC <> 0.
      PS_RETURN-TYPE = GC_S.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = CO1_X.
    ELSE.
      CLEAR PS_RETURN.
      MOVE-CORRESPONDING LT_RET TO PS_RETURN.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.
  ENDIF.
ENDFORM. " BAPI_TRANSACTION_COMMIT

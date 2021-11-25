*&---------------------------------------------------------------------*
*& Include          ZCOR0110F01
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
   WHERE A~VERSI = 'B1'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM SET_SCREEN .

  LOOP AT SCREEN.

    IF SCREEN-NAME = 'PA_KOKRS' OR
       SCREEN-NAME = 'PA_VERSN'.

      SCREEN-INPUT = 0.
      MODIFY SCREEN.

    ENDIF.

    CASE ABAP_TRUE.
      WHEN PA_RAD1.
        IF SCREEN-GROUP1 = 'WBS' OR
            SCREEN-GROUP1 = 'G2'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN PA_RAD2.
        IF SCREEN-GROUP1 = 'KOS' OR
            SCREEN-GROUP1 = 'G1'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_B1
*&---------------------------------------------------------------------*
FORM CHECK_B1.

  IF PA_SPERL > PA_EPERL AND
      PA_EPERL IS NOT INITIAL.
    SET CURSOR FIELD 'PA_SPERL'.
    MESSAGE E018  WITH TEXT-E03.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATA_GET
*&---------------------------------------------------------------------*
FORM DATA_GET .

  DATA LV_MONTH TYPE N LENGTH 2.

  DATA: LV_FIELDNAME TYPE FIELDNAME.

  DATA LV_FYEAR TYPE JAHRPERBL.

  FIELD-SYMBOLS: <FS_HSL>   TYPE ANY,
                 <FS_SUM>   TYPE ANY,
                 <FS_ALL>   TYPE ANY,
                 <FS_WKG>   TYPE ANY,
                 <FS_VALUE> TYPE ANY.

**ADD BSGSM_FCM  2021.09.01..


  DATA : BEGIN OF LT_ZCOT0010  OCCURS 0,
           BUKRS  TYPE ZCOT0010-BUKRS,
           FKSTAR TYPE ZCOT0010-FKSTAR,
           CTYPE  TYPE ZCOT0010-CTYPE,
           CPERD  TYPE ZCOT0010-CPERD,
           CPTXT  TYPE DDTEXT,
         END OF LT_ZCOT0010,
         LS_ZCOT0010 LIKE LINE OF LT_ZCOT0010..

  DATA : BEGIN OF LT_ZCOT0010B  OCCURS 0,
           FKSTAR TYPE ZCOT0010-FKSTAR,
           CTYPE  TYPE ZCOT0010-CTYPE,
           CPERD  TYPE ZCOT0010-CPERD,
           CPTXT  TYPE DDTEXT,
         END OF LT_ZCOT0010B,
         LS_ZCOT0010B LIKE LINE OF LT_ZCOT0010B.

**END BY SGSM_FCM


  CLEAR: GT_OUTTAB, GS_OUTTAB.

  SELECT A~KOSTL, B~KTEXT,
         A~BUKRS " ADD BSGSM_FCM
    INTO TABLE @GT_KOSTL
    FROM CSKS AS A
    LEFT JOIN CSKT AS B
      ON A~KOKRS = B~KOKRS
     AND A~KOSTL = B~KOSTL
     AND A~DATBI = B~DATBI
     AND B~SPRAS = @SY-LANGU
   WHERE A~KOKRS = @PA_KOKRS.




**  SELECT DISTINCT
**         A~FKSTAR, A~CTYPE, A~CPERD , B~DDTEXT AS CPTXT
**    FROM ZCOT0010 AS A
**    LEFT JOIN DD07V AS B
**      ON A~CPERD      = B~DOMVALUE_L
**     AND B~DOMNAME    = 'ZDCPERD'
**     AND B~DDLANGUAGE = @SY-LANGU
**    INTO TABLE @DATA(LT_ZCOT0010)
**   WHERE A~GJAHR = @PA_GJAHR
**     AND A~KOKRS = @PA_KOKRS.


***MODI BY BSGSM_FCM   2021.09.01..

*관리회계영역레벨  통제
  SELECT DISTINCT
         A~FKSTAR, A~CTYPE, A~CPERD , B~DDTEXT AS CPTXT
    FROM ZCOT0010B AS A
    LEFT JOIN DD07V AS B
      ON A~CPERD      = B~DOMVALUE_L
     AND B~DOMNAME    = 'ZDCPERD'
     AND B~DDLANGUAGE = @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE  @LT_ZCOT0010B
   WHERE A~GJAHR = @PA_GJAHR
     AND A~KOKRS = @PA_KOKRS.


*회사코드레벨 통제
  SELECT DISTINCT
        A~BUKRS AS BUKRS , "add bsgsm_fcm  2021.09.01
         A~FKSTAR, A~CTYPE, A~CPERD , B~DDTEXT AS CPTXT
    FROM ZCOT0010 AS A
    LEFT JOIN DD07V AS B
      ON A~CPERD      = B~DOMVALUE_L
     AND B~DOMNAME    = 'ZDCPERD'
     AND B~DDLANGUAGE = @SY-LANGU
     INTO CORRESPONDING FIELDS OF TABLE  @LT_ZCOT0010
*     INTO TABLE @DATA(lt_zcot0010)
   WHERE A~GJAHR = @PA_GJAHR
     AND A~KOKRS = @PA_KOKRS.


  SELECT BUKRS , FLAG
    FROM ZCOT000
   WHERE FLAG = 'X'
    INTO TABLE @DATA(LT_000). " 통제여부

** END BY BSGSM_FCM  <<<<<<<<<<<

*-- 계획
  SELECT A~ROBJNR, A~RKSTAR, B~KTEXT, A~PROCESS_9, A~RTCUR,
         SUM( A~HSL01 ) AS HSL01, SUM( A~HSL02 ) AS HSL02,
         SUM( A~HSL03 ) AS HSL03, SUM( A~HSL04 ) AS HSL04,
         SUM( A~HSL05 ) AS HSL05, SUM( A~HSL06 ) AS HSL06,
         SUM( A~HSL07 ) AS HSL07, SUM( A~HSL08 ) AS HSL08,
         SUM( A~HSL09 ) AS HSL09, SUM( A~HSL10 ) AS HSL10,
         SUM( A~HSL11 ) AS HSL11, SUM( A~HSL12 ) AS HSL12
    INTO TABLE @DATA(LT_ZCOT0040)
  FROM ZCOT0040 AS A
  LEFT JOIN CSKU AS B
    ON A~RKSTAR = B~KSTAR
   AND B~KTOPL = @GV_KTOPL
   AND B~SPRAS = @SY-LANGU
 WHERE A~RLDNR = '00'
   AND A~RRCTY = '1'
   AND A~RVERS = 'B1'
   AND A~RYEAR  = @PA_GJAHR
   AND A~ROBJNR IN @R_OBJNR
   AND A~RKOKRS = @PA_KOKRS
   AND A~RKSTAR IN @R_KSTAR
  GROUP BY A~ROBJNR, A~RKSTAR, B~KTEXT, A~PROCESS_9, A~RTCUR.

*-- 실적 ( D1~D4)
  SELECT A~WRTTP,A~OBJNR, A~KSTAR, B~KTEXT,
         SUM( A~WKG001 ) AS WKG001, SUM( A~WKG002 ) AS WKG002,
         SUM( A~WKG003 ) AS WKG003, SUM( A~WKG004 ) AS WKG004,
         SUM( A~WKG005 ) AS WKG005, SUM( A~WKG006 ) AS WKG006,
         SUM( A~WKG007 ) AS WKG007, SUM( A~WKG008 ) AS WKG008,
         SUM( A~WKG009 ) AS WKG009, SUM( A~WKG010 ) AS WKG010,
         SUM( A~WKG011 ) AS WKG011, SUM( A~WKG012 ) AS WKG012
    INTO TABLE @DATA(LT_COSP)
    FROM COSP AS A
    LEFT JOIN CSKU AS B
      ON A~KSTAR = B~KSTAR
     AND B~KTOPL = @GV_KTOPL
     AND B~SPRAS = @SY-LANGU
   WHERE A~LEDNR = '00'
     AND A~VERSN = '000'
     AND A~WRTTP IN ('04', '60', '21', '22')
     AND A~GJAHR = @PA_GJAHR
     AND A~KSTAR IN @R_KSTAR
     AND A~OBJNR IN @R_OBJNR
     AND A~VRGNG <> 'SDOR'
   GROUP BY A~WRTTP, A~OBJNR, A~KSTAR, B~KTEXT.

*-- F3
  SELECT OBJNR, KSTAR,
         SUM( WKG001 ) AS WKG001, SUM( WKG002 ) AS WKG002,
         SUM( WKG003 ) AS WKG003, SUM( WKG004 ) AS WKG004,
         SUM( WKG005 ) AS WKG005, SUM( WKG006 ) AS WKG006,
         SUM( WKG007 ) AS WKG007, SUM( WKG008 ) AS WKG008,
         SUM( WKG009 ) AS WKG009, SUM( WKG010 ) AS WKG010,
         SUM( WKG011 ) AS WKG011, SUM( WKG012 ) AS WKG012
    INTO TABLE @DATA(LT_COSP_F3)
    FROM COSP
   WHERE LEDNR = '00'
     AND VERSN = '000'
     AND WRTTP IN ('04', '60', '21', '22')
     AND GJAHR = @PA_GJAHR
     AND KSTAR IN @R_KSTAR
     AND OBJNR IN @R_OBJNR
     AND VRGNG = 'RKU1'
   GROUP BY OBJNR, KSTAR.

*-- F1/F2
  SELECT RACCT, OBJNR, BLART,
         SUM( HSL ) AS HSL
    INTO TABLE @DATA(LT_ACDOCA)
    FROM ACDOCA
   WHERE RLDNR = '0L'
     AND GJAHR = @PA_GJAHR
     AND KOKRS = @PA_KOKRS
     AND RACCT IN @R_KSTAR
     AND OBJNR IN @R_OBJNR
     AND BLART IN ('DD', 'SS')
     AND FISCYEARPER IN @R_FYEAR
   GROUP BY RACCT, OBJNR, BLART.

  CASE ABAP_TRUE.

    WHEN PA_RAD1.
      DELETE LT_ZCOT0040 WHERE ROBJNR NP 'KS*'.
      DELETE LT_COSP     WHERE OBJNR  NP 'KS*'.
      DELETE LT_COSP_F3  WHERE OBJNR  NP 'KS*'.
      DELETE LT_ACDOCA   WHERE OBJNR  NP 'KS*'.

    WHEN PA_RAD2.
      DELETE LT_ZCOT0040 WHERE ROBJNR NP 'PR*'.
      DELETE LT_COSP     WHERE OBJNR  NP 'PR*'.
      DELETE LT_COSP_F3  WHERE OBJNR  NP 'PR*'.
      DELETE LT_ACDOCA   WHERE OBJNR  NP 'PR*'.

  ENDCASE.

  LOOP AT LT_ZCOT0040 ASSIGNING FIELD-SYMBOL(<LS_ZCOT0040>).

    PERFORM READ_TEXT USING <LS_ZCOT0040>-ROBJNR
                      CHANGING GS_OUTTAB.

    MOVE: <LS_ZCOT0040>-RKSTAR TO GS_OUTTAB-KSTAR,
          <LS_ZCOT0040>-KTEXT  TO GS_OUTTAB-KSTXT,
          <LS_ZCOT0040>-RTCUR  TO GS_OUTTAB-WAERS,
          <LS_ZCOT0040>-ROBJNR TO GS_OUTTAB-OBJNR.

    CLEAR LV_MONTH.

    LOOP AT GT_MONTH.

      LV_FIELDNAME = '<LS_ZCOT0040>-HSL' && GT_MONTH-V1.

      ASSIGN (LV_FIELDNAME) TO <FS_HSL>.

      CASE <LS_ZCOT0040>-PROCESS_9.

        WHEN 'ENTR'.

          ASSIGN COMPONENT  <LS_ZCOT0040>-PROCESS_9
                 OF STRUCTURE GS_OUTTAB TO <FS_SUM>.

          <FS_SUM> = <FS_SUM> + <FS_HSL>.

      ENDCASE.

      ASSIGN COMPONENT 'ALL' OF STRUCTURE GS_OUTTAB TO <FS_ALL>.
      <FS_ALL> = <FS_ALL> + <FS_HSL>.

    ENDLOOP.

    COLLECT GS_OUTTAB INTO GT_OUTTAB.
    CLEAR   GS_OUTTAB.

  ENDLOOP.

  LOOP AT LT_COSP ASSIGNING FIELD-SYMBOL(<LS_COSP>).

    PERFORM READ_TEXT USING    <LS_COSP>-OBJNR
                      CHANGING GS_OUTTAB.

    MOVE: <LS_COSP>-KSTAR  TO GS_OUTTAB-KSTAR,
          <LS_COSP>-KTEXT  TO GS_OUTTAB-KSTXT,
          GV_WAERS         TO GS_OUTTAB-WAERS,
          <LS_COSP>-OBJNR  TO GS_OUTTAB-OBJNR.

    LOOP AT GT_MONTH.

      CASE <LS_COSP>-WRTTP.

        WHEN '04'.
          LV_FIELDNAME = '<LS_COSP>-WKG0' && GT_MONTH-V1.
          ASSIGN (LV_FIELDNAME) TO <FS_WKG>.

          ASSIGN COMPONENT 'D1HSL'  OF STRUCTURE GS_OUTTAB
                                    TO <FS_VALUE>.
          <FS_VALUE> = <FS_VALUE> + <FS_WKG>.

        WHEN '60'.
          LV_FIELDNAME = '<LS_COSP>-WKG0' && GT_MONTH-V1.
          ASSIGN (LV_FIELDNAME) TO <FS_WKG>.

          ASSIGN COMPONENT 'D2HSL'  OF STRUCTURE GS_OUTTAB
                                    TO <FS_VALUE>.
          <FS_VALUE> = <FS_VALUE> + <FS_WKG>.

        WHEN '22'.   "PO
          LV_FIELDNAME = '<LS_COSP>-WKG0' && GT_MONTH-V1.
          ASSIGN (LV_FIELDNAME) TO <FS_WKG>.

          ASSIGN COMPONENT 'D3HSL'  OF STRUCTURE GS_OUTTAB
                                    TO <FS_VALUE>.
          <FS_VALUE> = <FS_VALUE> + <FS_WKG>.

        WHEN '21'.   "PR
          LV_FIELDNAME = '<LS_COSP>-WKG0' && GT_MONTH-V1.
          ASSIGN (LV_FIELDNAME) TO <FS_WKG>.

          ASSIGN COMPONENT 'D4HSL'  OF STRUCTURE GS_OUTTAB
                                    TO <FS_VALUE>.
          <FS_VALUE> = <FS_VALUE> + <FS_WKG>.

      ENDCASE.

    ENDLOOP.

    COLLECT GS_OUTTAB INTO GT_OUTTAB.
    CLEAR   GS_OUTTAB.

  ENDLOOP.

  LOOP AT GT_OUTTAB INTO GS_OUTTAB.

    LOOP AT LT_COSP_F3 ASSIGNING  FIELD-SYMBOL(<LS_COSP_F3>)
                         WHERE OBJNR = GS_OUTTAB-OBJNR
                           AND KSTAR = GS_OUTTAB-KSTAR.

      LOOP AT GT_MONTH.

        LV_FIELDNAME = '<LS_COSP_F3>-WKG0' && GT_MONTH-V1.
        ASSIGN (LV_FIELDNAME) TO <FS_WKG>.

        ASSIGN COMPONENT 'F3HSL'  OF STRUCTURE GS_OUTTAB
                                            TO <FS_VALUE>.
        <FS_VALUE> = <FS_VALUE> + <FS_WKG>.

      ENDLOOP.

    ENDLOOP.

    READ TABLE  LT_ACDOCA ASSIGNING  FIELD-SYMBOL(<LS_ACDOCA_DD>)
                              WITH KEY RACCT  = GS_OUTTAB-KSTAR
                                       OBJNR  = GS_OUTTAB-OBJNR
                                       BLART  = 'DD'.

    IF SY-SUBRC = 0.

      ASSIGN COMPONENT 'F1HSL'  OF STRUCTURE GS_OUTTAB
       TO <FS_VALUE>.

      <FS_VALUE> = <FS_VALUE> + <LS_ACDOCA_DD>-HSL.

    ENDIF.

    READ TABLE  LT_ACDOCA ASSIGNING  FIELD-SYMBOL(<LS_ACDOCA_SS>)
                              WITH KEY RACCT  = GS_OUTTAB-KSTAR
                                       OBJNR  = GS_OUTTAB-OBJNR
                                       BLART  = 'SS'.

    IF SY-SUBRC = 0.

      ASSIGN COMPONENT 'F2HSL'  OF STRUCTURE GS_OUTTAB
       TO <FS_VALUE>.

      <FS_VALUE> = <FS_VALUE> + <LS_ACDOCA_SS>-HSL.

    ENDIF.

    GS_OUTTAB-USAGE = ( GS_OUTTAB-D1HSL +
                        GS_OUTTAB-D2HSL +
                        GS_OUTTAB-D3HSL +
                        GS_OUTTAB-D4HSL ) -
                      ( GS_OUTTAB-F1HSL +
                        GS_OUTTAB-F2HSL +
                        GS_OUTTAB-F3HSL  ).

    GS_OUTTAB-AVALT = GS_OUTTAB-ALL - GS_OUTTAB-USAGE.


**
**    IF gs_outtab-kostl IS NOT INITIAL.
**
**      READ TABLE lt_zcot0010 INTO DATA(ls_zcot0010)
**          WITH KEY fkstar = gs_outtab-kstar
**                   ctype   = '1'.
**
**      IF sy-subrc = 0.
**
**        MOVE: ls_zcot0010-cperd TO gs_outtab-cperd,
**              ls_zcot0010-cptxt TO gs_outtab-cptxt.
**
**      ENDIF.
**
**    ELSE.
**
**      READ TABLE gt_prps WITH KEY objnr = gs_outtab-objnr.
**
**      IF sy-subrc = 0.
**
**        READ TABLE lt_zcot0010 INTO ls_zcot0010
**            WITH KEY fkstar = gs_outtab-kstar
**                     ctype  = gt_prps-zzcyp.
**
**        IF sy-subrc = 0.
**
**          MOVE: ls_zcot0010-cperd TO gs_outtab-cperd,
**                ls_zcot0010-cptxt TO gs_outtab-cptxt.
**
**        ENDIF.
**
**      ENDIF.
**
**    ENDIF.

**MODI BY BSGSM_FCM 2021.09.01. >>>>>>>>>>>>>>>>

    IF GS_OUTTAB-KOSTL IS NOT INITIAL.
*CCTR 의 회사코드 찾기
      READ TABLE GT_KOSTL ASSIGNING FIELD-SYMBOL(<ZZ>)
                               WITH KEY  KOSTL = GS_OUTTAB-KOSTL.

      IF <ZZ> IS  ASSIGNED.
        READ TABLE LT_000 ASSIGNING FIELD-SYMBOL(<FS>) WITH KEY BUKRS = <ZZ>-BUKRS
                                                                FLAG = 'X'. " 회사별 통제
        IF <FS> IS  ASSIGNED AND SY-SUBRC EQ 0.
          CLEAR LS_ZCOT0010.
          READ TABLE LT_ZCOT0010 INTO  LS_ZCOT0010
                                 WITH KEY  bukrs = <zz>-bukrs
                                           FKSTAR = GS_OUTTAB-KSTAR
                                           CTYPE   = '1'.

          IF SY-SUBRC = 0.

            MOVE: LS_ZCOT0010-CPERD TO GS_OUTTAB-CPERD,
                  LS_ZCOT0010-CPTXT TO GS_OUTTAB-CPTXT.

          ENDIF.

        ELSE.   " 관리회계레벨 통제

          CLEAR LS_ZCOT0010B.
          READ TABLE LT_ZCOT0010B INTO LS_ZCOT0010B
                                 WITH KEY FKSTAR = GS_OUTTAB-KSTAR
                 CTYPE   = '1'.

          IF SY-SUBRC = 0.

            MOVE: LS_ZCOT0010B-CPERD TO GS_OUTTAB-CPERD,
                  LS_ZCOT0010B-CPTXT TO GS_OUTTAB-CPTXT.

          ENDIF.
        ENDIF.

      ENDIF.


    ELSE.

*WBS 회사코드 찾기

      READ TABLE GT_PRPS ASSIGNING FIELD-SYMBOL(<PS>) WITH KEY OBJNR = GS_OUTTAB-OBJNR.

      IF <PS> IS ASSIGNED AND SY-SUBRC EQ 0.

        READ TABLE LT_000 ASSIGNING FIELD-SYMBOL(<FS2>) WITH KEY BUKRS = <PS>-BUKRS
                                                                         FLAG = 'X'. " 회사별 통제
        IF <FS2> IS  ASSIGNED AND SY-SUBRC EQ 0.
          CLEAR LS_ZCOT0010.
          READ TABLE LT_ZCOT0010 INTO LS_ZCOT0010
                       WITH KEY bukrs = <ps>-bukrs
                                FKSTAR = GS_OUTTAB-KSTAR
                                CTYPE  = GT_PRPS-ZZCYP.

          IF SY-SUBRC = 0.

            MOVE: LS_ZCOT0010-CPERD TO GS_OUTTAB-CPERD,
                  LS_ZCOT0010-CPTXT TO GS_OUTTAB-CPTXT.

          ENDIF.

        ELSE.
          CLEAR LS_ZCOT0010B.
          READ TABLE LT_ZCOT0010B INTO LS_ZCOT0010B
                     WITH KEY FKSTAR = GS_OUTTAB-KSTAR
                              CTYPE  = GT_PRPS-ZZCYP.

          IF SY-SUBRC = 0.

            MOVE: LS_ZCOT0010B-CPERD TO GS_OUTTAB-CPERD,
                  LS_ZCOT0010B-CPTXT TO GS_OUTTAB-CPTXT.

          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.


**END BY BSGSM_FCM <<<<<<<<<<<<<<<

    MODIFY GT_OUTTAB FROM GS_OUTTAB.

  ENDLOOP.

  DELETE GT_OUTTAB WHERE ENTR  = 0
                     AND ALL   = 0
                     AND USAGE = 0
                     AND AVALT = 0
                     AND D1HSL = 0
                     AND D2HSL = 0
                     AND D3HSL = 0
                     AND D4HSL = 0
                     AND F1HSL = 0
                     AND F2HSL = 0
                     AND F3HSL = 0.

  IF GT_OUTTAB[] IS INITIAL.
    MESSAGE S004 DISPLAY LIKE 'E'.
    STOP.
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

  DATA: L_TITLE TYPE LVC_TITLE,
        L_LINES TYPE C LENGTH 100,
        L_COUNT TYPE I.

  DATA LV_TEXT TYPE CHAR100.
  DATA LV_GUBUN  TYPE CHAR40.


  GO_ALV->SET_SCREEN_STATUS(
    PFSTATUS      =  'STANDARD'
    REPORT        =  GV_REPID
    SET_FUNCTIONS = GO_ALV->C_FUNCTIONS_ALL ).

  L_COUNT = LINES( GT_OUTTAB ).

  CASE ABAP_TRUE.

    WHEN PA_RAD1.
      CONCATENATE TEXT-T00  ':' TEXT-005
            INTO LV_TEXT SEPARATED BY SPACE.

    WHEN PA_RAD2.
      CONCATENATE TEXT-T00  ':' TEXT-006
            INTO LV_TEXT SEPARATED BY SPACE.

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
  DATA: LO_SORT     TYPE REF TO CL_SALV_SORTS.

*-- set column
  TRY.
      LR_COLUMNS = GO_ALV->GET_COLUMNS( ).
      LO_AGGRS   = GO_ALV->GET_AGGREGATIONS( ).
      LO_SORT = GO_ALV->GET_SORTS( ).

*      LR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).
      LR_COLUMNS->SET_CELL_TYPE_COLUMN( 'CELLTYPE' ).
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

        IF ( <COLUMN_REF>-COLUMNNAME = 'ENTR' OR
            <COLUMN_REF>-COLUMNNAME = 'ALL'   OR
            <COLUMN_REF>-COLUMNNAME = 'USAGE' OR
            <COLUMN_REF>-COLUMNNAME = 'AVALT' OR
            <COLUMN_REF>-COLUMNNAME CP 'D*HSL'  OR
            <COLUMN_REF>-COLUMNNAME CP 'F*HSL' ).

          CALL METHOD IR_AGG->ADD_AGGREGATION
            EXPORTING
              COLUMNNAME  = <COLUMN_REF>-COLUMNNAME
              AGGREGATION = IF_SALV_C_AGGREGATION=>TOTAL.

        ELSE.

          IF <COLUMN_REF>-COLUMNNAME = 'CPERD' OR
            <COLUMN_REF>-COLUMNNAME  = 'CPTXT'.

          ELSE.

            CALL METHOD IR_SORT->ADD_SORT
              EXPORTING
                COLUMNNAME = <COLUMN_REF>-COLUMNNAME
                SUBTOTAL   = IF_SALV_C_BOOL_SAP=>FALSE.

          ENDIF.

        ENDIF.

      ENDLOOP.

    CATCH CX_SALV_NOT_FOUND.
    CATCH CX_SALV_DATA_ERROR.
    CATCH CX_SALV_EXISTING.

  ENDTRY.

  CASE ABAP_TRUE.

    WHEN PA_RAD1.

      TRY.
          LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'POSID' ).
          LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
          LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'POST1' ).
          LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
          LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'OBJNR' ).
          LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
        CATCH CX_SALV_NOT_FOUND.
      ENDTRY.

    WHEN PA_RAD2.

      TRY.
          LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'KOSTL' ).
          LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
          LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'KTEXT' ).
          LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
          LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'OBJNR' ).
          LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
        CATCH CX_SALV_NOT_FOUND.
      ENDTRY.

  ENDCASE.

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

    WHEN '&RESH'.
      PERFORM DATA_GET.
      CALL METHOD GO_ALV->REFRESH
        EXPORTING
          REFRESH_MODE = IF_SALV_C_REFRESH=>FULL.

      MESSAGE S040.

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

        WHEN 'KSTAR'.
          P_COLUMN_TEXT = TEXT-C05.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'KSTXT'.
          P_COLUMN_TEXT = TEXT-C06.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'CPERD'.
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 4 ).
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'CPTXT'.
          P_COLUMN_TEXT = TEXT-C19.
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 10 ).
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'WAERS'.
          P_COLUMN_TEXT = TEXT-C07.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'ENTR'.
          P_COLUMN_TEXT = TEXT-C08.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '3' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'ALL'.
          P_COLUMN_TEXT = TEXT-C09.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '3' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'USAGE'.
          P_COLUMN_TEXT = TEXT-C10.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '5' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'AVALT'.
          P_COLUMN_TEXT = TEXT-C11.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '5' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'D1HSL'.
          P_COLUMN_TEXT = TEXT-C18.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '1' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'D2HSL'.
          P_COLUMN_TEXT = TEXT-C12.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '1' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'D3HSL'.
          P_COLUMN_TEXT = TEXT-C13.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '1' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'D4HSL'.
          P_COLUMN_TEXT = TEXT-C14.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '1' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'F1HSL'.
          P_COLUMN_TEXT = TEXT-C15.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '6' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'F2HSL'.
          P_COLUMN_TEXT = TEXT-C16.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '6' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'F3HSL'.
          P_COLUMN_TEXT = TEXT-C17.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '6' '0' '0'.
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
*& Form F4_KAGRU
*&---------------------------------------------------------------------*
FORM F4_KAGRU  CHANGING P_KAGRU.

  DATA LV_KAGRU TYPE KAGRU.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
      CLASS         = '0102'
      FIELD_NAME    = 'KSTAR'
      KOKRS         = PA_KOKRS
      KTOPL         = GC_KTOPL
    IMPORTING
      SET_NAME      = LV_KAGRU
    EXCEPTIONS
      NO_SET_PICKED = 1
      OTHERS        = 2.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

  IF LV_KAGRU IS NOT INITIAL.
    P_KAGRU  = LV_KAGRU.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_KSGRU
*&---------------------------------------------------------------------*
FORM F4_KSGRU  CHANGING P_KSGRU.

  DATA: HELP_SETNR     LIKE RGSMH-SETNR,
        HELP_SEARCHFLD LIKE RGSMH-SEARCHFLD,
        HELP_SET       LIKE RGSBS-SETNR,
        HELP_SETCLASS  LIKE RGSMH-CLASS.

  MOVE PA_KOKRS TO HELP_SEARCHFLD.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
      CLASS           = '0101'
      FIELD_NAME      = 'KOSTL'
      SEARCHFLD       = HELP_SEARCHFLD
      SEARCHFLD_INPUT = ' '
      SET             = HELP_SET
    IMPORTING
      SET_NAME        = HELP_SETNR
    EXCEPTIONS
      NO_SET_PICKED   = 1.

  IF SY-SUBRC = 0.
    P_KSGRU = HELP_SETNR.
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

  MOVE PA_KOKRS TO HELP_SEARCHFLD.

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
*&---------------------------------------------------------------------*
*& Form CHECK_B2
*&---------------------------------------------------------------------*
FORM CHECK_B2 .

  DATA LS_RETURN TYPE BAPIRET2.

  DATA LS_SETLEAF TYPE SETLEAF.

  CASE ABAP_TRUE.

    WHEN PA_RAD1.

      IF SO_KOSTL[] IS NOT INITIAL AND
         PA_KSGRU IS NOT INITIAL.
        SET CURSOR FIELD 'SO_KOSTL-LOW'.
        MESSAGE E000  WITH TEXT-E01.
      ENDIF.

      IF SO_KOSTL[] IS NOT INITIAL.

        SELECT SINGLE * FROM CSKS
          INTO @DATA(LS_CSKS)
         WHERE KOKRS = @PA_KOKRS
           AND KOSTL IN @SO_KOSTL
           AND DATBI >= @SY-DATUM
           AND DATAB <= @SY-DATUM.

        IF SY-SUBRC <> 0.
          SET CURSOR FIELD 'SO_KOSTL-LOW'.
          MESSAGE E027  WITH TEXT-E05.
        ENDIF.

      ENDIF.

      IF PA_KSGRU IS NOT INITIAL.

        PERFORM GET_CHECK_GROUP USING '0101'
                                      PA_KSGRU
                                CHANGING LS_RETURN.
        IF LS_RETURN-TYPE = 'E'.
          SET CURSOR FIELD 'PA_KSGRU'.
          MESSAGE E027  WITH TEXT-E06.
        ENDIF.

      ENDIF.

    WHEN PA_RAD2.

      IF SO_POSID[] IS NOT INITIAL AND
         PA_PDGR IS NOT INITIAL.
        SET CURSOR FIELD 'SO_POSID-LOW'.
        MESSAGE E026  WITH TEXT-E04.
      ENDIF.

      IF SO_POSID[] IS NOT INITIAL.

        SELECT SINGLE * FROM PRPS
          INTO @DATA(LS_PRPS)
         WHERE POSID IN @SO_POSID.

        IF SY-SUBRC <> 0.
          SET CURSOR FIELD 'SO_POSID-LOW'.
          MESSAGE E027  WITH TEXT-E07.
        ENDIF.

      ENDIF.

      IF PA_PDGR IS NOT INITIAL.

        PERFORM GET_CHECK_GROUP USING '0110'
                                      PA_PDGR
                                CHANGING LS_RETURN.

        IF LS_RETURN-TYPE = 'E'.
          SET CURSOR FIELD 'PA_PDGR'.
          MESSAGE E027  WITH TEXT-E08.
        ENDIF.

      ENDIF.

  ENDCASE.

  IF SO_KSTAR[] IS NOT INITIAL AND
     PA_KAGRU IS NOT INITIAL.

    SET CURSOR FIELD 'SO_KSTAR-LOW'.
    MESSAGE E026  WITH TEXT-E02.

  ENDIF.

  IF SO_KSTAR[] IS NOT INITIAL.

    SELECT SINGLE * FROM CSKA
      INTO @DATA(LS_CSKA)
     WHERE KTOPL = @GV_KTOPL
       AND KSTAR IN @SO_KSTAR.

    IF SY-SUBRC <> 0.
      SET CURSOR FIELD 'SO_KSTAR-LOW'.
      MESSAGE E027  WITH TEXT-E09.
    ENDIF.

  ENDIF.

  IF PA_KAGRU IS NOT INITIAL.

    PERFORM GET_CHECK_GROUP USING '0102'
                                  PA_KAGRU
                            CHANGING LS_RETURN.


    IF LS_RETURN-TYPE = 'E'.
      SET CURSOR FIELD 'PA_KAGRU'.
      MESSAGE E027  WITH TEXT-E10.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_HIERARCHY_TABLES
*&---------------------------------------------------------------------*
FORM READ_HIERARCHY_TABLES TABLES PT_VALUES STRUCTURE GRPVALUES
                            USING PV_CLASS TYPE SETCLASS
                                  PV_SETID.

  DATA: LV_SETID     LIKE SETHIER-SETID,
        LV_OVERWRITE LIKE SY-DATAR,
        LT_INFO      LIKE GRPHINFO OCCURS 0 WITH HEADER LINE,
        LT_NODES     LIKE GRPOBJECTS OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'G_SET_ENCRYPT_SETID'
    EXPORTING
      SETCLASS  = PV_CLASS
      SHORTNAME = PV_SETID  "코스트센터그룹
    IMPORTING
      SETID     = LV_SETID.

  IF SY-SUBRC = 0.

    CALL FUNCTION 'K_HIERARCHY_TABLES_READ'
      EXPORTING
        E_CLASS                     = PV_CLASS
        E_SETID                     = LV_SETID
        E_KOKRS                     = PA_KOKRS
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
*& Form SET_RANGES_OBJNR
*&---------------------------------------------------------------------*
FORM SET_RANGES_OBJNR .

  DATA: LV_SMON TYPE N LENGTH 2,
        LV_EMON TYPE N LENGTH 2,
        LV_MON  TYPE N LENGTH 2.

  CLEAR: GT_VALUES, GT_VALUES[],
         R_OBJNR,   R_OBJNR[],
         R_KSTAR,   R_KSTAR[],
         R_FYEAR,   R_FYEAR[],
         GT_MONTH,  GT_MONTH[].

  RANGES R_KOSTL FOR CSKS-KOSTL.
  RANGES R_POSID FOR PRPS-POSID.

*-- 오브젝트 / 회계연도
  CASE ABAP_TRUE.

    WHEN PA_RAD1.

      IF SO_KOSTL[] IS NOT INITIAL.

        R_KOSTL[] = SO_KOSTL[].

      ENDIF.

      IF PA_KSGRU IS NOT INITIAL.

        PERFORM READ_HIERARCHY_TABLES TABLES GT_VALUES
                                      USING '0101'
                                            PA_KSGRU.  "코스트센터 그룹

        LOOP AT GT_VALUES.

          MOVE: 'I'             TO R_KOSTL-SIGN,
                'BT'            TO R_KOSTL-OPTION,
                GT_VALUES-VFROM TO R_KOSTL-LOW,
                GT_VALUES-VTO   TO R_KOSTL-HIGH.

          COLLECT R_KOSTL.
          CLEAR   R_KOSTL.

        ENDLOOP.

      ENDIF.

      SELECT KOSTL FROM CSKS
        INTO TABLE @DATA(LT_CSKS)
       WHERE KOSTL IN @R_KOSTL
         AND KOKRS = @PA_KOKRS
         AND PRCTR IN @R_PRCTR2
         AND BUKRS IN @R_BUKRS2
         AND DATBI >= @SY-DATUM
         AND DATAB <= @SY-DATUM.

      LOOP AT LT_CSKS INTO DATA(LS_CSKS).

        MOVE: 'I'  TO R_OBJNR-SIGN,
              'EQ' TO R_OBJNR-OPTION.

        R_OBJNR-LOW  = 'KS' && PA_KOKRS && LS_CSKS-KOSTL.

        COLLECT R_OBJNR.
        CLEAR   R_OBJNR.

      ENDLOOP.

    WHEN PA_RAD2.

      IF SO_POSID[] IS NOT INITIAL.
        R_POSID[] = SO_POSID[].
      ENDIF.

      IF PA_PDGR IS NOT INITIAL.

        PERFORM READ_HIERARCHY_TABLES TABLES GT_VALUES
                                      USING '0110'
                                            PA_PDGR.   "WBS 요소 그룹

****        LOOP AT GT_VALUES.
****
****          MOVE: 'I'   TO R_POSID-SIGN,
****                'BT'  TO R_POSID-OPTION.
****
****          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
****            EXPORTING
****              INPUT     = GT_VALUES-VFROM
****            IMPORTING
****              OUTPUT    = R_POSID-LOW
****            EXCEPTIONS
****              NOT_FOUND = 1
****              OTHERS    = 2.
****
****          R_POSID-LOW =    GT_VALUES-VFROM .
****
****          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
****            EXPORTING
****              INPUT     = GT_VALUES-VTO
****            IMPORTING
****              OUTPUT    = R_POSID-HIGH
****            EXCEPTIONS
****              NOT_FOUND = 1
****              OTHERS    = 2.
****
****          R_POSID-HIGH = GT_VALUES-VTO.
****
****
****
****          COLLECT R_POSID.
****          CLEAR   R_POSID.
****
****
****        ENDLOOP.

*MODI BY JSY 20200207  REQUEST BY 강현수K...
*WBS 그룹으로 조회가 안되서 수정함
        CLEAR R_OBJNR. REFRESH R_OBJNR.
        CLEAR R_POSID. REFRESH R_POSID.

        LOOP AT GT_VALUES.

          MOVE: 'I'   TO R_OBJNR-SIGN,
                'BT'  TO R_OBJNR-OPTION.

          MOVE: 'I'   TO R_POSID-SIGN,
                'BT'  TO R_POSID-OPTION.

          MOVE : GT_VALUES-VFROM  TO R_POSID-LOW,
                 GT_VALUES-VTO   TO R_POSID-HIGH.

          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
            EXPORTING
              INPUT     = GT_VALUES-VFROM
            IMPORTING
              OUTPUT    = R_OBJNR-LOW
            EXCEPTIONS
              NOT_FOUND = 1
              OTHERS    = 2.


          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
            EXPORTING
              INPUT     = GT_VALUES-VTO
            IMPORTING
              OUTPUT    = R_OBJNR-HIGH
            EXCEPTIONS
              NOT_FOUND = 1
              OTHERS    = 2.

          R_OBJNR-LOW  = 'PR' && R_OBJNR-LOW.
          R_OBJNR-HIGH  = 'PR' && R_OBJNR-HIGH.

          COLLECT R_OBJNR.
          CLEAR   R_OBJNR.

        ENDLOOP.

*END BY JSY 20200207

      ENDIF.

      SELECT A~OBJNR, A~PSPNR, A~POSID,
             A~POST1, A~ZZCYP,
             A~PBUKR AS BUKRS " ADD BSGSM_FCM 2021.09.01
        INTO TABLE @GT_PRPS
        FROM PRPS AS A
       INNER JOIN PROJ AS B
          ON A~PSPHI = B~PSPNR
       WHERE A~PKOKR = @PA_KOKRS
         AND A~PBUKR IN @SO_BUKRS
         AND A~PRCTR IN @SO_PRCTR
         AND A~POSID IN @R_POSID
         AND A~OBJNR IN @R_OBJNR
         AND B~PSPID IN @SO_PSPID
         AND A~PBUKR IN @R_BUKRS
         AND A~PRCTR IN @R_PRCTR1
         AND B~PSPID IN @R_PSPID.

      IF SY-SUBRC <> 0 .
        MESSAGE S004 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

      CLEAR: R_OBJNR, R_OBJNR[].

      LOOP AT GT_PRPS.
        MOVE: 'I'            TO R_OBJNR-SIGN,
              'EQ'           TO R_OBJNR-OPTION,
              GT_PRPS-OBJNR  TO R_OBJNR-LOW.
        COLLECT R_OBJNR.
        CLEAR   R_OBJNR.
      ENDLOOP.

  ENDCASE.

*-- 기간
  IF PA_EPERL IS NOT INITIAL.

    MOVE: 'I'   TO R_FYEAR-SIGN,
          'BT'  TO R_FYEAR-OPTION.

    R_FYEAR-LOW  = PA_GJAHR && PA_SPERL.
    R_FYEAR-HIGH = PA_GJAHR && PA_EPERL.
    APPEND R_FYEAR.

  ELSE.

    MOVE: 'I'   TO R_FYEAR-SIGN,
          'EQ'  TO R_FYEAR-OPTION.

    R_FYEAR-LOW  = PA_GJAHR && PA_SPERL.
    APPEND R_FYEAR.

  ENDIF.

*-- 원가요소
  IF PA_KAGRU IS NOT INITIAL.

    PERFORM READ_HIERARCHY_TABLES TABLES GT_VALUES
                                  USING '0102'
                                        PA_KAGRU.  "원가요소 그룹

    LOOP AT GT_VALUES.

      MOVE: 'I'               TO R_KSTAR-SIGN,
             'BT'             TO R_KSTAR-OPTION,
             GT_VALUES-VFROM  TO R_KSTAR-LOW,
             GT_VALUES-VTO    TO R_KSTAR-HIGH.

      COLLECT R_KSTAR.
      CLEAR   R_KSTAR.
    ENDLOOP.

  ENDIF.

  IF SO_KSTAR[] IS NOT INITIAL.
    R_KSTAR[] = SO_KSTAR[].
  ENDIF.

  LV_SMON = PA_SPERL+1(2).
  LV_EMON = PA_EPERL+1(2).

  LV_MON = LV_SMON.

  DO .

    MOVE LV_MON TO GT_MONTH-V1.
    APPEND GT_MONTH.

    ADD 1 TO LV_MON.
    IF LV_MON > LV_EMON.
      EXIT.
    ENDIF.

  ENDDO.

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

  IF PA_EPERL IS NOT INITIAL.

    LV_M1 = PA_GJAHR && '.' && PA_SPERL+1(2).
    LV_M2 = PA_GJAHR && '.' && PA_EPERL+1(2).
    CONCATENATE LV_M1 '~' LV_M2
           INTO LV_TEXT SEPARATED BY SPACE.

  ELSE.
    LV_M1 = PA_GJAHR && '.' && PA_SPERL+1(2).
    LV_TEXT = LV_M1.

  ENDIF.

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
*& Form READ_TEXT
*&---------------------------------------------------------------------*
FORM READ_TEXT  USING    P_OBJNR TYPE J_OBJNR
                CHANGING PS_OUTTAB STRUCTURE ZCOS0060.

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
*& Form AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM AUTHORITY_CHECK .

  DATA LV_TYPE TYPE C.
  DATA LV_MESSAGE TYPE BAPI_MSG.

  DATA: LT_0070  LIKE TABLE OF ZCAS0070,
        LS_0070  LIKE ZCAS0070,
        LV_CLASS TYPE ZCAT0031-CD_CLASS,
        LV_CODE  TYPE ZCAT0031-CD_CODE.

  CLEAR: R_PRCTR1, R_PRCTR1[],
         R_BUKRS,  R_BUKRS[],
         R_PSPID,  R_PSPID[],
         R_PRCTR2, R_PRCTR2[],
         R_BUKRS2, R_BUKRS2[].

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

  CASE ABAP_TRUE.

    WHEN PA_RAD1.

      IF SO_KOSTL[] IS INITIAL AND PA_KSGRU IS INITIAL.
        MESSAGE S000 WITH TEXT-E13 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

      CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
        EXPORTING
          I_MODULE    = 'CO'
          I_KSTGR_CO  = PA_KSGRU
        IMPORTING
          E_TYPE      = LV_TYPE
          E_MESSAGE   = LV_MESSAGE
        TABLES
          IT_KOSTL_CO = SO_KOSTL[].

    WHEN PA_RAD2.

      IF SO_BUKRS[] IS INITIAL AND
         SO_PRCTR[] IS INITIAL AND
         SO_PSPID[] IS INITIAL AND
         ( SO_POSID[] IS INITIAL AND PA_PDGR IS INITIAL ).
        MESSAGE S000 WITH TEXT-E12 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

      CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
        EXPORTING
          I_MODULE     = 'CO'
          I_POSIDGR_CO = PA_PDGR
        IMPORTING
          E_TYPE       = LV_TYPE
          E_MESSAGE    = LV_MESSAGE
        TABLES
          IT_PRCTR_CO  = SO_PRCTR[]
          IT_BUKRS_CO  = SO_BUKRS[]
          IT_PSPID_CO  = SO_PSPID[]
          IT_POSID_CO  = SO_POSID[].

  ENDCASE.

  IF LV_TYPE = 'E'.
    MESSAGE S000 WITH LV_MESSAGE DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SELECT * FROM ZCOT0320
    INTO TABLE @DATA(LT_ZCOT0320)
    WHERE BNAME = @SY-UNAME.

  LOOP AT LT_ZCOT0320 INTO DATA(LS_ZCOT0320).

    IF LS_ZCOT0320-PRCTR1 IS NOT INITIAL.

      MOVE: LS_ZCOT0320-PRCTR1 TO R_PRCTR1-LOW,
            'I'                TO R_PRCTR1-SIGN,
            'EQ'               TO R_PRCTR1-OPTION.

      COLLECT R_PRCTR1.
      CLEAR   R_PRCTR1.

    ENDIF.

    IF LS_ZCOT0320-BUKRS IS NOT INITIAL.

      MOVE: LS_ZCOT0320-BUKRS TO R_BUKRS-LOW,
            'I'                TO R_BUKRS-SIGN,
            'EQ'               TO R_BUKRS-OPTION.

      COLLECT R_BUKRS.
      CLEAR   R_BUKRS.

    ENDIF.

    IF LS_ZCOT0320-PSPID IS NOT INITIAL.

      MOVE: LS_ZCOT0320-PSPID TO R_PSPID-LOW,
            'I'                TO R_PSPID-SIGN,
            'EQ'               TO R_PSPID-OPTION.

      COLLECT R_PSPID.
      CLEAR   R_PSPID.

    ENDIF.

    IF LS_ZCOT0320-PRCTR2 IS NOT INITIAL.

      MOVE: LS_ZCOT0320-PRCTR2 TO R_PRCTR2-LOW,
            'I'                TO R_PRCTR2-SIGN,
            'EQ'               TO R_PRCTR2-OPTION.

      COLLECT R_PRCTR2.
      CLEAR   R_PRCTR2.

    ENDIF.

    IF LS_ZCOT0320-BUKRS2 IS NOT INITIAL.

      MOVE: LS_ZCOT0320-BUKRS2 TO R_BUKRS2-LOW,
            'I'                TO R_BUKRS2-SIGN,
            'EQ'               TO R_BUKRS2-OPTION.

      COLLECT R_BUKRS2.
      CLEAR   R_BUKRS2.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CHECK_GROUP
*&---------------------------------------------------------------------*
FORM GET_CHECK_GROUP  USING    VALUE(P_SETCLASS)
                                        P_GROUP
*                                        P_CHRT_ACCTS
                               CHANGING PS_RETURN STRUCTURE BAPIRET2.

  CLEAR PS_RETURN.

  DATA LV_GROUPNAME TYPE GRPNAME.
  DATA LV_SETCLASS  TYPE SETCLASS.
  DATA LT_SETHIER TYPE TABLE OF SETHIER_CO WITH HEADER LINE.

  MOVE: P_GROUP     TO LV_GROUPNAME,
        P_SETCLASS  TO LV_SETCLASS.

  CALL FUNCTION 'K_GROUP_REMOTE_READ'
    EXPORTING
      SETCLASS   = LV_SETCLASS
      CO_AREA    = PA_KOKRS
      CHRT_ACCTS = GC_KTOPL
      GROUPNAME  = LV_GROUPNAME
    IMPORTING
      RETURN     = PS_RETURN
    TABLES
      ET_SETHIER = LT_SETHIER.

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
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM SCRFIELDS_FUNCTXT .

  GS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  GS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = GS_FUNTXT.

ENDFORM.

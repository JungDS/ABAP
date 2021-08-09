FUNCTION ZCO_FI_DOCUMENT_BUDGET_CHECK.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IS_BKPF) TYPE  BKPF
*"  EXPORTING
*"     REFERENCE(E_TYPE) TYPE  CHAR1
*"     REFERENCE(E_MESSAGE) TYPE  BAPI_MSG
*"  TABLES
*"      T_BSEG STRUCTURE  ZFIS600
*"      T_BSEG_OLD STRUCTURE  ZFIS600
*"  EXCEPTIONS
*"      NOT_VALID_BUKRS
*"      EXCEEDING_BUDGET
*"      NOT_VALID_CODE
*"----------------------------------------------------------------------

  DATA LV_CTYPE TYPE ZECTYPE.
  DATA LV_CPERD TYPE ZECPERD.

  DATA LV_MONTH TYPE N LENGTH 2.
  DATA LV_OBJNR TYPE J_OBJNR.

  DATA: LV_NTIME     TYPE I,
        LV_CPPERL    TYPE PPERL,
        LV_FIELDNAME TYPE FIELDNAME.

  DATA LV_FYEAR TYPE JAHRPERBL.

  RANGES R_BUDAT FOR ACDOCA-BUDAT.

  DATA: LV_DSUM TYPE HSLVT9_CS,
        LV_FSUM TYPE HSLVT9_CS,
        LV_ASUM TYPE HSLVT9_CS.

  DATA: LV_DSUM_CHAR TYPE CHAR255,
        LV_FSUM_CHAR TYPE CHAR255,
        LV_ASUM_CHAR TYPE CHAR255.

  FIELD-SYMBOLS: <FS_HSL> TYPE ANY,
                 <FS_WKG> TYPE ANY.

*  DATA LV_DMBTR_OLD TYPE DMBTR.
*  DATA LV_DMBTR_NEW TYPE DMBTR.

  DATA LV_DMBTR_OLD TYPE FINS_VHCUR12.  " MODI BY JSY
  DATA LV_DMBTR_NEW TYPE FINS_VHCUR12.

  RANGES R_KSTAR FOR CSKA-KSTAR.

  DATA: BEGIN OF LT_MONTH OCCURS 0,
          V1 TYPE N LENGTH 2,
        END OF LT_MONTH.

  SELECT SINGLE KOKRS INTO @DATA(LV_KOKRS)
    FROM TKA02
   WHERE BUKRS = @IS_BKPF-BUKRS.

  IF SY-SUBRC <> 0.
    RAISE NOT_VALID_BUKRS.
    EXIT.
  ENDIF.


  DATA LT_BSEG_NEW TYPE TABLE OF ZFIS600  WITH HEADER LINE.
*  DATA LT_BSEG_NEW TYPE TABLE OF BSEG WITH HEADER LINE.

*-- 예산유형별 통제 원가요소
  SELECT * FROM ZCOT0010
    INTO TABLE @DATA(LT_ZCOT0010)
    WHERE GJAHR  = @IS_BKPF-GJAHR
      AND KOKRS  = @LV_KOKRS.

  LT_BSEG_NEW[] = T_BSEG[].

  LOOP AT T_BSEG.

    CLEAR LV_OBJNR.
    CLEAR: LV_DSUM, LV_FSUM, LV_ASUM,
           R_BUDAT, R_BUDAT[],
           R_KSTAR, R_KSTAR[],
           LV_DMBTR_NEW, LV_DMBTR_OLD,
           LV_CPERD.

    CLEAR: LT_MONTH, LT_MONTH[].

    IF T_BSEG-KOSTL IS NOT INITIAL.

      LV_CTYPE = '1'.   "부서예산

*-- 예산통제 제외
      SELECT SINGLE KOSTL INTO @DATA(LV_KOSTL)
        FROM ZCOT0030
       WHERE GJAHR  = @IS_BKPF-GJAHR  " ADD BY JSY  200108
         AND KOKRS = @LV_KOKRS
         AND KOSTL = @T_BSEG-KOSTL
         AND BEXCL = @ABAP_TRUE.


* 20200108919_1
*  강현수K REQ 년초에  예산통제가 안되고 있음..
*  원인은 2019년 통제로 등록 100045 로  2020년이 통제 안되었음.
**            SELECT SINGLE KOSTL INTO @DATA(LV_KOSTL)
**        FROM ZCOT0030
**       WHERE KOKRS = @LV_KOKRS
**         AND KOSTL = @T_BSEG-KOSTL
**         AND BEXCL = @ABAP_TRUE.
**


      IF SY-SUBRC = 0.
        CONTINUE.
      ENDIF.

      LV_OBJNR = 'KS' && LV_KOKRS && T_BSEG-KOSTL.

      MOVE: 'I'          TO R_KSTAR-SIGN,
            'EQ'         TO R_KSTAR-OPTION,
            T_BSEG-HKONT TO R_KSTAR-LOW.

      APPEND R_KSTAR.

    ELSEIF T_BSEG-PROJK IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(LS_PRPS)
        FROM PRPS
       WHERE PSPNR = @T_BSEG-PROJK.

      IF SY-SUBRC <> 0.
        RAISE NOT_VALID_CODE.
        EXIT.
      ENDIF.

      LV_OBJNR = LS_PRPS-OBJNR.

*-- 예산통제 제외
      SELECT SINGLE POSID INTO @DATA(LV_POSID)
        FROM ZCOT0030
       WHERE KOKRS = @LV_KOKRS
         AND POSID = @LS_PRPS-POSID
         AND BEXCL = @ABAP_TRUE.

      IF SY-SUBRC = 0.
        CONTINUE.
      ENDIF.

      CASE LS_PRPS-ZZCYP.   "통제유형

        WHEN '1' OR '2'.

          LV_CTYPE = LS_PRPS-ZZCYP.

          MOVE: 'I'          TO R_KSTAR-SIGN,
                'EQ'         TO R_KSTAR-OPTION,
                T_BSEG-HKONT TO R_KSTAR-LOW.

          APPEND R_KSTAR.

        WHEN '3'.   "공사통제 유형

          LV_CTYPE = LS_PRPS-ZZCYP.

          LOOP AT LT_ZCOT0010 INTO DATA(LS_ZCOT0010)
                             WHERE CTYPE = LV_CTYPE
                               AND CPERD = '4'.      "공사는 항상 연기준

            MOVE: 'I'                TO R_KSTAR-SIGN,
                  'EQ'               TO R_KSTAR-OPTION,
                  LS_ZCOT0010-FKSTAR TO R_KSTAR-LOW.

            APPEND R_KSTAR.

          ENDLOOP.

        WHEN OTHERS.   "비통제
          CONTINUE.
      ENDCASE.

    ENDIF.

*-- 예산점검 마스터(ZCOT0010)
    CASE LV_CTYPE.

      WHEN '3'.  "공사통제유형
        IF R_KSTAR[] IS INITIAL.
          CONTINUE.
        ENDIF.

        LV_CPERD = '4'.

      WHEN  OTHERS.
        READ TABLE LT_ZCOT0010 INTO LS_ZCOT0010
             WITH KEY CTYPE  = LV_CTYPE
                      FKSTAR = T_BSEG-HKONT.

        IF SY-SUBRC = 0 .
          LV_CPERD = LS_ZCOT0010-CPERD.
        ELSE.
          CONTINUE.
        ENDIF.

    ENDCASE.

    CASE LV_CPERD.

      WHEN '1'.   "월기준

        LV_NTIME = 1.
        LV_MONTH = IS_BKPF-BUDAT+4(2).

        R_BUDAT-LOW    = IS_BKPF-BUDAT(4) && '0101'.

        CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
          EXPORTING
            DAY_IN            = R_BUDAT-LOW
          IMPORTING
            LAST_DAY_OF_MONTH = R_BUDAT-HIGH
          EXCEPTIONS
            DAY_IN_NO_DATE    = 1
            OTHERS            = 2.

        R_BUDAT-SIGN   = 'I'.
        R_BUDAT-OPTION = 'BT'.
        APPEND R_BUDAT.

      WHEN '2'.   "분기 기준

        LV_NTIME = 3.

        IF IS_BKPF-BUDAT+4(4) >= '0101' AND
           IS_BKPF-BUDAT+4(4) <= '0331'.

          LV_MONTH = '01'.

          R_BUDAT-LOW    = IS_BKPF-GJAHR && '0101'.
          R_BUDAT-HIGH   = IS_BKPF-GJAHR && '0331'.
          R_BUDAT-SIGN   = 'I'.
          R_BUDAT-OPTION = 'BT'.
          APPEND R_BUDAT.

        ELSEIF IS_BKPF-BUDAT+4(4) >= '0401' AND
                IS_BKPF-BUDAT+4(4) <= '0630'.

          LV_MONTH = '04'.

          R_BUDAT-LOW    = IS_BKPF-GJAHR && '0401'.
          R_BUDAT-HIGH   = IS_BKPF-GJAHR && '0630'.
          R_BUDAT-SIGN   = 'I'.
          R_BUDAT-OPTION = 'BT'.
          APPEND R_BUDAT.

        ELSEIF IS_BKPF-BUDAT+4(4) >= '0701' AND
                 IS_BKPF-BUDAT+4(4) <= '0930'.

          LV_MONTH = '07'.

          R_BUDAT-LOW    = IS_BKPF-GJAHR && '0701'.
          R_BUDAT-HIGH   = IS_BKPF-GJAHR && '0930'.
          R_BUDAT-SIGN   = 'I'.
          R_BUDAT-OPTION = 'BT'.
          APPEND R_BUDAT.

        ELSEIF IS_BKPF-BUDAT+4(4) >= '1001' AND
                 IS_BKPF-BUDAT+4(4) <= '1231'.

          LV_MONTH = '10'.

          R_BUDAT-LOW    = IS_BKPF-GJAHR && '1001'.
          R_BUDAT-HIGH   = IS_BKPF-GJAHR && '1231'.
          R_BUDAT-SIGN   = 'I'.
          R_BUDAT-OPTION = 'BT'.
          APPEND R_BUDAT.

        ENDIF.

      WHEN '3'.   "반기 기준

        LV_NTIME = 6.

        IF IS_BKPF-BUDAT+4(4) >= '0101' AND
           IS_BKPF-BUDAT+4(4) <= '0630'.

          LV_MONTH = '01'.

          R_BUDAT-LOW    = IS_BKPF-GJAHR && '0101'.
          R_BUDAT-HIGH   = IS_BKPF-GJAHR && '0630'.
          R_BUDAT-SIGN   = 'I'.
          R_BUDAT-OPTION = 'BT'.
          APPEND R_BUDAT.

        ELSEIF IS_BKPF-BUDAT+4(4) >= '0701' AND
                IS_BKPF-BUDAT+4(4) <= '1231'.

          LV_MONTH = '07'.

          R_BUDAT-LOW    = IS_BKPF-GJAHR && '0701'.
          R_BUDAT-HIGH   = IS_BKPF-GJAHR && '1231'.
          R_BUDAT-SIGN   = 'I'.
          R_BUDAT-OPTION = 'BT'.
          APPEND R_BUDAT.

        ENDIF.

      WHEN '4'.   "연 기준

        LV_NTIME = 12.
        LV_MONTH = '01'.

        R_BUDAT-LOW    = IS_BKPF-GJAHR && '0101'.
        R_BUDAT-HIGH   = IS_BKPF-GJAHR && '1231'.
        R_BUDAT-SIGN   = 'I'.
        R_BUDAT-OPTION = 'BT'.
        APPEND R_BUDAT.

      WHEN '5'.   "기준 없음(비통제 대상)
        CONTINUE.

    ENDCASE.

    DO LV_NTIME TIMES.
      LT_MONTH-V1 = LV_MONTH.
      APPEND LT_MONTH.
      ADD 1 TO LV_MONTH.
    ENDDO.

*-- 계획
    SELECT ROBJNR,
           SUM( HSL01 ) AS HSL01, SUM( HSL02 ) AS HSL02,
           SUM( HSL03 ) AS HSL03, SUM( HSL04 ) AS HSL04,
           SUM( HSL05 ) AS HSL05, SUM( HSL06 ) AS HSL06,
           SUM( HSL07 ) AS HSL07, SUM( HSL08 ) AS HSL08,
           SUM( HSL09 ) AS HSL09, SUM( HSL10 ) AS HSL10,
           SUM( HSL11 ) AS HSL11, SUM( HSL12 ) AS HSL12
      INTO TABLE @DATA(LT_ZCOT0040)
    FROM ZCOT0040
   WHERE RLDNR  = '00'
     AND RRCTY  = '1'
     AND RVERS  = 'B1'
     AND RYEAR  =   @IS_BKPF-GJAHR
     AND ROBJNR =   @LV_OBJNR
     AND RKOKRS =   @LV_KOKRS
     AND RKSTAR IN  @R_KSTAR
    GROUP BY ROBJNR.

*-- 실적
    SELECT OBJNR,
           SUM( WKG001 ) AS WKG001, SUM( WKG002 ) AS WKG002,
           SUM( WKG003 ) AS WKG003, SUM( WKG004 ) AS WKG004,
           SUM( WKG005 ) AS WKG005, SUM( WKG006 ) AS WKG006,
           SUM( WKG007 ) AS WKG007, SUM( WKG008 ) AS WKG008,
           SUM( WKG009 ) AS WKG009, SUM( WKG010 ) AS WKG010,
           SUM( WKG011 ) AS WKG011, SUM( WKG012 ) AS WKG012
      INTO TABLE @DATA(LT_COSP)
      FROM COSP
     WHERE LEDNR = '00'
       AND VERSN = '000'
       AND WRTTP IN ('04', '60', '21', '22')
       AND GJAHR = @IS_BKPF-GJAHR
       AND KSTAR IN  @R_KSTAR
       AND OBJNR = @LV_OBJNR
       AND VRGNG <> 'SDOR'
     GROUP BY OBJNR.

    SELECT OBJNR,
           SUM( WKG001 ) AS WKG001, SUM( WKG002 ) AS WKG002,
           SUM( WKG003 ) AS WKG003, SUM( WKG004 ) AS WKG004,
           SUM( WKG005 ) AS WKG005, SUM( WKG006 ) AS WKG006,
           SUM( WKG007 ) AS WKG007, SUM( WKG008 ) AS WKG008,
           SUM( WKG009 ) AS WKG009, SUM( WKG010 ) AS WKG010,
           SUM( WKG011 ) AS WKG011, SUM( WKG012 ) AS WKG012
      INTO TABLE @DATA(LT_COSP_RKU)
      FROM COSP
     WHERE LEDNR = '00'
       AND VERSN = '000'
       AND WRTTP IN ('04', '60', '21', '22')
       AND GJAHR = @IS_BKPF-GJAHR
       AND KSTAR IN  @R_KSTAR
       AND OBJNR = @LV_OBJNR
       AND VRGNG = 'RKU1'
     GROUP BY OBJNR, KSTAR.

    SELECT OBJNR,
           SUM( HSL ) AS HSL
      INTO TABLE @DATA(LT_ACDOCA)
      FROM ACDOCA
     WHERE RLDNR = '0L'
       AND GJAHR = @IS_BKPF-GJAHR
       AND KOKRS = @LV_KOKRS
       AND RACCT IN  @R_KSTAR
       AND OBJNR = @LV_OBJNR
       AND BUDAT IN @R_BUDAT
       AND BLART IN ('DD', 'SS')
     GROUP BY OBJNR.

    READ TABLE LT_ZCOT0040 ASSIGNING FIELD-SYMBOL(<LS_ZCOT0040>)
                           INDEX 1.
    IF SY-SUBRC = 0.

      LOOP AT LT_MONTH.

        LV_FIELDNAME = '<LS_ZCOT0040>-HSL' && LT_MONTH-V1.

        ASSIGN (LV_FIELDNAME) TO <FS_HSL>.
        LV_DSUM = LV_DSUM + <FS_HSL>.

*-- COSP
        READ TABLE LT_COSP ASSIGNING FIELD-SYMBOL(<LS_COSP>)
                    WITH KEY OBJNR = <LS_ZCOT0040>-ROBJNR.

        IF SY-SUBRC = 0.
          LV_FIELDNAME = '<LS_COSP>-WKG0' && LT_MONTH-V1.

          ASSIGN (LV_FIELDNAME) TO <FS_WKG>.
          LV_FSUM = LV_FSUM + <FS_WKG>.
        ENDIF.

*-- RKU
        READ TABLE LT_COSP_RKU ASSIGNING FIELD-SYMBOL(<LS_COSP_RKU>)
                    WITH KEY OBJNR = <LS_ZCOT0040>-ROBJNR.

        IF SY-SUBRC = 0.
          LV_FIELDNAME = '<LS_COSP_RKU>-WKG0' && LT_MONTH-V1.

          ASSIGN (LV_FIELDNAME) TO <FS_WKG>.
          LV_FSUM = LV_FSUM - <FS_WKG>.
        ENDIF.

      ENDLOOP.

      LOOP AT LT_BSEG_NEW WHERE KOSTL = T_BSEG-KOSTL
                             AND PROJK = T_BSEG-PROJK
                             AND HKONT IN R_KSTAR.

        LV_DMBTR_NEW =  LV_DMBTR_NEW + LT_BSEG_NEW-DMBTR.

      ENDLOOP.

      LOOP AT T_BSEG_OLD WHERE KOSTL = T_BSEG-KOSTL
                            AND PROJK = T_BSEG-PROJK
                            AND HKONT IN R_KSTAR.

        LV_DMBTR_OLD =  LV_DMBTR_OLD + T_BSEG_OLD-DMBTR.

      ENDLOOP.

      READ TABLE LT_ACDOCA ASSIGNING FIELD-SYMBOL(<LS_ACDOCA>)
                    WITH KEY OBJNR = <LS_ZCOT0040>-ROBJNR.

      IF SY-SUBRC = 0.
        LV_FSUM = LV_FSUM - <LS_ACDOCA>-HSL + LV_DMBTR_NEW
                                            - LV_DMBTR_OLD.
        LV_ASUM = LV_DSUM - LV_FSUM.

      ELSE.
        LV_FSUM = LV_FSUM + LV_DMBTR_NEW - LV_DMBTR_OLD.
        LV_ASUM = LV_DSUM - LV_FSUM.

      ENDIF.

      IF LV_ASUM < 0.

        E_TYPE = 'E'.

        LV_ASUM = ABS( LV_ASUM ).
        WRITE LV_ASUM TO LV_ASUM_CHAR
                 CURRENCY IS_BKPF-WAERS LEFT-JUSTIFIED NO-GAP.

        MESSAGE S043(ZCO01) WITH LV_ASUM_CHAR
                            INTO E_MESSAGE.
        EXIT.

      ENDIF.

    ELSE.

      E_TYPE = 'E'.
      MESSAGE S045(ZCO01) INTO E_MESSAGE.
      EXIT.

    ENDIF.

  ENDLOOP.

  IF E_TYPE IS INITIAL.
    E_TYPE = 'S'.
  ENDIF.

ENDFUNCTION.

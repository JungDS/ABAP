FUNCTION ZCO_MM_DOCUMENT_BUDGET_CHECK.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_GUBUN) TYPE  CHAR2
*"     VALUE(I_BUKRS) TYPE  BUKRS
*"  EXPORTING
*"     REFERENCE(E_TYPE) TYPE  CHAR1
*"     REFERENCE(E_MESSAGE) TYPE  BAPI_MSG
*"  TABLES
*"      T_DATA_NEW STRUCTURE  ZCOS0080 OPTIONAL
*"      T_DATA_OLD STRUCTURE  ZCOS0080 OPTIONAL
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

  DATA LT_DATA_NEW TYPE TABLE OF ZCOS0080 WITH HEADER LINE.

  FIELD-SYMBOLS: <FS_HSL> TYPE ANY,
                 <FS_WKG> TYPE ANY.


  RANGES R_KSTAR FOR CSKA-KSTAR.

  DATA: BEGIN OF LT_MONTH OCCURS 0,
          V1 TYPE N LENGTH 2,
        END OF LT_MONTH.

  DATA LV_DMBTR_OLD TYPE DMBTR.
  DATA LV_DMBTR_NEW TYPE DMBTR.

  LT_DATA_NEW[] = T_DATA_NEW[].

  SELECT SINGLE KOKRS INTO @DATA(LV_KOKRS)
    FROM TKA02
   WHERE BUKRS = @I_BUKRS.

  IF SY-SUBRC <> 0.
    RAISE NOT_VALID_BUKRS.
    EXIT.
  ENDIF.

  LOOP AT T_DATA_NEW.

    CLEAR LV_OBJNR.
    CLEAR: LV_DSUM, LV_FSUM, LV_ASUM,
           R_BUDAT, R_BUDAT[],
           R_KSTAR, R_KSTAR[], LV_CPERD.

    CLEAR: LV_DMBTR_NEW, LV_DMBTR_OLD.

    CLEAR: LT_MONTH, LT_MONTH[].

    IF T_DATA_NEW-KOSTL IS NOT INITIAL.

      LV_CTYPE = '1'.   "부서예산

*-- 예산통제 제외
      SELECT SINGLE KOSTL INTO @DATA(LV_KOSTL)
        FROM ZCOT0030
       WHERE KOKRS = @LV_KOKRS
         AND KOSTL = @T_DATA_NEW-KOSTL
         AND BEXCL = @ABAP_TRUE.

      IF SY-SUBRC = 0.
        CONTINUE.
      ENDIF.

      LV_OBJNR = 'KS' && LV_KOKRS && T_DATA_NEW-KOSTL.

      MOVE: 'I'              TO R_KSTAR-SIGN,
            'EQ'             TO R_KSTAR-OPTION,
            T_DATA_NEW-KSTAR TO R_KSTAR-LOW.

      APPEND R_KSTAR.

    ELSEIF T_DATA_NEW-PSPNR IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(LS_PRPS)
        FROM PRPS
       WHERE PSPNR = @T_DATA_NEW-PSPNR.

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

          MOVE: 'I'              TO R_KSTAR-SIGN,
                'EQ'             TO R_KSTAR-OPTION,
                T_DATA_NEW-KSTAR TO R_KSTAR-LOW.

          APPEND R_KSTAR.

        WHEN '3'.   "공사통제 유형

          LV_CTYPE = LS_PRPS-ZZCYP.

          SELECT * FROM ZCOT0010
            INTO TABLE @DATA(LT_ZCOT0010)
           WHERE GJAHR = @T_DATA_NEW-LFDAT(4)
             AND KOKRS = @LV_KOKRS
             AND CTYPE = @LV_CTYPE
             AND CPERD = '4'.      "공사유형은 연기준

          IF SY-SUBRC = 0.

            LOOP AT LT_ZCOT0010 INTO DATA(LS_ZCOT0010).

              MOVE: 'I'                TO R_KSTAR-SIGN,
                    'EQ'               TO R_KSTAR-OPTION,
                    LS_ZCOT0010-FKSTAR TO R_KSTAR-LOW.

              APPEND R_KSTAR.

            ENDLOOP.

          ELSE.
            CONTINUE.
          ENDIF.

        WHEN OTHERS.   "비통제
          CONTINUE.
      ENDCASE.

    ENDIF.

*-- 예산점검 마스터(ZCOT0010)
    CASE LV_CTYPE.

      WHEN '3'.  "공사통제유형
        LV_CPERD = '4'.

      WHEN  OTHERS.
        SELECT SINGLE * FROM ZCOT0010
           INTO @LS_ZCOT0010
          WHERE GJAHR  = @T_DATA_NEW-LFDAT(4)
            AND KOKRS  = @LV_KOKRS
            AND CTYPE  = @LV_CTYPE
            AND FKSTAR = @T_DATA_NEW-KSTAR.

        IF SY-SUBRC = 0 .
          LV_CPERD = LS_ZCOT0010-CPERD.
        ELSE.
          CONTINUE.
        ENDIF.

    ENDCASE.

    CASE LV_CPERD.

      WHEN '1'.   "월기준

        LV_NTIME = 1.
        LV_MONTH = T_DATA_NEW-LFDAT+4(2).

        R_BUDAT-LOW    = T_DATA_NEW-LFDAT(4) && '0101'.

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

        IF T_DATA_NEW-LFDAT+4(4) >= '0101' AND
           T_DATA_NEW-LFDAT+4(4) <= '0331'.

          LV_MONTH = '01'.

          R_BUDAT-LOW    = T_DATA_NEW-LFDAT(4) && '0101'.
          R_BUDAT-HIGH   = T_DATA_NEW-LFDAT(4) && '0331'.
          R_BUDAT-SIGN   = 'I'.
          R_BUDAT-OPTION = 'BT'.
          APPEND R_BUDAT.

        ELSEIF T_DATA_NEW-LFDAT+4(4) >= '0401' AND
                T_DATA_NEW-LFDAT+4(4) <= '0630'.

          LV_MONTH = '04'.

          R_BUDAT-LOW    = T_DATA_NEW-LFDAT(4) && '0401'.
          R_BUDAT-HIGH   = T_DATA_NEW-LFDAT(4) && '0630'.
          R_BUDAT-SIGN   = 'I'.
          R_BUDAT-OPTION = 'BT'.
          APPEND R_BUDAT.

        ELSEIF T_DATA_NEW-LFDAT+4(4) >= '0701' AND
                T_DATA_NEW-LFDAT+4(4) <= '0930'.

          LV_MONTH = '07'.

          R_BUDAT-LOW    = T_DATA_NEW-LFDAT(4) && '0701'.
          R_BUDAT-HIGH   = T_DATA_NEW-LFDAT(4) && '0930'.
          R_BUDAT-SIGN   = 'I'.
          R_BUDAT-OPTION = 'BT'.
          APPEND R_BUDAT.

        ELSEIF T_DATA_NEW-LFDAT+4(4) >= '1001' AND
                T_DATA_NEW-LFDAT+4(4) <= '1231'.

          LV_MONTH = '10'.

          R_BUDAT-LOW    = T_DATA_NEW-LFDAT(4) && '1001'.
          R_BUDAT-HIGH   = T_DATA_NEW-LFDAT(4) && '1231'.
          R_BUDAT-SIGN   = 'I'.
          R_BUDAT-OPTION = 'BT'.
          APPEND R_BUDAT.

        ENDIF.

      WHEN '3'.   "반기 기준

        LV_NTIME = 6.

        IF T_DATA_NEW-LFDAT+4(4) >= '0101' AND
           T_DATA_NEW-LFDAT+4(4) <= '0630'.

          LV_MONTH = '01'.

          R_BUDAT-LOW    = T_DATA_NEW-LFDAT(4) && '0101'.
          R_BUDAT-HIGH   = T_DATA_NEW-LFDAT(4) && '0630'.
          R_BUDAT-SIGN   = 'I'.
          R_BUDAT-OPTION = 'BT'.
          APPEND R_BUDAT.

        ELSEIF T_DATA_NEW-LFDAT+4(4) >= '0701' AND
                T_DATA_NEW-LFDAT+4(4) <= '1231'.

          LV_MONTH = '07'.

          R_BUDAT-LOW    = T_DATA_NEW-LFDAT(4) && '0701'.
          R_BUDAT-HIGH   = T_DATA_NEW-LFDAT(4) && '1231'.
          R_BUDAT-SIGN   = 'I'.
          R_BUDAT-OPTION = 'BT'.
          APPEND R_BUDAT.

        ENDIF.

      WHEN '4'.   "연 기준

        LV_NTIME = 12.
        LV_MONTH = '01'.

        R_BUDAT-LOW    = T_DATA_NEW-LFDAT(4) && '0101'.
        R_BUDAT-HIGH   = T_DATA_NEW-LFDAT(4) && '1231'.
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
     AND RYEAR  = @T_DATA_NEW-LFDAT(4)
     AND ROBJNR = @LV_OBJNR
     AND RKOKRS = @LV_KOKRS
     AND RKSTAR IN @R_KSTAR
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
       AND GJAHR = @T_DATA_NEW-LFDAT(4)
       AND KSTAR IN @R_KSTAR
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
       AND GJAHR = @T_DATA_NEW-LFDAT(4)
       AND KSTAR IN @R_KSTAR
       AND OBJNR = @LV_OBJNR
       AND VRGNG = 'RKU1'
     GROUP BY OBJNR, KSTAR.

    SELECT OBJNR,
           SUM( HSL ) AS HSL
      INTO TABLE @DATA(LT_ACDOCA)
      FROM ACDOCA
     WHERE RLDNR = '0L'
       AND GJAHR = @T_DATA_NEW-LFDAT(4)
       AND KOKRS = @LV_KOKRS
       AND RACCT IN @R_KSTAR
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

      LOOP AT LT_DATA_NEW WHERE KOSTL = T_DATA_NEW-KOSTL
                             AND PSPNR = T_DATA_NEW-PSPNR
                             AND KSTAR IN R_KSTAR
                             AND LFDAT IN R_BUDAT.

        LV_DMBTR_NEW =  LV_DMBTR_NEW + LT_DATA_NEW-DMBTR.

      ENDLOOP.

      LOOP AT T_DATA_OLD WHERE KOSTL = T_DATA_NEW-KOSTL
                            AND PSPNR = T_DATA_NEW-PSPNR
                            AND KSTAR IN R_KSTAR
                            AND LFDAT IN R_BUDAT.

        LV_DMBTR_OLD =  LV_DMBTR_OLD + T_DATA_OLD-DMBTR.

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
                 CURRENCY T_DATA_NEW-WAERS LEFT-JUSTIFIED NO-GAP.

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

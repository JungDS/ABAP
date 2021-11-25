FUNCTION ZCO_GET_BU_TYPE_BY_MAPPING.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_PSPNR) TYPE  PRPS-PSPNR
*"     REFERENCE(I_COPA) TYPE  CE11000 OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_WW120) TYPE  T2501-WW120
*"     REFERENCE(E_MAPPING_TYPE) TYPE  C
*"----------------------------------------------------------------------

  DATA: BEGIN OF LS_COND,
          ZZBGU LIKE PRPS-ZZBGU,
          ZZBGD LIKE PRPS-ZZBGD,
          ZZPRG LIKE PRPS-ZZPRG,
        END OF LS_COND,
        LT_COND LIKE TABLE OF LS_COND.


  CLEAR E_WW120.
  CHECK I_PSPNR IS NOT INITIAL.


*--------------------------------------------------------------------*
*1.   1순위 추출 (WBS별 매핑정보 활용)
*1-1.   WBS 별 BU 매핑정보 추출
*2.   1순위 미적용시 2순위 추출 (WBS 속성정보 기준 매핑정보 활용)
*2-1.   WBS 속성정보 추출 : 회사, 사업구분, 사업세부구분, 발주처유형
*2-2.   속성정보 기준 매핑정보 추출 (세부구분부터 우선순위 적용)
*         - 1. 모든 조건이 입력되고 일치하는 경우
*         - 2. 사업구분 + 세부구분이 일치하고 발주처는 공란인 경우
*         - 3. 사업구분 + 발주처가 일치하고 세부구분이 공란인 경우
*         - 4. 사업구분이 일치하고, 세부구분, 발주처가 공란인 경우
*         - 5. 발주처가 일치하고, 사업구분, 세부구분이 공란인 경우
*         - 6. 사업구분, 세부구분, 발주처가 공란인 경우
*--------------------------------------------------------------------*

*-- 1순위 : WBS별 매핑정보

  SELECT SINGLE WW120
    FROM ZCOT1320
   WHERE PSPNR EQ @I_PSPNR
    INTO @E_WW120.

  IF SY-SUBRC EQ 0 AND E_WW120 IS NOT INITIAL.
    " 매핑완료
    E_MAPPING_TYPE = 'W'.
    EXIT.
  ENDIF.


*-- 2순위 : WBS 속성정보

  IF I_COPA IS NOT SUPPLIED.

    SELECT SINGLE PBUKR, ZZBGU, ZZBGD, ZZPRG
      FROM PRPS
     WHERE PSPNR = @I_PSPNR
      INTO @DATA(LS_PRPS).

  ELSE.

    LS_PRPS = VALUE #(
      PBUKR = I_COPA-BUKRS
      ZZBGU = I_COPA-WW040
      ZZBGD = I_COPA-WW050
      ZZPRG = I_COPA-WW100
    ).

  ENDIF.

  SELECT ZZBGU,
         ZZBGD,
         ZZPRG,
         WW120
    FROM ZCOT1310
   WHERE BUKRS EQ @LS_PRPS-PBUKR                          " 회사
     AND ( ZZBGU IS INITIAL OR ZZBGU EQ @LS_PRPS-ZZBGU )  " 사업구분
     AND ( ZZBGD IS INITIAL OR ZZBGD EQ @LS_PRPS-ZZBGD )  " 세부사업
     AND ( ZZPRG IS INITIAL OR ZZPRG EQ @LS_PRPS-ZZPRG )  " 발주처유형
    INTO TABLE @DATA(LT_1310).

  CHECK SY-SUBRC EQ 0.

  SORT LT_1310 BY ZZBGU
                  ZZBGD
                  ZZPRG.


*-- 세부구분부터 우선순위 적용

  LT_COND = VALUE #(
*- 1. 모든 조건이 입력되고 일치하는 경우
    ( ZZBGU = LS_PRPS-ZZBGU
      ZZBGD = LS_PRPS-ZZBGD
      ZZPRG = LS_PRPS-ZZPRG )

*- 2. 사업구분 + 세부구분이 일치하고 발주처는 공란인 경우
    ( ZZBGU = LS_PRPS-ZZBGU
      ZZBGD = LS_PRPS-ZZBGD
      ZZPRG = SPACE )

*- 3. 사업구분 + 발주처가 일치하고 세부구분이 공란인 경우
    ( ZZBGU = LS_PRPS-ZZBGU
      ZZBGD = SPACE
      ZZPRG = LS_PRPS-ZZPRG )

*- 4. 사업구분이 일치하고, 세부구분, 발주처가 공란인 경우
    ( ZZBGU = LS_PRPS-ZZBGU
      ZZBGD = SPACE
      ZZPRG = SPACE )

*- 5. 발주처가 일치하고, 사업구분, 세부구분이 공란인 경우
    ( ZZBGU = SPACE
      ZZBGD = SPACE
      ZZPRG = LS_PRPS-ZZPRG )

*- 6. 사업구분, 세부구분, 발주처가 공란인 경우
    ( ZZBGU = SPACE
      ZZBGD = SPACE
      ZZPRG = SPACE )
  ).


*-- 우선순위에 따라 조건과 일치하는 라인이 있는지 점검
  LOOP AT LT_COND INTO LS_COND.

    READ TABLE LT_1310 INTO DATA(LS_1310)
                       WITH KEY ZZBGU = LS_COND-ZZBGU
                                ZZBGD = LS_COND-ZZBGD
                                ZZPRG = LS_COND-ZZPRG
                                BINARY SEARCH.

    CHECK SY-SUBRC EQ 0.

    E_WW120        = LS_1310-WW120.
    E_MAPPING_TYPE = 'A'.
    EXIT.

  ENDLOOP.

ENDFUNCTION.

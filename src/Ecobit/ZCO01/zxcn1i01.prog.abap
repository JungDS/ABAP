*----------------------------------------------------------------------*
***INCLUDE ZXCN1I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
MODULE PAI INPUT.

  IF ZCOS0021-ZZWAE IS INITIAL.
    CLEAR ZCOS0021-ZZTCV.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_TEXT  INPUT
*&---------------------------------------------------------------------*
MODULE GET_TEXT INPUT.

*-- 매출 유형내역
  SELECT SINGLE ZZSCTTX INTO @ZCOS0020-ZZSCTTX
    FROM ZCOT1010T
   WHERE ZZSCT = @ZCOS0020-ZZSCT
     AND SPRAS = @SY-LANGU.

*-- 프로젝트 단계내역
  SELECT SINGLE ZZPHATX INTO @ZCOS0020-ZZPHATX
    FROM ZCOT1020T
   WHERE ZZPHA = @ZCOS0020-ZZPHA
     AND SPRAS = @SY-LANGU.

*-- WBS 유형내역
  SELECT SINGLE ZZWBTTX INTO @ZCOS0020-ZZWBTTX
    FROM ZCOT1030T
   WHERE ZZWBT = @ZCOS0020-ZZWBT
     AND SPRAS = @SY-LANGU.

*-- 사업구분내역
  SELECT SINGLE ZZBGUTX INTO @ZCOS0020-ZZBGUTX
    FROM ZCOT1040T
   WHERE ZZBGU = @ZCOS0020-ZZBGU
     AND SPRAS = @SY-LANGU.

*-- 사업구분상세내역
  SELECT SINGLE ZZBGDTX INTO @ZCOS0020-ZZBGDTX
    FROM ZCOT1050T
   WHERE ZZBGU = @ZCOS0020-ZZBGU
     AND ZZBGD = @ZCOS0020-ZZBGD
     AND SPRAS = @SY-LANGU.

*-- 투자여부 내역
  SELECT SINGLE ZZIVCTX INTO @ZCOS0020-ZZIVCTXT
    FROM ZCOT0020T
   WHERE ZZIVC = @ZCOS0020-ZZIVC
     AND SPRAS = @SY-LANGU.


*-- 사업장유무내역
  SELECT SINGLE ZZBAGTX INTO @ZCOS0020-ZZBAGTX
    FROM ZCOT1070T
   WHERE ZZBAG = @ZCOS0020-ZZBAG
     AND SPRAS = @SY-LANGU.


*-- 행정구역내역
  SELECT SINGLE ZZADTTX INTO @ZCOS0020-ZZADTTX
    FROM ZCOT1090T
   WHERE ZZADT = @ZCOS0020-ZZADT
     AND SPRAS = @SY-LANGU.


*-- 발주처 유형내역
  SELECT SINGLE ZZPRGTX INTO @ZCOS0020-ZZPRGTX
    FROM ZCOT1100T
   WHERE ZZPRG = @ZCOS0020-ZZPRG
     AND SPRAS = @SY-LANGU.

*-- H/W 하위본부내역
  SELECT SINGLE ZZHWBTX INTO @ZCOS0020-ZZHWBTX
    FROM ZCOT1110T
   WHERE ZZHWB = @ZCOS0020-ZZHWB
     AND SPRAS = @SY-LANGU.

*-- 통제유형 내역
  SELECT SINGLE CTEXT INTO @ZCOS0020-ZZCYPTXT
    FROM ZCOT1130T
   WHERE CTYPE = @ZCOS0020-ZZCYP
     AND SPRAS = @SY-LANGU.

*-- 수주유형 내역
  SELECT SINGLE COTXT INTO @ZCOS0020-ZZCOPTXT
    FROM ZCOT1120T
   WHERE COTYP = @ZCOS0020-ZZCOP
     AND SPRAS = @SY-LANGU.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_TEXT_EQUIPMENT  INPUT
*&---------------------------------------------------------------------*
* [CO] ESG Pjt. 설비WBS 고객필드 UserExit - 2021.09.29 17:09:56, MDP_06
*&---------------------------------------------------------------------*
MODULE GET_TEXT_EQUIPMENT INPUT.

*-- 투자사유 내역
  CLEAR ZCOS0021-ZZIZWTX.
  SELECT SINGLE ZZIZWTX
    FROM ZCOT1270T
   WHERE SPRAS EQ @SY-LANGU
     AND ZZIZW EQ @ZCOS0021-ZZIZW
    INTO @ZCOS0021-ZZIZWTX.

*-- 설비분류(대) 내역
  CLEAR ZCOS0021-ZZCD1TX.
  SELECT SINGLE ZZCD1TX
    FROM ZCOT1280T
   WHERE SPRAS EQ @SY-LANGU
     AND ZZCD1 EQ @ZCOS0021-ZZCD1
    INTO @ZCOS0021-ZZCD1TX.

*-- 설비분류(중) 내역
  CLEAR ZCOS0021-ZZCD2TX.
  SELECT SINGLE ZZCD2TX
    FROM ZCOT1290T
   WHERE SPRAS EQ @SY-LANGU
     AND ZZCD1 EQ @ZCOS0021-ZZCD1
     AND ZZCD2 EQ @ZCOS0021-ZZCD2
    INTO @ZCOS0021-ZZCD2TX.

*-- 설비분류(소) 내역
  CLEAR ZCOS0021-ZZCD3TX.
  SELECT SINGLE ZZCD3TX
    FROM ZCOT1300T
   WHERE SPRAS EQ @SY-LANGU
     AND ZZCD1 EQ @ZCOS0021-ZZCD1
     AND ZZCD2 EQ @ZCOS0021-ZZCD2
     AND ZZCD3 EQ @ZCOS0021-ZZCD3
    INTO @ZCOS0021-ZZCD3TX.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  CHECK_RATE  INPUT
*&---------------------------------------------------------------------*
* [CO] ESG Pjt. 설비WBS 고객필드 UserExit - 2021.09.29 17:09:56, MDP_06
*&---------------------------------------------------------------------*
MODULE CHECK_RATE INPUT.

*  CHECK GV_PROFL EQ 'Z000003'. " 설비 투자및관리비용
*
*  DATA(LV_RATE) = ZCOS0021-ZZRT1 +
*                  ZCOS0021-ZZRT2 +
*                  ZCOS0021-ZZRT3.
*
*  CHECK LV_RATE NE 0
*    AND LV_RATE NE 100.
*
*  " 비율(%)의 합산 결과는 100% 여야 합니다.
*  MESSAGE E059(ZCO01).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_ZZUNT  INPUT
*&---------------------------------------------------------------------*
* [ESG_CO] DEV_ESG 설비WBS 고객필드 추가, 2021.11.22 17:36:19, MDP_06
*&---------------------------------------------------------------------*
MODULE CHECK_ZZUNT INPUT.

*  CHECK ZCOS0021-ZZUNT IS NOT INITIAL
*    AND ZCOS0021-ZZUNT NOT BETWEEN '0' AND '9'.
*
*  " & 필드값이 유효하지 않습니다.
*  MESSAGE E027(ZCO01).

ENDMODULE.

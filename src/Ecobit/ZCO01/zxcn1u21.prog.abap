*&---------------------------------------------------------------------*
*& Include          ZXCN1U21
*&---------------------------------------------------------------------*

 CLEAR ZCOS0020.
 CLEAR GV_DISPLAY.

 ZCOS0020-ZZSCT = SAP_PRPS_IMP-ZZSCT.
 ZCOS0020-ZZPHA = SAP_PRPS_IMP-ZZPHA.
 ZCOS0020-ZZWBT = SAP_PRPS_IMP-ZZWBT.
 ZCOS0020-ZZBGU = SAP_PRPS_IMP-ZZBGU.
 ZCOS0020-ZZBGD = SAP_PRPS_IMP-ZZBGD.
 ZCOS0020-ZZPRG = SAP_PRPS_IMP-ZZPRG.
 ZCOS0020-ZZADT = SAP_PRPS_IMP-ZZADT.
 ZCOS0020-ZZHWB = SAP_PRPS_IMP-ZZHWB.
 ZCOS0020-ZZBAG = SAP_PRPS_IMP-ZZBAG.
 ZCOS0020-ZZIVC = SAP_PRPS_IMP-ZZIVC.
 ZCOS0020-ZZCYP = SAP_PRPS_IMP-ZZCYP.
 ZCOS0020-ZZCOP = SAP_PRPS_IMP-ZZCOP.

 GV_DISPLAY     = SAP_DISPLAY_ONLY.

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

*-- 통제유형내역
 SELECT SINGLE CTEXT INTO @ZCOS0020-ZZCYPTXT
   FROM ZCOT1130T
  WHERE CTYPE = @ZCOS0020-ZZCYP
    AND SPRAS = @SY-LANGU.

*-- 수주유형 내역
 SELECT SINGLE COTXT INTO @ZCOS0020-ZZCOPTXT
   FROM ZCOT1120T
  WHERE COTYP = @ZCOS0020-ZZCOP
    AND SPRAS = @SY-LANGU.


*--------------------------------------------------------------------*
* [CO] ESG Pjt. 설비WBS 고객필드 UserExit - 2021.09.29 17:09:56, MDP_06
*--------------------------------------------------------------------*

  GV_PROFL        = SAP_PROJ_IMP-PROFL.

  CLEAR ZCOS0021.
  ZCOS0021-ZZIZW = SAP_PRPS_IMP-ZZIZW.
  ZCOS0021-ZZCD1 = SAP_PRPS_IMP-ZZCD1.
  ZCOS0021-ZZCD2 = SAP_PRPS_IMP-ZZCD2.
  ZCOS0021-ZZCD3 = SAP_PRPS_IMP-ZZCD3.
  ZCOS0021-ZZTRD = SAP_PRPS_IMP-ZZTRD.
  ZCOS0021-ZZWAE = SAP_PRPS_IMP-ZZWAE.
  ZCOS0021-ZZDT1 = SAP_PRPS_IMP-ZZDT1.
  ZCOS0021-ZZDT2 = SAP_PRPS_IMP-ZZDT2.
  ZCOS0021-ZZDT3 = SAP_PRPS_IMP-ZZDT3.

  DATA LV_STRING_IN TYPE C LENGTH 200.
  DATA LV_HTYPE     TYPE DD01V-DATATYPE.


*-- 계약금액 점검
  LV_STRING_IN = SAP_PRPS_IMP-ZZTCV.
  REPLACE '.' IN LV_STRING_IN WITH SPACE .
  CONDENSE LV_STRING_IN.

  CALL FUNCTION 'NUMERIC_CHECK' EXPORTING STRING_IN  = LV_STRING_IN
                                IMPORTING HTYPE      = LV_HTYPE.
  IF LV_HTYPE EQ 'NUMC'.
    ZCOS0021-ZZTCV = SAP_PRPS_IMP-ZZTCV.
  ELSE.
    ZCOS0021-ZZTCV = 0.
  ENDIF.


*-- 계약금 비율(%) 점검
  LV_STRING_IN = SAP_PRPS_IMP-ZZRT1.
  REPLACE '.' IN LV_STRING_IN WITH SPACE .
  CONDENSE LV_STRING_IN.

  CALL FUNCTION 'NUMERIC_CHECK' EXPORTING STRING_IN  = LV_STRING_IN
                                IMPORTING HTYPE      = LV_HTYPE.
  IF LV_HTYPE EQ 'NUMC'.
    ZCOS0021-ZZRT1 = SAP_PRPS_IMP-ZZRT1.
  ELSE.
    ZCOS0021-ZZRT1 = 0.
  ENDIF.


*-- 중도금 비율(%) 점검
  LV_STRING_IN = SAP_PRPS_IMP-ZZRT2.
  REPLACE '.' IN LV_STRING_IN WITH SPACE .
  CONDENSE LV_STRING_IN.

  CALL FUNCTION 'NUMERIC_CHECK' EXPORTING STRING_IN  = LV_STRING_IN
                                IMPORTING HTYPE      = LV_HTYPE.
  IF LV_HTYPE EQ 'NUMC'.
    ZCOS0021-ZZRT2 = SAP_PRPS_IMP-ZZRT2.
  ELSE.
    ZCOS0021-ZZRT2 = 0.
  ENDIF.


*-- 잔금 비율(%) 점검
  LV_STRING_IN = SAP_PRPS_IMP-ZZRT3.
  REPLACE '.' IN LV_STRING_IN WITH SPACE.
  CONDENSE LV_STRING_IN.

  CALL FUNCTION 'NUMERIC_CHECK' EXPORTING STRING_IN  = LV_STRING_IN
                                IMPORTING HTYPE      = LV_HTYPE.
  IF LV_HTYPE EQ 'NUMC'.
    ZCOS0021-ZZRT3 = SAP_PRPS_IMP-ZZRT3.
  ELSE.
    ZCOS0021-ZZRT3 = 0.
  ENDIF.


* -- 투자사유 내역
  SELECT SINGLE ZZIZWTX
    FROM ZCOT1270T
   WHERE SPRAS EQ @SY-LANGU
     AND ZZIZW EQ @ZCOS0021-ZZIZW
    INTO @ZCOS0021-ZZIZWTX.

* -- 설비분류(대) 내역
  SELECT SINGLE ZZCD1TX
    FROM ZCOT1280T
   WHERE SPRAS EQ @SY-LANGU
     AND ZZCD1 EQ @ZCOS0021-ZZCD1
    INTO @ZCOS0021-ZZCD1TX.

* -- 설비분류(중) 내역
  SELECT SINGLE ZZCD2TX
    FROM ZCOT1290T
   WHERE SPRAS EQ @SY-LANGU
     AND ZZCD1 EQ @ZCOS0021-ZZCD1
     AND ZZCD2 EQ @ZCOS0021-ZZCD2
    INTO @ZCOS0021-ZZCD2TX.

* -- 설비분류(소) 내역
  SELECT SINGLE ZZCD3TX
    FROM ZCOT1300T
   WHERE SPRAS EQ @SY-LANGU
     AND ZZCD1 EQ @ZCOS0021-ZZCD1
     AND ZZCD2 EQ @ZCOS0021-ZZCD2
     AND ZZCD3 EQ @ZCOS0021-ZZCD3
    INTO @ZCOS0021-ZZCD3TX.

*--------------------------------------------------------------------*
* [ESG_CO] DEV_ESG 설비WBS 고객필드 추가, 2021.11.22 17:36:19, MDP_06
*--------------------------------------------------------------------*

**-- 호기(Unit)
*  LV_STRING_IN = SAP_PRPS_IMP-ZZUNT.
*  CALL FUNCTION 'NUMERIC_CHECK' EXPORTING STRING_IN  = LV_STRING_IN
*                                IMPORTING HTYPE      = LV_HTYPE.
*  IF LV_HTYPE EQ 'NUMC'.
*    ZCOS0021-ZZUNT = SAP_PRPS_IMP-ZZUNT.
*  ELSE.
*    ZCOS0021-ZZUNT = SPACE.
*  ENDIF.

  ZCOS0021-ZZUNT = SAP_PRPS_IMP-ZZUNT.  " 호기(Unit)
  ZCOS0021-ZZCMD = SAP_PRPS_IMP-ZZCMD.  " 공사착공일(Commencement Date)
  ZCOS0021-ZZCPD = SAP_PRPS_IMP-ZZCPD.  " 공사준공일(Completion Date)

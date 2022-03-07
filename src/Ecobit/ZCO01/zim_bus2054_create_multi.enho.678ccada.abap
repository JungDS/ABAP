"Name: \FU:BAPI_BUS2054_CREATE_MULTI\SE:BEGIN\EI
ENHANCEMENT 0 ZIM_BUS2054_CREATE_MULTI.
*--------------------------------------------------------------------*
* ESG Prj : WBS 확장필드 금액단위 조정을 위한 Enhancement, 2021.10.08
*--------------------------------------------------------------------*
  CALL FUNCTION 'ZCO_WBS_EXTENSION_AMOUNT_ADJ'
    EXPORTING
      I_PROJECT_DEFINITION = I_PROJECT_DEFINITION
    TABLES
      EXTENSIONIN = EXTENSIONIN[].

ENDENHANCEMENT.

FUNCTION ZSH_PVER_F4_EXIT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR_T
*"     VALUE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------


  CHECK CALLCONTROL-STEP = 'DISP'.
  DELETE ADJACENT DUPLICATES FROM RECORD_TAB.


ENDFUNCTION.

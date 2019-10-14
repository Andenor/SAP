FUNCTION ztms_rfc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_SYSTEM) TYPE  TMSSYSNAM OPTIONAL
*"     REFERENCE(IV_DOMAIN) TYPE  TMSDOMNAM OPTIONAL
*"     REFERENCE(IV_ALLCLI) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(IV_TRCLI) TYPE  TRCLIENT OPTIONAL
*"     REFERENCE(IV_TRFUNCTION) TYPE  TRFUNCTION OPTIONAL
*"     REFERENCE(IV_PROJECT) TYPE  TRKORR OPTIONAL
*"     REFERENCE(IV_IMPORTS) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(IV_EXPORTS) TYPE  FLAG OPTIONAL
*"     REFERENCE(IV_ALL_STEPS) TYPE  FLAG OPTIONAL
*"     REFERENCE(IV_ALL_ALOG_STEPS) TYPE  FLAG OPTIONAL
*"     REFERENCE(IV_TPSTAT_KEY) TYPE  TMSTPKEY OPTIONAL
*"     REFERENCE(IV_MONITOR) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(EV_ALOG_LINENR) TYPE  FLAG
*"     REFERENCE(ET_TMSTPALOG) TYPE  TMSTPALOGS
*"     REFERENCE(ES_EXCEPTION) TYPE  STMSCALERT
*"  CHANGING
*"     REFERENCE(CV_START_DATE) TYPE  DATS OPTIONAL
*"     REFERENCE(CV_START_TIME) TYPE  TIMS OPTIONAL
*"     REFERENCE(CV_END_DATE) TYPE  DATS OPTIONAL
*"     REFERENCE(CV_END_TIME) TYPE  TIMS OPTIONAL
*"  EXCEPTIONS
*"      ALERT
*"      OTHERS
*"----------------------------------------------------------------------


  CALL FUNCTION 'TMS_TM_GET_HISTORY'
    EXPORTING
      iv_system         = iv_system
      iv_domain         = iv_domain
      iv_allcli         = iv_allcli
      iv_trcli          = iv_trcli
      iv_trfunction     = iv_trfunction
      iv_project        = iv_project
      iv_imports        = iv_imports
      iv_exports        = iv_exports
      iv_all_steps      = iv_all_steps
      iv_all_alog_steps = iv_all_alog_steps
      iv_tpstat_key     = iv_tpstat_key
      iv_monitor        = iv_monitor
    IMPORTING
      ev_alog_linenr    = ev_alog_linenr
      et_tmstpalog      = et_tmstpalog
      es_exception      = es_exception
    CHANGING
      cv_start_date     = cv_start_date
      cv_start_time     = cv_start_time
      cv_end_date       = cv_end_date
      cv_end_time       = cv_end_time
    EXCEPTIONS
      alert             = 1
      OTHERS            = 2.

  CASE sy-subrc.
    WHEN 1.
      RAISE alert.
    WHEN 2.
      RAISE others.
  ENDCASE.
ENDFUNCTION.

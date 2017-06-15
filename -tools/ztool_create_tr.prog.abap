*&---------------------------------------------------------------------*
*& Report ZTOOL_CREATE_TR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztool_create_tr.

DATA:
  BEGIN OF ls_rfcdest,
    mandt TYPE symandt,
    type  TYPE flag,
    dx2   TYPE rfcdest,
    dxe   TYPE rfcdest,
  END OF ls_rfcdest,

  BEGIN OF gs_req,
    datum     TYPE sydatum,
    typeot    TYPE c LENGTH 03,
    ihm_sap   TYPE c LENGTH 01,
    modul_sap TYPE c LENGTH 03,
    spcdfc    TYPE c LENGTH 10,
    chrono    TYPE c LENGTH 02,
    freetxt   TYPE as4text,
  END OF gs_req,

  gv_dummy TYPE flag.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
PARAMETERS: p_dx2 TYPE trkorr.
PARAMETERS: p_sync AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN SKIP.
PARAMETERS: p_k RADIOBUTTON GROUP a1 DEFAULT 'X'.
PARAMETERS: p_w RADIOBUTTON GROUP a1.
PARAMETERS: p_proj TYPE tr_extpid DEFAULT 'ATLAS_L2A' OBLIGATORY.
PARAMETERS: p_text TYPE as4text OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b01.

INITIALIZATION.

  IF sy-mandt EQ '100'.
    p_dx2 = 'DX2K903797'.
  ENDIF.

AT SELECTION-SCREEN.

  CASE abap_true.
    WHEN p_k.
      ls_rfcdest-type = 'K'.
      ls_rfcdest-mandt = '100'.

    WHEN p_w.
      ls_rfcdest-type = 'W'.
      ls_rfcdest-mandt = '110'.

  ENDCASE.

  CONCATENATE 'DX2CLNT' ls_rfcdest-mandt INTO ls_rfcdest-dx2.
  CONCATENATE 'DXECLNT' ls_rfcdest-mandt INTO ls_rfcdest-dxe.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'DIS'. screen-input = 0.
      WHEN OTHERS.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_text.
  DATA: ls_req TYPE  trwbo_request_header.

  CALL FUNCTION 'TR_F4_REQUESTS'
    EXPORTING
*     iv_username         = SY-UNAME    " User name
*     iv_trkorr_pattern   =     " Generic template for request number
      iv_trfunctions      = ls_rfcdest-type   " Request types (string from TRFUNCTIONs)
*     iv_trstatus         =     " Request status (string from TRSTATUSes)
*     iv_from_date        =
*     iv_to_date          =
      iv_client           = ls_rfcdest-mandt     " Source client
*     iv_project          =
*     iv_title            =     " Text for title line
*     iv_via_selection_screen = 'X'
*     iv_complete_requests    = 'X'
*     it_exclude_requests =     " Table of Request Numbers
    IMPORTING
*     ev_selected_request =     " Selected request
      es_selected_request = ls_req.

  CLEAR gs_req.
  IF ls_req-as4text NE ''.
    SPLIT ls_req-as4text AT '-' INTO gv_dummy
                                     gs_req-typeot
                                     gs_req-ihm_sap
                                     gs_req-modul_sap
                                     gs_req-spcdfc
                                     gs_req-chrono
                                     gs_req-freetxt.
    ADD 1 TO gs_req-chrono.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_req-chrono    " C field
      IMPORTING
        output = gs_req-chrono.    " Internal display of INPUT, any category

    CONCATENATE: sy-datum
                 gs_req-typeot
                 gs_req-ihm_sap
                 gs_req-modul_sap
                 gs_req-spcdfc
                 gs_req-chrono
                 gs_req-freetxt
                 INTO p_text SEPARATED BY '-'.
  ENDIF.

START-OF-SELECTION.

  DATA: gs_proj  TYPE bapiscts03,
        gt_user  TYPE STANDARD TABLE OF bapiscts12,
        gt_req   TYPE STANDARD TABLE OF bapiscts07,
        gv_dxe   TYPE bapiscts01-requestid,
        gs_layer TYPE bapiscts02.

  gs_proj-projecttyp = 'SAP_IMG_PS'.
  gs_proj-project_nr = p_proj.

  APPEND INITIAL LINE TO gt_user ASSIGNING FIELD-SYMBOL(<ls_user>).
  <ls_user>-task_owner = sy-uname.

  IF p_text NE ''.
    IF p_sync EQ abap_true.
      IF p_dx2 NE ''.

        CALL FUNCTION 'TR_READ_COMM' DESTINATION ls_rfcdest-dx2
          EXPORTING
            wi_trkorr        = p_dx2    " Request/Task
            wi_sel_e070      = abap_true    " Read E070
          EXCEPTIONS
            not_exist_e070   = 1
            no_authorization = 2
            OTHERS           = 3.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.
      ELSE.
        gs_layer = 'XDX2'.
        CALL FUNCTION 'BAPI_CTREQUEST_CREATE' DESTINATION ls_rfcdest-dx2
          EXPORTING
            request_type = ls_rfcdest-type   " W = Customizing request, otherwise Workbench request
            author       = sy-uname    " Author of request to be created
            text         = p_text    " Short text for the request to be created
            translayer   = gs_layer     " Transport layer of request
            project      = gs_proj    " Project assignment for the request
          IMPORTING
            requestid    = p_dx2     " Number of the created request
*           header       =     " Header information for request
*           return       =     " Return structure of the errors
          TABLES
            authorlist   = gt_user    " List of task authors and task types
            task_list    = gt_req
          EXCEPTIONS
            OTHERS       = 1.     " List of header information for the tasks

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
    gs_layer = 'XDXE'.
    CALL FUNCTION 'BAPI_CTREQUEST_CREATE' DESTINATION ls_rfcdest-dxe
      EXPORTING
        request_type = ls_rfcdest-type    " W = Customizing request, otherwise Workbench request
        author       = sy-uname    " Author of request to be created
        text         = p_text    " Short text for the request to be created
        translayer   = gs_layer   " Transport layer of request
        project      = gs_proj    " Project assignment for the request
      IMPORTING
        requestid    = gv_dxe     " Number of the created request
*       header       =     " Header information for request
*       return       =     " Return structure of the errors
      TABLES
        authorlist   = gt_user    " List of task authors and task types
        task_list    = gt_req
      EXCEPTIONS
        OTHERS       = 1.     " List of header information for the tasks

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    IF p_sync EQ abap_true.
      WRITE: / 'DX2 :', p_dx2.
      DATA: lt_attr TYPE trwbo_t_e070a.
      APPEND INITIAL LINE TO lt_attr ASSIGNING FIELD-SYMBOL(<ls_attr>).

      <ls_attr>-attribute = 'ZPROJECTTR'.
      <ls_attr>-reference = p_dx2.

      CALL FUNCTION 'TRINT_APPEND_ATTRIBUTES'
        EXPORTING
          iv_request        = gv_dxe
          it_attributes     = lt_attr
          iv_allow_ext_attr = abap_true
*      IMPORTING
*         et_attributes     =
        EXCEPTIONS
          invalid_attribute = 1
          db_access_error   = 2
          OTHERS            = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
    WRITE: / 'DXE :', gv_dxe.
  ENDIF.

END-OF-SELECTION.

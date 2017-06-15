TYPE-POOLS: icon, triwb, slis.

TYPES : ty_tmslog     TYPE ZTMS_ANALYSE_LOG,
        tyt_tmslog    TYPE TABLE OF ZTMS_ANALYSE_LOG,
        ty_trlog      TYPE ZTMS_TR_LOG,
        tyt_trlog     TYPE TABLE OF ZTMS_TR_LOG,
        ty_table_indx TYPE indx.


TYPES: BEGIN OF ty_parameter,
         variable TYPE char8,
         data     TYPE REF TO data,
       END OF ty_parameter,
       tyt_parameters TYPE TABLE OF ty_parameter.

TYPES: BEGIN OF ty_variante,
         repid TYPE sy-repid,
         varid TYPE char8,
*tyt_parameters .
       END OF ty_variante.

TYPES: BEGIN OF ty_key_suivi,
         idx_exp_src    TYPE i,
         trkorr_exp_src TYPE trkorr,
         as4text        TYPE tmstpalog-as4text,
         project        TYPE tr_extpid,
       END OF ty_key_suivi,

       BEGIN OF ty_syst_suivi,
         date  TYPE datum,
         time  TYPE uzeit,
         cnt   TYPE i,
         first TYPE i,
         last  TYPE i,
       END OF ty_syst_suivi.

TYPES: BEGIN OF ty_alv_comment.
    INCLUDE TYPE ty_trlog.
TYPES: text TYPE string,
       more TYPE string,
       END OF ty_alv_comment,
       tyt_alv_comments TYPE TABLE OF ty_alv_comment.

TYPES: BEGIN OF ty_alv.
    INCLUDE TYPE ty_key_suivi.
TYPES :
  syst1 TYPE ty_syst_suivi,
  syst2 TYPE ty_syst_suivi,
  syst3 TYPE ty_syst_suivi,
  syst4 TYPE ty_syst_suivi,
  syst5 TYPE ty_syst_suivi,
  syst6 TYPE ty_syst_suivi,
  syst7 TYPE ty_syst_suivi,
  syst8 TYPE ty_syst_suivi,
  syst9 TYPE ty_syst_suivi.

TYPES: t_color TYPE slis_t_specialcol_alv,
       celltab TYPE lvc_t_styl,
       comment TYPE i,
       END OF ty_alv,
       tyt_alv TYPE TABLE OF ty_alv
       .

TYPES: BEGIN OF ty_list_sys,
         idx(1)        TYPE n,
         system        TYPE tmssysnam,
         struct_txt(5) TYPE c,
         rfcdest       TYPE rfcdest,
         domadest      TYPE domname,
         datefrom      TYPE datum,
       END OF ty_list_sys,

       tyt_list_sys TYPE SORTED TABLE OF ty_list_sys
                    WITH UNIQUE KEY idx.

TYPES : tyt_log    TYPE tmstpalogs,
        ty_log     TYPE LINE OF tyt_log,
        ty_seq(5)  TYPE n,
        ty_indx(1) TYPE n,
        tyr_trkorr TYPE lcl_tms_analyse=>typ_r_trkorr.

CONSTANTS : gc_syst1(5)      TYPE c VALUE 'SYST1',
            gc_syst2(5)      TYPE c VALUE 'SYST2',
            gc_syst3(5)      TYPE c VALUE 'SYST3',
            gc_syst4(5)      TYPE c VALUE 'SYST4',
            gc_syst5(5)      TYPE c VALUE 'SYST5',
            gc_syst6(5)      TYPE c VALUE 'SYST6',
            gc_syst7(5)      TYPE c VALUE 'SYST7',
            gc_syst8(5)      TYPE c VALUE 'SYST8',
            gc_syst9(5)      TYPE c VALUE 'SYST9',

            gc_zero          TYPE i VALUE 0,
            gc_gblue         TYPE i VALUE 1,
            gc_lgray         TYPE i VALUE 2,
            gc_yello         TYPE i VALUE 3,
            gc_bgree         TYPE i VALUE 4,
            gc_green         TYPE i VALUE 5,
            gc_red           TYPE i VALUE 6,
            gc_purpl         TYPE i VALUE 7,

            gc_inten         TYPE i VALUE 1,
            gc_inver         TYPE i VALUE 1,

            gc_textmode_edit TYPE flag VALUE 'E',
            gc_textmode_show TYPE flag VALUE 'S',
            gc_textmode_new  TYPE flag VALUE 'N'
            .

FIELD-SYMBOLS:<fs_log> TYPE ty_log.

DATA: gt_comments TYPE tyt_alv_comments.

DATA: gt_alv      TYPE tyt_alv,
      gv_datefrom TYPE datum VALUE '20010101',
      gv_dateto   TYPE datum VALUE '99991231',
      gt_tmslog   TYPE tyt_tmslog,
      gt_trlog    TYPE tyt_trlog.

DATA: gt_list_sys TYPE tyt_list_sys.

DATA: ok_code   LIKE sy-ucomm,
      save_ok   LIKE sy-ucomm,
      gv_trkorr TYPE trkorr,
      gv_lib    TYPE e07t-as4text.


DATA: gv_editor_length TYPE i VALUE 50,
      editor_container TYPE REF TO cl_gui_custom_container,
      text_editor      TYPE REF TO cl_gui_textedit,
      gv_text          TYPE string,
      gv_textmode      TYPE flag.

SELECTION-SCREEN BEGIN OF SCREEN 1200 AS SUBSCREEN NESTING LEVEL 4.
SELECT-OPTIONS: s_trkorr FOR gv_trkorr,
                s_lib FOR gv_lib.
SELECTION-SCREEN END OF SCREEN 1200.

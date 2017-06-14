FUNCTION-POOL ZTMS_ANALYSE.                        "MESSAGE-ID ..

TYPES: tyt_trkorr TYPE TABLE OF trkorr,
       tyt_e071 TYPE TABLE OF e071,
       tyr_obj_typ TYPE RANGE OF trobjtype,
       tyr_trkorr TYPE RANGE OF trkorr.

TYPES : BEGIN OF ty_trkorr_obj,
          pgmid    TYPE pgmid,
          object   TYPE trobjtype,
          obj_name TYPE trobj_name,
          trkorr TYPE trkorr,
        END OF ty_trkorr_obj,
        tyt_trkorr_obj TYPE TABLE OF ty_trkorr_obj.


* INCLUDE LZACND...                          " Local class definition
*INCLUDE lzacnfrm.

CONSTANTS:  gc_esc              VALUE '"'.

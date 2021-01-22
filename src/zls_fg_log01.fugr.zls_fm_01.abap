FUNCTION ZLS_FM_01.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IJ_OBJEKT) TYPE  STRING
*"     VALUE(IV_TRFCQNAM) TYPE  TRFCQNAM OPTIONAL
*"----------------------------------------------------------------------
*  IF iv_trfcqnam IS NOT INITIAL.
*
*    CALL FUNCTION 'TRFC_SET_QIN_PROPERTIES'
*      EXPORTING
*        qin_name           = iv_trfcqnam
*      EXCEPTIONS
*        invalid_queue_name = 1
*        error_message      = 1
*        OTHERS             = 2.
*
*    CALL FUNCTION '/ZRG/UTILS_LOGGING_SAVE' IN BACKGROUND TASK
*      EXPORTING
*        ij_objekt = ij_objekt.

*    COMMIT WORK.

*  ELSE.
*
    zls_cl_log=>_rfc_call_save( ij_objekt ).


*  ENDIF.


ENDFUNCTION.

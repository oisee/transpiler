*&---------------------------------------------------------------------*
*& Modern ABAP translation from JavaScript Fibonacci function
*& Original JavaScript:
*& function fibonacci(n) {
*&   if (n <= 1) return n;
*&   return fibonacci(n - 1) + fibonacci(n - 2);
*& }
*&---------------------------------------------------------------------*

CLASS lcl_math_utilities DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: fibonacci
      IMPORTING
        iv_n TYPE i
      RETURNING
        VALUE(rv_result) TYPE i.
ENDCLASS.

CLASS lcl_math_utilities IMPLEMENTATION.
  METHOD fibonacci.
    " Base case: fibonacci(0) = 0, fibonacci(1) = 1
    rv_result = COND #( 
      WHEN iv_n <= 1 
      THEN iv_n
      ELSE fibonacci( iv_n - 1 ) + fibonacci( iv_n - 2 ) 
    ).
  ENDMETHOD.
ENDCLASS.

" Test program
START-OF-SELECTION.
  DATA(lv_input) = 8.
  DATA(lv_result) = lcl_math_utilities=>fibonacci( lv_input ).
  
  WRITE: / |Fibonacci({ lv_input }) = { lv_result }|.
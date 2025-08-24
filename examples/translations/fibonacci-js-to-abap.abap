* Generated ABAP translation from JavaScript Fibonacci function
* Original JavaScript:
* function fibonacci(n) {
*   if (n <= 1) return n;
*   return fibonacci(n - 1) + fibonacci(n - 2);
* }

FORM fibonacci USING p_n TYPE i CHANGING p_result TYPE i.
  DATA: lv_temp1 TYPE i,
        lv_temp2 TYPE i.
        
  IF p_n LE 1.
    p_result = p_n.
    EXIT.
  ENDIF.
  
  " Calculate fibonacci(n-1)
  PERFORM fibonacci USING p_n - 1 CHANGING lv_temp1.
  
  " Calculate fibonacci(n-2) 
  PERFORM fibonacci USING p_n - 2 CHANGING lv_temp2.
  
  " Return sum
  p_result = lv_temp1 + lv_temp2.
ENDFORM.

" Test the fibonacci function
DATA: lv_input TYPE i VALUE 8,
      lv_result TYPE i.

PERFORM fibonacci USING lv_input CHANGING lv_result.

WRITE: / 'Fibonacci(', lv_input, ') =', lv_result.
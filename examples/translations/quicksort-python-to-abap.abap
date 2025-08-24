* Generated ABAP translation from Python quicksort function
* Original Python:
* def quicksort(arr):
*     if len(arr) <= 1:
*         return arr
*     pivot = arr[len(arr) // 2]
*     left = [x for x in arr if x < pivot]
*     middle = [x for x in arr if x == pivot]  
*     right = [x for x in arr if x > pivot]
*     return quicksort(left) + middle + quicksort(right)

TYPES: BEGIN OF ty_int_table,
         value TYPE i,
       END OF ty_int_table.
       
TYPES: tt_int_table TYPE TABLE OF ty_int_table.

FORM quicksort USING pt_arr TYPE tt_int_table
               CHANGING pt_result TYPE tt_int_table.
               
  DATA: lv_size TYPE i,
        lv_pivot_index TYPE i,
        lv_pivot_value TYPE i,
        lt_left TYPE tt_int_table,
        lt_middle TYPE tt_int_table,
        lt_right TYPE tt_int_table,
        lt_sorted_left TYPE tt_int_table,
        lt_sorted_right TYPE tt_int_table,
        ls_element TYPE ty_int_table.
        
  " Clear result table
  CLEAR pt_result.
  
  " Get array size
  lv_size = lines( pt_arr ).
  
  " Base case: array with 0 or 1 element
  IF lv_size LE 1.
    pt_result = pt_arr.
    EXIT.
  ENDIF.
  
  " Choose pivot (middle element)
  lv_pivot_index = lv_size DIV 2.
  READ TABLE pt_arr INTO ls_element INDEX lv_pivot_index.
  lv_pivot_value = ls_element-value.
  
  " Partition array into left, middle, and right
  LOOP AT pt_arr INTO ls_element.
    IF ls_element-value LT lv_pivot_value.
      APPEND ls_element TO lt_left.
    ELSEIF ls_element-value EQ lv_pivot_value.
      APPEND ls_element TO lt_middle.
    ELSE.
      APPEND ls_element TO lt_right.
    ENDIF.
  ENDLOOP.
  
  " Recursively sort left and right partitions
  PERFORM quicksort USING lt_left CHANGING lt_sorted_left.
  PERFORM quicksort USING lt_right CHANGING lt_sorted_right.
  
  " Combine results: sorted_left + middle + sorted_right
  LOOP AT lt_sorted_left INTO ls_element.
    APPEND ls_element TO pt_result.
  ENDLOOP.
  
  LOOP AT lt_middle INTO ls_element.
    APPEND ls_element TO pt_result.
  ENDLOOP.
  
  LOOP AT lt_sorted_right INTO ls_element.
    APPEND ls_element TO pt_result.
  ENDLOOP.
  
ENDFORM.

" Test the quicksort function
DATA: lt_numbers TYPE tt_int_table,
      lt_sorted TYPE tt_int_table,
      ls_number TYPE ty_int_table.

" Initialize test array: [3, 1, 4, 1, 5, 9, 2, 6, 5]
ls_number-value = 3. APPEND ls_number TO lt_numbers.
ls_number-value = 1. APPEND ls_number TO lt_numbers.
ls_number-value = 4. APPEND ls_number TO lt_numbers.
ls_number-value = 1. APPEND ls_number TO lt_numbers.
ls_number-value = 5. APPEND ls_number TO lt_numbers.
ls_number-value = 9. APPEND ls_number TO lt_numbers.
ls_number-value = 2. APPEND ls_number TO lt_numbers.
ls_number-value = 6. APPEND ls_number TO lt_numbers.
ls_number-value = 5. APPEND ls_number TO lt_numbers.

" Sort the array
PERFORM quicksort USING lt_numbers CHANGING lt_sorted.

" Display results
WRITE: / 'Original array:'.
LOOP AT lt_numbers INTO ls_number.
  WRITE ls_number-value.
ENDLOOP.

WRITE: / 'Sorted array:'.
LOOP AT lt_sorted INTO ls_number.
  WRITE ls_number-value.
ENDLOOP.
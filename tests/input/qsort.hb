{

  //standard quicksort
  qsort = fun arr, lo, hi : {
    if lo < hi then {
      p = partition(arr, lo, hi);
      qsort(arr, lo, p - 1);
      qsort(arr, p + 1, hi)
    } else 0
  };

  swap = fun arr, l, r : {
    temp = arr[l];
    arr[l] = arr[r];
    arr[r] = temp;
    arr
  };
  
  //quicksort partition
  partition = fun arr, lo, hi : {
    index = Int((lo + hi) / 2);
    pivot = arr[index];
    swap(arr, hi, index);
    store = lo;
    i = lo;
    while (i < hi) 
    {
      if (arr[i] <= pivot) then
      {
        swap(arr, i, store);
        store = store + 1
      }
      else [];
      i = i + 1
    }; 
    swap(arr, hi, store);
    store
  };
  
  test = [12, 1, 5, 3, 8, -1, 0];
  qsort(test, 0, len(test) - 1);
  print(test)
}

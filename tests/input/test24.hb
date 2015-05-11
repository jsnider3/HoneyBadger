{
  //Test conversion to bools.
  if Bool(1) then print(true) else print(false);
  if Bool(0) then print(true) else print(false);
  if Bool("true") then print(true) else print(false);
  if Bool("false") then print(true) else print(false);
  if Bool(3.5) then print(true) else print(false);
  if Bool(0.0) then print(true) else print(false);
  if Bool([1,2,3,4]) then print(true) else print(false);
  if Bool([]) then print(true) else print(false)
}

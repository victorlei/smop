{ x[NR] = $0; } END {
  print "@multitable @columnfractions .33 .33 .33";
  rem = NR % 3;
  n = NR - rem;
  for (i = 1; i <= n; i += 3)
    printf ("@item %s @tab %s @tab %s\n", x[i], x[i+1], x[i+2]);
  if (rem == 1)
    printf ("@item %s\n", x[NR]);
  else if (rem == 2)
    printf ("@item %s @tab %s\n", x[NR-1], x[NR]);
  print "@end multitable";
}

#!/bin/bash
# author: jianingy.yang@gmail.com

echo -e "\033[38;05;240m"---------------------------------------------------------------

for code in {0..255}
do 
  echo -ne "\033[38;05;${code}m"
  printf "%03d" $code
  echo -ne " "
  [ "$(($code % 16))" -eq "15" ] && echo 
done

echo -e "\033[38;05;240m"---------------------------------------------------------------

for high in {0..1}
do
  echo -n "  "
  for code in {31..38}
  do 
    echo -ne "\033[${high};${code}m"
    printf "%1d;%02d\t" $high $code
    echo -ne " "
  done
  echo
done

echo -e "\033[0m"


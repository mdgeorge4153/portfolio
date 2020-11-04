#!/bin/bash

subdir="Submissions"
tmpdir="tmpsubs"
delimiter="@"

cp -rdf $subdir $tmpdir;

cd $tmpdir;

#This helps me divide them into more different comment files
#CMS likes that better
basefile="../comments"
for j in {1..6};
do
  echo "NetID,Style,@Int, Integer, Float@,Root23,FieldOfFractions,Rationals + Rot15,Reals,Geometry,Game,Written,Adjustment,Total,Add Comments" > "$basefile$j.csv";
done

j=1;
num=0;
file="$basefile$j.csv";

for i in $(ls);
do
  ((num++));
  if [ $num -eq 50 ];
  then
    num=0;
    ((j++));
    file="$basefile$j.csv";
  fi;
  echo -n "$i,,,,,,,,,,,,$delimiter" >> $file;
  for x in $(ls "$i/$i"_*.txt);
  do
    if [ -a $x ];
    then
      echo -e "\n$(basename $x | cut -d '_' --fields=2 | cut -d '.' --fields=1):\n" >> $file; 
      if [ $(file $x | grep -c UTF-16) -eq 1 ];
        then
          iconv -f UTF-16LE $x | uni2ascii > tmp;
          printf "%s\n" "$(< tmp)" >> $file;
        else
          printf "%s\n" "$(< $x)" >> $file;
      fi;
    fi;
  done;
  echo "$delimiter" >> $file;
done

cd ..;

rm -rf $tmpdir;

#Once all comment files are created, you want to open them in libreoffice
#Note that the delimiter is an @ sign, because I assumed people wouldn't use them.
#Save them as a ods, and then resave them as a csv (with a " delimiter as usual).
#That way libreoffice takes care of formatting.

exit 0;

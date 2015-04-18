rm ./handin/*
cp eval.h ./handin
cp eval.c ./handin
cp tables.h ./handin
cp tables.c ./handin
cp bad*.simple ./handin
cp good*.simple ./handin
cp *.in ./handin
cp *.out ./handin
cp *.err ./handin
cp control*.simple ./handin
cd handin
rm bad5*
rm bad6*
tar czvf handin.tar *
tar tvf handin.tar

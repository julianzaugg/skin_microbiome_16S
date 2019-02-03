# Run in /Result_tables/DESeq_results

ls -1 *.csv | head -n 1 | xargs head -n 1 | sed "s/^,/Comparison,OTU.ID,/" > all

rm DESeq_results_combined.csv
rm temp
for i in *.csv; do

reduced_name=${i%.csv}
cat $i | grep -v "baseMean" | sed "s/^/$reduced_name,/g" >> temp

done
mv all DESeq_results_combined.csv
sort -g -t ',' -k 8,8 temp >> DESeq_results_combined.csv
rm temp
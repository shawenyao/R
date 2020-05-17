:: set path for the rscript.exe file
set rscript=C:\"Program Files"\R\R-3.5.0\bin\rscript.exe
:: run rscripts in parallel
start %rscript% cmd_example.R -arg1 job1 -arg2 value1.2 -arg3 value1.3
start %rscript% cmd_example.R -arg1 job2 -arg2 value2.2 -arg3 value2.3
start %rscript% cmd_example.R -arg1 job3 -arg2 value3.2 -arg3 value3.3
:: prevent the main batch window from auto-exit
pause
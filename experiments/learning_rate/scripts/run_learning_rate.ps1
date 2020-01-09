$script="../experiments/learning_rate/scripts/run_learning_rate.pl"
$to_script_from_learners="../experiments/learning_rate/scripts"
$louise_root="../../../louise"
$thelma_root="../../../thelma"
$metagol_root="../../../metagol"

. cd $louise_root
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_kin -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_mtg_fragment -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_robots -t halt

. cd $to_script_from_learners

#. cd $thelma_root
#& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_kin -t halt
#& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_mtg_fragment -t halt
#& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_robots -t halt
#
#. cd $to_script_from_learners

. cd $metagol_root
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_kin -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_mtg_fragment -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_robots -t halt

. cd $to_script_from_learners

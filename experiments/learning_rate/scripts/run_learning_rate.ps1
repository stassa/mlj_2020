$script="../experiments/learning_rate/scripts/run_learning_rate.pl"
$to_script_from_learners="../experiments/learning_rate/scripts"
$louise_root="../../../louise"
$metagol_root="../../../metagol"

. cd $louise_root

& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_kin -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_mtg_fragment -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g 'write_dataset(robots)' -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_robots -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_connected_ambiguities -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_connected_false_positives -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_connected_false_negatives -t halt

. cd $to_script_from_learners

. cd $metagol_root

& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g 'write_dataset(kin)' -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g 'write_dataset(mtg_fragment)' -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g 'write_dataset(robots)' -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g 'write_dataset(connected_ambiguities)' -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g 'write_dataset(connected_false_positives)' -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g 'write_dataset(connected_false_negatives)' -t halt

& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_kin -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_mtg_fragment -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_robots -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_connected_ambiguities -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_connected_false_positives -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_connected_false_negatives -t halt

. cd $to_script_from_learners

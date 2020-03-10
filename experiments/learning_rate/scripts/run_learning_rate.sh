# Runs all scripts concurrently - debug output will be a bit confusing. 
script="../experiments/learning_rate/scripts/run_learning_rate.pl"
to_script_from_learners="../experiments/learning_rate/scripts"
louise_root="../../../louise"
metagol_root="../../../metagol"

cd $louise_root

swipl -s $script -g run_kin -t halt &
swipl -s $script -g run_mtg_fragment -t halt &
swipl -s $script -g run_robots -t halt &

cd $to_script_from_learners

cd $metagol_root

swipl -s $script -g 'write_dataset(kin)' -t halt
swipl -s $script -g 'write_dataset(mtg_fragment)' -t halt
swipl -s $script -g 'write_dataset(robots)' -t halt

swipl -s $script -g run_kin -t halt &
swipl -s $script -g run_mtg_fragment -t halt &
swipl -s $script -g run_robots -t halt &

cd $to_script_from_learners

script="../experiments/learning_rate/scripts/run_learning_rate.pl"
to_script_from_learners="../experiments/learning_rate/scripts"
louise_root="../../../louise"
thelma_root="../../../thelma"

cd $louise_root

swipl -s $script -g run_kin -t halt
swipl -s $script -g run_mtg_fragment -t halt
swipl -s $script -g run_robots -t halt

cd $to_script_from_learners

cd $thelma_root
swipl -s $script -g run_kin -t halt
swipl -s $script -g run_mtg_fragment -t halt
swipl -s $script -g run_robots -t halt

cd $to_script_from_learners


##root="/data2/ep2216/projects/phd_stuff/"
##script="scripts/experiments/learning_rate/run_learning_rate.pl"
##swipl -s $root"louise/"$script -g run_kin -t halt
##swipl -s $root"thelma/"$script -g run_kin -t halt
#
## Must be run as separate processes to avoid permission errors when importing
## experiment file interface predicates
## Louise
#louise_root="/data2/ep2216/projects/phd_stuff/louise"
#cd $louise_root
#pwd
#echo "Running learning rate experiment on kin.pl"
#swipl -s scripts/experiments/learning_rate/run_learning_rate.pl -g run_kin -t halt
#echo "Running learning rate experiment on mtg_fragment.pl"
#swipl -s scripts/experiments/learning_rate/run_learning_rate.pl -g run_mtg_fragment -t halt
#echo "Running learning rate experiment on robots.pl"
#swipl -s scripts/experiments/learning_rate/run_learning_rate.pl -g run_robots -t halt
### Thelma
#thelma_root="/data2/ep2216/projects/phd_stuff/thelma"
#cd $thelma_root
#pwd
#echo "Running learning rate experiment on kin.pl"
#swipl -s scripts/experiments/learning_rate/run_learning_rate.pl -g run_kin -t halt
#echo "Running learning rate experiment on mtg_fragment.pl"
#swipl -s scripts/experiments/learning_rate/run_learning_rate.pl -g run_mtg_fragment -t halt
#echo "Running learning rate experiment on robots.pl"
#swipl -s scripts/experiments/learning_rate/run_learning_rate.pl -g run_robots -t halt

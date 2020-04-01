# Script to run experiments on Windows with powershell
# NOTE Experiments can take a long time to run, especially the path_*
# experiments. If you only want to test that every test runs correctly and
# generates the expected output edit the run_learning_rate.pl script in the
# same directory as this script and change the experiment parameters for the
# number of cycles and steps of each experiment. The .pl script describes how
# to do this in more detail.
$script="../experiments/learning_rate/scripts/run_learning_rate.pl"
$to_script_from_learners="../experiments/learning_rate/scripts"
$louise_root="../../../louise"
$metagol_root="../../../metagol"

# The Prolog script that actually runs the experiment, run_learning_rate.pl,
# must be executed in the root directory of the $louise_root or $metagol_root
# directories to find necessary files in expected paths.
. cd $louise_root

# Generate grid world tasks and primitive moves for grid world experiement and
# auto-generated datasets for graph-connectedness experiments.
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g 'generate_dataset(robots)' -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g 'generate_dataset(path_ambiguities)' -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g 'generate_dataset(path_false_positives)' -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g 'generate_dataset(path_false_negatives)' -t halt

# Run experiments with Louise
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_kin -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_mtg_fragment -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_robots -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_path_ambiguities -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_path_false_positives -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_path_false_negatives -t halt

. cd $to_script_from_learners

. cd $metagol_root

# Generate Metagol datasets from Louise experiment files (including generated
# grid world and path files)
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g 'write_dataset(kin)' -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g 'write_dataset(mtg_fragment)' -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g 'write_dataset(robots)' -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g 'write_dataset(path_ambiguities)' -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g 'write_dataset(path_false_positives)' -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g 'write_dataset(path_false_negatives)' -t halt

# Run experiments with Metagol
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_kin -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_mtg_fragment -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_robots -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_path_ambiguities -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_path_false_positives -t halt
& 'C:\Program Files\swipl\bin\swipl.exe' -s $script -g run_path_false_negatives -t halt

. cd $to_script_from_learners

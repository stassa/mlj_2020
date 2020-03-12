#================================================================================
# ***** RUNNING THIS SCRIPT WILL DELETE THE OUTPUT OF COMPLETED EXPERIMENTS *****
#================================================================================
#
# This is a bash script to delete output directories and their contents on
# Windows. It is meant to be used to cleanup after testing experiments' setup.
# It _will_ remove the output of completed experiments _without_ warnings. So
# either a) make sure you backup your experiment outputs in a directory other
# than the directories affected by this script (listed below) or, b) don't run
# this script when there is data you need in one of the directories or files to
# be deleted, listed below.

louise_root="../../../louise"
metagol_root="../../../metagol"
experiment_root=".."
data_root="../../../data"

# Directories and files to be deleted.
louise_output="$louise_root/output"
metagol_output="$metagol_root/output"
metagol_experiment_data="$metagol_root/data/experiment"
experiment_output="$experiment_root/output/"
robots_world="$data_root/robots/worlds/"

rm -rf "$louise_output/testing"
rm -rf "$metagol_output/testing"
rm -rf $metagol_experiment_data
rm -rf $experiment_output
rm -rf "$robots_world*.pl"

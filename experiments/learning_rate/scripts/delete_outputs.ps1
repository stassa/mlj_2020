#================================================================================
# ***** RUNNING THIS SCRIPT WILL DELETE THE OUTPUT OF COMPLETED EXPERIMENTS *****
#================================================================================
#
# This is a Powershell script to delete output directories and their contents
# on Windows. It is meant to be used to cleanup after testing experiments'
# setup. It _will_ remove the output of completed experiments _without_
# warnings. So either a) make sure you backup your experiment outputs in a
# directory not affected by this script (listed below) or, b) don't run this
# script when there is data you need in one of the directories or files to be
# deleted, listed below.

$louise_root="../../../louise"
$metagol_root="../../../metagol_2"
$experiment_root="../"
$data_root="../../../data"

# Directories and files to be deleted.
$louise_output="$louise_root/output"
$metagol_output="$metagol_root/output"
$metagol_experiment_data="$metagol_root/data/experiment"
$experiment_output="$experiment_root/output/"
$robots_world="$data_root/robots/worlds/"

# Uncomment the -WhatIf parameters to do a dry run.
if (Test-Path "$louise_output/testing")
{
	Remove-Item "$louise_output/testing" -Recurse #-WhatIf
}

if (Test-Path "$metagol_output/testing")
{
	Remove-Item "$metagol_output/testing" -Recurse #-WhatIf
}

if (Test-Path $metagol_experiment_data)
{
	Remove-Item $metagol_experiment_data -Recurse #-WhatIf
}

if (Test-Path $experiment_output)
{
	Remove-Item $experiment_output -Recurse #-WhatIf
}

if (Test-Path "$robots_world/*.pl")
{
	Remove-Item "$robots_world/*.pl" #-WhatIf
}

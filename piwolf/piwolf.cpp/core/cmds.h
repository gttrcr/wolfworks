#pragma once

//placeholders
#define PLACEHOLDER0 "#PWPH0#"
#define PLACEHOLDER1 "#PWPH1#"
#define PLACEHOLDER2 "#PWPH2#"
#define PLACEHOLDER3 "#PWPH3#"
#define PLACEHOLDER4 "#PWPH4#"
#define PLACEHOLDER5 "#PWPH5#"
#define PLACEHOLDER6 "#PWPH6#"
#define PLACEHOLDER7 "#PWPH7#"
#define PLACEHOLDER8 "#PWPH8#"
#define PLACEHOLDER9 "#PWPH8#"

//folders
#define FOLDER_INPUT "input/"
#define FOLDER_OUTPUT "output/"
#define FOLDER_PIWOLF_SCRIPT "script/"

//files
#define FILE_PIWOLF_PID_SCRIPT "piwolfpidscript"
#define FILE_PIWOLF_PID_ESTIMATED "piwolfpidestimated"

//commands
#define CMD_PID_OF_KERNELS "ps -aux | grep SCREEN | awk '{print $2}'"
#define CMD_OS_RESOURCES "grep -c ^processor /proc/cpuinfo && for (( i=0; i<`grep -c ^processor /proc/cpuinfo`; i++ ))\ndo\ncpuName='/cpu'$i && awk -v a=\"$(awk $cpuName' /{print $2+$4,$2+$4+$5}' /proc/stat; sleep 0.1)\" $cpuName' /{split(a,b,\" \"); print 100*($2+$4-b[1])/($2+$4+$5-b[2])}' /proc/stat;\ndone && free -h | sed -n 2p | awk '{print $3}' && vcgencmd measure_temp | cut -d'=' -f2 && vcgencmd get_throttled | cut -d'=' -f2"
#define CMD_ABORT_ALL "pkill -f wolframscript"
#define CMD_REBOOT "reboot"
#define CMD_SHUTDOWN "sudo shutdown -h now"
#define CMD_CLEAR_FOLDERS "rm -rf #PWPH0#input/* rm -rf #PWPH0#output/* rm -rf #PWPH0#store.json"
#define CMD_PROGRESS_OF_RUNNING "actual=$(ps -p #PWPH0# -oetime= | tr '-' ':' | awk -F: '{ total=0; m=1; } { for (i=0; i < NF; i++) {total += $(NF-i)*m; m *= i >= 2 ? 24 : 60 }} {print total}') && total=$(cat #PWPH1#piwolfpidestimated | grep #PWPH0# | tail -1 | awk '{print $2}') && echo $(($((1000000000*$actual))/$total))"

//#PWPH0# input file name (path+filename), #PWPH1# output file name (path+filename), #PWPH2# FILE_PIWOLF_PID_SCRIPT, #PWPH3# filename
#define CMD_START_ASYNC_WOLFRAM_KERNEL "pid=`screen -S wolfscreen -L -Logfile #PWPH1# -d -m wolframscript -f #PWPH0# && screen -ls | grep wolfscreen | tr '.' '\n' | head -n 1 | xargs` && echo $pid '#PWPH3#' >> #PWPH2# && echo $pid"
#define CMD_START_SYNC_WOLFRAM_KERNEL "wolframscript -f #PWPH0# > #PWPH1# && cat #PWPH1#"

//wolfram command
#define WOLFRAM_ABSOLUTE_TIMING "AbsoluteTiming[#PWPH0#]"
#define WOLFRAM_PRINT "Print[#PWPH0#]"
#define WOLFRAM_FIRST "First[#PWPH0#]"

//scripts used by piwolf
#define COMPLEXITY_FIT "complexityFit.nb"
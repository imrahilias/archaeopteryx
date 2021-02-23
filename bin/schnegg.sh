#!/bin/zsh
## "Slurm's control daemon's control daemon"

helpfun()
{
    echo -e "Slurm's control daemon's control daemon \n"
    echo -e "Usage: $0 OPTION \n"
    echo -e "Options:"
    echo -e "-d \t drain node"
    echo -e "-f \t flood node"
    echo -e "-h \t print this help message"
    echo -e "-p \t pause for 1h, then resume all (regardless if there were some jobs suspended for different reasons!)"
    echo -e "-r \t resume all jobs"
    echo -e "-s \t suspend all jobs"
}

coffeebreak()
{
        ## suspend all running jobs for a user
        squeue -ho %A -t R | xargs -n 1 sudo scontrol suspend &&

        ## drain node
        ##sudo scontrol update nodename=archaeopteryx state=drain reason=coffeebreak &&
        
        ## wait for 1h
        sleep 1h &&
        
        ## resume all suspended jobs for a user, so those get activated first
        squeue -o "%.18A %.18t" -u m | awk '{if ($2 =="S"){print $1}}' | xargs -n 1 sudo scontrol resume
        
        ## resume node
        ##sudo scontrol update nodename=archaeopteryx state=resume
}

# Print helpFunction in case parameters are empty
if [ "$#" != "1" ]
then
    echo "please specify exactly one command, not two. not zero, and especially not three!";
    helpfun
    exit 0
fi

case "$1" in
    -d | --drain) ## drain node
        sudo scontrol update nodename=archaeopteryx state=drain reason=slow
        ;;
    -f | --flood) ## resume node
        sudo scontrol update nodename=archaeopteryx state=resume
        ;;
    -h | --help)
	helpfun
	;;            
    -p | --pause)
        coffeebreak
	;;
    -r | --resume) ## resume all suspended jobs for user m
        squeue -o "%.18A %.18t" -u m | awk '{if ($2 =="S"){print $1}}' | xargs -n 1 sudo scontrol resume
	;;
    -s | --suspend) ## suspend all running jobs for user m
        squeue -ho %A -t R | xargs -n 1 sudo scontrol suspend
	;;
    -*)
	echo "Error: Unknown option: $1" >&2
        exit 0
	;;
esac



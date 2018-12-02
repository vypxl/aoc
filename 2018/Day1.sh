#! /bin/bash

# Part 1 - Just add them together with bc
echo "Solution for part 1:"
cat 1.in | xargs | bc


# Part 2 - Put all new frequencies into an associative array until a duplicate is encountered
freq=0
iters=0
declare -A seen

# Loop until solution is found
while true; do
    # Read every line
    while read f; do
        # Update current Frequency
        freq=$(($freq+$f))
        
        # Check if already encountered
        if [ ${seen["$freq"]} ]; then
            # Print solution and exit
            echo "Solution for part 2:"
            echo $freq
            echo "Took $iters iterations."
            exit 0
        else
            # Add frequency to seen
            seen["$freq"]=1
        fi
        iters=$(($iters+1))
    done < 1.in
done

# Solution 1: 445
# Solution 2: 219

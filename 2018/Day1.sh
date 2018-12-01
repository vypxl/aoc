#! /bin/bash

# Part 1 - Just add them together with bc
echo "Solution for part 1:"
cat input1.txt | xargs | bc


# Part 2 - Repeatedly create variables for each state until an already defined state is encountered
freq=0
iters=0

# Loop until solution is found
while true; do
    # Read every line
    while read f; do
        # Update current Frequency
        freq=$(($freq+$f))
        # New variable name
        name="v$(echo $freq | sed -e"s/\+//g" | sed -e"s/-/_/g")"
        
        # If name is already defined
        if eval "[ -n \"\$$name\" ]"; then
            # Print solution and exit
            echo "Solution for part 2:"
            echo $freq
            exit 0
        else
            # Define the new state
            eval "$name=1"
        fi
        iters=$(($iters+1))
    done < input1.txt
    echo "$iters"
done

# Solution 1: 445
# Solution 2: 219
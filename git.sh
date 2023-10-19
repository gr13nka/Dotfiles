#!/bin/bash
#now=$(date+"%T")
#
cd
./cp.sh
cd ~/alg
git add .
git commit -m "commit"
git push alg

cd ~/OOP
git add .
git commit -m "commit"
git push 

cd ~/evm
git add .
git commit -m "commit"
git push


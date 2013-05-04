#!/bin/sh

function clean() {
    rm tr.po tr.itf tr_tr.po tr_tr.itf
}

echo "Checking if goal_trans/3 can be used as the name of a goal translation..."
clean
# If you do not see any goal_from_goal_trans(_,_,_) term, there is a bug
ciaoc -c tr.pl | grep -q goal_from_goal_trans && \
  echo "OK" || \
  echo "Bug: goal translations with goal_trans/3 as name are ignored (or the wrong predicate is called)"
clean


#!/bin/bash

# filename   : with_venv.sh
# created at : 2013-12-07 11:27:18
# author     : Jianing Yang <jianingy@unitedstack.com>

VIRTUALENVWRAPPER_DIR=$HOME/.virtualenvs

if [ ! -d $VIRTUALENVWRAPPER_DIR ]; then
    echo "virtualenvwrapper environment doesn't exist"
    exit 1
fi

if [ $# -lt 2 ]; then
    echo "not enough argument"
    exit 2
fi

VIRTUALENV_NAME=$1
shift

VIRTUALENV_ACTIVATE="$VIRTUALENVWRAPPER_DIR/$VIRTUALENV_NAME/bin/activate"

if [ ! -f $VIRTUALENV_ACTIVATE ]; then
    echo "virtualenv $VIRTUALENV_NAME doesn't exist"
    exit 3
fi

source $VIRTUALENV_ACTIVATE && "$@"

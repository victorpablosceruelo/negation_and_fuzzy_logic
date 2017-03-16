CIAO_ENGINE=/home/erik/thesis/CiaoDE/ciao/engine

TARGET=wxCiao.so

SOURCES=CiaoFunctions.cpp\
CiaoApp.cpp\
CiaoFrame.cpp

CFLAGS=$(shell wx-config --cflags) -g -fPIC  -I$(CIAO_ENGINE)
LDFLAGS=$(shell wx-config --libs)


include make.default

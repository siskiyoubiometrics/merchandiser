# Builds one driver binary per oracle library plus the debug-state checker.
# Run ../build.sh first.
CC      ?= gcc
CFLAGS  ?= -O2 -Wall -Wextra -std=gnu11
BUILD   := $(abspath ../build)

all: nvel_driver_single nvel_driver_double check_debug

nvel_driver_single: nvel_driver.c $(BUILD)/libnvel_single.so
	$(CC) $(CFLAGS) -o $@ nvel_driver.c -L$(BUILD) -lnvel_single -Wl,-rpath,$(BUILD) -lm

nvel_driver_double: nvel_driver.c $(BUILD)/libnvel_double.so
	$(CC) $(CFLAGS) -DNVEL_DOUBLE -o $@ nvel_driver.c -L$(BUILD) -lnvel_double -Wl,-rpath,$(BUILD) -lm

check_debug: check_debug.c
	$(CC) $(CFLAGS) -o $@ check_debug.c -ldl

clean:
	rm -f nvel_driver_single nvel_driver_double check_debug

.PHONY: all clean

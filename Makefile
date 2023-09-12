# Makefile for SWI-Prolog project

# Variables
SRC = main.pl        # Replace with your Prolog source file
TARGET = out/main

# SWI-Prolog compiler and options
SWIPL = swipl
SWIPL_FLAGS = -nodebug -g true -O -q --toplevel=main --stand_alone=true

# Build rule
$(TARGET): $(SRC)
	$(SWIPL) $(SWIPL_FLAGS) -o $@ -c $<

# Run rule
run: $(TARGET)
	./$(TARGET)

# Clean rule
clean:
	rm -f $(TARGET)

# Phony targets
.PHONY: all clean

# Default target
all: $(TARGET)


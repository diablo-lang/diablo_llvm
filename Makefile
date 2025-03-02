# Set the name of the final executable and paths
TARGET = llvm_bin/output
LLVM_IR = llvm_bin/output.ll
SRC_DIR = src
EXAMPLES_DIR = examples
DUNE_EXEC = dune exec -- diablo
CLANG = clang

# The rule for running the compiler (diablo) and generating LLVM IR
$(LLVM_IR): $(EXAMPLES_DIR)/arithmetic.dbl
	$(DUNE_EXEC) $(EXAMPLES_DIR)/arithmetic.dbl

# Rule to compile the generated LLVM IR into a final executable
$(TARGET): $(LLVM_IR)
	$(CLANG) $(LLVM_IR) -o $(TARGET)

# Rule to run the final executable
run: $(TARGET)
	./$(TARGET)

# Default rule to build both the LLVM IR and the final executable
all: $(TARGET)

# Clean the build artifacts
clean:
	rm -f $(LLVM_IR) $(TARGET)

# Help command to display make targets
help:
	@echo "Makefile for compiling Diablo source code."
	@echo "Usage:"
	@echo "  make        - Generate LLVM IR and build the executable."
	@echo "  make clean  - Clean generated files."
	@echo "  make help   - Show this help message."

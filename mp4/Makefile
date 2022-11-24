MPDIR = $(LEVEL)

LLVM_DIR = /home/scarlet/CS426/llvm-14.0.0.src
LLC=$(LLVM_DIR)/build/bin/llc

src=$(LEVEL)/RegAllocSimple.cpp

$(LLC): $(src)
	cp $(src) $(LLVM_DIR)/lib/CodeGen
	# make -C $(LLVM_DIR)/build llc -j8
	ninja -C $(LLVM_DIR)/build llc -j8

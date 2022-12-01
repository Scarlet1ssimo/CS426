This feature is typically easy to implement.
First we need information about whether a virtual register live across a function call.
We assume every `regmask` corresponds a function call. Then we scan the MOs **in a reverse order** like this before going into each MBB:

```c++
using RegSet = DenseSet<uint>;
static bool set_union(RegSet &A, const RegSet &B) {
    bool changed = 0;
    for (auto q : B) {
        if (!A.contains(q)) {
            A.insert(q);
            changed = 1;
        }
    }
    return changed;
}
// ...

VirtRegAcrossFunction.clear();
RegSet AfterFunction, BeforeFunction;

for (MachineInstr &I : reverse(MBB)) {
    for (auto &MO : reverse(I.operands())) {
        if (MO.isReg() && MO.getReg().isVirtual()) {
            auto rv = MO.getReg().virtRegIndex();
            if (!BeforeFunction.contains(rv))
                AfterFunction.insert(rv);
            else
                VirtRegAcrossFunction.insert(&MO);
        }
        if (MO.isRegMask()) {
            set_union(BeforeFunction, AfterFunction);
            AfterFunction.clear();
        }
    }
}
```

Notice that the first encounter of a virtual register we put it in `AfterFunction`, and at each function call(`regmask`), we promote elements in `AfterFunction` to `BeforeFunction`. Then if we encounter a virtual register in `BeforeFunction`, then we mark this MO as lives-across-function by inserting it into `VirtRegAcrossFunction`.

When we assign a register we prioritize this method before others (usual assignment):
```c++
MCRegister getUnusedPhysReg(MachineOperand &MO, TargetRegisterClass *RC) {
    for (auto R : RegClassInfo.getOrder(RC)) {
        if (!IsAllocByLiveVirtReg(R) && !IsExistingPhysReg(R)) {
            if (VirtRegAcrossFunction.contains(&MO)) {
                // - Try allocate callee-saved reg if across calls
                if (TRI->isCalleeSavedPhysReg(R, *MO.getParent()->getParent()->getParent()))
                    return R;
            } else {
                // - Try allocate caller-saved reg if not across calls
                if (!TRI->isCalleeSavedPhysReg(R, *MO.getParent()->getParent()->getParent()))
                    return R;
            }
        }
    }
    // Other usual methods
}
```


ADD SP, SP, #-4
STR FP, [SP]
ADD FP, SP, #0
ADD SP, SP, #-120
MOV r0, #10
STR r0, [FP - 4]
MOV r0, r1
STR r0, [FP - 4]
LDR r1, [FP - 8]
MOV r0, r1
STR r0, [FP - 4]
MOV r0, #10
ADD r0, r0, #20
STR r0, [FP - 4]
ADD r0, r1, #20
STR r0, [FP - 4]
LDR r1, [FP - 8]
ADD r0, r1, #20
STR r0, [FP - 4]
ADD r0, r1, #10
STR r0, [FP - 4]
LDR r1, [FP - 8]
ADD r0, r1, #10
STR r0, [FP - 4]
LDR r1, [FP - 8]
LDR r2, [FP - 12]
ADD r0, r1, r2
STR r0, [FP - 4]
LDR r2, [FP - 12]
ADD r0, r1, r2
STR r0, [FP - 4]
LDR r1, [FP - 8]
ADD r0, r1, r2
STR r0, [FP - 4]
ADD r0, r1, r2
STR r0, [FP - 4]
MOV r0, #10
SUB r0, r0, #20
STR r0, [FP - 4]
SUB r0, r1, #20
STR r0, [FP - 4]
LDR r1, [FP - 8]
SUB r0, r1, #20
STR r0, [FP - 4]
RSB r0, #10, r1
STR r0, [FP - 4]
LDR r1, [FP - 8]
RSB r0, #10, r1
STR r0, [FP - 4]
LDR r1, [FP - 8]
LDR r2, [FP - 12]
SUB r0, r1, r2
STR r0, [FP - 4]
LDR r2, [FP - 12]
SUB r0, r1, r2
STR r0, [FP - 4]
LDR r1, [FP - 8]
SUB r0, r1, r2
STR r0, [FP - 4]
SUB r0, r1, r2
STR r0, [FP - 4]
MOV r0, #10
MUL r0, r0, #20
STR r0, [FP - 4]
MUL r0, r1, #20
STR r0, [FP - 4]
LDR r1, [FP - 8]
MUL r0, r1, #20
STR r0, [FP - 4]
MUL r0, r1, #10
STR r0, [FP - 4]
LDR r1, [FP - 8]
MUL r0, r1, #10
STR r0, [FP - 4]
LDR r1, [FP - 8]
LDR r2, [FP - 12]
MUL r0, r1, r2
STR r0, [FP - 4]
LDR r2, [FP - 12]
MUL r0, r1, r2
STR r0, [FP - 4]
LDR r1, [FP - 8]
MUL r0, r1, r2
STR r0, [FP - 4]
MUL r0, r1, r2
STR r0, [FP - 4]
ADD SP, FP, #0
LDR FP, [SP]
ADD SP, SP, #4

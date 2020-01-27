.section	__TEXT,__cstring,cstring_literals
.align 1
.align 0
.globl _TwoDividesByTwo_zdtrModule4_bytes
_TwoDividesByTwo_zdtrModule4_bytes:
	.asciz "main"
.data
.align 3
.align 0
.globl _TwoDividesByTwo_zdtrModule3_closure
_TwoDividesByTwo_zdtrModule3_closure:
	.quad	_ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	_TwoDividesByTwo_zdtrModule4_bytes
.section	__TEXT,__cstring,cstring_literals
.align 1
.align 0
.globl _TwoDividesByTwo_zdtrModule2_bytes
_TwoDividesByTwo_zdtrModule2_bytes:
	.asciz "TwoDividesByTwo"
.data
.align 3
.align 0
.globl _TwoDividesByTwo_zdtrModule1_closure
_TwoDividesByTwo_zdtrModule1_closure:
	.quad	_ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	_TwoDividesByTwo_zdtrModule2_bytes
.data
.align 3
.align 0
.globl _TwoDividesByTwo_zdtrModule_closure
_TwoDividesByTwo_zdtrModule_closure:
	.quad	_ghczmprim_GHCziTypes_Module_con_info
	.quad	_TwoDividesByTwo_zdtrModule3_closure+1
	.quad	_TwoDividesByTwo_zdtrModule1_closure+1
	.quad	3
.text
.align 3
_TwoDividesByTwo_twoDividesByTwo_info_dsp:
.align 3
	.quad	4294967301
	.quad	0
	.long	14
	.long	0
.globl _TwoDividesByTwo_twoDividesByTwo_info
_TwoDividesByTwo_twoDividesByTwo_info:
Lc2zy:
	leaq -8(%rbp),%rax
	cmpq %r15,%rax
	jb Lc2zI
Lc2zJ:
	leaq _c2zv_info(%rip),%rax
	movq %rax,-8(%rbp)
	movq %r14,%rbx
	addq $-8,%rbp
	testb $7,%bl
	jne Lc2zv
Lc2zw:
	jmp *(%rbx)
Lc2zM:
	movq $16,904(%r13)
	jmp _stg_gc_unpt_r1
.align 3
	.quad	0
	.long	30
	.long	0
_c2zv_info:
Lc2zv:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja Lc2zM
Lc2zL:
	movq 7(%rbx),%rax
	sarq $1,%rax
	sarq $1,%rax
	leaq _ghczmprim_GHCziTypes_Izh_con_info(%rip),%rbx
	movq %rbx,-8(%r12)
	movq %rax,(%r12)
	leaq -7(%r12),%rbx
	addq $8,%rbp
	jmp *(%rbp)
Lc2zI:
	leaq _TwoDividesByTwo_twoDividesByTwo_closure(%rip),%rbx
	jmp *-8(%r13)
	.long  _TwoDividesByTwo_twoDividesByTwo_info - _TwoDividesByTwo_twoDividesByTwo_info_dsp
.data
.align 3
.align 0
.globl _TwoDividesByTwo_twoDividesByTwo_closure
_TwoDividesByTwo_twoDividesByTwo_closure:
	.quad	_TwoDividesByTwo_twoDividesByTwo_info
.subsections_via_symbols
.ident "GHC 8.6.5"



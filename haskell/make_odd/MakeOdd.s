.section	__TEXT,__cstring,cstring_literals
.align 1
.align 0
.globl _MakeOdd_zdtrModule4_bytes
_MakeOdd_zdtrModule4_bytes:
	.asciz "main"
.data
.align 3
.align 0
.globl _MakeOdd_zdtrModule3_closure
_MakeOdd_zdtrModule3_closure:
	.quad	_ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	_MakeOdd_zdtrModule4_bytes
.section	__TEXT,__cstring,cstring_literals
.align 1
.align 0
.globl _MakeOdd_zdtrModule2_bytes
_MakeOdd_zdtrModule2_bytes:
	.asciz "MakeOdd"
.data
.align 3
.align 0
.globl _MakeOdd_zdtrModule1_closure
_MakeOdd_zdtrModule1_closure:
	.quad	_ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	_MakeOdd_zdtrModule2_bytes
.data
.align 3
.align 0
.globl _MakeOdd_zdtrModule_closure
_MakeOdd_zdtrModule_closure:
	.quad	_ghczmprim_GHCziTypes_Module_con_info
	.quad	_MakeOdd_zdtrModule3_closure+1
	.quad	_MakeOdd_zdtrModule1_closure+1
	.quad	3
.text
.align 3
_s2z3_info_dsp:
.align 3
	.quad	1
	.long	16
	.long	0
_s2z3_info:
Lc2zq:
	leaq -24(%rbp),%rax
	cmpq %r15,%rax
	jb Lc2zE
Lc2zF:
	leaq _stg_upd_frame_info(%rip),%rax
	movq %rax,-16(%rbp)
	movq %rbx,-8(%rbp)
	leaq _c2zn_info(%rip),%rax
	movq %rax,-24(%rbp)
	movq 16(%rbx),%rbx
	addq $-24,%rbp
	testb $7,%bl
	jne Lc2zn
Lc2zo:
	jmp *(%rbx)
Lc2zI:
	movq $16,904(%r13)
	jmp _stg_gc_unpt_r1
.align 3
	.quad	0
	.long	30
	.long	0
_c2zn_info:
Lc2zn:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja Lc2zI
Lc2zH:
	movq 7(%rbx),%rax
	bsf %rax,%rax
	movl $64,%ebx
	cmovne %rax,%rbx
	leaq _ghczmprim_GHCziTypes_Izh_con_info(%rip),%rax
	movq %rax,-8(%r12)
	movq %rbx,(%r12)
	leaq -7(%r12),%rbx
	addq $8,%rbp
	jmp *(%rbp)
Lc2zE:
	jmp *-16(%r13)
	.long  _s2z3_info - _s2z3_info_dsp
.text
.align 3
_s2z9_info_dsp:
.align 3
	.quad	2
	.long	18
	.long	0
_s2z9_info:
Lc2zN:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb Lc2zO
Lc2zP:
	leaq _stg_upd_frame_info(%rip),%rax
	movq %rax,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq 24(%rbx),%rsi
	movq 16(%rbx),%r14
	addq $-16,%rbp
	jmp _base_DataziBits_zdfBitsIntzuzdcshiftR_info
Lc2zO:
	jmp *-16(%r13)
	.long  _s2z9_info - _s2z9_info_dsp
.text
.align 3
_MakeOdd_makeOdd_info_dsp:
.align 3
	.quad	4294967301
	.quad	0
	.long	14
	.long	0
.globl _MakeOdd_makeOdd_info
_MakeOdd_makeOdd_info:
Lc2zR:
	addq $80,%r12
	cmpq 856(%r13),%r12
	ja Lc2zV
Lc2zU:
	leaq _s2z3_info(%rip),%rax
	movq %rax,-72(%r12)
	movq %r14,-56(%r12)
	leaq _s2z9_info(%rip),%rax
	movq %rax,-48(%r12)
	movq %r14,-32(%r12)
	leaq -72(%r12),%rax
	movq %rax,-24(%r12)
	leaq _ghczmprim_GHCziTuple_Z2T_con_info(%rip),%rbx
	movq %rbx,-16(%r12)
	movq %rax,-8(%r12)
	leaq -48(%r12),%rax
	movq %rax,(%r12)
	leaq -15(%r12),%rbx
	jmp *(%rbp)
Lc2zV:
	movq $80,904(%r13)
	leaq _MakeOdd_makeOdd_closure(%rip),%rbx
	jmp *-8(%r13)
	.long  _MakeOdd_makeOdd_info - _MakeOdd_makeOdd_info_dsp
.data
.align 3
.align 0
.globl _MakeOdd_makeOdd_closure
_MakeOdd_makeOdd_closure:
	.quad	_MakeOdd_makeOdd_info
.subsections_via_symbols
.ident "GHC 8.6.5"



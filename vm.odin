package main

using import "core:fmt"
      import "core:strings"

Register :: u64;

rr  :: NUM_REGISTERS-1;
rip :: NUM_REGISTERS-2;
rsp :: NUM_REGISTERS-3;
rj  :: NUM_REGISTERS-4;
rt  :: NUM_REGISTERS-5;
rim :: NUM_REGISTERS-6;

Instruction :: struct {
	kind: Instruction_Type,
	p1, p2, p3: u64,
}

Label_Reference :: struct {
	ip: u64,
	str: string,
}
DEBUG_MODE :: true;

NUM_REGISTERS    :: 1024;
STACK_SIZE       :: 1024;
MAIN_MEMORY_SIZE :: 1024;
VM :: struct {
	instructions:         [dynamic]Instruction,
	stack_memory:         [STACK_SIZE]u64,
	register_memory:      [NUM_REGISTERS]u64,
	main_memory:          [MAIN_MEMORY_SIZE]u64,

	do_print_registers: bool,
	instruction_hit_counts: [dynamic]int,

	label_references: [dynamic]Label_Reference,
	label_to_ip: map[string]u64,
}

patch_labels :: proc(vm: ^VM) {
	for ref in vm.label_references {
		ip, ok := vm.label_to_ip[ref.str];
		assert(ok, ref.str);

		hi := &vm.instructions[ref.ip];
		lo := &vm.instructions[ref.ip+1];

		hi.p2 = ((ip & HIHI_BITS_64) >> 48);
		hi.p3 = ((ip & HILO_BITS_64) >> 32);
		lo.p2 = ((ip & LOHI_BITS_64) >> 16);
		lo.p3 = ((ip & LOLO_BITS_64) >>  0);
	}
}

execute :: proc(vm: ^VM) {
	patch_labels(vm);

	when DEBUG_MODE {
		vm.instruction_hit_counts = make([dynamic]int, len(vm.instructions));
	}

	for step(vm) {
	}
}

destroy_vm :: proc(using vm: ^VM) {
	delete(label_to_ip);
	delete(label_references);
	delete(instruction_hit_counts);
}

step :: proc(using vm: ^VM) -> bool {
	if register_memory[rip] >= cast(u64)len(instructions) do return false;

	instruction := instructions[register_memory[rip]];
	register_memory[rip] += 1;

	switch cast(Instruction_Type)instruction.kind {
		case .MOV:  register_memory[instruction.p1] = register_memory[instruction.p2];
		case .MOVI: register_memory[instruction.p1] = instruction.p2;
		case .PUSH:
			register_memory[rsp] += 1;
			stack_memory[register_memory[rsp]] = register_memory[instruction.p1];
		case .POP:
			register_memory[instruction.p1] = stack_memory[register_memory[rsp]];

			when DEBUG_MODE {
				stack_memory[register_memory[rsp]] = 0;
			}
			register_memory[rsp] -= 1;

		case .GOTO:     register_memory[rip] = register_memory[rj];
		case .JEQ:      if register_memory[instruction.p2] == register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];
		case .JNE:      if register_memory[instruction.p2] != register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];
		case .JLT:      if transmute(i64)register_memory[instruction.p2] <  transmute(i64)register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];
		case .JGE:      if transmute(i64)register_memory[instruction.p2] >= transmute(i64)register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];
		case .JLTU:     if register_memory[instruction.p2] <  register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];
		case .JGEU:     if register_memory[instruction.p2] >= register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];

		case .EQ:       register_memory[instruction.p1] = cast(u64)(register_memory[instruction.p1] == register_memory[instruction.p2]);

		case .SV8:      main_memory[register_memory[instruction.p1]] = cast(u64)cast( u8)register_memory[instruction.p2];
		case .SV16:     main_memory[register_memory[instruction.p1]] = cast(u64)cast(u16)register_memory[instruction.p2];
		case .SV32:     main_memory[register_memory[instruction.p1]] = cast(u64)cast(u32)register_memory[instruction.p2];
		case .SV64:     main_memory[register_memory[instruction.p1]] = cast(u64)cast(u64)register_memory[instruction.p2];
		case .LD8:      register_memory[instruction.p1] = cast(u64)cast( u8)main_memory[register_memory[instruction.p2]];
		case .LD16:     register_memory[instruction.p1] = cast(u64)cast(u16)main_memory[register_memory[instruction.p2]];
		case .LD32:     register_memory[instruction.p1] = cast(u64)cast(u32)main_memory[register_memory[instruction.p2]];
		case .LD64:     register_memory[instruction.p1] = cast(u64)cast(u64)main_memory[register_memory[instruction.p2]];

		case .ADD:      register_memory[instruction.p1] = transmute(u64)(transmute(i64)register_memory[instruction.p2] +  transmute(i64)register_memory[instruction.p3]);
		case .SUB:      register_memory[instruction.p1] = transmute(u64)(transmute(i64)register_memory[instruction.p2] -  transmute(i64)register_memory[instruction.p3]);
		case .MUL:      register_memory[instruction.p1] = transmute(u64)(transmute(i64)register_memory[instruction.p2] *  transmute(i64)register_memory[instruction.p3]);
		case .DIV:      register_memory[instruction.p1] = transmute(u64)(transmute(i64)register_memory[instruction.p2] /  transmute(i64)register_memory[instruction.p3]);
		case .MOD:      register_memory[instruction.p1] = transmute(u64)(transmute(i64)register_memory[instruction.p2] %  transmute(i64)register_memory[instruction.p3]);
		case .MODMOD:   register_memory[instruction.p1] = transmute(u64)(transmute(i64)register_memory[instruction.p2] %% transmute(i64)register_memory[instruction.p3]);

		case .ADDI:     register_memory[instruction.p1] = transmute(u64)(transmute(i64)register_memory[instruction.p2] + cast(i64)(instruction.p3));

		case .ADDU:     register_memory[instruction.p1] = register_memory[instruction.p2] +  register_memory[instruction.p3];
		case .SUBU:     register_memory[instruction.p1] = register_memory[instruction.p2] -  register_memory[instruction.p3];
		case .MULU:     register_memory[instruction.p1] = register_memory[instruction.p2] *  register_memory[instruction.p3];
		case .DIVU:     register_memory[instruction.p1] = register_memory[instruction.p2] /  register_memory[instruction.p3];
		case .MODU:     register_memory[instruction.p1] = register_memory[instruction.p2] %  register_memory[instruction.p3];
		case .MODMODU:  register_memory[instruction.p1] = register_memory[instruction.p2] %% register_memory[instruction.p3];

		case .ADDF:     register_memory[instruction.p1] = transmute(u64)(transmute(f64)register_memory[instruction.p2] + transmute(f64)register_memory[instruction.p3]);
		case .SUBF:     register_memory[instruction.p1] = transmute(u64)(transmute(f64)register_memory[instruction.p2] - transmute(f64)register_memory[instruction.p3]);
		case .MULF:     register_memory[instruction.p1] = transmute(u64)(transmute(f64)register_memory[instruction.p2] * transmute(f64)register_memory[instruction.p3]);
		case .DIVF:     register_memory[instruction.p1] = transmute(u64)(transmute(f64)register_memory[instruction.p2] / transmute(f64)register_memory[instruction.p3]);

		case .SHL:      register_memory[instruction.p1] = register_memory[instruction.p2] << register_memory[instruction.p3];
		case .SHR:      register_memory[instruction.p1] = register_memory[instruction.p2] >> register_memory[instruction.p3];
		case .AND:      register_memory[instruction.p1] = register_memory[instruction.p2] &  register_memory[instruction.p3];
		case .OR:       register_memory[instruction.p1] = register_memory[instruction.p2] |  register_memory[instruction.p3];
		case .XOR:      register_memory[instruction.p1] = register_memory[instruction.p2] ~  register_memory[instruction.p3];

		case .QUIT:     register_memory[rip] -= 1; return false;
		case .BREAK:    return false;
		case:          assert(false, aprint(instruction.kind));
	}

	println(instruction);
	println(register_memory[0:10]);

	return true;
}

print_registers :: proc(vm: ^VM) {
	println("Stack:");
	for v, i in vm.stack_memory {
		if cast(u64)i > vm.register_memory[rsp] do break;

		print(v, "  ");
	}
	print("\n");

	println("register_memory:");

	str_buf := strings.make_builder();
	defer strings.destroy_builder(&str_buf);

	counter := 0;
	for r, i in vm.register_memory {
		buf: [1024]byte;
		length_of_register_name  := len(bprint(buf[:], cast(Register)i));
		length_of_register_value := len(bprint(buf[:], r));

		sbprint(&str_buf, cast(Register)i, ": ");
		for _ in 0..(max(5-length_of_register_name, 2)) do sbprint(&str_buf, " ");
		sbprint(&str_buf, r);
		for _ in 0..(max(12-length_of_register_value, 2)) do sbprint(&str_buf, " ");

		counter += 1;
		if counter >= 8 {
			counter = 0;
			println(strings.to_string(str_buf));
			clear(&str_buf.buf);
		}
	}
}

when DEBUG_MODE {
	print_instruction_counts :: proc(vm: ^VM) {
		for val, idx in vm.instruction_hit_counts {
			println(vm.instructions[idx].kind, val);
		}
	}
}

Instruction_Type :: enum u16 {
	INVALID,

	QUIT, BREAK, GOTO,

	JUMP, JEQ, JNE, JLT, JGE, JLTU, JGEU,

	EQ,

	SV8, SV16, SV32, SV64,
	LD8, LD16, LD32, LD64,

	ADD, SUB, MUL, DIV, MOD, MODMOD,

	ADDI,

	ADDU, SUBU, MULU, DIVU, MODU, MODMODU,

	ADDF, SUBF, MULF, DIVF,

	SHL, SHR, // SHA,
	SHLI, SHRI, // SHAI,

	AND, OR, XOR,

	MOV, MOVI,

	// PSEUDOINSTRUCTIONS

	PUSH, POP,

	CALL, RET,
	// TAIL,

	ANDI, ORI, XORI,
}

HI_BITS_32 ::   0xFFFF0000;
LO_BITS_32 ::   0x0000FFFF;

HIHI_BITS_64 :: 0xFFFF000000000000;
HILO_BITS_64 :: 0x0000FFFF00000000;
LOHI_BITS_64 :: 0x00000000FFFF0000;
LOLO_BITS_64 :: 0x000000000000FFFF;

quit :: inline proc(using vm: ^VM) {
	inst := Instruction{.QUIT, 0, 0, 0};
	append(&instructions, inst);
}
brk :: inline proc(using vm: ^VM) {
	inst := Instruction{.BREAK, 0, 0, 0};
	append(&instructions, inst);
}
goto :: inline proc(using vm: ^VM, str: string) {
	ref := Label_Reference{cast(u64)len(instructions), str};
	append(&label_references, ref);
	movi(vm, rj, 0); // will be backpatched
	inst := Instruction{.GOTO, 0, 0, 0};
	append(&instructions, inst);
}

mov :: inline proc(using vm: ^VM, rd, p1: Register) {
	inst := Instruction{.MOV, rd, p1, 0};
	append(&instructions, inst);
}
movi :: inline proc(using vm: ^VM, rd: Register, p1: u64) {
	inst := Instruction{.MOVI, rd, p1, 0};
	append(&instructions, inst);
}

jeq :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions), str};
	append(&label_references, ref);
	movi(vm, rj, 0); // will be backpatched
	inst := Instruction{.JEQ, 0, r1, r2};
	append(&instructions, inst);
}
jne :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions), str};
	append(&label_references, ref);
	movi(vm, rj, 0); // will be backpatched
	inst := Instruction{.JNE, 0, r1, r2};
	append(&instructions, inst);
}
jlt :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions), str};
	append(&label_references, ref);
	movi(vm, rj, 0); // will be backpatched
	inst := Instruction{.JLT, 0, r1, r2};
	append(&instructions, inst);
}
jge :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions), str};
	append(&label_references, ref);
	movi(vm, rj, 0); // will be backpatched
	inst := Instruction{.JGE, 0, r1, r2};
	append(&instructions, inst);
}
jltu :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions), str};
	append(&label_references, ref);
	movi(vm, rj, 0); // will be backpatched
	inst := Instruction{.JLTU, 0, r1, r2};
	append(&instructions, inst);
}
jgeu :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions), str};
	append(&label_references, ref);
	movi(vm, rj, 0); // will be backpatched
	inst := Instruction{.JGEU, 0, r1, r2};
	append(&instructions, inst);
}
eq :: inline proc(using vm: ^VM, rd, r1, r2: Register) {
	inst := Instruction{.EQ, rd, r1, r2};
	append(&instructions, inst);
}

sv8 :: inline proc(using vm: ^VM, p1, p2: Register) {
	inst := Instruction{.SV8, p1, p2, 0};
	append(&instructions, inst);
}
sv16 :: inline proc(using vm: ^VM, p1, p2: Register) {
	inst := Instruction{.SV16, p1, p2, 0};
	append(&instructions, inst);
}
sv32 :: inline proc(using vm: ^VM, p1, p2: Register) {
	inst := Instruction{.SV32, p1, p2, 0};
	append(&instructions, inst);
}
sv64 :: inline proc(using vm: ^VM, p1, p2: Register) {
	inst := Instruction{.SV64, p1, p2, 0};
	append(&instructions, inst);
}
ld8 :: inline proc(using vm: ^VM, p1, p2: Register) {
	inst := Instruction{.LD8, p1, p2, 0};
	append(&instructions, inst);
}
ld16 :: inline proc(using vm: ^VM, p1, p2: Register) {
	inst := Instruction{.LD16, p1, p2, 0};
	append(&instructions, inst);
}
ld32 :: inline proc(using vm: ^VM, p1, p2: Register) {
	inst := Instruction{.LD32, p1, p2, 0};
	append(&instructions, inst);
}
ld64 :: inline proc(using vm: ^VM, p1, p2: Register) {
	inst := Instruction{.LD64, p1, p2, 0};
	append(&instructions, inst);
}
add :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.ADD, rd, p1, p2};
	append(&instructions, inst);
}
addi :: inline proc(using vm: ^VM, rd, p1: Register, p2: i64) {
	inst := Instruction{.ADDI, rd, p1, transmute(u64)p2};
	append(&instructions, inst);
}
sub :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.SUB, rd, p1, p2};
	append(&instructions, inst);
}
mul :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.MUL, rd, p1, p2};
	append(&instructions, inst);
}
div :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.DIV, rd, p1, p2};
	append(&instructions, inst);
}
mod :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.MOD, rd, p1, p2};
	append(&instructions, inst);
}
modmod :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.MODMOD, rd, p1, p2};
	append(&instructions, inst);
}
addu :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.ADDU, rd, p1, p2};
	append(&instructions, inst);
}
subu :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.SUBU, rd, p1, p2};
	append(&instructions, inst);
}
mulu :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.MULU, rd, p1, p2};
	append(&instructions, inst);
}
divu :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.DIVU, rd, p1, p2};
	append(&instructions, inst);
}
modu :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.MODU, rd, p1, p2};
	append(&instructions, inst);
}
modmodu :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.MODMODU, rd, p1, p2};
	append(&instructions, inst);
}
addf :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.ADDF, rd, p1, p2};
	append(&instructions, inst);
}
subf :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.SUBF, rd, p1, p2};
	append(&instructions, inst);
}
mulf :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.MULF, rd, p1, p2};
	append(&instructions, inst);
}
divf :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.DIVF, rd, p1, p2};
	append(&instructions, inst);
}
shl :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.SHL, rd, p1, p2};
	append(&instructions, inst);
}
shr :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.SHR, rd, p1, p2};
	append(&instructions, inst);
}
and :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.AND, rd, p1, p2};
	append(&instructions, inst);
}
or :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.OR, rd, p1, p2};
	append(&instructions, inst);
}
xor :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{.XOR, rd, p1, p2};
	append(&instructions, inst);
}
stack_push :: inline proc(using vm: ^VM, r1: Register) {
	inst := Instruction{.PUSH, r1, 0, 0};
	append(&instructions, inst);
}
stack_pop :: inline proc(using vm: ^VM, r1: Register) {
	inst := Instruction{.POP, r1, 0, 0};
	append(&instructions, inst);
}

// PSEUDOINSTRUCTIONS

call :: inline proc(using vm: ^VM, str: string) {
	ref := Label_Reference{cast(u64)len(instructions), str};
	append(&label_references, ref);
	movi(vm, rj, 0); // will be backpatched
	stack_push(vm, rip);
	mov(vm, rip, rj);
}
ret :: inline proc(using vm: ^VM) {
	stack_pop(vm, rt);
	addi(vm, rt, rt, 1);
	mov(vm, rip, rt);
}

movis :: inline proc(using vm: ^VM, rd: Register, p1: i64) {
	movi(vm, rd, transmute(u64)p1);
}
movif :: inline proc(using vm: ^VM, rd: Register, p1: f64) {
	movi(vm, rd, transmute(u64)p1);
}
// movi :: inline proc(using vm: ^VM, rd: Register, p1: u64) {
// 	movi(vm, rd, p1);
// 	hi := Instruction{.MOVHI, rd, p1 >> 48,       p1 << 16 >> 48};
// 	lo := Instruction{.MOVLO, rd, p1 << 32 >> 48, p1 << 48 >> 48};
// 	append(&instructions, hi);
// 	append(&instructions, lo);
// }
// movif :: inline proc(using vm: ^VM, rd: Register, p1: f64) {
// 	hihi := (transmute(u64)p1 & HIHI_BITS_64) >> 48;
// 	hilo := (transmute(u64)p1 & HILO_BITS_64) >> 32;
// 	lohi := (transmute(u64)p1 & LOHI_BITS_64) >> 16;
// 	lolo := (transmute(u64)p1 & LOLO_BITS_64) >>  0;
// 	hi := Instruction{.MOVHI, rd, hihi, hilo};
// 	lo := Instruction{.MOVLO, rd, lohi, lolo};
// 	append(&instructions, hi);
// 	append(&instructions, lo);
// }

shli :: inline proc(using vm: ^VM, rd: Register, p1: u64) {
	movi(vm, rim, p1);
	shl(vm, rd, rd, rim);
}
shri :: inline proc(using vm: ^VM, rd: Register, p1: u64) {
	movi(vm, rim, p1);
	shr(vm, rd, rd, rim);
}
andi :: inline proc(using vm: ^VM, rd, r1: Register, p2: u64) {
	movi(vm, rim, p2);
	and(vm, rd, r1, rim);
}
ori :: inline proc(using vm: ^VM, rd, r1: Register, p2: u64) {
	movi(vm, rim, p2);
	or(vm, rd, r1, rim);
}
xori :: inline proc(using vm: ^VM, rd, r1: Register, p2: u64) {
	movi(vm, rim, p2);
	xor(vm, rd, r1, rim);
}
label :: inline proc(using vm: ^VM, str: string) {
	instruction_pointer := cast(u64)len(instructions);
	_, ok := label_to_ip[str];
	assert(!ok, tprint("Already have label: ", str));
	println(str);
	label_to_ip[str] = instruction_pointer;
}

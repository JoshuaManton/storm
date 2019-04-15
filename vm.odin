package main

using import "core:fmt"
      import "core:strings"

Register :: u16;

rr  :: max(u16)-1;
rip :: max(u16)-2;
rsp :: max(u16)-3;
rj  :: max(u16)-4;
rt  :: max(u16)-5;
rim :: max(u16)-6;

Instruction :: struct {
	kind: Instruction_Type,
	p1, p2, p3: u16,
}

Label_Reference :: struct {
	ip: u64,
	str: string,
}
DEBUG_MODE :: true;

STACK_SIZE :: 50000; // number of u64's
NUM_REGISTERS :: max(u16); // yeah this is pretty cheese but once crest has proper register allocation then I'll worry about making the VM actually be hardware-implementable
MEMORY_SIZE :: 64;
VM :: struct {
	encoded_instructions: [dynamic]u64,
	stack_memory:         [STACK_SIZE]u64,
	register_memory:      [NUM_REGISTERS]u64,
	main_memory:          [MEMORY_SIZE]u64,



	do_print_registers: bool,
	instructions_readable: [dynamic]Instruction,
	instruction_hit_counts: [dynamic]int,

	label_references: [dynamic]Label_Reference,
	label_to_ip: map[string]u64,
}

patch_labels :: proc(vm: ^VM) {
	for ref in vm.label_references {
		ip, ok := vm.label_to_ip[ref.str];
		assert(ok, ref.str);

		hi := decode(vm.encoded_instructions[ref.ip]);
		lo := decode(vm.encoded_instructions[ref.ip+1]);

		hi.p2 = cast(u16)((ip & HIHI_BITS_64) >> 48);
		hi.p3 = cast(u16)((ip & HILO_BITS_64) >> 32);
		lo.p2 = cast(u16)((ip & LOHI_BITS_64) >> 16);
		lo.p3 = cast(u16)((ip & LOLO_BITS_64) >>  0);

		vm.encoded_instructions[ref.ip]   = encode(hi);
		vm.encoded_instructions[ref.ip+1] = encode(lo);
	}
}

encode_instructions :: proc(vm: ^VM) {
	clear(&vm.encoded_instructions);
	for inst, i in vm.instructions_readable {
		encoded := encode(inst);
		append(&vm.encoded_instructions, encoded);
	}
}

execute :: proc(vm: ^VM) {
	encode_instructions(vm);
	patch_labels(vm);

	when DEBUG_MODE {
		vm.instruction_hit_counts = make([dynamic]int, len(vm.instructions_readable));
	}

	for step(vm) {
	}
}

destroy_vm :: proc(using vm: ^VM) {
	delete(instructions_readable);
	delete(encoded_instructions);
	delete(label_to_ip);
	delete(label_references);
	delete(instruction_hit_counts);
}

step :: proc(using vm: ^VM) -> bool {
	if register_memory[rip] >= cast(u64)len(instructions_readable) do return false;

	when DEBUG_MODE {
		this_instruction_idx := register_memory[rip];
		instruction_raw := encoded_instructions[this_instruction_idx];

		instruction_hit_counts[this_instruction_idx] += 1;
		instruction := decode(instruction_raw);
	}
	else {
		instruction := transmute(Instruction)encoded_instructions[register_memory[rip]];
	}

	register_memory[rip] += 1;

	using Instruction_Type;
	switch cast(Instruction_Type)instruction.kind {
		case MOV:      register_memory[instruction.p1] = register_memory[instruction.p2];
		case MOVHI:    register_memory[instruction.p1] =  (u64(instruction.p2) << 48) | (u64(instruction.p3) << 32);
		case MOVLO:    register_memory[instruction.p1] |= (u64(instruction.p2) << 16) | (u64(instruction.p3) <<  0);
		case PUSH:
			register_memory[rsp] += 1;
			stack_memory[register_memory[rsp]] = register_memory[instruction.p1];
		case POP:
			register_memory[instruction.p1] = stack_memory[register_memory[rsp]];

			when DEBUG_MODE {
				stack_memory[register_memory[rsp]] = 0;
			}
			register_memory[rsp] -= 1;

		case GOTO:     register_memory[rip] = register_memory[rj];
		case JEQ:      if register_memory[instruction.p2] == register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];
		case JNE:      if register_memory[instruction.p2] != register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];
		case JLT:      if transmute(i64)register_memory[instruction.p2] <  transmute(i64)register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];
		case JGE:      if transmute(i64)register_memory[instruction.p2] >= transmute(i64)register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];
		case JLTU:     if register_memory[instruction.p2] <  register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];
		case JGEU:     if register_memory[instruction.p2] >= register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];

		case SV8:      main_memory[register_memory[instruction.p1]] = cast(u64)cast( u8)register_memory[instruction.p2];
		case SV16:     main_memory[register_memory[instruction.p1]] = cast(u64)cast(u16)register_memory[instruction.p2];
		case SV32:     main_memory[register_memory[instruction.p1]] = cast(u64)cast(u32)register_memory[instruction.p2];
		case SV64:     main_memory[register_memory[instruction.p1]] = cast(u64)cast(u64)register_memory[instruction.p2];
		case LD8:      register_memory[instruction.p1] = cast(u64)cast( u8)main_memory[register_memory[instruction.p2]];
		case LD16:     register_memory[instruction.p1] = cast(u64)cast(u16)main_memory[register_memory[instruction.p2]];
		case LD32:     register_memory[instruction.p1] = cast(u64)cast(u32)main_memory[register_memory[instruction.p2]];
		case LD64:     register_memory[instruction.p1] = cast(u64)cast(u64)main_memory[register_memory[instruction.p2]];

		case ADD:      register_memory[instruction.p1] = transmute(u64)(transmute(i64)register_memory[instruction.p2] + transmute(i64)register_memory[instruction.p3]);
		case SUB:      register_memory[instruction.p1] = transmute(u64)(transmute(i64)register_memory[instruction.p2] - transmute(i64)register_memory[instruction.p3]);
		case MUL:      register_memory[instruction.p1] = transmute(u64)(transmute(i64)register_memory[instruction.p2] * transmute(i64)register_memory[instruction.p3]);
		case DIV:      register_memory[instruction.p1] = transmute(u64)(transmute(i64)register_memory[instruction.p2] / transmute(i64)register_memory[instruction.p3]);
		// todo: Not sure about % vs %%
		case MOD:      register_memory[instruction.p1] = transmute(u64)(transmute(i64)register_memory[instruction.p2] % transmute(i64)register_memory[instruction.p3]);

		case ADDI:     register_memory[instruction.p1] = transmute(u64)(transmute(i64)register_memory[instruction.p2] + cast(i64)(transmute(i16)instruction.p3));

		case ADDU:     register_memory[instruction.p1] = register_memory[instruction.p2] + register_memory[instruction.p3];
		case SUBU:     register_memory[instruction.p1] = register_memory[instruction.p2] - register_memory[instruction.p3];
		case MULU:     register_memory[instruction.p1] = register_memory[instruction.p2] * register_memory[instruction.p3];
		case DIVU:     register_memory[instruction.p1] = register_memory[instruction.p2] / register_memory[instruction.p3];
		// todo: Not sure about % vs %%
		case MODU:     register_memory[instruction.p1] = register_memory[instruction.p2] % register_memory[instruction.p3];

		case ADDF:     register_memory[instruction.p1] = transmute(u64)(transmute(f64)register_memory[instruction.p2] + transmute(f64)register_memory[instruction.p3]);
		case SUBF:     register_memory[instruction.p1] = transmute(u64)(transmute(f64)register_memory[instruction.p2] - transmute(f64)register_memory[instruction.p3]);
		case MULF:     register_memory[instruction.p1] = transmute(u64)(transmute(f64)register_memory[instruction.p2] * transmute(f64)register_memory[instruction.p3]);
		case DIVF:     register_memory[instruction.p1] = transmute(u64)(transmute(f64)register_memory[instruction.p2] / transmute(f64)register_memory[instruction.p3]);

		case SHL:      register_memory[instruction.p1] = register_memory[instruction.p2] << register_memory[instruction.p3];
		case SHR:      register_memory[instruction.p1] = register_memory[instruction.p2] >> register_memory[instruction.p3];
		case AND:      register_memory[instruction.p1] = register_memory[instruction.p2] &  register_memory[instruction.p3];
		case OR:       register_memory[instruction.p1] = register_memory[instruction.p2] |  register_memory[instruction.p3];
		case XOR:      register_memory[instruction.p1] = register_memory[instruction.p2] ~  register_memory[instruction.p3];

		case QUIT:     register_memory[rip] -= 1; return false;
		case BREAK:    return false;
		case:          assert(false, aprint(instruction.kind));
	}

	when DEBUG_MODE {
		if vm.do_print_registers {
			println("-------------------------------------------------------------------");
			println("Instruction:", this_instruction_idx, instruction.kind, instruction.p1, instruction.p2, instruction.p3);
			print_registers(vm);
		}
	}

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
			println(vm.instructions_readable[idx].kind, val);
		}
	}
}

using Instruction_Type :: enum u16 {
	INVALID,

	QUIT, BREAK, GOTO,

	JUMP, JEQ, JNE, JLT, JGE, JLTU, JGEU,

	SV8, SV16, SV32, SV64,
	LD8, LD16, LD32, LD64,

	ADD, SUB, MUL, DIV, MOD,

	ADDI,

	ADDU, SUBU, MULU, DIVU, MODU,

	ADDF, SUBF, MULF, DIVF,

	SHL, SHR, // SHA,
	SHLI, SHRI, // SHAI,

	AND, OR, XOR,

	MOV, MOVHI, MOVLO,

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

encode :: inline proc(instruction: Instruction) -> u64 {
	op := transmute(u64)instruction;
	return op;
}
decode :: inline proc(raw: u64) -> Instruction {
	instruction := transmute(Instruction)raw;
	return instruction;
}

quit :: inline proc(using vm: ^VM) {
	inst := Instruction{QUIT, 0, 0, 0};
	append(&instructions_readable, inst);
}
brk :: inline proc(using vm: ^VM) {
	inst := Instruction{BREAK, 0, 0, 0};
	append(&instructions_readable, inst);
}
goto :: inline proc(using vm: ^VM, str: string) {
	ref := Label_Reference{cast(u64)len(instructions_readable), str};
	append(&label_references, ref);
	moviu(vm, rj, 0); // will be backpatched
	inst := Instruction{GOTO, 0, 0, 0};
	append(&instructions_readable, inst);
}

mov :: inline proc(using vm: ^VM, rd, p1: Register) {
	inst := Instruction{MOV, cast(u16)rd, cast(u16)p1, 0};
	append(&instructions_readable, inst);
}

jeq :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions_readable), str};
	append(&label_references, ref);
	moviu(vm, rj, 0); // will be backpatched
	inst := Instruction{JEQ, 0, cast(u16)r1, cast(u16)r2};
	append(&instructions_readable, inst);
}
jne :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions_readable), str};
	append(&label_references, ref);
	moviu(vm, rj, 0); // will be backpatched
	inst := Instruction{JNE, 0, cast(u16)r1, cast(u16)r2};
	append(&instructions_readable, inst);
}
jlt :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions_readable), str};
	append(&label_references, ref);
	moviu(vm, rj, 0); // will be backpatched
	inst := Instruction{JLT, 0, cast(u16)r1, cast(u16)r2};
	append(&instructions_readable, inst);
}
jge :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions_readable), str};
	append(&label_references, ref);
	moviu(vm, rj, 0); // will be backpatched
	inst := Instruction{JGE, 0, cast(u16)r1, cast(u16)r2};
	append(&instructions_readable, inst);
}
jltu :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions_readable), str};
	append(&label_references, ref);
	moviu(vm, rj, 0); // will be backpatched
	inst := Instruction{JLTU, 0, cast(u16)r1, cast(u16)r2};
	append(&instructions_readable, inst);
}
jgeu :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions_readable), str};
	append(&label_references, ref);
	moviu(vm, rj, 0); // will be backpatched
	inst := Instruction{JGEU, 0, cast(u16)r1, cast(u16)r2};
	append(&instructions_readable, inst);
}
sv8 :: inline proc(using vm: ^VM, p1, p2: Register) {
	inst := Instruction{SV8, cast(u16)p1, cast(u16)p2, 0};
	append(&instructions_readable, inst);
}
sv16 :: inline proc(using vm: ^VM, p1, p2: Register) {
	inst := Instruction{SV16, cast(u16)p1, cast(u16)p2, 0};
	append(&instructions_readable, inst);
}
sv32 :: inline proc(using vm: ^VM, p1, p2: Register) {
	inst := Instruction{SV32, cast(u16)p1, cast(u16)p2, 0};
	append(&instructions_readable, inst);
}
sv64 :: inline proc(using vm: ^VM, p1, p2: Register) {
	inst := Instruction{SV64, cast(u16)p1, cast(u16)p2, 0};
	append(&instructions_readable, inst);
}
ld8 :: inline proc(using vm: ^VM, p1, p2: Register) {
	inst := Instruction{LD8, cast(u16)p1, cast(u16)p2, 0};
	append(&instructions_readable, inst);
}
ld16 :: inline proc(using vm: ^VM, p1, p2: Register) {
	inst := Instruction{LD16, cast(u16)p1, cast(u16)p2, 0};
	append(&instructions_readable, inst);
}
ld32 :: inline proc(using vm: ^VM, p1, p2: Register) {
	inst := Instruction{LD32, cast(u16)p1, cast(u16)p2, 0};
	append(&instructions_readable, inst);
}
ld64 :: inline proc(using vm: ^VM, p1, p2: Register) {
	inst := Instruction{LD64, cast(u16)p1, cast(u16)p2, 0};
	append(&instructions_readable, inst);
}
add :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{ADD, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
addi :: inline proc(using vm: ^VM, rd, p1: Register, p2: i16) {
	inst := Instruction{ADDI, cast(u16)rd, cast(u16)p1, transmute(u16)p2};
	append(&instructions_readable, inst);
}
sub :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{SUB, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
mul :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{MUL, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
div :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{DIV, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
mod :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{MOD, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
addu :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{ADDU, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
subu :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{SUBU, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
mulu :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{MULU, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
divu :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{DIVU, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
modu :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{MODU, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
addf :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{ADDF, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
subf :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{SUBF, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
mulf :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{MULF, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
divf :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{DIVF, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
shl :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{SHL, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
shr :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{SHR, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
and :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{AND, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
or :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{OR, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
xor :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	inst := Instruction{XOR, cast(u16)rd, cast(u16)p1, cast(u16)p2};
	append(&instructions_readable, inst);
}
stack_push :: inline proc(using vm: ^VM, r1: Register) {
	inst := Instruction{PUSH, cast(u16)r1, 0, 0};
	append(&instructions_readable, inst);
}
stack_pop :: inline proc(using vm: ^VM, r1: Register) {
	inst := Instruction{POP, cast(u16)r1, 0, 0};
	append(&instructions_readable, inst);
}

// PSEUDOINSTRUCTIONS

call :: inline proc(using vm: ^VM, str: string) {
	ref := Label_Reference{cast(u64)len(instructions_readable), str};
	append(&label_references, ref);
	moviu(vm, rj, 0); // will be backpatched
	stack_push(vm, rip);
	mov(vm, rip, rj);
}
ret :: inline proc(using vm: ^VM) {
	stack_pop(vm, rt);
	addi(vm, rt, rt, 1);
	mov(vm, rip, rt);
}

movi :: inline proc(using vm: ^VM, rd: Register, _p1: i64) {
	p1 := transmute(u64)_p1;
	hi := Instruction{MOVHI, cast(u16)rd, u16(p1 >> 48),       u16(p1 << 16 >> 48)};
	lo := Instruction{MOVLO, cast(u16)rd, u16(p1 << 32 >> 48), u16(p1 << 48 >> 48)};
	append(&instructions_readable, hi);
	append(&instructions_readable, lo);
}
moviu :: inline proc(using vm: ^VM, rd: Register, p1: u64) {
	hi := Instruction{MOVHI, cast(u16)rd, u16(p1 >> 48),       u16(p1 << 16 >> 48)};
	lo := Instruction{MOVLO, cast(u16)rd, u16(p1 << 32 >> 48), u16(p1 << 48 >> 48)};
	append(&instructions_readable, hi);
	append(&instructions_readable, lo);
}
movif :: inline proc(using vm: ^VM, rd: Register, p1: f64) {
	hihi := u16((transmute(u64)p1 & HIHI_BITS_64) >> 48);
	hilo := u16((transmute(u64)p1 & HILO_BITS_64) >> 32);
	lohi := u16((transmute(u64)p1 & LOHI_BITS_64) >> 16);
	lolo := u16((transmute(u64)p1 & LOLO_BITS_64) >>  0);
	hi := Instruction{MOVHI, cast(u16)rd, hihi, hilo};
	lo := Instruction{MOVLO, cast(u16)rd, lohi, lolo};
	append(&instructions_readable, hi);
	append(&instructions_readable, lo);
}

shli :: inline proc(using vm: ^VM, rd: Register, p1: u64) {
	movi(vm, rim, cast(i64)p1);
	shl(vm, rd, rd, rim);
}
shri :: inline proc(using vm: ^VM, rd: Register, p1: u64) {
	movi(vm, rim, cast(i64)p1);
	shr(vm, rd, rd, rim);
}
andi :: inline proc(using vm: ^VM, rd, r1: Register, p2: u64) {
	movi(vm, rim, cast(i64)p2);
	and(vm, rd, r1, rim);
}
ori :: inline proc(using vm: ^VM, rd, r1: Register, p2: u64) {
	movi(vm, rim, cast(i64)p2);
	or(vm, rd, r1, rim);
}
xori :: inline proc(using vm: ^VM, rd, r1: Register, p2: u64) {
	movi(vm, rim, cast(i64)p2);
	xor(vm, rd, r1, rim);
}
label :: inline proc(using vm: ^VM, str: string) {
	instruction_pointer := cast(u64)len(instructions_readable);
	_, ok := label_to_ip[str];
	assert(!ok, tprint("Already have label: ", str));
	label_to_ip[str] = instruction_pointer;
}

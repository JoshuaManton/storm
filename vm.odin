package main

using import "core:fmt"
      import "core:strings"

Register :: u64;

rip :: NUM_REGISTERS-1;
rsp :: NUM_REGISTERS-2;
rj  :: NUM_REGISTERS-3;
rim :: NUM_REGISTERS-4;
rz  :: NUM_REGISTERS-5;

Instruction :: struct {
	kind: Instruction_Type,
	p1, p2, p3: u64,
}

Label_Reference :: struct {
	ip: u64,
	str: string,
}
DEBUG_MODE :: true;

NUM_REGISTERS    :: 50;
STACK_SIZE       :: 2048;
MAIN_MEMORY_SIZE :: 50;
VM :: struct {
	instructions:    [dynamic]Instruction,
	stack_memory:    [STACK_SIZE]u64,
	register_memory: [NUM_REGISTERS]u64,
	main_memory:     [MAIN_MEMORY_SIZE]u64,

	do_print_registers: bool,
	instruction_hit_counts: [dynamic]int,

	label_references: [dynamic]Label_Reference,
	label_to_ip: map[string]u64,
}

patch_labels :: proc(vm: ^VM) {
	for ref in vm.label_references {
		ip, ok := vm.label_to_ip[ref.str];
		assert(ok, ref.str);

		vm.instructions[ref.ip].p2 = ip;
	}
}

execute :: proc(vm: ^VM) {
	patch_labels(vm);

	when DEBUG_MODE {
		vm.instruction_hit_counts = make([dynamic]int, len(vm.instructions));
	}

	println("Generated", len(vm.instructions), "Instructions");
	// for i in vm.instructions do println("Instruction:", i);

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
	println(register_memory[rip], instruction);

	register_memory[rip] += 1;

	switch cast(Instruction_Type)instruction.kind {
		case .MOV:  register_memory[instruction.p1] = register_memory[instruction.p2];
		case .MOVI: register_memory[instruction.p1] = instruction.p2;
		case .PUSH:
			stack_memory[register_memory[rsp]] = register_memory[instruction.p1];
			register_memory[rsp] += 1;

		case .POP:
			register_memory[rsp] -= 1;
			register_memory[instruction.p1] = stack_memory[register_memory[rsp]];
			when DEBUG_MODE {
				stack_memory[register_memory[rsp]] = 0;
			}

		case .JEQ:      if register_memory[instruction.p2] == register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];
		case .JEZ:      if register_memory[instruction.p2] == 0                               do register_memory[rip] = register_memory[rj];
		case .JNE:      if register_memory[instruction.p2] != register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];
		case .JLT:      if transmute(i64)register_memory[instruction.p2] <  transmute(i64)register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];
		case .JGE:      if transmute(i64)register_memory[instruction.p2] >= transmute(i64)register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];
		case .JLTU:     if register_memory[instruction.p2] <  register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];
		case .JGEU:     if register_memory[instruction.p2] >= register_memory[instruction.p3] do register_memory[rip] = register_memory[rj];

		case .EQ:       register_memory[instruction.p1] = (register_memory[instruction.p2] == register_memory[instruction.p3] ? 1 : 0);

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

	register_memory[rz] = 0;

	println(register_memory[:20]);
	println(stack_memory[:register_memory[rsp]]);

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

	QUIT, BREAK,

	JUMP, JEQ, JEZ, JNE, JLT, JGE, JLTU, JGEU,

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

add_instruction :: proc(using vm: ^VM, inst: Instruction) {
	append(&instructions, inst);
}

quit :: inline proc(using vm: ^VM) {
	add_instruction(vm, Instruction{.QUIT, 0, 0, 0});
}
brk :: inline proc(using vm: ^VM) {
	add_instruction(vm, Instruction{.BREAK, 0, 0, 0});
}

mov :: inline proc(using vm: ^VM, rd, p1: Register) {
	add_instruction(vm, Instruction{.MOV, rd, p1, 0});
}
movi :: inline proc(using vm: ^VM, rd: Register, p1: u64) {
	add_instruction(vm, Instruction{.MOVI, rd, p1, 0});
}

jeq :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions), str};
	append(&label_references, ref);
	movi(vm, rj, 0); // will be backpatched
	add_instruction(vm, Instruction{.JEQ, 0, r1, r2});
}
jez :: inline proc(using vm: ^VM, str: string, r1: Register) {
	ref := Label_Reference{cast(u64)len(instructions), str};
	append(&label_references, ref);
	movi(vm, rj, 0); // will be backpatched
	add_instruction(vm, Instruction{.JEZ, 0, r1, 0});
}
jne :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions), str};
	append(&label_references, ref);
	movi(vm, rj, 0); // will be backpatched
	add_instruction(vm, Instruction{.JNE, 0, r1, r2});
}
jlt :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions), str};
	append(&label_references, ref);
	movi(vm, rj, 0); // will be backpatched
	add_instruction(vm, Instruction{.JLT, 0, r1, r2});
}
jge :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions), str};
	append(&label_references, ref);
	movi(vm, rj, 0); // will be backpatched
	add_instruction(vm, Instruction{.JGE, 0, r1, r2});
}
jltu :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions), str};
	append(&label_references, ref);
	movi(vm, rj, 0); // will be backpatched
	add_instruction(vm, Instruction{.JLTU, 0, r1, r2});
}
jgeu :: inline proc(using vm: ^VM, str: string, r1, r2: Register) {
	ref := Label_Reference{cast(u64)len(instructions), str};
	append(&label_references, ref);
	movi(vm, rj, 0); // will be backpatched
	add_instruction(vm, Instruction{.JGEU, 0, r1, r2});
}
eq :: inline proc(using vm: ^VM, rd, r1, r2: Register) {
	add_instruction(vm, Instruction{.EQ, rd, r1, r2});
}

sv8 :: inline proc(using vm: ^VM, p1, p2: Register) {
	add_instruction(vm, Instruction{.SV8, p1, p2, 0});
}
sv16 :: inline proc(using vm: ^VM, p1, p2: Register) {
	add_instruction(vm, Instruction{.SV16, p1, p2, 0});
}
sv32 :: inline proc(using vm: ^VM, p1, p2: Register) {
	add_instruction(vm, Instruction{.SV32, p1, p2, 0});
}
sv64 :: inline proc(using vm: ^VM, p1, p2: Register) {
	add_instruction(vm, Instruction{.SV64, p1, p2, 0});
}
ld8 :: inline proc(using vm: ^VM, p1, p2: Register) {
	add_instruction(vm, Instruction{.LD8, p1, p2, 0});
}
ld16 :: inline proc(using vm: ^VM, p1, p2: Register) {
	add_instruction(vm, Instruction{.LD16, p1, p2, 0});
}
ld32 :: inline proc(using vm: ^VM, p1, p2: Register) {
	add_instruction(vm, Instruction{.LD32, p1, p2, 0});
}
ld64 :: inline proc(using vm: ^VM, p1, p2: Register) {
	add_instruction(vm, Instruction{.LD64, p1, p2, 0});
}
add :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.ADD, rd, p1, p2});
}
addi :: inline proc(using vm: ^VM, rd, p1: Register, p2: i64) {
	add_instruction(vm, Instruction{.ADDI, rd, p1, transmute(u64)p2});
}
sub :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.SUB, rd, p1, p2});
}
mul :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.MUL, rd, p1, p2});
}
div :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.DIV, rd, p1, p2});
}
mod :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.MOD, rd, p1, p2});
}
modmod :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.MODMOD, rd, p1, p2});
}
addu :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.ADDU, rd, p1, p2});
}
subu :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.SUBU, rd, p1, p2});
}
mulu :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.MULU, rd, p1, p2});
}
divu :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.DIVU, rd, p1, p2});
}
modu :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.MODU, rd, p1, p2});
}
modmodu :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.MODMODU, rd, p1, p2});
}
addf :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.ADDF, rd, p1, p2});
}
subf :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.SUBF, rd, p1, p2});
}
mulf :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.MULF, rd, p1, p2});
}
divf :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.DIVF, rd, p1, p2});
}
shl :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.SHL, rd, p1, p2});
}
shr :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.SHR, rd, p1, p2});
}
and :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.AND, rd, p1, p2});
}
or :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.OR, rd, p1, p2});
}
xor :: inline proc(using vm: ^VM, rd, p1, p2: Register) {
	add_instruction(vm, Instruction{.XOR, rd, p1, p2});
}
stack_push :: inline proc(using vm: ^VM, r1: Register) {
	add_instruction(vm, Instruction{.PUSH, r1, 0, 0});
}
stack_pop :: inline proc(using vm: ^VM, r1: Register) {
	add_instruction(vm, Instruction{.POP, r1, 0, 0});
}

// PSEUDOINSTRUCTIONS

call :: inline proc(using vm: ^VM, str: string) {
	stack_push(vm, rip);

	ref := Label_Reference{cast(u64)len(instructions), str};
	append(&label_references, ref);
	movi(vm, rip, 0); // will be backpatched
}
// ret :: inline proc(using vm: ^VM) {
// 	// stack_pop(vm, rt);
// 	// addi(vm, rt, rt, 1);
// 	mov(vm, rip, rt);
// }

movis :: inline proc(using vm: ^VM, rd: Register, p1: i64) {
	movi(vm, rd, transmute(u64)p1);
}
movif :: inline proc(using vm: ^VM, rd: Register, p1: f64) {
	movi(vm, rd, transmute(u64)p1);
}

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
	label_to_ip[str] = instruction_pointer;
}

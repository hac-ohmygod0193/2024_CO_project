//author heng-an
module SP_pipeline(
	// INPUT SIGNAL
	clk,
	rst_n,
	in_valid,
	inst,
	mem_dout,
	// OUTPUT SIGNAL
	out_valid,
	inst_addr,
	mem_wen,
	mem_addr,
	mem_din
);



//------------------------------------------------------------------------
//   INPUT AND OUTPUT DECLARATION                         
//------------------------------------------------------------------------

input                    clk, rst_n, in_valid;
input             [31:0] inst;
input  signed     [31:0] mem_dout;
output reg               out_valid;
output reg        [31:0] inst_addr;
output reg               mem_wen;
output reg        [11:0] mem_addr;
output reg signed [31:0] mem_din;

//------------------------------------------------------------------------
//   DECLARATION
//------------------------------------------------------------------------

// REGISTER FILE, DO NOT EDIT THE NAME.
reg	signed [31:0] r [0:31]; 

// PARAMETER
// opcode //
localparam R_TYPE = 6'b000000;
localparam ANDI = 6'b000001;
localparam ORI = 6'b000010;
localparam ADDI = 6'b000011;
localparam SUBI = 6'b000100;
localparam LW = 6'b000101;
localparam SW = 6'b000110;
localparam BEQ = 6'b000111;
localparam BNE = 6'b001000;
localparam LUI = 6'b001001;
localparam J = 6'b001010;
localparam JAL = 6'b001011;
// ALUop //
localparam AND = 4'b0000;
localparam OR = 4'b0001;
localparam ADD = 4'b0010;
localparam SUB = 4'b0011;
localparam SLT = 4'b0100;
localparam SLL = 4'b0101;
localparam NOR = 4'b0110;
localparam JR = 4'b0111;
localparam LUI_op = 4'b1000;
localparam JAL_op = 4'b1001;
localparam NONE = 4'b1010;
// state
localparam IF = 3'b000;
localparam ID = 3'b001; 
localparam EX = 3'b010;
localparam MEM = 3'b011;
localparam WB = 3'b100;
//------------------------------------------------------------------------
//   DESIGN
//------------------------------------------------------------------------


//valid stage//
reg valid[0:3];

// Control Signal //
reg [3:0] ALUop[0:3];
reg RegDst[0:3], Jump[0:3], Branch[0:3], MemRead[0:3], MemtoReg[0:3], ALUSrc[0:3], MemWrite[0:3], RegWrite[0:3];
// Register //

reg [31:0] PC[0:3];
// PC //
reg [31:0] next_PC;
// ALU //
reg [31:0] ALUResult[0:3];

reg [5:0] opcode[0:3];

reg [4:0] rs[0:3], rt[0:3], rd[0:3];

reg [15:0] immediate[0:3];

reg [31:0] immediate_32[0:3], ze_immediate[0:3];
reg signed [31:0] se_immediate[0:3];

reg [5:0] funct[0:3];

reg [10:6] shamt[0:3];

reg [25:0] jump_address[0:3];

reg [31:0] branchAddr[0:3];
// Mux //
reg [4:0] writeReg[0:3];

reg [31:0] ALUin1[0:3];
reg signed [31:0] ALUin2[0:3];
reg [31:0] WriteData[0:3];


integer i;
integer c;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        for (c = 0; c < 4; c = c + 1) begin
			valid[c] <= 0;
		end
        out_valid <= 0;
        inst_addr <= 0;
        for (i = 0; i < 32; i = i + 1) begin
            r[i] <= 0;
        end
    end
end
// pass the value to next stage
always @(posedge clk) begin
	for (c = 1; c < 4; c = c + 1) begin
		valid[c] <= valid[c-1];
		opcode[c] <= opcode[c-1];
		rs[c] <= rs[c-1];
		rt[c] <= rt[c-1];
		rd[c] <= rd[c-1];
		immediate[c] <= immediate[c-1];
		funct[c] <= funct[c-1];
		shamt[c] <= shamt[c-1];
		jump_address[c] <= jump_address[c-1];

		immediate_32[c] <= immediate_32[c-1];
		ze_immediate[c] <= ze_immediate[c-1];
		se_immediate[c] <= se_immediate[c-1];

		ALUop[c] <= ALUop[c-1];

		RegDst[c] <= RegDst[c-1];
		Jump[c] <= Jump[c-1];
		Branch[c] <= Branch[c-1];
		MemRead[c] <= MemRead[c-1];
		MemtoReg[c] <= MemtoReg[c-1];
		ALUSrc[c] <= ALUSrc[c-1];
		MemWrite[c] <= MemWrite[c-1];
		RegWrite[c] <= RegWrite[c-1];
		writeReg[c] <= writeReg[c-1];

		ALUResult[c] <= ALUResult[c-1];
		PC[c] <= PC[c-1];
	end
end


// Instruction Decode Logic //
always @(*) begin
    // --- Decode --- // c=0
    if(in_valid) begin
		valid[0] <= 1;
        opcode[0] <= inst[31:26];
        rs[0] <= inst[25:21];
        rt[0] <= inst[20:16];
        // R-type
        rd[0] <= inst[15:11];
        shamt[0] <= inst[10:6];
        funct[0] <= inst[5:0];
        // I-type
        immediate[0] <= inst[15:0];
        ze_immediate[0] <= { 16'b0, immediate[0] };
        se_immediate[0] <= { {16{inst[15]}}, immediate[0] };
        // Jump
        jump_address[0] <= inst[25:0];
        
        // --- Control --- //
        case (opcode[0])
			R_TYPE: begin // R-type instructions 0x00
				RegDst[0] = 1; Jump[0] = 0; Branch[0] = 0;
				MemRead[0] = 0; MemtoReg[0] = 0;
				MemWrite[0] = 0; RegWrite[0] = 1;
				ALUSrc[0] = 0;
				// ALUControl //
				case (funct[0])
					6'b000000: ALUop[0] = AND; // and 0x00
					6'b000001: ALUop[0] = OR; // or 0x01
					6'b000010: ALUop[0] = ADD; // add 0x02
					6'b000011: ALUop[0] = SUB; // sub 0x03
					6'b000100: ALUop[0] = SLT; // slt 0x04
					6'b000101: ALUop[0] = SLL; // sll 0x05  
					6'b000110: ALUop[0] = NOR; // nor 0x06
					6'b000111: begin       // jr  0x07
						ALUop[0] = JR;
						RegWrite[0] = 0;
					end
					default: ALUop[0] = AND; // default case
				endcase
			end
			ANDI: begin // andi 0x01
				RegDst[0] = 0; Jump[0] = 0; Branch[0] = 0;
				MemRead[0] = 0; MemtoReg[0] = 0;
				MemWrite[0] = 0; RegWrite[0] = 1;
				ALUSrc[0] = 1; ALUop[0] = AND; // AND operation
			end
			ORI: begin // ori 0x02
				RegDst[0] = 0; Jump[0] = 0; Branch[0] = 0;
				MemRead[0] = 0; MemtoReg[0] = 0;
				MemWrite[0] = 0; RegWrite[0] = 1;
				ALUSrc[0] = 1; ALUop[0] = OR; // OR operation
			end
			ADDI: begin // addi 0x03
				RegDst[0] = 0; Jump[0] = 0; Branch[0] = 0;
				MemRead[0] = 0; MemtoReg[0] = 0;
				MemWrite[0] = 0; RegWrite[0] = 1;
				ALUSrc[0] = 1; ALUop[0] = ADD; // ADD operation
			end
			SUBI: begin // subi 0x04
				RegDst[0] = 0; Jump[0] = 0; Branch[0] = 0;
				MemRead[0] = 0; MemtoReg[0] = 0;
				MemWrite[0] = 0; RegWrite[0] = 1;
				ALUSrc[0] = 1; ALUop[0] = SUB; // SUB operation
			end
			LW: begin // lw 0x05
				RegDst[0] = 0; Jump[0] = 0; Branch[0] = 0;
				MemRead[0] = 1; MemtoReg[0] = 1;
				MemWrite[0] = 0; RegWrite[0] = 1;
				ALUSrc[0] = 1; ALUop[0] = ADD; // ADD for address calculation
			end
			SW: begin // sw 0x06
				RegDst[0] = 0; Jump[0] = 0; Branch[0] = 0;
				MemRead[0] = 0; MemtoReg[0] = 0;
				MemWrite[0] = 1; RegWrite[0] = 0;
				ALUSrc[0] = 1; ALUop[0] = ADD; // ADD for address calculation
			end
			BEQ: begin // beq 0x07
				RegDst[0] = 0; Jump[0] = 0; Branch[0] = 1;
				MemRead[0] = 0; MemtoReg[0] = 0;
				MemWrite[0] = 0; RegWrite[0] = 0;
				ALUSrc[0] = 0; ALUop[0] = NONE;
				ALUResult[0] = (r[rs[0]]==r[rt[0]]) ? se_immediate[0] : 0;
			end
			BNE: begin // bne 0x08
				RegDst[0] = 0; Jump[0] = 0; Branch[0] = 1;
				MemRead[0] = 0; MemtoReg[0] = 0;
				MemWrite[0] = 0; RegWrite[0] = 0;
				ALUSrc[0] = 0; ALUop[0] = NONE;
				ALUResult[0] = (r[rs[0]]!=r[rt[0]]) ? se_immediate[0] : 0;
			end
			LUI: begin // lui 0x09
				RegDst[0] = 0; Jump[0] = 0; Branch[0] = 0;
				MemRead[0] = 0; MemtoReg[0] = 0;
				MemWrite[0] = 0; RegWrite[0] = 1;
				ALUSrc[0] = 1; ALUop[0] = LUI_op; // LUI operation
			end
			J: begin // J
				RegDst[0] = 0; Jump[0] = 1; Branch[0] = 0;
				MemRead[0] = 0; MemtoReg[0] = 0;
				MemWrite[0] = 0; RegWrite[0] = 0;
				ALUSrc[0] = 0; ALUop[0] = NONE;
			end
			JAL: begin // Jal
				RegDst[0] = 0; Jump[0] = 1; Branch[0] = 0;
				MemRead[0] = 0; MemtoReg[0] = 0;
				MemWrite[0] = 0; RegWrite[0] = 0;
				ALUSrc[0] = 0; ALUop[0] = JAL_op;
				PC[0] = inst_addr + 4;
                
			end
			default: begin
				RegDst[0] = 0; Jump[0] = 0; Branch[0] = 0;
				MemRead[0] = 0; MemtoReg[0] = 0;
				MemWrite[0] = 0; RegWrite[0] = 0;
				ALUSrc[0] = 0; ALUop[0] = NONE;
			end
		endcase

        // --- Read Register --- //
		
        
        // --- Write Register --- //
        writeReg[0] = (RegDst[0]) ? rd[0] : rt[0]; // RegDst MUX    
    end
	else begin
		valid[0] <= 0;
	end
end
// Program Counter Logic //
always @(*) begin
    if(in_valid) begin
        case(opcode[0])
            R_TYPE: begin
                if(funct[0] == JR) next_PC = r[rs[0]];
                else next_PC = inst_addr + 4;
            end
			ANDI, ORI, ADDI, SUBI, LW, SW: next_PC = inst_addr + 4;
            BEQ: next_PC = inst_addr + 4 + ((r[rs[0]] == r[rt[0]]) ? (se_immediate[0] << 2) : 0);
            BNE: next_PC = inst_addr + 4 + ((r[rs[0]] != r[rt[0]]) ? (se_immediate[0] << 2) : 0);
            J:   next_PC = {inst_addr[31:28], jump_address[0] << 2};
            JAL: begin
				next_PC = {inst_addr[31:28], jump_address[0] << 2};
            end
            default: next_PC = inst_addr + 4;
        endcase
    end
end
// Execute Logic //
always @(*) begin
	if(valid[1]) begin  // EX stage(c=1)
		// --- Execute --- //
		// --- ALU --- //
		case (ALUop[1])
			AND: begin
				if(opcode[1]==R_TYPE) begin
					ALUResult[1] = r[rs[1]] & r[rt[1]];
				end
				else begin
					ALUResult[1] = r[rs[1]] & ze_immediate[1];
				end
			end
			OR: begin
				if(opcode[1]==R_TYPE) begin
					ALUResult[1] = r[rs[1]] | r[rt[1]];
				end
				else begin
					ALUResult[1] = r[rs[1]] | ze_immediate[1];
				end
			end
			ADD: begin
				if(opcode[1]==R_TYPE) begin
					ALUResult[1] = r[rs[1]] + r[rt[1]];
				end
				else begin
					ALUResult[1] = r[rs[1]] + se_immediate[1];
				end
			end
			SUB: begin
				if(opcode[1]==R_TYPE) begin
					ALUResult[1] = r[rs[1]] - r[rt[1]];
				end
				else begin
					ALUResult[1] = r[rs[1]] - se_immediate[1];
				end
			end
			SLT: begin //slt
				if (r[rs[1]] < r[rt[1]]) begin
					ALUResult[1] = 1;
				end else begin
					ALUResult[1] = 0;
				end
			end
			SLL: ALUResult[1] = r[rs[1]] << shamt[1]; // sll , r[rs[1]] = r[rs] << shamt
			NOR: ALUResult[1] = ~(r[rs[1]] | r[rt[1]]); // nor
			JR: begin // jr
				ALUResult[1] = r[rs[1]];
			end
			LUI_op: ALUResult[1] = {immediate[1], 16'b0}; // lui
			default: ALUResult[1] = 0;
		endcase
		
	end
end



always @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        inst_addr <= 0;
		next_PC <= 0;
    end
    else begin
        inst_addr <= next_PC;
    end
end
always @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        mem_addr <= 0;
        mem_din <= 0;
        mem_wen <= 1;
    end
    else if (valid[1]) begin // EX stage c=1
        mem_addr <= (opcode[1]==LW || opcode[1]==SW) ? ALUResult[1] : 0;
        mem_din <= (opcode[1]==SW) ? r[rt[1]] : 0;
        mem_wen <= (opcode[1]==SW) ? 0 : 1;
    end
end

// Memory Access Logic //
always @(*) begin
    if(valid[2]) begin // MEM stage c=2           
        // --- Memory --- //
        case(opcode[2])
            LW: begin // lw
                mem_addr = ALUResult[2];
                //r[rt[2]] = mem_dout;  // mem_out is the data read from memory 
            end
            SW: begin // sw
                mem_wen = 0; // low to store data to memory
                mem_addr = ALUResult[2];
                mem_din = r[rt[2]]; // mem_in is the data to be written to memory
            end
            default: begin
                mem_addr = 0;
                mem_din = 0;
            end
        endcase
    end
end
// Write Back Logic //
always @(posedge clk) begin
    if(valid[3]) begin // WB stage c=3
        // --- Write Back --- //
		case(opcode[3]) 
            R_TYPE: if(funct[3] != JR) r[rd[3]] = ALUResult[3];
            ANDI, ORI, ADDI, SUBI, LUI: r[rt[3]] = ALUResult[3];
            LW: r[rt[3]] = mem_dout;
            JAL: begin 
				r[31] = PC[3];
			end
        endcase
		
    end
end
always @(posedge clk or negedge rst_n) begin
	if(!rst_n) begin
		out_valid <= 0;
	end
	else begin
		out_valid <= valid[3];
	end
end
endmodule
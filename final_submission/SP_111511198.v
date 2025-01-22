// author heng-an
module SP(
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
// State Machine //
reg [2:0] state, next_state;

// Control Signal //
reg [3:0] ALUop;
reg RegDst, Jump, Branch, MemRead, MemtoReg, ALUSrc, MemWrite, RegWrite;
// Register //
reg signed [31:0] readData1, readData2;

// PC //
reg [31:0] temp_PC;
reg [31:0] PC, next_PC;
// ALU //
reg [1:0] ALUdo;
reg [31:0] ALUResult;

reg [5:0] opcode;

reg [4:0] rs, rt, rd;

reg [15:0] immediate;

reg [31:0] immediate_32, ze_immediate, se_immediate;

reg [5:0] funct;

reg [10:6] shamt;

reg [25:0] jump_address;

reg [31:0] branchAddr;
// Mux //
reg [4:0] writeReg;

reg [31:0] ALUin1, ALUin2;
reg [31:0] WriteData;

// FSM State Logic //
integer i;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        state <= IF;
        out_valid <= 0;
        inst_addr <= 0;
        for (i = 0; i < 32; i = i + 1) begin
            r[i] <= 0;
        end
    end
    else begin
        state <= next_state;
    end
end
always @(*) begin
    case(state)
        IF: begin
            if(in_valid) begin
                next_state = ID;
            end
            else begin
                next_state = IF;
            end
        end
        ID: begin
            next_state = EX;
        end
        EX: begin
            if(opcode == LW || opcode == SW) begin
                next_state = MEM;
            end
            else begin
                next_state = WB;
            end
        end
        MEM: begin
            next_state = WB;
        end
        WB: begin
            if(in_valid) begin
                next_state = ID;
            end
            else begin
                next_state = IF;
            end
        end
        default: begin
            next_state = IF;
        end
    endcase
end

// Instruction Decode Logic //
always @(*) begin
    // --- Decode --- //
    if(in_valid) begin
        PC <= inst_addr + 4;
        opcode <= inst[31:26];
        rs <= inst[25:21];
        rt <= inst[20:16];
        // R-type
        rd <= inst[15:11];
        shamt <= inst[10:6];
        funct <= inst[5:0];
        // I-type
        immediate <= inst[15:0];
        ze_immediate <= { 16'b0, immediate };
        se_immediate <= { {16{inst[15]}}, immediate };
        // Jump
        jump_address <= inst[25:0];
        
        // --- Control --- //
        case (opcode)
            R_TYPE: begin // R-type instructions 0x00
                RegDst = 1; Jump = 0; Branch = 0;
                MemRead = 0; MemtoReg = 0;
                MemWrite = 0; RegWrite = 1;
                ALUSrc = 0;
                // ALUControl //
                case (funct)
                    6'b000000: ALUop = AND; // and 0x00
                    6'b000001: ALUop = OR; // or 0x01
                    6'b000010: ALUop = ADD; // add 0x02
                    6'b000011: ALUop = SUB; // sub 0x03
                    6'b000100: ALUop = SLT; // slt 0x04
                    6'b000101: ALUop = SLL; // sll 0x05  
                    6'b000110: ALUop = NOR; // nor 0x06
                    6'b000111: begin       // jr  0x07
                        ALUop = JR;
                        RegWrite = 0;
                    end
                    default: ALUop = AND; // default case
                endcase
            end
            ANDI: begin // andi 0x01
                RegDst = 0; Jump = 0; Branch = 0;
                MemRead = 0; MemtoReg = 0;
                MemWrite = 0; RegWrite = 1;
                ALUSrc = 1; ALUop = AND; // AND operation
            end
            ORI: begin // ori 0x02
                RegDst = 0; Jump = 0; Branch = 0;
                MemRead = 0; MemtoReg = 0;
                MemWrite = 0; RegWrite = 1;
                ALUSrc = 1; ALUop = OR; // OR operation
            end
            ADDI: begin // addi 0x03
                RegDst = 0; Jump = 0; Branch = 0;
                MemRead = 0; MemtoReg = 0;
                MemWrite = 0; RegWrite = 1;
                ALUSrc = 1; ALUop = ADD; // ADD operation
            end
            SUBI: begin // subi 0x04
                RegDst = 0; Jump = 0; Branch = 0;
                MemRead = 0; MemtoReg = 0;
                MemWrite = 0; RegWrite = 1;
                ALUSrc = 1; ALUop = SUB; // SUB operation
            end
            LW: begin // lw 0x05
                RegDst = 0; Jump = 0; Branch = 0;
                MemRead = 1; MemtoReg = 1;
                MemWrite = 0; RegWrite = 1;
                ALUSrc = 1; ALUop = ADD; // ADD for address calculation
            end
            SW: begin // sw 0x06
                RegDst = 0; Jump = 0; Branch = 0;
                MemRead = 0; MemtoReg = 0;
                MemWrite = 1; RegWrite = 0;
                ALUSrc = 1; ALUop = ADD; // ADD for address calculation
            end
            BEQ: begin // beq 0x07
                RegDst = 0; Jump = 0; Branch = 1;
                MemRead = 0; MemtoReg = 0;
                MemWrite = 0; RegWrite = 0;
                ALUSrc = 0; ALUop = NONE;
                ALUResult = (r[rs]==r[rt]) ? se_immediate : 0;
            end
            BNE: begin // bne 0x08
                RegDst = 0; Jump = 0; Branch = 1;
                MemRead = 0; MemtoReg = 0;
                MemWrite = 0; RegWrite = 0;
                ALUSrc = 0; ALUop = NONE;
                ALUResult = (r[rs]!=r[rt]) ? se_immediate : 0;
            end
            LUI: begin // lui 0x09
                RegDst = 0; Jump = 0; Branch = 0;
                MemRead = 0; MemtoReg = 0;
                MemWrite = 0; RegWrite = 1;
                ALUSrc = 1; ALUop = LUI_op; // LUI operation
            end
            J: begin // J
                RegDst = 0; Jump = 1; Branch = 0;
                MemRead = 0; MemtoReg = 0;
                MemWrite = 0; RegWrite = 0;
                ALUSrc = 0; ALUop = NONE;
            end
            JAL: begin // Jal
                RegDst = 0; Jump = 1; Branch = 0;
                MemRead = 0; MemtoReg = 0;
                MemWrite = 0; RegWrite = 0;
                ALUSrc = 0; ALUop = JAL_op;
            end
            default: begin
                RegDst = 0; Jump = 0; Branch = 0;
                MemRead = 0; MemtoReg = 0;
                MemWrite = 0; RegWrite = 0;
                ALUSrc = 0; ALUop = NONE;
            end
        endcase

        // --- Read Register --- //
        readData1 <= r[rs];
        readData2 <= r[rt];
        // --- Write Register --- //
        writeReg = (RegDst) ? rd : rt; // RegDst MUX
        // --- ALU input --- //
        ALUin1 <= readData1;
        immediate_32 <= (opcode==ANDI || opcode==ORI) ? ze_immediate : se_immediate;
        ALUin2 <= (ALUSrc) ? immediate_32 : readData2; // ALUSrc MUX
    end
end

// Execute Logic //
always @(*) begin
    if(state == EX) begin         
        // --- Execute --- //
        // --- ALU --- //
        case (ALUop)
            AND: ALUResult = readData1 & ALUin2; // and
            OR: ALUResult = readData1 | ALUin2; // or
            ADD: ALUResult = readData1 + ALUin2; // add
            SUB: ALUResult = readData1 - ALUin2; // sub
            SLT: begin //slt
                if (readData1 < readData2) begin
                    ALUResult = 1;
                end else begin
                    ALUResult = 0;
                end
            end
            SLL: ALUResult = readData1 << shamt; // sll , readData1 = r[rs] << shamt
            NOR: ALUResult = ~(readData1 | ALUin2); // nor
            JR: begin // jr
                ALUResult = readData1;
            end
            LUI_op: ALUResult = {immediate, 16'b0}; // lui
            JAL_op: ALUResult = inst_addr + 4; // jal
            default: ALUResult = 0;
        endcase
        mem_addr <= (MemWrite || MemRead) ? ALUResult : 0;
        mem_din <= (MemWrite) ? readData2 : 0;
        mem_wen <= ~MemWrite;
    end
end

// Program Counter Logic //
always @(*) begin
    next_PC = inst_addr;
    if (state == EX) begin
        case(opcode)
            R_TYPE: begin
                if(funct == JR) next_PC = r[rs];
                else next_PC = inst_addr + 4;
            end
            ORI: next_PC = inst_addr + 4;
            ADDI: next_PC = inst_addr + 4;
            SUBI: next_PC = inst_addr + 4;
            LW: next_PC = inst_addr + 4;
            SW: next_PC = inst_addr + 4;
            BEQ: next_PC = inst_addr + 4 + ((r[rs] == r[rt]) ? (se_immediate << 2) : 0);
            BNE: next_PC = inst_addr + 4 + ((r[rs] != r[rt]) ? (se_immediate << 2) : 0);
            J:   next_PC = {inst_addr[31:28], jump_address << 2};
            JAL: begin
                r[31] = ALUResult;
                next_PC = {inst_addr[31:28], jump_address << 2};
            end
            default: next_PC = inst_addr + 4;
        endcase
    end
end

always @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        PC <= 0;
    end
    else begin
        PC <= next_PC;
    end
end
always @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        mem_addr <= 0;
        mem_din <= 0;
        mem_wen <= 1;
    end
    else if (state == EX) begin
        mem_addr <= (opcode==LW || opcode==SW) ? ALUResult : 0;
        mem_din <= (opcode==SW) ? r[rt] : 0;
        mem_wen <= (opcode==SW) ? 0 : 1;
    end
end

// Next PC Logic //
always @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        PC <= 0;
        inst_addr <= 0;
        next_PC <= 4;
    end
    else begin
        inst_addr <= next_PC;
    end
end

// Memory Access Logic //
always @(*) begin
    if(state == MEM) begin           
        // --- Memory --- //
        case(opcode)
            6'b000101: begin // lw
                mem_addr = ALUResult;
                r[rt] = mem_dout;  // mem_out is the data read from memory 
            end
            6'b000110: begin // sw
                mem_wen = 0; // low to store data to memory
                mem_addr = ALUResult;
                mem_din = readData2; // mem_in is the data to be written to memory
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
    if(state == WB) begin
        // --- Write Back --- //
        if(RegWrite) begin
            r[writeReg] <= (MemtoReg) ? mem_dout : ALUResult;
        end
        out_valid <= 1;
    end
    else begin
        out_valid <= 0;
    end
end
endmodule
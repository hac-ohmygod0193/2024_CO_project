// author heng-an
`define CYCLE_TIME 10
`timescale 1ns/10ps
module PATTERN_p(
    // Output Signals
    clk,
    rst_n,
    in_valid,
    inst,
    // Input Signals
    out_valid,
    inst_addr
);

//================================================================
//   Input and Output Declaration                         
//================================================================

output reg clk,rst_n,in_valid;
output reg [31:0] inst;

input wire out_valid;
input wire [31:0] inst_addr;

//================================================================
// parameters & integer
//================================================================

integer execution_num = 1000, out_max_latency = 10, seed = 64;
integer i, t, latency, out_valid_counter, in_valid_counter;
integer golden_inst_addr_in, golden_inst_addr_out,pat; // ************** Program Counter ************* //
integer instruction [999:0];                           // ******** Instruction (from inst.txt) ******* //
integer opcode,rs,rt,rd,shamt,func,immediate, address;
integer golden_r [31:0];                               // *********** Gloden answer for Reg ********** //
integer mem [4095:0];                                  // ******** Data Memory (from mem.txt) ******** //

//================================================================
// clock setting
//================================================================

real CYCLE = `CYCLE_TIME;

always #(CYCLE/2.0) clk = ~clk;

//================================================================
// initial
//================================================================

initial begin

    // read data mem & instrction
    $readmemh("instruction.txt", instruction);
    $readmemh("mem.txt", mem);

    // initialize control signal 
    rst_n = 1'b1;
    in_valid = 1'b0;

    // initial variable
    golden_inst_addr_in = 0;
    golden_inst_addr_out = 0;
    in_valid_counter = 0;
    out_valid_counter = 0;
    latency = -1;
    for(i = 0; i < 32; i = i + 1)begin
        golden_r[i] = 0;
    end

    // inst=X
    inst = 32'bX;

    // reset check task
    reset_check_task;

    // generate random idle clk
	t = $random(seed) % 3 + 1'b1;
	repeat(t) @(negedge clk);

    // main pattern
	while(out_valid_counter < execution_num)begin
		input_task;
        check_ans_task;
        @(negedge clk);
	end

    // check out_valid
    check_memory_and_out_valid;
    display_pass_task;

end
//================================================================
// task
//================================================================

// reset check task
task reset_check_task; begin

    // force clk
    force clk = 0;

    // generate reset signal
    #CYCLE; rst_n = 1'b0;
    #CYCLE; rst_n = 1'b1;

    // check output signal=0
    if(out_valid !== 1'b0 || inst_addr !== 32'd0)begin
        $display("************************************************************");     
        $display("*  Output signal should be 0 after initial RESET  at %8t   *",$time);
        $display("************************************************************");
        repeat(2) #CYCLE;
        $finish;
    end

    // check r
    for(i = 0; i < 32; i = i + 1)begin
        if(My_SP.r[i]!==32'd0)begin
            $display("************************************************************");     
            $display("*  Register r should be 0 after initial RESET  at %8t      *",$time);
            $display("************************************************************");
            repeat(2) #CYCLE;
            $finish;
        end
    end

    // release clk
    #CYCLE; release clk;

end
endtask
reg [31:0] ze_immediate; 
reg signed [31:0] se_immediate;
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
// input task
task input_task; begin

    // input
    if(in_valid_counter < execution_num)begin

        // check inst_addr        
        if(inst_addr !== golden_inst_addr_in)begin
            display_fail_task;
            $display("inst_addr %d",inst_addr);
            $display("opcode %d",opcode);
            $display("func %d",func);
            $display("-------------------------------------------------------------------");
            $display("*                        PATTERN NO.%4d 	                        *",in_valid_counter);
            $display("*                          inst_addr  error 	                    *");
            $display("*                          opcode %d, inst %h                     *", opcode, inst);
            $display("*          answer should be : %d , your answer is : %d            *",golden_inst_addr_in,inst_addr);
            $display("-------------------------------------------------------------------");
            repeat(2) @(negedge clk);
            $finish;
        end

        // inst=? ,in_valid=1
        inst = instruction[golden_inst_addr_in>>2];
        in_valid = 1'b1;        
        // $display(" %h ", inst);

        // PC
        opcode = instruction[golden_inst_addr_in>>2][31:26];
        rs = instruction[golden_inst_addr_in>>2][25:21];
        // You can calculate golden_inst_addr_in is (triggered by Jump, beq...etc.) or (PC + 4), as a check for design output inst_addr
        // PC & jump, beq...etc.
        // hint: it's necessary to consider sign extension while calculating
        // R-type
        rt = instruction[golden_inst_addr_in>>2][20:16];
        rd = instruction[golden_inst_addr_in>>2][15:11];
        shamt = instruction[golden_inst_addr_in>>2][10:6];
        func = instruction[golden_inst_addr_in>>2][5:0];
        // I-type
        immediate = instruction[golden_inst_addr_in>>2][15:0];
        ze_immediate = {16'b0,immediate};
        se_immediate = (immediate[15] == 1'b1) ? {16'hffff, immediate[15:0]} : immediate;
        // PC & jump, beq...etc.
        address = instruction[golden_inst_addr_in>>2][25:0];


        case(opcode)
            // R-type
            R_TYPE: begin
                if(func!=JR)begin
                    golden_inst_addr_in = golden_inst_addr_in + 4;
                end
                else golden_inst_addr_in = golden_r[rs]; // JR
            end
            // ANDI, ORI, ADDI, SUBI, LW, SW
            ANDI, ORI, ADDI, SUBI, LW, SW: begin
                golden_inst_addr_in = golden_inst_addr_in + 4;
            end
            // BEQ
            BEQ: begin
                if(golden_r[rs] == golden_r[rt])begin
                    golden_inst_addr_in = golden_inst_addr_in + 4 + {se_immediate,2'b00};
                end
                else begin
                    golden_inst_addr_in = golden_inst_addr_in + 4;
                end
            end
            // BNE
            BNE: begin
                if(golden_r[rs] != golden_r[rt])begin
                    golden_inst_addr_in = golden_inst_addr_in + 4 + {se_immediate,2'b00};
                end
                else begin
                    golden_inst_addr_in = golden_inst_addr_in + 4;
                end
            end
            // LUI
            LUI: begin
                golden_inst_addr_in = golden_inst_addr_in + 4;
            end
            // J
            J: begin
                golden_inst_addr_in = {golden_inst_addr_in[31:28],address<<2};
            end
            // JAL
            JAL: begin
                golden_inst_addr_in = {golden_inst_addr_in[31:28],address<<2};
            end
            
        endcase

        // in_valid_counter
        in_valid_counter = in_valid_counter + 1;

    end
    else begin
        // inst = x ,in_valid = 0
        inst = 32'bX;
        in_valid = 1'b0;
    end

end
endtask

// check_ans_task
task check_ans_task; begin

    // check out_valid
    if(out_valid)begin
        // answer calculate
        opcode = instruction[golden_inst_addr_out>>2][31:26];
        rs = instruction[golden_inst_addr_out>>2][25:21];
        // R-type
        // I-type
        // PC & jump, beq...etc.
        // hint: it's necessary to consider sign extension while calculating
        
        // R-type
        rt = instruction[golden_inst_addr_out>>2][20:16];
        rd = instruction[golden_inst_addr_out>>2][15:11];
        shamt = instruction[golden_inst_addr_out>>2][10:6];
        func = instruction[golden_inst_addr_out>>2][5:0];
        // I-type
        immediate = instruction[golden_inst_addr_out>>2][15:0];
        ze_immediate = {16'b0,immediate};
        se_immediate = (immediate[15] == 1'b1) ? {16'hffff, immediate[15:0]} : immediate;
        // PC & jump, beq...etc.
        address = instruction[golden_inst_addr_out>>2][25:0];
        
        // calculate golden answer
        case(opcode)
            // R-type
            R_TYPE: begin
                if(func!=JR)begin
                    golden_inst_addr_out = golden_inst_addr_out + 4;
                end
                case(func)
                    // AND
                    AND: begin
                        golden_r[rd] = golden_r[rs] & golden_r[rt];
                    end
                    // OR
                    OR: begin
                        golden_r[rd] = golden_r[rs] | golden_r[rt];
                    end
                    // ADD
                    ADD: begin
                        golden_r[rd] = golden_r[rs] + golden_r[rt];
                    end
                    // SUB
                    SUB: begin
                        golden_r[rd] = golden_r[rs] - golden_r[rt];
                    end
                    // SLT
                    SLT: begin
                        if(golden_r[rs] < golden_r[rt])begin
                            golden_r[rd] = 1;
                        end
                        else begin
                            golden_r[rd] = 0;
                        end
                    end
                    // SLL
                    SLL: begin
                        golden_r[rd] = golden_r[rs] << shamt;
                    end
                    // NOR
                    NOR: begin
                        golden_r[rd] = ~(golden_r[rs] | golden_r[rt]);
                    end
                    // JR
                    JR: begin
                        golden_inst_addr_out = golden_r[rs];
                    end
                endcase
            end
            // ANDI
            ANDI: begin
                golden_r[rt] = golden_r[rs] & ze_immediate;
                golden_inst_addr_out = golden_inst_addr_out + 4;
            end
            // ORI
            ORI: begin
                golden_r[rt] = golden_r[rs] | ze_immediate;
                golden_inst_addr_out = golden_inst_addr_out + 4;
            end
            // ADDI
            ADDI: begin
                golden_r[rt] = golden_r[rs] + se_immediate;
                golden_inst_addr_out = golden_inst_addr_out + 4;
            end
            // SUBI
            SUBI: begin
                golden_r[rt] = golden_r[rs] - se_immediate;
                golden_inst_addr_out = golden_inst_addr_out + 4;
            end
            // LW
            LW: begin
                golden_r[rt] = mem[golden_r[rs] + se_immediate];
                if(golden_r[rs] + se_immediate < 0 || golden_r[rs] + se_immediate > 4096)begin
                    display_fail_task;
                    $display(se_immediate);
                    $display(golden_r[rs]);
                    $display("-------------------------------------------------------------------");
                    $display("*                        PATTERN NO.%4d 	                        *",pat);
                    $display("*                 memory out of range on lw                       *");
                    $display("*                    Mem_Addr overflow @%d                        *",pat);
                    $display("-------------------------------------------------------------------");
                    repeat(2) @(negedge clk);
                    $finish;
                end
                golden_inst_addr_out = golden_inst_addr_out + 4;
            end
            // SW
            SW: begin
                mem[golden_r[rs] + se_immediate] = golden_r[rt];
                if(golden_r[rs] + se_immediate < 0 || golden_r[rs] + se_immediate > 4096)begin
                    display_fail_task;
                    $display(se_immediate);
                    $display(golden_r[rs]);
                    $display("-------------------------------------------------------------------");
                    $display("*                        PATTERN NO.%4d 	                        *",pat);
                    $display("*                 memory out of range on lw                       *");
                    $display("*                    Mem_Addr overflow @%d                        *",pat);
                    $display("-------------------------------------------------------------------");
                    repeat(2) @(negedge clk);
                    $finish;
                end
                golden_inst_addr_out = golden_inst_addr_out + 4;
            end
            // BEQ
            BEQ: begin
                if(golden_r[rs] == golden_r[rt])begin
                    golden_inst_addr_out = golden_inst_addr_out + 4 + {se_immediate,2'b00};
                end
                else begin
                    golden_inst_addr_out = golden_inst_addr_out + 4;
                end
            end
            // BNE
            BNE: begin
                if(golden_r[rs] != golden_r[rt])begin
                    golden_inst_addr_out = golden_inst_addr_out + 4 + {se_immediate,2'b00};
                end
                else begin
                    golden_inst_addr_out = golden_inst_addr_out + 4;
                end
            end
            // LUI
            LUI: begin
                golden_r[rt] = {immediate,16'b0};
                golden_inst_addr_out = golden_inst_addr_out + 4;
            end
            // J
            J: begin
                golden_inst_addr_out = {golden_inst_addr_out[31:28],address<<2};
            end
            // JAL
            JAL: begin
                golden_r[31] = golden_inst_addr_out + 4;
                golden_inst_addr_out = {golden_inst_addr_out[31:28],address<<2};
            end
            
        endcase

        // out_valid_counter+
        out_valid_counter = out_valid_counter+1;

        // check register
        for(i = 0; i < 32; i = i + 1)begin
            if(My_SP.r[i] !== golden_r[i])begin
                display_fail_task;
                $display("inst_addr %d",golden_inst_addr_out);
                $display("opcode %d",opcode);
                $display("func %d",func);
                $display("rs %d, r[rs] %d",rs, golden_r[rs]);
                $display("immediate %d",immediate);

                $display("-------------------------------------------------------------------");
                $display("*                        PATTERN NO.%4d 	                        *",out_valid_counter);
                $display("*                   register [%2d]  error 	                    *",i);
                $display("*          answer should be : %d , your answer is : %d            *",golden_r[i],My_SP.r[i]);
                $display("-------------------------------------------------------------------");
                repeat(2) @(negedge clk);
                $finish;
            end
        end
        
    end
    else begin
        // check execution cycle
        if(out_valid_counter == 0)begin
            latency = latency+1;
            if(latency == out_max_latency)begin
                $display("***************************************************");     
                $display("*   the execution cycles are more than 10 cycles  *",$time);
                $display("***************************************************");
                repeat(2) @(negedge clk);
                $finish;
            end

        end
        // check out_valid pulled down
        else begin
            $display("************************************************************");     
            $display("*  out_valid should not fall when executing  at %8t        *",$time);
            $display("************************************************************");
            repeat(2) #CYCLE;
            $finish;
        end
    end
    $display("*                        Pass PATTERN NO.%4d 	                  *",out_valid_counter);

end
endtask

// check_memory_and_out_valid
task check_memory_and_out_valid; begin

    // check memory
    for(i = 0; i < 4096; i = i + 1)begin
        if(My_MEM.mem[i] !== mem[i])begin
            display_fail_task;
            $display("opcode %d",opcode);
            $display("func %d",func);
            $display("-------------------------------------------------------------------");
            $display("*                     MEM [%4d]  error                            *",i);
            $display("*          answer should be : %d , your answer is : %d            *",mem[i],My_MEM.mem[i]);
            $display("-------------------------------------------------------------------");
            repeat(2) @(negedge clk);
            $finish;
        end
    end

    // check out_valid
    if(out_valid == 1'b1)begin
        $display("************************************************************");     
        $display("*  out_valid should be low after finish execute at %8t     *",$time);
        $display("************************************************************");
        repeat(2) #CYCLE;
        $finish;
    end

end
endtask

// display fail task
task display_fail_task; begin

        $display("\n");
        $display("        ----------------------------               ");
        $display("        --                        --       |\__||  ");
        $display("        --  OOPS!!                --      / X,X  | ");
        $display("        --                        --    /_____   | ");
        $display("        --  \033[0;31mSimulation Failed!!\033[m   --   /^ ^ ^ \\  |");
        $display("        --                        --  |^ ^ ^ ^ |w| ");
        $display("        ----------------------------   \\m___m__|_|");
        $display("\n");
end 
endtask

// display pass task
task display_pass_task; begin

        $display("\n");
        $display("        ----------------------------               ");
        $display("        --                        --       |\__||  ");
        $display("        --  Congratulations !!    --      / O.O  | ");
        $display("        --                        --    /_____   | ");
        $display("        --  \033[0;32mSimulation PASS!!\033[m     --   /^ ^ ^ \\  |");
        $display("        --                        --  |^ ^ ^ ^ |w| ");
        $display("        ----------------------------   \\m___m__|_|");
        $display("\n");
		repeat(2) @(negedge clk);
		$finish;

end 
endtask

endmodule
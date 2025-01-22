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

// input task
task input_task; begin

    // input
    if(in_valid_counter < execution_num)begin

        // check inst_addr        
        if(inst_addr !== golden_inst_addr_in)begin
            display_fail_task;
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
        /*

            Complete your function validation here

        */

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
        /*



            Complete your function validation here



        */

        // out_valid_counter+
        out_valid_counter = out_valid_counter+1;

        // check register
        for(i = 0; i < 32; i = i + 1)begin
            if(My_SP.r[i] !== golden_r[i])begin
                display_fail_task;
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
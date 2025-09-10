`include "uvm_macros.svh"
import uvm_pkg::*;

//////// Interface /////

interface intf(input logic clk, input logic rst);
	logic [7:0] wr_data;
    	logic wr_en;
    	logic rd_en;
    	logic full;
    	logic empty;
    	logic [7:0] rd_data;
    	logic rd_valid;
endinterface

////////// Transaction //////

class my_transaction extends uvm_sequence_item;
	rand logic [7:0] wr_data;
    	rand logic wr_en;
    	rand logic rd_en;
    	logic full;
    	logic empty;
    	logic [7:0] rd_data;
    	logic rd_valid;
	
	function new(string name="my_transaction");
		super.new(name);
	endfunction
	`uvm_object_utils_begin(my_transaction)
		`uvm_field_int(wr_data,UVM_ALL_ON)
		`uvm_field_int(wr_en,UVM_ALL_ON)
		`uvm_field_int(rd_en,UVM_ALL_ON)
		`uvm_field_int(full,UVM_ALL_ON)
		`uvm_field_int(empty,UVM_ALL_ON)
		`uvm_field_int(rd_data,UVM_ALL_ON)
		`uvm_field_int(rd_valid,UVM_ALL_ON)
	`uvm_object_utils_end
endclass

////////// Sequence //////////

class my_sequence extends uvm_sequence#(my_transaction);
	`uvm_object_utils(my_sequence)
	
	function new(string name = "my_sequence");
		super.new(name);
	endfunction

	task body();
		repeat (520) begin 
			my_transaction trans;
			trans = my_transaction :: type_id :: create("trans");
			start_item(trans);
			assert(trans.randomize() with {trans.wr_en ==1 ; trans.rd_en ==0;});
			finish_item(trans);
		end
		repeat (2) begin
			my_transaction trans;
			trans = my_transaction :: type_id :: create("trans");
			start_item(trans);
			trans.wr_en = 0;
			trans.rd_en = 0;
			trans.wr_data = 0;
			finish_item(trans);
		end
		/*repeat (520) begin 
			my_transaction trans;
			trans = my_transaction :: type_id :: create("trans");
			start_item(trans);
			trans.wr_en = 0; 
			trans.rd_en = 1;
			trans.wr_data=0;
			finish_item(trans);
		end*/
	endtask
endclass


////////// Sequencer ////

class my_sequencer extends uvm_sequencer#(my_transaction);
	`uvm_component_utils(my_sequencer)
	
	function new(string name ="my_sequencer", uvm_component parent);
		super.new(name,parent);
	endfunction

endclass

////////// Driver //////

class my_driver extends uvm_driver#(my_transaction);
	`uvm_component_utils(my_driver)
	virtual intf vif;
	
	function new(string name="my_driver",uvm_component parent);
		super.new(name,parent);
	endfunction
	
	function void build_phase(uvm_phase phase);
		super.build_phase(phase);
		if(!uvm_config_db#(virtual intf) :: get(this, "", "vif", vif))
			`uvm_fatal("DRIVER","no virtual interface");
		
	endfunction
	
	task run_phase(uvm_phase phase);
		my_transaction trans;
		forever begin
		seq_item_port.get_next_item(trans);
		@(posedge vif.clk);
			vif.wr_en =trans.wr_en;
			vif.rd_en =trans.rd_en;
			vif.wr_data=trans.wr_data;
		seq_item_port.item_done();
		end
	endtask
endclass

/////////// Monitor ////////////

class my_monitor extends uvm_monitor;
	`uvm_component_utils(my_monitor)
	virtual intf vif;
	uvm_analysis_port#(my_transaction) ap;

	function new(string name="my_monitor", uvm_component parent);
		super.new(name,parent);
		ap = new("ap",this);
	endfunction
	
	function void build_phase(uvm_phase phase);
		super.build_phase(phase);
		if(!uvm_config_db#(virtual intf) :: get(this, "", "vif", vif))
			`uvm_fatal("MONITOR","no virtual interface");
	endfunction

	task run_phase(uvm_phase phase);
		my_transaction trans;
		forever begin
			trans = my_transaction :: type_id :: create("trans",this);
			@(posedge vif.clk);
			if(vif.wr_en || vif.rd_en) begin
				trans.wr_en = vif.wr_en;
				trans.rd_en = vif.rd_en;
				trans.wr_data = vif.wr_data;
				trans.full = vif.full;
				trans.empty = vif.empty;
				trans.rd_data = vif.rd_data;
				trans.rd_valid = vif.rd_valid;
				ap.write(trans);
			end
		end
	endtask
endclass


//////////// Scoreboard ////////

class my_sb extends uvm_scoreboard;
	`uvm_component_utils(my_sb)
	uvm_analysis_imp#(my_transaction,my_sb) sb_imp;

	logic [7:0] trans_mem [0:511];
	bit [9:0] wr_addr;
	bit [9:0] rd_addr;
	function new(string name="my_sb", uvm_component parent);
		super.new(name,parent);
		sb_imp = new("sb_imp",this);
		wr_addr = 0;
		rd_addr = 0;
	endfunction

	function void build_phase(uvm_phase phase);
		super.build_phase(phase);
	endfunction

	function void write(my_transaction trans);
		bit [7:0] ex_data;
		if(trans.wr_en) begin
			if(wr_addr<512) begin
				trans_mem[wr_addr] = trans.wr_data;
				wr_addr = wr_addr+1;
			end else begin
				if(!trans.full) 
					`uvm_info("FIFO", "Was not full",UVM_LOW)
				else begin
					`uvm_info("FIFO", "Was full",UVM_LOW)
					wr_addr = 0;
				end
                	end
		end
		else if(trans.rd_en) begin
			if(trans.rd_valid) begin
				if(rd_addr <512) begin
					ex_data = trans_mem[rd_addr];
					rd_addr = rd_addr+1;
					if(ex_data == trans.rd_data)
						`uvm_info("rd_data", "data was matched", UVM_MEDIUM)
				        else 
						`uvm_info("rd_data", "data was not matched", UVM_MEDIUM)

				end else begin
				if(!trans.empty) 
					`uvm_info("FIFO", "Was not empty",UVM_LOW)
				else 
					`uvm_info("FIFO", "Was empty",UVM_LOW)
				end
                	end else
				`uvm_info("FIFO", "rd_en asserted but rd_valid false", UVM_LOW)

		end
		else begin
		end
	endfunction 
endclass


//////////// Agent //////

class my_agent extends uvm_agent;
	`uvm_component_utils(my_agent)
	virtual intf vif;
	my_driver dri;
	my_monitor mon;
	my_sequencer sequ;

	function new(string name="my_agent", uvm_component parent);
		super.new(name,parent);
	endfunction

	function void build_phase(uvm_phase phase);
		super.build_phase(phase);
		if(!uvm_config_db#(virtual intf) :: get(this,"","vif",vif))
			`uvm_fatal("AGENT","no virtual for agent");
		mon = my_monitor :: type_id :: create("mon",this);
		mon.vif = vif;
		if(get_is_active() == UVM_ACTIVE) begin
			sequ = my_sequencer :: type_id :: create ("sequ",this);
			dri = my_driver :: type_id :: create ("dri",this);
			dri.vif = vif;
		end
	endfunction 
	
	function void connect_phase(uvm_phase phase);
		super.connect_phase(phase);
		if(get_is_active() == UVM_ACTIVE)
			dri.seq_item_port.connect(sequ.seq_item_export);
	endfunction
endclass


/////////// Environment ////////////

class my_env extends uvm_env;
	`uvm_component_utils(my_env)
	my_agent age;
	my_sb sb;
	
	function new(string name="my_env", uvm_component parent);
		super.new(name,parent);
	endfunction

	function void build_phase(uvm_phase phase);
		super.build_phase(phase);
		age = my_agent :: type_id :: create ("age",this);
		sb = my_sb :: type_id :: create("sb",this);
	endfunction 

	function void connect_phase(uvm_phase phase);
		super.connect_phase(phase);
		age.mon.ap.connect(sb.sb_imp);
	endfunction 
endclass

//////////// Test ///////

class my_test extends uvm_test;
	`uvm_component_utils(my_test)
	
	my_env e0;
	my_sequence seq;

	function new(string name="my_test", uvm_component parent);
		super.new(name,parent);
	endfunction
	
	function void build_phase(uvm_phase phase);
		super.build_phase(phase);
		e0 = my_env :: type_id :: create("e0",this);
		uvm_config_db#(uvm_active_passive_enum) :: set(null,"e0.age","is_active",UVM_ACTIVE);
		seq = my_sequence :: type_id :: create("seq",this);
	endfunction

	task run_phase(uvm_phase phase);
		phase.raise_objection(this);
		seq.start(e0.age.sequ);
		phase.drop_objection(this);
	endtask
endclass


////////// Top //////////

module verification_of_syn_fifo ();
	reg clk;
	reg rst;
	intf vif (clk,rst);
	syn_FIFO dut( 
		.clk(clk),
		.rst(rst),
		.wr_en(vif.wr_en),
		.rd_en(vif.rd_en),
		.wr_data(vif.wr_data),
		.full(vif.full),
		.empty(vif.empty),
		.rd_data(vif.rd_data),
		.rd_valid(vif.rd_valid));

	always #5 clk =~clk;
	initial begin
		clk =0; rst = 1;
		#10 rst = 0;
		uvm_config_db#(virtual intf)::set(null, "*", "vif", vif);
		run_test("my_test");
	end
endmodule
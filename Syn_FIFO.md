---
title: "Synchronous FIFO Verilog Implementation"
date: 2025-07-29
---
module syn_FIFO(
    input clk,
    input rst,        
    input [7:0] wr_data, ///// Varible to write data into memory
    input wr_en,         ///// Tell to write 
    input rd_en,         ///// Tell to read 
    output reg full,     //// when no read was happen then full was active
    output reg empty,    //// when no write was happen then empty was active
    output reg[7:0] rd_data,  //// Tell us the data that has been read from memory
    output reg rd_valid      //// Tell us read data was valid or not
    );
reg [7:0] FIFO_data[0:511];     /// Memory
reg [8:0] wr_ptr,rd_ptr;        //// Pointers form both write and read
integer i;

///// Write Operation code //////

always @(posedge clk ) begin
    if (rst) begin
        wr_ptr<=9'b0;
        full<=1'b0;
        for (i=0; i<512; i=i+1) begin
            FIFO_data[i]=8'd0;
        end
    end else if (wr_ptr-rd_ptr==511 && wr_en==1'b1) begin
        FIFO_data[wr_ptr]<=wr_data;
        wr_ptr<=wr_ptr+1;
        full<=1;
    end else if (rd_ptr-wr_ptr==1 && wr_en==1) begin
        FIFO_data[wr_ptr]<=wr_data;
        wr_ptr<=wr_ptr+1;
        full<=1;
    end else if (wr_en==1 && !full) begin
        FIFO_data[wr_ptr]=wr_data;
        wr_ptr<=wr_ptr+1;
        full<=0;
    end else if (rd_ptr==wr_ptr ||rd_en==1) 
        full<=0;
   
end

////// Read Operation /////////////

always @(posedge clk) begin
    if (rst) begin 
        rd_ptr<=9'd0;
        rd_valid<=1'b0;
        rd_data<=8'd0;
        empty<=1;
    end else if (rd_en==1 && rd_ptr-wr_ptr==511) begin 
        rd_data<= FIFO_data[rd_ptr];
        rd_ptr<=rd_ptr+1; 
        rd_valid<=1;
        empty<=1;
    end else if (rd_en==1 && wr_ptr-rd_ptr==1 ) begin 
        rd_data<= FIFO_data[rd_ptr];
        rd_ptr<=rd_ptr+1; 
        rd_valid<=1;
        empty<=1;
    end else if (rd_en==1 && !empty) begin
        rd_data<= FIFO_data[rd_ptr];
        rd_ptr<=rd_ptr+1; 
        rd_valid<=1;
        empty<=0;
    end else if ((wr_ptr == rd_ptr) || wr_en==1) 
        empty<=0;
    
        
end
endmodule

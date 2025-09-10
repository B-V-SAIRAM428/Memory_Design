
module syn_FIFO#(parameter data_width = 8, parameter depth=512)(
    input clk,
    input rst,
    input [data_width-1:0] wr_data,
    input wr_en,
    input rd_en,
    output  full,
    output  empty,
    output reg[data_width-1:0] rd_data,
    output reg rd_valid
    );
localparam addr_width = $clog2(depth);
reg [data_width-1:0] FIFO_data[0:depth-1];
reg [addr_width:0] wr_ptr,rd_ptr;
integer i;
always @(posedge clk ) begin
    if (rst) begin
        wr_ptr<= 0; 
    end else if (wr_en && !full) begin
        FIFO_data[wr_ptr[addr_width-1:0]]<=wr_data;
        wr_ptr<=wr_ptr+1;
    end
end
always @(posedge clk) begin
    if (rst) begin 
        rd_ptr<=9'd0;
        rd_valid<=1'b0;
        rd_data<=8'd0;
    end else if (rd_en && !empty) begin 
        rd_data<= FIFO_data[rd_ptr[addr_width-1:0]];
        rd_ptr<=rd_ptr+1; 
        rd_valid<=1;
   end
end

assign full = (~wr_ptr[addr_width] == rd_ptr[addr_width]) && (wr_ptr[addr_width-1:0] == rd_ptr[addr_width-1:0]);
assign empty = (wr_ptr == rd_ptr);
endmodule

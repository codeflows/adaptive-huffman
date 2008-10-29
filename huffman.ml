(**
  Adaptive Huffman Coding in O'Caml
  Jari Aarniala (jari.aarniala@cs.helsinki.fi)
  $Id: huffman.ml,v 1.3 2003/02/20 18:55:06 foo Exp $
 *)

(* node in the tree *)
type node = {
  value : char;
  mutable order : int;
  mutable weight : int;
  mutable left : tree;
  mutable right : tree;
  mutable parent : tree;
}
and
  (* tree is either empty or a node *)
  tree = Empty | Node of node
and
  (* a huffman tree with it's contents and properties *)
  huffman_tree = {
    mutable root : tree;
    mutable nyt : tree;
    mutable next_order : int;
    mutable source : Buffer.t;
    lookup_by_value : tree array;
    lookup_by_order : tree array;
  }
;;

(* is this node empty? *)
let is_empty = function
  Empty -> true
| _ -> false

(* dumps the huffman tree in XML format *)
let to_xml huffman out =
  let print_val s n =
    Buffer.add_string s " value=\"";
    Buffer.add_char   s n.value;
    Buffer.add_char   s '\"'
  in
  let rec p s t =
  match t with
    Empty -> ()
  | Node( node ) ->
    Buffer.add_string s "<node";
    if is_empty node.left && is_empty node.right then print_val s node;
    Buffer.add_string s " order=\"";
    Buffer.add_string s (string_of_int node.order);
    Buffer.add_string s "\" weight=\"";
    Buffer.add_string s (string_of_int node.weight);
    Buffer.add_string s "\">\n";
    p s node.left;
    p s node.right;
    Buffer.add_string s "</node>\n"
  in
    let buff = Buffer.create 4096 in
      Buffer.add_string buff "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n";
      Buffer.add_string buff "<?xml-stylesheet type=\"text/xsl\" href=\"tree.xsl\"?>\n";
      Buffer.add_string buff "<huffman>\n";
      Buffer.add_string buff "<source><![CDATA[";
      Buffer.add_string buff (Buffer.contents huffman.source);
      Buffer.add_string buff "]]></source>\n";
      p buff huffman.root;
      Buffer.add_string buff "</huffman>\n";
      Buffer.output_buffer out buff ;;

(* dumps XML tree to stdout *)
let dump_xml tree =
  to_xml tree stdout ;;

(*
  Creates a new node to a Huffman tree
 *)
let create_node huffman value weight parent =
  let next_order = huffman.next_order in
    let created_node = Node ({
      value  = value;
      order  = next_order;
      weight = weight;
      left   = Empty;
      right  = Empty;
      parent = parent
   }) in
   (* update lookup array *)
   huffman.lookup_by_order.(next_order) <- created_node;
   (* update order and return new node*)
   huffman.next_order <- huffman.next_order - 1;
   created_node

(*
  Creates a new Huffman tree
 *)
let create_tree =
  let new_tree =
    { root=Empty;
      lookup_by_value=Array.make 256 Empty;
      lookup_by_order=Array.make 513 Empty;
      next_order=512;
      nyt=Empty;
      source=Buffer.create 2048 } in
    let new_root = create_node new_tree 'a' 0 Empty in
      new_tree.root <- new_root;
      new_tree.nyt <- new_root;
      new_tree
      
(*
  Swaps two nodes in a Huffman tree.
 *)
let swap huffman x y =
  let xt = huffman.lookup_by_order.(x.order) and
      yt = huffman.lookup_by_order.(y.order) in
  let swap_parents a b =
    match a,b with
      Node(j),Node(k) when j = k ->
        (* nodes have the same parent *)
        if j.left = xt then begin j.left <- yt; j.right <- xt; end
        else                begin j.left <- xt; j.right <- yt; end
    | Node(j),Node(k) ->
        (* different parents *)
        if j.left = xt then j.left <- yt else j.right <- yt;
        if k.left = yt then k.left <- xt else k.right <- xt
    | _ ->
        failwith "Assertion error: swap(x,y) : parent was not a node!"
  in
    (* we must never swap parent and child *)
    if x.parent <> yt && y.parent <> xt then
    begin
      (* first swap the references the parent nodes have to these two *)
      swap_parents x.parent y.parent;
      let x_old_parent = x.parent and
          x_old_order = x.order in
        (* then swap the references these nodes have to
           their parents and also their orders *)
        x.parent <- y.parent;
        y.parent <- x_old_parent;
        x.order <- y.order;
        y.order <- x_old_order;
        (* update the lookup array *)
        huffman.lookup_by_order.(x.order) <- xt;
        huffman.lookup_by_order.(y.order) <- yt
    end

(*
  Called when a character that has been seen before is encountered.
  If the node is not of maximum order in its weight class it is
  swapped with the highest ordered node.
 *)
let handle huffman node =
  (* finds the node with the highest order in the same weight class *)
  let rec max_order n =
    match huffman.lookup_by_order.(n.order + 1) with
      Node( other ) when other.weight = node.weight && other.order < 512  ->
        (* same weight, not root, keep searching if some node has even
           higher order in the same weight class *)
        max_order other
    | _ ->
        (* something else, return the previous result *)
        n
  in
    (* if this is the root node, it must not be swapped, just return it *)
    if node.order = 512 then
      node
    else
      let highest = max_order node in
        if highest.order <> node.order then
          (* found other node in the same weight class
             that had the highest order, swap these *)
          swap huffman node highest;
      node (* return the node we handled *)

(*
  Adds a value to the tree by spawning new nodes from the NYT node.
 *)
let add huffman value =
  match huffman.nyt with
    Node( n ) as t ->
      (* new character node *)
      n.right <- create_node huffman value 1 t;
      (* new NYT node *)
      n.left <- create_node huffman 'a' 0 t;
      (* update reference to the new NYT *)
      huffman.nyt <- n.left;
      (* update lookup table *)
      huffman.lookup_by_value.(int_of_char value) <- n.right;
      (* return old NYT *)
      n
  | Empty ->
    (* NYT is empty?!! *)
    failwith "Assertion failure: NYT was empty in function add!"

(*
  Updates a Huffman tree with the given character
 *)
let update huffman value =
  let get_node = function
    Empty ->
      (* add the value *)
      add huffman value
  | Node(n) as t -> 
      (* existing value, handle it *)
      handle huffman n
  in
    (* update_tree traverses the tree upwards as necessary *)
    let rec update_tree tree =
      let node = get_node tree in
        (* increment value *)
        node.weight <- node.weight + 1;
        if node.order <> 512 then
          (* this is not root, handle parent node *)
          update_tree node.parent
    in
      Buffer.add_char huffman.source value ;
      update_tree huffman.lookup_by_value.(int_of_char value)
    
(***********************
  Encoding functionality
 ***********************)
 
open Int32

(* the encoder type *)
type encoder =
  { input : in_channel;
    output : out_channel;
    mutable buffer : int32;
    mutable buffer_pos : int;
    bit_stack : int Stack.t;
    huffman : huffman_tree; }
    
(* creates a new encoder that reads from the input and writes to the output *)
let create_encoder input output =
  let new_encoder =
  { input=input;
    output=output;
    buffer=of_int 0;
    buffer_pos=0;
    bit_stack=Stack.create();
    huffman=create_tree; }
    in
      set_binary_mode_in input true;
      set_binary_mode_out output true;
      new_encoder

(* for debugging, a binary dump of a Int32 *)
let to_bin x y =
  let one = of_int 1 in
    for i=y downto 0 do
      if i = 23 || i = 15 || i = 7 then print_string " " else () ;
      print_int (to_int (logand (shift_right x i) one));
    done;
    print_string "\n" ;;

(* writes a byte from buffer to disk *)
let write_byte enc =
  let x = (logand (shift_right enc.buffer 16) (of_int 0xff)) in
    output_byte enc.output (to_int x)

(* if necessary, flushes the buffer *)
let flush enc =
  while enc.buffer_pos > 7 do
    begin
      write_byte enc;
      enc.buffer_pos <- enc.buffer_pos - 8;
      enc.buffer <- shift_left (logand enc.buffer (of_int 0xffff)) 8
    end;
  done

(* puts a bit to the buffer *)
let put_bit enc bit =
  enc.buffer <- logor enc.buffer (shift_left (of_int bit) (23 - enc.buffer_pos));
  enc.buffer_pos <- enc.buffer_pos + 1;
  flush enc

(* puts a char to the buffer. if is_last = true, EOF bit is set *)
let put_char enc ch is_last =
  enc.buffer <- logor enc.buffer (shift_left (of_int (int_of_char ch)) (16 - enc.buffer_pos));
  enc.buffer_pos <- enc.buffer_pos + 8;
  if is_last then put_bit enc 1 else put_bit enc 0

(*
  Writes the code for the specified character by climbing up the tree.
 *)
let write_code encoder tree =
  let rec go_up child parent = match parent with
    Node(k) ->
      begin
        (* if child is left child, write 0, otherwise 1 *)
        if k.left = child then Stack.push 0 encoder.bit_stack
        else Stack.push 1 encoder.bit_stack
      end;
      (* if not at root level, go up *)
      if k.order <> 512 then go_up parent k.parent
      (* else spit out the bits, in LIFO order *)
      else
        while not( Stack.is_empty encoder.bit_stack ) do
          put_bit encoder (Stack.pop encoder.bit_stack)
        done
  | _ ->
      failwith "Assertion failure: node empty while going up the tree?"
  in
    match tree with
      Node(k) ->
        (* if this is not the root node, go up the tree *)
        if k.order <> 512 then go_up tree k.parent
    | Empty ->
        failwith "Assertion failure: node empty in write_code?!?"
  
(* ends the encoding process by flushing the buffer to disk *)
let write_eof enc =
  write_code enc enc.huffman.nyt;
  put_char enc 'x' true;
  (* if the buffer has something left, flush the last byte *)
  if enc.buffer_pos <> 0 then enc.buffer_pos <- 8;
  flush enc

(*
  Encodes the given value
 *)
let encode encoder value =
  let t = encoder.huffman.lookup_by_value.(int_of_char value) in
    begin
    match t with
      Empty ->
        (* new character, write out the code for the NYT node and
           the character value itself *)
        write_code encoder encoder.huffman.nyt;
        (* print char to output *)
        put_char encoder value false;
    | Node(k) ->
        (* old character, write out the code for its node *)
        write_code encoder t
    end;
    (* in any case update the tree *)
    update encoder.huffman value

(*
  Runs the encoder
 *)
let run_encoder enc =
  let at_eof = ref false in
    while not(!at_eof) do
      try
        encode enc (input_char enc.input)
      with End_of_file -> at_eof := true
    done;
    (* write EOF *)
    write_eof enc;
    (* close the output / input channel *)
    close_out enc.output;
    close_in enc.input;
    print_string "Done.\n"

(***********************
  Decoding functionality
 ***********************)

(* the decoder type *)
type decoder =
  { dec_input : in_channel;
    dec_output : out_channel;
    mutable dec_buffer : int32;
    mutable dec_buffer_pos : int;
    mutable dec_buffer_size : int;
    dec_huffman : huffman_tree; }

(* input buffer size *)
let max_size = 24;;
    
(* creates a decoder *)
let create_decoder input output =
  let new_decoder =
  { dec_input=input;
    dec_output=output;
    dec_buffer=of_int 0;
    dec_buffer_pos=0;
    dec_buffer_size=0;
    dec_huffman=create_tree; }
    in
      set_binary_mode_in input true;
      set_binary_mode_out output true;
      new_decoder

(* fills buffer up as much as possible *)
let fill_buffer dec =
  let shift dec = 
    dec.dec_buffer <- shift_left dec.dec_buffer dec.dec_buffer_pos;
    dec.dec_buffer_size <- dec.dec_buffer_size - dec.dec_buffer_pos;
    dec.dec_buffer_pos <- 0;
  in
  if dec.dec_buffer_pos <> 0 then shift dec;
  let avail = max_size - dec.dec_buffer_size in
    let read_blocks = avail / 8 in
      for i=1 to read_blocks do
        try
          let c = input_byte dec.dec_input in
            dec.dec_buffer <- logor
              dec.dec_buffer
              (shift_left (of_int c) (max_size - dec.dec_buffer_size - 8));
            dec.dec_buffer_size <- dec.dec_buffer_size + 8
        with End_of_file -> ()
      done

(* ensures buffer has at least 'needed' bits *)
let ensure_buffer dec needed =
  if dec.dec_buffer_pos + needed > dec.dec_buffer_size then
    (* not enough stuff in buffer, read more *)
    fill_buffer dec;
    if dec.dec_buffer_size - dec.dec_buffer_pos < needed then
      failwith "Not enough bits available!\n"

(* gets a char from buffer *)
let get_char dec =
  ensure_buffer dec 8; (* make sure there are enough bits available *)
  let s = max_size - dec.dec_buffer_pos - 8 in
    let a = of_int (0xff lsl (s)) in
      let c = shift_right (logand dec.dec_buffer a) s in
        dec.dec_buffer_pos <- dec.dec_buffer_pos + 8;
        char_of_int (to_int c)

(* gets a bit from buffer *)
let get_bit dec =
  ensure_buffer dec 1; (* make sure there is a bit available *)
  let s = max_size - dec.dec_buffer_pos - 1 in
    let a = of_int (0x1 lsl (s)) in
      let c = shift_right (logand dec.dec_buffer a) s in
        dec.dec_buffer_pos <- dec.dec_buffer_pos + 1;
        to_int c

(* check whether given node is a leaf *)
let is_leaf node = match (node.left,node.right) with
  (Empty,Empty) -> true
| _ -> false

(* goes down the tree in search of leaf nodes *)
let rec go_down dec tree =
  let handle_update c =
    output_char dec.dec_output c;
    update dec.dec_huffman c;
    go_down dec dec.dec_huffman.root
  in
  match tree with
    Node(n) ->
      if is_leaf n then
        (* in a leaf node *)
        begin
          if dec.dec_huffman.nyt = tree then
            (* NYT node, read a new character in *)
            let next_char = get_char dec in
              let bit = get_bit dec in
                if bit = 1 then
                  (* this bit marks the end, just leave the function *)
                  ()
                else handle_update next_char
          else
            (* character leaf node *)
            handle_update n.value
        end
      else
       (* internal node, go deeper *)
        begin
          if get_bit dec = 0 then go_down dec n.left
          else go_down dec n.right
        end;
   | _ ->
        failwith "Assertion failure: in go_down, tree was empty"

(* runs the decoder *)
let run_decoder dec =
  (* read the first char *)
  let first_char = get_char dec in
    let bit = get_bit dec in
      if bit = 1 then failwith "Nothing to do : EOF!";
      (* output char *)
      output_char dec.dec_output first_char;
      (* update tree *)
      update dec.dec_huffman first_char;
      (* go to recursive mode *)
      go_down dec dec.dec_huffman.root;
      (* when it's done, we're done *)
      close_in dec.dec_input;
      close_out dec.dec_output;
      print_string "Done.\n"

(********************
  Program entry point
 ********************)

let main() =
  let usage() =
    print_string "\nAdaptive Huffman encoder, (C) Jari Aarniala 2003\n\n";
    print_string "Usage: ";
    print_string Sys.argv.(0);
    print_string " cmd infile outfile [dump_file.xml]\n";
    print_string "- cmd is either enc or dec (for encoding/decoding)\n";
    print_string "- infile is the source file, outfile is the target file\n";
    print_string "- if an xml file is specified at the end of the command,\n";
    print_string "  the created huffman tree will be dumped to it\n";
    exit 0
  in
  if Array.length Sys.argv < 4 then usage();
  if Sys.argv.(1) = "enc" then
    begin
      let inp = open_in Sys.argv.(2) in
      let outp = open_out Sys.argv.(3) in
      let my_encoder = create_encoder inp outp in
      run_encoder my_encoder;
      if Array.length Sys.argv > 4 then
        begin
          let file = open_out Sys.argv.(4) in
            to_xml my_encoder.huffman file;
            close_out file
        end;
    end
  else if Sys.argv.(1) = "dec" then
    begin
      let inp = open_in Sys.argv.(2) in
      let outp = open_out Sys.argv.(3) in
      let my_decoder = create_decoder inp outp in
      run_decoder my_decoder;
      if Array.length Sys.argv > 4 then
        begin
          let file = open_out Sys.argv.(4) in
            to_xml my_decoder.dec_huffman file;
            close_out file
        end;
    end
    else
      begin
        print_string "Invalid command!\n";
        usage()
      end
  
;;

main () ;;


(*STACK-ADJUSTING PART *)
(* deal with exact params
^ (Printf.sprintf "%s: \n" label_arity_exact)
^ "\tsub rsp, 8*1\n" (*one extra space on stack*)
^ "\tmov rdi, rsp\n"

^ "\tmov rax, qword[rdi + 8 * 1] ;move ret\n"
^ "\tmov qword[rdi], rax\n"
^ "\tadd rdi, 8*1\n"
^ "\tmov rax, qword[rdi + 8 * 1] ;move env\n"
^ "\tmov qword[rdi], rax\n"
^ "\tadd rdi, 8*1\n"
^ (Printf.sprintf "\tmov qword[rdi], %d\n" count')
^ (Printf.sprintf "\tlea r9, [rsp + 8 * (2 + %d)]\n" count')
^ (Printf.sprintf "%s : \n" label_stack_ expand_loop)
^ "\tcmp rdi, r9\n"
^ (Printf.sprintf "\tje %s\n" label_stack_expand_loop_end)
^ "\tmov rax, qword [rdi + 8 * 1]\n"
^ "\tmov qword[rdi], rax\n"
^ "\tadd rdi, 8\n"
^ (printf.sprintf "\tjmp %s \n" label_stack_expand_loop)
^ (Printf.sprintf "%s: \n" label_stack_expand_loop_end)
^ "\tmov qword[rdi], sob_nil\n"
(* end deal with exact params *)
^ (Printf.sprintf "\tjmp %s \n" label_stack_ok) *)


(* ^ (Printf.sprintf "%s: \n" label_arity_ more)
(* deal with more params *)
^ "\tmov rcx, qword[rsp + 8 * 2]\n" (*rcx holds n*)
^ "\tlea rsi, [rsp + 8 * (rcx + 2)]\n" (*rsi starts at the address of last parameter*)
^ "\tmov r8, rsi\n"
^ (Printf.sprintf "\tsub rcx, %d\n" count) (*amount of optional params*)
^ "\tmov r9, sob nil\n"
^ (Printf.sprintf "%s: \t; params into heap loop\n" label_params_loop)
^ "\tcmp rcx, 0\n"
^ (Printf.sprintf "\tje %s\n" label_params_loop_exit)
^ "\tmov rdi, (1 + 8 +8)\n"
^ "\tcall malloc\n"
^ "\tmov byte[rax], T_pair\n"
^ "\tmov rbx, qword[rsi]\n"
^ "\tmov SOB_PAIR_CAR(rax), rbx\n"
^ "\tmov SOB_PAIR_CDR(rax), r9\n" (*link to next pair*)
^ "\tmov r9, rax\n"
^ "\tsub rsi, 8*1\n"
^ "\tdec rcx\n"
^ (Printf.sprintf "\tjmp %s\n" label_params_loop)
^ (Printf.sprintf "%s:\t; params into heap loop exit\n" label_params_loop_exit)
(* r9 holds optional params list *)
^ "\tmov qword[r8], r9\n"
^ "\tadd rsi, 8*1\n"
^ (Printf.sprintf "\tmov rcx, %d\n" count)
^ "\tlea rsi, [rsp + 8 * (2 + rcx)]\n" (* pos of param at (count)*)
^ "\tsub r8, 8*1\n" (* pos of param n-2 *)
^ (Printf.sprintf "%s:\t; stack shrink\n" label_stack_shrink_loop)
^ "\tcmp rcx, 0\n"
^ (Printf.sprintf "\tje %s\n" label_stack_shrink_loop_exit)
^ "\tmov rax, qword[rsi]\n"
^ "\tmov qword[r8], rax\n"
^ "\tsub rsi, 8*1\n"
^ "\tsub r8, 8*1\n"
^ "\tdec rcx\n"
^ (Printf.sprintf "\tjmp %s\n" label_stack_shrink_loop)
^ (Printf.sprintf "%s:\t; stack shrink exit\n" label_stack_shrink_loop_exit)
^ (Printf.sprintf "\tmov qword[r8], %d\n" count')
^ "\tsub rsi, 8*1\n"
^ "\tsub r8, 8*1\n"
^ "\tmov rax, qword[rsi]\n"
^ "\tmov qword[r8], rax\n"
^ "\tsub rsi, 8*1\n"
^ "\tsub r8, 8*1\n"
^ "\tmov rax, qword[rsi]\n"
^ "\tmov qword[r8], rax\n"
^ "\tmov rsp, r8\n" (*fix rsp pos*)
^ (Printf.sprintf "\tjmp %s\n" label_stack_ok) *)

(* ^ (Printf.sprintf "%s:\n" label_stack_ok)
(* code section*)
^ "\tenter 0, 0\n"
^ (run count' (env + 1) body)
^ "\tleave\n"
(**)
^ (Printf.sprintf "\tret AND_KILL_FRAME(%d )\n" count') *)







let label_loop_env = make_lambda_opt_loop_env ()
and label_loop_env_end = make_lambda_opt_loop_env_end ()
and label_params_loop = make_lambda_opt_loop_params ()
and label_params_loop_exit = make_lambda_opt_loop_params_end ()
and label_code = make_lambda_opt_code ()
and label_arity_exact = make_lambda_opt_arity_exact ()
and label_arity_more = make_lambda_opt_arity_more ()
and label_stack_ok = make_lambda_opt_stack_ok ()
and label_stack_expand_loop = make_lambda_opt_loop ()
and label_stack_expand_loop_end = make_lambda_opt_loop_exit ()
and label_stack_shrink_loop = make_lambda_opt_loop ()
and label_stack_shrink_loop_exit = make_lambda_opt_loop_exit ()
and label_end = make_lambda_opt_end ()
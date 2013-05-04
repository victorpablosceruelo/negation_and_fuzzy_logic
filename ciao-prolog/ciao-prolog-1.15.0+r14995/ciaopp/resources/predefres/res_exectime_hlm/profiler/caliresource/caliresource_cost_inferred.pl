%% Warning!!! This module was auto generated in caliresource_indep
%% Do not change it or the changes made by you will be lost
bench_model('caliresource_bench_auto:verify_list_type'(_),[$(0,1)+1,0,$(0,1)+1,$(0,1)+1,2* $(0,1),0]).
bench_model('caliresource_bench_auto:list_no_last_call_opt'(_),[$(0,1)+1,0,$(0,1)+1,2* $(0,1)+1,2* $(0,1),0]).
bench_model('caliresource_bench_auto:list_many_args'(_,_,_,_,_,_,_,_,_),[$(0,1)+1,8* $(0,1)+8,9* $(0,1)+9,$(0,1)+1,2* $(0,1),16* $(0,1)]).
bench_model('caliresource_bench_auto:unify_two_lists'(_,_),[$(0,1)+1,$(0,1)+1,2* $(0,1)+2,$(0,1)+1,2* $(0,1),2* $(0,1)]).
bench_model('caliresource_bench_auto:list_var_output'(_,_),[$(0,1)+1,2* $(0,1)+1,2* $(0,1)+2,$(0,1)+1,2* $(0,1),51* $(0,1)]).
bench_model('caliresource_bench_auto:list_var_input'(_,_),[2* $(0,1)+1,2* $(0,1)+1,2* $(0,1)+2,$(0,1)+1,101* $(0,1),101* $(0,1)]).
bench_model('caliresource_bench_auto:list_deep_ground_input'(_,_),[52* $(0,1)+1,$(0,1)+1,2* $(0,1)+2,$(0,1)+1,2* $(0,1),2* $(0,1)]).
bench_model('caliresource_bench_auto:list_flat_ground_input'(_,_),[52* $(0,1)+1,$(0,1)+1,2* $(0,1)+2,$(0,1)+1,2* $(0,1),2* $(0,1)]).
bench_model('caliresource_bench_auto:list_flat_ground_output'(_,_),[$(0,1)+1,52* $(0,1)+1,2* $(0,1)+2,$(0,1)+1,2* $(0,1),$(0,1)]).
bench_model('caliresource_bench_auto:list_deep_ground_output'(_,_),[$(0,1)+1,52* $(0,1)+1,2* $(0,1)+2,$(0,1)+1,2* $(0,1),$(0,1)]).
bench_model('caliresource_bench_auto:environment',[0,0,0,986,0,0]).
bench_model('caliresource_bench_auto:lcall',[0,0,0,201,0,0]).
bench_metric('caliresource_bench_auto:verify_list_type'(A),['caliresource_bench_auto:int_list'(A)],[length],[1]).
bench_metric('caliresource_bench_auto:list_no_last_call_opt'(A),['caliresource_bench_auto:int_list'(A)],[length],[1]).
bench_metric('caliresource_bench_auto:list_many_args'(A,B,C,D,E,F,G,H,I),['caliresource_bench_auto:int_list'(A),'term_typing:var'(B),'term_typing:var'(C),'term_typing:var'(D),'term_typing:var'(E),'term_typing:var'(F),'term_typing:var'(G),'term_typing:var'(H),'term_typing:var'(I)],[length,length,length,length,length,length,length,length,length],[1,0,0,0,0,0,0,0,0]).
bench_metric('caliresource_bench_auto:unify_two_lists'(A,B),['caliresource_bench_auto:int_list'(A),'term_typing:var'(B)],[length,length],[1,0]).
bench_metric('caliresource_bench_auto:list_var_output'(A,B),['caliresource_bench_auto:int_list'(A),'term_typing:var'(B)],[length,length],[1,0]).
bench_metric('caliresource_bench_auto:list_var_input'(A,B),['caliresource_bench_auto:input_list'(A),'term_typing:var'(B)],[length,length],[1,0]).
bench_metric('caliresource_bench_auto:list_deep_ground_input'(A,B),['caliresource_bench_auto:deep_list'(A),'term_typing:var'(B)],[length,length],[1,0]).
bench_metric('caliresource_bench_auto:list_flat_ground_input'(A,B),['caliresource_bench_auto:flat_list'(A),'term_typing:var'(B)],[length,length],[1,0]).
bench_metric('caliresource_bench_auto:list_flat_ground_output'(A,B),['caliresource_bench_auto:flat_list'(A),'term_typing:var'(B)],[length,length],[1,0]).
bench_metric('caliresource_bench_auto:list_deep_ground_output'(A,B),['caliresource_bench_auto:deep_list'(A),'term_typing:var'(B)],[length,length],[1,0]).
bench_metric('caliresource_bench_auto:environment',[],[],[]).
bench_metric('caliresource_bench_auto:lcall',[],[],[]).
valid_resources([giunif,gounif,nargs,steps,viunif,vounif]).

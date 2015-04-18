#include "ast.h"
#include "tables.h"
#include "eval.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

int eval_debug = 1;
varctx_t* tainttree;

void debug_eval(int val)
{
    eval_debug = val;
}

value_t eval_exp(ast_t *e, varctx_t *tbl, memctx_t *mem)
{
  char tmp[10];
  char* tmp2;
  value_t* ret;
  value_t one,two,three,valt,index;
    switch(e->tag){
    case int_ast: return ((value_t){.value = e->info.integer, .taint = 0}); break;
    case var_ast: 
    	valt = lookup_var(e->info.varname, tbl);
    	if(tainttree != NULL && valt.taint == 1){
    		tainttree->next = newvar(e->info.varname, NULL);
    		tainttree = tainttree->next;
    	}
    	return valt;
    	break;
    case node_ast: {
	switch(e->info.node.tag){
	case MEM:
		index = eval_exp(e->info.node.arguments->elem, tbl,mem);
		valt = load(index.value, mem);
    	if(tainttree != NULL && (valt.taint == 1)){
    		sprintf(tmp, "mem[%d]", index.value);
    		tmp2 = (char*)malloc(strlen(tmp)+1);
    		strcpy(tmp2,tmp);
    		tainttree->next = newvar(tmp2, NULL);
    		tainttree = tainttree->next;
    	}
    	value_t* newvaltp = (value_t*)malloc(sizeof(value_t));
    	newvaltp->value = valt.value;
    	newvaltp->taint = valt.taint || index.taint;
    	return *newvaltp;
	  break;
	case PLUS:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return 
	  	((value_t){.value=one.value+two.value, .taint = (one.taint || two.taint) && !(e->info.node.arguments->elem->tag == var_ast && e->info.node.arguments->next->elem->info.node.tag == NEGATIVE && strcmp(e->info.node.arguments->elem->info.varname, e->info.node.arguments->next->elem->info.node.arguments->elem->info.varname)==0)});
	  break;
	case MINUS:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return 
	    ((value_t){.value=one.value-two.value, .taint = (one.taint || two.taint) && !(e->info.node.arguments->elem->tag == var_ast && e->info.node.arguments->next->elem->tag == var_ast && strcmp(e->info.node.arguments->elem->info.varname, e->info.node.arguments->next->elem->info.varname)==0)});
	  break;
	case DIVIDE:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return 
	    ((value_t){.value=one.value/two.value, .taint = (one.taint || two.taint) && !(e->info.node.arguments->elem->tag == var_ast && e->info.node.arguments->next->elem->tag == var_ast && strcmp(e->info.node.arguments->elem->info.varname, e->info.node.arguments->next->elem->info.varname)==0)});
	  break;
	case TIMES:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return 
	    ((value_t){.value=one.value*two.value, .taint = one.taint || two.taint});
	  break;
	case EQ:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return 
	    ((value_t){.value=one.value==two.value, .taint = (one.taint || two.taint) && !(e->info.node.arguments->elem->tag == var_ast && e->info.node.arguments->next->elem->tag == var_ast && strcmp(e->info.node.arguments->elem->info.varname, e->info.node.arguments->next->elem->info.varname)==0)});
	  break;
	case NEQ:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return 
	    ((value_t){.value=one.value!=two.value, .taint = (one.taint || two.taint) && !(e->info.node.arguments->elem->tag == var_ast && e->info.node.arguments->next->elem->tag == var_ast && strcmp(e->info.node.arguments->elem->info.varname, e->info.node.arguments->next->elem->info.varname)==0)});
	  break;
	case GT:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return ((value_t){.value=one.value>two.value, .taint = (one.taint || two.taint) && !(e->info.node.arguments->elem->tag == var_ast && e->info.node.arguments->next->elem->tag == var_ast && strcmp(e->info.node.arguments->elem->info.varname, e->info.node.arguments->next->elem->info.varname)==0)});
	    break;
	case LT:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return ((value_t){.value=one.value<two.value, .taint = (one.taint || two.taint) && !(e->info.node.arguments->elem->tag == var_ast && e->info.node.arguments->next->elem->tag == var_ast && strcmp(e->info.node.arguments->elem->info.varname, e->info.node.arguments->next->elem->info.varname)==0)});
	    break;
	case LEQ:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return ((value_t){.value=one.value<=two.value, .taint = (one.taint || two.taint) && !(e->info.node.arguments->elem->tag == var_ast && e->info.node.arguments->next->elem->tag == var_ast && strcmp(e->info.node.arguments->elem->info.varname, e->info.node.arguments->next->elem->info.varname)==0)});
	    break;
	case GEQ:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return ((value_t){.value=one.value>=two.value, .taint =(one.taint || two.taint) && !(e->info.node.arguments->elem->tag == var_ast && e->info.node.arguments->next->elem->tag == var_ast && strcmp(e->info.node.arguments->elem->info.varname, e->info.node.arguments->next->elem->info.varname)==0)});
	    break;
	case AND:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return ((value_t){.value=one.value&&two.value, .taint = one.taint || two.taint});
	    break;
	case OR:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return ((value_t){.value=one.value||two.value, .taint = one.taint || two.taint});
	  break;
	case NEGATIVE:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
	  return ((value_t){.value=-one.value, .taint = one.taint});
	case NOT:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
	  return ((value_t){.value=!one.value, .taint = one.taint});
    case IFE:
        one = eval_exp(e->info.node.arguments->elem,tbl,mem);
        two = one.value?eval_exp(e->info.node.arguments->next->elem,tbl,mem):eval_exp(e->info.node.arguments->next->next->elem,tbl,mem);
		int v = two.value;
		int t = one.taint || two.taint;
		//int t = one.taint || two.taint || three.taint;
        return  ((value_t){.value=v, .taint=t}); 
	case READINT:
	  ret = (value_t*)malloc(sizeof(value_t));
	  printf("> ");
	  scanf("%d", &(ret->value));
	  ret->taint = 0;
	  return *ret;
	  break;
	case READSECRETINT:
	  ret = (value_t*)malloc(sizeof(value_t));
	  printf("# ");
	  scanf("%d", &(ret->value));
	  ret->taint = 1;
	  if(tainttree != NULL){
	  	tmp2 = (char*)malloc(7);
	  	strcpy(tmp2, "Direct");
    	tainttree->next = newvar(tmp2, NULL);
    	tainttree = tainttree->next;
      }
	  return *ret;
	  break;
	default:
	  assert(0); // Unknown/unhandled op.
	}
    }
    }
}

state_t* eval_stmts(ast_t *p, state_t *state)
{
    ast_list_t *stmts;
    ast_list_t *ip;
    ast_t *t1, *t2;
    ast_t *s;
    value_t v;

    assert(p != NULL);
    assert(p->info.node.tag == SEQ);
    ip = p->info.node.arguments;
    while(ip != NULL)
    {
	s = ip->elem;

	switch(s->info.node.tag){
	case ASSIGN:
	    /* the lhs */
	    t1 = s->info.node.arguments->elem;
	    /* the rhs */
	    t2 = s->info.node.arguments->next->elem;
	    v = eval_exp(t2, state->tbl, state->mem);
	    switch(t1->tag){
	    case var_ast:
		    state->tbl = update_var(t1->info.string, v, state->tbl);
		    break;
	    case node_ast:
		    assert(t1->info.node.tag == MEM);
		    state->mem = store(eval_exp(t1->info.node.arguments->elem,
					  state->tbl, 
					  state->mem).value, v, state->mem);
		break;
	  default:
		assert(0);
	    }
	  break;
	case PRINT:
	    switch(s->info.node.arguments->elem->tag){
	      case str_ast:
		    printf("%s\n", s->info.node.arguments->elem->info.string);
		    fprintf(stderr, "Tainted variable: None\n");
		    break;
	      default:
	        tainttree = newvar("first", NULL);
	        varctx_t* headtree = tainttree;
	        if(eval_exp(s->info.node.arguments->elem, 
		        	    state->tbl,
		                state->mem).taint == 1){
	            printf("<secret>\n");
	            fprintf(stderr, "Tainted variable: ");
	            tainttree = headtree->next;
		        while(tainttree->next != NULL){
		        	fprintf(stderr, "%s, ",tainttree->name);
		        	tainttree = tainttree->next;
		        }
		        fprintf(stderr, "%s\n",tainttree->name);
	        }else{
		    	printf("%d\n", eval_exp(s->info.node.arguments->elem, 
		    				state->tbl,
		    				state->mem).value);
		    	fprintf(stderr, "Tainted variable: None\n");
		    }
		    tainttree = NULL;
		    break;
	    }

	  break;
	case IF:

	    if(eval_exp(s->info.node.arguments->elem, state->tbl, state->mem).value){
		state = eval_stmts(s->info.node.arguments->next->elem, state);
	    } else {
		state = eval_stmts(s->info.node.arguments->next->next->elem, state);
        } 
	  break;
	case SEQ:
	    state = eval_stmts(s->info.node.arguments->next->elem, state);
	  break;
	case ASSERT:
	    if(eval_exp(s->info.node.arguments->elem, state->tbl,state->mem).value ==0){
		    printf("Assert failed!\n");
	    }
	  break;
	default:
	  printf("Unknown statement type\n");
	  assert(0);
	  break;
	}
	ip = ip->next;
    }
    return state;
}

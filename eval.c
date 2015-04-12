#include "ast.h"
#include "tables.h"
#include "eval.h"
#include <assert.h>

int eval_debug = 0;

void debug_eval(int val)
{
    eval_debug = val;
}

value_t eval_exp(ast_t *e, varctx_t *tbl, memctx_t *mem)
{
  value_t ret,one,two,three;
    switch(e->tag){
    case int_ast: return ((value_t){.value = e->info.integer, .taint = 0}); break;
    case var_ast: return lookup_var(e->info.varname, tbl); break;
    case node_ast: {
	switch(e->info.node.tag){
	case MEM:
	  return load(eval_exp(e->info.node.arguments->elem, tbl,mem).value, mem);
	  break;
	case PLUS:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return 
	  	((value_t){.value=one.value+two.value, .taint = one.taint || two.taint});
	  break;
	case MINUS:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return 
	    ((value_t){.value=one.value-two.value, .taint = one.taint || two.taint});
	  break;
	case DIVIDE:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return 
	    ((value_t){.value=one.value/two.value, .taint = one.taint || two.taint});
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
	    ((value_t){.value=one.value==two.value, .taint = one.taint || two.taint});
	  break;
	case NEQ:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return 
	    ((value_t){.value=one.value!=two.value, .taint = one.taint || two.taint});
	  break;
	case GT:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return ((value_t){.value=one.value>two.value, .taint = one.taint || two.taint});
	    break;
	case LT:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return ((value_t){.value=one.value<two.value, .taint = one.taint || two.taint});
	    break;
	case LEQ:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return ((value_t){.value=one.value<=two.value, .taint = one.taint || two.taint});
	    break;
	case GEQ:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
	  return ((value_t){.value=one.value>=two.value, .taint = one.taint || two.taint});
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
	  return ((value_t){.value=-one.value, .taint = one.taint || two.taint});
	case NOT:
		one = eval_exp(e->info.node.arguments->elem,tbl,mem);
	  return ((value_t){.value=!one.value, .taint = one.taint || two.taint});;
        case IFE:
        one = eval_exp(e->info.node.arguments->elem,tbl,mem);
		two = eval_exp(e->info.node.arguments->next->elem,tbl,mem);
		three = eval_exp(e->info.node.arguments->next->next->elem,tbl,mem);
		int v = one.value?two.value:three.value;
		int t = one.taint || two.taint || three.taint;
          return  ((value_t){.value=v, .taint=t}); 
	case READINT:
	  printf("> ");
	  scanf("%d", &ret.value);
	  ret.taint = 0;
	  return ret;
	  break;
	case READSECRETINT:
	  printf("# ");
	  scanf("%d", &ret.value);
	  ret.taint = 1;
	  return ret;
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
		break;
	    default:
	    if(eval_exp(s->info.node.arguments->elem, 
					state->tbl,
					state->mem).taint == 1){
	    	printf("<secret>\n");
	    }else{
			printf("%u\n", eval_exp(s->info.node.arguments->elem, 
						state->tbl,
						state->mem).value);
		}
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

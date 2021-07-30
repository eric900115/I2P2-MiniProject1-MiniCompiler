#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/*
For the language grammar, please refer to Grammar section on the github page:
  https://github.com/lightbulb12294/CSI2P-II-Mini1#grammar
*/

#define MAX_LENGTH 200
typedef enum
{
    ASSIGN,
    ADD,
    SUB,
    MUL,
    DIV,
    REM,
    PREINC,
    PREDEC,
    POSTINC,
    POSTDEC,
    IDENTIFIER,
    CONSTANT,
    LPAR,
    RPAR,
    PLUS,
    MINUS
} Kind;
typedef enum
{
    STMT,
    EXPR,
    ASSIGN_EXPR,
    ADD_EXPR,
    MUL_EXPR,
    UNARY_EXPR,
    POSTFIX_EXPR,
    PRI_EXPR
} GrammarState;
typedef struct TokenUnit
{
    Kind kind;
    int val; // record the integer value or variable name
    struct TokenUnit *next;
} Token;
typedef struct ASTUnit
{
    Kind kind;
    int val; // record the integer value or variable name
    struct ASTUnit *lhs, *mid, *rhs;
} AST;

/// utility interfaces

// err marco should be used when a expression error occurs.
#define err(x)                                                \
    {                                                         \
        puts("Compile Error!");                               \
        if (DEBUG)                                            \
        {                                                     \
            fprintf(stderr, "Error at line: %d\n", __LINE__); \
            fprintf(stderr, "Error message: %s\n", x);        \
        }                                                     \
        exit(0);                                              \
    }
// You may set DEBUG=1 to debug. Remember setting back to 0 before submit.
#define DEBUG 0
// Split the input char array into token linked list.
Token *lexer(const char *in);
// Create a new token.
Token *new_token(Kind kind, int val);
// Translate a token linked list into array, return its length.
size_t token_list_to_arr(Token **head);
// Parse the token array. Return the constructed AST.
AST *parser(Token *arr, size_t len);
// Parse the token array. Return the constructed AST.
AST *parse(Token *arr, int l, int r, GrammarState S);
// Create a new AST node.
AST *new_AST(Kind kind, int val);
// Find the location of next token that fits the condition(cond). Return -1 if not found. Search direction from start to end.
int findNextSection(Token *arr, int start, int end, int (*cond)(Kind));
// Return 1 if kind is ASSIGN.
int condASSIGN(Kind kind);
// Return 1 if kind is ADD or SUB.
int condADD(Kind kind);
// Return 1 if kind is MUL, DIV, or REM.
int condMUL(Kind kind);
// Return 1 if kind is RPAR.
int condRPAR(Kind kind);
// Check if the AST is semantically right. This function will call err() automatically if check failed.
void semantic_check(AST *now);
// Generate ASM code.
int codegen(AST *root);
// Free the whole AST.
void freeAST(AST *now);
//void inc_dec(int inc,int dec);

/// debug interfaces

// Print token array.
void token_print(Token *in, size_t len);
// Print AST tree.
void AST_print(AST *head);

char input[MAX_LENGTH];

int flag = 1;
int main()
{
    while (fgets(input, MAX_LENGTH, stdin) != NULL)
    {
        Token *content = lexer(input);
        size_t len = token_list_to_arr(&content);
        AST *ast_root = parser(content, len);

        semantic_check(ast_root);
        flag = 1;
        //AST_print(ast_root);
        codegen(ast_root);
        free(content);
        freeAST(ast_root);
    }
    return 0;
}

Token *lexer(const char *in)
{
    Token *head = NULL;
    Token **now = &head;
    for (int i = 0; in[i]; i++)
    {
        if (in[i] == ' ' || in[i] == '\n') // ignore space and newline
            continue;
        else if (isdigit(in[i]))
        {
            (*now) = new_token(CONSTANT, atoi(in + i));
            while (in[i + 1] && isdigit(in[i + 1]))
                i++;
        }
        else if ('x' <= in[i] && in[i] <= 'z') // variable
            (*now) = new_token(IDENTIFIER, in[i]);
        else
            switch (in[i])
            {
            case '=':
                (*now) = new_token(ASSIGN, 0);
                break;
            case '+':
                if (in[i + 1] && in[i + 1] == '+')
                {
                    i++;
                    // In lexer scope, all "++" will be labeled as PREINC.
                    (*now) = new_token(PREINC, 0);
                }
                // In lexer scope, all single "+" will be labeled as PLUS.
                else
                    (*now) = new_token(PLUS, 0);
                break;
            case '-':
                if (in[i + 1] && in[i + 1] == '-')
                {
                    i++;
                    // In lexer scope, all "--" will be labeled as PREDEC.
                    (*now) = new_token(PREDEC, 0);
                }
                // In lexer scope, all single "-" will be labeled as MINUS.
                else
                    (*now) = new_token(MINUS, 0);
                break;
            case '*':
                (*now) = new_token(MUL, 0);
                break;
            case '/':
                (*now) = new_token(DIV, 0);
                break;
            case '%':
                (*now) = new_token(REM, 0);
                break;
            case '(':
                (*now) = new_token(LPAR, 0);
                break;
            case ')':
                (*now) = new_token(RPAR, 0);
                break;
            default:
                err("Unexpected character.");
            }
        now = &((*now)->next);
    }
    return head;
}

Token *new_token(Kind kind, int val)
{
    Token *res = (Token *)malloc(sizeof(Token));
    res->kind = kind;
    res->val = val;
    res->next = NULL;
    return res;
}

size_t token_list_to_arr(Token **head)
{
    size_t res;
    Token *now = (*head), *del;
    for (res = 0; now != NULL; res++)
        now = now->next;
    now = (*head);
    if (res != 0)
        (*head) = (Token *)malloc(sizeof(Token) * res);
    for (int i = 0; i < res; i++)
    {
        (*head)[i] = (*now);
        del = now;
        now = now->next;
        free(del);
    }
    return res;
}

AST *parser(Token *arr, size_t len)
{
    for (int i = 1; i < len; i++)
    {
        // correctly identify "ADD" and "SUB"
        if (arr[i].kind == PLUS || arr[i].kind == MINUS)
        {
            switch (arr[i - 1].kind)
            {
            case PREINC:
            case PREDEC:
            case IDENTIFIER:
            case CONSTANT:
            case RPAR:
                arr[i].kind = arr[i].kind - PLUS + ADD;
            default:
                break;
            }
        }
    }
    return parse(arr, 0, len - 1, STMT);
}

AST *parse(Token *arr, int l, int r, GrammarState S)
{
    AST *now = NULL;
    if (l > r)
    {
        if (S == STMT)
            return now;
        else
            err("Unexpected parsing range.");
    }
    int nxt;
    switch (S)
    {
    case STMT:
        if (l > r)
            return now;
        else
            return parse(arr, l, r, EXPR);
    case EXPR:
        return parse(arr, l, r, ASSIGN_EXPR);
    case ASSIGN_EXPR:
        if ((nxt = findNextSection(arr, l, r, condASSIGN)) != -1)
        {
            now = new_AST(arr[nxt].kind, 0);
            now->lhs = parse(arr, l, nxt - 1, UNARY_EXPR);
            now->rhs = parse(arr, nxt + 1, r, ASSIGN_EXPR);
            return now;
        }
        return parse(arr, l, r, ADD_EXPR);
    case ADD_EXPR:
        if ((nxt = findNextSection(arr, r, l, condADD)) != -1)
        {
            now = new_AST(arr[nxt].kind, 0);
            now->lhs = parse(arr, l, nxt - 1, ADD_EXPR);
            now->rhs = parse(arr, nxt + 1, r, MUL_EXPR);
            return now;
        }
        return parse(arr, l, r, MUL_EXPR);
    case MUL_EXPR:
        // TODO: Implement MUL_EXPR.
        // hint: Take ADD_EXPR as reference.
        if ((nxt = findNextSection(arr, r, l, condMUL)) != -1)
        {
            now = new_AST(arr[nxt].kind, 0);
            now->lhs = parse(arr, l, nxt - 1, MUL_EXPR);
            now->rhs = parse(arr, nxt + 1, r, UNARY_EXPR);
            return now;
        }
        return parse(arr, l, r, UNARY_EXPR);

    case UNARY_EXPR:
        // TODO: Implement UNARY_EXPR.
        // hint: Take POSTFIX_EXPR as reference.
        if (arr[l].kind == PLUS || arr[l].kind == MINUS || arr[l].kind == PREINC || arr[l].kind == PREDEC)
        {
            now = new_AST(arr[l].kind, 0);
            now->mid = parse(arr, l + 1, r, UNARY_EXPR);
            return now;
        }

        return parse(arr, l, r, POSTFIX_EXPR);

    case POSTFIX_EXPR:
        if (arr[r].kind == PREINC || arr[r].kind == PREDEC)
        {
            // translate "PREINC", "PREDEC" into "POSTINC", "POSTDEC"
            now = new_AST(arr[r].kind - PREINC + POSTINC, 0);
            now->mid = parse(arr, l, r - 1, POSTFIX_EXPR);
            return now;
        }
        return parse(arr, l, r, PRI_EXPR);
    case PRI_EXPR:
        if (findNextSection(arr, l, r, condRPAR) == r)
        {
            now = new_AST(LPAR, 0);
            now->mid = parse(arr, l + 1, r - 1, EXPR);
            return now;
        }
        if (l == r)
        {
            if (arr[l].kind == IDENTIFIER || arr[l].kind == CONSTANT)
            {
                return new_AST(arr[l].kind, arr[l].val);
            }
            err("Unexpected token during parsing.");
        }
        err("No token left for parsing.");
    default:
        err("Unexpected grammar state.");
    }
}

AST *new_AST(Kind kind, int val)
{
    AST *res = (AST *)malloc(sizeof(AST));
    res->kind = kind;
    res->val = val;
    res->lhs = res->mid = res->rhs = NULL;
    return res;
}

int findNextSection(Token *arr, int start, int end, int (*cond)(Kind))
{
    int par = 0;
    int d = (start < end) ? 1 : -1;
    for (int i = start; (start < end) ? (i <= end) : (i >= end); i += d)
    {
        if (arr[i].kind == LPAR)
            par++;
        if (arr[i].kind == RPAR)
            par--;
        if (par == 0 && cond(arr[i].kind) == 1)
            return i;
    }
    return -1;
}

int condASSIGN(Kind kind)
{
    return kind == ASSIGN;
}

int condADD(Kind kind)
{
    return kind == ADD || kind == SUB;
}

int condMUL(Kind kind)
{
    return kind == MUL || kind == DIV || kind == REM;
}

int condRPAR(Kind kind)
{
    return kind == RPAR;
}

void semantic_check(AST *now)
{
    if (now == NULL)
        return;
    // Left operand of '=' must be an identifier or identifier with one or more parentheses.
    if (now->kind == ASSIGN)
    {
        AST *tmp = now->lhs;
        while (tmp->kind == LPAR)
            tmp = tmp->mid;
        if (tmp->kind != IDENTIFIER)
            err("Lvalue is required as left operand of assignment.");
    }
    // Operand of INC/DEC must be an identifier or identifier with one or more parentheses.
    // TODO: Implement the remaining semantic_check code.
    // hint: Follow the instruction above and ASSIGN-part code to implement.
    // hint: Semantic of each node needs to be checked recursively (from the current node to lhs/mid/rhs node).
    if (now->kind == PREINC || now->kind == PREDEC || now->kind == POSTDEC || now->kind == POSTINC)
    {
        AST *tmp = now->mid;
        while (tmp->kind == LPAR)
            tmp = tmp->mid;
        if (tmp->kind != IDENTIFIER)
            err("INC/DEC semantic error");
    }

    semantic_check(now->lhs);
    semantic_check(now->mid);
    semantic_check(now->rhs);
}

int codegen(AST *root)
{
    // TODO: Implement your codegen in your own way.
    // You may modify the function parameter or the return type, even the whole structure as you wish.

    static int flag_x = 0, flag_y = 0, flag_z = 0;
    static int posinc = -1, posdec = -1, minus = -1;
    static int inc_id, dec_id;
    static int minus_count;
    static int r_count = 3;
    static int pos_incx = 0, pos_incy = 0, pos_incz = 0;
    static int pos_decx = 0, pos_decy = 0, pos_decz = 0;

    if (root == NULL)
        return;

    if (root->kind == ASSIGN)
    {
        int a = codegen(root->rhs);
        int b = codegen(root->lhs);
        int tmp = 4 * abs(b + 1);
        if (a >= 0 && b < 0)
        {
            printf("add r%d 0 %d\n", tmp / 4, a);
            printf("store [%d] r%d\n", tmp, tmp / 4);
            printf("load r%d [%d]\n", tmp / 4, tmp);
            printf("load r%d [%d]\n", a, tmp);
        }
        else if (a < 0 && b < 0)
        {
            printf("store [%d] r%d\n", tmp, abs(a + 1));
            printf("load r%d [%d]\n", tmp / 4, tmp);
            printf("load r%d [%d]\n", r_count, tmp);
        }

        if (flag == 1)
        {
            flag = -1;
            if (pos_incx > 0)
            {
                if (flag_x == 0)
                    printf("load r0 [0]\n");
                printf("add r0 r0 %d\n", pos_incx);
                printf("store [0] r0\n");
            }
            if (pos_incy > 0)
            {
                if (flag_y == 0)
                    printf("load r1 [4]\n");
                printf("add r1 r1 %d\n", pos_incy);
                printf("store [4] r1\n");
            }
            if (pos_incz > 0)
            {
                if (flag_z == 0)
                    printf("load r2 [8]\n");
                printf("add r2 r2 %d\n", pos_incz);
                printf("store [8] r2\n");
            }
            if (pos_decx > 0)
            {
                if (flag_x == 0)
                    printf("load r0 [0]\n");
                printf("sub r0 r0 %d\n", pos_decx);
                printf("store [0] r0\n");
            }
            if (pos_decy > 0)
            {
                if (flag_y == 0)
                    printf("load r1 [4]\n");
                printf("sub r1 r1 %d\n", pos_decy);
                printf("store [4] r1\n");
            }
            if (pos_decz > 0)
            {
                if (flag_z == 0)
                    printf("load r2 [8]\n");
                printf("sub r2 r2 %d\n", pos_decz);
                printf("store [8] r2\n");
            }
        }
        pos_decx = pos_decy = pos_decz = pos_incx = pos_incy = pos_incz = 0;

        r_count++;

        if (a >= 0)
            return a;
        else
            return -r_count;
    }
    else if (root->kind == ADD)
    {
        r_count++;
        int a = codegen(root->lhs);
        r_count++;
        int b = codegen(root->rhs);

        r_count = r_count - 2;
        if (a >= 0 && b >= 0)
            printf("add r%d %d %d\n", r_count, a, b);
        else if (a >= 0 && b < 0)
            printf("add r%d %d r%d\n", r_count, a, abs(b + 1));
        else if (a < 0 && b >= 0)
            printf("add r%d r%d %d\n", r_count, abs(a + 1), b);
        else if (a < 0 && b < 0)
            printf("add r%d r%d r%d\n", r_count, abs(a + 1), abs(b + 1));

        int tmp = -r_count - 1;
        return tmp;
    }
    else if (root->kind == SUB)
    {
        r_count++;
        int a = codegen(root->lhs);
        r_count++;
        int b = codegen(root->rhs);
        r_count = r_count - 2;

        if (a >= 0 && b >= 0)
        {
            printf("sub r%d %d %d\n", r_count, a, b);
        }
        else if (a >= 0 && b < 0)
            printf("sub r%d %d r%d\n", r_count, a, abs(b));
        else if (a < 0 && b >= 0)
            printf("sub r%d r%d %d\n", r_count, abs(a + 1), b);
        else
            printf("sub r%d r%d r%d\n", r_count, abs(a + 1), abs(b + 1));

        int tmp = -r_count - 1;
        return tmp;
    }
    else if (root->kind == DIV)
    {
        r_count++;
        int a = codegen(root->lhs);
        r_count++;
        int b = codegen(root->rhs);
        r_count = r_count - 2;

        if (a >= 0 && b >= 0)
        {
            printf("div r%d %d %d\n", r_count, a, b);
        }
        else if (a >= 0 && b < 0)
            printf("div r%d %d r%d\n", r_count, a, abs(b + 1));
        else if (a < 0 && b >= 0)
            printf("div r%d r%d %d\n", r_count, abs(a + 1), b);
        else
            printf("div r%d r%d r%d\n", r_count, abs(a + 1), abs(b + 1));

        int tmp = -r_count - 1;
        return tmp;
    }
    else if (root->kind == MUL)
    {
        r_count++;
        int a = codegen(root->lhs);
        r_count++;
        int b = codegen(root->rhs);
        r_count = r_count - 2;

        if (a >= 0 && b >= 0)
            printf("mul r%d %d %d\n", r_count, a, b);
        else if (a >= 0 && b < 0)
            printf("mul r%d %d r%d\n", r_count, a, abs(b + 1));
        else if (a < 0 && b >= 0)
            printf("mul r%d r%d %d\n", r_count, abs(a + 1), b);
        else
            printf("mul r%d r%d r%d\n", r_count, abs(a + 1), abs(b + 1));

        int tmp = -r_count - 1;
        return tmp;
    }
    else if (root->kind == REM)
    {
        r_count++;
        int a = codegen(root->lhs);
        r_count++;
        int b = codegen(root->rhs);
        r_count = r_count - 2;

        if (a >= 0 && b >= 0)
            printf("rem r%d %d %d\n", r_count, a, b);
        else if (a >= 0 && b < 0)
            printf("rem r%d %d r%d\n", r_count, a, abs(b + 1));
        else if (a < 0 && b >= 0)
            printf("rem r%d r%d %d\n", r_count, abs(a + 1), b);
        else if (a < 0 && b < 0)
            printf("rem r%d r%d r%d\n", r_count, abs(a + 1), abs(b + 1));

        int tmp = -r_count - 1;
        return tmp;
    }
    else if (root->kind == PREINC)
    {

        int s = codegen(root->mid);
        int tmp = abs(s + 1);
        int t2 = tmp * 4;
        printf("add r%d r%d 1\n", tmp, tmp);
        printf("store [%d] r%d\n", t2, tmp);

        return s;
    }
    else if (root->kind == PREDEC)
    {
        int s = codegen(root->mid);
        int tmp = abs(s + 1);
        int t2 = tmp * 4;
        printf("sub r%d r%d 1\n", tmp, tmp);
        printf("store [%d] r%d\n", t2, tmp);
        return s;
    }
    else if (root->kind == POSTINC)
    {
        int s = codegen(root->mid);
        if (s == -1)
            pos_incx++;
        else if (s == -2)
            pos_incy++;
        else if (s == -3)
            pos_incz++;
        inc_id = abs(s + 1);
        posinc = 1;
        return s;
    }
    else if (root->kind == POSTDEC)
    {
        int s = codegen(root->mid);
        if (s == -1)
            pos_decx++;
        else if (s == -2)
            pos_decy++;
        else if (s == -3)
            pos_decz++;
        dec_id = abs(s + 1);
        posdec = 1;
        return s;
    }
    else if (root->kind == IDENTIFIER)
    {
        if (root->val == 120)
        {
            if (flag_x == 0)
            {
                printf("load r0 [0]\n");
                flag_x = 1;
            }
            return -1;
        }
        else if (root->val == 121)
        {
            if (flag_y == 0)
            {
                printf("load r1 [4]\n");
                flag_y = 1;
            }
            return -2;
        }
        else if (root->val == 122)
        {
            if (flag_z == 0)
            {
                printf("load r2 [8]\n");
                flag_z = 1;
            }

            return -3;
        }
    }
    else if (root->kind == CONSTANT)
    {
        return root->val;
    }
    else if (root->kind == LPAR)
    {
        return codegen(root->mid);
    }
    else if (root->kind == PLUS)
    {
        return codegen(root->mid);
    }
    else if (root->kind == MINUS)
    {
        int a = codegen(root->mid);

        if (a >= 0)
            printf("sub r%d 0 %d\n", r_count + 1, a);

        else if (a < 0)
            printf("sub r%d 0 r%d\n", r_count + 1, abs(a + 1));

        return -r_count - 2;
    }
}
//ASSIGN, ADD, SUB, MUL, DIV, REM, PREINC, PREDEC,
//POSTINC, POSTDEC, IDENTIFIER, CONSTANT, LPAR, RPAR, PLUS, MINUS

void freeAST(AST *now)
{
    if (now == NULL)
        return;
    freeAST(now->lhs);
    freeAST(now->mid);
    freeAST(now->rhs);
    free(now);
}

void token_print(Token *in, size_t len)
{
    const static char KindName[][20] = {
        "Assign", "Add", "Sub", "Mul", "Div", "Rem", "Inc", "Dec", "Inc", "Dec", "Identifier", "Constant", "LPar", "RPar", "Plus", "Minus"};
    const static char KindSymbol[][20] = {
        "'='", "'+'", "'-'", "'*'", "'/'", "'%'", "\"++\"", "\"--\"", "\"++\"", "\"--\"", "", "", "'('", "')'", "'+'", "'-'"};
    const static char format_str[] = "<Index = %3d>: %-10s, %-6s = %s\n";
    const static char format_int[] = "<Index = %3d>: %-10s, %-6s = %d\n";
    for (int i = 0; i < len; i++)
    {
        switch (in[i].kind)
        {
        case LPAR:
        case RPAR:
        case PREINC:
        case PREDEC:
        case ADD:
        case SUB:
        case MUL:
        case DIV:
        case REM:
        case ASSIGN:
        case PLUS:
        case MINUS:
            printf(format_str, i, KindName[in[i].kind], "symbol", KindSymbol[in[i].kind]);
            break;
        case CONSTANT:
            printf(format_int, i, KindName[in[i].kind], "value", in[i].val);
            break;
        case IDENTIFIER:
            printf(format_str, i, KindName[in[i].kind], "name", (char *)(&(in[i].val)));
            break;
        default:
            puts("=== unknown token ===");
        }
    }
}

void AST_print(AST *head)
{
    static char indent_str[MAX_LENGTH] = "";
    static int indent = 0;
    char *indent_now = indent_str + indent;
    const static char KindName[][20] = {
        "Assign", "Add", "Sub", "Mul", "Div", "Rem", "PreInc", "PreDec", "PostInc", "PostDec", "Identifier", "Constant", "Parentheses", "Parentheses", "Plus", "Minus"};
    const static char format[] = "%s\n";
    const static char format_str[] = "%s, <%s = %s>\n";
    const static char format_val[] = "%s, <%s = %d>\n";
    if (head == NULL)
        return;
    indent_str[indent - 1] = '-';
    printf("%s", indent_str);
    indent_str[indent - 1] = ' ';
    if (indent_str[indent - 2] == '`')
        indent_str[indent - 2] = ' ';
    switch (head->kind)
    {
    case ASSIGN:
    case ADD:
    case SUB:
    case MUL:
    case DIV:
    case REM:
    case PREINC:
    case PREDEC:
    case POSTINC:
    case POSTDEC:
    case LPAR:
    case RPAR:
    case PLUS:
    case MINUS:
        printf(format, KindName[head->kind]);
        break;
    case IDENTIFIER:
        printf(format_str, KindName[head->kind], "name", (char *)&(head->val));
        break;
    case CONSTANT:
        printf(format_val, KindName[head->kind], "value", head->val);
        break;
    default:
        puts("=== unknown AST type ===");
    }
    indent += 2;
    strcpy(indent_now, "| ");
    AST_print(head->lhs);
    strcpy(indent_now, "` ");
    AST_print(head->mid);
    AST_print(head->rhs);
    indent -= 2;
    (*indent_now) = '\0';
}

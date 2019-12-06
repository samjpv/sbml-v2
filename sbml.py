# Samuel van der Sloot
# 111459704

import sys
import ply.lex as lex
import ply.yacc as yacc
from copy import copy

# list of token names

# reserved = {
#     'andalso' : 'CONJUNCTION',
# }

tokens = (
    'INTEGER',
    'REAL',
    'BOOLEAN',
    'STRING',
    'LPAREN',
    'RPAREN',
    'LBRACKET',
    'RBRACKET',
    'EXPONENT',
    'MULT',
    'DIV',
    'INTDIV',
    'MOD',
    'ADD',
    'MINUS',
    'APPEND',
    'IN',
    'NOT',
    'CONJUNCTION',
    'OR',
    'LESSTHAN',
    'LESSTHANEQ',
    'EQUALS',
    'NOTEQUALS',
    'GREATERTHAN',
    'GREATERTHANEQ',
    'COMMA',
    'POUND',
    'SEMICOLON',
    'PRINT',
    'IF',
    'ELSE',
    'WHILE',
    'VAR',
    'ASSIGN',
    'RBRACE',
    'LBRACE',

)

# constant definitions
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_EXPONENT = r'\*\*'
t_MULT = r'\*'
t_DIV = r'\/'
t_ADD = r'\+'
t_MINUS = r'\-'
t_APPEND = r'\:\:'
t_LESSTHAN = r'<'
t_LESSTHANEQ = r'<='
t_EQUALS = r'=='
t_NOTEQUALS = r'<>'
t_GREATERTHAN = r'>'
t_GREATERTHANEQ = r'>='
t_COMMA = r','
t_POUND = r'\#'
t_SEMICOLON = r';'
t_ASSIGN = r'\='
t_LBRACE = r'\{'
t_RBRACE = r'\}'

t_ignore = ' \t'

########################################################################################################################

# variable record
variables = {}


# SYNTAX TREE
class Node:
    def __init__(self):
        self.children = []


class Variable:
    def __init__(self, name=""):
        self.name = name

    def eval(self):
        if self.name in variables:
            result = variables[self.name]
            return result
        else:
            print("SYNTAX ERROR")

    def __str__(self):
        return "Variable: " + self.name


class AssignNode:
    def __init__(self, name, value):
        self.name = name
        self.value = value  # An expression node

    def eval(self):
        variables[self.name] = self.value.eval()


class Addition:
    def __init__(self, p, left, right):
        self.left = left
        self.right = right
        self.p = p

    def eval(self):
        if type(self.left.eval()) == type(self.right.eval()):
            result = self.left.eval() + self.right.eval()
        elif type(self.left.eval()) != type(self.right.eval()):
            if type(self.left.eval()) == str:
                if type(self.right.eval()) == list:
                    result = [self.left] + self.right.eval()
            elif type(self.right.eval()) == str:
                if type(self.left.eval()) == list:
                    result = self.left.eval() + [self.right]
            elif (type(self.left.eval()) == int or type(self.left.eval()) == float) and type(self.right.eval()) == list:
                result = [self.left] + self.right.eval()
            elif (type(self.right.eval()) == int or type(self.right.eval()) == float) and type(
                    self.left.eval()) == list:
                result = self.left.eval() + [self.right]
            else:
                result = self.left.eval() + self.right.eval()
        else:
            p_error(self.p)

        return result


class Subtraction:
    def __init__(self, p, left, right):
        self.left = left
        self.right = right
        self.p = p

    def eval(self):
        if (type(self.left.eval()) == int or type(self.left.eval()) == float) and (
                type(self.right.eval()) == int or type(self.right.eval()) == float):
            result = self.left.eval() - self.right.eval()


        else:
            p_error(self.p)

        return result


class Negation:
    def __init__(self, p, value):
        self.value = value
        self.p = p

    def eval(self):
        if type(self.value.eval()) == int or type(self.value.eval()) == float:
            result = -self.value.eval()

        else:
            p_error(self.p)

        return result


class Multiplication:
    def __init__(self, p, left, right):
        self.left = left
        self.right = right
        self.p = p

    def eval(self):
        if type(self.left.eval()) == int or type(self.left.eval()) == float and type(self.right.eval()) == int or type(
                self.right.eval()) == float:
            result = self.left.eval() * self.right.eval()

        return result


class Division:
    def __init__(self, p, left, right):
        self.left = left
        self.right = right
        self.p = p

    def eval(self):
        if ((type(self.left.eval()) == int and type(self.right.eval()) == int) or (
                type(self.left.eval()) == float and type(
            self.right.eval()) == float)) and self.right.eval() != 0:
            result = self.left.eval() / self.right.eval()
        else:
            p_error(self.p)

        return result


class IntDivision:
    def __init__(self, p, left, right):
        self.left = left
        self.right = right
        self.p = p

    def eval(self):
        if type(self.left.eval()) == int and type(self.right.eval()) == int and self.right.eval() != 0:
            result = self.left.eval() // self.right.eval()
        else:
            p_error(self.p)

        return result


class Comparison:
    def __init__(self, p, op, left, right):
        self.left = left
        self.right = right
        self.op = op
        self.p = p

    def eval(self):
        if self.op == '<':
            if (type(self.left.eval()) == int and type(self.right.eval()) == int) or (
                    type(self.left.eval()) == str and type(self.right.eval()) == str):
                result = self.left.eval() < self.right.eval()
            else:
                p_error(self.p)
        elif self.op == '<=':
            if (type(self.left.eval()) == int and type(self.right.eval()) == int) or (
                    type(self.left.eval()) == str and type(self.right.eval()) == str):
                result = self.left.eval() <= self.right.eval()
            else:
                p_error(self.p)
        elif self.op == '==':
            if (type(self.left.eval()) == int and type(self.right.eval()) == int) or (
                    type(self.left.eval()) == str and type(self.right.eval()) == str):
                result = self.left.eval() == self.right.eval()
            else:
                p_error(self.p)
        elif self.op == '!=':
            if (type(self.left.eval()) == int and type(self.right.eval()) == int) or (
                    type(self.left.eval()) == str and type(self.right.eval()) == str):
                result = self.left.eval() != self.right.eval()
            else:
                p_error(self.p)
        elif self.op == '>':
            if (type(self.left.eval()) == int and type(self.right.eval()) == int) or (
                    type(self.left.eval()) == str and type(self.right.eval()) == str):
                result = self.left.eval() > self.right.eval()
            else:
                p_error(self.p)
        else:
            if (type(self.left.eval()) == int and type(self.right.eval()) == int) or (
                    type(self.left.eval()) == str and type(self.right.eval()) == str):
                result = self.left.eval() >= self.right.eval()
            else:
                p_error(self.p)
        return result


class AndStatement:
    def __init__(self, p, left, right):
        self.left = left
        self.right = right
        self.p = p

    def eval(self):
        if type(self.left.eval()) == bool and type(self.right.eval()) == bool:
            result = self.right.eval() and self.right.eval()
        else:
            p_error(self.p)
        return result


class OrStatement:
    def __init__(self, p, left, right):
        self.left = left
        self.right = right
        self.p = p

    def eval(self):
        if type(self.left.eval()) == bool and type(self.right.eval()) == bool:
            result = self.left.eval() or self.right.eval()
        else:
            p_error(self.p)
        return result


class NotStatement:
    def __init__(self, p, operand):
        self.operand = operand
        self.p = p

    def eval(self):
        if type(self.operand.eval()) == bool:
            result = not self.operand.eval()
        else:
            p_error(self.p)
        return result


class AppendStatement:
    def __init__(self, p, left, right):
        self.left = left
        self.right = right
        self.p = p

    def eval(self):
        if type(self.right.eval()) == list:
            self.right.append(self.left)
            result = self.right.eval()
        else:
            p_error(self.p)
        return result


class InStatement:
    def __init__(self, p, left, right):
        self.left = left
        self.right = right
        self.p = p

    def eval(self):
        if type(self.right.eval()) == list:
            result = self.left.eval() in self.right.eval()
        else:
            p_error(self.p)
        return result


class ModuloStatement:
    def __init__(self, p, left, right):
        self.left = left
        self.right = right
        self.p = p

    def eval(self):
        if type(self.left.eval()) == int and type(self.right.eval()) == int:
            result = self.left.eval() % self.right.eval()
        else:
            p_error(self.p)
        return result


class ExponentStatement:
    def __init__(self, p, left, right):
        self.left = left
        self.right = right
        self.p = p

    def eval(self):
        if (type(self.left.eval()) == int or type(self.left.eval()) == float) and (
                type(self.right.eval()) == int or type(self.right.eval()) == float):
            result = self.left.eval() ** self.right.eval()
        else:
            p_error(self.p)
        return result


# BASIC TYPE NODES
class IntNode:
    def __init__(self, value):
        self.value = value

    def eval(self):
        return self.value


class RealNode:
    def __init__(self, value):
        self.value = value

    def eval(self):
        return self.value


class BooleanNode:
    def __init__(self, value):
        self.value = value

    def eval(self):
        return self.value


class StringNode:
    def __init__(self, value):
        self.value = value

    def eval(self):
        return self.value


class ListNode:
    def __init__(self, value):
        self.value = value

    def eval(self):
        result = []
        for item in self.value:
            result.append(item.eval())
        return result


class TupleNode:
    def __init__(self, value):
        self.value = value

    def eval(self):
        return self.value


class BlockNode:
    def __init__(self, p):
        self.arr = []
        self.p = p

    def eval(self):
        for stmt in self.arr:
            print(stmt)
            stmt.eval()


class TupleIndexNode:
    def __init__(self, p, left, right):
        self.left = left
        self.right = right
        self.p = p

    def eval(self):
        if self.right.eval() in variables:
            if type(self.left.eval()) == int and type(variables[self.right.eval()]) == tuple:
                result = variables[self.right.eval()][self.left.eval() - 1]
            else:
                p_error(self.p)
        else:
            if type(self.left.eval()) == int and type(self.right.eval()) == tuple:
                result = self.right.eval()[self.left.eval() - 1]
            else:
                p_error(self.p)
        return result.eval()


class ListIndexNode:
    def __init__(self, p, left, right):
        self.left = left
        self.right = right
        self.p = p

    def eval(self):
        if type(self.left.eval()) == list and type(self.right.eval()) == int:
            result = self.left.eval()[self.right.eval()]
        else:
            p_error(self.p)
        return result


class ListSpliceNode:
    def __init__(self, p, list, index, value):
        self.list = list
        self.index = index
        self.value = value
        self.p = p

    def eval(self):
        print("List is: "+str(self.list.eval()))
        print("Index is: "+str(self.index.eval()))
        print("Value is: "+str(self.value.eval()))
        if type(self.list.eval()) == list and type(self.index.eval()) == int:
            if self.list in variables:
                variables[self.list.eval()][self.index.eval()] = self.value.eval()
                print("test1")
            else:
                lst = self.list.eval()
                lst[self.index.eval()] = self.value.eval()

        else:
            p_error(self.p)


class PrintNode:
    def __init__(self, p, expr):
        self.expr = expr
        self.p = p

    def eval(self):
        print(self.expr.eval())


class ifNode:
    def __init__(self, p, condition, block):
        self.condition = condition
        self.block = block
        self.p = p

    def eval(self):

        if type(self.condition.eval()) == bool:
            if self.condition.eval():
                self.block.eval()
        else:
            p_error(self.p)


class ifElseNode:
    def __init__(self, p, condition, lblock, rblock):
        self.condition = condition
        self.lblock = lblock
        self.rblock = rblock
        self.p = p

    def eval(self):
        if type(self.condition.eval()) == bool:
            if self.condition.eval():
                self.lblock.eval()
            else:
                self.rblock.eval()
        else:
            p_error(self.p)


class whileNode:
    def __init__(self, p, condition, block):
        self.condition = condition
        self.p = p
        self.block = block

    def eval(self):
        if type(self.condition.eval()) == bool:
            while self.condition.eval():
                self.block.eval()


# blocks
blockRoot = []


########################################################################################################################

def t_INTDIV(t):
    r'div'
    return t

def t_MOD(t):
    r'mod'
    return t

def t_IN(t):
    r'in'
    return t

def t_PRINT(t):
    r'print'
    return t


def t_WHILE(t):
    r'while'
    return t


def t_IF(t):
    r'if'
    return t


def t_ELSE(t):
    r'else'
    return t


def t_CONJUNCTION(t):
    r'andalso'
    return t


def t_OR(t):
    r'orelse'
    return t


def t_NOT(t):
    r'not'
    return t


# operand definition
def t_REAL(t):
    r'((\d*\.\d+)|(\d+\.\d*))([e][-]?\d+)?'
    t.value = float(t.value)
    return t


def t_INTEGER(t):
    r'0*[0-9]+'
    t.value = int(t.value)
    return t


def t_BOOLEAN(t):
    r'(False)|(True)'
    t.value = eval(t.value)
    return t


def t_STRING(t):
    r'(\"[^\"]*\")|(\'[^\']*\')'
    return t


def t_VAR(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    if t in variables:
        return variables[t]
    else:
        return t


# newline character
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


# error handling
def t_error(t):
    print("SYNTAX ERROR")


def p_error(p):
    print("SEMANTIC ERROR")


########################################################################################################################

lexer = lex.lex()


# STATEMENT STRUCTURES

# block
def p_block(p):
    'block : LBRACE statement blocktail RBRACE'
    StmBlock = [p[2]] + p[3]
    global blockRoot
    p[0] = BlockNode(p)
    for statement in StmBlock:
        p[0].arr.append(statement)

    blockRoot.append(p[0])


def p_blocktail_default(p):
    'blocktail : statement blocktail'
    p[0] = [p[1]] + p[2]


def p_blocktail_empty(p):
    'blocktail : '
    p[0] = []


def p_statement(p):
    'statement : expr SEMICOLON'
    p[0] = p[1]


def p_print(p):
    'expr : PRINT LPAREN expr RPAREN'
    p[0] = PrintNode(p, p[3])


# CONDITIONALS ###################################
def p_conditional_if(p):
    'statement : IF LPAREN expr RPAREN block %prec CONDITIONAL'
    p[0] = ifNode(p, p[3], p[5])


def p_conditional_ifelse(p):
    'statement : IF LPAREN expr RPAREN block ELSE block %prec CONDITIONAL'
    p[0] = ifElseNode(p, p[3], p[5], p[7])


# LOOPS #########################################
def p_while_loop(p):
    'statement : WHILE LPAREN expr RPAREN block'
    p[0] = whileNode(p, p[3], p[5])


# VARIABLE ASSIGNMENT
def p_variable(p):
    'expr : VAR %prec VARIABLE'
    p[0] = Variable(p[1])


def p_variable_assign(p):
    'expr : VAR ASSIGN expr'
    p[0] = AssignNode(p[1], p[3])


# OPERAND GRAMMAR
def p_int(p):
    'expr : INTEGER'
    p[0] = IntNode(p[1])


def p_real(p):
    'expr : REAL'
    p[0] = RealNode(p[1])


def p_boolean(p):
    'expr : BOOLEAN'
    p[0] = BooleanNode(p[1])


def p_string(p):
    'expr : STRING'
    if p[1][0] is '"':
        p[0] = StringNode(p[1].strip('"'))
    elif p[1][0] is "'":
        p[0] = StringNode(p[1].strip("'"))
    else:
        p_error(p)


# LIST GRAMMAR
def p_expr_listdefault(p):
    'expr : LBRACKET expr listtail RBRACKET'
    p[0] = ListNode([p[2]] + p[3])


def p_expr_listempy(p):
    'expr : LBRACKET RBRACKET'
    p[0] = ListNode([])


def p_listtail_default(p):
    'listtail : COMMA expr listtail'
    p[0] = [p[2]] + p[3]


def p_listtail_empty(p):
    'listtail : '
    p[0] = []


def p_expr_listindex(p):
    'expr : expr LBRACKET expr RBRACKET %prec LISTINDEX'
    p[0] = ListIndexNode(p, p[1], p[3])


def p_expr_listsplice(p):
    'expr : expr LBRACKET expr RBRACKET ASSIGN expr'
    p[0] = ListSpliceNode(p, p[1], p[3], p[6])


# TUPLE GRAMMAR
def p_expr_tupledefault(p):
    'expr : LPAREN expr tupletail RPAREN %prec TUPLE'
    p[0] = TupleNode((p[2],) + p[3])


def p_expr_tupleempty(p):
    'expr : LPAREN RPAREN'
    p[0] = TupleNode(())


def p_expr_tupleindex(p):
    'expr : POUND expr expr %prec TUPLEINDEX'
    p[0] = TupleIndexNode(p, p[2], p[3])


def p_tupletail_default(p):
    'tupletail : COMMA expr tupletail'
    p[0] = (p[2],) + p[3]


def p_tupletail_empty(p):
    'tupletail : '
    p[0] = ()


# ADDITION
def p_addition(p):
    'expr : expr ADD expr %prec ADD'
    p[0] = Addition(p, p[1], p[3])


# SUBTRACTION
def p_negativenum(p):
    'expr : MINUS expr %prec NEG'
    p[0] = Negation(p, p[2])


def p_subtraction(p):
    'expr : expr MINUS expr %prec SUB'
    p[0] = Subtraction(p, p[1], p[3])


# LESS THAN
def p_lessThan(p):
    'expr : expr LESSTHAN expr'
    p[0] = Comparison(p, p[2], p[1], p[3])


# LESS THAN OR EQUAL TO
def p_lessThanEq(p):
    'expr : expr LESSTHANEQ expr'
    p[0] = Comparison(p, p[2], p[1], p[3])


# EQUALS (BOOL)
def p_equals(p):
    'expr : expr EQUALS expr'
    p[0] = Comparison(p, p[2], p[1], p[3])


# NOT EQUAL
def p_notEquals(p):
    'expr : expr NOTEQUALS expr'
    p[0] = Comparison(p, p[2], p[1], p[3])


# GREATER THAN
def p_greaterThan(p):
    'expr : expr GREATERTHAN expr'
    p[0] = Comparison(p, p[2], p[1], p[3])


# GREATER THAN OR EQUAL TO
def p_greaterThanEq(p):
    'expr : expr GREATERTHANEQ expr'
    p[0] = Comparison(p, p[2], p[1], p[3])


# NEGATION
def p_not(p):
    'expr : NOT expr'
    p[0] = NotStatement(p, p[2])


# CONJUNCTION
def p_conjunction(p):
    'expr : expr CONJUNCTION expr'
    p[0] = AndStatement(p, p[1], p[3])


# OR ELSE
def p_or(p):
    'expr : expr OR expr'
    p[0] = OrStatement(p, p[1], p[3])


# EXPRESSION WITH PARENTHESES
def p_parenthetical(p):
    'expr : LPAREN expr RPAREN'
    p[0] = (p[2])


# EXPRESSION WITH EXPONENT
def p_exponentiate(p):
    'expr : expr EXPONENT expr'
    p[0] = ExponentStatement(p, p[1], p[3])


# DIVISION
def p_divide(p):
    'expr : expr DIV expr'
    p[0] = Division(p, p[1], p[3])


# MULTIPLICATION
def p_multiply(p):
    'expr : expr MULT expr'
    p[0] = Multiplication(p, p[1], p[3])


# INT DIVISION
def p_intDivide(p):
    'expr : expr INTDIV expr'
    p[0] = IntDivision(p, p[1], p[3])


# MODULO
def p_modulo(p):
    'expr : expr MOD expr'
    p[0] = ModuloStatement(p, p[1], p[3])


# MEMBER OF SET
def p_in(p):
    'expr : expr IN expr %prec MEMBER'
    p[0] = InStatement(p, p[1], p[3])


# APPEND ITEM TO SET
def p_append(p):
    'expr : expr APPEND expr'
    p[0] = AppendStatement(p, p[1], p[3])


########################################################################################################################

# setting precedence to resolve ambiguity
precedence = (
    ('left', 'VARIABLE'),
    ('left', 'OR'),
    ('left', 'CONJUNCTION'),
    ('left', 'NOT'),
    ('left', 'LESSTHANEQ', 'LESSTHAN', 'EQUALS', 'GREATERTHANEQ', 'GREATERTHAN', 'NOTEQUALS'),
    ('right', 'APPEND'),
    ('left', 'MEMBER'),
    ('left', 'SUB'),
    ('right', 'NEG'),
    ('left', 'ADD'),
    ('left', 'INTDIV', 'MOD', 'DIV', 'MULT'),
    ('right', 'EXPONENT'),
    ('left', 'LISTINDEX'),
    ('left', 'TUPLEINDEX'),
    ('left', 'TUPLE'),
    ('left', 'CONDITIONAL'),
    ('left', 'LBRACKET', 'RBRACKET'),
    ('left', 'LPAREN', 'RPAREN'),
)

parser = yacc.yacc()

# while True:
#     try:
#         s = input("Enter a proposition: ")
#     except EOFError:
#         break
#     if not s:
#         continue
#     result = parser.parse(s)  #, debug=True
#     try:
#         blockRoot[len(blockRoot) - 1].eval()
#     except:
#         print("SYNTAX ERROR")
#
#     print(variables)
    # for x in blockRoot:
    #     print("Block root: " + str(x))
    # for x in blockRoot[0].arr:
    #     print("Statements in outer block " + str(x))

try:
    f = open(sys.argv[-1])
    s = f.read()

except EOFError:
    print("invalid file")
result = parser.parse(s)  # , debug=True
try:
    blockRoot[len(blockRoot) - 1].eval()
except:
    print("BLOCK SYNTAX ERROR")
print("RESULT:", result)

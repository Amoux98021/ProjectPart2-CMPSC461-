# If you haven't read the P2 post in campuswire, read it before you continue.

# If you have working lexer of project 1, then you are good to go, you just
# need few modifications in the lexer. I believe you are better off, if you
# just extend it.

# If you don't have a working lexer for project 1, we have provide a skelton of 
# lexer. You need to complete the functions commented with tokenize.

# newline lexer.
class Token:
    def __init__(self, token_type, value=None):
        self.type = token_type
        self.value = value

class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.current_char = self.text[self.pos]

    def error(self):
        raise Exception('Invalid character')

    def advance(self):
        self.pos += 1
        if self.pos < len(self.text):
            self.current_char = self.text[self.pos]
        else:
            self.current_char = None
        if self.current_char == '\n':  # Skip over newline characters
            self.advance()

    def peek(self):
        peek_pos = self.pos + 1
        if peek_pos < len(self.text):
            return self.text[peek_pos]
        else:
            return None

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    # implement
    def number(self):
        result = ''
        is_float = False
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()

        if self.current_char == '.':
            result += self.current_char
            self.advance()
            is_float = True
            while self.current_char is not None and self.current_char.isdigit():
                result += self.current_char
                self.advance()
        return (result, is_float)

    # implement
    def identifier(self):
        result = ''
        while self.current_char is not None and (self.current_char.isalnum() or self.current_char == '_'):
            result += self.current_char
            self.advance()
        return result

    def get_next_token(self):
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue
            elif self.current_char.isdigit():
                res = self.number()
                if res[1] == 1:
                    return Token('FNUMBER', res[0])
                else:
                    return Token('NUMBER', res[0])
            elif self.current_char.isalpha() or self.current_char == '_':
                return self.keyword_or_identifier()
            elif self.current_char == '+' or self.current_char == '-' or self.current_char == '*' or self.current_char == '/':
                return self.operator()
            elif self.current_char == '(' or self.current_char == ')':
                token = Token('PARENTHESIS', self.current_char)
                self.advance()
                return token
            elif self.current_char == '{' or self.current_char == '}':
                token = Token('SCOPE', self.current_char)
                self.advance()
                return token
            elif self.current_char == '\n':  # Change delimiter to newline character
                token = Token('DELIMITER', self.current_char)
                self.advance()
                return token
            elif self.current_char == '!':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return Token('OPERATOR', '!=')
                else:
                    self.error()

            elif self.current_char == '=':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return Token('OPERATOR', '==')
                else:
                    return Token('OPERATOR', '=')

            elif self.current_char == '<':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return Token('OPERATOR', '<=')
                else:
                    return Token('OPERATOR', '<')
            elif self.current_char == '>':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return Token('OPERATOR', '>=')
                else:
                    return Token('OPERATOR', '>')
            else:
                self.error()
        return Token('EOF')

    def keyword_or_identifier(self):
        result = self.identifier()
        if result in ['int', 'float']:  # You can expand this list with more keywords
            return Token('KEYWORD', result)
        else:
            return Token('IDENTIFIER', result)

    #implement
    def operator(self):
        char = self.current_char
        if char in '+-*/':
            self.advance()
            return Token('OPERATOR', char)
        elif char == '=':
            self.advance()
            if self.current_char == '=':
                self.advance()
                return Token('OPERATOR', '==')
            return Token('OPERATOR', '=')
        elif char == '!':
            self.advance()
            if self.current_char == '=':
                self.advance()
                return Token('OPERATOR', '!=')
            self.error()
        elif char in '<>':
            self.advance()
            if self.current_char == '=':
                char += '='
                self.advance()
            return Token('OPERATOR', char)
        else:
            self.error()

# Parse Tree Node definitions.
# Don't need to modify these definitions for the completion of project 2.

# But if you are interested in modifying these definitions for
# learning purposes. Then Don't whatever you want.

class Node:
    pass

class ProgramNode(Node):
    def __init__(self, statements):
        self.statements = statements

class DeclarationNode(Node):
    def __init__(self, identifier, expression, myType):
        self.identifier = identifier
        self.expression = expression
        self.type       = myType

class AssignmentNode(Node):
    def __init__(self, identifier, expression):
        self.identifier = identifier
        self.expression = expression

class IfStatementNode(Node):
    def __init__(self, condition, if_block, else_block):
        self.condition = condition
        self.if_block = if_block
        self.else_block = else_block

class WhileLoopNode(Node):
    def __init__(self, condition, loop_block):
        self.condition = condition
        self.loop_block = loop_block

class ConditionNode(Node):
    def __init__(self, left, operator, right):
        self.left = left
        self.operator = operator
        self.right = right

class ArithmeticExpressionNode(Node):
    def __init__(self, operator, left, right, myType):
        self.operator = operator
        self.left = left
        self.right = right
        self.type  = myType

class TermNode(Node):
    def __init__(self, operator, left, right, myType):
        self.operator = operator
        self.left = left
        self.right = right
        self.type  = myType

class FactorNode(Node):
    def __init__(self, value, myType):
        self.value = value
        self.type = myType




# final parser - student copy

# Skelton of Parser class.
# For project 1, we should have implemented parser that returns a string representation.
# For project 2:
  # 1. You have to build the Parse tree with the node definitions given to you. The core
  # logic of how to parse the lanague will not differ, but you to have create Tree node
  # whereever you are creating tuple in the project 1.
  # 2. Implement symbol table and scoping rules. 
  #   Hint: You can use stack to model the nested scopes and a dictionary to store identifiers
  #   and its type.

  # For those who are interested, you call print_parse_tree to view the text representation
  # of Parse Tree.


class Parser:
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()
        self.scope_stack = [{}]  # Start with one global scope
       
        self.messages = []

    def print_parse_tree(self, node, indent=0):
        message = ""
        if isinstance(node, ProgramNode):
            message += '  ' * indent + 'Program\n'
            for statement in node.statements:
                message += self.print_parse_tree(statement, indent + 1)
        elif isinstance(node, DeclarationNode):
            message += '  ' * indent + 'Declaration: ' + node.identifier + '\n'
            message += self.print_parse_tree(node.expression, indent + 1)
        elif isinstance(node, AssignmentNode):
            message += '  ' * indent + 'Assignment: ' + node.identifier + '\n'
            message += self.print_parse_tree(node.expression, indent + 1)
        elif isinstance(node, IfStatementNode):
            message += '  ' * indent + 'If Statement\n'
            message += self.print_parse_tree(node.condition, indent + 1)
            message += '  ' * indent + 'Then Block:\n'
            for statement in node.if_block:
                message += self.print_parse_tree(statement, indent + 2)
            if node.else_block:
                message += '  ' * indent + 'Else Block:\n'
                for statement in node.else_block:
                    message += self.print_parse_tree(statement, indent + 2)
        elif isinstance(node, WhileLoopNode):
            message += '  ' * indent + 'While Loop\n'
            message += self.print_parse_tree(node.condition, indent + 1)
            message += '  ' * indent + 'Loop Block:\n'
            for statement in node.loop_block:
                message += self.print_parse_tree(statement, indent + 2)
        elif isinstance(node, ConditionNode):
            message += '  ' * indent + 'Condition : with operator ' + node.operator + '\n'
            message += '  ' * indent + 'LHS\n'
            message += self.print_parse_tree(node.left, indent + 2)
            message += '  ' * indent + 'RHS\n'
            message += self.print_parse_tree(node.right, indent + 2)
        elif isinstance(node, ArithmeticExpressionNode):
            message += '  ' * indent + 'Arithmetic Expression: ' + node.operator + '\n'
            message += self.print_parse_tree(node.left, indent + 1)
            message += self.print_parse_tree(node.right, indent + 1)
        elif isinstance(node, TermNode):
            message += '  ' * indent + 'Term: ' + node.operator + '\n'
            message += self.print_parse_tree(node.left, indent + 1)
            message += self.print_parse_tree(node.right, indent + 1)
        elif isinstance(node, FactorNode):
            message += '  ' * indent + 'Factor: ' + str(node.value) + '\n'

        return message


    def error(self, message):
        self.messages.append(message)

    #def eat(self, token_type, token_value=None):
    #    if self.current_token.type == token_type:
    #        if token_value is None or self.current_token.value == token_value:
    #            self.current_token = self.lexer.get_next_token()
    #        else:
    #            self.error(f'Expected token "{token_value}", but found "{self.current_token.value}"')
    #    else:
    #        self.error(f'Expected token type {token_type}, but found {self.current_token.type}')

    def eat(self, token_type):
        print(f"Eating {token_type}, current token is {self.current_token.type}: {self.current_token.value}")
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error(f'Expected token of type {token_type}, but found {self.current_token.type}')

    # enter the new scope in the program
    def enter_scope(self, scope_prefix):
        self.scope_stack.append({})
    # leave the current scope
    def leave_scope(self):
        if len(self.scope_stack) > 1:
            self.scope_stack.pop()
        else:
            self.error("Attempted to leave global scope.")       
    # return the current scope
    def current_scope(self):
        return self.scope_stack[-1]

    def declare_variable(self, var_name, var_type):
        scope = self.current_scope()
        if var_name in scope:
            self.error(f"Variable '{var_name}' already declared in this scope.")
        scope[var_name] = var_type

    def find_variable(self, var_name):
        for scope in reversed(self.scope_stack):
            if var_name in scope:
                return scope[var_name]
        self.error(f"Variable '{var_name}' not declared in any accessible scope.")
        return None

    def checkVarDeclared(self, identifier):
        # Check if the variable has been declared in the current scope
        scope = self.current_scope()
        if identifier in scope:
            self.error(f"Variable '{identifier}' has already been declared in the current scope.")

    def checkVarUse(self, identifier):
        # Check if the variable has been declared in any accessible scope
        if self.find_variable(identifier) is None:
            self.error(f"Variable '{identifier}' has not been declared in the current or any enclosing scopes.")

    # return false when types mismatch, otherwise ret true
    def checkTypeMatch(self, expected_type, actual_type):
        # Check if the variable's type matches the expected type
        if expected_type != actual_type:
            self.error(f"Type mismatch: Expected {expected_type}, found {actual_type}")
        return expected_type == actual_type

    # return its type or None if not found
    def getMyType(self, identifier):
        # Retrieve the type of the variable if declared
        return self.find_variable(identifier)

    def parse_program(self):
        statements = []
        while self.current_token.type != 'EOF':
            statements.append(self.parse_statement())
            if self.current_token.type == 'DELIMITER':
                self.eat('DELIMITER')
        return ProgramNode(statements)

    def parse_statement(self):
        if self.current_token.type == 'KEYWORD':
            return self.parse_declaration()
        elif self.current_token.type == 'IDENTIFIER':
            return self.parse_assignment()
        elif self.current_token.value == 'if':
            return self.parse_if_statement()
        elif self.current_token.value == 'while':
            return self.parse_while_loop()
        else:
            self.error(f"Unexpected token {self.current_token.value}")

    def parse_declaration(self):
        var_type = self.current_token.value
        self.eat('KEYWORD')
        var_name = self.current_token.value
        self.eat('IDENTIFIER')
        self.eat('OPERATOR', '=')
        expr = self.parse_expression()
        # Variable declaration type checking
        expr_type = self.getMyType(expr.identifier) if isinstance(expr, FactorNode) else None
        self.checkTypeMatch(var_type, expr_type)
        self.declare_variable(var_name, var_type)
        return DeclarationNode(var_name, expr, var_type)

    def parse_assignment(self):
        var_name = self.current_token.value
        self.eat('IDENTIFIER')
        self.eat('OPERATOR', '=')
        expr = self.parse_expression()
        var_type = self.getMyType(var_name)
        expr_type = self.getMyType(expr.identifier) if isinstance(expr, FactorNode) else None
        self.checkTypeMatch(var_type, expr_type)
        return AssignmentNode(var_name, expr)
 
    def parse_if_statement(self):
        self.eat('KEYWORD')  # eat 'if'
        condition = self.parse_condition()
        self.eat('PARENTHESIS', '{')  # eat '{'
        self.enter_scope()
        if_block = []
        while self.current_token.type != 'PARENTHESIS' or self.current_token.value != '}':
            if_block.append(self.parse_statement())
        self.eat('PARENTHESIS', '}')  # eat '}'
        self.leave_scope()
        
        else_block = []
        if self.current_token.type == 'KEYWORD' and self.current_token.value == 'else':
            self.eat('KEYWORD')  # eat 'else'
            self.eat('PARENTHESIS', '{')  # eat '{'
            self.enter_scope()
            while self.current_token.type != 'PARENTHESIS' or self.current_token.value != '}':
                else_block.append(self.parse_statement())
            self.eat('PARENTHESIS', '}')  # eat '}'
            self.leave_scope()
        
        return IfStatementNode(condition, if_block, else_block if else_block else None)

    def parse_while_loop(self):
        self.eat('KEYWORD')  # eat 'while'
        condition = self.parse_condition()
        self.eat('PARENTHESIS', '{')  # eat '{'
        self.enter_scope()
        loop_block = []
        while self.current_token.type != 'PARENTHESIS' or self.current_token.value != '}':
            loop_block.append(self.parse_statement())
        self.eat('PARENTHESIS', '}')  # eat '}'
        self.leave_scope()
        
        return WhileLoopNode(condition, loop_block)
    
    # No need to check type mismatch here.
    def parse_condition(self):
        left = self.parse_expression()
        operator = self.current_token.value
        self.eat('OPERATOR')  # eat the relational operator
        right = self.parse_expression()
        
        return ConditionNode(left, operator, right)

    def parse_arithmetic_expression(self):
        return self.parse_expression()

    def parse_expression(self):
        node = self.parse_term()
        while self.current_token.type == 'OPERATOR' and self.current_token.value in ('+', '-'):
            token = self.current_token
            self.eat('OPERATOR')
            node = ArithmeticExpressionNode(token.value, node, self.parse_term(), None)  # Type management needed
        return node

    def parse_term(self):
        node = self.parse_factor()
        while self.current_token.type == 'OPERATOR' and self.current_token.value in ('*', '/'):
            token = self.current_token
            self.eat('OPERATOR')
            node = TermNode(token.value, node, self.parse_factor(), None)  # Type management needed
        return node

    def parse_factor(self):
        if self.current_token.type == 'NUMBER' or self.current_token.type == 'FNUMBER':
            token = self.current_token
            self.eat(token.type)
            return FactorNode(token.value, 'int' if token.type == 'NUMBER' else 'float')
        elif self.current_token.type == 'IDENTIFIER':
            token = self.current_token
            var_type = self.getMyType(token.value)
            self.eat('IDENTIFIER')
            return FactorNode(token.value, var_type)
        elif self.current_token.type == 'PARENTHESIS' and self.current_token.value == '(':
            self.eat('PARENTHESIS', '(')
            node = self.parse_expression()
            self.eat('PARENTHESIS', ')')
            return node
        else:
            self.error("Invalid syntax in factor")
            return None

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
        num_str = ''
        is_float = False

        while self.current_char is not None and (self.current_char.isdigit() or self.current_char == '.'):
            if self.current_char == '.':
                if is_float:  # Second dot in number, Error
                    self.error()
                is_float = True
            num_str += self.current_char
            self.advance()

        if is_float:
            return (float(num_str), 1)  # Returning tuple with FLOAT flag
        else:
            return (int(num_str), 0)  # Returning tuple with INT flag

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

    #implement
    def keyword_or_identifier(self):
        ident = self.identifier()
        keywords = {'if', 'then', 'else', 'while', 'do', 'int', 'float'}
        if ident in keywords:
            return Token(ident.upper(), ident)  # Return as uppercase token type for keywords
        else:
            return Token('VARIABLE', ident)

    #implement
    def operator(self):
        op_char = self.current_char
        if op_char in ('+', '-', '*', '/', '=', '>', '<', '!', '==', '!=', '<=', '>='):
            self.advance()
            # Handling two-character operators
            if op_char == '=' and self.current_char == '=':
                self.advance()
                return Token('OPERATOR', '==')
            elif op_char == '!' and self.current_char == '=':
                self.advance()
                return Token('OPERATOR', '!=')
            elif op_char == '<' and self.current_char == '=':
                self.advance()
                return Token('OPERATOR', '<=')
            elif op_char == '>' and self.current_char == '=':
                self.advance()
                return Token('OPERATOR', '>=')
            else:  # Single-character operators
                return Token('OPERATOR', op_char)
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
        self.scopes = [{}]  # Initializing with a global scope
       
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

    def eat(self, token_type):
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error(f'Expected token of type {token_type}, but found {self.current_token.type}')

    # enter the new scope in the program
    def enter_scope(self, scope_prefix):
        self.scopes.append({})
        
    # leave the current scope
    def leave_scope(self):
        if len(self.scopes) > 1:
            self.scopes.pop()
        else:
            self.error("Error- Trying to leave global scope.")
        
    # return the current scope
    def current_scope(self):
        return self.scopes[-1]

    def checkVarDeclared(self, identifier):
        for scope in reversed(self.scopes):
            if identifier in scope:
                return True
        self.error(f"Variable '{identifier}' is not declared.")
        return False

    def declare_variable(self, identifier, var_type):
        current_scope = self.scopes[-1]
        if identifier in current_scope:
            self.error(f"Variable '{identifier}' is already declared in the current scope.")
        else:
            current_scope[identifier] = var_type

    #def checkVarDeclared(self, identifier):
        
    #    if #implement :
    #        self.error(f'Variable {identifier} has already been declared in the current scope')

    def checkVarUse(self, identifier):
        # check var declared, so we can use it.
        if not self.checkVarDeclared(identifier):
            self.error(f"Variable '{identifier}' used before declaration.")

    # return false when types mismatch, otherwise ret true
    def checkTypeMatch(self, vType, eType, var, exp):
        # This method checks for type consistency between two variables or expressions.
        if vType != eType:
            self.error(f'Type mismatch between {vType} and {eType} in {var} = {exp}.')
            return False
        return True

    # return its type or None if not found
    def getMyType(self, identifier):
        # This method retrieves the type of a variable if it has been declared.
        for scope in reversed(self.scopes):
            if identifier in scope:
                return scope[identifier]
        self.error(f'Variable {identifier} is not declared.')
        return None
      
    def parse_program(self):
        statements = []
        while self.current_token.type != 'EOF':
            statements.append(self.parse_statement())
            if self.current_token.type == 'DELIMITER':
                self.eat('DELIMITER')
        return ProgramNode(statements)

    def parse_statement(self):
        if self.current_token.type == 'IF':
            return self.parse_if_statement()
        elif self.current_token.type == 'WHILE':
            return self.parse_while_loop()
        elif self.current_token.type == 'TYPE':  # Assuming you have a TYPE token
            return self.parse_declaration()
        elif self.current_token.type == 'VARIABLE':
            return self.parse_assignment()
        else:
            self.error("Error- Unrecognized statement starting with token: " + self.current_token.type)

    def parse_declaration(self):
        self.eat('TYPE')
        type_token = self.current_token
        self.eat('VARIABLE')
        var_name = type_token.value
        self.eat('EQUALS')
        expr = self.parse_arithmetic_expression()
        # Assume type checking and declaration logic here.
        self.declare_variable(var_name, expr.type)
        return DeclarationNode(var_name, expr, expr.type)

    def parse_assignment(self):
        var_name = self.current_token.value
        self.eat('VARIABLE')
        self.eat('EQUALS')
        expr = self.parse_arithmetic_expression()
        if self.getMyType(var_name) != expr.type:
            self.error(f"Type mismatch in assignment to '{var_name}'.")
        return AssignmentNode(var_name, expr)

    def parse_if_statement(self):
        self.eat('IF')
        condition = self.parse_condition()
        self.eat('THEN')
        self.enter_scope('if')
        if_block = self.parse_block()  # Implement parse_block to handle a series of statements
        self.leave_scope()
        else_block = None
        if self.current_token.type == 'ELSE':
            self.eat('ELSE')
            self.enter_scope('else')
            else_block = self.parse_block()
            self.leave_scope()
        return IfStatementNode(condition, if_block, else_block)
 
    def parse_while_loop(self):
        self.eat('WHILE')
        condition = self.parse_condition()
        self.eat('DO')
        self.enter_scope('while')
        loop_block = self.parse_block()
        self.leave_scope()
        return WhileLoopNode(condition, loop_block)
    
    # No need to check type mismatch here.
    def parse_condition(self):
        left = self.parse_arithmetic_expression()
        if self.current_token.type == 'OPERATOR' and self.current_token.value in ('==', '!=', '<', '>', '<=', '>='):
            operator = self.current_token.value
            self.eat('OPERATOR')
            right = self.parse_arithmetic_expression()
            return ConditionNode(left, operator, right)
        else:
            self.error("Error- Expected a comparison operator in the condition.")

    def parse_arithmetic_expression(self):
        node = self.parse_term()
        while self.current_token.type in ('PLUS', 'MINUS'):
            token = self.current_token
            self.eat(token.type)
            right = self.parse_term()
            # Check consistency between the left and right parts of the expression
            if node.type != right.type:
                self.error("Error- Type mismatch in arithmetic expression")
            node = ArithmeticExpressionNode(token.type, node, right, node.type)
        return node

    def parse_term(self):
        node = self.parse_factor()
        while self.current_token.type == 'OPERATOR' and self.current_token.value in ('*', '/'):
            token = self.current_token
            self.eat('OPERATOR')
            if token.value == '/':
                # Division
                node = TermNode('/', node, self.parse_factor(), 'int')  # Adjust types as needed
            else:  # Multiplication
                node = TermNode('*', node, self.parse_factor(), 'int')  # Adjust types as needed
        return node

    def parse_block(self):
        statements = []
        while self.current_token.type not in ('EOF', 'ELSE', 'END'):  
            statements.append(self.parse_statement())
            if self.current_token.type == 'DELIMITER':
                self.eat('DELIMITER')
        return statements  

    def parse_factor(self):
        token = self.current_token
        if token.type == 'NUMBER':
            self.eat('NUMBER')
            return FactorNode(token.value, 'int')  # Assume int for simplicity
        elif token.type == 'VARIABLE':
            if not self.checkVarDeclared(token.value):
                self.error(f"Variable {token.value} not declared.")
            self.eat('VARIABLE')
            var_type = self.getMyType(token.value)
            return FactorNode(token.value, var_type)
        elif token.type == 'PARENTHESIS' and token.value == '(':
            self.eat('PARENTHESIS')
            node = self.parse_arithmetic_expression()
            self.eat('PARENTHESIS')  # Expecting closing parenthesis
            return node

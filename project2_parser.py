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
        print(f"Token created: {self.type}, {self.value}")

class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.current_char = self.text[self.pos] if self.text else None

    def advance(self):
        self.pos += 1
        if self.pos < len(self.text):
            self.current_char = self.text[self.pos]
        else:
            self.current_char = None
        print(f"Advanced to: {self.current_char}")

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def number(self):
        num = ''
        dot_seen = False
        while self.current_char is not None and (self.current_char.isdigit() or self.current_char == '.'):
            if self.current_char == '.':
                if dot_seen:
                    break
                dot_seen = True
            num += self.current_char
            self.advance()
        return Token('FLOAT', float(num)) if dot_seen else Token('INT', int(num))

    def identifier(self):
        result = ''
        while self.current_char is not None and (self.current_char.isalnum() or self.current_char == '_'):
            result += self.current_char
            self.advance()
        keywords = {
            'int': 'TYPE', 'float': 'TYPE',
            'if': 'IF', 'while': 'WHILE', 'then': 'THEN'
        }
        return Token(keywords.get(result, 'IDENTIFIER'), result)

    def get_next_token(self):
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue
            if self.current_char.isdigit() or (self.current_char == '.' and self.peek().isdigit()):
                return self.number()
            if self.current_char.isalpha() or self.current_char == '_':
                return self.identifier()
            if self.current_char in '+-*/();':
                token = Token('OPERATOR', self.current_char)
                self.advance()
                return token
            if self.current_char in '{}':
                token = Token('BLOCK_DELIMITER', self.current_char)
                self.advance()
                return token
            if self.current_char == '=' or self.current_char in ['!', '<', '>']:
                char = self.current_char
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return Token('OPERATOR', char + '=')
                return Token('OPERATOR', char)
            self.error(self.current_char)
        return Token('EOF')

    def peek(self):
        peek_pos = self.pos + 1
        if peek_pos < len(self.text):
            return self.text[peek_pos]
        return None

    def error(self, char):
        raise Exception(f'Invalid character: {char}')

# You can add similar print statements in the Parser to track the flow of parsing
# and see which paths are being taken and what decisions are made at each step.


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
        self.scopes = [{}]  # Stack of scopes, where each scope is a dictionary
        self.messages = []  # To store error messages
        print(f"Initial token: {self.current_token.type}, {self.current_token.value}")

    def enter_scope(self):
        self.scopes.append({})
        print("Entered a new scope.")

    def leave_scope(self):
        self.scopes.pop()
        print("Left a scope.")

    def current_scope(self):
        return self.scopes[-1]

    def check_var_declaration(self, identifier):
        if identifier in self.current_scope():
            self.error(f'Variable {identifier} has already been declared in the current scope')
            return False
        return True

    def declare_var(self, identifier, vtype):
        self.current_scope()[identifier] = vtype
        print(f"Declared {identifier} as {vtype}")

    def find_var(self, identifier):
        for scope in reversed(self.scopes):
            if identifier in scope:
                print(f"Found {identifier} in scope.")
                return scope[identifier]
        self.error(f'Variable {identifier} has not been declared in the current or any enclosing scopes')
        return None

    def check_type(self, expected, actual, message):
        if expected != actual:
            self.error(message)

    def parse_program(self):
        statements = []
        while self.current_token.type != 'EOF':
            statement = self.parse_statement()
            if statement:
                statements.append(statement)
            if self.current_token.type == 'EOF':
                print("Reached EOF.")
                break
        return ProgramNode(statements)

    def parse_statement(self):
        print(f"Parsing statement with token: {self.current_token.type}, {self.current_token.value}")
        if self.current_token.type == 'TYPE':
            return self.parse_declaration()
        elif self.current_token.type == 'IDENTIFIER' and self.peek_token().value == '=':
            return self.parse_assignment()
        elif self.current_token.type == 'IF':
            return self.parse_if_statement()
        elif self.current_token.type == 'WHILE':
            return self.parse_while_loop()
        elif self.current_token.type == 'EOF':
            return None
        else:
            self.error(f"Unexpected token {self.current_token.type} in parse_statement")
            self.current_token = self.lexer.get_next_token()
            if self.current_token.type == 'EOF':
                print("Encountered EOF unexpectedly.")
                return None

    def parse_declaration(self):
        vtype = self.current_token.value
        self.eat('TYPE')
        identifier = self.current_token.value
        if not self.check_var_declaration(identifier):
            return None
        self.eat('IDENTIFIER')
        self.eat('OPERATOR', '=')
        expression = self.parse_arithmetic_expression()
        expression_type = expression.type if expression else 'None'
        self.check_type(vtype, expression_type, f'Type Mismatch between {vtype} and {expression_type}')
        self.declare_var(identifier, vtype)
        return DeclarationNode(identifier, expression, vtype)

    def parse_assignment(self):
        identifier = self.current_token.value
        vtype = self.find_var(identifier)
        if not vtype:
            return None
        self.eat('IDENTIFIER')
        self.eat('OPERATOR', '=')
        expression = self.parse_arithmetic_expression()
        expression_type = expression.type if expression else 'None'
        self.check_type(vtype, expression_type, f'Type Mismatch between {vtype} and {expression_type}')
        return AssignmentNode(identifier, expression)

    def parse_arithmetic_expression(self):
        left_node = self.parse_term()
        while self.current_token.type == 'OPERATOR' and self.current_token.value in ['+', '-']:
            operator = self.current_token.value
            self.eat('OPERATOR')
            right_node = self.parse_term()
            if right_node is None:
                return None
            expr_type = 'float' if left_node.type == 'float' or right_node.type == 'float' else 'int'
            left_node = ArithmeticExpressionNode(operator, left_node, right_node, expr_type)
        return left_node

    def parse_term(self):
        left_node = self.parse_factor()
        while self.current_token.type == 'OPERATOR' and self.current_token.value in ['*', '/']:
            operator = self.current_token.value
            self.eat('OPERATOR')
            right_node = self.parse_factor()
            if right_node is None:
                return None
            term_type = 'float' if left_node.type == 'float' or right_node.type == 'float' else 'int'
            left_node = TermNode(operator, left_node, right_node, term_type)
        return left_node

    def parse_factor(self):
        if self.current_token.type in ['INT', 'FLOAT']:
            value = self.current_token.value
            token_type = self.current_token.type
            self.eat(token_type)
            return FactorNode(value, 'float' if token_type == 'FLOAT' else 'int')
        elif self.current_token.type == 'IDENTIFIER':
            identifier = self.current_token.value
            var_type = self.find_var(identifier)
            if var_type is None:
                self.error(f"Undefined variable {identifier}")
                return None
            self.eat('IDENTIFIER')
            return FactorNode(identifier, var_type)
        elif self.current_token.type == 'OPERATOR' and self.current_token.value == '(':
            self.eat('OPERATOR', '(')
            node = self.parse_arithmetic_expression()
            if self.current_token.value != ')':
                self.error("Missing closing ')'")
                return None
            self.eat('OPERATOR', ')')
            return node
        else:
            self.error(f"Unexpected token {self.current_token.type}")
            return None

    def parse_if_statement(self):
        self.eat('IF')
        condition = self.parse_condition()
        self.eat('THEN')
        self.eat('BLOCK_DELIMITER', '{')  # Ensure entering block
        self.enter_scope()
        if_body = self.parse_statements_until('BLOCK_DELIMITER', '}')
        self.leave_scope()
        self.eat('BLOCK_DELIMITER', '}')
        else_body = []
        if self.current_token.type == 'ELSE':
            self.eat('ELSE')
            self.eat('BLOCK_DELIMITER', '{')
            self.enter_scope()
            else_body = self.parse_statements_until('BLOCK_DELIMITER', '}')
            self.leave_scope()
            self.eat('BLOCK_DELIMITER', '}')
        return IfStatementNode(condition, if_body, else_body)

    def parse_statements_until(self, token_type, token_value):
        statements = []
        while not (self.current_token.type == token_type and self.current_token.value == token_value):
            statement = self.parse_statement()
            if statement:
                statements.append(statement)
            if self.current_token.type == 'EOF':
                break  # Stop parsing if EOF is encountered unexpectedly
        return statements

    def parse_while_loop(self):
        self.eat('WHILE')
        condition = self.parse_condition()
        self.eat('OPERATOR', 'do')
        self.eat('OPERATOR', '{')
        self.enter_scope()
        loop_body = self.parse_statements_until('BLOCK_DELIMITER', '}')
        self.leave_scope()
        self.eat('BLOCK_DELIMITER', '}')
        return WhileLoopNode(condition, loop_body)

    def parse_condition(self):
        left = self.parse_arithmetic_expression()
        operator = self.current_token.value
        self.eat('OPERATOR')
        right = self.parse_arithmetic_expression()
        return ConditionNode(left, operator, right)

    def error(self, message):
        print(f"Error: {message}")  # Log the error to the console for debugging
        self.messages.append(message)

    def eat(self, token_type, token_value=None):
        print(f"Trying to eat: expected {token_type} with value {token_value}, found {self.current_token.type} with value {self.current_token.value}")
        if self.current_token.type == token_type and (token_value is None or self.current_token.value == token_value):
            self.current_token = self.lexer.get_next_token()
            print(f"Token consumed, next token: {self.current_token.type}")
        else:
            expected = f"{token_type} ({token_value})" if token_value else token_type
            self.error(f'Expected {expected}, found {self.current_token.type} ({self.current_token.value})')

    def peek_token(self):
        original_pos = self.lexer.pos
        original_char = self.lexer.current_char
        original_token = self.current_token

        self.current_token = self.lexer.get_next_token()
        peeked_token = self.current_token  

        self.lexer.pos = original_pos
        self.lexer.current_char = original_char
        self.current_token = original_token

        print(f"Peeked at token: {peeked_token.type} with value {peeked_token.value}")
        return peeked_token


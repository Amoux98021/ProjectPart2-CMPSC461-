# If you haven't read the P2 post in campuswire, read it before you continue.

# If you have working lexer of project 1, then you are good to go, you just
# need few modifications in the lexer. I believe you are better off, if you
# just extend it.

# If you don't have a working lexer for project 1, we have provide a skelton of 
# lexer. You need to complete the functions commented with tokenize.

# newline lexer.

import unittest

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
        """Handles the parsing of a number, either float or integer."""
        num = ''
        while self.current_char is not None and self.current_char.isdigit():
            num += self.current_char
            self.advance()

        if self.current_char == '.':
            num += '.'
            self.advance()
            while self.current_char is not None and self.current_char.isdigit():
                num += self.current_char
                self.advance()
            return Token('FLOAT', float(num))

        return Token('INT', int(num))

    # implement
    def identifier(self):
        """Parse identifiers and distinguish them from keywords."""
        result = ''
        while self.current_char is not None and (self.current_char.isalnum() or self.current_char == '_'):
            result += self.current_char
            self.advance()

        # Check if the result is a keyword
        keywords = {'int': 'TYPE', 'float': 'TYPE', 'if': 'IF', 'while': 'WHILE', 'else': 'ELSE'}
        token_type = keywords.get(result, 'IDENTIFIER')
        return Token(token_type, result)

    def get_next_token(self):
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue
            elif self.current_char.isdigit() or (self.current_char == '.' and self.peek().isdigit()):
                return self.number() # Directly return the Token object from number()
            elif self.current_char.isalpha() or self.current_char == '_':
                return self.identifier()
            elif self.current_char in '+-*/':
                return self.operator()
            elif self.current_char in '(){}':
                token = Token('PARENTHESIS' if self.current_char in '()' else 'SCOPE', self.current_char)
                self.advance()
                return token
            elif self.current_char == ';':
                self.advance()  # Simply consume the semicolon and continue
                return Token('SEMICOLON', ';')
            elif self.current_char in ['!', '=', '<', '>']:
                return self.operator()
            else:
                self.error()  # Call error if none of the conditions are met
        return Token('EOF')

    #implement
    def keyword_or_identifier(self):
        result = self.identifier()
        keywords = {
            'int': 'TYPE',
            'float': 'TYPE',
            'if': 'IF',
            'else': 'ELSE',
            'while': 'WHILE',
        }
        token_type = keywords.get(result, 'IDENTIFIER')
        return Token(token_type, result)

    #implement
    def operator(self):
        """Handles the recognition of arithmetic and logical operators."""
        if self.current_char in '+-*/':
            char = self.current_char
            self.advance()
            return Token('OPERATOR', char)
        elif self.current_char == '=':
            self.advance()
            if self.current_char == '=':
                self.advance()
                return Token('OPERATOR', '==')
            return Token('OPERATOR', '=')
        elif self.current_char == '!':
            self.advance()
            if self.current_char == '=':
                self.advance()
                return Token('OPERATOR', '!=')
            self.error('Expected "=" after "!", got "{}" instead'.format(self.current_char))
        elif self.current_char == '<':
            self.advance()
            if self.current_char == '=':
                self.advance()
                return Token('OPERATOR', '<=')
            return Token('OPERATOR', '<')
        elif self.current_char == '>':
            self.advance()
            if self.current_char == '=':
                self.advance()
                return Token('OPERATOR', '>=')
            return Token('OPERATOR', '>')
        else:
            self.error('Unknown operator {}'.format(self.current_char))

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
        self.scopes = [{}]
       
        self.messages = []

    def peek_token(self):
        # Save current state
        original_pos = self.lexer.pos
        original_char = self.lexer.current_char
        original_token = self.current_token
        
        # Get the next token to peek
        self.current_token = self.lexer.get_next_token()
        peeked_token = self.current_token  # Store the peeked token
        
        # Restore lexer to original state
        self.lexer.pos = original_pos
        self.lexer.current_char = original_char
        self.current_token = original_token
        
        return peeked_token

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

    def eat(self, token_type, token_value=None):
        if self.current_token.type == token_type and (token_value is None or self.current_token.value == token_value):
            print(f"Eating {self.current_token.type}, {self.current_token.value}")
            previous_token = self.current_token
            self.current_token = self.lexer.get_next_token()
            if self.current_token == previous_token:  # Check to avoid infinite loops
                self.error("Stuck on token: {} {}".format(token_type, token_value))
                self.current_token = None  # Break out of the cycle
        else:
            expected_description = f"{token_type}{' with value ' + token_value if token_value else ''}"
            found_description = f"{self.current_token.type}{' with value ' + self.current_token.value if self.current_token.value else ''}"
            self.error(f'Expected token of {expected_description}, but found {found_description}')
            print(f"Recovering from error, skipping token: {self.current_token.type}, {self.current_token.value}")
            self.current_token = self.lexer.get_next_token()  # Force advancing the token

    # enter the new scope in the program
    def enter_scope(self, scope_prefix):
        self.scopes.append({})

    # leave the current scope
    def leave_scope(self):
        self.scopes.pop()

    # return the current scope
    def current_scope(self):
        return self.scopes[-1] if self.scopes else None

    def checkVarDeclared(self, identifier):
        """ Check if the variable has already been declared in the current scope """
        if identifier in self.current_scope():
            self.error(f'Variable {identifier} has already been declared in the current scope')

    def checkVarUse(self, identifier):
        """ Check if the variable has been declared in the current or any enclosing scope """
        for scope in reversed(self.scopes):
            if identifier in scope:
                return
        self.error(f'Variable {identifier} has not been declared in the current or any enclosing scopes')

    def checkTypeMatch(self, vType, eType, var, exp):
        """ Check if the types match between the declared variable and the expression in assignments or operations """
        if vType != eType:
            self.error(f'Type mismatch between declared type {vType} and expression type {eType} for variable {var} in expression {exp}')
            return False
        return True

    def getMyType(self, identifier):
        """ Retrieve the type of a declared variable from the current or enclosing scopes """
        for scope in reversed(self.scopes):
            if identifier in scope:
                return scope[identifier]
        return None

    def parse_program(self):
        statements = []
        while self.current_token.type != 'EOF':
            print(f"parse_program - Current token: {self.current_token.type}, {self.current_token.value}")
            if self.current_token.type in ['TYPE', 'IDENTIFIER', 'IF', 'WHILE', '{']:
                statement = self.parse_statement()
                if statement:
                    statements.append(statement)
            else:
                self.error(f"Unexpected token {self.current_token.type} at global scope")
                self.eat(self.current_token.type)
        return ProgramNode(statements)

    def parse_statement(self):
        print(f"Parsing statement, current token: {self.current_token.type}, {self.current_token.value}")
        if self.current_token.type == 'TYPE':  # Type token starts a declaration
            result = self.parse_declaration()
        elif self.current_token.type == 'IDENTIFIER':
            # Peek ahead to see if the next token is an '=' to confirm it's an assignment
            next_token = self.peek_token()
            print(f"Peeked next token for IDENTIFIER: {next_token.type}, {next_token.value}")
            if next_token.type == 'OPERATOR' and next_token.value == '=':
                self.eat('IDENTIFIER')  # Make sure to consume the IDENTIFIER before parsing the assignment
                result = self.parse_assignment()
            else:
                self.error(f"Unexpected syntax after identifier {self.current_token.value}")
                self.eat('IDENTIFIER')  # Consume the IDENTIFIER to prevent infinite loop
                result = None
        elif self.current_token.type == 'IF':  # If token starts an if statement
            result = self.parse_if_statement()
        elif self.current_token.type == 'WHILE':  # While token starts a while loop
            result = self.parse_while_loop()
        elif self.current_token.type == 'SCOPE' and self.current_token.value == '{':
            result = self.parse_block()  # Handling block scopes directly if needed
        else:
            self.error(f"Unexpected token {self.current_token.type} in parse_statement")
            self.eat(self.current_token.type)  # Make sure to consume the unexpected token
            result = None

        # Attempt to consume a semicolon if one should follow the current statement
        if self.current_token.type == 'SEMICOLON':
            self.eat('SEMICOLON')

        return result

    def parse_declaration(self):
        print("Starting parse_declaration")
        var_type = self.current_token.value
        self.eat('TYPE')
        identifier = self.current_token.value
        self.eat('IDENTIFIER')
        self.eat('OPERATOR', '=')
        print(f"Declaration of {identifier} of type {var_type}")

        expression = self.parse_arithmetic_expression()
        if expression is None:
            self.error("Failed to parse the expression in declaration.")
            return None

        if identifier in self.current_scope():
            self.error(f"Variable {identifier} already declared in this scope")
        else:
            self.current_scope()[identifier] = var_type  # Add to symbol table

        if expression.type != var_type:
            self.error(f"Type mismatch: expected {var_type}, got {expression.type}")

        return DeclarationNode(identifier, expression, var_type)

    def parse_assignment(self):
        print("Starting parse_assignment")
        identifier = self.current_token.value
        if not self.getMyType(identifier):  # Ensures variable is declared
            self.error(f"Undeclared variable {identifier}")
            self.advance_to_next_statement()  # Skip to a recovery point
            return None

        self.eat('IDENTIFIER')
        if self.current_token.type == 'OPERATOR' and self.current_token.value == '=':
            self.eat('OPERATOR', '=')
        else:
            self.error("Expected '=' after identifier in assignment")
            self.advance_to_next_statement()
            return None

        expression = self.parse_expression()  # Adjust to use parse_expression which includes logic and comparison
        if expression is None:
            self.error("Failed to parse the expression in assignment.")
            return None

        var_type = self.getMyType(identifier)
        if var_type != expression.type:
            self.error(f"Type mismatch in assignment to {identifier}: expected {var_type}, got {expression.type}")
        
        return AssignmentNode(identifier, expression)

    def advance_to_next_statement(self):
        print("Attempting to recover from an error...")
        while self.current_token and self.current_token.type not in ['SEMICOLON', 'EOF', 'SCOPE']:
            self.current_token = self.lexer.get_next_token()
        if self.current_token.type == 'SEMICOLON':
            self.eat('SEMICOLON')

    def parse_factor(self):
        print(f"Starting parse_factor with token: {self.current_token.type}, {self.current_token.value}")
        if self.current_token.type == 'NUMBER' or self.current_token.type == 'INT':
            value = self.current_token.value
            self.eat(self.current_token.type)  # This should consume either NUMBER or INT
            return FactorNode(value, 'int' if isinstance(value, int) else 'float')
        elif self.current_token.type == 'IDENTIFIER':
            identifier = self.current_token.value
            if not self.getMyType(identifier):
                self.error(f"Use of undeclared variable {identifier}")
                return None
            self.eat('IDENTIFIER')
            return FactorNode(identifier, self.getMyType(identifier))
        else:
            self.error(f"Invalid factor, expected number or identifier, got: {self.current_token.type}")
            return None


    def parse_term(self):
        node = self.parse_factor()
        if node is None:
            print("Failed to parse initial factor in term")
            return None

        while self.current_token.type == 'OPERATOR' and self.current_token.value in ['*', '/']:
            operator = self.current_token.value
            self.eat('OPERATOR')
            right_node = self.parse_factor()
            if right_node is None:
                print("Failed to parse subsequent factor in term")
                return None

            node = TermNode(operator, node, right_node, node.type)
        return node

    def parse_arithmetic_expression(self):
        node = self.parse_term()
        if node is None:
            print("Failed to parse initial term in arithmetic expression")
            return None

        while self.current_token.type == 'OPERATOR' and self.current_token.value in ['+', '-']:
            operator = self.current_token.value
            self.eat('OPERATOR')
            right_node = self.parse_term()
            if right_node is None:
                print("Failed to parse subsequent term in arithmetic expression")
                return None

            node = ArithmeticExpressionNode(operator, node, right_node, node.type)
        return node

    def parse_expression(self):
        """
        Parses expressions including logical and comparison operators.
        """
        node = self.parse_arithmetic_expression()  # Start with the highest precedence: arithmetic
        if node is None:
            print("Failed to parse initial arithmetic expression in logical expression")
            return None

        while self.current_token.type == 'OPERATOR' and self.current_token.value in ['&&', '||', '==', '!=', '<', '>', '<=', '>=']:
            operator = self.current_token.value
            self.eat('OPERATOR')
            right_node = self.parse_arithmetic_expression()  # Parse the right-hand side as an arithmetic expression
            if right_node is None:
                print("Failed to parse subsequent arithmetic expression in logical expression")
                return None

            # Here, type checks and compatibility might need to be enforced depending on language specs
            node = ConditionNode(left=node, operator=operator, right=right_node)  # Adjust node creation for logical ops

        return node

    def parse_if_statement(self):
        print("Starting parse_if_statement")
        self.eat('IF')
        
        # Assuming the condition is wrapped in parentheses
        self.eat('PARENTHESIS', '(')
        condition = self.parse_expression()  # Updated to handle logical conditions
        if condition is None:
            self.error("Failed to parse the condition of the if statement.")
            return None
        self.eat('PARENTHESIS', ')')
        
        # Parse the 'then' block
        self.eat('SCOPE', '{')
        if_block = []
        while not (self.current_token.type == 'SCOPE' and self.current_token.value == '}'):
            stmt = self.parse_statement()
            if stmt is None:
                self.error("Failed to parse a statement in the 'then' block of the if statement.")
                continue  # Skip to the next statement or end of block
            if_block.append(stmt)
        self.eat('SCOPE', '}')  # End of 'then' block
        
        # Optionally parse the 'else' block
        else_block = []
        if self.current_token.type == 'ELSE':
            self.eat('ELSE')
            self.eat('SCOPE', '{')
            while not (self.current_token.type == 'SCOPE' and self.current_token.value == '}'):
                stmt = self.parse_statement()
                if stmt is None:
                    self.error("Failed to parse a statement in the 'else' block of the if statement.")
                    continue  # Skip to the next statement or end of block
                else_block.append(stmt)
            self.eat('SCOPE', '}')  # End of 'else' block

        return IfStatementNode(condition, if_block, else_block)

    def parse_while_loop(self):
        print("Starting parse_while_loop")
        self.eat('WHILE')  # Consumes the 'WHILE' token
        
        # Assuming the condition is wrapped in parentheses
        self.eat('PARENTHESIS', '(')
        condition = self.parse_expression()  # Use parse_expression to handle complex conditions
        if condition is None:
            self.error("Failed to parse the condition of the while loop.")
            return None
        self.eat('PARENTHESIS', ')')
        
        # Parse the loop block
        self.eat('SCOPE', '{')
        loop_block = []
        while not (self.current_token.type == 'SCOPE' and self.current_token.value == '}'):
            stmt = self.parse_statement()
            if stmt is None:
                self.error("Failed to parse a statement in the loop block of the while statement.")
                continue  # Skip to the next statement or end of block
            loop_block.append(stmt)
        self.eat('SCOPE', '}')  # End of loop block
        
        return WhileLoopNode(condition, loop_block)

    def parse_condition(self):
        print("Starting parse_condition")
        left = self.parse_expression()  # Parse the left side of the condition
        if left is None:
            self.error("Failed to parse the left side of the condition.")
            return None

        operator = self.current_token.value  # Should be a logical or comparison operator
        if self.current_token.type != 'OPERATOR':
            self.error(f"Expected an operator in condition, found: {self.current_token.type}")
            return None
        self.eat('OPERATOR')
        
        right = self.parse_expression()  # Parse the right side of the condition
        if right is None:
            self.error("Failed to parse the right side of the condition.")
            return None
        
        return ConditionNode(left, operator, right)


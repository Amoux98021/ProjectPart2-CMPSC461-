# If you haven't read the P2 post in campuswire, read it before you continue.

# If you have working lexer of project 1, then you are good to go, you just
# need few modifications in the lexer. I believe you are better off, if you
# just extend it.

# If you don't have a working lexer for project 1, we have provide a skelton of 
# lexer. You need to complete the functions commented with tokenize.

# newline lexer.
class Token:
    def __init__(self, type, value=None):
        self.type = type
        self.value = value

    def __repr__(self):
        return f"Token({self.type}, {repr(self.value)})"

class Lexer:
    def __init__(self, code):
        self.code = code
        self.position = 0
        self.current_char = self.code[self.position] if self.code else None

    def advance(self):
        self.position += 1
        if self.position >= len(self.code):
            self.current_char = None
        else:
            self.current_char = self.code[self.position]

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def number(self):
        result = ''
        is_float = False
        while self.current_char is not None and (self.current_char.isdigit() or self.current_char == '.'):
            if self.current_char == '.':
                if is_float:  # Second '.' in a number
                    break
                is_float = True
            result += self.current_char
            self.advance()
        return float(result) if is_float else int(result)

    def identifier(self):
        result = ''
        while self.current_char is not None and (self.current_char.isalnum() or self.current_char == '_'):
            result += self.current_char
            self.advance()
        return result

    def get_token(self):
        self.skip_whitespace()

        if self.current_char is None:
            return Token('EOI', None)

        if self.current_char.isdigit() or self.current_char == '.':
            return Token('NUMBER', self.number())

        if self.current_char.isalpha():
            ident = self.identifier()
            if ident in ('int', 'float'):
                return Token('TYPE', ident)
            return Token('VARIABLE', ident)

        if self.current_char in ('+', '-', '*', '/', '(', ')', '=', '>', '<', '!', ';', '{', '}'):
            char = self.current_char
            self.advance()
            if self.current_char in ('=', '!', '>', '<') and char in ('=', '!', '>', '<'):  # for operators like '==', '!=', '<=', '>=', '!='
                char += self.current_char
                self.advance()
            return Token('OPERATOR', char)

        raise Exception(f"Unrecognized character {self.current_char}")

    def peek(self):
        peek_pos = self.position + 1
        if peek_pos >= len(self.code):
            return None
        return self.code[peek_pos]


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
        self.current_token = self.lexer.get_token()
        self.symbol_table_stack = [{}]
        self.messages = []

    def error(self, message_type, detail):
        full_message = f"{message_type}: {detail}"
        self.messages.append(full_message)

    def eat(self, token_type, token_value=None):
        if self.current_token.type == token_type and (token_value is None or self.current_token.value == token_value):
            self.current_token = self.lexer.get_token()
        else:
            expected = f"{token_type} with value {token_value}" if token_value else f"{token_type}"
            found = f"{self.current_token.type} with value {self.current_token.value}"
            self.error("Syntax Error", f'Expected token {expected}, but found {found}')

    def enter_scope(self):
        self.symbol_table_stack.append({})

    def leave_scope(self):
        self.symbol_table_stack.pop()

    def current_scope(self):
        return self.symbol_table_stack[-1]

    def check_var_declared(self, identifier):
        if identifier in self.current_scope():
            self.error("Declaration Error", f'Variable {identifier} has already been declared in the current scope')

    def check_var_use(self, identifier):
        if not any(identifier in scope for scope in self.symbol_table_stack):
            self.error("Declaration Error", f'Variable {identifier} has not been declared in the current or any enclosing scopes')

    def check_type_match(self, declared_type, actual_type, identifier):
        if declared_type != actual_type:
            self.error("Type Mismatch", f'expected {declared_type}, got {actual_type} for variable {identifier}')

    def parse_program(self):
        statements = []
        while self.current_token.type != 'EOF':
            statements.append(self.parse_statement())
        return ProgramNode(statements)

    def parse_statement(self):
        if self.current_token.type == 'TYPE':
            return self.parse_declaration()
        elif self.current_token.type == 'VARIABLE':
            return self.parse_assignment()
        elif self.current_token.type == 'IF':
            return self.parse_if_statement()
        elif self.current_token.type == 'WHILE':
            return self.parse_while_loop()
        else:
            self.error("Syntax Error", f"Unexpected token {self.current_token.type}")
            return None

    def parse_declaration(self):
        declared_type = self.current_token.value
        self.eat('TYPE')
        var_name = self.current_token.value
        self.eat('VARIABLE')
        self.eat('OPERATOR', '=')
        expr = self.parse_arithmetic_expression()
        self.check_var_declared(var_name)
        self.current_scope()[var_name] = declared_type
        return DeclarationNode(var_name, expr, declared_type)

    def parse_assignment(self):
        var_name = self.current_token.value
        self.check_var_use(var_name)
        self.eat('VARIABLE')
        self.eat('OPERATOR', '=')
        expr = self.parse_arithmetic_expression()
        var_type = self.current_scope().get(var_name)
        expr_type = expr.type if hasattr(expr, 'type') else None
        if var_type != expr_type:
            self.error("Type Mismatch", f"cannot assign {expr_type} to {var_type} in variable '{var_name}'")
        return AssignmentNode(var_name, expr)

    def parse_if_statement(self):
        self.eat('IF')
        condition = self.parse_condition()
        self.eat('OPERATOR', '{')
        self.enter_scope()
        if_block = []
        while self.current_token.type != 'OPERATOR' or self.current_token.value != '}':
            if_block.append(self.parse_statement())
        self.eat('OPERATOR', '}')
        self.leave_scope()

        else_block = []
        if self.current_token.type == 'ELSE':
            self.eat('ELSE')
            self.eat('OPERATOR', '{')
            self.enter_scope()
            while self.current_token.type != 'OPERATOR' or self.current_token.value != '}':
                else_block.append(self.parse_statement())
            self.eat('OPERATOR', '}')
            self.leave_scope()

        return IfStatementNode(condition, if_block, else_block)

    def parse_while_loop(self):
        self.eat('WHILE')
        condition = self.parse_condition()
        self.eat('OPERATOR', '{')
        self.enter_scope()
        loop_block = []
        while self.current_token.type != 'OPERATOR' or self.current_token.value != '}':
            loop_block.append(self.parse_statement())
        self.eat('OPERATOR', '}')
        self.leave_scope()
        return WhileLoopNode(condition, loop_block)

    def parse_condition(self):
        left = self.parse_arithmetic_expression()
        operator = self.current_token.value
        self.eat('OPERATOR')  # Eat the comparison operator
        right = self.parse_arithmetic_expression()
        return ConditionNode(left, operator, right)

    def parse_arithmetic_expression(self):
        left = self.parse_term()
        while self.current_token.value in ('+', '-'):
            operator = self.current_token.value
            self.eat('OPERATOR')
            right = self.parse_term()
            left = ArithmeticExpressionNode(operator, left, right, left.type)  # Assuming left and right have the same type
        return left

    def parse_term(self):
        left = self.parse_factor()
        while self.current_token.value in ('*', '/'):
            operator = self.current_token.value
            self.eat('OPERATOR')
            right = self.parse_factor()
            left = TermNode(operator, left, right, left.type)  # Assuming left and right have the same type
        return left

    def parse_factor(self):
        if self.current_token.type == 'NUMBER':
            value = self.current_token.value
            type_name = 'float' if '.' in str(value) else 'int'
            self.eat('NUMBER')
            return FactorNode(value, type_name)
        elif self.current_token.type == 'VARIABLE':
            var_name = self.current_token.value
            self.check_var_use(var_name)
            var_type = self.current_scope().get(var_name, None)
            self.eat('VARIABLE')
            return FactorNode(var_name, var_type)
        else:
            self.error("Syntax Error", "Expected a number or variable")
            return None

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

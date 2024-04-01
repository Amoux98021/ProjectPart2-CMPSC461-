import project2_parser as p2


'''
We check the following rules for project 2.

Rules:
1. No expression in the language should involve variables
or expressions of different types.

2. You must define the variable before its use. You have
to define in the same scope or any enclosing scopes.

3. You must not define two variables with same name within
the scope.

You must emit the messages in the same order as you do the
parsing.


1:    int a = 10
2:    int b = 10.2
3:    if a > 10 then {
4:      int a = a * a
5:      int b = 10
6:      int a = a * b
    }

Violations of scoping rules and type mismatch in the
above program.

In line 2 variable b of type int is assigned to float.
In line 6 variable a is declared twice in the same scope.
'''


def test_parser(code, expected_messages):
    lexer = p2.Lexer(code)
    parser = p2.Parser(lexer)
    parse_tree = parser.parse_program()


    #print(parser.messages)
    if parser.messages == expected_messages:
      return 0;
    return 1;


#testcase 1
def test1():
    text1 = '''
    int a = 10
    int b = 10.2
    '''
    correctMessages = ['Type Mismatch between int and float']
    if test_parser(text1, correctMessages) == 0:
      return 1;
    return 0;

def test2():
    #testcase 2
    text2 = '''
    int a = 10
    float b = 10.2
    if a > 10 then {
      int a = a * a
      int b = 10
      int a = a * b
    }
    '''
    correctMessages = ['Variable a has already been declared in the current scope']
    if test_parser(text2, correctMessages) == 0:
      return 1;
    return 0;

def test3():
      #testcase 3
    text3 = '''
    int a = 10
    float b = 10.2
    if a > 10 then {
      float a = 10.2
      int b = 10
    } else {
      a = 10
      b = a * 10.56778
    }
    '''
    correctMessages = ['Type Mismatch between int and float', 'Type Mismatch between float and None']
    if test_parser(text3, correctMessages) == 0:
      return 1;
    return 0;

def test4():
    #testcase 4
    text4 = '''
    while x > 0 do {
      int x = 10
      int y = x
    }
    int c = y
    '''

    correctMessages = ['Variable x has not been declared in the current or any enclosing scopes',
                      'Variable y has not been declared in the current or any enclosing scopes',
                      'Type Mismatch between int and None']

    if test_parser(text4, correctMessages) == 0:
      return 1;
    return 0;

def test5():
    #testcase 5
    text5 = '''
    float x = 0.234
    int y = 0
    int z = 0
    if x > y then {
      float sum = 10.50
      int cnt   = 20
      if cnt > 0 then {
        int x = 1
        sum   = 2.0 * sum
        x     = x + 1
      }
      x = 10
      while x > 1.9 do {
        z = z + y
        x = x - 1.0
        sum = x + sum
      }
    }
    '''
    correctMessages = ['Type Mismatch between float and int']
    if test_parser(text5, correctMessages) == 0:
      return 1;
    return 0;

def test6():
    # testcase 6
    text6 = '''
    int a = 10
    float b = 10.2
    if a > 10 then {
      int a = c
      int c = a
      int a = c * b
    } else {
      while a > 10 do {
        a = a - 12.456
        b = b + 1.0
      }
    }
    '''
    correctMessages = ['Variable c has not been declared in the current or any enclosing scopes',
                       'Type Mismatch between int and None',
                       'Variable a has already been declared in the current scope',
                       'Type Mismatch between int and float',
                       'Type Mismatch between int and None',
                       'Type Mismatch between int and float',
                       'Type Mismatch between int and None']

    if test_parser(text6, correctMessages) == 0:
      return 1;
    return 0;

def test7():
    # testcase 7
    text7 = '''
      int a = 10
      int b = 12
      float z = 10.2

      if a + b > 10 then {
          int zoo = 10
          if a > 10  then {
            int zoo = 12
            float a = 10.2
            zoo = 10

          }
          zoo = a
      }
      else {
          float c = 20.3
          c = 10.2
      }
      while a > 10 do {
          int y = 100
          y = y + y

      }
    '''

 
    correctMessages = []
    if test_parser(text7, correctMessages) == 0:
      return 1;
    return 0;

#print(passed)
passed = 0
passed += test1()
passed += test2()
passed += test3()
passed += test4()
passed += test5()
passed += test6()
passed += test7()
print(passed)
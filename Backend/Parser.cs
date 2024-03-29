/*
Boa is the reference implementation for a language, also called Boa,
which is similar to python. This implementation is both interpreted
and compiled, targetting the Microsoft .NET Framework.

http://www.adammil.net/
Copyright (C) 2004-2006 Adam Milazzo

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/

using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Text;
using Scripting;
using Scripting.Backend;

namespace Boa.Backend
{

#region Tokens
enum Token
{ None,

  // punctuation
  Period, Comma, BackQuote, LParen, RParen, LBrace, RBrace, LBracket, RBracket, Question, Colon,
  Semicolon, Percent, Plus, Minus, Asterisk, Slash,
  
  // punctuation operators
  Power, FloorDivide, LeftShift, RightShift,
  BitAnd, BitOr, BitNot, BitXor, LogAnd, LogOr, LogNot,
  
  // keywords
  Def, Print, Return, While, If, Elif, Else, Pass, Break, Continue, Global, Import, From, For, In, Not,
  Lambda, Try, Except, Finally, Raise, Class, Assert, Is, Del, Yield, Lock, Using,
  
  // abstract
  Identifier, Literal, Assign, Compare, StaticMember, EOL, EOF
}
#endregion

#region Parser
public sealed class Parser
{ public Parser(string source, TextReader data)
  { sourceFile=source; this.data=data.ReadToEnd();
    NextToken();
  }
  public Parser(string source, string data)
  { sourceFile=source; this.data=data;
    NextToken();
  }

  static Parser()
  { stringTokens = new SortedList();
    Token[] tokens =
    { Token.Def, Token.Print, Token.Return, Token.While, Token.Import, Token.From,
      Token.For, Token.If,  Token.Elif, Token.Else, Token.Pass, Token.Break, Token.Continue, Token.Global, Token.In,
      Token.Lambda, Token.Try, Token.Except, Token.Finally, Token.Raise, Token.Class, Token.Assert, Token.Is,
      Token.Del, Token.Yield, Token.Not, Token.Lock, Token.Using,
    };
    foreach(Token token in tokens) stringTokens.Add(Enum.GetName(typeof(Token), token).ToLower(), token);
    stringTokens.Add("and", Token.LogAnd);
    stringTokens.Add("or",  Token.LogOr);
  }

  public Node Parse()
  { using(CachedList<Node> stmts = CachedList<Node>.Alloc())
    { while(true)
      { if(TryEat(Token.EOL)) continue;
        if(TryEat(Token.EOF)) break;
        stmts.Add(ParseStatement());
      }
      return stmts.Count==0 ? new PassNode() : (Node)new BodyNode(stmts.ToArray());
    }
  }

  // expression := <ternary> | <lambda> (',' <expression>)?
  public Node ParseExpression()
  { if(token==Token.Lambda) return ParseLambda();
    Node expr = ParseTernary();
    if(bareTuples && token==Token.Comma)
      using(CachedList<Node> exprs = CachedList<Node>.Alloc())
      { exprs.Add(expr);
        bareTuples = false;
        while(TryEat(Token.Comma) && token!=Token.EOL && token!=Token.Assign) exprs.Add(ParseExpression());
        bareTuples = true;
        expr = new TupleNode(exprs.ToArray());
      }
    return expr;
  }

  // statement     := <stmt_line> | <compound_stmt>
  // compount_stmt := <if_stmt> | <while_stmt> | <for_stmt> | <def_stmt> | <try_stmt> | <global_stmt> |
  //                  <import_stmt> | <class_stmt> | <label_stmt> | <lock_stmt> | <using_stmt>
  // label_stmt    := <identitier> ':' <suite>
  public Node ParseStatement()
  { switch(token)
    { case Token.If:     return ParseIf();
      case Token.While:  return ParseWhile();
      case Token.For:    return ParseFor();
      case Token.Def:    return ParseDef();
      case Token.Try:    return ParseTry();
      case Token.Global: return ParseGlobal();
      case Token.Class:  return ParseClass();
      case Token.Import: case Token.From: return ParseImport();
      case Token.Lock: case Token.Using: return ParseUsingBlock(token);
      default:
        if(token==Token.Identifier)
        { string label = (string)value;
          if(PeekToken()==Token.Colon)
          { NextToken();
            return new BlockNode(label, ParseSuite());
          } 
        }
        return ParseStmtLine();
    }
  }

  bool InFunction { get { return funcDepth>0; } }
  bool InLoop { get { return loopDepth>0; } }

  #region GetEscapeChar
  /*
  \newline  Ignored
  \\        Backslash
  \"        Double quotation mark
  \'        Single quotation mark
  \n        Newline
  \t        Tab
  \r        Carriage return
  \b        Backspace
  \e        Escape
  \a        Bell
  \f        Form feed
  \v        Vertical tab
  \xHH      Up to 2 hex digits -> byte value
  \uHHHH    Up to 4 hex digits -> 16-bit unicode value
  \cC       Control code (eg, \cC is ctrl-c)
  \OOO      Up to 3 octal digits -> byte value
  */
  char GetEscapeChar()
  { char c = ReadChar();
    if(char.IsDigit(c))
    { if(c>'7') SyntaxError("invalid octal digit");
      int num = (c-'0');
      for(int i=1; i<3; i++)
      { c = ReadChar();
        if(!char.IsDigit(c) || c>'7') { lastChar=c; break; }
        num = (num<<3) | (c-'0');
      }
      return (char)num;
    }
    else switch(c)
    { case '\"': return '\"';
      case '\'': return '\'';
      case 'n':  return '\n';
      case 't':  return '\t';
      case 'r':  return '\r';
      case 'b':  return '\b';
      case 'e':  return (char)27;
      case 'a':  return '\a';
      case 'f':  return '\f';
      case 'v':  return '\v';
      case '\\': return '\\';
      case 'x': case 'u':
      { int num = 0;
        for(int i=0,limit=(c=='x'?2:4); i<limit; i++)
        { c = ReadChar();
          if(char.IsDigit(c)) num = (num<<4) | (c-'0');
          else if((c<'A' || c>'F') && (c<'a' || c>'f'))
          { if(i==0) SyntaxError("expected hex digit");
            lastChar = c;
            break;
          }
          else num = (num<<4) | (char.ToUpper(c)-'A'+10);
        }
        return (char)num;
      }
      case 'c':
        c = ReadChar();
        if(!char.IsLetter(c)) SyntaxError("expected letter");
        return (char)(char.ToUpper(c)-64);
      case '\n': return '\0';
      default: SyntaxError(string.Format("unknown escape character '{0}'", c)); return c;
    }
  }
  #endregion

  void Eat(Token type) { if(token!=type) Unexpected(token, type); NextToken(); }
  void Expect(Token type) { if(token!=type) Unexpected(token); }

  Token NextToken()
  { if(nextToken!=Token.None)
    { token = nextToken;
      value = nextValue;
      nextToken = Token.None;
    }
    else token = ReadToken();
    return token;
  }

  // argument_list := <argument> (',' <argument>)*
  // argument := ('*' | '**')? <expression> | <identifier> '=' <expression>
  Argument[] ParseArguments()
  { bool obt=bareTuples, owe=wantEOL;
    bareTuples = wantEOL = false;

    CachedList<Argument> args = null;
    try
    { Eat(Token.LParen);
      if(token==Token.RParen) return new Argument[0];
      args = CachedList<Argument>.Alloc();
      do
      { if(TryEat(Token.Asterisk)) args.Add(new Argument(ParseExpression(), ArgType.List));
        else if(TryEat(Token.Power)) args.Add(new Argument(ParseExpression(), ArgType.Dict));
        else if(token!=Token.Identifier) args.Add(new Argument(ParseExpression()));
        else
        { Node e = ParseExpression();
          if(TryEat(Token.Assign))
          { if(!(e is VariableNode)) Unexpected(Token.Assign);
            args.Add(new Argument(((VariableNode)e).Name.String, ParseExpression()));
          }
          else args.Add(new Argument(e));
        }
      } while(TryEat(Token.Comma));

      ListDictionary ld = new ListDictionary();
      foreach(Argument a in args)
        if(a.Name!=null && ld.Contains(a.Name)) SyntaxError("duplicate keyword argument '{0}'", a.Name);

      return args.ToArray();
    }
    finally { bareTuples=obt; wantEOL=owe; if(args!=null) args.Dispose(); }
  }

  // bitwise    := <shift> (<bitwise_op> <shift>)*
  // bitwise_op := '&' | '|' | '^'
  Node ParseBitwise()
  { Node expr = ParseShift();
    while(true)
    { BinaryOperator op;
      switch(token)
      { case Token.BitAnd: op = BinaryOperator.BitwiseAnd; break;
        case Token.BitOr:  op = BinaryOperator.BitwiseOr;  break;
        case Token.BitXor: op = BinaryOperator.BitwiseXor; break;
        default: return expr;
      }
      NextToken();
      expr = new OpNode(op, expr, ParseShift());
    }
  }
  
  // cim_expr := <primary> ('(' <argument_list> ')' | '[' <index> ']' | '.' <identifier> | '::' <identifier>)*
  Node ParseCIM()
  { Node expr = ParsePrimary();
    while(true)
    { if(token==Token.LParen)
      { expr = new CallNode(expr, ParseArguments());
        Eat(Token.RParen);
      }
      else if(TryEat(Token.LBracket))
      { Node start = token==Token.Colon ? null : ParseExpression();
        if(TryEat(Token.Colon))
        { Node stop = token==Token.Colon || token==Token.RBracket ? null : ParseExpression();
          Node step = TryEat(Token.Colon) ? ParseExpression() : null;
          start = new SliceNode(start, stop, step);
        }
        Eat(Token.RBracket);
        expr = new IndexNode(expr, start);
      }
      else if(TryEat(Token.StaticMember)) expr = new GetSlotNode(expr, new LiteralNode(ParseIdentifier()));
      else if(TryEat(Token.Period))
      { Node member = new LiteralNode(ParseIdentifier());
        if(token!=Token.LParen) expr = new GetPropertyNode(expr, member);
        else
        { expr = new CallPropertyNode(expr, member, ParseArguments());
          Eat(Token.RParen);
        }
      }
      else return expr;
    }
  }

  // class_stmt  := 'class' <identifier> <inheritance>? <suite>
  // inheritance := '(' <expr_list> ')'
  Node ParseClass()
  { /*Eat(Token.Class);
    string name = ParseIdentifier();
    Node[] bases=null;
    if(TryEat(Token.LParen))
    { if(TryEat(Token.RParen)) bases = new Expression[0];
      else
      { CachedArray inherit = new ArrayList();
        bool obt = bareTuples;
        bareTuples = false;

        do inherit.Add(ParseExpression()); while(TryEat(Token.Comma));
        Eat(Token.RParen);
        bases = (Node[])inherit.ToArray(typeof(Node));

        bareTuples = obt;
      }
    }
    else bases = new Node[0];
    return new ClassStatement(name, bases, ParseSuite());*/
    throw new NotImplementedException();
  }

  // compare    := <isin> (<compare_op> <isin>)*
  // compare_op := '==' | '!=' | '<' | '>' | '<=' | '>=' | '<>' | 'is' | 'is not'
  Node ParseCompare()
  { Node expr = ParseIsIn();
    CachedList<object> comps = null;
    while(true)
    { if(token==Token.Compare || token==Token.Is)
      { if(comps==null)
        { comps = CachedList<object>.Alloc();
          comps.Add(expr);
        }

        if(token==Token.Compare)
        { comps.Add(value);
          NextToken();
        }
        else // token==Token.Is
        { if(comps==null) comps = CachedList<object>.Alloc();
          bool not = NextToken()==Token.Not;
          comps.Add(not ? Operator.NotIdentical : Operator.Identical);
          if(not) NextToken();
        }
        comps.Add(ParseIsIn());
      }
      else break;
    }
    if(comps!=null)
    { if(comps.Count==3) expr = new OpNode((ComparisonOperator)comps[1], (Node)comps[0], (Node)comps[2]);
      else
      { Node[] exprs = new Node[(comps.Count+1)/2];
        ComparisonOperator[] ops = new ComparisonOperator[comps.Count/2];
        exprs[0] = (Node)comps[0];
        for(int i=1,j=0; i<comps.Count; )
        { ops[j] = (ComparisonOperator)comps[i++];
          exprs[++j] = (Node)comps[i++];
        }
        expr = new CompareNode(exprs, ops);
      }
      comps.Dispose();
    }
    return expr;
  }

  // def_stmt := 'def' <identifier> '(' <param_list> ')' ':' <suite>
  Node ParseDef()
  { bool oldYields = hasYield;
    hasYield=false; funcDepth++;
    Eat(Token.Def);
    string name = ParseIdentifier();
    Eat(Token.LParen);
    Parameter[] parms = ParseParamList(Token.RParen);
    Eat(Token.RParen);
    Node body = new BlockNode("*FUNCTION*", ParseSuite());
    Node func = new LambdaNode(name, parms, hasYield ? new GeneratorNode(body) : body);
    hasYield=oldYields; funcDepth--;
    return new SetNode(name, func, SetType.Set);
  }

  // module := <identifier> ('.' <identifier>)*
  string ParseDotted()
  { string ret = ParseIdentifier();
    while(TryEat(Token.Period)) ret = ret + "." + ParseIdentifier();
    return ret;
  }

  // expr_stmt  := (<lvalue> '=')* <expression>
  // assignable := <name> | <index> | <slice>
  // lvalue     := <assignable> | <tuple of <assignable>>
  Node ParseExprStmt()
  { Node lhs = ParseExpression();
    if(token!=Token.Assign) return lhs;

    using(CachedList<Node> list = CachedList<Node>.Alloc())
    { BinaryOperator op = (BinaryOperator)value;

      while(TryEat(Token.Assign))
      { if(op!=null)
        { if(list.Count>0) SyntaxError("can't chain in-place assignment");
          if(!(lhs is VariableNode || lhs is GetSlotBase || lhs is IndexNode))
            SyntaxError("can't do in-place assignment with {0}", lhs.GetType());
        }
        else if(!(lhs is VariableNode || lhs is GetSlotBase || lhs is TupleNode || lhs is IndexNode))
          SyntaxError("can't assign to {0}", lhs.GetType());
        list.Add(lhs);
        lhs = ParseExpression();
      }
      return op==null ? new AssignNode(list.ToArray(), lhs) : new AssignNode(list[0], new OpNode(op, list[0], lhs));
    }
  }

  // factor    := <power> (<factor_op> <power>)*
  // factor_op := '*' | '/' | '%' | '//'
  Node ParseFactor()
  { Node expr = ParsePower();
    while(true)
    { BinaryOperator op;
      switch(token)
      { case Token.Asterisk:    op = BinaryOperator.Multiply; break;
        case Token.Slash:       op = BinaryOperator.Divide; break;
        case Token.Percent:     op = BinaryOperator.Modulus; break;
        case Token.FloorDivide: op = BinaryOperator.FloorDivide; break;
        default: return expr;
      }
      NextToken();
      expr = new OpNode(op, expr, ParsePower());
    }
  }

  // for_stmt := 'for' <namelist> 'in' <expression> <suite>
  Node ParseFor()
  { int indent = this.indent;
    Eat(Token.For);
    Name[] names = ParseNameList();
    Eat(Token.In);
    Position start = tokenStart;
    Node loopExp = SetPos(ParseExpression(), start);
    loopDepth++;
    Node body=ParseSuite(), elze=this.indent==indent && TryEat(Token.Else) ? ParseSuite() : null;
    loopDepth--;
    return new ForNode(names, loopExp, body, elze);
  }

  // global_stmt := 'global' <namelist> EOL
  Node ParseGlobal()
  { Eat(Token.Global);
    using(CachedList<string> names = CachedList<string>.Alloc())
    { do names.Add(ParseIdentifier()); while(TryEat(Token.Comma));
      Eat(Token.EOL);
      return new GlobalNode(names.ToArray());
    }
  }

  string ParseIdentifier()
  { Expect(Token.Identifier);
    string ret = (string)value;
    NextToken();
    return ret;
  }

  // if_stmt := 'if' <expression> <suite> ('elif' <expression> <suite>)* ('else' <suite>)?
  Node ParseIf()
  { if(token!=Token.If && token!=Token.Elif) Unexpected(token);
    int indent=this.indent;
    NextToken();
    Position start = tokenStart;
    Node test = SetPos(ParseExpression(), start);
    Node body = ParseSuite(), elze = null;
    if(this.indent==indent)
    { if(token==Token.Elif) elze = ParseIf();
      else if(TryEat(Token.Else)) elze = ParseSuite();
    }
    return new IfNode(test, body, elze);
  }

  // import_stmt := 'import' <import_package> (',' <import_package>)* EOL |
  //                'from' <dotted> 'import' <import_ident> (',' <import_ident>)* EOL |
  //                'from' <dotted> 'import' '*' EOL
  // import_package := <dotted> ('as' <identifier>)?
  // import_ident   := <identifier> ('as' <identifier>)?
  Node ParseImport()
  { Position start = tokenStart;
    Node stmt;
    if(TryEat(Token.From))
    { string module = ParseDotted();
      Eat(Token.Import);
      if(TryEat(Token.Asterisk))
      { if(InFunction) SyntaxError("'from ... import *' cannot be used within a function");
        stmt = new ImportFromNode(module, null, null, !InFunction);
      }
      else
      { Expect(Token.Identifier);
        using(CachedList<string> names=CachedList<string>.Alloc(), asNames=CachedList<string>.Alloc())
        { do
          { names.Add(ParseIdentifier());
            if(token==Token.Identifier && (string)value=="as")
            { NextToken();
              asNames.Add(ParseIdentifier());
            }
            else asNames.Add(null);
          } while(token!=Token.EOL && TryEat(Token.Comma));
          stmt = new ImportFromNode(module, names.ToArray(), asNames.ToArray(), !InFunction);
        }
      }
    }
    else
    { Eat(Token.Import);
      using(CachedList<string> names=CachedList<string>.Alloc(), asNames=CachedList<string>.Alloc())
      { do
        { names.Add(ParseDotted());
          if(token==Token.Identifier && (string)value=="as")
          { NextToken();
            asNames.Add(ParseIdentifier());
          }
          else asNames.Add(null);
        } while(token!=Token.EOL && TryEat(Token.Comma));
        stmt = new ImportNode(names.ToArray(), asNames.ToArray());
      }
    }
    stmt = SetPos(stmt, start);
    Eat(Token.EOL);
    return stmt;
  }

  // isin := <bitwise> ('not'? 'in' <bitwise>)*
  Node ParseIsIn()
  { Node expr = ParseBitwise();
    while(true)
    { if(TryEat(Token.In)) expr = new OpNode(InOperator.Instance, expr, ParseBitwise());
      else if(TryEat(Token.Not))
      { Eat(Token.In);
        expr = new UnaryOpNode(UnaryOperator.LogicalNot, new OpNode(InOperator.Instance, expr, ParseBitwise()));
      }
      else return expr;
    }
  }

  // TODO: detecting the end of the lambda is currently too inelegant. fix that.
  // lambda := 'lambda' <namelist> ':' <lambda_body>
  // lambda_body := <simple_stmt> (';' <simple_stmt>)* <lambda_end>
  // lambda_end := EOL | ',' | ')' | ']' | '}' | 'for'
  Node ParseLambda()
  { funcDepth++;
    Eat(Token.Lambda);
    Parameter[] parms = ParseParamList(Token.Colon);
    Eat(Token.Colon);

    using(CachedList<Node> list = CachedList<Node>.Alloc())
    { do
      { switch(token)
        { case Token.EOL: case Token.Comma: case Token.RParen: case Token.RBrace: case Token.RBracket: case Token.For:
            goto done;
          case Token.Return:
            switch(NextToken())
            { case Token.EOL: case Token.Comma: case Token.RParen: case Token.RBrace: case Token.RBracket:
              case Token.For:
                list.Add(new LiteralNode(null));
                goto done;
              default: list.Add(ParseSimpleStmt()); goto done;
            }
          default: list.Add(ParseSimpleStmt()); break;
        }
      } while(TryEat(Token.Semicolon));

      done:
      if(list.Count==0) Unexpected(token);
      funcDepth--;
      return new LambdaNode(parms, new BlockNode("*FUNCTION*", list.Count==1 ? list[0] : new BodyNode(list.ToArray())));
    }
  }

  // list_comprehension := <expression> ('for' <namelist> 'in' <expression> ('if' <expression>)?)+
  Node ParseListComprehension(Node expr, bool yieldResult)
  { ArrayList list = new ArrayList();
    do
    { Eat(Token.For);
      Name[] names = ParseNameList();
      Eat(Token.In);
      list.Add(new ListCompNode.For(names, ParseExpression(), TryEat(Token.If) ? ParseExpression() : null));
    } while(token==Token.For);
    return new ListCompNode(expr, (ListCompNode.For[])list.ToArray(typeof(ListCompNode.For)), yieldResult);
  }

  // logand := <lognot> ('&&' <lognot>)*
  Node ParseLogAnd()
  { Node expr = ParseLogNot();
    if(token==Token.LogAnd)
      using(CachedList<Node> list = CachedList<Node>.Alloc())
      { list.Add(expr);
        while(TryEat(Token.LogAnd)) list.Add(ParseLogNot());
        expr = new OpNode(Operator.LogicalAnd, list.ToArray());
      }
    return expr;
  }

  // lognot := 'not' <lognot> | <compare>
  Node ParseLogNot()
  { return TryEat(Token.Not) ? new UnaryOpNode(Operator.LogicalNot, ParseLogNot()) : ParseCompare();
  }
  
  // logor := <logand> ('||' <logand>)*
  Node ParseLogOr()
  { Node expr = ParseLogAnd();
    if(token==Token.LogOr)
      using(CachedList<Node> list = CachedList<Node>.Alloc())
      { list.Add(expr);
        while(TryEat(Token.LogOr)) list.Add(ParseLogAnd());
        expr = new OpNode(Operator.LogicalOr, list.ToArray());
      }
    return expr;
  }

  // primary := LITERAL | <ident> | '(' <expression> ')' | '[' <array_list> ']' | '{' <hash_list> '}' |
  //            '[' <list_comprehension> ']' | <tuple of <expression>> | '`' <expression> '`'
  //            '&' <primary>
  // tuple of T := '(' (<T> ',')+ <T>? ')'
  Node ParsePrimary()
  { Node expr;
    bool obt=bareTuples, owe=wantEOL;

    switch(token)
    { case Token.Literal:
        if(value is string)
        { string s = (string)value;
          while(NextToken()==Token.Literal && value is string) s += (string)value;
          if(token==Token.Literal) SyntaxError("unexpected literal '{0}' in string concatenation");
          return new LiteralNode(s);
        }
        else expr = new LiteralNode(value);
        break;
      case Token.Identifier: expr = new VariableNode((string)value); break;
      case Token.LParen:
        bareTuples = wantEOL = false;
        NextToken();
        if(token==Token.RParen) expr = new TupleNode();
        else
        { expr = ParseExpression();
          if(token==Token.Comma)
            using(CachedList<Node> list = CachedList<Node>.Alloc())
            { NextToken();
              list.Add(expr);
              while(token!=Token.RParen)
              { bareTuples = false;
                list.Add(ParseExpression());
                if(!TryEat(Token.Comma)) break;
              }
              expr = new TupleNode(list.ToArray());
            }
          else if(token==Token.For) expr = new GeneratorNode(ParseListComprehension(expr, true));
        }
        Expect(Token.RParen);
        break;
      case Token.LBracket:
        bareTuples = wantEOL = false;
        NextToken();
        if(token==Token.RBracket) expr = new ListNode();
        else
        { Node fe = ParseExpression();
          if(token==Token.For) expr = ParseListComprehension(fe, false);
          else
            using(CachedList<Node> list = CachedList<Node>.Alloc())
            { list.Add(fe);
              TryEat(Token.Comma);
              while(token!=Token.RBracket)
              { bareTuples = false;
                list.Add(ParseExpression());
                if(!TryEat(Token.Comma)) break;
              }
              expr = new ListNode(list.ToArray());
            }
        }
        Expect(Token.RBracket);
        break;
      case Token.LBrace:
        bareTuples = wantEOL = false;
        NextToken();
        if(token==Token.RBrace) expr = new HashNode();
        else
        { List<KeyValuePair<Node,Node>> list = new List<KeyValuePair<Node,Node>>();
          while(token!=Token.RBrace)
          { Node e = ParseExpression();
            Eat(Token.Colon);
            list.Add(new KeyValuePair<Node,Node>(e, ParseExpression()));
            if(!TryEat(Token.Comma)) break;
          }
          expr = new HashNode(list.ToArray());
        }
        Expect(Token.RBrace);
        break;
      case Token.BackQuote:
        NextToken();
        expr = ParseExpression();
        Expect(Token.BackQuote);
        expr = new ReprNode(expr);
        break;
      case Token.BitAnd: // ampersand
        NextToken();
        return new ReferenceNode(ParsePrimary()); // we return because ParsePrimary() calls NextToken() at the end and we don't want to call it again
      default: Unexpected(token); return null;
    }
    bareTuples=obt; wantEOL=owe;
    NextToken();
    return expr;
  }

  // print_stmt := 'print' ('>>' <expression>)? (<expression> (',' <expression>)* ','?)?
  Node ParsePrintStmt()
  { Eat(Token.Print);
    if(token==Token.EOL || token==Token.Semicolon) return new PrintNode();

    bool trailing, old=bareTuples;
    bareTuples = false;

    Node file = TryEat(Token.RightShift) ? ParseExpression() : null;
    if(file!=null)
    { if(token==Token.EOL || token==Token.Semicolon)
      { bareTuples = old;
        return new PrintNode(file);
      }
    }

    using(CachedList<Node> stmts = CachedList<Node>.Alloc())
    { while(true)
      { trailing = true;
        stmts.Add(ParseExpression());
        if(!TryEat(Token.Comma)) break;
        else
        { trailing = false;
          if(token==Token.EOL || token==Token.Semicolon) break;
        }
      }
      bareTuples = old;
      return new PrintNode(file, stmts.ToArray(), trailing || stmts.Count==0);
    }
  }

  // namelist := <identifier> (',' <identifier>)*
  Name[] ParseNameList()
  { using(CachedList<Name> list = CachedList<Name>.Alloc())
    { do list.Add(new Name(ParseIdentifier())); while(TryEat(Token.Comma));
      return list.ToArray();
    }
  }

  // param_list := <required_params>? <pcomma> <optional_params>? <pcomma> ('*' <identifier>)? <pcomma>
  //               ('**' <identifier>)?
  // required_params := <identifier> (',' <identifier>)
  // optional_params := <optional_param> (',' <optional_param>)*
  // optional_param  := <identifier> '=' <expression>
  // pcomma := ','?    (comma required to separate argument/parameter groups)
  Parameter[] ParseParamList(Token end)
  { if(token==end) return new Parameter[0];
    using(CachedList<Parameter> parms = CachedList<Parameter>.Alloc())
    { string ident;
      bool obt=bareTuples, owe=wantEOL;
      bareTuples = wantEOL = false;

      while(true) // required identifiers
      { if(TryEat(Token.Asterisk)) goto list;
        if(token==Token.Power) goto dict;
        ident = ParseIdentifier();
        if(TryEat(Token.Assign)) break;
        parms.Add(new Parameter(ident));
        if(token==end) goto done;
        Eat(Token.Comma);
      }
      while(true) // positional parameters
      { parms.Add(new Parameter(ident, ParseExpression()));
        if(token==end) goto done;
        Eat(Token.Comma);
        if(TryEat(Token.Asterisk)) break;
        if(token==Token.Power) goto dict;
        ident = ParseIdentifier();
        Eat(Token.Assign);
      }
      list: if(token==Token.Identifier) parms.Add(new Parameter(ParseIdentifier(), ParamType.List));
      if(token==end) goto done;
      Eat(Token.Comma);
      dict: Eat(Token.Power);
      parms.Add(new Parameter(ParseIdentifier(), ParamType.Dict));

      done:
      ListDictionary ld = new ListDictionary();
      foreach(Parameter p in parms)
        if(ld.Contains(p.Name.String)) SyntaxError("duplicate parameter name '{0}'", p.Name.String);

      bareTuples=obt; wantEOL=owe;
      return parms.ToArray();
    }
  }

  // power := <unary> ('**' <unary>)*
  Node ParsePower()
  { Node expr = ParseUnary();
    while(TryEat(Token.Power)) expr = new OpNode(BinaryOperator.Power, expr, ParseUnary());
    return expr;
  }

  // shift := <term> (('<<' | '>>') <term>)*
  Node ParseShift()
  { Node expr = ParseTerm();
    while(true)
    { BinaryOperator op;
      switch(token)
      { case Token.LeftShift:  op = BinaryOperator.LeftShift; break;
        case Token.RightShift: op = BinaryOperator.RightShift; break;
        default: return expr;
      }
      NextToken();
      expr = new OpNode(op, expr, ParseTerm());
    }
  }

  // simple_stmt := <expr_stmt> | <print_stmt> | <break_stmt> | <continue_stmt> | <pass_stmt> | <return_stmt>
  //                <assert_stmt> | <del_stmt> | <yield_stmt> | <goto_stmt>
  // break_stmt    := 'break' <blockname>? EOL
  // continue_stmt := 'continue' <blockname>? EOL
  // pass_stmt     := 'pass' EOL
  // raise_stmt    := 'raise' <expression>?
  // return_stmt   := 'return' <expression>?
  // assert_stmt   := 'assert' <expression>
  // yield_stmt    := 'yield' <expression>
  // del_stmt      := 'del' <lvalue> (',' <lvalue>)*
  // goto_stmt     := 'goto' <identifier>
  Node ParseSimpleStmt()
  { Position start = tokenStart;
    Node ret = null;

    switch(token)
    { case Token.Print: ret = ParsePrintStmt(); break;
      case Token.Break:
        if(NextToken()==Token.Identifier)
        { ret = new BreakNode((string)value);
          NextToken();
        }
        else if(!InLoop) SyntaxError("'break' encountered outside loop");
        else ret = new BreakNode(null);
        break;
      case Token.Continue:
        if(NextToken()==Token.Identifier)
        { ret = new RestartNode((string)value);
          NextToken();
        }
        else if(!InLoop) SyntaxError("'continue' encountered outside loop");
        else ret = new RestartNode(null);
        break;
      case Token.Pass: NextToken(); ret = new PassNode(); break;
      case Token.Return:
        NextToken();
        ret = token==Token.EOL || token==Token.Semicolon ? new ReturnNode() : new ReturnNode(ParseExpression());
        break;
      case Token.Raise:
        NextToken();
        if(token==Token.EOL || token==Token.Semicolon) return new ThrowNode();
        ret = ParseExpression();
        if(!TryEat(Token.Comma)) ret = new ThrowNode(ret);
        else
          using(CachedList<Node> objs = CachedList<Node>.Alloc())
          { do objs.Add(ParseExpression()); while(TryEat(Token.Comma));
            ret = new ThrowNode(ret, objs.ToArray());
          }
        break;
      case Token.Assert: NextToken(); ret = new AssertNode(ParseExpression()); break;
      case Token.Yield: NextToken(); hasYield=true; ret = new YieldNode(ParseExpression()); break;
      case Token.Del:
        using(CachedList<Node> list = CachedList<Node>.Alloc())
        { NextToken();
          bool obt = bareTuples;
          bareTuples = false;
          do
          { ret = ParseExpression();
            if(!(ret is VariableNode || ret is GetSlotNode || ret is TupleNode || ret is IndexNode))
              SyntaxError("can't delete {0}", ret.GetType());
            list.Add(ret);
          } while(TryEat(Token.Comma));
          bareTuples = obt;
          ret = new BoaDeleteNode(list.ToArray());
        }
        break;
      default: ret = ParseExprStmt(); break;
    }
    return SetPos(ret, start);
  }

  // stmt_line := <simple_stmt> (';' <simple_stmt>)* (NEWLINE | EOF)
  Node ParseStmtLine()
  { Node stmt = ParseSimpleStmt();
    if(token==Token.Semicolon)
      using(CachedList<Node> stmts = CachedList<Node>.Alloc())
      { stmts.Add(stmt);
        if(token==Token.Semicolon) while(TryEat(Token.Semicolon)) stmts.Add(ParseSimpleStmt());
        stmt = new BodyNode(stmts.ToArray());
      }
    Eat(Token.EOL);
    return stmt;
  }

  // suite := ':' stmt_line | ':'? NEWLINE INDENT <statement>+ UNINDENT
  Node ParseSuite()
  { if(TryEat(Token.Colon) && token!=Token.EOL) return ParseStmtLine();
    int indent = this.indent;
    Eat(Token.EOL);
    if(this.indent<=indent) SyntaxError("expected indent");
    using(CachedList<Node> stmts = CachedList<Node>.Alloc())
    { while(this.indent>indent)
      { if(TryEat(Token.EOL)) continue;
        stmts.Add(ParseStatement());
      }
      return stmts.Count==1 ? stmts[0] : new BodyNode(stmts.ToArray());
    }
  }

  // term := <factor> (('+' | '-') <factor>)*
  Node ParseTerm()
  { Node expr = ParseFactor();
    while(true)
    { BinaryOperator op;
      switch(token)
      { case Token.Plus:  op = BinaryOperator.Add; break;
        case Token.Minus: op = BinaryOperator.Subtract; break;
        default: return expr;
      }
      NextToken();
      expr = new OpNode(op, expr, ParseFactor());
    }
  }

  // ternary := <logor> ('?' <expression> ':' <expression>)
  Node ParseTernary()
  { Node expr = ParseLogOr();
    if(TryEat(Token.Question))
    { Node it = ParseExpression();
      Eat(Token.Colon);
      expr = new IfNode(expr, it, ParseExpression());
    }
    return expr;
  }

  // TODO: expand this to allow an except clause to catch multiple exception types
  // try := 'try' <suite> NEWLINE ('except' <expression>? (',' <ident>)? <suite>)*
  //        ('except' <suite>)? ('else' <suite>)? ('finally' <suite>)?
  Node ParseTry()
  { int indent = this.indent;
    Eat(Token.Try);
    Node body=ParseSuite(), elze=null, final=null;
    using(CachedList<Except> list = CachedList<Except>.Alloc())
    { while(indent==this.indent && TryEat(Token.Except))
      { bool obt = bareTuples;
        bareTuples = false;
        Node type = token==Token.Comma || token==Token.Colon || token==Token.EOL ? null : ParseExpression();
        bareTuples = true;
        string name = TryEat(Token.Comma) ? ParseIdentifier() : null;
        list.Add(new Except(name, type, ParseSuite()));
        if(type==null) break;
      }
      if(indent==this.indent && TryEat(Token.Else)) elze = ParseSuite();
      if(indent==this.indent && TryEat(Token.Finally)) final = ParseSuite();
      if(list.Count==0 && elze==null && final==null) SyntaxError("expecting 'except', 'else', or 'finally'");
      return new TryNode(body, final, elze, list.ToArray());
    }
  }

  // unary     := <unary_op> <unary>
  // unary_op  := '!' | '~' | '-' | '+'
  Node ParseUnary()
  { UnaryOperator op=null;
    switch(token)
    { case Token.LogNot: op = UnaryOperator.LogicalNot; break;
      case Token.Minus:  op = UnaryOperator.UnaryMinus; break;
      case Token.BitNot: op = UnaryOperator.BitwiseNot; break;
      case Token.Plus:   NextToken(); return ParseUnary();
    }
    if(op!=null)
    { NextToken();
      return new UnaryOpNode(op, ParseUnary());
    }
    return ParseCIM();
  }

  // lock_stmt  ::= 'lock' <expression> <suite>
  // using_stmt ::= 'using' <expression> <suite>
  Node ParseUsingBlock(Token token)
  { Eat(token);
    Position start = tokenStart;
    Node expr=SetPos(ParseExpression(), start), body=ParseSuite();
    return token==Token.Lock ? new LockNode(expr, body) : (Node)new UsingNode(expr, body);
  }

  // while_stmt := 'while' <expression> <suite>
  Node ParseWhile()
  { int indent = this.indent;
    Eat(Token.While);
    Position start = tokenStart;
    Node expr = SetPos(ParseExpression(), start);
    loopDepth++;
    Node body=ParseSuite(), elze=this.indent==indent && TryEat(Token.Else) ? ParseSuite() : null;
    loopDepth--;
    return new WhileNode(expr, body, elze);
  }

  Token PeekToken()
  { Position start = tokenStart;
    if(nextToken!=Token.None) return nextToken;
    nextToken = ReadToken(ref nextValue);
    tokenStart = start;
    return nextToken;
  }

  void PushBack()
  { if(pos==0) throw new InvalidOperationException();
    pos--;
    lastChar = '\0';
  }

  char ReadChar()
  { char c;
    if(lastChar!=0) { c=lastChar; lastChar='\0'; return c; }
    else if(pos>=data.Length) { indent=-1; return '\0'; }
    c = data[pos++]; column++;
    if(c=='\n') { line++; column=1; }
    else if(c=='\r')
    { if(pos<data.Length && data[pos]=='\n') pos++;
      c='\n'; line++; column=1;
    }
    else if(c==0) c = ' ';
    return c;
  }

  #region ReadToken
  Token ReadToken()
  { lastTokenEnd = new Position(line, column - (lastChar==0 ? 0 : 1));
    return ReadToken(ref value);
  }

  Token ReadToken(ref object value)
  { char c;
    while(true)
    { if(token==Token.EOL)
      { c = ReadChar();
        if(wantEOL)
        { indent=0;
          while(c!=0 && (char.IsWhiteSpace(c) || c=='#'))
          { if(c=='\n') indent=0;
            else if(c=='#') { do c = ReadChar(); while(c!='\n' && c!=0); indent=0; }
            else indent++;
            c = ReadChar();
          }
        }
        else while(c!=0 && char.IsWhiteSpace(c)) c = ReadChar();
      }
      else do c=ReadChar(); while(c!='\n' && c!=0 && char.IsWhiteSpace(c));

      tokenStart = new Position(line, column-1);

      if(char.IsDigit(c) || c=='.')
      { if(c=='.')
        { lastChar = ReadChar();
          if(!char.IsDigit(lastChar)) return Token.Period;
        }

        string s=string.Empty;
        bool period=false, hex=false, lastPeriod=false;

        while(true)
        { if(c=='.')
          { if(hex || lastPeriod) break;
            lastPeriod = true;
            c = '\0';
            if(period) break;
            goto nextchar;
          }
          else if(!char.IsDigit(c) && (!hex || (c<'a' || c>'f') && (c<'A' || c>'F')))
          { if(!hex && (c=='x' || c=='X') && s=="0") { s=string.Empty; hex=true; goto nextchar; }
            break;
          }
          else if(lastPeriod) { s+="."; period=true; lastPeriod=false; }
          s += c;
          nextchar: c = ReadChar();
        }

        try
        { if(!lastPeriod && char.ToUpper(c)=='J')
          { if(hex) SyntaxError("'J' modifier cannot be used with hex numbers");
            value = new Complex(0, double.Parse(s));
          }
          else if(!lastPeriod && char.ToUpper(c)=='E')
          { if(hex) SyntaxError("'E' modifier cannot be used with hex numbers");
            double num=double.Parse(s), exp;
            bool   neg=false;
            s = string.Empty;
            c = ReadChar();
            if(c=='-') { neg=true; c=ReadChar(); }
            else if(c=='+') c=ReadChar();
            if(!char.IsNumber(c)) SyntaxError("Expected number in scientific notation.");
            do
            { s += c;
              c = ReadChar();
            } while(char.IsNumber(c));
            lastChar = c;

            exp = double.Parse(s);
            value = num * Math.Pow(10, neg ? -exp : exp);
          }
          else if(period)
          { if(hex) SyntaxError("invalid number or attribute reference");
            if(char.ToUpper(c)=='L') throw new NotImplementedException("decimal type");
            else { lastChar=c; value = double.Parse(s); }
          }
          else
          { if(char.ToUpper(c)!='L') lastChar = c;
            try { value = hex ? Convert.ToInt32(s, 16) : int.Parse(s); }
            catch(OverflowException)
            { try { value = hex ? Convert.ToInt64(s, 16) : long.Parse(s); }
              catch(OverflowException) { value = Integer.Parse(s, hex ? 16 : 10); }
            }
          }

          if(lastPeriod)
          { if(lastChar!=0) PushBack();
            lastChar = '.';
          }

          return Token.Literal;
        }
        catch(FormatException) { SyntaxError("invalid number"); }
      }
      else if(c=='_' || char.IsLetter(c))
      { StringBuilder sb = new StringBuilder();

        if(c=='r')
        { char temp=c; c=ReadChar();
          if(c=='\"' || c=='\'')
          { char delim = c;
            while((c=ReadChar())!=0 && c!=delim) sb.Append(c);
            if(c==0) SyntaxError("unterminated string literal");
            value = sb.ToString();
            return Token.Literal;
          }
          else sb.Append(temp);
        }

        while(c=='_' || char.IsLetterOrDigit(c)) { sb.Append(c); c = ReadChar(); }
        lastChar = c;

        string s = sb.ToString();
        if(s=="null"  || s=="None") { value=null;  return Token.Literal; }
        if(s=="true"  || s=="True")  { value=true;  return Token.Literal; }
        if(s=="false" || s=="False") { value=false; return Token.Literal; }
        value = stringTokens[s];
        if(value!=null) return (Token)value;
        
        value = s;
        return Token.Identifier;
      }
      else switch(c)
      { case '\n': newline: if(wantEOL) return Token.EOL; else { token=Token.EOL; break; }
        case '\"': case '\'':
        { StringBuilder sb = new StringBuilder();
          char delim = c;
          bool triple = false;

          c = ReadChar();
          if(c==delim)
          { c = ReadChar();
            if(c==delim) triple = true;
            else { lastChar=c; value=string.Empty; return Token.Literal; }
          }
          else if(c=='\\') { char e = GetEscapeChar(); if(e!=0) sb.Append(e); }
          else sb.Append(c);

          while(true)
          { c = ReadChar();
            if(c=='\\') { char e = GetEscapeChar(); if(e!=0) sb.Append(e); }
            else if(c==delim)
            { if(!triple) break;
              if((c=ReadChar())==delim)
              { if((c=ReadChar())==delim) break;
                else
                { sb.Append(delim, 2);
                  if(c=='\\') { char e = GetEscapeChar(); if(e!=0) sb.Append(e); }
                  else sb.Append(c);
                }
              }
              else
              { sb.Append(delim);
                if(c=='\\') { char e = GetEscapeChar(); if(e!=0) sb.Append(e); }
                else sb.Append(c);
              }
            }
            else if(c==0) SyntaxError("unterminated string literal");
            else sb.Append(c);
          }
          value = sb.ToString();
          return Token.Literal;
        }
        case '<':
          c = ReadChar();
          if(c=='<') return Token.LeftShift;
          if(c=='=') value=BinaryOperator.LessEqual;
          else if(c=='>') value = BinaryOperator.NotEqual;
          else { lastChar = c; value = BinaryOperator.Less; }
          return Token.Compare;
        case '>':
          c = ReadChar();
          if(c=='>') return Token.RightShift;
          if(c=='=') value=BinaryOperator.MoreEqual;
          else { lastChar = c; value = BinaryOperator.More; }
          return Token.Compare;
        case '=':
          c = ReadChar();
          if(c=='=') { value=BinaryOperator.Equal; return Token.Compare; }
          else { lastChar=c; value=null; return Token.Assign; }
        case '!':
          c = ReadChar();
          if(c=='=') { value=BinaryOperator.NotEqual; return Token.Compare; }
          else { lastChar = c; return Token.LogNot; }
        case '&':
          c = ReadChar();
          if(c=='&')
          { c = ReadChar();
            if(c=='=') { value=BinaryOperator.LogicalAnd; return Token.Assign; }
            lastChar = c; return Token.LogAnd;
          }
          if(c=='=') { value=BinaryOperator.BitwiseAnd; return Token.Assign; }
          lastChar = c; return Token.BitAnd;
        case '|':
          c = ReadChar();
          if(c=='|')
          { c = ReadChar();
            if(c=='=') { value=BinaryOperator.LogicalOr; return Token.Assign; }
            lastChar = c; return Token.LogOr;
          }
          if(c=='=') { value=BinaryOperator.BitwiseOr; return Token.Assign; }
          lastChar = c; return Token.BitOr;
        case '^':
          c = ReadChar();
          if(c=='=') { value=BinaryOperator.BitwiseXor; return Token.Assign; }
          lastChar = c; return Token.BitXor;
        case '+':
          c = ReadChar();
          if(c=='=') { value=BinaryOperator.Add; return Token.Assign; }
          lastChar = c; return Token.Plus;
        case '-':
          c = ReadChar();
          if(c=='=') { value=BinaryOperator.Subtract; return Token.Assign; }
          lastChar = c; return Token.Minus;
        case '*':
          c = ReadChar();
          if(c=='=') { value=BinaryOperator.Multiply; return Token.Assign; }
          if(c=='*')
          { c = ReadChar();
            if(c=='=') { value=BinaryOperator.Power; return Token.Assign; }
            lastChar = c; return Token.Power;
          }
          lastChar = c; return Token.Asterisk;
        case '/':
          c = ReadChar();
          if(c=='/')
          { c = ReadChar();
            if(c=='=') { value=BinaryOperator.FloorDivide; return Token.Assign; }
            lastChar = c; return Token.FloorDivide;
          }
          if(c=='=') { value=BinaryOperator.Divide; return Token.Assign; }
          if(c=='*')
          { do c = ReadChar(); while(c!=0 && (c!='*' || (c=ReadChar())!='/'));
            break;
          }
          lastChar = c; return Token.Slash;
        case '%':
          c = ReadChar();
          if(c=='=') { value=BinaryOperator.Modulus; return Token.Assign; }
          lastChar = c; return Token.Percent;
        case '~': return Token.BitNot;
        case ':':
          c = ReadChar();
          if(c==':') return Token.StaticMember;
          lastChar = c;
          return Token.Colon;
        case '`': return Token.BackQuote;
        case ',': return Token.Comma;
        case '(': return Token.LParen;
        case ')': return Token.RParen;
        case '[': return Token.LBracket;
        case ']': return Token.RBracket;
        case '{': return Token.LBrace;
        case '}': return Token.RBrace;
        case '?': return Token.Question;
        case ';': return Token.Semicolon;
        case '#':
          do c = ReadChar(); while(c!='\n' && c!=0);
          goto newline;
        case '\\':
          c = ReadChar();
          if(c=='\n') break;
          goto default;
        case '\0': if(wantEOL) { nextToken=Token.EOF; return Token.EOL; } else return Token.EOF;
        default: SyntaxError(string.Format("unexpected character '{0}' (0x{1:X})", c, (int)c)); break;
      }
    }
  }
  #endregion

  Node SetPos(Node node, Position start)
  { node.StartPos = start;
    node.EndPos = lastTokenEnd;
    return node;
  }

  void SyntaxError(string format, params object[] args) { SyntaxError(string.Format(format, args)); }
  void SyntaxError(string message) { throw Ops.SyntaxError("{0}({1},{2}): {3}", sourceFile, line, column, message); }

  bool TryEat(Token type)
  { if(token==type) { NextToken(); return true; }
    return false;
  }
  
  void Unexpected(Token token) { SyntaxError("unexpected token {0}", token, sourceFile, line, column); }
  void Unexpected(Token got, Token expect)
  { SyntaxError("unexpected token {0} (expecting {1})", got, expect, sourceFile, line, column);
  }

  string     sourceFile, data;
  Token      token=Token.EOL, nextToken=Token.None;
  object     value, nextValue;
  Position   tokenStart, lastTokenEnd;
  int        line=1, column=1, pos, indent, funcDepth, loopDepth;
  char       lastChar;
  bool       bareTuples=true, wantEOL=true, hasYield;
  
  static SortedList stringTokens;
}
#endregion

} // namespace Boa.Backend
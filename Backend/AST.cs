/*
Boa is the reference implementation for a language, also called Boa,
which is similar to python. This implementation is both interpreted
and compiled, targetting the Microsoft .NET Framework.

http://www.adammil.net/
Copyright (C) 2005 Adam Milazzo

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
using System.Reflection;
using System.Reflection.Emit;
using Scripting;

namespace Boa.Backend
{

#region AST
public sealed class AST
{ AST() { }

  public static Node Create(Node node)
  { node.Walk(new BoaWalker());
    return node;
  }
  
  /* This walker performs the following tasks:
    1. Distinguish between local/global names by searching for 'global' nodes, and convert 
        def fun(a):
          global g
          (x,g) = (1,2)
          def helper(g): (x,y,g) = (4,5,6)
       Into
        def fun(a):
          localbind x
            (x,g) = (1,2)
            def helper(g):
              localbind y
                (x,y,g) = (4,5,6)

       It should also handle this case:
         def fun(a):
           global a # error, local redefined as global
           def nested():
             global a # error, local redefined as global
  
       And, when we support warnings, this case:
         def fun():
           x = 5
           global x # warning, 'x' assigned to before global declaration.
  */
  public sealed class BoaWalker : IWalker
  { public void PostWalk(Node n) { }

    public bool Walk(Node n)
    { if(n is LambdaNode)
      { foreach(Parameter p in ((LambdaNode)n).Parameters) if(p.Default!=null) p.Default.Walk(this);

        if(names==null) names = new ArrayList();

        LambdaNode oldFunc=func;
        func = (LambdaNode)n;
        int oldCount = baseCount;
        baseCount = names.Count;

        foreach(Parameter p in func.Parameters) names.Add(p.Name);
        func.Body.Walk(this);

        int pbase = baseCount+func.Parameters.Length; // discount the parameters
        if(pbase!=names.Count)
        { int numLocal = 0;
          for(int i=pbase; i<names.Count; i++) if(((Name)names[i]).Depth!=Name.Global) numLocal++;
          if(numLocal!=0)
          { string[] localNames = new string[numLocal];
            for(int i=pbase,j=0; i<names.Count; i++)
            { Name name = (Name)names[i];
              if(name.Depth!=Name.Global) localNames[j++] = name.String;
            }
            func.Body = new LocalBindNode(localNames, new Node[localNames.Length], func.Body);
          }
        }
        names.RemoveRange(baseCount, names.Count-baseCount);

        func=oldFunc; baseCount=oldCount;
        return false;
      }
      else if(n is GlobalNode)
      { if(func==null) throw Ops.SyntaxError(n, "'global' encounted outside function"); // TODO: make this a warning
        GlobalNode gn = (GlobalNode)n;

        foreach(string str in gn.Names)
        { int i;
          for(i=names.Count-1; i>=0; i--)
          { Name name = (Name)names[i];
            if(name.String==str)
            { if(name.Depth==0 || name.Depth==Name.Local && i<baseCount)
                throw Ops.SyntaxError(n, "'{0}' defined as both global and local", str); // parameter
              else if(name.Depth==Name.Local)
              { throw Ops.SyntaxError(n, "'{0}' was assigned to before 'global' statement", str); // TODO: make this a warning
                name.Depth = Name.Global;
                break;
              }
            }
          }
          if(i==-1) names.Add(new Name(str, Name.Global));
        }
      }
      else if(n is SetNode)
      { if(func!=null)
          foreach(MutatedName mn in ((SetNode)n).GetMutatedNames())
          { int i;
            for(i=names.Count-1; i>=0; i--) if(((Name)names[i]).String==mn.Name.String) break;
            if(i==-1) names.Add(new Name(mn.Name.String, Name.Local));
          }
      }

      return true;
    }

    ArrayList names;
    LambdaNode func;
    int baseCount;
  }
}
#endregion

#region BoaLanguage
public sealed class BoaLanguage : Language
{ public override string BuiltinsNamespace { get { return "Boa.Mods"; } }
  public override string Name { get { return "Boa"; } }

  #region Ops
  public override int Compare(object a, object b) { return Ops.TypeName(a).CompareTo(Ops.TypeName(b)); }
  #endregion

  #region EmitConstant
  public override bool EmitConstant(CodeGenerator cg, object value)
  { if(value is Tuple)
    { cg.EmitObjectArray(((Tuple)value).items);
      cg.EmitCall(typeof(Tuple), "Make", typeof(object[])); // TODO: use the constructor
    }
    else if(value is List)
    { List list = (List)value;
      cg.EmitInt(list.Count);
      cg.EmitNew(typeof(List), typeof(int));
      MethodInfo mi = typeof(IList).GetMethod("Add");
      foreach(object o in list)
      { cg.ILG.Emit(OpCodes.Dup);
        cg.EmitConstantObject(o);
        cg.EmitCall(mi);
      }
    }
    else if(value is Dict)
    { Dict dict = (Dict)value;
      cg.EmitInt(dict.Count);
      cg.EmitNew(typeof(Dict), typeof(int));
      MethodInfo mi = typeof(IDictionary).GetMethod("Add");
      foreach(DictionaryEntry de in dict)
      { cg.ILG.Emit(OpCodes.Dup);
        cg.EmitConstantObject(de.Key);
        cg.EmitConstantObject(de.Value);
        cg.EmitCall(mi);
      }
    }
    else if(value is Slice)
    { Slice slice = (Slice)value;
      cg.EmitConstantObject(slice.start);
      cg.EmitConstantObject(slice.stop);
      cg.EmitConstantObject(slice.step);
      cg.EmitNew(typeof(Slice), typeof(object), typeof(object), typeof(object));
    }
    else return false;
    return true;
  }
  #endregion

  public override void EmitPackedArguments(CodeGenerator cg, Node[] args, int start, int length)
  { if(length==0) cg.EmitFieldGet(typeof(List), "Empty");
    else
    { cg.EmitObjectArray(args, start, length);
      cg.EmitNew(typeof(List), typeof(object[]));
    }
  }

  public override bool ExcludeFromImport(string name) { return name.StartsWith("_"); }
  public override bool IsHashableConstant(object value) { return value is Tuple || value is Slice; }

  public override object PackArguments(object[] args, int start, int length)
  { return length==0 ? List.Empty : new List(args, start, length);
  }

  public override Node Parse(string sourceName, string code)
  { return AST.Create(new Parser(sourceName, code).Parse());
  }

  public override Node Parse(string sourceName, System.IO.TextReader data)
  { return AST.Create(new Parser(sourceName, data).Parse());
  }

  #region Repr
  public override string Repr(object obj)
  { switch(Convert.GetTypeCode(obj))
    { case TypeCode.Boolean: return (bool)obj ? "true" : "false";
      case TypeCode.Double: return ((double)obj).ToString("R");
      case TypeCode.Empty: return "null";
      case TypeCode.Int64: case TypeCode.UInt64: return obj.ToString()+'L';
      case TypeCode.Object:
        if(obj is IRepresentable) return ((IRepresentable)obj).__repr__();
        if(obj is Array) return ArrayOps.Repr((Array)obj);
        break;
      case TypeCode.Single: return ((float)obj).ToString("R");
      case TypeCode.String: return StringOps.Escape((string)obj);
      case TypeCode.UInt32:
      { string ret = obj.ToString();
        if((uint)obj>int.MaxValue) ret += 'L';
        return ret;
      }
    }
    return obj.ToString();
  }
  #endregion

  public override string Repr(Node node) { throw new NotImplementedException("node repr"); }
  
  #region TypeName
  public override string TypeName(Type type)
  { switch(Type.GetTypeCode(type))
    { case TypeCode.Boolean: return "bool";
      case TypeCode.Empty: return "null";
      case TypeCode.Byte:  return "byte";
      case TypeCode.SByte: return "sbyte";
      case TypeCode.Int16: return "short";
      case TypeCode.UInt16: return "ushort";
      case TypeCode.Int32: return "int";
      case TypeCode.UInt32: return "uint";
      case TypeCode.Int64: return "long";
      case TypeCode.UInt64: return "ulong";
      case TypeCode.Char: return "char";
      case TypeCode.Object:
        if(type==typeof(Slice)) return "slice";
        if(type==typeof(Tuple)) return "tuple";
        if(type==typeof(List)) return "list";
        if(type==typeof(Dict)) return "dict";
        if(type==typeof(Integer)) return "bigint";
        if(type==typeof(Complex)) return "complex";
        if(type==typeof(MultipleValues)) return "multiplevalues";
        if(type==typeof(Reference)) return "ref";
        if(type==typeof(Type) || type==typeof(ReflectedType)) return "type";
        goto default;
      case TypeCode.Double: return "double";
      case TypeCode.Single: return "single";
      case TypeCode.String: return "string";
      default: return type.FullName;
    }
  }
  #endregion
  
  public static readonly BoaLanguage Instance = new BoaLanguage();
}
#endregion

#region InOperator
public sealed class InOperator : Operator
{ InOperator() : base("in", 2, 2) { }

  public override void Emit(string name, CodeGenerator cg, ref Type etype, params Node[] args)
  { CheckArity(name, args);
    // TODO: optimize "exp in (x, y, z)" into "var=exp, var==x || var==y || var==z"
    cg.EmitNodes(args[0], args[1]);
    cg.EmitCall(typeof(BoaOps), "IsIn");
    if(etype!=typeof(bool)) { cg.BoolToObject(); etype=typeof(object); }
  }

  public override object Evaluate(string name, params object[] args) { return BoaOps.IsIn(args[0], args[1]); }
  
  public static readonly InOperator Instance = new InOperator();
}
#endregion

#region AssignNode
public sealed class AssignNode : SetNode
{ public AssignNode(Node lhs, Node rhs) : base(lhs, rhs, SetType.Set) { }
  public AssignNode(Node[] lhs, Node rhs) : base(lhs, rhs, SetType.Set) { }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { cg.MarkPosition(this);

    int num=0, length=0;
    foreach(Node n in LHS)
    { TupleNode tn = n as TupleNode;
      if(tn!=null)
      { if(num++==0) length = tn.Expressions.Length;
        else if(tn.Expressions.Length!=length)
          throw Ops.SyntaxError(this, "inconsistent tuple lengths in multiple tuple assignment");
      }
    }
    if(num==0) { base.Emit(cg, ref etype); return; } // if there are no tuple nodes, then it's a simple assignment

    if(RHS.IsConstant)
    { object value = RHS.Evaluate();
      Type type = null;
      if(num!=LHS.Length) // emit if there are non-tuples on the LHS
      { type = GetNodeType();
        RHS.Emit(cg, ref type);
        if(etype!=typeof(void)) etype = type;
      }

      for(int i=LHS.Length-1,nt=0; i>=0; i--)
      { TupleNode tn = LHS[i] as TupleNode;
        if(tn!=null) EmitTupleAssignment(cg, tn, value);
        else
        { if(++nt!=num || etype!=typeof(void)) cg.ILG.Emit(OpCodes.Dup);
          EmitSet(cg, LHS[i], type);
        }
      }

      if(type==null && etype!=typeof(void))
      { etype = GetNodeType();
        RHS.Emit(cg, ref etype);
      }
    }
    /* TODO: if it's just tuple -> tuple assignment, we can convert it to a simpler form. this could be optimized more
    // (eg, with smart dependency checking so (x,y[x],z) = (y,z,x) produces y[x]=z; z=x; x=y), but the effort is
    // better spent elsewhere for now.
    // TODO: but it should at least be optimized so that ((x))=((y)), (x,y)=(a,b), and (x,y)=(y,x) are relatively efficient
    else if(RHS is TupleNode)
    { throw new NotImplementedException();
    }*/
    else
    { Type type = typeof(object);
      RHS.Emit(cg, ref type);

      Slot tmp;
      if(num==LHS.Length && etype==typeof(void)) tmp = null; // all tuple assignments, and no return value needed
      else
      { cg.ILG.Emit(OpCodes.Dup);
        type = etype==typeof(void) || etype==typeof(object) ? typeof(object) : type;
        tmp  = cg.AllocLocalTemp(type);
        tmp.EmitSet(cg);
      }

      cg.EmitInt(length);
      cg.EmitCall(typeof(BoaOps), "PrepareTupleAssignment");

      for(int i=LHS.Length-1,ta=0; i>=0; i--)
      { TupleNode tn = LHS[i] as TupleNode;
        if(tn!=null)
        { if(i!=0) cg.ILG.Emit(OpCodes.Dup);
          if(ta++!=0)
          { cg.ILG.Emit(OpCodes.Dup);
            cg.EmitCall(typeof(IEnumerator), "Reset"); // TODO: perhaps to be safe, we should create a new enumerator each time rather than using Reset(), in case Reset() is not supported?
          }
          EmitTupleAssignment(cg, tn);
        }
        else
        { tmp.EmitGet(cg);
          if(++num==LHS.Length && etype==typeof(void)) cg.FreeLocalTemp(tmp); // free 'tmp' as soon as we're done with it
          EmitSet(cg, LHS[i], type);
        }
      }
      
      if(etype!=typeof(void))
      { tmp.EmitGet(cg);
        cg.FreeLocalTemp(tmp);
        etype = type;
      }
    }

    TailReturn(cg);
  }

  public override MutatedName[] GetMutatedNames()
  { ArrayList names = new ArrayList();
    foreach(Node n in LHS)
    { VariableNode vn = n as VariableNode; // treat top-level VariableNodes differently than nested ones because we can
      if(vn!=null) names.Add(new MutatedName(vn.Name, RHS)); // easily determine the value of a top-level one (RHS),
      else GetMutatedNames(names, n);                        // but a nested VariableNode is more complicated
    }
    return (MutatedName[])names.ToArray(typeof(MutatedName));
  }

  protected override void Assign(Node lhs, object value)
  { TupleNode tn = lhs as TupleNode;
    if(tn==null) base.Assign(lhs, value);
    throw new NotImplementedException("tuple assignment");
  }

  protected override void EmitSet(CodeGenerator cg, Node lhs, Type onStack)
  { IndexNode idx = lhs as IndexNode;
    if(idx!=null) idx.EmitSet(cg, onStack);
    else base.EmitSet(cg, lhs, onStack);
  }

  protected override void GetMutatedNames(IList names, Node lhs)
  { VariableNode vn = lhs as VariableNode;
    if(vn!=null) names.Add(new MutatedName(vn.Name, null));
    else
    { TupleNode tn = lhs as TupleNode;
      if(tn!=null) foreach(Node n in tn.Expressions) GetMutatedNames(names, n);
      else throw UnhandledNodeType(lhs);
    }
  }

  protected override void UpdateNames(MutatedName[] names, ref int i, Node lhs)
  { TupleNode tn = lhs as TupleNode;
    if(tn!=null) foreach(Node n in tn.Expressions) UpdateNames(names, ref i, n);
    else base.UpdateNames(names, ref i, lhs);
  }

  void EmitTupleAssignment(CodeGenerator cg, TupleNode lhs, object value)
  { IEnumerator e = BoaOps.PrepareTupleAssignment(value, lhs.Expressions.Length);
    for(int i=0; e.MoveNext() && i<lhs.Expressions.Length; i++)
    { Node n = lhs.Expressions[i];
      TupleNode tn = n as TupleNode;
      if(tn!=null) EmitTupleAssignment(cg, tn, e.Current);
      else
      { Type type = n.GetNodeType();
        EmitConstant(cg, e.Current, ref type);
        EmitSet(cg, n, type);
      }
    }
  }

  void EmitTupleAssignment(CodeGenerator cg, TupleNode lhs)
  { for(int i=0; i<lhs.Expressions.Length; i++)
    { cg.ILG.Emit(OpCodes.Dup);
      cg.EmitCall(typeof(IEnumerator), "MoveNext");
      cg.ILG.Emit(OpCodes.Pop); // ignore the return value (may cause problems if we encounter bad enumerators)
      if(i!=lhs.Expressions.Length-1) cg.ILG.Emit(OpCodes.Dup);
      cg.EmitPropGet(typeof(IEnumerator), "Current");

      TupleNode tn = lhs.Expressions[i] as TupleNode;
      if(tn==null) EmitSet(cg, lhs.Expressions[i], typeof(object));
      else
      { cg.EmitInt(tn.Expressions.Length);
        cg.EmitCall(typeof(BoaOps), "PrepareTupleAssignment");
        EmitTupleAssignment(cg, tn);
      }
    }
  }
}
#endregion

#region BoaDeleteNode
public sealed class BoaDeleteNode : WrapperNode
{ public BoaDeleteNode(Node[] nodes)
  { if(nodes.Length==1) Node = new DeleteNode(nodes[0]);
    else
    { Node[] dels = new Node[nodes.Length];
      for(int i=0; i<nodes.Length; i++) dels[i] = new DeleteNode(nodes[i]);
      Node = new BodyNode(dels);
    }
  }

  // TODO: add support for deleting indices, etc
  sealed class DeleteNode : Scripting.DeleteNode
  { public DeleteNode(Node node) : base(node) { }
  }
}
#endregion

#region CompareNode
public sealed class CompareNode : Node
{ public CompareNode(Node[] exprs, ComparisonOperator[] ops) { Expressions=exprs; Ops=ops; }
  
  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(IsConstant) EmitConstant(cg, Evaluate(), ref etype);
    else if(Ops.Length==1) Ops[0].Emit(cg, ref etype, Expressions[0], Expressions[1]);
    else
    { Label isfalse=cg.ILG.DefineLabel(), end=cg.ILG.DefineLabel();
      Slot tmp = null;
      Type type;

      // TODO: this assumes nodes don't clear the stack and don't interrupt...
      Expressions[0].Emit(cg);
      for(int i=0; i<Ops.Length; i++)
      { if(i!=0) tmp.EmitGet(cg);
        Expressions[i+1].Emit(cg);
        if(i==0) tmp = cg.AllocLocalTemp(typeof(object));
        tmp.EmitSet(cg);

        type = typeof(bool);
        Ops[i].EmitOp(cg, ref type);
        if(type==typeof(bool) || type==typeof(CodeGenerator.negbool))
          cg.ILG.Emit(type==typeof(bool) ? OpCodes.Brfalse : OpCodes.Brtrue, isfalse);
        else
        { cg.EmitIsTrue();
          cg.ILG.Emit(OpCodes.Brfalse, isfalse);
        }
      }

      type = etype;
      EmitConstant(cg, true, ref type);
      cg.ILG.Emit(OpCodes.Br_S, end);
      cg.ILG.MarkLabel(isfalse);
      EmitConstant(cg, false, ref etype);
      cg.ILG.MarkLabel(end);

      cg.FreeLocalTemp(tmp);
    }
  }

  public readonly Node[] Expressions;
  public readonly ComparisonOperator[] Ops;
}
#endregion

#region ForNode
public sealed class ForNode : LoopNode
{ public ForNode(Name[] names, Node expression, Node body, Node elze)
  { /*
      try
      { tmp e = Ops.GetEnumerator(<expression>);
        BLOCK:
        { if(!e.MoveNext())
          { <else>;
            break BLOCK;
          }
          (<name0>, <name1>, ...) = e.Current;
          <body>
          continue BLOCK;
        }
      }
      catch(StopIterationException) { }
    */

    Language lang = Options.Current.Language;
    string ename=lang.GenerateName(null, "e"), blockname=lang.GenerateName(null, "for");

    Node assign;
    if(names.Length==0) assign = new PassNode();
    else if(names.Length==1)
      assign = new AssignNode(new VariableNode(names[0]), new GetCurrentNode(new VariableNode(ename)));
    else
    { Node[] vars = new Node[names.Length];
      for(int i=0; i<vars.Length; i++) vars[i] = new VariableNode(names[i]);
      assign = new AssignNode(new TupleNode(vars), new GetCurrentNode(new VariableNode(ename)));
    }

    body.Walk(new LoopJumpReplacer(blockname));
    Node block = new BlockNode(blockname,
                               new BodyNode(new IfNode(new UnaryOpNode(UnaryOperator.LogicalNot,
                                                                       new MoveNextNode(new VariableNode(ename))),
                                                       elze==null ? (Node)new BreakNode(blockname)
                                                                  : new BodyNode(elze, new BreakNode(blockname))),
                                            assign, body, new RestartNode(blockname)));
    Node = new TryNode(new LocalBindNode(ename, new GetEnumeratorNode(expression), typeof(IEnumerator), block),
                       new Except(new TypeNode(typeof(StopIterationException)), new PassNode()));
  }

  #region GetCurrentNode
  sealed class GetCurrentNode : Node
  { public GetCurrentNode(Node enumerator) { Enumerator = enumerator; }

    public override void Emit(CodeGenerator cg, ref Type etype)
    { cg.EmitTypedNode(Enumerator, typeof(IEnumerator));
      cg.EmitPropGet(typeof(IEnumerator), "Current");
      etype = typeof(object);
      TailReturn(cg);
    }

    public override object Evaluate() { return ((IEnumerator)Enumerator.Evaluate()).Current; }

    public override void Walk(IWalker w)
    { if(w.Walk(this)) Enumerator.Walk(w);
      w.PostWalk(this);
    }

    public readonly Node Enumerator;
  }
  #endregion

  #region GetEnumeratorNode
  sealed class GetEnumeratorNode : Node
  { public GetEnumeratorNode(Node expression) { Expression=expression; }
    
    public override void Emit(CodeGenerator cg, ref Type etype)
    { Expression.Emit(cg);
      cg.EmitCall(typeof(BoaOps), "GetEnumerator");
      etype = typeof(IEnumerator);
    }

    public override object Evaluate() { return BoaOps.GetEnumerator(Expression.Evaluate()); }
    public override Type GetNodeType() { return typeof(IEnumerator); }

    public override void Walk(IWalker w)
    { if(w.Walk(this)) Expression.Walk(w);
      w.PostWalk(this);
    }

    public readonly Node Expression;
  }
  #endregion

  #region MoveNextNode
  sealed class MoveNextNode : Node
  { public MoveNextNode(Node enumerator) { Enumerator = enumerator; }

    public override void Emit(CodeGenerator cg, ref Type etype)
    { cg.EmitTypedNode(Enumerator, typeof(IEnumerator));
      cg.EmitCall(typeof(IEnumerator), "MoveNext");
      if(etype!=typeof(bool))
      { cg.ILG.Emit(OpCodes.Box, typeof(bool));
        etype = typeof(object);
      }
      TailReturn(cg);
    }

    public override object Evaluate() { return ((IEnumerator)Enumerator.Evaluate()).MoveNext(); }
    public override Type GetNodeType() { return typeof(bool); }

    public override void Walk(IWalker w)
    { if(w.Walk(this)) Enumerator.Walk(w);
      w.PostWalk(this);
    }

    public readonly Node Enumerator;
  }
  #endregion
}
#endregion

#region GlobalNode
public sealed class GlobalNode : MarkerNode
{ public GlobalNode(string[] names) { Names = names; }
  public readonly string[] Names;
}
#endregion

#region HashNode
public sealed class HashNode : Node
{ public HashNode(params DictionaryEntry[] entries) { Entries = entries; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(IsConstant)
    { cg.EmitConstantObject(Evaluate());
      cg.EmitNew(typeof(Dict), typeof(IDictionary));
    }
    else
    { cg.EmitInt(Entries.Length);
      cg.EmitNew(typeof(Dict), typeof(int));

      MethodInfo add = typeof(IDictionary).GetMethod("set_Item");
      if(!ClearsStack)
      { foreach(DictionaryEntry de in Entries)
        { cg.ILG.Emit(OpCodes.Dup);
          cg.EmitNodes((Node)de.Key, (Node)de.Value);
          cg.EmitCall(add);
        }
        etype = typeof(Dict);
      }
      else
      { Slot tmp = cg.AllocLocalTemp(typeof(IDictionary)), a=null, b=null;
        tmp.EmitSet(cg);
        foreach(DictionaryEntry de in Entries)
        { Node key = (Node)de.Key, val = (Node)de.Value;
          if(val.ClearsStack)
          { key.Emit(cg);
            if(a==null) a = cg.AllocLocalTemp(typeof(object));
            a.EmitSet(cg);
            val.Emit(cg);
            if(b==null) b = cg.AllocLocalTemp(typeof(object));
            b.EmitSet(cg);
            
            tmp.EmitGet(cg);
            a.EmitGet(cg);
            b.EmitGet(cg);
          }
          else if(key.ClearsStack)
          { key.Emit(cg);
            if(a==null) a = cg.AllocLocalTemp(typeof(object));
            a.EmitSet(cg);
            
            tmp.EmitGet(cg);
            a.EmitGet(cg);
            val.Emit(cg);
          }
          else
          { tmp.EmitGet(cg);
            key.Emit(cg);
            val.Emit(cg);
          }
          
          cg.EmitCall(add);
        }

        cg.FreeLocalTemp(tmp);
        if(a!=null) cg.FreeLocalTemp(a);
        if(b!=null) cg.FreeLocalTemp(b);
        etype = typeof(IDictionary);
      }
    }
  }

  public override object Evaluate()
  { Dict dict = new Dict();
    foreach(DictionaryEntry de in Entries)
      dict[((Node)de.Key).Evaluate()] = ((Node)de.Value).Evaluate();
    return dict;
  }

  public override void MarkTail(bool tail)
  { foreach(DictionaryEntry de in Entries) { ((Node)de.Key).MarkTail(false); ((Node)de.Value).MarkTail(false); }
    Tail = tail;
  }

  public override void Optimize()
  { bool isconst = true;
    foreach(DictionaryEntry de in Entries)
      if(!((Node)de.Key).IsConstant || !((Node)de.Value).IsConstant) { isconst=false; break; }
    IsConstant = isconst;
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this))
      foreach(DictionaryEntry de in Entries) { ((Node)de.Key).Walk(w); ((Node)de.Value).Walk(w); }
    w.PostWalk(this);
  }

  public readonly DictionaryEntry[] Entries;
}
#endregion

#region ImportNode
public sealed class ImportNode : WrapperNode
{ public ImportNode(string[] names, string[] asNames)
  { Node[] assigns = new Node[names.Length];
    for(int i=0; i<names.Length; i++)
      assigns[i] = new SetNode(new VariableNode(names[i]),
                               new ImportOneNode(asNames[i]==null ? names[i] : asNames[i], 
                                                 asNames[i]==null));
    Node = new BodyNode(assigns);
  }

  sealed class ImportOneNode : Node
  { public ImportOneNode(string name, bool importTop) { Name=name; ImportTop=importTop; }

    public override void Emit(CodeGenerator cg, ref Type etype)
    { cg.EmitString(Name);
      if(!ImportTop) cg.EmitCall(typeof(Importer), "Load", typeof(string));
      else
      { cg.EmitBool(true);
        cg.EmitBool(true);
        cg.EmitCall(typeof(Importer), "Load", typeof(string), typeof(bool), typeof(bool));
      }
      etype = typeof(MemberContainer);
    }

    public override object Evaluate() { return Importer.Load(Name, true, ImportTop); }
    public override Type GetNodeType() { return typeof(MemberContainer); }

    public readonly string Name;
    public readonly bool ImportTop;
  }
}
#endregion

#region ImportFromNode
public sealed class ImportFromNode : Node
{ public ImportFromNode(string module, string[] names, string[] asNames)
  { Module=module; Names=names; AsNames=asNames;
    for(int i=0; i<AsNames.Length; i++) if(AsNames[i]==null) AsNames[i] = Names[i];
  }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { cg.EmitString(Module);
    cg.EmitCall(typeof(Importer), "Load", typeof(string));
    cg.EmitFieldGet(typeof(TopLevel), "Current");
    if(Names==null) cg.EmitCall(typeof(MemberContainer), "Import", typeof(TopLevel));
    else
    { cg.EmitConstantObject(Names);
      cg.EmitConstantObject(AsNames);
      cg.EmitCall(typeof(MemberContainer), "Import", typeof(TopLevel), typeof(string[]), typeof(string[]));
    }

    if(etype!=typeof(void)) { cg.ILG.Emit(OpCodes.Ldnull); etype=typeof(object); }
    TailReturn(cg);
  }

  public override object Evaluate()
  { MemberContainer mc = Importer.Load(Module);
    if(Names==null) mc.Import(TopLevel.Current);
    else mc.Import(TopLevel.Current, Names, AsNames);
    return null;
  }

  public override Type GetNodeType() { return null; }

  public readonly string Module;
  public readonly string[] Names, AsNames;
}
#endregion

#region IndexNode
public sealed class IndexNode : Node
{ public IndexNode(Node value, Node index) { Value=value; Index=index; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(etype==typeof(void))
    { if(!IsConstant) cg.EmitVoids(Value, Index);
    }
    else
    { if(IsConstant) cg.EmitConstantObject(Evaluate());
      else
      { cg.EmitNodes(Value, Index);
        cg.EmitCall(typeof(Ops), "GetIndex");
      }
    }
  }

  public void EmitSet(CodeGenerator cg, Type onStack)
  { throw new NotImplementedException();
  }

  public override void MarkTail(bool tail)
  { Tail = tail;
    Value.MarkTail(false);
    Index.MarkTail(false);
  }

  public override void Optimize() { IsConstant = Value.IsConstant && Index.IsConstant; }

  public readonly Node Value, Index;
}
#endregion

#region ListNode
public sealed class ListNode : Node
{ public ListNode(params Node[] expressions) { Expressions = expressions; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(etype==typeof(void))
    { if(!IsConstant) cg.EmitVoids(Expressions);
    }
    else
    { if(IsConstant)
      { cg.EmitConstantObject(Evaluate());
        cg.EmitNew(typeof(List), typeof(ICollection));
      }
      else
      { cg.EmitObjectArray(Expressions);
        cg.EmitNew(typeof(List), typeof(object[]));
      }
      etype = typeof(List);
    }
  }

  public override object Evaluate() { return new List(MakeObjectArray(Expressions)); }
  public override Type GetNodeType() { return typeof(List); }

  public override void MarkTail(bool tail)
  { foreach(Node n in Expressions) n.MarkTail(false);
    Tail = tail;
  }
  
  public override void Optimize() { IsConstant = AreConstant(Expressions); }

  public override void Walk(IWalker w)
  { if(w.Walk(this)) foreach(Node n in Expressions) n.Walk(w);
    w.PostWalk(this);
  }
  
  public readonly Node[] Expressions;
}
#endregion

#region LockNode
public sealed class LockNode : ExceptionNode
{ public LockNode(Node lockObj, Node body) : base(body, null) { Lock=lockObj; NeedFinally=true; }

  public override object Evaluate()
  { if(Lock is TupleNode)
    { TupleNode tup = (TupleNode)Lock;
      object[] arr = new object[tup.Expressions.Length];
      for(int i=0; i<arr.Length; i++)
      { arr[i] = tup.Expressions[i].Evaluate();
        if(arr[i]==null) throw Ops.ValueError("cannot lock a null value");
      }
      for(int i=0; i<arr.Length; i++) System.Threading.Monitor.Enter(arr[i]);
      try { return Body.Evaluate(); }
      finally { foreach(object o in arr) System.Threading.Monitor.Exit(o); }
    }
    else
    { object o = Lock.Evaluate();
      if(o==null) throw Ops.ValueError("cannot lock a null value");
      lock(o) return Body.Evaluate();
    }
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this))
    { BaseWalk(w);
      Lock.Walk(w);
    }
    w.PostWalk(this);
  }

  public readonly Node Lock;
  
  protected override void EmitPreTry(CodeGenerator cg)
  { Node[] locks = Lock is TupleNode ? ((TupleNode)Lock).Expressions : new Node[] { Lock };
    Label good=cg.ILG.DefineLabel(), evil=locks.Length==1 ? new Label() : cg.ILG.DefineLabel();

    slots = new Slot[locks.Length];
    for(int i=0; i<slots.Length; i++)
    { locks[i].Emit(cg);
      cg.ILG.Emit(OpCodes.Dup);
      slots[i] = cg.AllocLocalTemp(typeof(object));
      slots[i].EmitSet(cg);
      if(i==locks.Length-1) cg.ILG.Emit(OpCodes.Brtrue_S, good);
      else cg.ILG.Emit(OpCodes.Brfalse, evil);
    }
    
    if(locks.Length!=1) cg.ILG.MarkLabel(evil);
    cg.EmitString("cannot lock a null value");
    cg.EmitCall(typeof(Ops), "ValueError", typeof(string));
    cg.ILG.Emit(OpCodes.Throw);
    
    cg.ILG.MarkLabel(good);
    foreach(Slot s in slots)
    { s.EmitGet(cg);
      cg.EmitCall(typeof(System.Threading.Monitor), "Enter");
    }
  }

  protected override void EmitFinally(CodeGenerator cg)
  { foreach(Slot s in slots)
    { s.EmitGet(cg);
      cg.EmitCall(typeof(System.Threading.Monitor), "Exit");
      cg.FreeLocalTemp(s);
    }
    slots = null;
  }

  Slot[] slots;
}
#endregion

#region LoopNode
public abstract class LoopNode : WrapperNode
{ protected sealed class LoopJumpReplacer : IWalker
  { public LoopJumpReplacer(string name) { Name = name; }
  
    public bool Walk(Node node)
    { if(node is LoopNode || node is LambdaNode) return false;
      else if(node is JumpNode)
      { JumpNode jn = (JumpNode)node;
        if(jn.Name==null) jn.Name = Name;
      }
      return true;
    }

    public void PostWalk(Node node) { }

    string Name;
  }
}
#endregion

#region PassNode
public sealed class PassNode : WrapperNode
{ public PassNode() : base(new LiteralNode(null)) { }
}
#endregion

#region PrintNode
public sealed class PrintNode : Node
{ public PrintNode() : this(null, null, true) { }
  public PrintNode(Node file) : this(file, null, true) { }
  public PrintNode(Node file, Node[] exprs) : this(file, exprs, exprs==null || exprs.Length==0) { }
  public PrintNode(Node file, Node[] exprs, bool trailingNewline)
  { File=file; Expressions=exprs; TrailingNewline=trailingNewline;
  }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { Slot file;

    if(File==null) file = null;
    else
    { File.Emit(cg);
      file = cg.AllocLocalTemp(typeof(object));
      file.EmitSet(cg);
    }

    if(Expressions!=null)
      foreach(Node n in Expressions)
      { if(file==null) cg.ILG.Emit(OpCodes.Ldnull);
        else file.EmitGet(cg);
        n.Emit(cg);
        cg.EmitCall(typeof(BoaOps), "Print");
      }

    if(TrailingNewline)
    { if(file==null) cg.ILG.Emit(OpCodes.Ldnull);
      else file.EmitGet(cg);
      cg.EmitCall(typeof(BoaOps), "PrintNewline");
    }
    
    if(file!=null) cg.FreeLocalTemp(file);
    
    if(etype!=typeof(void)) { cg.ILG.Emit(OpCodes.Ldnull); etype=typeof(object); }
    TailReturn(cg);
  }

  public override object Evaluate()
  { object file = File==null ? null : File.Evaluate();
    if(Expressions!=null) foreach(Node n in Expressions) BoaOps.Print(file, n.Evaluate());
    if(TrailingNewline) BoaOps.PrintNewline(file);
    return null;
  }

  public override Type GetNodeType() { return null; }

  public override void MarkTail(bool tail)
  { if(File!=null) File.MarkTail(false);
    if(Expressions!=null) foreach(Node n in Expressions) n.MarkTail(false);
    Tail = tail;
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this))
    { if(File!=null) File.Walk(w);
      if(Expressions!=null) foreach(Node n in Expressions) n.Walk(w);
    }
    w.PostWalk(this);
  }

  public readonly Node File;
  public readonly Node[] Expressions;
  public readonly bool TrailingNewline;
}
#endregion

#region ReprNode
public sealed class ReprNode : WrapperNode
{ public ReprNode(Node expression) : base(expression) { }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(IsConstant) cg.EmitConstantObject(Evaluate());
    else
    { Node.Emit(cg);
      cg.EmitCall(typeof(Ops), "Repr");
    }
    TailReturn(cg);
  }

  public override Type GetNodeType() { return typeof(string); }
}
#endregion

#region SliceNode
public sealed class SliceNode : Node
{ public SliceNode(Node start, Node stop) { Start=start; Stop=stop; }
  public SliceNode(Node start, Node stop, Node step) { Start=start; Stop=stop; Step=step; }
  
  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(etype==typeof(void))
    { if(!IsConstant) cg.EmitVoids(Start, Stop, Step);
    }
    else
    { if(IsConstant) cg.EmitConstantObject(Evaluate());
      else
      { cg.EmitNodes(Start, Stop, Step);
        cg.EmitNew(typeof(Slice), typeof(object), typeof(object), typeof(object));
      }
      etype = typeof(Slice);
    }
  }

  public override object Evaluate()
  { return new Slice(Start==null ? null : Start.Evaluate(),
                     Stop==null ? null : Stop.Evaluate(),
                     Step==null ? null : Step.Evaluate());
  }

  public override Type GetNodeType() { return typeof(Slice); }

  public override void MarkTail(bool tail)
  { Tail = tail;
    if(Start!=null) Start.MarkTail(false);
    if(Stop!=null) Stop.MarkTail(false);
    if(Step!=null) Step.MarkTail(false);
  }

  public override void Optimize() { IsConstant = AreConstant(Start, Stop, Step); }

  public override void Walk(IWalker w)
  { if(w.Walk(this))
    { if(Start!=null) Start.Walk(w);
      if(Stop!=null) Stop.Walk(w);
      if(Step!=null) Step.Walk(w);
    }
    w.PostWalk(this);
  }

  public readonly Node Start, Stop, Step;
}
#endregion

#region TupleNode
public sealed class TupleNode : Node
{ public TupleNode(params Node[] expressions) { Expressions = expressions; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(etype==typeof(void))
    { if(!IsConstant) cg.EmitVoids(Expressions);
    }
    else
    { if(IsConstant) cg.EmitConstantObject(Evaluate());
      else
      { cg.EmitObjectArray(Expressions);
        cg.EmitCall(typeof(Tuple), "Make", typeof(object[])); // TODO: use the constructor
      }
      etype = typeof(Tuple);
    }
    TailReturn(cg);
  }

  public override object Evaluate() { return Tuple.Make(MakeObjectArray(Expressions)); }
  public override Type GetNodeType() { return typeof(Tuple); }

  public override void MarkTail(bool tail)
  { foreach(Node n in Expressions) n.MarkTail(false);
    Tail = tail;
  }
  
  public override void Optimize() { IsConstant = AreConstant(Expressions); }
  
  public override void Walk(IWalker w)
  { if(w.Walk(this)) foreach(Node n in Expressions) n.Walk(w);
    w.PostWalk(this);
  }
  
  public readonly Node[] Expressions;
}
#endregion

#region UsingNode
public sealed class UsingNode : ExceptionNode
{ public UsingNode(Node temp, Node body) : base(body, null) { Temp=temp; NeedFinally=true; }

  public override object Evaluate()
  { if(Temp is TupleNode)
    { TupleNode tup = (TupleNode)Temp;
      IDisposable[] arr = new IDisposable[tup.Expressions.Length];
      for(int i=0; i<arr.Length; i++) arr[i] = (IDisposable)tup.Expressions[i].Evaluate();
      try { return Body.Evaluate(); }
      finally { foreach(IDisposable disp in arr) if(disp!=null) disp.Dispose(); }
    }
    else
    { IDisposable disp = (IDisposable)Temp.Evaluate();
      if(disp==null) return Body.Evaluate();
      else
        try { return Body.Evaluate(); }
        finally { disp.Dispose(); }
    }
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this))
    { BaseWalk(w);
      Temp.Walk(w);
    }
    w.PostWalk(this);
  }

  public readonly Node Temp;
  
  protected override void EmitPreTry(CodeGenerator cg)
  { Node[] temps = Temp is TupleNode ? ((TupleNode)Temp).Expressions : new Node[] { Temp };

    slots = new Slot[temps.Length];
    for(int i=0; i<slots.Length; i++)
    { cg.EmitTypedNode(temps[i], typeof(IDisposable));
      slots[i] = cg.AllocLocalTemp(typeof(IDisposable));
      slots[i].EmitSet(cg);
    }
  }

  protected override void EmitFinally(CodeGenerator cg)
  { foreach(Slot s in slots)
    { Label skip = cg.ILG.DefineLabel();
      s.EmitGet(cg);
      cg.ILG.Emit(OpCodes.Brfalse_S, skip);
      s.EmitGet(cg);
      cg.EmitCall(typeof(IDisposable), "Dispose");
      cg.ILG.MarkLabel(skip);
      cg.FreeLocalTemp(s);
    }
    slots = null;
  }

  Slot[] slots;
}
#endregion

#region WhileNode
public sealed class WhileNode : LoopNode
{ public WhileNode(Node test, Node body) : this(test, body, null) { }
  public WhileNode(Node test, Node body, Node elze)
  { /* try
         while1:
           if !test: break while1
           body
         [else]
       catch StopIteration: pass
    */
    string name = GenerateName("while");
    body = new BlockNode(name, new BodyNode(new IfNode(new UnaryOpNode(Operator.LogicalNot, test),
                                                       new BreakNode(name)),
                                            body));
    if(elze!=null) body = new BodyNode(body, elze);
    Node = new TryNode(body, new Except(new TypeNode(typeof(StopIterationException)), new PassNode()));
  }
}
#endregion

} // namespace Boa.Backend
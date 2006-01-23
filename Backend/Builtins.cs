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
using System.IO;
using System.Reflection;
using Scripting;

namespace Boa.Backend
{

public sealed class Builtins
{ public static MemberContainer Instance
  { get
    { if(instance==null) instance = Importer.Load(typeof(Builtins));
      return instance;
    }
  }

public static void loadAssemblyByName(string name) { Scripting.Backend.Interop.LoadAssemblyByName(name); }

  [DocString(@"abs(object) -> object

Return the absolute value of a number. The argument may be a plain or long integer or a floating point number. If the
argument is a complex number, its magnitude is returned.")]
  public static object abs(object o)
  { switch(Convert.GetTypeCode(o))
    { case TypeCode.Boolean: return (bool)o ? 1 : 0;
      case TypeCode.Byte: case TypeCode.UInt16: case TypeCode.UInt32: case TypeCode.UInt64: return o;
      case TypeCode.Decimal: return Math.Abs((Decimal)o);
      case TypeCode.Double: return Math.Abs((double)o);
      case TypeCode.Int16: return Math.Abs((short)o);
      case TypeCode.Int32: return Math.Abs((int)o);
      case TypeCode.Int64: return Math.Abs((long)o);
      case TypeCode.Object:
        if(o is Integer) return ((Integer)o).Abs;
        if(o is Complex) return ((Complex)o).Magnitude;
        break;
      case TypeCode.SByte: return Math.Abs((sbyte)o);
      case TypeCode.Single: return Math.Abs((float)o);
    }
    throw Ops.ArgError("invalid operand type for abs(): got '{0}'", Ops.TypeName(o));
  }

  [DocString(@"callable(object) -> bool

Return true if the object argument is callable, false if not.")]
  public static object callable(object o) { return Ops.IsProcedure(o); }

  [DocString(@"chr(i) -> str

Return a string of one character whose ASCII code is the integer passed.
For example, chr(97) returns the string 'a'. This is the inverse of ord().")]
  public sealed class chr : Primitive
  { public chr() : base("chr", 1, 1) { }
    public override object Call(object[] args) { return new string((char)Ops.ToInt(args[0]), 1); }
  }

  [DocString(@"cmp(x, y) -> int

Compare the two objects x and y and return an integer according to the outcome. The return value is negative if x<y,
zero if x==y and strictly positive if x>y.")]
  public static int cmp(object a, object b) { return Ops.Compare(a, b); }

  [DocString(@"delattr(object, name)

This is a relative of setattr(). The arguments are an object and a string. The string must be the name of one of the
object's attributes. The function deletes the named attribute, provided the object allows it. For example,
delattr(x, 'foobar') is equivalent to del x.foobar")]
  public static void delattr(object o, string name) { Ops.DeleteSlot(o, name); }

  [DocString(@"dir([object]) -> list

Without arguments, return the list of names in the current local symbol table. With an argument, attempts to return a
list of valid attributes for that object. The resulting list is sorted alphabetically.")]
  public static List dir() { throw new NotImplementedException(); }
  public static List dir(object o) { return new List(MemberContainer.FromObject(o).GetMemberNames()); }

  [DocString(@"fancyCallable(object) -> bool

Return true if the object argument is callable with keywords or list/dictionary arguments..")]
  public static object fancyCallable(object o) { return o is IFancyProcedure; }

  [DocString(@"filter(function, sequence) -> sequence

Construct a sequence from those elements of 'sequence' for which function returns true. 'sequence' must be an
enumerable object. If 'sequence' is a string or a tuple, the result also has that type; otherwise it is always a list.
If function is null, the truth function is assumed, that is, all elements of list that are false (zero or empty) are
removed.

Note that filter(function, list) is equivalent to [item for item in list if function(item)] if function is not null and
[item for item in list if item] if function is null.")]
  public static object filter(object function, object seq)
  { IProcedure proc = Ops.MakeProcedure(function);
    if(proc==null && function!=null) Ops.ExpectProcedure(function);

    object[] args;
    bool realloc;
    if(proc==null) { args=null; realloc=false; }
    else
    { realloc = proc.NeedsFreshArgs;
      args = realloc ? null : new object[1];
    }

    if(seq is string)
    { if(proc==null) return seq;

      System.Text.StringBuilder sb = new System.Text.StringBuilder();
      string str = (string)seq;
      for(int i=0; i<str.Length; i++)
      { if(realloc) args = new object[1];
        args[0] = new string(str[i], 1);
        if(Ops.IsTrue(proc.Call(args))) sb.Append(str[i]);
      }
      return sb.ToString();
    }
    else
    { IEnumerator e = BoaOps.GetEnumerator(seq);
      List ret = new List();

      if(proc==null)
      { while(e.MoveNext()) if(Ops.IsTrue(e.Current)) ret.Add(e.Current);
      }
      else
        while(e.MoveNext())
        { if(realloc) args = new object[1];
          args[0] = e.Current;
          if(Ops.IsTrue(proc.Call(args))) ret.Add(e.Current);
        }
      return seq is Tuple ? ret.ToTuple() : (object)ret;
    }
  }

  [DocString(@"map(function, seq1, ...) -> list

Takes a function object (or null) and one or more sequences, and applies the
function to the items of the sequences and return a list of the results. The
function must take as many arguments as the number of sequences passed. The
function will be applied to the items of all lists in parallel; if a list is
shorter than another it is assumed to be extended with null items. If
the function is null, the identity function is assumed; if there are multiple
list arguments, map() returns a list consisting of tuples containing the
corresponding items from all lists (a kind of transpose operation). The list
arguments may be any kind of sequence; the result is always a list.")]
  public static List map(object function, object seq)
  { IProcedure proc = Ops.MakeProcedure(function);
    List ret;
    IEnumerator e;

    ICollection col = seq as ICollection;
    if(col!=null) { ret=new List(col.Count); e=col.GetEnumerator(); }
    else { ret=new List(); e=BoaOps.GetEnumerator(seq); }

    if(proc==null)
    { if(function!=null) Ops.ExpectProcedure(function);
      while(e.MoveNext()) ret.Add(e.Current);
    }
    else
    { bool realloc = proc.NeedsFreshArgs;
      object[] args = realloc ? null : new object[1];
      while(e.MoveNext())
      { if(realloc) args = new object[1];
        args[0] = e.Current;
        ret.Add(proc.Call(args));
      }
    }

    return ret;
  }

  public static List map(object function, params object[] seqs)
  { if(seqs.Length==0) throw new ArgumentException("map(): at least 2 arguments required");
    if(seqs.Length==1) return map(function, seqs[0]);

    IProcedure proc = Ops.MakeProcedure(function);
    object[] args;
    bool realloc;

    if(proc==null)
    { if(function!=null) Ops.ExpectProcedure(function);
      realloc = true;
      args = null;
    }
    else
    { realloc = proc.NeedsFreshArgs;
      args = realloc ? null : new object[seqs.Length];
    }

    List ret = new List();
    IEnumerator[] enums = new IEnumerator[seqs.Length];
    for(int i=0; i<enums.Length; i++) enums[i] = BoaOps.GetEnumerator(seqs[i]);
    
    bool done;
    while(true)
    { done = true;
      if(realloc) args = new object[seqs.Length];
      for(int i=0; i<enums.Length; i++)
        if(enums[i].MoveNext()) { args[i]=enums[i].Current; done=false; }
        else args[i] = null;
      if(done) break;
      ret.Add(proc==null ? Tuple.Make(args) : proc.Call(args));
    }
    return ret;
  }

  [DocString(@"ord(char) -> int

Return the ASCII value of a string of one character. Eg, ord('a') returns the integer 97, ord('\u2020') returns 8224.
This is the inverse of chr().")]
  public sealed class ord : Primitive
  { public ord() : base("ord", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      object c = args[0];

      string s = c as string;
      if(s!=null)
      { if(s.Length!=1)
          throw new ArgumentException(name+": expected a character but got string of length "+s.Length.ToString());
        return (int)s[0];
      }

      if(c is char) return (int)(char)c;

      throw new ArgumentException(name+": expected a charcter but got "+Ops.TypeName(c));
    }
  }
  
  [DocString(@"range([start,] stop[, step]) -> list

This is a versatile function to create lists containing arithmetic
progressions. It is most often used in for loops. The arguments must be
plain integers. If the step argument is omitted, it defaults to 1. If the
start argument is omitted, it defaults to 0. The full form returns a list
of plain integers [start, start + step, start + 2 * step, ...]. If step is
positive, the last element is the largest start + i * step less than stop;
if step is negative, the last element is the largest start + i * step
greater than stop. step must not be zero.

Example:

>>> range(10)
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
>>> range(1, 11)
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
>>> range(0, 30, 5)
[0, 5, 10, 15, 20, 25]
>>> range(0, 10, 3)
[0, 3, 6, 9]
>>> range(0, -10, -1)
[0, -1, -2, -3, -4, -5, -6, -7, -8, -9]
>>> range(0)
[]
>>> range(1, 0)
[]")]
  public static List range(int stop) { return range(0, stop, 1); }
  public static List range(int start, int stop) { return range(start, stop, 1); }
  public static List range(int start, int stop, int step)
  { if(step==0) throw new ArgumentException("step of 0 passed to range()");
    if(step<0 && start<=stop || step>0 && start>=stop) return new List();
    int sign = Math.Sign(step);
    List ret = new List((stop-start+step-sign)/step);
    if(step<0) for(; start>stop; start+=step) ret.Add(start);
    else for(; start<stop; start+=step) ret.Add(start);
    return ret;
  }

  [DocString(@"reduce(function, sequence[, initializer]) -> object

Apply function of two arguments cumulatively to the items of sequence, from
left to right, so as to reduce the sequence to a single value. For example,
reduce(lambda x,y: x+y, [1, 2, 3, 4, 5]) calculates ((((1+2)+3)+4)+5). The
left argument, x, is the accumulated value and the right argument, y, is the
update value from the sequence. If the optional initializer is present, it
is placed before the items of the sequence in the calculation, and serves as
a default when the sequence is empty. If initializer is not given and
sequence contains only one item, the first item is returned.")]
  public static object reduce(object function, object seq)
  { IProcedure proc = Ops.ExpectProcedure(function);
    IEnumerator e = BoaOps.GetEnumerator(seq);
    if(!e.MoveNext()) throw new ArgumentException("reduce() of empty sequence with no initial value");

    bool realloc = proc.NeedsFreshArgs;
    object[] args = realloc ? null : new object[2];
    object ret = e.Current;

    while(e.MoveNext())
    { if(realloc) args = new object[2];
      args[0] = ret;
      args[1] = e.Current;
      ret = proc.Call(args);
    }

    return ret;
  }

  public static object reduce(object function, object seq, object initial)
  { IProcedure proc = Ops.ExpectProcedure(function);
    IEnumerator e = BoaOps.GetEnumerator(seq);
    bool realloc = proc.NeedsFreshArgs;
    object[] args = realloc ? null : new object[2];
    while(e.MoveNext())
    { if(realloc) args = new object[2];
      args[0] = initial;
      args[1] = e.Current;
      initial = proc.Call(args);
    }
    return initial;
  }

  [DocString(@"repr(object) -> str

Return a string containing a printable representation of an object. This is
the same value yielded by conversions (reverse quotes). It is sometimes
useful to be able to access this operation as an ordinary function. For many
types, this function makes an attempt to return a string that would yield an
object with the same value when passed to eval().")]
  public static string repr(object o) { return Ops.ToCode(o); }

  [DocString(@"typeof(object) -> type

Return the type of the given object.")]
  public static MemberContainer @typeof(object o) { return MemberContainer.FromObject(o); }

  static MemberContainer instance;
}

} // namespace NetLisp.Backend
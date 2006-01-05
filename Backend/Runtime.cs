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
using System.Collections.Specialized;
using System.Globalization;
using System.Text;
using System.Text.RegularExpressions;
using Scripting;

namespace Boa.Backend
{

#region ArrayOps
public sealed class ArrayOps
{ ArrayOps() { }

  public static int Compare(object a, object b)
  { if(b==null) return 1;
    Array aa, ab, ret=null;
    if(a is Tuple)
    { Tuple ta = (Tuple)a, tb = b as Tuple;
      if(tb==null) goto badTypes;
      aa = ta.items;
      ab = tb.items;
    }
    else if(a is Array)
    { if(a.GetType()!=b.GetType()) goto badTypes;
      aa  = (Array)a;
      ab  = (Array)b;
      ret = Array.CreateInstance(aa.GetType().GetElementType(), aa.Length+ab.Length);
    }
    else
    { aa = ToArray(a);
      ab = ToArray(b);
    }
    if(ret==null) ret = new object[aa.Length+ab.Length];
    IList A=aa, B=ab;
    int len = Math.Min(aa.Length, ab.Length);
    for(int i=0; i<len; i++)
    { int c = Ops.Compare(A[i], B[i]);
      if(c!=0) return c;
    }
    return aa.Length==ab.Length ? 0 : aa.Length<ab.Length ? -1 : 1;

    badTypes:
    throw Ops.TypeError("invalid operand types for sequence comparison: '{0}' and '{1}'",
                        Ops.TypeName(a), Ops.TypeName(b));
  }

  public static int Compare(object[] arr1, int len1, object[] arr2, int len2)
  { int len = Math.Min(len1, len2);
    for(int i=0; i<len; i++)
    { int c = Ops.Compare(arr1[i], arr2[i]);
      if(c!=0) return c;
    }
    return len1==len2 ? 0 : len1<len2 ? -1 : 1;
  }
  
  public static object Concat(object a, object b)
  { Array aa, ab, ret=null;
    if(a is Tuple)
    { Tuple ta = (Tuple)a, tb = b as Tuple;
      if(tb==null) goto badTypes;
      aa = ta.items;
      ab = tb.items;
    }
    else if(a is Array)
    { if(a.GetType()!=b.GetType()) goto badTypes;
      aa  = (Array)a;
      ab  = (Array)b;
      ret = Array.CreateInstance(aa.GetType().GetElementType(), aa.Length+ab.Length);
    }
    else
    { aa = ToArray(a);
      ab = ToArray(b);
    }
    if(ret==null) ret = new object[aa.Length+ab.Length];
    aa.CopyTo(ret, 0);
    ab.CopyTo(ret, aa.Length);
    return a is Tuple ? new Tuple((object[])ret) : a is Array ? ret : (object)new List((object[])ret, ret.Length);
    
    badTypes:
    throw Ops.TypeError("invalid operand types for sequence concatenation: '{0}' and '{1}'",
                        Ops.TypeName(a), Ops.TypeName(b));
  }

  public static object Multiply(object a, object b)
  { int bv;
    if(b is int) bv = (int)b;
    else switch(Convert.GetTypeCode(b))
    { case TypeCode.Boolean: if((bool)b) return a; else bv=0; break;
      case TypeCode.Byte: bv = (byte)b; break;
      case TypeCode.Int16: bv = (short)b; break;
      case TypeCode.Int32: bv = (int)b; break;
      case TypeCode.Int64:
      { long lv = (long)b;
        if(lv>int.MaxValue || lv<int.MinValue) throw new OverflowException("long int too large to convert to int");
        bv = (int)lv;
        break;
      }
      case TypeCode.Object:
      { if(b is Integer) bv = ((Integer)b).ToInt32();
        IConvertible ic = b as IConvertible;
        if(ic==null) return Ops.Invoke(b, "__rmul__", a);
        bv = ic.ToInt32(NumberFormatInfo.InvariantInfo);
        break;
      }
      case TypeCode.SByte: bv = (sbyte)b; break;
      case TypeCode.UInt16: bv = (ushort)b; break;
      case TypeCode.UInt32:
      { uint ui = (uint)b;
        if(ui>int.MaxValue) throw new OverflowException("long int too large to convert to int");
        bv = (int)ui;
        break;
      }
      case TypeCode.UInt64:
      { ulong ul = (uint)b;
        if(ul>int.MaxValue) throw new OverflowException("long int too large to convert to int");
        bv = (int)ul;
        break;
      }
      default: throw Ops.TypeError("invalid operand types for sequence multiplication: '{0}' and '{1}'",
                                   Ops.TypeName(a), Ops.TypeName(b));
    }
    
    if(bv<0) throw Ops.ValueError("multiplier for sequence multiplication cannot be negative");
    if(bv==1) return a;

    Array source, dest;
    if(a is Array)
    { source = (Array)a;
      dest = Array.CreateInstance(source.GetType().GetElementType(), source.Length*bv);
    }
    else
    { source = ToArray(a);
      dest = new object[source.Length*bv];
    }

    int ai = source.Length, bvb=bv, mask=0;
    Array.Copy(source, dest, ai);
    while((bv>>=1) != 0) { Array.Copy(dest, 0, dest, ai, ai); ai += ai; mask=mask<<1 | 1; }
    bv = bvb&mask;
    if(bv>0) Array.Copy(dest, 0, dest, ai, (bvb&mask)*source.Length);

    return a is Tuple ? new Tuple((object[])dest) : a is Array ? dest : (object)new List((object[])dest, dest.Length);
  }
  
  public static string Repr(Array arr)
  { System.Text.StringBuilder sb = new System.Text.StringBuilder();
    sb.Append(arr.GetType().FullName);
    sb.Append('(');
    for(int i=0; i<arr.Length; i++)
    { if(i>0) sb.Append(", ");
      sb.Append(Ops.Repr(arr.GetValue(i)));
    }
    sb.Append(')');
    return sb.ToString();
  }
  
  public static object[] ToArray(object o)
  { object[] ret;
    if(o is Tuple) ret = ((Tuple)o).items;
    else if(o is ICollection)
    { ICollection ic = (ICollection)o;
      ret = new object[ic.Count];
      ic.CopyTo(ret, 0);
    }
    else if(o is ISequence)
    { ISequence s = (ISequence)o;
      ret = new object[s.__len__()];
      for(int i=0; i<ret.Length; i++) ret[i] = s.__getitem__(i);
    }
    else
    { ret = new object[Ops.ToInt(Ops.Invoke(o, "__len__"))];
      object getitem = Ops.GetMember(o, "__getitem__");
      for(int i=0; i<ret.Length; i++) ret[i] = Ops.Call(getitem, i);
    }
    return ret;
  }
}
#endregion

#region BoaHash
public sealed class BoaHash : IHashCodeProvider
{ BoaHash() { }

  // FIXME: implement this
  public int GetHashCode(object obj)
  { return obj==null ? 0 : obj.GetHashCode();
  }
  
  public static readonly BoaHash Instance = new BoaHash();
}
#endregion

#region BoaOps
public sealed class BoaOps
{ BoaOps() { }

  public static int FixIndex(int index, int length)
  { if(index<0)
    { index += length;
      if(index<0) throw new ArgumentOutOfRangeException(string.Format("index out of range: {0}", index-length));
    }
    else if(index>=length) throw new ArgumentOutOfRangeException(string.Format("index out of range: {0}", index));
    return index;
  }

  public static int FixSliceIndex(int index, int length)
  { if(index<0)
    { index += length;
      if(index<0) index=0;
    }
    else if(index>length) index=length;
    return index;
  }

  public static IEnumerator GetEnumerator(object o)
  { IEnumerator e;
    if(GetEnumerator(o, out e)) return e;
    throw new ArgumentException("Objects of type '"+Ops.TypeName(o)+"' are not enumerable");
  }

  public static bool GetEnumerator(object o, out IEnumerator e)
  { if(o is string) e = new BoaCharEnumerator((string)o);
    else if(o is IEnumerable) e = ((IEnumerable)o).GetEnumerator();
    else if(o is IEnumerator) e = (IEnumerator)o;
    else
    { object iter;
      if(Ops.Invoke(o, "__iter__", out iter)) e = new IterEnumerator(iter);
      else
      { object len, getitem;
        if(Ops.GetMember(o, "__len__", out len) && Ops.GetMember(o, "__getitem__", out getitem))
          e = new SeqEnumerator(o);
        else { e=null; return false; }
      }
    }
    return true;
  }
  
  public static bool IsIn(object needle, object haystack)
  { IList list = haystack as IList;
    if(list!=null) return list.Contains(needle);
    
    IDictionary dict = haystack as IDictionary;
    if(dict!=null) return dict.Contains(needle);

    IEnumerator e = GetEnumerator(haystack);
    while(e.MoveNext()) if(Ops.AreEqual(needle, e.Current)) return true;
    return false;
  }

  public static void Print(object file, object o)
  { string str = Ops.Str(o);
    if(file==null) { Console.Write(str); return; }

    IFile f = file as IFile;
    if(f!=null) { f.write(str); return; }

    System.IO.Stream stream = file as System.IO.Stream;
    if(stream!=null)
    { byte[] data = System.Text.Encoding.Default.GetBytes(str);
      stream.Write(data, 0, data.Length);
      return;
    }

    Ops.Invoke(file, "write", str);
  }

  public static void PrintNewline(object file)
  { if(file==null) { Console.WriteLine(); return; }

    IFile f = file as IFile;
    if(f!=null) { f.writebyte('\n'); return; }

    System.IO.Stream stream = file as System.IO.Stream;
    if(stream!=null) { stream.WriteByte((byte)'\n'); return; }

    Ops.Invoke(file, "write", "\n");
  }

  public static List SequenceSlice(IList list, Slice slice)
  { Tuple tup = slice.indices(list.Count);
    return SequenceSlice(list, (int)tup.items[0], (int)tup.items[1], (int)tup.items[2]);
  }

  public static List SequenceSlice(IList list, int start, int stop, int step)
  { if(step<0 && start<=stop || step>0 && start>=stop) return new List();
    int sign = Math.Sign(step);
    List ret = new List((stop-start+step-sign)/step);
    if(step<0) for(; start>stop; start+=step) ret.append(list[start]);
    else for(; start<stop; start+=step) ret.append(list[start]);
    return ret;
  }
}
#endregion

#region Enumerators
#region BoaCharEnumerator
public class BoaCharEnumerator : IEnumerator
{ public BoaCharEnumerator(string s) { str=s; index=-1; }

  public object Current
  { get
    { if(index<0 || index>=str.Length) throw new InvalidOperationException();
      return new string(str[index], 1);
    }
  }

  public bool MoveNext()
  { if(index>=str.Length-1) return false;
    index++;
    return true;
  }

  public void Reset() { index=-1; }
  
  string str;
  int index;
}
#endregion

#region IterEnumerator
public class IterEnumerator : IEnumerator
{ public IterEnumerator(object o)
  { iter = o;
    next = Ops.GetMember(o, "next");
    Ops.GetMember(o, "reset", out reset);
  }

  public object Current
  { get
    { if(state!=State.IN) throw new InvalidOperationException();
      return current;
    }
  }

  public bool MoveNext()
  { if(state==State.EOF) return false;
    try { current=Ops.Call(next); state=State.IN; return true; }
    catch(StopIterationException) { state=State.EOF; return false; }
  }

  public void Reset()
  { if(reset==null) throw new NotImplementedException("this iterator does not implement reset()");
    Ops.Call(reset);
    state = State.BOF;
  }

  enum State : byte { BOF, IN, EOF }
  object iter, current, next, reset;
  State state;
}
#endregion

#region SeqEnumerator
public class SeqEnumerator : IEnumerator
{ public SeqEnumerator(object seq)
  { length  = Ops.ToInt(Ops.Invoke(seq, "__length__"));
    getitem = Ops.GetMember(seq, "__getitem__");
    index   = -1;
  }

  public object Current
  { get
    { if(index<0 || index>=length) throw new InvalidOperationException();
      return current;
    }
  }
  
  public bool MoveNext()
  { if(index>=length-1) return false;
    current = Ops.Call(getitem, ++index);
    return true;
  }
  
  public void Reset() { index=-1; }

  object getitem, current;
  int index, length;
}
#endregion
#endregion

// TODO: redesign these interfaces
#region Interfaces
public interface ICallable
{ object Call(params object[] args);
}
public interface IFancyCallable : ICallable
{ object Call(object[] positional, string[] names, object[] values);
}

public interface IContainer
{ int __len__();
  bool __contains__(object value);
}

public interface IDescriptor
{ object __get__(object instance);
}

public interface IDataDescriptor : IDescriptor
{ void __set__(object instance, object value);
  void __delete__(object instance);
}

public interface IFile
{ bool canread { get; }
  bool canseek { get; }
  bool canwrite { get; }
  bool closed { get; }
  System.Text.Encoding encoding { get; set; }
  int  length { get; }
  void close();
  void flush();
  bool isatty();
  string next();
  byte[] read();
  byte[] read(int bytes);
  string readstr();
  string readstr(int bytes);
  int readbyte();
  string readline();
  string readline(int size);
  List readlines();
  List readlines(int sizehint);
  int seek(int offset);
  int seek(int offset, int whence);
  int tell();
  void truncate();
  void truncate(int size);
  void write(byte[] bytes);
  void write(string str);
  void writebyte(int value);
  void writelines(object sequence);
}

public interface IHasAttributes
{ List __attrs__();
  object __getattr__(string key);
  void __setattr__(string key, object value);
  void __delattr__(string key);
}

public interface IRepresentable
{ string __repr__();
}

public interface ISequence : IContainer
{ object __add__(object o);
  object __getitem__(int index);
  object __getitem__(Slice slice);
}

public interface IMutableSequence : ISequence
{ void __delitem__(int index);
  void __delitem__(Slice slice);
  void __setitem__(int index, object value);
  void __setitem__(Slice slice, object value);
}

public interface IMapping : IContainer
{ void clear();
  object copy();
  
  object get(object key);
  object get(object key, object defaultValue);

  bool has_key(object key);
  
  //static object fromkeys(object seq);
  //static object fromkeys(object seq, object value);

  object pop(object key);
  object pop(object key, object defaultValue);
  Tuple popitem();
  
  object setdefault(object key);
  object setdefault(object key, object defaultValue);
  
  void update(object dict);

  List items();
  List keys();
  List values();

  IEnumerator iteritems();
  IEnumerator iterkeys();
  IEnumerator itervalues();

  void __delitem__(object key);
  object __getitem__(object key);
  void __setitem__(object key, object value);
}
#endregion

#region Dict
public sealed class Dict : IDictionary, IRepresentable
{ public Dict() { dict=new ListDictionary(); isList=true; }
  public Dict(int count)
  { dict = count<=10 ? (IDictionary)new ListDictionary(ScriptComparer.Default)
                     : new Hashtable(count, BoaHash.Instance, ScriptComparer.Default);
    isList = count<=10;
  }
  public Dict(IDictionary d)
  { if(d.Count>10) d = new Hashtable(d, BoaHash.Instance, ScriptComparer.Default);
    else
    { dict = new ListDictionary(ScriptComparer.Default);
      foreach(DictionaryEntry de in d) dict.Add(de.Key, de.Value);
      isList = true;
    }
  }

  const int upThresh=12, downThresh=8;

  #region IDictionary Members
  public object this[object key]
  { get { return dict[key]; }
    set
    { int before = dict.Count;
      dict[key] = value;
      MaybeConvert(before);
    }
  }

  public bool IsFixedSize { get { return dict.IsFixedSize; } }
  public bool IsReadOnly { get { return dict.IsReadOnly; } }
  public ICollection Keys { get { return dict.Keys; } }
  public ICollection Values { get { return dict.Values; } }

  public void Add(object key, object value)
  { int before = dict.Count;
    dict.Add(key, value);
    MaybeConvert(before);
  }

  public void Clear()
  { if(dict.Count!=0)
    { dict = new ListDictionary(ScriptComparer.Default);
      isList = true;
    }
  }

  public bool Contains(object key) { return dict.Contains(key); }

  public IDictionaryEnumerator GetEnumerator() { return dict.GetEnumerator(); }

  public void Remove(object key)
  { int before = dict.Count;
    dict.Remove(key);
    MaybeConvert(before);
  }
  #endregion

  #region ICollection Members
  public int Count { get { return dict.Count; } }
  public bool IsSynchronized { get { return dict.IsSynchronized; } }
  public object SyncRoot { get { return this; } }

  public void CopyTo(Array array, int index) { dict.CopyTo(array, index); }
  #endregion

  #region IEnumerable Members
  IEnumerator System.Collections.IEnumerable.GetEnumerator() { return dict.GetEnumerator(); }
  #endregion

  #region IRepresentable Members

  public string __repr__()
  {
    // TODO:  Add Dict.__repr__ implementation
    return null;
  }

  #endregion

  void MaybeConvert(int before)
  { if(isList && before<upThresh && dict.Count>=upThresh)
    { dict = new Hashtable(dict, BoaHash.Instance, ScriptComparer.Default);
      isList = false;
    }
    else if(!isList && before>downThresh && dict.Count<=downThresh)
    { IDictionary nd = new ListDictionary(ScriptComparer.Default);
      foreach(DictionaryEntry de in dict) nd[de.Key] = de.Value;
      dict = nd;
      isList = true;
    }
  }

  IDictionary dict;
  bool isList;
}
#endregion

// FIXME: don't allow __repr__ to go into an infinite loop with circular references
#region List
public sealed class List : IMutableSequence, IList, IComparable, ICloneable, IRepresentable
{ public List() { items = new object[16]; }
  public List(int capacity) { items = new object[Math.Max(capacity, 4)]; }
  public List(ICollection c) : this(c.Count) { c.CopyTo(items, 0); size=c.Count; }
  public List(IEnumerator e) : this() { while(e.MoveNext()) append(e.Current); }
  public List(object o) : this(BoaOps.GetEnumerator(o)) { }
  internal List(object[] arr) { items=arr; size=arr.Length; }
  internal List(object[] arr, int length) { items=arr; size=length; }
  internal List(object[] arr, int start, int length)
  { if(start==0) { items=arr; size=length; }
    else
    { size  = length;
      items = new object[length];
      Array.Copy(arr, start, items, 0, length);
    }
  }

  public void append(object item)
  { ResizeTo(size+1);
    items[size++] = item;
  }

  public int count(object item)
  { int num=0;
    for(int i=0; i<size; i++) if(items[i].Equals(item)) num++;
    return num;
  }

  public void extend(object seq)
  { List list = this;
    if(!TryAppend(seq, ref list))
    { IEnumerator e = BoaOps.GetEnumerator(seq);
      while(e.MoveNext()) append(e.Current);
    }
  }

  public override bool Equals(object o) { return CompareTo(o)==0; }
  public override int GetHashCode() { throw Ops.TypeError("list objects are unhashable"); }

  public int index(object item) { return IndexOrError(IndexOf(item, 0, size)); }
  public int index(object item, int start)
  { start = BoaOps.FixIndex(start, size);
    return IndexOrError(IndexOf(item, start, size-start));
  }
  public int index(object item, int start, int end)
  { start = BoaOps.FixIndex(start, size);
    end   = BoaOps.FixIndex(end, size);
    if(start>end) { int t=end; end=start; start=t; }
    return IndexOrError(IndexOf(item, start, end-start));
  }

  public void insert(int index, object o) { Insert(index==size ? index : BoaOps.FixIndex(index, size), o); }

  public object pop()
  { if(size==0) throw Ops.ValueError("pop off of empty list");
    return items[--size];
  }

  public void remove(object item) { RemoveAt(index(item)); }

  public void RemoveRange(int start, int count)
  { if(start<0 || start+count>size) throw Ops.ValueError("RemoveRange(): indices out of range");
    size -= count;
    for(; start<size; start++) items[start] = items[start+count];
    for(int i=0; i<count; i++) items[size+i] = null;
  }
  
  public void reverse() { Array.Reverse(items, 0, size); }

  public void sort() { Array.Sort(items, 0, size, ScriptComparer.Default); }
  public void sort(IProcedure cmpfunc) { Array.Sort(items, 0, size, new FunctionComparer(cmpfunc)); }

  public List sorted()
  { List ret = new List(this);
    ret.sort();
    return ret;
  }

  public List sorted(IProcedure cmpfunc)
  { List ret = new List(this);
    ret.sort(cmpfunc);
    return ret;
  }

  public Tuple ToTuple()
  { object[] ti = new object[size];
    items.CopyTo(ti, 0);
    return new Tuple(ti);
  }

  #region IMutableSequence Members
  public object __add__(object o)
  { List list=null;
    if(!TryAppend(o, ref list))
      throw Ops.TypeError("can not concatenate list to ('{0}')", Ops.TypeName(o));
    return list;
  }

  public bool __contains__(object value) { return IndexOf(value, 0, size) != -1; }

  public void __delitem__(int index) { RemoveAt(BoaOps.FixIndex(index, size)); }
  public void __delitem__(Slice slice)
  { Tuple tup = slice.indices(size);
    int start=(int)tup.items[0], stop=(int)tup.items[1], step=(int)tup.items[2];
    if(step<0 && start<=stop || step>0 && start>=stop) return;

    if(step==1) RemoveRange(start, stop-start);
    else if(step==-1) RemoveRange(stop+1, start-stop);
    else
    { int off=1;
      for(int i=start,next=start+step; i<size; i++)
      { if(i+off==next) { if((next+=step)>=stop) next=0; else off++; }
        items[i] = items[i+off];
      }
      size -= off;
    }
  }

  public object __getitem__(int index) { return items[BoaOps.FixIndex(index, size)]; }
  public object __getitem__(Slice slice) { return BoaOps.SequenceSlice(this, slice); }

  public int __len__() { return size; }

  public void __setitem__(int index, object value) { items[BoaOps.FixIndex(index, size)] = value; }
  public void __setitem__(Slice slice, object value)
  { Tuple tup = slice.indices(size);
    int start=(int)tup.items[0], stop=(int)tup.items[1], step=(int)tup.items[2];
    if(step<0 && start<=stop || step>0 && start>=stop) return;
    int sign=Math.Sign(step), slen=(stop-start+step-sign)/step;

    ISequence seq = value as ISequence;
    if(seq==null && value is string) seq = new StringOps.SequenceWrapper((string)value);

    int len = seq==null ? Ops.ToInt(Ops.Invoke(value, "__len__")) : seq.__len__();
    if(step==1 || step==-1)
    { int diff = Math.Abs(len-slen);
      if(step==1)
      { if(len<slen) RemoveRange(start+len, diff);
        else if(len>slen) Insert(start+slen, diff);
      }
      else if(len>slen) { Insert(stop+1, diff); start += diff; }
      else if(len<slen) { RemoveRange(stop+1, diff); start -= diff; }
    }
    else if(len!=slen)
      throw Ops.ValueError("can't assign sequence of size {0} to extended slice of size {1}", len, slen);

    if(seq!=null)
    { if(step==1) for(int i=0; i<len; i++) items[i+start] = seq.__getitem__(i);
      else if(step==-1) for(int i=0; i<len; i++) items[start-i] = seq.__getitem__(i);
      else if(step>0) for(int i=0; start<stop; i++,start+=step) items[start] = seq.__getitem__(i);
      else for(int i=0; start>stop; i++,start+=step) items[start] = seq.__getitem__(i);
    }
    else
    { object getitem = Ops.GetMember(value, "__getitem__");
      if(step==1) for(int i=0; i<len; i++) items[i+start] = Ops.Call(getitem, i);
      else if(step==-1) for(int i=0; i<len; i++) items[start-i] = Ops.Call(getitem, i);
      else if(step>0) for(int i=0; start<stop; i++,start+=step) items[start] = Ops.Call(getitem, i);
      else for(int i=0; start>stop; i++,start+=step) items[start] = Ops.Call(getitem, i);
    }
  }
  #endregion

  #region IList Members
  public bool IsReadOnly { get { return false; } }
  public object this[int index]
  { get
    { if(index<0 || index>=size) throw new IndexOutOfRangeException();
      return items[index];
    }
    set
    { if(index<0 || index>=size) throw new IndexOutOfRangeException();
      items[index] = value;
    }
  }

  public void RemoveAt(int index)
  { if(index<0 || index>=size) throw new IndexOutOfRangeException();
    size--;
    for(; index<size; index++) items[index] = items[index+1];
    items[size] = null;
  }

  public void Insert(int index, object value)
  { if(index<0 || index>size) throw new IndexOutOfRangeException();
    ResizeTo(size+1);
    for(int i=size++; i>index; i--) items[i] = items[i-1];
    items[index] = value;
  }

  public void Remove(object value)
  { int index = IndexOf(value);
    if(index!=-1) RemoveAt(index);
  }

  public bool Contains(object value) { return __contains__(value); }
  public void Clear() { size=0; }

  public int IndexOf(object value) { return IndexOf(value, 0, size); }
  public int IndexOf(object value, int start, int length)
  { int end = start+length;
    if(start<0 || end>size) throw new ArgumentOutOfRangeException();
    for(; start<end; start++) if(Ops.Compare(items[start], value)==0) return start;
    return -1;
  }

  public int Add(object value) { append(value); return size-1; }
  public bool IsFixedSize { get { return false; } }
  #endregion

  #region ICollection Members
  public bool IsSynchronized { get { return false; } }
  public int Count { get { return size; } }
  public void CopyTo(Array array, int index) { Array.Copy(items, 0, array, index, size); }
  public object SyncRoot { get { return this; } }
  #endregion

  #region IEnumerable Members
  public IEnumerator GetEnumerator() { return new ListEnumerator(this); }
  
  class ListEnumerator : IEnumerator
  { public ListEnumerator(List list) { this.list=list; index=-1; }

    public void Reset() { index=-1; }

    public object Current
    { get
      { if(index<0 || index>=list.size) throw new InvalidOperationException();
        return list.items[index];
      }
    }

    public bool MoveNext()
    { if(index>=list.size-1) return false;
      index++;
      return true;
    }

    List list;
    int index;
  }
  #endregion

  #region IComparable Members
  public int CompareTo(object o)
  { List list = o as List;
    if(list!=null) return ArrayOps.Compare(items, size, list.items, list.size);
    else return Ops.TypeName(this).CompareTo(Ops.TypeName(o));
  }
  #endregion

  #region ICloneable Members
  public object Clone() { return new List(this); }
  #endregion

  #region IRepresentable Members
  public string __repr__()
  { System.Text.StringBuilder sb = new System.Text.StringBuilder();
    sb.Append('[');
    for(int i=0; i<size; i++)
    { if(i>0) sb.Append(", ");
      sb.Append(Ops.Repr(items[i]));
    }
    sb.Append(']');
    return sb.ToString();
  }
  #endregion

  public override string ToString() { return __repr__(); }
  
  sealed class FunctionComparer : IComparer
  { public FunctionComparer(IProcedure func) { this.func=func; }
    public int Compare(object x, object y) { return Ops.ToInt(func.Call(x, y)); }
    IProcedure func;
  }

  int IndexOrError(int index)
  { if(index<0) throw Ops.ValueError("item not in list");
    return index;
  }

  void Insert(int start, int count)
  { ResizeTo(size+count);
    for(int i=size-1; i>=start; i--) items[i+count] = items[i];
    size += count;
  }

  void ResizeTo(int capacity)
  { if(capacity>items.Length)
    { int len = Math.Max(items.Length, 4);
      while(len<capacity) len*=2;
      object[] arr = new object[len];
      Array.Copy(items, arr, size);
      items = arr;
    }
  }

  bool TryAppend(object o, ref List list)
  { ICollection col = o as ICollection;
    if(col!=null)
    { if(list==null)
      { list = new List(size+col.Count);
        Array.Copy(items, list.items, size);
      }
      else list.ResizeTo(size+col.Count);
      col.CopyTo(list.items, size);
      list.size = size+col.Count;
    }
    else
    { IEnumerator e;
      if(BoaOps.GetEnumerator(o, out e))
      { if(list==null) list = new List(this);
        while(e.MoveNext()) list.append(e.Current);
      }
      else return false;
    }
    return true;
  }

  internal static readonly List Empty = new List(Ops.EmptyArray);

  object[] items;
  int size;
}
#endregion

#region Slice
public sealed class Slice : IRepresentable
{ public Slice() { }
  public Slice(object stop) { this.stop=stop; }
  public Slice(object start, object stop) { this.start=start; this.stop=stop; }
  public Slice(object start, object stop, object step)
  { if(step!=null && Ops.ToInt(step)==0) throw Ops.ValueError("slice(): step cannot be zero");
    this.start=start; this.stop=stop; this.step=step;
  }

  public override bool Equals(object obj)
  { Slice o = obj as Slice;
    return o!=null && (start==null ? o.start==null : Ops.AreEqual(start, o.start)) &&
                      (stop ==null ? o.stop ==null : Ops.AreEqual(stop, o.stop))  &&
                      (step ==null ? o.step ==null : Ops.AreEqual(step, o.step));
  }

  public override int GetHashCode()
  { return (start==null ? 0 : start.GetHashCode()) ^ (stop==null ? 0 : stop.GetHashCode());
  }

  public Tuple indices(int length)
  { int step  = (this.step==null ? 1 : Ops.ToInt(this.step));
    int start = (this.start==null ? step>0 ? 0 : length-1 : BoaOps.FixSliceIndex(Ops.ToInt(this.start), length));
    int stop  = (this.stop==null ? step>0 ? length : -1 : BoaOps.FixSliceIndex(Ops.ToInt(this.stop), length));
    return new Tuple(start, stop, step);
  }

  public override string ToString() { return __repr__(); }

  public string __repr__()
  { return string.Format("slice({0}, {1}, {2})", Ops.Repr(start), Ops.Repr(stop), Ops.Repr(step));
  }

  public readonly object start, stop, step;
}
#endregion

#region StringOps
public sealed class StringOps
{ StringOps() { }

  #region SequenceWrapper
  public class SequenceWrapper : ISequence
  { public SequenceWrapper(string str) { this.str = str; }

    #region ISequence Members
    public object __add__(object o) { throw Ops.TypeError("strings are immutable"); }
    public object __getitem__(int index) { return new string(str[BoaOps.FixIndex(index, str.Length)], 1); }
    object ISequence.__getitem__(Slice slice) { return StringOps.Slice(str, slice); }
    public int __len__() { return str.Length; }
    public bool __contains__(object value)
    { if(value is string)
      { string needle = (string)value;
        return (needle.Length==0 ? str.IndexOf(needle[0]) : str.IndexOf(needle)) != -1;
      }
      if(value is char) return str.IndexOf((char)value)!=-1;
      return false;
    }
    #endregion
    
    string str;
  }

  #endregion

  #region StringFormatter
  sealed class StringFormatter
  { public StringFormatter(string source, object args) { this.source=source; this.args=args; tup=args as Tuple; }

    public string Format()
    { MatchCollection matches = StringOps.printfre.Matches(source);
      Code[] formats = new Code[matches.Count];

      int argwant=0, arggot, pos=0;
      bool dict=false, nodict=false;
      for(int i=0; i<formats.Length; i++)
      { formats[i] = new Code(matches[i]);
        argwant += formats[i].Args;
        if(formats[i].Key==null) nodict=true;
        else if(formats[i].Length==-2 || formats[i].Precision==-2) nodict=true;
        else dict=true;
      }
      if(dict && nodict) throw Ops.TypeError("keyed format codes and non-keyed format codes (or codes with a "+
                                             "length/precision of '#') mixed in format string");

      arggot = tup==null ? 1 : tup.items.Length;
      if(tup==null && dict) getitem = Ops.GetMember(args, "__getitem__");
      else if(dict) throw Ops.TypeError("format requires a mapping");
      else if(argwant!=arggot) throw Ops.TypeError("incorrect number of arguments for string formatting "+
                                                   "(expected {0}, but got {1})", argwant, arggot);

      System.Text.StringBuilder sb = new System.Text.StringBuilder();
      for(int fi=0; fi<formats.Length; fi++)
      { Code f = formats[fi];
        if(f.Match.Index>pos) sb.Append(source.Substring(pos, f.Match.Index-pos));
        pos = f.Match.Index+f.Match.Length;        

        if(f.Length==-2) f.Length = Ops.ToInt(NextArg());
        if(f.Precision==-2) f.Precision = Ops.ToInt(NextArg());
        
        char type = f.Type;
        switch(type) // TODO: support long integers
        { case 'd': case 'i': case 'u': case 'x': case 'X':
          { long i = Ops.ToLong(GetArg(f.Key));
            string s = null;
            bool neg, prefix=false, althex=false;
            if(type=='d' || type=='i') { int iv=(int)i; s=iv.ToString(); neg=prefix=iv<0; }
            else
            { uint ui = (uint)i;
              if(type=='u') s=ui.ToString();
              else if(type=='x' || type=='X')
              { s = ui.ToString("x");
                althex = f.HasFlag('#');
              }
              neg = false;
            }

            int ptype = f.HasFlag('+') ? 1 : f.HasFlag(' ') ? 2 : 0;
            if(!neg && ptype!=0) { s=(ptype==1 ? '+' : ' ') + s; prefix=true; }
            if(althex) f.Length -= 2;
            if(f.Length>0 && s.Length<f.Length)
            { if(f.HasFlag('-')) s = s.PadRight(f.Length, ' ');
              else if(f.HasFlag('0'))
              { if(prefix) s = s[0] + s.Substring(1).PadLeft(f.Length-1, '0');
                else s = s.PadLeft(f.Length, '0');
              }
              else
              { if(althex) { s = "0x"+s; prefix=true; f.Length += 2; }
                s = s.PadLeft(f.Length, ' ');
              }
            }
            if(!prefix && althex) s = "0x"+s;
            if(type=='x') s = s.ToLower();
            else if(type=='X') s = s.ToUpper();
            sb.Append(s);
            break;
          }

          case 'o': throw new NotImplementedException();
          case 'e': case 'E': case 'f': case 'F': case 'g': case 'G':
          { string fmt = f.Match.Groups[5].Value;
            string s;
            double d = Ops.ToFloat(GetArg(f.Key));
            bool neg = d<0;
            if((type=='f' || type=='F') && f.Precision<0)
            { s = d.ToString("f15");
              if(s.IndexOf('.')!=-1)
              { s = s.TrimEnd('0');
                if(s[s.Length-1]=='.') s = s.Substring(0, s.Length-1);
              }
            }
            else s = d.ToString(f.Precision>0 ? fmt+f.Precision : fmt);

            if(!neg)
            { if(f.HasFlag('+')) s = '+'+s;
              else if(f.HasFlag(' ')) s = ' '+s;
            }
            if(f.Length>0 && s.Length<f.Length)
            { if(f.HasFlag('-')) s = s.PadRight(f.Length, ' ');
              else if(f.HasFlag('0')) throw new NotImplementedException(); //s = Boa.Modules._string.zfill(s, f.Length);
              else s = s.PadLeft(f.Length, ' ');
            }
            sb.Append(s);
            break;
          }

          case 'c':
          { object c = GetArg(f.Key);
            string s = c as string;
            sb.Append(s==null ? (char)Ops.ToInt(c) : s[0]);
            break;
          }

          case 'r': sb.Append(Ops.Repr(GetArg(f.Key))); break;
          case 's': sb.Append(Ops.Str(GetArg(f.Key))); break;
          case '%': sb.Append('%'); break;
          default: throw Ops.ValueError("unsupported format character '{0}' (0x{1:X})", f.Type, (int)f.Type);
        }
      }
      if(pos<source.Length) sb.Append(source.Substring(pos));
      return sb.ToString();
    }
    
    #region Code
    struct Code
    { public Code(Match m)
      { Match     = m;
        Length    = m.Groups[3].Success ? m.Groups[3].Value=="*" ? -2 : Ops.ToInt(m.Groups[3].Value) : -1;
        Precision = m.Groups[4].Success ? m.Groups[4].Value=="*" ? -2 : Ops.ToInt(m.Groups[4].Value) : -1;
      }

      public int    Args  { get { return 1 + (Length==-2 ? 1 : 0) + (Precision==-2 ? 1 : 0); } }
      public string Flags { get { return Match.Groups[2].Value; } }
      public string Key   { get { return Match.Groups[1].Success ? Match.Groups[1].Value : null; } }
      public char   Type  { get { return Match.Groups[5].Value[0]; } }
      
      public bool HasFlag(char c) { return Flags.IndexOf(c)!=-1; }

      public Match Match;
      public int   Length, Precision;
    }
    #endregion
    
    object GetArg(string name) { return name==null ? NextArg() : Ops.Call(getitem, name); }
    object NextArg() { return tup==null ? args : tup.items[argi++]; }

    string source;
    int argi;
    Tuple tup;
    object args, getitem;
  }
  #endregion

  public static string Escape(string s)
  { StringBuilder sb = new StringBuilder(s.Length+10);
    char quote = '\'';
    if(s.IndexOf('\'')!=-1 && s.IndexOf('\"')==-1) quote = '\"';
    sb.Append(quote);
    for(int i=0; i<s.Length; i++)
    { char c = s[i];
      switch(c)
      { case '\\': sb.Append(@"\\"); break;
        case '\t': sb.Append(@"\t"); break;
        case '\n': sb.Append(@"\n"); break;
        case '\r': sb.Append(@"\r"); break;
        case (char)27: sb.Append(@"\e"); break;
        case '\a': sb.Append(@"\a"); break;
        case '\f': sb.Append(@"\f"); break;
        case '\v': sb.Append(@"\v"); break;
        default: 
          if(c==quote) { sb.Append('\\').Append(c); }
          else if(c<32 || c>=0x7f) sb.AppendFormat(c>0xff ? @"\x{0:x4}" : @"\x{0:x2}", (int)c);
          else sb.Append(c);
          break;
      }
    }
    sb.Append(quote);
    return sb.ToString();
  }
  
  public static string Multiply(string str, object times)
  { int n = Ops.ToInt(times);
    StringBuilder sb = new StringBuilder(str.Length*n);
    while(n-->0) sb.Append(str);
    return sb.ToString();
  }

  public static string PrintF(string format, object args) { return new StringFormatter(format, args).Format(); }

  public static string Slice(string s, Slice slice)
  { Tuple tup = slice.indices(s.Length);
    int start=(int)tup.items[0], stop=(int)tup.items[1], step=(int)tup.items[2], sign=Math.Sign(step);
    if(step<0 && start<=stop || step>0 && start>=stop) return string.Empty;
    if(step==1) return s.Substring(start, stop-start);
    else
    { StringBuilder sb = new StringBuilder((stop-start+step-sign)/step);
      if(step<0) for(; start>stop; start+=step) sb.Append(s[start]);
      else for(; start<stop; start+=step) sb.Append(s[start]);
      return sb.ToString();
    }
  }
  
  // keep in sync with Parser.GetEscapeChar()
  public static string Unescape(string s) { return Unescape(s, null); }
  public static string Unescape(string s, System.Text.RegularExpressions.Match m)
  { StringBuilder sb = new StringBuilder();
    char lastChar='\0';
    for(int pos=0; pos<s.Length || lastChar!=0; )
    { char c;
      if(lastChar==0) c = s[pos++];
      else { c=lastChar; lastChar='\0'; }

      if(c!='\\') sb.Append(c);
      else
      { c = ReadChar(s, ref pos);
        if(char.IsDigit(c))
        { int num = c-'0';
          if(m==null)
          { if(c>'7') throw Ops.ValueError("invalid octal digit near string index {0}", pos);
            for(int i=1; i<3; i++)
            { c = ReadChar(s, ref pos);
              if(!char.IsDigit(c) || c>'7') { lastChar=c; break; }
              num = (num<<3) | (c-'0');
            }
            sb.Append((char)num);
          }
          else
          { while(true)
            { c = ReadChar(s, ref pos);
              if(!char.IsDigit(c)) { lastChar=c; break; }
              num = num*10 + (c-'0');
            }
            if(num<m.Groups.Count && m.Groups[num].Success) sb.Append(m.Groups[num].Value);
            else throw Ops.ValueError("reference to group {0} found near string index {1}, "+
                                      "but there are only {2} groups", num, pos, m.Groups.Count);
          }
        }
        else switch(c)
        { case '\0': throw Ops.ValueError("unterminated escape sequence");
          case 'n': sb.Append('\n'); break;
          case 't': sb.Append('\t'); break;
          case 'g':
            if(m==null) sb.Append(c);
            else
            { c = ReadChar(s, ref pos);
              if(c=='<') lastChar=c;
              else
              { string name = string.Empty;
                while((c=ReadChar(s, ref pos))!='>' && c!=0) name += c;
                if(c==0) throw Ops.ValueError("unterminated group name near string index {0}", pos);
                System.Text.RegularExpressions.Group g = m.Groups[name];
                if(g==null) throw Ops.ValueError("nonexistant group '{0}' referenced near string index {1}", pos);
                if(g.Success) sb.Append(g.Value);
              }
            }
            break;
          case 'r': sb.Append('\r'); break;
          case 'b': sb.Append('\b'); break;
          case 'e': sb.Append((char)27); break;
          case 'a': sb.Append('\a'); break;
          case 'f': sb.Append('\f'); break;
          case 'v': sb.Append('\v'); break;
          case 'x': case 'u':
          { int num = 0;
            for(int i=0,limit=(c=='x'?2:4); i<limit; i++)
            { c = ReadChar(s, ref pos);
              if(char.IsDigit(c)) num = (num<<4) | (c-'0');
              else if((c<'A' || c>'F') && (c<'a' || c>'f'))
              { if(i==0) throw Ops.ValueError("expected hex digit near string index {0}", pos);
                lastChar = c;
                break;
              }
              num = (num<<4) | (char.ToUpper(c)-'A'+10);
            }
            sb.Append((char)num);
            break;
          }
          case 'c':
            c = ReadChar(s, ref pos);
            if(!char.IsLetter(c)) throw Ops.ValueError("expected letter for \\c near string index {0}", pos);
            sb.Append((char)(char.ToUpper(c)-64));
            break;
          default: sb.Append(c); break;
        }
      }
    }
    if(lastChar!=0) sb.Append(lastChar);
    return sb.ToString();
  }

  static char ReadChar(string str, ref int pos) { return pos>=str.Length ? '\0' : str[pos++]; }

  static readonly Regex printfre =
    new Regex(@"%(?:\(([^)]+)\))?([#0 +-]*)(\d+|\*)?(?:.(\d+|\*))?[hlL]?(.)",
              RegexOptions.Compiled|RegexOptions.Singleline);
}

#endregion

// FIXME: don't allow __repr__ to go into an infinite loop with circular references
#region Tuple
public sealed class Tuple : ISequence, IList, IComparable, IRepresentable
{ public Tuple() { items = Ops.EmptyArray; }
  public Tuple(object obj)
  { ICollection col = obj as ICollection;
    if(col!=null)
    { items = new object[col.Count];
      col.CopyTo(items, 0);
    }
    else
    { List list = new List(obj);
      items = new object[list.Count];
      list.CopyTo(items, 0);
    }
  }
  internal Tuple(ICollection col)
  { items = new object[col.Count];
    col.CopyTo(items, 0);
  }
  internal Tuple(params object[] items) { this.items=items; }

  #region ISequence Members
  public object __add__(object o)
  { Tuple tup = o as Tuple;
    if(tup==null) throw Ops.TypeError("cannot concatenate tuple to {0}", Ops.TypeName(o));
    object[] arr = new object[items.Length+tup.items.Length];
    items.CopyTo(arr, 0);
    tup.items.CopyTo(arr, items.Length);
    return new Tuple(arr);
  }

  public object __getitem__(int index) { return items[BoaOps.FixIndex(index, items.Length)]; }
  public object __getitem__(Slice slice) { return BoaOps.SequenceSlice(this, slice); }
  public int __len__() { return items.Length; }
  public bool __contains__(object value)
  { for(int i=0; i<items.Length; i++) if(Ops.Compare(items[i], value)==0) return true;
    return false;
  }
  #endregion

  #region ICollection Members
  public bool IsSynchronized { get { return items.IsSynchronized; } }
  public int Count { get { return items.Length; } }
  public void CopyTo(Array array, int index) { items.CopyTo(array, index); }
  public object SyncRoot { get { return items; } }
  #endregion

  #region IList Members
  public object this[int index]
  { get { return items[BoaOps.FixIndex(index, items.Length)]; }
    set { throw ImmutableError(); }
  }

  public bool IsFixedSize { get { return true; } }
  public bool IsReadOnly { get { return true; } }

  public int Add(object o) { throw ImmutableError(); }

  public bool Contains(object o)
  { foreach(object i in items) if(Ops.AreEqual(o, i)) return true;
    return false;
  }
  
  public void Clear() { throw ImmutableError(); }

  public int IndexOf(object o)
  { for(int i=0; i<items.Length; i++) if(Ops.AreEqual(o, items[i])) return i;
    return -1;
  }
  
  public void Insert(int index, object o) { throw ImmutableError(); }
  public void Remove(object o) { throw ImmutableError(); }
  public void RemoveAt(int index) { throw ImmutableError(); }
  #endregion

  #region IEnumerable Members
  public IEnumerator GetEnumerator() { return items.GetEnumerator(); }
  #endregion

  #region IComparable Members
  public int CompareTo(object o)
  { Tuple tup = o as Tuple;
    if(tup!=null) return ArrayOps.Compare(items, items.Length, tup.items, tup.items.Length);
    else return Ops.TypeName(this).CompareTo(Ops.TypeName(o));
  }
  #endregion

  #region IRepresentable Members
  public string __repr__()
  { System.Text.StringBuilder sb = new System.Text.StringBuilder();
    sb.Append('(');
    for(int i=0; i<items.Length; i++)
    { if(i>0) sb.Append(", ");
      sb.Append(Ops.Repr(items[i]));
    }
    if(items.Length==1) sb.Append(',');
    sb.Append(')');
    return sb.ToString();
  }
  #endregion

  public override bool Equals(object o) { return CompareTo(o)==0; }

  public override int GetHashCode()
  { if(hashCode!=null) return (int)hashCode;

    int hash=0;
    foreach(object o in items) if(o!=null) hash ^= o.GetHashCode();
    hashCode = hash;
    return hash;
  }

  public override string ToString() { return __repr__(); }

  public static readonly Tuple Empty = new Tuple();
  
  internal readonly object[] items;
  object hashCode;

  static Exception ImmutableError() { return new InvalidOperationException("Tuples cannot be modified"); }
}
#endregion

} // namespace Boa.Backend
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
using System.Collections.Specialized;
using System.Globalization;
using System.Text;
using System.Text.RegularExpressions;
using Scripting;
using Scripting.Backend;

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
    throw Ops.ArgError("invalid operand types for sequence comparison: '{0}' and '{1}'",
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
    throw Ops.ArgError("invalid operand types for sequence concatenation: '{0}' and '{1}'",
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
        if(ic==null) return Ops.InvokeProperty(b, "__rmul__", a);
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
      default: throw Ops.ArgError("invalid operand types for sequence multiplication: '{0}' and '{1}'",
                                  Ops.TypeName(a), Ops.TypeName(b));
    }
    
    if(bv<0) throw new ArgumentException("multiplier for sequence multiplication cannot be negative");
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
  
  public static string ToCode(Array arr)
  { System.Text.StringBuilder sb = new System.Text.StringBuilder();
    sb.Append(arr.GetType().FullName);
    sb.Append('(');
    for(int i=0; i<arr.Length; i++)
    { if(i>0) sb.Append(", ");
      sb.Append(Ops.ToCode(arr.GetValue(i)));
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
    else throw new ArgumentException(Ops.TypeName(o)+" cannot be converted to an array");
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

  public static System.IO.Stream ExpectFile(object obj)
  { System.IO.Stream stream = obj as System.IO.Stream;
    if(stream==null)
      throw new ArgumentException("Expected object of type System.IO.Stream, but received "+Ops.TypeName(obj));
    return stream;
  }

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
    else { e=null; return false; }
    return true;
  }
  
  public static object GetIndex(object obj, object index)
  { Slice slice = index as Slice;
    if(slice==null)
    { string s = obj as string;
      if(s!=null) return new string(s[FixIndex(Ops.ToInt(index), s.Length)], 1);
      return Ops.GetIndex(obj, index);
    }
    else
    { ISliceable seq = obj as ISliceable;
      if(seq!=null) return seq.GetSlice(slice);
      string s = obj as string;
      if(s!=null) return StringOps.Slice(s, slice);
      throw new ArgumentException("objects of type '"+Ops.TypeName(obj)+"' cannot be sliced");
    }
  }

  public static void SetIndex(object value, object obj, object index)
  { Slice slice = index as Slice;
    if(slice==null) Ops.SetIndex(value, obj, index);
    else
    { IMutableSliceable seq = obj as IMutableSliceable;
      if(seq==null) throw new ArgumentException("objects of type '"+Ops.TypeName(obj)+"' cannot be on the left hand "+
                                                "side of slice assignment");
      seq.SetSlice(slice, value);
    }
  }

  public static ICollection GetSliceableCollection(object obj)
  { ICollection col = obj as ICollection;
    if(col!=null) return col;
    string s = obj as string;
    if(s!=null) return new StringOps.SequenceWrapper(s);
    throw new ArgumentException("slice assignment expects a string or a collection");
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

  public static bool IsTrue(object value)
  { switch(Convert.GetTypeCode(value))
    { case TypeCode.Boolean: return (bool)value;
      case TypeCode.Byte:    return (byte)value!=0;
      case TypeCode.Char:    return (char)value!=0;
      case TypeCode.Decimal: return (Decimal)value!=0;
      case TypeCode.Double:  return (double)value!=0;
      case TypeCode.Empty:   return false;
      case TypeCode.Int16:   return (short)value!=0;
      case TypeCode.Int32:   return (int)value!=0;
      case TypeCode.Int64:   return (long)value!=0;
      case TypeCode.Object:
        if(value is Integer) return (Integer)value!=0;
        if(value is Complex) return ComplexOps.NonZero((Complex)value);
        if(value is ICollection) return ((ICollection)value).Count>0;
        return true;
      case TypeCode.SByte:  return (sbyte)value!=0;
      case TypeCode.Single: return (float)value!=0;
      case TypeCode.String: return ((string)value).Length>0;
      case TypeCode.UInt16: return (short)value!=0;
      case TypeCode.UInt32: return (uint)value!=0;
      case TypeCode.UInt64: return (ulong)value!=0;
    }
    return true;
  }

  public static IEnumerator PrepareTupleAssignment(object value, int items)
  { if(value is ICollection)
    { ICollection col = (ICollection)value;
      return col.GetEnumerator();
    }
    else if(value is string)
    { string s = (string)value;
      if(s.Length==items) return new BoaCharEnumerator(s);
    }
    else throw new ArgumentException("objects of type '"+Ops.TypeName(value)+"' cannot be on the right hand side of "+
                                     "a tuple assignment");

    throw new ArgumentException("wrong number of values to unpack");
  }

  public static void Print(object file, object o)
  { string str = Ops.Str(o);
    if(file==null) { Console.Write(str); return; }

    byte[] data = System.Text.Encoding.Default.GetBytes(str);
    ExpectFile(file).Write(data, 0, data.Length);
  }

  public static void PrintNewline(object file)
  { if(file==null) { Console.WriteLine(); return; }
    ExpectFile(file).WriteByte((byte)'\n');
  }

  public static List SequenceSlice(IList list, Slice slice)
  { int start, stop, step;
    slice.GetIndices(list.Count, out start, out stop, out step);
    return SequenceSlice(list, start, stop, step);
  }

  public static List SequenceSlice(IList list, int start, int stop, int step)
  { if(step<0 && start<=stop || step>0 && start>=stop) return new List();
    int sign = Math.Sign(step);
    List ret = new List((stop-start+step-sign)/step);
    if(step<0) for(; start>stop; start+=step) ret.Add(list[start]);
    else for(; start<stop; start+=step) ret.Add(list[start]);
    return ret;
  }
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
  public string ToCode() { throw new NotImplementedException(); }
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

  public void Reset() { index = -1; }
  
  string str;
  int index;
}
#endregion

#region Interfaces
public interface IRepresentable { string ToCode(); }

public interface ISliceable : ICollection
{ ICollection GetSlice(Slice slice);
}

public interface IMutableSliceable : ISliceable
{ void SetSlice(Slice slice, object collection);
  void RemoveAt(Slice slice);
}
#endregion

// FIXME: don't allow ToCode to go into an infinite loop with circular references
#region List
public sealed class List : IMutableSliceable, IList, IComparable, ICloneable, IRepresentable
{ public List() { items = new object[16]; }
  public List(int capacity) { items = new object[Math.Max(capacity, 4)]; }
  public List(ICollection c) : this(c.Count) { c.CopyTo(items, 0); count = c.Count; }
  public List(IEnumerator e) : this() { while(e.MoveNext()) Add(e.Current); }
  public List(object o) : this(BoaOps.GetEnumerator(o)) { }
  internal List(object[] arr) { items=arr; count=arr.Length; }
  internal List(object[] arr, int length) { items=arr; count=length; }
  internal List(object[] arr, int start, int length)
  { if(start==0) { items=arr; count=length; }
    else
    { count = length;
      items = new object[length];
      Array.Copy(arr, start, items, 0, length);
    }
  }

  public void AddRange(ICollection col)
  { ResizeTo(count+col.Count);
    col.CopyTo(items, count);
    count += col.Count;
    version++;
  }

  public override bool Equals(object o) { return CompareTo(o)==0; }
  public override int GetHashCode() { throw new InvalidOperationException("list objects are unhashable"); }

  public object Pop()
  { if(count==0) throw new InvalidOperationException("Cannot pop from an empty list");
    version++;
    return items[--count];
  }

  public void RemoveRange(int start, int length)
  { if(length!=0)
    { start = BoaOps.FixIndex(start, count);
      if(start<0 || start+length>count) throw new ArgumentException("indices out of range");
      count -= length;
      for(; start<count; start++) items[start] = items[start+length];
      for(int i=0; i<length; i++) items[count+i] = null;
      version++;
    }
  }

  public void Reverse() { Array.Reverse(items, 0, count); version++; }

  public void Sort() { Array.Sort(items, 0, count, ScriptComparer.Default); version++; }
  public void Sort(IProcedure cmpfunc) { Array.Sort(items, 0, count, new FunctionComparer(cmpfunc)); version++; }

  public List Sorted()
  { List ret = new List(this);
    ret.Sort();
    return ret;
  }

  public List Sorted(IProcedure cmpfunc)
  { List ret = new List(this);
    ret.Sort(cmpfunc);
    return ret;
  }

  public Tuple ToTuple()
  { object[] ti = new object[count];
    Array.Copy(items, ti, count);
    return Tuple.Make(ti);
  }

  #region IMutableSliceable Members
  public ICollection GetSlice(Slice slice)
  { int start, stop, step;
    slice.GetIndices(count, out start, out stop, out step);
    if(step==1)
    { object[] arr = new object[stop-start];
      Array.Copy(items, start, arr, 0, stop-start);
      return new List(arr);
    }
    else if(step==-1)
    { object[] arr = new object[start-stop];
      for(int i=0; i<arr.Length; i++) arr[i] = items[start-i];
      return new List(arr);
    }
    else return BoaOps.SequenceSlice(this, start, stop, step);
  }

  public void SetSlice(Slice slice, object collection)
  { ICollection col = BoaOps.GetSliceableCollection(collection);
    int start, stop, step;
    slice.GetIndices(count, out start, out stop, out step);
    int sign = Math.Sign(step), sliceLen = (stop-start+step-sign)/step, colLen = col.Count;

    if(step==1)
    { if(colLen<sliceLen) RemoveRange(start, sliceLen-colLen);
      else if(colLen>sliceLen) InsertRange(start, colLen-sliceLen);
      col.CopyTo(items, start);
    }
    else if(step==-1)
    { start = stop+1;
      if(colLen<sliceLen) RemoveRange(start, sliceLen-colLen);
      else if(colLen>sliceLen) InsertRange(start, colLen-sliceLen);
      col.CopyTo(items, start);
      Array.Reverse(items, start, colLen);
    }
    else if(colLen!=sliceLen)
      throw new ArgumentException(string.Format("Can't assign sequence of size {0} to extended slice of size {1}",
                                                colLen, sliceLen));
    else
    { IList list = col as IList;
      if(list!=null)
      { if(step>0) for(int i=0; start<stop; i++,start+=step) items[start] = list[i];
        else for(int i=0; start>stop; i++,start+=step) items[start] = list[i];
      }
      else
      { object[] arr = new object[colLen];
        col.CopyTo(arr, 0);
        if(step>0) for(int i=0; start<stop; i++,start+=step) items[start] = arr[i];
        else for(int i=0; start>stop; i++,start+=step) items[start] = arr[i];
      }
    }

    version++;
  }

  public void RemoveAt(Slice slice)
  { int start, stop, step;
    slice.GetIndices(count, out start, out stop, out step);
    if(step<0 && start<=stop || step>0 && start>=stop) return;

    if(step==1) RemoveRange(start, stop-start);
    else if(step==-1) RemoveRange(stop+1, start-stop);
    else
    { int offset = 1;
      for(int i=start,next=start+step; i<count; i++)
      { if(i+offset==next)
        { next += step;
          if(next>=stop) next = 0;
          else offset++;
        }
        items[i] = items[i+offset];
      }
      count -= offset;
    }
    
    version++;
  }
  #endregion

  #region IList Members
  public object this[int index]
  { get
    { index = BoaOps.FixIndex(index, count);
      if(index<0 || index>=count) throw new IndexOutOfRangeException();
      return items[index];
    }
    set
    { index = BoaOps.FixIndex(index, count);
      if(index<0 || index>=count) throw new IndexOutOfRangeException();
      items[index] = value;
      version++;
    }
  }

  public bool IsFixedSize { get { return false; } }
  public bool IsReadOnly { get { return false; } }

  public int Add(object value)
  { version++;
    ResizeTo(count+1);
    items[count] = value;
    return count++;
  }

  public bool Contains(object value) { return IndexOf(value, 0, count) != -1; }

  public void Clear()
  { if(count!=0)
    { count = 0;
      version++;
    }
  }

  public int IndexOf(object value) { return IndexOf(value, 0, count); }
  public int IndexOf(object value, int start)
  { start = BoaOps.FixIndex(start, count);
    return IndexOf(value, start, count-start);
  }
  public int IndexOf(object value, int start, int length)
  { start = BoaOps.FixIndex(start, count);
    int end = start+length;
    if(start<0 || end>count) throw new ArgumentOutOfRangeException();
    for(; start<end; start++) if(Ops.Compare(items[start], value)==0) return start;
    return -1;
  }

  public void Insert(int index, object value)
  { index = BoaOps.FixIndex(index, count);
    if(index<0 || index>count) throw new IndexOutOfRangeException();
    ResizeTo(count+1);
    for(int i=count++; i>index; i--) items[i] = items[i-1];
    items[index] = value;
    version++;
  }

  public void Remove(object value)
  { int index = IndexOf(value);
    if(index!=-1) RemoveAt(index);
  }

  public void RemoveAt(int index)
  { index = BoaOps.FixIndex(index, count);
    if(index<0 || index>=count) throw new IndexOutOfRangeException();
    count--;
    for(; index<count; index++) items[index] = items[index+1];
    items[count] = null;
    version++;
  }
  #endregion

  #region ICollection Members
  public bool IsSynchronized { get { return false; } }
  public int Count { get { return count; } }
  public void CopyTo(Array array, int index) { Array.Copy(items, 0, array, index, count); }
  public object SyncRoot { get { return this; } }
  #endregion

  #region IEnumerable Members
  public IEnumerator GetEnumerator() { return new ListEnumerator(this); }
  
  sealed class ListEnumerator : IEnumerator
  { public ListEnumerator(List list) { this.list=list; index=-1; version=list.version; }

    public void Reset()
    { if(list.version!=version) throw new InvalidOperationException("The collection has been modified.");
      index = -1;
    }

    public object Current
    { get
      { if(index<0) throw new InvalidOperationException();
        return current;
      }
    }

    public bool MoveNext()
    { if(list.version!=version) throw new InvalidOperationException("The collection has been modified.");
      if(index==list.count-1 || index==-2) return false;
      current = list.items[++index];
      return true;
    }

    List list;
    object current;
    int index, version;
  }
  #endregion

  #region IComparable Members
  public int CompareTo(object o) // TODO: is this right?
  { List list = o as List;
    if(list!=null) return ArrayOps.Compare(items, count, list.items, list.count);
    else return Ops.Compare(this, o);
  }
  #endregion

  #region ICloneable Members
  public object Clone() { return new List(this); }
  #endregion

  #region IRepresentable Members
  public string ToCode()
  { System.Text.StringBuilder sb = new System.Text.StringBuilder();
    sb.Append('[');
    for(int i=0; i<count; i++)
    { if(i>0) sb.Append(", ");
      sb.Append(Ops.ToCode(items[i]));
    }
    sb.Append(']');
    return sb.ToString();
  }
  #endregion

  public override string ToString() { return ToCode(); }

  public static readonly List Empty = new List(); // FIXME: make this list immutable

  sealed class FunctionComparer : IComparer
  { public FunctionComparer(IProcedure func) { this.func = func; }
    public int Compare(object x, object y) { return Ops.ToInt(func.Call(x, y)); }
    IProcedure func;
  }

  void InsertRange(int start, int length)
  { ResizeTo(count+length);
    for(int i=count-1; i>=start; i--) items[i+length] = items[i];
    count += length;
  }

  void ResizeTo(int capacity)
  { if(capacity>items.Length)
    { int len = Math.Max(items.Length, 4);
      while(len<capacity) len*=2;
      object[] arr = new object[len];
      Array.Copy(items, arr, count);
      items = arr;
    }
  }

  object[] items;
  int count, version;
}
#endregion

#region Slice
public sealed class Slice : IRepresentable
{ public Slice() { }
  public Slice(object stop) { this.stop = stop; }
  public Slice(object start, object stop) { this.start=start; this.stop=stop; }
  public Slice(object start, object stop, object step)
  { if(step!=null && Ops.ToInt(step)==0) throw new ArgumentException("slice(): step cannot be zero");
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

  public Tuple GetIndices(int length)
  { int start, stop, step;
    GetIndices(length, out start, out stop, out step);
    return Tuple.Make(start, stop, step);
  }

  public void GetIndices(int length, out int start, out int stop, out int step)
  { step  = (this.step==null ? 1 : Ops.ToInt(this.step));
    start = (this.start==null ? step>0 ? 0 : length-1 : BoaOps.FixSliceIndex(Ops.ToInt(this.start), length));
    stop  = (this.stop==null ? step>0 ? length : -1 : BoaOps.FixSliceIndex(Ops.ToInt(this.stop), length));
  }

  public override string ToString() { return ToCode(); }

  public string ToCode()
  { return string.Format("slice({0}, {1}, {2})", Ops.ToCode(start), Ops.ToCode(stop), Ops.ToCode(step));
  }

  public readonly object start, stop, step;
}
#endregion

#region StringOps
public sealed class StringOps
{ StringOps() { }

  #region SequenceWrapper
  public class SequenceWrapper : ISliceable, ICollection, IList
  { public SequenceWrapper(string str) { this.str = str; }

    #region ISliceable Members
    public ICollection GetSlice(Slice slice)
    { int start, stop, step;
      slice.GetIndices(str.Length, out start, out stop, out step);
      if(step==1)
      { string[] arr = new string[stop-start];
        for(int i=0; i<arr.Length; i++) arr[i] = new string(str[i], 1);
        return arr;
      }
      else if(step==-1)
      { string[] arr = new string[start-stop];
        for(int i=0; i<arr.Length; i++) arr[i] = new string(str[start-i], 1);
        return arr;
      }
      else return BoaOps.SequenceSlice(this, start, stop, step);
    }
    #endregion
    
    #region ICollection Members
    public int Count { get { return str.Length; } }
    public bool IsSynchronized { get { return true; } }
    public object SyncRoot { get { return str; } }

    public void CopyTo(Array array, int index)
    { for(int i=0; i<str.Length; i++) array.SetValue(new string(str[i], 1), i);
    }
    #endregion

    #region IEnumerable Members
    public IEnumerator GetEnumerator() { return new BoaCharEnumerator(str); }
    #endregion

    #region IList Members
    public object this[int index]
    { get { return new string(str[BoaOps.FixIndex(index, str.Length)], 1); }
      set { throw ImmutableError(); }
    }

    public bool IsFixedSize { get { return true; } }
    public bool IsReadOnly { get { return true; } }

    public int Add(object value) { throw ImmutableError(); }
    public void Clear() { throw ImmutableError(); }
    public bool Contains(object value) { return IndexOf(value) != -1; }

    public int IndexOf(object value)
    { string s = value as string;
      if(s!=null) return str.IndexOf(s);
      else if(value is char) return str.IndexOf((char)value);
      else return -1;
    }

    public void Insert(int index, object value) { throw ImmutableError(); }
    public void Remove(object value) { throw ImmutableError(); }
    public void RemoveAt(int index) { throw ImmutableError(); }
    #endregion

    readonly string str;
    
    static InvalidOperationException ImmutableError() { return new InvalidOperationException("strings are immutable"); }
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
      if(dict && nodict) throw Ops.ArgError("keyed format codes and non-keyed format codes (or codes with a "+
                                            "length/precision of '#') mixed in format string");

      arggot = tup==null ? 1 : tup.items.Length;
      if(tup==null && dict) getitem = Ops.GetProperty(args, "__getitem__");
      else if(dict) throw Ops.ArgError("format requires a mapping");
      else if(argwant!=arggot) throw Ops.ArgError("incorrect number of arguments for string formatting "+
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

          case 'r': sb.Append(Ops.ToCode(GetArg(f.Key))); break;
          case 's': sb.Append(Ops.Str(GetArg(f.Key))); break;
          case '%': sb.Append('%'); break;
          default: throw new FormatException(string.Format("unsupported format character '{0}' (0x{1:X})",
                                                           f.Type, (int)f.Type));
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
    if(str.Length==1) return new string(str[0], n);
    StringBuilder sb = new StringBuilder(str.Length*n);
    while(n-->0) sb.Append(str);
    return sb.ToString();
  }

  public static string PrintF(string format, object args) { return new StringFormatter(format, args).Format(); }

  public static string Slice(string s, Slice slice)
  { int start, stop, step;
    slice.GetIndices(s.Length, out start, out stop, out step);
    if(step<0 && start<=stop || step>0 && start>=stop) return string.Empty;
    if(step==1) return s.Substring(start, stop-start);
    else
    { StringBuilder sb = new StringBuilder((stop-start+step-Math.Sign(step))/step);
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
          { if(c>'7') throw new FormatException("invalid octal digit near string index "+pos.ToString());
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
            else throw new FormatException(string.Format("reference to group {0} found near string index {1}, "+
                                                         "but there are only {2} groups", num, pos, m.Groups.Count));
          }
        }
        else switch(c)
        { case '\0': throw new FormatException("unterminated escape sequence");
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
                if(c==0) throw new FormatException("unterminated group name near string index "+pos.ToString());
                System.Text.RegularExpressions.Group g = m.Groups[name];
                if(g==null)
                  throw new FormatException(string.Format("nonexistant group '{0}' referenced near string index {1}",
                                                          name, pos));
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
              { if(i==0) throw new FormatException("expected hex digit near string index "+pos.ToString());
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
            if(!char.IsLetter(c)) throw new FormatException("expected letter for \\c near string index "+pos.ToString());
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

// FIXME: don't allow ToCode to go into an infinite loop with circular references
#region Tuple
public sealed class Tuple : ISliceable, IList, IComparable, IRepresentable
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
  Tuple(object[] items) { this.items=items; }
// FIXME: the object[] constructor should be internal. fix the access issues with LCG
public static Tuple Make(ICollection col)
{ object[] items = new object[col.Count];
  col.CopyTo(items, 0);
  return new Tuple(items);
}
public static Tuple Make(params object[] items) { return new Tuple(items); }

  #region ISliceable Members
  public ICollection GetSlice(Slice slice)
  { int start, stop, step;
    slice.GetIndices(items.Length, out start, out stop, out step);
    if(step==1)
    { object[] arr = new object[stop-start];
      Array.Copy(items, start, arr, 0, stop-start);
      return new Tuple(arr);
    }
    else if(step==-1)
    { object[] arr = new object[start-stop];
      for(int i=0; i<arr.Length; i++) arr[i] = items[start-i];
      return new Tuple(arr);
    }
    else return BoaOps.SequenceSlice(this, start, stop, step);
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
  public bool Contains(object o) { return IndexOf(o) != -1; }
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
  public int CompareTo(object o) // TODO: is this right?
  { Tuple tup = o as Tuple;
    if(tup!=null) return ArrayOps.Compare(items, items.Length, tup.items, tup.items.Length);
    else return Ops.Compare(this, o);
  }
  #endregion

  #region IRepresentable Members
  public string ToCode()
  { System.Text.StringBuilder sb = new System.Text.StringBuilder();
    sb.Append('(');
    for(int i=0; i<items.Length; i++)
    { if(i>0) sb.Append(", ");
      sb.Append(Ops.ToCode(items[i]));
    }
    if(items.Length==1) sb.Append(',');
    sb.Append(')');
    return sb.ToString();
  }
  #endregion

  public override bool Equals(object o) { return CompareTo(o)==0; }

  public override int GetHashCode()
  { if(hashCode!=null) return (int)hashCode;

    int hash = 0;
    foreach(object o in items) if(o!=null) hash ^= o.GetHashCode();
    hashCode = hash;
    return hash;
  }

  public override string ToString() { return ToCode(); }

  public static readonly Tuple Empty = new Tuple();
  
  public readonly object[] items; // FIXME: this should be internal
  object hashCode;

  static InvalidOperationException ImmutableError() { return new InvalidOperationException("Tuples are immutable"); }
}
#endregion

} // namespace Boa.Backend
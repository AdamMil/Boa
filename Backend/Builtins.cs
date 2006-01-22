/*
Boa is the reference implementation for a language, also called Boa,
which is similar to python. This implementation is both interpreted
and compiled, targetting the Microsoft .NET Framework.

http://www.adammil.net/
Copyright (C) 2005-2006 Adam Milazzo

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

  public static List dir(object o) { return new List(MemberContainer.FromObject(o).GetMemberNames(true)); }

  static MemberContainer instance;
}

} // namespace NetLisp.Backend
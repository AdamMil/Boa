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
using System.IO;
using Scripting;

namespace Boa.TextFrontend
{

public sealed class BoaTextFrontend : Scripting.TextFrontend
{ public BoaTextFrontend(TextReader input, TextWriter output) : base(input, output) { }

  public static int Main(string[] args)
  { Options.Current.Language = Boa.Backend.BoaLanguage.Instance;
    return new BoaTextFrontend(Console.In, Console.Out).DoMain(args);
  }

  protected override string GetSnippet()
  { Output.Write(">>> ");
    string code = Input.ReadLine();
    if(code==null) return null;
    if(code=="<<<")
    { code = null;
      while(true)
      { Output.Write("... ");
        string line = Input.ReadLine();
        if(line==null) break;
        code += line+"\n";
      }
    }
    return code;
  }

  protected override void ShowBanner()
  { Output.WriteLine("Boa, a python-like language for the .NET platform");
    Output.WriteLine("Copyright Adam Milazzo 2004-2006. http://www.adammil.net");
  }

  protected override void ShowUsage()
  { ShowBanner();
    Output.WriteLine("usage: boa [option] ... [file | - | --] [arg] ...");
    Output.WriteLine();
    ShowCompilationOptions();
    Output.WriteLine();
    Output.WriteLine("Other arguments:");
    Output.WriteLine("file            Read script file");
    Output.WriteLine("-               Read standard input");
    Output.WriteLine("--              Enter interactive mode");
    Output.WriteLine("arg ...         Arguments to store in sys.argv[1:]");
    Output.WriteLine();
    Output.WriteLine("Environment variables:");
    Output.WriteLine("BOA_LIB_PATH    A path to prefix to the module search path");
  }
}

} // namespace Boa.TextFrontend
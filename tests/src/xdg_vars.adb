pragma License (GPL);
------------------------------------------------------------------------------
-- EMAIL: <darkestkhan@gmail.com>                                           --
-- License: GNU GPLv3 or any later as published by Free Software Foundation --
-- (see README file)                                                        --
--                    Copyright © 2013, 2015 darkestkhan                    --
------------------------------------------------------------------------------
--  This Program is Free Software: You can redistribute it and/or modify    --
--  it under the terms of The GNU General Public License as published by    --
--    the Free Software Foundation, either version 3 of the license, or     --
--                (at Your option) any later version.                       --
--                                                                          --
--      This Program is distributed in the hope that it will be useful,     --
--      but WITHOUT ANY WARRANTY; without even the implied warranty of      --
--      MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE. See the        --
--              GNU General Public License for more details.                --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--   along with this program. If not, see <http://www.gnu.org/licenses/>.   --
------------------------------------------------------------------------------

  --------------------------------------------------------------------------
  -- Test ability to read content of variables and their default values   --
  -- if not set.                                                          --
  --------------------------------------------------------------------------

with Ada.Environment_Variables;
with Ada.Command_Line;
with Ada.Text_IO;

with XDG;
procedure XDG_Vars is

  package EV  renames Ada.Environment_Variables;
  package TIO renames Ada.Text_IO;
  package CLI renames Ada.Command_Line;

  type String_Access is access String;
  type XDG_Variable is
    ( Data_Home, Config_Home, Cache_Home, Runtime_Dir, Data_Dirs, Config_Dirs );

  type XDG_Wrapped is
  record
    Name    : String_Access := null;  -- Variable name
    Value   : String_Access := null;  -- Test value
    Default : String_Access := null;  -- Default value
    Variable: XDG_Variable;
  end record;

  procedure Set
    ( This: in out XDG_Wrapped; Name: in String;
      Value: in String; Default: in String; Var: XDG_Variable
    )
  is
  begin
    This.Name     := new String'(Name);
    This.Value    := new String'(Value);
    This.Default  := new String'(Default);
    This.Variable := Var;
  end Set;

  -- Error count;
  Errors: Natural := 0;

  procedure Test (This: in XDG_Wrapped)
  is
    -- Wrapper to call correct function from XDG package.
    function Call_Function return String
    is
    begin
      case This.Variable is
        when Data_Home    => return XDG.Data_Home;
        when Config_Home  => return XDG.Config_Home;
        when Cache_Home   => return XDG.Cache_Home;
        when Runtime_Dir  => return XDG.Runtime_Dir;
        when Data_Dirs    => return XDG.Data_Dirs;
        when Config_Dirs  => return XDG.Config_Dirs;
      end case;
    end Call_Function;

    procedure Put_Error (This: in XDG_Wrapped; Test_Step: in Positive)
    is
      Message: constant String := "Test error when testing: ";
    begin
      TIO.Put_Line
        ( File => TIO.Standard_Error, Item => Message & " " &
          XDG_Variable'Image (This.Variable) & " : at test step : " &
          Positive'Image (Test_Step)
        );
    end Put_Error;

  begin
    begin
      if Call_Function /= This.Default.all then
        Put_Error (This, 1);
        Errors := Errors + 1;
      end if;
    exception
      when others =>
        Put_Error (This, 1);
        Errors := Errors + 1;
    end;

    begin
      EV.Set (This.Name.all, This.Value.all);
      if Call_Function /= This.Value.all then
        Put_Error (This, 2);
        Errors := Errors + 1;
      end if;
    exception
      when others =>
        Put_Error (This, 2);
        Errors := Errors + 1;
    end;
  end Test;

  Home_Path: constant String := EV.Value ("HOME");

  type XDG_Data is array (Positive range <>) of XDG_Wrapped;

  Test_Data: XDG_Data (1 .. 6);

  Error_Message: constant String :=
    "xdg_vars: Total number of unexpected failures triggered: ";

begin
  -- Clear variable values as they may have been carried from environment,
  -- which could make it hard to test this functionality, and restore HOME
  -- as it is being used.
  EV.Clear;
  EV.Set ("HOME", Home_Path);

  -- Set up entire test
  Set ( Test_Data (1), "XDG_DATA_HOME", EV.Value ("HOME") & "/data/",
        EV.Value ("HOME") & "/.local/share/", Data_Home
      );
  Set ( Test_Data (2), "XDG_CONFIG_HOME", EV.Value ("HOME") & "/config/",
        EV.Value ("HOME") & "/.config/", Config_Home
      );
  Set ( Test_Data (3), "XDG_CACHE_HOME", EV.Value ("HOME") & "/cache/",
        EV.Value ("HOME") & "/.cache/", Cache_Home
      );
  Set ( Test_Data (4), "XDG_RUNTIME_DIR", EV.Value ("HOME") & "/runtime/", "",
        Runtime_Dir
      );
  Set ( Test_Data (5), "XDG_DATA_DIRS", EV.Value ("HOME") & "/data_dirs/",
        "/usr/local/share/:/usr/share/", Data_Dirs
      );
  Set ( Test_Data (6), "XDG_CONFIG_DIRS", EV.Value ("HOME") & "/xdg/",
        "/etc/xdg/", Config_Dirs
      );

  -- Actually perform test
  for I in Test_Data'Range loop
    Test (Test_Data (I));
  end loop;

  if Errors /= 0 then
    TIO.Put_Line
      ( File => TIO.Standard_Error,
        Item => Error_Message & Natural'Image (Errors)
      );
    CLI.Set_Exit_Status (CLI.Failure);
  end if;
end XDG_Vars;

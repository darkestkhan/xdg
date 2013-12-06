pragma License (GPL);
------------------------------------------------------------------------------
-- EMAIL: <darkestkhan@gmail.com>                                           --
-- License: GNU GPLv3 or any later as published by Free Software Foundation --
-- (see README file)                                                        --
--                    Copyright © 2013 darkestkhan                          --
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
  -- Test parameterized Data_Home, Config_Home and Cache_Home functions.  --
  --------------------------------------------------------------------------

with Ada.Environment_Variables;
with Ada.Command_Line;
with Ada.Text_IO;

with XDG;
procedure XDG_Home_Paths is

  package EV  renames Ada.Environment_Variables;
  package CLI renames Ada.Command_Line;
  package TIO renames Ada.Text_IO;

  type String_Access is access String;

  type XDG_Paths is (Data_Home, Config_Home, Cache_Home);

  -- Error count;
  Errors: Natural := 0;
  Home_Path: constant String := EV.Value ("HOME");
  -- NOTE: '/' at the end of Dir is added in order to ease up testing.
  Dir: constant String := "XDG?/";

  function Get_Path (To: in XDG_Paths) return String
  is
  begin
    case To is
      when Data_Home    => return XDG.Data_Home   (Dir);
      when Config_Home  => return XDG.Config_Home (Dir);
      when Cache_Home   => return XDG.Cache_Home  (Dir);
    end case;
  end Get_Path;

  Var_Names: constant array (XDG_Paths) of String_Access :=
    ( new String'("XDG_DATA_HOME"),
      new String'("XDG_CONFIG_HOME"),
      new String'("XDG_CACHE_HOME")
    );

  Paths: constant array (XDG_Paths) of String_Access :=
    ( new String'(Home_Path & "data/"),
      new String'(Home_Path & "config/"),
      new String'(Home_Path & "cache/")
    );

  Error_Message: constant String := "Test error when testing: ";
  Error_Message_At_Exit: constant String :=
    "xdg_home_paths: Total number of unexpected failures triggered: ";

begin
  for I in XDG_Paths loop
    EV.Clear  (Var_Names (I).all);
    EV.Set    (Var_Names (I).all, Paths (I).all);
    if Get_Path (I) /= Paths (I).all & Dir then
      Errors := Errors + 1;
      TIO.Put_Line
        ( File => TIO.Standard_Error,
          Item => Error_Message & " " & XDG_Paths'Image (I)
        );
      TIO.Put_Line
        ( File => TIO.Standard_Error,
          Item => "  Expected value: " & Paths (I).all & Dir
        );
      TIO.Put_Line
        ( File => TIO.Standard_Error,
          Item => "  Received value: " & Get_Path (I)
        );
    end if;
  end loop;

  if Errors /= 0 then
    TIO.Put_Line
      ( File => TIO.Standard_Error,
        Item => Error_Message_At_Exit & Natural'Image (Errors)
      );
    CLI.Set_Exit_Status (CLI.Failure);
  end if;
end XDG_Home_Paths;

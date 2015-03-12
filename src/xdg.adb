pragma License (GPL);
------------------------------------------------------------------------------
-- EMAIL: <darkestkhan@gmail.com>                                           --
-- License: GNU GPLv3 or any later as published by Free Software Foundation --
-- (see README file)                                                        --
--                    Copyright © 2014 - 2015 darkestkhan                   --
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
--   along with this program.  If not, see <http://www.gnu.org/licenses/>.  --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
------------------------------------------------------------------------------
with Ada.Directories;
with Ada.Environment_Variables;
package body XDG is

  ----------------------------------------------------------------------------

  package EV renames Ada.Environment_Variables;

  ----------------------------------------------------------------------------

  generic
    Variable: String;
    Default : String;
  function Get_Home return String;

  function Get_Home return String
  is
    Home: constant String := EV.Value ("HOME");
  begin
    if EV.Exists (Variable) then
      declare
        Value: constant String := EV.Value (Variable);
      begin
        if Value (Value'Last) = '/' then
          return Value;
        else
          return Value & '/';
        end if;
      end;
    else
      if Home (Home'Last) = '/' then
        return Home & Default;
      else
        return Home & '/' & Default;
      end if;
    end if;
  end Get_Home;

  function Get_Data_Home    is new Get_Home ("XDG_DATA_HOME", ".local/share/");
  function Get_Config_Home  is new Get_Home ("XDG_CONFIG_HOME", ".config/");
  function Get_Cache_Home   is new Get_Home ("XDG_CACHE_HOME", ".cache/");

  function Data_Home    return String renames Get_Data_Home;
  function Config_Home  return String renames Get_Config_Home;
  function Cache_Home   return String renames Get_Cache_Home;

  function Runtime_Dir return String
  is
  begin
    if EV.Exists ("XDG_RUNTIME_DIR") then
      declare
        Value: constant String := EV.Value ("XDG_RUNTIME_DIR");
      begin
        if Value (Value'Last) = '/' then
          return Value;
        else
          return Value & '/';
        end if;
      end;
    else
      return "";
    end if;
  end Runtime_Dir;

  function Data_Dirs return String
  is
  begin
    if EV.Exists ("XDG_DATA_DIRS") then
      return EV.Value ("XDG_DATA_DIRS");
    else
      return "/usr/local/share/:/usr/share/";
    end if;
  end Data_Dirs;

  function Config_Dirs return String
  is
  begin
    if EV.Exists ("XDG_CONFIG_DIRS") then
      return EV.Value ("XDG_CONFIG_DIRS");
    else
      return "/etc/xdg/";
    end if;
  end Config_Dirs;

  ----------------------------------------------------------------------------

  generic
    with function XDG_Path return String;
  function XDG_Home (Directory: in String) return String;

  function XDG_Home (Directory: in String) return String
  is
    Path: constant String := XDG_Path;
  begin
    if Path (Path'Last) = '/' then
      if Directory (Directory'Last) = '/' then
        return Path & Directory;
      else
        return Path & Directory & '/';
      end if;
    else
      if Directory (Directory'Last) = '/' then
        return Path & '/' & Directory;
      else
        return Path & '/' & Directory & '/';
      end if;
    end if;
  end XDG_Home;

  function Data_Home_Path   is new XDG_Home (Data_Home);
  function Config_Home_Path is new XDG_Home (Config_Home);
  function Cache_Home_Path  is new XDG_Home (Cache_Home);

  function Data_Home    (Directory: in String) return String
    renames Data_Home_Path;
  function Config_Home  (Directory: in String) return String
    renames Config_Home_Path;
  function Cache_Home   (Directory: in String) return String
    renames Cache_Home_Path;

  function Runtime_Dir  (Directory: in String) return String
  is
    Path: constant String := Runtime_Dir;
  begin
    if Path'Length = 0 then
      raise No_Runtime_Dir;
    elsif Directory (Directory'Last) = '/' then
      return Path & Directory;
    else
      return Path & Directory & "/";
    end if;
  end Runtime_Dir;

  ----------------------------------------------------------------------------

  generic
    with function XDG_Path (Directory: in String) return String;
  procedure Create_Home (Directory: in String);

  procedure Create_Home (Directory: in String)
  is
    package AD renames Ada.Directories;
    Path: constant String := XDG_Path (Directory);
  begin
    AD.Create_Path (Path);
  end Create_Home;

  procedure Create_Data   is new Create_Home (Data_Home);
  procedure Create_Config is new Create_Home (Config_Home);
  procedure Create_Cache  is new Create_Home (Cache_Home);

  procedure Create_Data_Home    (Directory: in String) renames Create_Data;
  procedure Create_Config_Home  (Directory: in String) renames Create_Config;
  procedure Create_Cache_Home   (Directory: in String) renames Create_Cache;

  ----------------------------------------------------------------------------

  generic
    with function XDG_Path (Directory: in String) return String;
  procedure Delete_Home (Directory: in String; Empty_Only: in Boolean := True);

  procedure Delete_Home (Directory: in String; Empty_Only: in Boolean := True)
  is
    package AD renames Ada.Directories;
    Path: constant String := XDG_Path (Directory);
  begin
    if Empty_Only then
      AD.Delete_Directory (Path);
    else
      AD.Delete_Tree (Path);
    end if;
  end Delete_Home;

  procedure Delete_Data   is new Delete_Home (Data_Home);
  procedure Delete_Config is new Delete_Home (Config_Home);
  procedure Delete_Cache  is new Delete_Home (Cache_Home);

  procedure Delete_Data_Home
    ( Directory : in String;
      Empty_Only: in Boolean := True
    ) renames Delete_Data;
  procedure Delete_Config_Home
    ( Directory : in String;
      Empty_Only: in Boolean := True
    ) renames Delete_Config;
  procedure Delete_Cache_Home
    ( Directory : in String;
      Empty_Only: in Boolean := True
    ) renames Delete_Cache;

  ----------------------------------------------------------------------------

  generic
    with function XDG_Home (Directory: in String) return String;
  function Check_Home (Directory: in String) return Boolean;

  function Check_Home (Directory: in String) return Boolean
  is
    package AD renames Ada.Directories;
    use type AD.File_Kind;
    Path: constant String := XDG_Home (Directory);
  begin
    if AD.Exists (Path) and then AD.Kind (Path) /= AD.Directory then
      return False;
    else
      return True;
    end if;
  end Check_Home;

  function Check_Data_Home    is new Check_Home (Data_Home);
  function Check_Config_Home  is new Check_Home (Config_Home);
  function Check_Cache_Home   is new Check_Home (Cache_Home);

  function Is_Valid_Data_Home   (Directory: in String) return Boolean
    renames Check_Data_Home;
  function Is_Valid_Config_Home (Directory: in String) return Boolean
    renames Check_Config_Home;
  function Is_Valid_Cache_Home  (Directory: in String) return Boolean
    renames Check_Cache_Home;

  ----------------------------------------------------------------------------

end XDG;

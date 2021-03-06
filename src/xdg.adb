------------------------------------------------------------------------------
-- EMAIL: <darkestkhan@gmail.com>                                           --
-- License: ISC License (see COPYING file)                                  --
--                                                                          --
--                    Copyright © 2014 - 2015 darkestkhan                   --
------------------------------------------------------------------------------
-- Permission to use, copy, modify, and/or distribute this software for any --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- The software is provided "as is" and the author disclaims all warranties --
-- with regard to this software including all implied warranties of         --
-- merchantability and fitness. In no event shall the author be liable for  --
-- any special, direct, indirect, or consequential damages or any damages   --
-- whatsoever resulting from loss of use, data or profits, whether in an    --
-- action of contract, negligence or other tortious action, arising out of  --
-- or in connection with the use or performance of this software.           --
------------------------------------------------------------------------------
with Ada.Directories;
with Ada.Environment_Variables;

private with XDG.Defaults;
package body XDG is

  ----------------------------------------------------------------------------

  package EV renames Ada.Environment_Variables;

  -- Directory separator is different for Windows and UNIX so use appropriate
  -- one.
  Sep: constant Character := XDG.Defaults.Separator;

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
        if Value (Value'Last) = Sep then
          return Value;
        else
          return Value & Sep;
        end if;
      end;
    else
      if Home (Home'Last) = Sep then
        return Home & Default;
      else
        return Home & Sep & Default;
      end if;
    end if;
  end Get_Home;

  function Get_Data_Home
    is new Get_Home ("XDG_DATA_HOME", XDG.Defaults.Home);
  function Get_Config_Home
    is new Get_Home ("XDG_CONFIG_HOME", XDG.Defaults.Config);
  function Get_Cache_Home
    is new Get_Home ("XDG_CACHE_HOME", XDG.Defaults.Cache);

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
        if Value (Value'Last) = Sep then
          return Value;
        else
          return Value & Sep;
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
      return XDG.Defaults.Data_Dirs;
    end if;
  end Data_Dirs;

  function Config_Dirs return String
  is
  begin
    if EV.Exists ("XDG_CONFIG_DIRS") then
      return EV.Value ("XDG_CONFIG_DIRS");
    else
      return XDG.Defaults.Config_Dirs;
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
    if Path (Path'Last) = Sep then
      if Directory (Directory'Last) = Sep then
        return Path & Directory;
      else
        return Path & Directory & Sep;
      end if;
    else
      if Directory (Directory'Last) = Sep then
        return Path & Sep & Directory;
      else
        return Path & Sep & Directory & Sep;
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
    elsif Directory (Directory'Last) = Sep then
      return Path & Directory;
    else
      return Path & Directory & Sep;
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

  procedure Create_Data     is new Create_Home (Data_Home);
  procedure Create_Config   is new Create_Home (Config_Home);
  procedure Create_Cache    is new Create_Home (Cache_Home);
  procedure Create_Runtime  is new Create_Home (Runtime_Dir);

  procedure Create_Data_Home    (Directory: in String) renames Create_Data;
  procedure Create_Config_Home  (Directory: in String) renames Create_Config;
  procedure Create_Cache_Home   (Directory: in String) renames Create_Cache;
  procedure Create_Runtime_Dir  (Directory: in String) renames Create_Runtime;

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

  procedure Delete_Data     is new Delete_Home (Data_Home);
  procedure Delete_Config   is new Delete_Home (Config_Home);
  procedure Delete_Cache    is new Delete_Home (Cache_Home);
  procedure Delete_Runtime  is new Delete_Home (Runtime_Dir);

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
  procedure Delete_Runtime_Dir
    ( Directory : in String;
      Empty_Only: in Boolean := True
    ) renames Delete_Runtime;

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
  function Check_Runtime_Dir  is new Check_Home (Runtime_Dir);

  function Is_Valid_Data_Home   (Directory: in String) return Boolean
    renames Check_Data_Home;
  function Is_Valid_Config_Home (Directory: in String) return Boolean
    renames Check_Config_Home;
  function Is_Valid_Cache_Home  (Directory: in String) return Boolean
    renames Check_Cache_Home;
  function Is_Valid_Runtime_Dir (Directory: in String) return Boolean
    renames Check_Runtime_Dir;

  ----------------------------------------------------------------------------

end XDG;

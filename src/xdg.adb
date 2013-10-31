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
with Ada.Environment_Variables;
package body XDG is

  package EV renames Ada.Environment_Variables;

  function Data_Home return String
  is
  begin
    if EV.Exists ("XDG_DATA_HOME") then
      return EV.Value ("XDG_DATA_HOME");
    else
      return EV.Value ("HOME") & "/.local/share/";
    end if;
  end Data_Home;

  function Config_Home return String
  is
  begin
    if EV.Exists ("XDG_CONFIG_HOME") then
      return EV.Value ("XDG_CONFIG_HOME");
    else
      return EV.Value ("HOME") & "/.config/";
    end if;
  end Config_Home;

  function Cache_Home return String
  is
  begin
    if EV.Exists ("XDG_CACHE_HOME") then
      return EV.Value ("XDG_CACHE_HOME");
    else
      return EV.Value ("HOME") & "/.cache/";
    end if;
  end Cache_Home;

  function Runtime_Dir return String
  is
  begin
    if EV.Exists ("XDG_RUNTIME_DIR") then
      return EV.Value ("XDG_RUNTIME_DIR");
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

end XDG;

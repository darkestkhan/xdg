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
package XDG is

  pragma Preelaborate (XDG);

  -- Directory in which user specific data files should be stored.
  function Data_Home    return String;
  -- As above but for configuration files.
  function Config_Home  return String;
  -- As above but for non-essential data files.
  function Cache_Home   return String;
  -- As above but for non-essential runtime files.
  -- Returns null string in case XDG_RUNTIME_DIR is not set.
  function Runtime_Dir  return String;

  -- Preference ordered set of base directories to search for data files
  -- in addition to Data_Home. Directories are separated by ':'.
  function Data_Dirs    return String;
  -- As above but for config files.
  function Config_Dirs  return String;

end XDG;

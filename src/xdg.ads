pragma License (GPL);
------------------------------------------------------------------------------
-- EMAIL: <darkestkhan@gmail.com>                                           --
-- License: GNU GPLv3 or any later as published by Free Software Foundation --
-- (see README file)                                                        --
--                    Copyright © 2014 - 2015 darkestkhan                   --
------------------------------------------------------------------------------
--  This Program is Free Software: You can redistribute it and/or modify    --
--  it under the terms of The GNU General Public License as published by    --
--    the Free Software Foundation: either version 3 of the license, or     --
--                 (at your option) any later version.                      --
--                                                                          --
--      This Program is distributed in the hope that it will be useful,     --
--      but WITHOUT ANY WARRANTY; without even the implied warranty of      --
--      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the        --
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

  ----------------------------------------------------------------------------
  -- This package implements functionality pertaining to XDG Base           --
  -- Directories Specification.                                             --
  -- <http://standards.freedesktop.org/basedir-spec/basedir-spec-0.7.html>  --
  ----------------------------------------------------------------------------

  ---------------------------------------------------------------------------
  -- NOTE/FIXME: Runtime_Dir creation and deletion is not supported due to
  -- need for doing checks of permissions and for its location on local drive.
  ---------------------------------------------------------------------------

package XDG is

  ----------------------------------------------------------------------------
  -- NOTE: All functions returning pathname are making sure last character of
  -- said pathname is '/'.

  ----------------------------------------------------------------------------
  -- Directory in which user specific data files should be stored.
  function Data_Home    return String;
  -- As above but for configuration files.
  function Config_Home  return String;
  -- As above but for non-essential data files.
  function Cache_Home   return String;
  -- As above but for non-essential runtime files.
  -- Returns null string in case XDG_RUNTIME_DIR is not set.
  function Runtime_Dir  return String;

  ----------------------------------------------------------------------------
  -- Preference ordered set of base directories to search for data files
  -- in addition to Data_Home. Directories are separated by ':'.
  function Data_Dirs    return String;
  -- As above but for config files.
  function Config_Dirs  return String;

  ----------------------------------------------------------------------------
  -- NOTE: Subprograms below work only within XDG base directories.
  -- For example: Data_Home (Directory) will return path resultant of
  -- ${XDG_DATA_HOME}/${DIRECTORY}.

  ----------------------------------------------------------------------------
  -- These functions return path to directory.
  -- NOTE: Runtime_Dir (Directory) will raise No_Runtime_Dir exception if
  --  there is no Runtime_Dir defined.
  function Data_Home    (Directory: in String) return String;
  function Config_Home  (Directory: in String) return String;
  function Cache_Home   (Directory: in String) return String;
  function Runtime_Dir  (Directory: in String) return String;

  No_Runtime_Dir: exception;

  ----------------------------------------------------------------------------
  -- These procedures create path to directory. If target Directory already
  -- exists nothing will happen.
  procedure Create_Data_Home    (Directory: in String);
  procedure Create_Config_Home  (Directory: in String);
  procedure Create_Cache_Home   (Directory: in String);

  ----------------------------------------------------------------------------
  -- These procedures delete directory. If Empty_Only is true Directory will
  -- be deleted only when empty, otherwise will delete Directory with its entire
  -- content.
  procedure Delete_Data_Home
    ( Directory : in String;
      Empty_Only: in Boolean := True
    );
  procedure Delete_Config_Home
    ( Directory : in String;
      Empty_Only: in Boolean := True
    );
  procedure Delete_Cache_Home
    ( Directory : in String;
      Empty_Only: in Boolean := True
    );

  ----------------------------------------------------------------------------
  -- These functions check if directory exists and if it actually is directory.
  -- Returns false only when target file exists and is not directory. This way
  -- you can check if Directory is valid location and if so you can create it
  -- using one of the procedures available from this package.
  function Is_Valid_Data_Home    (Directory: in String) return Boolean;
  function Is_Valid_Config_Home  (Directory: in String) return Boolean;
  function Is_Valid_Cache_Home   (Directory: in String) return Boolean;

  ----------------------------------------------------------------------------

end XDG;

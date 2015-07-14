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

  ----------------------------------------------------------------------------
  -- This package implements functionality pertaining to XDG Base           --
  -- Directories Specification.                                             --
  -- <http://standards.freedesktop.org/basedir-spec/basedir-spec-0.7.html>  --
  ----------------------------------------------------------------------------

  ---------------------------------------------------------------------------
  -- FIXME: Create_* and Delete_* subprograms should check if Directory passed
  --  is valid directory inside appropriate place (that is, ie. to prevent
  --  deletion of "../../").
  ---------------------------------------------------------------------------

package XDG is

  ----------------------------------------------------------------------------
  -- NOTE: All functions returning pathname are making sure last character of
  -- said pathname is '/' (or '\' in case of Windows).

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
  -- NOTE: Default value for Windows is "".
  function Data_Dirs    return String;
  -- As above but for config files.
  function Config_Dirs  return String;

  ----------------------------------------------------------------------------
  -- NOTE: Subprograms below work only within XDG base directories.
  -- For example: Data_Home (Directory) will return path resultant of
  -- ${XDG_DATA_HOME}/${DIRECTORY}.
  -- NOTE: Subprogram operating on XDG_RUNTIME_DIR will raise No_Runtime_Dir
  --  exception if there is no XDG_RUNTIME_DIR environment variable defined.

  ----------------------------------------------------------------------------
  -- These functions return path to directory.
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
  procedure Create_Runtime_Dir  (Directory: in String);

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
  procedure Delete_Runtime_Dir
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
  function Is_Valid_Runtime_Dir  (Directory: in String) return Boolean;

  ----------------------------------------------------------------------------

end XDG;

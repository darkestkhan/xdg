------------------------------------------------------------------------------
-- EMAIL: <darkestkhan@gmail.com>                                           --
-- License: ISC License (see COPYING file)                                  --
--                                                                          --
--                    Copyright © 2015 darkestkhan                          --
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
private package XDG.Defaults is

  -- Directory separator character.
  Separator: constant Character := '/';

  Home  : constant String := ".local/share/";
  Config: constant String := ".config/";
  Cache : constant String := ".cache/";

  Data_Dirs   : constant String := "/usr/local/share/:/usr/share/";
  Config_Dirs : constant String := "/etc/xdg/";

end XDG.Defaults;

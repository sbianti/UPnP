--------------------------------------------------------------------------------
--  This file is part of UPnP package   				      --
--  									      --
--  Copyright © 2015 Sébastien Bianti					      --
--  									      --
--  This program is free software; you can redistribute it and/or modify      --
--  it under the terms of the GNU General Public License version 3 as	      --
--  published by the Free Software Foundation.				      --
--  									      --
--  This program is distributed in the hope that it will be useful,	      --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of	      --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	      --
--  GNU General Public License for more details.			      --
--  									      --
--  You should have received a copy of the GNU General Public License	      --
--  along with this program; if not, write to the Free Software	              --
--  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA --
--------------------------------------------------------------------------------

package body UPnP is

   function Service_Type_Field(Kind: in Short_Kind_Of_Service_Type;
			       UUID: in String := "") return String is
   begin
      case Kind is
	 when ST_All => return "ssdp:all";

	 when ST_Root_Device => return "upnp:rootdevice";

	 when ST_UUID =>
	    if UUID = "" then raise Constraint_Error
	      with "Field uuid:<uuid_value> needs value";
	    end if;

	    return "uuid:" & UUID;
      end case;
   end Service_Type_Field;

   function Service_Type_Field(Kind: in Long_Kind_Of_Service_Type;
			       Type_Version: in String;
			       Domain_Name: in String := "") return String is
   begin
      if Type_Version = "" then raise Constraint_Error
	with "Type value needed for " & Kind'Img & " kind of ST field";
      end if;

      case Kind is
	 when Urn_Schemas_Device =>
	    return "urn:schemas-upnp-org:device:" & Type_Version;

	 when Urn_Schemas_Service =>
	    return "urn:schemas-upnp-org:service:" & Type_Version;

	 when Urn_Domain_Device =>
	    return "urn:" & Domain_Name & ":device:" & Type_Version;

	 when Urn_Domain_Service =>
	    return "urn:" & Domain_Name & ":service:" & Type_Version;
      end case;
   end Service_Type_Field;

end UPnP;

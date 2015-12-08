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

package UPnP is

   pragma Pure(UPnP);

   subtype Ipv4_Host_Type is String (1..20);
   Default_Host_Field: constant Ipv4_Host_Type := "239.255.255.250:1900";

   type Short_Kind_Of_Service_Type is (ST_All,
				       ST_Root_Device,
				       ST_UUID);

   type Long_Kind_Of_Service_Type is (Urn_Schemas_Device,
				      Urn_Schemas_Service,
				      Urn_Domain_Device,
				      Urn_Domain_Service);

   function Service_Type_Field(Kind: in Short_Kind_Of_Service_Type;
			       UUID: in String := "") return String;

   function Service_Type_Field(Kind: in Long_Kind_Of_Service_Type;
			       Type_Version: in String;
			       Domain_Name: in String := "") return String;
end UPnP;

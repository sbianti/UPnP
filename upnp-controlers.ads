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

with Ada.Strings.Unbounded;
with SSDP.Clients;

package UPnP.Controlers is
   use Ada.Strings.Unbounded;

   type MX_Value is new Integer range 1..5;

   type UPnP_Controler is new SSDP.Clients.SSDP_Client with private;

   Overriding function Initialize(Service_Type,
				    Universal_Serial_Number: in String)
				 return UPnP_Controler;

   function Initialize(Service_Type,
			 Universal_Serial_Number,
			 User_Agent: in String;
		       MX: in MX_Value := 3;
		       Host: in Ipv4_Host_Type := Default_Host_Field)
		      return UPnP_Controler;

   procedure M_Search
     (Device: in UPnP_Controler;
      Other_Header: in SSDP.Message_Header_Array := SSDP.Null_Header_Array);

   procedure Start_Listening renames SSDP.Clients.Start_Listening;

   procedure Stop_Listening renames SSDP.Clients.Stop_Listening;

private
   type UPnP_Controler is new SSDP.Clients.SSDP_Client with
      record
	 Host: Ipv4_Host_Type;
	 User_Agent: Unbounded_String;
	 MX: MX_Value;
      end record;
end UPnP.Controlers;

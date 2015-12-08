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

package body UPnP.Controlers is

   function To_US(S: in String) return Unbounded_String
     renames To_Unbounded_String;

   Overriding function Initialize(Service_Type,
				    Universal_Serial_Number: in String)
				 return UPnP_Controler is
   begin
      return Initialize(Service_Type, Universal_Serial_Number,"");
   end Initialize;

   function Initialize(Service_Type,
			 Universal_Serial_Number,
			 User_Agent: in String;
		       MX: in MX_Value := 3;
		       Host: in Ipv4_Host_Type := Default_Host_Field)
		      return UPnP_Controler is
      use SSDP.Clients;

      SSDP_Part: SSDP_Client := Initialize(Service_Type,
					   Universal_Serial_Number);
   begin
      return (SSDP_Part with
		Host, To_US(User_Agent), MX);
   end Initialize;

   procedure M_Search
     (Device: in UPnP_Controler;
      -- Other_Headers is just here for inheritance sake. Don't use it.
      Other_Header: in SSDP.Message_Header_Array := SSDP.Null_Header_Array) is

      use SSDP.Clients;

      Headers: SSDP.Message_Header_Array(1..3);
   begin
      -------------------------------------------------------
      --  M-SEARCH * HTTP/1.1				   -- (required by SSDP)
      --  HOST: 239.255.255.250:1900			   -- Mandatory
      --  MAN: "ssdp:discover"			           -- (required by SSDP)
      --  MX: seconds to delay response		           -- Mandatory
      --  ST: search target				   -- (required by SSDP)
      --  USER-AGENT: OS/version UPnP/1.1 product/version  -- Optional
      -------------------------------------------------------

      Headers(1..2) := (To_US("HOST: " & Device.Host),
			To_US("MX:") & Device.MX'Img);

      if Device.User_Agent /= Null_Unbounded_String then
	 Headers(3) := To_US("USER-AGENT: ") & Device.User_Agent;
	 M_Search(SSDP_Client(Device), Headers);
      else
	 M_Search(SSDP_Client(Device), Headers(1..2));
      end if;

   end M_Search;

end UPnP.Controlers;

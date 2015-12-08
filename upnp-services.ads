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
with SSDP.Services;

package UPnP.Services is
   use Ada.Strings.Unbounded, SSDP;

   UPnP_Multicast_Port: constant Natural := Natural(SSDP.Multicast_Port);

   type UPnP_Root_Device (Embedded_Count: Natural) is
     new SSDP.Services.SSDP_Service with private;

   type UPnP_Embedded_Device (<>) is private;

   type UPnP_Embedded_Device_Access is access UPnP_Embedded_Device;

   type UPnP_Embedded_Device_Access_Array is
     array (Natural range <>) of UPnP_Embedded_Device_Access;

   type UPnP_Service is record
      Name: Unbounded_String;
      Version: Natural;
   end record;

   type UPnP_Service_Array is array (Natural range <>) of UPnP_Service;

   -- UPnP identifies devices and services with USN and Service types
   -- However, there are two ways to announce those values in UPnP:
   -- Schemas:
   --  USN ≡ uuid:<UUID_Value>::urn:schemas-upnp-org:device:<dev_type>:<version>
   --  NT  ≡ same as USN but without the uuid field.
   -- Domain:
   --  USN ≡ uuid:<UUID_Value>::urn:<domain_name>:device:<dev_type>:<version>
   --  NT  ≡ same as above without uuid field.
   type Identification_Mode is (Schemas, Domain);

   function Create_Embedded_Device
     (Naming: in Identification_Mode;
      Name: in String;
      Version: in Natural;
      Services: in UPnP_Service_Array) return UPnP_Embedded_Device;

   function Initialize(Service_Type, Universal_Serial_Number,
			 Location: in String;
		       Cache_Control: in Positive)
		      return UPnP_Root_Device;

   function Initialize(Service_Type, Universal_Serial_Number,
			 Location, AL, -- only one is required
			 Expires: in String;
		       Cache_Control: in Natural := 0) -- dito
		      return UPnP_Root_Device;

   function Initialize(Universal_Serial_Number, Location: in String;
		       Cache_Control: in Natural;
		       Server: in String;
		       Embedded: in UPnP_Embedded_Device_Access_Array;
		       Bootid: in Positive := 1;
		       Configid: in Natural := 0;
		       Searchport: in Natural := UPnP_Multicast_Port;
		       Host: in Ipv4_Host_Type := Default_Host_Field)
		      return UPnP_Root_Device;

   Overriding procedure M_Search_Response
     (Device: in UPnP_Root_Device;
      USN_Requester: in String;
      To: in SSDP.Services.Sock_Addr_Type;
      Other_Headers: in Message_Header_Array := Null_Header_Array);

   Overriding procedure Notify_Alive
     (Device: in UPnP_Root_Device;
      Other_Headers: in Message_Header_Array := Null_Header_Array);

   Overriding procedure Notify_Bye_Bye
     (Device: in UPnP_Root_Device;
      Other_Headers: in Message_Header_Array := Null_Header_Array;
      Remove: in Boolean := True);

   procedure Start_Listening renames SSDP.Services.Start_Listening;

   procedure Stop_Listening renames SSDP.Services.Stop_Listening;

private
   use SSDP.Services;

   type UPnP_Embedded_Device(Service_Count: Positive) is record
      Naming: Identification_Mode;
      Name: Unbounded_String;
      Version: Natural;
      Service: UPnP_Service_Array(1..Service_Count);
   end record;

   type UPnP_Root_Device(Embedded_Count: Natural) is new SSDP_Service with
      record
	 -- Fields from SSDP_Service:
	 --  Universal_Serial_Number,
	 --  Service_Type,
	 --  Location: Unbounded_String;
	 --  Cache_Control: Natural;
	 Host: Ipv4_Host_Type;
	 Server: Unbounded_String;
	 Bootid: Positive;
	 Configid,
	 Searchport: Natural;
	 Embedded: UPnP_Embedded_Device_Access_Array(1..Embedded_Count);
      end record;

end UPnP.Services;

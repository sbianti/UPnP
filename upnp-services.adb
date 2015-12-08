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

with Ada.Strings.Fixed;
with UPnP.Utils;

package body UPnP.Services is
   use SSDP.Services, SSDP, UPnP.Utils;

   Root_Device_NT: constant String := "upnp:rootdevice";

   function Create_Embedded_Device
     (Naming: in Identification_Mode;
      Name: in String;
      Version: in Natural;
      Services: in UPnP_Service_Array) return UPnP_Embedded_Device is
   begin
      return (Services'Length, Naming, To_US(Name), Version, Services);
   end Create_Embedded_Device;

   function Initialize(Service_Type,
			 Universal_Serial_Number,
			 Location: in String;
		       Cache_Control: in Positive)
		      return UPnP_Root_Device is
   begin
      Pl_Error("No server set");
      return Initialize(Universal_Serial_Number => Universal_Serial_Number,
			Location => Location,
			Cache_Control => Cache_Control,
			Server =>  "",
			Embedded => (1..0 => null));
   end Initialize;

   function Initialize(Service_Type, Universal_Serial_Number,
			 Location, AL, -- only one is required
			 Expires: in String;
		       Cache_Control: in Natural := 0) -- dito
		      return UPnP_Root_Device is
   begin
      Pl_Error("No server set");
      return Initialize(Universal_Serial_Number, Location,
			Cache_Control, "",
			UPnP_Embedded_Device_Access_Array'(1..0 => null));
   end Initialize;

   procedure Split_Domain(Device: in UPnP_Embedded_Device;
			  Name: out Unbounded_String;
			  NT: out Unbounded_String) is
      Extended_Name: String := To_String(Device.Name);
      Colon_Index: Positive := Ada.Strings.Fixed.Index(Extended_Name, ":");
   begin
      if Colon_Index = 0 then
	 Name := To_US("No domain Name");
	 NT := To_US("No Service type");
	 Pl_Warning("No ':' found → Neither domain name, nor NT.");
      else
	 Name := To_US(Extended_Name(1..Colon_Index - 1));
	 NT := To_US(Extended_Name(Colon_Index + 1..Extended_Name'Last));
      end if;
   end Split_Domain;

   function Format_Service_Type(Device: in UPnP_Embedded_Device;
				S: in Natural) return String is

      function Version(Service: in UPnP_Service) return String is
      begin
	 return Service.Version'Img(2..Service.Version'Img'Last);
      end Version;

      Domain_Name, Domain_NT: Unbounded_String;
   begin
      case Device.Naming is
	 when Schemas =>
	    return "urn:schemas-upnp-org:service:" &
	      To_String(Device.Service(S).Name) & ":" &
	      Version(Device.Service(S));
	 when Domain =>
	    Split_Domain(Device, Domain_Name, Domain_NT);

	    return "urn:" & To_String(Domain_Name) & ":service:" &
	      To_String(Domain_NT) & ":" & Version(Device.Service(S));
      end case;
   end Format_Service_Type;

   function Format_USN(Universal_Serial_Number: in Unbounded_String;
		       Device: in UPnP_Embedded_Device;
		       S: in Natural) return String is
   begin
      return "uuid:" & To_String(Universal_Serial_Number) & "::" &
	Format_Service_Type(Device, S);
   end Format_USN;

   function Format_Service_Type(Device: in UPnP_Embedded_Device)
			       return String is
      Domain_Name, Domain_NT: Unbounded_String;
   begin
      case Device.Naming is
	 when Schemas =>
	    return "urn:schemas-upnp-org:device:" & To_String(Device.Name) &
	      ":" & Device.Version'Img(2..Device.Version'Img'Last);
	 when Domain =>
	    Split_Domain(Device, Domain_Name, Domain_NT);

	    return "urn:" & To_String(Domain_Name) & ":device:" &
	      To_String(Domain_NT) & ":" &
	      Device.Version'Img(2..Device.Version'Img'Last);
      end case;
   end Format_Service_Type;

   function Format_USN(Universal_Serial_Number: in Unbounded_String;
		       Device: in UPnP_Embedded_Device) return String is
   begin
      return "uuid:" & To_String(Universal_Serial_Number) & "::" &
	Format_Service_Type(Device);
   end Format_USN;

   function Initialize(Universal_Serial_Number, Location: in String;
		       Cache_Control: in Natural;
		       Server: in String;
		       Embedded: in UPnP_Embedded_Device_Access_Array;
		       Bootid: in Positive := 1;
		       Configid: in Natural := 0;
		       Searchport: in Natural := UPnP_Multicast_Port;
		       Host: in Ipv4_Host_Type := Default_Host_Field)
		      return UPnP_Root_Device is

      function Initialize_All(NT, USN, Location: in String;
			      Cache_Control: in Natural) return SSDP_Service is
	 Tmp: SSDP_Service;
      begin
	 for D in Embedded'Range loop
	    for S in Embedded(D).Service'Range loop
	       Tmp := Initialize(Format_Service_Type(Embedded(D).all, S),
				 Format_USN(To_US(USN), Embedded(D).all, S),
				 Location, Cache_Control);
	    end loop;

	    Tmp := Initialize(Format_Service_Type(Embedded(D).all),
			      Format_USN(To_US(USN), Embedded(D).all),
			      Location, Cache_Control);

	    Tmp := Initialize("uuid:" & USN, "uuid:" & USN,
			      Location, Cache_Control);
	 end loop;

	 Tmp := Initialize(Root_Device_NT,
			   "uuid:" & USN & "::" & Root_Device_NT,
			   Location,
			   Cache_Control);

	 Tmp.Set_USN(To_US(USN));

	 return Tmp;
      end Initialize_All;

      SSDP_Part: SSDP_Service := Initialize_All(Root_Device_NT,
						Universal_Serial_Number,
						Location,
						Cache_Control);
   begin
      return (SSDP_Part with
		Embedded'Length,
	      Host,
	      To_US(Server),
	      Bootid,
	      Configid,
	      Searchport,
	      Embedded
	     );
   end Initialize;

   function Get_Announcing_Headers(Device: in UPnP_Root_Device)
				  return Message_Header_Array is
      Headers: Message_Header_Array(1..5);
   begin
      Headers(1..4) := (To_US("HOST: " & Device.Host),
			To_US("BOOTID.UPNP.ORG:" & Device.Bootid'Img),
			To_US("CONFIGID.UPNP.ORG:" & Device.Configid'Img),
			To_US("SERVER: ") & Device.Server);

      if Device.Searchport /= UPnP_Multicast_Port then
	 Headers(5) := To_US("SEARCHPORT.UPNP.ORG:" & Device.Searchport'Img);
	 return Headers;
      else
	 return Headers(1..4);
      end if;
   end Get_Announcing_Headers;

   Overriding procedure M_Search_Response
     (Device: in UPnP_Root_Device;
      USN_Requester: in String;
      To: in SSDP.Services.Sock_Addr_Type;
      Other_Headers: in Message_Header_Array := Null_Header_Array)
   is
      Headers: Message_Header_Array := Get_Announcing_Headers(Device);
   begin
      M_Search_Response(SSDP_Service(Device), USN_Requester, To, Other_Headers);
   end M_Search_Response;

   Overriding procedure Notify_Alive
     (Device: in UPnP_Root_Device;
      Other_Headers: in Message_Header_Array := Null_Header_Array)
   is
      Headers: Message_Header_Array := Get_Announcing_Headers(Device);
      SSDP_Part: SSDP_Service := SSDP_Service(Device);
   begin
      if Other_Headers /= Null_Header_Array then
	 Pl_Warning("Notify_Alive ignores supplementary headers");
      end if;

      SSDP_Part.Set_NT(To_US(Root_Device_NT));
      SSDP_Part.Set_USN(To_US("uuid:") & Device.Get_USN &
			  To_US("::" & Root_Device_NT)
		       );
      Notify_Alive(SSDP_Part, Headers);

      for D in Device.Embedded'Range loop
	 SSDP_Part.Set_NT(To_US(Format_Service_Type(Device.Embedded(D).all)));
	 SSDP_Part.Set_USN(To_US(Format_USN(Get_USN(Device),
					    Device.Embedded(D).all)));
	 Notify_Alive(SSDP_Part, Headers);

	 SSDP_Part.Set_NT(To_US("uuid:") & Device.Get_USN);
	 SSDP_Part.Set_USN(SSDP_Part.Get_NT);
	 Notify_Alive(SSDP_Part, Headers);

	 for S in Device.Embedded(D).Service'Range loop
	    SSDP_Part.Set_NT(To_US(Format_Service_Type(Device.Embedded(D).all,
						       S)));
	    SSDP_Part.Set_USN(To_US(Format_USN(Get_USN(Device),
					       Device.Embedded(D).all, S)));
	    Notify_Alive(SSDP_Part, Headers);
	 end loop;
      end loop;
   end Notify_Alive;

   Overriding procedure Notify_Bye_Bye
     (Device: in UPnP_Root_Device;
      Other_Headers: in Message_Header_Array := Null_Header_Array;
      Remove: in Boolean := True)
   is
      Headers: Message_Header_Array(1..3);
      SSDP_Part: SSDP_Service := SSDP_Service(Device);
   begin
      Headers := (To_US("HOST: " & Device.Host),
		  To_US("BOOTID.UPNP.ORG:" & Device.Bootid'Img),
		  To_US("CONFIGID.UPNP.ORG:" & Device.Configid'Img));

      for D in Device.Embedded'Range loop
	 for S in Device.Embedded(D).Service'Range loop
	    SSDP_Part.Set_NT(To_US(Format_Service_Type(Device.Embedded(D).all,
						       S)));
	    SSDP_Part.Set_USN(To_US(Format_USN(Get_USN(Device),
					       Device.Embedded(D).all, S)));
	    Notify_Bye_Bye(SSDP_Part, Headers, Remove);
	 end loop;

	 SSDP_Part.Set_NT(To_US(Format_Service_Type(Device.Embedded(D).all)));
	 SSDP_Part.Set_USN(To_US(Format_USN(Get_USN(Device),
					    Device.Embedded(D).all)));
	 Notify_Bye_Bye(SSDP_Part, Headers, Remove);

	 SSDP_Part.Set_NT(To_US("uuid:") & Device.Get_USN);
	 SSDP_Part.Set_USN(SSDP_Part.Get_NT);
	 Notify_Bye_Bye(SSDP_Part, Headers, Remove);
      end loop;

      SSDP_Part.Set_NT(To_US(Root_Device_NT));
      SSDP_Part.Set_USN(To_US("uuid:") & Device.Get_USN &
			  To_US("::" & Root_Device_NT)
		       );
      Notify_Bye_Bye(SSDP_Part, Headers, Remove);
   end Notify_Bye_Bye;

end UPnP.Services;

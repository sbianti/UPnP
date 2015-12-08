--------------------------------------------------------------------------------
--  This file is part of UPnP package			        	      --
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

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Fixed;

with SSDP;
with UPnP.Controlers;
with UPnP.Command_Scheduling;

with Test_Utils;

with Get_Options; --https://github.com/sbianti/GetOptions

procedure Test_UPnP_Controlers is
   use SSDP, Ada.Exceptions, Test_Utils, UPnP.Controlers;

   Controler: UPnP_Controler;

   Default_UUID: aliased constant String :=
     "uuid:0dbcf247-96ca-4d58-b3de-fedcba987654";
   UUID_Value: access constant String;

   Default_Service_Type: aliased constant String := "ssdp:all";
   Service_Type_Value: access constant String;

   User_Agent: constant String := "Linux/5.0 UPnP/1.1 test_controler/0.0";

   Str: String(1..10);

   Lg: Natural;

procedure Default_Scheduling is
      use Ada.Text_IO;
   begin
      Controler := Initialize(Service_Type_Value.all, UUID_Value.all,
			      User_Agent => User_Agent, MX => 3);

      Start_Listening;

      delay 0.5;
      M_Search(Controler, Null_Header_Array);
      Put_Line("First discover sent");

      delay 1.0;
      M_Search(Controler, Null_Header_Array);
      Put_Line("Second discover sent");

      Get_Line(Str, Lg);

      Stop_Listening;
   end Default_Scheduling;

   type Test_Options is (Batch, Service_Type, UUID);

   package Get_Test_Options is new Get_Options(Test_Options);
   use Get_Test_Options;

   EOL: constant Character := Character'Val(10);

   Help_Section: constant Unbounded_String := To_US("Options:");

   Help_Header: constant String :=
     "   Test program for UPnP clients API" & EOL & EOL &
     "   usage: " & Command_Name &
     " [--batch «batch_line»][--service_type=rootdevice]" &
     "[--uuid [type:]unique_value]" & EOL & EOL &
     "     batch_line ≡ command [command ]*" & EOL &
     "     command ≡ command_name[,occurence_number[,random_time_range]" &
     "|[,fix_delay]]" & EOL &
     "       command_name ∈ {discover, sleep}" & EOL &
     "       occurence_number ≡ INTEGER_VALUE" & EOL &
     "       fix_delay ≡ DECIMAL_VALUE" & EOL &
     "       random_time_range ≡ lower_bound,upper_bound" & EOL &
     "       lower_bound and upper_bound ∈ DECIMAL_VALUES";

   Result: Option_Result_Array;

   Setting: Option_Setting_Array :=
     (Batch =>
	(Short_Name => No_Short_Name,
	 Needs_Value => Yes,
	 Value_Form =>
	   To_US("""discover,3,0.5,2.5 sleep,10.0 discover,3,1.0,3.0"""),
	 Short_Description =>
	   To_US("Three DISCOVER sent with a random delay between 0”5 and 2”5" &
		   EOL & "followed by a delay of 10”" & EOL &
		   "followed by three DISCOVER spaced by a random duration " &
		   "between 1” and 3”")),

      Service_Type =>
	(Short_Name => 't',
	 Needs_Value => Yes,
	 Value_Form => To_US(Default_Service_Type),
	 Short_Description =>
	   To_US("The device service type to search for." &
		   " Available values are:" & EOL &
		   "   all → search for all ssdp devices." & EOL &
		   "         A UPnP device receiving that will advertize all its internal devices and services" & EOL &
		   "   rootdevice → Search for root devices only." & EOL &
		   "   uuid:[UUID] → Search for this particular device" & EOL &
		   "   device:[Type]:[version] → Search for this particular device/version as declared by the UPnP Forum" & EOL &
		   "   service:[Type]:[version] → Search for this particular service/version as declared by the UPnP Forum" & EOL &
		   "   [Domain-Name]:device:[Type]:[version] → Search for this particular [device]:[version] as declared by this specific [Domain]"  & EOL &
		   "   [Domain-Name]:service:[Type]:[version] → Search for this particular [service]:[version] as declared by this specific [Domain]"
		)),

      UUID =>
	(Short_Name => 'i',
	 Needs_Value => Yes,
	 Short_Description => To_US("Set the uuid of this controler"),
	 Value_Form => To_US("uuid:AAAABBBB-1111-2222-3333-CCDDEEFF0055"))
     );

   package Scheduling is new UPnP.Command_Scheduling(Client_Command_Name_Type);
   use Scheduling;

   Schedule: Schedule_Type;

begin
   Result := Parse(Setting, Help_Header, "", Help_Sections =>
		     (Batch => Help_Section,
		      others => Null_Unbounded_String));

   if Result(Service_Type).Is_Set then
      declare
	 use UPnP, Ada.Strings.Fixed;
	 Posn_Serv, Posn_Dev: Natural;
	 Domain_Name_Min_Width: constant Natural := 5; -- arbitrary
	 ST: access String := Result(Service_Type).Value;
      begin
	 if ST.all = "all" then
	    Service_Type_Value := new String'(Service_Type_Field(ST_All));
	 elsif ST.all = "rootdevice" then
	    Service_Type_Value :=
	      new String'(Service_Type_Field(ST_Root_Device));
	 elsif ST.all'Length < 12 then raise Scheduling.Parsing_Error
	   with "Incorrect Service-Type given: " & ST.all &
	   " see --help documentation";
	 elsif ST.all(1..5) = "uuid:" then
	    Service_Type_Value :=
	      new String'(Service_Type_Field(ST_UUID, ST.all(6..ST'Last)));
	 elsif ST.all(1..7) = "device:" then
	    Service_Type_Value :=
	      new String'(Service_Type_Field(Urn_Schemas_Device,
					     ST.all(8..ST'Last)));
	 elsif ST.all(1..8) = "service:" then
	    Service_Type_Value :=
	      new String'(Service_Type_Field(Urn_Schemas_Service,
					     ST.all(9..ST'Last)));
	 else
	    Posn_Dev := Index(ST.all, ":device:");
	    Posn_Serv := Index(ST.all, ":service:");

	    if Posn_Dev >= Domain_Name_Min_Width then
	       Service_Type_Value :=
		 new String'(Service_Type_Field(Urn_Domain_Device,
						ST.all(Posn_Dev + 8..ST'Last),
						ST.all(1..Posn_Dev - 1)));
	    elsif Posn_Serv >= Domain_Name_Min_Width then
	       Service_Type_Value :=
		 new String'(Service_Type_Field(Urn_Domain_Service,
						ST.all(Posn_Serv + 9..ST'Last),
						ST.all(1..Posn_Serv - 1)));
	    else raise Scheduling.Parsing_Error
	      with "Service-Type " & ST.all & " is not well formed: see --help";
	    end if;
	 end if;
      end;
      Ada.Text_IO.Put_Line("ST: " & Service_Type_Value.all);
   else
      Service_Type_Value := Default_Service_Type'Access;
   end if;

   if Result(UUID).Is_Set then
      UUID_Value := new String'(Get_Value(Result(UUID), 1));
   else
      UUID_Value := Default_UUID'Access;
   end if;

   if not Result(Batch).Is_Set then
      Default_Scheduling;
   else
      Controler := Initialize(Service_Type_Value.all, UUID_Value.all,
			      User_Agent => User_Agent, MX => 3);

      Start_Listening;

      Schedule := Parse(Result(Batch).Value.all);

      Batch(Controler, Null_Header_Array, Schedule);

      -- Waiting for user interaction:
      Ada.Text_IO.Get_Line(Str, Lg);
      Stop_Listening;
   end if;

exception
   when End_Of_Program_With_Help_Menu =>
      Stop_Listening;

   when E: others =>
      Pl_Error(Exception_Message(E));
      Stop_Listening;
end Test_UPnP_Controlers;

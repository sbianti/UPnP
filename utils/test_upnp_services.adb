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

with UPnP.Services;
with UPnP.Command_Scheduling;

with Test_Utils, Ada.Strings.Unbounded;

with Get_Options; --https://github.com/sbianti/GetOptions

procedure Test_UPnP_Services is
   use SSDP, UPnP, Ada.Exceptions, Test_Utils, UPnP.Services;

   type Test_Options is (Batch, UUID, Bye_Bye_On_Exit, Lifetime,
			 Device_Name, Service_Names);

   package Get_Test_Options is new Get_Options(Test_Options);
   use Get_Test_Options;

   EOL: constant Character := Character'Val(10);

   Help_Section: constant Unbounded_String := To_US("Options:");

   Help_Header: constant String :=
     "   Test program for UPnP services API" & EOL & EOL &
     "   usage: " & Command_Name &
     " [--batch «batch_line»] [--device_name=«name»] " &
     "[--services=«service_1:version_1,service_2:version_2…]" &
     "[--bye_bye_on_exit] [--uuid [type:]unique_value]" & EOL & EOL &
     "     batch_line ≡ command [command ]*" & EOL &
     "     command ≡ command_name[,occurence_number[,random_time_range]" &
     "|[,fix_delay]]" & EOL &
     "       command_name ∈ {alive, sleep, byebye}" & EOL &
     "       occurence_number ≡ INTEGER_VALUE" & EOL &
     "       fix_delay ≡ DECIMAL_VALUE" & EOL &
     "       random_time_range ≡ lower_bound,upper_bound" & EOL &
     "       lower_bound and upper_bound ∈ DECIMAL_VALUES";

   Default_UUID: aliased constant String :=
     "822ccbbf-3aa6-44c2-80ef-0307f9123456";
   UUID_Value: access constant String;

   Default_Lifetime: constant Positive := 1800;
   Lifetime_Value: Positive;

   Default_Services: aliased constant UPnP_Service_Array :=
     ((To_US("Fake Video Service"), 1),
      (To_US("Fake Audio Service"), 1),
      (To_US("Fake combo service"), 2));
   Service_Array: access constant UPnP_Service_Array;

   Default_Device_Name: aliased constant String := "UPnP fake embedded device";
   Device_Name_Value: access constant String;

   Device_Version: Natural := 1;

   Str: String(1..10);

   Lg: Natural;

   Result: Option_Result_Array;

   Setting: Option_Setting_Array :=
     (Batch =>
	(Short_Name => No_Short_Name,
	 Needs_Value => Yes,
	 Short_Description =>
	   To_US("Five ALIVE sent with a random delay between 1”5 and 4”" &
		   EOL & "followed by a delay of 12”" & EOL &
		   "followed by three ALIVE spaced by a fix duration of 2”" &
		   EOL & "followed by two BYEBYE messages sent without delay"),
	 Value_Form =>
	   To_US("""alive,5,1.5,4.0 sleep,12.0 alive,3,2.0 byebye,2""")),

      Bye_Bye_On_Exit =>
	(Short_Name => 'b',
	 Needs_Value => Optional,
	 Short_Description =>
	   To_US("One, or many bye-byes are sent when exiting normaly"),
	 Value_Form => To_US("2")),

      UUID =>
	(Short_Name => 'i',
	 Needs_Value => Yes,
	 Short_Description => To_US("Set the uuid of the declared service"),
	 Value_Form => To_US("uuid:AAAABBBB-1111-2222-3333-CCDDEEFF0055")),

      Lifetime =>
	(Short_Name => 'l',
	 Needs_Value => Yes,
	 Short_Description => To_US("The amount of time (in second) after " &
				      "which the service is considered gone"),
	 Value_Form => To_US("300")),

      Device_Name =>
	(Short_Name => 'd',
	 Needs_Value => Yes,
	 Short_Description => To_US("The name and the version of the " &
				      "embedded device, separated by a colon"),
	 Value_Form => To_US("DimmableLight:1")),

      Service_Names =>
	(Short_Name => 's',
	 Needs_Value => Yes,
	 Short_Description => To_US("A list of “service_name:version”" &
				      " separated by colons"),
	 Value_Form => To_US("SwitchPower:1, Dimming:1"))
     );

   function Default_Initialization return UPnP_Root_Device is
      Embedded_Devices: UPnP_Embedded_Device :=
	Create_Embedded_Device(Schemas,
			       Device_Name_Value.all, Device_version,
			       Service_Array.all);
      Devices_Access: UPnP_Embedded_Device_Access :=
	new UPnP_Embedded_Device'(Embedded_Devices);
   begin
      return Initialize(Universal_Serial_Number => UUID_Value.all,
			Location => "<http://lala.yoyo.com>",
			Cache_Control => Lifetime_Value,
			Server => "Linux/??, UPnP/1.0, test_services",
			Embedded => (1 => Devices_Access)
		       );
   end Default_Initialization;

   package Scheduling is new Command_Scheduling(Service_Command_Name_Type);
   use Scheduling;

   Schedule: Schedule_Type;

   procedure Default_Scheduling(Device: in UPnP_Root_Device) is
      use Ada.Text_IO, Ada.Strings.Unbounded;
   begin
      Start_Listening;
      Put_Line("Nouveau Root Device: NT:" & To_String(Device.Get_NT));
      Put_Line("                    USN:" & To_String(Device.Get_USN));

      -- At first, a freshly connected device should send byebyes for all of its
      -- {root-device, embedded devices and embedded services}
      Notify_Bye_Bye(Device, Remove => False);

      delay 0.2;
      Notify_Alive(Device, Null_Header_Array);
      Put_Line("Notify sent");

      delay 2.5;
      Notify_Alive(Device, Null_Header_Array);
      Put_Line("Second notify sent");

      Get_Line(Str, Lg);
      Put_Line("Bye bye !");
      Notify_Bye_Bye(Device);

      Stop_Listening;
   end Default_Scheduling;

   package Natural_IO is new Ada.Text_IO.Integer_IO(Positive);

   procedure Get_Global_Device_Values(Value: in String) is
      use Ada.Strings, Ada.Strings.Fixed;
      Posn: Natural := Fixed.Index(Value, ":");
      Last: Natural;
   begin
      if Posn = 0 then raise Scheduling.Parsing_Error
	with "device name should follow this format: “Name:Version”";
      elsif Posn = Value'First then raise Scheduling.Parsing_Error
	with "device name empty: «" & Value & "»" & EOL & "See --help";
       elsif Posn = Value'Last then raise Scheduling.Parsing_Error
	with "no version number: «" & Value & "»" & EOL & "See --help";
      end if;

      Device_Name_Value := new String'(Trim(Value(Value'First..Posn - 1),
					    Side => Left));
      Natural_IO.Get(Value(Posn + 1..Value'Last), Device_Version, Last);

   exception
      when Ada.Text_IO.Data_Error => raise Scheduling.Parsing_Error
	 with "device version needs to be an integer, not «" &
	 Value(Posn + 1..Value'Last) & "»" & EOL & "See --help";
   end Get_Global_Device_Values;

   procedure Get_Global_Services_Values(Value: in String) is
      use Ada.Strings.Fixed;

      Coma_Posn: Natural;
      Current: Natural := Value'First;
      Count: Natural := 0;
   begin
      if Value'Length = 0 then raise Scheduling.Parsing_Error
	with "Empty list of services disallowed: see --help";
      elsif Value(Value'Last) = ':' then raise Scheduling.Parsing_Error
	with "':' must be followed by a version number: «" & Value & "»";
      end if;

      loop
	 Coma_Posn := Index(Value, ",", From => Current);

	 if Coma_Posn = Current then raise Scheduling.Parsing_Error
	   with "Empty service name: value beginning or finishing with ',' " &
	   "or having «,,» are disallowed";
	 end if;

	 Count := Count + 1;

	 exit when Coma_Posn = 0;

	 Current := Coma_Posn + 1;
      end loop;

      declare
	 Services: UPnP_Service_Array(1..Count);
	 Colon_Posn, Version_Val, Last: Natural;
	 use Natural_IO, Ada.Strings;
      begin
	 Current := Value'First;

	 for I in 1..Count loop
	    Coma_Posn := Index(Value, ",", From => Current);

	    if Coma_Posn = 0 then
	       Coma_Posn := Value'Last;
	    end if;

	    -- backward to allow the service name to have a ':' inside:
	    Colon_Posn := Index(Value(Current..Coma_Posn), ":", Backward);
	    if Colon_Posn = 0 then raise Scheduling.Parsing_Error
	      with "«:version» missing in this service: «" &
	      Value(Current..Coma_Posn) & "»";
	    elsif Colon_Posn = Coma_Posn then raise Scheduling.Parsing_Error
	      with "empty version number here: " & Value(Current..Coma_Posn)
	      & " see --help";
	    end if;

	    begin
	       Get(Value(Colon_Posn + 1..Coma_Posn), Version_Val, Last);
	    exception
	       when Ada.Text_IO.Data_Error => raise Scheduling.Parsing_Error
		  with "version number needs to be an Integer here: «" &
		  Value(Colon_Posn + 1..Coma_Posn) & "»";
	    end;

	    Services(I) := (To_US(Trim(Value(Current..Colon_Posn - 1), Both)),
			    Version_Val);

	    Current := Coma_Posn + 1;
	 end loop;

	 Service_Array := new UPnP_Service_Array'(Services);
      end;
   end Get_Global_Services_Values;

   Last: Natural;
begin
   Result := Parse(Setting, Help_Header, "", Help_Sections =>
		     (Batch => Help_Section, others => Null_Unbounded_String));

   if Result(UUID).Is_Set then
      UUID_Value := Result(UUID).Value;
   else
      UUID_Value := Default_UUID'Access;
   end if;

   if Result(Lifetime).Is_Set then
      Natural_IO.Get(Result(Lifetime).Value.all, Lifetime_Value, Last);
   else
      Lifetime_Value := Default_Lifetime;
   end if;

   if Result(Device_Name).Is_Set then
      Get_Global_Device_Values(Get_Value(Result(Device_Name), 1));
   else
      Device_Name_Value := Default_Device_Name'Access;
   end if;

   if Result(Service_Names).Is_Set then
      Get_Global_Services_Values(Get_Value(Result(Service_Names), 1));
   else
      Service_Array := Default_Services'Access;
   end if;

   if not Result(Batch).Is_Set then
      Default_Scheduling(Default_Initialization);
   else
      declare
	 Root_Device: UPnP_Root_Device := Default_Initialization;
      begin

	 Start_Listening;

	 Schedule := Parse(Result(Batch).Value.all);

	 Batch(Root_Device, Null_Header_Array, Schedule);

	 -- Waiting for user interaction:
	 Ada.Text_IO.Get_Line(Str, Lg);

	 if Result(Bye_Bye_On_Exit).Is_Set then
	    declare
	       Bye_Bye_Value: String := Result(Bye_Bye_On_Exit).Value.all;
	       Bye_Bye_Number: Positive;
	    begin

	       if Bye_Bye_Value = "" then
		  Bye_Bye_Number := 1;
	       else
		  Natural_IO.Get(Bye_Bye_Value, Bye_Bye_Number, Last);

		  if Bye_Bye_Number > 30 then
		     Pl_Warning("Bye-Bye number limited to 30 to avoid flooding");
		     Bye_Bye_Number := 30;
		  end if;
	       end if;

	       Pl_Debug("Sending" & Bye_Bye_Number'Img & " final ByeBye");
	       for I in 1..Bye_Bye_Number loop
		  Notify_Bye_Bye(Root_Device);
	       end loop;

	    exception
	       when E: Ada.Text_IO.Data_Error =>
		  Pl_Error(Exception_Name(E) & ": Bad bye_bye value '" &
			     Bye_Bye_Value & "' should be a positive number." &
			     " Sending one ByeBye");
		  Notify_Bye_Bye(Root_Device);
	       when E: others =>
		  Pl_Debug(Exception_Information(E));
	    end;
	 end if;

	 Stop_Listening;
      end;
   end if;

   exception
      when End_Of_Program_With_Help_Menu => Stop_Listening;

      when E: others =>
	 Pl_Error(Exception_Message(E));
	 Stop_Listening;
end Test_UPnP_Services;

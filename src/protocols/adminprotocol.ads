with Types;
with Endianess;

package AdminProtocol is

   subtype Cmd_NativeType is Types.Integer32;
   subtype Cmd_NetworkType is Endianess.LittleEndianInteger32;

   CmdMessage : constant Cmd_NativeType:=0;

end AdminProtocol;

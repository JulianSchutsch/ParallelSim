with Network.Config;
with Basics; use Basics;

package SimAdmin is

   procedure Initialize
     (NetworkImplementation : Network.Config.Implementation_Type;
      Config                : StringStringMap.Map);

   procedure Finalize;

   function Process
     return Boolean;

end SimAdmin;

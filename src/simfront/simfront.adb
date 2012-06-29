-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published
--   by the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

pragma Ada_2005;

with SimNodes; use SimNodes;
with ProcessLoop;
with Logging;
with Basics; use Basics;

package body SimFront is


   LogImplementation : Logging.Implementation_Type;
   LogContext        : Logging.Context_ClassAccess:=null;
   LogChannel        : Logging.Channel_ClassAccess:=null;

   procedure Initialize is
   begin
      SimNodes.Initialize(SimNodes.NodeTypeFront);
      LogImplementation:=Logging.Implementations.Find
        (Configuration => Configuration,
         Node          => U("Logging"));
      LogContext:=LogImplementation.NewContext
        (Configuration => Configuration,
         ConfigNode    => U("Logging"),
         ModuleName    => U("SimFront"));
      LogContext.NewChannel
        (ChannelName => U("Main"),
         Channel     => LogChannel);
      LogChannel.Write
        (Level   => Logging.LevelEvent,
         Message => "Simfront initialized?");
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      LogImplementation.FreeContext(LogContext);
      SimNodes.Finalize;
   end Finalize;
   ---------------------------------------------------------------------------

   function Process
     return Boolean is
   begin
      ProcessLoop.Process;
      return Terminated;
   end Process;
   ---------------------------------------------------------------------------

end SimFront;

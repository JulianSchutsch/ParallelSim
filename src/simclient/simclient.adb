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

with Basics; use Basics;

package body SimClient is

   LogImplementation : Logging.Implementation_Type;

   procedure Initialize
     (Configuration : Config.Config_Type) is
   begin
      LogImplementation:=Logging.Implementations.Find(U("StdOut"));
      LogContext:=LogImplementation.NewContext
        (Configuration => Configuration,
         ConfigNode    => U(""),
         ModuleName    => U("SimClient"));
      LogContext.NewChannel
        (ChannelName => U("Main"),
         Channel     => LogMainChannel);
   end Initialize;

   procedure Finalize is
   begin
      LogImplementation.FreeContext(LogContext);
   end Finalize;

end SimClient;

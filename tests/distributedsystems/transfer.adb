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

-- Revision History
--   22.Jun 2012 Julian Schutsch
--     - Original version

procedure Transfer is

   Implementation : DistributedSystems.Implementation_Type;
   Configuration  : Config.Config_Type;

begin

   ProgramArguments.Initialize;

   Network.UseImplementations.Register;
   DistributedSystems.UseImplementations.Register;

   Implementation:=DistributedSystems.Implementations.Find
     (Configuration => ProgramArguments.Configuration,
      Node          => U("Arguments"));

   Implementation.InitializeNode(Configuration);

   -- All nodes send large chunks of data to node0
   -- Node0 has to verify these packets
   -- Node0 only terminates properly if all packets are received
   --  and contain the right data
   -- Data generated is based on random integers and the
   --  Global ID is used to initialize the seed.



   Implementation.FinalizeNode.all;

   DistributedSystems.UseImplementations.Unregister;
   Network.UseImplementations.Unregister;

end Transfer;

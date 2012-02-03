package Network.Peers is
   type PeerData is abstract tagged null record;

   procedure OnReceive(Data: PeerData) is abstract;

end Network.Peers;

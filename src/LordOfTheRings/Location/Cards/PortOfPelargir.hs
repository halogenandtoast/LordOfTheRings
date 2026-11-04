module LordOfTheRings.Location.Cards.PortOfPelargir (portOfPelargir) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype PortOfPelargir = PortOfPelargir LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

portOfPelargir :: LocationCard PortOfPelargir
portOfPelargir = location PortOfPelargir Cards.portOfPelargir

-- | While Port of Pelargir is in the staging area, no more than 1 damage can be healed from
-- each character each round.
instance HasModifiersFor PortOfPelargir where
  getModifiersFor (PortOfPelargir a) = when (a.placement == StagingArea) do
    characters <- selectCharacters AnyHero AnyAlly
    for_ characters \c -> modified a c [HealLimitEachRound 1]

-- | Shadow: If this attack damages a character, that character cannot ready until the end
-- of the round.
instance RunMessage PortOfPelargir where
  runMessage msg l@(PortOfPelargir a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      attackModifier (CardIdSource cid) eid (AttackDamageModifiesDamagedUntilEndOfRound CannotReady)
      pure l
    _ -> PortOfPelargir <$> runMessage msg a

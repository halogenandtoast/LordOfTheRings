module LordOfTheRings.Location.Cards.MakeshiftFortification (makeshiftFortification) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation, InPlayArea))

newtype MakeshiftFortification = MakeshiftFortification LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

makeshiftFortification :: LocationCard MakeshiftFortification
makeshiftFortification = location MakeshiftFortification Cards.makeshiftFortification

-- | While Makeshift Fortification is the active location, it gains archery 3.
instance HasModifiersFor MakeshiftFortification where
  getModifiersFor (MakeshiftFortification a) = when (a.placement == ActiveLocation) $ modifySelf a [GainKeyword (Archery 3)]

-- | Forced: After a player takes control of Makeshift Fortification, return the topmost
-- enemy in the encounter discard pile to the staging area. Read off the location as it goes
-- to his play area.
instance RunMessage MakeshiftFortification where
  runMessage msg (MakeshiftFortification a) = case msg of
    PlaceLocation lid (InPlayArea pid) | lid == a.id && a.placement /= InPlayArea pid -> do
      discards <- getEncounterDiscard
      traverse_ addToStagingFromDiscard $ find (cardMatches $ CardWithType EnemyType) discards
      MakeshiftFortification <$> runMessage msg a
    _ -> MakeshiftFortification <$> runMessage msg a

module LordOfTheRings.Location.Cards.CormallenFields (cormallenFields) where

import LordOfTheRings.Keyword (Keyword (Battle, Siege))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype CormallenFields = CormallenFields LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cormallenFields :: LocationCard CormallenFields
cormallenFields = location CormallenFields Cards.cormallenFields

-- | While the current quest has battle, Cormallen Fields gets +2 threat.
-- While the current quest has siege, when faced with the option to travel, the players
-- must travel to Cormallen Fields, if able.
instance HasModifiersFor CormallenFields where
  getModifiersFor (CormallenFields a) = do
    battle <- currentQuestHas Battle
    siege <- currentQuestHas Siege
    when battle $ modified a a [ThreatModifier 2]
    when siege $ modified a a [MustBeTraveledTo]

instance RunMessage CormallenFields where
  runMessage msg (CormallenFields a) = CormallenFields <$> runMessage msg a

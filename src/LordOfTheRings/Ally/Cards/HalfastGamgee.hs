module LordOfTheRings.Ally.Cards.HalfastGamgee (halfastGamgee) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (Placement (Unplaced))

newtype HalfastGamgee = HalfastGamgee AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

halfastGamgee :: AllyCard HalfastGamgee
halfastGamgee = ally HalfastGamgee Cards.halfastGamgee

-- | Reduce the cost to play Halfast Gamgee by 1 for each side quest in the victory display.
instance HasModifiersFor HalfastGamgee where
  getModifiersFor (HalfastGamgee a) = when (a.placement == Unplaced) do
    n <- getSideQuestsInVictoryDisplay
    when (n > 0) $ modified a (CardIdTarget a.allyCardId) [CostModifier (negate n)]

-- | Response: After Halfast Gamgee enters play, add 1 resource to a hero's resource pool.
instance HasAbilities HalfastGamgee where
  getAbilities (HalfastGamgee a) = [restricted a 1 (exists AnyHero) $ response $ entersPlay a]

instance RunMessage HalfastGamgee where
  runMessage msg s@(HalfastGamgee a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- select AnyHero
      chooseOrRunOneM pid do
        questionLabeled "halfastGamgee.hero"
        causedBy a
        targets heroes \h -> placeResources a h 1
      pure s
    _ -> HalfastGamgee <$> runMessage msg a

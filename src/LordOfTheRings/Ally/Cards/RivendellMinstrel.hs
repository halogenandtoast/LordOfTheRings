module LordOfTheRings.Ally.Cards.RivendellMinstrel (rivendellMinstrel) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Song))

newtype RivendellMinstrel = RivendellMinstrel AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rivendellMinstrel :: AllyCard RivendellMinstrel
rivendellMinstrel = ally RivendellMinstrel Cards.rivendellMinstrel

-- | Response: After you play Rivendell Minstrel from your hand, search your deck for 1
-- Song card and add it to your hand. Shuffle your deck.
instance HasAbilities RivendellMinstrel where
  getAbilities (RivendellMinstrel a) = [mkAbility a 1 $ response $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)]

instance RunMessage RivendellMinstrel where
  runMessage msg s@(RivendellMinstrel a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      searchDeck pid a maxBound (CardWithTrait Song) False
      pure s
    _ -> RivendellMinstrel <$> runMessage msg a

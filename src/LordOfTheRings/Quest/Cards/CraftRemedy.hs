module LordOfTheRings.Quest.Cards.CraftRemedy (craftRemedy) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype CraftRemedy = CraftRemedy QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest (Desert Creatures).
craftRemedy :: QuestCard CraftRemedy
craftRemedy = quest CraftRemedy Cards.craftRemedy

-- | Treat the printed text box of each damaged character as if it were blank (except for
-- Traits): 'Blank' takes its abilities and keywords, and leaves its traits.
instance HasModifiersFor CraftRemedy where
  getModifiersFor (CraftRemedy a) = do
    damaged <- damagedCharacters AnyHero AnyAlly
    for_ damaged \c -> modified a c [Blank]

-- | Response: When this is defeated, heal 1 damage from each character in play. The
-- players gain by it, so it is taken, as Make Camp's is.
instance RunMessage CraftRemedy where
  runMessage msg (CraftRemedy attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      damaged <- damagedCharacters AnyHero AnyAlly
      for_ damaged \c -> healDamage attrs c 1
      CraftRemedy <$> runMessage msg attrs
    _ -> CraftRemedy <$> runMessage msg attrs

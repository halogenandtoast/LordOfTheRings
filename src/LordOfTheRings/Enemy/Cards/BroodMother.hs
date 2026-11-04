module LordOfTheRings.Enemy.Cards.BroodMother (broodMother) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Quest.CardDefs qualified as Quests

newtype BroodMother = BroodMother EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

broodMother :: EnemyCard BroodMother
broodMother = enemy BroodMother Cards.broodMother

-- | Cannot have attachments. Cannot take damage. Brood Mother engages the first player: no
-- other player can engage her. The Spiders' Hive (3B) has her lose "Cannot take damage"
-- while it has at least 5 progress.
instance HasModifiersFor BroodMother where
  getModifiersFor (BroodMother a) = do
    lead <- getLead
    others <- filter (/= lead) <$> getPlayerOrder
    exposed <- getsGame (any hiveExposed . toList . (.entities.quests))
    modifySelf a $ [CannotHaveAttachments, EngagesOnly lead] <> map CannotEngagePlayer others <> [CannotTakeDamage | not exposed]
   where
    hiveExposed q = toCardCode q == toCardCode Quests.theSpidersHive && (toAttrs q).flipped && (toAttrs q).progress >= 5

-- | Forced: At the end of the round, search the encounter deck and discard pile for a
-- Spider Broodling and add it to the staging area, if able. Shuffle the encounter deck.
instance HasAbilities BroodMother where
  getAbilities (BroodMother a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage BroodMother where
  runMessage msg e@(BroodMother attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      searchEncounterDeckAndDiscard lead attrs (cardIs Cards.spiderBroodling)
      pure e
    _ -> BroodMother <$> runMessage msg attrs

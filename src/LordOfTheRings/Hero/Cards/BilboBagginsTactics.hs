module LordOfTheRings.Hero.Cards.BilboBagginsTactics (bilboBagginsTactics) where

import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype BilboBagginsTactics = BilboBagginsTactics HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bilboBagginsTactics :: HeroCard BilboBagginsTactics
bilboBagginsTactics = hero BilboBagginsTactics Cards.bilboBagginsTactics

-- | Response: After Bilbo Baggins commits to the quest, choose an enemy in the staging
-- area. Until the end of the phase, Bilbo Baggins gets +X willpower where X is that
-- enemy's threat. If the players quest successfully, deal 1 damage to that enemy (2 damage
-- instead if it is guarding a card).
--
-- X is counted as the response resolves: a player card's lasting effect is not
-- recalculated (FAQ 1.55). The damage is a delayed effect the game resolves: it is dealt
-- even if Bilbo has left play by then (Rules Reference, Lasting Effects: "whether or not
-- the card that created the lasting effect is or remains in play").
instance HasAbilities BilboBagginsTactics where
  getAbilities (BilboBagginsTactics a) =
    [restricted a 1 (exists EnemyInStagingArea) $ response $ CommittedToQuestWindow After (toTarget a)]

instance RunMessage BilboBagginsTactics where
  runMessage msg h@(BilboBagginsTactics attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      enemies <- select EnemyInStagingArea
      chooseOrRunOneM pid do
        questionLabeled "chooseEnemy"
        causedBy attrs
        targets enemies \eid -> do
          x <- getEnemyThreat eid
          phaseModifier attrs attrs (WillpowerModifier x)
          phaseModifier attrs eid (DamageIfQuestSuccessful 1 2)
      pure h
    _ -> BilboBagginsTactics <$> runMessage msg attrs

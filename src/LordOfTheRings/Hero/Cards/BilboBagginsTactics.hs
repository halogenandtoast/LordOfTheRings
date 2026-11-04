module LordOfTheRings.Hero.Cards.BilboBagginsTactics (bilboBagginsTactics) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameModifiers)
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
-- recalculated (FAQ 1.55). The enemy is marked until the end of the phase for the damage.
instance HasAbilities BilboBagginsTactics where
  getAbilities (BilboBagginsTactics a) =
    [ restricted a 1 (exists EnemyInStagingArea) $ response $ CommittedToQuestWindow After (toTarget a)
    , restricted a 2 (ModifierFrom (toSource a) ChosenByEffect) $ silent $ QuestedSuccessfullyWindow After
    ]

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
          phaseModifier attrs eid ChosenByEffect
      pure h
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      let chosen ms = any (\m -> modifierSource m == toSource attrs && modifierType m == ChosenByEffect) ms
      marked <- getsGame (Map.keys . Map.filter chosen . gameModifiers)
      for_ [eid | EnemyTarget eid <- marked] \eid -> do
        guarding <- selectAny (EnemyWithId eid <> EnemyGuardingACard)
        dealDamage attrs eid (if guarding then 2 else 1)
      pure h
    _ -> BilboBagginsTactics <$> runMessage msg attrs

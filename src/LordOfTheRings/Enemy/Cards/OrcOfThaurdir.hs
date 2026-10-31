module LordOfTheRings.Enemy.Cards.OrcOfThaurdir (orcOfThaurdir) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers (discardTopOfCapturedDeck)

newtype OrcOfThaurdir = OrcOfThaurdir EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcOfThaurdir :: EnemyCard OrcOfThaurdir
orcOfThaurdir = enemy OrcOfThaurdir Cards.orcOfThaurdir

-- | Forced: When Orc of Thaurdir attacks you, either discard the top 2 cards of your
-- captured deck, or deal Orc of Thaurdir 2 additional shadow cards.
instance HasAbilities OrcOfThaurdir where
  getAbilities (OrcOfThaurdir a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

-- | Shadow: For each damage dealt by this attack discard the top card of your captured
-- deck. The scenario reads the attack's damage.
instance RunMessage OrcOfThaurdir where
  runMessage msg e@(OrcOfThaurdir attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] \pid ->
        chooseOneM pid do
          questionLabeled "orcOfThaurdir.choose"
          causedBy attrs
          labeled "orcOfThaurdir.discardCaptured" $ replicateM_ 2 (discardTopOfCapturedDeck pid)
          labeled "orcOfThaurdir.shadow" $ replicateM_ 2 (dealShadowCard attrs.id)
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      attackModifier (CardIdSource cid) eid (AttackDamageDiscardsCaptured pid)
      pure e
    _ -> OrcOfThaurdir <$> runMessage msg attrs

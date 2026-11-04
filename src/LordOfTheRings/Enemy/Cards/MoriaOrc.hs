module LordOfTheRings.Enemy.Cards.MoriaOrc (moriaOrc) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.JourneyInTheDark.Helpers (getDoom, removeDoomDamage)

newtype MoriaOrc = MoriaOrc EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

moriaOrc :: EnemyCard MoriaOrc
moriaOrc = enemy MoriaOrc Cards.moriaOrc

{- | When Revealed: Either remove 1 damage from Doom, Doom, Doom, or reveal an additional
encounter card. The first player decides; the damage is offered only while there is some
to remove (FAQ 1.44: an option is chosen only if it can be done in full).
Shadow: The defending player raises his threat by 1 for each enemy engaged with him.
-}
instance RunMessage MoriaOrc where
  runMessage msg e@(MoriaOrc attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      damage <- maybe (pure 0) (getDamage . ObjectiveTarget) =<< getDoom
      chooseOrRunOneM lead do
        questionLabeled "moriaOrc.choose"
        causedBy attrs
        when (damage > 0) $ labeled "moriaOrc.removeDamage" $ removeDoomDamage attrs
        labeled "moriaOrc.reveal" revealEncounterCard
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      engaged <- length <$> select (EnemyEngagedWith $ PlayerWithId pid)
      when (engaged > 0) $ encounterRaisesThreat pid engaged
      pure e
    _ -> MoriaOrc <$> runMessage msg attrs

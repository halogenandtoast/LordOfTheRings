module LordOfTheRings.Enemy.Cards.AMurderOfCrows (aMurderOfCrows) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype AMurderOfCrows = AMurderOfCrows EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aMurderOfCrows :: EnemyCard AMurderOfCrows
aMurderOfCrows = enemy AMurderOfCrows Cards.aMurderOfCrows

-- | Replace destruction, so the Crows do not trigger effects for a destroyed enemy.
instance HasAbilities AMurderOfCrows where
  getAbilities (AMurderOfCrows a) = [mkAbility a 1 $ forced $ ThisEnemyDefeatedWindow When a.id]

-- | Reveal a random card from the encounter discard pile, if able.
instance RunMessage AMurderOfCrows where
  runMessage msg e@(AMurderOfCrows attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      cards <- shuffleM =<< getEncounterDiscard
      traverse_ revealFromEncounterDiscard (headMay cards)
      pure e
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      cancelMatching \case
        DoStep 1 (Destroy _ (isTarget attrs -> True)) -> True
        _ -> False
      shuffleIntoEncounterDeck attrs
      pure e
    _ -> AMurderOfCrows <$> runMessage msg attrs

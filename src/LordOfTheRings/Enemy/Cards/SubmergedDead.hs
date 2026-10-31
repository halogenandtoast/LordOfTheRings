module LordOfTheRings.Enemy.Cards.SubmergedDead (submergedDead) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (underwaterActive)

newtype SubmergedDead = SubmergedDead EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

submergedDead :: EnemyCard SubmergedDead
submergedDead = enemy SubmergedDead Cards.submergedDead

-- | When Revealed: If the active location is Underwater, return each other Submerged Dead in
-- the encounter discard pile to the staging area.
-- Shadow: If the active location is Underwater, put Submerged Dead into play in the staging
-- area.
instance RunMessage SubmergedDead where
  runMessage msg e@(SubmergedDead attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      whenM underwaterActive do
        pile <- getEncounterDiscard
        for_ [c | c <- pile, toCardCode c == toCardCode attrs, c.id /= attrs.cardId] addToStagingFromDiscard
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      whenM underwaterActive $ putShadowEnemyIntoStaging eid cid
      pure e
    _ -> SubmergedDead <$> runMessage msg attrs

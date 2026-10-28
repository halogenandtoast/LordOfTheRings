module LordOfTheRings.Enemy.Cards.MorgulSorcerer (morgulSorcerer) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (getToTheTower, placeOnTower)

newtype MorgulSorcerer = MorgulSorcerer EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

morgulSorcerer :: EnemyCard MorgulSorcerer
morgulSorcerer = enemy MorgulSorcerer Cards.morgulSorcerer

-- | Forced: After any number of progress tokens are placed on To the Tower, deal Morgul
-- Sorcerer a shadow card. Read off the tokens as they are placed, no window opening for an
-- objective's progress.
-- Shadow: Defending player must place 1 progress token on To the Tower or treat this
-- attack as undefended. An undefended attack already is, so it asks nothing.
instance RunMessage MorgulSorcerer where
  runMessage msg e@(MorgulSorcerer attrs) = case msg of
    PlaceTokens _ (ObjectiveTarget oid) Progress n | n > 0 -> do
      tower <- getToTheTower
      when (tower == Just oid) $ dealShadowCard attrs.id
      pure e
    ResolveShadowEffect cid _ pid defenders | cid == attrs.cardId -> do
      unless (null defenders) do
        chooseOneM pid do
          questionLabeled "morgulSorcerer.choose"
          causedBy attrs
          labeled "morgulSorcerer.progress" $ placeOnTower attrs 1
          -- the attack's own messages are on the game's queue once the choice is made
          labeled "morgulSorcerer.undefended" $ doStep 1 msg
      pure e
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == attrs.cardId -> do
      makeUndefended pid eid
      pure e
    _ -> MorgulSorcerer <$> runMessage msg attrs

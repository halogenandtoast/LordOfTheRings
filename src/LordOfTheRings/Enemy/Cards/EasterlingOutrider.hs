module LordOfTheRings.Enemy.Cards.EasterlingOutrider (easterlingOutrider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRiverRunning.Helpers (placeOnEasterlingPursuit)

newtype EasterlingOutrider = EasterlingOutrider EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

easterlingOutrider :: EnemyCard EasterlingOutrider
easterlingOutrider = enemy EasterlingOutrider Cards.easterlingOutrider

-- | Surge.
-- Forced: After Easterling Outrider engages a player, either place 1 resource on
-- Easterling Pursuit, or Easterling Outrider makes an immediate attack. The engaged player
-- chooses, as with The Dragon's Thrall.
instance HasAbilities EasterlingOutrider where
  getAbilities (EasterlingOutrider a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Discard an attachment you control.
instance RunMessage EasterlingOutrider where
  runMessage msg e@(EasterlingOutrider attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid ->
        chooseOneM pid do
          questionLabeled "easterlingOutrider.choose"
          causedBy attrs
          labeled "easterlingOutrider.pursuit" $ placeOnEasterlingPursuit attrs 1
          labeled "easterlingOutrider.attack" $ immediateAttack pid attrs.id
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      unless (null attachments) $ chooseAndDiscard (CardIdSource cid) pid attachments
      pure e
    _ -> EasterlingOutrider <$> runMessage msg attrs

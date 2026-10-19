module LordOfTheRings.Enemy.Cards.CavernGuardian (cavernGuardian) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype CavernGuardian = CavernGuardian EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

cavernGuardian :: EnemyCard CavernGuardian
cavernGuardian = enemy CavernGuardian Cards.cavernGuardian

-- | Doomed 1. Shadow: Choose and discard 1 attachment you control. Discarded objective cards
-- are returned to the staging area. (If this attack is undefended, discard all
-- attachments you control.)
instance RunMessage CavernGuardian where
  runMessage msg e@(CavernGuardian attrs) = case msg of
    ResolveShadowEffect cid eid pid defender | cid == attrs.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      ifUndefended defender (for_ attachments discard) (chooseAndDiscard (EnemySource eid) pid attachments)
      pure e
    _ -> CavernGuardian <$> runMessage msg attrs

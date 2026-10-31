module LordOfTheRings.Enemy.Cards.IllFatedGuard (illFatedGuard) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype IllFatedGuard = IllFatedGuard EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

illFatedGuard :: EnemyCard IllFatedGuard
illFatedGuard = enemy IllFatedGuard Cards.illFatedGuard

-- | While Ill-fated Guard is engaged with a player, that player cannot commit more than 1
-- character to each escape test.
instance HasModifiersFor IllFatedGuard where
  getModifiersFor (IllFatedGuard a) = case a.placement of
    EngagedWith pid -> modified a pid [EscapeTestCommitLimit 1]
    _ -> pure ()

-- | Shadow: Discard all attachments from each character you control with 3 or more
-- willpower.
instance RunMessage IllFatedGuard where
  runMessage msg e@(IllFatedGuard attrs) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      characters <- charactersControlledBy pid
      strong <- filterM (fmap (>= 3) . getWillpower) characters
      for_ strong \c -> traverse_ discard =<< getAttachmentsOn c
      pure e
    _ -> IllFatedGuard <$> runMessage msg attrs

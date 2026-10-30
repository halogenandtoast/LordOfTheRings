module LordOfTheRings.Treachery.Cards.FreezingBlast (freezingBlast) where

import LordOfTheRings.Helpers.Character (getCommittedCharacters)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FreezingBlast = FreezingBlast TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

freezingBlast :: TreacheryCard FreezingBlast
freezingBlast = treachery FreezingBlast Cards.freezingBlast

-- | Doomed 2.
-- When Revealed: Remove each damaged character from the quest.
-- Shadow: Attacking enemy gets +1 attack for each damaged character the defending player
-- controls.
instance RunMessage FreezingBlast where
  runMessage msg t@(FreezingBlast attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      committed <- getCommittedCharacters
      damaged <- filterM (fmap (> 0) . getDamage) committed
      traverse_ removeFromQuest damaged
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      damaged <- damagedCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
      enemyGetsAttack attrs eid (length damaged)
      pure t
    _ -> FreezingBlast <$> runMessage msg attrs

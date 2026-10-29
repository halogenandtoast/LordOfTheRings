module LordOfTheRings.Treachery.Cards.WildTenacity (wildTenacity) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Trait (Trait (Guardian))

newtype WildTenacity = WildTenacity TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wildTenacity :: TreacheryCard WildTenacity
wildTenacity = treachery WildTenacity Cards.wildTenacity

-- | When Revealed: Remove X time counters from each enemy in play with the time keyword,
-- where X is the number of players.
-- Shadow: If attacking enemy is a Guardian deal it 2 additional shadow cards for this
-- attack.
instance RunMessage WildTenacity where
  runMessage msg t@(WildTenacity attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- length <$> getRemainingPlayers
      enemies <- select AnyEnemy
      timed <- filterM (fmap (> 0) . getTimeX . toTarget) enemies
      for_ timed \e -> removeTimeCounters attrs e x
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      guardian <- selectAny (EnemyWithId eid <> EnemyWithTrait Guardian)
      when guardian $ dealShadowCardsDuringAttack eid pid defenders 2
      pure t
    _ -> WildTenacity <$> runMessage msg attrs
